import System.Console.Haskeline
import System.Console.Haskeline.Completion(CompletionFunc)
import System.Console.GetOpt (getOpt, ArgOrder(..), OptDescr(..), ArgDescr(..))
import System.Environment (getArgs)
import Network
import GHC.IO.Handle
import Data.List (intercalate, find, isPrefixOf)
import Data.Maybe (fromMaybe, fromJust)
import GHC.Word (Word16, Word32, Word8)
import Network.Socket.Internal (PortNumber(..))
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.State (StateT(..), MonadState(..), evalStateT)
import Control.Monad.IO.Class (MonadIO)
import Control.Applicative ((<$>))
import Control.Monad (liftM)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

import Jdi

data Flag = Version
          | Port { port :: String }
          | Host { host :: String }
            deriving (Show, Eq)

options :: [OptDescr Flag]
options =
    [ Option ['v'] ["version"] (NoArg Version) "show version number"
    , Option ['p'] ["port"]    (ReqArg Port "PORT") "port number"
    , Option ['h'] ["host"]    (ReqArg Host "HOST") "host number"
    ]

cmdArgsErrMsg :: [String] -> String
cmdArgsErrMsg errors = 
    "There are errors in command args parsing:\n\n"
    ++ concat errors

isPort :: Flag -> Bool
isPort (Port _) = True
isPort _ = False

isHost :: Flag -> Bool
isHost (Host _) = True
isHost _ = False

getPort :: [Flag] -> PortID
getPort opts = PortNumber $ fromIntegral $ ((read $ port $ fromMaybe (Port "2044") (find isPort opts)) :: Int)

getHost :: [Flag] -> String
getHost opts = host $ fromMaybe (Host "localhost") (find isHost opts)

cmdList = ["quit", "version", "breakpoint", "next", "continue", "print"]

commandLineComplete :: MonadIO m => CompletionFunc m
commandLineComplete (leftLine, _) = do
    return ("", ncl)
    where
        ncl = map (\name -> Completion name name True) $ filter (line `isPrefixOf`) cmdList
        line = reverse leftLine

main :: IO ()
main = do
    args <- getArgs
    let (opts, unparsed, errors) = getOpt Permute options args
    if not $ null errors
        then putStr $ cmdArgsErrMsg errors
        else if Version `elem` opts
                 then putStrLn "1.0"
                 else if ((isPort `any` opts) && (isHost `any` opts))
                          then  runInputT (Settings commandLineComplete Nothing True) $
                                    evalStateT (runVirtualMachine
                                                    (getHost opts)
                                                    (getPort opts)
                                                    (initialSetup >> eventLoop))
                                               (DebugConfig [])
                          else putStrLn "Host and port arguments are required"

data Bp = Bp String
          deriving (Show, Eq)

data DebugConfig = DebugConfig
    { breakpoints :: [Bp] }
    
type Debugger = StateT DebugConfig

addBreakpoint :: MonadIO m => String -> Debugger m ()
addBreakpoint s = do
    dc <- get
    put $ dc {breakpoints = Bp s : breakpoints dc}

listBreakpoints :: MonadIO m => Debugger m [Bp]
listBreakpoints = breakpoints `liftM` get

initialSetup :: VirtualMachine (Debugger (InputT IO)) ()
initialSetup = do
    enable createClassPrepareRequest
    return ()

eventLoop :: VirtualMachine (Debugger (InputT IO)) ()
eventLoop = do
    es <- removeEvent
    let event = head $ events es
    case eventKind event of
        ClassPrepare -> do
            liftIO $ putStrLn "Received ClassPrepare request"
            liftIO $ putStrLn $ show $ referenceType event
            resume es
            eventLoop
        otherwise -> do
            liftIO $ putStrLn $ show event
            commandLoop
            eventLoop

commandLoop :: MonadException m => VirtualMachine (Debugger (InputT m)) ()
commandLoop = do
    minput <- (lift . lift) $ getInputLine "(jdb) "
    case minput of
        Nothing -> return ()
        Just input -> 
            case parseCommand (fromJust minput) of
                QuitCommand -> return ()
                VersionCommand -> do
                    p <- version
                    lift . lift . outputStrLn $ show p
                    commandLoop
                ContinueCommand ->
                    resumeVm
                UnknownCommand error -> do
                    lift . lift . outputStrLn $ "Error during parsing the command:"
                    commandLoop

data Command = VersionCommand
             | ContinueCommand
             | BreakpointLineCommand String Int -- class line
             | BreakpointMethodCommand String String -- class method
             | QuitCommand
             | UnknownCommand ParseError
               deriving Show

parseCommand :: String -> Command
parseCommand input = case parse commandParser "(unknown)" input of
    Left parseError -> UnknownCommand parseError
    Right command -> command

commandParser :: CharParser st Command
commandParser =
    parseVersion <|> parseContinue <|> parseQuit

parseVersion :: CharParser st Command
parseVersion = do
    string "version"
    return VersionCommand

parseContinue :: CharParser st Command
parseContinue = do
    string "continue"
    return ContinueCommand

parseQuit :: CharParser st Command
parseQuit = do
    string "quit"
    return QuitCommand

{-
processCommand v | take 4 v == "set " = lift $ addBreakpoint (drop 4 v)
processCommand "list" = do
    bps <- lift $ listBreakpoints
    liftIO $ putStrLn $ show bps
    -}
