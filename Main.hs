import System.Console.Haskeline
import System.Console.GetOpt (getOpt, ArgOrder(..), OptDescr(..), ArgDescr(..))
import System.Environment (getArgs)
import Network
import GHC.IO.Handle
import Data.List (intercalate, find)
import Data.Maybe (fromMaybe)
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

main :: IO ()
main = do
    args <- getArgs
    let (opts, unparsed, errors) = getOpt Permute options args
    if not $ null errors
        then putStr $ cmdArgsErrMsg errors
        else if Version `elem` opts
                 then putStrLn "1.0"
                 else if ((isPort `any` opts) && (isHost `any` opts))
                          then  runInputT defaultSettings $
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

commandLoop :: VirtualMachine (Debugger (InputT IO)) ()
commandLoop = do
    minput <- (lift . lift) $ getInputLine "(jdb) "
    case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just "resume" -> do
            processCommand "resume"
            return ()
        Just input -> do
            processCommand input
            commandLoop

processCommand :: String -> VirtualMachine (Debugger (InputT IO)) ()
processCommand "version" = do
    p <- version
    liftIO $ putStrLn $ show p

processCommand "resume" = do
    resumeVm

{-
processCommand "show idsizes" = do
    is <- getIdSizes
    liftIO $ putStrLn $ show is

    -}
processCommand v | take 4 v == "set " = lift $ addBreakpoint (drop 4 v)
processCommand "list" = do
    bps <- lift $ listBreakpoints
    liftIO $ putStrLn $ show bps

processCommand cmd = liftIO $ do
    putStrLn ("Unknown command sequence: " ++ cmd)
