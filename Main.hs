import System.Console.Haskeline
import System.Console.Haskeline.Completion(CompletionFunc)
import System.Console.GetOpt (getOpt, ArgOrder(..), OptDescr(..), ArgDescr(..))
import System.Environment (getArgs)
import Network
import GHC.IO.Handle
import Data.List (intercalate, find, isPrefixOf, isSuffixOf, dropWhileEnd, dropWhile)
import Data.Maybe (fromMaybe, fromJust)
import GHC.Word (Word16, Word32, Word8)
import Network.Socket.Internal (PortNumber(..))
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.State (StateT(..), MonadState(..), evalStateT)
import Control.Monad.IO.Class (MonadIO)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM, forM, forM_, filterM, void)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Data.Char (isSpace)
import qualified JarFind as JF

import Jdi

data Flag = Version
          | Port { port :: String }
          | Host { host :: String }
          | ClassPath { path :: String }
            deriving (Show, Eq)

options :: [OptDescr Flag]
options =
    [ Option ['v'] ["version"] (NoArg Version) "show version number"
    , Option ['p'] ["port"]    (ReqArg Port "PORT") "port number"
    , Option ['h'] ["host"]    (ReqArg Host "HOST") "host number"
    , Option ['c'] ["class-path"]    (ReqArg ClassPath "CLASS-PATH") "class-path path"
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

isClassPath :: Flag -> Bool
isClassPath (ClassPath _) = True
isClassPath _ = False

getPort :: [Flag] -> PortID
getPort opts = PortNumber $ fromIntegral $ ((read $ port $ fromMaybe (Port "2044") (find isPort opts)) :: Int)

getHost :: [Flag] -> String
getHost opts = host $ fromMaybe (Host "localhost") (find isHost opts)

extractClassFileSource :: [Flag] -> JF.ClassFileSource
extractClassFileSource fs = JF.ClassPath $ map spToCfp $ filter isClassPath fs
    where spToCfp (ClassPath path) =
            if ".jar" `isSuffixOf` path
                then JF.JarFile path
                else if ".class" `isSuffixOf` path
                     then JF.ClassFile path
                     else error $ "unknown extension of the file: " ++ path

data CommandElement m st = CommandName String (CharParser st String) (CompletionFunc m)
                         | ClassName   String (CharParser st String) (CompletionFunc m)
                         | MethodName  String
                         | LineNum     Int

cmdList = ["quit", "version", "breakpoint", "next", "continue", "print"]

trim = dropWhileEnd isSpace . (dropWhile isSpace)

buildCompletions = map (\name -> Completion name name True)

listMethods className = []

commandLineComplete :: MonadIO m => CompletionFunc m
commandLineComplete (leftLine, _) = do
    if "breakpoint" `isPrefixOf` line
        then do
            let whiteSpaceTerminated = " " `isSuffixOf` line
            let w = words line
            case (length w, whiteSpaceTerminated) of
                (2, True) -> return (leftLine, buildCompletions $ listMethods (w !! 1))
        else return ("", ncl)
    where
        ncl = buildCompletions $ filter (line `isPrefixOf`) cmdList
        line = reverse leftLine

deriving instance Show JF.ClassFileSource

main :: IO ()
main = do
    args <- getArgs
    let (opts, unparsed, errors) = getOpt Permute options args
    putStrLn $ show $ extractClassFileSource opts
    if not $ null errors
        then putStr $ cmdArgsErrMsg errors
        else if Version `elem` opts
                 then putStrLn "1.0"
                 else if ((isPort `any` opts) && (isHost `any` opts))
                          then runInputT (Settings commandLineComplete Nothing True) $
                                    evalStateT (runVirtualMachine
                                                    (getHost opts)
                                                    (getPort opts)
                                                    (initialSetup >> eventLoop))
                                               (DebugConfig [])
                          else putStrLn "Host and port arguments are required"

data DebugConfig = DebugConfig
    { breakpoints :: [Command] }
    
type Debugger = StateT DebugConfig

addBreakpoint :: Monad m => Command -> Debugger m ()
addBreakpoint c = do
    dc <- get
    put $ dc {breakpoints = c : breakpoints dc}

listBreakpoints :: Monad m => Debugger m [Command]
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
            setupBreakpoints $ referenceType event
            resume es
            eventLoop
        otherwise -> do
            liftIO $ putStrLn $ show event
            commandLoop
            eventLoop

className :: Command -> String
className (BreakpointLineCommand cn _) = cn
className (BreakpointMethodCommand cn _) = cn

setupBreakpoints :: ReferenceType -> VirtualMachine (Debugger (InputT IO)) ()
setupBreakpoints refType = do
    refName <- name refType
    bpList <- filter ((refName ==) . className) <$> lift listBreakpoints
    forM_ bpList (setupBreakpoint refType)

setupBreakpoint :: ReferenceType -> Command -> VirtualMachine (Debugger (InputT IO)) ()
setupBreakpoint refType (BreakpointLineCommand nm line) = do
    lineLocations <- allLineLocations refType
    let matchingLines = filter ((line ==) . lineNumber) lineLocations
    if null matchingLines
        then liftIO . putStrLn $ "there is no executable source code for line: " ++ show line
        else void $ enable $ createBreakpointRequest (head matchingLines)
setupBreakpoint refType (BreakpointMethodCommand nm method) = do
    methods <- allMethods refType
    matchingMethods <- filterM ((liftM (method ==)) . name) methods
    if null matchingMethods
        then liftIO . putStrLn $ "there is no method with name: " ++ method
        else void $ enable =<< createBreakpointRequest <$> (location $ head matchingMethods)

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
                BreakpointLineCommand name line -> do
                    lift . addBreakpoint $ BreakpointLineCommand name line
                    commandLoop
                BreakpointMethodCommand name method -> do
                    lift . addBreakpoint $ BreakpointMethodCommand name method
                    commandLoop
                ListCommand -> do
                    l <- lift listBreakpoints
                    lift . lift . outputStrLn $ show l
                    commandLoop
                UnknownCommand error -> do
                    lift . lift . outputStrLn $ "Error during parsing the command: " ++ (show error)
                    commandLoop

data Command = VersionCommand
             | ContinueCommand
             | BreakpointLineCommand String Int -- class line
             | BreakpointMethodCommand String String -- class method
             | QuitCommand
             | ListCommand
             | UnknownCommand ParseError
               deriving Show

parseCommand :: String -> Command
parseCommand input = case parse commandParser "(unknown)" input of
    Left parseError -> UnknownCommand parseError
    Right command -> command

commandParser :: CharParser st Command
commandParser = parseVersion
            <|> parseContinue
            <|> parseQuit
            <|> parseBreakpointCommand
            <|> parseList

parseList :: CharParser st Command
parseList = string "list" >> return ListCommand

parseBreakpointCommand :: CharParser st Command
parseBreakpointCommand = do
    parseBreakpoint
    char ' '
    className <- parseClassName
    char ' '
    ((return . (BreakpointMethodCommand className) =<< parseMethod) <|>
     (return . (BreakpointLineCommand className) =<< parseLineNum))

parseBreakpoint :: CharParser st String
parseBreakpoint = string "breakpoint"

parseClassName :: CharParser st String
parseClassName = many1 (noneOf " ")

parseMethod :: CharParser st String
parseMethod = many1 (noneOf " 0123456789")

parseLineNum :: CharParser st Int
parseLineNum = read <$> many1 digit

parseVersion :: CharParser st Command
parseVersion = string "version" >> return VersionCommand

parseContinue :: CharParser st Command
parseContinue = string "continue" >> return ContinueCommand

parseQuit :: CharParser st Command
parseQuit = string "quit" >> return QuitCommand
