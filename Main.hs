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

import qualified Language.Java.Jdi as J

data Flag = Version
          | Port { port :: String }
          | Host { host :: String }
          | ClassPath { path :: String }
          | SourcePath { path :: String }
            deriving (Show, Eq)

options :: [OptDescr Flag]
options =
    [ Option ['v'] ["version"] (NoArg Version) "show version number"
    , Option ['p'] ["port"]    (ReqArg Port "PORT") "port number"
    , Option ['h'] ["host"]    (ReqArg Host "HOST") "host number"
    , Option ['c'] ["class-path"]    (ReqArg ClassPath "CLASS-PATH") "class-path path"
    , Option ['s'] ["source-path"]   (ReqArg SourcePath "SOURCE-PATH") "source-path path"
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

isMethod :: JF.Member -> Bool
isMethod (JF.Method {}) = True
isMethod _ = False

commandLineComplete :: Monad m => [JF.Class] -> CompletionFunc m
commandLineComplete classes (leftLine, _) = do
    if "breakpoint " `isPrefixOf` line
        then do
            case (length w, whiteSpaceTerminated) of
                (1, True) -> return (leftLine,
                                buildCompletions $ allClassesNames)
                (2, False) -> return (drop (length $ w !! 1) leftLine,
                                buildCompletions
                                    $ filter ((w !! 1) `isPrefixOf`)
                                                            allClassesNames)
                (2, True) -> return (leftLine,
                                buildCompletions $ listMethodsNames (w !! 1))
                (3, False) -> return (drop (length $ w !! 2) leftLine,
                                buildCompletions
                                    $ filter ((w !! 2) `isPrefixOf`)
                                                $ listMethodsNames (w !! 1))
                (3, True) -> return (leftLine, [])
        else return ("", ncl)
    where
        w = words line
        whiteSpaceTerminated = " " `isSuffixOf` line
        ncl = buildCompletions $ filter (line `isPrefixOf`) cmdList
        line = reverse leftLine
        listMethodsNames = map JF.mName . methodsOfClass
        myClasses className = filter ((className ==) . JF.clsName) classes
        allMembersOfClass = concat . map JF.clsMembers . myClasses
        allClassesNames = map JF.clsName classes
        methodsOfClass = filter isMethod . allMembersOfClass
        allAvailablePrintables = []

main :: IO ()
main = do
    args <- getArgs
    let (opts, unparsed, errors) = getOpt Permute options args
    cpClasses <- map snd <$> (JF.parseFileSource $ extractClassFileSource opts)
    if not $ null errors
        then putStr $ cmdArgsErrMsg errors
        else if Version `elem` opts
                 then putStrLn "1.0"
                 else if ((isPort `any` opts) && (isHost `any` opts))
                          then runInputT (Settings (commandLineComplete cpClasses) Nothing True) $
                                    evalStateT (J.runVirtualMachine
                                                    (getHost opts)
                                                    (getPort opts)
                                                    (initialSetup >> eventLoop))
                                               (DebugConfig [] Nothing)
                          else putStrLn "Host and port arguments are required"

data DebugConfig = DebugConfig
    { breakpoints :: [Command]
    , currentThread :: Maybe J.ThreadReference }
    
type Debugger = StateT DebugConfig

addBreakpoint :: Monad m => Command -> Debugger m ()
addBreakpoint c = do
    dc <- get
    put $ dc {breakpoints = c : breakpoints dc}

listBreakpoints :: Monad m => Debugger m [Command]
listBreakpoints = breakpoints `liftM` get

setCurrentThread :: Monad m => J.ThreadReference -> Debugger m ()
setCurrentThread tr = do
    dc <- get
    put $ dc {currentThread = Just tr}

getCurrentThread :: Monad m => Debugger m (Maybe J.ThreadReference)
getCurrentThread = currentThread `liftM` get

initialSetup :: J.VirtualMachine (Debugger (InputT IO)) ()
initialSetup = do
    J.enable J.createClassPrepareRequest
    return ()

eventLoop :: J.VirtualMachine (Debugger (InputT IO)) ()
eventLoop = do
    es <- J.removeEvent
    let event = head $ J.events es
    case J.eventKind event of
        J.ClassPrepare -> do
            liftIO $ putStrLn "Received ClassPrepare request"
            liftIO $ putStrLn $ show $ J.referenceType event
            setupBreakpoints $ J.referenceType event
            J.resume es
            eventLoop
        J.Breakpoint -> do
            lift . setCurrentThread $ J.thread event
            liftIO $ putStrLn $ show event
            l <- J.location event
            liftIO $ putStrLn $ show l
            sn <- J.sourceName l
            liftIO $ putStrLn sn
            commandLoop
            eventLoop
        J.SingleStep -> do
            liftIO $ putStrLn $ show event
            l <- J.location event
            liftIO $ putStrLn $ show l
            sn <- J.sourceName l
            liftIO $ putStrLn sn
            commandLoop
            eventLoop
        J.VmDeath -> do
            liftIO $ putStrLn $ show event
            return ()
        otherwise -> do
            liftIO $ putStrLn $ show event
            commandLoop
            eventLoop

className :: Command -> String
className (BreakpointLineCommand cn _) = cn
className (BreakpointMethodCommand cn _) = cn

setupBreakpoints :: J.ReferenceType -> J.VirtualMachine (Debugger (InputT IO)) ()
setupBreakpoints refType = do
    refName <- J.name refType
    bpList <- filter ((refName ==) . className) <$> lift listBreakpoints
    forM_ bpList (setupBreakpoint refType)

setupBreakpoint :: J.ReferenceType -> Command -> J.VirtualMachine (Debugger (InputT IO)) ()
setupBreakpoint refType (BreakpointLineCommand nm line) = do
    lineLocations <- J.allLineLocations refType
    let matchingLines = filter ((line ==) . J.lineNumber) lineLocations
    if null matchingLines
        then liftIO . putStrLn $ "there is no executable source code for line: " ++ show line
        else void $ J.enable $ J.createBreakpointRequest (head matchingLines)
setupBreakpoint refType (BreakpointMethodCommand nm method) = do
    methods <- J.allMethods refType
    matchingMethods <- filterM ((liftM (method ==)) . J.name) methods
    if null matchingMethods
        then liftIO . putStrLn $ "there is no method with name: " ++ method
        else void $ J.enable =<< J.createBreakpointRequest <$> (J.location $ head matchingMethods)

commandLoop :: MonadException m => J.VirtualMachine (Debugger (InputT m)) ()
commandLoop = do
    minput <- (lift . lift) $ getInputLine "(jdb) "
    case minput of
        Nothing -> return ()
        Just input -> 
            case parseCommand (fromJust minput) of
                QuitCommand -> return ()
                VersionCommand -> do
                    p <- J.version
                    lift . lift . outputStrLn $ show p
                    commandLoop
                ContinueCommand ->
                    J.resumeVm
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
                PrintCommand arg -> do
                    (Just ct) <- lift $ getCurrentThread
                    fr <- head <$> J.frames ct 0 0
                    liftIO $ putStrLn $ show fr
                    loc <- J.location fr
                    liftIO $ putStrLn $ show loc
                    var <- head <$> J.variablesByName (J.method loc) arg
                    liftIO $ putStrLn $ show loc
                    v <- J.getValue fr var
                    liftIO $ putStrLn $ show v
                    commandLoop
                NextCommand -> do
                    ct <- lift $ getCurrentThread
                    case ct of
                        Just curThread -> do
                            J.enable $ J.addCountFilter
                                            1
                                            (J.createStepRequest
                                                curThread
                                                J.StepLine
                                                J.StepOver)
                            J.resumeVm
                        Nothing -> do
                            lift . lift . outputStrLn $ "no previous breakpoint available"
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
             | NextCommand
             | PrintCommand String
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
            <|> parseNext
            <|> parsePrint

parseNext :: CharParser st Command
parseNext = string "next" >> return NextCommand

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

parsePrint :: CharParser st Command
parsePrint = do
    string "print "
    s <- many1 (noneOf " ")
    return $ PrintCommand s
