import System.Console.Haskeline
import System.Console.Haskeline.History (historyLines)
import System.IO (readFile)
import System.Exit (exitSuccess)
import System.Console.Haskeline.Completion(CompletionFunc)
import System.Console.GetOpt (getOpt, ArgOrder(..), OptDescr(..), ArgDescr(..))
import System.Environment (getArgs)
import Network
import GHC.IO.Handle
import Data.List (intercalate, find, isPrefixOf,
                  isSuffixOf, dropWhileEnd, dropWhile)
import Data.Maybe (fromMaybe, fromJust)
import GHC.Word (Word16, Word32, Word8)
import Network.Socket.Internal (PortNumber(..))
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.Error (runErrorT, ErrorT, MonadError(..), Error(..))
import Control.Monad.State (StateT(..), MonadState(..), evalStateT)
import Control.Monad.IO.Class (MonadIO)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM, forM, forM_, filterM, void, when)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Data.Char (isSpace)
import qualified JarFind as JF
import qualified Language.Java.Parser as JP
import qualified Language.Java.Syntax as JS
import Language.Java.Pretty (prettyPrint)

import qualified Language.Java.Jdi as J
import qualified Language.Java.Jdi.VirtualMachine as Vm
import qualified Language.Java.Jdi.Event as E
import qualified Language.Java.Jdi.EventSet as ES
import qualified Language.Java.Jdi.EventRequest as ER
import qualified Language.Java.Jdi.ReferenceType as RT
import qualified Language.Java.Jdi.ArrayReference as AR
import qualified Language.Java.Jdi.StringReference as SR
import qualified Language.Java.Jdi.Value as V
import qualified Language.Java.Jdi.StackFrame as SF
import qualified Language.Java.Jdi.ThreadReference as TR
import qualified Language.Java.Jdi.Method as M
import qualified Language.Java.Jdi.Location as L

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
    , Option ['c'] ["class-path"] (ReqArg ClassPath "CLASS-PATH")
                                  "class-path path"
    , Option ['s'] ["source-path"] (ReqArg SourcePath "SOURCE-PATH")
                                   "source-path path"
    ]

isPort :: Flag -> Bool
isPort (Port _) = True
isPort _ = False

isHost :: Flag -> Bool
isHost (Host _) = True
isHost _ = False

isClassPath :: Flag -> Bool
isClassPath (ClassPath _) = True
isClassPath _ = False

isSourcePath :: Flag -> Bool
isSourcePath (SourcePath _) = True
isSourcePath _ = False

getPort :: [Flag] -> PortID
getPort opts = PortNumber $ fromIntegral $ fromMaybe 2044 portValue
    where
        portValue :: Maybe Int
        portValue = do
            v <- find isPort opts
            return $ read $ port v

getHost :: [Flag] -> String
getHost opts = fromMaybe "localhost" $ find isHost opts >>= return . host

extractClassFileSource :: [Flag] -> JF.ClassFileSource
extractClassFileSource fs = JF.ClassPath $ map spToCfp $ filter isClassPath fs
    where spToCfp (ClassPath path) =
            if ".jar" `isSuffixOf` path
                then JF.JarFile path
                else if ".class" `isSuffixOf` path
                     then JF.ClassFile path
                     else error $ "unknown extension of the file: " ++ path

data CommandElement m st = CommandName String
                                       (CharParser st String)
                                       (CompletionFunc m)
                         | ClassName   String
                                       (CharParser st String)
                                       (CompletionFunc m)
                         | MethodName  String
                         | LineNum     Int

cmdList = ["quit", "version", "breakpoint", "next", "continue", "print", "list"]

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

cmdArgsErrMsg :: [String] -> String
cmdArgsErrMsg errors =
    "There are errors in command args parsing:\n\n"
    ++ concat errors

main :: IO ()
main = do
    args <- getArgs
    let (opts, unparsed, errors) = getOpt Permute options args
    when (not $ null errors) $ fail $ cmdArgsErrMsg errors
    when (Version `elem` opts) $ putStrLn "0.0.1" >> exitSuccess
    when (((not . isPort) `all` opts) && ((not . isHost) `all` opts))
        $ fail "Host and port arguments are required"

    cpClasses <- map snd <$> (JF.parseFileSource $ extractClassFileSource opts)
    let sourceFiles = map path $ filter isSourcePath opts
    res <- runInputT (Settings (commandLineComplete cpClasses) Nothing True)
               $ runErrorT $ evalStateT (Vm.runVirtualMachine
                               (getHost opts)
                               (getPort opts)
                               (initialSetup >> eventLoop))
                               (DebugConfig [] Nothing sourceFiles Nothing)
    case res of
          Left e -> putStrLn $ "Error during execution: " ++ e
          Right v -> putStrLn $ "Success"

data DebugConfig = DebugConfig
    { breakpoints :: [Command] -- breakpoints whose classes are not loaded yet.
    , currentThread :: Maybe TR.ThreadReference
    , sourceFiles :: [String]
    , currentLocation :: Maybe L.Location
    }

getCurrentLocation :: (Error e, MonadError e m) => Debugger m L.Location
getCurrentLocation = do
    l <- currentLocation `liftM` get
    case l of
        Just loc -> return loc
        Nothing  -> throwError $ strMsg "Current location is unavailable"

setCurrentLocation :: Monad m => L.Location -> Debugger m ()
setCurrentLocation l = do
    dc <- get
    put $ dc {currentLocation = Just l}

type Debugger = StateT DebugConfig

alignNum :: Int -> Int -> String
alignNum width number = replicate (width - length s) ' ' ++ s ++ " "
    where s = show number

linesFromSourceName :: (MonadIO m, Error e, MonadError e m)
                    => String -> Debugger m [String]
linesFromSourceName sourceName = do
    sourceList <- filter (sourceName `isSuffixOf`) `liftM` getSourceFiles
    when (length sourceList > 1) $
        throwError $ strMsg "there are more than one source file"
    if null sourceList
        then return []
        else do
            rawLines <- lines `liftM` (liftIO . readFile) (head sourceList)
            let l = (length rawLines)
            let width = length $ show l
            return $ zipWith (++) (map (alignNum width) [1..l]) rawLines

printSourceLines :: Vm.VirtualMachine (Debugger (ErrorT String (InputT IO))) ()
printSourceLines = do
    l <- lift getCurrentLocation
    liftIO $ putStrLn $ show l
    sn <- J.sourceName l
    let ln = L.lineNumber l
    dat <- lift $ linesFromSourceName sn
    when (null dat) $
        throwError $ sn ++ ":" ++ show ln ++ " - source unavailable"
    let blockStart = max 0 (ln - 5)
    liftIO $ putStrLn $ intercalate "\n" $ take 10 $ drop blockStart $ dat
    `catchError` (\e -> liftIO . putStrLn $ show e)

addBreakpoint :: Monad m => Command -> Debugger m ()
addBreakpoint c = do
    dc <- get
    put $ dc {breakpoints = c : breakpoints dc}

listBreakpoints :: Monad m => Debugger m [Command]
listBreakpoints = breakpoints `liftM` get

setCurrentThread :: Monad m => TR.ThreadReference -> Debugger m ()
setCurrentThread tr = do
    dc <- get
    put $ dc {currentThread = Just tr}

initialSetup :: Vm.VirtualMachine (Debugger (ErrorT String (InputT IO))) ()
initialSetup = do
    ER.enable ER.createClassPrepareRequest
    return ()

getSourceFiles :: (Error e, MonadError e m) => Debugger m [String]
getSourceFiles = sourceFiles `liftM` get

printSourceLine :: Vm.VirtualMachine (Debugger (ErrorT String (InputT IO))) ()
printSourceLine = do
    l <- lift getCurrentLocation
    let ln = L.lineNumber l
    sn <- J.sourceName l
    dat <- lift $ linesFromSourceName sn
    when (null dat) $
        throwError $ sn ++ ":" ++ show ln ++ " - source unavailable"
    liftIO $ putStrLn $ dat !! (ln - 1)
    `catchError` (\e -> liftIO . putStrLn $ show e)

eventLoop :: Vm.VirtualMachine (Debugger (ErrorT String (InputT IO))) ()
eventLoop = do
    es <- ES.removeEvent
    let event = head $ ES.events es
    continue <- case E.eventKind event of
        E.ClassPrepare -> do
            liftIO $ putStrLn "Received ClassPrepare request"
            liftIO $ putStrLn $ show $ E.referenceType event
            setupBreakpoints $ E.referenceType event
            J.resume es
            return True
        E.Breakpoint -> do
            lift . setCurrentThread $ E.thread event
            liftIO $ putStrLn $ show event
            l <- J.location event
            lift $ setCurrentLocation l
            printSourceLine
            commandLoop
        E.SingleStep -> do
            liftIO $ putStrLn $ show event
            l <- J.location event
            lift $ setCurrentLocation l
            printSourceLine
            commandLoop
        E.VmDeath -> do
            liftIO $ putStrLn $ show event
            return False
        otherwise -> do
            liftIO $ putStrLn $ show event
            commandLoop
    if continue
        then eventLoop
        else return ()


className :: Command -> String
className (BreakpointLineCommand cn _) = cn
className (BreakpointMethodCommand cn _) = cn

setupBreakpoints :: RT.ReferenceType
                 -> Vm.VirtualMachine (Debugger (ErrorT String (InputT IO))) ()
setupBreakpoints refType = do
    refName <- J.name refType
    bpList <- filter ((refName ==) . className) <$> lift listBreakpoints
    forM_ bpList (setupBreakpoint refType)

setupBreakpoint :: RT.ReferenceType
                -> Command
                -> Vm.VirtualMachine (Debugger (ErrorT String (InputT IO))) ()
setupBreakpoint refType (BreakpointLineCommand nm line) = do
    lineLocations <- J.allLineLocations refType
    let matchingLines = filter ((line ==) . L.lineNumber) lineLocations
    if null matchingLines
        then liftIO . putStrLn
                $ "there is no executable source code for line: " ++ show line
        else void $ ER.enable $ ER.createBreakpointRequest (head matchingLines)
setupBreakpoint refType (BreakpointMethodCommand nm method) = do
    methods <- RT.methods refType
    matchingMethods <- filterM ((liftM (method ==)) . J.name) methods
    if null matchingMethods
        then liftIO . putStrLn $ "there is no method with name: " ++ method
        else do
            l <- J.location $ head matchingMethods
            let br = ER.createBreakpointRequest l
            void $ ER.enable br

getCurrentThread :: (Error e, MonadError e m) => Debugger m TR.ThreadReference
getCurrentThread = do
    value <- currentThread `liftM` get
    case value of
        Just v -> return v
        Nothing -> throwError (strMsg "No current thread is available")

calcSyntaxTree :: MonadException m =>
    JS.Exp -> Vm.VirtualMachine (Debugger (ErrorT String (InputT m))) V.Value
calcSyntaxTree (JS.ExpName (JS.Name ((JS.Ident name):[]))) = do
    ct <- lift getCurrentThread
    fr <- head <$> TR.allFrames ct
    loc <- J.location fr
    vars <- M.variables (L.method loc)
    args <- M.arguments (L.method loc)
    let ref = L.declaringType loc
    fields <- RT.fields ref
    allVars <- filterM (((name ==) `liftM`) . J.name) (vars ++ args)
    allFields <- filterM (((name ==) `liftM`) . J.name) fields
    when (null allVars && null allFields)
                    $ throwError $ "Unknown variable name: " ++ name
    when (length allVars + length allFields > 1)
                    $ throwError $ "Ambiguous name: " ++ name
    if null allVars
        then RT.getValue ref $ head allFields
        else SF.getValue fr $ head allVars
calcSyntaxTree (JS.ArrayAccess (JS.ArrayIndex arrExp indExp)) = do
    arr <- calcSyntaxTree arrExp
    ind <- calcSyntaxTree indExp
    case (arr, ind) of
        ((V.ArrayValue ref), (V.IntValue i)) -> AR.getValue ref i
        otherwise -> throwError $ "Type error"
calcSyntaxTree (JS.Lit (JS.Int v)) = return (V.IntValue $ fromIntegral v)
calcSyntaxTree e = throwError
        $ "Processing of this expression is not implemented yet: " ++ (show e)

showValue :: MonadException m =>
             V.Value
          -> Vm.VirtualMachine (Debugger (ErrorT String (InputT m))) String
showValue (V.CharValue c)   = return [c]
showValue (V.LongValue c)   = return $ show c
showValue (V.IntValue c)    = return $ show c
showValue (V.StringValue s) = show <$> SR.stringValue s
showValue (V.ArrayValue a)  = do
    av <- AR.getValues a
    vs <- mapM showValue av
    return $ "[" ++ intercalate ", " vs ++ "]"
showValue v                 = return $ show v

liftInpTtoVM = lift . lift . lift

commandLoop :: Vm.VirtualMachine (Debugger (ErrorT String (InputT IO))) Bool
commandLoop = do
    minput <- liftInpTtoVM $ getInputLine "(jdb) "
    case minput of
        Nothing -> do -- Ctrl-D was pressed.
            return False
        Just input -> do -- Something was entered. Empty line as an option.
            line <- if input == ""
                        then do
                            hlines <- historyLines <$> liftInpTtoVM getHistory
                            if null hlines
                                then return ""
                                else return $ head hlines
                        else return input
            case parseCommand line of
                QuitCommand -> return False
                VersionCommand -> do
                    p <- Vm.version
                    liftIO $ putStrLn $ show p
                    commandLoop
                ContinueCommand ->
                    Vm.resume >> return True
                BreakpointLineCommand name line -> do
                    lift . addBreakpoint $ BreakpointLineCommand name line
                    commandLoop
                BreakpointMethodCommand name method -> do
                    lift . addBreakpoint $ BreakpointMethodCommand name method
                    commandLoop
                ListCommand -> do
                    printSourceLines
                    commandLoop
                PrintCommand arg -> do
                    case JP.parser JP.exp arg of
                        Right st -> do
                                v <- calcSyntaxTree st
                                sv <- showValue v
                                liftIO $ putStrLn sv
                              `catchError` (\e -> liftIO . putStrLn $ show e)
                        Left  e  ->
                               liftIO . putStrLn $ "SyntaxError " ++ (show e)
                    commandLoop
                NextCommand -> do
                    ct <- lift getCurrentThread
                    ER.enable $ ER.addCountFilter
                                    1
                                    (ER.createStepRequest
                                        ct
                                        J.StepLine
                                        J.StepOver)
                    Vm.resume
                    return True
                    `catchError` (\e -> do
                                    liftIO . putStrLn $ show e
                                    commandLoop)
                ErroneousCommand error -> do
                    liftIO $ putStrLn
                        $ "Error during parsing the command: " ++ error
                    commandLoop

data Command = VersionCommand
             | ContinueCommand
             | BreakpointLineCommand String Int -- class line
             | BreakpointMethodCommand String String -- class method
             | QuitCommand
             | ListCommand
             | NextCommand
             | PrintCommand String
             | ErroneousCommand String
               deriving Show

parseCommand :: String -> Command
parseCommand "" = ErroneousCommand "Empty line. No previous history."
parseCommand input = case parse commandParser "(unknown)" input of
    Left parseError -> ErroneousCommand $ show parseError
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
    s <- many1 (noneOf "")
    return $ PrintCommand s
