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
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Binary.Get (runGet, Get)
import Data.Binary.Put (runPut)
import Data.Binary (get)

import Jdi

openConnection :: String -> PortID -> IO Handle
openConnection host port = do
    h <- connectTo host port
    hSetBinaryMode h True
    return h

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
                          then  runInputT defaultSettings $ runVirtualMachine (getHost opts) (getPort opts) eventLoop
                          else putStrLn "Host and port arguments are required"

eventLoop :: VirtualMachine (InputT IO) ()
eventLoop = do
    event <- removeEvent
    liftIO $ putStrLn $ show event
    commandLoop
    eventLoop

commandLoop :: VirtualMachine (InputT IO) ()
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

processCommand :: String -> VirtualMachine (InputT IO) ()
processCommand "version" = do
    p <- version
    liftIO $ putStrLn $ show p

processCommand "resume" = do
    resume

{-
processCommand "show idsizes" = do
    is <- getIdSizes
    liftIO $ putStrLn $ show is

processCommand "set" = do
    liftIO $ sendPacket h $ eventSetRequest cntr ClassPrepare All
    idsizes <- getIdSizes
    r <- liftIO $ waitReply h idsizes $ \_ -> parseEventSetRequestReply idsizes
    liftIO $ putStrLn $ show r
    -}

processCommand cmd = liftIO $
    putStrLn ("Unknown command sequence: " ++ cmd)

