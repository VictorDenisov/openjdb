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

import Jdwp.Protocol
import Jdwp.Configuration

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
              then bracket (openConnection (getHost opts) (getPort opts)) hClose mainLoop
              else putStrLn "Host and port arguments are required"

mainLoop :: Handle -> IO ()
mainLoop h = do
    handshake h
    packet <- waitVmStartEvent h
    putStrLn $ show packet
    runInputT defaultSettings $ evalConfT (initialSetup h >> loop) initConf
    where 
        loop :: ConfT (InputT IO) ()
        loop = do
            minput <- lift $ getInputLine "(jdb) "
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just input -> do
                    lift $ outputStrLn $ "Input was: " ++ input
                    cntr <- getPacketIdCounter
                    incPacketIdCounter
                    processCommand h cntr input
                    loop

handshake :: Handle -> IO ()
handshake h = do
    putStrLn "Connected. Initiating handshake..."
    hPutStr h "JDWP-Handshake"
    hFlush h
    value <- B.hGet h 14
    if value == (B8.pack "JDWP-Handshake")
    then putStrLn "Handshake successful."
    else putStrLn "Handshake FAILED!"

initialSetup :: Handle -> ConfT (InputT IO) ()
initialSetup h = do
    liftIO $ putStrLn "Sending id sizes request..."
    cntr <- getPacketIdCounter
    incPacketIdCounter
    processCommand h cntr "idsizes"

processCommand :: Handle -> PacketId -> String -> ConfT (InputT IO) ()
processCommand h cntr "version" = do
    liftIO $ sendPacket h $ versionCommand cntr
    liftIO $ putStrLn "version request sent"
    idsizes <- getIdSizes
    p <- liftIO $ waitReply h idsizes $ \_ -> parseVersionReply idsizes
    liftIO $ putStrLn $ show p

processCommand h cntr "resume" = do
    liftIO $ sendPacket h $ resumeVmCommand cntr
    idsizes <- getIdSizes
    r <- liftIO $ waitReply h idsizes $ \_ -> parseEmptyData idsizes
    liftIO $ putStrLn $ show r
    e <- liftIO $ waitEvent h idsizes
    liftIO $ putStrLn $ show e

processCommand h cntr "idsizes" = do
    liftIO $ sendPacket h $ idSizesCommand cntr
    idsizes <- getIdSizes
    r <- liftIO $ waitReply h idsizes $ \_ -> parseIdSizesReply idsizes
    liftIO $ putStrLn $ show r
    setIdSizes $ idSizes $ dat r

processCommand h cntr "show idsizes" = do
    is <- getIdSizes
    liftIO $ putStrLn $ show is

processCommand h cntr "set" = do
    liftIO $ sendPacket h $ eventSetRequest cntr ClassPrepare All
    idsizes <- getIdSizes
    r <- liftIO $ waitReply h idsizes $ \_ -> parseEventSetRequestReply idsizes
    liftIO $ putStrLn $ show r

processCommand _ _ cmd = liftIO $
    putStrLn ("Hello from processCommand " ++ cmd)

receivePacket :: Handle -> IdSizes -> ReplyDataParser -> IO Packet
receivePacket h idsizes f = do
    inputAvailable <- hWaitForInput h (-1)
    if inputAvailable
    then do putStrLn "Receiving a packet..."
            lengthString <- B.hGet h 4
            let length = (fromIntegral $ runGet (parseInt) lengthString) - 4
            reminder <- B.hGet h length
            let p = runGet (parsePacket idsizes f) (lengthString `B.append` reminder)
            return p
    else error "No input available where expected"

waitReply :: Handle -> IdSizes -> ReplyDataParser -> IO Packet
waitReply h idsizes f = do
    packet <- receivePacket h idsizes f
    case packet of
        CommandPacket _ _ _ _ _ _ -> error "reply expected, but command received"
        {- Normally here some queue should be implemented, but currectly for brevity
         - we assume that we never get event before reply.
         -}
        ReplyPacket _ _ _ _ _ -> return packet

waitEvent :: Handle -> IdSizes -> IO Packet
waitEvent h idsizes = do
    packet <- receivePacket h idsizes $ \_ -> error "ReplyDataParser is invoked where only command parsing is expected"
    case packet of
        CommandPacket _ _ _ _ _ _ -> return packet
        ReplyPacket _ _ _ _ _ -> error "CommandPacket is expected, but reply packet received"
        
-- When we parse this event we don't have information about size of threadId.
-- We use the fact that threadId is the last field in the event and we can determine its size
-- as request_length - length_of_fields_before_threadId.
-- for current version of JDWP length_of_fields_before_threadIs is 21.
waitVmStartEvent :: Handle -> IO Packet
waitVmStartEvent h = do
    inputAvailable <- hWaitForInput h (-1)
    if inputAvailable
    then do putStrLn "Waiting for VmStartEvent"
            lengthString <- B.hGet h 4
            let length = (fromIntegral $ runGet (parseInt) lengthString) - 4
            reminder <- B.hGet h length
            let threadIdSize = fromIntegral $ (length + 4) - 21
            let replyParser = \_ -> error "ReplyDataParser is not expected to be invoked."
            let p = runGet (parsePacket (IdSizes 0 0 threadIdSize 0 0) replyParser) (lengthString `B.append` reminder)
            return p
    else error "No input available where expected"
    

sendPacket :: Handle -> Packet -> IO ()
sendPacket h p = do
    B.hPut h $ runPut $ putPacket p
    hFlush h
