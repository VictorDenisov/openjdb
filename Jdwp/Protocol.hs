module Jdwp.Protocol where

import Prelude hiding (length, id)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import Data.Binary (Binary(..), Get, Put)
import Data.Bits ((.&.))
import Data.List (find)
import Control.Applicative ((<$>), (<*>))

------------Packet description and parsing section.
-- {{{
type PacketId = Word32
type CommandSet = Word8
type Command = Word8

data Packet = CommandPacket { length     :: Word32
                            , id         :: PacketId
                            , flags      :: Word8
                            , commandSet :: CommandSet
                            , command    :: Command
                            , dat        :: PacketData
                            }
            | ReplyPacket   { length     :: Word32
                            , id         :: PacketId
                            , flags      :: Word8
                            , errorCode  :: Word16
                            , dat        :: PacketData
                            }
              deriving Show

type ReplyDataParser = PacketId -> Get PacketData

parsePacket :: IdSizes -> ReplyDataParser -> Get Packet
parsePacket idsizes replyDataParser = do
    l <- get
    i <- get
    f <- get
    if (f .&. 0x80) == 0
    then do
        cs <- get
        c  <- get
        d  <- (commandParser $ dataParsers (cs, c)) idsizes
        return (CommandPacket l i f cs c d)
    else do
        e <- get
        d <- case e of
            0 -> replyDataParser i
            _ -> parseEmptyData idsizes
        return (ReplyPacket l i f e d)

parseList :: Word32 -> Get a -> Get [a]
parseList 0 p = return []
parseList l p = do
    x <- p
    xs <- parseList (l - 1) p
    return (x:xs)

putPacket :: Packet -> Put
putPacket (CommandPacket l i f cs c d) = do
    put l
    put i
    put f
    put cs
    put c
    putPacketData d

putPacket (ReplyPacket l i f e d) = do
    put l
    put i
    put f
    put e
    putPacketData d
-- }}}
---------------General types section
-- {{{
type JavaByte            = Word8
type JavaInt             = Word32
type JavaLong            = Word64
type JavaString          = String
type JavaBoolean         = Bool
type JavaFieldId         = Word64
type JavaMethodId        = Word64
type JavaObjectId        = Word64
type JavaReferenceTypeId = Word64
type JavaFrameId         = Word64
type JavaThreadId        = JavaObjectId

parseByte :: Get JavaByte
parseByte = get

parseBoolean :: Get JavaBoolean
parseBoolean = (/= 0) <$> (get :: Get Word8)

parseInt :: Get JavaInt
parseInt = get

parseLong :: Get JavaLong
parseLong = get

parseString :: Get JavaString
parseString = do
    len <- (get :: Get Word32)
    list <- parseList len (get :: Get Word8)
    return $ B8.unpack $ B.pack list

parseReferenceTypeId :: JavaInt -> Get JavaReferenceTypeId
parseReferenceTypeId 1 = fromIntegral <$> (get :: Get Word8)
parseReferenceTypeId 2 = fromIntegral <$> (get :: Get Word16)
parseReferenceTypeId 4 = fromIntegral <$> (get :: Get Word32)
parseReferenceTypeId 8 = fromIntegral <$> (get :: Get Word64)
parseReferenceTypeId s = error $ "Currently we can not process values of this size: " ++ (show s)

parseThreadId :: JavaInt -> Get JavaThreadId
parseThreadId 1 = fromIntegral <$> (get :: Get Word8)
parseThreadId 2 = fromIntegral <$> (get :: Get Word16)
parseThreadId 4 = fromIntegral <$> (get :: Get Word32)
parseThreadId 8 = fromIntegral <$> (get :: Get Word64)
parseThreadId s = error $ "Currently we can not process values of this size: " ++ (show s)

-- }}}
-------PacketData parsing section---------------------
-- {{{
-- PacketData components declaration.
---- {{{
data PacketData = EventSet
                    { suspendPolicy :: SuspendPolicy
                    , events        :: [Event]
                    }
                | VersionReply 
                    { description :: JavaString
                    , jdwpMajor   :: JavaInt
                    , jdwpMinor   :: JavaInt
                    , vmVersion   :: JavaString
                    , vmName      :: JavaString
                    }
                | IdSizesReply
                    { idSizes :: IdSizes
                    }
                | ThreadIdPacketData
                    { tId :: JavaThreadId
                    }
                | EventRequestSetPacketData
                    { eventKind :: EventKind
                    , suspendPolicy :: SuspendPolicy
                    }
                | EventRequestSetReply
                    { requestIdReply :: JavaInt
                    }
                | EmptyPacketData
                  deriving (Eq, Show)

data IdSizes = IdSizes
                    { fieldIdSize         :: JavaInt
                    , methodIdSize        :: JavaInt
                    , objectIdSize        :: JavaInt
                    , referenceTypeIdSize :: JavaInt
                    , frameIdSize         :: JavaInt
                    } deriving (Eq, Show)

threadIdSize :: IdSizes -> JavaInt
threadIdSize is = objectIdSize is

data Event = VmStartEvent
                { requestId :: JavaInt
                , threadId  :: JavaThreadId
                }
           | VmDeathEvent
                { requestId :: JavaInt
                }
           | ClassPrepareEvent
                { requestId   :: JavaInt
                , threadId    :: JavaThreadId
                , refTypeTag  :: TypeTag
                , typeId      :: JavaReferenceTypeId
                , signature   :: JavaString
                , classStatus :: ClassStatus
                }
           | NoEvent
             deriving (Show, Eq)

data EventKind = VmDisconnected
               | VmStart
               | ThreadDeath
               | SingleStep
               | Breakpoint
               | FramePop
               | Exception
               | UserDefined
               | ThreadStart
               | ThreadEnd
               | ClassPrepare
               | ClassUnload
               | ClassLoad
               | FieldAccess
               | FieldModification
               | ExceptionCatch
               | MethodEntry
               | MethodExit
               | VmInit
               | VmDeath
                 deriving (Eq, Show)

data SuspendPolicy = None
                   | EventThread
                   | All
                     deriving (Eq, Show)

data TypeTag = Class
             | Interface
             | Array
               deriving (Eq, Show)

newtype ClassStatus = ClassStatus JavaInt
                      deriving (Eq, Show)

fromNumber :: [(JavaByte, a)] -> JavaByte -> a
fromNumber list n = case find ((== n) . fst) list of
                            Just (_, v)  -> v
                            Nothing -> error $ "Number " ++ (show n) ++ " doesn't match any value from list"

toNumber :: (Eq a, Show a) => [(JavaByte, a)] -> a -> JavaByte
toNumber list e = case find ((== e) . snd) list of
                            Just (n, _) -> n
                            Nothing     -> error $ "list doesn't have value " ++ (show e)

eventKindNumberList :: [(JavaByte, EventKind)]
eventKindNumberList = [ (  1, SingleStep)
                      , (  2, Breakpoint)
                      , (  3, FramePop)
                      , (  4, Exception)
                      , (  5, UserDefined)
                      , (  6, ThreadStart)
                      , (  7, ThreadEnd)
                      , (  8, ClassPrepare)
                      , (  9, ClassUnload)
                      , ( 10, ClassLoad)
                      , ( 20, FieldAccess)
                      , ( 21, FieldModification)
                      , ( 30, ExceptionCatch)
                      , ( 40, MethodEntry)
                      , ( 41, MethodExit)
                      , ( 90, VmInit)
                      , ( 99, VmDeath)
                      , (100, VmDisconnected)
                      ]

eventKindFromNumber :: JavaByte -> EventKind
eventKindFromNumber = fromNumber eventKindNumberList

numberFromEventKind :: EventKind -> JavaByte
numberFromEventKind = toNumber eventKindNumberList

suspendPolicyNumberList :: [(JavaByte, SuspendPolicy)]
suspendPolicyNumberList = [ (0, None)
                          , (1, EventThread)
                          , (2, All)
                          ]

suspendPolicyFromNumber :: JavaByte -> SuspendPolicy
suspendPolicyFromNumber = fromNumber suspendPolicyNumberList

numberFromSuspendPolicy :: SuspendPolicy -> JavaByte
numberFromSuspendPolicy = toNumber suspendPolicyNumberList

typeTagNumberList :: [(JavaByte, TypeTag)]
typeTagNumberList = [ (1, Class)
                    , (2, Interface)
                    , (3, Array)
                    ]

typeTagFromNumber :: JavaByte -> TypeTag
typeTagFromNumber = fromNumber typeTagNumberList

numberFromTypeTag :: TypeTag -> JavaByte
numberFromTypeTag = toNumber typeTagNumberList
-- }}}
-- Parsing and putting functions
---- {{{
--- SuspendPolicy
putSuspendPolicy :: SuspendPolicy -> Put
putSuspendPolicy s = put $ numberFromSuspendPolicy s

parseSuspendPolicy :: Get SuspendPolicy
parseSuspendPolicy = suspendPolicyFromNumber <$> (get :: Get JavaByte)

--- ClassStatus
putClassStatus :: ClassStatus -> Put
putClassStatus (ClassStatus v) = put v

parseClassStatus :: Get ClassStatus
parseClassStatus = ClassStatus <$> get

--- EventKind
putEventKind :: EventKind -> Put
putEventKind e = put $ numberFromEventKind e

parseEventKind :: Get EventKind
parseEventKind = eventKindFromNumber <$> (get :: Get JavaByte)

--- TypeTag
putTypeTag :: TypeTag -> Put
putTypeTag t = put $ numberFromTypeTag t

parseTypeTag :: Get TypeTag
parseTypeTag = typeTagFromNumber <$> (get :: Get JavaByte)

putPacketData :: PacketData -> Put
putPacketData (EventSet sp e) = do
    putSuspendPolicy sp
    mapM_ putEvent e
putPacketData (ThreadIdPacketData i) =
    put i
putPacketData (EventRequestSetPacketData ek sp) = do
    putEventKind ek
    putSuspendPolicy sp
    put (0 :: JavaInt)
putPacketData (EmptyPacketData) = return ()

putEvent :: Event -> Put
putEvent (VmStartEvent ri ti) = do
    put ri
    put ti

parseIdSizesReply :: IdSizes -> Get PacketData
parseIdSizesReply _ = IdSizesReply <$> (IdSizes
                        <$> parseInt
                        <*> parseInt
                        <*> parseInt
                        <*> parseInt
                        <*> parseInt)

parseVersionReply :: IdSizes -> Get PacketData
parseVersionReply _ = VersionReply
                        <$> parseString
                        <*> parseInt
                        <*> parseInt
                        <*> parseString
                        <*> parseString

parseEventSetRequestReply :: IdSizes -> Get PacketData
parseEventSetRequestReply _ = EventRequestSetReply
                        <$> parseInt

parseEventSet :: IdSizes -> Get PacketData
parseEventSet idsizes = do
    sp <- parseSuspendPolicy
    eventCount <- parseInt
    eventList <- parseList eventCount (parseEvent idsizes)
    return $ EventSet sp eventList

parseEvent :: IdSizes -> Get Event
parseEvent idsizes = do
    eventKind <- parseEventKind
    case eventKind of
        ClassPrepare -> ClassPrepareEvent
                            <$> parseInt
                            <*> (parseThreadId $ threadIdSize idsizes)
                            <*> parseTypeTag
                            <*> (parseReferenceTypeId $ referenceTypeIdSize idsizes)
                            <*> parseString
                            <*> parseClassStatus
        VmInit  -> VmStartEvent <$> parseInt <*> (parseThreadId $ threadIdSize idsizes)
        VmDeath -> VmDeathEvent <$> parseInt
        _       -> return NoEvent

parseEmptyData :: IdSizes -> Get PacketData
parseEmptyData _ = return EmptyPacketData
-- }}}
data DataParser = DataParser
                { commandParser :: IdSizes -> Get PacketData
                , replyParser   :: IdSizes -> Get PacketData
                }

dataParsers :: (CommandSet, Command) -> DataParser
dataParsers ( 1,   1) = DataParser parseEmptyData parseVersionReply
dataParsers ( 1,   7) = DataParser parseEmptyData parseIdSizesReply
dataParsers (15,   1) = DataParser parseEmptyData parseEventSetRequestReply
dataParsers (64, 100) = DataParser parseEventSet  parseEmptyData
dataParsers _ = undefined

-- }}}
------------Command Constructors Section
-- {{{
versionCommand :: PacketId -> Packet
versionCommand id = CommandPacket 11 id 0 1 1 EmptyPacketData

idSizesCommand :: PacketId -> Packet
idSizesCommand id = CommandPacket 11 id 0 1 7 EmptyPacketData

resumeVmCommand :: PacketId -> Packet
resumeVmCommand id = CommandPacket 11 id 0 1 9 EmptyPacketData

resumeThreadCommand :: PacketId -> JavaThreadId -> Packet
resumeThreadCommand id threadId = CommandPacket 19 id 0 11 3 (ThreadIdPacketData threadId)

eventSetRequest :: PacketId -> EventKind -> SuspendPolicy -> Packet
eventSetRequest id ek sp = CommandPacket 17 id 0 15 1 $ EventRequestSetPacketData ek sp

-- }}}
-- vim: foldmethod=marker foldmarker={{{,}}}
