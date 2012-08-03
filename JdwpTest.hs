import Test.HUnit

import qualified Data.ByteString.Lazy as B
import Data.Word (Word8)
import Data.Binary.Get (runGet)
import Jdwp
import Data.Binary (decodeFile)

testParseByte :: [Word8] -> JavaByte -> Test
testParseByte xs answer = TestCase (
    assertEqual
        ("testParseByte " ++ (show xs))
        answer (runGet parseByte (B.pack xs))
    )

testParseInt :: [Word8] -> JavaInt -> Test
testParseInt xs answer = TestCase (
    assertEqual
        ("testParseInt " ++ (show xs))
        answer (runGet parseInt (B.pack xs))
    )

testParseBoolean :: [Word8] -> JavaBoolean -> Test
testParseBoolean xs answer = TestCase (
    assertEqual
        ("testParseBoolean " ++ (show xs))
        answer (runGet parseBoolean (B.pack xs))
    )

testParseString :: [Word8] -> JavaString -> Test
testParseString xs answer = TestCase (
    assertEqual
        ("testParseString " ++ (show xs))
        answer (runGet parseString (B.pack xs))
    )

testParseEvent :: [Word8] -> Event -> Test
testParseEvent xs answer = TestCase (
    assertEqual
        ("testParseEvent " ++ (show xs))
        answer (runGet parseEvent (B.pack xs))
    )


tests = TestList
    [ TestLabel "parseByte tests" $ TestList
        [ testParseByte [343] 87 ]
    , TestLabel "parseInt tests" $ TestList
        [ testParseInt [0, 0, 1, 2] 258 ]
    , TestLabel "parseBoolean tests" $ TestList
        [ testParseBoolean [2] True
        , testParseBoolean [0] False
        ]
    , TestLabel "parseString tests" $ TestList
        [ testParseString [0, 0, 0, 2, 0x68, 0x69] "hi" ]
    , TestLabel "parseEvent tests" $ TestList
        [ testParseEvent [90, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 2] (VmStartEvent 1 2) ]
    ]

main = do
    line <- B.readFile "sample/reply.txt"
    let value = runGet (parseList 2 (parsePacket (\id -> replyParser $ dataParsers (1, 1)))) line
    putStrLn $ show value
    
--main = putStrLn $ show $ runGet parseInt (B.pack [0, 0, 1, 2])
--main = runTestTT tests
