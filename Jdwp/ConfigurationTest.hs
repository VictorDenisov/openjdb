import Jdwp.Configuration
import Control.Monad.State (get, put, lift)
import Test.HUnit

testUpdateConfiguration :: Test
testUpdateConfiguration = TestCase (
    assertEqual
        "testUpdateConfiguration"
        ((), initConf {packetIdCounter = 2})
        (runConf setTwoCommandId initConf))

testIncPacketIdCounter :: Test
testIncPacketIdCounter = TestCase (
    assertEqual
        "testIncPacketIdCounter"
        ((), initConf {packetIdCounter = 1})
        (runConf incPacketIdCounter initConf))

testSetIdSizes :: Test
testSetIdSizes = TestCase (
    assertEqual
        "testSetIdSizes"
        ((), initConf {idSizes = IdSizes 1 2 3 4 5})
        (runConf (setIdSizes $ IdSizes 1 2 3 4 5) initConf))

testGetPacketIdCounter :: Test
testGetPacketIdCounter = TestCase (
    assertEqual
        "testGetPacketIdCounter"
        3
        (evalConf getPacketIdCounter $ initConf {packetIdCounter = 3}))

tests = TestList [ testUpdateConfiguration 
                 , testIncPacketIdCounter
                 , testSetIdSizes
                 ]

setTwoCommandId :: Conf ()
setTwoCommandId = do
    s <- get
    put $ s { packetIdCounter = (packetIdCounter s) + 2}

main = do
    runTestTT tests
