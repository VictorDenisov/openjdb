import Jdwp.Configuration
import Control.Monad.State (get, put, lift)
import Test.HUnit

testUpdateConfiguration :: Test
testUpdateConfiguration = TestCase (
    assertEqual
        "testUpdateConfiguration"
        ((), initConf {commandCounter = 2})
        (runConf setTwoCommandId initConf))

testIncCmdCounter :: Test
testIncCmdCounter = TestCase (
    assertEqual
        "testIncCmdCounter"
        ((), initConf {commandCounter = 1})
        (runConf incCmdCounter initConf))

testSetIdSizes :: Test
testSetIdSizes = TestCase (
    assertEqual
        "testSetIdSizes"
        ((), initConf {idSizes = IdSizes 1 2 3 4 5})
        (runConf (setIdSizes $ IdSizes 1 2 3 4 5) initConf))

tests = TestList [ testUpdateConfiguration 
                 , testIncCmdCounter
                 , testSetIdSizes
                 ]

setTwoCommandId :: Config ()
setTwoCommandId = do
    s <- get
    put $ s { commandCounter = (commandCounter s) + 2}

main = do
    runTestTT tests
