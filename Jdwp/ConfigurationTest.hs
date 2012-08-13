import Jdwp.Configuration
import Control.Monad.State (get, put, lift)
import Test.HUnit

testUpdateConfiguration :: Test
testUpdateConfiguration = TestCase (
    assertEqual
        "testUpdateConfiguration"
        ((), initialConf {commandCounter = 2})
        (runConf setTwoCommandId initialConf))

testIncCmdCounter :: Test
testIncCmdCounter = TestCase (
    assertEqual
        "testIncCmdCounter"
        ((), initialConf {commandCounter = 1})
        (runConf incCmdCounter initialConf))

tests = TestList [ testUpdateConfiguration 
                 , testIncCmdCounter
                 ]

main = do
    runTestTT tests

setTwoCommandId :: Config ()
setTwoCommandId = do
    s <- get
    put $ s { commandCounter = (commandCounter s) + 2}
