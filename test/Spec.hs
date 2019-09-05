import           Trafikinformation        (execUntilNoItems)
import           TrafikLib.ParseRss       (Item (..))
import           TrafikLib.TrafikConfig   (TrafikConfig (..))

import           Control.Monad.State.Lazy (State (..), execState, gets, modify)
import qualified Test.HUnit               as Test

main :: IO ()
main = do
  let initialState = FakeState initialItemStack 0 [] []
  let endState = execState (execUntilNoItems configUsingFakeState) initialState
  Test.runTestTT $ testSuite endState
  return ()

testSuite :: FakeState -> Test.Test
testSuite (FakeState itemStack' sleepCounter' urlsPlayed' logs') =
  let t1 = Test.assertBool "itemStack should be empty" (null itemStack')
      t2 = Test.assertEqual "sleepCounter" sleepCounter' 8
      t3 = Test.assertEqual "Expected URLs played" (reverse urlsPlayed') ["url:a", "url:b", "url:c", "url:d", "url:e"]
      t4 =
        Test.assertEqual
          "Expected logs"
          (reverse logs')
          [ "Initial item: title:oldItem"
          , "loop"
          , "Playing: title:a"
          , "loop"
          , "loop"
          , "Playing: title:b"
          , "Playing: title:c"
          , "loop"
          , "loop"
          , "Playing: title:d"
          , "loop"
          , "Playing: title:e"
          , "loop"
          , "loop"
          , "popItemStack == empty"
          ]
   in Test.TestList $ map Test.TestCase [t1, t2, t3, t4]

data FakeState =
  FakeState
    { itemStack    :: [[Item]]
    , sleepCounter :: Int
    , urlsPlayed   :: [String]
    , logs         :: [String]
    }

initialItemStack :: [[Item]]
initialItemStack =
  let s0 = [stubItem "oldItem"]
      s1 = stubItem "a" : s0
      s3 = stubItem "c" : stubItem "b" : s1
      s4 = stubItem "d" : s3
      s5 = stubItem "e" : s4
   in [s0, s1, s1, s3, s3, s4, s5, s5]

stubItem :: String -> Item
stubItem s = Item ("title:" ++ s) ("url:" ++ s)

configUsingFakeState :: TrafikConfig (State FakeState)
configUsingFakeState =
  TrafikConfig
    { fetchFunction = popItemStack
    , sleepFunction = modify (\s -> s {sleepCounter = sleepCounter s + 1})
    , playFunction = \url -> modify (\s -> s {urlsPlayed = url : urlsPlayed s})
    , logFunction = \logString -> modify (\s -> s {logs = logString : logs s})
    }

popItemStack :: State FakeState (Either String [Item])
popItemStack = do
  itemStack' <- gets itemStack
  case itemStack' of
    [] -> return $ Left "popItemStack == empty"
    (top:rest) -> do
      modify (\s -> s {itemStack = rest})
      return $ Right top
