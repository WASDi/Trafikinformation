module TrafikLib.TrafikConfig where

import           Control.Concurrent (threadDelay)
import           TrafikLib.Fetching (fetchItems)
import           TrafikLib.ParseRss (Item)
import           TrafikLib.Play     (playUrl)

data TrafikConfig =
  TrafikConfig
    { fetchFunction :: IO (Either String [Item])
    , sleepFunction :: IO ()
    , playFunction  :: String -> IO ()
    }

defaultConfig :: TrafikConfig
defaultConfig =
  TrafikConfig {fetchFunction = fetchItems, sleepFunction = threadDelay (60 * 1000 * 1000), playFunction = playUrl}
