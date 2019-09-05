module TrafikLib.TrafikConfig where

import           Control.Concurrent (threadDelay)
import           TrafikLib.Fetching (fetchItems)
import           TrafikLib.ParseRss (Item)
import           TrafikLib.Play     (playUrl)

data TrafikConfig m =
  TrafikConfig
    { fetchFunction :: m (Either String [Item])
    , sleepFunction :: m ()
    , playFunction  :: String -> m ()
    , logFunction   :: String -> m ()
    }

defaultConfig :: TrafikConfig IO
defaultConfig =
  TrafikConfig
    { fetchFunction = fetchItems
    , sleepFunction = threadDelay (60 * 1000 * 1000)
    , playFunction = playUrl
    , logFunction = putStrLn
    }
