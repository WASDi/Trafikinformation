module TrafikLib.Fetching
  ( fetchItems
  ) where

import qualified System.Process     as P
import qualified TrafikLib.ParseRss as ParseRss

onlineMode :: Bool
onlineMode = True

fetchItems :: IO (Either String [ParseRss.Item])
fetchItems = ParseRss.parseRss <$> fetchRss

fetchRss :: IO String
fetchRss =
  if onlineMode
    then wget rssUrl
    else readFile "example.rss"

wget :: String -> IO String
wget url = P.readProcess "wget" ["-O", "-", "--quiet", url] []

rssUrl :: String
rssUrl = "https://api.sr.se/api/rss/pod/18748"
