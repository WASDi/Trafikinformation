module Trafikinformation
  ( main
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Monad      (join)
import           TrafikLib.Fetching (fetchItems)
import           TrafikLib.ParseRss (Item, title, url)
import           TrafikLib.Play     (playUrl)

main :: IO ()
main = do
  putStrLn "Starting..."
  initialItems >>= bodyLoop
  putStrLn "Exiting..."
  return ()

initialItems :: IO (Either String [Item])
initialItems = withFetchingItems id

bodyLoop :: Either String [Item] -> IO ()
bodyLoop (Left err) = putStrLn err
bodyLoop (Right prevItems) = sleep60s >> body prevItems >>= bodyLoop
  where
    sleep60s = threadDelay (60 * 1000 * 1000)

body :: [Item] -> IO (Either String [Item])
body prevItems = do
  putStrLn "loop"
  eitherItems <- fetchItems
  case eitherItems of
    Left err -> return $ Left err
    Right [] -> return $ Left "body: No items"
    Right currItems -> do
      let diffItems = getDiffItems prevItems currItems
      play $ reverse diffItems
      return . Right $ currItems

withFetchingItems :: ([Item] -> a) -> IO (Either String a)
withFetchingItems f = do
  eitherItems <- fetchItems
  return $ f <$> eitherItems

getDiffItems :: [Item] -> [Item] -> [Item]
getDiffItems []            = id
getDiffItems (latestOld:_) = takeWhile ((title latestOld /=) . title)

play :: [Item] -> IO ()
play [] = return ()
play (x:xs) = do
  putStrLn $ "Playing: " ++ title x
  playUrl (url x)
  play xs
