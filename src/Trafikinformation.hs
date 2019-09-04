module Trafikinformation
  ( main
  ) where

import           Control.Monad             (join)
import           Control.Monad.Reader      (ReaderT (..), ask, asks)
import           Control.Monad.Trans.Class (lift)
import           TrafikLib.ParseRss        (Item (..))
import           TrafikLib.TrafikConfig    (TrafikConfig (..), defaultConfig)

main :: IO ()
main = do
  putStrLn "Starting..."
  runReaderT execUntilNoItems defaultConfig
  putStrLn "Exiting..."
  return ()

execUntilNoItems :: ReaderT TrafikConfig IO ()
execUntilNoItems = initialItems >>= bodyLoop

initialItems :: ReaderT TrafikConfig IO (Either String [Item])
initialItems = asks fetchFunction >>= lift

bodyLoop :: Either String [Item] -> ReaderT TrafikConfig IO ()
bodyLoop (Left err) = lift $ putStrLn err
bodyLoop (Right prevItems) = do
  asks sleepFunction >>= lift
  currItems <- body prevItems
  bodyLoop currItems

body :: [Item] -> ReaderT TrafikConfig IO (Either String [Item])
body prevItems = do
  lift $ putStrLn "loop"
  eitherItems <- asks fetchFunction >>= lift
  case eitherItems of
    Left err -> return $ Left err
    Right [] -> return $ Left "body: No items"
    Right currItems -> do
      let diffItems = getDiffItems prevItems currItems
      play $ reverse diffItems
      return . Right $ currItems

getDiffItems :: [Item] -> [Item] -> [Item]
getDiffItems []            = id
getDiffItems (latestOld:_) = takeWhile ((title latestOld /=) . title)

play :: [Item] -> ReaderT TrafikConfig IO ()
play [] = return ()
play (Item title url:xs) = do
  playFunction' <- asks playFunction
  lift $ putStrLn ("Playing: " ++ title)
  lift $ playFunction' url
  play xs
