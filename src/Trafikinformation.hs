module Trafikinformation
  ( main
  , execUntilNoItems
  ) where

import           Control.Monad             (join)
import           Control.Monad.Reader      (ReaderT (..), ask, asks)
import           Control.Monad.Trans.Class (lift)
import           TrafikLib.ParseRss        (Item (..))
import           TrafikLib.TrafikConfig    (TrafikConfig (..), defaultConfig)

main :: IO ()
main = do
  putStrLn "Starting..."
  execUntilNoItems defaultConfig
  putStrLn "Exiting..."
  return ()

type TrafikApp m = ReaderT (TrafikConfig m) m

execUntilNoItems :: (Monad m) => TrafikConfig m -> m ()
execUntilNoItems = runReaderT (initialItems >>= bodyLoop)

initialItems :: (Monad m) => TrafikApp m (Either String [Item])
initialItems = do
  eitherItems <- asks fetchFunction >>= lift
  logFunction' <- asks logFunction
  case eitherItems of
    Left err -> return $ Left err
    Right [] -> return $ Left "initialItems: No items"
    Right currItems@(first:_) -> do
      lift $ logFunction' ("Initial item: " ++ title first)
      return $ Right currItems

bodyLoop :: (Monad m) => Either String [Item] -> TrafikApp m ()
bodyLoop (Left err) = asks logFunction >>= lift . ($ err)
bodyLoop (Right prevItems) = do
  asks sleepFunction >>= lift
  currItems <- body prevItems
  bodyLoop currItems

body :: (Monad m) => [Item] -> TrafikApp m (Either String [Item])
body prevItems = do
  logFunction' <- asks logFunction
  lift $ logFunction' "loop"
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

play :: (Monad m) => [Item] -> TrafikApp m ()
play [] = return ()
play (Item title url:xs) = do
  playFunction' <- asks playFunction
  logFunction' <- asks logFunction
  lift $ logFunction' ("Playing: " ++ title)
  lift $ playFunction' url
  play xs
