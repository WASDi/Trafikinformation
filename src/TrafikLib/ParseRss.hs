module TrafikLib.ParseRss
  ( parseRss
  , Item(..)
  , firstLeftOrRights -- TODO write tests
  ) where

import           Data.Either           (partitionEithers)

import qualified Text.XML.Light        as XML
import qualified Text.XML.Light.Input  as XMLI
import qualified Text.XML.Light.Output as XMLO
import qualified Text.XML.Light.Proc   as XMLP
import qualified Text.XML.Light.Types  as XMLT

data Item =
  Item
    { title :: String
    , url   :: String
    }
  deriving (Show)

parseRss :: String -> Either String [Item]
parseRss rssString = do
  root <- getRoot $ XMLI.parseXMLDoc rssString
  channel <- findChild "channel" root
  let rssItems = findChildren "item" channel
  items <- firstLeftOrRights $ map parseItem rssItems
  case items of
    [] -> Left "No items parsed"
    _  -> Right items

parseItem :: XMLT.Element -> Either String Item
parseItem element = do
  title <- XMLP.strContent <$> findChild "title" element
  enclosure <- findChild "enclosure" element
  case findAttr "url" enclosure of
    (Just url) -> Right $ Item title url
    Nothing    -> Left $ "URL not found for " ++ title

getRoot :: Maybe XMLT.Element -> Either String XMLT.Element
getRoot (Just x) = Right x
getRoot Nothing  = Left "parseXMLDoc == Nothing"

findChild :: String -> XMLT.Element -> Either String XMLT.Element
findChild name element =
  case XMLP.findChild (XML.unqual name) element of
    (Just child) -> Right child
    Nothing      -> Left $ "Child not found: " ++ name

findChildren :: String -> XMLT.Element -> [XMLT.Element]
findChildren = XMLP.findChildren . XML.unqual

findAttr :: String -> XMLT.Element -> Maybe String
findAttr = XMLP.findAttr . XML.unqual

firstLeftOrRights :: [Either a b] -> Either a [b]
firstLeftOrRights = firstLeftOrRights' . partitionEithers
  where
    firstLeftOrRights' (x:_, _) = Left x
    firstLeftOrRights' (_, xs)  = Right xs
