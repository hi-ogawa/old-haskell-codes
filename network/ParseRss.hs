module ParseRss where

import Text.XML.HaXml hiding (when)
import Text.XML.HaXml.Parse (xmlParse')
import Text.XML.HaXml.Pretty (document, content, element)
import System.Locale (defaultTimeLocale, rfc822DateFormat)
import Data.Time.Format (parseTime)
import Data.Time.LocalTime (ZonedTime, getZonedTime, zonedTimeToUTC)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime)
import Data.Maybe (fromJust)
import Data.List (elemIndex, isPrefixOf)
import Control.Monad (when)
import qualified Control.Exception as E (catch, IOException)

data Item = Item {title :: String, link :: String, pubDate :: ZonedTime} deriving Show

rssToItems :: Monad m => String -> m [Item] 
rssToItems rssText = 
  case xmlParse' "" rssText of
    Left   msg                                  -> fail msg
    Right (Document _ _ (Elem _ _ cons) _)      
      -> do when (null items) (fail "PARSE ERROR: can't find the tag > item")
            mapM itemToData items
      where items = concatMap (deep $ tag "item") cons
        
itemToData :: Monad m => Content i -> m Item
itemToData item =
  do p     <- childTagElem item "pubDate" >>= parseRFC822
     l     <- childTagElem item "link"
     t     <- childTagElem item "title"
     return $ Item t l p
        
childTagElem :: Monad m => Content i -> String -> m String
childTagElem c str = 
  case txt `o` children `o` tag str `o` children $ c of
    []          -> fail ("PARSE ERROR: can't find tag > " ++ str)
    (elem:_)    -> return.show.content $ elem

-- calendar parse --
parseRFC822 :: Monad m => String -> m ZonedTime
parseRFC822 str = 
  case parseTime defaultTimeLocale rfc822DateFormat str' of
    Just ti -> return ti
    Nothing -> fail ("PARSE ERROR: pubDate element "++str')
  where str' = map (\c -> if c == '-' then ' ' else c) str -- for evergreen rss syntax

-- tmpPathより前回の更新時刻を参考にしフィルターする
itemFilter1 :: [Item] -> IO [Item]
itemFilter1 is = (do preTime <- read `fmap` readFile tmpPath
                     return $ filter (\i -> comp (pubDate i) > comp preTime) is'
                 ) 
                 `E.catch` (\(e :: E.IOException) -> return is')
  where comp = zonedTimeToUTC
        is'  = filter (not . isPrefixOf "<![CDATA[PR:" . title) is -- for ameba ad
        
-- 前回のupdateの時刻を保存する
tmpPath = "./.hs-tmp"