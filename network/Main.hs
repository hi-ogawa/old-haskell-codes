import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import qualified Codec.Binary.UTF8.String as UTF (decodeString)
import Network.URI (isURI)
import System.Time (getClockTime)
import Data.Time.LocalTime (getZonedTime)
import Control.Monad (when)
import qualified Control.Exception as E (catch, IOException)
-- import System.IO.Unsafe (unsafePerformIO)

import Tweet -- (tweet)
import ParseRss -- (rssToItems, itemFilter1, title, link, tmpPath, Item)
import LogDatabase (logTweetResponse, itemFilter2)

logPath = "./.hs-log"

-- MAIN --
main = do mapM_ tweetFromRss rssUrls
          -- show `fmap` getZonedTime >>= writeFile tmpPath        -- updateした時刻を記録

-- tweeting with error handling--
tweetFromRss :: (String, String) -> IO ()
tweetFromRss (name, url) = 
 (do items' <- urlToItems url >>= itemFilter2
     mapM_ (\i -> do res <- tweet $ take 140 ("<"++ name ++ "> " ++ link i ++" "++ title i)
                     logTweetResponse name i res
           ) (reverse items')
 )
 `E.catch`
 errorHandler (name, url)
 
urlToItems :: String -> IO [Item]
urlToItems url = 
  do when (not $ isURI url) (fail "url invalid")
     let req = getRequest url
     res <- simpleHTTP req              -- if can't connect to the host, throw IO error
     text' <- getResponseBody res       -- if result err happend, throw IO error
     items <- rssToItems . UTF.decodeString $ text' -- if parse error happened, throw IO error
     return items

-- errorが起きたら@often_idにリプで報告、ログに記録--
errorHandler :: (String, String) -> E.IOException -> IO ()
errorHandler (name, url) err = 
  do (do tweet $ take 140 ("@often_id ERROR " ++ name ++ " - " ++ url ++ " log: " ++ show err)
         return ()
       ) `E.catch` (\(e :: E.IOException) -> return ())
     addErrorLog (name, url) err

-- error log の管理 --
addErrorLog :: (String, String) -> E.IOException -> IO ()
addErrorLog (name, url) err = 
  do time <- show `fmap` getClockTime
     appendFile logPath 
       $ (map (\c -> if c == '\n' then ' ' else c) (time++": "++name++" "++url++", <log> "++show err)) ++ "\n"

-- 監視するRSSのリスト --
rssUrls = [("伊藤梨沙子", "http://www.itorisako.com/rss.xml")
          ,("長崎すみれ", "http://www.nagasakisumire.com/rss.xml")
          ,("岡本夏美" , "http://www.natsumifc.com/rss.xml")
          ,("星名美津紀", "http://feedblog.ameba.jp/rss/ameblo/hoshina-miduki/rss20.xml")
          ,("伊藤優衣", "http://feedblog.ameba.jp/rss/ameblo/ito-yui-blog/rss20.xml")
          ,("田中れいな", "http://feedblog.ameba.jp/rss/ameblo/tanakareina-blog/rss20.xml")
          ,("娘9期","http://feedblog.ameba.jp/rss/ameblo/morningmusume-9ki/rss20.xml")
          ,("娘10期", "http://feedblog.ameba.jp/rss/ameblo/morningmusume-10ki/rss20.xml")]
