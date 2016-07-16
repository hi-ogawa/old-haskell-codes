module LogDatabase where

import Database.HDBC
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Data.Time.LocalTime (ZonedTime, getZonedTime)
import Data.List (isPrefixOf)
import ParseRss (Item, title, link, pubDate)
import Tweet (myshow)
import qualified Network.OAuth.Http.Response as Res (Response)

tableSql = "CREATE TABLE item (name, title, link, pubDate, tweetResponse)"
viewSql  = "create view item_view as select name, title, link, pubDate from item"

connectDB  = connectSqlite3 "tweetlog.sqlite3db"
createTable conn = run conn tableSql []

initDB = do conn <- connectDB
            createTable conn
            commit conn

insertDB :: Connection -> String -> Item -> Res.Response -> IO Integer
insertDB conn name i res =  run conn "INSERT INTO item VALUES (?, ?, ?, ?, ?)" [toSql name, toSql (title i), toSql (link i), toSql (pubDate i), toSql (myshow res)]

selectDB :: IO [String]
selectDB = do conn <- connectDB
              (concatMap (map fromSql)) `fmap` quickQuery' conn "SELECT link from item_view" []

itemFilter2 :: [Item] -> IO [Item]
itemFilter2 is = do ls <- selectDB
                    return $ filter (\i -> not $ elem (link i) ls) is'
  where is'  = filter (not . isPrefixOf "<![CDATA[PR:" . title) is -- for ameba ad
        
        
logTweetResponse :: String -> Item -> Res.Response -> IO ()
logTweetResponse name i res = do conn <- connectDB
                                 insertDB conn name i res
                                 commit conn
