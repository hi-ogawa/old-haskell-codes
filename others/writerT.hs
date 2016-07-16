import Control.Monad.Writer
import Data.Maybe
import System.Time

-- これはログエントリのフォーマットです
data Entry = Log {count::Int, msg::String} deriving (Eq,Show)

data Packet = Packet {sourceIP::Int, contents::String} deriving Show
data Rule =  Rule Judge Int deriving Show
data Judge = Accept | Reject deriving (Eq,Show)

-- メッセージをログに追加
logMsg :: String -> Writer [Entry] ()
logMsg s = tell [Log 1 s]

-- これは一つのパケットを扱います
filterOne :: [Rule] -> Packet -> Writer [Entry] (Maybe Packet)
filterOne rules packet = do rule <- return (match rules packet)
                            case rule of
                              Nothing  -> do logMsg ("DROPPING UNMATCHED PACKET: " ++ (show packet))
                                             return Nothing
                              (Just r) -> do (logMsg ("MATCH: " ++ (show r) ++ " <=> " ++ (show packet)))
                                             case r of
                                               (Rule Accept _) -> return (Just packet)
                                               (Rule Reject _) -> return Nothing
                                               
match [] _ = Nothing
match ((Rule judge ip):ls) packet | judge == Accept && ip == sourceIP packet    = Just $ Rule judge ip
                                  | judge == Reject && ip == sourceIP packet    = Just $ Rule judge ip
                                  | otherwise                                   = match ls packet

  
packets = [Packet 20 "mail1", Packet 15 "http1", Packet 10 "smtp1"]
rules = map (Rule Accept) [0..10]
        ++ map (Rule Reject) [20..30]

main = do let (filtered_packets, log) = runWriter $ mapM (filterOne rules) packets
          putStrLn $ "filter-log: " ++ (show log)
          putStrLn $ "filtered packets: " ++ (show $ catMaybes filtered_packets)
          return ()
          
main' = do (filtered_packets, log) <- runWriterT $ mapM (filterOne' rules) packets
           putStrLn $ "filter-log: " ++ (show log)
           putStrLn $ "filtered packets: " ++ (show $ catMaybes filtered_packets)
           return ()
          
          
data Entry' = Log' {timestamp::ClockTime, msg'::String} deriving (Show)

logMsg' :: String -> WriterT [Entry'] IO ()
logMsg' s = do time <- liftIO getClockTime
               tell [Log' time s]
                 
filterOne' :: [Rule] -> Packet -> WriterT [Entry'] IO (Maybe Packet)
filterOne' rules packet = do let rule = match rules packet
                             case rule of
                               Nothing  -> do logMsg' ("DROPPING UNMATCHED PACKET: " ++ (show packet))
                                              return Nothing
                               Just r   -> do logMsg' ("MATCH: " ++ (show r) ++ " <=> " ++ (show packet))
                                              case r of
                                                (Rule Accept _) -> return $ Just packet
                                                (Rule Reject _) -> return Nothing