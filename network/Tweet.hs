module Tweet where

import Data.Maybe (fromJust)
import qualified Network.OAuth.Consumer as C
import qualified Network.OAuth.Http.Request as Req
import qualified Network.OAuth.Http.Response as Res
import qualified Network.OAuth.Http.CurlHttpClient as Cli
import qualified Data.ByteString.Lazy.Char8 as BS

reqUrl = fromJust . Req.parseURL $ "https://api.twitter.com/oauth/request_token"
accUrl = fromJust . Req.parseURL $ "https://api.twitter.com/oauth/access_token"
tweetUrl = fromJust . Req.parseURL $ "http://api.twitter.com/1/statuses/update.xml"

authUrl :: C.Token -> String
authUrl = ("https://api.twitter.com/oauth/authorize?oauth_token=" ++)
          . Req.findWithDefault ("oauth_token","") . C.oauthParams

-- my secret information --
myApp = C.Application "dudJv93OsQHSqNGU7iWcug" "WFDaq9tLXtsmN8aCTwyNdvGukI69ppF5ZgBDc4GhaxU" C.OOB
params = [("oauth_token", "1047393704-4sRJK1oAHlLw9IRw3pbP1aOpHGGy25DJuldKSGI")
         ,("oauth_token_secret", "PaF3EF3er6TPqRRnWcQodzh83nRIiFSNxxBbhPxmD8s")]
myToken = C.AccessToken myApp (Req.fromList params)
------------------------

-- tweet from My Account
tweet :: String -> IO Res.Response
tweet  msg = tweetFromAccToken myToken msg
  
-- tweet from Acccess Token
tweetFromAccToken :: C.Token -> String -> IO Res.Response  
tweetFromAccToken tok message = C.runOAuthM tok $ do
  C.signRq2 C.HMACSHA1 Nothing request >>= C.serviceRequest Cli.CurlClient
  where request = tweetUrl {Req.method = Req.POST
                           ,Req.qString = Req.singleton ("status", message)}

-- get access token --
authenticate :: IO C.Token
authenticate = C.runOAuthM (C.fromApplication myApp) $ do
  C.signRq2 C.HMACSHA1 Nothing reqUrl >>= C.oauthRequest Cli.CurlClient
  C.cliAskAuthorization authUrl
  C.signRq2 C.HMACSHA1 Nothing accUrl >>= C.oauthRequest Cli.CurlClient


-- my show instance --
myshow :: Res.Response -> String
myshow Res.RspHttp{Res.status=i, Res.reason=rea, Res.rspHeaders=fls, Res.rspPayload=bs} =
  "OAuth-Response status: " ++ show i ++", "++"reason: "++ show rea ++ "\n"
  ++ "response-header:\n" ++ concatMap ((++"\n").show) (Req.toList fls)
  ++ BS.unpack bs
