module Wikipedia where

import Data.Array
-- import Data.Maybe
import Network.HTTP
import Network.Stream
import Text.Printf
import Text.Regex.PCRE

asMaybe :: Result (Response a) -> Maybe a
asMaybe = either (const Nothing)
                 (Just . rspBody)

articleUrl :: String -> String -> String
articleUrl = printf "http://%s.wikipedia.org/w/index.php?action=raw&title=%s"

redirectPattern :: Regex
redirectPattern = makeRegex "(?i)#REDIRECT \\[\\[([^[\\]]+)\\]\\]"

redirectTopic :: String -> Maybe String
redirectTopic page = fmap capture $ matchOnceText redirectPattern page
    where capture (_, matches, _) = fst $ matches ! 1

fetchArticle :: String -> String -> IO (Maybe String)
fetchArticle lang topic =
    do resp <- (simpleHTTP . getRequest) $ articleUrl lang topic
       let page = asMaybe resp
       case page >>= redirectTopic of 
           Just topic' -> fetchArticle lang topic'
           Nothing -> return page

main :: IO ()
main = do res <- fetchArticle "simple" "..."
          case res of
              Nothing -> return ()
              Just page -> putStrLn page
