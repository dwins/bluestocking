module Wikipedia where

import Text.Printf
import Network.HTTP
import Network.Stream

articleUrl :: String -> String -> String
articleUrl = printf "http://%s.wikipedia.org/w/index.php?action=raw&title=%s"

fetchArticle :: String -> String -> IO (Maybe String)
fetchArticle lang topic = do resp <- (simpleHTTP . getRequest) url
                             let body = asMaybe resp
                             resolved <- resolveWikiRedirect body
                             return body
    where url = articleUrl lang topic

wikiRedirect :: String -> Maybe String
wikiRedirect page = if "#redirect" `isPrefixOf` lpage 
                    then drop

resolveWikiRedirect :: Maybe String -> IO (Maybe String)
resolveWikiRedirect Nothing = return Nothing
resolveWikiRedirect page =

asMaybe :: Result (Response a) -> Maybe a
asMaybe = either (const Nothing)
                 (Just . rspBody)

main :: IO ()
main = do res <- fetchArticle "simple" "Article"
          print res
