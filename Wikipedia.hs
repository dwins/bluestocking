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

breakOn :: String -> String -> [String]
breakOn pattern text =
   case matchOnce (makeRegex pattern :: Regex) text of
       Nothing -> [text]
       Just matches ->
           let (offset, len) = matches ! 0
               (h, t) = splitAt offset text
           in h : (breakOn pattern . drop len) t

capture :: String -> Int -> String -> String
capture pattern n text = foldr id text transforms
   where transforms = map toTransform $ matchAllText (makeRegex pattern :: Regex) text
         toTransform ms = matchTransform (ms ! 0) (ms ! n)
         matchTransform (_, (offset, len)) (sub, _) = replaceRange offset len sub

subst :: String -> String -> String -> String
subst pattern sub text = foldr id text transforms
   where transforms = map toTransform $ matchAll (makeRegex pattern :: Regex) text
         toTransform = matchTransform . (! 0)
         matchTransform (offset, len) = replaceRange offset len sub

remove :: String -> String -> String
remove pattern text = foldr id text transforms
   where transforms = map toTransform $ matchAll (makeRegex pattern :: Regex) text
         toTransform = matchTransform . (! 0)
         matchTransform (offset, len) = removeRange offset len

removeRange :: Int -> Int -> String -> String
removeRange i n text = h ++ drop n t
    where (h, t) = splitAt i text

replaceRange :: Int -> Int -> String -> String -> String
replaceRange i n sub text = h ++ sub ++ drop n t
    where (h, t) = splitAt i text

redirectPattern :: Regex
redirectPattern = makeRegex "(?i)#REDIRECT \\[\\[([^[\\]]+)\\]\\]"

redirectTopic :: String -> Maybe String
redirectTopic page = fmap getCapture $ matchOnceText redirectPattern page
    where getCapture (_, matches, _) = fst $ matches ! 1

fetchArticle :: String -> String -> IO (Maybe String)
fetchArticle lang topic =
    do resp <- (simpleHTTP . getRequest) $ articleUrl lang topic
       let page = asMaybe resp
       case page >>= redirectTopic of 
           Just topic' -> fetchArticle lang topic'
           Nothing -> return page

unhtml, unwiki :: String -> String
unhtml = subst "(?i)&nbsp;" " " .
         subst "(?i)<br[ \\\\]*?>" "\n" .
         subst "(?i)&amp;" "&" .
         remove "(?m)<!--.*?--\\s*>" .
         remove "(?i)<ref[^>]*>[^>]*<\\/ ?ref>" .
         remove "(?m)<.*?>"

unwiki = capture "(?i)\\{\\{IPA(?:\\-[^|{}]+)*?\\|([^|{}]+)(\\|[^{}]+)*?\\}\\}" 1 .
         capture "(?i)\\{\\{Lang(\\-[^|{}]+)*?\\|([^|{}]+)(\\|[^{}]+)*?\\}\\}" 1 .
         capture "\\[(.*?)\\|.*?\\]" 1 .
         capture "\\[\\[[^[\\]]*?\\|([^[\\]]*?)\\]\\]" 1 .
         capture "\\[\\[([^[\\]]+?)\\]\\]" 1 .
         capture "\\[[^[\\]]*? ([^[\\]]*?)\\]" 1 .
         remove "(?m)\\{\\{[^{}]+\\}\\}" .
         remove "(?i)\\[\\[(?:Category|Image|File):[^[\\]]*?\\]\\]" .
         remove "''+" .
         remove "(?m)^\\*$"

paragraphs :: String -> [String]
paragraphs = breakOn "\\n\\n+" . cleanWhitespace 
    where cleanWhitespace = subst "\\r\\n|\\n|\\r" "\n"

main :: IO ()
main = do res <- fetchArticle "simple" "..."
          case res of
              Nothing -> return ()
              Just page -> putStrLn $ head . paragraphs . unhtml . unwiki $ page
