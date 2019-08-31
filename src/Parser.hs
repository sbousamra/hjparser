module Parser where

import Data.List (intercalate)
import Control.Applicative
import Safe (readMay)

data JSON = JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JObject [(String, JSON)]
  | JArray [JSON]
  deriving (Eq, Show)

renderKeyValue :: (String, JSON) -> String
renderKeyValue (k, v) = "\"" ++ k ++ "\"" ++ ": " ++ render v

renderNull :: String
renderNull = "null"

renderBool :: JSON -> String
renderBool (JBool True) = "true"
renderBool (JBool False) = "false"

renderNumber :: JSON -> String
renderNumber (JNumber v) = show v

renderString :: JSON -> String
renderString (JString v) = "\"" ++ v ++ "\""

renderObject :: JSON -> String
renderObject (JObject v) = "{ " ++ (intercalate ",\n" $ fmap renderKeyValue v) ++ " }"

renderArray :: JSON -> String
renderArray (JArray v) = "[" ++ (intercalate ",\n" $ fmap render v) ++ "]"

render :: JSON -> String
render JNull = renderNull
render (JBool i) = renderBool $ JBool i
render (JNumber i) = renderNumber $ JNumber i
render (JString i) = renderString $ JString i
render (JObject i) = renderObject $ JObject i
render (JArray i) = renderArray $ JArray i

parseNull :: String -> Maybe JSON
parseNull "null" = Just JNull
parseNull _ = Nothing

parseBool :: String -> Maybe JSON
parseBool "true" = Just $ JBool True
parseBool "false" = Just $ JBool False
parseBool _ = Nothing

parseNumber :: String -> Maybe JSON
parseNumber i = fmap JNumber $ readMay i

-- TODO
parseString :: String -> Maybe JSON
parseString i = undefined

-- TODO
parseObject :: String -> Maybe JSON
parseObject i = undefined

-- TODO
parseArray :: String -> Maybe JSON
parseArray i = undefined

parse :: String -> Maybe JSON
parse i = parseNull i 
  <|> parseBool i 
  <|> parseNumber i 
  <|> parseString i 
  <|> parseObject i 
  <|> parseArray i