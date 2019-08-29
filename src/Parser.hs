module Parser where

import Data.List (intercalate)

data JSON = JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JObject [(String, JSON)]
  | JArray [JSON]
  deriving (Eq, Show)

parse :: String -> Maybe JSON
parse v = Just (JString v)

dummy :: JSON
dummy = JObject [("dummy", JString "json"), ("key", JString "value")]

renderKeyValue :: (String, JSON) -> String
renderKeyValue (k, v) = "\"" ++ k ++ "\"" ++ ": " ++ render v

render :: JSON -> String
render v = case v of
  JString i -> "\"" ++ i ++ "\""
  JNumber i -> show i
  JBool True -> "true" 
  JBool False -> "false"
  JNull -> "null"
  JObject i -> "{ " ++ (intercalate ",\n" $ fmap renderKeyValue i) ++ " }"
  JArray i -> "[" ++ (intercalate ",\n" $ fmap render i) ++ "]"