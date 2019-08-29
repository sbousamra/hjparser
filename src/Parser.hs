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

-- { "cock": "smith", 
--   "grin": "meat" 
-- }

dummy :: JSON
dummy = JObject [("cock", JString "smith"), ("grin", JString "meat")]

renderKeyValue :: (String, JSON) -> String
renderKeyValue (k, v) = "\"" ++ k ++ "\"" ++ ": " ++ render v

render :: JSON -> String
render v = case v of
  JString i -> "\""  ++ i ++ "\""
  JObject i -> "{ " ++ (intercalate ",\n" $ fmap renderKeyValue i) ++ " }"