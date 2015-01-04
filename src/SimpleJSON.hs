module SimpleJSON where

import Data.Maybe

data JValue = JString String
            | JNumber Double
            | JBoolean Bool
            | JNull
            | JArray [JValue]
            | JObject [(String, JValue)]
              deriving (Eq, Ord)  

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBoolean v) = Just v
getBool _         = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber v) = Just v
getDouble _           = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber v) = Just (floor v)
getInt _           = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject v) = Just v
getObject _           = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray v) = Just v
getArray _          = Nothing

isNull :: JValue -> Bool
isNull v = v == JNull

instance Show JValue where
  show (JString v) = show v
  show (JNumber v) = show v
  show (JNull) = "null"
  show (JArray v) = show v
  show (JBoolean v) | v == True = "true"
                    | v == False = "false"
  show (JObject v) =
    "{" ++ (helper v) ++ "}" where
      helper ((s,v):os) = show s ++ " = " ++ show v ++
                          if os == [] then ""
                          else "," ++ helper(os)
      helper _ = ""


-- Unit Test:
--   show $ JArray [JString "hekko", JNumber 23, JBoolean True, JObject [("name", JString "Phil"), ("age", JNumber 37)]]
--   Result: ["hekko",23.0,true,{"name" = "Phil","age" = 37.0}]      
