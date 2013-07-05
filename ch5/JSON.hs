module SimpleJSON
       (
         JValue (..)
       , getString
       , getInt
       , getDouble
       , getBool
       , getObject
       , getArray
       , isNull
       ) where

import Prettify

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
            deriving (Eq, Ord, Show)

getString (JString s) = Just s
getString _           = Nothing

getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool (JBool b) = Just b
getBool _         = Nothing

getObject (JObject o) = Just o
getObject _           = Nothing

getArray (JArray a) = Just a
getArray _          = Nothing

isNull v            = v == JNull

renderJValue :: JValue -> Doc
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "fase"
renderJValue JNull = text "null"
renderJValue (JNumber n) = double n
renderJValue (JString s) = string s
renderJValue (JArray array) = series '[' ']' renderJValue array
renderJValue (JObject obj) = series '{' '}' field obj
  where field (k, v) = string k
                       <> text ": "
                       <> renderJValue v
