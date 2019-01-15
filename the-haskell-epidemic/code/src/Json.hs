{-# LANGUAGE RecordWildCards #-}

module Json where

import Data.Functor                                  (($>))
import Data.HashMap.Lazy                             as HashMap
import Data.List                                     (intercalate)
import Test.QuickCheck
import Test.QuickCheck.Instances.UnorderedContainers ()
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token
import Text.ParserCombinators.Parsec.Number

data Json = JNull
          | JBool Bool
          | JString String
          | JNumber Double
          | JArray [Json]
          | JObject (HashMap String Json)
  deriving (Show, Eq)

jsonValue :: Parsec String () Json
jsonValue =
  whiteSpace >> choice [jstring, jnumber, jbool, jnull, jarray, jobject]
  where
    jnull = reserved "null" $> JNull
    jbool = reserved "true" $> JBool True <|> reserved "false" $> JBool False
    jstring = JString <$> stringLiteral
    jnumber = JNumber <$> numberLiteral
    jarray  = JArray  <$> brackets (commaSep jsonValue)
    jobject = JObject <$> HashMap.fromList <$> braces (commaSep keyValuePair)

    numberLiteral = lexeme (sign <*> (either fromIntegral id <$> naturalOrFloat))

    keyValuePair = do
      key <- stringLiteral
      colon
      value <- jsonValue
      return (key, value)

    TokenParser {..} = makeTokenParser emptyDef

render :: Json -> String
render json =
  case json of
    JNull -> "null"
    JBool True -> "true"
    JBool False -> "false"
    JString s -> show s
    JNumber n -> show n
    JArray xs -> "[ " ++ intercalate ", " (fmap render xs) ++ " ]"
    JObject kvs -> "{ " ++ intercalate ", " (fmap renderKV (HashMap.toList kvs)) ++ " }"
    where
      renderKV (k, v) = show k ++ " : " ++ render v

instance Arbitrary Json where
  arbitrary = sized genJson
     where
      jnull         = pure JNull
      jbool         = JBool   <$> arbitrary
      jstring       = JString <$> arbitrary
      jnumber       = JNumber <$> arbitrary
      jobject size  = JObject <$> resize (size `div` 2) arbitrary
      jarray size   = JArray  <$> resize (size `div` 2) arbitrary
      genJson size = 
        if size >= 0
          then oneof [jnull, jbool, jstring, jnumber, jobject size, jarray size]
          else oneof [jnull, jbool, jstring, jnumber]
