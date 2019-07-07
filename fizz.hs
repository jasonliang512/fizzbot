{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens                   ( (^.) )
import           Data.Aeson                     ( FromJSON
                                                , withObject
                                                , object
                                                , parseJSON
                                                , (.:)
                                                , (.=)
                                                )
import           Data.Aeson.Lens                ( key
                                                , _String
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as Text
import           Network.Wreq                   ( Response
                                                , get
                                                , responseBody
                                                , post
                                                , asJSON
                                                )


-- DATA STRUCTURES ---------------------------------------------------


data FizzRule = FizzRule Int String deriving (Show)

instance FromJSON FizzRule where
  parseJSON = withObject "Rule" $ \o -> FizzRule
    <$> o .: "number"
    <*> o .: "response"


data Question = Question [FizzRule] [Int] deriving (Show)

instance FromJSON Question where
  parseJSON = withObject "Question" $ \o -> Question
    <$> o .: "rules"
    <*> o .: "numbers"


-- FIZZBUZZ ----------------------------------------------------------


fizzbuzz :: [FizzRule] -> Int -> String
fizzbuzz rules x = fromMaybe (show x) $ mconcat $ map dorule rules
 where
  dorule (FizzRule n str) | x `mod` n == 0 = Just str
                          | otherwise      = Nothing

solveFizzbuzz :: Question -> String
solveFizzbuzz (Question rules xs) = unwords $ map (fizzbuzz rules) xs


-- API INTERACTION ---------------------------------------------------


apiHost :: String
apiHost = "https://api.noopschallenge.com"

postAnswer :: String -> String -> IO (Response ByteString)
postAnswer question answer =
  post (apiHost ++ question) $ object ["answer" .= answer]

doQuestion :: String -> IO ()
doQuestion question = do
  g <- get (apiHost ++ question) >>= asJSON :: IO (Response Question)
  let body   = g ^. responseBody
  let answer = solveFizzbuzz body
  print body
  print answer
  p <- postAnswer question answer
  case p ^. responseBody . key "result" . _String of
    "correct" ->
      doQuestion
        $  Text.unpack
        $  p
        ^. responseBody
        .  key "nextQuestion"
        .  _String
    _ -> print $ p ^. responseBody

main :: IO ()
main = do
  res <- postAnswer "/fizzbot/questions/1" "Haskell!"
  doQuestion $ Text.unpack $ res ^. responseBody . key "nextQuestion" . _String
