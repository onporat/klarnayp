{-# LANGUAGE OverloadedStrings #-}

module Solr where
--  ( searchPeople
--  , Person()
--  ) where

import           Control.Lens
import           Data.Aeson      (Value)
import           Data.Aeson.Lens (key, _Array, _String)
import           Data.Text       (Text, append, pack)
import           Network.Wreq    (Options, asValue, defaults, getWith, param,
                                  responseBody)

data Person = Person
  { personName    :: !Text
  , personAvatar  :: !Text
  , personStreet  :: !Text
  , personCity    :: !Text
  , personCountry :: !Text
  }
  deriving (Show)


type SearchOptions = Text

solrPeopleUrl :: String
solrPeopleUrl = "http://localhost:8983/solr/people/select"

getSearchParams :: SearchOptions -> Options
getSearchParams searchOpts = defaults
  & param "wt" .~ ["json"]
  & param "start" .~ ["0"]
  & param "rows" .~ ["10"]
  & param "q" .~ ["name:" `append` searchOpts]


-- | Extract our typed data model from an untyped JSON object.
jsonToPerson :: Value -> Person
jsonToPerson json =
     Person { personName = view (key "name" . _String) json
            , personAvatar = view (key "avatar_origin" . _String) json
            , personStreet = view (key "address.street" . _String) json
            , personCity = view (key "address.city" . _String) json
            , personCountry = view (key "address.country" . _String) json
            }

searchPeople :: SearchOptions -> IO [Person]
searchPeople searchOpts = do
  -- Go out to Web to receive a lazy ByteString.
  response <- getWith (getSearchParams searchOpts) solrPeopleUrl

  -- Parse the ByteString response, including headers and body,
  -- into an untyped JSON object.
  jsonResponse <- asValue response

  -- Extract the list of people
  let foundPeople = toListOf ( responseBody
                             . key "response"
                             . key "docs"
                             . _Array
                             . traverse
                             ) jsonResponse

  -- For each event, extract its name and the name of its venue.
  return (map jsonToPerson foundPeople)

