{-# LANGUAGE OverloadedStrings #-}

module Solr
  ( searchPeople
  ) where

import           Control.Lens
import           Data.Aeson            (Value)
import           Data.Aeson.Lens       (key, _Array, _String)
import           Data.Text             (Text, append, empty, intercalate, pack)
import           Data.Time.Calendar    (Day, addGregorianYearsClip)
import           Data.Time.Clock       (UTCTime (..), getCurrentTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Network.Wreq          (Options, asValue, defaults, getWith,
                                        param, responseBody)
import           QueryParser
import           Text.Printf           (printf)
import           Types



solrPeopleUrl :: String
solrPeopleUrl = "http://localhost:8983/solr/people/select"


getSearchParams :: Text -> Options
getSearchParams searchOpts = defaults
  & param "wt" .~ ["json"]
  & param "start" .~ ["0"]
  & param "rows" .~ ["10"]
  & param "q" .~ [searchOpts]


-- | Extract our typed data model from an untyped JSON object.
jsonToPerson :: Value -> Person
jsonToPerson json =
     Person { personName = view (key "name" . _String) json
            , personAvatar = view (key "avatar_origin" . _String) json
            , personStreet = view (key "address.street" . _String) json
            , personCity = view (key "address.city" . _String) json
            , personCountry = view (key "address.country" . _String) json
            }

searchPeople :: Text -> IO [Person]
searchPeople searchQuery = do
  -- Go out to Web to receive a lazy ByteString.
  UTCTime today _ <- getCurrentTime

  case parseQuery searchQuery of
    Left err -> return []
    Right searchTokens -> do

      let searchOpts = intercalate " AND " $ map (toSolrQuery today) $ filter (/= Unknown) searchTokens

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

toSolrQuery :: Day -> QueryToken -> Text
toSolrQuery _ (Name n)  = "name:" `append` n
toSolrQuery _ (Phone p) = pack (printf "phone:%s-%s" l r :: String)
  where
    (l,r) = splitAt 4 p
toSolrQuery t (Age a) = pack $ printf "birthday:[%d TO %d]" from to
  where
    from = utcTimeToSec $ ageToUTCTime t a
    to   = utcTimeToSec $ ageToUTCTime t (a - 1)
toSolrQuery _ Unknown = empty


utcTimeToSec :: UTCTime -> Integer
utcTimeToSec u = floor $ utcTimeToPOSIXSeconds u :: Integer

ageToUTCTime :: Day -> Integer -> UTCTime
ageToUTCTime t a = UTCTime (addGregorianYearsClip (negate a) t) 0
