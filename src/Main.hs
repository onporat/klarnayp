{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import           Data.Time.Calendar    (Day, diffDays)
import           Data.Time.Clock       (UTCTime (..), getCurrentTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Control.Lens
import           Control.Monad.Trans
import           Data.Monoid
import           Data.Text
import           Data.Text.Encoding
import           Heist
import           Heist.Interpreted
import           Snap
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
import           Solr
import           Text.XmlHtml        hiding (render)
import           Types

data App
  = App { _heist               :: Snaplet (Heist App)
        , _searchInputContents :: Maybe Text
        }
makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist


indexHandler :: Handler App App ()
indexHandler = do
  mQuery <- getParam "q"
  case mQuery of
    Just query -> do
      UTCTime today _ <- liftIO getCurrentTime
      searchInputContents .= (Just $ decodeUtf8 query)
      results <- liftIO $ searchPeople $ decodeUtf8 query
      renderWithSplices "index" ("people" ## personsSplice today results)
    Nothing ->
      renderWithSplices "index" ("people" ## return [])


searchInputAttributeSplice :: AttrSplice (Handler App App)
searchInputAttributeSplice _ = do
  mContents <- lift $ use searchInputContents
  case mContents of
    Just contents -> return [("value", contents)]
    Nothing -> return []


personSplice :: Monad m => Day -> Person -> Splices (HeistT n m Template)
personSplice today person = do
  let UTCTime bday _ = posixSecondsToUTCTime (fromInteger $ personBirthday person)
  let age            = diffDays today bday `div` 365
  "name" ## textSplice (personName person)
  "phone" ## textSplice (personPhone person)
  "age" ## textSplice (pack $ show $ age)
  "avatar" ## textSplice (personAvatar person)
  "street" ## textSplice (personStreet person)
  "city" ## textSplice (personCity person)
  "country" ## textSplice (personCountry person)


personsSplice  :: Day -> [Person] -> Splice (Handler App App)
personsSplice _ [] = return [TextNode "No results, please review your search or try a different one"]
personsSplice today p  = mapSplices (runChildrenWith . personSplice today) p


appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Klarna Yellow Pages exercise" Nothing $ do
  h <- nestSnaplet "heist" heist $ heistInit "templates"
  modifyHeistState $ bindAttributeSplices ("search-input" ## searchInputAttributeSplice)
  addRoutes [ ("static", serveDirectory "static")
            , ("", indexHandler)
            ]
  return $ App { _heist = h
               , _searchInputContents = Nothing
               }

main :: IO ()
main = do
  (_, site, _) <- runSnaplet Nothing appInit
  quickHttpServe site
