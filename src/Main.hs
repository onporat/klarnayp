{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import Snap
import Snap.Snaplet.Heist
import Snap.Util.FileServe
import Control.Lens
import Data.Text
import Data.Text.Encoding
import Data.Monoid
import Heist
import Heist.Interpreted
import Control.Monad.Trans
import Solr

data App
  = App { _heist :: Snaplet (Heist App)
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
      searchInputContents .= (Just $ decodeUtf8 query)
      results <- liftIO $ searchPeople $ decodeUtf8 query
      renderWithSplices "index" ("people" ## personsSplice results)
    Nothing -> render "index"


searchInputAttributeSplice :: AttrSplice (Handler App App)
searchInputAttributeSplice _ = do
  mContents <- lift $ use searchInputContents
  case mContents of
    Just contents -> return [("value", contents)]
    Nothing -> return []


personSplice :: Monad m => Person -> Splices (HeistT n m Template)
personSplice person = do
  "name" ## textSplice (personName person)
  "avatar" ## textSplice (personAvatar person)
  "street" ## textSplice (personStreet person)
  "city" ## textSplice (personCity person)
  "country" ## textSplice (personCountry person)


personsSplice  :: [Person] -> Splice (Handler App App)
personsSplice = mapSplices (runChildrenWith . personSplice)


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
