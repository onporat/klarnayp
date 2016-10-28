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
      render "index"
    Nothing -> render "index"

searchInputAttributeSplice :: AttrSplice (Handler App App)
searchInputAttributeSplice _ = do
  mContents <- lift $ use searchInputContents
  case mContents of
    Just contents -> return [("value", contents)]
    Nothing -> return []

memoiseInit :: SnapletInit App App
memoiseInit = makeSnaplet "app" "Klarna Yellow Pages exercise" Nothing $ do
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
  (_, site, _) <- runSnaplet Nothing memoiseInit
  quickHttpServe site
