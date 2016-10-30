module Types where

import           Data.Text (Text)

data Person = Person
  { personName    :: !Text
  , personAvatar  :: !Text
  , personStreet  :: !Text
  , personCity    :: !Text
  , personCountry :: !Text
  }
  deriving (Show)


data QueryToken
  = Name !Text
  | Age  Integer
  | Phone !String
  | Unknown
  deriving (Show,Eq)
