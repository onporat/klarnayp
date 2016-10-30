module Types where

import           Data.Text (Text)

data Person = Person
  { personName     :: !Text
  , personPhone    :: !Text
  , personBirthday :: Integer
  , personAvatar   :: !Text
  , personStreet   :: !Text
  , personCity     :: !Text
  , personCountry  :: !Text
  }
  deriving (Show)


data QueryToken
  = Name !Text
  | Age  Integer
  | Phone !String
  | Unknown
  deriving (Show,Eq)
