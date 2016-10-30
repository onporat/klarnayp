{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module QueryParser
  ( parseQuery
  , parseQueryTest
  ) where

import Control.Applicative (empty)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
import Data.Text (Text, pack)
import Types


sc :: Parser ()
sc = L.space (void spaceChar) empty empty


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- at leat two chars name
name :: Parser QueryToken
name = Name . pack <$> (lexeme ((:) <$> letterChar <*> some letterChar))

number :: Parser QueryToken
number = lexeme (p >>= check)
  where
    p       = (:) <$> numberChar <*> some numberChar
    check x = if (length x) == 2 && (head x) /= '0'
                then return $ Age (read x)
                else if length x == 9
                       then return $ Phone x
                       else return Unknown


unknown :: Parser QueryToken
unknown = do
  (skipSome (noneOf " "))
  return $ Unknown


queryTokenParser :: Parser QueryToken
queryTokenParser =     try name
                   <|> try number
                   <|> unknown


queryParser :: Parser [QueryToken]
queryParser = do
  skipMany spaceChar
  many queryTokenParser

parseQuery = parse queryParser ""

parseQueryTest = parseTest queryParser
