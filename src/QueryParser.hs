{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module QueryParser
  ( parseQuery
  ) where

import           Control.Applicative   (empty)
import           Control.Monad         (void)
import           Data.Text             (Text, pack)
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text
import           Types


sc :: Parser ()
sc = L.space (void spaceChar) (void (char '.')) empty


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


-- at leat two chars name
name :: Parser QueryToken
name = Name . pack <$> (lexeme ((:) <$> letterChar <*> some letterChar))


-- accept digits or dash, remove dashes
numString :: Parser String
numString = do
    nums <- some (digitChar <|> char '-')
    return (filter (/='-') nums)


number :: Parser QueryToken
number = lexeme (numString >>= check)
  where
    check x = if (length x) == 2 && (head x) /= '0'
                then return $ Age (read x)
                else if length x >= 9
                       then return $ Phone x
                       else return Unknown


unknown :: Parser QueryToken
unknown = do
  lexeme $ skipSome (noneOf " ")
  return $ Unknown


queryTokenParser :: Parser QueryToken
queryTokenParser =     try name
                   <|> try number
                   <|> unknown


queryParser :: Parser [QueryToken]
queryParser = do
  skipMany spaceChar
  many queryTokenParser


parseQuery :: Text -> Either ParseError [QueryToken]
parseQuery = parse queryParser ""

