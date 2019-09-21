{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Char as Char
import Control.Monad
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import SExpr

type Parser = Parsec Void Text

{- Taken from https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html -}

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | 'integer' parses an integer.
integer :: Parser Integer
integer = lexeme L.decimal

{- End taken from -}

-- Taken from https://hackage.haskell.org/package/megaparsec-6.5.0/docs/Text-Megaparsec-Char-Lexer.html#v:charLiteral
stringLiteral :: Parser Text
stringLiteral = char '"' *> (Text.pack <$> (manyTill L.charLiteral (char '"')))

sExpr :: Parser SExpr
sExpr = optional sc *> sExpr'

sExpr' :: Parser SExpr
sExpr'
  =   Num <$> integer
  <|> String <$> stringLiteral
  <|> Symbol <$> sSymbol
  <|> List <$> between (symbol "(") (symbol ")") (many sExpr')
  <|> Vector <$> between (symbol "[") (symbol "]") (many sExpr')
  <|> Map <$> between (symbol "{") (symbol "}") (many sExpr')

sSymbol :: Parser Text
sSymbol = lexeme $ Text.pack <$> ((:) <$> satisfy isSymbolChar <*> many (satisfy isSymbolChar))

isSymbolChar :: Char -> Bool
isSymbolChar c = Text.any (==c) "~!@#$%^&*_+-=,./?:<>"
  || Char.isAlphaNum c
  || (c >= '\x1f000' && c <= '\x1ffff')
