module Cashflow.Parser

where

import qualified Text.ParserCombinators.Parsec as P
import Control.Applicative 

import qualified Cashflow.Data as D

ws :: P.Parser String
ws = many (P.oneOf " \t")

int ::  P.Parser Int
int = read <$> P.many1 P.digit

lexeme :: P.Parser a -> P.Parser a
lexeme p = ws *> p <* ws

description :: P.Parser String
description = ws *> many (P.noneOf "\n\r\t:") <* P.char ':'

section :: P.Parser String
section = P.char '[' *> many (P.noneOf "\n\r\t[]") <* P.char ']'

entry :: P.Parser D.Entry
entry = do
    desc <- description
    ammount <- lexeme int
    return D.Entry{D.entryDescription=desc, D.entryAmmount=ammount}

