module Cashflow.Parser

where

import qualified Text.ParserCombinators.Parsec as P
import Control.Applicative 

import qualified Cashflow.Data as D

ws :: P.Parser String
ws = many (P.oneOf " \t,")

newLine :: P.Parser String
newLine = P.many1 (P.oneOf "\r\n#")

int ::  P.Parser Int
int = read <$> P.many1 P.digit

month :: P.Parser D.Month
month = fmap read . foldr1 (<|>) $ 
    map (P.try . P.string . show) [D.Jan ..] 

tentative :: P.Parser Bool
tentative = P.option False $ P.char '~' *> pure True

tentativeMonth :: P.Parser (Bool, D.Month)
tentativeMonth = P.option (True, D.Dec) $
                    (,) <$> tentative <*> month

lexeme :: P.Parser a -> P.Parser a
lexeme p = ws *> p <* ws

parseLine :: P.Parser a -> P.Parser a
parseLine p = p <* newLine 

description :: P.Parser String
description = ws *> many (P.noneOf "\n\r\t:#") <* P.char ':'

sectionName :: P.Parser String
sectionName = P.char '[' *> many (P.noneOf "\n\r\t[]#") <* P.char ']'

entry :: P.Parser D.Entry
entry = D.Entry <$> description <*> lexeme int

monthlyExpence :: P.Parser D.MonthlyExpence
monthlyExpence = D.MonthlyExpence <$> entry

income :: P.Parser D.Income
income = D.Income <$> entry

asset :: P.Parser D.Asset
asset = D.Asset <$> entry

projection :: P.Parser D.Projection
projection = D.Projection <$> entry <*> lexeme month

expence :: P.Parser D.Expence
expence = exp <$> entry <*> tentativeMonth
    where exp = \e (t, m) -> D.Expence e m t

debt :: P.Parser D.Debt
debt = D.Debt <$> entry <*> pure "creditor" <*> lexeme month <*> lexeme int

comment :: P.Parser String
comment = P.char '#' *> many (P.noneOf "\n\r") 

-- file = 
--
-- parseFile = P.parse file

