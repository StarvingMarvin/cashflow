module Cashflow.Parser (
    P.ParseError,
    D.Entries,
    parseFile
) where

import qualified Text.Parsec as P
import qualified Text.Parsec.String as PS
import Control.Applicative 
import Data.Monoid

import qualified Cashflow.Data as D

ws :: PS.Parser String
ws = many (P.oneOf " \t,")

newLine :: PS.Parser String
newLine = P.many1 (P.oneOf "\r\n")

(<||>) :: PS.Parser a -> PS.Parser a -> PS.Parser a
p <||> q = P.try p <|> q

lexeme :: PS.Parser a -> PS.Parser a
lexeme p = p <* ws

emptyLines :: PS.Parser [String]
emptyLines = many ((P.many1 $ P.oneOf " \r\n") <|> comment)

parseLine :: PS.Parser a -> PS.Parser a
parseLine p = lexeme p <* emptyLines

int ::  PS.Parser Int
int = read <$> P.many1 P.digit

month :: PS.Parser D.Month
month = fmap read . foldr1 (<|>) $ 
    map (P.try . P.string . show) [D.Jan ..] 

tentative :: PS.Parser Bool
tentative = P.option False $ P.char '~' *> pure True

tentativeMonth :: PS.Parser (Bool, D.Month)
tentativeMonth = P.option (True, D.Dec) $
                    (,) <$> tentative <*> month

description :: PS.Parser String
description = many (P.noneOf "\n\r\t:#[]") <* P.char ':'

sectionName :: PS.Parser String
sectionName = P.char '[' *> many (P.noneOf "\n\r\t[]#") <* P.char ']'

entry :: PS.Parser D.Entry
entry = D.Entry <$> lexeme description <*> lexeme int

parseSection :: (Monoid a) => String -> PS.Parser a -> PS.Parser a
parseSection name p = (parseLine $ P.string $ "[" ++ name ++ "]")
                        *> (transform <$> parser)
    where   parser = many $ parseLine p
            transform = foldr1 mappend

monthlyExpence :: PS.Parser D.Entries
monthlyExpence = D.fromMonthlyExpence . D.MonthlyExpence <$> entry

income :: PS.Parser D.Entries
income = D.fromIncome . D.Income <$> entry

asset :: PS.Parser D.Entries
asset = D.fromAsset . D.Asset <$> entry

projection :: PS.Parser D.Entries
projection = D.fromProjection <$> (D.Projection <$> entry 
                                                <*> lexeme month)

expence :: PS.Parser D.Entries
expence = D.fromExpence <$> (exp <$> entry <*> tentativeMonth)
    where exp = \e (t, m) -> D.Expence e m t

debt :: PS.Parser D.Entries
debt = D.fromDebt <$> (D.Debt <$> entry <*> pure "creditor" 
                              <*> lexeme month <*> lexeme int)

monthlyExpences :: PS.Parser D.Entries
monthlyExpences = parseSection "monthly expences" monthlyExpence

incomes :: PS.Parser D.Entries
incomes = parseSection "income" income

assets :: PS.Parser D.Entries
assets = parseSection "assets" asset

projections :: PS.Parser D.Entries
projections = parseSection "projections" projection

expences :: PS.Parser D.Entries
expences = parseSection "expences" expence

debts :: PS.Parser D.Entries
debts = parseSection "debt" debt

comment :: PS.Parser String
comment = P.char '#' *> many (P.noneOf "\n\r") <* newLine

file :: PS.Parser D.Entries
file = fmap flat $ (many section) <* P.eof
    where   flat = foldr1 mappend
            section = expences <||> monthlyExpences <||> incomes 
                    <||> debts <||> assets <||> projections

parseFile :: String -> IO (Either P.ParseError D.Entries)
parseFile = PS.parseFromFile file

