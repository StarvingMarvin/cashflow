module Cashflow.Parser (
    P.ParseError
    ,D.Entries
    ,parseFile
    ,parse
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

string :: PS.Parser String
string = many $ P.noneOf "\n\r\t:#[]()"

description :: PS.Parser String
description = string <* P.char ':'

entry :: PS.Parser D.Entry
entry = D.Entry <$> lexeme description <*> lexeme int

collapse :: (Functor f, Monoid a) => f [a] -> f a
collapse = fmap mconcat

parseWithHeading :: PS.Parser a -> (a -> PS.Parser b) -> PS.Parser [b]
parseWithHeading heading values = do
        h <- (parseLine heading)
        many $ parseLine $ values h

sectionHeading :: String -> PS.Parser String
sectionHeading name = P.try $ P.between (P.char '[') (P.char ']') (P.string name)

parseSection :: (Monoid a) => String -> PS.Parser a -> PS.Parser a
parseSection name = collapse . parseWithHeading (sectionHeading name) . const

groupHeading :: PS.Parser String
groupHeading = P.between (P.char '(') (P.char ')') string

parseGroup :: (Monoid a) => (String -> PS.Parser a) -> PS.Parser a
parseGroup = collapse . (parseWithHeading groupHeading)

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

debt :: String -> PS.Parser D.Entries
debt creditor = D.fromDebt <$> (D.Debt <$> entry <*> pure creditor 
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

debtGroup :: PS.Parser D.Entries
debtGroup = parseGroup debt

debts :: PS.Parser D.Entries
debts = parseSection "debt" debtGroup

comment :: PS.Parser String
comment = P.char '#' *> many (P.noneOf "\n\r") <* newLine

file :: PS.Parser D.Entries
file = collapse $ (many section) <* P.eof
    where   section = expences <|> monthlyExpences <|> incomes 
                    <|> debts <|> assets <|> projections

parseFile :: String -> IO (Either P.ParseError D.Entries)
parseFile = PS.parseFromFile file

parse :: String -> Either P.ParseError D.Entries
parse s = P.parse file "" s

