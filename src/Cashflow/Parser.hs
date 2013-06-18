module Cashflow.Parser {- (
    parseFile
) -} where

import qualified Text.ParserCombinators.Parsec as P
import Control.Applicative 
import Data.Monoid

import qualified Cashflow.Data as D

ws :: P.Parser String
ws = many (P.oneOf " \t,")

newLine :: P.Parser String
newLine = P.many1 (P.oneOf "\r\n")

(<||>) :: P.Parser a -> P.Parser a -> P.Parser a
p <||> q = P.try p <|> q

lexeme :: P.Parser a -> P.Parser a
lexeme p = p <* ws

emptyLines :: P.Parser [String]
emptyLines = many ((P.many1 $ P.oneOf " \r\n") <|> comment)

parseLine :: P.Parser a -> P.Parser a
parseLine p = {- emptyLines *> -} lexeme p <* emptyLines --(newLine <|> comment)

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

description :: P.Parser String
description = many (P.noneOf "\n\r\t:#[]") <* P.char ':'

sectionName :: P.Parser String
sectionName = P.char '[' *> many (P.noneOf "\n\r\t[]#") <* P.char ']'

entry :: P.Parser D.Entry
entry = D.Entry <$> lexeme description <*> lexeme int

parseSection :: (Monoid a) => String -> P.Parser a -> P.Parser a
parseSection name p = (parseLine $ P.string $ "[" ++ name ++ "]")
                        *> (transform <$> parser)
    where   parser = P.many $ parseLine p
            transform = foldr1 mappend

monthlyExpence :: P.Parser D.Entries
monthlyExpence = D.fromMonthlyExpence . D.MonthlyExpence <$> entry

income :: P.Parser D.Entries
income = D.fromIncome . D.Income <$> entry

asset :: P.Parser D.Entries
asset = D.fromAsset . D.Asset <$> entry

projection :: P.Parser D.Entries
projection = D.fromProjection <$> (D.Projection <$> entry 
                                                <*> lexeme month)

expence :: P.Parser D.Entries
expence = D.fromExpence <$> (exp <$> entry <*> tentativeMonth)
    where exp = \e (t, m) -> D.Expence e m t

debt :: P.Parser D.Entries
debt = D.fromDebt <$> (D.Debt <$> entry <*> pure "creditor" 
                              <*> lexeme month <*> lexeme int)

monthlyExpences :: P.Parser D.Entries
monthlyExpences = parseSection "monthly expences" monthlyExpence

incomes :: P.Parser D.Entries
incomes = parseSection "income" income

assets :: P.Parser D.Entries
assets = parseSection "assets" asset

projections :: P.Parser D.Entries
projections = parseSection "projections" projection

expences :: P.Parser D.Entries
expences = parseSection "expences" expence

debts :: P.Parser D.Entries
debts = parseSection "debt" debt

comment :: P.Parser String
comment = P.char '#' *> many (P.noneOf "\n\r") <* newLine

{- fmap flat $ (many section) -}
file :: P.Parser [D.Entries]
file = (sequence sections) <* P.eof
    where   --flat = foldr1 mappend
            section = expences <||> monthlyExpences <||> incomes 
                    <||> debts <||> assets <||> projections
            sections = [expences, monthlyExpences, debts, 
                        incomes, assets, projections]
--parseFile :: String -> IO (Either P.ParseError D.Entries)
--parseFile = P.parseFromFile file

