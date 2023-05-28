{-# LANGUAGE OverloadedStrings #-}

import Parsing (Parser, readAndParseInput)
import Text.Megaparsec (sepEndBy1)
import Text.Megaparsec.Char (char, eol, string, letterChar)
import RIO


type StackLine = [Maybe Char]
type StackLines = [StackLine]


crateParser :: Parser (Maybe Char)
crateParser = crate <|> empty
    where
        crate = do
            char '['
            c <- letterChar
            char ']'
            return $ Just c
        empty = string "   " >> return Nothing


stackLineParser :: Parser StackLine
stackLineParser = sepEndBy1 crateParser (char ' ')


stackParser :: Parser StackLines
stackParser = sepEndBy1 stackLineParser eol


readInput :: (HasLogFunc env) => RIO env StackLines
readInput = readAndParseInput "day_5/input.txt" stackParser


solution :: StackLines -> StackLines
solution xs = xs


run :: (HasLogFunc env) => RIO env ()
run = readInput >>= logInfo . display . tshow . solution


main :: IO()
main = runSimpleApp run
