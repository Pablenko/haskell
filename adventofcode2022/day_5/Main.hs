{-# LANGUAGE OverloadedStrings #-}

import Parsing (Parser, readAndParseInput, integerParser, lexeme)
import Text.Megaparsec (sepEndBy1, sepBy, sepBy1)
import Text.Megaparsec.Char (char, eol, string, letterChar, digitChar, hspace)
import RIO
import Control.Monad (void)
import Data.Map (Map, empty, insertWith)
import Data.Maybe (catMaybes)


type StackLine = [Maybe Char]
type StackLines = [StackLine]


data Move = Move {
    count :: Int,
    from :: Int,
    to :: Int
} deriving (Show)


data DataInput = DataInput
   {
        stackLines :: StackLines,
        indexes :: [Int],
        moves :: [Move]
    } deriving (Show) 


type CrateStack = Map Int [Char]


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


indexesParser :: Parser [Int]
indexesParser = sepBy1 integerParser hspace


moveParser :: Parser Move
moveParser = do
    string "move "
    moves <- integerParser
    string "from "
    fromCrate <- integerParser
    string "to "
    toCrate <- integerParser
    return $ Move moves fromCrate toCrate


movesParser :: Parser [Move]
movesParser = sepEndBy1 moveParser eol 


dataParser :: Parser DataInput
dataParser = DataInput <$> stackParser <*> indexesParser <* eol <* eol <*> movesParser


readInput :: (HasLogFunc env) => RIO env DataInput
readInput = readAndParseInput "day_5/input.txt" dataParser


firstTup :: (a, b) -> a
firstTup (a, b) = a


secondTup :: (a, b) -> b
secondTup (a, b) = b


buildCreateStack :: StackLines -> CrateStack
buildCreateStack slines = foldl (\stackMap elem -> insertWith (++) (firstTup elem) (catMaybes $ secondTup elem) stackMap) empty $ zip [1..] slines


solution :: DataInput -> CrateStack
solution input = buildCreateStack $ stackLines input


run :: (HasLogFunc env) => RIO env ()
run = readInput >>= logInfo . display . tshow . solution


main :: IO()
main = runSimpleApp run
