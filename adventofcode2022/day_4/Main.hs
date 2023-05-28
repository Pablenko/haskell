import RIO
import qualified RIO.Text as Text
import Text.Megaparsec (parse, sepEndBy1)
import Text.Megaparsec.Char (hspace, char, eol)
import Parsing (Parser, readAndParseInput, lexeme, integerParser)


data Pair = Pair Int Int deriving (Show)
data Line = Line Pair Pair deriving (Show)


firstP :: Pair -> Int
firstP (Pair a b) = a


secondP :: Pair -> Int
secondP (Pair a b) = b


makePairs :: Int -> Int -> Int -> Int -> Line
makePairs x1 y1 x2 y2 = Line (Pair x1 y1) (Pair x2 y2)


lineParser :: Parser Line
lineParser = makePairs <$> integerParser <* lexeme (char '-') <*> integerParser
                       <*  lexeme (char ',')
                       <*> integerParser <* lexeme (char '-') <*> integerParser


readInput :: (HasLogFunc env) => RIO env [Line]
readInput = readAndParseInput "day_4/input.txt" (sepEndBy1 lineParser eol)


overlapping :: Line -> Int
overlapping (Line p1 p2)
    | firstP p1 >= firstP p2 && secondP p1 <= secondP p2 = 1
    | firstP p2 >= firstP p1 && secondP p2 <= secondP p1 = 1
    | otherwise = 0


solution :: [Line] -> Int
solution = sum . map overlapping


run :: (HasLogFunc env) => RIO env ()
run = readInput >>= logInfo . display . tshow . solution


main :: IO()
main = runSimpleApp run
