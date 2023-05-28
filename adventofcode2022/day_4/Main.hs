import RIO
import qualified RIO.Text as Text
import Text.Megaparsec (Parsec, parse, sepEndBy1)
import Text.Megaparsec.Char (hspace, char, eol)
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text
data Pair = Pair Int Int deriving (Show)
data Line = Line Pair Pair deriving (Show)


readAndParseInput :: (MonadReader env m, MonadIO m, HasLogFunc env) => FilePath -> Parser a -> m a
readAndParseInput filePath parser = do
    x <- parse parser filePath <$> readFileUtf8 filePath
    either (\e -> do { logError . display . tshow $ "Wrong file format, parsing error!"; exitFailure }) return x


lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace


integerParser :: Parser Int
integerParser = do
    sign_ <- maybe 1 (const (-1)) <$> optional (char '-')
    abs_  <- lexeme L.decimal
    return (sign_ * abs_)


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
readInput = readAndParseInput "input.txt" (sepEndBy1 lineParser eol)


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
