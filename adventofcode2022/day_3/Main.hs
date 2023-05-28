import Data.List (intersect)
import Data.Char (ord)
import RIO
import qualified RIO.Text as Text


readInput :: MonadIO m => m [Text]
readInput = do Text.lines <$> readFileUtf8 "input.txt"


calculatePriority :: Char -> Int
calculatePriority x
    | 'a' <= x && x <= 'z' = ord x - ord 'a' + 1
    | otherwise = ord x - ord 'A' + 27


extractLetter :: Text -> Char
extractLetter t = head $ intersect (Text.unpack firstPart) (Text.unpack secondPart)
    where (firstPart, secondPart) = Text.splitAt middle t
          middle = Text.length t `div` 2


solution :: [Text] -> Int
solution xs = sum $ map (calculatePriority . extractLetter) xs


run :: (HasLogFunc env) => RIO env ()
run = readInput >>= logInfo . display . solution


main :: IO()
main = runSimpleApp run
