module Parsing (readAndParseInput, Parser, lexeme, integerParser) where

import RIO
import Text.Megaparsec (parse, Parsec)
import Text.Megaparsec.Char (hspace, char)
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text


readAndParseInput :: (MonadReader env m, MonadIO m, HasLogFunc env) => FilePath -> Parser a -> m a
readAndParseInput filePath parser = do
    x <- parse parser filePath <$> readFileUtf8 filePath
    either (\e -> do { logError . display . tshow $ "Wrong file format, parsing error!"; exitFailure }) return x


lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace


integerParser :: Parser Int
integerParser = lexeme L.decimal
