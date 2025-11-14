{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Makefile.Parse.Internal where

import Control.Monad
import Data.Foldable
import           Data.Attoparsec.Text
import           Data.Makefile
import Control.Applicative

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- $setup
-- >>> :set -XOverloadedStrings

-- | Parse makefile.
--
-- Tries to open and parse a file name @Makefile@ in the current directory.
parseMakefile :: IO (Either String Makefile)
parseMakefile = Atto.parseOnly makefile <$> T.readFile "Makefile"

-- | Parse the specified file as a makefile.
parseAsMakefile :: FilePath -> IO (Either String Makefile)
parseAsMakefile f = Atto.parseOnly makefile <$> T.readFile f

parseMakefileContents :: T.Text -> Either String Makefile
parseMakefileContents = Atto.parseOnly makefile

-- | Similar to 'Atto.parseOnly' but fails if all input has not been consumed.
parseAll :: Parser a -> T.Text -> Either String a
parseAll p = Atto.parseOnly (p <* Atto.endOfInput)
--------------------------------------------------------------------------------
-- Parsers


-- | Parser for a makefile
makefile :: Parser Makefile
makefile = Makefile <$> many' entry

-- | Parser for a makefile entry (either a rule or a variable assignment)
entry :: Parser Entry
entry = assignment <|> rule <|> otherLine

-- | Parser of variable assignment (see 'Assignment'). Note that leading and
-- trailing whitespaces will be stripped both from the variable name and
-- assigned value.
--
-- Note that this tries to follow GNU make's (crazy) behavior when it comes to
-- variable names and assignment operators.
--
-- >>> parseAll assignment "foo = bar "
-- Right (Assignment RecursiveAssign "foo" "bar")
--
-- >>> parseAll assignment "foo := bar "
-- Right (Assignment SimpleAssign "foo" "bar")
--
-- >>> parseAll assignment "foo ::= bar "
-- Right (Assignment SimplePosixAssign "foo" "bar")
--
-- >>> parseAll assignment "foo?= bar "
-- Right (Assignment ConditionalAssign "foo" "bar")
--
-- >>> parseAll assignment "foo??= bar "
-- Right (Assignment ConditionalAssign "foo?" "bar")
--
-- >>> parseAll assignment "foo!?!= bar "
-- Right (Assignment ShellAssign "foo!?" "bar")
assignment :: Parser Entry
assignment = do
  varName <- variableName
  assType <- assignmentType
  varVal <- toEscapedLineEnd
  return (Assignment assType varName varVal)

-- | Read chars while some ('Parser', monadic) predicate is 'True'.
--
-- XXX: extremely inefficient.
takeWhileM :: (Char -> Parser Bool) -> Parser T.Text
takeWhileM a = (T.pack . reverse) <$> go []
  where
    go cs = do
      c <- Atto.anyChar
      True <- a c
      go (c:cs) <|> pure (c:cs)


-- | Parse a variable name, not consuming any of the assignment operator. See
-- also 'assignment'.
--
-- >>> Atto.parseOnly variableName "foo!?!= bar "
-- Right "foo!?"
variableName :: Parser T.Text
variableName = stripped $ takeWhileM go
  where
    go '+' = Atto.peekChar' >>= \case
                  '=' -> return False
                  _c -> return True
    go '?' = Atto.peekChar' >>= \case
                  '=' -> return False
                  _c -> return True
    go '!' = Atto.peekChar' >>= \case
                  '=' -> return False
                  _c -> return True
    -- those chars are not allowed in variable names
    go ':' = return False
    go '#' = return False
    go '=' = return False
    go (Atto.isEndOfLine -> True) = return False
    go _c = return True

-- | Parse an assignment type, not consuming any of the assigned value. See
-- also 'assignment'.
--
-- >>> Atto.parseOnly assignmentType "!= bar "
-- Right ShellAssign
assignmentType :: Parser AssignmentType
assignmentType =
  ("=" *> pure RecursiveAssign)
  <|> ("+=" *> pure AppendAssign)
  <|> ("?=" *> pure ConditionalAssign)
  <|> ("!=" *> pure ShellAssign)
  <|> (":=" *> pure SimpleAssign)
  <|> ("::=" *> pure SimplePosixAssign)

-- | Parser for an entire rule
rule :: Parser Entry
rule =
  Rule
    <$> target
    <*> (many' dependency <* (Atto.takeWhile (not.Atto.isEndOfLine) <* endOfLine'))
    <*> many' command

-- | Succeeds on 'Atto.endOfLine' (line end) or if the end of input is reached.
endOfLine' :: Parser ()
endOfLine' =
    Atto.endOfLine <|> (Atto.atEnd >>= check)
  where
    check True = pure ()
    check False = mzero

-- | Parser for a command
command :: Parser Command
command = Command <$> recipeLine

recipeLine :: Parser T.Text
recipeLine =
    Atto.char '\t' *> recipeLineContents ""
  where
    recipeLineContents pre = do
      cur <- Atto.takeWhile $ \c ->
          c /= '\\' && not (Atto.isEndOfLine c)
      asum
        [ -- Multi-line
          Atto.char '\\'
            *> Atto.endOfLine
            *> (void (Atto.char '\t') <|> pure ())
            *> recipeLineContents (pre <> cur <> "\\\n")
        , -- Just EOL or EOF
          endOfLine' *> pure (pre <> cur)
        , -- It was just a backslash within a recipe line, we're not doing
          -- anything particular
          Atto.char '\\' *> recipeLineContents (pre <> cur <> "\\")
        ]

-- | Parser for a (rule) target
target :: Parser Target
target = Target <$> (go $ stripped (Atto.takeWhile (/= ':') <* Atto.char ':'))
  where
    -- takes care of some makefile target quirks
    go :: Parser a -> Parser a
    go p =
        Atto.takeWhile (liftA2 (||) (== ' ') (== '\t'))
          *> (Atto.peekChar >>= \case
              Just '#' -> mzero
              Just '\n' -> mzero
              _ -> p)

-- | Parser for a (rule) dependency
dependency :: Parser Dependency
dependency = Dependency <$> (sameLine <|> newLine)
  where
    sameLine =
      Atto.takeWhile (== ' ')
        *> Atto.takeWhile1 (`notElem` [' ', '\n', '#', '\\'])
    newLine =
      Atto.takeWhile (== ' ')
        *> Atto.char '\\'
        *> Atto.char '\n'
        *> (sameLine <|> newLine)

-- | Catch all, used for
--    * comments, empty lines
--    * lines that failed to parse
--
-- >>> parseAll otherLine "# I AM A COMMENT\n"
-- Right (OtherLine "# I AM A COMMENT")
--
-- Ensure all 'Entry's consume the end of line:
-- >>> parseAll otherLine "\n"
-- Right (OtherLine "")
--
otherLine :: Parser Entry
otherLine = OtherLine <$> go
  where
    go = asum
      [ -- Typical case of empty line
        Atto.endOfLine *> pure ""
      , -- Either a line of spaces and/or comment, or a line that we failed to
        -- parse
        Atto.takeWhile1 (not . Atto.isEndOfLine) <* Atto.endOfLine
      ]

toLineEnd :: Parser T.Text
toLineEnd = Atto.takeWhile (`notElem` ['\n', '#'])

-- | Get the contents until the end of the (potentially multi) line. Multiple
-- lines are separated by a @\\@ char and individual lines will be stripped and
-- spaces will be interspersed.
--
-- The final @\n@ character is consumed.
--
-- >>> Atto.parseOnly toEscapedLineEnd "foo bar \\\n baz"
-- Right "foo bar baz"
--
-- >>> Atto.parseOnly toEscapedLineEnd "foo \t\\\n bar \\\n baz \\\n \t"
-- Right "foo bar baz"
toEscapedLineEnd :: Parser T.Text
toEscapedLineEnd = (T.unwords . filter (not . T.null)) <$> go
  where
    go = do
      l <- toLineEnd <* (void (Atto.char '\n') <|> pure ())
      case T.stripSuffix "\\" l of
        Nothing -> return [T.strip l]
        Just l' -> (T.strip l':) <$> go

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

stripped :: Parser T.Text -> Parser T.Text
stripped = fmap T.strip
