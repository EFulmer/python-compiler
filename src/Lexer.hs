{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lexer where

import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data ParseExpr = Newline
    | Indent
    | Dedent
    | Id Text
    | Literal Text
    | Keyword Text
    | Punct Text
    | Endmarker

keyword :: Parser Text
keyword = choice
    [ string "False"
    , string "await"
    , string "else"
    , string "import"
    , string "pass"
    , string "None"
    , string "break"
    , string "except"
    , string "in"
    , string "raise"
    , string "True"
    , string "class"
    , string "finally"
    , string "is"
    , string "return"
    , string "and"
    , string "continue"
    , string "for"
    , string "lambda"
    , string "try"
    , string "as"
    , string "def"
    , string "from"
    , string "nonlocal"
    , string "while"
    , string "assert"
    , string "del"
    , string "global"
    , string "not"
    , string "with"
    , string "async"
    , string "elif"
    , string "if"
    , string "or"
    , string "yield" ]

-- The inconsistency between using String and Text, requiring the use of
-- T.Pack, strikes me as a potential code smell...
identifierStart :: Parser Char
identifierStart = choice [ char '_', letterChar ]

identifierContinue :: Parser Text
identifierContinue = Text.Megaparsec.some alphaNumChar

identifier :: Parser Text
identifier = do
    start <- identifierStart
    rest <- identifierContinue
    return $ T.cons start (T.pack rest)

operator :: Parser Text
operator = choice
    [ string "+"
    , string "-"
    , string "*"
    , string "**"
    , string "/"
    , string "//"
    , string "%"
    , string "@"
    , string "<<"
    , string ">>"
    , string "&"
    , string "|"
    , string "^"
    , string "~"
    , string ":="
    , string "<"
    , string ">"
    , string "<="
    , string ">="
    , string "=="
    , string "!="
    ]

delimiter :: Parser Text
delimiter = choice
    [ string "("
    , string ")"
    , string "["
    , string "]"
    , string "{"
    , string "}"
    , string ","
    , string ":"
    , string "!"
    , string "."
    , string ";"
    , string "@"
    , string "="
    , string "->"
    , string "+="
    , string "-="
    , string "*="
    , string "/="
    , string "//="
    , string "%="
    , string "@="
    , string "&="
    , string "|="
    , string "^="
    , string ">>="
    , string "<<="
    , string "**="
    ]

punctuation :: Parser Text
punctuation = try operator <|> delimiter

-- This ignores the comment, of course.
comment :: Parser ()
comment = L.skipLineComment "#"
