-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Language.Bond.Lexer
    (  angles
    , braces
    , brackets
    , colon
    , comma
    , commaEnd
    , commaEnd1
    , commaSep1
    , decimal
    , equal
    , float
    , identifier
    , namespaceIdentifier
    , integer
    , keyword
    , lexeme
    , natural
    , parens
    , unescapedStringLiteral
    , semi
    , semiEnd
    , semiOrCommaEnd
    , semiOrCommaSep
    , semiOrCommaSep1
    , semiOrCommaSepEnd
    , semiOrCommaSepEnd1
    , semiSep
    , stringLiteral
    , symbol
    , whiteSpace
    ) where

import Data.List
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Text
--import Text.ParserCombinators.Parsec
import qualified Text.Megaparsec.Lexer as P
import Data.Char ( isAlpha, toLower, toUpper, isSpace, digitToInt )

reservedNames::[String]
reservedNames = ["blob"
            , "bond_meta"
            , "bonded"
            , "bool"
            , "class"
            , "double"
            , "enum"
            , "false"
            , "float"
            , "import"
            , "int16"
            , "int32"
            , "int64"
            , "int8"
            , "list"
            , "map"
            , "namespace"
            , "nullable"
            , "optional"
            , "required"
            , "required_optional"
            , "Schema"
            , "sealed"
            , "service"
            , "set"
            , "string"
            , "struct"
            , "true"
            , "uint16"
            , "uint32"
            , "uint64"
            , "uint8"
            , "using"
            , "var"
            , "vector"
            , "view_of"
            , "void"
            , "wstring"
            ]



simpleSpace =
        skipSome (satisfy isSpace)

whiteSpace::Parser ()
whiteSpace = P.space (void simpleSpace) lineComment blockComment
        where lineComment = P.skipLineComment "//"
              blockComment = P.skipBlockComment  "/*" "*/"

lexeme  = P.lexeme whiteSpace

symbol :: String -> Parser String
symbol = P.symbol whiteSpace

angles = between (symbol "<") (symbol ">")
braces = between (symbol "{") (symbol "}")
brackets = between (symbol "[") (symbol "]")
colon = symbol ":"
comma = symbol ","
commaSep1 p    = sepBy1 p comma
decimal = lexeme P.decimal
makeIdentifier :: [String]->Parser String
makeIdentifier theReservedNames = lexeme (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` theReservedNames
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x
identifier :: Parser String
identifier = makeIdentifier reservedNames
namespaceIdentifier :: Parser String
namespaceIdentifier = makeIdentifier (delete "Schema" reservedNames )

--identifier :: Parser String
--identifier = lexeme (p >>= check)
--  where
--    p       = (:) <$> letterChar <*> many alphaNumChar
--    check x = if x `elem` reservedNames
--                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
--                else return x

natural = lexeme P.integer
integer = P.signed whiteSpace (lexeme P.integer)
keyword::String -> Parser ()
keyword w = string w *> notFollowedBy alphaNumChar *> whiteSpace
parens = between (symbol "(") (symbol ")")
semi = symbol ";"
semiSep p   = sepBy p semi
equal = symbol "="

semiEnd p   = endBy p semi

commaEnd p  = endBy p comma
commaEnd1 p = endBy1 p comma

semiOrComma = semi <|> comma

semiOrCommaSep p     = sepBy p semiOrComma
semiOrCommaSep1 p    = sepBy1 p semiOrComma
semiOrCommaEnd p     = endBy p semiOrComma
semiOrCommaSepEnd p  = sepEndBy p semiOrComma
semiOrCommaSepEnd1 p = sepEndBy1 p semiOrComma

quote = symbol "\""
quotes = between quote quote


stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill P.charLiteral (char '"')

unescapedStringLiteral = quotes $ many $ satisfy (/= '"')
float = P.signed whiteSpace (lexeme P.float)
