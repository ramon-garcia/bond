-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Language.Bond.Codegen.CustomMapping
    ( AliasMapping(..)
    , Fragment(..)
    , NamespaceMapping(..)
    , parseAliasMapping
    , parseNamespaceMapping
    ) where

import Data.Char
import Control.Applicative
import Prelude
import Text.Megaparsec hiding (many, optional, (<|>))
import Text.Megaparsec.String
import Language.Bond.Syntax.Types

-- | Specification of a fragment of type alias mappings.
data Fragment =
    Fragment String |                   -- ^ hardcoded string fragment
    Placeholder Int                     -- ^ placeholder for the n-th type argument of the type alias, applicable only to generic aliases

-- | Specification of a type alias mapping.
data AliasMapping = AliasMapping
    { aliasName :: QualifiedName        -- ^ qualified name of a type alias
    , aliasTemplate :: [Fragment]       -- ^ list of fragments comprising the custom mapping for the alias
    }

-- | Specification of namespace mapping.
data NamespaceMapping = NamespaceMapping
    { fromNamespace :: QualifiedName    -- ^ schema namespace
    , toNamespace :: QualifiedName      -- ^ namespace in the generated code
    }

whitespace :: Parser String
whitespace = many (char ' ') <?> "whitespace"
identifier :: Parser String
identifier = some (alphaNumChar <|> char '_') <?> "identifier"
qualifiedName :: Parser [String]
qualifiedName = sepBy1 identifier (char '.') <?> "qualified name"
symbol :: String -> Parser String
symbol s = whitespace *> string s <* whitespace
equal :: Parser String
equal = symbol "="
integer :: Parser Integer
integer = decimal <$> some digitChar <?> "decimal number"
  where
    decimal = foldl (\x d -> 10 * x + toInteger (digitToInt d)) 0

-- | Parse a type alias mapping specification used in command-line arguments of 
-- <https://microsoft.github.io/bond/manual/compiler.html#command-line-options gbc>.
--
-- ==== __Examples__
--
-- > > parseAliasMapping "Example.OrderedSet=SortedSet<{0}>"
-- > Right (AliasMapping {aliasName = ["Example","OrderedSet"], aliasTemplate = [Fragment "SortedSet<",Placeholder 0,Fragment ">"]})
parseAliasMapping:: String -> Either (ParseError Char Dec) AliasMapping
parseAliasMapping s = parse aliasMapping s s
  where
    aliasMapping = AliasMapping <$> qualifiedName <* equal <*> some (placeholder <|> fragment) <* eof
    placeholder = Placeholder <$> fromIntegral <$> between (char '{') (char '}') integer
    fragment = Fragment <$> some (noneOf "{")

-- | Parse a namespace mapping specification used in command-line arguments of 
-- <https://microsoft.github.io/bond/manual/compiler.html#command-line-options gbc>.
--
-- ==== __Examples__
--
-- > > parseNamespaceMapping "bond=Microsoft.Bond"
-- > Right (NamespaceMapping {fromNamespace = ["bond"], toNamespace = ["Microsoft","Bond"]})
parseNamespaceMapping :: String -> Either (ParseError Char Dec) NamespaceMapping
parseNamespaceMapping s = parse namespaceMapping s s
  where
    namespaceMapping = NamespaceMapping <$> qualifiedName <* equal <*> qualifiedName

