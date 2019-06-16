{-# LANGUAGE OverloadedStrings #-}

module TypeCategories where

import           Data.Text                      ( Text
                                                )
import qualified Data.Map                      as Map
import DelphiAst
import Data.List (foldl')

categorize :: [TypeDefinition] -> Map.Map Text [TypeDefinition]
categorize defs = foldl' f Map.empty defs
  where
    f :: (Map.Map Text [TypeDefinition]) -> TypeDefinition -> (Map.Map Text [TypeDefinition])
    f a b@TypeDef{} = ins a b "Type Definitions"
    f a b@TypeAlias{} = ins a b "type aliases"
    f a b@EnumDefinition{} = ins a b "Enum Definitions"
    f a b@SetDefinition{} = ins a b "Set Definitions"
    f a b@Record{} = ins a b "Records"
    f a b@ForwardClass{} = ins a b "Forward Class Declarations"
    f a b@TypeAttribute{} = ins a b "Type Attributes"
    f a b@InterfaceType{} = ins a b "Interface Types"
    f a b@Class{} = ins a b "Class Types"
    f a b@TypeExpression{} = ins a b "Type Expressions"

    ins a b k = Map.insertWith (<>) k [b] a
