{-# LANGUAGE OverloadedStrings #-}

module TypeChecker where

import DelphiAst

-- Reads Unit, and lists all types defined in the interface section.
-- It does not resolve them, however.
exportedTypes :: Unit -> [TypeDefinition]
exportedTypes (Unit directive name (Interface _ intf) _ _ _) = concatMap getTypesFromInterface intf
exportedTypes otherwise = []

getTypesFromInterface (TypeDefinitions a) = a
