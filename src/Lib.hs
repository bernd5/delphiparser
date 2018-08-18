{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  ) where

import Data.Text
import DelphiAst
import DelphiLexer
import DelphiParser
import Text.Pretty.Simple (pPrint)

--sharedPointer :: Unit
--sharedPointer = Unit "SharedPointer"
--  ( Interface [ TypeDef (Type "TDeallocator") ( ReferenceToProcedure [ Arg "AObj" $ Type "TObject" ])
--              , Record ( Generic "TShared" [Arg "T" $ Constraint [ClassConstraint]])
--                  [ Private [ Field "FFreeTheValue" $ Type "IInterface" ]
--                  , Public
--                        [ Constructor "Create" [Arg "AValue" $ Type "T"]
--                        , Procedure "Assign" [Arg "AValue" $ Type "T"] []
--                        , Procedure "SetDeallocator" [Arg "ADealloc" $ Type "TDeallocator"] []
--                        , Function "Temporary" [] (Type "T") []
--                        , Function "Cast<TT: class>" [] (Generic "TShared" [Arg "TT" UnspecifiedType]) []
--                        , Function "Release" [] (Type "T") []]]
--              , Class (Type "TFreeTheValue") [Type "TInterfacedObject"]
--                  [ Public
--                       [ Field "FObjectToFree" (Type "TObject")
--                       , Field "FCustomDeallocator" (Type "TDeallocator")
--                       , Constructor "Create" [Arg "AObjToFree" (Type "TObject")]
--                       , Destructor "Destroy" [Override]]]
--              , Record (Generic "TSharedList" [Arg "T" $ Constraint [ClassConstraint]])
--                  [ Private
--                    [ Field "FList" (Array $ GenericInstance "TShared" [Type "T"])
--                    , Function "GetItem" [Arg "I" $ Type "Integer"] (Type "T") []
--                    , Function "GetSharedItem" [Arg "I" $ Type "Integer"] (GenericInstance "TShared" [Type "T"]) []]
--                  , Public
--                    [ Function "Count" [] (Type "Integer") []
--                    , IndexProperty "Items" (Arg "I" $ Type "Integer") (Type "T")
--                        (Just "GetItem") Nothing []
--                    , IndexProperty "SharedItems" (Arg "I" $ Type "Integer") (GenericInstance "TShared" [Type "T"])
--                        (Just "GetSharedItem") Nothing [Default]
--                    , Procedure "Add" [Arg "AObject" (GenericInstance "TShared" [Type "T"])] []
--                    , Procedure "Clear" [] [] ]]
--              ]
--  )
--  (Implementation
--    [ MemberFunctionImpl (Type "TShared<T>") (Type "Temporary") [] (Type "T")
--      $ Begin [
--        "Result" `Assign` Nil
--      , If ( (ValueOf "FFreeTheValue" `Neq` Nil)
--             `And`
--             (((ValueOf "FFreeTheValue") `As` (Type "TFreeTheValue") `Prop` "FObjectToFree") `Neq` Nil))
--           $
--        Then ( "Result" `Assign` (ValueOf "FFreeTheValue" `As` (Type "TFreeTheValue") `Prop` "FObjectToFree" `As` (Type "T")))]])
--  Initialization
--  Finalization
someFunc :: IO ()
someFunc
  --pPrint sharedPointer
 = do
  delphi <- readFile "/Users/johnchapman/dev/sharedpointer/sharedpointer.pas"
  parseDelphiUnit delphi
