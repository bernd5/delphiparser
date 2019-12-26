{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module HaskellPrettyPrint
  ( HH(..)
  )
where

import Data.Text hiding (map, concat)
import DelphiAst

data HaskellState content = HaskellState { contentLines :: content }

instance Monad HaskellState where
instance Applicative HaskellState where
instance Functor HaskellState where

type HS = HaskellState [Text]

class HH a where
  hh :: a -> HS

instance HH Unit where
  hh (Program 
      (Lexeme coments name)
      uses
      impl
      expr) = do
        coments' <- hh coments
        uses' <- hh uses
        pure $ [ "-- Program: " <> name <> " -- Automatically converted from Pascal sources"
               , coments'
               , uses'
               ]

instance HH (Lexeme Text) where
  hh (Lexeme c a) = pure [ pack $ show c
                         , a
                         ]

instance HH Uses where
  hh (Uses names directives) = pure $ concat [ (map hh names)
                                             , hh directives
                                             ]

instance HH Directive where
  hh (Compound a b) = pure [ hh a , hh b ]
  hh (Comment a) = pure ["--" <> a]
  hh a = pure [(pack . show) a]

