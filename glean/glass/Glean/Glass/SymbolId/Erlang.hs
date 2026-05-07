{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.Erlang
  ({- instances -})
  where

import TextShow

import Glean (Nat, fromNat, keyOf)
import Glean.Glass.SymbolId.Class
import Glean.Glass.Types (Name(..))
import qualified Glean.Haxl.Repos as Glean

import qualified Glean.Schema.Erlang.Types as Erlang
import Data.Text (Text, intercalate)

import Glean.Schema.CodeErlang.Types as CodeErlang
    ( Entity_1(..) )

instance Symbol CodeErlang.Entity_1 where
  toSymbol e = case e of
    CodeErlang.Entity_1_decl decl -> toSymbol decl
    CodeErlang.Entity_1_EMPTY -> return []

instance Symbol Erlang.Declaration_1 where
  toSymbol decl = case decl of
    Erlang.Declaration_1_func func -> toSymbolPredicate func
    Erlang.Declaration_1_EMPTY -> return []

instance Symbol Erlang.FunctionDeclaration_1_key where
  toSymbol (Erlang.FunctionDeclaration_1_key fqn _file _span) = toSymbol fqn

instance Symbol Erlang.Fqn_1 where
  toSymbol (Erlang.Fqn_1 module_ name arity) =
    return [module_, name, showt (fromNat arity)]

instance Symbol Text where
  toSymbol module_ = return [module_]

instance Symbol (Text, Nat) where
  toSymbol (name, arity) =
      return [intercalate "." [name, showt (fromNat arity)]]

instance ToQName CodeErlang.Entity_1 where
  toQName e = case e of
    CodeErlang.Entity_1_decl x -> toQName x
    CodeErlang.Entity_1_EMPTY -> return $ Left "unknown Entity"

instance ToQName Erlang.Declaration_1 where
  toQName e = case e of
    Erlang.Declaration_1_func x -> Glean.keyOf x >>= toQName
    Erlang.Declaration_1_EMPTY -> return $ Left "unknown Declaration"

instance ToQName Erlang.FunctionDeclaration_1_key where
  toQName e = case e of
    Erlang.FunctionDeclaration_1_key fqn _file _span -> toQName fqn

instance ToQName Erlang.Fqn_1 where
  toQName e = case e of
    Erlang.Fqn_1 module_ name arity -> pairToQName (name, arity) module_

pairToQName
  :: (Symbol name, Symbol container)
  => name
  -> container
  -> Glean.RepoHaxl u w (Either a (Name, Name))
pairToQName a b = Right <$> symbolPairToQName "." a b
