{-# LANGUAGE FlexibleInstances #-}

module Exe6 where

import Calc
import qualified Data.Map as M

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer 
                | Var String
                | Add VarExprT VarExprT
                | Mul VarExprT VarExprT
                deriving Show

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup 

-- lit :: Integer -> M.Map String Integer -> Maybe Integer
-- add :: (Map -> Maybe Integer) -> (Map -> Maybe Integer) -> Map -> Maybe Integer

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x _ = Just x 
  add a b m = add' (a m) (b m)
    where 
      add' :: Maybe Integer -> Maybe Integer -> Maybe Integer
      add' (Just x) (Just y) = Just (x+y)
      add' _ _ = Nothing 
  mul a b m = mul' (a m) (b m)
    where 
      mul' :: Maybe Integer -> Maybe Integer -> Maybe Integer
      mul' (Just x) (Just y) = Just (x*y)
      mul' _ _ = Nothing 

withVars :: [(String, Integer)]
  -> (M.Map String Integer -> Maybe Integer)
  -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
