-------------------------------------------------------------------------------
-- Intermediate Representation for the Imp language
-------------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module Lang.Imp.Ast
    ( Name,
      Expr (..),
      Cmd (..),
    )
where

type Name = String

data Expr a where
    Bool_Not :: Expr Bool -> Expr Bool
    Bool_And :: Expr Bool -> Expr Bool -> Expr Bool
    Bool_Or :: Expr Bool -> Expr Bool -> Expr Bool
    Rel_Equal :: Expr Integer -> Expr Integer -> Expr Bool
    Rel_LessThan :: Expr Integer -> Expr Integer -> Expr Bool
    Arith_Constant :: Integer -> Expr Integer
    Arith_Add :: Expr Integer -> Expr Integer -> Expr Integer
    Arith_Sub :: Expr Integer -> Expr Integer -> Expr Integer
    Arith_Mul :: Expr Integer -> Expr Integer -> Expr Integer
    Arith_Div :: Expr Integer -> Expr Integer -> Expr Integer
    Arith_Mod :: Expr Integer -> Expr Integer -> Expr Integer
    Arith_Variable :: Name -> Expr Integer

deriving instance Eq (Expr a)

deriving instance Show (Expr a)

data Cmd
    = Cmd_Skip
    | Cmd_Assign Name (Expr Integer)
    | Cmd_IfThenElse (Expr Bool) Cmd Cmd
    | Cmd_While (Expr Bool) Cmd
    | Cmd_Sequence [Cmd]
    deriving (Eq, Show)