-------------------------------------------------------------------------------
-- Pretty printer for the Imp language
-------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
module Lang.Imp.Pretty
  ( ppExpr,
    ppCmd,
  )
where

import Lang.Imp.Ast

todo :: a
todo = error "TODO"

-- | Pretty print an expression
ppExpr :: Expr a -> String
ppExpr = todo

-- | Pretty print a command
ppCmd :: Cmd -> String
ppCmd = todo