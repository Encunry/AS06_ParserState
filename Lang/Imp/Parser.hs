-------------------------------------------------------------------------------
-- Parser for the Imp language
-- https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form
-------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Lang.Imp.Parser
    ( parse_imp,
    )
where

import Control.Applicative hiding (Const)
import Lang.Imp.Ast
import Lang.Parser.Combinators

todo :: a
todo = error "TODO"

-- cSeq = cmd { ";" cmd }
cmd_sequence_parser :: Parser Cmd
cmd_sequence_parser = do
    cmd <- cmd_parser
    cmds <- many (symbol ";" *> cmd_parser)
    pure $ Cmd_Sequence (cmd : cmds)

-- cmd = cSkip | cAssign | cIfThEl | cWhile
cmd_parser :: Parser Cmd
cmd_parser 
    = cmd_skip_parser
    <|> cmd_assign_parser
    <|> cmd_ifThenElse_parser
    <|> cmd_while_parser

-- cSkip = "skip"
cmd_skip_parser :: Parser Cmd
cmd_skip_parser = symbol "skip" *> pure Cmd_Skip

-- cAssign = ident ":=" arithExpr
cmd_assign_parser :: Parser Cmd
cmd_assign_parser = do
    name <- token ident
    symbol ":="
    expr <- arith_expr_parser
    pure (Cmd_Assign name expr)

-- cIfThEl = "if" boolExpr "then" cseq "else" cseq "endif"
cmd_ifThenElse_parser :: Parser Cmd
cmd_ifThenElse_parser = do
    symbol "if"
    bExpr <- bool_expr_parser
    symbol "then"
    thCmd <- cmd_sequence_parser
    symbol "else"
    elCmd <- cmd_sequence_parser
    symbol "endif"
    pure (Cmd_IfThenElse bExpr thCmd elCmd)

-- cWhile = "while" boolExpr "do" cseq "endwhile"
cmd_while_parser :: Parser Cmd
cmd_while_parser = do
    symbol "while"
    bExpr <- bool_expr_parser
    symbol "do"
    cmd <- cmd_sequence_parser
    symbol "endwhile"
    pure (Cmd_While bExpr cmd)

-- arithExpr = const | var | arithBinOp
arith_expr_parser :: Parser (Expr Integer)
arith_expr_parser = constant_parser <|> variable_parser <|> arith_bin_expr_parser

-- nat = <digit> {<digit>}
nat_parser :: Parser Integer
nat_parser = fmap read (some digit)

-- const = nat | "-" nat
constant_parser :: Parser (Expr Integer)
constant_parser = fmap Arith_Constant (nat_parser <|> (pure negate <* char '-' <*> nat_parser))

-- var = ident
variable_parser :: Parser (Expr Integer)
variable_parser = fmap Arith_Variable (token ident)

-- arithBinOp = "(" arithExpr arithOp arithExpr ")"
arith_bin_expr_parser :: Parser (Expr Integer)
arith_bin_expr_parser = do
    symbol "("
    l <- arith_expr_parser
    op <- arith_op_parser
    r <- arith_expr_parser
    symbol ")"
    pure
        ( case op of
            Arith_Add_Op  -> Arith_Add l r
            Arith_Sub_Op  -> Arith_Sub l r
            Arith_Mult_Op -> Arith_Mul l r
            Arith_Div_Op  -> Arith_Div l r
            Arith_Mod_Op  -> Arith_Mod l r
        )

data AOp 
    = Arith_Add_Op 
    | Arith_Sub_Op
    | Arith_Mult_Op
    | Arith_Div_Op
    | Arith_Mod_Op

-- arithOp = "+" | "-" | "*" | "/" | "%"
arith_op_parser :: Parser AOp
arith_op_parser =
  symbol "+" *> pure Arith_Add_Op
    <|> symbol "-" *> pure Arith_Sub_Op
    <|> symbol "*" *> pure Arith_Mult_Op
    <|> symbol "/" *> pure Arith_Div_Op
    <|> symbol "%" *> pure Arith_Mod_Op

-- boolExpr = bNotExpr | bAndEpr | relBExpr
bool_expr_parser :: Parser (Expr Bool)
bool_expr_parser = bool_not_expr_parser <|> bool_op_expr_parser <|> rel_bool_expr_parser

-- bNotExpr = "not" boolExpr
bool_not_expr_parser :: Parser (Expr Bool)
bool_not_expr_parser = do
    string "not"
    bExpr <- bool_expr_parser
    pure (Bool_Not bExpr)

-- bOpExprP = "(" boolExpr bOp boolExpr ")"
bool_op_expr_parser :: Parser (Expr Bool)
bool_op_expr_parser = do
    symbol "("
    l <- bool_expr_parser
    op <- bool_op_parser
    r <- bool_expr_parser
    symbol ")"
    pure
        ( case op of
            Bool_And_Op -> Bool_And l r
            Bool_Or_Op -> Bool_Or l r
        )

data Bool_Op
    = Bool_And_Op
    | Bool_Or_Op

-- bOp = "&&" | "||"
bool_op_parser :: Parser Bool_Op
bool_op_parser =
    symbol "&&" *> pure Bool_And_Op
        <|> symbol "||" *> pure Bool_Or_Op

-- relBExpr = "(" arithExpr relOp arithExpr ")"
rel_bool_expr_parser :: Parser (Expr Bool)
rel_bool_expr_parser = do
    symbol "("
    l <- arith_expr_parser
    op <- rel_op_parser
    r <- arith_expr_parser
    symbol ")"
    case op of
        Rel_Equal_Op -> pure $ Rel_Equal l r
        Rel_LessThan_Op -> pure $ Rel_LessThan l r
        Rel_LessThanEqual_Op -> pure $ Bool_Or (Rel_LessThan l r) (Rel_Equal l r)
        Rel_GreaterThan_Op -> pure $ Bool_Not $ Bool_Or (Rel_LessThan l r) (Rel_Equal l r)
        Rel_GreaterThanEqual_Op -> pure $ Bool_Not $ Rel_LessThan l r

-- a <= b is syntactic sugar and expressed as: a < b || a == b

data ROp
    = Rel_Equal_Op
    | Rel_LessThan_Op
    | Rel_LessThanEqual_Op
    | Rel_GreaterThan_Op
    | Rel_GreaterThanEqual_Op

-- arithExpr = "<=" | "<" | "="
rel_op_parser :: Parser ROp
rel_op_parser =
    symbol "<=" *> pure Rel_LessThanEqual_Op -- order is relevant here
        <|> symbol ">=" *> pure Rel_GreaterThanEqual_Op
        <|> symbol ">" *> pure Rel_GreaterThan_Op
        <|> symbol "<" *> pure Rel_LessThan_Op
        <|> symbol "=" *> pure Rel_Equal_Op

-- ident = <lower> {<alphaNum>}
ident :: Parser String
ident = do
    c <- lower
    cs <- many alphaNum
    pure (c : cs)

-- Entry point for the parser
parse_imp :: String -> Either String Cmd
parse_imp input = case parse cmd_sequence_parser input of
    Nothing -> Left "Parse error"
    Just (cmd, "") -> Right cmd
    Just (_, rest) -> Left ("Parse error: " ++ rest)
