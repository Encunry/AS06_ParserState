-------------------------------------------------------------------------------
-- Interpreter for the Imp language
-------------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module Lang.Imp.Interpreter
  ( State,
    emptyState,
    ImpExpr (..),
    ask,
    failWith,
    readVariable,
    eval,
    Imp_Cmd (..),
    getState,
    setState,
    modifyState,
    assignVariable,
    liftReader,
    exec,
  )
where

import qualified Data.Map.Strict as M
import Lang.Imp.Ast

todo :: a
todo = error "TODO"

type State = M.Map Name Integer

emptyState :: State
emptyState = M.empty

-- ImpExpr: Value which takes a state and either returns a value or an error.
newtype ImpExpr a = ImpExpr {runExpr :: State -> Either String a}

-- The Functor instance for ImpExpr:
instance Functor ImpExpr where
  fmap :: (a -> b) -> ImpExpr a -> ImpExpr b
  fmap f ma = ma >>= pure . f

-- The Applicative instance for ImpExpr:
instance Applicative ImpExpr where
  pure :: a -> ImpExpr a
  pure a = ImpExpr (\_ -> Right a)

  (<*>) :: ImpExpr (a -> b) -> ImpExpr a -> ImpExpr b
  mf <*> ma = do
    f <- mf
    a <- ma
    pure (f a)

-- The Monad instance for ImpExpr:
instance Monad ImpExpr where
  (>>=) :: ImpExpr a -> (a -> ImpExpr b) -> ImpExpr b
  ImpExpr a >>= f =
    ImpExpr
      ( \st ->
          a st >>= \x -> runExpr (f x) st -- the (>>=) on this line is the one from Either
      )

-- Returns the current state.
ask :: ImpExpr State
ask = ImpExpr $ \st -> Right st

-- Fails with Left and the given error message.
failWith :: String -> ImpExpr a
failWith s = ImpExpr $ \_ -> Left s

-- Reads a variable from the state. Fails with Left if the variable is not found.
readVariable :: Name -> ImpExpr Integer
readVariable n = do
  st <- ask
  maybe (failWith $ "variable not found: " ++ n) pure (M.lookup n st)

eval :: Expr a -> ImpExpr a
eval (Bool_Not b) = fmap not (eval b)
eval (Bool_And a b) = pure (&&) <*> eval a <*> eval b
eval (Bool_Or a b) = pure (||) <*> eval a <*> eval b
eval (Rel_Equal a b) = pure (==) <*> eval a <*> eval b
eval (Rel_LessThan a b) = pure (<) <*> eval a <*> eval b
eval (Arith_Variable n) = readVariable n
eval (Arith_Constant n) = pure n
eval (Arith_Add a b) = pure (+) <*> eval a <*> eval b
eval (Arith_Sub a b) = pure (-) <*> eval a <*> eval b
eval (Arith_Mul a b) = pure (*) <*> eval a <*> eval b
eval (Arith_Div a b) = do
    a' <- eval a
    b' <- eval b
    if b' == 0
      then failWith "division by zero"
      else pure (div a' b')
eval (Arith_Mod a b) = pure mod <*> eval a <*> eval b

-- State monad for the Cmd interpreter:
newtype Imp_Cmd a = Imp_Cmd {run_state :: State -> Either String (State, a)}

instance Functor Imp_Cmd where
    fmap :: (a -> b) -> Imp_Cmd a -> Imp_Cmd b
    fmap fun ma = ma >>= pure . fun

instance Applicative Imp_Cmd where
    pure :: a -> Imp_Cmd a
    pure a = Imp_Cmd (\st -> Right (st, a))
    (<*>) :: Imp_Cmd (a -> b) -> Imp_Cmd a -> Imp_Cmd b
    mf <*> ma = do
        f <- mf
        a <- ma
        pure (f a)

instance Monad Imp_Cmd where
  (>>=) :: Imp_Cmd a -> (a -> Imp_Cmd b) -> Imp_Cmd b
  Imp_Cmd a >>= f =
    Imp_Cmd
        ( \st ->
            a st >>= \(st', x) -> run_state (f x) st'
        )

-- Returns the current state
-- TODO: Implement the `getState` function.
getState :: Imp_Cmd State
getState = Imp_Cmd (\state -> Right (state, state))

-- Overwrites the state with the given argument
-- TODO: Implement the `setState` function.
setState :: State -> Imp_Cmd ()
setState new_state = Imp_Cmd (\_ -> Right (new_state, ()))

-- Helper to update the current state
modifyState :: (State -> State) -> Imp_Cmd ()
modifyState f = do
    st <- getState
    setState (f st)

-- Sets the assignment in the state
-- TODO: Implement the `assignVariable` function.
assignVariable :: Name -> Integer -> Imp_Cmd ()
assignVariable name val = do
    state <- getState
    setState $ M.insert name val state


-- Lifts an ImpExpr into an ImpCmd
liftReader :: ImpExpr a -> Imp_Cmd a
liftReader expr = Imp_Cmd (\st -> fmap (st,) (runExpr expr st))

-- TODO: Complete the implementation of the `exec` function.
exec :: Cmd -> Imp_Cmd ()
exec Cmd_Skip = pure ()
exec (Cmd_Assign name expr) = do
    val <- liftReader $ eval expr
    assignVariable name val
exec (Cmd_IfThenElse bool t e) = do
    condition <- liftReader $ eval bool
    if condition
        then do 
            exec t
        else 
            exec e
exec (Cmd_While b c) = do
  cond <- liftReader $ eval b
  if cond
    then do
      exec c
      exec (Cmd_While b c)
    else pure ()
exec (Cmd_Sequence cs) = mapM_ exec cs
