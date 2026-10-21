{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Data.Map.Strict as M
import Lang.Imp.Ast
import Lang.Imp.Interpreter
import Lang.Imp.Parser
import Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldContain)

testExec :: Cmd -> State -> State
testExec cmd inputState = either error fst (run_state (exec cmd) inputState)

evalSpec :: Spec
evalSpec =
    describe "eval" $ do
        it "runExpr (eval (Arith_Constant 1)) emptyState == Right 1" $
            runExpr (eval (Arith_Constant 1)) emptyState `shouldBe` Right 1
        it "runExpr (eval (Arith_Variable \"a\")) emptyState == Left \"variable not found: a\"" $
            runExpr (eval (Arith_Variable "a")) emptyState `shouldBe` Left "variable not found: a"
        it "runExpr (eval (Arith_Variable \"a\")) (M.fromList [(\"a\", 5)]) == Right 5" $
            runExpr (eval (Arith_Variable "a")) (M.fromList [("a", 5)]) `shouldBe` Right 5

execSpec :: Spec
execSpec =
    describe "exec" $ do
        it "exec Cmd_Skip emptyState == emptyState" $
            run_state (exec Cmd_Skip) emptyState `shouldBe` Right (emptyState, ())
        it "M.toList (exec (Cmd_Assign \"a\" (Arith_Constant 2)) emptyState) == [(\"a\", 2)]" $
            M.toList endState `shouldBe` [("a", 2)]
  where
    endState = testExec (Cmd_Assign "a" (Arith_Constant 2)) emptyState

{-
Factorial:
i
z := x;
y := 1;
while (not (z = 0))
    y := y * z;
    z := z - 1
endwhile
-}
fact :: Cmd
fact =
    Cmd_Sequence
        [ Cmd_Assign "z" (Arith_Variable "x"),
          Cmd_Assign "y" (Arith_Constant 1),
          Cmd_While
            (Bool_Not (Rel_Equal (Arith_Variable "z") (Arith_Constant 0)))
            ( Cmd_Sequence
                [ Cmd_Assign "y" (Arith_Mul (Arith_Variable "y") (Arith_Variable "z")),
                  Cmd_Assign "z" (Arith_Sub (Arith_Variable "z") (Arith_Constant 1))
                ]
            )
        ]

factSpec :: Spec
factSpec =
    describe "fact" $ do
        -- fact [x=5] ~> y=120
        it "fact [\"x\" = 5] == [\"y\" = 120]" $
            M.lookup "y" endState `shouldBe` Just 120
  where
    endState = testExec fact (M.fromList [("x", 5)])

{-
Quotient remainder:

q := 0;
r := m;
while (n <= r)
    q := q + 1;
    r := r - n
endwhile
-}
intDiv :: Cmd
intDiv =
    Cmd_Sequence
        [ Cmd_Assign "q" (Arith_Constant 0),
          Cmd_Assign "r" (Arith_Variable "m"),
          Cmd_While
            ( Bool_Or
                (Rel_LessThan (Arith_Variable "n") (Arith_Variable "r"))
                (Rel_Equal (Arith_Variable "n") (Arith_Variable "r"))
            )
            ( Cmd_Sequence
                [ Cmd_Assign "q" (Arith_Add (Arith_Variable "q") (Arith_Constant 1)),
                  Cmd_Assign "r" (Arith_Sub (Arith_Variable "r") (Arith_Variable "n"))
                ]
            )
        ]

intDivSpec :: Spec
intDivSpec =
    describe "intDiv" $ do
        -- intDiv 5 2 == q_uotient=2 r_emainder=1
        it "intDiv [\"m\" = 5, \"n\" = 2] ~> [\"q\" = 2, \"r\" = 1]" $
            M.toList endState `shouldContain` [("q", 2), ("r", 1)]
  where
    endState = testExec intDiv (M.fromList [("m", 5), ("n", 2)])

{-
function gcd(a, b)
    while b ≠ 0
        t := b
        b := a mod b
        a := t
    return a
-}
gcd' :: Cmd
gcd' =
    Cmd_Sequence
        [ Cmd_While
            (Bool_Not (Rel_Equal (Arith_Variable "b") (Arith_Constant 0)))
            ( Cmd_Sequence
                [ Cmd_Assign "t" (Arith_Variable "b"),
                  Cmd_Assign "b" (Arith_Mod (Arith_Variable "a") (Arith_Variable "b")),
                  Cmd_Assign "a" (Arith_Variable "t")
                ]
            )
        ]

gcdSpec :: Spec
gcdSpec =
    describe "gcd'" $ do
        it "gcd' [\"a\" = 252, \"b\" = 105] -> [\"a\" = 21]" $
            M.lookup "a" endState `shouldBe` Just 21
  where
    endState = testExec gcd' (M.fromList [("a", 252), ("b", 105)])

parseSpec :: Spec
parseSpec =
    describe "Parser'" $ do
        it "parseImp \"x := 5\" == Right (Cmd_Sequence [Cmd_Assign \"x\" (Arith_Constant 5)])" $
            parse_imp "x := 5"
                `shouldBe` Right (Cmd_Sequence [Cmd_Assign "x" (Arith_Constant 5)])
        it "parseImp \"x := 5; y := 6\" == Right (Cmd_Sequence [Cmd_Assign \"x\" (Arith_Constant 5), Cmd_Assign \"y\" (Arith_Constant 6)])" $
            parse_imp "x := 5; y := 6"
                `shouldBe` Right
                    ( Cmd_Sequence
                        [ Cmd_Assign "x" (Arith_Constant 5),
                          Cmd_Assign "y" (Arith_Constant 6)
                        ]
                    )
        -- a > b  desugars to  not (a < b || a == b)
        it "parseImp \"while (a > b) do x := 1 endwhile\" desugars > to Bool_Not (Bool_Or (Rel_LessThan a b) (Rel_Equal a b))" $
            parse_imp "while (a > b) do x := 1 endwhile"
                `shouldBe` Right
                    ( Cmd_Sequence
                        [ Cmd_While
                            ( Bool_Not
                                ( Bool_Or
                                    (Rel_LessThan (Arith_Variable "a") (Arith_Variable "b"))
                                    (Rel_Equal (Arith_Variable "a") (Arith_Variable "b"))
                                )
                            )
                            (Cmd_Sequence [Cmd_Assign "x" (Arith_Constant 1)])
                        ]
                    )
        -- a >= b  desugars to  not (a < b)
        it "parse_imp \"while (a >= b) do x := 1 endwhile\" desugars >= to Bool_Not (Rel_LessThan a b)" $
            parse_imp "while (a >= b) do x := 1 endwhile"
                `shouldBe` Right
                    ( Cmd_Sequence
                        [ Cmd_While
                            (Bool_Not (Rel_LessThan (Arith_Variable "a") (Arith_Variable "b")))
                            (Cmd_Sequence [Cmd_Assign "x" (Arith_Constant 1)])
                        ]
                    )

main :: IO ()
main = hspec $ do
    describe "1. Evaluation" $ do
        evalSpec
    describe "2. Execution" $ do
        execSpec
    describe "3. Example programs" $ do
        factSpec
        intDivSpec
        gcdSpec
    describe "4. Parser tests" $ do
        parseSpec