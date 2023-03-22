-- | Semantics (interpreter) for variable based value domain calculator with explicit documented signatures.
-- The variables are linked to values in State.
--
-- Author Magne Haveraaen
-- Since 2020-03-19

module Pam8TSignatureInterpreter where

-- Use ASTs based on signatures.
import Pam8TSignature
import Pam8TSignatureAST

-- Use state for variables with store.
import Pam8State


-----------------------
-- | Evaluate a Calculator expression with a function model and state.
-- The function model of the intrinsic functions are given by the funmod argument.
evaluate :: FunModel valuedomain -> State valuedomain -> CalcExprAST valuedomain -> valuedomain
evaluate funmod state (Lit i) = i
evaluate funmod state (Fun str args) 
  = funmod str (map (evaluate funmod state) args)
evaluate funmod state (Var vname) = getValue vname state

-- | Execute a statement: set/assign a variable in state from a calculator expression.
execute :: Show valuedomain => FunModel valuedomain -> CalcStmtAST valuedomain -> State valuedomain -> State valuedomain
execute funmod (SetVar vname exp) state
  = addVariable vname (evaluate funmod state exp) state
execute funmod (AssVar vname exp) state
  = changeValue vname (evaluate funmod state exp) state


-----------------------
-- | Check if all variables in an expression have been declared in the state.
-- Returns a list of all undeclared variables.
allDeclared :: State valuedomain -> CalcExprAST valuedomain -> [VarName]
allDeclared state (Lit _) = []
allDeclared state (Fun fname args) = foldl (++) [] (map (allDeclared state) args)
allDeclared state (Var vname) = if isDeclared vname state then [] else [vname]

-----------------------
-- | Unit test for calculator with variables and open ended set of intrinsic functions.
-- Can only test the structural part of the calculator:
-- declaring, assigning and accessing variables using "fake" semantic function testfunmod.
unittestPam8TSignatureInterpreter = do
  print $ "-- unittestPam8TSignatureInterpreter --"
  let -- | Fake integer semantics function: always returns 5.
      testfunmod :: FunModel Integer
      testfunmod str plist = 5
  -- Create variables in order, but with the wrong values.
  let state1 = execute testfunmod (SetVar "x" (Lit 11)) newState
  let state2 = execute testfunmod (SetVar "y" (Lit 10)) state1
  let state3 = execute testfunmod (SetVar "z" (Lit 23)) state2
  -- Correct the values for the variables out of order.
  let state4 = execute testfunmod (AssVar "y" (Lit 37)) state3
  let state5 = execute testfunmod (AssVar "z" (Lit 39)) state4
  let state6 = execute testfunmod (AssVar "x" (Lit 31)) state5
  -- Create the expected set of variable-values.
  let state1' = execute testfunmod (SetVar "x" (Lit 31)) newState
  let state2' = execute testfunmod (SetVar "y" (Lit 37)) state1'
  let state3' = execute testfunmod (SetVar "z" (Lit 39)) state2'
  -- Checking if all variables in an AST have been declared.
  let expr = Fun "Add" [(Var "x"), (Var "z")]
  let chv0 = allDeclared newState expr == ["x","z"]
  let chv1 = allDeclared state1 expr == ["z"]
  let chv2 = allDeclared state3 expr == []
  -- Output result of unit test
  putStrLn $ 
    if (state3' == state6)
    && (evaluate testfunmod state6 (Var "x") == 31)
    && (evaluate testfunmod state6 (Var "y")  == 37)
    && (evaluate testfunmod state6 (Var "z")  == 39)
    && (testfunmod "func" []) == 5
    && chv0 && chv1 && chv2
    then "Unit tests hold"
    else "Tests failed"

