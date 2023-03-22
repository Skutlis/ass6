-- | AST for variable based value domain calculator with explicit documented signatures.
-- Checks calculator expressions for type errors against the signature.
--
-- Author Magne Haveraaen
-- Since 2020-03-19

module Pam8TSignatureAST where

-- Based on signatures and function models
import Pam8TSignature

-----------------------
-- | Expressions for a calculator with variables.
-- The calculator supports literals Lit for any value domain,
-- an open ended set of primitive functions Fun, and
-- an open ended set of variables Var.
data CalcExprAST valuedomain
  = Lit valuedomain
  | Fun FunName [CalcExprAST valuedomain]
  | Var VarName
  deriving (Eq, Read, Show)


-- | Statement for declaring (setting) and changing (assigning) a variable
data CalcStmtAST valuedomain
  = SetVar VarName (CalcExprAST valuedomain)
  | AssVar VarName (CalcExprAST valuedomain)
  deriving (Eq, Read, Show)

-----------------------
-- | Check that an expression is compatible with a given signature.
-- Returns a list of undeclared functions and variables used in the expression.
-- The check infers the type of each subexpression, and collates the problems in a list.
typeCheckExpr :: Signature -> TypeEnvironment -> ValueType valuedomain -> CalcExprAST valuedomain -> (TypeName,[FunName])
typeCheckExpr sig@(types,fundecls) tenv valtyp (Lit val) = (valtyp val,[])
typeCheckExpr sig@(types,fundecls) tenv valtyp fcall@(Fun fn exprs) = typeCheckExpr' fundecls fcall
  where
    typeCheckExpr' :: [FunDeclaration] -> CalcExprAST valuedomain -> (TypeName,[FunName])
    typeCheckExpr' ((fname,params,res,doc):fundecls) fcall@(Fun fn exprs)
      = if fname == fn && params == typs
        then (res,errs)
        else typeCheckExpr' fundecls fcall
    typeCheckExpr' [] fcall@(Fun fn exprs) = 
      if elem "" typs
        then ("",show fn:errs)
        else ("",("("++(show fn)++","++(show typs)++","++(show "?")++")"):errs)
    typeCheckExpr' _ _ = error "Not accessible"
    -- | Check subexpressions
    subexpr = (map (typeCheckExpr sig tenv valtyp) exprs)
    comb :: [(TypeName,[String])] -> ([TypeName],[String])
    comb ((t,es):tes) = (t:ts',es++es') where (ts',es') = comb tes
    comb [] = ([],[])
    (typs,errs) = comb subexpr
typeCheckExpr sig@(types,fundecls) tenv valtyp (Var vname) = 
  case lookup vname tenv of
    Nothing -> ("",["(Var "++show vname++")"])
    Just tname -> (tname,[])


-----------------------
-- | Unit test for typeCheckExpr
-- Contains an example of an AST and several related signatures.
unittestPam8TSignatureAST = do
  print $ "-- unittestPam8TSignatureAST --"
  let sig1 = ([("Int","")],[("Sub",["Int","Int"],"Int","")])::Signature
  let chs1 = checkSignature sig1 == []
  let sig2 = ([("Int","")],[("Mult",["Int","Int"],"Int",""),("Add",["Int"],"Int","")])::Signature
  let chs2 = checkSignature sig2 == []
  let sig3 = ([("Int","")],[("Neg",["Int"],"Int",""),("Sub",["Int","Int"],"Int",""),("Add",["Int","Int"],"Int","")])::Signature
  let chs3 = checkSignature sig3 == []
  let sig4 = ([("Int","")],[("Neg",["Int"],"Int",""),("Add",["Int","Int"],"Int",""),("Mult",["Int","Int"],"Int",""),("Sub",["Int","Int"],"Int","")])::Signature
  let chs4 = checkSignature sig4 == []
  let expr = Fun "Neg" [Fun "Mult" [Fun "Add" [(Lit 3),(Fun "Sub" [(Lit 7),(Lit 13)])],(Lit 19)]]
  let chsig = chs1 && chs2 && chs3 && chs4
  let valtyp _ = "Int"
  let ch1 = snd (typeCheckExpr sig1 newTypeEnvironment valtyp expr) == ["\"Neg\"","\"Mult\"","(\"Add\",[\"Int\",\"Int\"],\"?\")"]
  let ch2 = snd (typeCheckExpr sig2 newTypeEnvironment valtyp expr) == ["\"Neg\"","\"Mult\"","\"Add\"","(\"Sub\",[\"Int\",\"Int\"],\"?\")"]
  let ch3 = snd (typeCheckExpr sig3 newTypeEnvironment valtyp expr) ==["\"Neg\"","(\"Mult\",[\"Int\",\"Int\"],\"?\")"]
  let ch4 = snd (typeCheckExpr sig4 newTypeEnvironment valtyp expr) == []
  print $ if chsig && ch1 && ch2 && ch3 && ch4 then "OK" else "Not OK"
