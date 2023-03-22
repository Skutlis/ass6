-- | Calculator template for variable based integer calculator with explicit signature documentation.
-- At startup type checks the function model for conformance to the signature.
-- Type checks every user calculator expression against the function declarations in the signature.
--
-- Author Magne Haveraaen
-- Since 2020-03-19

module Pam8TCalculatorTemplate where

-- Use variable calculator with explicit signatures.
import Pam8TSignature
import Pam8TSignatureAST

-- Use interpreter with state and explicit signatures.
import Pam8TSignatureInterpreter
import Pam8State

-- Use editable console.
import System.Console.Haskeline
-- Use a Haskell parser wrapped in Maybe.
import Text.Read (readMaybe)


-----------------------
-- | Interactive calculator parameterised by function models.
-- First it is checked that the function model accepts the signature.
-- Then for every user provided statement, the calculator checks for defined variables and functions.
calculatorTemplate :: (Show valuedomain, Read valuedomain) =>
  Signature -> FunModel valuedomain -> TestData valuedomain -> ValueType valuedomain -> IO ()
calculatorTemplate intrinsics@(types,fundecls) funmod testfun valtyp = do
  putStrLn $ "Interactive calulator with variables and the following intrinsic functions"
  -- Check consistency of intrinsic signature
  if checkSignature intrinsics == []
    then putStrLn ""
    else error $ "Error: missing type declarations " ++ (show $ checkSignature intrinsics)
          ++ " for intrinsic signature " ++ (show intrinsics)
  -- Validate test functions for the type names
  let chtdv = checkTestValues intrinsics testfun valtyp
  -- Validate that all intrinsic functions have a model with expected return type in funmod
  -- (this has already been tested, but not performed, by Haskell's lazy evaluation).
  let chfm = map (\(_,_,res,_)->res) fundecls == map valtyp (checkFunModel intrinsics funmod testfun valtyp)
  if chtdv && chfm
    then putStrLn $ signatureToString intrinsics
    else error $ "Not all intrinsic functions have a model, sig=" ++ (show intrinsics)
  runInputT defaultSettings (loop newTypeEnvironment newState)
  where
  -- Parses and executes CalcStmtAST and prints what happens.
  -- The recursive call to loop must update the type environment and state.
  loop tenv state = do
    input <- getInputLine "¢ "
    case input of
      Nothing -> return ()
      Just "" ->
        do outputStrLn $ "Finished" ; return ()
      Just "show" -> 
        do outputStrLn $ "state = " ++ (show state) ; loop tenv state
      Just str -> do
        case readMaybe str of
          Nothing -> do
            outputStrLn $ "Not a statement: " ++ str
            outputStrLn $ "Statement forms are"
            outputStrLn $ "  SetVar \"vname\" (CalcExpr)"
            outputStrLn $ "  AssVar \"vname\" (CalcExpr)"
            outputStrLn $ "CalcExpr forms are"
            outputStrLn $ "  Lit value"
            outputStrLn $ "  Fun \"fname\" [arg_1,..,arg_n]"
            outputStrLn $ "  Var \"vname\""
            outputStrLn $ "See the signature for a listing of function names and their arguments."
            loop tenv state
          Just stmt@(SetVar vname expr)|isDeclared vname state -> do
            outputStrLn $ "Error: output variable " ++ (show vname) ++ " already exists."
            checkExpression state intrinsics tenv valtyp expr
            loop tenv state
          Just stmt@(SetVar vname expr)|[]<-allDeclared state expr,(res,[])<-typeCheckExpr intrinsics tenv valtyp expr -> do
            outputStrLn $ "SetVar " ++ (show vname) ++ " = " ++ (show $ evaluate funmod state expr)
            loop ((vname,res):tenv) $ execute funmod stmt state
          Just stmt@(SetVar vname expr) -> do
            let undeclared = allDeclared state expr
            checkExpression state intrinsics tenv valtyp expr
            loop tenv state
          Just stmt@(AssVar vname expr)|isDeclared vname state,[]<-allDeclared state expr,(res,[])<-typeCheckExpr intrinsics tenv valtyp expr, Just res' <- lookup vname tenv -> do
            if res == res'
              then outputStr ""
              else outputStrLn $ "Warning: type of variable " ++ (show vname) ++ " has changed from " ++ (show res') ++ " to " ++ (show res)
            outputStrLn $ "AssVar " ++ (show vname) ++ " = " ++ (show $ evaluate funmod state expr)
            loop ((vname,res):tenv) $ execute funmod stmt state
          Just stmt@(AssVar vname expr) -> do
            if isDeclared vname state
              then outputStr ""
              else outputStrLn $ "Error: output variable " ++ (show vname) ++ " has not been declared."
            checkExpression state intrinsics tenv valtyp expr
            loop tenv state
  -- | Check if an expression uses only declared functions and declared variables.
  checkExpression :: State valuedomain -> Signature -> TypeEnvironment -> ValueType valuedomain -> CalcExprAST valuedomain -> InputT IO ()
  checkExpression state intrinsics tenv valtyp expr = do
    let undeclared = typeCheckExpr intrinsics tenv valtyp expr
    if undeclared == ("",[])
      then outputStr ""
      else outputStrLn $ "Syntax error, undeclared functions/variables: " 
        ++ (foldl listargcomma "" $ snd $ undeclared)
    let undeclared = allDeclared state expr
    if undeclared == []
      then outputStr ""
      else outputStrLn $ "Error: expression contains undeclared variables " ++ (show undeclared)


-----------------------
-- | Pretty print a signature
signatureToString :: Signature -> String
signatureToString sig@(types,fundecls) =
  -- Print type declarations with documentation
  foldl (++) "" 
    (map 
     (\(typ,doc) -> (printdoc doc ++ "  type " ++ typ ++ "\n")) 
     types
    ) ++
  -- Print function declarations with documentation
  foldl (++) "" 
    (map 
     (\(fn,args,res,doc) -> (printdoc doc ++ "  " ++ fn ++ " :: " ++
                         (foldr listargcomma "" args) ++ " -> " ++ res ++ "\n")) 
     fundecls
    )

-- | Create a (multiline) documentation with "  -- |" prefix at each line.
printdoc :: DocString -> String
printdoc doc = res
   where
     strs = splitOn '\n' doc
     lines = map (\str -> "  -- | " ++ str ++ "\n") strs
     res = foldr (++) "" lines

-- | Split a list into a list of lists on the delimiter element.
-- For instance splitOn 0 [1,2,0,4,0,0,5,7] = [[1,2],[4],[],[5,7]]
splitOn :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitOn delimiter list = foldr f [[]] list
  where
    f c (x:xs) | c == delimiter = []:(x:xs)
               | otherwise = (c:x):xs
    f c []     = error "Will never be called"

-- | Insert a comma between two nonempty strings.
listargcomma :: String -> String -> String
listargcomma "" "" = ""
listargcomma str1 "" = str1
listargcomma "" str2 = str2
listargcomma str1 str2 = str1 ++ ", " ++ str2
