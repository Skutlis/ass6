-- | The notion of a signature and function model with documentation strings.
-- The calculator expressions are type-checked against the signature declarations.
-- This provides a setting for an open ended set of intrinsic (built in) functions.
--
-- Author Magne Haveraaen
-- Since 2020-03-19

module Pam8TSignature where

-----------------------
-- | A signature is a list of type and function declarations.
-- Each type used in a function declaration must be a declared type.
type Signature = ([TypeDeclaration],[FunDeclaration])

-- | A function declaration provides a function name, a list of parameter types, a return type
-- and a documentation string. The documentation string can have embedded newlines.
type FunDeclaration = (FunName,[TypeName],TypeName, DocString)

-- | A type declaration provides a type name and a documentation string. 
-- The documentation string can have embedded newlines.
type TypeDeclaration = (TypeName,DocString)

-----------------------
-- | The model for a function call is a mapping 
-- from the function name (String) and related argument list of valuedomain 
-- to a resulting valuedomain.
type FunModel valuedomain = FunName -> [valuedomain] -> valuedomain

-- | Creates a list of test data to match the parameter list of a function declaration.
type TestData valuedomain = [TypeName] -> [valuedomain]

-- | Determines the type of a value in the value domain.
type ValueType valuedomain = valuedomain -> TypeName

-----------------------
-- | Type environment: an association list between a variable/unit/etc and its type.
-- Typically a variable may be declared multiple times with possibly different types,
-- while a unit name should only be declared once and thus have a unique type.
type TypeEnvironment = [(String,TypeName)]

-- | An association list between variable names and their types.
-- A variable may be redeclared.
type VarEnvironment = TypeEnvironment

-----------------------
-- | Differentiating between the different purposes for strings

-- | Function names
type FunName = String
-- | Variable names
type VarName = String
-- | Type names
type TypeName = String
-- | Documentation strings
type DocString = String
-- | Unit names
type UnitName = String

-----------------------
-- | Consistency check for signatures:
-- Each type name in a function declaration must be a declared type.
-- Returns a list of all undeclared type names.
checkSignature :: Signature -> [TypeName]
checkSignature (types,(fname,params,res,doc):fundecls) =
  checkTypeLists types (res:params) ++ checkSignature (types,fundecls)
  where
    -- | Checks whether each name in the list of type names is declared.
    -- The type declarations are the first argument, the list of types the second.
    -- Returns a list of all misspelled type names (from the second list).
    checkTypeLists :: [TypeDeclaration] -> [TypeName] -> [TypeName]
    checkTypeLists types (typ:typs) 
      = if lookup typ types == Nothing then typ:problems else problems
        where problems = checkTypeLists types typs
    checkTypeLists types [] = [] 
checkSignature (types,[]) = []


-----------------------

-- | Checks if a test data generator produces data of the correct type for all types in the signature.
checkTestValues :: Signature -> TestData valuedomain -> ValueType valuedomain -> Bool 
checkTestValues sig testdata valuetype = map valuetype (testdata typelist) == typelist
  where typelist = map fst (fst sig)


-----------------------

-- | Create a new, empty type environment
newTypeEnvironment :: TypeEnvironment
newTypeEnvironment = []

-- | Checks if all type names in an environment have been declared in the signature.
-- Returns a list of all misspelled type names.
checkTypeEnvironment :: Signature -> TypeEnvironment -> [TypeName]
checkTypeEnvironment sig@(types,fdecls) ((var,typ):tenv) 
  = if lookup typ types == Nothing then typ:problems else problems
    where problems = checkTypeEnvironment sig tenv
checkTypeEnvironment sig [] = []

-- | Checks that the type environment is a mapping from strings to types.
-- Generally, a type environment is an association list from strings to type names,
-- and may contain multiple types for each string.
-- A mapping gives each string a unique type. 
-- Thus we need to check that each string appears only once in the environment.
checkUniqueMap :: TypeEnvironment -> [String]
checkUniqueMap ((str,typ):utm) = case lookup str utm of
  Nothing -> checkUniqueMap utm
  Just typ' -> str:checkUniqueMap utm
checkUniqueMap [] = []


-----------------------
-- | Checks that a function model provides a semantics for all functions declared in a signature.
-- Turns each function declaration in the signature into a call of the corresponding function,
-- in order to check the function model recognises a function and computes a result of the right type.
-- Uses a test data function which maps a parameter list to a corresponding list of values,
-- and a value type function which recognises a value and returns its type.
checkFunModel :: Signature -> FunModel valuedomain -> TestData valuedomain -> ValueType valuedomain -> [valuedomain]
checkFunModel (types,(fname,params,res,doc):fundecls) funmodel testfun valtyp
  = resval' :checkFunModel (types,fundecls) funmodel testfun valtyp
  where
    resval = funmodel fname (testfun params)
    resval' = if valtyp resval == res
      then resval
      else error $ "Function model computes wrong result type: expected=" ++ (show res) ++ " found=" ++ (show $ valtyp resval)
checkFunModel (types,[]) funmodel testfun valtyp = []

-----------------------
-- | Unit test for consistency checking signatures, type environments and function models:
-- checkSignature, checkFunModel and checkTypeEnvironment on an integer domain. 
unittestPam8TSignature = do
  print $ "-- unittestPam8TSignature --"
  let sig1 = ([],("f",["X","Y","Z"],"Int","doc f X Y Z"):[])::Signature
  let sig2 = ([("Integer","")],[("f",["Integer","Integer"],"Integer","doc f")])::Signature
  let sig3 = ([("X",""),("Z",""),("T","")],snd sig1)::Signature
  -- Internal consistency of a signature
  let ch1 = checkSignature sig1 == ["Int","X","Y","Z"]
  let ch2 = checkSignature sig2 == []
  let ch3 = checkSignature sig3 == ["Int","Y"]
  -- Function models and test data
  let fmod "f" [x,y] = x+y ; fmod "f" [x,y,z] = x*y*z ; fmod _ _ = error ""
  let testfun params = [10..9+length params]
  let valuetyp1 val = "Int"
  let valuetyp2 val = "Integer"
  let chtdv1 = checkTestValues sig1 testfun valuetyp1
  let chtdv2 = checkTestValues sig2 testfun valuetyp2
  let chfm1 = checkFunModel sig1 fmod testfun valuetyp1 == [1320]
  let chfm2 = checkFunModel sig2 fmod testfun valuetyp2 == [21]
  -- Checking variable types
  let vars = [("x","Integer"),("y","Integer")]::TypeEnvironment
  let chv = checkTypeEnvironment sig2 vars == []
  -- Collating the test results
  print $ if ch1 && ch2 && ch3 && chtdv1 && chtdv2 && chfm1 && chfm2 && chv 
    then "OK" 
    else "Not OK"
