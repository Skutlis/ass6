-- | A yarn calculator (faithfully following P8T): signature, model and calculator.
--
-- Author Magne Haveraaen
-- Since 2021-05-22 (yarn API) and 2022-03-31 (personnel calculator)
module IntrinsicsYarn1 where

-- Use signatures

-- Use the calculator template for signatures and function models.

import Pam8UCalculatorTemplate
import Pam8USignature
import Pam8USignatureAST

-----------------------

-- | Declaration of Signature and their argument list and return type.
yarnSignature :: Signature
yarnSignature =
  ( [ ("Amount", "The number of rolls of yarn (amount)"),
      ("Cost", "The total cost for a purchase of yarn (NOK)"),
      ("Density", "Density of thread (gram/meter)"),
      ("Length", "Length of a roll of yarn (meter)"),
      ("UnitCost", "The unit cost for yarn (NOK/meter)"),
      ("Weight", "Weight of a roll of yarn (gram)")
    ],
    [ ("Add", ["Cost", "Cost"], "Cost", "Add two costs"),
      ("Sub", ["Cost", "Cost"], "Cost", "Subtract two costs"),
      ("Mult", ["Density", "Length"], "Weight", "Compute weight from density and length"),
      ("Slash", ["Weight", "Density"], "Length", "Compute length from weight and density"),
      ("Slash", ["Weight", "Length"], "Density", "Compute density"),
      ("Add", ["Length", "Length"], "Length", "Add two lengths"),
      ("Sub", ["Length", "Length"], "Length", "Subtract two lengths"),
      ("Mult", ["Length", "Amount"], "Length", "Multiply length by amount"),
      ("Slash", ["Length", "Length"], "Amount", "Compute amount"),
      ("Mult", ["UnitCost", "Length"], "Cost", "Compute cost based on length"),
      ("Slash", ["Cost", "Length"], "UnitCost", "Compute unit cost from cost and length of yarn"),
      ("Add", ["Weight", "Weight"], "Weight", "Add two weights"),
      ("Sub", ["Weight", "Weight"], "Weight", "Subtract two weights"),
      ("Mult", ["Weight", "Amount"], "Weight", "Multiply weight by amount")
    ]
  )

-----------------------

-- | Semantics of chosen yarn Signature. Taking into account that a*b = b*a
yarnSemantics :: FunModel (Double, UnitName)
yarnSemantics "Add" [(i1, "NOK"), (i2, "NOK")] = (i1 + i2, "NOK")
yarnSemantics "Add" [(i1, "meter"), (i2, "meter")] = (i1 + i2, "meter")
yarnSemantics "Add" [(i1, "gram"), (i2, "gram")] = (i1 + i2, "gram")
yarnSemantics "Add" [(i1, n1), (i2, n2)] = error $ "Cannot add " ++ n1 ++ " with " ++ n2
yarnSemantics "Sub" [(i1, "NOK"), (i2, "NOK")] = (i1 - i2, "NOK")
yarnSemantics "Sub" [(i1, "meter"), (i2, "meter")] = (i1 - i2, "meter")
yarnSemantics "Sub" [(i1, "gram"), (i2, "gram")] = (i1 - i2, "gram")
yarnSemantics "Sub" [(i1, n1), (i2, n2)] = error $ "Cannot subtract " ++ n1 ++ " with " ++ n2
yarnSemantics "Mult" [(i1, "gram/meter"), (i2, "meter")] = (i1 * i2, "gram")
yarnSemantics "Mult" [(i1, "meter"), (i2, "gram/meter")] = (i1 * i2, "gram")
yarnSemantics "Mult" [(i1, "NOK/meter"), (i2, "meter")] = (i1 * i2, "NOK")
yarnSemantics "Mult" [(i1, "meter"), (i2, "NOK/meter")] = (i1 * i2, "NOK")
yarnSemantics "Mult" [(i1, "meter"), (i2, "amount")] = (i1 * i2, "meter")
yarnSemantics "Mult" [(i1, "amount"), (i2, "meter")] = (i1 * i2, "meter")
yarnSemantics "Mult" [(i1, "gram"), (i2, "amount")] = (i1 * i2, "gram")
yarnSemantics "Mult" [(i1, "amount"), (i2, "gram")] = (i1 * i2, "gram")
yarnSemantics "Mult" [(i1, n1), (i2, n2)] = error $ "Cannot multiply " ++ n1 ++ " with " ++ n2
yarnSemantics "Slash" [(i1, "gram"), (i2, "gram/meter")] = if i2 /= 0 then (i1 / i2, "meter") else error "Cannot divide by 0"
yarnSemantics "Slash" [(i1, "gram"), (i2, "meter")] = if i2 /= 0 then (i1 / i2, "gram/meter") else error "Cannot divide by 0"
yarnSemantics "Slash" [(i1, "meter"), (i2, "meter")] = if i2 /= 0 then (i1 / i2, "amount") else error "Cannot divide by 0"
yarnSemantics "Slash" [(i1, "NOK"), (i2, "meter")] = if i2 /= 0 then (i1 / i2, "NOK/meter") else error "Cannot divide by 0"
yarnSemantics "Slash" [(i1, n1), (i2, n2)] = error $ "Cannot divide " ++ n1 ++ " by " ++ n2
yarnSemantics fname alist = error $ "Unknown function name/arg list " ++ (show fname) ++ " " ++ (show alist)

-----------------------

-- | Function creating test data.
yarnTestData :: [TypeName] -> [(Double, UnitName)]
yarnTestData ("Amount" : xs) = (3, "amount") : yarnTestData xs
yarnTestData ("Cost" : xs) = (5, "NOK") : yarnTestData xs
yarnTestData ("Density" : xs) = (3, "gram/meter") : yarnTestData xs
yarnTestData ("Length" : xs) = (2, "meter") : yarnTestData xs
yarnTestData ("UnitCost" : xs) = (10, "NOK/meter") : yarnTestData xs
yarnTestData ("Weight" : xs) = (8, "gram") : yarnTestData xs
yarnTestData (x : xs) = error $ "UnitName " ++ x ++ " is no recognized!"
yarnTestData [] = []

-- | Inferring the type of a value
typeOfValue :: ValueType (Double, UnitName)
typeOfValue (i, "amount") = "Amount"
typeOfValue (i, "NOK") = "Cost"
typeOfValue (i, "gram/meter") = "Density"
typeOfValue (i, "meter") = "Length"
typeOfValue (i, "NOK/meter") = "UnitCost"
typeOfValue (i, "gram") = "Weight"
typeOfValue (i, n) = error $ "Unknown unit " ++ n

-----------------------

-- | Unit test of the yarn signature:
-- • For each type in the signature, check that test data function generates data of the expected type.
-- • For each function declaration in the signature check that there is a corresponding function model.
unittestIntrinsicsYarn = do
  print $ "did nothing"

-----------------------

-- | Interactive calculator with variables and given selection of yarn Signature.

-- | Run the following commands in sequence at the prompt
-- SetVar "a" (Fun ...)
-- show
main = do
  putStrLn $ "-- Calculator for yarn --"

  calculatorTemplate yarnSignature yarnSemantics yarnTestData typeOfValue
