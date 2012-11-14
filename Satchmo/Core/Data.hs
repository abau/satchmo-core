module Satchmo.Core.Data
  ( Literal, variable, isPositive, literal, not
  , Clause, clause, literals
  , CNF, cnf, clauses, numClauses, numVariables, toDimacs
  )
where

import           Prelude hiding (not)
import qualified Prelude as P
import           Data.Word (Word)
import qualified Data.Set as S

type Variable = Word
data Literal  = Literal { variable   :: Variable
                        , isPositive :: Bool
                        } deriving (Show)

literal :: Bool -> Variable -> Literal
literal pos var = Literal var pos

not :: Literal -> Literal
not literal = 
  literal { isPositive = P.not $ isPositive literal }

newtype Clause = Clause { literals :: [Literal] }
                 deriving (Show)

clause :: [Literal] -> Clause
clause = Clause

newtype CNF = CNF { clauses :: [Clause]  } 
              deriving (Show)

cnf :: [Clause] -> CNF
cnf = CNF

numClauses :: CNF -> Int
numClauses = length . clauses

numVariables :: CNF -> Int
numVariables = 
  let addTo variableSet clause =
        let variables = map variable $ literals clause
        in
          variableSet `S.union` (S.fromList variables)
  in
    S.size . foldl addTo S.empty . clauses

toDimacs :: CNF -> String
toDimacs cnf = 
  unlines $ unwords [ "p","cnf"
                    , show $ numVariables cnf
                    , show $ numClauses   cnf
                    ]
          : (map clauseToDimacs $ clauses cnf)
  where 
    clauseToDimacs clause = 
      let literals' = map literalToDimacs $ literals clause
      in
        unwords $ literals' ++ ["0"]

    literalToDimacs literal =
      (if isPositive literal then ""
                             else "-")
      ++ (show $ variable literal)
