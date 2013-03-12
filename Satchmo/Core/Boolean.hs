module Satchmo.Core.Boolean
where

import           Prelude hiding (not,and,or)
import qualified Prelude as P
import           Control.Monad (foldM,ap,when)
import           Data.List (partition)
import           Satchmo.Core.Data (Literal,clause)
import qualified Satchmo.Core.Data as D
import           Satchmo.Core.MonadSAT (MonadSAT,fresh,emit)

data Boolean = Boolean  { encode ::  Literal }
             | Constant { value  :: !Bool }
             deriving (Show)

isConstant :: Boolean -> Bool
isConstant (Constant {}) = True
isConstant _             = False

boolean :: MonadSAT m => m Boolean
boolean = return Boolean `ap` fresh

constant :: Bool -> Boolean
constant = Constant

not :: Boolean -> Boolean
not boolean = case boolean of
  Boolean literal   -> Boolean  $ D.not literal
  Constant constant -> Constant $ P.not constant

assert :: MonadSAT m => [Boolean] -> m ()
assert booleans = 
  let (constants,rest) = partition isConstant booleans
      constantValue    = P.or $ map value constants
  in
    when (P.not constantValue) $ emit $ clause $ map encode rest

assertOr :: MonadSAT m => [Boolean] -> m ()
assertOr = assert

assertAnd :: MonadSAT m => [Boolean] -> m ()
assertAnd = mapM_ $ assertOr . return

and :: MonadSAT m => [Boolean] -> m Boolean
and booleans = case booleans of
  []  -> return $ constant True
  [x] -> return x
  xs  -> do y <- boolean                                  
            sequence_ $ do x <- xs                        -- y -> xs
                           return $ assertOr [not y, x]
            assertOr $ y : map not xs                     -- y <- xs
            return y

or :: MonadSAT m => [Boolean] -> m Boolean
or booleans = case booleans of
  []  -> return $ constant False
  [x] -> return x
  xs  -> return not `ap` and (map not xs)

xor :: MonadSAT m => [ Boolean ] -> m Boolean
xor booleans = case booleans of
  []     -> return $ constant False
  (x:xs) -> foldM xor2 x xs

equals :: MonadSAT m => [ Boolean ] -> m Boolean
equals booleans = case booleans of
  []     -> return $ constant True
  [_]    -> return $ constant True
  (x:xs) -> foldM equals2 x xs

equals2 :: MonadSAT m => Boolean -> Boolean -> m Boolean
equals2 a b = return not `ap` xor2 a b

xor2 :: MonadSAT m => Boolean -> Boolean -> m Boolean
xor2 = fun2 (/=)

implies :: MonadSAT m => Boolean -> Boolean -> m Boolean
implies a b = or [not a, b]

ifThenElse :: MonadSAT m => Boolean -> m Boolean -> m Boolean -> m Boolean
ifThenElse condition ifTrue ifFalse = do
  trueBranch  <- ifTrue
  falseBranch <- ifFalse
  and =<< sequence [ condition     `implies` trueBranch
                   , not condition `implies` falseBranch ]

ifThenElseM :: MonadSAT m => m Boolean -> m Boolean -> m Boolean -> m Boolean
ifThenElseM conditionM ifTrue ifFalse = do
  c <- conditionM
  ifThenElse c ifTrue ifFalse

-- |@select True b@ returns @b@, @select False b@ returns @not b@
select :: Bool -> Boolean -> Boolean
select True  b = b
select False b = not b

-- |@antiSelect True b@ returns @not b@, @antiSelect False b@ returns @b@
antiSelect :: Bool -> Boolean -> Boolean
antiSelect True  b = not b
antiSelect False b = b

-- | Implement a function by giving a full CNF that determines the outcome
fun2 :: MonadSAT m => ( Bool -> Bool -> Bool ) -> Boolean -> Boolean -> m Boolean
fun2 f x y = do
  r <- boolean
  sequence_ $ do
    a <- [ False, True ]
    b <- [ False, True ]
    return $ assertOr [ select a x
                      , select b y
                      , select (f a b) r ]
  return r

-- | Implement the function by giving a full CNF that determines the outcome
fun3 :: MonadSAT m => ( Bool -> Bool -> Bool -> Bool ) 
                   -> Boolean -> Boolean -> Boolean
                   -> m Boolean
fun3 f x y z = do
  r <- boolean
  sequence_ $ do
    a <- [ False, True ]
    b <- [ False, True ]
    c <- [ False, True ]
    return $ assertOr
        [ select a x
        , select b y
        , select c z
        , select (f a b c) r 
        ]
  return r

