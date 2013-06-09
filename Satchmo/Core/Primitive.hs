module Satchmo.Core.Primitive
where

import Prelude hiding (not,and,or)
import Control.Monad (ap,foldM)
import Satchmo.Core.MonadSAT (MonadSAT (..))

-- |Class of primitives to build boolean formulas.
-- Minimal definition is @constant, evaluateConstant, primitive, assert, not, and@.
class Show p => Primitive p where
  -- |Encodes a boolean value
  constant :: Bool -> p

  -- |@evaluateConstant p@ evaluates the value of @p@ if @p@ is constant
  evaluateConstant :: p -> Maybe Bool

  -- |Makes a primitive whose value is unknown
  primitive :: MonadSAT m => m p

  -- |Asserts a disjunction of primitives
  assert :: MonadSAT m => [p] -> m ()

  -- |Encodes negation
  not :: p -> p

  -- |Encodes conjunction
  and :: MonadSAT m => [p] -> m p

  -- |Encodes disjunction
  or :: MonadSAT m => [p] -> m p
  or []  =  return $ constant False
  or [x] =  return x
  or xs  =  return not `ap` and (map not xs)

  -- |Encodes implication
  implies :: MonadSAT m => p -> p -> m p
  implies a b = or [not a, b]

  -- |Encodes exclusive disjunction
  xor :: MonadSAT m => [p] -> m p
  xor []     = return $ constant False
  xor (x:xs) = foldM xor2 x xs
    where
      xor2 x y = do
        notBothFalse <- or [x,y]
        notBothTrue  <- return not `ap` and [x,y]
        and [ notBothFalse, notBothTrue ]

  -- |Encodes equality
  equals :: MonadSAT m => [p] -> m p
  equals xs = return not `ap` xor xs

-- |Checks whether a primitive is constant
isConstant :: (Primitive p) => p -> Bool
isConstant p = case evaluateConstant p of
  Nothing -> False
  Just _  -> True

-- |@assertOr = assert@
assertOr :: (MonadSAT m, Primitive p) => [p] -> m ()
assertOr = assert

-- |Asserts a conjunction of primitives
assertAnd :: (MonadSAT m, Primitive p) => [p] -> m ()
assertAnd = mapM_ $ assertOr . return

-- |Encodes a conditional expression
ifThenElse :: (MonadSAT m, Primitive p) => p -> p -> p -> m p
ifThenElse condition ifTrue ifFalse =
  and =<< sequence [ condition     `implies` ifTrue
                   , not condition `implies` ifFalse ]

-- |Monadic version of @ifThenElse@
ifThenElseM :: (MonadSAT m, Primitive p) => m p -> m p -> m p -> m p
ifThenElseM conditionM ifTrue ifFalse = do
  c <- conditionM
  t <- ifTrue
  f <- ifFalse
  ifThenElse c t f

-- |@select True p@ returns @p@, @select False p@ returns @not p@
select :: Primitive p => Bool -> p -> p
select True  p = p
select False p = not p

-- |@antiSelect True p@ returns @not p@, @antiSelect False p@ returns @p@
antiSelect :: Primitive p => Bool -> p -> p
antiSelect True  p = not p
antiSelect False p = p

-- |Encodes a function by giving a full CNF that determines the outcome
fun2 :: (MonadSAT m, Primitive p) => ( Bool -> Bool -> Bool ) -> p -> p -> m p
fun2 f x y = do
  r <- primitive
  sequence_ $ do
    a <- [ False, True ]
    b <- [ False, True ]
    return $ assertOr [ select a x
                      , select b y
                      , select (f a b) r ]
  return r

-- |Encodes the function by giving a full CNF that determines the outcome
fun3 :: (MonadSAT m, Primitive p) => ( Bool -> Bool -> Bool -> Bool ) 
                                  -> p -> p -> p -> m p
fun3 f x y z = do
  r <- primitive
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
