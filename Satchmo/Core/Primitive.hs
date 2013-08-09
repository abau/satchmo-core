module Satchmo.Core.Primitive
  ( Primitive (..)
  , evaluateConstant, isConstant, assertOr, assertAnd, ifThenElse, ifThenElseM
  , select, antiSelect, fun2, fun3
  )
where

import Prelude hiding (not,and,or)
import Control.Monad (ap)
import Satchmo.Core.MonadSAT (MonadSAT (..))
import Satchmo.Core.Data (Literal)

-- |Class of primitives to build boolean formulas.
-- Minimal definition is @constant, evaluateConstant, primitive, assert, not, and@.
class (Show p,Eq p) => Primitive p where
  -- |Encodes a boolean value
  constant :: Bool -> p

  -- |Evaluates a primitive to either a literal or a constant
  evaluate :: p -> Either Literal Bool

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
  xor xs = foldT return xor2 xor3 xs
    where
      xor2 x y = do
        r <- primitive
        assert [ not x,     y ,     r ]
        assert [     x, not y ,     r ]
        assert [     x,     y , not r ]
        assert [ not x, not y , not r ]
        return r
      xor3 a b c = do
        x <- primitive
        let implies xs ys = assert $ map not xs ++ ys
        implies [not a, not b, not c] [not x]
        implies [not a, not b,     c] [    x]
        implies [not a,     b, not c] [    x]
        implies [not a,     b,     c] [not x]
        implies [    a, not b, not c] [    x]
        implies [    a, not b,     c] [not x]
        implies [    a,     b, not c] [not x]
        implies [    a,     b,     c] [    x]
        return x

  -- |Encodes equality
  equals :: MonadSAT m => [p] -> m p
  equals xs = return not `ap` xor xs

-- |@evaluateConstant p@ evaluates the value of @p@ if @p@ is constant
evaluateConstant :: (Primitive p) => p -> Maybe Bool
evaluateConstant p = case evaluate p of
  Right constant -> return constant
  _              -> Nothing

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
ifThenElse i t e = do
  r <- primitive
  assert [ not i, not t ,     r ]
  assert [ not i,     t , not r ]
  assert [     i, not e ,     r ]
  assert [     i,     e , not r ]
  return r

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

{-
foldB :: Monad m => (a -> m b) -> (b -> b -> m b) -> [a] -> m b
foldB u f xs = case xs of
    [ ] -> error "Satchmo.Core.Primitive.foldB"
    [x] -> u x
    _   -> do 
        let (pre,post) = splitAt (div (length xs) 2) xs
        a <- foldB u f pre ; b <- foldB u f post
        f a b
        -}

foldT :: Monad m => (a -> m b) -> (a -> a -> m b) -> (b -> b -> b -> m b) -> [a] -> m b
foldT u f g xs = case xs of
    [ ] -> error "Satchmo.Core.Primitive.foldT"
    [x] -> u x
    [x,y] -> f x y
    _   -> do 
        let n = length xs ; t = div n 3
            (pre,midpost) = splitAt t xs ; (mid,post) = splitAt t midpost
        a <- foldT u f g pre ; b <- foldT u f g mid ; c <- foldT u f g post
        g a b c
