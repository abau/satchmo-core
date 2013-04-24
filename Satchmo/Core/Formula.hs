{-# LANGUAGE FlexibleContexts #-}
module Satchmo.Core.Formula
  ( Formula (..), toBoolean, atom, decodeFormula, renderAscii, render
  , module Satchmo.Core.Primitive)
where

import           Prelude hiding (not,and,or)
import qualified Prelude as P
import           Data.List (intercalate)
import           Data.Maybe (fromJust)
import           Satchmo.Core.Boolean (Boolean (..))
import           Satchmo.Core.MonadSAT (MonadSAT)
import           Satchmo.Core.Primitive 
import           Satchmo.Core.Data (Literal (..))
import           Satchmo.Core.Decode (Decode,decode)

data Formula = Atom     Boolean
             | Not      Formula
             | And     [Formula]
             | Or      [Formula]
             | Implies  Formula Formula
             | Xor     [Formula]
             | Equiv   [Formula]
             deriving (Eq,Ord)

instance Primitive Formula where
  constant = Atom . constant

  evaluateConstant formula = case formula of
    Atom a   -> evaluateConstant a

    Not f    -> fmap P.not $ evaluateConstant f

    And fs   -> if all isConstant fs
                then Just $ all (fromJust . evaluateConstant) fs
                else Nothing

    Or fs    -> if all isConstant fs
                then Just $ any (fromJust . evaluateConstant) fs
                else Nothing

    Implies   a b -> do a' <- evaluateConstant a
                        b' <- evaluateConstant b
                        return ( (P.not a') || b' )
    
    Xor fs   -> if all isConstant fs
                then Just $ foldl1 xor2 $ map (fromJust . evaluateConstant) fs
                else Nothing

    Equiv fs -> if all isConstant fs
                then Just $ foldl1 (==) $ map (fromJust . evaluateConstant) fs
                else Nothing

  primitive   = primitive >>= return . Atom  
  assert xs   = mapM toBoolean xs >>= assert

  not         =          Not
  and         = return . And
  or          = return . Or
  implies a b = return $ Implies a b
  xor         = return . Xor
  equals      = return . Equiv

instance Show Formula where
  show = renderAscii

-- |Encodes a formula
toBoolean :: MonadSAT m => Formula -> m Boolean
toBoolean formula = case formula of
  Atom a      -> return a
  Not  f      -> toBoolean f >>= return . not
  And fs      -> mapM toBoolean fs >>= and
  Or  fs      -> mapM toBoolean fs >>= or
  Implies a b -> mapM toBoolean [a,b] >>= \[a',b'] -> implies a' b'
  Xor fs      -> mapM toBoolean fs >>= xor
  Equiv fs    -> mapM toBoolean fs >>= equals

-- |@atom = primitive@ but with non-ambiguous type
atom :: MonadSAT m => m Formula
atom = primitive

-- |Decodes a formula. Putting this into a @Decode@ instance would require
-- @UndecidableInstances@.
decodeFormula :: Decode m Boolean Bool => Formula -> m Bool
decodeFormula formula = case formula of
  Atom a      -> decode a
  Not f       -> decodeFormula f          >>= return . P.not
  And fs      -> mapM decodeFormula fs    >>= return . P.and
  Or fs       -> mapM decodeFormula fs    >>= return . P.or
  Implies a b -> mapM decodeFormula [a,b] >>= \[a',b'] -> return $ (P.not a') || b'
  Xor fs      -> mapM decodeFormula fs    >>= \(x:xs)  -> return $ foldl xor2 x xs
  Equiv fs    -> mapM decodeFormula fs    >>= \(x:xs)  -> return $ all (x ==) xs


xor2 :: Bool -> Bool -> Bool
xor2 a b = P.not $ a == b

-- |Renders a formula to ASCII
renderAscii :: Formula -> String
renderAscii = render rAtom 
                     ("!" ++)
                     (intercalate " /\\ ")
                     (intercalate " \\/ ")
                     (\a b -> unwords [a,"->",b])
                     (intercalate " != ")
                     (intercalate " == ")
  where 
    rAtom (Constant False) = "0"
    rAtom (Constant True)  = "1"
    rAtom (Boolean b) | isPositive b = rVariable $ variable b
    rAtom (Boolean b)                = "!" ++ (rVariable $ variable b)

    rVariable v = "x_" ++ show v

-- |Renders a formula
render :: (Boolean -> String)           -- ^How to render @Atom@
       -> (String -> String)            -- ^How to render @Not@
       -> ([String] -> String)          -- ^How to render @And@
       -> ([String] -> String)          -- ^How to render @Or@
       -> (String -> String -> String)  -- ^How to render @Implies@
       -> ([String] -> String)          -- ^How to render @Xor@
       -> ([String] -> String)          -- ^How to render @Equiv@
       -> Formula                       -- ^Formula to render
       -> String
render rAtom rNot rAnd rOr rImplies rXor rEquiv = go
  where 
    go formula = case formula of
      Atom a      -> rAtom a
      Not  f      -> rNot $ go' f
      And fs      -> rAnd $ map go' fs
      Or  fs      -> rOr  $ map go' fs
      Implies a b -> rImplies (go' a) (go' b)
      Xor fs      -> rXor $ map go' fs
      Equiv  fs   -> rEquiv  $ map go' fs

    go' formula = case formula of
      Atom {} -> go formula
      _       -> "(" ++ go formula ++ ")"
