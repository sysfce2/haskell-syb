{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Perm (tests) where

{-

This module illustrates permutation phrases.
Disclaimer: this is a perhaps naive, certainly undebugged example.

-}

import Test.Tasty.HUnit

import Control.Applicative (Alternative(..))
import Control.Monad
import Data.Generics

---------------------------------------------------------------------------
-- We want to read terms of type T3 regardless of the order T1 and T2.
---------------------------------------------------------------------------

data T1 = T1       deriving (Show, Eq, Data)
data T2 = T2       deriving (Show, Eq, Data)
data T3 = T3 T1 T2 deriving (Show, Eq, Data)


---------------------------------------------------------------------------
-- A silly monad that we use to read lists of constructor strings.
---------------------------------------------------------------------------

-- Type constructor
newtype ReadT a = ReadT { unReadT :: [String] -> Maybe ([String],a) }



-- Run a computation
runReadT :: ReadT a -> [String] -> Maybe a
runReadT x y = case unReadT x y of
                 Just ([],z) -> Just z
                 _           -> Nothing

-- Read one string
readT :: ReadT String
readT =  ReadT (\x -> case x of
                        []     -> Nothing
                        y : ys -> Just (ys, y)
               )

instance Functor ReadT where
  fmap  = liftM

instance Applicative ReadT where
  pure x = ReadT (\y -> Just (y,x))
  (<*>) = ap

instance Alternative ReadT where
  (<|>) = mplus
  empty = mzero

-- ReadT is a monad!
instance Monad ReadT where
  return   = pure
  c >>= f  = ReadT (\x -> case unReadT c x of
                            Nothing -> Nothing
                            Just (x', a) -> unReadT (f a) x'
                   )

-- ReadT also accommodates mzero and mplus!
instance MonadPlus ReadT where
  mzero = ReadT (const Nothing)
  f `mplus` g = ReadT (\x -> case unReadT f x of
                               Nothing -> unReadT g x
                               y -> y
                      )


---------------------------------------------------------------------------
-- A helper type to appeal to predicative type system.
---------------------------------------------------------------------------

newtype GenM = GenM { unGenM :: forall a. Data a => a -> ReadT a }


---------------------------------------------------------------------------
-- The function that reads and copes with all permutations.
---------------------------------------------------------------------------

buildT :: forall a. Data a => ReadT a
buildT = result

 where
  result = do str <- readT
              con <- string2constr str
              ske <- return $ fromConstr con
              fs  <- return $ gmapQ buildT' ske
              perm [] fs ske

  -- Determine type of data to be constructed
  myType = myTypeOf result
    where
      myTypeOf :: forall b. ReadT b -> b
      myTypeOf =  undefined

  -- Turn string into constructor
  string2constr str = maybe mzero
                            return
                            (readConstr (dataTypeOf myType) str)

  -- Specialise buildT per kid type
  buildT' :: forall b. Data b => b -> GenM
  buildT' (_::b) = GenM (const mzero `extM` const (buildT::ReadT b))

  -- The permutation exploration function
  perm :: forall b. Data b => [GenM] -> [GenM] -> b -> ReadT b
  perm [] [] a = return a
  perm fs [] a = perm [] fs a
  perm fs (f:fs') a = (
                        do a' <- gmapMo (unGenM f) a
                           perm fs fs' a'
                      )
                        `mplus`
                      (
                        do guard (not (null fs'))
                           perm (f:fs) fs' a
                      )


---------------------------------------------------------------------------
-- The main function for testing
---------------------------------------------------------------------------

tests :: Assertion
tests =
     ( runReadT buildT ["T1"] :: Maybe T1           -- should parse fine
   , ( runReadT buildT ["T2"] :: Maybe T2           -- should parse fine
   , ( runReadT buildT ["T3","T1","T2"] :: Maybe T3 -- should parse fine
   , ( runReadT buildT ["T3","T2","T1"] :: Maybe T3 -- should parse fine
   , ( runReadT buildT ["T3","T2","T2"] :: Maybe T3 -- should fail
   ))))) @=? output

output :: (Maybe T1, (Maybe T2, (Maybe T3, (Maybe T3, Maybe a))))
output = (Just T1,(Just T2,(Just (T3 T1 T2),(Just (T3 T1 T2),Nothing))))
