module Builders (tests) where

import Test.Tasty.HUnit

import Data.Generics.Builders


-- Main function for testing
tests :: Assertion
tests = ( constrs :: [Maybe Int]
        , constrs :: [String]
        , constrs :: [Either Int Double]
        , constrs :: [((), Integer)]
        ) @=? output

output :: ([Maybe Int], [String], [Either Int Double], [((), Integer)])
output = ([Nothing,Just 0],["","\NUL"],[Left 0,Right 0.0],[((),0)])
