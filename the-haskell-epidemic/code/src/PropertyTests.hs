{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PropertyTests where

import Text.Parsec (parse)
import Test.QuickCheck
import Data.List (sort, delete)
import Json

prop_sortMonotonicity :: [Int] -> Bool
prop_sortMonotonicity list =
  isIncreasing (sort list)
  where
    isIncreasing (x : y : tail) = x <= y && isIncreasing (y:tail)
    isIncreasing _              = True

prop_sortPermutation :: [Int] -> Bool
prop_sortPermutation list =
  sort list `isPermutationOf` list
  where
    isPermutationOf (head:tail) other = head `elem` other && isPermutationOf tail (delete head other)
    isPermutationOf [] []             = True
    isPermutationOf _ _               = False

prop_parseRender :: Json -> Bool
prop_parseRender json =
  parse jsonValue "" (render json) == Right json

-- Aggregate all tests
return []
runTests :: IO Bool
runTests = $quickCheckAll


-------------------------
------ Generators -------
-------------------------
data Person = Person { name :: String, age :: Int }

instance Arbitrary Person where
  arbitrary = do
    name  <- arbitrary
    age   <- arbitrary
    return (Person name age)
