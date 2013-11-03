{-# LANGUAGE TemplateHaskell #-}
import SimpleDTime
import Test.QuickCheck
import Test.QuickCheck.All

prop_test1 dt = succMonth dt == addMonth dt 1

prop_test2 dt = addMonth dt 2 > addMonth dt 1

prop_test3 dt = dt < addMonth dt 1

prop_test4 dt = dt > addMonth dt (-1)

prop_test5 dt = addMonth dt (-2) < addMonth dt (-1)

prop_test6 dt = addYear dt 1 == addMonth dt 12
prop_test7 dt = addYear dt (-1) == addMonth dt (-12)

prop_test8 dt = addYear dt 1 > addMonth dt 11
prop_test9 dt = addYear dt 1 < addMonth dt 13

prop_test10 dt = addYear dt (-1) < addMonth dt (-11)
prop_test11 dt = addYear dt (-1) > addMonth dt (-13)

prop_test12 dt = month dt == month (addMonth dt 12)

instance Arbitrary DTime where
  arbitrary = do
    y <- choose (1970, 2048) :: Gen Int
    m <- choose (1, 12) :: Gen Int
    d <- choose (1, 28) :: Gen Int
    return (DTime y m d)

runTests = $quickCheckAll

main = runTests
