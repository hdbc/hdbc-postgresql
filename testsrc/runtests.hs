{- arch-tag: Test runner
-}

module Main where 

import Test.HUnit
import Tests
import TestUtils

main = do printDBInfo
          runTestTT tests

