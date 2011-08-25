#!/usr/bin/env runhaskell -i

-- Note the -i to prevent GHC from searching ./ as part of the library search path.

module Main where 
import System.Random.AES.Tests (runTests)

main = runTests
