#!/usr/bin/env runhaskell
import Distribution.Simple


main = do
  putStrLn$ "*** Running Setup.hs..."
  defaultMainWithHooks simpleUserHooks
