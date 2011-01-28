#!/usr/bin/env runhaskell
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
-- | A simple script to do some very basic timing of the RNGs.

--   It is important that we also run established stastical tests on
--   these RNGs a some point...

import Codec.Encryption.BurtonRNGSlow as BS

import System.Random
import System.Posix (sleep)
import System.CPUTime (getCPUTime)

import System.CPUTime.Rdtsc
import Control.Concurrent
import Control.Monad (when)
import Control.Concurrent.Chan
import Control.Exception
import Data.IORef
import Data.List
import Data.Int
import Data.List.Split
import Text.Printf

loop :: RandomGen g => IORef Int -> (Int,g) -> IO b
loop !counter !(!n,!g) = 
  do -- modifyIORef counter (+1)d
     -- Incrementing counter strictly (avoiding stack overflow) is annoying:
     c <- readIORef counter
     let c' = c+1
     evaluate c'
     writeIORef counter c'
     
--     putStr (show n); putChar ' '
     loop counter (next g)

data NoopRNG = NoopRNG

-- I cannot *believe* there is not a standard call or an
-- easily-findable hackage library supporting locale-based printing of
-- numbers. [2011.01.28]
commaint :: Integral a => a -> String
commaint n = 
   reverse $
   concat $
   intersperse "," $ 
   chunk 3 $ 
   reverse (show n)

padleft n str | length str >= n = str
padleft n str | otherwise       = take (n - length str) (repeat ' ') ++ str

padright n str | length str >= n = str
padright n str | otherwise       = str ++ take (n - length str) (repeat ' ')


instance RandomGen NoopRNG where 
  split g = (g,g)
  next g  = (0,g)

timeone freq msg mkgen =
  do counter :: IORef Int <- newIORef 1
     tid <- forkIO $ loop counter (next$ mkgen 23852358661234)   
     threadDelay (1000*1000) -- One second
     killThread tid
     total <- readIORef counter

     let cycles_per :: Double = fromIntegral freq / fromIntegral total
         cycles_per_str = if cycles_per < 100 
			  then printf "%.2f" cycles_per
			  else commaint (round cycles_per)

     putStrLn$ "    "++ padleft 11 (commaint total) ++" random ints generated "++ padright 27 ("["++msg++"]") ++" ~ "
	       ++ cycles_per_str ++" cycles/int"


-- WARNING! This is not actually good enough.  Even if we use forkOS
-- we would have to go further and pin the OS thread (e.g. in linux)
-- to keep the OS from waking up the process on a different core.
measure_freq :: IO Int64
measure_freq = do 
   -- We measure the clock frequency on a bound thread.
   -- Otherwise we can get really screwy results from rdtsc if we
   -- migrate physical threads when we sleep or threadDelay.
   tmpchan <- newChan
   forkOS $ do 
      t1 <- rdtsc
      -- sleep 1
      threadDelay (1000*1000)
      t2 <- rdtsc
      -- Just to be careful let's make sure this doesn't happen (this was how I found the problem before forkOS):
      when (t2 < t1) $ 
        putStrLn$ "WARNING: rdtsc not monotonically increasing, first "++show t1++" then "++show t2++" on the same OS thread"
      writeChan tmpchan (if t2>t1 then t2-t1 else t1-t2)
   freq <- readChan tmpchan
   return (fromIntegral freq)

-- This version simply busy-waits to stay on the same core:
-- WOW! I'm STILL experiencing the non-monotonic rdtsc, even when not compiled with -threaded.
-- It can't be overflow can it?  The counter should be 64 bit...
measure_freq2 :: IO Int64
measure_freq2 = do 
  let second = 1000 * 1000 * 1000 * 1000 -- picoseconds are annoying
  t1 <- rdtsc 
  start <- getCPUTime
  let loop !n !last = 
       do t2 <- rdtsc 
	  when (t2 < last) $
	       putStrLn$ "COUNTERS WRAPPED "++ show (last,t2) 
	  cput <- getCPUTime		
	  if (cput - start < second) 
	   then loop (n+1) t2
	   else return (n,t2)
  (n,t2) <- loop 0 t1
  putStrLn$ "  Approx getCPUTime calls per second: "++ commaint n
  when (t2 < t1) $ 
    putStrLn$ "WARNING: rdtsc not monotonically increasing, first "++show t1++" then "++show t2++" on the same OS thread"

  return$ fromIntegral (t2 - t1)
  
main = do 

   putStrLn$ "How many random numbers can we generate in a second on one thread?"

   t1 <- rdtsc
   t2 <- rdtsc
   putStrLn ("  Cost of rdtsc (ffi call):    " ++ show (t2 - t1))

   freq <- measure_freq2
   putStrLn$ "  Approx clock frequency:  " ++ commaint freq

   putStrLn$ "  First, timing with System.Random interface:"
   timeone freq "constant zero gen" (const NoopRNG)
   timeone freq "System.Random stdGen" mkStdGen
   timeone freq "BurtonGenSlow/reference" mkBurtonGen_reference
   timeone freq "BurtonGenSlow" mkBurtonGen

   putStrLn$ "Finished."

