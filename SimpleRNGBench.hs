#!/usr/bin/env runhaskell
{-# LANGUAGE BangPatterns, ScopedTypeVariables, ForeignFunctionInterface #-}
-- | A simple script to do some very basic timing of the RNGs.

--   It is important that we also run established stastical tests on
--   these RNGs a some point...

module Main where

import qualified Codec.Encryption.BurtonRNGSlow as BS

--import qualified Codec.Crypto.IntelAES.GladmanAES  as GA
import qualified Codec.Crypto.GladmanAES  as GA
import qualified Codec.Crypto.IntelAES.AESNI       as NI
import qualified Codec.Crypto.IntelAES             as IA
import qualified Codec.Crypto.ConvertRNG           as CR
-- import qualified Codec.Crypto.AES.Random        as Svein

import System.Exit (exitSuccess, exitFailure)
import System.Environment
import System.Random
import System.Posix    (sleep)
import System.CPUTime  (getCPUTime)
-- import Data.Time.Clock (diffUTCTime)
import System.CPUTime.Rdtsc
import System.Console.GetOpt

import GHC.Conc
import Control.Concurrent
import Control.Monad 
import Control.Concurrent.Chan
import Control.Exception

import Crypto.Random (CryptoRandomGen(..))

import Data.IORef
import Data.List
import Data.Int
import Data.Word
import Data.List.Split
import Data.Serialize
import qualified Data.ByteString as B
import Text.Printf

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable (peek,poke)

import Benchmark.BinSearch

----------------------------------------------------------------------------------------------------
-- TEMP: MOVE ME ELSEWHERE:

mkAESGen_gladman :: Int -> CR.CRGtoRG (CR.BCtoCRG (GA.AES GA.N128))
mkAESGen_gladman int = CR.convertCRG gen
 where
  Right (gen :: CR.BCtoCRG (GA.AES GA.N128)) = newGen (B.append halfseed halfseed )
  halfseed = encode word64
  word64 = fromIntegral int :: Word64


mkAESGen_gladman0 :: Int -> CR.CRGtoRG0 (CR.BCtoCRG (GA.AES GA.N128))
mkAESGen_gladman0 int = CR.CRGtoRG0 gen
 where
  Right (gen :: CR.BCtoCRG (GA.AES GA.N128)) = newGen (B.append halfseed halfseed )
  halfseed = encode word64
  word64 = fromIntegral int :: Word64


----------------------------------------------------------------------------------------------------
-- Miscellaneous helpers:

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

fmt_num n = if n < 100 
	    then printf "%.2f" n
	    else commaint (round n)

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
--
-- UPDATE: [2011.01.28] This was a bug in the rdtsc package that dropping the precision to 32 bits.
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

----------------------------------------------------------------------------------------------------
-- Drivers to get random numbers repeatedly.

incr !counter = 
  do -- modifyIORef counter (+1)d
     -- Incrementing counter strictly (avoiding stack overflow) is annoying:
     c <- readIORef counter
     let c' = c+1
     evaluate c'
     writeIORef counter c'     

loop :: RandomGen g => IORef Int -> (Int,g) -> IO b
loop !counter !(!n,!g) = 
  do incr counter
--     putStr (show n); putChar ' '
     loop counter (next g)

data NoopRNG = NoopRNG
instance RandomGen NoopRNG where 
  split g = (g,g)
  next g  = (0,g)

--foreign import ccall "cbits/c_test.c" blast_rands :: Ptr Int -> Ptr Int -> IO ()

type Kern = Int -> Ptr Int -> IO ()

-- [2011.01.28] Changing this to take "count" and "accumulator ptr" as arguments:
foreign import ccall "cbits/c_test.c" blast_rands :: Kern
foreign import ccall "cbits/c_test.c" store_loop  :: Kern
foreign import ccall unsafe "stdlib.hs" rand :: IO Int

loop2 :: IORef Int -> IO ()
loop2 !counter = 
  do incr counter
     n <- rand
     loop2 counter 


----------------------------------------------------------------------------------------------------
-- Timing:

timeit numthreads freq msg mkgen =
  do 
     counters <- forM [1..numthreads] (const$ newIORef 1) 
     tids <- forM counters $ \counter -> 
	        forkIO $ loop counter (next$ mkgen 23852358661234)   
     threadDelay (1000*1000) -- One second
     mapM_ killThread tids

     finals <- mapM readIORef counters
     let mean :: Double = fromIntegral (foldl1 (+) finals) / fromIntegral numthreads
         cycles_per :: Double = fromIntegral freq / mean
     print_result (round mean) msg cycles_per

print_result total msg cycles_per = 
     putStrLn$ "    "++ padleft 11 (commaint total) ++" random ints generated "++ padright 27 ("["++msg++"]") ++" ~ "
	       ++ fmt_num cycles_per ++" cycles/int"


-- FIXME: This isn't working yet because when the C call goes into a tight infinite loop killThread hangs...
-- KEEPING AROUND ONLY FOR FUTURE INVESTIGATION
------------------------------------------------------------
time_c :: Int -> Int64 -> (Ptr Int -> Ptr Int -> IO ()) -> IO Int
time_c numthreads freq ffn = do 
  counter :: ForeignPtr Int <- mallocForeignPtr
  sum     :: ForeignPtr Int <- mallocForeignPtr
  tid <- forkOS $ 
	 withForeignPtr counter $ \ cntr ->
	 withForeignPtr sum $ \ sm ->
          ffn cntr sm
  threadDelay (1000*1000) -- One second
  stat <- threadStatus tid
  putStrLn$ "Thread making foreign call's status: "++ show stat
  putStrLn$ "Killing thread running the foreign C call...\n"
  killThread tid -- This will hang!!!
  putStrLn$ "Killed.\n"
  total <- withForeignPtr counter peek 
  putStrLn$ "Got total: " ++ show total
  return total
------------------------------------------------------------


-- This version flips things around, and assume something about the
-- timing so that we can run a fixed number of randoms and time it.
-- (We could do binary search here.)
time_c2 :: Int -> Int64 -> String -> (Int -> Ptr Int -> IO ()) -> IO Int
time_c2 numthreads freq msg ffn = do 
  ptr     :: ForeignPtr Int <- mallocForeignPtr

  let kern = if numthreads == 1
	     then ffn
	     else replicate_kernel numthreads ffn 
      wrapped n = withForeignPtr ptr (kern$ fromIntegral n)
  (n,t) <- binSearch False 1 (1.0, 1.05) wrapped

  -- ONLY if we're in multi-threaded mode do we then run again with
  -- that input size on all threads:
----------------------------------------
-- NOTE, this approach is TOO SLOW.  For workloads that take a massive
-- parallel slowdown it doesn't make sense to use the same input size
-- in serial and in parallel.
-- DISABLING:
{-
  (n2,t2) <- 
    if numthreads > 1 then do
      ptrs <- mapM (const mallocForeignPtr) [1..numthreads]
      tmpchan <- newChan
      putStrLn$ "       [forking threads for multithreaded measurement, input size "++ show n++"]"
      start <- getCPUTime
      tids <- forM ptrs $ \ptr -> forkIO $ 
	       do withForeignPtr ptr (ffn$ fromIntegral n)
		  writeChan tmpchan ()     
      forM ptrs $ \_ -> readChan tmpchan
      end <- getCPUTime
      let t2 :: Double = fromIntegral (end-start) / 1000000000000.0
      putStrLn$ "       [joined threads, time "++ show t2 ++"]"
      return (n * fromIntegral numthreads, t2)
    else do 
      return (n,t)
-}
----------------------------------------

  let total_per_second = round $ fromIntegral n * (1 / t)
      cycles_per = fromIntegral freq * t / fromIntegral n
  print_result total_per_second msg cycles_per
  return total_per_second

-- This lifts the C kernel to operate 
replicate_kernel :: Int -> Kern -> Kern
replicate_kernel numthreads kern n ptr = do
  ptrs <- forM [1..numthreads]
	    (const mallocForeignPtr) 
  tmpchan <- newChan
  -- let childwork = ceiling$ fromIntegral n / fromIntegral numthreads
  let childwork = n -- Keep it the same.. interested in per-thread throughput.
  -- Fork/join pattern:
  tids <- forM ptrs $ \ptr -> forkIO $ 
	   withForeignPtr ptr $ \p -> do
	      kern (fromIntegral childwork) p
	      result <- peek p
	      writeChan tmpchan result

  results <- forM [1..numthreads] $ \_ -> 
	       readChan tmpchan
  -- Meaningless semantics here... sum the child ptrs and write to the input one:
  poke ptr (foldl1 (+) results)
  return ()

----------------------------------------------------------------------------------------------------
-- Main Script

data Flag = NoC | Help | Test
  deriving (Show, Eq)

options = 
   [ Option ['h']  ["help"]  (NoArg Help)  "print program help"
   , Option []     ["noC"]   (NoArg NoC)   "omit C benchmarks, haskell only"
   , Option ['t']  ["test"]  (NoArg Test)  "run some basic tests"
   ]

  
main = do 
   argv <- getArgs
   let (opts,_,other) = getOpt Permute options argv

   when (Test `elem` opts)$ do
       NI.testIntelAES
       exitSuccess

   when (not$ null other) $ do
       putStrLn$ "ERROR: Unrecognized options: " 
       mapM_ putStr other
       exitFailure

   when (Help `elem` opts) $ do
       putStr$ usageInfo "Benchmark random number generation" options
       exitSuccess

   putStrLn$ "\nHow many random numbers can we generate in a second on one thread?"

   t1 <- rdtsc
   t2 <- rdtsc
   putStrLn ("  Cost of rdtsc (ffi call):    " ++ show (t2 - t1))

   freq <- measure_freq2
   putStrLn$ "  Approx clock frequency:  " ++ commaint freq

--   svein <- Svein.newAESGen

   let gamut th = do
       putStrLn$ "  First, timing with System.Random interface:"
       timeit th freq "constant zero gen" (const NoopRNG)
       timeit th freq "System.Random stdGen" mkStdGen
       timeit th freq "PureHaskell/reference" BS.mkBurtonGen_reference
       timeit th freq "PureHaskell"           BS.mkBurtonGen
--       timeit th freq "Gladman inefficient"     GA.mkAESGen0
--       timeit th freq "Gladman"                 GA.mkAESGen
--       timeit th freq "Svein's Gladman package" (const svein)
       timeit th freq "IntelAES inefficient"    NI.mkAESGen0
       timeit th freq "IntelAES"                NI.mkAESGen
       timeit th freq "Compound gladman/intel"  IA.mkAESGen

       when (not$ NoC `elem` opts) $ do
	  putStrLn$ "  Comparison to C's rand():"
	  time_c2 th freq "ptr store in C loop"   store_loop
	  time_c2 th freq "rand/store in C loop"  blast_rands
	  time_c2 th freq "rand in Haskell loop" (\n ptr -> forM_ [1..n]$ \_ -> rand )
	  time_c2 th freq "rand/store in Haskell loop"  (\n ptr -> forM_ [1..n]$ \_ -> do n <- rand; poke ptr n )
	  return ()
          -- timeit 1 freq "rand / Haskell loop" mkBurtonGen

   gamut 1

   when (numCapabilities > 1) $ do 
--   when (False) $ do 
       putStrLn$ "\nNow "++ show numCapabilities ++" threads, reporting mean randoms-per-second-per-thread:"
       gamut numCapabilities
       return ()

   putStrLn$ "Finished."
