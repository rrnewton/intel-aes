{-|
   Module      :  Codec.Crypto.ConvertRNG
   Copyright   :  (c) Ryan Newton 2011
   License     :  BSD-style (see the file LICENSE)
   Maintainer  :  rrnewton@gmail.com
   Stability   :  experimental
   Portability :  portable, GHC

 This module bridges these three interfaces:

@
   Crypto.Classes.BlockCipher
   Crypto.Random.CryptoRandomGen
   System.Random.RandomGen
@

    Specifically, a block cipher can be converted to generate a
    'CryptoRandomGen', which in turn can be converted to provide the
    @RandomGen@ interface.

  -}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-# LANGUAGE  ScopedTypeVariables #-}
-- FlexibleInstances, EmptyDataDecls, FlexibleContexts, NamedFieldPuns, , ForeignFunctionInterface

module Codec.Crypto.ConvertRNG 
  ( BCtoCRG(..), convertCRG     
  , CRGtoRG()
  , CRGtoRG_Unbuffered(..) -- Inefficient version for testing...
  )
where

import System.Random 
import System.IO.Unsafe (unsafePerformIO)
import GHC.IO (unsafeDupablePerformIO)

-- import Data.List
import Data.Word
import Data.Tagged
import Data.Serialize

import qualified Data.Bits
import qualified Data.ByteString as B
-- import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as BI

-- import Crypto.Random.DRBG ()
-- import Crypto.Modes

import Crypto.Random (CryptoRandomGen(..), GenError(..), splitGen, genBytes)
import Crypto.Classes (BlockCipher(..), blockSizeBytes, for)
import Crypto.Types (ByteLength)

import Control.Monad
-- import Foreign.Ptr
import qualified Foreign.ForeignPtr as FP
import Foreign.Storable


----------------------------------------------------------------------------------------------------
-- Converting CryptoRandomGen to RandomGen
------------------------------------------

-- There's a potential overlapping instances problem here.  Someone
-- may want to do their own RandomGen instance, creating a problem
-- with this:
--
--   instance CryptoRandomGen g => RandomGen g where 
--
-- NOTE: The above would also be an undecidable instance.  Another
-- option is to have a type used just for lifting.  See below.


-- | Converting CryptoRandomGen to RandomGen.
--   This naive version is inefficent and should not be used in most cases.
data CRGtoRG_Unbuffered a = CRGtoRG_Unbuffered a
instance CryptoRandomGen g => RandomGen (CRGtoRG_Unbuffered g) where 
   next  (CRGtoRG_Unbuffered g) = 
--       case genBytes (max bytes_in_int (keyLength g `quot` 8)) g of 
       case genBytes bytes_in_int g of 
         Left err -> error$ "CryptoRandomGen genBytes error: " ++ show err
	 Right (bytes,g') -> 
           case decode bytes of 
	      Left err -> error$ "Deserialization error:"++ show err
	      Right n -> (n, CRGtoRG_Unbuffered g')
	     
   split (CRGtoRG_Unbuffered g) = 
       case splitGen g of 
         Left err      -> error$ "CryptoRandomGen splitGen error:"++ show err
	 Right (g1,g2) -> (CRGtoRG_Unbuffered g1, CRGtoRG_Unbuffered g2)

-- Another option would be to amortize overhead by generating a large
-- buffer of random bits at once.
-- data CRGtoRG a = CRGtoRG a BUFFER INDEX

-- Any better way to do this?
bytes_in_int = (round $ 1 + logBase 2 (fromIntegral (maxBound :: Int)))  `quot` 8
-- steps = 128 `quot` bits_in_int

------------------------------------------------------------
-- | Convert CryptoRandomGen to RandomGen.  This version is buffered.
--   That is, it fills a sizable buffer with random bits in bulk and
--   advances an index into that buffer as random bits are requested.
data CRGtoRG a = CRGtoRG a 
    {-#UNPACK#-}!         (FP.ForeignPtr Int)
    {-#UNPACK#-}!         Int

instance CryptoRandomGen g => RandomGen (CRGtoRG g) where 
   next (CRGtoRG g _ ind) | ind == bufsize = next (convertCRG g) -- Refill the buffer
   next (CRGtoRG g buf ind) = 
       -- As long as this memory is in use it will not be modified.
       -- The peek action should therefore be dupable:
       unsafeDupablePerformIO $ 
         FP.withForeignPtr buf $ \ ptr -> 
           do x <- peekElemOff ptr ind 
	      return (x, CRGtoRG g buf (ind+1))
	     
   split (CRGtoRG g buf ind) = 
       case splitGen g of 
         Left err      -> error$ "CryptoRandomGen splitGen error:"++ show err
	 Right (g1,g2) -> (CRGtoRG g1 buf ind, convertCRG g2)


-- | The constructor for CRGtoRG values.
convertCRG :: CryptoRandomGen g => g -> CRGtoRG g
convertCRG crg = CRGtoRG g' (FP.castForeignPtr ptr) 0
 where 
  (ptr,_,_)     = BI.toForeignPtr bs
  Right (bs,g') = genBytes (bufsize * bytes_in_int) crg


-- How many 8 byte chunks should we buffer each time?
-- TODO: Autotune this...
bufsize = 256



----------------------------------------------------------------------------------------------------
-- We would also like every BlockCipher to constitute a valid CryptoRandomGen.
-- Again there's the tension with UndecidableInstances vs explicit lifting.

-- | A BlockCipher can generate random numbers.
data BCtoCRG a = BCtoCRG a Word64
-- When lifting we include a counter which increments as random numbers are generated.

instance BlockCipher x => CryptoRandomGen (BCtoCRG x) where 
  newGen  bytes = case buildKey bytes of Nothing -> Left NotEnoughEntropy 
					 Just x  -> Right (BCtoCRG x 0)
  genSeedLength = let res = Tagged ((keyLength `for` cipherOf res) `div` 8)
                  in res
	where cipherOf :: Tagged (BCtoCRG x) y -> x
	      cipherOf = undefined

  -- If this is called for less than blockSize data there's some waste but it should work.
  genBytes req (BCtoCRG (bcgen :: x) counter) = 
      -- What's the most efficient way to do this?
      unsafePerformIO $ do  -- Potentially heavyweight... not allowing dupable.
--      unsafeDupablePerformIO $ do
	-- Number of times to stamp out the counter:
        let bsize = untag (blockSizeBytes :: Tagged x ByteLength)
	    numstamps = (req + 7) `quot` 8
	    numblocks = (req + bsize - 1) `quot` bsize
	    total     = max (numstamps * 8) (numblocks * bsize)

        -- putStrLn$ "[temp] requested "++show req++" bytes, stamping  "++show (numstamps*8)++
	-- 	  " into "++show numblocks++" block(s), output buf size "++show total

        buf :: FP.ForeignPtr Word64 <- FP.mallocForeignPtrBytes total
	FP.withForeignPtr buf $ \ptr -> 
	  forM_ [0..numstamps-1] $ \i -> 
	    pokeElemOff ptr i (counter + fromIntegral i)
        let cipher = encryptBlock bcgen (BI.fromForeignPtr (FP.castForeignPtr buf) 0 total)
	    newgen = BCtoCRG bcgen (counter + fromIntegral numstamps)
	-- At the end we may have requested more bytes than needed, so we might crop:
	if req==total then return$ Right (cipher, newgen)
	              else return$ Right (B.take req cipher, newgen)

  reseed bs (BCtoCRG k _) = newGen (xorExtendBS (encode k) bs)

xorExtendBS a b = B.append (B.pack$ B.zipWith Data.Bits.xor a b) rem
      where
      al = B.length a
      bl = B.length b
      rem | bl > al   = B.drop al b
          | otherwise = B.drop bl a

