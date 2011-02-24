{-# LANGUAGE PackageImports #-}
{- | 

   This module includes two all-haskell implementations of Burton
   Smith's algorithm for a statistically-sound binary tree of random
   number generators.  See the following thread:

   <http://www.mail-archive.com/haskell-cafe@haskell.org/msg83901.html>

   Generally, Codec.Crypto.IntelAES should be used in favor of this
   module, but it is included for benchmarking purposes.

-}

module Codec.Encryption.BurtonRNGSlow
    (
     mkBurtonGen_reference,
     mkBurtonGen
     -- Plus, instances exported of course.
    )
where 

import System.Random (RandomGen, next, split)
import Crypto.Random.DRBG ()

import Codec.Encryption.AES (encrypt)
import "largeword" Data.LargeWord

--------------------------------------------------------------------------------
-- Reference implementation.

-- | Type of random number generators
--   This is a very simple but extremely inefficient vesion.
data RNG_ref = RNG_ref {-# UNPACK #-} !Word128 -- Seed
                       {-# UNPACK #-} !Word128 -- Counter
next128 (RNG_ref k c) = (encrypt k c, RNG_ref k (c+1))

-- | This instance is inefficient because it creates 128bits of
--   randomness but only uses an Int-sized (32 or 64 bit) subset of
--   them.
instance RandomGen RNG_ref where
  next g = (fromIntegral n, g')
   where (n,g') = next128 g
  split g@(RNG_ref k c) = (g', mkBurtonGen_reference n)
   where (n,g') = next128 g

-- | Extra slow reference implementation.
mkBurtonGen_reference :: Word128 -> RNG_ref
mkBurtonGen_reference seed = RNG_ref seed 0

--------------------------------------------------------------------------------
bits_in_int = round $ 1 + logBase 2 (fromIntegral (maxBound :: Int))
steps = 128 `quot` bits_in_int

-- | Type representing a more efficient random number generator that
-- | still uses an all-Haskell implementation.
data RNG = RNG {-# UNPACK #-} !Word128 -- Seed
               {-# UNPACK #-} !Word128 -- Last batch of random bits generated.
	       {-# UNPACK #-} !Word128 -- Counter
	       {-# UNPACK #-} !Int     -- Phase/step

-- | The idea with this one is that once we generate 128 bits of
--   randomness we parcel it out into two or four ints.
mkBurtonGen :: Word128 -> RNG
mkBurtonGen seed = RNG seed (encrypt seed 0) 1 0

next128' (RNG k _ c _) = RNG k (encrypt k c) (c+1) 0

instance RandomGen RNG where
  -- In this scenario its time to generate a new batch of bits:
  --  next g@(RNG k bits c s) | s == steps = next (next128' g)

-- FIXME: ASSUMES 64 BIT INTS  -- NONPORTABLE:
  -- This takes advantage the structure of the Word128 type:
  next g@(RNG k bits c 0) = (fromIntegral (loHalf bits), RNG k bits c 1)
  next g@(RNG k bits c 1) = (fromIntegral (hiHalf bits), next128' g)

  -- We waste some random bits here:
  split g@(RNG k bits c s) = (g', next128' g')
   where g' = next128' g

