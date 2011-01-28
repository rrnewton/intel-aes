


module Codec.Encryption.BurtonRNGSlow 
    (
     mkBurtonGen
     -- Plus, instances exported of course.
    )
where 

import System.Random ()
import Crypto.Random.DRBG ()

import Codec.Encryption.AES (encrypt)
import Data.LargeWord
import System.Random

data RNG = RNG Word128 Word128 deriving Show
next128 (RNG k c) = (encrypt k c, RNG k (c+1))

instance RandomGen RNG where
  next g = (fromIntegral n, g')
   where (n,g') = next128 g
  split g@(RNG k c) = (g', RNG n 0)
   where (n,g') = next128 g


mkBurtonGen :: Word128 -> RNG
mkBurtonGen seed = RNG seed 0
