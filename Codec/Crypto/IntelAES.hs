{-|

     This module provides an AES implementation that will test the CPU
     and use hardware acceleration where available, otherwise it will
     fall back to Dr. Brian Gladman's software implementation.

     This module also exports a random number generator based on AES
     both using the System.Random.RandomGen interface and the
     Codec.Crypto.Random.

  -}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Codec.Crypto.IntelAES
    (
      mkAESGen
     -- Plus, instances exported of course.
    )
where 

import qualified Codec.Crypto.IntelAES.AESNI      as NI
import qualified Codec.Crypto.IntelAES.GladmanAES as GL

-- import System.Random 
-- import System.IO.Unsafe (unsafePerformIO)
-- import GHC.IO (unsafeDupablePerformIO)

-- import Data.List
-- import Data.Word
-- import Data.Tagged
-- import Data.Serialize

-- import qualified Data.Bits
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Char8 as BC
-- import qualified Data.ByteString.Internal as BI

-- import Crypto.Random.DRBG ()
-- import Crypto.Modes

-- import Crypto.Random (CryptoRandomGen(..), GenError(..), splitGen, genBytes)
-- import Crypto.Classes (BlockCipher(..), blockSizeBytes)
-- import Crypto.Types

-- import Control.Monad
-- import Foreign.Ptr
-- import qualified Foreign.ForeignPtr as FP
-- import Foreign.Storable


-- type CompoundAESRNG = (LiftCRG (BCtoCRG (IntelAES N128)))
--     Either (LiftCRG (BCtoCRG (IntelAES N128)))
--	    ()

--mkAESGen :: Int -> CompoundAESRNG
--mkAESGen = undecidable

mkAESGen = NI.mkAESGen

