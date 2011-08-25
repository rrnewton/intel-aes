{-|
     Module      :  System.Random.AES
     Copyright   :  (c) Ryan Newton 2011
     License     :  BSD-style (see the file LICENSE)
     Maintainer  :  rrnewton@gmail.com
     Stability   :  experimental
     Portability :  Mac OS, Linux, Untested on Windows

     This module provides cryptographic-strength random number
     generators based on AES.

     The generators are exposed both using the
     'System.Random.RandomGen' interface and the
     'Codec.Crypto.CryptoRandomGen' one.

     Internally, this module uses two different AES implementations.
     If the CPU ID indicates that AESNI instructions are available, it
     will use an Intel-provided hardware-accelerated implementation,
     otherwise, it will fall back to Dr. Brian Gladman's well-known
     software implementation.

  -}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface, CPP, ScopedTypeVariables  #-}

module System.Random.AES
    (
      mkAESGen, mkAESGenCRG,
      AesCRG(), AesRG(),
      supportsAESNI,
     -- Plus, instances exported of course.
--      testIntelAES
    )
where 

import qualified Codec.Crypto.IntelAES.AESNI  as NI
import qualified Codec.Crypto.GladmanAES      as GA
-- import GHC.IO (unsafeDupablePerformIO)
import Data.Tagged
import Data.Word
import Data.Serialize
import qualified Data.ByteString as B
-- import Crypto.Random (CryptoRandomGen(..), GenError(..), splitGen, genBytes)
import Crypto.Random (CryptoRandomGen(..))
-- import Crypto.Types
import Codec.Crypto.ConvertRNG 

-- This type represents an RNG which may have one of two different
-- representations, corresponding to the software or the hardware
-- supported version.
newtype AesCRG = 
  AesCRG 
   (Either (BCtoCRG (NI.IntelAES NI.N128))
	   (BCtoCRG (GA.AES GA.N128)))

-- | A type representing an AES-based random number generator which
--   will use AESNI instructions when available, and invoke the
--   portable Gladman implementation when not.
type AesRG = CRGtoRG AesCRG

-- | Simple function to create a random number generator from an Int,
--   exposing the 'System.Random.RandomGen' interface, analogous to
--   'System.Random.newStdGen'.  Only 128-bit encryption is currently
--   provided.
mkAESGen :: Int -> AesRG
mkAESGen int = convertCRG (mkAESGenCRG int)

-- | This variant creates an random number generator which exposes the
--   'Crypto.Random.CryptoRandomGen' interface.
mkAESGenCRG :: Int -> AesCRG
mkAESGenCRG int = gen
   where
  Right (gen :: AesCRG) = newGen (B.append halfseed halfseed )
  halfseed = encode word64
  word64 = fromIntegral int :: Word64



foreign import ccall unsafe "iaesni.h" check_for_aes_instructions :: Bool

-- | Does the machine support AESNI instructions?
supportsAESNI :: Bool
supportsAESNI = check_for_aes_instructions

-- | This instance provides the CryptoRandomGen interface, which
--   allows bulk generation of random bytes.
instance CryptoRandomGen AesCRG where 

--  newGen :: B.ByteString -> Either GenError AesCRG
  newGen = 
     if check_for_aes_instructions
     -- Ick, boilerplate:
     then \bytes -> case newGen bytes of Left err  -> Left err
					 Right gen -> Right$ AesCRG$ Left gen
     else \bytes -> case newGen bytes of Left err  -> Left err
					 Right gen -> Right$ AesCRG$ Right gen

  genSeedLength = Tagged 128
 
  -- ByteLength -> AesCRG -> Either GenError (B.ByteString, AesCRG)
  genBytes req (AesCRG (Left gen)) = 

#if 0
    -- UNFINISHED: Let's try to reduce that boilerplate if we can...
    mapRight (mapSnd (AesCRG . Left) ) $ genBytes req gen
#else
    case genBytes req gen of 
      Left  err  -> Left err
      Right (bytes,gen') -> Right (bytes, AesCRG (Left gen'))
#endif


-- <boilerplate> OUCH
  genBytes req (AesCRG (Right gen)) = 
    case genBytes req gen of 
      Left  err  -> Left err
      Right (bytes,gen') -> Right (bytes, AesCRG (Right gen'))
  reseed bs (AesCRG (Left gen)) = 
    case reseed bs gen of 
      Left  err  -> Left err
      Right gen' -> Right (AesCRG (Left gen'))
  reseed bs (AesCRG (Right gen)) = 
    case reseed bs gen of 
      Left  err  -> Left err
      Right gen' -> Right (AesCRG (Right gen'))
-- </boilerplate>

-- UNFINISHED Refactoring:
{-# INLINE mapRight #-}
mapRight fn x@(Left _) = x
mapRight fn (Right x)  = Right$ fn x
{-# INLINE mapSnd #-}
mapSnd fn (x,y) = (x,fn y)

