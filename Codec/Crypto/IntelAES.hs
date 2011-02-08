{-|
     Module      :  Codec.Crypto.IntelAES
     Copyright   :  (c) Ryan Newton 2011
     License     :  BSD-style (see the file LICENSE)
     Maintainer  :  rrnewton@gmail.com
     Stability   :  experimental
     Portability :  linux only (NEEDS PORTING)

     This module provides an AES implementation that will test the CPU
     ID and use hardware acceleration where available, otherwise it will
     fall back to Dr. Brian Gladman's software implementation.

     This module also exports a random number generator based on AES
     both using the System.Random.RandomGen interface and the
     Codec.Crypto.Random.

  -}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface, CPP, ScopedTypeVariables  #-}

module Codec.Crypto.IntelAES
    (
      mkAESGen,
      CompoundAESRNG(), 
     -- Plus, instances exported of course.
      testIntelAES
    )
where 

import qualified Codec.Crypto.IntelAES.AESNI      as NI
import qualified Codec.Crypto.GladmanAES as GA
import GHC.IO (unsafeDupablePerformIO)
import Data.Tagged
import Data.Word
import Data.Serialize
import qualified Data.ByteString as B
import Crypto.Random (CryptoRandomGen(..), GenError(..), splitGen, genBytes)
import Crypto.Types
import Codec.Crypto.ConvertRNG 
import Debug.Trace

newtype CompoundCRG = 
  CompoundCRG 
   (Either (BCtoCRG (NI.IntelAES NI.N128))
	   (BCtoCRG (GA.AES GA.N128)))

-- | A type representing an AES-based random number generator which
--   will use AESNI instructions when available, and invoke the
--   portable Gladman implementation when not.
type CompoundAESRNG = CRGtoRG CompoundCRG

-- | Simple function to create a random number generator from an Int,
--   analogous to `System.Random.newStdGen`.  Only 128-bit encryption
--   is provided for now.
mkAESGen :: Int -> CompoundAESRNG
mkAESGen int = convertCRG gen
   where
  Right (gen :: CompoundCRG) = newGen (B.append halfseed halfseed )
  halfseed = encode word64
  word64 = fromIntegral int :: Word64



-- foreign import ccall unsafe "iaesni.h" check_for_aes_instructions :: IO Bool
foreign import ccall unsafe "iaesni.h" check_for_aes_instructions :: Bool


{-# INLINE mapRight #-}
mapRight fn x@(Left _) = x
mapRight fn (Right x)  = Right$ fn x

{-# INLINE mapSnd #-}
mapSnd fn (x,y) = (x,fn y)


instance CryptoRandomGen CompoundCRG where 

--  newGen :: B.ByteString -> Either GenError CompoundCRG
  newGen = 
--     if unsafeDupablePerformIO check_for_aes_instructions
     trace ("Checked for AES instructions: "++ show check_for_aes_instructions)$
     if check_for_aes_instructions
     -- Ick, boilerplate:
     then \bytes -> case newGen bytes of Left err  -> Left err
					 Right gen -> Right$ CompoundCRG$ Left gen
     else \bytes -> case newGen bytes of Left err  -> Left err
					 Right gen -> Right$ CompoundCRG$ Right gen

  genSeedLength = Tagged 128

 
  -- ByteLength -> CompoundCRG -> Either GenError (B.ByteString, CompoundCRG)
  genBytes req (CompoundCRG (Left gen)) = 
-- Let's try to reduce that boilerplate if we can...
#if 0
    mapRight (mapSnd (CompoundCRG . Left) ) $ genBytes req gen
#else
    case genBytes req gen of 
      Left  err  -> Left err
      Right (bytes,gen') -> Right (bytes, CompoundCRG (Left gen'))
#endif

-- <boilerplate> OUCH
  genBytes req (CompoundCRG (Right gen)) = 
    case genBytes req gen of 
      Left  err  -> Left err
      Right (bytes,gen') -> Right (bytes, CompoundCRG (Right gen'))
  reseed bs (CompoundCRG (Left gen)) = 
    case reseed bs gen of 
      Left  err  -> Left err
      Right gen' -> Right (CompoundCRG (Left gen'))
  reseed bs (CompoundCRG (Right gen)) = 
    case reseed bs gen of 
      Left  err  -> Left err
      Right gen' -> Right (CompoundCRG (Right gen'))
-- </boilerplate>



testIntelAES = do 
  putStrLn$ "Running crude tests."
--  b <- check_for_aes_instructions
  let b = check_for_aes_instructions
  putStrLn$ "Machine supports AESNI: "++ show b

