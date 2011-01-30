{- | 
     This version uses an Intel AES-NI based assembly code
     implementation of encryption.  Or, on processors where AES-NI is
     unavailable, it falls back to Dr. Brian Gladman's implementation
     provided by the "AES" package.  

     Note: This module is simply a wrapper around the Intel-provided
     AESNI sample library, fonud here:

@
   http://software.intel.com/en-us/articles/download-the-intel-aesni-sample-library/
@
 -}
{-# LANGUAGE FlexibleInstances, EmptyDataDecls, FlexibleContexts, NamedFieldPuns,
    UndecidableInstances, ScopedTypeVariables #-}

module Codec.Crypto.IntelAES
    (
      testIntelAES
    , LiftCRG1(..)
     -- Plus, instances exported of course.
    )
where 

import System.Random 
import System.Exit
import System.IO.Unsafe (unsafePerformIO)
import GHC.IO (unsafeDupablePerformIO)

import Data.Word
import Data.Tagged
import Data.Serialize

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI

import Crypto.Random.DRBG ()
import Crypto.Modes
import Crypto.Random (CryptoRandomGen(..), splitGen, genBytes)
import Crypto.Classes (BlockCipher(..))
import Crypto.Types

import Control.Monad
import Foreign.Ptr
import qualified Foreign.ForeignPtr as FP
import Foreign.Storable

----------------------------------------------------------------------------------------------------
-- This should go somewhere else...

-- Also, there's an overlapping instances problem here.  Someone may
-- want to do their own RandomGen instance... 
-- instance CryptoRandomGen g => RandomGen g where 
--   next = undefined
--   split = undefined

-- NOTE: The above would also be an undecidable instance.  Another
-- option is to have a type used just for lifting:

-- This naive version is probably pretty inefficent:
data LiftCRG1 a = LiftCRG1 a
instance CryptoRandomGen g => RandomGen (LiftCRG1 g) where 
   next  (LiftCRG1 g) = 
       case genBytes bytes_in_int g of 
         Left err -> error$ "CryptoRandomGen genBytes error: " ++ show err
	 Right (bytes,g') -> 
           case decode bytes of 
	      Left err -> error$ "Deserialization error:"++ show err
	      Right n -> (n, LiftCRG1 g')
	     
   split (LiftCRG1 g) = 
       case splitGen g of 
         Left err      -> error$ "CryptoRandomGen splitGen error:"++ show err
	 Right (g1,g2) -> (LiftCRG1 g1, LiftCRG1 g2)

-- Another option would be to amortize overhead by generating a large
-- buffer of random bits at once.
-- data LiftCRG a = LiftCRG a BUFFER INDEX

-- Any better way to do this?
bytes_in_int = (round $ 1 + logBase 2 (fromIntegral (maxBound :: Int)))  `quot` 8
-- steps = 128 `quot` bits_in_int


------------------------------------------------------------
-- We would also like every BlockCipher to constitute a valid CryptoRandomGen.
-- Again there's the tension with UndecidableInstances vs explicit lifting.

instance BlockCipher x => CryptoRandomGen x where 
  newGen         = undefined
  genSeedLength = Tagged 0

  genBytes req g = undefined
	  -- let reqI = fromIntegral req
	  --     rnd = L.take reqI bs
	  --     rest = L.drop reqI bs
	  -- in if L.length rnd == reqI
	  -- 	  then Right (B.concat $ L.toChunks rnd, SysRandom rest)
	  -- 	  else Left $ GenErrorOther "Error obtaining enough bytes from system random for given request"
--  reseed _ _ = Left NeedsInfiniteSeed
  reseed _ _ = undefined



----------------------------------------------------------------------------------------------------

type PlainText   = Ptr Word8
type CipherText  = Ptr Word8
type Key         = Ptr Word8
type NullResult = IO ()

foreign import ccall unsafe "iaesni.h" intel_AES_enc128     :: PlainText -> CipherText -> Key -> Int -> NullResult
foreign import ccall unsafe "iaesni.h" intel_AES_enc128_CBC :: PlainText -> CipherText -> Key -> Int -> Ptr Word8 -> NullResult
-- Copy/paste:
foreign import ccall unsafe "iaesni.h" intel_AES_enc192     :: PlainText -> CipherText -> Key -> Int -> NullResult
foreign import ccall unsafe "iaesni.h" intel_AES_enc192_CBC :: PlainText -> CipherText -> Key -> Int -> Ptr Word8 -> NullResult
foreign import ccall unsafe "iaesni.h" intel_AES_enc256     :: PlainText -> CipherText -> Key -> Int -> NullResult
foreign import ccall unsafe "iaesni.h" intel_AES_enc256_CBC :: PlainText -> CipherText -> Key -> Int -> Ptr Word8 -> NullResult
foreign import ccall unsafe "iaesni.h" intel_AES_dec128     :: CipherText -> PlainText -> Key -> Int -> NullResult
foreign import ccall unsafe "iaesni.h" intel_AES_dec128_CBC :: CipherText -> PlainText -> Key -> Int -> Ptr Word8 -> NullResult
foreign import ccall unsafe "iaesni.h" intel_AES_dec192     :: CipherText -> PlainText -> Key -> Int -> NullResult
foreign import ccall unsafe "iaesni.h" intel_AES_dec192_CBC :: CipherText -> PlainText -> Key -> Int -> Ptr Word8 -> NullResult
foreign import ccall unsafe "iaesni.h" intel_AES_dec256     :: CipherText -> PlainText -> Key -> Int -> NullResult
foreign import ccall unsafe "iaesni.h" intel_AES_dec256_CBC :: CipherText -> PlainText -> Key -> Int -> Ptr Word8 -> NullResult

foreign import ccall unsafe "iaesni.h" intel_AES_encdec128_CTR :: Ptr Word8 -> Ptr Word8 -> Key -> Int -> Ptr Word8 -> NullResult
foreign import ccall unsafe "iaesni.h" intel_AES_encdec192_CTR :: Ptr Word8 -> Ptr Word8 -> Key -> Int -> Ptr Word8 -> NullResult
foreign import ccall unsafe "iaesni.h" intel_AES_encdec256_CTR :: Ptr Word8 -> Ptr Word8 -> Key -> Int -> Ptr Word8 -> NullResult


foreign import ccall unsafe "stdlib.h" malloc :: Int -> IO (Ptr Word8)
foreign import ccall unsafe "stdlib.h" calloc :: Int -> Int -> IO (Ptr Word8)

foreign import ccall unsafe "c_test.c" temp_test128 :: IO ()

----------------------------------------------------------------------------------------------------

-- Haskell datatypes to model the different AES modes:
data N128
data N192
data N256

data IntelAES n = IntelAES { aesKeyRaw :: B.ByteString }

instance Serialize (IntelAES N128) where
	get = getGeneral 16
	put = putByteString . aesKeyRaw


{-# INLINE unpackKey #-}
unpackKey (IntelAES {aesKeyRaw}) = kptr
  -- TODO: ASSERT that key is the right length and offset is zero...
  where 
  (kptr,koff,klen) = BI.toForeignPtr aesKeyRaw

{-# INLINE template #-}
template core ctx@(IntelAES {aesKeyRaw}) plaintext = 
--	 unsafeDupablePerformIO $
	 unsafePerformIO $
	 do let kfptr                   = unpackKey ctx 
		(in_fptr,in_off,in_len) = BI.toForeignPtr plaintext
		(blocks,r) = quotRem in_len 16
	    -- The buffer should be a multiple of 128 bits (16 bytes)
	    when (r > 0)$ error "encryptBlock: for AES implementation block size must be a multiple of 128bits"

	    output <- FP.mallocForeignPtrBytes in_len 
	    FP.withForeignPtr kfptr $ \ keyptr -> 
	      FP.withForeignPtr in_fptr $ \ inptr -> 
		FP.withForeignPtr output  $ \ outptr -> 
		  core inptr outptr keyptr blocks

	    return (BI.fromForeignPtr output 0 in_len)

instance BlockCipher (IntelAES N128) where
	blockSize    = Tagged 128
	encryptBlock = template intel_AES_enc128
	decryptBlock = template intel_AES_dec128
	    
        -- What's the right behavior here?  Currently this refuses to
        -- generate keys if given an insufficient # of bytes.
	buildKey bytes = if B.length bytes >= 16
			 then Just$ newCtx bytes
			 else Nothing
	keyLength (IntelAES {aesKeyRaw}) = B.length aesKeyRaw

getGeneral :: BlockCipher (IntelAES n) => Int -> Get (IntelAES n)
getGeneral n = do
	bs <- getByteString n
	case buildKey bs of
		Nothing -> fail "Could not build key from serialized bytestring"
		Just x  -> return x

newCtx :: B.ByteString -> IntelAES n
newCtx key = IntelAES key

----------------------------------------------------------------------------------------------------
-- Testing

unpack_ptr :: Storable a => Ptr a -> Int -> IO [a]
unpack_ptr ptr len = loop len []
  where 
  loop 0 acc = return acc
  loop i acc = do x    <- peekElemOff ptr (i-1)
		  loop (i-1) (x:acc)

-- This is not a meaningful test yet... one option would be to reproduce the tests in aessample.c
testIntelAES = do 

  ------------------------------------------------------------
  let bytes = 256
  plaintext  <- calloc bytes 1
  key        <- calloc 16 1
  ciphertext <- calloc bytes 1

  forM [0..bytes-1] $ \i -> do 
    pokeElemOff plaintext i (fromIntegral i)

  forM [0..15] $ \i -> do 
    pokeElemOff key i (fromIntegral i)

  putStrLn$ "Plaintext:" 
  ls <- unpack_ptr plaintext bytes
  print ls

  putStrLn$ "Key:" 
  ls <- unpack_ptr key 16
  print ls

  putStrLn$ "Cipher text:" 
  ls <- unpack_ptr ciphertext bytes
  print ls

  putStrLn$ "\nCalling foreign AES encode routine: byte"
  -- Divide byte length by 128 bits (16 bytes):
  intel_AES_enc256 plaintext ciphertext key (bytes `quot` 16)
  putStrLn$ "Done with foreign call"

  putStrLn$ "Cipher text:" 
  ls <- unpack_ptr ciphertext bytes
  print ls

  putStrLn$ "================================================================================" 

  ------------------------------------------------------------
  putStrLn$ "\nNow let's try it as a block cypher..."
  let inp = B.pack $ take bytes [0..]
      ctxt :: IntelAES N128 = newCtx (B.take 16 inp) 
      cipher = encryptBlock ctxt inp

      backagain = decryptBlock ctxt cipher

  putStrLn$ "\nCiphertext: "++ show (B.unpack cipher)
  putStrLn$ "\nAnd back again: "++ show (B.unpack backagain)

  ------------------------------------------------------------
  putStrLn$ "Done."
  exitSuccess
  -- putStrLn$ "Next calling test routine in C:"
  -- temp_test128 
  -- putStrLn$ "Done with that test routine"
  
