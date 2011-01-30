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
     ScopedTypeVariables #-}

module Codec.Crypto.IntelAES
    (
      testIntelAES
    , mkAESGen, SimpleAESRNG, LiftCRG0(..)

    -- Inefficient version for testing:
    , mkAESGen0, SimpleAESRNG0, LiftCRG0(..)

    , BCtoCRG(..)
     -- Plus, instances exported of course.
    )
where 

import System.Random 
import System.IO.Unsafe (unsafePerformIO)
import GHC.IO (unsafeDupablePerformIO)

import Data.List
import Data.Word
import Data.Tagged
import Data.Serialize

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as BI

import Crypto.Random.DRBG ()
import Crypto.Modes

import Crypto.Random (CryptoRandomGen(..), GenError(..), splitGen, genBytes)
import Crypto.Classes (BlockCipher(..), blockSizeBytes)
import Crypto.Types

import Control.Monad
import Foreign.Ptr
import qualified Foreign.ForeignPtr as FP
import Foreign.Storable

----------------------------------------------------------------------------------------------------

type SimpleAESRNG = LiftCRG (BCtoCRG (IntelAES N128))

-- Expose a simple System.Random.RandomGen interface:
mkAESGen :: Int -> SimpleAESRNG
mkAESGen int = convertCRG gen
 where
  Right (gen :: BCtoCRG (IntelAES N128)) = newGen (B.append halfseed halfseed )
  halfseed = encode word64
  word64 = fromIntegral int :: Word64


-- TEMP: Inefficient version for testing:
type SimpleAESRNG0 = LiftCRG0 (BCtoCRG (IntelAES N128))
mkAESGen0 :: Int -> SimpleAESRNG0
mkAESGen0 int = LiftCRG0 gen
 where
  Right (gen :: BCtoCRG (IntelAES N128)) = newGen (B.append halfseed halfseed )
  halfseed = encode word64
  word64 = fromIntegral int :: Word64



----------------------------------------------------------------------------------------------------
-- Converting CryptoRandomGen to RandomGen
-- This should go somewhere else...
----------------------------------------

-- There's a potential overlapping instances problem here.  Someone
-- may want to do their own RandomGen instance, creating a problem
-- with this:
--
--   instance CryptoRandomGen g => RandomGen g where 
--
-- NOTE: The above would also be an undecidable instance.  Another
-- option is to have a type used just for lifting.  See below.


-- | Converting CryptoRandomGen to RandomGen.
-- | This naive version is probably pretty inefficent:
data LiftCRG0 a = LiftCRG0 a
instance CryptoRandomGen g => RandomGen (LiftCRG0 g) where 
   next  (LiftCRG0 g) = 
--       case genBytes (max bytes_in_int (keyLength g `quot` 8)) g of 
       case genBytes bytes_in_int g of 
         Left err -> error$ "CryptoRandomGen genBytes error: " ++ show err
	 Right (bytes,g') -> 
           case decode bytes of 
	      Left err -> error$ "Deserialization error:"++ show err
	      Right n -> (n, LiftCRG0 g')
	     
   split (LiftCRG0 g) = 
       case splitGen g of 
         Left err      -> error$ "CryptoRandomGen splitGen error:"++ show err
	 Right (g1,g2) -> (LiftCRG0 g1, LiftCRG0 g2)

-- Another option would be to amortize overhead by generating a large
-- buffer of random bits at once.
-- data LiftCRG a = LiftCRG a BUFFER INDEX

-- Any better way to do this?
bytes_in_int = (round $ 1 + logBase 2 (fromIntegral (maxBound :: Int)))  `quot` 8
-- steps = 128 `quot` bits_in_int

------------------------------------------------------------
-- | Now let's try to make that a bit more efficient.
-- | Keep a buffer of random bits and an index into that buffer.
data LiftCRG a = LiftCRG a 
    {-#UNPACK#-}!         (FP.ForeignPtr Int)
    {-#UNPACK#-}!         Int

instance CryptoRandomGen g => RandomGen (LiftCRG g) where 
   next (LiftCRG g _ ind) | ind == bufsize = next (convertCRG g) -- Refill the buffer
   next (LiftCRG g buf ind) = 
       unsafeDupablePerformIO $ 
         FP.withForeignPtr buf $ \ ptr -> 
           do x <- peekElemOff ptr ind 
	      return (x, LiftCRG g buf (ind+1))
	     
   split (LiftCRG g buf ind) = 
       case splitGen g of 
         Left err      -> error$ "CryptoRandomGen splitGen error:"++ show err
	 Right (g1,g2) -> (LiftCRG g1 buf ind, convertCRG g2)


convertCRG :: CryptoRandomGen g => g -> LiftCRG g
convertCRG crg = LiftCRG g' (FP.castForeignPtr ptr) 0
 where 
  (ptr,_,_)     = BI.toForeignPtr bs
  Right (bs,g') = genBytes (bufsize * bytes_in_int) crg


-- How many 8 byte chunks should we buffer each time?
-- TODO: Autotune this...
bufsize = 256



----------------------------------------------------------------------------------------------------
-- We would also like every BlockCipher to constitute a valid CryptoRandomGen.
-- Again there's the tension with UndecidableInstances vs explicit lifting.

-- When lifting we include a counter:
data BCtoCRG a = BCtoCRG a Word64

instance BlockCipher x => CryptoRandomGen (BCtoCRG x) where 
  newGen  bytes = case buildKey bytes of Nothing -> Left NotEnoughEntropy 
					 Just x  -> Right (BCtoCRG x 0)
  genSeedLength = Tagged 128

  -- If this is called for less than blockSize data 
  genBytes req (BCtoCRG (bcgen :: k) counter) = 
      -- What's the most efficient way to do this?
      unsafePerformIO $ do
--      unsafeDupablePerformIO $ do
	-- Number of times to stamp out the counter:
        let bsize = untag (blockSizeBytes :: Tagged k ByteLength)
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

  reseed bs gen = newGen bs



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

{-# INLINE unpackKey #-}
unpackKey (IntelAES {aesKeyRaw}) = kptr
  -- TODO: ASSERT that key is the right length and offset is zero...
  where 
  (kptr,koff,klen) = BI.toForeignPtr aesKeyRaw

{-# INLINE template #-}
template core keysize ctx@(IntelAES {aesKeyRaw}) plaintext = 
--	 unsafeDupablePerformIO $
	 unsafePerformIO $
	 do let kfptr                   = unpackKey ctx 
		(in_fptr,in_off,in_len) = BI.toForeignPtr plaintext
		(blocks,r) = quotRem in_len keysize
	    -- The buffer should be a multiple of the key size (128/192,256 bits):
	    when (r > 0)$ 
	       error$ "encryptBlock: block size "++show in_len++
		      " bytes , but with AES implementation block size must be a multiple of "++show keysize

	    output <- FP.mallocForeignPtrBytes in_len 
	    FP.withForeignPtr kfptr $ \ keyptr -> 
	      FP.withForeignPtr in_fptr $ \ inptr -> 
		FP.withForeignPtr output  $ \ outptr -> 
		  core inptr outptr keyptr blocks

	    return (BI.fromForeignPtr output 0 in_len)

instance BlockCipher (IntelAES N128) where
	blockSize    = Tagged 128
	encryptBlock = template intel_AES_enc128 16
	decryptBlock = template intel_AES_dec128 16
        -- What's the right behavior here?  Currently this refuses to
        -- generate keys if given an insufficient # of bytes.
	buildKey bytes | B.length bytes >= 16 = Just$ newCtx bytes
        buildKey _     | otherwise            = Nothing
	keyLength (IntelAES {aesKeyRaw}) = B.length aesKeyRaw * 8 -- bits

instance Serialize (IntelAES N128) where
	get = getGeneral 16
	put = putByteString . aesKeyRaw

-- <boilerplate>
instance BlockCipher (IntelAES N192) where
	blockSize    = Tagged 192
	encryptBlock = template intel_AES_enc192 24
	decryptBlock = template intel_AES_dec192 24
	buildKey bytes | B.length bytes >= 24 = Just$ newCtx bytes
        buildKey _     | otherwise            = Nothing
	keyLength (IntelAES {aesKeyRaw}) = B.length aesKeyRaw
instance Serialize (IntelAES N192) where
	get = getGeneral 24
	put = putByteString . aesKeyRaw
instance BlockCipher (IntelAES N256) where
	blockSize    = Tagged 192
	encryptBlock = template intel_AES_enc256 32
	decryptBlock = template intel_AES_dec256 32
	buildKey bytes | B.length bytes >= 32 = Just$ newCtx bytes
        buildKey _     | otherwise            = Nothing
	keyLength (IntelAES {aesKeyRaw}) = B.length aesKeyRaw
instance Serialize (IntelAES N256) where
	get = getGeneral 32
	put = putByteString . aesKeyRaw
-- </boilerplate>


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
  putStrLn$ "\nNow let's try it as a block cypher... encrypt increasing bytes:"
  let inp = B.pack $ take bytes [0..]
      ctxt :: IntelAES N128 = newCtx (B.take 16 inp) 
      cipher = encryptBlock ctxt inp

      backagain = decryptBlock ctxt cipher

  putStrLn$ "\nCiphertext: "++ show (B.unpack cipher)
  putStrLn$ "\nAnd back again: "++ show (B.unpack backagain)

  putStrLn$ "================================================================================" 
  putStrLn$ "\nFinally lets use it to generate some random numbers:"
  let 
      gen2 = mkAESGen 92438653296
      fn (0,_) = Nothing
      fn (i,g) = let (n,g') = next g in Just (n, (i-1,g'))
      nums = unfoldr fn (20,gen2)
  putStrLn$ "Randoms: " ++ show nums

  ------------------------------------------------------------
  putStrLn$ "Done."
  -- putStrLn$ "Next calling test routine in C:"
  -- temp_test128 
  -- putStrLn$ "Done with that test routine"
  

