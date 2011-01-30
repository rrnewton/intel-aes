
-- | This version uses an Intel AES-NI based assembly code
-- | implementation of encryption.  Or, on processors where AES-NI is
-- | unavailable, it falls back to Dr. Brian Gladman's implementation.
-- | This module is simply a wrapper around the Intel-provided AESNI
-- | sample library:
-- |    http://software.intel.com/en-us/articles/download-the-intel-aesni-sample-library/

module Codec.Encryption.BurtonRNG
    (
     mkBurtonGen,
     -- Plus, instances exported of course.
     testBurton
    )
where 

import System.Random ()
import Crypto.Random.DRBG ()
import Data.Word
import Foreign.Ptr

mkBurtonGen = undefined

-- UNFINISHED

foreign import ccall unsafe "iaesni.h" intel_AES_enc128 :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Int -> IO ()
-- foreign import ccall unsafe "intel_aes64.a" intel_AES_enc128 :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Int -> ()
-- (UCHAR *plainText, UCHAR *cipherText, UCHAR *key, size_t numBlocks)


-- foreign import ccall unsafe "iaesni.h" _alloca :: Int -> IO (Ptr Word8)

foreign import ccall unsafe "stdlib.h" malloc :: Int -> IO (Ptr Word8)

{-
void intel_AES_enc128(UCHAR *plainText,UCHAR *cipherText,UCHAR *key,size_t numBlocks)
void intel_AES_enc128_CBC(UCHAR *plainText,UCHAR *cipherText,UCHAR *key,size_t numBlocks,UCHAR *iv)
void intel_AES_enc192(UCHAR *plainText,UCHAR *cipherText,UCHAR *key,size_t numBlocks)
void intel_AES_enc192_CBC(UCHAR *plainText,UCHAR *cipherText,UCHAR *key,size_t numBlocks,UCHAR *iv)
void intel_AES_enc256(UCHAR *plainText,UCHAR *cipherText,UCHAR *key,size_t numBlocks)
void intel_AES_enc256_CBC(UCHAR *plainText,UCHAR *cipherText,UCHAR *key,size_t numBlocks,UCHAR *iv)
void intel_AES_dec128(UCHAR *cipherText,UCHAR *plainText,UCHAR *key,size_t numBlocks)
void intel_AES_dec128_CBC(UCHAR *cipherText,UCHAR *plainText,UCHAR *key,size_t numBlocks,UCHAR *iv)
void intel_AES_dec192(UCHAR *cipherText,UCHAR *plainText,UCHAR *key,size_t numBlocks)
void intel_AES_dec192_CBC(UCHAR *cipherText,UCHAR *plainText,UCHAR *key,size_t numBlocks,UCHAR *iv)
void intel_AES_dec256(UCHAR *cipherText,UCHAR *plainText,UCHAR *key,size_t numBlocks)
void intel_AES_dec256_CBC(UCHAR *cipherText,UCHAR *plainText,UCHAR *key,size_t numBlocks,UCHAR *iv)
void intel_AES_encdec256_CTR(UCHAR *in,UCHAR *out,UCHAR *key,size_t numBlocks,UCHAR *ic)
void intel_AES_encdec192_CTR(UCHAR *in,UCHAR *out,UCHAR *key,size_t numBlocks,UCHAR *ic)
void intel_AES_encdec128_CTR(UCHAR *in,UCHAR *out,UCHAR *key,size_t numBlocks,UCHAR *ic)
-}

testBurton = do 
  putStrLn$ "\n Allocating aligned buffers.."
  -- buf1 <- _alloca 1024
  -- buf2 <- _alloca 1024
  -- buf3 <- _alloca 1024
  buf1 <- malloc 1024
  buf2 <- malloc 1024
  buf3 <- malloc 1024
  putStrLn$ "\nCalling foreign AES encode routine"
  intel_AES_enc128 buf1 buf2 buf3 1
  putStrLn$ "Done with foreign call"

{-
int test128(unsigned long numBlocks)
{
	unsigned int buffer_size = numBlocks*16;
	unsigned int i;
	UCHAR *testVector = (UCHAR*)_alloca(buffer_size);
	UCHAR *testResult = (UCHAR*)_alloca(buffer_size);
	for (i=0;i<buffer_size;i++)
	{
		testVector[i] = test_plain_text[i % 16];
		testResult[i] = 0xee;
	}

	intel_AES_enc128(testVector,testResult,test_key_128,numBlocks);

	for (i=0;i<buffer_size;i++)
	{
		if (testResult[i] != test_cipher_text_128[i % 16])
			return TEST_FAIL_ENC;

		testVector[i] = 0xdd;
	}

	intel_AES_dec128(testResult,testVector,test_key_128,numBlocks);

	for (i=0;i<buffer_size;i++)
	{
		if (testVector[i] != test_plain_text[i % 16])
			return TEST_FAIL_DEC;
	}

	return TEST_PASS;

}
-}