/* 
 * Copyright (c) 2010, Intel Corporation
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 * 
 *     * Redistributions of source code must retain the above copyright notice, 
 *       this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright notice, 
 *       this list of conditions and the following disclaimer in the documentation 
 *       and/or other materials provided with the distribution.
 *     * Neither the name of Intel Corporation nor the names of its contributors 
 *       may be used to endorse or promote products derived from this software 
 *       without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
 * IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE 
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
*/

#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>
#include <memory.h>
#ifdef __linux__
#include <alloca.h>
#ifndef _alloca
#define _alloca alloca
#endif
#endif

#include "iaesni.h"

// test vectors from fips-197 Appendix C http://csrc.nist.gov/publications/fips/fips197/fips-197.pdf

unsigned char test_plain_text[16] = {0x00,0x11,0x22,0x33,0x44,0x55,0x66,0x77,0x88,0x99,0xaa,0xbb,0xcc,0xdd,0xee,0xff};
unsigned char test_cipher_text_128[16] = {0x69,0xc4,0xe0,0xd8,0x6a,0x7b,0x04,0x30,0xd8,0xcd,0xb7,0x80,0x70,0xb4,0xc5,0x5a};
unsigned char test_cipher_text_192[16] = {0xdd,0xa9,0x7c,0xa4,0x86,0x4c,0xdf,0xe0,0x6e,0xaf,0x70,0xa0,0xec,0x0d,0x71,0x91};
unsigned char test_cipher_text_256[16] = {0x8e,0xa2,0xb7,0xca,0x51,0x67,0x45,0xbf,0xea,0xfc,0x49,0x90,0x4b,0x49,0x60,0x89};
unsigned char test_key_128[16] = {0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f};
unsigned char test_key_192[24] = {0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f,0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17};
unsigned char test_key_256[32] = {0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f,0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1a,0x1b,0x1c,0x1d,0x1e,0x1f};
unsigned char test_iv[16] = {0x00,0x11,0x22,0x33,0x44,0x55,0x66,0x77,0x88,0x99,0xaa,0xbb,0xcc,0xdd,0xee,0xff};


#define TEST_PASS (0)
#define TEST_FAIL_ENC (1)
#define TEST_FAIL_DEC (2)

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


int test128_CBC(unsigned long numBlocks)
{
	unsigned int buffer_size = numBlocks*16;
	unsigned int i;
	UCHAR *testVector = (UCHAR*)_alloca(buffer_size);
	UCHAR *testResult = (UCHAR*)_alloca(buffer_size);
	UCHAR local_test_iv[16];
	unsigned int half_size;

	for (i=0;i<buffer_size;i++)
	{
		testVector[i] = test_plain_text[i % 16];
		testResult[i] = 0xee;
	}

	memcpy(local_test_iv,test_iv,16);
	intel_AES_enc128_CBC(testVector,testResult,test_key_128,numBlocks,local_test_iv);

	//test chaining as well
	memcpy(local_test_iv,test_iv,16);
	half_size = numBlocks/2;
	intel_AES_dec128_CBC(testResult,testVector,test_key_128,half_size,local_test_iv);
	intel_AES_dec128_CBC(testResult+16*(half_size),testVector+16*(half_size),test_key_128,numBlocks - half_size,local_test_iv);

	for (i=0;i<buffer_size;i++)
	{
		if (testVector[i] != test_plain_text[i % 16])
		{
			printf("%d",i);
			return TEST_FAIL_DEC;
		}
	}

	return TEST_PASS;

}




int test192(unsigned long numBlocks)
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

	intel_AES_enc192(testVector,testResult,test_key_192,numBlocks);

	for (i=0;i<buffer_size;i++)
	{
		if (testResult[i] != test_cipher_text_192[i % 16])
			return TEST_FAIL_ENC;

		testVector[i] = 0xdd;
	}

	intel_AES_dec192(testResult,testVector,test_key_192,numBlocks);

	for (i=0;i<buffer_size;i++)
	{
		if (testVector[i] != test_plain_text[i % 16])
			return TEST_FAIL_DEC;
	}

	return 0;

}


int test192_CBC(unsigned long numBlocks)
{
	unsigned int buffer_size = numBlocks*16;
	unsigned int i;
	UCHAR *testVector = (UCHAR*)_alloca(buffer_size);
	UCHAR *testResult = (UCHAR*)_alloca(buffer_size);
	UCHAR local_test_iv[16];
	unsigned int half_size;

	memcpy(local_test_iv,test_iv,16);

	for (i=0;i<buffer_size;i++)
	{
		testVector[i] = test_plain_text[i % 16];
		testResult[i] = 0xee;
	}

	intel_AES_enc192_CBC(testVector,testResult,test_key_128,numBlocks,local_test_iv);


	memcpy(local_test_iv,test_iv,16);
	half_size = numBlocks/2;
	intel_AES_dec192_CBC(testResult,testVector,test_key_128,half_size,local_test_iv);
	intel_AES_dec192_CBC(testResult+16*(half_size),testVector+16*(half_size),test_key_128,numBlocks - half_size,local_test_iv);

	for (i=0;i<buffer_size;i++)
	{
		if (testVector[i] != test_plain_text[i % 16])
			return TEST_FAIL_DEC;
	}

	return TEST_PASS;

}


int test256(unsigned long numBlocks)
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

	intel_AES_enc256(testVector,testResult,test_key_256,numBlocks);

	for (i=0;i<buffer_size;i++)
	{
		if (testResult[i] != test_cipher_text_256[i % 16])
			return TEST_FAIL_ENC;

		testVector[i] = 0xdd;
	}

	intel_AES_dec256(testResult,testVector,test_key_256,numBlocks);

	for (i=0;i<buffer_size;i++)
	{
		if (testVector[i] != test_plain_text[i % 16])
			return TEST_FAIL_DEC;
	}

	return 0;

}


int test256_CBC(unsigned long numBlocks)
{
	unsigned int buffer_size = numBlocks*16;
	unsigned int i;
	UCHAR *testVector = (UCHAR*)_alloca(buffer_size);
	UCHAR *testResult = (UCHAR*)_alloca(buffer_size);
	UCHAR local_test_iv[16];
	unsigned int half_size = numBlocks/2;

	memcpy(local_test_iv,test_iv,16);

	for (i=0;i<buffer_size;i++)
	{
		testVector[i] = test_plain_text[i % 16];
		testResult[i] = 0xee;
	}

	intel_AES_enc256_CBC(testVector,testResult,test_key_256,numBlocks,local_test_iv);


	memcpy(local_test_iv,test_iv,16);
	intel_AES_dec256_CBC(testResult,testVector,test_key_256,half_size,local_test_iv);
	intel_AES_dec256_CBC(testResult+16*(half_size),testVector+16*(half_size),test_key_256,numBlocks - half_size,local_test_iv);

	for (i=0;i<buffer_size;i++)
	{
		if (testVector[i] != test_plain_text[i % 16])
			return TEST_FAIL_DEC;
	}

	return TEST_PASS;

}


int  main(void)
{
	int i;

	if (check_for_aes_instructions() == 0)
	{
		printf("no AES instructions detected! - stopping the test app\n");
		return 1;
	}

	printf("AES instructions detected\n");
	for (i=1;i<4096;i+=i*3/2)
	{
		printf("testing %d blocks,AES-128: %s",i,(test128(i) != TEST_PASS) ? "FAIL" : "PASS");
		printf(",AES-192: %s",(test192(i) != TEST_PASS) ? "FAIL" : "PASS");
		printf(",AES-256: %s",(test256(i) != TEST_PASS) ? "FAIL" : "PASS");
		printf("\n");
	}

	for (i=1;i<4096;i+=i*3/2)
	{
		printf("testing %d blocks,AES-128-CBC: %s",i,(test128_CBC(i) != TEST_PASS) ? "FAIL" : "PASS");
		printf(",AES-192-CBC: %s",(test192_CBC(i) != TEST_PASS) ? "FAIL" : "PASS");
		printf(",AES-256-CBC: %s\n",(test256_CBC(i) != TEST_PASS) ? "FAIL" : "PASS");
		printf("\n");
	}
	return 0;
}
