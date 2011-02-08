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

/* 
	This program can take a long time to run and it may use the whole system.
	The program gets timing info for various sizes, methods, and number of threads.
	It also raises the priority of the timing threads to reduce the variation in performance.
	It has windows specific code and I haven't ported it to linux.
	See aes_gladman_subset\src\modetest.c for example implementation in linux and windows
*/
#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>
#include <memory.h>
#include "iaesni.h"
#include <windows.h>

// test vectors from fips-197 Appendix C http://csrc.nist.gov/publications/fips/fips197/fips-197.pdf

unsigned char test_plain_text[16] = {0x00,0x11,0x22,0x33,0x44,0x55,0x66,0x77,0x88,0x99,0xaa,0xbb,0xcc,0xdd,0xee,0xff};
unsigned char test_cipher_text_128[16] = {0x69,0xc4,0xe0,0xd8,0x6a,0x7b,0x04,0x30,0xd8,0xcd,0xb7,0x80,0x70,0xb4,0xc5,0x5a};
unsigned char test_cipher_text_192[16] = {0xdd,0xa9,0x7c,0xa4,0x86,0x4c,0xdf,0xe0,0x6e,0xaf,0x70,0xa0,0xec,0x0d,0x71,0x91};
unsigned char test_cipher_text_256[16] = {0x8e,0xa2,0xb7,0xca,0x51,0x67,0x45,0xbf,0xea,0xfc,0x49,0x90,0x4b,0x49,0x60,0x89};
unsigned char test_key_128[16] = {0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f};
unsigned char test_key_192[24] = {0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f,0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17};
unsigned char test_key_256[32] = {0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f,0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1a,0x1b,0x1c,0x1d,0x1e,0x1f};
unsigned char test_iv[16] = {0x00,0x11,0x22,0x33,0x44,0x55,0x66,0x77,0x88,0x99,0xaa,0xbb,0xcc,0xdd,0xee,0xff};

SYSTEM_INFO si;


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

	for (i=0;i<buffer_size;i++)
	{
		testVector[i] = test_plain_text[i % 16];
		testResult[i] = 0xee;
	}

	memcpy(local_test_iv,test_iv,16);
	intel_AES_enc128_CBC(testVector,testResult,test_key_128,numBlocks,local_test_iv);

	//test chaining as well
	memcpy(local_test_iv,test_iv,16);
	unsigned int half_size = numBlocks/2;
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

	memcpy(local_test_iv,test_iv,16);

	for (i=0;i<buffer_size;i++)
	{
		testVector[i] = test_plain_text[i % 16];
		testResult[i] = 0xee;
	}

	intel_AES_enc192_CBC(testVector,testResult,test_key_128,numBlocks,local_test_iv);


	memcpy(local_test_iv,test_iv,16);
	unsigned int half_size = numBlocks/2;
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

	memcpy(local_test_iv,test_iv,16);

	for (i=0;i<buffer_size;i++)
	{
		testVector[i] = test_plain_text[i % 16];
		testResult[i] = 0xee;
	}

	intel_AES_enc256_CBC(testVector,testResult,test_key_128,numBlocks,local_test_iv);


	memcpy(local_test_iv,test_iv,16);
	unsigned int half_size = numBlocks/2;
	intel_AES_dec256_CBC(testResult,testVector,test_key_128,half_size,local_test_iv);
	intel_AES_dec256_CBC(testResult+16*(half_size),testVector+16*(half_size),test_key_128,numBlocks - half_size,local_test_iv);

	for (i=0;i<buffer_size;i++)
	{
		if (testVector[i] != test_plain_text[i % 16])
			return TEST_FAIL_DEC;
	}

	return TEST_PASS;

}

#define MAX_NUM_THREADS (16)
#define ITER 30000

#include "iaes_asm_interface.h"

typedef 	void (__cdecl *AESPROC)(sAesData *);
typedef 	void (__cdecl *KEYGENPROC)(UCHAR *key,UCHAR *round_keys);

struct ThreadCommad;
DWORD __stdcall ThreadFunction(ThreadCommad *cmd);



struct ThreadCommad
{
	volatile LONG *start_mutex;
	volatile LONG *end_mutex;
	UINT num_threads;
	UINT thread_num;
	UINT num_iterations;
	void (__cdecl *proc)(sAesData *);
	KEYGENPROC key_gen_proc;
	sAesData data;
	UCHAR *key;
	double avg_clocks;
	char padd[256];
};

struct TestCfg
{
	UINT num_threads;
	UINT buffer_size;
	UINT alignment;
	KEYGENPROC key_gen_proc;
	AESPROC proc;
	bool in_place;
	bool aligned_round_keys;
	char *name;
};

TestCfg cfg[] = {
//	{1,2048,4096,iEncExpandKey256,iEnc256,false,true,"Enc-ECB-256"},
//	{1,2048,4096,iEncExpandKey192,iEnc192,false,true,"Enc-ECB-192"},
//	{1,2048,4096,iEncExpandKey128,iEnc128,false,true,"Enc-ECB-128"},
	
	{1,2048,4096,iEncExpandKey256,iEnc256_CBC,false,true,"Enc-CBC-256"},
	{1,2048,4096,iEncExpandKey192,iEnc192_CBC,false,true,"Enc-CBC-192"},
	{1,2048,4096,iEncExpandKey128,iEnc128_CBC,false,true,"Enc-CBC-128"},

//	{1,2048,4096,iDecExpandKey256,iDec256,false,true,"Dec-ECB-256"},
//	{1,2048,4096,iDecExpandKey192,iDec192,false,true,"Dec-ECB-192"},
//	{1,2048,4096,iDecExpandKey128,iDec128,false,true,"Dec-ECB-128"},
	
	{1,2048,4096,iDecExpandKey256,iDec256_CBC,false,true,"Dec-CBC-256"},
	{1,2048,4096,iDecExpandKey192,iDec192_CBC,false,true,"Dec-CBC-192"},
	{1,2048,4096,iDecExpandKey128,iDec128_CBC,false,true,"Dec-CBC-128"},

};

struct TestCommand
{
	LONG start_mutex;
	char pad1[256];
	LONG end_mutex;
	char pad2[256];

	UCHAR iv[16];

	UCHAR round_keys_buf[17*16];
	bool in_place;
	UINT num_threads;

	ThreadCommad cmds[MAX_NUM_THREADS];

	void BuildTest(TestCfg *cfg)
	{
		UCHAR key[16];
		UINT i;
		UINT block_size = ((cfg->buffer_size + 15)/16)*16;
		num_threads = cfg->num_threads;
		start_mutex = end_mutex = 0;
		UCHAR *round_keys = round_keys_buf;
		in_place = cfg->in_place;

		if (cfg->aligned_round_keys && ((DWORD_PTR)round_keys & 0xf) != 0)
		{
			*(DWORD_PTR *)&round_keys += 15;
			*(DWORD_PTR *)&round_keys /= 16;
			*(DWORD_PTR *)&round_keys *= 16;
		}
		else if (!cfg->aligned_round_keys && ((DWORD_PTR)round_keys & 0xf) == 0)
		{
			*(DWORD_PTR *)&round_keys += 15;
			*(DWORD_PTR *)&round_keys /= 16;
			*(DWORD_PTR *)&round_keys *= 16;
			*(DWORD_PTR *)&round_keys += 4;
		}

		cfg->key_gen_proc(key,round_keys);
		
		for (i=0;i<num_threads;i++)
		{
			cmds[i].data.in_block = (UCHAR *)_aligned_malloc(block_size,cfg->alignment);
			if (!in_place) cmds[i].data.out_block = (UCHAR *)_aligned_malloc(block_size,cfg->alignment);
			cmds[i].num_iterations = ITER;
			cmds[i].num_threads = num_threads;
			cmds[i].thread_num = i;
			cmds[i].proc = cfg->proc;
			cmds[i].start_mutex = &start_mutex;
			cmds[i].end_mutex = &end_mutex;
			cmds[i].data.iv = iv;
			cmds[i].data.expanded_key = round_keys;
			cmds[i].data.num_blocks = block_size/16;
			cmds[i].key_gen_proc = cfg->key_gen_proc;
			cmds[i].key = (UCHAR *)key;
		}
	}

	void ExecuteTest(double *min,double *max,double *avg)
	{
		HANDLE *h = new HANDLE[num_threads];
		UINT i;

		for (i=0;i<num_threads;i++)
		{
			h[i] = CreateThread(NULL,NULL,(LPTHREAD_START_ROUTINE)ThreadFunction,&cmds[i],0,NULL);
		}
		WaitForMultipleObjects(num_threads,h,TRUE,INFINITE);

		double amin = 1e38,amax = 0,sum = 0;
		for (i=0;i<num_threads;i++)
		{
			if (cmds[i].avg_clocks < amin) amin = cmds[i].avg_clocks;
			if (cmds[i].avg_clocks > amax) amax = cmds[i].avg_clocks;
			sum += cmds[i].avg_clocks;
		}

		sum /= num_threads;

		*min = amin;
		*max = amax;
		*avg = sum;

		delete[] h;

	}

	void FreeTest()
	{
		UINT i;
		for (i=0;i<num_threads;i++)
		{
			_aligned_free(cmds[i].data.in_block);
			if (!in_place) _aligned_free(cmds[i].data.out_block);
		}
		
	}


};


DWORD __stdcall ThreadFunction(ThreadCommad *cmd)
{
	UINT i;
	if (cmd->num_threads != si.dwNumberOfProcessors)
		SetThreadAffinityMask(GetCurrentThread(),1<<(cmd->thread_num*2));
	else
		SetThreadAffinityMask(GetCurrentThread(),1<<(cmd->thread_num));


	SetThreadPriority(GetCurrentThread(),THREAD_PRIORITY_TIME_CRITICAL );
	Sleep(10);
	InterlockedIncrement(cmd->start_mutex);
	while(*cmd->start_mutex < cmd->num_threads);
	cmd->key_gen_proc(cmd->key,cmd->data.expanded_key);
	cmd->proc(&cmd->data);
	unsigned __int64 start = do_rdtsc();
	for (i=0;i<cmd->num_iterations;i++)
	{
		cmd->key_gen_proc(cmd->key,cmd->data.expanded_key);
		cmd->proc(&cmd->data);
	}
	unsigned __int64 stop = do_rdtsc() - start;
	InterlockedIncrement(cmd->end_mutex);
	while(*cmd->end_mutex < cmd->num_threads);
	cmd->avg_clocks = (double)stop/(double)cmd->num_iterations;
	return 0;
}




UINT test_threads[] = {1,2,4,6,12};


void main()
{
	int i;

	fprintf(stderr, "This program can take a long time to run and it may use the whole system.\n");
	fprintf(stderr, "The program gets timing info for various sizes, methods, and number of threads.\n");
	fprintf(stderr, "It also raises the priority of the timing threads to reduce the variation in performance.\n");
	if (!check_for_aes_instructions())
	{
		printf("no AES instructions detected! - stopping the test app\n");
		return;
	}

	printf("AES instructions detected\n");

#if 0
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
#endif

	TestCommand cmd;
	UINT nthr;
	UINT buffsize;

	GetSystemInfo(&si);

	printf("#threads,#bytes,");
	for (i=0;i<sizeof(cfg)/sizeof(TestCfg);i++)
		printf("%s,,,",cfg[i].name);
	printf("\n");
	printf(",,");
	for (i=0;i<sizeof(cfg)/sizeof(TestCfg);i++)
		printf("min,max,avg,",cfg[i].name);
	printf("\n");

	for (nthr = 0;nthr < sizeof(test_threads)/sizeof(UINT);nthr++)
	{
		if (test_threads[nthr] > si.dwNumberOfProcessors) continue;

		for (buffsize = 16;buffsize < 32*1024;buffsize +=(buffsize/2048 + 1)*16)
		{

			for (i=0;i<sizeof(cfg)/sizeof(TestCfg);i++)
			{
				double min,max,avg;
				cfg[i].buffer_size = buffsize;
				cfg[i].num_threads = test_threads[nthr];
				if (i == 0) 			printf("%d,%d,",cfg[i].num_threads,cfg[i].buffer_size);
				cmd.BuildTest(&cfg[i]);
				cmd.ExecuteTest(&min,&max,&avg);
				printf("%lf,%lf,%lf,",min/buffsize,max/buffsize,avg/buffsize);
				cmd.FreeTest();
			}
			printf("\n");
		}
	}

}
