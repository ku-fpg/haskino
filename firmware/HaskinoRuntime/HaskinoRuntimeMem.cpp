/******************************************************************************
 *
 * Module      :  HaskinoRuntimeMem
 * Copyright   :  (c) University of Kansas
 * License     :  BSD3
 * Stability   :  experimental
 *
 * Haskino Runtime Memory Management module
 *****************************************************************************/

#include <Arduino.h>
#include "HaskinoRuntime.h"

// Malloc functions

typedef struct __attribute__ ((packed)) mem_block_t
    {
    struct mem_block_t  *next;
    size_t              size;
    } MEM_BLOCK;

#define MEM_HDR_SIZE (sizeof(struct mem_block_t))

static MEM_BLOCK *free16;
static MEM_BLOCK *free32;
static MEM_BLOCK *free64;
static MEM_BLOCK *free128;
static MEM_BLOCK *free256;

static uint8_t memBlocks16[NUM_MEM_BLK_16][MEM_HDR_SIZE+16];
static uint8_t memBlocks32[NUM_MEM_BLK_32][MEM_HDR_SIZE+32];
static uint8_t memBlocks64[NUM_MEM_BLK_64][MEM_HDR_SIZE+64];
static uint8_t memBlocks128[NUM_MEM_BLK_128][MEM_HDR_SIZE+128];
static uint8_t memBlocks256[NUM_MEM_BLK_256][MEM_HDR_SIZE+256];

void haskinoMemInit(void)
    {
    int i;
    MEM_BLOCK *blk;

    for (i=0;i<NUM_MEM_BLK_16;i++)
        {
        blk = (MEM_BLOCK *) memBlocks16[i];
        blk->size = 16;
        if (i == NUM_MEM_BLK_16-1)
            blk->next = NULL;
        else
            blk->next = (MEM_BLOCK *) &memBlocks16[i+1];
        }
    free16 = (MEM_BLOCK *) memBlocks16;

    for (i=0;i<NUM_MEM_BLK_32;i++)
        {
        blk = (MEM_BLOCK *) memBlocks32[i];
        blk->size = 32;
        if (i == NUM_MEM_BLK_32-1)
            blk->next = NULL;
        else
            blk->next = (MEM_BLOCK *) &memBlocks32[i+1];
        }
    free32 = (MEM_BLOCK *) memBlocks32;

    for (i=0;i<NUM_MEM_BLK_64;i++)
        {
        blk = (MEM_BLOCK *) memBlocks64[i];
        blk->size = 64;
        if (i == NUM_MEM_BLK_64-1)
            blk->next = NULL;
        else
            blk->next = (MEM_BLOCK *) &memBlocks64[i+1];
        }
    free64 = (MEM_BLOCK *) memBlocks64;

    for (i=0;i<NUM_MEM_BLK_128;i++)
        {
        blk = (MEM_BLOCK *) memBlocks128[i];
        blk->size = 128;
        if (i == NUM_MEM_BLK_128-1)
            blk->next = NULL;
        else
            blk->next = (MEM_BLOCK *) &memBlocks128[i+1];
        }
    free128 = (MEM_BLOCK *) memBlocks128;

    for (i=0;i<NUM_MEM_BLK_256;i++)
        {
        blk = (MEM_BLOCK *) memBlocks256[i];
        blk->size = 256;
        if (i == NUM_MEM_BLK_256-1)
            blk->next = NULL;
        else
            blk->next = (MEM_BLOCK *) &memBlocks256[i+1];
        }
    free256 = (MEM_BLOCK *) memBlocks256;
    }

static void *haskinoBlockAlloc(MEM_BLOCK **freeList)
    {
    MEM_BLOCK *block;

    if (*freeList == NULL)
        {
        return NULL;
        }
    else
        {
        block = *freeList;
        *freeList = block->next;
        }

    return ((byte *) block) + sizeof(MEM_BLOCK);
    }

void *haskinoMalloc(size_t size)
    {
    if (size <= 16)
        return haskinoBlockAlloc(&free16);
    else if (size <= 32)
        return haskinoBlockAlloc(&free32);
    else if (size <= 64)
        return haskinoBlockAlloc(&free64);
    else if (size <= 128)
        return haskinoBlockAlloc(&free128);
    else if (size <= 256)
        return haskinoBlockAlloc(&free256);
    else
        return NULL;
    }

static void haskinoBlockFree(MEM_BLOCK **freeList, MEM_BLOCK *block)
    {
    block->next = *freeList;
    *freeList = block;
    }

void haskinoFree(void *mem)
    {
    MEM_BLOCK *block;
    size_t size;

    block = (MEM_BLOCK *) (((byte *) mem) - sizeof(MEM_BLOCK));
    size = block->size;

    if (size == 16)
        haskinoBlockFree(&free16, block);
    else if (size == 32)
        haskinoBlockFree(&free32, block);
    else if (size == 64)
        haskinoBlockFree(&free64, block);
    else if (size == 128)
        haskinoBlockFree(&free128, block);
    else if (size == 0)
        haskinoBlockFree(&free256, block);
    }

