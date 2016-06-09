/******************************************************************************
 *
 * Module      :  HaskinoRuntimeMem
 * Copyright   :  (c) University of Kansas
 * License     :  BSD3
 * Stability   :  experimental
 *
 * Haskino Runtime Memory Allocation header
 *****************************************************************************/

#ifndef HaskinoRuntimeMemH
#define HaskinoRuntimeMemH

// Mem Functions

void haskinoMemInit(void);
void *haskinoMalloc(size_t size);
void haskinoFree(void *mem);

// Static Block Allocations Configuration

#define NUM_MEM_BLK_16  16
#define NUM_MEM_BLK_32  2
#define NUM_MEM_BLK_64  2
#define NUM_MEM_BLK_128 0
#define NUM_MEM_BLK_256 0

#endif /* HaskinoRuntimeMemH */

