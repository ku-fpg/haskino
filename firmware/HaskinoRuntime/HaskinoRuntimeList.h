/******************************************************************************
 *
 * Module      :  HaskinoRuntimeList
 * Copyright   :  (c) University of Kansas
 * License     :  BSD3
 * Stability   :  experimental
 *
 * Haskino Runtime List handling header
 *****************************************************************************/

#ifndef HaskinoRuntimeListH
#define HaskinoRuntimeListH

byte *listAlloc(int n);
void listFree(byte *l);
void listAssign(byte **v, byte *l);

#endif /* HaskinoRuntimeListH */
