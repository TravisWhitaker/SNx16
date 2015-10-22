{-|
Module      : SNXi.Exec
Description : VM Execution
Copyright   : Chris Ranc, Travis Whitaker 2015
License     : All Rights Reserved
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : POSIX

SNx16 virtual machine execution environment
-}

module SNXi.Exec where

import Data.Word

import Data.Int

import Data.Bits

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU

import SNXi.Types
import SNXi.Op

bootCPU :: V.Vector Op -> CPU
bootCPU o = CPU {
    sp    = (2^16 - 1)
  , r1    = 0
  , r2    = 0
  , r3    = 0
  , r4    = 0
  , r5    = 0
  , r6    = 0
  , r7    = 0
  , r8    = 0
  , r9    = 0
  , r10   = 0
  , r11   = 0
  , r12   = 0
  , r13   = 0
  , r14   = 0
  , r15   = 0
  , r16   = 0
  , r17   = 0
  , r18   = 0
  , r19   = 0
  , r20   = 0
  , r21   = 0
  , r22   = 0
  , r23   = 0
  , r24   = 0
  , r25   = 0
  , r26   = 0
  , r27   = 0
  , r28   = 0
  , r29   = 0
  , r30   = 0
  , r31   = 0
  , dmem  = VU.generate (2^16) (const 0)
  , pc    = 0
  , pmem  = o
  , zero  = False
  , neg   = False
  , carry = False
  }

cycleCPU :: CPU -> Maybe CPU
cycleCPU c = (flip op c) <$> ((pmem c) V.!? (fromIntegral (pc c)))
