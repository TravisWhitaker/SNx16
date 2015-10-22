{-|
Module      : SNXi.Types
Description : VM Types
Copyright   : Chris Ranc, Travis Whitaker 2015
License     : All Rights Reserved
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : POSIX

SNx16 virtual machine data types.
-}

{-# LANGUAGE BangPatterns #-}

module SNXi.Types where

import Data.Word

import Data.Map.Strict as M

import Data.Text as T

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU

data Reg = SP
         | R1
         | R2
         | R3
         | R4
         | R5
         | R6
         | R7
         | R8
         | R9
         | R10
         | R11
         | R12
         | R13
         | R14
         | R15
         | R16
         | R17
         | R18
         | R19
         | R20
         | R21
         | R22
         | R23
         | R24
         | R25
         | R26
         | R27
         | R28
         | R29
         | R30
         | R31
         deriving (Eq, Ord, Show)

data RegW = R Reg
          | U Reg
          | L Reg
          deriving (Eq, Ord, Show)

data CPU = CPU {
    sp    :: !Word16
  , r1    :: !Word16
  , r2    :: !Word16
  , r3    :: !Word16
  , r4    :: !Word16
  , r5    :: !Word16
  , r6    :: !Word16
  , r7    :: !Word16
  , r8    :: !Word16
  , r9    :: !Word16
  , r10   :: !Word16
  , r11   :: !Word16
  , r12   :: !Word16
  , r13   :: !Word16
  , r14   :: !Word16
  , r15   :: !Word16
  , r16   :: !Word16
  , r17   :: !Word16
  , r18   :: !Word16
  , r19   :: !Word16
  , r20   :: !Word16
  , r21   :: !Word16
  , r22   :: !Word16
  , r23   :: !Word16
  , r24   :: !Word16
  , r25   :: !Word16
  , r26   :: !Word16
  , r27   :: !Word16
  , r28   :: !Word16
  , r29   :: !Word16
  , r30   :: !Word16
  , r31   :: !Word16
  , dmem  :: VU.Vector Word8 -- ^ Limited to 2^16 elements.
  , pc    :: !Word16
  , pmem  :: V.Vector Op     -- ^ Limited to 2^16 elements.
  , zero  :: !Bool
  , neg   :: !Bool
  , carry :: !Bool
  } deriving (Eq, Ord, Show)

data Op = AddR RegW RegW !Word8 -- ^ Actually restricted to 2 bits.
        | AddI RegW !Word8      -- ^ Actually restricted to 7 bits.
        | MulR RegW RegW !Word8 -- ^ Actually restricted to 2 bits.
        | MulI RegW !Word8      -- ^ Actually restricted to 7 bits.
        | JI !Word16            -- ^ Actually restricted to 12 bits.
        | JR RegW !Word8        -- ^ Actually restricted to 4 bits.
        | JE RegW !Word8        -- ^ Actually restricted to 4 bits.
        | JH RegW !Word8        -- ^ Actually restricted to 4 bits.
        | JL RegW !Word8        -- ^ Actually restricted to 4 bits.
        | JC RegW !Word8        -- ^ Actually restricted to 4 bits.
        | MovR RegW RegW
        | MovI RegW !Word8      -- ^ Actually restricted to 7 bits.
        | MovL RegW RegW
        | MovS RegW RegW
        deriving (Eq, Ord, Show)

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
