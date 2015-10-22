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

import qualified Data.Map.Strict as M

import qualified Data.Text as T

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

showCPU :: CPU -> String
showCPU c = concat [ "CPU | Zero: "
                   , show (zero c)
                   , "\tNeg: "
                   , show (neg c)
                   , "\tCarry: "
                   , show (carry c)
                   , "\nPC: "
                   , show (pc c)
                   , "\nSP:  "
                   , show (sp c)
                   , "\tR1:  "
                   , show (r1 c)
                   , "\tR2:  "
                   , show (r2 c)
                   , "\tR3:  "
                   , show (r3 c)
                   , "\tR4:  "
                   , show (r4 c)
                   , "\tR5:  "
                   , show (r5 c)
                   , "\nR6:  "
                   , show (r6 c)
                   , "\tR7:  "
                   , show (r7 c)
                   , "\tR8:  "
                   , show (r8 c)
                   , "\tR9:  "
                   , show (r9 c)
                   , "\tR10: "
                   , show (r10 c)
                   , "\tR11: "
                   , show (r11 c)
                   , "\nR12: "
                   , show (r12 c)
                   , "\tR13: "
                   , show (r13 c)
                   , "\tR14: "
                   , show (r14 c)
                   , "\tR15: "
                   , show (r15 c)
                   , "\tR16: "
                   , show (r16 c)
                   , "\tR17: "
                   , show (r17 c)
                   , "\nR18: "
                   , show (r18 c)
                   , "\tR19: "
                   , show (r19 c)
                   , "\tR20: "
                   , show (r20 c)
                   , "\tR21: "
                   , show (r21 c)
                   , "\tR22: "
                   , show (r22 c)
                   , "\tR23: "
                   , show (r23 c)
                   , "\nR24: "
                   , show (r24 c)
                   , "\tR25: "
                   , show (r25 c)
                   , "\tR26: "
                   , show (r26 c)
                   , "\tR27: "
                   , show (r27 c)
                   , "\tR28: "
                   , show (r28 c)
                   , "\tR29: "
                   , show (r29 c)
                   , "\nR30: "
                   , show (r30 c)
                   , "\tR31: "
                   , show (r31 c)
                   ]

showCPUmem :: CPU -> String
showCPUmem c = (showCPU c) ++ "\n" ++ show (dmem c)
