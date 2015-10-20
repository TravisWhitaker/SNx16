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

import Data.Word

import qualified Data.IntMap.Strict as M

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
    sp    :: Word16
  , r1    :: Word16
  , r2    :: Word16
  , r3    :: Word16
  , r4    :: Word16
  , r5    :: Word16
  , r6    :: Word16
  , r7    :: Word16
  , r8    :: Word16
  , r9    :: Word16
  , r10   :: Word16
  , r11   :: Word16
  , r12   :: Word16
  , r13   :: Word16
  , r14   :: Word16
  , r15   :: Word16
  , r16   :: Word16
  , r17   :: Word16
  , r18   :: Word16
  , r19   :: Word16
  , r20   :: Word16
  , r21   :: Word16
  , r22   :: Word16
  , r23   :: Word16
  , r24   :: Word16
  , r25   :: Word16
  , r26   :: Word16
  , r27   :: Word16
  , r28   :: Word16
  , r29   :: Word16
  , r30   :: Word16
  , r31   :: Word16
  , mem   :: M.IntMap Word16
  , zero  :: Bool
  , carry :: Bool
  , neg   :: Bool
  } deriving (Eq, Ord, Show)

data Op = AddR RegW RegW Word8 -- ^ Actually restricted to 2 bits.
        | AddI RegW Word8      -- ^ Actually restricted to 7 bits.
        | MulR RegW RegW Word8 -- ^ Actually restricted to 2 bits.
        | MulI RegW Word8      -- ^ Actually restricted to 7 bits.
        | JI Word16            -- ^ Actually restricted to 12 bits.
        | JR RegW Word8        -- ^ Actually restricted to 8 bits.
        | JE RegW Word8        -- ^ Actually restricted to 8 bits.
        | JH RegW Word8        -- ^ Actually restricted to 8 bits.
        | JL RegW Word8        -- ^ Actually restricted to 8 bits.
        | JC RegW Word8        -- ^ Actually restricted to 8 bits.
        | MovR RegW RegW
        | MovL RegW RegW
        | MovS RegW RegW
