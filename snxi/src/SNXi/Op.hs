{-|
Module      : SNXi.Op
Description : VM Instruction Implementations
Copyright   : Chris Ranc, Travis Whitaker 2015
License     : All Rights Reserved
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : POSIX

SNx16 virtual machine insruction implementations.
-}

module SNXi.Op where

import Data.Word

import Data.Int

import Data.Bits

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU

import SNXi.Types

getReg :: Reg -> CPU -> Word16
getReg SP  = sp
getReg R1  = r1
getReg R2  = r2
getReg R3  = r3
getReg R4  = r4
getReg R5  = r5
getReg R6  = r6
getReg R7  = r7
getReg R8  = r8
getReg R9  = r9
getReg R10 = r10
getReg R11 = r11
getReg R12 = r12
getReg R13 = r13
getReg R14 = r14
getReg R15 = r15
getReg R16 = r16
getReg R17 = r17
getReg R18 = r18
getReg R19 = r19
getReg R20 = r20
getReg R21 = r21
getReg R22 = r22
getReg R23 = r23
getReg R24 = r24
getReg R25 = r25
getReg R26 = r26
getReg R27 = r27
getReg R28 = r28
getReg R29 = r29
getReg R30 = r30
getReg R31 = r31

getRegW :: RegW -> CPU -> Word16
getRegW (R r) = getReg r
getRegW (U r) = (0xFF00 .&.) . (getReg r)
getRegW (L r) = (0x00FF .&.) . (getReg r)

setReg :: Reg -> CPU -> Word16 -> CPU
setReg SP c  = (\v -> c {sp  = v})
setReg R1 c  = (\v -> c {r1  = v})
setReg R2 c  = (\v -> c {r2  = v})
setReg R3 c  = (\v -> c {r3  = v})
setReg R4 c  = (\v -> c {r4  = v})
setReg R5 c  = (\v -> c {r5  = v})
setReg R6 c  = (\v -> c {r6  = v})
setReg R7 c  = (\v -> c {r7  = v})
setReg R8 c  = (\v -> c {r8  = v})
setReg R9 c  = (\v -> c {r9  = v})
setReg R10 c = (\v -> c {r10 = v})
setReg R11 c = (\v -> c {r11 = v})
setReg R12 c = (\v -> c {r12 = v})
setReg R13 c = (\v -> c {r13 = v})
setReg R14 c = (\v -> c {r14 = v})
setReg R15 c = (\v -> c {r15 = v})
setReg R16 c = (\v -> c {r16 = v})
setReg R17 c = (\v -> c {r17 = v})
setReg R18 c = (\v -> c {r18 = v})
setReg R19 c = (\v -> c {r19 = v})
setReg R20 c = (\v -> c {r20 = v})
setReg R21 c = (\v -> c {r21 = v})
setReg R22 c = (\v -> c {r22 = v})
setReg R23 c = (\v -> c {r23 = v})
setReg R24 c = (\v -> c {r24 = v})
setReg R25 c = (\v -> c {r25 = v})
setReg R26 c = (\v -> c {r26 = v})
setReg R27 c = (\v -> c {r27 = v})
setReg R28 c = (\v -> c {r28 = v})
setReg R29 c = (\v -> c {r29 = v})
setReg R30 c = (\v -> c {r30 = v})
setReg R31 c = (\v -> c {r31 = v})

setRegW :: RegW -> CPU -> Word16 -> CPU
setRegW (R r) c v = (setReg r) c v
setRegW (U r) c v = (setReg r) c (0xFF00 .&. v)
setRegW (L r) c v = (setReg r) c (0x00FF .&. v)

getDmemB :: Word16 -> CPU -> Word8
getDmemB a c = (dmem c) VU.! (fromIntegral a)

getDmemW :: Word16 -> CPU -> Word16
getDmemW a c = let m = dmem c
                   i = fromIntegral a
                   l = m VU.! i
                   h = m VU.! (i+1)
                   l' :: Word16
                   l' = fromIntegral l
                   h' :: Word16
                   h' = (fromIntegral h) `shiftL` 8
               in h' .|. l'

setDmemB :: Word16 -> Word8 -> CPU -> CPU
setDmemB a v c = c {dmem = (dmem c) VU.// [((fromIntegral a), v)]}

setDmemW :: Word16 -> Word16 -> CPU -> CPU
setDmemW a v c = let l :: Word8
                     l = fromIntegral (0x00FF .&. v)
                     h :: Word8
                     h = fromIntegral ((0xFF00 .&. v) `div` 256)
                     i = fromIntegral a
                 in c {dmem = (dmem c) VU.// [(i, l), (i+1, h)]}

getPmem :: Word16 -> CPU -> Op
getPmem i c = (pmem c) V.! (fromIntegral i)

signExtend :: Word8 -> Word16
signExtend b = let b' :: Int8
                   b' = fromIntegral b
                   b'' :: Int16
                   b'' = fromIntegral b'
               in fromIntegral b''

signedAdd :: Word16 -> Word16 -> Word16
signedAdd a b = let a' :: Int16
                    a' = fromIntegral a
                    b' :: Int16
                    b' = fromIntegral b
                in fromIntegral $ a' + b'

signedMul :: Word16 -> Word16 -> Word16
signedMul a b = let a' :: Int16
                    a' = fromIntegral a
                    b' :: Int16
                    b' = fromIntegral b
                in fromIntegral $ a' * b'

incPC :: CPU -> CPU
incPC c = c {pc = (pc c) + 1}

setPC :: Word16 -> CPU -> CPU
setPC i c = c {pc = i}

-- | Please help.
addFlags :: Word16 -> Word16 -> Word16 -> (Bool, Bool, Bool)
addFlags a b s = let a' :: Int16
                     a' = fromIntegral a
                     a'' :: Int
                     a'' = fromIntegral a'
                     b' :: Int16
                     b' = fromIntegral b
                     b'' :: Int
                     b'' = fromIntegral b'
                     s' :: Int16
                     s' = fromIntegral s
                     z = s == 0
                     n = s < 0
                     c = (a'' + b'') > (fromIntegral s')
                 in (z, n, c)

-- | Please help.
mulFlags :: Word16 -> Word16 -> Word16 -> (Bool, Bool, Bool)
mulFlags a b s = let a' :: Int16
                     a' = fromIntegral a
                     a'' :: Int
                     a'' = fromIntegral a'
                     b' :: Int16
                     b' = fromIntegral b
                     b'' :: Int
                     b'' = fromIntegral b'
                     s' :: Int16
                     s' = fromIntegral s
                     z = s == 0
                     n = s < 0
                     c = (a'' * b'') > (fromIntegral s')
                 in (z, n, c)

setFlags :: (Bool, Bool, Bool) -> CPU -> CPU
setFlags (z, n, ca) c = c {zero = z, neg = n, carry = ca}

-- | Operators are assumed to be correct by construction.
op :: Op -> CPU -> CPU
op (AddR ra rb shr) c = let a = getRegW ra c
                            b = (getRegW rb c) `shiftR` (fromIntegral shr)
                            s = signedAdd a b
                            f = addFlags a b s
                        in incPC $ setFlags f $ setRegW ra c s
op (AddI ra im)     c = let a = getRegW ra c
                            i = signExtend im
                            s = signedAdd a i
                            f = addFlags a i s
                        in incPC $ setFlags f $ setRegW ra c s
op (MulR ra rb shr) c = let a = getRegW ra c
                            b = (getRegW rb c) `shiftR` (fromIntegral shr)
                            s = signedMul a b
                            f = mulFlags a b s
                        in incPC $ setFlags f $ setRegW ra c s
op (MulI ra im)     c = let a = getRegW ra c
                            i = signExtend im
                            s = signedMul a i
                            f = mulFlags a i s
                        in incPC $ setRegW ra c s
op (JI im)          c = setPC (signedAdd (pc c) im) c
op (JR ra im)       c = setPC (signedAdd (getRegW ra c) (fromIntegral im)) c
op (JE ra im)       c = if (zero c)
                        then setPC (signedAdd (getRegW ra c) (fromIntegral im)) c
                        else incPC c
op (JH ra im)       c = if (neg c)
                        then incPC c
                        else setPC (signedAdd (getRegW ra c) (fromIntegral im)) c

op (JL ra im)       c = if (neg c)
                        then setPC (signedAdd (getRegW ra c) (fromIntegral im)) c
                        else incPC c
op (JC ra im)       c = if (carry c)
                        then setPC (signedAdd (getRegW ra c) (fromIntegral im)) c
                        else incPC c
op (MovR rd rs)     c = let s = getRegW rs c
                        in incPC $ setRegW rd c s
op (MovI rd im)     c = let i = fromIntegral im
                        in incPC $ setRegW rd c i
op (MovL (R rd) ra) c = let v = getDmemW (getRegW ra c) c
                        in incPC $ setRegW (R rd) c v
op (MovL (U rd) ra) c = let v  = getDmemB (getRegW ra c) c
                            v' :: Word16
                            v' = (fromIntegral v) `shiftL` 8
                        in incPC $ setRegW (U rd) c v'
op (MovL (L rd) ra) c = let v = getDmemB (getRegW ra c) c
                            v' :: Word16
                            v' = fromIntegral v
                        in incPC $ setRegW (L rd) c v'
op (MovS ra (R rs)) c = let a = getRegW ra c
                            v = getRegW (R rs) c
                        in incPC $ setDmemW a v c
op (MovS ra (U rs)) c = let a = getRegW ra c
                            v = getRegW (U rs) c
                            v' :: Word8
                            v' = fromIntegral (v `div` 256)
                        in incPC $ setDmemB a v' c
op (MovS ra (L rs)) c = let a = getRegW ra c
                            v = getRegW (L rs) c
                            v' :: Word8
                            v' = fromIntegral (v .&. 0x00FF)
                        in incPC $ setDmemB a v' c
