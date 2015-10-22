{-|
Module      : SNXi.Parser
Description : Assembler Directive Parser
Copyright   : Chris Ranc, Travis Whitaker 2015
License     : All Rights Reserved
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : POSIX

SNx16 assembly language parser. Statements consist essentially of these parts:

@
label: mnemonic a0, a1, a2 ; /comment/
@

Labels on each line are optional, but must be followed by a colon when present.
The SNx16 architecture has 32 registers. One register, number zero, has the
special name @sp@. All other registers are numbered from 1 to 31. Registers may
be accessed in three modes: full-width mode, upper byte mode, and lower byte
mode. Prefixing the register name with @r@ indicates full-width mode, @u@
indicates upper byte mode, and @l@ indicates lower-byte mode.

Some mnemonics also accept immediate values. Decimal immediate values are not
specially indicated, but may be negated with a prefix minus sign. Hex immediate
values are prefixed with @0x@. Binary immediate values are prefixed with @0b@.

SNXi has eight instructions, with the following forms. @ImN@ indicates an
immediate value with /N/ bits.

Signed Addition (signed immediates, only full-width):
@
add ra, rb   ; /ra <- ra + rb/
add ra, Im7  ; /ra <- ra + Im7/
add ra, rb>>Im2 ; /ra <- ra + (rb>>Im2)
@

Signed Multiplicaiton (signed immediates, only full-width):
@
mul ra, rb       ; /ra <- Lower16(ra * rb)/
mul ra, Im7      ; /ra <- Lower16(ra * Im7)/
mul ra, rb>>Im2  ; /ra <- Lower16(ra * (rb>>Im2))/
@

Unconditional Jump (unsigned immediates, only full-width):
@
j Im12       ; /PC <- PC + Im12/
j [ra]       ; /PC <- PC + ra/
j [ra + Im4] ; /PC <- PC + ra + Im4/
@

Jump If Equal (unsigned immediates, only full-width):
@
je [ra]       ; /PC <- PC + ra/
je [ra + Im4] ; /PC <- PC + Im4/

Jump Higher (unsigned immediates, only full-width):
@
jh [ra] ; /PC <- PC + ra/
jh [ra + Im4] ; /PC <- PC + Im4/
@

Jump Lower (unsigned immediates, only full-width):
@
jl [ra] ; /PC <- PC + ra/
jl [ra + Im4] ; /PC <- PC + Im4/
@

Jump If Carry Set (unsigned immediates, only full-width):
@
jc [ra] ; /PC <- PC + ra/
jc [ra + Im4] ; /PC <- PC + Im4/
@

Data Movement (any width mode):
@
mov ra, rb   ; /ra <- rb/
mov ra, [rb] ; /ra <- M[rb]/
mov [ra], rb ; /M[ra] <- rb/
@

Note that according to the ISA instruction encoding specification, only @mov@
instructions can operate on registers in any width mode. However, this
interpreter will allow the use of any register width mode with any instruction.
This behavior is trivial to simulate, but such instructions aren't encodable.
-}

{-# LANGUAGE OverloadedStrings #-}

module SNXi.Parser where

import Control.Applicative

import Control.Monad

import Data.Maybe

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.Map.Strict as M

import qualified Data.Vector.Unboxed as V

import Data.Attoparsec.ByteString.Char8 as A
import Data.Attoparsec.ByteString.Lazy  as AL

checkWidth8 :: Int -> Word8 -> Bool
checkWidth8 w v = (fromIntegral v) < 2^w

checkWidth16 :: Int -> Word8 -> Bool
checkWidth16 w v = (fromIntegral v) < 2^w

validLabel :: B.ByteString -> Bool
validLabel = B.foldr f True
    where f _ False = False
          f c True  = and [ (c /= ' ')
                          , (c /= '\t')
                          , (c /= ',')
                          , (c /= ';')
                          ]

skipSpace :: A.Parser ()
skipSpace = A.takeWhile (\c -> (c /= ' ') && (\c /= '\t'))

-- | Assemble and link executable object code.
objs :: A.Parser (V.Vector Op)
    (lb, os) <- translate

translate :: A.Parser (M.Map B.ByteString Word16, V.Vector (Op, Maybe B.ByteString))
translate = fmap (\(a, b) -> (a, V.fromList b)) tr
    where tr = dirs >>= foldM addDir (M.empty, [])

dirs :: A.Parser [(Maybe B.ByteString, Op, Maybe B.ByteString)]
dirs = (catMaybes <$> many1 dir) <* A.endOfInput

dir :: A.Parser Maybe (Maybe B.ByteString, Op, Maybe B.ByteString)
dir = blankLine <|> ((commentLine <|> labeledDir <|> unlabeledDir) <* (A.char '\n'))
    where blankLine = A.char '\n' >> return Nothing
          commentLine = A.char ';' >> A.takeWhile (/= '\n') >> return Nothing
          labeledDir = do
                l <- A.takeWhile (/= ':')
                skipSpace
                (o, l') <- mnemonic
                ((skipSpace >> commentLine) <|> commentLine <|> return ())
                if validLabel l then return (Just (Just l, o, l'))
                                else fail "bad label, must not contain space, tab, comma, or semicolon."
          unlabeledDir = do
                skipSpace
                (o, l') <- mnemonic
                ((skipSpace >> commentLine) <|> commentLine <|> return ())
                return (Just (Nothing, o, l'))
