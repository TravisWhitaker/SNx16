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
-}

module SNXi.Parser where

import Control.Applicative

import Data.Attoparsec as A


