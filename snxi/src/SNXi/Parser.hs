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
(This parser currenly only supports decimal literals.)

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
mov ra, Im7  ; /ra <- zeroEx(Im7)
mov ra, [rb] ; /ra <- M[rb]/
mov [ra], rb ; /M[ra] <- rb/
@

Note that according to the ISA instruction encoding specification, only @mov@
instructions can operate on registers in any width mode. However, this
interpreter will allow the use of any register width mode with any instruction.
This behavior is trivial to simulate, but such instructions aren't encodable.
-}

{-# LANGUAGE OverloadedStrings, TupleSections #-}

module SNXi.Parser where

import Control.Applicative

import Control.Monad

import Data.List

import Data.Maybe

import Data.Word

import Data.Int

import Data.Bits

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.Map.Strict as M

import qualified Data.Vector as V

import Data.Attoparsec.ByteString.Char8 as A hiding (skipSpace)
import Data.Attoparsec.ByteString.Lazy  as AL

import SNXi.Types

checkWidth :: Integral a => a -> a -> Bool
checkWidth w v = v < 2^w

validLabel :: B.ByteString -> Bool
validLabel = B.foldr f True
    where f _ False = False
          f c True  = and [ (c /= ' ')
                          , (c /= '\t')
                          , (c /= ',')
                          , (c /= ';')
                          , (c /= '[')
                          , (c /= ']')
                          ]

skipSpace :: A.Parser ()
skipSpace = A.takeWhile (\c -> (c == ' ') || (c == '\t')) >> return ()

skipArgSep :: A.Parser ()
skipArgSep = skipSpace >> (A.char ',') >> skipSpace

-- | Assemble and link executable object code.
objs :: A.Parser (V.Vector Op)
objs = translate >>= link
    where link (sym, is) = return $ V.imap (deref sym) is
          deref m i (o, Nothing) = o
          deref m i ((JI _), Just l) = case M.lookup l m of Nothing -> error "no such label"
                                                            (Just r) -> JI (rel (fromIntegral i) r)
          deref m i ((JEI _), Just l) = case M.lookup l m of Nothing -> error "no such label"
                                                             (Just r) -> JEI (rel (fromIntegral i) r)
          deref m i ((JHI _), Just l) = case M.lookup l m of Nothing -> error "no such label"
                                                             (Just r) -> JHI (rel (fromIntegral i) r)
          deref m i ((JLI _), Just l) = case M.lookup l m of Nothing -> error "no such label"
                                                             (Just r) -> JLI (rel (fromIntegral i) r)
          deref m i ((JCI _), Just l) = case M.lookup l m of Nothing -> error "no such label"
                                                             (Just r) -> JCI (rel (fromIntegral i) r)
          rel :: Word16 -> Word16 -> Word16
          rel i r = let i' :: Int
                        i' = fromIntegral i
                        r' :: Int
                        r' = fromIntegral r
                        o = r - i
                        o' :: Int16
                        o' = fromIntegral o
                        o'' :: Word16
                        o'' = fromIntegral o'
                    in o''

-- | Build symbol table.
translate :: A.Parser (M.Map B.ByteString Word16, V.Vector (Op, Maybe B.ByteString))
translate = dirs >>= val
    where val ds   = if (length ds) < (2^16) then return (trans ds) else fail "code segment too large"
          trans ds = (symb ds, vect ds)
          vect ds  = V.fromList $ map (\(_, o, l) -> (o, l)) ds
          symb ds  = foldl' insym M.empty $ zip [0..] $ map (\(l, _, _) -> l) ds
          insym m (i,Nothing) = m
          insym m (i,Just l)  = M.insert l i m

-- | Get list of directives, with optional labels and references.
dirs :: A.Parser [(Maybe B.ByteString, Op, Maybe B.ByteString)]
dirs = (catMaybes <$> many1 dir) <* A.endOfInput

-- | Parse a line, potentially producing a directive.
dir :: A.Parser (Maybe (Maybe B.ByteString, Op, Maybe B.ByteString))
dir = (blankLine <|> (((skipSpace >> commentLine) <|> unlabeledDir <|> labeledDir) <* (A.char '\n'))) <|> fail "bad directive"
    where blankLine = A.char '\n' >> return Nothing
          commentLine = A.char ';' >> A.takeWhile (/= '\n') >> return Nothing
          labeledDir = do
                l <- lab
                ":"
                skipSpace
                (o, l') <- mnemonic
                ((skipSpace >> commentLine) <|> commentLine <|> return Nothing)
                return (Just (Just l, o, l'))
          unlabeledDir = do
                skipSpace
                (o, l') <- mnemonic
                ((skipSpace >> commentLine) <|> commentLine <|> return Nothing)
                return (Just (Nothing, o, l'))

mnemonic :: A.Parser (Op, Maybe B.ByteString)
mnemonic = (nl <$> (add <|> mul <|> mov)) <|> je <|> jh <|> jl <|> jc <|> j <|> fail "malformed mnemonic"
    where nl = (,Nothing)

add :: A.Parser Op
add = "add" >> skipSpace >> (addS <|> addR <|> addI)
    where addR = AddR <$> (regw <* skipArgSep) <*> regw <*> pure 0
          addI = AddI <$> (regw <* skipArgSep) <*> imm 7
          addS = AddR <$> (regw <* skipArgSep) <*> (regw <* ">>") <*> imm 2

mul :: A.Parser Op
mul = "mul" >> skipSpace >> (mulR <|> mulI <|> mulS)
    where mulR = MulR <$> (regw <* skipArgSep) <*> regw <*> pure 0
          mulI = MulI <$> (regw <* skipArgSep) <*> imm 7
          mulS = MulR <$> (regw <* skipArgSep) <*> (regw <* ">>") <*> imm 2

j :: A.Parser (Op, Maybe B.ByteString)
j = "j" >> skipSpace >> (jr <|> ji)
    where ji = ((JI 0,) . Just) <$> lab
          jr = (\(r, d) -> (JR r d, Nothing)) <$> rega'

je :: A.Parser (Op, Maybe B.ByteString)
je = "je" >> skipSpace >> (jer <|> jei)
    where jei = ((JEI 0,) . Just) <$> lab
          jer = (\(r, d) -> ((JE r d), Nothing)) <$> rega'

jh :: A.Parser (Op, Maybe B.ByteString)
jh = "jh" >> skipSpace >> (jhr <|> jhi)
    where jhi = ((JHI 0,) . Just) <$> lab
          jhr = (\(r, d) -> ((JH r d), Nothing)) <$> rega'

jl :: A.Parser (Op, Maybe B.ByteString)
jl = "jl" >> skipSpace >> (jlr <|> jli)
    where jli = ((JLI 0,) . Just) <$> lab
          jlr = (\(r, d) -> ((JL r d), Nothing)) <$> rega'

jc :: A.Parser (Op, Maybe B.ByteString)
jc = "jc" >> skipSpace >> (jcr <|> jci)
    where jci = ((JCI 0,) . Just) <$> lab
          jcr = (\(r, d) -> ((JC r d), Nothing)) <$> rega'

mov :: A.Parser Op
mov = "mov" >> skipSpace >> (movR <|> movI <|> movL <|> movS <|> fail "malformed mov")
    where movR = MovR <$> (regw <* skipArgSep) <*> regw
          movI = MovI <$> (regw <* skipArgSep) <*> imm 7
          movL = MovL <$> (regw <* skipArgSep) <*> rega
          movS = MovS <$> (rega <* skipArgSep) <*> regw

regw :: A.Parser RegW
--regw = rf <|> ru <|> rl
regw = rf

rega :: A.Parser RegW
--rega = "[" >> skipSpace >> ((rf <|> ru <|> rl) <* (skipSpace >> "]"))
rega = "[" >> skipSpace >> (rf <* (skipSpace >> "]"))

rega' :: A.Parser (RegW, Word8)
--rega' = ((,0) <$> rega) <|> rega''
--    where rega'' = do
--            "["
--            skipSpace
--            r <- (rf <|> ru <|> rl)
--            skipSpace
--            "+"
--            skipSpace
--            i <- imm 4
--            skipSpace
--            "]"
--            return (r, i)
rega' = ((,0) <$> rega) <|> rega''
    where rega'' = do
            "["
            skipSpace
            r <- rf
            skipSpace
            "+"
            skipSpace
            i <- imm 4
            skipSpace
            "]"
            return (r, i)

rf :: A.Parser RegW
rf = "r" >> R <$> rn

--ru :: A.Parser RegW
--ru = "u" >> U <$> rn

--rl :: A.Parser RegW
--rl = "l" >> L <$> rn

rn :: A.Parser Reg
rn = let pSP  = "sp" >> return SP
         p1   = "1"  >> return R1
         p2   = "2"  >> return R2
         p3   = "3"  >> return R3
         p4   = "4"  >> return R4
         p5   = "5"  >> return R5
         p6   = "6"  >> return R6
         p7   = "7"  >> return R7
         p8   = "8"  >> return R8
         p9   = "9"  >> return R9
         p10  = "10" >> return R10
         p11  = "11" >> return R11
         p12  = "12" >> return R12
         p13  = "13" >> return R13
         p14  = "14" >> return R14
         p15  = "15" >> return R15
         p16  = "16" >> return R16
         p17  = "17" >> return R17
         p18  = "18" >> return R18
         p19  = "19" >> return R19
         p20  = "20" >> return R20
         p21  = "21" >> return R21
         p22  = "22" >> return R22
         p23  = "23" >> return R23
         p24  = "24" >> return R24
         p25  = "25" >> return R25
         p26  = "26" >> return R26
         p27  = "27" >> return R27
         p28  = "28" >> return R28
         p29  = "29" >> return R29
         p30  = "30" >> return R30
         p31  = "31" >> return R31
     in A.choice [ pSP
                 , p10
                 , p11
                 , p12
                 , p13
                 , p14
                 , p15
                 , p16
                 , p17
                 , p18
                 , p19
                 , p20
                 , p21
                 , p22
                 , p23
                 , p24
                 , p25
                 , p26
                 , p27
                 , p28
                 , p29
                 , p30
                 , p31
                 , p1
                 , p2
                 , p3
                 , p4
                 , p5
                 , p6
                 , p7
                 , p8
                 , p9
                 ]

imm :: Int -> A.Parser Word8
imm w = A.signed A.decimal >>= val
    where val :: Int -> A.Parser Word8
          val i = case compare 0 i of EQ -> return (fromIntegral 0)
                                      LT -> if checkWidth w i then return (fromIntegral i)
                                                               else fail "positive immediate out of range"
                                      GT -> let minb = (-(2^(w-1)))
                                            in (if i < minb then fail "negative immediate out of range"
                                                else return $ (0xFF `shiftL` w) .|. (fromIntegral i))

lab :: A.Parser B.ByteString
lab = A.takeWhile1 (\c -> (c /= ':') && (c /= '\n') && (c /= ' ') && (c /= '\t') && (c /= ';')) >>= val
    where val l = if validLabel l then return l
                                  else fail "bad label, must not contain space, tab, comma, or semicolon."
