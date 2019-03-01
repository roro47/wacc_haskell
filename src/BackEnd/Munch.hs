module BackEnd.Munch where

import Data.Maybe
import Data.List
import Control.Monad.State.Lazy
import qualified Data.Set as Set

import FrontEnd.Parser
import FrontEnd.SemanticAnalyzer
import FrontEnd.AST
import BackEnd.Instructions as ARM
import BackEnd.IR as IR
import BackEnd.Assem as ASSEM
import BackEnd.Temp hiding (newTemp, newDataLabel)
import BackEnd.Translate as Translate
import BackEnd.Frame as Frame
import BackEnd.Builtin
import BackEnd.Canon as C hiding (newTemp)


bopToCBS :: BOp ->  Maybe (Suffix -> Cond -> Calc)
bopToCBS bop
  = lookup bop [(IR.PLUS, ARM.ADD), (IR.AND, ARM.AND), (IR.OR, ARM.ORR),
            (IR.LSHIFT, ARM.LSL), (IR.RSHIFT, ARM.LSR), (IR.MINUS, ARM.SUB)]

justret e = do
  (i, t) <- munchExp e
  return (i , t)

munchExp :: Exp -> State TranslateState ([ASSEM.Instr], Temp)
munchExp (CALL (NAME "#retVal") [e]) = justret e

munchExp (CALL (NAME "#memaccess") [CONSTI i]) = do
  t <- newTemp
  return ([IOPER {assem = CBS_ (ADD NoSuffix AL) (RTEMP t) SP (IMM i),
                 src = [13], dst = [t], jump = []}], t)

munchExp (CALL (NAME "malloc") [CONSTI i, TEMP t]) = do
  let ldr = IOPER { assem = S_ (LDR W AL) (R0) (NUM (i)),
                  src = [], dst = [0], jump = []}
      move = move_to_r 0 t
  return ([ldr, ljump_to_label "malloc", move], t) --malloc notice dummy here

munchExp (CALL (NAME "#arrayelem") ((CONSTI size) : ident : pos)) = do
  (ii, it) <- munchExp ident
  result <- singleIndex size it pos
  return (ii ++ result, it)
    where
      singleIndex :: Int -> Temp -> [Exp] -> State TranslateState [ASSEM.Instr]
      singleIndex _ _ [] = return []
      singleIndex size t (p:ps) = do
        (pi, pt) <- munchExp p
        let op = if size == 1 then (R (RTEMP pt)) else (LSL_ (RTEMP pt) 2)
            ldr = IOPER { assem = S_ (LDR W AL) (RTEMP t) (Imm (RTEMP t) 0),
                        src = [t], dst = [t], jump = []}
            m0 = move_to_r pt 0
            m1 = move_to_r t 1
            bl = IOPER {assem = BRANCH_ (BL AL) (L_ "p_check_array_bounds"),
                        src = [0, 1], dst = [], jump = ["p_check_array_bounds"]}
            skiplen = IOPER {assem = CBS_ (ADD NoSuffix AL) (RTEMP t) (RTEMP t) (IMM 4),
                             src = [t], dst = [t], jump = []}
            topos = IOPER {assem = CBS_ (ADD NoSuffix AL) (RTEMP t) (RTEMP t) op,
                             src = [t, pt], dst = [t], jump = []}
        rest <- singleIndex size t ps
        return (pi ++ [ldr, m0, m1, bl, skiplen, topos] ++ rest)

munchExp (CALL (NAME "#neg") [(CONSTI i)]) = do
  t <- newTemp
  let ldr = IOPER { assem = S_ (LDR W AL) (RTEMP t) (NUM (-i)),
                   src = [], dst = [t], jump = []}
  return ([ldr], t)

munchExp (CALL (NAME "#neg") [e]) = do
  (i, t) <- munchExp e
  let rsbs = IOPER { assem = CBS_ (RSB S AL) (RTEMP t) (RTEMP t) (IMM 0),
                     src = [t], dst = [t], jump = []}
      check = IOPER {assem = BRANCH_ (BL VS) (L_ "p_throw_overflow_error"),
                     src = [], dst = [], jump =["p_throw_overflow_error"] }
  return (i ++ [rsbs, check] , t)

munchExp (CALL (NAME "#!") [e]) = do
  (i, t) <- munchExp e
  return (i ++ [IOPER {assem = CBS_ (EOR NoSuffix AL) (RTEMP t) (RTEMP t)(IMM 1),
                       src = [t], dst = [t], jump = []}], t)

munchExp (CALL (NAME "#len") [e]) = do
  (i, t) <- munchExp e
  return (i ++ [IOPER {assem = S_ (LDR W AL) (RTEMP t) (Imm (RTEMP t) 0),
                       src = [t], dst = [t], jump = []}], t)

munchExp (CALL (NAME "#skip") _) = return ([], dummy)

munchExp (CALL (NAME "#p_print_ln") es) = do
  ls <- mapM (liftM fst.munchExp) es
  let ln = IOPER {assem = BRANCH_ (BL AL) (L_ "p_print_ln"),
                  src = [], dst = [], jump = ["p_print_ln"]}
  return ((concat ls)++[ln], dummy)

munchExp (CALL (NAME "#p_read_int") [MEM e _]) = do
  (i, t) <- munchExp e
  let mv = move_to_r t 0
      putchar = ljump_to_label "p_read_int"
  return (i ++ [mv, putchar], dummy)

munchExp (CALL (NAME "#p_read_char") [MEM e _]) = do
  (i, t) <- munchExp e
  let mv = move_to_r t 0
      putchar = ljump_to_label "p_read_char"
  return (i ++ [mv, putchar], dummy)

munchExp (CALL (NAME "#p_putchar") [e]) = do
  (i, t) <- munchExp e
  let mv = move_to_r t 0
      putchar = ljump_to_label "putchar"
  return (i ++ [mv, putchar], dummy)

munchExp (CALL (NAME "#p_free_pair") [MEM e _]) = do
  (i, t) <- munchExp e
  let ldr = IOPER { assem = S_ (LDR W AL) (RTEMP t) (Imm (RTEMP t) 0),
                    src = [], dst = [t], jump = []}
      mv = move_to_r t 0
      freePair = ljump_to_label "p_free_pair"
  return (i ++ [ldr, mv, freePair], dummy)

munchExp (CALL (NAME "exit") [e]) = do
  let exit = ljump_to_label "exit"
  case e of
    CONSTI n ->
      return ([ IOPER { assem = MC_ (ARM.MOV AL) R0 (IMM n),
                      src = [],
                      dst = [0],
                      jump = [] },
                exit ], dummy)
    TEMP t ->
      return ([ IMOV { assem = MC_ (ARM.MOV AL) R0 (R (RTEMP t)),
                       src = [t],
                       dst = [0] },
                exit ], dummy)
    otherwise -> do
      mv <- munchStm (IR.MOV (TEMP 0) e)
      return (mv ++ [exit], dummy)

munchExp (CALL (NAME n) [e])
  | "#p_" `isPrefixOf` n = do
    (i, t) <- munchExp e
    return  (i++ [(move_to_r t 0), (ljump_to_label (drop 1 n))], dummy)

munchExp (CALL (NAME n) e)
  | n == "#fst" = accessPair True e
  | n == "#snd" = accessPair False e
  | n == "#newpair" = createPair e

{- r0 / r1 : result in r0 -}
munchExp (BINEXP DIV e1 e2) = do
  (i1, t1) <- munchExp e1 -- dividend
  (i2, t2) <- munchExp e2 --divisor
  let divLabel = "__aeabi_idiv"
      moveDividend = move_to_r t1 0
      moveDivisor = move_to_r t2 1
      check = IOPER {assem = BRANCH_ (BL AL) (L_ "p_check_divide_by_zero"),
                    src = [0, 1], dst = [], jump = ["p_check_divide_by_zero"]}
      divInstr = IOPER {assem = BRANCH_ (BL AL) (L_ divLabel),
                      src = [0, 1], dst = [0], jump = [divLabel]} in
      return $ (i1 ++ i2 ++ [moveDividend, moveDivisor, check, divInstr], 0)

{- r0 % r1 : result in r1 -}
munchExp (BINEXP MOD e1 e2) = do
  (i1, t1) <- munchExp e1 -- dividend
  (i2, t2) <- munchExp e2  --divisor
  let modLabel = "__aeabi_idivmod"
      moveDividend = move_to_r t1 0
      moveDivisor = move_to_r t2 1
      check = IOPER {assem = BRANCH_ (BL AL) (L_ "p_check_divide_by_zero"),
                    src = [0, 1], dst = [], jump = ["p_check_divide_by_zero"]}
      modInstr = IOPER {assem = BRANCH_ (BL AL) (L_ modLabel),
                  src = [0, 1], dst = [1], jump = [modLabel]} in
      return $ (i1 ++ i2 ++ [moveDividend, moveDivisor, check ,modInstr], 1)

{-If munched stm is of length 2 here then it must be a SEQ conaing a naive stm and a label -}
munchExp (ESEQ (SEQ cjump@(CJUMP rop _ _ _ _) (SEQ false true)) e) = do
  cinstr <- munchStm cjump
  state <- get
  falseinstr <- munchStm false
  if (length falseinstr == 2) then
    do
    put state
    let (fCommand, fBranch) = deSeq false
    condf <- condStm fCommand
    branchf <- munchStm fBranch
    state2 <- get
    trueinstr <- munchStm true
    if(length trueinstr == 2) then
      do
      put state2
      condt <- condStm (snd $ deSeq true) -- branch info not needed
      (i, t) <- munchExp e
      return ((condf (invert rop)) ++ (condt $ same rop) ++ i, t)
    else
      do
      (i, t) <- munchExp e
      return ((condf (invert rop)) ++ branchf ++ trueinstr ++ i, t)
  else
    do
    trueinstr <- munchStm true
    (i, t) <- munchExp e
    return (cinstr ++ falseinstr ++ trueinstr ++ i, t)

munchExp (ESEQ stm e) = do
  ls <- munchStm stm
  (i, t) <- munchExp e
  return (ls++i, t)

-- passing input in reverse sequence
munchExp (CALL (NAME f) es) = do
  pushParams <- (mapM pushParam es)
  return (concat (reverse pushParams) ++ [bToFunc] ++ adjustSP, 0)
  where pushParam exp = do
          (i, t) <- munchExp exp
          return $ i ++ [IOPER {assem = S_ (STR W AL) (RTEMP t) (PRE SP (-4)),
                                src = [t, 13], dst = [13], jump = []}]
        adjustSP = if totalParamSize == 0 then [] else
          [IOPER { assem = CBS_ (ADD NoSuffix AL) SP SP (IMM totalParamSize),
                  src = [Frame.sp],
                  dst = [Frame.sp],
                  jump = [] }]
        bToFunc =
          IOPER { assem = BRANCH_ (BL AL) (L_ fname),
                  src = [],
                  dst = [],
                  jump = [fname] }
        totalParamSize = (length es) * 4
        fname = "f_" ++ f


munchExp (CALL f es) = do
  (fi, ft) <- munchExp f -- assume result returned in ft
  ls <- mapM (liftM fst.munchExp) es
  let returnVal = move_to_r ft 0
  return ((concat ls) ++ fi ++ [returnVal], 0) -- returned in reg 0


munchExp (TEMP t) = return ([],t)

munchExp (BINEXP MUL e1 e2) = do -- only the lower register is returned
  (i1, t1) <- munchExp e1
  (i2, t2) <- munchExp e2
  tLo <- newTemp
  tHi <- newTemp
  let smull = IOPER {assem = C2_ (SMULL NoSuffix AL) (RTEMP tLo)
                    (RTEMP tHi) (RTEMP t1) (RTEMP t2),
                    src = [t1, t2], dst = [tLo, tHi], jump = []}
      cmp = IOPER {assem = MC_ (CMP AL) (RTEMP tHi) (ASR_ (RTEMP tLo) 31),
                   src = [tHi, tLo], dst = [], jump = []}
      throw = IOPER {assem = BRANCH_ (BL ARM.NE) (L_ "p_throw_overflow_error"),
                   src = [], dst = [], jump = ["p_throw_overflow_error"]}
  return $ (i1 ++ i2 ++ [smull, cmp, throw], tLo)

munchExp x = do
  c <- condExp x
  return $ c AL

lslOP :: Exp -> Exp -> BOp -> Int -> State TranslateState (Cond -> ([ASSEM.Instr], Temp))
lslOP e1 e2 bop int = do
  (i1, t1) <- munchExp e1
  (i2, t2) <- munchExp e2
  return $ \c -> (i1 ++ i2 ++ [IOPER {assem = CBS_ (addsubtoCalc bop $ c) (RTEMP t1) (RTEMP t1)
                  (LSL_ (RTEMP t2) (log2 int)), dst = [t1], src = [t1,t2], jump = []}] , t1)

canlsl bop int = (bop == MINUS || bop == PLUS) && (int == 2 || int == 4 || int == 8)

plusMinus destination source op srcreg srcinstr = do
  (i1, t1) <- munchExp destination
  let calc = IOPER {assem = CBS_ (op S AL) (RTEMP t1) (RTEMP t1) source,
                    src = ([t1] ++ srcreg), dst = [t1], jump = []}
      br = IOPER {assem = BRANCH_ (BL VS) (L_ "p_throw_overflow_error"),
                    src = [], dst = [], jump = ["p_throw_overflow_error"]}
  return $ \c -> (i1++srcinstr++[calc, br], t1)

condExp :: Exp -> State TranslateState (Cond -> ([ASSEM.Instr], Temp))
-- LSL inside ADD SUB  ** ugly pattern match to avoid run time loop --
condExp (BINEXP bop (BINEXP MUL e1 (CONSTI int)) e2)
  | canlsl bop int
      = lslOP e1 e2 bop int

condExp (BINEXP bop e1 (BINEXP MUL (CONSTI int) e2))
  | canlsl bop int
      = lslOP e1 e2 bop int

condExp (BINEXP bop e1 (BINEXP MUL e2 (CONSTI int)))
  | canlsl bop int
      = lslOP e1 e2 bop int

condExp (BINEXP bop (BINEXP MUL (CONSTI int) e1) e2)
  | canlsl bop int
      = lslOP e1 e2 bop int

condExp (BINEXP bop e1@(CONSTI int1) e2@(CONSTI int2))
  | bop /= PLUS && bop /= MINUS = do
    (i1, t1) <- munchExp e1
    let cbs = bopToCBS bop
        calc = \c -> IOPER {assem = CBS_ ((fromJust cbs) NoSuffix c) (RTEMP t1)
                      (RTEMP t1) (IMM int2), dst = [t1], src = [t1], jump = []}
    case cbs of
      Nothing -> fail ""
      otherwise -> return $ \c -> (i1 ++ [calc c], t1)

condExp (BINEXP PLUS e1 e2) = do
  (i2, t2) <- munchExp e2
  plusMinus e1 (R (RTEMP t2)) ADD [t2] i2

condExp (BINEXP MINUS e1 e2) = do
  (i2, t2) <- munchExp e2
  plusMinus e1 (R (RTEMP t2)) SUB [t2] i2

condExp (BINEXP bop (CONSTI int) e) = condExp (BINEXP bop e (CONSTI int))

condExp (BINEXP bop e (CONSTI int)) = do
  (i1, t1) <- munchExp e
  let cbs = bopToCBS bop
      calc = \c -> IOPER {assem = CBS_ ((fromJust cbs) NoSuffix c) (RTEMP t1)
                    (RTEMP t1) (IMM int), dst = [t1], src = [t1], jump = []}
  case cbs of
    Nothing -> fail ""
    otherwise -> return $ \c -> (i1 ++ [calc c], t1)


condExp (BINEXP bop e1 e2) = do
  (i1, t1) <- munchExp e1
  (i2, t2) <- munchExp e2
  let cbs = bopToCBS bop
      calc = \c -> IOPER {assem = CBS_ ((fromJust cbs) NoSuffix c) (RTEMP t1)
            (RTEMP t1) (R $ RTEMP t2), dst = [t1], src = [t1, t2], jump = []}
  case cbs of
    Nothing -> fail ""
    otherwise -> return $ \c -> (i1 ++ i2 ++ [calc c], t1)

condExp (CONSTI int) = do
  t <- newTemp
  return $ \c -> ([(wrapAssem c (\c -> (S_ (LDR W c) (RTEMP t) (NUM int))) [] [t])], t)

condExp (CONSTC chr) = do
  t <- newTemp
  return $ \c -> ([(wrapAssem c (\c -> (MC_ (ARM.MOV c) (RTEMP t) (CHR chr))) [] [t])], t)

condExp (NAME l) = do
  t <- newTemp
  return $ \c -> ([(wrapAssem c (\c -> (S_ (LDR W c) (RTEMP t) (MSG l))) [] [t])], t)

condExp (MEM (CONSTI i) _) = do
  newt <- newTemp
  return $ \c -> ([IOPER{assem = S_ (ARM.LDR W c) (RTEMP newt) (NUM i) , dst = [newt],
                         src = [], jump = []}], newt)

condExp (MEM m size) = do
  (i, t) <- munchExp m
  newt <- newTemp
  let suff = if size == 4 then W else SB
  return $ \c -> (i ++ [IOPER {assem = S_ (ARM.LDR suff c) (RTEMP newt) (Imm (RTEMP t) 0)
                        , dst = [newt], src = [t], jump = []}], newt)

--only AL is of type IMOV
wrapAssem :: Cond -> (Cond -> ARM.Instr) -> [Temp] -> [Temp] -> ASSEM.Instr
wrapAssem AL instr s@(s':_) d@(d':_) = IMOV {assem = instr AL, src = s, dst = d}
wrapAssem c instr s d = IOPER {assem = instr c, src = s, dst = d, jump = []}

addsubtoCalc :: BOp -> (Cond -> Calc)
addsubtoCalc PLUS = (\c -> ARM.ADD NoSuffix c)
addsubtoCalc MINUS = (\c -> ARM.SUB NoSuffix c)

log2 :: Int -> Int
log2 2 = 1
log2 4 = 2
log2 8 = 3

munchMem :: Exp -> State TranslateState ([ASSEM.Instr], [Int], SLOP2)
--- PRE-INDEX ---
-- HANDLED USING optimise

-- TODO: more simplification allowed here : eval the expression if possible to a int....
---- IMMEDIATE ----
munchMem (TEMP t) = return ([], [t], Imm (RTEMP t) 0)
munchMem (BINEXP PLUS (TEMP t) (CONSTI int)) = return ([], [t], Imm (RTEMP t) int)
munchMem (BINEXP PLUS (CONSTI int) (TEMP t)) = return ([], [t], Imm (RTEMP t) int)
munchMem (BINEXP MINUS (TEMP t) (CONSTI int)) = return ([], [t], Imm (RTEMP t) (-int))
munchMem (CONSTI int) = return ([], [], NUM int)

--- ALL OTHER CASES ---
{- Including msg -}
munchMem e = do
  (i, t) <- munchExp e
  return (i, [t], MSG "SLOP2 NOT USED")

--- CAUTION : NEED TO TEST THE IMM OFFSET RANGE OF THE TARGET MACHINE ---
optimise :: [ASSEM.Instr] -> [ASSEM.Instr]
-- catch overflow of sp
optimise ((IOPER { assem = (CBS_ (ADD s c) reg0 reg1 (IMM i))}):remain)
  | (i > 1024 || i < -1024) && reg0 == reg1
    =  ((IOPER { assem = (CBS_ (ADD s c) reg0 reg1 (IMM 1024)),
                src = [toNum reg0], dst = [toNum reg1], jump = []}) :
        (IOPER { assem = (CBS_ (ADD s c) reg0 reg1 (IMM (i - 1024))),
                    src = [toNum reg0], dst = [toNum reg1], jump = []}):(optimise remain))
-- load large number?
---IMMEDIATE ---
optimise ((IOPER { assem = (CBS_ c reg0 reg1 (IMM 0))}) :
          (IOPER { assem = (S_ sl reg3 (Imm reg2 0))}) :remain)
  | (stackEqualCond c sl) && reg0 == reg2
    = optimise (IOPER { assem = (S_ sl reg3 (Imm reg1 0)),
                        src = [13], dst = [toNum reg3], jump = [] }:remain)
optimise (IOPER {assem = CBS_ a@(ADD NoSuffix AL) reg0 reg1 (IMM 0)} : remain)
  |reg0 == reg1 = optimise remain
optimise (IOPER { assem = CBS_ a@(ADD NoSuffix AL) (RTEMP t1) SP (IMM i)} : -- remoge this after unified
          IOPER { assem = S_ op (RTEMP t3) (Imm (RTEMP t4) i')}:remain)
  | t4 == t1 = optimise ((IOPER { assem = S_ op (RTEMP t3) (Imm SP (i+i')),
                                  src = [13], dst = [t3], jump = []}): remain)
optimise (IOPER { assem = CBS_ a@(ADD NoSuffix AL) (RTEMP t1) SP (IMM i)} :
          IOPER { assem = S_ op (RTEMP t3) (Imm (RTEMP t4) i')}:remain)
  | t4 == t1 = optimise ((IOPER { assem = S_ op (RTEMP t3) (Imm SP (i+i')),
                                  src = [13], dst = [t3], jump = []}): remain)
-- PRE-INDEX --
optimise ((IOPER { assem = (CBS_ c (RTEMP t11) (RTEMP t12) (IMM int))}) :
          (IOPER { assem = (S_ sl (RTEMP t21) (Imm (RTEMP t22) 0))}) :remain)
  | (stackEqualCond c sl) && t11 == t12 && t22 == t11
        = optimise (IOPER { assem = (S_ sl (RTEMP t21) (PRE (RTEMP t11) (opVal c * int))),
                           src = [t11], dst = [t12], jump = []} : remain)
optimise ((IOPER { assem = (S_ sl (RTEMP t21) (Imm (RTEMP t22) 0))}):
          (IOPER { assem = (CBS_ c (RTEMP t11) (RTEMP t12) (IMM int))}) :remain)
  | (stackEqualCond c sl) && t11 == t12 && t22 == t11
        = optimise (IOPER { assem = (S_ sl (RTEMP t21) (PRE (RTEMP t11) (opVal c * int))),
                            src = [t11], dst = [t12], jump = [] } : remain)
optimise ((IMOV { assem = MC_ (ARM.MOV _) a (R b)}):remain)
  | a == b = optimise remain

optimise ((IOPER { assem = (BRANCH_ (B AL) (L_ a))}) : l@(ILABEL {assem = (LAB b)}) : remain)
  | a == b = optimise (l:remain)
optimise (x:xs) = x : (optimise xs)
optimise [] = []

stackEqualCond :: Calc -> SL -> Bool
stackEqualCond (ARM.ADD _ c1) (LDR _ c2) = c1 == c2
stackEqualCond (ARM.ADD _ c1) (STR _ c2) = c1 == c2
stackEqualCond (ARM.SUB _ c1) (LDR _ c2) = c1 == c2
stackEqualCond (ARM.SUB _ c1) (STR _ c2) = c1 == c2
stackEqualCond _ _ = False

opVal :: Calc -> Int
opVal (ARM.ADD _ _) = 1
opVal _ = -1

munchStm :: Stm -> State TranslateState [ASSEM.Instr] -- everything with out condition
munchStm (EXP call@(CALL _ _)) = do
  (intrs, reg) <- munchExp call
  return intrs

munchStm (LABEL label) = return [ILABEL {assem = LAB label, lab = label}]

-- -- moving stack pointer don't need to check overflow
munchStm (IR.MOV (TEMP 13) (BINEXP bop (TEMP 13) (CONSTI offset)))
  | bop == MINUS = return $ ret SUB
  | bop == PLUS = return $ ret ADD
    where
      ret = \op -> [IOPER { assem = CBS_ (op NoSuffix AL) SP SP (IMM offset),
                    src = [Frame.sp],
                    dst = [Frame.sp],
                    jump = [] } ]

munchStm (SEQ s1 s2) = do
  l1 <- munchStm s1
  l2 <- munchStm s2
  return $ l1 ++ l2

munchStm (CJUMP rop e1 (CONSTI i) t f) = do -- ASSUME CANONICAL
  (i1, t1) <- munchExp e1
  let compare = IOPER {assem = MC_ (ARM.CMP AL) (RTEMP t1) (IMM i), dst = [],
                       src = [t1], jump = []}
      jtrue = IOPER {assem = BRANCH_ (ARM.B (same rop)) (L_ t), dst = [], src = [], jump = [t]}
  return $ i1 ++ [compare, jtrue] -- NO JFALSE AS FALSE BRANCH FOLLOWS THIS DIRECTLY

munchStm (CJUMP rop e1 e2 t f) = do -- ASSUME CANONICAL
  (i1, t1) <- munchExp e1
  (i2, t2) <- munchExp e2
  let compare = IOPER {assem = MC_ (ARM.CMP AL) (RTEMP t1) (R (RTEMP t2)), dst = [t1],
                       src = [t2], jump = []}
      jtrue = IOPER {assem = BRANCH_ (ARM.B (same rop)) (L_ t), dst = [], src = [], jump = [t]}
  return $ i1 ++ i2 ++ [compare, jtrue] -- NO JFALSE AS FALSE BRANCH FOLLOWS THIS DIRECTLY

munchStm (EXP e) = do
  (i, t) <- munchExp e
  return i

munchStm x = do
  m <- condStm x
  return $ m AL

-- ALLOW the suffix + cond of a load / store to change
suffixStm :: Stm -> State TranslateState (Cond -> [ASSEM.Instr])
suffixStm (IR.MOV (MEM me t') e) = do -- STR
  (i, t) <- munchExp e
  (l, ts, op) <- munchMem me
  let suff = if t' == 4 then W else B_
  if null l then
    return (\c -> (i ++ [IOPER { assem = S_ (ARM.STR suff c) (RTEMP t) op,
                                          src = [t]++ts, dst = [], jump = []}]))
  else
    let s = head ts in
    return (\c -> ( i ++ l ++ [IOPER { assem = S_ (ARM.STR suff c) (RTEMP t) (Imm (RTEMP s) 0),
                                              src = [s, t], dst = [], jump = []}]))

suffixStm (IR.MOV e (MEM me t')) = do -- LDR
  (i, t) <- munchExp e
  (l, ts, op) <- munchMem me
  let suff = if t' == 4 then W else SB
  if null l then
    return (\c -> ( i ++ [IOPER { assem = S_ (ARM.LDR suff c) (RTEMP t) op,
                                          src = ts, dst = [t], jump = []}]))
  else
    let s = head ts in
    return (\c -> ( i ++ l ++ [IOPER { assem = S_ (ARM.LDR suff c) (RTEMP t) (Imm (RTEMP s) 0),
                                       src = [s], dst = [t], jump = []}]))

condStm :: Stm -> State TranslateState (Cond -> [ASSEM.Instr])  --allow for conditions to change

condStm ir@(IR.MOV e (MEM me _)) = do
  ret <- suffixStm ir
  return (\c -> ret c)

condStm ir@(IR.MOV (MEM me _) e) = do
  ret <- suffixStm ir
  return (\c -> ret c)

condStm (IR.MOV e (CONSTI int)) = do
  (i, t) <- munchExp e
  return (\c -> i ++ [IOPER { assem = MC_ (ARM.MOV c) (RTEMP t) (IMM int), src = [], dst = [t], jump = []}])

condStm (IR.MOV e1 e2) = do  --In which sequence ?
  (i1, t1) <- munchExp e1
  (i2, t2) <- munchExp e2
  return (\c -> i1 ++ i2 ++ [move_to_r t2 t1])

condStm (IR.PUSHREGS regs) = do
  let regs' = map RTEMP regs
  return (\c -> [IOPER { assem = STACK_ (ARM.PUSH c) regs',
                         dst = [],
                         src = [],
                         jump = [] }])

condStm (IR.POPREGS regs) = do
  let regs' = map RTEMP regs
  return (\c -> [IOPER { assem = STACK_ (ARM.POP c) regs',
                         dst = [],
                         src = [],
                         jump = [] }])



condStm (IR.PUSH e) = do
  (i, t) <- munchExp e
  return (\c -> i ++ [IOPER {assem = STACK_ (ARM.PUSH c) [RTEMP t], dst = [sp],
                             src = [t], jump = []}]) --sp here or not ??

condStm (IR.POP e) = do
  (i, t) <- munchExp e
  return (\c -> i ++ [IOPER {assem = STACK_ (ARM.POP c) [RTEMP t], dst = [t, sp],
                             src = [sp], jump = []}])

condStm (JUMP e ls) = do
  case e of
    (CONSTI 1) ->
      return (\c -> [IOPER { assem = BRANCH_ (ARM.B AL) (L_ (head ls)),
                            dst = [],
                            src = [],
                            jump = [head ls] }])
    (CONSTI 0) -> return (\c -> [])
    otherwise -> do
      (i, t) <- munchExp e
      return (\c -> [IOPER { assem = MC_ (CMP c) (RTEMP t) (IMM 1),
                             dst = [],
                             src = [],
                             jump = [] },
                     IOPER { assem = BRANCH_ (ARM.B c) (L_ (head ls)),
                             dst = [],
                             src = [],
                             jump = [head ls] }])

condStm (NOP) = return $ \c -> []

condStm t = fail $ show t

munchBuiltInFuncFrag :: Fragment -> State TranslateState [ASSEM.Instr]
munchBuiltInFuncFrag (PROC stm frame) = do
  munch <- munchStm stm
  return (pushlr : munch ++ [poppc])

munchDataFrag :: Fragment -> [ASSEM.Instr]
munchDataFrag (STRING label str l)
  = [ILABEL {assem = (M label l str), lab = label}]
--subrtacting the space occupied by ""

createPair :: [Exp] -> State TranslateState ([ASSEM.Instr], Temp)
-- pre : exps is [length fst, length snd, fst, snd]
createPair [(CONSTI fsize), (CONSTI ssize), f, s] = do
  (i1, t1) <- munchExp f
  (i2, t2) <- munchExp s
  tadddr <- newTemp -- pair addr
  let
      suffix = \size -> if size == 4 then W else B_
      ld8 = IOPER { assem = (S_ (LDR W AL) R0 (NUM 8)), src = [], dst = [0], jump = []}
      ldsize = \size -> IOPER { assem = (S_ (LDR W AL) R0 (NUM size)), src = [], dst = [0], jump = []}
      malloc = IOPER { assem = BRANCH_ (BL AL) (L_ "malloc"), src = [0], dst = [0], jump = ["malloc"]}
      strPairAddr = IMOV { assem = MC_ (ARM.MOV AL) (RTEMP tadddr) (R R0), src = [0], dst = [tadddr]}
      savefst = IOPER { assem = (S_ (STR (suffix fsize) AL) (RTEMP t1) (Imm R0 0)),
                        src = [t1, 0], dst = [], jump = []}
      savesnd = IOPER { assem = (S_ (STR (suffix ssize) AL) (RTEMP t2) (Imm R0 0)),
                        src = [t2, 0], dst = [], jump = []}
      strfstaddr = IOPER { assem = (S_ (STR W AL) R0 (Imm (RTEMP tadddr) 0)),
                           src = [tadddr, 0], dst = [], jump = []}
      strsndaddr = IOPER { assem = (S_ (STR W AL) R0 (Imm (RTEMP tadddr) 4)),
                           src = [tadddr, 0], dst = [], jump = []}
      strpaironstack= IOPER { assem = (S_ (STR W AL) (RTEMP tadddr) (Imm (RTEMP 13) 0)),
                              src = [tadddr, 13], dst = [], jump = []}
  return ([ld8, malloc, strPairAddr] ++ i1 ++ [(ldsize fsize), malloc, savefst, strfstaddr]
           ++ i2 ++ [(ldsize ssize), malloc, savesnd, strsndaddr, strpaironstack], dummy)

accessPair :: Bool -> [Exp] -> State TranslateState ([ASSEM.Instr], Temp)
accessPair isfst [MEM e ty] = do
  (i, t) <- munchExp (MEM e ty)
  let offset = if isfst then 0 else 4
      getpaddr = move_to_r t 0
      check = IOPER { assem = BRANCH_ (BL AL) (L_ "p_check_null_pointer")
                      , src = [0], dst = [], jump = ["p_check_null_pointer"]}
      s1 = IOPER {assem = (S_ (LDR (if ty == 1 then SB else W) AL) (RTEMP t)
                           (Imm (RTEMP t) offset)), src = [t], dst = [t], jump = []}
  return (i ++ [getpaddr, check, s1], t)  -- cannot handle sb/w here as only return reg


-------------------- Utilities ---------------------
condIR = [IR.EQ, IR.LT, IR.LE, IR.GT, IR.GE, IR.NE]
condARM = [ARM.EQ, ARM.LT, ARM.LE, ARM.GT, ARM.GE, ARM.NE]

invert :: ROp -> Cond
invert a = fromJust $ lookup a (zip condIR (reverse condARM))

same :: ROp -> Cond
same a = fromJust $ lookup a (zip condIR condARM)

deSeq :: Stm -> (Stm, Stm)
deSeq (SEQ s1 s2) = (s1, s2)

optimizeInstrs :: [ASSEM.Instr] -> [ASSEM.Instr]
optimizeInstrs instrs = filter (\x -> not $ containsDummy x) instrs'
  where instrs' = optimise (normAssem [(13, SP), (14, LR), (15, PC), (1, R1), (0, R0)] instrs)


munchmany [] = return []
munchmany (x:xs) = do
  m <- munchStm x
  ms <- munchmany xs
  return $ (m++ms)

type GenBuiltIn = State TranslateState [ASSEM.Instr]

genBuiltIns = [p_print_ln,
               p_print_int,
               p_print_bool,
               p_print_string,
               p_print_reference,
               p_check_null_pointer,
               p_throw_runtime_error,
               p_read_int,
               p_read_char,
               p_free_pair,
               p_check_array_bounds,
               p_throw_overflow_error,
               p_check_divide_by_zero]


p_print_ln :: GenBuiltIn
p_print_ln = do
  msg <- newDataLabel
  addFragment (Frame.STRING msg ("\"\\0\"") 1)
  return $[add_label "p_print_ln",
           pushlr,
           ld_msg_toR0 msg,
           r0_add4,
           ljump_to_label "puts",
           r0_clear,
           ljump_to_label "fflush",
           poppc]

p_print_int :: GenBuiltIn
{-In ref compiler this temp is R1 -}
p_print_int = do
 msg <- newDataLabel
 addFragment (Frame.STRING msg "\"%d\\0\"" 3)
 return $[add_label "p_print_int",
          pushlr,
          mv_r0_r1,
          IOPER { assem = S_ (LDR W AL) R0 (MSG msg), src = [],
                  dst = [0], jump = []}]
          ++ end

p_print_bool :: GenBuiltIn
p_print_bool = do
  truemsg <- newDataLabel
  falsemsg <- newDataLabel
  addFragment (Frame.STRING truemsg "\"true\\0\"" 5)
  addFragment (Frame.STRING falsemsg "\"false\\0\"" 6)
  return $[add_label "p_print_bool",
          pushlr,
          cmp_r0,
          ld_cond_msg_toR0 truemsg ARM.NE,
          ld_cond_msg_toR0 falsemsg ARM.EQ]
          ++ end


p_print_string :: GenBuiltIn
p_print_string = do
  msg <- newDataLabel
  addFragment (Frame.STRING msg "\"%.*s\\0\"" 5)
  return $[add_label "p_print_string",
          pushlr,
          IOPER {assem = S_ (LDR W AL) R1 (Imm R0 0), src = [0], dst = [1],
                 jump = []},
          IOPER { assem = CBS_ (ADD NoSuffix AL) R2 R0 (IMM 4), src = [0],
                  dst = [2], jump = []},
          ld_msg_toR0 msg] ++ end

p_print_reference :: GenBuiltIn
p_print_reference = do
  msg <- newDataLabel
  addFragment (Frame.STRING msg "\"%p\\0\"" 3)
  return $[add_label "p_print_reference",
          pushlr,
          mv_r0_r1,
          ld_msg_toR0 msg] ++ end

p_check_null_pointer :: GenBuiltIn
p_check_null_pointer = do
  msg <- newDataLabel
  let m = "NullReferenceError: dereference a null reference\\n"
  addFragment (Frame.STRING msg ("\"" ++ m ++"\\0" ++ "\"") (length m))
  let s = "p_throw_runtime_error"
  return $[add_label "p_check_null_pointer",
          pushlr,
          cmp_r0,
          ld_cond_msg_toR0 msg ARM.EQ,
          ljump_cond s ARM.EQ,
          poppc]

p_throw_runtime_error :: GenBuiltIn
p_throw_runtime_error = do
  let s = "p_print_string"
  return [add_label "p_throw_runtime_error",
          jump_to_label s,
          IMOV {assem = MC_ (ARM.MOV AL) R0 (IMM (-1)), src = [], dst = [0]},
          jump_to_label "exit"]

p_read_int :: GenBuiltIn
p_read_int = do
  r <- p_read "\"%d\\0\"" 3
  return $ (add_label "p_read_int"): r

p_read_char :: GenBuiltIn
p_read_char = do
  r <- p_read "\" %c\\0\"" 4
  return $ (add_label "p_read_char"): r

p_read :: String -> Int -> GenBuiltIn
p_read str l =  do
  msg <- newDataLabel
  addFragment (Frame.STRING msg str l)
  return [pushlr,
          mv_r0_r1,
          ld_msg_toR0 msg,
          r0_add4,
          ljump_to_label "scanf",
          poppc]

p_free_pair :: GenBuiltIn
p_free_pair = do
  msg <- newDataLabel
  let m = "NullReferenceError: dereference a null reference"
  let str = "\"" ++ m ++ "\\0\""
      runTimeError = "p_throw_runtime_error"
  addFragment (Frame.STRING msg str (length m + 1))
  return [add_label "p_free_pair",
          pushlr,
          cmp_r0,
          ld_cond_msg_toR0 msg ARM.EQ,
          ljump_cond runTimeError ARM.EQ,
          IOPER { assem = STACK_ (ARM.PUSH AL) [R0], src = [0], dst = [13], jump = []},
          IOPER {assem = S_ (LDR W AL) R0 (Imm R0 0), src = [0], dst = [0], jump = []},
          ljump_to_label "free",
          IOPER {assem = S_ (LDR W AL) R0 (Imm SP 0), src = [13], dst = [0], jump = []},
          IOPER {assem = S_ (LDR W AL) R0 (Imm R0 4), src = [0], dst = [0], jump = []},
          ljump_to_label "free",
          IOPER { assem = STACK_ (ARM.POP AL) [R0], src = [13], dst = [13, 0], jump = []},
          ljump_to_label "free",
          poppc]


p_check_array_bounds :: GenBuiltIn
p_check_array_bounds = do
  let m1 = "ArrayIndexOutOfBoundsError: negative index"
      m2 = "ArrayIndexOutOfBoundsError: index too large"
  msgneg <- newDataLabel
  msgover <- newDataLabel
  addFragment (Frame.STRING msgneg ("\"" ++ m1 ++ "\\0\"") (length m1 + 1))
  addFragment (Frame.STRING msgover ("\"" ++ m2 ++ "\\0\"") (length m2 + 1))
  return [add_label "p_check_array_bounds",
          pushlr,
          cmp_r0,
          ld_cond_msg_toR0 msgneg ARM.LT,
          ljump_cond "p_throw_runtime_error" ARM.LT,
          IOPER {assem = S_ (LDR W AL) R1 (Imm R1 0), src = [1],
                 dst = [1], jump = []},
          IOPER {assem = MC_ (CMP AL) R0 (R R1), src = [0, 1], dst = [], jump = []},
          ld_cond_msg_toR0 msgover ARM.CS,
          ljump_cond "p_throw_runtime_error" ARM.CS,
          poppc]

p_throw_overflow_error :: GenBuiltIn
p_throw_overflow_error = do
  msg <- newDataLabel
  let m = "OverflowError: the result is too small/large to store in a 4-byte signed-integer."
  addFragment (Frame.STRING msg  ("\"" ++ m ++ "\"") (length m))
  return [add_label "p_throw_overflow_error",
          ld_msg_toR0 msg, ljump_to_label "p_throw_runtime_error"]

p_check_divide_by_zero :: GenBuiltIn
p_check_divide_by_zero = do
  msg <- newDataLabel
  let m = "DivideByZeroError: divide or modulo by zero"
  addFragment (Frame.STRING msg ("\"" ++ m ++ "\\0\"") (length m + 1))
  return [add_label "p_check_divide_by_zero",
          pushlr,
          IOPER {assem = MC_ (CMP AL) R1 (IMM 0), src = [1], dst = [], jump = []},
          ld_cond_msg_toR0 msg ARM.EQ,
          ljump_cond "p_throw_runtime_error" ARM.EQ,
          poppc]
