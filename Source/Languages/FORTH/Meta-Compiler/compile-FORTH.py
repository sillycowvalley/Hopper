'''
----------------------------------------------------------------------

#   #                                   #####  ###  ####  ##### #   #
#   #  ###  ####  ####   ###  ####      #     #   # #   #   #   #   #
##### #   # #   # #   # ##### #   # ### ###   #   # ####    #   #####
#   # #   # #   # #   # #     #         #     #   # #  #    #   #   #
#   #  ###  ####  ####   #### #         #      ###  #   #   #   #   #
            #     #
            #     #

M E T A - C O M P I L E R

MIT License

Copyright (c) [2024] [Nils "slowcorners" Kullberg]

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
'''

import sys, os

# ----------------------------------------------------------------------
# OPCODES (FORTH PRIMARIES)

OP_QRXT = 0;        OP_QRX = 1;         OP_TXSTO = 2;       OP_STIO = 3
OP_LIT = 4;         OP_CALL = 5;        OP_NEXT = 6;        OP_QBRAN = 7
OP_BRAN = 8;        OP_EXEC = 9;        OP_EXIT = 10;       OP_STORE = 11
OP_AT = 12;         OP_CSTOR = 13;      OP_CAT = 14;        OP_RPAT = 15
OP_RPSTO = 16;      OP_TOR = 17;        OP_FROMR = 18;      OP_RAT = 19
OP_SPAT = 20;       OP_SPSTO = 21;      OP_DROP = 22;       OP_DUP = 23
OP_SWAP = 24;       OP_OVER = 25;       OP_ZLESS = 26;      OP_AND = 27
OP_OR = 28;         OP_XOR = 29;        OP_UMPLU = 30;      OP_CMOVE = 31

OP_ONEP = 32;       OP_TWOP = 33;       OP_ONEM = 34;       OP_TWOM = 35
OP_ROT = 36;        OP_TDROP = 37;      OP_TDUP = 38;       OP_NIP = 39
OP_TUCK = 40;       OP_NOT = 41;        OP_NEG = 42;        OP_DNEG = 43
OP_DPLUS = 44;      OP_SUB = 45;        OP_PSTOR = 46;      OP_INCR = 47
OP_DECR = 48;       OP_QDUP = 49;       OP_ABS = 50;        OP_ATEXE = 51
OP_ZEQU = 52;       OP_EQU = 53;        OP_NEQ = 54;        OP_ULESS = 55
OP_LESS = 56;       OP_GREAT = 57;      OP_MAX = 58;        OP_MIN = 59
OP_WTHIN = 60;      OP_UMSMO = 61;      OP_MSMOD = 62;      OP_UMSTA = 63
OP_MSTAR = 64;      OP_LSHFT = 65;      OP_TWPWR = 66;      OP_RSHFT = 67
OP_PICON = 68;      OP_PIFUN = 69;      OP_DIR = 70;        OP_FTOR = 71
OP_FFRMR = 72

# ----------------------------------------------------------------------
# GLOBAL DEFINES

TRUE = 0xFFFF
FALSE = 0x0000

# Input and output files
FORTH_FTH = 'FORTH.FTH'
FORTH_PRECOMPILED_HS = 'Precompiled.hs'
FORTH_PRECOMPILED_LST = 'Precompiled.lst'
source_file = None

# Sizes of various areas
MEM_SIZE = 65536
B_BUF = 1024
UAREA_SIZE = 80
TIB_SIZE = 82
PAD_SIZE = 66
STACK_SIZE = 256
RSTACK_SIZE = 256
DSKBF_SIZE =  B_BUF + 10
DSKBF_NUM = 3
BOOT_SIZE = 40

# Memory Map
MMEND = MEM_SIZE - 1
ENDBF = MMEND
DSKBF = (ENDBF - (DSKBF_NUM * DSKBF_SIZE))
UA0 = DSKBF - UAREA_SIZE
TIB0 = UA0 - TIB_SIZE
RP0 = TIB0
SP0 = RP0 - RSTACK_SIZE

# USER VARIABLES
UVAR_STATE = UA0 + 0;   UVAR_BASE = UA0 + 2;    UVAR_CONT = UA0 + 4;    UVAR_CURR = UA0 + 6
UVAR_DP = UA0 + 8;      UVAR_OUT = UA0 + 10;    UVAR_HLD = UA0 + 12;    UVAR_NTIB = UA0 + 14
UVAR_SPAN = UA0 + 16;   UVAR_INPTR = UA0 + 18;  UVAR_CSP = UA0 + 20;    UVAR_HNDLR = UA0 + 22
UVAR_LATES = UA0 + 24;  UVAR_TMP = UA0 + 26;    UVAR_VQKEY = UA0 + 28;  UVAR_VEMIT = UA0 + 30
UVAR_VECHO = UA0 + 32;  UVAR_VEXPE = UA0 + 34;  UVAR_VTAP = UA0 + 36;   UVAR_VPRMP = UA0 + 38
UVAR_VEVAL = UA0 + 40;  UVAR_VNUMB = UA0 + 42;  UVAR_SEED = UA0 + 44;   UVAR_VUNIQ = UA0 + 46
UVAR_NP = UA0 + 48;     UVAR_VDISP = UA0 + 50;  UVAR_TASKS = UA0 + 52;  UVAR_ALOAD = UA0 + 54
UVAR_SCRN = UA0 + 56;   UVAR_POSN = UA0 + 58;   UVAR_CURN = UA0 + 60;   UVAR_LPS = UA0 + 62
UVAR_LINES = UA0 + 64;  UVAR_USE = UA0 + 66;    UVAR_PREV = UA0 + 68;   UVAR_VUNIQ = UA0 + 70
UVAR_FSYS = UA0 + 72;   UVAR_FILE = UA0 + 74;   UVAR_FILE2 = UA0 + 76;  UVAR_TICKS = UA0 + 78

# BOOT TABLE
BOOT_COLD = 0;          BOOT_WARM = 2;          BOOT_free5 = 4;         BOOT_free6 = 6
BOOT_CPU = 8;           BOOT_VER = 10;          BOOT_SP0 = 12;          BOOT_RP0 = 14
BOOT_UA0 = 16;          BOOT_TIB = 18;          BOOT_FIRST = 20;        BOOT_LIMIT = 22
BOOT_CR = 24;           BOOT_NL = 26;           BOOT_BSP = 28;          BOOT_DP0 = 30
BOOT_free1 = 32;        BOOT_free2 = 34;        BOOT_free3 = 36;        BOOT_free4 = 38


_CR  = 0x0D0A
_NL  = 0x000D
_BSP = 0x0008

# ----------------------------------------------------------------------
# GLOBAL VARIABLES

# ROM contents during compilation
ram = [255] * MEM_SIZE
# Bit masks for immediate and compile-only words
IMMEDIATE = 0x40
COMP_ONLY = 0x20
# FORTH machine registers
ip = 0
wa = 0
sp = 0
rp = 0
# Line width count when generating output
lw = 0
# Line number of FORTH.FTH during meta-compilation
line_num = 0
# Dictionary of precompiled primaries
precompiled = {}
# A list of messages from the compilation
msgs = []

# ----------------------------------------------------------------------
# Lower level help functions

def nop():
    pass

def ldb(addr):
    return ram[addr]

def stb(addr, data):
    global ram
    #if data > 255 or data < 0: stop('stb(): <%d> does not fit into byte' % data)
    ram[addr] = data & 0xFF
    return data

def ldw(addr):
    return ldb(addr) + (ldb(addr + 1) << 8)

def stw(addr, data):
    stb(addr, data & 0x00FF)
    stb(addr + 1, data >> 8)
    return data

def push(w):
    global sp
    sp -= 2; stw(sp, w)
    return w

def pop():
    global sp
    w = ldw(sp); sp += 2
    return w

def nth(n):
    return ldw(sp + (n << 1))

def top():
    return ldw(sp)

def rpush(w):
    global rp
    rp -= 2; stw(rp, w)
    return w

def rpop():
    global rp
    w = ldw(rp); rp += 2
    return w

def rnth(n):
    return ldw(rp + (n << 1))

def rtop():
    return ldw(rp)

def imm():
    global ip
    tmp = ldw(ip); ip += 2
    return tmp

# ----------------------------------------------------------------------
# The Virtual Machine (the actual functions run by the primaries)
# ----------------------------------------------------------------------

def qrxt():
    pop(); push(0)

def qrx():
    push(0)

def txsto():
    sys.stdout.write(chr(pop()))
    sys.stdout.flush()

def stio():
    pass

def lit():
    push(imm())

def call():
    global ip
    rpush(ip)
    ip = wa

def next():
    global ip, rp
    tmp = rtop() - 1
    if tmp >= 0:
        stw(rp, tmp)
        ip = ldw(ip)
    else:
        rp += 2; ip += 2

def qbranch():
    global ip
    if pop() == 0: ip = ldw(ip)
    else: ip += 2

def branch():
    global ip
    ip = ldw(ip)

def EXIT():
    global ip
    ip = rpop()

def EXEC():
    global wa
    wa = pop()
    tmp = ldb(wa); wa += 1
    primaries[tmp]()

def STORE():
    addr = pop()
    stw(addr, pop())

def AT():
    push(ldw(pop()))

def CSTOR():
    addr = pop()
    stb(addr, pop() & 0xFF)

def CAT():
    push(ldb(pop()))

def RPAT():
    push(rp)

def RPSTO():
    global rp
    rp = pop()

def TOR():
    rpush(pop())

def FROMR():
    push(rpop())

def RAT():
    push(rtop())

def SPAT():
    push(sp)

def SPSTO():
    global sp
    sp = pop()

def DROP():
    global sp
    sp += 2

def DUP():
    push(top())

def SWAP():
    a = pop(); b = pop()
    push(a); push(b)

def OVER():
    push(nth(1))

def ZLESS():
    if pop() & 0x8000: push(TRUE)
    else: push(FALSE)

def AND():
    push(pop() & pop())

def OR():
    push(pop() | pop())

def XOR():
    push(pop() ^ pop())

def UMPLU():
    sum = pop() + pop()
    push(sum & 0xFFFF)
    push(sum >> 16)

def PLUS():
    UMPLU(); DROP()

def CMOVE():
    cnt = pop(); trg = pop(); src = pop()
    if trg == src or cnt == 0: return
    if (trg < src):
        while cnt:
            stb(trg, ldb(src))
            trg += 1; src += 1; cnt -= 1
    else:
        src += cnt; trg += cnt
        while cnt:
            trg -= 1; src -= 1; cnt -= 1
            stb(trg, ldb(src))

def ZERO():
    push(0)

def ONE():
    push(1)

def TWO():
    push(2)

def MONE():
    push(-1)

def MTWO():
    push(-2)

def ONEP():
    ONE(); PLUS()

def TWOP():
    TWO(); PLUS()

def ONEM():
    MONE(); PLUS()

def TWOM():
    MTWO(); PLUS()

def ROT():
    tmp = ldw(sp + 4)
    stw(sp + 4, ldw(sp + 2))
    stw(sp + 2, ldw(sp))
    stw(sp, tmp)

def TDROP():
    global sp
    sp += 4

def TDUP():
    OVER(); OVER()

def NIP():
    SWAP(); DROP()

def TUCK():
    SWAP(); OVER()

def NOT():
    stw(sp, ldw(sp) ^ 0xFFFF)

def NEG():
    NOT(); ONEP()

def DNEG():
    NOT(); TOR(); NOT(); ONE(); UMPLU(); FROMR(); PLUS()

def DPLUS():
    TOR(); SWAP(); TOR(); UMPLU(); FROMR(); FROMR(); PLUS(); PLUS()

def SUB():
    NEG(); PLUS()

def PSTOR():
    DUP(); AT(); ROT(); PLUS(); SWAP(); STORE()

def INCR():
    DUP(); AT(); ONEP(); SWAP(); STORE()

def DECR():
    DUP(); AT(); ONEM(); SWAP(); STORE()

def QDUP():
    if top(): DUP()

def ABS():
    DUP(); ZLESS()
    if pop(): NEG()

def ATEXE():
    AT(); QDUP()
    if pop():
        EXEC()

def ZEQU():
    if pop() == 0: push(TRUE)
    else: push(FALSE)

def EQU():
    SUB(); ZEQU()

def NEQ():
    EQU(); ZEQU()

def ULESS():
    tmp = pop()
    if pop() < tmp: push(TRUE)
    else: push(FALSE)

def LESS():
    SUB(); ZLESS()

def GREAT():
    SWAP(); LESS()

def MAX():
    tmp = pop()
    if top() < tmp: stw(sp, tmp)

def MIN():
    tmp = pop()
    if top() > tmp: stw(sp, tmp)

def WTHIN():
    OVER(); SUB(); TOR(); SUB(); FROMR(); ULESS()

def UMSMO():
    second = pop()
    first = pop() << 16
    first += pop()
    push((first % second) & 0xFFFF)
    push((first // second) & 0xFFFF)

def MSMOD():
    DUP(); ZLESS(); DUP(); TOR()
    if pop():
        NEG(); TOR(); DNEG(); FROMR()
    TOR(); DUP(); ZLESS()
    if pop():
        RAT(); PLUS()
    FROMR(); UMSMO(); FROMR()
    if pop():
        SWAP(); NEG(); SWAP()

def UMSTA():
    prod = pop() * pop()
    push(prod & 0xFFFF)
    push(prod >> 16)

def MSTAR():
    first = pop()
    second = pop()
    sign1 = first & 0x8000
    sign2 = second & 0x8000
    prod = first * second
    if (sign1 and not sign2) or (sign2 and not sign1): prod = -prod
    push(prod & 0xFFFF)
    push(prod >> 16)

def LSHFT():
    steps = pop()
    stw(sp, ldw(sp) << steps)

def TWPWR():
    stw(sp, 1 << ldw(sp))

def RSHFT():
    steps = pop()
    stw(sp, ldw(sp) >> steps)

# Pico-specific primaries

def PICON():
    pass

def PIFUN():
    pass

def DIR():
    pass

def FTOR():
    pass

def FFRMR():
    pass

def SEEK():
    pass

def RW():
    pass

# ----------------------------------------------------------------------
# INNER INTERPRETER

# primaries[OPCODE]()
primaries = {
    OP_QRXT: qrxt,      OP_QRX: qrx,        OP_TXSTO: txsto,    OP_STIO: stio,
    OP_LIT: lit,        OP_CALL: call,      OP_NEXT: next,      OP_QBRAN: qbranch,
    OP_BRAN: branch,    OP_EXEC: EXEC,      OP_EXIT: EXIT,      OP_STORE: STORE,
    OP_AT: AT,          OP_CSTOR: CSTOR,    OP_CAT: CAT,        OP_RPAT: RPAT,
    OP_RPSTO: RPSTO,    OP_TOR: TOR,        OP_FROMR: FROMR,    OP_RAT: RAT,
    OP_SPAT: SPAT,      OP_SPSTO: SPSTO,    OP_DROP: DROP,      OP_DUP: DUP,
    OP_SWAP: SWAP,      OP_OVER: OVER,      OP_ZLESS: ZLESS,    OP_AND: AND,
    OP_OR: OR,          OP_XOR: XOR,        OP_UMPLU: UMPLU,    OP_CMOVE: CMOVE,
    OP_ONEP: ONEP,      OP_TWOP: TWOP,      OP_ONEM: ONEM,      OP_TWOM: TWOM,
    OP_ROT: ROT,        OP_TDROP: TDROP,    OP_TDUP: TDUP,      OP_NIP: NIP,
    OP_TUCK: TUCK,      OP_NOT: NOT,        OP_NEG: NEG,        OP_DNEG: DNEG,
    OP_DPLUS: DPLUS,    OP_SUB: SUB,        OP_PSTOR: PSTOR,    OP_INCR: INCR,
    OP_DECR: DECR,      OP_QDUP: QDUP,      OP_ABS: ABS,        OP_ATEXE: ATEXE,
    OP_ZEQU: ZEQU,      OP_EQU: EQU,        OP_NEQ: NEQ,        OP_ULESS: ULESS,
    OP_LESS: LESS,      OP_GREAT: GREAT,    OP_MAX: MAX,        OP_MIN: MIN,
    OP_WTHIN: WTHIN,    OP_UMSMO: UMSMO,    OP_MSMOD: MSMOD,    OP_UMSTA: UMSTA,
    OP_MSTAR: MSTAR,    OP_LSHFT: LSHFT,    OP_TWPWR: TWPWR,    OP_RSHFT: RSHFT,
    OP_PICON: PICON,    OP_PIFUN: PIFUN,    OP_DIR: DIR,        OP_FTOR: FTOR,
    OP_FFRMR: FFRMR
}

def inner():
    global wa, ip
    while True:
        wa = ldw(ip); ip += 2
        if wa == 0xFFFF: break
        opcode = ldb(wa); wa += 1
        primaries[opcode]()

# ----------------------------------------------------------------------
# TEMPORARY FORTH WORDS (USED IN THE BEGINNING OF META-COMPILATION)

def _HEAD():
    name = token()
    if not name: stop('_HEAD(): Missing token')
    push(ldw(ldw(UVAR_CURR))); _COMMA() # Link to latest word in CURRENT vocabulary
    stw(UVAR_LATES, ldw(UVAR_DP))       # Latest now to point to this header
    push(len(name) | 0x80); _CCOMM()    # Length byte
    for i in range(len(name)):
        push(ord(name[i])); _CCOMM()    # Actual FORTH word name

def _BSLAS():
    stw(UVAR_INPTR, ldw(UVAR_NTIB))

def _CCOMM():
    stb(ldw(UVAR_DP), pop())
    stw(UVAR_DP, ldw(UVAR_DP) + 1)

def _COMMA():
    stw(ldw(UVAR_DP), pop())
    stw(UVAR_DP, ldw(UVAR_DP) + 2)

def _CALLC():
    push(OP_CALL); _CCOMM()

def _OVERT():
    stw(ldw(UVAR_CURR), ldw(UVAR_LATES))

def _COLON():
    _HEAD(); _CALLC()
    stw(UVAR_STATE, TRUE)

def _SEMIC():
    push(must_find('EXIT')); _COMMA()
    stw(UVAR_STATE, FALSE)
    _OVERT()

def _CREAT():
    _HEAD(); _CALLC(); _OVERT()

def _CONST():
    _CREAT(); push(must_find('con')); _COMMA(); _COMMA()

def _USER():
    _CREAT(); push(must_find('con')); _COMMA(); push(UA0); PLUS(); _COMMA()

def _VAR():
    _CREAT(); push(must_find('var')); _COMMA(); push(0); _COMMA()

def _BCOMP():
    push(must_find(token())); _COMMA()

def _DOTH():
    print(' %04.4X ' % pop(), end = '')

def _DOTD():
    print(' %d ' % pop(), end = '')

def _TEMP(name, function, flags = 0):
    global precompiled
    precompiled[name] = (function, flags)

def _STOP():
    stop('STOP encountered')

def precompile():
    _TEMP('HEAD', _HEAD)
    _TEMP('\\', _BSLAS, IMMEDIATE)
    _TEMP('OVERT', _OVERT)
    _TEMP('C,', _CCOMM)
    _TEMP(',', _COMMA)
    _TEMP('call,', _CALLC)
    _TEMP(':', _COLON)
    _TEMP(';', _SEMIC, IMMEDIATE)
    _TEMP('CREATE', _CREAT)
    _TEMP('CONSTANT', _CONST)
    _TEMP('USER', _USER)
    _TEMP('VARIABLE', _VAR)
    _TEMP('[COMPILE]', _BCOMP, IMMEDIATE)
    _TEMP('.H', _DOTH)
    _TEMP('.D', _DOTD)
    _TEMP('STOP', _STOP)

# ----------------------------------------------------------------------
# THE ACTUAL META-COMPILER

def stop(txt):
    global msgs
    msgs.append(txt)
    raise Exception(txt, line_num) from None

def token():
    tok = ''
    # Skip leading blanks
    while True:
        ch = ldb(TIB0 + ldw(UVAR_INPTR))
        if ch == 0: return None
        if ch != 32: break
        stw(UVAR_INPTR, ldw(UVAR_INPTR) + 1)
    # Collect the non-blank token
    while True:
        ch = chr(ldb(TIB0 + ldw(UVAR_INPTR)))
        if ch == chr(0) or ch == ' ':
            if ch == ' ': stw(UVAR_INPTR, ldw(UVAR_INPTR) + 1)
            return tok
        tok = tok + ch
        stw(UVAR_INPTR, ldw(UVAR_INPTR) + 1)

def number(tok):
    isNum, value, base, nega = False, 0, ldw(UVAR_BASE), False
    try:
        # If number starts with $ or - or any combination of the two
        # then treat as hex and/or negative
        if tok[0] == '$':
            base = 16
            tok = tok[1:]
        if tok[0] == '-':
            nega = True
            tok = tok[1:]
        if tok[0] == '$':
            base = 16
            tok = tok[1:]
        value = int(tok, base)
        if nega: value = -value & 0xFFFF
        isNum = True
    except ValueError:
        stop('number(): [%s] is not a number' % tok)
    return isNum, value

def find_word(name):
    wrk = ldw(ldw(UVAR_CONT))
    while True:
        if wrk == 0: break
        if (ldb(wrk) & 0x1F) == len(name):
            wrk2 = wrk + 1
            for i in range(len(name)):
                if chr(ldb(wrk2 + i)) != name[i]: break
            else:
                return (wrk2 + len(name), ldb(wrk))
        wrk = ldw(wrk - 2)
    wrk = ldw(ldw(UVAR_CURR))
    while True:
        if wrk == 0: return (None, None)
        if (ldb(wrk) & 0x1F) == len(name):
            wrk2 = wrk + 1
            for i in range(len(name)):
                if chr(ldb(wrk2 + i)) != name[i]: break
            else:
                return (wrk2 + len(name), ldb(wrk))
        wrk = ldw(wrk - 2)

def must_find(name):
    wa, lb = find_word(name)
    if wa == None:
        stop('Cannot find <%s>')
    else:
        return wa

def find_precompiled(tok):
    if tok in precompiled:
        return precompiled[tok]
    else:
        return None

def run_word(addr):
    global ip
    rpush(ip); rpush(0xFFFF); rpush(addr)
    ip = rp; inner()
    rpop(); rpop(); ip = rpop()

def eval(tok):
    global ip
    addr, lb = find_word(tok)
    if addr:
        # Name found in FORTH vocabulary
        if ldw(UVAR_STATE) == FALSE or (lb & IMMEDIATE):
            run_word(addr)
        else:
            push(addr); _COMMA()
    else:
        prim_func = find_precompiled(tok)
        if prim_func:
            # Name found within precompiled words
            if ldw(UVAR_STATE) == FALSE or (prim_func[1] & IMMEDIATE):
                prim_func[0]()
            else:
                stop('eval(): Cannot compile precompiled <%s>' % tok)
        else:
            # Word not found, try to convert to number
            isNum, value = number(tok)
            if isNum:
                push(value)
                if ldw(UVAR_STATE):
                    push(must_find('lit')); _COMMA()
                    _COMMA()
            else:
                stop('eval(): Unknown word <%s>' % tok)

def handle_line(txt):
    txt_len = len(txt)
    if txt_len > 80:
        stop('handle_line(): Line too long')
    # Copy input line to TIB
    for i in range(txt_len): stb(TIB0 + i, ord(txt[i]))
    stw(TIB0 + txt_len, 0)
    stw(UVAR_NTIB, txt_len)
    stw(UVAR_INPTR, 0)
    while True:
        tok = token()
        if tok == None: return
        eval(tok)

def compile_FORTH():
    global source_file, line_num, sp, rp
    # Get FORTH.FTH
    src = open(FORTH_FTH, 'r')
    source_file = src.readlines()
    src.close()
    # Initialze the FORTH virtual machine
    sp = SP0
    rp = RP0
    stw(UVAR_DP, 40)
    stw(UVAR_STATE, 0)
    stw(UVAR_BASE, 10)
    stw(UVAR_NP, 0)
    stw(UVAR_CONT, UVAR_NP)
    stw(UVAR_CURR, UVAR_NP)
    stw(UVAR_VUNIQ, 0)
    # BOOT TABLE
    stw(BOOT_SP0, SP0)
    stw(BOOT_RP0, RP0)
    stw(BOOT_UA0, UA0)
    stw(BOOT_TIB, TIB0)
    stw(BOOT_FIRST, DSKBF)
    stw(BOOT_LIMIT, ENDBF)
    stw(BOOT_CR, _CR)
    stw(BOOT_NL, _NL)
    stw(BOOT_BSP, _BSP)
    # Precompile temporary words for meta-compilation
    precompile()
    # Compile FORTH.FTH
    for source_line in source_file:
        line_num += 1
        #print(line_num, end = ' ')
        #print('-' * 24)
        #print('Now on line %d with DP=%04.4X' % (line_num, ldw(UVAR_DP)))
        #print('NP(%04.4X)=%04.4X, CONT=%04.4X, CURR=%04.4X' %
        #      (UVAR_NP, ldw(UVAR_NP), ldw(UVAR_CONT), ldw(UVAR_CURR)))
        handle_line(source_line[:-1])
        #print('NP(%04.4X)=%04.4X, CONT=%04.4X, CURR=%04.4X' %
        #      (UVAR_NP, ldw(UVAR_NP), ldw(UVAR_CONT), ldw(UVAR_CURR)))
        #print(find_word('RESET'))
        #list_words()
        

# ----------------------------------------------------------------------
# OUTPUT FILE GENERATION

def generate_HS_file():
    loc, high_loc = 0, ldw(UVAR_DP)
    objFile = open('../' + FORTH_PRECOMPILED_HS, 'w')
    
    # Constants to picoFORTH.c
    #objFile.write('#define BOOT_COLD %d\n' % BOOT_COLD)
    #objFile.write('#define BOOT_SP0 %d\n' % BOOT_SP0)
    #objFile.write('#define BOOT_RP0 %d\n' % BOOT_RP0)
    #objFile.write('\n')
    #objFile.write('#define UVAR_FSYS %d\n' % UVAR_FSYS)
    #objFile.write('#define UVAR_FILE %d\n' % UVAR_FILE)
    #objFile.write('#define UVAR_TASKS %d\n' % UVAR_TASKS)
    #objFile.write('#define UVAR_TICKS %d\n' % UVAR_TICKS)
    #objFile.write('\n')
    #objFile.write('#define B_BUF %d\n' % B_BUF)
    #objFile.write('\n')

    # Contents of the FORTH RAM
    objFile.write('unit Precompiled {\n')
    objFile.write('const string PRECOMPILED = {\n')
    objFile.write('0x%02.2X, 0x%02.2X,' % (ldw(UVAR_DP) & 0xFF, ldw(UVAR_DP) >> 8))
    while loc < high_loc:
        if loc % 16 == 0: objFile.write('\n')
        objFile.write('0x%02.2X, ' % ram[loc])
        loc += 1
    objFile.write('\n};}\n')
    
    # All done
    objFile.close()

def FORTH_name(addr):
    fName = ''
    for i in range(ldb(addr) & 0x1F):
        fName = fName + chr(ldb(addr + i + 1))
    return fName

def dump(addr, n):
    i, du = 0, ''
    while i < n:
        du = du + '\n'
        du = du + ('%04.4X  ' % (addr + i))
        for j in range(16): du = du + ('%02.2X ' % ldb(addr + i + j))
        du = du + ' '
        for j in range(16):
            ch = ldb(addr + i + j)
            if ch > 31 and ch < 127: du = du + chr(ch)
            else: du = du + '.'
        i += 16
    du = du + '\n'
    return du

def list_words():
    lstFile = open(FORTH_PRECOMPILED_LST, 'w')
    wrk, nwds = ldw(ldw(UVAR_CONT)), 0
    lstFile.write('\n')
    print()
    for msg in msgs:
        lstFile.write('%s\n' % msg)
        print(msg)
    while wrk:
        if (nwds % 3) == 0:
            lstFile.write('\n')
            print()
        fName = FORTH_name(wrk)
        lstFile.write('%14s(%04.4X)' % (fName, wrk))
        print('%14s(%04.4X)' % (fName, wrk), end = '')
        wrk = ldw(wrk - 2)
        nwds += 1
    print()
    lstFile.write('\n%d words.\n%s\n' % (nwds, dump(0, ldw(UVAR_DP))))
    print('\n%d words.\n%s' % (nwds, dump(0, ldw(UVAR_DP))))
    for msg in msgs:
        lstFile.write('%s\n' % msg)
        print(msg)
    lstFile.close()

# ----------------------------------------------------------------------
#   " M A I N   P R O G R A M "

try:
    compile_FORTH()
    msgs.append('Reached end of FORTH.FTH')
    msgs.append('Compilation finished with no exceptions.')
except Exception as exc:
    print('\nStop: %s on line %d' % (exc.args[0], exc.args[1]))
except KeyboardInterrupt:
    print('KeyboardInterrupt')
generate_HS_file()
list_words()

# End of compile-FORTH.py
# ----------------------------------------------------------------------
