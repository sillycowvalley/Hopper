\ ----------------------------------------------------------------------
\
\ #   #                                   #####  ###  ####  ##### #   #
\ #   #  ###  ####  ####   ###  ####      #     #   # #   #   #   #   #
\ ##### #   # #   # #   # ##### #   # ### ###   #   # ####    #   #####
\ #   # #   # #   # #   # #     #         #     #   # #  #    #   #   #
\ #   #  ###  ####  ####   #### #         #      ###  #   #   #   #   #
\             #     #
\             #     #
\
\ MIT License
\
\ Copyright (c) [2024] [Nils "slowcorners" Kullberg]
\
\ Permission is hereby granted, free of charge, to any person obtaining a copy
\ of this software and associated documentation files (the "Software"), to deal
\ in the Software without restriction, including without limitation the rights
\ to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
\ copies of the Software, and to permit persons to whom the Software is
\ furnished to do so, subject to the following conditions:
\
\ The above copyright notice and this permission notice shall be included in all
\ copies or substantial portions of the Software.
\
\ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
\ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
\ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
\ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
\ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
\ OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
\ SOFTWARE.
\
\
\
\ ----------------------------------------------------------------------
\ P R E D E F I N E D   W O R D S   (contained in the meta-compiler)
\ ----------------------------------------------------------------------
\
\ : HEAD
\       CURRENT @ @ ,
\       HERE LATEST !
\       <length> C,
\       <nameChars> ... ;
\
\ HEAD EXIT 10 C,
\
\ : call,       <CALL> C, ;
\ : HERE        DP @ ;
\ : ALLOT       DP +! ;
\ : \           #TIB @ >IN ! ; IMMEDIATE
\ : OVERT       LATEST @ CURRENT @ ! ;
\ : IMMEDIATE   LATEST C@ $40 OR LATEST C! ;
\ : ,           HERE !  2 ALLOT ;
\ : C,          HERE C! 1 ALLOT ;
\ : [           0 STATE ! ; IMMEDIATE
\ : ]           -1 STATE ! ;
\ : COMPILE     R> DUP @ , 2+ >R ;
\ : [COMPILE]   ' , ; IMMEDIATE
\ : :           HEAD call, ] ;
\ : ;           COMPILE EXIT [COMPILE] [ OVERT ; IMMEDIATE

\ ----------------------------------------------------------------------
\ P R I M A R I E S
\ ----------------------------------------------------------------------

\ System interface

HEAD ?rx/t 0 C, OVERT   HEAD ?rx 1 C, OVERT     HEAD tx! 2 C, OVERT
HEAD !io 3 C, OVERT

\ Inner interpreter
HEAD lit 4 C, OVERT     HEAD call 5 C, OVERT    HEAD next 6 C, OVERT
HEAD ?branch 7 C, OVERT HEAD branch 8 C, OVERT  HEAD EXECUTE 9 C, OVERT
HEAD EXIT 10 C, OVERT

\ Memory access
HEAD ! 11 C, OVERT      HEAD @ 12 C, OVERT
HEAD C! 13 C, OVERT     HEAD C@ 14 C, OVERT

\ EXITurn stack
HEAD RP@ 15 C, OVERT    HEAD RP! 16 C, OVERT
HEAD >R 17 C, OVERT     HEAD R> 18 C, OVERT     HEAD R@ 19 C, OVERT

\ Data stack
HEAD SP@ 20 C, OVERT    HEAD SP! 21 C, OVERT    HEAD DROP 22 C, OVERT
HEAD DUP 23 C, OVERT    HEAD SWAP 24 C, OVERT   HEAD OVER 25 C, OVERT

\ Logic
HEAD 0< 26 C, OVERT
HEAD AND 27 C, OVERT    HEAD OR 28 C, OVERT     HEAD XOR 29 C, OVERT

\ Arithmetic
HEAD UM+ 30 C, OVERT

\ Block move
HEAD CMOVE 31 C, OVERT

\ Optimized secondaries
HEAD 1+ 32 C, OVERT     HEAD 2+ 33 C, OVERT     HEAD 1- 34 C, OVERT
HEAD 2- 35 C, OVERT     HEAD ROT 36 C, OVERT    HEAD 2DROP 37 C, OVERT
HEAD 2DUP 38 C, OVERT   HEAD NIP 39 C, OVERT    HEAD TUCK 40 C, OVERT
HEAD NOT 41 C, OVERT    HEAD NEGATE 42 C, OVERT HEAD DNEGATE 43 C, OVERT
HEAD D+ 44 C, OVERT     HEAD - 45 C, OVERT      HEAD +! 46 C, OVERT
HEAD ++ 47 C, OVERT     HEAD -- 48 C, OVERT
HEAD ?DUP 49 C, OVERT   HEAD ABS 50 C, OVERT    HEAD @EXECUTE 51 C, OVERT
HEAD 0= 52 C, OVERT     HEAD = 53 C, OVERT      HEAD <> 54 C, OVERT
HEAD U< 55 C, OVERT     HEAD < 56 C, OVERT      HEAD > 57 C, OVERT
HEAD MAX 58 C, OVERT    HEAD MIN 59 C, OVERT    HEAD WITHIN 60 C, OVERT
HEAD UM/MOD 61 C, OVERT HEAD M/MOD 62 C, OVERT
HEAD UM* 63 C, OVERT    HEAD M* 64 C, OVERT
HEAD << 65 C, OVERT     HEAD 2^ 66 C, OVERT     HEAD >> 67 C, OVERT

\ FORTH utilities
HEAD dispatch 71 C, OVERT

\ ----------------------------------------------------------------------
\ S E C O N D A R I E S
\ ----------------------------------------------------------------------

\ ----------------------------------------
\ Some fundamentals

: NOP ;
: +         UM+ DROP ;

\ ----------------------------------------
\ Run-time functions for CONSTANT and VARIABLE

: con       R> @ ;
: var       R> ;

\ ----------------------------------------
\ Boot Table constants

: CPU        8 @ ;
: VER       10 @ ;
: SP0       12 @ ;
: RP0       14 @ ;
: UA0       16 @ ;
: TIB       18 @ ;
: FIRST     20 @ ;
: LIMIT     22 @ ;
: _CR       24   ;
: NL        26 @ ;
: DP0       30 @ ;

\ ----------------------------------------
\ Miscellaneous constants

64      CONSTANT C/L            \ Characters per line (on a screen)
1024    CONSTANT B/BUF          \ Bytes per buffer (a "hard disk" block)

$00     CONSTANT <NUL>
$01     CONSTANT <SOH>
$02     CONSTANT <STX>
$04     CONSTANT <EOT>
$06     CONSTANT <ACK>
$08     CONSTANT <BSP>
$15     CONSTANT <NAK>
$18     CONSTANT <CAN>
$1A     CONSTANT <CTRL-Z>
$7F     CONSTANT <DEL>

\ ----------------------------------------
\ User variables

 0 USER STATE    2 USER BASE     4 USER CONTEXT  6 USER CURRENT
 8 USER DP      10 USER OUT     12 USER HLD     14 USER #TIB
16 USER SPAN    18 USER >IN     20 USER CSP     22 USER HANDLER
24 USER LAST    26 USER tmp     28 USER '?KEY   30 USER 'EMIT
32 USER 'ECHO   34 USER 'EXPECT 36 USER 'TAP    38 USER 'PROMPT
40 USER 'EVAL   42 USER 'NUMBER 44 USER SEED    46 USER 'UNIQUE
48 USER NP      50 USER 'DISPATCH 52 USER TASKS 54 USER AUTOLOAD
56 USER SCR#    58 USER POS#    60 USER CUR#    62 USER L/S
64 USER LINES   66 USER USE     68 USER PREV    70 USER 'UNIQUE
72 USER FSYS    74 USER file    76 USER file2   78 USER TICKS

\ ----------------------------------------
\ Common low level functions and constants

  0 CONSTANT  0
  1 CONSTANT  1       2 CONSTANT  2
 -1 CONSTANT -1      -2 CONSTANT -2

 -1 CONSTANT TRUE    0 CONSTANT FALSE   32 CONSTANT BL

: HERE      DP @ ;
: ALLOT     DP +! ;
: ,         HERE ! 2 ALLOT ;
: C,        HERE C! 1 ALLOT ;
: call,     5 C, ;
: PAD       HERE 80 + ;
: LATEST    CURRENT @ @ ;
: COMPILE   R> DUP @ , 2+ >R ;
: COMP-ONLY LATEST C@ $20 OR LATEST C! ;
: IMMEDIATE LATEST C@ $40 OR LATEST C! ;
: LITERAL   COMPILE lit , ; IMMEDIATE

\ ----------------------------------------
\ Conditionals and loops

: <MARK     HERE ;
: <RESOLVE  , ;
: >MARK     HERE 0 , ;
: >RESOLVE  <MARK SWAP ! ;
: FOR       COMPILE >R <MARK ; IMMEDIATE
: BEGIN     <MARK ; IMMEDIATE
: NEXT      COMPILE next <RESOLVE ; IMMEDIATE
: UNTIL     COMPILE ?branch <RESOLVE ; IMMEDIATE
: AGAIN     COMPILE branch <RESOLVE ; IMMEDIATE
: IF        COMPILE ?branch >MARK ; IMMEDIATE
: AHEAD     COMPILE branch >MARK ; IMMEDIATE
: REPEAT    [COMPILE] AGAIN >RESOLVE ; IMMEDIATE
: THEN      >RESOLVE ; IMMEDIATE
: AFT       DROP [COMPILE] AHEAD [COMPILE] BEGIN SWAP ; IMMEDIATE
: ELSE      [COMPILE] AHEAD SWAP [COMPILE] THEN ; IMMEDIATE
: WHEN      [COMPILE] IF OVER ; IMMEDIATE
: WHILE     [COMPILE] IF SWAP ; IMMEDIATE

\ ----------------------------------------
\ Basic terminal I/O

: ?KEY      '?KEY @EXECUTE ;
: EMIT      'EMIT @EXECUTE OUT ++ ;
: ECHO      'ECHO @EXECUTE ;
: PROMPT    'PROMPT @EXECUTE ;

: KEY       BEGIN ?KEY 0= WHILE 'DISPATCH @EXECUTE REPEAT ;
: NUF?      ?KEY DUP IF 2DROP KEY NL = THEN ;
: PACE      11 EMIT ;
: SPACE     BL EMIT ;
: CLS       12 EMIT ;
: CHARS     SWAP 0 MAX FOR AFT DUP EMIT THEN NEXT DROP ;
: >CHAR     $7F AND DUP 127 BL WITHIN IF DROP 46 THEN ;
: SPACES    BL CHARS ;
: COUNT     DUP 1+ SWAP C@ ;
: CR        _CR 1+ C@ EMIT _CR C@ EMIT 0 OUT ! LINES ++ ;
: ?CR       OUT @ + 1+ C/L 2- > IF CR THEN ;
: TYPE      FOR AFT DUP C@ EMIT 1+ THEN NEXT DROP ;
: _TYPE     DUP ?CR FOR AFT DUP C@ >CHAR EMIT 1+ THEN NEXT DROP ;
: $@        R> R@ R> COUNT + >R SWAP >R ;
: (")       R> DUP COUNT + >R ;
: (.")      $@ COUNT TYPE ; COMP-ONLY

\ ----------------------------------------
\ Divide and Multiply

: /MOD      OVER 0< SWAP M/MOD ; \ ( n n -- r q )
: MOD       /MOD DROP ;
: /         /MOD NIP ;

: *         UM* DROP ;
: */MOD     >R M* R> M/MOD ;
: */        */MOD NIP ;
: 2*        DUP UM+ DROP ;

\ ----------------------------------------
\ Stack and block functions

: DEPTH     SP0 SP@ - 2 / ;
: PICK      1+ 2* SP@ + @ ;
: 2!        SWAP OVER ! 2+ ! ;
: 2@        DUP 2+ @ SWAP @ ;
: file>R    R> file 2@ >R >R >R 0 0 file 2! ;
: R>file    R> R> R> file 2! >R ;

: FILL      SWAP FOR SWAP AFT 2DUP C! 1+ THEN NEXT 2DROP ;
: PACK$     DUP >R 2DUP C! 1+ SWAP CMOVE R> ;

\ ----------------------------------------
\ Numeric output

: DIGIT     9 OVER < 7 AND + 48 + ;
: EXTRACT   0 SWAP UM/MOD SWAP DIGIT ;
: <#        PAD HLD ! ;
: HOLD      HLD @ 1- DUP HLD ! C! ;
: #         BASE @ EXTRACT HOLD ;
: #S        BEGIN # DUP 0= UNTIL ;
: SIGN      0< IF 45 HOLD THEN ;
: #>        DROP HLD @ PAD OVER - ;
: str       DUP >R ABS <# #S R> SIGN #> ;
: R36       36 BASE ! ;
: HEX       16 BASE ! ;
: DECIMAL   10 BASE ! ;
: .R        >R str R> OVER - SPACES TYPE ;
: U.R       >R <# #S #> R> OVER - SPACES TYPE ;
: U.        <# #S #> SPACE TYPE ;
: .         BASE @ 10 <> IF U. EXIT THEN str SPACE TYPE ;
: ?         @ . ;
: .BASE     BASE @ DECIMAL DUP . BASE ! ;
: .FREE     SP@ HERE - U. ;
: .DEL      <BSP> ECHO BL ECHO <BSP> ECHO ;

: -TRIM                         \ ( addr count -- addr count' )
    BEGIN
        DUP 0= IF EXIT THEN
        2DUP 1- + C@ BL = NOT IF EXIT THEN
        1-
    AGAIN ;

\ ----------------------------------------
\ Numeric input

: DIGIT?    >R 48 - 9 OVER < IF 7 - DUP 10 < OR THEN DUP R> U< ;

: NUMBER?
    BASE @ >R 0 OVER COUNT OVER C@ 36 =
    IF HEX SWAP 1+ SWAP 1- THEN
    OVER C@ 45 = >R SWAP R@ - SWAP R@ + ?DUP
    IF
        1-
        FOR DUP >R
            C@ BASE @ DIGIT?
        WHILE SWAP
            BASE @ * +
            R> 1+
        NEXT
            R@ NIP IF NEGATE THEN SWAP
        ELSE
            R> R>
            2DROP 2DROP 0
        THEN DUP
    THEN R> 2DROP R> BASE ! ;

\ ----------------------------------------
\ Parsing

: parse
    tmp ! OVER >R DUP
    IF 1- tmp @ BL =
        IF
            FOR BL OVER C@ - 0< NOT WHILE 1+
            NEXT R> DROP 0 DUP EXIT
            THEN R>
        THEN OVER SWAP
        FOR tmp @ OVER C@ - tmp @ BL =
            IF 0< THEN WHILE 1+
        NEXT DUP >R ELSE R> DROP DUP 1+ >R
        THEN OVER - R> R> - EXIT
    THEN OVER R> - ;

: PARSE >R TIB >IN @ + #TIB @ >IN @ - R> parse >IN +! ;

: CHAR      BL PARSE DROP C@ ;
: ASCII     CHAR STATE @ IF COMPILE lit , THEN ; IMMEDIATE
: WORD      PARSE HERE PACK$ ;
: TOKEN     BL PARSE 31 MIN HERE 2+ PACK$ ;

: .(        ASCII ) PARSE TYPE ; IMMEDIATE
: (         ASCII ) PARSE 2DROP ; IMMEDIATE

\ ----------------------------------------
\ Dictionary search

: LEN?      C@ $1F AND SWAP C@ = ;         ( a va -- flag )

: SAME?                             ( a1 a2 n -- a1 a2 iflag )
    FOR AFT
        OVER R@ + C@ OVER R@ + C@
        - ?DUP
        IF R> DROP EXIT THEN
    THEN NEXT 0 ;

: >CODE     DUP C@ $1F AND + 1+ ;           ( na -- ca )

: >NAME                                     ( ca -- na )
    BEGIN
        1- DUP C@ $80 AND
    UNTIL ;

: find                              ( a va -- ca na | a F )
    BEGIN                                   \ a va
        2DUP LEN?                           \ a va flag
        IF
            OVER C@ >R                      \ a va
            2DUP SWAP 1+ SWAP 1+            \ a va a+1 va+1
            R> SAME? 0=
            IF
                2DROP NIP                   \ va
                DUP >CODE                   \ na ca
                SWAP EXIT                   \ ca na
            ELSE
                2DROP
            THEN
        THEN
        2- @ DUP 0=
    UNTIL ;

: NAME?     CONTEXT @ @ find DUP 0=
            IF DROP LATEST find THEN ;

\ ----------------------------------------
\ Cut the depencies to precompiled words 1(2)

: \ #TIB @ >IN ! ; IMMEDIATE

: ?UNIQUE   'UNIQUE @EXECUTE ;

: HEAD
    TOKEN ?UNIQUE DUP >R
    2- LATEST SWAP !                \ Link to previous
    R@ LAST !                       \ LAST = This header
    R@ DUP C@ + 1+ DP !             \ ALLOT name
    R@ C@ $80 XOR R> C! ;           \ Set high bit of length

: OVERT     LAST @ CURRENT @ ! ;
: CREATE    HEAD call, OVERT ;
: CONSTANT  CREATE COMPILE con , ;
: USER      CREATE COMPILE con UA0 + , ;
: VARIABLE  CREATE COMPILE var 0 , ;

: [         FALSE STATE ! ; IMMEDIATE
: ]         TRUE STATE ! ;
: :         HEAD call, ] ;

\ ----------------------------------------
\ FORTH Interest Group extras

\ : VOCABULARY
\    <BUILDS CURRENT @ 2+ , $81 C, BL C, DOES> CONTEXT ! ;
\
\ | LINK | HEAD | call | doDOES | doVOC | GRAFT | 0x8120 |
\                  0        1       3      5^
\                                        CURRENT
\                                        CONTEXT
: voc           R> CONTEXT ! ;
: VOCABULARY    CREATE COMPILE voc CURRENT @ 2+ , $81 C, BL C, ;
: DEFINITIONS   CONTEXT @ CURRENT ! ;

\ ----------------------------------------
\ Terminal input

: ^H        >R OVER R@ < DUP IF .DEL THEN R> + ;
: TAP       DUP ECHO OVER C! 1+ 0 OVER ! ;

: kTAP
    DUP _CR C@ = OVER _CR 1+ C@ = OR IF
        DROP NIP DUP EXIT
    THEN
    DUP <BSP> = SWAP <DEL> = OR IF ^H ELSE BL TAP THEN ;

: accept                            ( buf cnt -- )
    OVER + OVER
    BEGIN
        2DUP <>
    WHILE
        KEY DUP BL - 95 U<
        IF TAP ELSE kTAP THEN
    REPEAT DROP OVER - ;

: EXPECT    'EXPECT @EXECUTE SPAN ! DROP ;
: QUERY     TIB 80 'EXPECT @EXECUTE #TIB ! DROP 0 >IN ! ;

\ ----------------------------------------
\ Strings

: ,"        ASCII " PARSE HERE PACK$ C@ 1+ ALLOT ;

: "         STATE @ IF COMPILE (") ,"
            ELSE ASCII " PARSE PAD PACK$ THEN ; IMMEDIATE

: ."        COMPILE (.") ," ; IMMEDIATE
: .ID       ?DUP IF COUNT $1F AND _TYPE EXIT THEN ." <noname>" ;
: .CPU      BASE @ R36 CPU U. BASE ! ;
: .VER      BASE @ HEX VER <# # # # ASCII . HOLD # #> TYPE BASE ! ;
: .FREE     SP@ HERE - U. ;

: MORE?
    L/S @
    IF
        LINES @ L/S @ U<
        IF
            FALSE EXIT
        THEN 0 LINES ! ." More? " KEY NL =
        -6 OUT +!
        6 FOR AFT .DEL THEN NEXT \ Erase "More?" message
    ELSE
        FALSE
    THEN ;

: ?unique   DUP NAME? IF ."  redefined" THEN DROP ;

\ ----------------------------------------
\ Exception handling

: catch
    SP@ >R HANDLER @ >R RP@ HANDLER !
    EXECUTE R> HANDLER ! R> DROP 0 ;

: CATCH
    TOKEN NAME?
    IF
        COMPILE lit , COMPILE catch EXIT
    THEN
    ."  catch what? " ; COMP-ONLY IMMEDIATE

: THROW
    ?DUP
    IF
        HANDLER @
        IF
            HANDLER @ RP!
            R> HANDLER !
            R> SWAP >R SP!
            DROP R>
        ELSE
            ." No catcher!"
        THEN
    THEN ;

0 HANDLER !

: NULL$     " " ;

: ABORT     NULL$ THROW ;
: abort"    IF $@ THROW THEN $@ DROP ; COMP-ONLY
: ABORT"    COMPILE abort" ," ; IMMEDIATE

: '         TOKEN NAME? IF EXIT THEN THROW ;

\ ----------------------------------------
\ Cut the depencies to precompiled words 2(2)

: [COMPILE]     ' , ; IMMEDIATE
: ;         COMPILE EXIT [COMPILE] [ OVERT ; IMMEDIATE

' ?unique 'UNIQUE !

\ ----------------------------------------
\ SlowCorners extras
\
\ Simple but effective DOER .. MAKE

\ : MAKE
\     [COMPILE] ' 3 + [COMPILE] ' 1+ STATE @
\     IF COMPILE lit , COMPILE lit , COMPILE !
\     ELSE SWAP !
\     THEN ; IMMEDIATE

\ : doer      R> @ >R ;
\ : DOER      CREATE COMPILE doer [ ' NOP 1+ ] LITERAL , ;

\ ----------------------------------------
\ 16-bit version of George Marsaglia's wonderful XorShift
\ pseudo-random number generator

: MAXRND    SEED @ DUP 7 << XOR DUP 9 >> XOR DUP 8 << XOR DUP SEED ! ;

: SCALE     65535 SWAP 0 SWAP UM/MOD NIP ;              ( max -- divisor )
: RND       SCALE MAXRND SWAP 0 SWAP UM/MOD NIP 1+ ;    ( max -- rnd )
: DICE      SWAP FOR 0 AFT OVER RND + THEN NEXT NIP ;   ( n eyes -- sum )

\ ----------------------------------------
\ Interpret and Compile

: $INTERPRET
    NAME? ?DUP
    IF C@ $20 AND ABORT" compile only" EXECUTE EXIT
    THEN 'NUMBER @EXECUTE IF EXIT THEN THROW ;

: $COMPILE                      \ a --
    NAME? ?DUP
    IF C@ $40 AND
        IF EXECUTE ELSE , THEN EXIT
    THEN 'NUMBER @EXECUTE
    IF [COMPILE] LITERAL EXIT
    THEN THROW ;

: .OK
    STATE @ 0=
    IF
        BL ECHO ASCII o ECHO ASCII k ECHO
    THEN _CR 1+ C@ ECHO _CR C@ ECHO ;

: ?STACK    DEPTH 0< ABORT" underflow" ;

: EVAL
    BEGIN
        TOKEN DUP C@
    WHILE STATE @
        IF $COMPILE
        ELSE $INTERPRET
        THEN ?STACK
    REPEAT DROP PROMPT ;

\ ----------------------------------------
\ Dictionary dump

: WORDS
    CR 0 LINES ! CONTEXT @
    BEGIN @ ?DUP
    WHILE DUP .ID SPACE 2- MORE?
    UNTIL DROP THEN ;

: #WORDS
    0 CONTEXT @ BEGIN @ ?DUP WHILE SWAP 1+ SWAP 2- REPEAT ;

: duNUM                                 ( addr )
    15 FOR DUP C@ 3 .R 1+ NEXT DROP ;
;

: duASCII                               ( addr )
    SPACE SPACE 16 _TYPE ;
;

: DUMP
    CR SWAP 6 SPACES
    DUP $000F AND FOR AFT 3 SPACES THEN NEXT
    ASCII | EMIT CR $FFF0 AND SWAP
    BASE @ >R HEX 16 /
    FOR
        DUP 5 U.R DUP duNUM 0 OUT ! DUP duASCII CR
        16 +
    NEXT
    DROP R> BASE ! ;

\ ----------------------------------------
\ O U T E R   I N T E R P R E T E R
\ ----------------------------------------

: -ECHO
    [ ' DROP ]    LITERAL 'ECHO ! ;

: +ECHO
    [ ' tx!  ]    LITERAL 'ECHO ! ;

: CONSOLE
    [ ' NUMBER? ] LITERAL 'NUMBER !
    [ ' accept ]  LITERAL 'EXPECT !
    [ ' ?rx  ]    LITERAL '?KEY !
    [ ' tx!  ]    LITERAL 'EMIT !
    [ ' .OK   ]   LITERAL 'PROMPT !
    [ ' ?unique ] LITERAL 'UNIQUE !
    +ECHO ;

\ Used by both FILE LOAD and SCREEN LOAD as "CATCH load"
: load  BEGIN QUERY EVAL AGAIN ;

: QUIT
    RP0 RP! SP0 SP!
    BEGIN
        [COMPILE] [
        BEGIN
            QUERY SPACE CATCH EVAL ?DUP   \ Execute until error
        UNTIL
        CONSOLE DUP 0 > OVER 1000 < AND IF
            ." EXC_" 0 U.R CR
        ELSE
            NULL$ OVER <> IF
                SPACE COUNT TYPE ."  ?" CR
            THEN
        THEN
        SP0 SP!
    AGAIN ;

VOCABULARY FORTH IMMEDIATE

' FORTH >NAME ' FORTH 3 + !

FORTH DEFINITIONS

: WARM
    RP0 RP! SP0 SP!
    CONSOLE
    [COMPILE] FORTH DEFINITIONS DECIMAL
    FIRST USE ! FIRST PREV !
    ." Welcome to picoFORTH v. " .VER CR
    QUIT ;

: COLD
    DP0 DP !
    1 SEED !
    [ ' dispatch ] LITERAL 'DISPATCH !
    0 L/S !
    WARM ;

\ ----------------------------------------
\ Raspberyy Pi Pico vocabulary

VOCABULARY PICO IMMEDIATE

PICO DEFINITIONS

HEAD pi# 68 C, OVERT
: pico#     R> @ ;
: Pico#     CREATE COMPILE pico# pi# , ;

HEAD pi() 69 C, OVERT
: pico()    R> @ pi() ;
: Pico()    CREATE COMPILE pico() , ;

\ ----------------------------------------
\ LittlFS file system

FORTH DEFINITIONS

VOCABULARY FILE IMMEDIATE

FILE DEFINITIONS

200 CONSTANT EXC_NOT_FOUND
201 CONSTANT EXC_ALR_OPEN
202 CONSTANT EXC_NOT_OPEN
203 CONSTANT EXC_WR_ERR
204 CONSTANT EXC_RD_ERR
205 CONSTANT EXC_UMO_ERR
206 CONSTANT EXC_MOU_ERR
207 CONSTANT EXC_EOF
208 CONSTANT EXC_SEEK
209 CONSTANT EXC_RW_ERR

$0001   CONSTANT O_RD
$0002   CONSTANT O_WR
$0003   CONSTANT O_RDWR
$0100   CONSTANT O_CREAT
$0200   CONSTANT O_EXCL
$0400   CONSTANT O_TRUNC
$0800   CONSTANT O_APPEND

PICO
200 Pico() del
201 Pico() open
202 Pico() close
203 Pico() write
204 Pico() read
205 Pico() format
206 Pico() seek

HEAD DIR 70 C, OVERT

: DEL       del ?DUP IF THROW THEN ;
: OPEN      open ?DUP IF THROW THEN ;
: CLOSE     close ?DUP IF THROW THEN ;
: WRITE     write ?DUP IF THROW THEN ;
: READ      read ?DUP IF THROW THEN ;
: EXISTS    O_RD OPEN CLOSE ;
: FORMAT    format ?DUP IF THROW THEN ;
: SEEK      seek ?DUP IF THROW THEN ;

\ tx! to file
: file!     SP@ 1 WRITE DROP ;
\ ?rx from file
: ?file
    'DISPATCH @EXECUTE
    0 SP@ 1 READ NEGATE ?DUP
    IF EXIT THEN EXC_EOF THROW ;

: LIST
    CR O_RD OPEN
    BEGIN
        CATCH ?file IF CLOSE CR EXIT THEN
        DROP EMIT
    AGAIN ;

: LOAD
    file>R '?KEY @ >R ( 'ECHO @ >R -ECHO )
    [ ' ?file ] LITERAL '?KEY !
    O_RD CATCH OPEN DUP 0= IF
        DROP CATCH load CLOSE
    THEN
    ( R> 'ECHO ! ) R> '?KEY ! R>file
    DUP EXC_EOF = IF DROP EXIT THEN THROW ;

\ ----------------------------------------
\ XMODEM File Transfer

FILE DEFINITIONS

VOCABULARY XMODEM IMMEDIATE

XMODEM DEFINITIONS

111     CONSTANT EXC_CANCEL
112     CONSTANT EXC_SYNC_ERR

VARIABLE 'ACTION
VARIABLE P-BUFF
VARIABLE P-NUM
VARIABLE P-SIZE
VARIABLE BYTES

: FAILURE                                   ( -- )
    CATCH CLOSE DROP
    <CAN> EMIT <CAN> EMIT <CAN> EMIT
    EXC_SYNC_ERR THROW ;

: FLUSH-INPUT                               ( -- )
    BEGIN
        1000 ?rx/t
    WHILE
        DROP
    REPEAT ;

: ACCEPT    <ACK> EMIT ;
: REJECT    FLUSH-INPUT <NAK> EMIT ;

: CSUM                                      ( addr n -- csum8 )
    0 SWAP
    FOR AFT
        OVER R@ + C@ +
    THEN NEXT
    NIP $00FF AND ;

: CHECK?                                    ( -- flag )
    P-BUFF @ 2+ P-SIZE @ 2DUP + C@ >R CSUM R> = ( packet checksum ok )
    P-BUFF @ C@ P-BUFF @ 1+ C@ NOT $FF AND =    ( byte1 == ~byte2 )
    AND
    P-BUFF @ C@ P-NUM C@ =                  ( addr_byte1 == expected )
    P-BUFF @ C@ 1+ $FF AND P-NUM C@ = OR    ( addr_byte1+1 == expected )
    AND ;

: ?CTRL-Z                                   ( addr n -- addr n' )
    0 >R
    BEGIN
        R@ OVER <
    WHILE
        OVER R@ + C@ <CTRL-Z> = IF
            DROP R> EXIT
        THEN
        R> 1+ >R
    REPEAT
    R> DROP ;

: HANDLE-PACKET
    CHECK? IF
        P-BUFF @ C@ P-NUM C@ = IF
            P-BUFF @ 2+ P-SIZE @ ?CTRL-Z WRITE
            P-NUM C@ 1+ P-NUM C!
            P-SIZE @ BYTES +!
        THEN
        ACCEPT
    ELSE
        REJECT
    THEN ;

: RCVE-PACKET
    P-BUFF @ P-SIZE @ 3 +
    FOR AFT                             ( addr ) 
        1000 ?rx/t IF                   ( addr ch )
            OVER C! 1+                  ( addr+1 )
        ELSE
            REJECT R> 2DROP FALSE EXIT  ( skip loop counter and address )
        THEN
    THEN NEXT DROP TRUE ;

: RCVE      RCVE-PACKET IF HANDLE-PACKET THEN ;

: DONE
    FLUSH-INPUT ACCEPT ACCEPT
    CATCH CLOSE DROP
    R> DROP ;                           ( Also exit calling word )

: CANCEL
    ACCEPT FLUSH-INPUT
    CATCH CLOSE DROP
    EXC_CANCEL THROW ;

: PROBE
    [ ' FAILURE ] LITERAL 'ACTION !
    32 FOR AFT
        <NAK> EMIT
        1000 ?rx/t IF
            DUP <SOH> = IF
                128 P-SIZE ! [ ' RCVE ] LITERAL 'ACTION !
            ELSE
                DUP <STX> = IF
                    1024 P-SIZE ! [ ' RCVE ] LITERAL 'ACTION !
                ELSE
                    DUP <EOT> = IF
                        [ ' DONE ] LITERAL 'ACTION !
                    ELSE
                        DUP <CAN> = IF
                            [ ' CANCEL ] LITERAL 'ACTION !
                        THEN
                    THEN
                THEN
            THEN
            R> 2DROP EXIT
        THEN
    THEN NEXT
    FAILURE ;

: RECEIVE
    O_WR O_CREAT O_TRUNC OR OR CATCH OPEN IF FAILURE THEN
    FIRST P-BUFF !
    1 P-NUM !
    0 BYTES !
    BEGIN
        PROBE 'ACTION @EXECUTE
    AGAIN
;

FORTH DEFINITIONS FILE

: R/W
    \ ." R/W: " U. U. U. EXIT
    file>R " FORTH.DAT" O_RDWR OPEN
    SWAP SEEK IF B/BUF READ DROP ELSE B/BUF WRITE B/BUF THEN
    CLOSE R>file
;

\ ----------------------------------------
\ FORTH SCREEN I/O

VOCABULARY SCREEN IMMEDIATE

SCREEN DEFINITIONS

107 CONSTANT EXC_EOSCR

: +BUF                 ( addr1 -- addr flag )
    B/BUF 10 + +
    DUP LIMIT =
    IF DROP FIRST THEN
    DUP PREV @ <>
;

: BUFFER                ( n -- addr )
    USE @ DUP >R
    BEGIN +BUF UNTIL
    USE ! R@ @ 0<
    IF R@ 2+ R@ @ $7FFF AND 0 R/W THEN
    R@ ! R@ PREV ! R> 2+ ;

: UPDATE    PREV @ @ $8000 OR PREV @ ! ;

: DISCARD
    FIRST LIMIT OVER - 0 FILL ;

: FLUSH
    LIMIT FIRST - B/BUF / FOR 0 BUFFER DROP NEXT ;

: BLOCK                 ( n -- addr )
    >R
    PREV @ DUP @        ( prev blk# )
    R@ - DUP +
    IF
        BEGIN
            +BUF 0=
            IF
                DROP R@ BUFFER DUP R@ 1 R/W 2-
            THEN
            DUP @ R@ - DUP + 0=
        UNTIL
        DUP PREV !
    THEN
    R> DROP 2+ ;

: line                      ( line screen -- addr count )
    BLOCK SWAP C/L * + C/L ;

: LIST                   ( n -- )
    CR 15
    FOR
        15 R@ - DUP 2 .R SPACE OVER line -TRIM TYPE CR
    NEXT
    DROP ;

: INDEX                 ( start end -- )
    CR OVER -
    FOR
        DUP 3 .R SPACE 0 OVER line -TRIM TYPE CR 1+
    NEXT
    DROP ;

: CLEAR                 ( start end -- )
    1+ SWAP
    BEGIN
        2DUP = IF 2DROP FLUSH EXIT THEN
        DUP BLOCK B/BUF BL FILL UPDATE 1+
    AGAIN ;

: <nl>      SCR# @ NEGATE SCR# ! ;
: line#     POS# @ C/L / ;
: col#      POS# @ C/L MOD ;
: #rest     C/L col# - ;

: ?scr
    SCR# @ 0< IF <nl> NL TRUE EXIT THEN
    POS# @ B/BUF <
    IF
        SCR# @ BLOCK POS# @ + #rest -TRIM
        IF
            POS# ++ C@
            DUP 0= IF EXC_EOSCR THROW THEN
            col# 0= IF <nl> THEN
        ELSE
            DROP #rest POS# +! NL
        THEN
        TRUE
    ELSE
        EXC_EOSCR THROW
    THEN ;

: LOAD
    '?KEY @ >R 'ECHO @ >R SCR# @ >R POS# @ >R
    SCR# ! 0 POS# ! [ ' ?scr ] LITERAL '?KEY ! -ECHO
    CATCH load
    R> POS# ! R> SCR# ! R> 'ECHO ! R> '?KEY ! 
    DUP EXC_EOSCR = IF DROP EXIT THEN
    THROW ;

: FORMAT
    file>R FILE " FORTH.DAT" O_WR O_CREAT O_TRUNC OR OR OPEN
    FOR AFT
        1024 FOR AFT BL file! THEN NEXT
        ASCII . ECHO
    THEN NEXT
    CLOSE R>file ;

\ ----------------------------------------
\ Multitasker

\ TASK:
\ +--------+--------+--------+
\ | Ticker |   ca   |  next  |
\ +--------+--------+--------+
\  0        2        4

FORTH DEFINITIONS

: task      R> ;
: TASK      CREATE COMPILE task 0 , [ ' NOP ] LITERAL , 0 , ;

: >QUEUE                    ( taddr -- )
    0 OVER 4 + ! TASKS
    BEGIN DUP @ 0= IF ! EXIT THEN @ 4 +
    AGAIN ;

: RUNS      ' SWAP 2+ ! ;
: AFTER     DUP >R ! R> >QUEUE ;            ( delay taddr -- )
: START     0 SWAP AFTER ;                  ( taddr -- )
: STAT      TASKS @ OVER 4 + ! TASKS ! ;    ( taddr -- )

: STOP                                      ( taddr -- )
    TASKS
    BEGIN 2DUP @ DUP
    WHILE = IF SWAP 4 + @ SWAP ! EXIT THEN @ 4 +
    REPEAT 2DROP 2DROP ;

\ ----------------------------------------
\ FORGET with built-in FENCE

FORTH DEFINITIONS

: FORGET
    CURRENT @ CONTEXT @ - ABORT" need definitions"
    ' DUP [ HERE ] LITERAL > NOT ABORT" within FENCE"
    DUP DP !
    >NAME 2- @ CONTEXT @ ! ;

\ ----------------------------------------
\ Update Boot Table

' COLD 1+ 0 !               \  0: COLD vector (skip initial <CALL>)
' WARM 1+ 2 !               \  2: WARM vector (skip initial <CALL>)
                            \  4: Processor Core (0 or 1)
                            \  6: -
R36 RPP DECIMAL 8 !         \  8: Processor type in Radix 36
HEX 0000 DECIMAL 10 !       \ 10: picoFORTH version as 4 hex nybbles
                            \ 12: SP0   (set by compile-FORTH.py)
                            \ 14: RP0   (set by compile-FORTH.py)
                            \ 16: UA0   (set by compile-FORTH.py)
                            \ 18: TIB   (set by compile-FORTH.py)
                            \ 20: FIRST (set by compile-FORTH.py)
                            \ 22: LIMIT (set by compile-FORTH.py)
                            \ 24: _CR   (set by compile-FORTH.py)
                            \ 26: NL    (set by compile-FORTH.py)
                            \ 28: BSP   (set by compile-FORTH.py)
HERE 30 !                   \ 30: DP0
                            \ 32: -
                            \ 34: -
                            \ 36: -
                            \ 38: -

\ ----------------------------------------
\ Make "dangerous" primitives compile-only

' file>R >NAME DUP C@ $20 OR SWAP C!
' R>file >NAME DUP C@ $20 OR SWAP C!



\ End of FORTH.FTH
