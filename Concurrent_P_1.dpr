program Concurrent_P_1;

{$APPTYPE CONSOLE}
 {AL HARTMANN
 INFORMATION SCIENCE
 CALIFORNIA INSTITUTE OF TECHNOLOGY
 PASADENA, CALIFORNIA 91125
 PDP 11/45 CONCURRENT PASCAL
 COMPILER PASS 1: LEXICAL ANALYSIS
 OCTOBER 1974

 2023-2024 M.P. Úprava pro Delphi
 }
uses
  SysUtils, Classes;


const

  nkw= 51;             (* number of reserved words recognised *)
  TEST_MAX = 100;
  line_length=1000;
  LISTOPTION = 0;    SUMMARYOPTION = 1;  TESTOPTION = 2;     CHECKOPTION = 3;
  CODEOPTION = 4;    NUMBEROPTION = 5;
  MAXWORD = 100;
  TEXT_LENGTH = 18;
  PRINTLIMIT = 132;   MAXDIGIT = {6} 12;

  WORDLENGTH = {2}1 {BYTES};
  REALLENGTH = {8}{4} 2 {BYTES};
  SPLITLENGTH = {4} 2{WORDS PER SPLIT REAL};
  TWOWORDS = {4} 2 {BYTES};
  SETLENGTH1 = {16} 8 {BYTES};

type
OPTION = LISTOPTION..NUMBEROPTION;
TEXT_TYPE = ARRAY (.1..TEXT_LENGTH.) OF CHAR;

TABLEPTR = ^TABLE;
TABLE = RECORD
          NEXTPORTION: TABLEPTR;
          CONTENTS: ARRAY (.1..MAXWORD.) OF INTEGER
        END;

TABLEPART = RECORD
              PROGLENGTH, CODELENGTH, STACKLENGTH,
                VARLENGTH: INTEGER;
              JUMPTABLE, BLOCKTABLE, STACKTABLE,
                CONSTTABLE: TABLEPTR
            END;
TABLESPTR = ^TABLEPART;

PASSPTR = ^PASSLINK;
PASSLINK =
  RECORD
    OPTIONS: SET OF OPTION;
    LABELS, BLOCKS, CONSTANTS, RESETPOINT: INTEGER;
    TABLES: TABLESPTR
  END;

elem = integer;
rprvek =  ^prvek;
prvek = record
 element:elem;
 spoj: rprvek;
end;

 list = record
    Data: array of Integer;
    Length: Integer;
    Position: Integer;
  end;

   { a split real is used with
    an undefined tag field to
    convert a real "a" to its
    binary representation by
    two integers "b" and "c"
    (or vice versa) }
  splitreal =
    record
      case split: boolean of
        false: (a: real);
        true: (b, c: integer)
    end;

var
 source, tiskarna,ERRORS:Text;
 Nazev_Vstupniho_Souboru:string;
 err: boolean;
 OK: BOOLEAN;

 PRINTED:integer;

 TEST,DEBUG,GENERATE: boolean;
 TEST_INDEX: integer;
 TEST_BUF: ARRAY (.1..TEST_MAX.) OF INTEGER;
 INTER_PASS_PTR:PASSPTR;


outfile1, outfile2, outfile3, outfile4,outfile5,outfile6,outfile7: list;
chyby:list;



procedure Init(var L: list);
{create list}
begin
  SetLength(L.Data, 0);
  L.Length := 0;
  L.Position := -1;
end;

procedure First(var L: list);
begin
  L.Position := 0;
end;


function EMPTY(var L: list): Boolean;
begin
  Result := L.Length = 0;
end;

 function END_L(var L: list): Boolean;
begin
  Result := L.Position >= L.Length - 1;
end;


 procedure Next(var L: list);
begin
  if L.Position < L.Length - 1 then
    L.Position := L.Position + 1;
end;



procedure GET_L(var L:list; var X:integer);
begin
  if (L.Position >= 0) and (L.Position < L.Length) then
  begin
    {Result}X := L.Data[L.Position];
    Next(L);
  end
  else
  X := 0; // Or some other default value to indicate an error
end;


procedure Add(X: Integer; var L:  list );
begin
  SetLength(L.Data, L.Length + 1);
  L.Data[L.Length] := X;
  L.Length := L.Length + 1;
end;

procedure putreal(value: real;var dual: splitreal);

begin
  dual.split := false;
  dual.a := value;
  dual.split := true;
 { put(dual.b);
  put(dual.c) }
end;

procedure getreal(dual: splitreal;
  var value: real);
begin
  dual.split := true;
  value := dual.a;
 { get(dual.b);
  get(dual.c);  }
  dual.split := false;

end;

PROCEDURE STORE_TEST (ARG: INTEGER);
  BEGIN
   IF TEST_INDEX < TEST_MAX THEN BEGIN
     TEST_INDEX:= TEST_INDEX + 1;
     TEST_BUF(.TEST_INDEX.):= ARG
   END
  END;

PROCEDURE PRINT_TEST;
VAR I: INTEGER;
BEGIN
  { PRINTED:= PRINTLIMIT;  }
 if TEST_INDEX> 0 then
  begin
   FOR I:= 1 TO TEST_INDEX DO write(tiskarna, '   ', TEST_BUF(.I.));
    writeln (tiskarna);
   TEST_INDEX:= 0
  end
END;

 PROCEDURE INIT_OPTIONS;

  BEGIN
   NEW(INTER_PASS_PTR);
    WITH INTER_PASS_PTR^ DO BEGIN
      OPTIONS:=(.LISTOPTION,CHECKOPTION,NUMBEROPTION.);
      TABLES:=NIL;

    END
  END;

PROCEDURE PRINTABS(ARG: INTEGER);
VAR T: ARRAY (.1..MAXDIGIT.) OF CHAR; REM, DIGIT, I: INTEGER;
BEGIN
  REM:= ARG; DIGIT:= 0;
  REPEAT
    DIGIT:= DIGIT + 1;
    T(.DIGIT.):= CHR(ABS(REM MOD 10) + ORD('0'));
    REM:= REM DIV 10;
  UNTIL REM = 0;
  FOR I:= DIGIT DOWNTO 1 DO WRITE(tiskarna,T(.I.));
  FOR I:= DIGIT + 1 TO MAXDIGIT DO WRITE(tiskarna,' ');
END;

PROCEDURE PRINTEOL;
BEGIN WRITELN(tiskarna); PRINTED:= 0 END;

PROCEDURE PRINTFF(THIS_PASS:integer);
VAR I:INTEGER;
BEGIN
   writeln(tiskarna);
   write(tiskarna, ' STEP ', intToSTR(THIS_PASS));
   PRINTEOL
END;

PROCEDURE PRINTOP(OP: INTEGER);
BEGIN
  IF PRINTED = PRINTLIMIT THEN PRINTEOL;
  writeln(tiskarna);
  WRITE(tiskarna,'C'); PRINTABS(OP);
  PRINTED:= PRINTED + 1;
 END;

PROCEDURE PRINTARG(ARG: INTEGER);
BEGIN
  IF PRINTED = PRINTLIMIT THEN PRINTEOL;
  IF ARG < 0 THEN WRITE(tiskarna, '-') ELSE WRITE(tiskarna,' ');
  PRINTABS(ARG);
  PRINTED:= PRINTED + 1;
 END;

  PROCEDURE PUT_E(arg1,ARG2,ARG3:INTEGER);
  BEGIN
    ADD (arg1, chyby);
    ADD (arg2, chyby);
    ADD (arg3, chyby);

  END;

procedure Pass1(VAR OK: BOOLEAN(*; VAR LINK: POINTER*));

const
 THIS_PASS=1;
 ID_PIECE_LENGTH = 9; (*TEN CHARS PER PIECE*)
 MAX_PIECES = 13;  (*FOURTEEN PIECES => 140 CHARS*)
 MAX_INDEX=700; (*MAX LOADING=0.98*HASH_MAX1-NO. OF RES.WDS.*)
 HASH_MAX=750;  (* HASH_TABLE UPPER_BOUND *)
 HASH_MAX1=751; (*PRIME LENGTH OF HASH_TABLE*)
 NULL=32767;
 SPAN=26  (* NUMBER OF DISTINCT ID CHARS *);
 MIN_ORD=0;         MAX_ORD=127;
 {EOL = '\n';     FF = '(:12:)';      EOM = '(:25:)';}
 PAGELENGTH = 256;

 MAX_STRING_LENGTH = 80 {CHARS};

 INTEGER_LIMIT= {(MAX_INTEGER-9) DIV 10} 3275;
 MAX_EXPONENT=38;
 MAX_INTEGER=32767;

 {ERRORS}

COMMENT_ERROR=1;   NUMBER_ERROR=2;     INSERT_ERROR=3;     STRING_ERROR=4;
CHAR_ERROR=5; END_OF_FILE_ERROR =6;



 LISTOPTION = 0;    SUMMARYOPTION = 1;  TESTOPTION = 2;     CHECKOPTION = 3;
 CODEOPTION = 4;    NUMBEROPTION = 5;



(* OUTPUT OPERATORS *)

EOM2=0;            BEGIN2=1;           IF2=2;              CASE2=3;
WHILE2=4;          REPEAT2=5;          FOR2=6;             CYCLE2=7;
WITH2=8;           INIT2=9;            ID2=10;             REAL2=11;
STRING2=12;        INTEGER2=13;        CHAR2=14;           OPEN2=15;
NOT2=16;           SUB2=17;            SET2=18;            ARRAY2=19;
RECORD2=20;        CLASS2=21;          MONITOR2=22;        PROCESS2=23;
PERIOD2=24;        STAR2=25;           SLASH2=26;          DIV2=27;
MOD2=28;           AND2=29;            PLUS2=30;           MINUS2=31;
OR2=32;            EQ2=33;             NE2=34;             LE2=35;
GE2=36;            LT2=37;             GT2=38;             IN2=39;
CONST2=40;         TYPE2=41;           VAR2=42;            PROCEDURE2=43;
FUNCTION2=44;      PROGRAM2=45;        SEMICOLON2=46;      CLOSE2=47;
UP_TO2=48;         OF2=49;             COMMA2=50;          BUS2=51;
COLON2=52;         END2=53;            ENTRY2=54;          UNIV2=55;
BECOMES2=56;       THEN2=57;           ELSE2=58;           DO2=59;
UNTIL2=60;         TO2=61;             DOWNTO2=62;         LCONST2=63;
MESSAGE2=64;       NEW_LINE2=65;

(* STANDARD SPELLING/NOUN INDICES*)

XUNDEF=0;          XFALSE=1;           XTRUE=2;            XINTEGER=3;
XBOOLEAN=4;        XCHAR=5;            XQUEUE=6;           XABS=7;
XATTRIBUTE=8;      XCHR=9;             XCONTINUE=10;       XCONV=11;
XDELAY=12;         XEMPTY=13;          XIO=14;             XORD=15;
XPRED=16;          XSTOP=17;           XREALTIME=18;       XSETHEAP=19;
XSUCC=20;          XTRUNC=21;          XSTART=22;          XWAIT=23;
XREAL=24;

type
 PAGE = ARRAY (.1..PAGELENGTH.) OF INTEGER;

 PIECE=ARRAY(.0..ID_PIECE_LENGTH.) OF CHAR;

 SPELLING_INDEX = integer;

 PIECE_PTR=^ID_PIECE;

  ID_PIECE=
    RECORD
      PART:PIECE;
      NEXT:PIECE_PTR
    END;

 PASSPTR = ^PASSLINK;
 OPTION = LISTOPTION..NUMBEROPTION;
 PASSLINK =
  RECORD
    OPTIONS: SET OF OPTION;
    LABELS, BLOCKS, CONSTANTS, RESETPOINT: INTEGER;
    TABLES: POINTER
  END;

  TREAT=(CANCEL,IGNORE);


 var

   REAL0, REAL1, REAL10, MAX_REAL, REAL_LIMIT: REAL;

   UPTO_SW, BUS_SW, END_SCAN: BOOLEAN;

   PAGES_IN, WORDS_IN: INTEGER;
   PAGES_OUT, WORDS_OUT: INTEGER;

  PIECES: INTEGER;
  CHAR_INDEX:0..ID_PIECE_LENGTH (*CURRENT CHAR INDEX*);

  SYMB: INTEGER (*ID SYMBOL*);

  STRING_LENGTH:INTEGER;
  STRING_TEXT: ARRAY (.1..MAX_STRING_LENGTH.) OF CHAR;

  CURRENT_INDEX  (*LAST ASSIGNED INDEX*),
  INDEX  (*LAST SCANNED INDEX*)  : SPELLING_INDEX;

  ID_TEXT: ARRAY(.0..MAX_PIECES.) OF PIECE;

  BLANK: PIECE (*BLANK PADDING*);

  LETTERS, DIGITS, ALFAMERICS, NON_ALFAS, STRING_SPECIAL:  SET OF CHAR;

  HASH_KEY: 0..HASH_MAX;  (* INDEX TO HASH_TABLE *)
  HASH_TABLE:
    ARRAY (.0..HASH_MAX.) OF
      RECORD
        SPIX:SPELLING_INDEX;
        NAME:ID_PIECE
      END;

 CH:CHAR;


  procedure GETCH(var CH:char);
  begin
  read(source, ch);
  write (tiskarna, ch);

  if eoln(source) then
  begin
   if eof(source) then
     END_SCAN:=true;;
     writeln(tiskarna);

  end;

  end;


  PROCEDURE PUT0NC(OP:INTEGER);
  BEGIN
    IF TEST THEN STORE_TEST(OP);
    ADD (OP, outfile1);

    GETCH(CH);
  END;

  PROCEDURE PUT0(OP:INTEGER);
  BEGIN
    IF TEST THEN STORE_TEST(OP);
    ADD (op, outfile1);
   END;

  PROCEDURE PUT1(OP,ARG:INTEGER);
  BEGIN
    IF TEST THEN BEGIN
      STORE_TEST(OP);  STORE_TEST(ARG)
    END;
    ADD (op, outfile1);
    ADD (arg, outfile1);
  END;

 PROCEDURE PUT2(OP,ARG1,ARG2:INTEGER);
  BEGIN
   ADD (op, outfile1);
   ADD (arg1, outfile1);
   ADD (arg2, outfile1);
    IF TEST THEN BEGIN
      PRINTOP(OP);
      PRINTARG(ARG1); PRINTARG(ARG2)
    END
  END;
  (*NOTE: A PASS RUNNING WITH TEST OUTPUT SHOULD START
 BY CALLING PROCEDURE PRINTFF*)

  PROCEDURE PUT_ARG(ARG:INTEGER);
  BEGIN
    PUT0(ARG);
    IF TEST THEN STORE_TEST(ARG)
  END;


  PROCEDURE PUT_STRING (STRING1: (*UNIV PACKED_ *)STRING; STRING_LENGTH: INTEGER);
  VAR I: INTEGER;
  BEGIN
    PUT1(STRING2, STRING_LENGTH);  PUT1(LCONST2, STRING_LENGTH);
    FOR I:= 1 TO STRING_LENGTH {DIV WORDLENGTH} DO
      PUT_ARG(ord (STRING1(.I.)))
  END;

  procedure ERROR(ERROR_NUM:integer);
  begin
   PUT2(MESSAGE2,THIS_PASS,ERROR_NUM);
   WRITE(tiskarna,'***ERROR***');
   ERR:=true;
   writeln(tiskarna, IntToStr(ERROR_NUM));
   write(tiskarna,' ':4);
   end;


  PROCEDURE GET_CHAR(SKIP_FIRST:BOOLEAN);
  BEGIN
    IF SKIP_FIRST THEN BEGIN GETCH (CH) END;
    REPEAT
    IF CH='"' THEN BEGIN
         REPEAT GETCH (CH)  UNTIL (CH='"');
      IF CH = '"' THEN BEGIN GETCH (CH) END ELSE ERROR(COMMENT_ERROR)
      END;
      WHILE CH = ' ' DO GETCH (CH);
     UNTIL (CH<>' ') AND (CH<>'"')
  END;


  PROCEDURE STD_ID(ID:PIECE; INDEX:SPELLING_INDEX);
  VAR {S:SPELLING_INDEX;} CHAR_INDEX:INTEGER;
      Y:integer;
  BEGIN
    HASH_KEY:=1;
    FOR CHAR_INDEX:=0 TO ID_PIECE_LENGTH DO
      IF ID(.CHAR_INDEX.)<>' ' THEN
       begin
        Y:= ORD(ID(.CHAR_INDEX.));
        HASH_KEY:=HASH_KEY*(Y MOD SPAN +1) MOD HASH_MAX1;
       end;
    WHILE HASH_TABLE(.HASH_KEY.).SPIX<>NULL DO
      HASH_KEY:=(HASH_KEY+1) MOD HASH_MAX1;
    (* NOW WE HAVE ENTRY SLOT *)
    WITH HASH_TABLE(.HASH_KEY.) DO BEGIN
      SPIX:=INDEX;
      WITH NAME DO BEGIN PART:=ID; NEXT:=NIL END
    END
  END;

  PROCEDURE STD_NAMES;
  BEGIN
    STD_ID('END       ',-END2);
    STD_ID('IF        ',-IF2);
    STD_ID('THEN      ',-THEN2);
    STD_ID('BEGIN     ',-BEGIN2);
    STD_ID('ELSE      ',-ELSE2);
    STD_ID('DO        ',-DO2);
    STD_ID('WITH      ',-WITH2);
    STD_ID('IN        ',-IN2);
    STD_ID('OF        ',-OF2);
    STD_ID('WHILE     ',-WHILE2);
    STD_ID('CASE      ',-CASE2);
    STD_ID('REPEAT    ',-REPEAT2);
    STD_ID('UNTIL     ',-UNTIL2);
    STD_ID('PROCEDURE ',-PROCEDURE2);
    STD_ID('VAR       ',-VAR2);
    STD_ID('FOR       ',-FOR2);
    STD_ID('ARRAY     ',-ARRAY2);
    STD_ID('RECORD    ',-RECORD2);
    STD_ID('SET       ',-SET2);
    STD_ID('TO        ',-TO2);
    STD_ID('DOWNTO    ',-DOWNTO2);
    STD_ID('MOD       ',-MOD2);
    STD_ID('OR        ',-OR2);
    STD_ID('AND       ',-AND2);
    STD_ID('NOT       ',-NOT2);
    STD_ID('DIV       ',-DIV2);
    STD_ID('CONST     ',-CONST2);
    STD_ID('TYPE      ',-TYPE2);
    STD_ID('FUNCTION  ',-FUNCTION2);
    STD_ID('PROGRAM   ',-PROGRAM2);
    STD_ID('CLASS     ',-CLASS2);
    STD_ID('CYCLE     ',-CYCLE2);
    STD_ID('ENTRY     ',-ENTRY2);
    STD_ID('INIT      ',-INIT2);
    STD_ID('MONITOR   ',-MONITOR2);
    STD_ID('PROCESS   ',-PROCESS2);
    STD_ID('UNIV      ',-UNIV2);
    STD_ID('FALSE     ',XFALSE);
    STD_ID('TRUE      ',XTRUE);
    STD_ID('INTEGER   ',XINTEGER);
    STD_ID('BOOLEAN   ',XBOOLEAN);
    STD_ID('CHAR      ',XCHAR);
    STD_ID('QUEUE     ',XQUEUE);
    STD_ID('ABS       ',XABS);
    STD_ID('ATTRIBUTE ',XATTRIBUTE);
    STD_ID('CHR       ',XCHR);
    STD_ID('CONTINUE  ',XCONTINUE);
    STD_ID('CONV      ',XCONV);
    STD_ID('DELAY     ',XDELAY);
    STD_ID('EMPTY     ',XEMPTY);
    STD_ID('IO        ',XIO);
    STD_ID('ORD       ',XORD);
    STD_ID('PRED      ',XPRED);
    STD_ID('STOP      ',XSTOP);
    STD_ID('REALTIME  ',XREALTIME);
    STD_ID('SETHEAP   ',XSETHEAP);
    STD_ID('SUCC      ',XSUCC);
    STD_ID('TRUNC     ',XTRUNC);
    STD_ID('START     ',XSTART);
    STD_ID('WAIT      ',XWAIT);
    STD_ID('REAL      ',XREAL);
  END;

PROCEDURE INIT_PASS (*(VAR LINK: PASSPTR)*);
BEGIN
 (*  LINK:= PARAM(.2.).PTR; *)
  OK:= TRUE;
  PAGES_IN:= 1; WORDS_IN:= PAGELENGTH;
  PAGES_OUT:= 1; WORDS_OUT:= 0
END;

 procedure Initialize;
 VAR S:SPELLING_INDEX; C:MIN_ORD..MAX_ORD;
 i:integer;
 begin
  TEST:=true;
  IF TEST THEN PRINTFF(THIS_PASS);
  err:=false;
  INIT(outfile1);
  init(chyby);
  test_index:=0;
  UPTO_SW:=FALSE; BUS_SW:=FALSE;
  REAL0:= 0;  REAL1:= 1;  REAL10:= 10;
  MAX_REAL:= 3.40e+38;
  {LARGEST_REAL(MAX_REAL);}
  REAL_LIMIT:= {3.40e+38;} MAX_REAL / REAL10;
  BLANK:='          ';
  DIGITS:=(.'0','1','2','3','4','5','6','7','8','9'.);
  LETTERS:=(.'A','B','C','D','E','F','G','H','I','J','K','L',
      'M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','_',
      'a','b','c','d','e','f','g','h','i','j','k','l','m','n',
      'o','p','q','r','s','t','u','v','w','y','z'.);
  ALFAMERICS:=LETTERS + DIGITS;
  STRING_SPECIAL:= (.'''', '('.);
  NON_ALFAS:= (..);
  FOR C:= MIN_ORD TO MAX_ORD DO NON_ALFAS:= NON_ALFAS + (.CHR(C).);
  NON_ALFAS:= NON_ALFAS - ALFAMERICS;
  END_SCAN:=false;
  BUS_SW:=FALSE;
  FOR S:=0 TO HASH_MAX DO HASH_TABLE(.S.).SPIX:=NULL;
  CURRENT_INDEX:= XREAL;
  STD_NAMES;
 { with g do
   begin
    for i:=1 to line_length do
     line[i]:=' ';
    cc:=0;
    ll:=0;
   end;   }
 end;

  PROCEDURE NUMBER;
  VAR MANTISSA, POWER_OF_TEN, RESULT: REAL;
    ERROR_SW,EXPONENT_SIGN:BOOLEAN;
    REAL_VAL:SPLITREAL; OP:INTEGER;
    EXPONENT,EXPONENT_PART,I:INTEGER;
    X:integer;
  BEGIN
    OP:= INTEGER2;
    MANTISSA:= REAL0;
    ERROR_SW:= FALSE;
    EXPONENT:= 0;
   {COLLECT INTEGER PART}
    REPEAT
      IF MANTISSA<=REAL_LIMIT THEN
       begin
        X:=  (ORD(CH)-ORD('0'));
        MANTISSA:=MANTISSA*REAL10+{CONV} X;
       end
      ELSE ERROR_SW:=TRUE;
      Getch(CH);
    UNTIL NOT(CH IN DIGITS);
    {COLLECT FRACTIONAL PART}
    IF CH='.' THEN BEGIN
      Getch(CH);
      IF CH=')' THEN BUS_SW:=TRUE ELSE
      IF CH='.' THEN UPTO_SW:=TRUE ELSE
      BEGIN
        OP:=REAL2;
        IF NOT(CH IN DIGITS) THEN ERROR(NUMBER_ERROR) ELSE
        REPEAT
          IF MANTISSA <= REAL_LIMIT THEN BEGIN
            MANTISSA:=MANTISSA*REAL10+{CONV}(ORD(CH)-ORD('0'));
            EXPONENT:=EXPONENT-1
          END;
         Getch(CH)
        UNTIL NOT(CH IN DIGITS);
      END
    END;
    {COLLECT EXPONENT PART}
    IF CH='E' THEN BEGIN
      OP:=REAL2;
      Getch(CH);
      EXPONENT_PART:=0; EXPONENT_SIGN:=FALSE;
      IF CH='+' THEN BEGIN  Getch(CH) END ELSE
      IF CH='-' THEN BEGIN
        EXPONENT_SIGN:= TRUE;  Getch(CH);
      END;
      IF NOT(CH IN DIGITS) THEN ERROR(NUMBER_ERROR) ELSE
      REPEAT
        IF EXPONENT_PART<=INTEGER_LIMIT THEN
          EXPONENT_PART:=EXPONENT_PART*10-ORD('0') +ORD(CH)
        ELSE ERROR_SW:=TRUE;
         Getch(CH)
      UNTIL NOT(CH IN DIGITS);
      {ASSERT EXPONENT <= 0;}
      IF EXPONENT_SIGN THEN
        IF MAX_EXPONENT + EXPONENT >= EXPONENT_PART
        THEN EXPONENT:= EXPONENT - EXPONENT_PART
        ELSE ERROR_SW:= TRUE
        ELSE EXPONENT:=EXPONENT+EXPONENT_PART
    END;
    {NOW CONSTRUCT THE NUMBER}
    IF OP=INTEGER2 THEN BEGIN
      IF MANTISSA>{CONV}(MAX_INTEGER) THEN BEGIN
        ERROR(NUMBER_ERROR); MANTISSA:= REAL0
      END;
      PUT1(INTEGER2,TRUNC(MANTISSA))
    END ELSE {OP=REAL2} BEGIN
      IF ERROR_SW THEN BEGIN
        ERROR(NUMBER_ERROR);
       { SPLIT(REAL0, REAL_VAL)  }
        putreal(REAL0, REAL_VAL);
      END ELSE BEGIN
        {COMPUTE THE APPROPRIATE POWER OF TEN}
        POWER_OF_TEN:=REAL1;
        IF EXPONENT<0 THEN BEGIN
          EXPONENT_SIGN:=TRUE;
          EXPONENT:=ABS(EXPONENT)
        END ELSE EXPONENT_SIGN:=FALSE;
        IF EXPONENT>MAX_EXPONENT THEN BEGIN
          ERROR(NUMBER_ERROR);
          EXPONENT:=0
        END;
        FOR I:=1 TO EXPONENT DO POWER_OF_TEN:=POWER_OF_TEN*REAL10;
        {NOW EITHER MANTISSA=0.0 OR MANTISSA>=1.0}
        IF MANTISSA = REAL0 THEN RESULT:= REAL0 ELSE
        IF EXPONENT_SIGN THEN RESULT:= MANTISSA / POWER_OF_TEN ELSE
        {IF MANTISSA>=1.0 THEN WE MUST HAVE:
          MANTISSA*POWER_OF_TEN<=MAX_REAL
          => POWER_OF_TEN<=MAX_REAL/MANTISSA<=MAX_REAL}
        IF POWER_OF_TEN < MAX_REAL / MANTISSA THEN
          RESULT:= MANTISSA * POWER_OF_TEN
        ELSE BEGIN
          ERROR(NUMBER_ERROR); RESULT:= REAL0
        END;
        {SPLIT(RESULT, REAL_VAL); }
        {procedure putreal(value: real;var dual: splitreal);}
       putreal(RESULT, REAL_VAL);
      END;
      PUT0(REAL2);
      {PUT1(LCONST2,REALLENGTH); }
      PUT1(LCONST2,SPLITLENGTH);
      {FOR I:= 1 TO SPLITLENGTH DO PUT_ARG(REAL_VAL(.I.)) }
      PUT_ARG(REAL_VAL.b);
      PUT_ARG(REAL_VAL.c);
    END
  END;

 procedure skipblank;
  begin
   while (ch= ' ') do
   GETCH(CH);
 end;

  PROCEDURE INSERT_ID;
  VAR I:INTEGER; P,P1:PIECE_PTR;
  BEGIN
    WITH HASH_TABLE(.HASH_KEY.) DO BEGIN
      CURRENT_INDEX:=CURRENT_INDEX+1;
      IF CURRENT_INDEX>=MAX_INDEX THEN BEGIN
       ERROR(INSERT_ERROR); {CH:=EOM; WRITE(EOL)*}
      END;
      SPIX:=CURRENT_INDEX;
      WITH NAME DO BEGIN PART:=ID_TEXT(.0.); NEXT:=NIL END;
      IF PIECES>0 THEN BEGIN
        NEW(P); NAME.NEXT:=P; P^.PART:=ID_TEXT(.1.);
        FOR I:=2 TO PIECES DO BEGIN
          NEW(P1); P^.NEXT:=P1;
          P1^.PART:=ID_TEXT(.I.); P:=P1
        END;
        P^.NEXT:=NIL
      END
    END
  END;

 FUNCTION SAME_ID:BOOLEAN;
  VAR SAME:BOOLEAN; THIS_PIECE:PIECE_PTR; I:INTEGER;
  BEGIN
    WITH HASH_TABLE(.HASH_KEY.) DO BEGIN
      SAME:=NAME.PART=ID_TEXT(.0.);
      IF PIECES>0 THEN
        IF SAME THEN BEGIN
          THIS_PIECE:=NAME.NEXT;
          I:=1;
          REPEAT
            IF THIS_PIECE=NIL THEN BEGIN
              SAME:=FALSE (*CANDIDATE IS TOO SHORT*);
              I:=PIECES+1 (* QUIT*)
            END ELSE BEGIN (*COMPARE AND INCREMENT*)
              SAME:=SAME AND (THIS_PIECE^.PART=ID_TEXT(.I.));
              THIS_PIECE:=THIS_PIECE^.NEXT;
              I:=I+1;
            END
          UNTIL I>PIECES;
          SAME:=SAME AND (THIS_PIECE=NIL)
        END;
      SAME_ID:=SAME
    END
  END;

PROCEDURE SEARCH_ID;
  VAR FINISHED:BOOLEAN;
  BEGIN
    FINISHED:=FALSE;
    REPEAT
      WITH HASH_TABLE(.HASH_KEY.) DO
        IF SPIX<>NULL THEN
            IF SAME_ID THEN (*FOUND IT*) BEGIN
              FINISHED:=TRUE;
              IF SPIX>=0 THEN BEGIN
                SYMB:=ID2; INDEX:=SPIX
              END ELSE SYMB:=ABS(SPIX)
            END ELSE HASH_KEY:=(HASH_KEY+1) MOD HASH_MAX1
        ELSE (*SYM=NULL*) BEGIN
          INSERT_ID;
          SYMB:=ID2;
          INDEX:=CURRENT_INDEX;
          FINISHED:=TRUE
        END
    UNTIL FINISHED (*WITH SEARCH*)
  END;

  (* STRING *)

  PROCEDURE STRING_CHAR;
  BEGIN
   IF STRING_LENGTH = MAX_STRING_LENGTH THEN ERROR(STRING_ERROR)
   ELSE BEGIN
    STRING_LENGTH:=STRING_LENGTH+1;
    STRING_TEXT(.STRING_LENGTH.):= CH;
      GETCH (CH);
   END
  END;


 PROCEDURE SSTRING;
  VAR ORD_VALUE: INTEGER; DONE: BOOLEAN;
  BEGIN
    STRING_LENGTH:= 0;
    GETCH (CH); DONE:= FALSE;
    REPEAT
      WHILE NOT (CH IN STRING_SPECIAL) DO STRING_CHAR;
      CASE CH OF
        '''':
          BEGIN
            STRING_CHAR;
            IF CH = '''' THEN BEGIN GETCH (CH) END ELSE DONE:= TRUE
          END;
        '(':
          BEGIN
            STRING_CHAR;
            IF CH = ':' THEN BEGIN
            REPEAT GETCH(CH) UNTIL CH <> ' ';
              ORD_VALUE:= 0;
              IF CH IN DIGITS THEN
                REPEAT
                  IF ORD_VALUE <= MAX_ORD THEN
                    ORD_VALUE:= ORD_VALUE * 10 + (ORD(CH) - ORD('0'));
                GETCH (CH);
                UNTIL NOT (CH IN DIGITS)
              ELSE ERROR(STRING_ERROR);
          WHILE CH=' ' DO BEGIN GETCH (CH) END;
          IF CH=':' THEN BEGIN GETCH (CH) END ELSE ERROR(STRING_ERROR);
          IF CH=')' THEN BEGIN GETCH (CH) END ELSE ERROR(STRING_ERROR);
              IF ORD_VALUE > MAX_ORD THEN BEGIN
                ERROR(STRING_ERROR);
                ORD_VALUE:= ORD('?')
              END;
              STRING_TEXT(.STRING_LENGTH.):= CHR(ORD_VALUE)
            END
          END
      END
    UNTIL DONE;
    IF STRING_LENGTH <= 1 THEN BEGIN
      ERROR(STRING_ERROR);
      STRING_LENGTH:= 1;  STRING_TEXT(.1.):= '?'
    END ELSE STRING_LENGTH:= STRING_LENGTH - 1;
    IF STRING_LENGTH > 1 THEN IF STRING_LENGTH MOD WORDLENGTH <> 0 THEN
      BEGIN ERROR(STRING_ERROR); STRING_LENGTH:= 1 END;
    IF STRING_LENGTH = 1 THEN PUT1(CHAR2, ORD(STRING_TEXT(.1.)))
    ELSE PUT_STRING(STRING_TEXT, STRING_LENGTH)
  END;

PROCEDURE IDENTIFIER;
 var Y:integer;
  BEGIN
    PIECES:=-1;
    CHAR_INDEX:=ID_PIECE_LENGTH;
    HASH_KEY:= 1;
    REPEAT
      IF CHAR_INDEX=ID_PIECE_LENGTH THEN BEGIN
        CHAR_INDEX:= 0;  PIECES:= SUCC(PIECES);
        ID_TEXT(.PIECES.):=BLANK;
      END ELSE CHAR_INDEX:= SUCC(CHAR_INDEX);
      CH:=UpCASE(CH);
      ID_TEXT(.PIECES,CHAR_INDEX.):=CH;
      Y:= ORD(CH);
      HASH_KEY:=HASH_KEY*(Y MOD SPAN +1) MOD HASH_MAX1;
      GETCH(CH);
    UNTIL CH IN NON_ALFAS;
    SEARCH_ID;
    IF SYMB=ID2 THEN PUT1(ID2,INDEX)
    ELSE BEGIN
      PUT0(SYMB);
      IF SYMB=END2 THEN BEGIN
        GET_CHAR(FALSE);
        IF CH='.' THEN BEGIN
          PUT0(PERIOD2);
         END_SCAN:=TRUE
        END
      END
    END
  END;

procedure scan;
begin
  GETCH(CH);
  REPEAT
    CASE CH OF
      ' ':   while CH=' ' do GETCH(CH)
            (*skipblank*);
       '"': BEGIN
             (* comment *)
             repeat
               GETCH(CH);
              until (CH =   '"');
             GETCH(CH);
        END;
        
       '.': BEGIN
              (*SYM:=PERSYM;   *)
               GETCH(CH);
               IF UPTO_SW THEN BEGIN
                PUT0(UP_TO2);
                UPTO_SW:=FALSE
               END ELSE IF CH='.' THEN PUT0NC(UP_TO2)
              ELSE IF CH=')' THEN PUT0NC(BUS2)
              ELSE PUT0(PERIOD2)
             END;
        ':':BEGIN
              GETCH(CH);
              IF CH='='
               THEN PUT0NC(BECOMES2)
               ELSE PUT0(COLON2)
             END;
         '<':BEGIN
             GETCH(CH);
            IF CH='=' THEN PUT0NC(LE2) ELSE
            IF CH='>' THEN PUT0NC(NE2) ELSE
              PUT0(LT2)
             (* LT2 *)
            END;
        '=':
            PUT0NC(EQ2);
       '>': BEGIN
            GETCH(CH);
            IF CH='=' THEN PUT0NC(GE2) ELSE PUT0(GT2)
            END;
        '''':   SSTRING;
         (* STRING *)

         '0','1','2','3','4','5','6','7','8','9':
            NUMBER;

         'A','B','C','D','E','F','G','H','I','J','K','L','M','N',
        'O','P','Q','R','S','T','U','V','W','X','Y','Z','_',
         'a','b','c','d','e','f','g','h','i','j','k','l','m','n',
        'o','p','q','r','s','t','u','v','w','y','z':
          IDENTIFIER;
        '(': BEGIN
            GETCH(CH);
            IF CH='.' THEN  PUT0NC(SUB2) ELSE PUT0(OPEN2)
            END;
         ')':
          IF BUS_SW THEN BEGIN
            PUT0NC(BUS2);
            BUS_SW:=FALSE
          END ELSE PUT0NC(CLOSE2);

        ',':
           PUT0NC(COMMA2);

        ';':
           PUT0NC(SEMICOLON2);

        '*':
           PUT0NC(STAR2);

        '/':
           PUT0NC(SLASH2);

        '+':
          PUT0NC(PLUS2);

        '-':
          PUT0NC(MINUS2);

        '&':
          PUT0NC(AND2);
      else begin
        (*SYM:=NULSYM;*)
        GETCH(CH);
      end;
    end;

 UNTIL END_SCAN;
 PUT0(EOM2)

end;

procedure vypis;
var i:integer;

begin
 writeln(tiskarna);
 for i := 1 to TEST_INDEX do
  writeln (tiskarna, TEST_BUF[i]);
end;


{MAIN PROGRAM PASS 1}
begin
 INIT_PASS;
 Initialize;
 scan;
 vypis;
 OK:=not(ERR);
end;

procedure Pass2(var OK);

CONST
 EOL = '(:10:)';
 MAXARG = 10;
 IDLENGTH = 12;
 max_file_length=1000;

{INPUT OPERATORS}

EOM1=0;            BEGIN1=1;           IF1=2;              CASE1=3;
WHILE1=4;          REPEAT1=5;          FOR1=6;             CYCLE1=7;
WITH1=8;           INIT1=9;            ID1=10;             REAL1=11;
STRING1=12;        INTEGER1=13;        CHAR1=14;           OPEN1=15;
NOT1=16;           SUB1=17;            SET1=18;            ARRAY1=19;
RECORD1=20;        CLASS1=21;          MONITOR1=22;        PROCESS1=23;
PERIOD1=24;        STAR1=25;           SLASH1=26;          DIV1=27;
MOD1=28;           AND1=29;            PLUS1=30;           MINUS1=31;
OR1=32;            EQ1=33;             NE1=34;             LE1=35;
GE1=36;            LT1=37;             GT1=38;             IN1=39;
CONST1=40;         TYPE1=41;           VAR1=42;            PROCEDURE1=43;
FUNCTION1=44;      PROGRAM1=45;        SEMICOLON1=46;      CLOSE1=47;
UP_TO1=48;         OF1=49;             COMMA1=50;          BUS1=51;
COLON1=52;         END1=53;            ENTRY1=54;          UNIV1=55;
BECOMES1=56;       THEN1=57;           ELSE1=58;           DO1=59;
UNTIL1=60;         TO1=61;             DOWNTO1=62;         LCONST1=63;
MESSAGE1=64;       NEW_LINE1=65;

{OUTPUT OPERATORS}

EOM2=1;            CONST_ID2=2;        CONST_DEF2=3;       TYPE_ID2=4;
TYPE_DEF2=5;       VAR_ID2=6;          VAR_LIST2=7;        VARE_LIST2=8;
INITS_DEF2=9;      INITS_END2=10;      PROC_ID2=11;        PROC_DEF2=12;
PROCE_DEF2=13;     PROC_END2=14;       PROCE_END2=15;      FUNC_ID2=16;
FUNC_DEF2=17;      FUNCE_DEF2=18;      FUNC_END2=19;       FUNCE_END2=20;
PROG_ID2=21;       PROG_DEF2=22;       INTF_ID2=23;        TYPE2=24;
ENUM2=25;          ENUM_ID2=26;        ENUM_DEF2=27;       SUBR_DEF2=28;
SET_DEF2=29;       ARRAY_DEF2=30;      REC2=31;            FIELD_ID2=32;
FIELDLIST2=33;     REC_DEF2=34;        CLASS2=35;          MONITOR2=36;
PROCESS2=37;       STACK2=38;          PSTART2=39;         PARM_ID2=40;
PARM_TYPE2=41;     UNIV_TYPE2=42;      CPARMLIST2=43;      VPARMLIST2=44;
BODY2=45;          BODY_END2=46;       ANAME2=47;          STORE2=48;
CALL_NAME2=49;     CALL2=50;           ARG_LIST2=51;       ARG2=52;
FALSEJUMP2=53;     DEF_LABEL2=54;      JUMP_DEF2=55;       INTF2=56;
DEF_CASE2=57;      CASE2=58;           JUMP2=59;           END_CASE2=60;
ADDRESS2=61;       FOR_STORE2=62;      FOR_LIM2=63;        FOR_UP2=64;
FOR_DOWN2=65;      WITH_VAR2=66;       WITH_TEMP2=67;      WITH2=68;
INIT_NAME2=69;     INIT2=70;           VALUE2=71;          LT2=72;
EQ2=73;            GT2=74;             LE2=75;             NE2=76;
GE2=77;            IN2=78;             UPLUS2=79;          UMINUS2=80;
PLUS2=81;          MINUS2=82;          OR2=83;             STAR2=84;
SLASH2=85;         DIV2=86;            MOD2=87;            AND2=88;
FNAME2=89;         NOT2=90;            EMPTY_SET2=91;      INCLUDE2=92;
FUNCTION2=93;      CALL_FUNC2=94;      NAME2=95;           COMP2=96;
SUB2=97;           ARROW2=98;          CONSTANT2=99;       REAL2=100;
FREAL2=101;        INTEGER2=102;       FINTEGER2=103;      CHAR2=104;
FCHAR2=105;        STRING2=106;        FSTRING2=107;       NEW_LINE2=108;
LCONST2=109;       MESSAGE2=110;       PROCE_ID2=111;      FUNCE_ID2=112;
PEND2=113;         CASE_JUMP2=114;

{OTHER CONSTANTS}


THIS_PASS = 2;     SPELLING_MAX = 700;
COMP_BLOCK=TRUE;   ROUTINE_BLOCK=FALSE;

LISTOPTION = 0;    SUMMARYOPTION = 1;  TESTOPTION = 2;     CHECKOPTION = 3;
CODEOPTION = 4;    NUMBEROPTION = 5;

{MODES}

CLASS_MODE=1;      MONITOR_MODE=2;     PROCESS_MODE=3;     PROC_MODE=4;
PROCE_MODE=5;      FUNC_MODE=6;        FUNCE_MODE=7;       PROGRAM_MODE=8;

{ERRORS}

PROG_ERROR=1;      DEC_ERROR=2;        CONSTDEF_ERROR=3;   TYPEDEF_ERROR=4;
TYPE_ERROR=5;      ENUM_ERROR=6;       SUBR_ERROR=7;       SET_ERROR=8;
ARRAY_ERROR=9;     RECORD_ERROR=10;    STACK_ERROR=11;     VAR_ERROR=12;
ROUTINE_ERROR=13;  PROC_ERROR=14;      FUNC_ERROR=15;      WITH_ERROR=16;
PARM_ERROR=17;     BODY_ERROR=18;      STATS_ERROR=19;     STAT_ERROR=20;
IDSTAT_ERROR=21;   ARG_ERROR=22;       COMP_ERROR=23;      IF_ERROR=24;
CASE_ERROR=25;     LABEL_ERROR=26;     WHILE_ERROR=27;     REPEAT_ERROR=28;
FOR_ERROR=29;      CYCLE_ERROR=30;     EXPR_ERROR=31;      VARIABLE_ERROR=32;
CONSTANT_ERROR=33; INIT_ERROR=34;      MPROG_ERROR=35;

{STANDARD SPELLING/NOUN INDICES}

XUNDEF=0;          XFALSE=1;           XTRUE=2;            XINTEGER=3;
XBOOLEAN=4;        XCHAR=5;            XQUEUE=6;           XABS=7;
XATTRIBUTE=8;      XCHR=9;             XCONTINUE=10;       XCONV=11;
XDELAY=12;         XEMPTY=13;          XIO=14;             XORD=15;
XPRED=16;          XSTOP=17;           XREALTIME=18;       XSETHEAP=19;
XSUCC=20;          XTRUNC=21;          XSTART=22;          XWAIT=23;
XREAL=24;

TYPE

IDENTIFIER = ARRAY (.1..IDLENGTH.) OF CHAR;

POINTER = ^INTEGER;
OPTION = LISTOPTION..NUMBEROPTION;
PASSPTR = ^PASSLINK;
PASSLINK =
  RECORD
    OPTIONS: SET OF OPTION;
    LABELS, BLOCKS, CONSTANTS, RESETPOINT: INTEGER;
    TABLES: POINTER
  END;

ARGTAG =
  (NILTYPE, BOOLTYPE, INTTYPE, IDTYPE, PTRTYPE);

ARGTYPE = RECORD
                 CASE TAG: ARGTAG OF
                   NILTYPE, BOOLTYPE: (BOOL: BOOLEAN);
                   INTTYPE: (INT: INTEGER);
                   IDTYPE: (ID: IDENTIFIER);
                   PTRTYPE: (PTR: PASSPTR)
               END;

  ARGLIST = ARRAY (.1..MAXARG.) OF ARGTYPE;

  SPELLING_INDEX=0..SPELLING_MAX;

  LABEL1= INTEGER;

  SYMBOL=EOM1..NEW_LINE1;

  SETS=SET OF SYMBOL;

   tuples=record
    oper:integer;
    arg1,arg2,arg3:integer;
   end;

 var
  SY:integer{Symbol };
  ARG:INTEGER;
  CURRENT_LABEL:LABEL1;

{KEY SETS}

QIGNORE,           QOPEN,              QCLOSE,             QEOM,
QEND,              QSEMICOLON,         QBODY,              QID,
QDEFINITIONS,      QROUTINES,          QDECLARATIONS,      QDEF,
QDEC,              QCONSTANT,          QCONST_DEF,         QTYPE,
QTYPE_DEF,         QSUBR_LIMIT,        QDIMENSION,         QOF_TYPE,
QVAR_DEF,          QBLOCK,             QPARM_END,          QID_LIST,
QPROC_END,         QPROC_PARMS,        QFUNC_END,          QFUNC_TYPE,
QPROG_END,         QINTERFACE,         QPARM_LIST,         QSTAT,
QBODY_END,         QENTRY,             QSTAT_LIST,         QID_END,
QARGUMENT,         QARG_END,           QIF_END,            QTHEN_END,
QCASES,            QCASE_END,          QLABEL_LIST,        QDO_TAIL,
QUNARY,            QFACTOR,            QEXPR,              QUNTIL_TAIL,
QFOR_END,          QFORB_END,          QEXPR_OP,           QSEXPR_OP,
QTERM_OP,          QTERM_LIST,         QFACTOR_LIST,       QSET_EXPR,
QSELECT,           QSUB_END,           QARG,               QCOMMA,
QVARE_DEF,         QTYPE_LIST,         QWITH_LIST,         QINIT_LIST,
QTO_TAIL,          QSTACK:             SETS;


  PROCEDURE PUT_ARG(ARG:INTEGER);
  BEGIN
    ADD (arg, outfile2);
    IF TEST THEN PRINTARG(ARG)
  END;

  PROCEDURE PUT0(OP:INTEGER);
  BEGIN
    ADD (op, outfile2);
    IF TEST THEN PRINTOP(OP)
  END;

  PROCEDURE PUT1(OP,ARG:INTEGER);
  BEGIN
    ADD (op, outfile2);
    ADD (arg, outfile2);
    IF TEST THEN BEGIN
      PRINTOP(OP); PRINTARG(ARG)
    END
  END;

  PROCEDURE PUT2(OP,ARG1,ARG2:INTEGER);
  BEGIN
    ADD (op, outfile2);
    ADD (arg1, outfile2);
    ADD (arg2, outfile2);
    IF TEST THEN BEGIN
      PRINTOP(OP);
      PRINTARG(ARG1); PRINTARG(ARG2)
    END
  END;

  PROCEDURE PUT3(OP,ARG1,ARG2,ARG3:INTEGER);
  BEGIN
    PUT2(OP,ARG1,ARG2);
    PUT_ARG(ARG3);
   END;


  PROCEDURE GET;
  VAR LENGTH,I,VAL,PASS_NO,MESSAGE_NO,LINE_NO:INTEGER;
    DONE:BOOLEAN;
    LLENGTH:integer;
  BEGIN
    DONE:=FALSE;
    REPEAT
      GET_L (outfile1,SY);
      IF SY IN QIGNORE THEN
        CASE SY OF
          LCONST1: BEGIN
             GET_L (outfile1,LENGTH);
             PUT1(LCONST2,LENGTH);
             LLENGTH:= LENGTH;
            FOR I:=1 TO LLENGTH DO BEGIN
             GET_L (outfile1,VAL);
             PUT_ARG(VAL)
            END
          END;
          MESSAGE1: BEGIN
            GET_L (outfile1,PASS_NO);
            GET_L (outfile1,MESSAGE_NO);
            PUT2(MESSAGE2,PASS_NO,MESSAGE_NO)
          END;
          NEW_LINE1: BEGIN
          GET_L (outfile1,LINE_NO);
          PUT1(NEW_LINE2,LINE_NO)
          END
        END
      ELSE DONE:=TRUE
    UNTIL DONE;
    IF SY IN QARG THEN
     GET_L (outfile1,ARG)
  END;


   {PARSING ROUTINES}


  PROCEDURE PROGRAM_; FORWARD;
  PROCEDURE BLOCK(KEYS:SETS; IN_COMPONENT:BOOLEAN); FORWARD;
  PROCEDURE DECLARATIONS(KEYS:SETS); FORWARD;
  PROCEDURE CONST_DEC(KEYS:SETS); FORWARD;
  PROCEDURE TYPE_DEC(KEYS:SETS); FORWARD;
  PROCEDURE TYPE_(KEYS:SETS); FORWARD;
  PROCEDURE ENUM_TYPE(KEYS:SETS); FORWARD;
  PROCEDURE SUBR_TYPE(KEYS:SETS); FORWARD;
  PROCEDURE SET_TYPE(KEYS:SETS);  FORWARD;
  PROCEDURE ARRAY_TYPE(KEYS:SETS); FORWARD;
  PROCEDURE RECORD_TYPE(KEYS:SETS);  FORWARD;
  PROCEDURE COMP_TYPE(KEYS:SETS);  FORWARD;
  PROCEDURE VAR_DEC(KEYS:SETS);  FORWARD;
  PROCEDURE ID_LIST(KEYS:SETS; OP,ERROR_NUM:INTEGER; VAR ID_COUNT:INTEGER);
  FORWARD;
  PROCEDURE IDENTIFIER_S(KEYS:SETS; OP,ERROR_NUM:INTEGER);  FORWARD;
  PROCEDURE ROUTINE_DEC(KEYS:SETS);  FORWARD;
  PROCEDURE PROC_DEC(KEYS:SETS); FORWARD;
  PROCEDURE FUNC_DEC(KEYS:SETS);  FORWARD;
  PROCEDURE PROG_DEC(KEYS:SETS);  FORWARD;
  PROCEDURE PARM_LIST(KEYS:SETS; MODE:INTEGER);  FORWARD;
  PROCEDURE BODY(KEYS:SETS);  FORWARD;
  PROCEDURE STAT_LIST (KEYS:SETS);           FORWARD;
  PROCEDURE STAT(KEYS:SETS);  FORWARD;
  PROCEDURE ID_STAT(KEYS:SETS);  FORWARD;
  PROCEDURE ARG_LIST(KEYS:SETS);  FORWARD;
  PROCEDURE COMPOUND_STAT(KEYS:SETS); FORWARD;
  PROCEDURE IF_STAT(KEYS:SETS);  FORWARD;
  PROCEDURE CASE_STAT(KEYS:SETS);  FORWARD;
  PROCEDURE LABEL_LIST(KEYS:SETS);  FORWARD;
  PROCEDURE WHILE_STAT(KEYS:SETS);  FORWARD;
  PROCEDURE REPEAT_STAT(KEYS:SETS);  FORWARD;
  PROCEDURE FOR_STAT(KEYS:SETS);  FORWARD;
  PROCEDURE CYCLE_STAT(KEYS:SETS);  FORWARD;
  PROCEDURE WITH_STAT(KEYS:SETS);  FORWARD;
  PROCEDURE INIT_STAT(KEYS:SETS);  FORWARD;
  PROCEDURE EXPR(KEYS:SETS);  FORWARD;
  PROCEDURE SEXPR(KEYS:SETS);  FORWARD;
  PROCEDURE TERM(KEYS:SETS); FORWARD;
  PROCEDURE FACTOR(KEYS:SETS); FORWARD;
  PROCEDURE FACTOR_ID(KEYS:SETS);  FORWARD;
  PROCEDURE VARIABLE(KEYS:SETS);  FORWARD;
  PROCEDURE CONSTANT(KEYS:SETS);  FORWARD;


  {INITIALIZE}
  PROCEDURE INITIALIZE;
  BEGIN
    IF TEST THEN PRINTFF(THIS_PASS);
    Test:=true;
    FIRST(outfile1);
    INIT (outfile2);
    CURRENT_LABEL:=1; {LABEL 1 DENOTES THE BLOCK OF THE INITIAL PROCESS;
      IT IS ONLY REFERENCED BY THE FIRST JUMP INSTRUCTION IN THE PROGRAM}
    QIGNORE:=(.LCONST1,MESSAGE1,NEW_LINE1.);
    QCOMMA:=(.COMMA1.);
    QOPEN:=(.OPEN1.); QCLOSE:=(.CLOSE1.);
    QEOM:=(.EOM1.); QEND:=(.END1.);
    QSEMICOLON:=(.SEMICOLON1.);
    QBODY:=(.BEGIN1.); QID:=(.ID1.);
    QDEFINITIONS:=(.CONST1,TYPE1.);
    QROUTINES:=(.PROCEDURE1,FUNCTION1,PROGRAM1.);
    QSTACK:=(.PLUS1.);
    QENTRY:=(.ENTRY1.);
    QDECLARATIONS:=QDEFINITIONS + (.VAR1.) + QROUTINES;
    QDEF:=(.ID1,SEMICOLON1,EQ1.);
    QDEC:=(.ID1,SEMICOLON1,COLON1.);
    QCONSTANT:=(.ID1,INTEGER1,REAL1,CHAR1,STRING1.);
    QCONST_DEF:=QDEF + QCONSTANT;
    QTYPE:=(.OPEN1,SET1,ARRAY1,RECORD1,CLASS1,MONITOR1,PROCESS1.)
      + QCONSTANT;
    QTYPE_DEF:=QDEF + QTYPE;
    QTYPE_LIST:=QTYPE + QCOMMA;
    QSUBR_LIMIT:=(.UP_TO1.) + QCONSTANT;
    QDIMENSION:=QTYPE + (.COMMA1,BUS1,OF1.);
    QOF_TYPE:=QTYPE + (.OF1.);
    QVAR_DEF:=QDEC + QTYPE; QVARE_DEF:=QVAR_DEF + (.ENTRY1.);
    QBLOCK:=QDECLARATIONS + QBODY;
    QPARM_END:=QSEMICOLON + QBLOCK;
    QID_LIST:=(.ID1,COMMA1.);
    QPROC_END:=(.ENTRY1,ID1,OPEN1.) + QPARM_END;
    QARG:=(.ID1,INTEGER1,CHAR1,STRING1.);
    QPROC_PARMS:=QPROC_END-QID;
    QFUNC_END:=QPROC_END + (.COLON1.);
    QFUNC_TYPE:=QPARM_END + QID;
    QPROG_END:=QPROC_END-QBLOCK;
    QINTERFACE:=(.ENTRY1,ID1,COMMA1,SEMICOLON1.);
    QPARM_LIST:=QDEC + (.UNIV1,VAR1.);
    QSTAT:=(.ID1,BEGIN1,IF1,CASE1,WHILE1,REPEAT1,FOR1,
      CYCLE1,WITH1,INIT1.);
    QBODY_END:=QSTAT + QEND;
    QSTAT_LIST :=QSTAT + QSEMICOLON;
    QID_END:=(.BECOMES1,OPEN1.);
    QINIT_LIST:=(.ID1,OPEN1,COMMA1.);
    QIF_END:=(.THEN1,ELSE1.) + QSTAT;
    QTHEN_END:=QIF_END-(.THEN1.);
    QCASES:=QCONSTANT + QSTAT + (.COLON1,COMMA1,SEMICOLON1.);
    QCASE_END:=QCASES + (.OF1,END1.);
    QLABEL_LIST:=QCONSTANT + QCOMMA;
    QDO_TAIL:=QSTAT + (.DO1.);
    QUNARY:=(.PLUS1,MINUS1.);
    QFACTOR:=QCONSTANT + (.OPEN1,NOT1,SUB1.);
    QEXPR:=QUNARY + QFACTOR;
    QARGUMENT:=QEXPR + QCOMMA;
    QARG_END:=QARGUMENT + QCLOSE;
    QUNTIL_TAIL:=QEXPR + (.UNTIL1.);
    QFOR_END:=QEXPR + QSTAT + (.BECOMES1,TO1,DOWNTO1,DO1.);
    QFORB_END:=QFOR_END-(.BECOMES1.);
    QEXPR_OP:=(.EQ1,NE1,LE1,GE1,LT1,GT1,IN1.);
    QSEXPR_OP:=(.PLUS1,MINUS1,OR1.);
    QTERM_OP:=(.STAR1,SLASH1,DIV1,MOD1,AND1.);
    QTERM_LIST:=QFACTOR +QSEXPR_OP;
    QFACTOR_LIST:=QFACTOR + QTERM_OP;
    QSET_EXPR:=QARGUMENT +(.BUS1.);
    QSELECT:=(.PERIOD1,SUB1.);
    QSUB_END:=QARGUMENT +(.BUS1.);
    QWITH_LIST:=QDO_TAIL +QCOMMA;
    QTO_TAIL:=QDO_TAIL +QEXPR;
    GET

  END;

  { PROGRAM }

  PROCEDURE ERROR(NUMBER:INTEGER; KEYS:SETS);
  BEGIN
   { PUT2(MESSAGE2,THIS_PASS,NUMBER);}
    write(errors, ' ', MESSAGE2,' ',THIS_PASS,' ',NUMBER);
    writeln(errors);
    PUT2(MESSAGE2,THIS_PASS,NUMBER);
    WHILE NOT (SY IN KEYS) DO GET
  END;

  PROCEDURE CHECK(NUMBER:INTEGER; KEYS:SETS);
  BEGIN
    IF NOT (SY IN KEYS) THEN ERROR(NUMBER,KEYS)
  END;

  PROCEDURE NEW_LABEL(VAR L:LABEL1);
  BEGIN
    CURRENT_LABEL:=CURRENT_LABEL+1;
    L:=CURRENT_LABEL
  END;


  PROCEDURE PROGRAM_;
  BEGIN

    PUT1(PSTART2,PROCESS_MODE);
    PUT0(PROCESS2);
    BLOCK(QEOM, COMP_BLOCK);
    IF SY=PERIOD1 THEN GET
     ELSE ERROR(MPROG_ERROR,QEOM);
    IF SY<>EOM1 THEN ERROR(MPROG_ERROR,QEOM);
    PUT0(EOM2)

  END;

  PROCEDURE  BLOCK(KEYS:SETS; IN_COMPONENT:BOOLEAN);
  BEGIN
   DECLARATIONS(KEYS + QBODY);
    IF IN_COMPONENT THEN PUT0(INITS_DEF2);
    BODY(KEYS);
    IF IN_COMPONENT THEN PUT0(INITS_END2)
  END;

  {############}
  {DECLARATIONS}
  {############}

  PROCEDURE  DECLARATIONS(KEYS:SETS);
  VAR LKEYS1,LKEYS2:SETS;
  BEGIN
    LKEYS1:=KEYS + QDECLARATIONS;
    LKEYS2:=KEYS + QROUTINES;
    CHECK(DEC_ERROR,LKEYS1);
    WHILE SY IN QDEFINITIONS DO BEGIN
      IF SY=CONST1 THEN CONST_DEC(LKEYS1) ELSE TYPE_DEC(LKEYS1);
      CHECK(DEC_ERROR,LKEYS1)
    END;
    IF SY=VAR1 THEN VAR_DEC(LKEYS2);
    CHECK(DEC_ERROR,LKEYS2);
    IF SY IN QROUTINES THEN ROUTINE_DEC(KEYS)
  END;

  PROCEDURE  CONST_DEC(KEYS:SETS);
  VAR LKEYS1,LKEYS2:SETS;
  BEGIN
    LKEYS1:=KEYS + QCONST_DEF;
    LKEYS2:=KEYS-QCONST_DEF;
    GET;
    REPEAT
      IDENTIFIER_S(LKEYS1,CONST_ID2,CONSTDEF_ERROR);
      IF SY=EQ1 THEN GET ELSE ERROR(CONSTDEF_ERROR,LKEYS1);
      CONSTANT(LKEYS1);
      PUT0(CONST_DEF2);
      IF SY=SEMICOLON1 THEN GET ELSE ERROR(CONSTDEF_ERROR,LKEYS1);
      CHECK(CONSTDEF_ERROR,LKEYS1)
    UNTIL SY IN LKEYS2
  END;

  PROCEDURE  TYPE_DEC(KEYS:SETS);
  VAR LKEYS1,LKEYS2:SETS;
  BEGIN
    LKEYS1:=KEYS + QTYPE_DEF;
    LKEYS2:=KEYS-QTYPE_DEF;
    GET;
    REPEAT
      IDENTIFIER_S(LKEYS1,TYPE_ID2,TYPEDEF_ERROR);
      IF SY=EQ1 THEN GET ELSE ERROR(TYPEDEF_ERROR,LKEYS1);
      TYPE_(LKEYS1);
      PUT0(TYPE_DEF2);
      IF SY=SEMICOLON1 THEN GET ELSE ERROR(TYPEDEF_ERROR,LKEYS1);
      CHECK(TYPEDEF_ERROR,LKEYS1)
    UNTIL SY IN LKEYS2
  END;

{####}
{TYPE}
{####}

  PROCEDURE  TYPE_(KEYS:SETS);
  BEGIN
    CHECK(TYPE_ERROR,KEYS + QTYPE);
    IF SY IN QTYPE THEN
      CASE SY OF
        OPEN1: ENUM_TYPE(KEYS);
        ID1,INTEGER1,REAL1,CHAR1,STRING1: SUBR_TYPE(KEYS);
        SET1: SET_TYPE(KEYS);
        ARRAY1: ARRAY_TYPE(KEYS);
        RECORD1: RECORD_TYPE(KEYS);
        CLASS1,MONITOR1,PROCESS1: COMP_TYPE(KEYS)
      END
    ELSE BEGIN
      ERROR(TYPE_ERROR,KEYS);
      PUT1(TYPE2,XUNDEF)
    END
  END;

  PROCEDURE  ENUM_TYPE(KEYS:SETS);
  VAR NUMBER:INTEGER;
  BEGIN
    PUT0(ENUM2); GET;
    ID_LIST(KEYS + QCLOSE,ENUM_ID2,ENUM_ERROR,NUMBER);
    PUT0(ENUM_DEF2);
    IF SY=CLOSE1 THEN GET ELSE ERROR(ENUM_ERROR,KEYS);
  END;

  PROCEDURE  SUBR_TYPE(KEYS:SETS);
  VAR SPIX:SPELLING_INDEX;
  BEGIN
    IF SY=ID1 THEN BEGIN
      SPIX:=ARG; GET;
      CHECK(SUBR_ERROR,KEYS + QSUBR_LIMIT);
      IF SY=UP_TO1 THEN BEGIN
        PUT1(CONSTANT2,SPIX);
        GET;
        CONSTANT(KEYS);
        PUT0(SUBR_DEF2)
      END ELSE PUT1(TYPE2,SPIX)
    END ELSE BEGIN
      CONSTANT(KEYS + QSUBR_LIMIT);
      IF SY=UP_TO1 THEN GET ELSE ERROR(SUBR_ERROR,KEYS + QCONSTANT);
      CONSTANT(KEYS);
      PUT0(SUBR_DEF2)
    END
  END;

  PROCEDURE  SET_TYPE(KEYS:SETS);
  BEGIN
    GET;
    IF SY=OF1 THEN GET ELSE ERROR(SET_ERROR,KEYS + QTYPE);
    TYPE_(KEYS);
    PUT0(SET_DEF2)
  END;

  PROCEDURE  ARRAY_TYPE(KEYS:SETS);
  VAR LKEYS1:SETS; I,DIMENSIONS:INTEGER; DONE:BOOLEAN;
  BEGIN
    LKEYS1:=KEYS + QDIMENSION;
    GET;
    IF SY=SUB1 THEN GET ELSE ERROR(ARRAY_ERROR,LKEYS1);
    DIMENSIONS:=0; DONE:=FALSE;
    REPEAT
      {INDEX} TYPE_(LKEYS1); DIMENSIONS:=DIMENSIONS+1;
      CHECK(ARRAY_ERROR,LKEYS1);
      IF SY IN QTYPE_LIST THEN
        IF SY=COMMA1 THEN GET ELSE ERROR(ARRAY_ERROR,LKEYS1)
      ELSE DONE:=TRUE
    UNTIL DONE;
    IF SY=BUS1 THEN GET ELSE ERROR(ARRAY_ERROR,KEYS + QOF_TYPE);
    IF SY=OF1 THEN GET ELSE ERROR(ARRAY_ERROR,KEYS + QTYPE);
    {ELEMENT} TYPE_(KEYS);
    FOR I:=1 TO DIMENSIONS DO PUT0(ARRAY_DEF2)
  END;

  PROCEDURE  RECORD_TYPE(KEYS:SETS);
  VAR NUMBER:INTEGER; DONE:BOOLEAN; LKEYS1:SETS;
  BEGIN
    LKEYS1:=KEYS + QVAR_DEF + QEND;
    PUT0(REC2); GET; DONE:= FALSE;
    REPEAT
      ID_LIST(LKEYS1,FIELD_ID2,RECORD_ERROR,NUMBER);
      IF SY=COLON1 THEN GET
        ELSE ERROR(RECORD_ERROR,LKEYS1);
      {FIELD} TYPE_(LKEYS1);
      PUT1(FIELDLIST2,NUMBER);
      CHECK(RECORD_ERROR,LKEYS1);
      IF SY IN QVAR_DEF THEN
        IF SY=SEMICOLON1 THEN GET
         ELSE ERROR(RECORD_ERROR,LKEYS1)
      ELSE DONE:=TRUE
    UNTIL DONE;
    PUT0(REC_DEF2);
    IF SY=END1 THEN GET ELSE ERROR(RECORD_ERROR,KEYS);
  END;

  PROCEDURE  COMP_TYPE(KEYS:SETS);
  VAR MODE,OP:INTEGER;
  BEGIN
    CASE SY OF
      CLASS1: BEGIN MODE:=CLASS_MODE; OP:=CLASS2 END;
      MONITOR1: BEGIN MODE:=MONITOR_MODE; OP:=MONITOR2 END;
      PROCESS1: BEGIN MODE:=PROCESS_MODE; OP:=PROCESS2 END
    END;
    GET;
    PARM_LIST(KEYS + QPARM_END + QSTACK,MODE);
    PUT0(OP);
    IF SY=SEMICOLON1 THEN GET;
    {NO CHECK SINCE AD HOC EXTENSION}
    IF SY=PLUS1 THEN BEGIN
      GET;
      IF SY=INTEGER1 THEN BEGIN
        PUT1(STACK2,ARG);
        GET
      END ELSE ERROR(STACK_ERROR,KEYS + QBLOCK)
    END;
    BLOCK(KEYS,COMP_BLOCK)
  END;

   {#########}
   {VARIABLES}
   {#########}

  PROCEDURE  VAR_DEC(KEYS:SETS);
  VAR OP,NUMBER:INTEGER; LKEYS1:SETS;
  BEGIN
    LKEYS1:=KEYS + QVARE_DEF;
    GET;
    REPEAT
      CHECK(VAR_ERROR,LKEYS1);
      IF SY=ENTRY1 THEN BEGIN OP:=VARE_LIST2; GET END ELSE OP:=VAR_LIST2;
      ID_LIST(LKEYS1,VAR_ID2,VAR_ERROR,NUMBER);
      IF SY=COLON1 THEN GET ELSE ERROR(VAR_ERROR,LKEYS1);
      {VAR} TYPE_(LKEYS1);
      PUT1(OP,NUMBER);
      IF SY=SEMICOLON1 THEN GET ELSE ERROR(VAR_ERROR,LKEYS1);
      CHECK(VAR_ERROR,LKEYS1)
    UNTIL NOT(SY IN QVARE_DEF);
  END;

  PROCEDURE  ID_LIST(KEYS:SETS; OP,ERROR_NUM:INTEGER; VAR ID_COUNT:INTEGER);
  VAR LKEYS1:SETS; DONE:BOOLEAN;
  BEGIN
    LKEYS1:=KEYS + QID_LIST;
    ID_COUNT:=0; DONE:=FALSE;
    REPEAT
      IDENTIFIER_S(LKEYS1,OP,ERROR_NUM);
      ID_COUNT:=ID_COUNT+1;
      CHECK(ERROR_NUM,LKEYS1);
      IF SY IN QID_LIST THEN
        IF SY=COMMA1 THEN GET ELSE ERROR(ERROR_NUM,LKEYS1)
      ELSE DONE:=TRUE
    UNTIL DONE
  END;

  PROCEDURE  IDENTIFIER_S(KEYS:SETS; OP,ERROR_NUM:INTEGER);
  BEGIN
    IF SY=ID1 THEN BEGIN PUT1(OP,ARG);
     GET
    END
    ELSE BEGIN
      ERROR(ERROR_NUM,KEYS);
      PUT1(OP,XUNDEF)
    END
  END;

  {########}
  {ROUTINES}
  {########}

  PROCEDURE  ROUTINE_DEC(KEYS:SETS);
  VAR LKEYS1:SETS; SEMI_EXPECTED:BOOLEAN;
  BEGIN
    LKEYS1:=KEYS + QROUTINES;
    REPEAT
      SEMI_EXPECTED:=TRUE;
      CASE SY OF
        PROCEDURE1: PROC_DEC(LKEYS1);
        FUNCTION1: FUNC_DEC(LKEYS1);
        PROGRAM1: BEGIN SEMI_EXPECTED:=FALSE; PROG_DEC(LKEYS1) END
      END;
      IF SEMI_EXPECTED THEN
        IF SY=SEMICOLON1 THEN GET ELSE ERROR(ROUTINE_ERROR,LKEYS1);
      CHECK(ROUTINE_ERROR,LKEYS1);
    UNTIL NOT(SY IN QROUTINES)
  END;

  PROCEDURE  PROC_DEC(KEYS:SETS);
  VAR MODE,ID_OP,DEF_OP,END_OP:INTEGER;
  BEGIN
    GET;
    CHECK(PROC_ERROR,KEYS + QPROC_END);
    IF SY=ENTRY1 THEN BEGIN
      GET;
      MODE:=PROCE_MODE;
      ID_OP:=PROCE_ID2;
      DEF_OP:=PROCE_DEF2; END_OP:=PROCE_END2
    END ELSE BEGIN
      MODE:=PROC_MODE;
      ID_OP:=PROC_ID2;
      DEF_OP:=PROC_DEF2; END_OP:=PROC_END2
    END;
    IDENTIFIER_S(KEYS + QPROC_PARMS,ID_OP,PROC_ERROR);
    PARM_LIST(KEYS + QPARM_END,MODE);
    PUT0(DEF_OP);
    IF SY=SEMICOLON1 THEN GET ELSE ERROR(PROC_ERROR,KEYS + QBLOCK);
    BLOCK(KEYS,ROUTINE_BLOCK);
    PUT0(END_OP)
  END;

  PROCEDURE  FUNC_DEC(KEYS:SETS);
  VAR MODE,ID_OP,DEF_OP,END_OP:INTEGER; LKEYS1:SETS;
  BEGIN
    LKEYS1:=KEYS + QFUNC_END;
    GET;
    CHECK(FUNC_ERROR,LKEYS1);
    IF SY=ENTRY1 THEN BEGIN
      GET;
      MODE:=FUNCE_MODE; ID_OP:=FUNCE_ID2;
      DEF_OP:=FUNCE_DEF2; END_OP:=FUNCE_END2
    END ELSE BEGIN
      MODE:=FUNC_MODE; ID_OP:=FUNC_ID2;
      DEF_OP:=FUNC_DEF2; END_OP:=FUNC_END2
    END;
    IDENTIFIER_S(LKEYS1,ID_OP,FUNC_ERROR);
    PARM_LIST(LKEYS1-QOPEN,MODE);
    IF SY=COLON1 THEN GET ELSE ERROR(FUNC_ERROR,KEYS + QFUNC_TYPE);
    IDENTIFIER_S(KEYS + QPARM_END,DEF_OP,FUNC_ERROR);
    IF SY=SEMICOLON1 THEN GET ELSE ERROR(FUNC_ERROR,KEYS + QBLOCK);
    BLOCK(KEYS,ROUTINE_BLOCK);
    PUT0(END_OP)
  END;

  PROCEDURE  PROG_DEC(KEYS:SETS);
  VAR DUMMY:INTEGER;
  BEGIN
    GET;
    IDENTIFIER_S(KEYS + QPROG_END,PROG_ID2,PROG_ERROR);
    PARM_LIST(KEYS + QINTERFACE,PROGRAM_MODE);
    PUT0(INTF2);
    IF SY=SEMICOLON1 THEN GET ELSE ERROR(PROG_ERROR,KEYS + QINTERFACE);
    CHECK(PROG_ERROR,KEYS + QENTRY);
    IF SY=ENTRY1 THEN BEGIN
      GET;
      ID_LIST(KEYS + QSEMICOLON,INTF_ID2,PROG_ERROR,DUMMY);
      IF SY=SEMICOLON1 THEN GET ELSE ERROR(PROG_ERROR,KEYS)
    END;
    PUT0(PROG_DEF2)
  END;

  PROCEDURE  PARM_LIST(KEYS:SETS; MODE:INTEGER);
  VAR LIST_OP,TYPE_OP,NUMBER:INTEGER; DONE:BOOLEAN; LKEYS1:SETS;
  BEGIN
    PUT1(PSTART2,MODE);
    CHECK(PARM_ERROR,KEYS + QOPEN);
    IF SY=OPEN1 THEN BEGIN
      LKEYS1:=KEYS + QPARM_LIST + QCLOSE;
      GET; DONE:=FALSE;
      REPEAT
        CHECK(PARM_ERROR,LKEYS1);
        IF SY=VAR1 THEN BEGIN
          GET; LIST_OP:=VPARMLIST2
        END ELSE LIST_OP:=CPARMLIST2;
        ID_LIST(LKEYS1,PARM_ID2,PARM_ERROR,NUMBER);
        IF SY=COLON1 THEN GET ELSE ERROR(PARM_ERROR,LKEYS1);
        CHECK(PARM_ERROR,LKEYS1);
        IF SY=UNIV1 THEN BEGIN
          GET; TYPE_OP:=UNIV_TYPE2
        END ELSE TYPE_OP:=PARM_TYPE2;
        {TYPE} IDENTIFIER_S(LKEYS1,TYPE_OP,PARM_ERROR);
        PUT1(LIST_OP,NUMBER);
        CHECK(PARM_ERROR,LKEYS1);
        IF SY IN QPARM_LIST THEN
          IF SY=SEMICOLON1 THEN GET ELSE ERROR(PARM_ERROR,LKEYS1)
        ELSE DONE:=TRUE
      UNTIL DONE;
      IF SY=CLOSE1 THEN GET ELSE ERROR(PARM_ERROR,KEYS);
      PUT0(PEND2)
    END
  END;

  {####}
  {BODY}
  {####}

  PROCEDURE  BODY(KEYS:SETS);
  BEGIN
    PUT0(BODY2);
    IF SY=BEGIN1 THEN GET ELSE ERROR(BODY_ERROR,KEYS + QBODY_END);
    STAT_LIST (KEYS + QEND);
    PUT0(BODY_END2);
    IF SY=END1 THEN GET ELSE ERROR(BODY_ERROR,KEYS)
  END;

  PROCEDURE  STAT_LIST(KEYS:SETS);
  VAR DONE:BOOLEAN; LKEYS1:SETS;
  BEGIN
    LKEYS1:=KEYS + QSTAT_LIST;
    DONE:=FALSE;
    REPEAT
      STAT(LKEYS1);
      CHECK(STATS_ERROR,LKEYS1);
      IF SY IN QSTAT_LIST  THEN
        IF SY=SEMICOLON1 THEN GET ELSE ERROR(STATS_ERROR,LKEYS1)
      ELSE DONE:=TRUE
    UNTIL DONE
  END;

  PROCEDURE STAT(KEYS:SETS);
  BEGIN
    CHECK(STAT_ERROR,KEYS + QSTAT);
    IF SY IN QSTAT THEN
      CASE SY OF
        ID1: ID_STAT(KEYS);
        BEGIN1: COMPOUND_STAT(KEYS);
        IF1: IF_STAT(KEYS);
        CASE1: CASE_STAT(KEYS);
        WHILE1: WHILE_STAT(KEYS);
        REPEAT1: REPEAT_STAT(KEYS);
        FOR1: FOR_STAT(KEYS);
        CYCLE1: CYCLE_STAT(KEYS);
        WITH1: WITH_STAT(KEYS);
        INIT1: INIT_STAT(KEYS)
      END
  END;

  PROCEDURE ID_STAT(KEYS:SETS);
  VAR LKEYS1:SETS;
  BEGIN
    LKEYS1:=KEYS + QID_END;
    VARIABLE(LKEYS1);
    CHECK(IDSTAT_ERROR,LKEYS1);
    IF SY=BECOMES1 THEN BEGIN
      PUT0(ANAME2); GET;
      EXPR(KEYS);
      PUT0(STORE2)
    END ELSE BEGIN
      PUT0(CALL_NAME2);
      ARG_LIST(KEYS);
      PUT0(CALL2)
    END
  END;

  PROCEDURE ARG_LIST(KEYS:SETS);
  VAR DONE:BOOLEAN; LKEYS1:SETS;
  BEGIN
    CHECK(ARG_ERROR,KEYS + QOPEN);
    IF SY=OPEN1 THEN BEGIN
       PUT0(ARG_LIST2);
       GET;
       DONE:=FALSE;
       LKEYS1:=KEYS + QARG_END;
      REPEAT
        EXPR(LKEYS1); PUT0(ARG2);
        CHECK(ARG_ERROR,LKEYS1);
        IF SY IN QARGUMENT THEN
          IF SY=COMMA1 THEN GET ELSE ERROR(ARG_ERROR,LKEYS1)
        ELSE DONE:=TRUE
      UNTIL DONE;
      IF SY=CLOSE1 THEN GET ELSE ERROR(ARG_ERROR,KEYS)
    END
  END;

  PROCEDURE COMPOUND_STAT(KEYS:SETS);
  BEGIN
    GET;
    STAT_LIST (KEYS);
    IF SY=END1 THEN GET ELSE ERROR(COMP_ERROR,KEYS)
  END;

  PROCEDURE IF_STAT(KEYS:SETS);
  VAR L1,L2:LABEL1; LKEYS1:SETS;
  BEGIN
    LKEYS1:=KEYS + QTHEN_END;
    GET;
    EXPR(KEYS + QIF_END);
    NEW_LABEL(L1); PUT1(FALSEJUMP2,L1);
    IF SY=THEN1 THEN GET ELSE ERROR(IF_ERROR,LKEYS1);
    STAT(LKEYS1);
    CHECK(IF_ERROR,LKEYS1);
    IF SY=ELSE1 THEN BEGIN
      NEW_LABEL(L2); PUT2(JUMP_DEF2,L2,L1);
      GET;
      STAT(KEYS);
      PUT1(DEF_LABEL2,L2)
    END ELSE PUT1(DEF_LABEL2,L1)
  END;

  PROCEDURE CASE_STAT(KEYS:SETS);
  VAR L0,LI,LN:LABEL1; DONE:BOOLEAN; LKEYS1:SETS;
  BEGIN
    LKEYS1:=KEYS + QCASES;
    GET;
    NEW_LABEL(L0);
    NEW_LABEL(LN);
    EXPR(KEYS + QCASE_END);
    PUT1(CASE_JUMP2,L0); DONE:=FALSE;
    IF SY=OF1 THEN GET ELSE ERROR(CASE_ERROR,LKEYS1);
    REPEAT
      NEW_LABEL(LI);
      PUT1(DEF_CASE2,LI);
      LABEL_LIST(LKEYS1);
      IF SY=COLON1 THEN GET
        ELSE ERROR(CASE_ERROR,LKEYS1);
      STAT(LKEYS1);
      PUT1(JUMP2,LN);
      CHECK(CASE_ERROR,LKEYS1);
      IF SY IN QCASES THEN
        IF SY=SEMICOLON1 THEN GET
         ELSE ERROR(CASE_ERROR,LKEYS1)
      ELSE DONE:=TRUE
    UNTIL DONE;
    PUT2(END_CASE2,L0,LN);
    IF SY=END1 THEN GET
     ELSE ERROR(CASE_ERROR,KEYS);
  END;

  PROCEDURE LABEL_LIST(KEYS:SETS);
  VAR DONE:BOOLEAN; LKEYS1:SETS;
  BEGIN                             //keys  KEYS OR QCASES;
    LKEYS1:=KEYS + QLABEL_LIST;  // QLABEL_LIST:=QCONSTANT OR QCOMMA;
    DONE:=FALSE;
    REPEAT
      CONSTANT(LKEYS1);
      PUT0(CASE2);
      CHECK(LABEL_ERROR,LKEYS1);
      IF SY IN QLABEL_LIST THEN
        IF SY=COMMA1 THEN GET ELSE ERROR(LABEL_ERROR,LKEYS1)
      ELSE DONE:=TRUE
    UNTIL DONE
  END;

  PROCEDURE WHILE_STAT(KEYS:SETS);
  VAR L1,L2:LABEL1;
  BEGIN
    NEW_LABEL(L1); NEW_LABEL(L2);
    PUT1(DEF_LABEL2,L1);
    GET;
    EXPR(KEYS + QDO_TAIL);
    PUT1(FALSEJUMP2,L2);
    IF SY=DO1 THEN GET ELSE ERROR(WHILE_ERROR,KEYS + QSTAT);
    STAT(KEYS);
    PUT2(JUMP_DEF2,L1,L2)
  END;

  PROCEDURE REPEAT_STAT(KEYS:SETS);
  VAR L:LABEL1;
  BEGIN
    NEW_LABEL(L);
    PUT1(DEF_LABEL2,L);
    GET;
    STAT_LIST (KEYS + QUNTIL_TAIL);
    IF SY=UNTIL1 THEN GET ELSE ERROR(REPEAT_ERROR,KEYS + QEXPR);
    EXPR(KEYS);
    PUT1(FALSEJUMP2,L)
  END;

  PROCEDURE FOR_STAT(KEYS:SETS);
  CONST UP=5; DOWN=3;
  VAR L1,L2:LABEL1; LKEYS1:SETS; OP,DIRECTION:INTEGER;
  BEGIN
    LKEYS1:=KEYS + QFORB_END;
    GET; NEW_LABEL(L1); NEW_LABEL(L2);
    IDENTIFIER_S(KEYS + QFOR_END,NAME2,FOR_ERROR); PUT0(ADDRESS2);
    IF SY=BECOMES1 THEN GET ELSE ERROR(FOR_ERROR,LKEYS1);
    EXPR(LKEYS1); PUT0(FOR_STORE2);
    CHECK(FOR_ERROR,LKEYS1); DIRECTION:=UP; OP:=FOR_UP2;
    IF SY=TO1 THEN GET
    ELSE IF SY=DOWNTO1 THEN BEGIN
      GET; DIRECTION:=DOWN; OP:=FOR_DOWN2
    END ELSE ERROR(FOR_ERROR,QTO_TAIL);
    EXPR(KEYS + QDO_TAIL);
    PUT3(FOR_LIM2,L1,DIRECTION,L2);
    IF SY=DO1 THEN GET ELSE ERROR(FOR_ERROR,KEYS);
    STAT(KEYS);
    PUT2(OP,L1,L2)
  END;

  PROCEDURE CYCLE_STAT(KEYS:SETS);
  VAR L:LABEL1;
  BEGIN
    GET; NEW_LABEL(L);
    PUT1(DEF_LABEL2,L);
    STAT_LIST (KEYS);
    IF SY=END1 THEN GET ELSE ERROR(CYCLE_ERROR,KEYS);
    PUT1(JUMP2,L)
  END;

  PROCEDURE WITH_STAT(KEYS:SETS);
  VAR WITH_COUNT,I:INTEGER; LKEYS1:SETS; DONE:BOOLEAN;
  BEGIN
    LKEYS1:=KEYS + QWITH_LIST;
    WITH_COUNT:=0; GET; DONE:=FALSE;
    REPEAT
      PUT0(WITH_VAR2);
      VARIABLE(LKEYS1);
      PUT0(WITH_TEMP2);
      WITH_COUNT:=WITH_COUNT+1;
      CHECK(WITH_ERROR,LKEYS1);
      IF SY IN QID_LIST THEN
        IF SY=COMMA1 THEN GET ELSE ERROR(WITH_ERROR,LKEYS1)
      ELSE DONE:=TRUE
    UNTIL DONE;
    IF SY=DO1 THEN GET ELSE ERROR(WITH_ERROR,KEYS + QSTAT);
    STAT(KEYS);
    FOR I:=1 TO WITH_COUNT DO PUT0(WITH2)
  END;

  PROCEDURE INIT_STAT(KEYS:SETS);
  VAR DONE:BOOLEAN; LKEYS1:SETS;
  BEGIN
    GET;
    LKEYS1:=KEYS + QINIT_LIST;
    DONE:=FALSE;
    REPEAT
      VARIABLE(LKEYS1); PUT0(INIT_NAME2);
      ARG_LIST(LKEYS1); PUT0(INIT2);
      CHECK(INIT_ERROR,LKEYS1);
      IF SY IN QINIT_LIST THEN
        IF SY=COMMA1 THEN GET ELSE ERROR(INIT_ERROR,LKEYS1)
      ELSE DONE:=TRUE
    UNTIL DONE
  END;

  {##########}
  {EXPRESSION}
  {##########}

  PROCEDURE EXPR(KEYS:SETS);
  VAR OP:INTEGER;
  BEGIN
    SEXPR(KEYS + QEXPR_OP);
    CHECK(EXPR_ERROR,KEYS + QEXPR_OP);
    IF SY IN QEXPR_OP THEN BEGIN
      CASE SY OF
        EQ1: OP:=EQ2;
        NE1: OP:=NE2;
        LE1: OP:=LE2;
        GE1: OP:=GE2;
        LT1: OP:=LT2;
        GT1: OP:=GT2;
        IN1: OP:=IN2
      END;
      PUT0(VALUE2); GET;
      SEXPR(KEYS);
      PUT0(OP)
    END
  END;

  PROCEDURE SEXPR(KEYS:SETS);
  VAR UNARY:BOOLEAN; LKEYS1:SETS; OP:INTEGER;
  BEGIN
    LKEYS1:=KEYS + QTERM_LIST;
    CHECK(EXPR_ERROR,LKEYS1);
    IF SY IN QUNARY THEN BEGIN
      UNARY:=TRUE;
      IF SY=PLUS1 THEN OP:=UPLUS2 ELSE OP:=UMINUS2;
      GET
    END ELSE UNARY:=FALSE;
    TERM(LKEYS1);
    IF UNARY THEN PUT0(OP);
    CHECK(EXPR_ERROR,LKEYS1);
    IF SY IN QTERM_LIST THEN BEGIN
      PUT0(VALUE2);
      REPEAT
        IF SY IN QSEXPR_OP THEN BEGIN
          CASE SY OF
            PLUS1: OP:=PLUS2;
            MINUS1: OP:=MINUS2;
            OR1: OP:=OR2
          END; GET
        END ELSE BEGIN
          ERROR(EXPR_ERROR,LKEYS1);
          OP:=PLUS2
        END;
        TERM(LKEYS1); PUT0(OP);
        CHECK(EXPR_ERROR,LKEYS1);
      UNTIL NOT(SY IN QTERM_LIST)
    END
  END;

  PROCEDURE TERM(KEYS:SETS);
  VAR OP:INTEGER; LKEYS1:SETS;
  BEGIN
    LKEYS1:=KEYS + QFACTOR_LIST;
    FACTOR(LKEYS1);
    CHECK(EXPR_ERROR,LKEYS1);
    IF SY IN QFACTOR_LIST THEN BEGIN
      PUT0(VALUE2);
      REPEAT
        IF SY IN QTERM_OP THEN BEGIN
          CASE SY OF
            STAR1: OP:=STAR2;
            SLASH1: OP:=SLASH2;
            DIV1: OP:=DIV2;
            MOD1: OP:=MOD2;
            AND1: OP:=AND2
          END;
          GET
        END ELSE BEGIN
          ERROR(EXPR_ERROR,LKEYS1);
          OP:=STAR2
        END;
        FACTOR(LKEYS1);
        PUT0(OP);
        CHECK(EXPR_ERROR,LKEYS1)
      UNTIL NOT(SY IN QFACTOR_LIST)
    END
  END;

  PROCEDURE FACTOR(KEYS:SETS);
  VAR LKEYS1:SETS;
  BEGIN
    CHECK(EXPR_ERROR,KEYS + QFACTOR);
    IF SY IN QFACTOR THEN
      CASE SY OF
        REAL1: BEGIN PUT0(FREAL2); GET END;
        STRING1:
           BEGIN
            PUT1(FSTRING2,ARG);
            GET
           END;
        INTEGER1: BEGIN PUT1(FINTEGER2,ARG); GET END;
        CHAR1: BEGIN PUT1(FCHAR2,ARG); GET END;
        ID1: FACTOR_ID(KEYS);
        OPEN1: BEGIN
          GET; EXPR(KEYS + QCLOSE);
          IF SY=CLOSE1 THEN GET ELSE ERROR(EXPR_ERROR,KEYS)
        END;
        NOT1: BEGIN
          GET; FACTOR(KEYS); PUT0(NOT2)
        END;
        SUB1: BEGIN
          GET; PUT0(EMPTY_SET2);
          LKEYS1:=KEYS + QSET_EXPR;
          CHECK(EXPR_ERROR,LKEYS1);
          WHILE SY IN QARGUMENT DO BEGIN
            EXPR(LKEYS1); PUT0(INCLUDE2);
            CHECK(EXPR_ERROR,LKEYS1);
            IF SY IN QARGUMENT THEN
              IF SY=COMMA1 THEN GET ELSE ERROR(EXPR_ERROR,LKEYS1);
            CHECK(EXPR_ERROR,LKEYS1)
          END;
          IF SY=BUS1 THEN GET ELSE ERROR(EXPR_ERROR,KEYS)
        END
      END
    ELSE PUT1(NAME2,XUNDEF)
  END;

  PROCEDURE FACTOR_ID(KEYS:SETS);
  BEGIN
    VARIABLE(KEYS + QOPEN);
    CHECK(EXPR_ERROR,KEYS + QOPEN);
    IF SY=OPEN1 THEN BEGIN
      PUT0(FUNCTION2);
      ARG_LIST(KEYS);
      PUT0(CALL_FUNC2)
    END ELSE PUT0(FNAME2)
  END;

  {########}
  {VARIABLE}
  {########}

  PROCEDURE VARIABLE(KEYS:SETS);
  VAR LKEYS1,LKEYS2:SETS; DONE:BOOLEAN;
  BEGIN
    LKEYS1:=KEYS + QSELECT;
    IDENTIFIER_S(LKEYS1,NAME2,VARIABLE_ERROR);
    CHECK(VARIABLE_ERROR,LKEYS1);
    WHILE SY IN QSELECT DO BEGIN
      IF SY=PERIOD1 THEN BEGIN
        GET;
        IDENTIFIER_S(LKEYS1,COMP2,VARIABLE_ERROR)
      END ELSE BEGIN {SY=SUB1}
        PUT0(ADDRESS2); GET;
        LKEYS2:=LKEYS1 + QSUB_END; DONE:=FALSE;
        REPEAT
          EXPR(LKEYS2); PUT0(SUB2);
          CHECK(VARIABLE_ERROR,LKEYS2);
          IF SY IN QARGUMENT THEN
            IF SY=COMMA1 THEN GET ELSE ERROR(VARIABLE_ERROR,LKEYS2)
          ELSE DONE:=TRUE
        UNTIL DONE;
        IF SY=BUS1 THEN GET ELSE ERROR(VARIABLE_ERROR,LKEYS1)
      END;
      CHECK(VARIABLE_ERROR,LKEYS1)
    END
  END;

  PROCEDURE CONSTANT(KEYS:SETS);
  BEGIN
    CHECK(CONSTANT_ERROR,KEYS + QCONSTANT);
    IF SY IN QCONSTANT THEN BEGIN
      CASE SY OF
        ID1: PUT1(CONSTANT2,ARG);
        INTEGER1: PUT1(INTEGER2,ARG);
        REAL1: PUT0(REAL2);
        CHAR1: PUT1(CHAR2,ARG);
        STRING1: PUT1(STRING2,ARG)
      END;
      GET
    END ELSE BEGIN
      ERROR(CONSTANT_ERROR,KEYS);
      PUT1(CONSTANT2,XUNDEF)
    END
  END;

  { MAIN PROGRAM PASS2}
begin
   INITIALIZE;
   PROGRAM_;
   INTER_PASS_PTR^.LABELS:=CURRENT_LABEL;
end;


procedure Pass3(var OK: boolean);

CONST
WORDLENGTH = {2}1 {BYTES};
REALLENGTH = {8}{4} 2 {BYTES};
SETLENGTH = {16} 8 {BYTES};
LISTOPTION = 0;    SUMMARYOPTION = 1;  TESTOPTION = 2;     CHECKOPTION = 3;
CODEOPTION = 4;    NUMBEROPTION = 5;
IDLENGTH = 12;
MAXDIGIT = 6;
MAXARG = 10;
PRINTLIMIT = 18;

{INPUT OPERATORS}
EOM1=1;            CONST_ID1=2;        CONST_DEF1=3;       TYPE_ID1=4;
TYPE_DEF1=5;       VAR_ID1=6;          VAR_LIST1=7;        EVAR_LIST1=8;
INITS_DEF1=9;      INITS_END1=10;      PROC_ID1=11;        PROC_DEF1=12;
PROCE_DEF1=13;     PROC_END1=14;       PROCE_END1=15;      FUNC_ID1=16;
FUNC_DEF1=17;      FUNCE_DEF1=18;      FUNC_END1=19;       FUNCE_END1=20;
PROG_ID1=21;       PROG_DEF1=22;       INTF_ID1=23;        TYPE1=24;
ENUM1=25;          ENUM_ID1=26;        ENUM_DEF1=27;       SUBR_DEF1=28;
SET_DEF1=29;       ARRAY_DEF1=30;      REC1=31;            FIELD_ID1=32;
FIELDLIST1=33;     REC_DEF1=34;        CLASS1=35;          MONITOR1=36;
PROCESS1=37;       STACK1=38;          PSTART1=39;         PARM_ID1=40;
PARM_TYPE1=41;     UNIV_TYPE1=42;      CPARMLIST1=43;      VPARMLIST1=44;
BODY1=45;          BODY_END1=46;       ANAME1=47;          STORE1=48;
CALL_NAME1=49;     CALL1=50;           ARG_LIST1=51;       ARG1=52;
FALSEJUMP1=53;     DEF_LABEL1=54;      JUMP_DEF1=55;       INTF1=56;
DEF_CASE1=57;      CASE1=58;           JUMP1=59;           END_CASE1=60;
ADDRESS1=61;       FOR_STORE1=62;      FOR_LIM1=63;        FOR_UP1=64;
FOR_DOWN1=65;      WITH_VAR1=66;       WITH_TEMP1=67;      WITH1=68;
INIT_NAME1=69;     INIT1=70;           VALUE1=71;          LT1=72;
EQ1=73;            GT1=74;             LE1=75;             NE1=76;
GE1=77;            IN1=78;             UPLUS1=79;          UMINUS1=80;
PLUS1=81;          MINUS1=82;          OR1=83;             STAR1=84;
SLASH1=85;         DIV1=86;            MOD1=87;            AND1=88;
FNAME1=89;         NOT1=90;            EMPTY_SET1=91;      INCLUDE1=92;
FUNCTION1=93;      CALL_FUNC1=94;      NAME1=95;           COMP1=96;
SUB1=97;           ARROW1=98;          CONSTANT1=99;       REAL1=100;
FREAL1=101;        INTEGER1=102;       FINTEGER1=103;      CHAR1=104;
FCHAR1=105;        STRING1=106;        FSTRING1=107;       NEW_LINE1=108;
LCONST1=109;       MESSAGE1=110;       PROCE_ID1=111;      FUNCE_ID1=112;
PEND1=113;         CASE_JUMP1=114;

{OUTPUT OPERATORS}
EOM2=1;            TYPE_DEF2=2;        NEW_NOUN2=3;        VAR_LIST2=4;
EVAR_LIST2=5;      INITS_DEF2=6;       PROC_DEF2=7;        PROCE_DEF2=8;
FUNC_DEF2=9;       FUNCE_DEF2=10;      PROG_DEF2=11;       TYPE2=12;
ENUM_DEF2=13;      SUBR_DEF2=14;       SET_DEF2=15;        INTF2=16;
ARRAY_DEF2=17;     REC2=18;            FIELDLIST2=19;      REC_DEF2=20;
CLASS2=21;         MONITOR2=22;        PROCESS2=23;        STACK2=24;
PSTART2=25;        PARM_TYPE2=26;      UNIV_TYPE2=27;      CPARMLIST2=28;
VPARMLIST2=29;     BODY2=30;           BODY_END2=31;       ADDRESS2=32;
RESULT2=33;        STORE2=34;          CALL_PROC2=35;      CALL_PROG2=36;
INTF_ID2=37;       PARM2=38;           FALSEJUMP2=39;      DEF_LABEL2=40;
JUMP_DEF2=41;      FUNCF_DEF2=42;      JUMP2=43;           CASE_LIST2=44;
FOR_STORE2=45;     FOR_LIM2=46;        FOR_UP2=47;         FOR_DOWN2=48;
WITH_VAR2=49;      WITH_TEMP2=50;      WITH2=51;           INIT2=52;
VALUE2=53;         LT2=54;             EQ2=55;             GT2=56;
LE2=57;            NE2=58;             GE2=59;             IN2=60;
UPLUS2=61;         UMINUS2=62;         PLUS2=63;           MINUS2=64;
OR2=65;            STAR2=66;           SLASH2=67;          DIV2=68;
MOD2=69;           AND2=70;            NOT2=71;            EMPTY_SET2=72;
INCLUDE2=73;       FUNCTION2=74;       CALL_FUNC2=75;      ROUTINE2=76;
VAR2=77;           ARROW2=78;          VCOMP2=79;          RCOMP2=80;
SUB2=81;           INDEX2=82;          REAL2=83;           STRING2=84;
LCONST2=85;        MESSAGE2=86;        NEW_LINE2=87;       FWD_DEF2=88;
CHK_TYPE2=89;      PROCF_DEF2=90;      UNDEF2=91;          PEND2=92;
CASE_JUMP2=93;

{OTHER CONSTANTS}
NOUN_MAX=700;
MIN_CASE=0;        MAX_CASE=127;       THIS_PASS=3;        SPELLING_MAX=700;
OPERAND_MAX=150;   UPDATE_MAX=100;     UPDATE_MAX1=101;    MAX_LEVEL=15;

{MODES}
CLASS_MODE=1;      MONITOR_MODE=2;     PROCESS_MODE=3;     PROC_MODE=4;
PROCE_MODE=5;      FUNC_MODE=6;        FUNCE_MODE=7;       PROGRAM_MODE=8;

{STANDARD SPELLING/NOUN INDICES}
XUNDEF=0;          XFALSE=1;           XTRUE=2;            XINTEGER=3;
XBOOLEAN=4;        XCHAR=5;            XQUEUE=6;           XABS=7;
XATTRIBUTE=8;      XCHR=9 ;            XCONTINUE=10;       XCONV=11;
XDELAY=12;         XEMPTY=13;          XIO=14;             XORD=15;
XPRED=16;          XSTOP=17;           XREALTIME=18;       XSETHEAP=19;
XSUCC=20;          XTRUNC=21;          XSTART=22;          XWAIT=23;
XREAL=24;

{STANDARD NOUN INDICES}
ZARITHMETIC=25;    ZINDEX=26;          ZPASSIVE=27;        ZVPARM=28;
ZCPARM=29;         ZSPARM=30;          ZWITH=31;

{ERRORS}
UNRES_ERROR=1;     AMBIGUITY_ERROR=2;  ABORT_ERROR=3;      CONSTID_ERROR=4;
SUBR_ERROR=5;      FEW_ARGS_ERROR=6;   ARG_LIST_ERROR=7;   MANY_ARGS_ERROR=8;
CASERANGE_ERROR=9; CASETYPE_ERROR=10;  AMBICASE_ERROR=11;  WITH_ERROR=12;
INIT_ERROR=13;     PROC_USE_ERROR=14;  NAME_ERROR=15;      COMP_ERROR=16;
SUB_ERROR=17;      INTERFACE_ERROR=18; CALL_NAME_ERROR=19;

{MISCELANEOUS}
NOT_POSSIBLY_FORWARD=FALSE;            POSSIBLY_FORWARD=TRUE;
OUTPUT=TRUE;       RETAIN=FALSE;       PROC_TYPE= 1;       STD_LEVEL=0;
GLOBAL_LEVEL=1;


TYPE

IDENTIFIER = ARRAY (.1..IDLENGTH.) OF CHAR;
POINTER = ^INTEGER;
OPTION = LISTOPTION..NUMBEROPTION;

PASSPTR = ^PASSLINK;
PASSLINK =
  RECORD
    OPTIONS: SET OF OPTION;
    LABELS, BLOCKS, CONSTANTS, RESETPOINT: INTEGER;
    TABLES: POINTER
  END;

ARGTAG =
  (NILTYPE, BOOLTYPE, INTTYPE, IDTYPE, PTRTYPE);
TYPE ARGTYPE = RECORD
                 CASE TAG: ARGTAG OF
                   NILTYPE, BOOLTYPE: (BOOL: BOOLEAN);
                   INTTYPE: (INT: INTEGER);
                   IDTYPE: (ID: IDENTIFIER);
                   PTRTYPE: (PTR: PASSPTR)
               END;

ARGLIST = ARRAY (.1..MAXARG.) OF ARGTYPE;

ENTRY_KIND=(INDEX_CONST,REAL_CONST,STRING_CONST,VARIABLE,
    PARAMETER,FIELD,SCALAR_KIND,SYSCOMP_KIND,ROUTINE_KIND,SET_KIND,
    PROGRAM_KIND,POINTER_KIND,ARRAY_KIND,RECORD_KIND,WITH_KIND,UNDEF_KIND);
OPERAND_CLASS=(VAR_CLASS,ROUTINE_CLASS,ICONST_CLASS,RCONST_CLASS,SCONST_CLASS,
    DEF_CLASS,UNDEF_CLASS,FCONST_CLASS,PROGRAM_CLASS,CASE_LABEL,
    FUNCVALUE_CLASS);

ERROR_NOTE=(YES,NO,SUPPRESS);

SPELLING_INDEX=0..SPELLING_MAX;
NOUN_INDEX = 0..NOUN_MAX;
STACK_INDEX=-1{0} ..OPERAND_MAX;
UNIV_SET = ARRAY (.1..8.) OF INTEGER;
UPDATE_INDEX=-1 {0}..UPDATE_MAX;
NAME_PTR=^NAME_REC;
VARIANT_PTR=^VARIANT_REC;

ENTRY_PTR=^ENTRY_REC;
ENTRY_REC=
    RECORD
      NOUN:integer{NOUN_INDEX};
      CASE KIND:ENTRY_KIND OF
        INDEX_CONST:(CONST_TYPE:NOUN_INDEX; CONST_VAL:INTEGER);
        REAL_CONST:(REAL_DISP:INTEGER);
        STRING_CONST:(STRING_LENGTH,STRING_DISP:INTEGER);
        VARIABLE:(VAR_TYPE:ENTRY_PTR);
        PARAMETER:(PARM_TYPE,NEXT_PARM:ENTRY_PTR);
        FIELD:(FIELD_TYPE:ENTRY_PTR; VARIANT:VARIANT_PTR);
        SCALAR_KIND:(RANGE_TYPE:NOUN_INDEX);
        SYSCOMP_KIND:(INIT_STAT:ENTRY_PTR; ENTRY_NAME:NAME_PTR);
        ROUTINE_KIND:(ROUT_PARM:ENTRY_PTR; ROUT_TYPE:NOUN_INDEX);
        PROGRAM_KIND:(PROG_PARM:ENTRY_PTR; INTERFACE1:NAME_PTR);
        POINTER_KIND:(OBJECT_TYPE:ENTRY_PTR);
        ARRAY_KIND:(INDEX_TYPE:NOUN_INDEX; EL_TYPE:ENTRY_PTR);
        WITH_KIND:(WITH_TYPE:NOUN_INDEX);
        RECORD_KIND:(FIELD_NAME:NAME_PTR)
    END;

  OPERAND=
    RECORD
      CASE CLASS1:OPERAND_CLASS OF
        VAR_CLASS:(VTYPE:ENTRY_PTR);
        PROGRAM_CLASS:(PROG,PPARM:ENTRY_PTR);
        ROUTINE_CLASS:(ROUT,PARM:ENTRY_PTR);
        FUNCVALUE_CLASS:(FUNC_TYPE:NOUN_INDEX);
        ICONST_CLASS:(ICONST_TYPE:NOUN_INDEX; ICONST_VAL:INTEGER);
        RCONST_CLASS:(RCONST_DISP:INTEGER);
        SCONST_CLASS:(SCONST_LENGTH,SCONST_DISP:INTEGER);
        CASE_LABEL:(LABEL1,INDEX:INTEGER);
        DEF_CLASS:(DEF_ENTRY:ENTRY_PTR; DEF_SPIX:SPELLING_INDEX)
    END;

  NAME_ACCESS=(GENERAL,EXTERNAL,INTERNAL,INCOMPLETE,
    UNRESOLVED,QUALIFIED,FUNCTIONAL,UNDEFINED);

  LEVEL_INDEX=0..MAX_LEVEL;

  SPELLING_ENTRY=
    RECORD
      ENTRY:ENTRY_PTR;
      LEVEL:LEVEL_INDEX;
      ACCESS:NAME_ACCESS
    END;

  DISPLAY_REC=
    RECORD
      BASE:0..UPDATE_MAX1;
      LEVEL_ENTRY:ENTRY_PTR;
      PREV_SYSCOMP:LEVEL_INDEX;
      PREV_LIST: NAME_PTR
    END;

  UPDATE_REC=
    RECORD
      UPDATE_SPIX:SPELLING_INDEX;
      OLD_ENTRY:SPELLING_ENTRY
    END;

  PACKED_SET=INTEGER;

  VARIANT_REC=
    RECORD
      TAG_DISP:INTEGER;
      LABEL_SET:PACKED_SET;
      NEXT_VARIANT:VARIANT_PTR
    END;

  NAME_REC=
    RECORD
      NAME_SPIX:SPELLING_INDEX;
      NAME_ENTRY:ENTRY_PTR;
      NEXT_NAME:NAME_PTR
    END;

VAR
  PARAMETERIZED,CONSTANTS: SET OF OPERAND_CLASS;

  QUALIFIABLE,TYPES,CONST_KINDS: SET OF ENTRY_KIND;
  NAME_LIST, OLD_NAMES: NAME_PTR;
  HALT, RESOLUTION: BOOLEAN;
  OPS:ARRAY (.STACK_INDEX.) OF OPERAND;
  UENTRY,FIRST_PARM,THIS_PARM: ENTRY_PTR;
  COMP_MODES,ENTRY_MODES: SET OF CLASS_MODE..PROGRAM_MODE;
  INACCESSIBLE,ENTRY_ACCESS,OP_ACCESS: SET OF NAME_ACCESS;
  LABELS: ARRAY (.MIN_CASE..MAX_CASE.) OF INTEGER;
  THIS_UPDATE: -1..UPDATE_MAX{UPDATE_INDEX};
  T:-1..OPERAND_MAX{STACK_INDEX};
  ENUM_VAL,THIS_LABEL,SY,CONST_DISP, UNRES_COUNT: INTEGER;
  ENUM_TYPE,THIS_NOUN: NOUN_INDEX;
  UPDATES:ARRAY (.UPDATE_INDEX.) OF UPDATE_REC;
  DISPLAY:ARRAY (.LEVEL_INDEX.) OF DISPLAY_REC;
  SYSCOMP_LEVEL,THIS_LEVEL,BODY_LEVEL: LEVEL_INDEX;
  SPELLING_TABLE:ARRAY (.SPELLING_INDEX.) OF SPELLING_ENTRY;



{NOTE: A PASS RUNNING WITH TEST OUTPUT SHOULD START
 BY CALLING PROCEDURE PRINTFF}
{#############}
{PASS ROUTINES}
{#############}
  PROCEDURE PUT_ARG(ARG:INTEGER);
  BEGIN
    ADD (arg, outfile3);
    IF TEST THEN PRINTARG(ARG)
  END;

  PROCEDURE PUT0(OP:INTEGER);
  BEGIN
    ADD (op,outfile3);
    IF TEST THEN PRINTOP(OP)
  END;
  PROCEDURE PUT1(OP,ARG:INTEGER);
  BEGIN
    ADD (op, outfile3);
    ADD (arg, outfile3);
    IF TEST THEN BEGIN
      PRINTOP(OP); PRINTARG(ARG)
    END
  END;
  PROCEDURE PUT2(OP,ARG1,ARG2:INTEGER);
  BEGIN
    ADD (op, outfile3);
    ADD (arg1, outfile3);
    ADD (arg2, outfile3);
    IF TEST THEN BEGIN
      PRINTOP(OP);
      PRINTARG(ARG1); PRINTARG(ARG2)
    END
  END;
  PROCEDURE PUT3(OP,ARG1,ARG2,ARG3:INTEGER);
  BEGIN
    PUT2(OP,ARG1,ARG2);
    PUT_ARG(ARG3)
  END;
  PROCEDURE PUT4(OP,ARG1,ARG2,ARG3,ARG4:INTEGER);
  BEGIN
    PUT3(OP,ARG1,ARG2,ARG3); PUT_ARG(ARG4)
  END;
  PROCEDURE IGNORE1(OP:INTEGER);
  VAR
   ARG: INTEGER;
  BEGIN
    get_l(outfile2, arg);
    PUT1(OP,ARG)
  END;
  PROCEDURE IGNORE2(OP:INTEGER);
  VAR ARG1,ARG2:INTEGER;
  BEGIN
    get_l(outfile2,arg1);
    get_l(outfile2, arg2);
    PUT2(OP,ARG1,ARG2)
  END;
  PROCEDURE IGNORE3(OP:INTEGER);
  VAR ARG1,ARG2,ARG3:INTEGER;
  BEGIN
    get_l(outfile2,arg1);
    get_l(outfile2,arg2);
    get_l(outfile2,arg3);
    PUT3(OP,ARG1,ARG2,ARG3)
  END;

  PROCEDURE LCONST;
  VAR LENGTH,I,ARG:INTEGER;
  BEGIN
     get_l(outfile2,LENGTH);
     PUT1(LCONST2,LENGTH);
    CONST_DISP:=CONST_DISP+LENGTH;
    FOR I:=1 TO LENGTH {DIV 2} DO BEGIN
      get_l(outfile2,arg); PUT_ARG(ARG)
    END
  END;

  PROCEDURE ERROR(NUMBER:INTEGER);
  BEGIN
    {PUT2(MESSAGE2,THIS_PASS,NUMBER);}
     write(errors, ' ',MESSAGE2,' ',THIS_PASS,' ',NUMBER);
     writeln(errors);
     PUT2(MESSAGE2,THIS_PASS,NUMBER);
  END;

  PROCEDURE ABORT;
  BEGIN
    ERROR(ABORT_ERROR); HALT:=TRUE
  END;

 {##############}
{INITIALIZATION}
{##############}
  PROCEDURE STD_ID(VAR STD_ENTRY:ENTRY_PTR; INDEX:SPELLING_INDEX);
  BEGIN
    NEW(STD_ENTRY); STD_ENTRY^.NOUN:=INDEX;
    WITH SPELLING_TABLE(.INDEX.) DO BEGIN
      ENTRY:=STD_ENTRY;
      LEVEL:=STD_LEVEL;
      ACCESS:=GENERAL
    END
  END;

  PROCEDURE STD_CONST(CONST_INDEX,TYPE_INDEX:SPELLING_INDEX;
    CONST_VALUE:INTEGER);
  VAR CONST_ENTRY:ENTRY_PTR;
  BEGIN
    STD_ID(CONST_ENTRY,CONST_INDEX);
    WITH CONST_ENTRY^ DO BEGIN
      KIND:=INDEX_CONST;
      CONST_TYPE:=TYPE_INDEX;
      CONST_VAL:=CONST_VALUE
    END
  END;

  PROCEDURE STD_PARM(VAR PARM_ENTRY:ENTRY_PTR; PARMTYPE:ENTRY_PTR;
    PARM_INDEX:NOUN_INDEX);
  BEGIN
    NEW(PARM_ENTRY);
    WITH PARM_ENTRY^ DO BEGIN
      NOUN:=PARM_INDEX;
      KIND:=PARAMETER;
      PARM_TYPE:=PARMTYPE;
      NEXT_PARM:=NIL
    END
  END;

  PROCEDURE STD_ENTRY(VAR E:ENTRY_PTR; INDEX:NOUN_INDEX);
  BEGIN
    NEW(E);
    WITH E^ DO BEGIN
      NOUN:=INDEX;
      KIND:=UNDEF_KIND
    END
  END;

  PROCEDURE STD_ROUT(ROUT_INDEX,ROUTTYPE:NOUN_INDEX; FIRST_PARM:ENTRY_PTR);
  VAR ROUT_ENTRY:ENTRY_PTR;
  BEGIN
    STD_ID(ROUT_ENTRY,ROUT_INDEX);
    WITH ROUT_ENTRY^ DO BEGIN
      KIND:=ROUTINE_KIND;
      ROUT_PARM:=FIRST_PARM;
      ROUT_TYPE:=ROUTTYPE
    END
  END;

  PROCEDURE STD_SCALAR(VAR SCALAR_ENTRY:ENTRY_PTR; SCALAR_INDEX:SPELLING_INDEX);
  BEGIN
    STD_ID(SCALAR_ENTRY,SCALAR_INDEX);
    WITH SCALAR_ENTRY^ DO BEGIN
      KIND:=SCALAR_KIND;
      RANGE_TYPE:=SCALAR_INDEX
    END
  END;

 PROCEDURE INITIALIZE;
 VAR I:INTEGER; INT_TYPE,REAL_TYPE,BOOL_TYPE,CHAR_TYPE,QUEUE_TYPE,
    INDEX_TYPE,ARITH_TYPE,PASSIVE_TYPE,ARITH_SPARM,INT_CPARM,QUEUE_VPARM,
    PAS2_VPARM,PAS1_VPARM,CHAR_CPARM,INDEX_CPARM,INDEX1_CPARM,REAL_CPARM,
    INDEX_SPARM,QUEUE_CPARM: ENTRY_PTR;
  BEGIN
   TEST:=true;
   FIRST(outfile2);
   INIT (outfile3);
   IF TEST THEN PRINTFF(THIS_PASS);
    THIS_NOUN:=ZWITH;
    CONST_DISP:=0;
    HALT:=FALSE; RESOLUTION:=FALSE;
    UNRES_COUNT:= 0;
    PARAMETERIZED:=(.ROUTINE_CLASS,PROGRAM_CLASS.);
    COMP_MODES:=(.CLASS_MODE,MONITOR_MODE,PROCESS_MODE.);
    ENTRY_MODES:=(.PROCE_MODE,FUNCE_MODE.);
    QUALIFIABLE:=(.SYSCOMP_KIND,RECORD_KIND.);
    CONSTANTS:=(.ICONST_CLASS,RCONST_CLASS,SCONST_CLASS.);
    TYPES:=(.SCALAR_KIND,SYSCOMP_KIND,ARRAY_KIND,RECORD_KIND,SET_KIND,
      UNDEF_KIND.);
    OP_ACCESS:=(.GENERAL,INTERNAL,QUALIFIED,FUNCTIONAL.);
    CONST_KINDS:=(.INDEX_CONST,REAL_CONST,STRING_CONST.);
    INACCESSIBLE:=(.UNDEFINED,INCOMPLETE.);
    ENTRY_ACCESS:=(.EXTERNAL,UNRESOLVED.);
    THIS_UPDATE:=-1;
    T:=-1;
    THIS_LEVEL:=STD_LEVEL;
    FOR I:=0 TO SPELLING_MAX DO
      SPELLING_TABLE(.I.).ACCESS:=UNDEFINED;
    {STANDARD ENTRYS}
    STD_CONST(XFALSE,XBOOLEAN,0);
    STD_CONST(XTRUE,XBOOLEAN,1);
    STD_ENTRY(UENTRY,XUNDEF);
    STD_ENTRY(INDEX_TYPE,ZINDEX);
    STD_ENTRY(ARITH_TYPE,ZARITHMETIC);
    STD_ENTRY(PASSIVE_TYPE,ZPASSIVE);
    STD_ID(QUEUE_TYPE,XQUEUE); QUEUE_TYPE^.KIND:=UNDEF_KIND;
    STD_SCALAR(INT_TYPE,XINTEGER);
    STD_SCALAR(REAL_TYPE,XREAL);
    STD_SCALAR(BOOL_TYPE,XBOOLEAN);
    STD_SCALAR(CHAR_TYPE,XCHAR);
    STD_PARM(ARITH_SPARM,ARITH_TYPE,ZSPARM);
    STD_PARM(INT_CPARM,INT_TYPE,ZCPARM);
    STD_PARM(QUEUE_CPARM,QUEUE_TYPE,ZCPARM);
    STD_PARM(QUEUE_VPARM,QUEUE_TYPE,ZVPARM);
    STD_PARM(CHAR_CPARM,CHAR_TYPE,ZCPARM);
    STD_PARM(INDEX_CPARM,INDEX_TYPE,ZCPARM);
    STD_PARM(INDEX_SPARM,INDEX_TYPE,ZSPARM);
    STD_PARM(PAS2_VPARM,PASSIVE_TYPE,ZVPARM);
    PAS2_VPARM^.NEXT_PARM:=INDEX_CPARM;
    STD_PARM(PAS1_VPARM,PASSIVE_TYPE,ZVPARM);
    PAS1_VPARM^.NEXT_PARM:=PAS2_VPARM;
    STD_PARM(INDEX1_CPARM,INDEX_TYPE,ZCPARM);
    INDEX1_CPARM^.NEXT_PARM:= INDEX_CPARM;
    STD_PARM(REAL_CPARM,REAL_TYPE,ZCPARM);
    STD_ROUT(XABS,ZARITHMETIC,ARITH_SPARM);
    STD_ROUT(XATTRIBUTE,XINTEGER,INDEX_CPARM);
    STD_ROUT(XCHR,XCHAR,INT_CPARM);
    STD_ROUT(XCONTINUE,PROC_TYPE,QUEUE_VPARM);
    STD_ROUT(XCONV,XREAL,INT_CPARM);
    STD_ROUT(XDELAY,PROC_TYPE,QUEUE_VPARM);
    STD_ROUT(XEMPTY,XBOOLEAN,QUEUE_CPARM);
    STD_ROUT(XIO,PROC_TYPE,PAS1_VPARM);
    STD_ROUT(XORD,XINTEGER,CHAR_CPARM);
    STD_ROUT(XPRED,ZINDEX,INDEX_SPARM);
    STD_ROUT(XSTOP,PROC_TYPE,INDEX1_CPARM);
    STD_ROUT(XREALTIME,XINTEGER,NIL);
    STD_ROUT(XSETHEAP,PROC_TYPE,INT_CPARM);
    STD_ROUT(XSUCC,ZINDEX,INDEX_SPARM);
    STD_ROUT(XTRUNC,XINTEGER,REAL_CPARM);
    STD_ROUT(XSTART,PROC_TYPE,NIL);
    STD_ROUT(XWAIT,PROC_TYPE,NIL);
   END;

 {NESTING }
 {"#######}

  PROCEDURE PUSH_LEVEL(E:ENTRY_PTR);
  BEGIN
    IF THIS_LEVEL>=MAX_LEVEL THEN ABORT ELSE THIS_LEVEL:=THIS_LEVEL+1;
    WITH DISPLAY(.THIS_LEVEL.) DO BEGIN
      BASE:=THIS_UPDATE+1;
      LEVEL_ENTRY:=E;
      PREV_SYSCOMP:=SYSCOMP_LEVEL;
      PREV_LIST:=NAME_LIST; NAME_LIST:=NIL
    END
  END;

  PROCEDURE POP_LEVEL;
  VAR U:UPDATE_INDEX;
      i:integer;
  BEGIN
    WITH DISPLAY (.THIS_LEVEL.) DO BEGIN
      SYSCOMP_LEVEL:=PREV_SYSCOMP;
      NAME_LIST:=PREV_LIST;
      FOR U:=THIS_UPDATE DOWNTO BASE DO
        WITH UPDATES(.U.) DO
          SPELLING_TABLE(.UPDATE_SPIX.):=OLD_ENTRY;
      THIS_UPDATE:=BASE-1
    END;
    THIS_LEVEL:=THIS_LEVEL-1
  END;

{#############}
{NAME HANDLING}
{#############}
  PROCEDURE PUSH;
  BEGIN
    IF T>= OPERAND_MAX THEN ABORT ELSE
    T:=T+1
  END;

  PROCEDURE NEW_ENTRY(VAR E:ENTRY_PTR);
  BEGIN
    IF THIS_NOUN>=NOUN_MAX THEN ABORT ELSE
    THIS_NOUN:=THIS_NOUN+1;
    NEW(E);
    WITH E^ DO BEGIN
      NOUN:=THIS_NOUN; KIND:=UNDEF_KIND
    END
  END;

  PROCEDURE PUSH_NEW_ENTRY(VAR E:ENTRY_PTR);
  BEGIN
    PUSH;
    NEW_ENTRY(E);
    WITH OPS(.T.) DO BEGIN
      CLASS1:=DEF_CLASS;
      DEF_ENTRY:=E; DEF_SPIX:=XUNDEF
    END
  END;

  PROCEDURE UPDATE(SPIX:SPELLING_INDEX; E:ENTRY_PTR; A:NAME_ACCESS);
  BEGIN
    IF THIS_LEVEL<>GLOBAL_LEVEL THEN
    BEGIN
      {SAVE OLD ENTRY}
      IF THIS_UPDATE>=UPDATE_MAX THEN ABORT ELSE
      THIS_UPDATE:=THIS_UPDATE+1;
      WITH UPDATES(.THIS_UPDATE.) DO BEGIN
        UPDATE_SPIX:=SPIX;
        OLD_ENTRY:=SPELLING_TABLE(.SPIX.)
      END
    END;
    WITH SPELLING_TABLE(.SPIX.) DO BEGIN
      ENTRY:=E; LEVEL:=THIS_LEVEL; ACCESS:=A
    END
  END;

  PROCEDURE PUSH_NEW_NAME(RESOLVE,OUTPUT:BOOLEAN; A:NAME_ACCESS);
  VAR SPIX:{SPELLING_INDEX}integer; E:ENTRY_PTR;
  BEGIN
    GET_L(outfile2, SPIX);
    IF SPIX<>XUNDEF
     THEN
      WITH SPELLING_TABLE(.SPIX.) DO
        IF (ACCESS<>UNDEFINED) AND (LEVEL=THIS_LEVEL)
        THEN
          IF RESOLVE AND (ACCESS=UNRESOLVED)
          THEN
           BEGIN
             E:=ENTRY; ACCESS:= A; RESOLUTION:= TRUE;
             UNRES_COUNT:= UNRES_COUNT - 1
           END
          ELSE
           BEGIN
             ERROR(AMBIGUITY_ERROR); SPIX:=XUNDEF;
           END
        ELSE
         BEGIN
          NEW_ENTRY(E);
          UPDATE(SPIX,E,A)
         END;
    PUSH;
    WITH OPS(.T.) DO
      IF SPIX=XUNDEF THEN BEGIN
        CLASS1:=UNDEF_CLASS;
        IF OUTPUT THEN PUT1(NEW_NOUN2,XUNDEF)
      END ELSE BEGIN
        CLASS1:=DEF_CLASS; DEF_ENTRY:=E; DEF_SPIX:=SPIX;
        IF OUTPUT THEN PUT1(NEW_NOUN2,E^.NOUN)
      END
  END;

  PROCEDURE PUSH_OLD_NAME;
  VAR SPIX:{SPELLING_INDEX} integer;
  BEGIN
    PUSH;  GET_L(outfile2, SPIX);
    WITH OPS(.T.),SPELLING_TABLE(.SPIX.) DO
      IF ACCESS IN INACCESSIBLE THEN BEGIN
        ERROR(NAME_ERROR);
        CLASS1:=UNDEF_CLASS
      END ELSE BEGIN
        CLASS1:=DEF_CLASS;
        DEF_ENTRY:=ENTRY; DEF_SPIX:=SPIX
      END
  END;

  PROCEDURE FIND_NAME(LIST:NAME_PTR; SPIX:SPELLING_INDEX; VAR E:ENTRY_PTR);
  VAR NAME:NAME_PTR;
  BEGIN
    E:=NIL; NAME:=LIST;
    WHILE NAME<>NIL DO
      WITH NAME^ DO
        IF NAME_SPIX=SPIX THEN BEGIN
          E:=NAME_ENTRY; NAME:=NIL
        END ELSE NAME:=NEXT_NAME;
    IF E=NIL THEN BEGIN
      ERROR(NAME_ERROR);
      E:=UENTRY
    END
  END;

  PROCEDURE CHAIN_NAME(E:ENTRY_PTR; SPIX:SPELLING_INDEX);
  VAR N:NAME_PTR;
  BEGIN
    NEW(N);
    WITH N^ DO BEGIN
      NAME_SPIX:=SPIX;
      NAME_ENTRY:=E;
      NEXT_NAME:=NAME_LIST; NAME_LIST:=N
    END
  END;

  PROCEDURE SET_ACCESS(SPIX:SPELLING_INDEX; A:NAME_ACCESS);
  BEGIN
    SPELLING_TABLE(.SPIX.).ACCESS:=A;
    T:=T-1
  END;

  PROCEDURE ENTER_NAMES(LIST:NAME_PTR);
  VAR THIS_NAME:NAME_PTR;
  BEGIN
    THIS_NAME:=LIST;
    WHILE THIS_NAME<>NIL DO
      {WITH THIS_NAME DO}  BEGIN
        UPDATE(This_NAME.NAME_SPIX,This_NAME.NAME_ENTRY,QUALIFIED);
        THIS_NAME:=This_NAME.NEXT_NAME
      END
  END;

  FUNCTION DEFINED:BOOLEAN;
  BEGIN
    DEFINED:=OPS(.T.).CLASS1<>UNDEF_CLASS
  END;

  FUNCTION TOP:ENTRY_PTR;
  BEGIN
    TOP:=OPS(.T.).DEF_ENTRY
  END;

{"#####################}
{CONSTANT DECLARATIONS}
{#####################}
  PROCEDURE CONST_ID;
  BEGIN
    PUSH_NEW_NAME(NOT_POSSIBLY_FORWARD,RETAIN,INCOMPLETE);
    IF DEFINED THEN THIS_NOUN:=THIS_NOUN-1{"CONST IDS DON'T POSSESS NOUNS}
  END;

  PROCEDURE CONST_DEF;
  BEGIN
    WITH OPS(.T-1.) DO
      IF CLASS1=DEF_CLASS THEN BEGIN
        WITH DEF_ENTRY^, OPS(.T.) DO
          IF CLASS1 IN CONSTANTS THEN
            CASE CLASS1 OF
              ICONST_CLASS: BEGIN
                KIND:=INDEX_CONST;
                CONST_TYPE:=ICONST_TYPE; CONST_VAL:=ICONST_VAL
              END;
              RCONST_CLASS: BEGIN
                KIND:=REAL_CONST; REAL_DISP:=RCONST_DISP
              END;
              SCONST_CLASS: BEGIN
                KIND:=STRING_CONST;
                STRING_LENGTH:=SCONST_LENGTH;
                STRING_DISP:=SCONST_DISP
              END
            END
          ELSE ERROR(CONSTID_ERROR);
        T:=T-1; SET_ACCESS(DEF_SPIX,GENERAL)
      END ELSE T:=T-2
  END;

{#################}
{TYPE DECLARATIONS}
{#################}

  PROCEDURE TYPE_ID;
  VAR SPIX:{SPELLING_INDEX}integer;
  BEGIN
    GET_L(outfile2, spix);
    WITH SPELLING_TABLE(.SPIX.) DO BEGIN
      IF (ACCESS<>UNDEFINED) AND (LEVEL=THIS_LEVEL) THEN BEGIN
        SPIX:=XUNDEF;
        ERROR(AMBIGUITY_ERROR)
      END ELSE UPDATE(SPIX,NIL,INCOMPLETE)
    END;
    PUSH;
    WITH OPS(.T.) DO
      IF SPIX=XUNDEF THEN CLASS1:=UNDEF_CLASS ELSE BEGIN
        CLASS1:=DEF_CLASS; DEF_SPIX:=SPIX
      END
  END;

  PROCEDURE TYPE_DEF;
  BEGIN
    WITH OPS(.T-1.) DO
      IF CLASS1=DEF_CLASS THEN
        WITH SPELLING_TABLE(.DEF_SPIX.) DO BEGIN
          IF DEFINED THEN ENTRY:=TOP ELSE ENTRY:=UENTRY;
          ACCESS:=GENERAL
        END;
    T:=T-2; PUT0(TYPE_DEF2)
  END;

  PROCEDURE TYPE_(OUTPUT:BOOLEAN; OP:INTEGER);
  var
   X:ENTRY_KIND;
  BEGIN
    PUSH_OLD_NAME;
    IF DEFINED THEN
     begin
     X:=TOP^.KIND;
      IF NOT({TOP^.KIND} X IN TYPES) THEN BEGIN
        ERROR(NAME_ERROR); OPS(.T.).CLASS1:=UNDEF_CLASS
      END;
     end;
    IF OUTPUT THEN
      IF DEFINED THEN PUT1(OP,TOP^.NOUN)
        ELSE PUT1(OP,XUNDEF)
  END;

  PROCEDURE ENUM_ID;
  BEGIN
    PUSH_NEW_NAME(NOT_POSSIBLY_FORWARD,RETAIN,GENERAL);
    IF DEFINED THEN BEGIN
      THIS_NOUN:=THIS_NOUN-1; {CONST IDS DON'T HAVE NOUNS}
      WITH TOP^ DO BEGIN
          KIND:=INDEX_CONST;
          CONST_TYPE:=ENUM_TYPE;
          ENUM_VAL:=ENUM_VAL+1; CONST_VAL:=ENUM_VAL
      END
    END;
    T:=T-1
  END;

  PROCEDURE ENUM;
  VAR E:ENTRY_PTR;
  BEGIN
    PUSH_NEW_ENTRY(E);
    ENUM_VAL:=-1;
    WITH E^ DO BEGIN
      KIND:=SCALAR_KIND;
      RANGE_TYPE:=NOUN; ENUM_TYPE:=NOUN
    END
  END;

  PROCEDURE SUBR_DEF;
  VAR MIN,MAX:INTEGER; TYPE1:NOUN_INDEX; E:ENTRY_PTR;
  BEGIN
    MIN:=0; MAX:=1; TYPE1:=XUNDEF;
    WITH OPS(.T.) DO
      IF CLASS1=ICONST_CLASS THEN BEGIN
        MAX:=ICONST_VAL; TYPE1:=ICONST_TYPE
      END ELSE ERROR(SUBR_ERROR);
    WITH OPS(.T-1.) DO
      IF CLASS1=ICONST_CLASS THEN BEGIN
        MIN:=ICONST_VAL;
        IF (MIN>MAX) OR (ICONST_TYPE<>TYPE1) THEN ERROR(SUBR_ERROR)
      END ELSE ERROR(SUBR_ERROR);
    T:=T-2;
    PUSH_NEW_ENTRY(E);
    WITH E^ DO BEGIN
      KIND:=SCALAR_KIND;
      RANGE_TYPE:=TYPE1;
      PUT4(SUBR_DEF2,NOUN,TYPE1,MIN,MAX)
    END
  END;

  PROCEDURE SET_DEF;
  VAR E:ENTRY_PTR;
  BEGIN
    T:=T-1;
    PUSH_NEW_ENTRY(E); E^.KIND:=SET_KIND;
    PUT1(SET_DEF2,E^.NOUN)
  END;

  PROCEDURE ARRAY_DEF;
  VAR INDEX:NOUN_INDEX; E,EL:ENTRY_PTR;
  BEGIN
    IF DEFINED THEN EL:=TOP ELSE EL:=UENTRY;
    T:=T-1;
    IF DEFINED THEN INDEX:=TOP^.NOUN ELSE INDEX:=XUNDEF;
    T:=T-1;
    PUSH_NEW_ENTRY(E);
    WITH E^ DO BEGIN
      KIND:=ARRAY_KIND;
      INDEX_TYPE:=INDEX;
      EL_TYPE:=EL;
      PUT1(ARRAY_DEF2,NOUN)
    END
  END;

  PROCEDURE REC;
  VAR E:ENTRY_PTR;
  BEGIN
    PUT0(REC2);
    PUSH_NEW_ENTRY(E);
    PUSH_LEVEL(E)
  END;

  PROCEDURE FIELD_LIST;
  VAR I,NUMBER:INTEGER; TYP:ENTRY_PTR;
  BEGIN
   GET_L(outfile2, Number);
    IF DEFINED THEN TYP:=TOP ELSE TYP:=UENTRY;
    T:=T-1;
    FOR I:=1 TO NUMBER DO
      WITH OPS(.T.) DO
      IF DEFINED THEN BEGIN
        WITH DEF_ENTRY^ DO BEGIN
          KIND:=FIELD;
          FIELD_TYPE:=TYP
        END;
        CHAIN_NAME(DEF_ENTRY,DEF_SPIX);
        SET_ACCESS(DEF_SPIX,INTERNAL)
       END ELSE T:=T-1;
    PUT1(FIELDLIST2,NUMBER)
  END;

  PROCEDURE REC_DEF;

  BEGIN
    WITH TOP^ DO BEGIN
      KIND:=RECORD_KIND;
      FIELD_NAME:=NAME_LIST;
      PUT1(REC_DEF2,NOUN)
    END;
    POP_LEVEL
  END;

  PROCEDURE COMP_DEF(OP:INTEGER);
  VAR E:ENTRY_PTR;
  BEGIN
    SYSCOMP_LEVEL:=THIS_LEVEL;
    WITH TOP^ DO BEGIN
      KIND:=SYSCOMP_KIND;
      PUSH_NEW_ENTRY(E) {INITIAL STATEMENT};
      INIT_STAT:=E;
      PUT2(OP,NOUN,E^.NOUN)
    END;
    WITH E^ DO BEGIN
      KIND:=ROUTINE_KIND;
      ROUT_PARM:=FIRST_PARM; ROUT_TYPE:=PROC_TYPE
    END;
    T:=T-1
  END;

  PROCEDURE INITS_DEF;
  BEGIN
    PUT0(INITS_DEF2);
    TOP^.ENTRY_NAME:=NAME_LIST;
  END;

{#####################}
{VARIABLE DECLARATIONS}
{#####################}
  PROCEDURE VAR_LIST(OP:INTEGER);
  VAR I,NUMBER:INTEGER; TYP:ENTRY_PTR;
  BEGIN
    GET_L(outfile2, NUMBER);
    PUT1(OP,NUMBER);
    IF DEFINED THEN
     TYP:=TOP
      ELSE
     TYP:=UENTRY;
    T:=T-1;
    FOR I:=1 TO NUMBER DO
      WITH OPS(.T.) DO
       IF DEFINED THEN BEGIN
        WITH DEF_ENTRY^ DO BEGIN
          KIND:=VARIABLE;
          VAR_TYPE:=TYP
        END;
        IF OP=EVAR_LIST2 THEN
         CHAIN_NAME(DEF_ENTRY,DEF_SPIX);
        SET_ACCESS(DEF_SPIX,INTERNAL)
       END ELSE T:=T-1
  END;

{###################}
{ROUTINE DECLARATIONS}
{###################}
  PROCEDURE PROC_DEF(OP:INTEGER);
  BEGIN
    IF DEFINED THEN
      WITH TOP^ DO BEGIN
          KIND:=ROUTINE_KIND;
          ROUT_PARM:=FIRST_PARM;
          ROUT_TYPE:=PROC_TYPE;
          IF RESOLUTION THEN BEGIN
            RESOLUTION:=FALSE; PUT1(PROCF_DEF2,NOUN)
          END ELSE PUT1(OP,NOUN)
        END
      ELSE PUT1(OP,XUNDEF)
  END;

  PROCEDURE FUNC_DEF(OP:INTEGER);
  CONST NO_OUTPUT=FALSE; NOOP=0;
  VAR TYP:NOUN_INDEX;
  BEGIN
    TYPE_(NO_OUTPUT,NOOP);
    IF DEFINED THEN TYP:=TOP^.NOUN ELSE TYP:=XUNDEF;
    T:=T-1;
    IF DEFINED THEN
      WITH TOP^ DO BEGIN
          KIND:=ROUTINE_KIND;
          ROUT_PARM:=FIRST_PARM;
          ROUT_TYPE:=TYP;
          IF RESOLUTION THEN BEGIN
            RESOLUTION:=FALSE; PUT2(FUNCF_DEF2,TYP,NOUN)
          END ELSE PUT2(OP,TYP,NOUN)
        END
      ELSE PUT2(OP,XUNDEF,XUNDEF)
  END;

  PROCEDURE ROUT_END(A:NAME_ACCESS);
  BEGIN
    IF DEFINED THEN SET_ACCESS(OPS(.T.).DEF_SPIX,A) ELSE T:=T-1;
    POP_LEVEL;
  END;

  PROCEDURE PROG_DEF;
  BEGIN
    WITH OPS(.T.) DO BEGIN
      IF DEFINED THEN BEGIN
        WITH DEF_ENTRY^ DO BEGIN
          KIND:=PROGRAM_KIND;
          PROG_PARM:=FIRST_PARM;
          INTERFACE1:=NAME_LIST;
          PUT1(PROG_DEF2,NOUN)
        END;
        SET_ACCESS(DEF_SPIX,INTERNAL)
      END ELSE BEGIN PUT1(PROG_DEF2,XUNDEF); T:=T-1 END;
    NAME_LIST:= OLD_NAMES
    END;
  END;

  PROCEDURE INTF_ID;
  VAR SPIX: integer{SPELLING_INDEX}; INTF_ENTRY: ENTRY_PTR;
  BEGIN
    GET_L(outfile2, SPIX);
    IF SPIX<>XUNDEF THEN
    WITH SPELLING_TABLE(.SPIX.) DO
      IF (ACCESS<>UNDEFINED) AND (LEVEL=SYSCOMP_LEVEL) THEN
        IF ACCESS IN ENTRY_ACCESS THEN
          CHAIN_NAME(ENTRY,SPIX)
        ELSE ERROR(INTERFACE_ERROR)
      ELSE BEGIN{"FORWARD REFERENCE}
        NEW_ENTRY(INTF_ENTRY); PUT1(FWD_DEF2, INTF_ENTRY^.NOUN);
        CHAIN_NAME(INTF_ENTRY, SPIX);
        UPDATE(SPIX, INTF_ENTRY, UNRESOLVED);
        UNRES_COUNT:= UNRES_COUNT + 1
      END
  END;

  PROCEDURE PSTART;
  VAR M:INTEGER; E:ENTRY_PTR;
  BEGIN
    get_l(outfile2,M);
    PUT1(PSTART2,M);
    IF M IN COMP_MODES THEN PUSH_NEW_ENTRY(E)
    ELSE IF M IN ENTRY_MODES THEN
      IF DEFINED THEN
        WITH OPS(.T.) DO CHAIN_NAME(DEF_ENTRY,DEF_SPIX);
    IF DEFINED THEN E:=TOP ELSE E:=UENTRY;
    PUSH_LEVEL(E);
    FIRST_PARM:=NIL
  END;

  PROCEDURE PARMLIST(OP:INTEGER);
  VAR I,NUMBER:INTEGER; PTYPE:ENTRY_PTR;
  BEGIN
    IF DEFINED THEN PTYPE:=TOP ELSE PTYPE:=UENTRY;
    get_l(outfile2,number);
    PUT1(OP,NUMBER);
    FOR I:=NUMBER DOWNTO 1 DO
      WITH OPS(.T-I.) DO
       IF CLASS1=DEF_CLASS THEN BEGIN
        WITH DEF_ENTRY^ DO BEGIN
          KIND:=PARAMETER;
          PARM_TYPE:=PTYPE;
          IF FIRST_PARM=NIL THEN FIRST_PARM:=DEF_ENTRY
          ELSE THIS_PARM^.NEXT_PARM:=DEF_ENTRY;
          THIS_PARM:=DEF_ENTRY;
          NEXT_PARM:=NIL
        END;
        SPELLING_TABLE(.DEF_SPIX.).ACCESS:=INTERNAL
       END;
    T:=T-NUMBER-1
  END;

{####}
{BODY}
{####}
  PROCEDURE BODY;
  BEGIN
    BODY_LEVEL:=THIS_LEVEL;
    PUT0(BODY2)
  END;

  PROCEDURE ANAME;
  BEGIN
    WITH OPS(.T.) DO
      IF CLASS1=FUNCVALUE_CLASS THEN PUT1(RESULT2,FUNC_TYPE)
      ELSE PUT0(ADDRESS2)
  END;

  PROCEDURE CALL_NAME;
  VAR INTF:NAME_PTR; ERR:BOOLEAN;
  BEGIN
    ERR:=FALSE;
    WITH OPS(.T.) DO BEGIN
      IF CLASS1=PROGRAM_CLASS THEN BEGIN
        PUT0(INTF2);
        INTF:=PROG^.INTERFACE1;
        WHILE INTF<>NIL DO
          WITH INTF^ DO BEGIN
            PUT1(INTF_ID2,NAME_ENTRY^.NOUN);
            INTF:=NEXT_NAME
          END
      END ELSE IF CLASS1=ROUTINE_CLASS THEN
        IF ROUT^.ROUT_TYPE<>PROC_TYPE THEN ERR:=TRUE ELSE {OK}
      ELSE ERR:=TRUE;
      IF ERR THEN BEGIN
        ERROR(CALL_NAME_ERROR);
        CLASS1:=UNDEF_CLASS
      END
    END
  END;

  PROCEDURE CALL(OP:INTEGER);
  BEGIN
    WITH OPS(.T.) DO
      IF CLASS1=ROUTINE_CLASS THEN BEGIN
        IF PARM<>NIL THEN ERROR(FEW_ARGS_ERROR);
        PUT0(OP)
      END ELSE IF CLASS1=PROGRAM_CLASS THEN BEGIN
        IF PPARM<>NIL THEN ERROR(FEW_ARGS_ERROR);
        PUT0(CALL_PROG2)
        END ELSE PUT0(OP);
    IF OP<>CALL_FUNC2 THEN T:=T-1
  END;

  PROCEDURE ARG_LIST;
  BEGIN
    WITH OPS(.T.) DO
      IF CLASS1 IN PARAMETERIZED THEN {OK}
      ELSE BEGIN
        ERROR(ARG_LIST_ERROR);
        CLASS1:=UNDEF_CLASS
      END
  END;

  PROCEDURE ARG;
  VAR THIS_PARM:ENTRY_PTR; ERR:ERROR_NOTE;
  BEGIN
    T:=T-1 {POP ARGUMENT}; ERR:=NO;
    WITH OPS(.T.) DO
      IF CLASS1=ROUTINE_CLASS THEN BEGIN
        THIS_PARM:=PARM;
        IF THIS_PARM=NIL THEN ERR:= YES ELSE PARM:=THIS_PARM^.NEXT_PARM
      END ELSE IF CLASS1=PROGRAM_CLASS THEN BEGIN
        THIS_PARM:=PPARM;
        IF THIS_PARM=NIL THEN ERR:= YES ELSE PPARM:=THIS_PARM^.NEXT_PARM
      END ELSE ERR:=SUPPRESS;
    IF ERR<>NO THEN BEGIN
      IF ERR=YES THEN ERROR(MANY_ARGS_ERROR);
      PUT2(PARM2,XUNDEF,XUNDEF)
    END ELSE
      WITH THIS_PARM^ DO
        PUT2(PARM2,NOUN,PARM_TYPE^.NOUN);
  END;

  PROCEDURE DEF_CASE;
  BEGIN
    get_l (outfile2, THIS_LABEL);
    PUT1(DEF_LABEL2,THIS_LABEL)
  END;

  PROCEDURE CASE_;
  VAR VAL:INTEGER;
  BEGIN
    WITH OPS(.T.) DO
      IF CLASS1=ICONST_CLASS THEN BEGIN
        PUT1(CHK_TYPE2,ICONST_TYPE);
        VAL:=ICONST_VAL;
        CLASS1:=CASE_LABEL;
        LABEL1:=THIS_LABEL;
        IF (VAL>=MIN_CASE) AND (VAL<=MAX_CASE) THEN
          INDEX:=VAL ELSE BEGIN
            ERROR(CASERANGE_ERROR);
            {VAL:=0 }
          END
      END ELSE BEGIN
        T:=T-1;
        ERROR(CASETYPE_ERROR)
      END
  END;

  PROCEDURE END_CASE;
  VAR L0,LN,MIN,MAX,I:INTEGER;
  BEGIN
    get_l(outfile2,L0);
    get_l(outfile2,LN);
       FOR I:=MIN_CASE TO MAX_CASE DO LABELS(.I.):=LN;
    IF OPS(.T.).CLASS1=CASE_LABEL THEN BEGIN
     MIN:=OPS(.T.).INDEX; MAX:=MIN;
    END ELSE BEGIN MIN:=0; MAX:=0 END;
    WHILE OPS(.T.).CLASS1=CASE_LABEL DO BEGIN
        WITH OPS(.T.) DO BEGIN
          IF LABELS(.INDEX.)=LN THEN
            LABELS(.INDEX.):=LABEL1
          ELSE ERROR(AMBICASE_ERROR);
          IF INDEX>MAX THEN MAX:=INDEX ELSE
            IF INDEX<MIN THEN MIN:=INDEX
        END;
        T:=T-1
    END;
      T:=T-1;
      PUT3(CASE_LIST2,L0,MIN,MAX);
      FOR I:=MIN TO MAX DO PUT_ARG(LABELS(.I.));
      PUT_ARG(LN)
  END;

  PROCEDURE WITH_TEMP;
  VAR TEMP:ENTRY_PTR; ERR:BOOLEAN;
  BEGIN
    ERR:=FALSE;
    WITH OPS(.T.) DO
      IF CLASS1=VAR_CLASS THEN
        WITH VTYPE^ DO
          IF KIND IN QUALIFIABLE THEN BEGIN
            NEW_ENTRY(TEMP);
            WITH TEMP^ DO BEGIN
              PUT1(WITH_TEMP2,NOUN);
              KIND:=WITH_KIND;
              WITH_TYPE:=VTYPE^.NOUN
            END;
            PUSH_LEVEL(TEMP);
            IF KIND=RECORD_KIND THEN ENTER_NAMES(FIELD_NAME)
            ELSE ENTER_NAMES(ENTRY_NAME);
          END ELSE ERR:=TRUE
      ELSE ERR:=TRUE;
    IF ERR THEN BEGIN
      ERROR(WITH_ERROR);
      PUSH_LEVEL(UENTRY); PUT1(WITH_TEMP2,XUNDEF)
    END;
    T:=T-1
  END;

  PROCEDURE INIT_NAME;
  VAR ERR:BOOLEAN;
  BEGIN
    ERR:=FALSE;
    WITH OPS(.T.) DO BEGIN
      IF CLASS1=VAR_CLASS THEN
        WITH VTYPE^ DO
          IF KIND=SYSCOMP_KIND THEN BEGIN
            WITH INIT_STAT^ DO BEGIN
              PUT1(RCOMP2,NOUN);
              CLASS1:=ROUTINE_CLASS;
              PARM:=ROUT_PARM
            END;
            ROUT:=INIT_STAT
          END ELSE ERR:=TRUE
      ELSE ERR:=TRUE;
      IF ERR THEN BEGIN
        ERROR(INIT_ERROR);
        CLASS1:=UNDEF_CLASS
      END
    END
  END;

{##########}
{"EXPRESSION}
{##########}
PROCEDURE FNAME;
  VAR TYP:NOUN_INDEX;
  BEGIN
    WITH OPS(.T.) DO
      IF CLASS1=ROUTINE_CLASS THEN
        WITH ROUT^ DO BEGIN
          IF ROUT_TYPE=PROC_TYPE THEN BEGIN
            ERROR(PROC_USE_ERROR);
            TYP:=XUNDEF
          END ELSE TYP:=ROUT_TYPE;
          PUT1(FUNCTION2, TYP);
          IF PARM<>NIL THEN ERROR(FEW_ARGS_ERROR);
          PUT0(CALL_FUNC2)
        END
      ELSE IF CLASS1=FUNCVALUE_CLASS THEN ERROR(NAME_ERROR)
  END;

  PROCEDURE FUNCTION_ERROR(ERROR_NUM:INTEGER);
  BEGIN
    ERROR(ERROR_NUM);
    OPS(.T.).CLASS1:=UNDEF_CLASS
  END;

  PROCEDURE FUNCTION_;
  VAR TYP: NOUN_INDEX;
  BEGIN
    TYP:= XUNDEF;
    WITH OPS(.T.) DO
      IF CLASS1=ROUTINE_CLASS THEN
        WITH ROUT^ DO
          IF ROUT_TYPE = PROC_TYPE THEN
            FUNCTION_ERROR(PROC_USE_ERROR)
          ELSE TYP:= ROUT_TYPE
      ELSE FUNCTION_ERROR(NAME_ERROR);
    PUT1(FUNCTION2, TYP)
  END;

  PROCEDURE BINARY(OP:INTEGER);
  BEGIN
    PUT0(OP);
    T:=T-1
  END;

  PROCEDURE POP2(OP:INTEGER);
  BEGIN
    PUT0(OP);
    T:=T-2
  END;

{########}
{"VARIABLE}
{########}
  PROCEDURE PUSH_OPERAND(OP_ENTRY:ENTRY_PTR; COMP,RESULT:BOOLEAN);
  VAR OP:INTEGER;
  BEGIN
    IF NOT COMP THEN PUSH;
    WITH OPS(.T.) , OP_ENTRY^ DO
      CASE KIND OF
        INDEX_CONST: BEGIN
          CLASS1:=FCONST_CLASS;
          PUT2(INDEX2,CONST_VAL,CONST_TYPE)
        END;
        REAL_CONST: BEGIN
          CLASS1:=FCONST_CLASS;
          PUT1(REAL2,REAL_DISP)
        END;
        STRING_CONST: BEGIN
          CLASS1:=FCONST_CLASS;
          PUT2(STRING2,STRING_LENGTH,STRING_DISP)
        END;
        VARIABLE,FIELD,PARAMETER: BEGIN
          CLASS1:=VAR_CLASS;
          CASE KIND OF
            VARIABLE:VTYPE:=VAR_TYPE;
            FIELD: VTYPE:=FIELD_TYPE;
            PARAMETER: VTYPE:=PARM_TYPE
          END;
          IF COMP THEN OP:=VCOMP2 ELSE OP:=VAR2;
          PUT2(OP,NOUN,VTYPE^.NOUN)
        END;
        ROUTINE_KIND: BEGIN
          IF RESULT THEN BEGIN
            CLASS1:=FUNCVALUE_CLASS;
            FUNC_TYPE:=OP_ENTRY^.ROUT_TYPE
          END ELSE BEGIN
            CLASS1:=ROUTINE_CLASS;
            ROUT:=OP_ENTRY;
            PARM:=ROUT_PARM
          END;
          IF COMP THEN OP:=RCOMP2 ELSE OP:=ROUTINE2;
          PUT1(OP,NOUN)
        END;
        PROGRAM_KIND: BEGIN
          CLASS1:=PROGRAM_CLASS;
          PROG:=OP_ENTRY;
          PPARM:=PROG_PARM;
          PUT1(ROUTINE2,NOUN)
        END;
        SCALAR_KIND,SYSCOMP_KIND,POINTER_KIND,ARRAY_KIND,RECORD_KIND,
        SET_KIND, UNDEF_KIND: BEGIN
          ERROR(NAME_ERROR);
          CLASS1:=UNDEF_CLASS;
          IF NOT COMP THEN PUT0(UNDEF2)
        END
      END
  END;

  PROCEDURE NAME;
  VAR SPIX:integer{SPELLING_INDEX}; COMP,ERR,RESULT:BOOLEAN; NAME_ENTRY:ENTRY_PTR;
  BEGIN
    get_l(outfile2,SPIX);
    ERR:=FALSE; COMP:=FALSE; RESULT:=FALSE;
    WITH SPELLING_TABLE(.SPIX.) DO
      IF ACCESS IN OP_ACCESS THEN BEGIN
        NAME_ENTRY:=ENTRY;
        CASE ACCESS OF
          GENERAL: ;
          FUNCTIONAL: RESULT:=TRUE;
          INTERNAL: IF LEVEL<SYSCOMP_LEVEL THEN ERR:=TRUE;
          QUALIFIED: BEGIN
            COMP:=TRUE; PUSH {WITH TEMP};
            WITH DISPLAY(.LEVEL.).LEVEL_ENTRY^ DO BEGIN
              PUT2(VAR2,NOUN,ZWITH);
              PUT1(ARROW2,WITH_TYPE)
            END
          END
        END
      END ELSE ERR:=TRUE;
    IF ERR THEN BEGIN
      ERROR(NAME_ERROR);
      NAME_ENTRY:=UENTRY
    END;
    PUSH_OPERAND(NAME_ENTRY,COMP,RESULT)
  END;

  PROCEDURE COMP;
  CONST QUALIFIED=TRUE; NOT_RESULT=FALSE;
  VAR SPIX:integer{SPELLING_INDEX}; COMPONENT:ENTRY_PTR; NAME_LIST:NAME_PTR;
    ERR:BOOLEAN;
  BEGIN
    get_l(outfile2,SPIX);
    ERR:=FALSE;
    WITH OPS(.T.) DO
      IF CLASS1=VAR_CLASS THEN BEGIN
        WITH VTYPE^ DO
          IF KIND=RECORD_KIND THEN NAME_LIST:=FIELD_NAME ELSE
          IF KIND=SYSCOMP_KIND THEN NAME_LIST:=ENTRY_NAME
          ELSE BEGIN ERR:=TRUE; NAME_LIST:=NIL END;
        FIND_NAME(NAME_LIST,SPIX,COMPONENT)
      END ELSE ERR:=TRUE;
    IF ERR THEN ERROR(COMP_ERROR)
    ELSE PUSH_OPERAND(COMPONENT,QUALIFIED,NOT_RESULT)
  END;

  PROCEDURE SUB_ERR;
  BEGIN
    ERROR(SUB_ERROR);
    PUT2(SUB2,XUNDEF,XUNDEF)
  END;

  PROCEDURE SUB;
  BEGIN
    T:=T-1;
    WITH OPS(.T.) DO
      IF CLASS1=VAR_CLASS THEN
        WITH VTYPE^ DO
          IF KIND=ARRAY_KIND THEN BEGIN
            PUT2(SUB2,INDEX_TYPE,EL_TYPE^.NOUN);
            VTYPE:=EL_TYPE
          END ELSE SUB_ERR
      ELSE SUB_ERR
  END;

{########}
{CONSTANT}
{########}
  PROCEDURE CONSTANT;
  BEGIN
    PUSH_OLD_NAME;
    WITH OPS(.T.) DO
      IF CLASS1=DEF_CLASS THEN
        WITH DEF_ENTRY^ DO
          IF KIND IN CONST_KINDS THEN
            CASE KIND OF
              INDEX_CONST: BEGIN
                CLASS1:=ICONST_CLASS;
                ICONST_TYPE:=CONST_TYPE;
                ICONST_VAL:=CONST_VAL
              END;
              REAL_CONST: BEGIN
                CLASS1:=RCONST_CLASS; RCONST_DISP:=REAL_DISP
              END;
              STRING_CONST:BEGIN
                CLASS1:=SCONST_CLASS;
                SCONST_LENGTH:=STRING_LENGTH;
                SCONST_DISP:=STRING_DISP
              END
            END
          ELSE BEGIN CLASS1:=UNDEF_CLASS; ERROR(CONSTID_ERROR) END
  END;

  PROCEDURE REAL_;
  BEGIN
    PUSH;
    WITH OPS(.T.) DO BEGIN
      CLASS1:=RCONST_CLASS; RCONST_DISP:=CONST_DISP
    END
  END;

  PROCEDURE FREAL;
  BEGIN
    PUSH; OPS(.T.).CLASS1:=FCONST_CLASS;
    PUT1(REAL2,CONST_DISP)
  END;

  PROCEDURE INDEX(TYP:NOUN_INDEX);
  BEGIN
    PUSH;
    WITH OPS(.T.) DO BEGIN
      CLASS1:=ICONST_CLASS;
      ICONST_TYPE:=TYP;
      get_l(outfile2,ICONST_VAL);
    END
  END;

  PROCEDURE FINDEX(TYP:NOUN_INDEX);
  VAR VALUE:INTEGER;
  BEGIN
    PUSH; OPS(.T.).CLASS1:=FCONST_CLASS;
    get_l(outfile2,VALUE);
    PUT2(INDEX2,VALUE,TYP)
  END;

  PROCEDURE SSTRING;
  BEGIN
    PUSH;
    WITH OPS(.T.) DO BEGIN
      CLASS1:=SCONST_CLASS;
      get_l(outfile2, SCONST_LENGTH);
      SCONST_DISP:=CONST_DISP
    END
  END;
  PROCEDURE FSTRING;
  VAR LENGTH:INTEGER;
  BEGIN
    PUSH; OPS(.T.).CLASS1:=FCONST_CLASS;
    get_l(outfile2,LENGTH);
    PUT2(STRING2,LENGTH,CONST_DISP)
  END;

{#########}
{MAIN LOOP - PASS3}
{#########}

begin
  INITIALIZE;
   REPEAT
    get_l(outfile2, SY);
   CASE SY OF

    ADDRESS1: PUT0(ADDRESS2);
    ANAME1: ANAME;
    AND1: BINARY(AND2);
    ARG_LIST1: ARG_LIST;
    ARG1: ARG;
    ARRAY_DEF1: ARRAY_DEF;
    BODY_END1: PUT0(BODY_END2);
    BODY1: BODY;
    CALL_FUNC1: CALL(CALL_FUNC2);
    CALL_NAME1: CALL_NAME;
    CALL1: CALL(CALL_PROC2);
    CASE_JUMP1: IGNORE1(CASE_JUMP2);
    CASE1: CASE_;
    CHAR1: INDEX(XCHAR);
    CLASS1: COMP_DEF(CLASS2);
    COMP1: COMP;
    CONST_DEF1: CONST_DEF;
    CONST_ID1: CONST_ID;
    CONSTANT1: CONSTANT;
    CPARMLIST1: PARMLIST(CPARMLIST2);
    DEF_CASE1: DEF_CASE;
    DEF_LABEL1: IGNORE1(DEF_LABEL2);
    DIV1: BINARY(DIV2);
    EMPTY_SET1: BEGIN PUSH; PUT0(EMPTY_SET2) END;
    END_CASE1: END_CASE;
    ENUM_DEF1: PUT2(ENUM_DEF2,ENUM_TYPE,ENUM_VAL);
    ENUM_ID1: ENUM_ID;
    ENUM1: ENUM;
    EOM1: HALT:=TRUE;
    EQ1: BINARY(EQ2);
    EVAR_LIST1: VAR_LIST(EVAR_LIST2);
    FALSEJUMP1: BEGIN IGNORE1(FALSEJUMP2); T:=T-1 END;
    FCHAR1: FINDEX(XCHAR);
    FIELD_ID1,PARM_ID1, VAR_ID1: PUSH_NEW_NAME(NOT_POSSIBLY_FORWARD,
    OUTPUT,INCOMPLETE);
    FIELDLIST1: FIELD_LIST;
    FINTEGER1: FINDEX(XINTEGER);
    FNAME1: FNAME;
    FOR_DOWN1: IGNORE2(FOR_DOWN2);
    FOR_LIM1: BEGIN IGNORE3(FOR_LIM2); T:=T-1 END;
    FOR_STORE1: POP2(FOR_STORE2);
    FOR_UP1: IGNORE2(FOR_UP2);
    FREAL1: FREAL;
    FSTRING1: FSTRING;
    FUNC_DEF1: FUNC_DEF(FUNC_DEF2);
    FUNC_END1, PROC_END1: ROUT_END(INTERNAL);
    FUNCE_DEF1: FUNC_DEF(FUNCE_DEF2);
    FUNC_ID1: PUSH_NEW_NAME(NOT_POSSIBLY_FORWARD,RETAIN,FUNCTIONAL);
    FUNCE_END1, PROCE_END1: ROUT_END(EXTERNAL);
    FUNCE_ID1: PUSH_NEW_NAME(POSSIBLY_FORWARD,RETAIN,FUNCTIONAL);
    FUNCTION1: FUNCTION_;
    GE1: BINARY(GE2);
    GT1: BINARY(GT2);
    INCLUDE1: BINARY(INCLUDE2);
    INIT_NAME1: INIT_NAME;
    INITS_DEF1: INITS_DEF;
    INITS_END1: POP_LEVEL;
    INIT1: CALL(INIT2);
    INTEGER1: INDEX(XINTEGER);
    INTF_ID1: INTF_ID;
    INTF1: BEGIN POP_LEVEL; OLD_NAMES:= NAME_LIST; NAME_LIST:= NIL END;
    IN1: BINARY(IN2);
    JUMP_DEF1: IGNORE2(JUMP_DEF2);
    JUMP1: IGNORE1(JUMP2);
    LCONST1: LCONST;
    LE1: BINARY(LE2);
    LT1: BINARY(LT2);
    MESSAGE1: IGNORE2(MESSAGE2);
    MINUS1: BINARY(MINUS2);
    MOD1: BINARY(MOD2);
    MONITOR1: COMP_DEF(MONITOR2);
    NAME1: NAME;
    NEW_LINE1: IGNORE1(NEW_LINE2);
    NE1: BINARY(NE2);
    NOT1: PUT0(NOT2);
    OR1: BINARY(OR2);
    PARM_TYPE1: TYPE_(OUTPUT,PARM_TYPE2);
    PEND1: PUT0(PEND2);
    PLUS1: BINARY(PLUS2);
    PROC_DEF1: PROC_DEF(PROC_DEF2);
    PROC_ID1,PROG_ID1: PUSH_NEW_NAME(NOT_POSSIBLY_FORWARD,RETAIN,INCOMPLETE);
    PROCE_DEF1: PROC_DEF(PROCE_DEF2);
    PROCE_ID1: PUSH_NEW_NAME(POSSIBLY_FORWARD,RETAIN,INCOMPLETE);
    PROCESS1: COMP_DEF(PROCESS2);
    PROG_DEF1: PROG_DEF;
    PSTART1: PSTART;
    REAL1: REAL_;
    REC_DEF1: REC_DEF;
    REC1: REC;
    SET_DEF1: SET_DEF;
    SLASH1: BINARY(SLASH2);
    STACK1: IGNORE1(STACK2);
    STAR1: BINARY(STAR2);
    STORE1: POP2(STORE2);
    STRING1: SSTRING;
    SUBR_DEF1: SUBR_DEF;
    SUB1: SUB;
    TYPE_DEF1: TYPE_DEF;
    TYPE_ID1: TYPE_ID;
    TYPE1: TYPE_(OUTPUT,TYPE2);
    UMINUS1: PUT0(UMINUS2);
    UNIV_TYPE1: TYPE_(OUTPUT,UNIV_TYPE2);
    UPLUS1: PUT0(UPLUS2);
    VALUE1: PUT0(VALUE2);
    VAR_LIST1: VAR_LIST(VAR_LIST2);
    VPARMLIST1: PARMLIST(VPARMLIST2);
    WITH_TEMP1: WITH_TEMP;
    WITH_VAR1: PUT0(WITH_VAR2);
    WITH1: BEGIN POP_LEVEL; PUT0(WITH2) END
    END
  UNTIL HALT;
  IF UNRES_COUNT > 0 THEN ERROR(UNRES_ERROR);
  PUT0(EOM2);
  INTER_PASS_PTR^.CONSTANTS:=CONST_DISP;
end;

procedure Pass4(var OK:boolean);

CONST

IDLENGTH = 12;

MAXARG = 10;

PRINTLIMIT = 18;   MAXDIGIT = 6;



{INPUT OPERATORS}
EOM1=1;            TYPE_DEF1=2;        NEW_NOUN1=3;        VAR_LIST1=4;
EVAR_LIST1=5;      INITS_DEF1=6;       PROC_DEF1=7;        PROCE_DEF1=8;
FUNC_DEF1=9;       FUNCE_DEF1=10;      PROG_DEF1=11;       TYPE1=12;
ENUM_DEF1=13;      SUBR_DEF1=14;       SET_DEF1=15;        INTF1=16;
ARRAY_DEF1=17;     REC1=18;            FIELDLIST1=19;      REC_DEF1=20;
CLASS1=21;         MONITOR1=22;        PROCESS1=23;        STACK1=24;
PSTART1=25;        PARM_TYPE1=26;      UNIV_TYPE1=27;      CPARMLIST1=28;
VPARMLIST1=29;     BODY1=30;           BODY_END1=31;       ADDRESS1=32;
RESULT1=33;        STORE1=34;          CALL_PROC1=35;      CALL_PROG1=36;
INTF_ID1=37;       PARM1=38;           FALSEJUMP1=39;      DEF_LABEL1=40;
JUMP_DEF1=41;      FUNCF_DEF1=42;      JUMP1=43;           CASE_LIST1=44;
FOR_STORE1=45;     FOR_LIM1=46;        FOR_UP1=47;         FOR_DOWN1=48;
WITH_VAR1=49;      WITH_TEMP1=50;      WITH1=51;           INIT1=52;
VALUE1=53;         LT1=54;             EQ1=55;             GT1=56;
LE1=57;            NE1=58;             GE1=59;             IN1=60;
UPLUS1=61;         UMINUS1=62;         PLUS1=63;           MINUS1=64;
OR1=65;            STAR1=66;           SLASH1=67;          DIV1=68;
MOD1=69;           AND1=70;            NOT1=71;            EMPTY_SET1=72;
INCLUDE1=73;       FUNCTION1=74;       CALL_FUNC1=75;      ROUTINE1=76;
VAR1=77;           ARROW1=78;          VCOMP1=79;          RCOMP1=80;
SUB1=81;           INDEX1=82;          REAL1=83;           STRING1=84;
LCONST1=85;        MESSAGE1=86;        NEW_LINE1=87;       FWD_DEF1=88;
CHK_TYPE1=89;      PROCF_DEF1=90;      UNDEF1=91;          PEND1=92;
CASE_JUMP1=93;

{OUTPUT OPERATORS}
EOM2=1;            BODY2=2;            BODY_END2=3;        ADDRESS2=4;
RESULT2=5;         STORE2=6;           CALL_PROC2=7;       CONSTPARM2=8;
VARPARM2=9;        FALSEJUMP2=10;      DEF_LABEL2=11;      JUMP_DEF2=12;
CASE_JUMP2=13;     JUMP2=14;           CASE_LIST2=15;      FOR_STORE2=16;
FOR_LIM2=17;       FOR_UP2=18;         FOR_DOWN2=19;       WITH2=20;
INIT2=21;          CALL_PROG2=22;      INTF_LBL2=23;       VALUE2=24;
LT2=25;            EQ2=26;             GT2=27;             LE2=28;
NE2=29;            GE2=30;             IN2=31;             UPLUS2=32;
UMINUS2=33;        PLUS2=34;           MINUS2=35;          OR2=36;
STAR2=37;          SLASH2=38;          DIV2=39;            MOD2=40;
AND2=41;           EMPTY_SET2=42;      INCLUDE2=43;        FUNCTION2=44;
CALL_FUNC2=45;     ROUTINE2=46;        VAR2=47;            ARROW2=48;
VCOMP2=49;         RCOMP2=50;          SUB2=51;            LCONST2=52;
MESSAGE2=53;       NEW_LINE2=54;       CHK_TYPE2=55;       SAVEPARM2=56;
CALL_GEN2=57;      NOT2=58;            UNDEF2=59;          RANGE2=60;

{STANDARD SPELLING/NOUN INDICES}
XUNDEF=0;          XFALSE=1;           XTRUE=2;            XINTEGER=3;
XBOOLEAN=4;        XCHAR=5;            XQUEUE=6;           XABS=7;
XATTRIBUTE=8;      XCHR=9 ;            XCONTINUE=10;       XCONV=11;
XDELAY=12;         XEMPTY=13;          XIO=14;             XORD=15;
XPRED=16;          XSTOP=17;           XREALTIME=18;       XSETHEAP=19;
XSUCC=20;          XTRUNC=21;          XSTART=22;          XWAIT=23;
XREAL=24;

{STANDARD NOUN INDICES}
ZARITHMETIC=25;    ZINDEX=26;          ZPASSIVE=27;        ZVPARM=28;
ZCPARM=29;         ZSPARM=30;          ZWITH=31;

{ERRORS}
NESTING_ERROR=1;   ADDRESS_ERROR=2;    ACTIVE_ERROR=3;     QUEUE_ERROR=4;
PROCESS_ERROR=5;   ENTRY_ERROR=6;      FUNCTYPE_ERROR=7;   TYPEID_ERROR=8;
ENUM1_ERROR=9;     ENUM2_ERROR=10;     INDEX_ERROR=11;     MEMBER_ERROR=12;
STACK_ERROR=13;    PARM1_ERROR=14;     PARM2_ERROR=15;     PARM3_ERROR=16;
PARM4_ERROR=17;    PARM5_ERROR=18;     PARM6_ERROR=19;     PARM7_ERROR=20;
COMPILER_ERROR=21; STRING_ERROR=22;

{CONTEXT}
FUNC_RESULT=1;     ENTRY_VAR=2;        VARIABLE=3;         VAR_PARM=4;
UNIV_VAR=5;        CONST_PARM=6;       UNIV_CONST=7;       FIELD=8;
EXPR=10;           CONSTANT=11;        SAVE_PARM=12;       WITH_CONST = 13;
WITH_VAR = 14;

{TYPE KIND}
INT_KIND=0;        REAL_KIND=1;        BOOL_KIND=2;        CHAR_KIND=3;
ENUM_KIND=4;       SET_KIND=5;         STRING_KIND=6;      PASSIVE_KIND=7;
POINTER_KIND=8;    QUEUE_KIND=9;       GENERIC_KIND=10;    UNDEF_KIND=11;
SYSCOMP_KIND=12;   ROUTINE_KIND=13;    ACTIVE_KIND=14;

{INPUT_MODES}
CLASS1_MODE=1;     MONITOR1_MODE=2;    PROCESS1_MODE=3;    PROC1_MODE=4;
PROCE1_MODE=5;     FUNC1_MODE=6;       FUNCE1_MODE=7;      PROGRAM1_MODE=8;
RECORD_MODE=9;

{OUTPUT_MODES}
SCONST2_MODE=11;   LCONST2_MODE=0;     PROC2_MODE=1;       PROGRAM2_MODE=2;
PE2_MODE=3;        CE2_MODE=4;         ME2_MODE=5;         PROCESS2_MODE=6;
CLASS2_MODE=7;     MONITOR2_MODE=8;    STD2_MODE=9;        UNDEF2_MODE=10;

{"MISCELANEOUS}
INITIAL_LEVEL=0;   RESOLVE=TRUE;       DONT_RESOLVE=FALSE;
MAX_INT=32667;     SET_MIN=0;          SET_MAX=127;        THIS_PASS=4;
STACK_MAX=100;     NOUN_MAX=700;       MAX_LEVEL=15;
INITIALBLOCK=1;    BYTELENGTH = 1;

TYPE
IDENTIFIER = ARRAY (.1..IDLENGTH.) OF CHAR;

ARGTAG =
  (NILTYPE, BOOLTYPE, INTTYPE, IDTYPE, PTRTYPE);

INPUT_MODE=CLASS1_MODE..RECORD_MODE;

  OUTPUT_MODE= LCONST2_MODE..SCONST2_MODE;
  DISPLACEMENT=INTEGER;
  STACK_INDEX={0}-1..STACK_MAX;
  NOUN_INDEX=0..NOUN_MAX;
  LEGACY_TYPE=(CLASS_LEGACY,MONITOR_LEGACY,PROCESS_LEGACY,QUEUE_LEGACY);
  LEGACYS=SET OF LEGACY_TYPE;
  TYPE_KIND=INT_KIND..ACTIVE_KIND;
  TYPE_KINDS=SET OF TYPE_KIND;
  CONTEXT_KIND=FUNC_RESULT..WITH_VAR;
  CONTEXTS=SET OF CONTEXT_KIND;

  PACKED_SET=integer{0..15};
  ENTRY_CLASS=(UNDEFINED,VALUE,ROUTINE,TEMPLATE);
  ENTRY_PTR=^ENTRY;
  ENTRY=
    RECORD
      CASE CLASS1:ENTRY_CLASS OF
        VALUE:(
          VMODE:OUTPUT_MODE; VDISP:DISPLACEMENT;
          CONTEXT:CONTEXT_KIND);
        ROUTINE:(
          RMODE:OUTPUT_MODE; RDISP:DISPLACEMENT;
          PARM_SIZE,VAR_SIZE,STACK_SIZE:DISPLACEMENT);
        TEMPLATE:(
          NOUN:NOUN_INDEX; SIZE:DISPLACEMENT; INHERITANCE:PACKED_SET;
          CASE KIND:TYPE_KIND OF
            INT_KIND,BOOL_KIND,CHAR_KIND,ENUM_KIND:(
              MIN,MAX:INTEGER);
            SYSCOMP_KIND:(SMODE:OUTPUT_MODE;
              OFFSET:DISPLACEMENT))
    END;
  DISPLAY_INDEX=0..MAX_LEVEL;
  DISPLAY_REC=
    RECORD
      LAST_MODE: OUTPUT_MODE;
      LAST_ADDRESS:DISPLACEMENT;
      LAST_INHERITANCE:PACKED_SET
    END;
  UNIV_SET = ARRAY (.1..8.) OF INTEGER;

  VAR
   SY,PARM_NUMBER:INTEGER;
    WITH_CONTEXT:CONTEXT_KIND;

  PACKED_CLASS,PACKED_MONITOR,PACKED_PROCESS,PACKED_QUEUE: PACKED_SET;
  N:NOUN_INDEX;
  DEBUG,DONE,UNIVERSAL,SAVE_CONTEXT,GENERIC_FUNCTION,INITIAL_ENTRY: BOOLEAN;
  NOUN_TABLE:ARRAY (.NOUN_INDEX.) OF ENTRY_PTR;
  STACK:ARRAY (.STACK_INDEX.) OF ENTRY_PTR;
  THIS_LEVEL: INTEGER;
  DISPLAY: ARRAY (.DISPLAY_INDEX.) OF DISPLAY_REC;
  INTF_LENGTH,CURRENT_DISP,CURRENT_LABEL,
  COMPVAR_LENGTH: DISPLACEMENT;
  CHK_MODE:INPUT_MODE;
  MODE: OUTPUT_MODE;
  T: INTEGER;
  PASS_BY_REFERENCE, ASSIGNABLE: CONTEXTS;
  RECORD_INHERITANCE: LEGACYS;
  UENTRY,NEW_ENTRY,{OLD_ENTRY,} UTYPE: ENTRY_PTR;
  SMALLS,ACTIVES,PASSIVES,FUNC_TYPES,INDEXS,LARGES: TYPE_KINDS;
  NONVARPARMS: SET OF INPUT_MODE;
  NONCOMPS: SET OF OUTPUT_MODE;


I: integer;


PROCEDURE PUT_ARG(ARG:INTEGER);
  BEGIN
    add(ARG, outfile4);
    IF DEBUG THEN PRINTARG(ARG)
  END;

PROCEDURE PUT0(OP:INTEGER);
  BEGIN
    add(OP, outfile4);
    IF DEBUG THEN PRINTOP(OP)
  END;

  PROCEDURE PUT1(OP,ARG1:INTEGER);
  BEGIN
    add(op,outfile4); add(ARG1,outfile4);
    IF DEBUG THEN BEGIN
      PRINTOP(OP); PRINTARG(ARG1)
    END
  END;

  PROCEDURE PUT2(OP,ARG1,ARG2:INTEGER);
  BEGIN
    add(op,outfile4); add(ARG1,outfile4);  add(ARG2,outfile4);
    IF DEBUG THEN BEGIN
      PRINTOP(OP); PRINTARG(ARG1); PRINTARG(ARG2)
    END
  END;

  PROCEDURE PUT3(OP,ARG1,ARG2,ARG3:INTEGER);
  BEGIN
    add(op,outfile4); add(ARG1,outfile4);  add(ARG2,outfile4);
    add(ARG3,outfile4);
    IF DEBUG THEN BEGIN
      PRINTOP(OP); PRINTARG(ARG1); PRINTARG(ARG2); PRINTARG(ARG3)
    END
  END;

  PROCEDURE PUT3_ARG(ARG1,ARG2,ARG3:INTEGER);
  BEGIN
    add(ARG1,outfile4);  add(ARG2,outfile4); add(ARG3,outfile4);
    IF DEBUG THEN BEGIN
      PRINTARG(ARG1); PRINTARG(ARG2); PRINTARG(ARG3)
    END
  END;

  PROCEDURE PUT4(OP,ARG1,ARG2,ARG3,ARG4:INTEGER);
  BEGIN
    PUT3(OP,ARG1,ARG2,ARG3); PUT_ARG(ARG4)
  END;

  PROCEDURE PUT5(OP,ARG1,ARG2,ARG3,ARG4,ARG5:INTEGER);
  BEGIN
    PUT3(OP,ARG1,ARG2,ARG3);
    PUT_ARG(ARG4); PUT_ARG(ARG5)
  END;

  {NOTE: A PASS RUNNING WITH TEST OUTPUT SHOULD START WITH PRINTFF}

{#######}
{PACKING}
{#######}
  PROCEDURE PACK (VAR PACKED_SET: INTEGER;
  UNPACKED_SET:LEGACYS {UNIV_SET});
  BEGIN
     {CLASS_LEGACY,MONITOR_LEGACY,PROCESS_LEGACY,QUEUE_LEGACY}
     if UNPACKED_SET =  [CLASS_LEGACY] then  PACKED_SET:=1 else
      if UNPACKED_SET =  [MONITOR_LEGACY] then  PACKED_SET:=2 else
            if UNPACKED_SET =  [PROCESS_LEGACY] then  PACKED_SET:=3 else
             if UNPACKED_SET =  [QUEUE_LEGACY] then  PACKED_SET:=4;
    {PACKED_SET:= UNPACKED_SET{(.1.)}
  END;

  PROCEDURE UNPACK (PACKED_SET: INTEGER;
  VAR UNPACKED_SET:LEGACYS {UNIV_SET} );
  BEGIN
   case PACKED_SET of
    1: UNPACKED_SET:= [CLASS_LEGACY];
    2: UNPACKED_SET:= [MONITOR_LEGACY];
    3: UNPACKED_SET:= [PROCESS_LEGACY];
    4: UNPACKED_SET:= [QUEUE_LEGACY];
   end;
    {UNPACKED_SET(.1.):= PACKED_SET }
  END;

{##########}
{INITIALIZE}
{##########}
  PROCEDURE STD_INDEX(N:NOUN_INDEX; K:TYPE_KIND; L,U:INTEGER);
  VAR E:ENTRY_PTR;
  BEGIN
    NEW(E); NOUN_TABLE(.N.):=E;
    WITH E^ DO BEGIN
      CLASS1:=TEMPLATE; NOUN:=N;
      SIZE:=WORDLENGTH; INHERITANCE:=0;
      KIND:=K; MIN:=L; MAX:=U
    END
  END;

  PROCEDURE STD_PARM(N:NOUN_INDEX; C:CONTEXT_KIND);
  VAR E:ENTRY_PTR;
  BEGIN
    NEW(E); NOUN_TABLE(.N.):=E;
    WITH E^ DO BEGIN
      CLASS1:=VALUE; VMODE:=UNDEF2_MODE;
      VDISP:= 0;
      CONTEXT:=C
    END
  END;

  PROCEDURE STD_ROUTINE(N:NOUN_INDEX; NO:INTEGER);
  VAR E:ENTRY_PTR;
  BEGIN
    NEW(E); NOUN_TABLE(.N.):=E;
    WITH E^ DO BEGIN
      CLASS1:= ROUTINE; RMODE:= STD2_MODE; RDISP:= NO;
      PARM_SIZE:= 0; VAR_SIZE:= 0;
    END
  END;

  PROCEDURE STD_NONINDEX(N:NOUN_INDEX; K:TYPE_KIND; S:DISPLACEMENT;
    I:PACKED_SET);
  VAR E:ENTRY_PTR;
  BEGIN
    NEW(E); NOUN_TABLE(.N.):=E;
    WITH E^ DO BEGIN
      CLASS1:=TEMPLATE;
      NOUN:=N; SIZE:=S; INHERITANCE:=I;
      KIND:=K
    END
  END;

  PROCEDURE INITIALIZE;
  VAR  TEMP_LEGACY: LEGACYS;
  BEGIN
    IF DEBUG THEN PRINTFF(THIS_PASS);
    first(outfile3);
    INIT (outfile4);
    GENERIC_FUNCTION:=FALSE;
    CURRENT_DISP:=0;
    T:=-1; DONE:=FALSE;
    THIS_LEVEL:=-1;
    MODE:=PROCESS2_MODE;
    INITIAL_ENTRY:=FALSE;
    SAVE_CONTEXT:=FALSE;
    COMPVAR_LENGTH:=0;
    PASSIVES:=(.INT_KIND,REAL_KIND,BOOL_KIND,CHAR_KIND,ENUM_KIND,SET_KIND,
      STRING_KIND,PASSIVE_KIND,UNDEF_KIND.);
    ASSIGNABLE:= (.FUNC_RESULT, VARIABLE, VAR_PARM, UNIV_VAR, WITH_VAR.);
    NONCOMPS:= (.PROC2_MODE, CE2_MODE, ME2_MODE, PE2_MODE,
      PROGRAM2_MODE, UNDEF2_MODE.);
    CURRENT_LABEL:=0;
    {THIS AUTOMATICALLY ASSIGNS LABEL 1 TO THE INITIAL PROCESS}
    NEW(UTYPE);
    WITH UTYPE^ DO BEGIN
      CLASS1:=TEMPLATE;
      NOUN:=XUNDEF; SIZE:=1;
      INHERITANCE:=0; KIND:=UNDEF_KIND
    END;
    INDEXS:=(.INT_KIND,BOOL_KIND,CHAR_KIND,ENUM_KIND.);
    PASS_BY_REFERENCE:=(.VAR_PARM,UNIV_VAR.);
    LARGES:=(.STRING_KIND,PASSIVE_KIND,ACTIVE_KIND,SYSCOMP_KIND.);
    TEMP_LEGACY:= (.CLASS_LEGACY.);
    PACK(PACKED_CLASS, TEMP_LEGACY);
    TEMP_LEGACY:= (.MONITOR_LEGACY.);
    PACK(PACKED_MONITOR, TEMP_LEGACY);
    TEMP_LEGACY:= (.PROCESS_LEGACY.);
    PACK(PACKED_PROCESS, TEMP_LEGACY);
    TEMP_LEGACY:= (.QUEUE_LEGACY.);
    PACK(PACKED_QUEUE, TEMP_LEGACY);
    SMALLS:=(.INT_KIND,REAL_KIND,CHAR_KIND,BOOL_KIND,ENUM_KIND,SET_KIND,
      QUEUE_KIND.);
    ACTIVES:=(.QUEUE_KIND,SYSCOMP_KIND,ACTIVE_KIND.);
    FUNC_TYPES:= (.INT_KIND, CHAR_KIND, BOOL_KIND,
      ENUM_KIND, REAL_KIND.);
    NONVARPARMS:=(.CLASS1_MODE,MONITOR1_MODE,PROCESS1_MODE,FUNC1_MODE,
      FUNCE1_MODE.);
    NEW(UENTRY); UENTRY^.CLASS1:=UNDEFINED; NOUN_TABLE(.XUNDEF.):=UENTRY;
    STD_INDEX(XINTEGER,INT_KIND,-32767,32767);
    STD_NONINDEX(XREAL,REAL_KIND,REALLENGTH,0);
    STD_INDEX(XBOOLEAN,BOOL_KIND,0,1);
    STD_INDEX(XCHAR,CHAR_KIND,0,127);
    STD_NONINDEX(XQUEUE,QUEUE_KIND,WORDLENGTH,PACKED_QUEUE);
    STD_NONINDEX(ZWITH,POINTER_KIND,WORDLENGTH,0);
    STD_NONINDEX(ZARITHMETIC,GENERIC_KIND,0,0);
    STD_NONINDEX(ZPASSIVE,GENERIC_KIND,0,0);
    STD_NONINDEX(ZINDEX,GENERIC_KIND,0,0);
    STD_PARM(ZVPARM,VAR_PARM);
    STD_PARM(ZCPARM,CONST_PARM);
    STD_PARM(ZSPARM,SAVE_PARM);
    STD_ROUTINE( XTRUNC,0);
    STD_ROUTINE( XABS,1);
    STD_ROUTINE( XSUCC,2);
    STD_ROUTINE( XPRED,3);
    STD_ROUTINE( XCONV,4);
    STD_ROUTINE( XEMPTY,5);
    STD_ROUTINE( XATTRIBUTE,6);
    STD_ROUTINE( XREALTIME,7);
    STD_ROUTINE( XORD,8);
    STD_ROUTINE( XCHR,9);
    STD_ROUTINE( XDELAY,0);
    STD_ROUTINE( XCONTINUE,1);
    STD_ROUTINE( XIO,2);
    STD_ROUTINE( XSTART,3);
    STD_ROUTINE( XSTOP,4);
    STD_ROUTINE( XSETHEAP,5);
    STD_ROUTINE( XWAIT,6);
  END;

{######}
{ERRORS}
{######}
  PROCEDURE ERROR(NUMBER:INTEGER);
  BEGIN
    {PUT2(MESSAGE2,THIS_PASS,NUMBER)}
    write(errors, ' ',MESSAGE2,' ',THIS_PASS,' ',NUMBER);
    writeln(errors);
    PUT2(MESSAGE2,THIS_PASS,NUMBER)
  END;

  PROCEDURE EOM;
  BEGIN
    WITH INTER_PASS_PTR^ DO BEGIN
      {RELEASE(RESETPOINT); }
      BLOCKS:=CURRENT_LABEL;
    END;
    PUT1(EOM2,COMPVAR_LENGTH );{initial PROCESS VAR SIZE}
    DONE:=TRUE
  END;

  PROCEDURE ABORT;
  BEGIN
    PUT_E(THIS_PASS,COMPILER_ERROR,0);
    write(errors, ' ',MESSAGE2,' ', THIS_PASS,' ', COMPILER_ERROR);
    writeln(errors);
    EOM
  END;
{######}
{IGNORE}
{######}

  PROCEDURE CASE_LIST;
  VAR I,ARG,MIN,MAX:INTEGER;
  BEGIN
     get_l(outfile3, ARG);  get_l(outfile3, MIN);
     get_l(outfile3, MAX);
    PUT3(CASE_LIST2,ARG,MIN,MAX);
    FOR I:=MIN TO MAX+1 DO BEGIN
      get_l(outfile3, ARG);
      PUT_ARG(ARG)
    END
  END;

  PROCEDURE LCONST;
  VAR LENGTH,I,ARG:INTEGER;
  BEGIN
    get_l(outfile3, LENGTH);
    PUT1(LCONST2,LENGTH);
    FOR I:=1 TO LENGTH {DIV WORDLENGTH} DO BEGIN
      get_l(outfile3, ARG);
      PUT_ARG(ARG)
    END
  END;

  PROCEDURE IGNORE1(OP:INTEGER);
  VAR ARG1:INTEGER;
  BEGIN
    get_l(outfile3, ARG1);
    PUT1(OP,ARG1)
  END;
  PROCEDURE IGNORE2(OP:INTEGER);
  VAR ARG1,ARG2:INTEGER;
  BEGIN
    get_l(outfile3, ARG1);
    get_l(outfile3, ARG2);
    PUT2(OP,ARG1,ARG2)
  END;

 {#############}
 {NOUN HANDLING}
{#############}
  PROCEDURE PUSH_NEW_ENTRY(VAR E:ENTRY_PTR);
  var N:integer;
  BEGIN
    get_l(outfile3, N);
    NEW(E);
    IF N<>XUNDEF THEN NOUN_TABLE(.N.):=E;
    IF T>=STACK_MAX THEN ABORT ELSE T:=T+1;
     STACK(.T.):=E
  END;

  PROCEDURE PUSH_OLD_ENTRY(VAR E:ENTRY_PTR);
  var
   N:integer;
  BEGIN
    get_l(outfile3, N);
    E:=NOUN_TABLE(.N.);
    IF T>=STACK_MAX THEN ABORT ELSE T:=T+1;
    STACK(.T.):=E
  END;

{#######}
{NESTING}
{#######}
  PROCEDURE PUSH_LEVEL(M:INPUT_MODE);
  BEGIN
    IF THIS_LEVEL>=MAX_LEVEL THEN ABORT ELSE THIS_LEVEL:=THIS_LEVEL+1;
    WITH DISPLAY(.THIS_LEVEL.) DO BEGIN
      LAST_MODE:=MODE;
      PACK(LAST_INHERITANCE,RECORD_INHERITANCE);
      LAST_ADDRESS:=CURRENT_DISP; CURRENT_DISP:=0;
      IF MODE IN NONCOMPS THEN
        IF M<>RECORD_MODE THEN BEGIN
          ERROR(NESTING_ERROR); MODE:=CLASS2_MODE
        END;
      CASE M OF
        CLASS1_MODE: MODE:=CLASS2_MODE;
        MONITOR1_MODE: MODE:=MONITOR2_MODE;
        PROCESS1_MODE: MODE:=PROCESS2_MODE;
        PROC1_MODE,FUNC1_MODE: MODE:=PROC2_MODE;
        PROCE1_MODE,FUNCE1_MODE:
          CASE MODE OF
            CLASS2_MODE: MODE:=CE2_MODE;
            MONITOR2_MODE: MODE:=ME2_MODE;
            PROCESS2_MODE: MODE:=PE2_MODE
          END;
        PROGRAM1_MODE: MODE:=PROGRAM2_MODE;
        RECORD_MODE: BEGIN
          RECORD_INHERITANCE:=(..);
          MODE:=UNDEF2_MODE
        END
      END
    END
  END;

  PROCEDURE POP_LEVEL;
  BEGIN
    WITH DISPLAY(.THIS_LEVEL.) DO BEGIN
      MODE:=LAST_MODE;
      UNPACK(LAST_INHERITANCE,RECORD_INHERITANCE);
      CURRENT_DISP:=LAST_ADDRESS
    END;
    THIS_LEVEL:=THIS_LEVEL-1
  END;

{###################}
{ADDRESS COMPUTATION}
{###################}
  FUNCTION ADD(A,B:INTEGER):INTEGER;
  BEGIN
  {ASSERT (A>=0) AND (B>=0);}
    IF MAX_INT-A>=B THEN ADD:=A+B
    ELSE BEGIN
      ERROR(ADDRESS_ERROR);
      ADD:=A
    END
  END;

  FUNCTION MULTIPLY(A,B:INTEGER):INTEGER;
  BEGIN
    {ASSERT (A>=0) AND (B>=0);}
    IF A<=MAX_INT DIV B THEN MULTIPLY:=A*B
    ELSE BEGIN
      MULTIPLY:=A;
      ERROR(ADDRESS_ERROR)
    END
  END;

  FUNCTION SUBTRACT(A,B:INTEGER):INTEGER;
  BEGIN
  {ASSERT A>=B;}
    IF (A>=0) AND (B>=0) THEN SUBTRACT:=A-B
    ELSE IF (A<0) AND (B<0) THEN SUBTRACT:=A-B
    ELSE SUBTRACT:=ADD(A,-B)
  END;

{#################}
{TYPE DECLARATIONS}
{#################}
  PROCEDURE TYPE_;
  VAR TYP:ENTRY_PTR;
  BEGIN
    PUSH_OLD_ENTRY(TYP);
    IF TYP=UENTRY THEN STACK(.T.):=UTYPE;
  END;

  PROCEDURE ENUM_DEF;
  VAR ENUM_ENTRY:ENTRY_PTR;
  BEGIN
    PUSH_NEW_ENTRY(ENUM_ENTRY);
    WITH ENUM_ENTRY^ DO BEGIN
      CLASS1:=TEMPLATE;
      NOUN:=N; SIZE:=WORDLENGTH; INHERITANCE:=0;
      KIND:=ENUM_KIND;
      MIN:=0;
      get_l(outfile3, MAX);
      IF MAX>SET_MAX THEN ERROR(ENUM2_ERROR)
    END;
    IF MODE=UNDEF2_MODE THEN ERROR(ENUM1_ERROR)
  END;

  PROCEDURE SUBR_DEF;
  VAR SUBR_ENTRY:ENTRY_PTR;
      NOUN1:integer;
  BEGIN
    PUSH_NEW_ENTRY(SUBR_ENTRY);
    WITH SUBR_ENTRY^ DO BEGIN
      CLASS1:=TEMPLATE;
      get_l(outfile3,{NOUN} NOUN1);
      SIZE:=WORDLENGTH; INHERITANCE:=0;
      IF NOUN1=XUNDEF THEN KIND:=ENUM_KIND
        ELSE KIND:=NOUN_TABLE(.{NOUNT}NOUN1.)^.KIND;
      get_l(outfile3, MIN);
      get_l(outfile3, MAX);
    END
  END;

  PROCEDURE MEMBER_CHECK;
  BEGIN
    WITH STACK(.T.)^ DO
      IF KIND IN INDEXS THEN
        IF (MIN<SET_MIN) OR (MAX>SET_MAX) THEN ERROR(MEMBER_ERROR)
        ELSE {OK}
      ELSE ERROR(MEMBER_ERROR)
  END;

  PROCEDURE SET_DEF;
  VAR SET_NOUN:NOUN_INDEX; SET_ENTRY:ENTRY_PTR;
  BEGIN
    MEMBER_CHECK;
    SET_NOUN:=STACK(.T.)^.NOUN;
    T:=T-1 {POP MEMBER TYPE};
    PUSH_NEW_ENTRY(SET_ENTRY);
    WITH SET_ENTRY^ DO BEGIN
      CLASS1:=TEMPLATE;
      NOUN:=SET_NOUN;
      SIZE:=SETLENGTH1; INHERITANCE:=0;
      KIND:=SET_KIND
    END
  END;

  PROCEDURE ARRAY_DEF;
  VAR SPAN,ARRAY_SIZE:DISPLACEMENT; ARRAY_KIND:TYPE_KIND;
    ARRAY_INHERITANCE:PACKED_SET; ARRAY_ENTRY:ENTRY_PTR;
  BEGIN
    WITH STACK(.T-1.)^ DO
      IF KIND IN INDEXS THEN SPAN:=ADD(SUBTRACT(MAX,MIN),1)
      ELSE BEGIN
        SPAN:=1; ERROR(INDEX_ERROR)
      END;
    WITH STACK(.T.)^ DO BEGIN
      IF KIND=CHAR_KIND THEN BEGIN
        IF SPAN MOD WORDLENGTH <>0 THEN BEGIN
          ERROR(STRING_ERROR);
          SPAN:=WORDLENGTH
        END;
        ARRAY_KIND:=STRING_KIND;
        ARRAY_SIZE:=SPAN
      END ELSE BEGIN
        IF KIND IN PASSIVES THEN ARRAY_KIND:=PASSIVE_KIND
        ELSE ARRAY_KIND:=ACTIVE_KIND;
        ARRAY_SIZE:=MULTIPLY(SPAN,SIZE)
      END;
      ARRAY_INHERITANCE:=INHERITANCE
    END;
    T:=T-2 {POP INDEX AND ELEMENT TYPES};
    PUSH_NEW_ENTRY(ARRAY_ENTRY);
    WITH ARRAY_ENTRY^ DO BEGIN
      CLASS1:=TEMPLATE;
      NOUN:=N; SIZE:=ARRAY_SIZE;
      INHERITANCE:=ARRAY_INHERITANCE;
      KIND:=ARRAY_KIND
    END
  END;

  PROCEDURE FIELDLIST;
  VAR THIS_SIZE:DISPLACEMENT;
      INHERITED1:LEGACYS; NUMBER,I:INTEGER;
  BEGIN
    WITH STACK(.T.)^ DO BEGIN
      UNPACK(INHERITANCE,INHERITED1);
      RECORD_INHERITANCE:=RECORD_INHERITANCE + INHERITED1;
      THIS_SIZE:=SIZE
    END;
    get_L(outfile3,NUMBER);
    FOR I:=NUMBER DOWNTO 1 DO
     {ASSIGN ADDRESSES IN FORWARD DIRECTION}
      WITH STACK(.T-I.)^ DO BEGIN
        CLASS1:=VALUE; VMODE:=MODE; CONTEXT:=FIELD;
        VDISP:=CURRENT_DISP; CURRENT_DISP:=ADD(CURRENT_DISP,THIS_SIZE)
      END;
    T:=T-NUMBER-1 {POP DECLARATION LIST}
  END;

  PROCEDURE REC_DEF;
  VAR REC_ENTRY:ENTRY_PTR;
  BEGIN
    PUSH_NEW_ENTRY(REC_ENTRY);
    WITH REC_ENTRY^ DO BEGIN
      CLASS1:=TEMPLATE;
      NOUN:=N; SIZE:=CURRENT_DISP;
      PACK(INHERITANCE,RECORD_INHERITANCE);
      IF INHERITANCE=0 THEN KIND:=PASSIVE_KIND
      ELSE KIND:=ACTIVE_KIND;
    END;
    POP_LEVEL
  END;

  PROCEDURE ROUTINE_DEF (RESOLVE: BOOLEAN);  FORWARD;
  PROCEDURE COMP_DEF(LEGACY:PACKED_SET);
  VAR COMP_ENTRY:ENTRY_PTR;
  BEGIN
    PUSH_NEW_ENTRY(COMP_ENTRY);
    WITH COMP_ENTRY^ DO BEGIN
      CLASS1:=TEMPLATE;
      NOUN:=N; INHERITANCE:=LEGACY;
      KIND:=SYSCOMP_KIND;
      SMODE:=MODE;
    END;
    ROUTINE_DEF(DONT_RESOLVE) {INITIAL STATEMENT}
  END;

  PROCEDURE STACK_;
  BEGIN
    IF STACK(.T-1.)^.SMODE<>PROCESS2_MODE THEN ERROR(STACK_ERROR);
    get_l(outfile3,STACK(.T.)^.STACK_SIZE)
  END;

{#####################}
{"VARIABLE DECLARATIONS}
{#####################}
  PROCEDURE VAR_LIST;
  VAR NUMBER,I:INTEGER; THIS_SIZE:DISPLACEMENT;
  INHERITED1:LEGACYS;
  BEGIN
    WITH STACK(.T.)^ DO BEGIN
      IF KIND IN ACTIVES THEN BEGIN {CHECK RULES}
        IF MODE IN NONCOMPS THEN
           ERROR(ACTIVE_ERROR);
        UNPACK(INHERITANCE,INHERITED1);
        IF QUEUE_LEGACY IN INHERITED1 THEN
          IF MODE<>MONITOR2_MODE THEN
            ERROR(QUEUE_ERROR);
        IF KIND=SYSCOMP_KIND THEN
          IF SMODE=PROCESS2_MODE THEN
            IF THIS_LEVEL<>INITIAL_LEVEL THEN
             ERROR(PROCESS_ERROR)
      END;
      THIS_SIZE:=SIZE
    END;
    get_l(outfile3, number);
    FOR I:=NUMBER DOWNTO 1 DO
    {ASSIGN ADDRESSES IN FORWARD DIRECTION}
      WITH STACK(.T-I.)^ DO BEGIN
        CLASS1:=VALUE; VMODE:=MODE; CONTEXT:=VARIABLE;
        CURRENT_DISP:=ADD(CURRENT_DISP,THIS_SIZE); VDISP:=-CURRENT_DISP
      END;
    T:=T-NUMBER-1 {POP DECLARATION LIST}
  END;

  PROCEDURE EVAR_LIST;
  BEGIN
    WITH STACK(.T.)^ DO
      IF (KIND IN ACTIVES) OR (MODE<>CLASS2_MODE) THEN ERROR(ENTRY_ERROR);
    VAR_LIST
  END;

{####################}
{ROUTINE DECLARATIONS}
{####################}
  PROCEDURE ROUTINE_DEF;
  VAR ROUTINE_ENTRY:ENTRY_PTR;
  BEGIN
    IF RESOLVE THEN BEGIN
      PUSH_OLD_ENTRY(ROUTINE_ENTRY);
      WITH ROUTINE_ENTRY^ DO BEGIN
        PARM_SIZE:=CURRENT_DISP;
        VAR_SIZE:= 0;
        STACK_SIZE:=0; RMODE:=MODE
      END
    END ELSE BEGIN
      PUSH_NEW_ENTRY(ROUTINE_ENTRY);
      WITH ROUTINE_ENTRY^ DO BEGIN
        CLASS1:=ROUTINE;
        PARM_SIZE:=CURRENT_DISP;
        STACK_SIZE:=0; RMODE:=MODE;
        CURRENT_LABEL:=CURRENT_LABEL+1; RDISP:=CURRENT_LABEL
      END
    END;
    CURRENT_DISP:=0
  END;

  PROCEDURE FUNC_DEF(RESOLVE:BOOLEAN);
  VAR FUNC_TYPE:ENTRY_PTR;
  BEGIN
    TYPE_;
    IF NOT(STACK(.T.)^.KIND IN FUNC_TYPES) THEN ERROR(FUNCTYPE_ERROR);
    T:=T-1 {POP FUNC TYPE};
    ROUTINE_DEF(RESOLVE)
  END;

  PROCEDURE INITS_DEF;
  BEGIN
    INITIAL_ENTRY:=TRUE;
    {TOP OF STACK IS INITIAL STATEMENT ENTRY; SECOND IS COMPONENT ENTRY}
    WITH STACK(.T-1.)^ DO
      IF SMODE=PROCESS2_MODE THEN BEGIN
        SIZE:=WORDLENGTH {CENTER}; OFFSET:=0
      END ELSE BEGIN
        SIZE:=CURRENT_DISP {VAR SIZE} + STACK(.T.)^.PARM_SIZE
          + WORDLENGTH {CENTER};
        OFFSET:=CURRENT_DISP {VAR SIZE}
      END;
  END;

  PROCEDURE PROG_DEF;
  BEGIN
    ROUTINE_DEF(DONT_RESOLVE);
    POP_LEVEL;
    T:=T-1
  END;

  PROCEDURE FWD_DEF;
  VAR ROUTINE_ENTRY:ENTRY_PTR;
  BEGIN
    PUSH_NEW_ENTRY(ROUTINE_ENTRY);
    WITH ROUTINE_ENTRY^ DO BEGIN
      CLASS1:=ROUTINE;
      CURRENT_LABEL:=CURRENT_LABEL+1; RDISP:=CURRENT_LABEL
    END;
    T:=T-1
  END;

  PROCEDURE PSTART;
  var
    CHK_MODE1:integer;
  BEGIN
    get_l(outfile3,CHK_MODE1{CHK_MODE});
    PUSH_LEVEL(CHK_MODE1{CHK_MODE});
    PARM_NUMBER:=0
  END;

  PROCEDURE PEND;
  VAR VSIZE:DISPLACEMENT; I:INTEGER;
  BEGIN
    CURRENT_DISP:=WORDLENGTH; {LEAVE A WORD FOR LINE NUMBER}
    FOR I:=0 TO PARM_NUMBER-1 DO
    {ASSIGN ADDRESSES IN REVERSE ORDER}
      WITH STACK(.T-I.)^ DO BEGIN
        VSIZE:=VDISP; VDISP:=CURRENT_DISP;
        CURRENT_DISP:=ADD(CURRENT_DISP,VSIZE);
        VMODE:=MODE
      END;
    CURRENT_DISP:=CURRENT_DISP-WORDLENGTH {CENTER};
    T:=T-PARM_NUMBER {POP PARMS};
  END;

 PROCEDURE PARM_CHECK;
  VAR INHERIT:LEGACYS;
  BEGIN
    WITH STACK(.T.)^ DO  {APPLY CHECKS}
      CASE CHK_MODE OF
        MONITOR1_MODE,PROCESS1_MODE,CLASS1_MODE:
          IF NOT(KIND IN SMALLS) THEN
            IF KIND=SYSCOMP_KIND THEN
              IF SMODE=MONITOR2_MODE THEN {OK}
              ELSE IF (SMODE=CLASS2_MODE) AND
                (CHK_MODE=CLASS1_MODE) THEN {OK}
                ELSE ERROR(PARM1_ERROR)
            ELSE ERROR(PARM2_ERROR);
        PROC1_MODE,FUNC1_MODE: ;
        PROCE1_MODE,FUNCE1_MODE: BEGIN
          UNPACK(INHERITANCE,INHERIT);
          IF QUEUE_LEGACY IN INHERIT THEN ERROR(PARM4_ERROR)
        END;
        PROGRAM1_MODE:
          IF KIND IN ACTIVES THEN ERROR(PARM5_ERROR)
      END
  END;

  PROCEDURE PARM_TYPE;
  BEGIN
    TYPE_;
    PARM_CHECK
  END;

  PROCEDURE UNIV_TYPE;
  BEGIN
    TYPE_;
    IF STACK(.T.)^.KIND IN ACTIVES THEN ERROR(PARM6_ERROR);
    UNIVERSAL:=TRUE;
    PARM_CHECK
  END;

  PROCEDURE PARMLIST(C:CONTEXT_KIND);
  VAR I,NUMBER:INTEGER; THIS_SIZE:DISPLACEMENT;
  BEGIN
    get_l(outfile3,NUMBER);
    PARM_NUMBER:=PARM_NUMBER+NUMBER;
    WITH STACK(.T.)^ DO
      IF (C IN PASS_BY_REFERENCE) OR (KIND IN LARGES)
        THEN THIS_SIZE:=WORDLENGTH ELSE THIS_SIZE:=SIZE;
    FOR I:=1 TO NUMBER DO
      WITH STACK(.T-I.)^ DO BEGIN
        CLASS1:=VALUE; VDISP:=THIS_SIZE;
        CONTEXT:=C
      END;
    T:=T-1 {POP TYPE}
  END;

  PROCEDURE CPARM_LIST;
  VAR C:CONTEXT_KIND;
  BEGIN
    IF UNIVERSAL THEN BEGIN
      C:=UNIV_CONST; UNIVERSAL:=FALSE
    END ELSE C:=CONST_PARM;
    PARMLIST(C)
  END;

  PROCEDURE VPARMLIST;
  VAR C:CONTEXT_KIND;
  BEGIN
    IF CHK_MODE IN NONVARPARMS THEN ERROR(PARM7_ERROR);
    IF UNIVERSAL THEN BEGIN
      C:=UNIV_VAR; UNIVERSAL:=FALSE
    END ELSE C:=VAR_PARM;
    PARMLIST(C)
  END;

{####}
{BODY}
{####}
  PROCEDURE BODY;
  BEGIN
    WITH STACK(.T.)^ DO BEGIN
      VAR_SIZE:=CURRENT_DISP;
      IF INITIAL_ENTRY THEN BEGIN
        INITIAL_ENTRY:=FALSE;
        COMPVAR_LENGTH:=CURRENT_DISP {SAVE LENGTH OF COMPONENT VARIABLES};
        CURRENT_DISP:=0 {INITIAL STATEMENT IS VARIABLE-LESS};
        PUT5(BODY2,RMODE,RDISP,0,0,STACK_SIZE)
      END ELSE
        PUT5(BODY2,RMODE,RDISP,PARM_SIZE,VAR_SIZE,STACK_SIZE)
    END
  END;

  PROCEDURE BODY_END;
  BEGIN
    PUT0(BODY_END2);
    T:=T-1;
    POP_LEVEL
  END;

{##########}
{STATEMENTS}
{"##########}

  PROCEDURE PUT_TYPE;
  VAR N:{NOUN_INDEX} integer; LENGTH:DISPLACEMENT;
  BEGIN
     get_L(outfile3,N);
       WITH NOUN_TABLE(.N.)^ DO
      IF CLASS1=TEMPLATE THEN BEGIN
        IF KIND=SYSCOMP_KIND THEN LENGTH:=OFFSET ELSE LENGTH:=SIZE;
        PUT3_ARG(KIND,NOUN,LENGTH)
      END ELSE PUT3_ARG(UNDEF_KIND,XUNDEF,1)
  END;

  PROCEDURE RESULT;
  VAR SHIFT: DISPLACEMENT;
  BEGIN
    WITH STACK(.T.)^ DO BEGIN
      IF (RMODE = PROC2_MODE) OR (RMODE = PE2_MODE)
      THEN SHIFT:= WORDLENGTH {CENTER LOCATION}
      ELSE SHIFT:= TWOWORDS {CENTER LOCATION AND COMPONENT ADDRESS};
      PUT1(RESULT2, PARM_SIZE + SHIFT)
    END;
    PUT_TYPE
  END;

  PROCEDURE INTF_ID;
  VAR N:integer{NOUN_INDEX};
  BEGIN
    get_l(outfile3,N);
    INTF_LENGTH:=INTF_LENGTH+WORDLENGTH;
    PUT1(INTF_LBL2,NOUN_TABLE(.N.)^.RDISP)
  END;
  PROCEDURE PARM;
  VAR PARM_NOUN:integer{NOUN_INDEX};
      OP:INTEGER;  PARM_CONTEXT:CONTEXT_KIND;
  BEGIN
    get_l(outfile3, PARM_NOUN);
    IF PARM_NOUN<>XUNDEF THEN
    WITH NOUN_TABLE(.PARM_NOUN.)^ DO BEGIN
      PARM_CONTEXT:= CONTEXT;
      CASE PARM_CONTEXT OF
        VAR_PARM,UNIV_VAR: OP:=VARPARM2;
        CONST_PARM,UNIV_CONST: OP:=CONSTPARM2;
        SAVE_PARM: BEGIN GENERIC_FUNCTION:=TRUE; OP:=SAVEPARM2 END
      END;
      PUT3(OP,VMODE,VDISP,CONTEXT)
    END
   ELSE PUT3(CONSTPARM2,UNDEF2_MODE,0,CONST_PARM);
    TYPE_;
    WITH STACK(.T.)^ DO BEGIN
      PUT3_ARG(KIND,NOUN,SIZE);
      IF PARM_CONTEXT = CONST_PARM THEN
        IF KIND IN INDEXS THEN
          IF N{"TYPE NOUN} <> XINTEGER THEN PUT2(RANGE2,MIN,MAX)
    END;
    T:=T-1
  END;

  PROCEDURE FOR_LIM;
  VAR ARG1,ARG2,ARG4:INTEGER;
  BEGIN
    get_L(outfile3, ARG1);
    get_L(outfile3, ARG2);
    get_L(outfile3, ARG4);
    CURRENT_DISP:=ADD(CURRENT_DISP,WORDLENGTH);
    PUT4(FOR_LIM2,ARG1,-CURRENT_DISP,ARG2,ARG4)
  END;

  PROCEDURE FOR_LOOP(OP:INTEGER);
  BEGIN
    CURRENT_DISP:=CURRENT_DISP-WORDLENGTH;
    IGNORE2(OP)
  END;

  PROCEDURE WITH_TEMP;
  VAR WITH_ENTRY:ENTRY_PTR;
  BEGIN
    PUSH_NEW_ENTRY(WITH_ENTRY);
    WITH WITH_ENTRY^ DO BEGIN
      CLASS1:=VALUE; VMODE:=PROC2_MODE {ALL TEMPS HAVE PROCEDURE MODE};
      CURRENT_DISP:=ADD(CURRENT_DISP,WORDLENGTH);
      VDISP:=-CURRENT_DISP;
      IF WITH_CONTEXT IN ASSIGNABLE THEN CONTEXT:= WITH_VAR
        ELSE CONTEXT:= WITH_CONST
    END;
    T:=T-1;
    PUT0(ADDRESS2)
  END;

  PROCEDURE WITH_;
  BEGIN
    CURRENT_DISP:=CURRENT_DISP-WORDLENGTH;
    PUT0(WITH2)
  END;

{################}
{VALUE OR ROUTINE}
{################}
  PROCEDURE FUNCTION_;
  BEGIN
    PUT0(FUNCTION2);
    PUT_TYPE
  END;

  PROCEDURE CALL_FUNC;
  BEGIN
    IF GENERIC_FUNCTION THEN BEGIN
      PUT0(CALL_GEN2);
      GENERIC_FUNCTION:= FALSE
    END ELSE PUT0(CALL_FUNC2)
  END;

  PROCEDURE INDEX;
  VAR VALUE:INTEGER;
  BEGIN
    get_L(outfile3,VALUE);
    PUT3(VAR2,SCONST2_MODE,VALUE,CONSTANT);
    PUT_TYPE
  END;

  PROCEDURE REAL_;
  VAR DISP:DISPLACEMENT;
  BEGIN
    get_L(outfile3,DISP);
    PUT3(VAR2,LCONST2_MODE,DISP,CONSTANT);
    PUT3_ARG(REAL_KIND,XREAL,REALLENGTH)
  END;

  PROCEDURE SSTRING;
  VAR LENGTH:INTEGER;  DISP:DISPLACEMENT;
  BEGIN
    get_L(outfile3,LENGTH);
    get_L(outfile3,DISP);
    PUT3(VAR2,LCONST2_MODE,DISP,CONSTANT);
    PUT3_ARG(STRING_KIND,LENGTH,LENGTH)
  END;

  PROCEDURE RCOMP(OP:INTEGER);
  VAR N:integer{NOUN_INDEX};
  BEGIN
    get_L(outfile3,N);
    WITH NOUN_TABLE(.N.)^ DO
      IF CLASS1=ROUTINE THEN
        PUT5(OP,RMODE,RDISP,PARM_SIZE,VAR_SIZE,STACK_SIZE)
      ELSE PUT0(UNDEF2)
  END;

  PROCEDURE VCOMP(OP:INTEGER);
  VAR N:integer{NOUN_INDEX};
  BEGIN
    get_L(outfile3,N);
    WITH NOUN_TABLE(.N.)^ DO BEGIN
      PUT3(OP,VMODE,VDISP,CONTEXT);
      PUT_TYPE;
      IF SAVE_CONTEXT THEN BEGIN
        WITH_CONTEXT:=CONTEXT; SAVE_CONTEXT:=FALSE
      END
    END
  END;

  PROCEDURE ARROW;
  BEGIN
    PUT0(ARROW2); PUT_TYPE
  END;

  PROCEDURE SUB;
  VAR {N:NOUN_INDEX;} INDEX,ELEMENT:ENTRY_PTR;
    LENGTH:DISPLACEMENT;
  BEGIN
    {INDEX} TYPE_; INDEX:=STACK(.T.); T:=T-1;
    {ELEMENT} TYPE_; ELEMENT:=STACK(.T.); T:=T-1;
    WITH ELEMENT^ DO
      IF KIND=CHAR_KIND THEN LENGTH:=BYTELENGTH ELSE LENGTH:=SIZE;
    WITH INDEX^ DO BEGIN
      IF KIND IN INDEXS THEN PUT3(SUB2,MIN,MAX,LENGTH) ELSE PUT3(SUB2,0,0,1);
      PUT3_ARG(KIND,NOUN,SIZE)
    END;
    WITH ELEMENT^ DO BEGIN
      IF KIND=SYSCOMP_KIND THEN LENGTH:=OFFSET;
      PUT3_ARG(KIND,NOUN,LENGTH)
    END
  END;

{MAIN LOOP PASS4}

begin
  INITIALIZE;
  REPEAT
   get_L (outfile3,SY);
  CASE SY OF

 ADDRESS1: PUT0(ADDRESS2);
 AND1: PUT0(AND2);
 ARRAY_DEF1: ARRAY_DEF;
 ARROW1: ARROW;
 BODY_END1: BODY_END;
 BODY1: BODY;
 CALL_FUNC1: CALL_FUNC;
 CALL_PROC1: PUT0(CALL_PROC2);
 CALL_PROG1: PUT1(CALL_PROG2,INTF_LENGTH);
 CASE_JUMP1: IGNORE1(CASE_JUMP2);
 CASE_LIST1: CASE_LIST;
 CHK_TYPE1: BEGIN PUT0(CHK_TYPE2); PUT_TYPE END;
 CLASS1: COMP_DEF(PACKED_CLASS);
 CPARMLIST1: CPARM_LIST;
 DEF_LABEL1: IGNORE1(DEF_LABEL2);
 DIV1: PUT0(DIV2);
 EMPTY_SET1: PUT0(EMPTY_SET2);
 EOM1: EOM;
 ENUM_DEF1: ENUM_DEF;
 EQ1: PUT0(EQ2);
 EVAR_LIST1: EVAR_LIST;
 FALSEJUMP1: IGNORE1(FALSEJUMP2);
 FIELDLIST1: FIELDLIST;
 FOR_DOWN1: FOR_LOOP(FOR_DOWN2);
 FOR_LIM1: FOR_LIM;
 FOR_STORE1: PUT0(FOR_STORE2);
 FOR_UP1: FOR_LOOP(FOR_UP2);
 FUNC_DEF1,FUNCE_DEF1: FUNC_DEF(DONT_RESOLVE);
 FUNCF_DEF1: FUNC_DEF(RESOLVE);
 FUNCTION1: FUNCTION_;
 FWD_DEF1: FWD_DEF;
 GE1: PUT0(GE2);
 GT1: PUT0(GT2);
 INCLUDE1: PUT0(INCLUDE2);
 INDEX1: INDEX;
 INITS_DEF1: INITS_DEF;
 INIT1: PUT0(INIT2);
 INTF_ID1: INTF_ID;
 INTF1: INTF_LENGTH:=0;
 IN1: PUT0(IN2);
 JUMP_DEF1: IGNORE2(JUMP_DEF2);
 JUMP1: IGNORE1(JUMP2);
 LCONST1: LCONST;
 LE1: PUT0(LE2);
 LT1: PUT0(LT2);
 MESSAGE1: IGNORE2(MESSAGE2);
 MINUS1: PUT0(MINUS2);
 MOD1: PUT0(MOD2);
 MONITOR1: COMP_DEF(PACKED_MONITOR);
 NEW_LINE1: IGNORE1(NEW_LINE2);
 NEW_NOUN1: PUSH_NEW_ENTRY(NEW_ENTRY);
 NE1: PUT0(NE2);
 NOT1: PUT0(NOT2);
 OR1: PUT0(OR2);
 PARM_TYPE1: PARM_TYPE;
 PARM1: PARM;
 PEND1: PEND;
 PLUS1: PUT0(PLUS2);
 PROC_DEF1,PROCE_DEF1: ROUTINE_DEF(DONT_RESOLVE);
 PROCF_DEF1: ROUTINE_DEF(RESOLVE);
 PROCESS1: COMP_DEF(PACKED_PROCESS);
 PROG_DEF1: PROG_DEF;
 PSTART1: PSTART;
 RCOMP1: RCOMP(RCOMP2);
 REAL1: REAL_;
 REC_DEF1: REC_DEF;
 REC1: PUSH_LEVEL(RECORD_MODE);
 RESULT1: RESULT;
 ROUTINE1: RCOMP(ROUTINE2);
 SET_DEF1: SET_DEF;
 SLASH1: PUT0(SLASH2);
 STACK1: STACK_;
 STAR1: PUT0(STAR2);
 STORE1: PUT0(STORE2);
 STRING1: SSTRING;
 SUBR_DEF1: SUBR_DEF;
 SUB1: SUB;
 TYPE_DEF1: T:=T-1;
 TYPE1: TYPE_;
 UMINUS1: PUT0(UMINUS2);
 UNDEF1: PUT0(UNDEF2);
 UNIV_TYPE1: UNIV_TYPE;
 UPLUS1: PUT0(UPLUS2);
 VALUE1: PUT0(VALUE2);
 VAR_LIST1: VAR_LIST;
 VAR1: VCOMP(VAR2);
 VCOMP1: VCOMP(VCOMP2);
 VPARMLIST1: VPARMLIST;
 WITH_TEMP1: WITH_TEMP;
 WITH_VAR1: SAVE_CONTEXT:=TRUE;
 WITH1: WITH_
 END
 UNTIL DONE;

end;

procedure Pass5(var OK);

const
LISTOPTION = 0;    SUMMARYOPTION = 1;  TESTOPTION = 2;     CHECKOPTION = 3;
CODEOPTION = 4;    NUMBEROPTION = 5;
IDLENGTH = 12;
PRINTLIMIT = 18;
MAXDIGIT = 6;

{INPUT OPERATORS}
EOM1=1;            BODY1=2;            BODY_END1=3;        ADDRESS1=4;
RESULT1=5;         STORE1=6;           CALL_PROC1=7;       CONSTPARM1=8;
VARPARM1=9;        FALSEJUMP1=10;      DEF_LABEL1=11;      JUMP_DEF1=12;
CASE_JUMP1=13;     JUMP1=14;           CASE_LIST1=15;      FOR_STORE1=16;
FOR_LIM1=17;       FOR_UP1=18;         FOR_DOWN1=19;       WITH1=20;
INIT1=21;          PROG_CALL1=22;      INTF_LBL1=23;       VALUE1=24;
LT1=25;            EQ1=26;             GT1=27;             LE1=28;
NE1=29;            GE1=30;             IN1=31;             UPLUS1=32;
UMINUS1=33;        PLUS1=34;           MINUS1=35;          OR1=36;
STAR1=37;          SLASH1=38;          DIV1=39;            MOD1=40;
AND1=41;           EMPTY_SET1=42;      INCLUDE1=43;        FUNCTION1=44;
CALL_FUNC1=45;     ROUTINE1=46;        VAR1=47;            ARROW1=48;
VCOMP1=49;         RCOMP1=50;          SUB1=51;            LCONST1=52;
MESSAGE1=53;       NEW_LINE1=54;       CHK_TYPE1=55;       SAVEPARM1=56;
CALL_GEN1=57;      NOT1=58;            UNDEF1=59;          RANGE1=60;

{OUTPUT OPERATORS}
PUSHCONST2=0;      PUSHVAR2=1;         PUSHIND2=2;         PUSHADDR2=3;
FIELD2=4;          INDEX2=5;           POINTER2=6;         VARIANT2=7;
RANGE2=8;          ASSIGN2=9;          ASSIGNTAG2=10;      COPY2=11;
NEW2=12;           NOT2=13;            AND2=14;            OR2=15;
NEG2=16;           ADD2=17;            SUB2=18;            MUL2=19;
DIV2=20;           MOD2=21;            {NOT USED}          {NOT USED}
FUNCTION2=24;      BUILDSET2=25;       COMPARE2=26;        COMPSTRCT2=27;
FUNCVALUE2=28;     DEFLABEL2=29;       JUMP2=30;           FALSEJUMP2=31;
CASEJUMP2=32;      INITVAR2=33;        CALL2=34;           ENTER2=35;
RETURN2=36;        POP2=37;            NEWLINE2=38;        ERR2=39;
LCONST2=40;        MESSAGE2=41;        INCREMENT2=42;      DECREMENT2=43;
PROCEDURE2=44;     INIT2=45;           PUSHLABEL2=46;      CALLPROG2=47;
EOM2=48;
{CONTEXT}
FUNC_RESULT=1;     ENTRY_VAR=2;        VARIABLE=3;         VAR_PARM=4;
UNIV_VAR=5;        CONST_PARM=6;       UNIV_CONST=7;       FIELD=8;
EXPR=10;           CONSTANT=11;        SAVE_PARM=12;       WITH_CONST = 13;
WITH_VAR = 14;
{TYPE KIND}
INT_KIND=0;        REAL_KIND=1;        BOOL_KIND=2;        CHAR_KIND=3;
ENUM_KIND=4;       SET_KIND=5;         STRING_KIND=6;      PASSIVE_KIND=7;
POINTER_KIND=8;    QUEUE_KIND= 9;      GENERIC_KIND=10;    UNDEF_KIND=11;
SYSCOMP_KIND=12;   ROUTINE_KIND=13;    ACTIVE_KIND=14;
{STANDARD SPELLING/NOUN INDICES}
XUNDEF=0;          XFALSE=1;           XTRUE=2;            XINTEGER=3;
XBOOLEAN=4;        XCHAR=5;            XQUEUE=6;           XABS=7;
XATTRIBUTE=8;      XCHR=9 ;            XCONTINUE=10;       XCONV=11;
XDELAY=12;         XEMPTY=13;          XIO=14;             XORD=15;
XPRED=16;          XSTOP=17;           XREALTIME=18;       XSETHEAP=19;
XSUCC=20;          XTRUNC=21;          XSTART=22;          XWAIT=23;
XREAL=24;
 {STANDARD NOUN INDICES}
ZARITHMETIC=25;    ZINDEX=26;          ZPASSIVE=27;        ZVPARM=28;
ZCPARM=29;         ZSPARM=30;          ZWITH=31;
{DATA TYPS}
BYTE_TYP=0;        WORD_TYP=1;         REAL_TYP=2;         SET_TYP=3;
STRUCT_TYP=4;
{ADDRESS MODES}
LCONST_MODE=0;      PROC_MODE=1;        PROG_MODE=2;
PE_MODE=3;         CE_MODE=4;          ME_MODE=5;          PROCESS_MODE=6;
CLASS_MODE=7;      MONITOR_MODE=8;     STD_MODE=9;         UNDEF_MODE=10;
SCONST_MODE=11;
TEMP_MODE=PROC_MODE;
{COMPARISONS}
LESS=0;            EQUAL=1;            GREATER=2;          NOTLESS=3;
NOTEQUAL=4;        NOTGREATER=5;       INSET=6;
{ERRORS}
COMPILER_ERROR=1;  TYPE_ERROR=2;       ADDRESS_ERROR=3;    ASSIGN_ERROR=4;
INIT_ERROR = 5;
THIS_PASS=5;       BYTELENGTH = 1;


TYPE
PASSPTR = ^PASSLINK;
PASSLINK =
  RECORD
    {OPTIONS: SET OF OPTION;}
    LABELS, BLOCKS, CONSTANTS, RESETPOINT: INTEGER;
    TABLES: POINTER
  END;
ARGTAG =  (NILTYPE, BOOLTYPE, INTTYPE, IDTYPE, PTRTYPE);
IDENTIFIER = ARRAY (.1..IDLENGTH.) OF CHAR;

ARGTYPE = RECORD
                 CASE TAG: ARGTAG OF
                   NILTYPE, BOOLTYPE: (BOOL: BOOLEAN);
                   INTTYPE: (INT: INTEGER);
                   IDTYPE: (ID: IDENTIFIER);
                   PTRTYPE: (PTR: PASSPTR)
               END;


  ADDR_STATE=(DIRECT,INDIRECT,ADDR,EXPRESSION);
  ADDR_MODE=LCONST_MODE..SCONST_MODE;
  ADDR_MODES=SET OF ADDR_MODE;

  DISPLACEMENT=INTEGER;
  TYPE_KIND=INT_KIND..{ROUTINE_KIND}ACTIVE_KIND;
  TYPE_KINDS=SET OF TYPE_KIND;
  CONTEXT_KIND=FUNC_RESULT..WITH_VAR;
  CONTEXTS=SET OF CONTEXT_KIND;
  OPERAND_CLASS=(UNDEFINED,VALUE,ROUTINE);
  OPERAND=
    RECORD
      KIND:TYPE_KIND; NOUN:INTEGER;
      MODE:ADDR_MODE; DISP:DISPLACEMENT; LENGTH:DISPLACEMENT;
      CASE CLASS1:OPERAND_CLASS OF
        VALUE:(CONTEXT:CONTEXT_KIND; STATE:ADDR_STATE);
        ROUTINE:(PARM_SIZE,VAR_SIZE,STACK_SIZE:DISPLACEMENT)
    END;
  OPERAND_PTR=^OPERAND;
  STACK_LINK=^STACK_ENTRY;
  STACK_ENTRY=RECORD
                OPND:OPERAND_PTR;
                RESET_POINT:INTEGER;
                NEXT_ENTRY:STACK_LINK
              END;

VAR
 INT_EXPR,REAL_EXPR,BOOL_EXPR,SET_EXPR,UNDEF_EXPR: OPERAND;

  SY: INTEGER;
  S,T: OPERAND_PTR;
 { INTER_PASS_PTR: PASSPTR;}
  CURRENT_MODE: ADDR_MODE;
  ROUTINE_MODES, INIT_MODES: ADDR_MODES;
  TOP_STACK,THIS_STACK,EMPTY_STACK:STACK_LINK;
  DONE: BOOLEAN;
  PASSIVES,INDEXS,LARGES,ARITHMETIC,INDIRECTS,SMALLS: TYPE_KINDS;
  UNIVERSAL,ASSIGNS,VAR_PARMS,CNST_PARMS, PARMS: CONTEXTS;




  PROCEDURE PUT_ARG(ARG:INTEGER);
  BEGIN
    ADD(ARG,outfile5);
    IF DEBUG THEN PRINTARG(ARG)
  END;

  PROCEDURE PUT0(OP:INTEGER);
  BEGIN
    ADD(OP,outfile5);
    IF DEBUG THEN PRINTOP(OP)
  END;

  PROCEDURE PUT1(OP,ARG1:INTEGER);
  BEGIN
    ADD(OP,outfile5); ADD(ARG1,outfile5);
      IF DEBUG THEN BEGIN
      PRINTOP(OP); PRINTARG(ARG1)
    END
  END;

  PROCEDURE PUT2(OP,ARG1,ARG2:INTEGER);
  BEGIN
   ADD(OP,outfile5); ADD(ARG1,outfile5); ADD(ARG2,outfile5);
    IF DEBUG THEN BEGIN
      PRINTOP(OP); PRINTARG(ARG1); PRINTARG(ARG2)
    END
  END;

  PROCEDURE PUT3(OP,ARG1,ARG2,ARG3:INTEGER);
  BEGIN
    PUT2(OP,ARG1,ARG2);
    PUT_ARG(ARG3)
  END;

  PROCEDURE PUT4(OP,ARG1,ARG2,ARG3,ARG4:INTEGER);
  BEGIN
    ADD(OP,outfile5); ADD(ARG1,outfile5); ADD(ARG2,outfile5);
    ADD(ARG3,outfile5); ADD(ARG4,outfile5);
    IF DEBUG THEN BEGIN
      PRINTOP(OP); PRINTARG(ARG1); PRINTARG(ARG2);
      PRINTARG(ARG3); PRINTARG(ARG4)
    END
  END;

  PROCEDURE PUT5(OP,ARG1,ARG2,ARG3,ARG4,ARG5:INTEGER);
  BEGIN
    ADD(OP,outfile5); ADD(ARG1,outfile5); ADD(ARG2,outfile5);
    ADD(ARG3,outfile5); ADD(ARG4,outfile5); ADD(ARG5,outfile5);
    IF DEBUG THEN BEGIN
      PRINTOP(OP); PRINTARG(ARG1); PRINTARG(ARG2);
      PRINTARG(ARG3); PRINTARG(ARG4); PRINTARG(ARG5)
    END
  END;

function MODE_TO_INT(MODE:ADDR_MODE): integer;
begin

{LCONST_MODE=0;      PROC_MODE=1;        PROG_MODE=2;
PE_MODE=3;         CE_MODE=4;          ME_MODE=5;          PROCESS_MODE=6;
CLASS_MODE=7;      MONITOR_MODE=8;     STD_MODE=9;         UNDEF_MODE=10;
SCONST_MODE=11;}
case MODE of
 LCONST_MODE: result:=0;
 PROC_MODE: result:=1;
 PROG_MODE : result:=2;
 PE_MODE: result:=3;
 CE_MODE: result:=4;
 ME_MODE: result:=5;
 PROCESS_MODE: result:=6;
 CLASS_MODE: result:=7;
 MONITOR_MODE: result:=8;
 STD_MODE: result:=9;
 UNDEF_MODE: result:=10;
 SCONST_MODE:result:=11;
 else
  result:=0;
 end;
end;

function  IntToMODE (I:integer):ADDR_MODE;
begin
case I of
  0: result:=LCONST_MODE;
  1: result:=PROC_MODE;
  2: result:=PROG_MODE;
  3: result:=PE_MODE;
  4: result:=CE_MODE;
  5: result:=ME_MODE ;
  6: result:=PROCESS_MODE;
  7: result:=CLASS_MODE ;
  8: result:=MONITOR_MODE ;
  9: result:=STD_MODE;
 10: result:=UNDEF_MODE;
 11: result:=SCONST_MODE;
end;
end;

function IntToCONTEXT(I:integer):CONTEXT_KIND;
begin
 {FUNC_RESULT=1;     ENTRY_VAR=2;        VARIABLE=3;         VAR_PARM=4;
UNIV_VAR=5;        CONST_PARM=6;       UNIV_CONST=7;       FIELD=8;
EXPR=10;           CONSTANT=11;        SAVE_PARM=12;       WITH_CONST = 13;
WITH_VAR = 14;}
 case I of

  1: result:=FUNC_RESULT;
  2: result:=ENTRY_VAR;
  3: result:=VARIABLE;
  4:  result:=VAR_PARM;
  5: result:=UNIV_VAR;
  6:  result:=CONST_PARM;
  7:  result:=UNIV_CONST;
  8: result:=FIELD;
  {9: result:=STD_MODE;}
 10: result:=EXPR;
 11: result:=CONSTANT;
 12: result:=SAVE_PARM;
 13: result:=WITH_CONST;
 14: result:= WITH_VAR;

 end;
end;

{"NOTE: A PASS RUNNING WITH TEST OUTPUT SHOULD START WITH PRINTFF}

{#########################}
{OPERAND STACK MANIPULATION}
{#########################}


 PROCEDURE POP;
  BEGIN
    T:=S;
    TOP_STACK:=TOP_STACK^.NEXT_ENTRY;
    {RELEASE(TOP_STACK^.RESET_POINT); }
    IF TOP_STACK=EMPTY_STACK
     THEN
       S:=NIL
     ELSE S:=TOP_STACK^.NEXT_ENTRY^.OPND;
  END;

  PROCEDURE PUSH;
  BEGIN
    S:=T;
    NEW(THIS_STACK);
    WITH THIS_STACK^ DO BEGIN
      NEW(OPND); T:=OPND;
      NEXT_ENTRY:=TOP_STACK; {MARK(RESET_POINT)}
    END;
    TOP_STACK:=THIS_STACK
  END;


{#########}
{INITIALIZE}
{#########}
  PROCEDURE INITIALIZE;
  BEGIN
    DONE:=FALSE;
  {  INIT_PASS(INTER_PASS_PTR);
    WITH INTER_PASS_PTR^ DO BEGIN
      DEBUG:=TESTOPTION IN OPTIONS;
      IF DEBUG THEN PRINTFF
    END;
  END; }
    IF DEBUG THEN PRINTFF(THIS_PASS);
    first(outfile4);
    INIT(outfile5);
    ARITHMETIC:=(.INT_KIND,REAL_KIND.);
    INDEXS:=(.INT_KIND,BOOL_KIND,CHAR_KIND,ENUM_KIND.);
    SMALLS:=INDEXS + (.REAL_KIND,SET_KIND,QUEUE_KIND,POINTER_KIND.);
    PASSIVES:=INDEXS + (.REAL_KIND,SET_KIND,POINTER_KIND,STRING_KIND,
      PASSIVE_KIND.);
    LARGES:=(.STRING_KIND,PASSIVE_KIND,ACTIVE_KIND,SYSCOMP_KIND.);
    INDIRECTS:=LARGES;
    INIT_MODES:= (.CLASS_MODE, MONITOR_MODE, PROCESS_MODE.);
    ROUTINE_MODES:= (.PROC_MODE,PE_MODE,CE_MODE,ME_MODE.);
    UNIVERSAL:=(.UNIV_VAR,UNIV_CONST.);
    ASSIGNS:=(.FUNC_RESULT,VARIABLE,VAR_PARM,UNIV_VAR, WITH_VAR.);
    VAR_PARMS:=(.VAR_PARM,UNIV_VAR.);
    CNST_PARMS:=(.CONST_PARM,UNIV_CONST.);
    PARMS:= VAR_PARMS + CNST_PARMS;
    S:=NIL; T:=NIL; NEW(EMPTY_STACK); TOP_STACK:=EMPTY_STACK;
    WITH EMPTY_STACK^ DO BEGIN
      NEXT_ENTRY:=NIL; OPND:=NIL; {MARK(RESET_POINT)}
    END;
    WITH INT_EXPR DO BEGIN
      KIND:=INT_KIND; NOUN:=XINTEGER; LENGTH:=WORDLENGTH;
      MODE:=UNDEF_MODE;
      CLASS1:=VALUE; CONTEXT:=EXPR; STATE:=EXPRESSION
    END;
    REAL_EXPR:=INT_EXPR;
    WITH REAL_EXPR DO BEGIN
      KIND:=REAL_KIND; NOUN:=XREAL;     LENGTH:=REALLENGTH
    END;
    BOOL_EXPR:=INT_EXPR;
    WITH BOOL_EXPR DO BEGIN
      KIND:=BOOL_KIND; NOUN:=XBOOLEAN
    END;
    SET_EXPR:=INT_EXPR;
    WITH SET_EXPR DO BEGIN
      KIND:=SET_KIND; NOUN:=XUNDEF;     LENGTH:=SETLENGTH1
    END;
    UNDEF_EXPR:=INT_EXPR;
    WITH UNDEF_EXPR DO BEGIN
      KIND:=UNDEF_KIND; NOUN:=XUNDEF
    END;
    PUT1(JUMP2,1) {JUMP TO BLOCK LABEL 1, THE INITIAL PROCESS}
  END;
  {#####}

{ERRORS}
{#####}
  PROCEDURE ERROR1(ERROR: INTEGER);
  BEGIN
    WITH T^ DO
      IF KIND=UNDEF_KIND THEN {SUPPRESS MESSAGE}
      ELSE {PUT2(MESSAGE2,THIS_PASS,ERROR);}
      begin
       write(errors,' C', MESSAGE2,' ',THIS_PASS,' ',ERROR);
       writeln(errors);
       writeln(tiskarna);
       writeln(tiskarna, MESSAGE2);
       PUT_E(THIS_PASS,ERROR, 0);
      end;
     T^:=UNDEF_EXPR
  END;

  PROCEDURE ERROR2(ERROR:INTEGER);
  BEGIN
    IF (T^.KIND=UNDEF_KIND) OR (S^.KIND=UNDEF_KIND) THEN {SUPPRESS MESSAGE}
    ELSE {PUT2(MESSAGE2,THIS_PASS,ERROR);}
      begin
       write(errors,' C', MESSAGE2,' ',THIS_PASS,' ',ERROR);
       writeln(errors);
       writeln(tiskarna);
       writeln(tiskarna, MESSAGE2);
       PUT_E(THIS_PASS,ERROR,0);
      end;
    S^:=UNDEF_EXPR
  END;

  PROCEDURE ERROR2P(ERROR:INTEGER);
  BEGIN
    ERROR2(ERROR); POP
  END;

  PROCEDURE EOM;
  VAR VAR_LENGTH:DISPLACEMENT;
  BEGIN
    get_l(outfile4,VAR_LENGTH); PUT1(EOM2,VAR_LENGTH);
    DONE:=TRUE
  END;

  PROCEDURE ABORT;
  BEGIN
   { PUT2(MESSAGE2,THIS_PASS,COMPILER_ERROR); }
     write(errors, ' ',THIS_PASS,' ',compiler_ERROR);
     writeln(errors);
     PUT_E(THIS_PASS,compiler_ERROR,0);
    EOM
  END;

 {############}
{TYPE CHECKING}
{############}
  FUNCTION TTYP:INTEGER {TYPE CODE};
  BEGIN
    WITH T^ DO
      CASE KIND OF
        INT_KIND,BOOL_KIND,ENUM_KIND,POINTER_KIND,
        QUEUE_KIND,UNDEF_KIND: TTYP:=WORD_TYP;
        REAL_KIND: TTYP:=REAL_TYP;
        CHAR_KIND: IF LENGTH=WORDLENGTH THEN TTYP:=WORD_TYP
          ELSE TTYP:=BYTE_TYP;
        SET_KIND: TTYP:=SET_TYP;
        STRING_KIND,PASSIVE_KIND: TTYP:=STRUCT_TYP;
       GENERIC_KIND,SYSCOMP_KIND,ROUTINE_KIND:
         BEGIN
          ERROR1(TYPE_ERROR); TTYP:=WORD_TYP END;
        ACTIVE_KIND:  BEGIN
          ERROR1(TYPE_ERROR); TTYP:=WORD_TYP END;
      END
  END;

  FUNCTION COMPATIBLE:BOOLEAN;
  {VAR RESULT1:BOOLEAN; }
  BEGIN
    IF (T^.CLASS1 <> VALUE) OR (S^.CLASS1 <> VALUE) THEN
      RESULT:= FALSE ELSE
    IF T^.CONTEXT IN UNIVERSAL THEN
      RESULT:=(S^.KIND IN PASSIVES) AND (T^.LENGTH=S^.LENGTH)
    ELSE
    IF T^.KIND=S^.KIND THEN
      CASE T^.KIND OF
        INT_KIND,REAL_KIND,BOOL_KIND,CHAR_KIND,
        QUEUE_KIND: RESULT:=TRUE;
        ENUM_KIND,PASSIVE_KIND:
          RESULT:=T^.NOUN=S^.NOUN;
        SYSCOMP_KIND :  RESULT:=T^.NOUN=S^.NOUN;
        ACTIVE_KIND:  RESULT:=T^.NOUN=S^.NOUN;
        STRING_KIND:
          RESULT:=(T^.LENGTH=S^.LENGTH) OR (T^.CONTEXT IN CNST_PARMS);
        SET_KIND,POINTER_KIND:
          RESULT:=(T^.NOUN=S^.NOUN) OR (T^.NOUN=XUNDEF)
            OR (S^.NOUN=XUNDEF);
        UNDEF_KIND,ROUTINE_KIND: RESULT:=FALSE
      END
    ELSE IF T^.KIND=GENERIC_KIND THEN
      CASE T^.NOUN OF
        ZARITHMETIC: RESULT:=S^.KIND IN ARITHMETIC;
        ZINDEX: RESULT:=S^.KIND IN INDEXS;
        ZPASSIVE: RESULT:=S^.KIND IN PASSIVES
      END
    ELSE RESULT:=FALSE;
    IF NOT RESULT THEN
       ERROR2(TYPE_ERROR);
    COMPATIBLE:=RESULT
  END;

{#####}
{IGNORE}
{#####}
  PROCEDURE LCONST;
  VAR LENGTH,I,ARG:INTEGER;
  BEGIN
   get_l(outfile4,LENGTH); PUT1(LCONST2,LENGTH);
    FOR I:=1 TO LENGTH {DIV WORDLENGTH} DO BEGIN
      get_L(outfile4,ARG); PUT_ARG(ARG)
    END
  END;

  PROCEDURE IGNORE1(OP:INTEGER);
  VAR ARG:INTEGER;
  BEGIN
    get_l(outfile4,ARG); PUT1(OP,ARG)
  END;

  PROCEDURE IGNORE2(OP:INTEGER);
  VAR ARG1,ARG2:INTEGER;
  BEGIN
    get_l(outfile4,ARG1);  get_l(outfile4,ARG2);
    PUT2(OP,ARG1,ARG2)
  END;

{###}
{BODY}
{###}
  PROCEDURE ROUTINE_;
  var
   X:integer;
   BEGIN
    PUSH;
    WITH T^ DO BEGIN
      get_l(outfile4, {MODE} X);
      MODE:=IntToMOde(X);
      get_l(outfile4,DISP);
      CLASS1:=ROUTINE;
       get_l(outfile4,PARM_SIZE);  get_l(outfile4,VAR_SIZE);
       get_l(outfile4,STACK_SIZE)
    END
  END;

  PROCEDURE BODY;
  BEGIN
    ROUTINE_;
    WITH T^ DO BEGIN
      PUT5(ENTER2,MODE,DISP,PARM_SIZE,VAR_SIZE,STACK_SIZE);
      CURRENT_MODE:=MODE
    END
  END;

  PROCEDURE BODY_END;
  BEGIN
    PUT1(RETURN2,CURRENT_MODE);
    POP
  END;

{######}
{LOADING}
{######}
  PROCEDURE ADDR_ERROR;
  BEGIN
    ERROR1(ADDRESS_ERROR);
    PUT1(PUSHCONST2,0)
  END;

  PROCEDURE ADDRESS;
  BEGIN
    WITH T^ DO
      IF CLASS1=VALUE THEN BEGIN
        CASE STATE OF
          DIRECT: BEGIN
            IF MODE=SCONST_MODE THEN ADDR_ERROR
            ELSE PUT2(PUSHADDR2,MODE,DISP);
            IF KIND=SYSCOMP_KIND THEN PUT1(FIELD2,LENGTH) {OFFSET}
          END;
          INDIRECT: PUT3(PUSHVAR2,WORD_TYP,MODE,DISP);
          ADDR: ;
          EXPRESSION: ADDR_ERROR
        END;
        STATE:=ADDR
      END ELSE ADDR_ERROR
  END;

  PROCEDURE TYPE_;
  var
   X:integer;
  BEGIN
    WITH T^ DO BEGIN
       get_l(outfile4,X{KIND});
       KIND:=IntToCONTEXT(X);
       get_l(outfile4,NOUN);
       get_l(outfile4,LENGTH)
    END
  END;

  PROCEDURE RESULT;
  BEGIN
    WITH T^ DO BEGIN
      CLASS1:=VALUE;
       get_l(outfile4,DISP);
      PUT2(PUSHADDR2,MODE,DISP);
      CONTEXT:=FUNC_RESULT; STATE:=ADDR;
      {RESULT} TYPE_
    END
  END;

  PROCEDURE VALUE_;
  BEGIN
    WITH T^ DO BEGIN
      IF KIND IN SMALLS THEN BEGIN {LOAD VALUE}
        CASE STATE OF
          DIRECT: IF MODE=SCONST_MODE THEN PUT1(PUSHCONST2,DISP) ELSE
            PUT3(PUSHVAR2,TTYP,MODE,DISP);
          INDIRECT: BEGIN
            PUT3(PUSHVAR2,WORD_TYP,MODE,DISP);
            PUT1(PUSHIND2,TTYP)
          END;
          ADDR: PUT1(PUSHIND2,TTYP);
          EXPRESSION:
        END;
        IF LENGTH=BYTELENGTH THEN LENGTH:=WORDLENGTH;
        STATE:=EXPRESSION
      END ELSE IF KIND IN INDIRECTS THEN ADDRESS
      ELSE {ERROR} PUT1(PUSHCONST2,0);
      CONTEXT:=EXPR
    END
  END;

  PROCEDURE STORE(POPVAR:BOOLEAN);
  VAR TYP:INTEGER; SIMILAR:BOOLEAN;
  BEGIN
    {EXPRESSION} VALUE_;
    SIMILAR:=COMPATIBLE;
    POP {EXPRESSION};
    IF SIMILAR THEN WITH T^ DO
      IF CONTEXT IN ASSIGNS THEN BEGIN
        TYP:=TTYP;
        IF TYP=STRUCT_TYP THEN PUT1(COPY2,LENGTH)
        ELSE PUT1(ASSIGN2,TYP)
      END ELSE ERROR1(ASSIGN_ERROR);
    IF POPVAR THEN POP {VARIABLE}
  END;

{#########}
{STATEMENTS}
{#########}
  PROCEDURE VAR_REF;
  var
   X,Y:integer;
  BEGIN
    WITH T^ DO BEGIN
      CLASS1:=VALUE;
      get_l(outfile4,{MODE}X);
      MODE:=IntToMOde(X);
      get_l(outfile4,DISP);
      get_l(outfile4,{CONTEXT}Y);
      CONTEXT:=IntToCONTEXT(Y);
    END
  END;

  PROCEDURE VAR_;
  BEGIN
    PUSH;
    VAR_REF; {VAR}
    TYPE_;
    WITH T^ DO
      IF(CONTEXT IN VAR_PARMS) OR
        (CONTEXT IN CNST_PARMS) AND (KIND IN LARGES)
        THEN STATE:=INDIRECT ELSE STATE:=DIRECT
  END;

  PROCEDURE CALL_PROC;
  BEGIN
    WITH T^ DO
      IF CLASS1=ROUTINE THEN
        IF MODE=STD_MODE THEN PUT1(PROCEDURE2,DISP)
        ELSE PUT3(CALL2,MODE,DISP,PARM_SIZE);
    POP
  END;

  PROCEDURE CONSTPARM(GENERIC: BOOLEAN);
  BEGIN
    {PARAMETER} VAR_;
    IF COMPATIBLE THEN IF T^.CONTEXT = UNIV_CONST
     THEN S^.KIND:= T^.KIND;
    POP {PARAMETER};
    {ARGUMENT} VALUE_;
    IF GENERIC THEN S^ {FUNCTION RESULT} :=
      T^ {ACTUAL ARGUMENT};
    POP {ARGUMENT}
  END;

  PROCEDURE VARPARM;
  BEGIN
    {ARGUMENT} ADDRESS;
    {PARAMETER} VAR_;
    IF COMPATIBLE THEN
      IF NOT (S^.CONTEXT IN ASSIGNS) THEN
        ERROR2(ASSIGN_ERROR);
    POP {PARAMETER};
    POP {ARGUMENT}
  END;

  PROCEDURE FALSE_JUMP;
  VAR L:DISPLACEMENT;
  BEGIN
    {BOOLEAN} VALUE_;
    IF T^.KIND<>BOOL_KIND THEN ERROR1(TYPE_ERROR);
    get_l(outfile4,L); PUT1(FALSEJUMP2,L);
    POP
  END;

  PROCEDURE CASE_JUMP;
  VAR L: DISPLACEMENT;
  BEGIN
    {SELECTOR} VALUE_;
    get_l(outfile4,L); PUT1(JUMP2,L)
  END;

  PROCEDURE DEF_LABEL;
  VAR L:DISPLACEMENT;
  BEGIN
   get_l(outfile4, L); PUT1(DEFLABEL2,L)
  END;

  PROCEDURE JUMP;
  VAR L:DISPLACEMENT;
  BEGIN
    get_l(outfile4,L); PUT1(JUMP2,L)
  END;

  PROCEDURE JUMP_DEF;
  BEGIN
    JUMP; DEF_LABEL
  END;

  PROCEDURE CHK_TYPE;
  BEGIN
    PUSH; T^:=INT_EXPR; TYPE_;
    IF COMPATIBLE THEN {OK};
    POP
  END;

  PROCEDURE CASE_LIST;
  VAR I,MIN,MAX:INTEGER; L:DISPLACEMENT;
  BEGIN
    POP {SELECTOR};
    DEF_LABEL;
    get_l(outfile4,MIN);
    get_l(outfile4,MAX);
    PUT2(CASEJUMP2,MIN,MAX);
    FOR I:=MIN TO MAX DO BEGIN
      get_l(outfile4,L); PUT_ARG(L)
    END;
    DEF_LABEL
  END;

  PROCEDURE POP_TEMP;
  BEGIN
    POP;
    PUT1(POP2,WORDLENGTH)
  END;

  PROCEDURE FOR_STORE;
  CONST LEAVE_FOR_VAR=FALSE;
  BEGIN
    {INITIAL} VALUE_;
    STORE(LEAVE_FOR_VAR);
    T^.STATE:=DIRECT
  END;

  PROCEDURE FOR_LIM;
  VAR OP:INTEGER; LIMIT_DISP:DISPLACEMENT; LABEL1:DISPLACEMENT;
  BEGIN
    {FINAL} VALUE_;
    DEF_LABEL;
    POP {LIMIT};
    {CONTROL VAR} VALUE_;
    T^.STATE:=DIRECT;
    get_l(outfile4,LIMIT_DISP);
    PUT3(PUSHVAR2,WORD_TYP,TEMP_MODE,LIMIT_DISP);
    get_l(outfile4,{COMPARISON"}OP);
    PUT2(COMPARE2,OP,WORD_TYP);
    get_l(outfile4,LABEL1); PUT1(FALSEJUMP2,LABEL1)
  END;

  PROCEDURE FOR_LOOP(OP:INTEGER);
  BEGIN
    {CONTROL VAR} ADDRESS;
    PUT0(OP);
    JUMP_DEF;
    POP_TEMP
  END;

  PROCEDURE INIT_;
  BEGIN
    WITH T^ DO
      IF CLASS1=ROUTINE THEN PUT4(INIT2,MODE,DISP,PARM_SIZE,VAR_SIZE)
        ELSE PUT4(INIT2,PROCESS_MODE,0,0,0);
      POP
  END;

  PROCEDURE INTF_LBL;
  VAR L:DISPLACEMENT;
  BEGIN
    get_l(outfile4, L); PUT1(PUSHLABEL2,L)
  END;

  PROCEDURE PROG_CALL;
  VAR INTF_LENGTH:INTEGER;
  BEGIN
    get_l(outfile4,INTF_LENGTH);
    PUT0(CALLPROG2);  PUT1(POP2,INTF_LENGTH);
    POP
  END;

 {#########}
{EXPRESSION}
{#########}
  PROCEDURE EQUALITY(OP:INTEGER);
  BEGIN
    {RIGHT OPERAND} VALUE_;
    IF COMPATIBLE THEN
      CASE T^.KIND OF
        CHAR_KIND,INT_KIND,BOOL_KIND,  ENUM_KIND,POINTER_KIND,
        REAL_KIND,SET_KIND: PUT2(COMPARE2,OP,TTYP);
        STRING_KIND,PASSIVE_KIND: PUT2(COMPSTRCT2,OP,T^.LENGTH);
        ACTIVE_KIND,QUEUE_KIND,GENERIC_KIND,UNDEF_KIND,
        SYSCOMP_KIND,ROUTINE_KIND: ERROR2(TYPE_ERROR)
      END;
    POP; T^:=BOOL_EXPR
  END;

  PROCEDURE INEQUALITY(OP:INTEGER);
  BEGIN
    {RIGHT OPERAND} VALUE_;
    IF COMPATIBLE THEN
      CASE T^.KIND OF
        INT_KIND,REAL_KIND,CHAR_KIND,BOOL_KIND,ENUM_KIND,SET_KIND:
          PUT2(COMPARE2,OP,TTYP);
        STRING_KIND: PUT2(COMPSTRCT2,OP,T^.LENGTH);
        PASSIVE_KIND,ACTIVE_KIND,POINTER_KIND,QUEUE_KIND,GENERIC_KIND,
        UNDEF_KIND,SYSCOMP_KIND,ROUTINE_KIND: ERROR2(TYPE_ERROR)
      END;
    POP; T^:=BOOL_EXPR
  END;

  PROCEDURE STRICT_INEQUALITY(OP:INTEGER);
  BEGIN
    {RIGHT OPERAND} VALUE_;
    IF COMPATIBLE THEN
      CASE T^.KIND OF
        INT_KIND,REAL_KIND,BOOL_KIND,CHAR_KIND,ENUM_KIND:
          PUT2(COMPARE2,OP,TTYP);
        STRING_KIND: PUT2(COMPSTRCT2,OP,T^.LENGTH);
        SET_KIND,POINTER_KIND,PASSIVE_KIND,ACTIVE_KIND,QUEUE_KIND,
        SYSCOMP_KIND,ROUTINE_KIND,UNDEF_KIND: ERROR2(TYPE_ERROR)
      END;
    POP; T^:=BOOL_EXPR
  END;

  PROCEDURE INCLUSION;
  BEGIN
    {RIGHT OPERAND} VALUE_;
    IF (T^.KIND=SET_KIND) AND (S^.KIND IN INDEXS)
      AND (S^.NOUN=T^.NOUN) THEN PUT2(COMPARE2,INSET,SET_TYP)
    ELSE ERROR2(TYPE_ERROR);
    POP; T^:=BOOL_EXPR
  END;

  PROCEDURE UMINUS;
  BEGIN
    {OPERAND} VALUE_;
    IF T^.KIND IN ARITHMETIC THEN PUT1(NEG2,TTYP) ELSE ERROR1(TYPE_ERROR)
  END;

  PROCEDURE UPLUS;
  BEGIN
    {OPERAND} VALUE_;
    IF T^.KIND IN ARITHMETIC THEN {OK} ELSE ERROR1(TYPE_ERROR)
  END;

  PROCEDURE PLUS_MINUS_STAR(OP:INTEGER);
  VAR TNOUN:INTEGER;
  BEGIN
    {RIGHT OPERAND} VALUE_;
    IF T^.KIND=S^.KIND THEN
      IF T^.KIND=INT_KIND THEN BEGIN
        PUT1(OP,WORD_TYP);
        POP; T^:=INT_EXPR
      END ELSE IF T^.KIND=REAL_KIND THEN BEGIN
        PUT1(OP,REAL_TYP);
        POP; T^:=REAL_EXPR
      END ELSE IF (T^.KIND=SET_KIND) AND (OP=SUB2)
        AND COMPATIBLE THEN BEGIN
        PUT1(SUB2,SET_TYP); TNOUN:=T^.NOUN;
        POP; T^:=SET_EXPR; T^.NOUN:=TNOUN
      END ELSE ERROR2P(TYPE_ERROR)
    ELSE ERROR2P(TYPE_ERROR)
  END;

  PROCEDURE SLASH;
  BEGIN
    {RIGHT OPERAND} VALUE_;
    IF (T^.KIND=REAL_KIND) AND (S^.KIND=REAL_KIND) THEN
      PUT1(DIV2,REAL_TYP)
    ELSE ERROR2(TYPE_ERROR);
    POP; T^:=REAL_EXPR
  END;

  PROCEDURE DIV_MOD(OP:INTEGER);
  BEGIN
    {RIGHT OPERAND} VALUE_;
    IF (T^.KIND=INT_KIND) AND (S^.KIND=INT_KIND) THEN
      PUT1(OP,WORD_TYP)
    ELSE ERROR2(TYPE_ERROR);
    POP; T^:=INT_EXPR
  END;

  PROCEDURE OR_AND(OP:INTEGER);
  VAR TNOUN:INTEGER;
  BEGIN
    {RIGHT OPERAND} VALUE_;
    IF T^.KIND=S^.KIND THEN
      IF T^.KIND=BOOL_KIND THEN BEGIN
        PUT1(OP,WORD_TYP);
        POP; T^:=BOOL_EXPR
      END ELSE IF (T^.KIND=SET_KIND)
        AND COMPATIBLE THEN BEGIN
        PUT1(OP,SET_TYP); TNOUN:=T^.NOUN;
        POP; T^:=SET_EXPR; T^.NOUN:=TNOUN
      END ELSE ERROR2P(TYPE_ERROR)
    ELSE ERROR2P(TYPE_ERROR)
  END;

  PROCEDURE NOT_;
  BEGIN
    {OPERAND} VALUE_;
    IF T^.KIND<>BOOL_KIND THEN ERROR1(TYPE_ERROR);
    T^:=BOOL_EXPR;
    PUT0(NOT2)
  END;

  PROCEDURE EMPTY_SET;
  BEGIN
    PUSH; T^:=SET_EXPR;
    PUT3(PUSHVAR2,SET_TYP,LCONST_MODE,0)
  END;

  PROCEDURE INCLUDE;
  BEGIN
    {SET MEMBER} VALUE_;
    IF T^.KIND IN INDEXS THEN BEGIN
      IF S^.NOUN=XUNDEF     THEN S^.NOUN:=T^.NOUN
      ELSE IF S^.NOUN<>T^.NOUN THEN ERROR2(TYPE_ERROR);
      PUT0(BUILDSET2)
    END ELSE ERROR2(TYPE_ERROR);
    POP
  END;

  PROCEDURE FUNCTION_;
  BEGIN
    PUSH; T^:= UNDEF_EXPR; T^.CONTEXT:= FUNC_RESULT;
    {FUNC} TYPE_;
    WITH S^ DO
      IF (CLASS1 = ROUTINE) AND (MODE <> STD_MODE)
      THEN PUT2(FUNCVALUE2, MODE, TTYP)
  END;

  PROCEDURE CALL_FUNC;
  BEGIN
    WITH S^ DO
     IF CLASS1 = ROUTINE THEN
      IF MODE=STD_MODE THEN PUT2(FUNCTION2, DISP, TTYP)
      ELSE PUT3(CALL2, MODE, DISP, PARM_SIZE);
    S^:=T^; POP
  END;

  PROCEDURE CALL_GEN;
  BEGIN
    WITH S^ DO PUT2(FUNCTION2,DISP,TTYP);
    T^.CONTEXT:= FUNC_RESULT; S^:= T^;
    POP {ARG}
  END;

 {#######}
{VARIABLE}
{#######}
  PROCEDURE UNDEF;
  BEGIN
    PUSH; T^:=UNDEF_EXPR;
    PUT1(PUSHCONST2,0)
  END;

  PROCEDURE VCOMP;
  VAR SAVE_CONTEXT:INTEGER;
  BEGIN
    {RECORD OR CLASS} ADDRESS; SAVE_CONTEXT:=T^.CONTEXT;
    VAR_REF; TYPE_;
    WITH T^ DO BEGIN
      PUT1(FIELD2,DISP);
      STATE:=ADDR;
      IF CONTEXT=VARIABLE THEN CONTEXT:=ENTRY_VAR
      ELSE CONTEXT:=SAVE_CONTEXT;
      IF KIND = SYSCOMP_KIND THEN PUT1(FIELD2, LENGTH);
    END;
  END;

  PROCEDURE RCOMP;
  VAR INITABLE: BOOLEAN;
  BEGIN
    WITH T^ DO
      IF CLASS1 = VALUE THEN
        IF CONTEXT IN PARMS THEN INITABLE:= FALSE ELSE INITABLE:= TRUE
      ELSE INITABLE:= TRUE;
    {SYSCOMP} ADDRESS;
    POP;
    {ENTRY} ROUTINE_;
    IF T^.MODE IN INIT_MODES THEN
      IF NOT INITABLE THEN ERROR1(INIT_ERROR)
  END;

  PROCEDURE SUB;
  VAR MIN,MAX,SIZE: INTEGER;
  BEGIN
    {SUBSCRIPT} VALUE_;
    get_l(outfile4,MIN); get_l(outfile4,MAX); get_l(outfile4,SIZE);
    PUT3(INDEX2,MIN,MAX,SIZE);
    PUSH;
    T^:=UNDEF_EXPR; {INDEX}
    TYPE_;
    IF COMPATIBLE THEN {OK};
    POP;
    POP;
    {ELEMENT} TYPE_;
    WITH T^ DO
      IF KIND=SYSCOMP_KIND THEN PUT1(FIELD2,LENGTH) {OFFSET}
  END;

  PROCEDURE ARROW;
  VAR SAVE_CONTEXT:CONTEXT_KIND;
  BEGIN
    WITH T^ DO
      IF KIND=POINTER_KIND THEN BEGIN
        SAVE_CONTEXT:=CONTEXT;
        {POINTER} VALUE_; CONTEXT:=SAVE_CONTEXT;
        STATE:=ADDR
      END ELSE ERROR1(TYPE_ERROR);
    {OBJECT} TYPE_
  END;


begin

{MAIN PROGRAM PASS5}
INITIALIZE;
REPEAT {MAIN LOOP}
 get_l(outfile4,SY);
 CASE SY OF
 ADDRESS1: ADDRESS;
 AND1: OR_AND(AND2);
 ARROW1: ARROW;
 BODY_END1: BODY_END;
 BODY1: BODY;
 CALL_FUNC1: CALL_FUNC;
 CALL_GEN1: CALL_GEN;
 CALL_PROC1: CALL_PROC;
 CASE_JUMP1: CASE_JUMP;
 CASE_LIST1: CASE_LIST;
 CHK_TYPE1: CHK_TYPE;
 CONSTPARM1: CONSTPARM(FALSE);
 DEF_LABEL1: DEF_LABEL;
 DIV1: DIV_MOD(DIV2);
 EMPTY_SET1: EMPTY_SET;
 EOM1: EOM;
 EQ1: EQUALITY(EQUAL);
 FALSEJUMP1: FALSE_JUMP;
 FOR_DOWN1: FOR_LOOP(DECREMENT2);
 FOR_LIM1: FOR_LIM;
 FOR_STORE1: FOR_STORE;
 FOR_UP1: FOR_LOOP(INCREMENT2);
 FUNCTION1: FUNCTION_;
 GE1: INEQUALITY(NOTLESS);
 GT1: STRICT_INEQUALITY(GREATER);
 INCLUDE1: INCLUDE;
 INIT1: INIT_;
 INTF_LBL1: INTF_LBL;
 IN1: INCLUSION;
 JUMP_DEF1: JUMP_DEF;
 JUMP1: JUMP;
 LCONST1: LCONST;
 LE1: INEQUALITY(NOTGREATER);
 LT1: STRICT_INEQUALITY(LESS);
 MESSAGE1: IGNORE2(MESSAGE2);
 MINUS1: PLUS_MINUS_STAR(SUB2);
 MOD1: DIV_MOD(MOD2);
 NEW_LINE1: IGNORE1(NEWLINE2);
 NE1: EQUALITY(NOTEQUAL);
 NOT1: NOT_;
 OR1: OR_AND(OR2);
 PLUS1: PLUS_MINUS_STAR(ADD2);
 PROG_CALL1: PROG_CALL;
 RANGE1: IGNORE2(RANGE2);
 RCOMP1: RCOMP;
 RESULT1: RESULT;
 ROUTINE1: ROUTINE_;
 SAVEPARM1: CONSTPARM(TRUE);
 SLASH1: SLASH;
 STAR1: PLUS_MINUS_STAR(MUL2);
 STORE1: STORE(TRUE);
 SUB1: SUB;
 UMINUS1: UMINUS;
 UNDEF1: UNDEF;
 UPLUS1: UPLUS;
 VALUE1: VALUE_;
 VARPARM1: VARPARM;

 VAR1: VAR_;
 VCOMP1: VCOMP;
 WITH1: POP_TEMP
 END
 UNTIL DONE;

end;

procedure Pass6(var OK:boolean;var LINK: PASSPTR);

const
PRINTLIMIT = 18;   MAXDIGIT = 6;
 IDLENGTH = 12;

MAXARG = 10;


{INPUT OPERATORS}

PUSHCONST1 = 0;    PUSHVAR1 = 1;       PUSHIND1 = 2;       PUSHADDR1 = 3;
FIELD1 = 4;        INDEX1 = 5;         POINTER1 = 6;       VARIANT1 = 7;
RANGE1 = 8;        ASSIGN1 = 9;        ASSIGNTAG1 = 10;    COPY1 = 11;
NEW1 = 12;         NOT1 = 13;          AND1 = 14;          OR1 = 15;
NEG1 = 16;         ADD1 = 17;          SUB1 = 18;          MUL1 = 19;
DIV1 = 20;         MOD1 = 21;          {NOT USED}          {NOT USED}
FUNCTION1 = 24;    BUILDSET1 = 25;     COMPARE1 = 26;      COMPSTRUC1 = 27;
FUNCVALUE1 = 28;   DEFLABEL1 = 29;     JUMP1 = 30;         FALSEJUMP1 = 31;
CASEJUMP1 = 32;    INITVAR1 = 33;      CALL1 = 34;         ENTER1 = 35;
RETURN1 = 36;      POP1 = 37;          NEWLINE1 = 38;      ERROR1 = 39;
CONSTANT1 = 40;    MESSAGE1 = 41;      INCREMENT1 = 42;    DECREMENT1 = 43;
PROCEDURE1 = 44;   INIT1 = 45;         PUSHLABEL1 = 46;    CALLPROG1 = 47;
EOM1=48;
{VIRTUAL DATA TYPES}
BYTETYPE = 0;      WORDTYPE = 1;       REALTYPE = 2;       SETTYPE = 3;
{VIRTUAL ADDRESSING MODES}
MODE0 = 0 {CONSTANT};
MODE1 = 1 {PROCEDURE};
MODE2 = 2 {PROGRAM};
MODE3 = 3 {PROCESS ENTRY};
MODE4 = 4 {CLASS ENTRY};
MODE5 = 5 {MONITOR ENTRY};
MODE6 = 6 {PROCESS};
MODE7 = 7 {CLASS};
MODE8 = 8 {MONITOR};
MODE9 = 9 {STANDARD};
MODE10=10 {UNDEFINED};
{COMPARISON OPERATORS}
LESS = 0;          EQUAL = 1;          GREATER = 2;        NOTLESS = 3;
NOTEQUAL = 4;      NOTGREATER = 5;     INSET = 6;
{STANDARD FUNCTIONS}
TRUNC1 = 0;        ABS1 = 1;           SUCC1 = 2;          PRED1 = 3;
CONV1 = 4;         EMPTY1 = 5;         ATTRIBUTE1 = 6;     REALTIME1 = 7;
MIN_FUNC = 0;      MAX_FUNC = 7;
{STANDARD PROCEDURES}
DELAY1 = 0;        CONTINUE1 = 1;      IO1 = 2;            START1 = 3;
STOP1 = 4;         SETHEAP1 = 5;       WAIT1 = 6;
MIN_PROC = 0;      MAX_PROC = 6;

{OUTPUT OPERATORS}
CONSTADDR2 = 0;    LOCALADDR2 = 1;     GLOBADDR2 = 2;      PUSHCONST2 = 3;
PUSHLOCAL2 = 4;    PUSHGLOB2 = 5;      PUSHIND2 = 6;       PUSHBYTE2 = 7;
PUSHREAL2 = 8;     PUSHSET2 = 9;       FIELD2 = 10;        INDEX2 = 11;
POINTER2 = 12;     VARIANT2 = 13;      RANGE2 = 14;        COPYBYTE2 = 15;
COPYWORD2 = 16;    COPYREAL2 = 17;     COPYSET2 = 18;      COPYTAG2 = 19;
COPYSTRUC2 = 20;   NEW2 = 21;          NEWINIT2 = 22;      NOT2 = 23;
ANDWORD2 = 24;     ANDSET2 = 25;       ORWORD2 = 26;       ORSET2 = 27;
NEGWORD2 = 28;     NEGREAL2 = 29;      ADDWORD2 = 30;      ADDREAL2 = 31;
SUBWORD2 = 32;     SUBREAL2 = 33;      SUBSET2 = 34;       MULWORD2 = 35;
MULREAL2 = 36;     DIVWORD2 = 37;      DIVREAL2 = 38;      MODWORD2 = 39;
BUILDSET2 = 40;    INSET2 = 41;        LSWORD2 = 42;       EQWORD2 = 43;
GRWORD2 = 44;      NLWORD2 = 45;       NEWORD2 = 46;       NGWORD2 = 47;
LSREAL2 = 48;      EQREAL2 = 49;       GRREAL2 = 50;       NLREAL2 = 51;
NEREAL2 = 52;      NGREAL2 = 53;       EQSET2 = 54;        NLSET2 = 55;
NESET2 = 56;       NGSET2 = 57;        LSSTRUCT2 = 58;     EQSTRUCT2 = 59;
GRSTRUCT2 = 60;    NLSTRUCT2 = 61;     NESTRUCT2 = 62;     NGSTRUCT2 = 63;
FUNCVALUE2 = 64;   JUMP2 = 65;         FALSEJUMP2 = 66;    CASEJUMP2 = 67;
INITVAR2 = 68;     CALL2 = 69;         CALLSYS2 = 70;      ENTER2 = 71;
EXIT2 = 72;        ENTERPROG2 = 73;    EXITPROG2 = 74;     BEGINCLAS2 = 75;
ENDCLASS2 = 76;    ENTERCLAS2 = 77;    EXITCLASS2 = 78;    BEGINMON2 = 79;
ENDMON2 = 80;      ENTERMON2 = 81;     EXITMON2 = 82;      BEGINPROC2 = 83;
ENDPROC2 = 84;     ENTERPROC2 = 85;    EXITPROC2 = 86;     POP2 = 87;
NEWLINE2 = 88;     INCRWORD2 = 89;     DECRWORD2 = 90;     INITCLASS2 = 91;
INITMON2 = 92;     INITPROC2 = 93;     PUSHLABEL2 = 94;    CALLPROG2 = 95;
TRUNCREAL2 = 96;   ABSWORD2 = 97;      ABSREAL2 = 98;      SUCCWORD2 = 99;
PREDWORD2 = 100;   CONVWORD2 = 101;    EMPTY2 = 102;       ATTRIBUTE2 = 103;
REALTIME2 = 104;   DELAY2 = 105;       CONTINUE2 = 106;    IO2 = 107;
START2 = 108;      STOP2 = 109;        SETHEAP2 = 110;     WAIT2 = 111;
MESSAGE2 = 112;    EOM2=113;
{OTHER CONSTANTS}
PDP11 = TRUE;
CONCURRENT = TRUE;
INITIALBLOCK = 1;
SPLITLENGTH = {4}2 {WORDS PER REAL};
TWOWORDS = {4}2;      THREEWORDS = {6}3;     FOURWORDS = {8}4;
FIVEWORDS = {10}5;
STACK_LIMIT = 32667 {GREATEST INTEGER - 100};              CODE_LIMIT = 32667;
THIS_PASS = 6;
{INFILE = 2;        OUTFILE = 1;}
STACK_ERROR = 1;   CODE_ERROR = 2;

TYPE
IDENTIFIER = ARRAY (.1..IDLENGTH.) OF CHAR;
POINTER = ^ INTEGER;

ARGTAG =
  (NILTYPE, BOOLTYPE, INTTYPE, IDTYPE, PTRTYPE);
ARGTYPE = RECORD
                 CASE TAG: ARGTAG OF
                   NILTYPE, BOOLTYPE: (BOOL: BOOLEAN);
                   INTTYPE: (INT: INTEGER);
                   IDTYPE: (ID: IDENTIFIER);
                   PTRTYPE: (PTR: PASSPTR)
               END;


var
{LINK: PASSPTR;}

SUMMARY, CHECK, NUMBER, AFTERBEGIN, AFTERERROR, DONE: BOOLEAN;
JUMPTABLE, BLOCKTABLE, STACKTABLE, CONSTTABLE: TABLEPTR;
CONSTANTS, STACKLENGTH, VARLENGTH, PARAMLENGTH, POPLENGTH, TEMP,
MAXTEMP, BLOCK, LOCATION, LINE, OP, ARG1, ARG2, ARG3, ARG4, ARG5: INTEGER;

{################}
{INPUT PROCEDURES}
{################}
PROCEDURE READ1ARG;
BEGIN  get_l(outfile5,arg1) END;

PROCEDURE READ2ARG;
BEGIN
get_l(outfile5,arg1);
get_l(outfile5,arg2)
END;

PROCEDURE READ3ARG;
BEGIN
  get_l(outfile5,arg1);
  get_l(outfile5,arg2);
  get_l(outfile5,arg3)
  END;

PROCEDURE READ4ARG;
BEGIN
   get_l(outfile5,arg1);
   get_l(outfile5,arg2);
   get_l(outfile5,arg3) ;
   get_l(outfile5,arg4) ;
END;

PROCEDURE READ5ARG;
BEGIN
   get_l(outfile5,arg1);
   get_l(outfile5,arg2);
   get_l(outfile5,arg3);
   get_l(outfile5,arg4);
   get_l(outfile5,arg5);
END;


(*#################*)
(*OUTPUT PROCEDURES*)
(*#################*)
PROCEDURE ERROR (PASS, NUMBER: INTEGER); FORWARD;

PROCEDURE WRITE1(OP: INTEGER);
BEGIN
  IF TEST THEN PRINTOP(OP);
  add(OP,outfile6);
  IF LOCATION < CODE_LIMIT THEN LOCATION:= LOCATION + WORDLENGTH
    ELSE BEGIN ERROR(THIS_PASS, CODE_ERROR); LOCATION:= 0 END;
END;

PROCEDURE WRITE2(OP, ARG: INTEGER);
BEGIN
  IF TEST THEN
  BEGIN PRINTOP(OP); PRINTARG(ARG) END;
  add(OP,outfile6); add(arg,outfile6);
  IF LOCATION < CODE_LIMIT THEN LOCATION:= LOCATION + TWOWORDS
    ELSE BEGIN ERROR(THIS_PASS, CODE_ERROR); LOCATION:= 0 END;
END;

PROCEDURE WRITE3(OP, ARG1, ARG2: INTEGER);
BEGIN
  IF TEST THEN
  BEGIN PRINTOP(OP);
    PRINTARG(ARG1); PRINTARG(ARG2);
  END;
  add(OP,outfile6); add(arg1,outfile6);  add(arg2,outfile6);
  IF LOCATION < CODE_LIMIT THEN LOCATION:= LOCATION + THREEWORDS
    ELSE BEGIN ERROR(THIS_PASS, CODE_ERROR); LOCATION:= 0 END;
END;

PROCEDURE WRITE4(OP, ARG1, ARG2, ARG3: INTEGER);
BEGIN
  IF TEST THEN
  BEGIN
    PRINTOP(OP); PRINTARG(ARG1);
    PRINTARG(ARG2); PRINTARG(ARG3);
  END;
  add(OP,outfile6); add(arg1,outfile6);  add(arg2,outfile6);
  add(arg3,outfile6);
  IF LOCATION < CODE_LIMIT THEN LOCATION:= LOCATION + FOURWORDS
    ELSE BEGIN ERROR(THIS_PASS, CODE_ERROR); LOCATION:= 0 END;
END;

PROCEDURE WRITE5(OP, ARG1, ARG2, ARG3, ARG4: INTEGER);
BEGIN
  IF TEST THEN
  BEGIN PRINTOP(OP);
    PRINTARG(ARG1); PRINTARG(ARG2);
    PRINTARG(ARG3); PRINTARG(ARG4);
  END;
  add(OP,outfile6); add(arg1,outfile6);  add(arg2,outfile6);
  add(arg3,outfile6); add(arg4,outfile6);
  IF LOCATION < CODE_LIMIT THEN LOCATION:= LOCATION + FIVEWORDS
    ELSE BEGIN ERROR(THIS_PASS, CODE_ERROR); LOCATION:= 0 END;
END;

PROCEDURE WRITEARG(ARG: INTEGER);
BEGIN
  IF TEST THEN PRINTARG(ARG);
  add(arg,outfile6);
  IF LOCATION < CODE_LIMIT THEN LOCATION:= LOCATION + WORDLENGTH
    ELSE BEGIN ERROR(THIS_PASS, CODE_ERROR); LOCATION:= 0 END;
END;

PROCEDURE WRITELOCATION;
BEGIN
  IF TEST THEN PRINTARG(LOCATION);
  add(location,outfile6);
END;

PROCEDURE COMMENT(LENGTH: INTEGER);
BEGIN LOCATION:= LOCATION - LENGTH END;

PROCEDURE ERROR;
BEGIN
  IF NOT AFTERERROR THEN BEGIN
    AFTERERROR:= TRUE;
    COMMENT(FOURWORDS);
    WRITE4(MESSAGE2, PASS, NUMBER, LINE);
    GENERATE:= FALSE
  END
END;

{################ }
{STACK PROCEDURES"}
{################}

PROCEDURE PUSHWORD;
BEGIN
  IF TEMP < STACK_LIMIT THEN TEMP:= TEMP + WORDLENGTH
    ELSE ERROR(THIS_PASS, STACK_ERROR);
  IF TEMP > MAXTEMP THEN MAXTEMP:= TEMP;
END;

PROCEDURE POPWORD;
BEGIN TEMP:= TEMP - WORDLENGTH END;
PROCEDURE PUSHREAL;
BEGIN
  IF TEMP < STACK_LIMIT THEN TEMP:= TEMP + REALLENGTH
    ELSE ERROR(THIS_PASS, STACK_ERROR);
  IF TEMP > MAXTEMP THEN MAXTEMP:= TEMP;
END;

PROCEDURE POPREAL;
BEGIN TEMP:= TEMP - REALLENGTH END;
PROCEDURE PUSHSET;
BEGIN
  IF TEMP < STACK_LIMIT THEN TEMP:= TEMP + SETLENGTH1
    ELSE ERROR(THIS_PASS, STACK_ERROR);
  IF TEMP > MAXTEMP THEN MAXTEMP:= TEMP;
END;

PROCEDURE POPSET;
BEGIN TEMP:= TEMP - SETLENGTH1 END;
PROCEDURE PUSH(LENGTH: INTEGER);
BEGIN
  IF TEMP < STACK_LIMIT - LENGTH THEN TEMP:= TEMP + LENGTH
    ELSE ERROR(THIS_PASS, STACK_ERROR);
  IF TEMP > MAXTEMP THEN MAXTEMP:= TEMP;
END;

PROCEDURE POP(LENGTH: INTEGER);
BEGIN TEMP:= TEMP - LENGTH END;

{###################}
{VARIABLE PROCEDURES}
{###################}
FUNCTION DISPL(ARG: INTEGER): INTEGER;
BEGIN
  IF ARG < 0 THEN DISPL:= ARG
             ELSE DISPL:= ARG + FOURWORDS;
END;

PROCEDURE PUSHVALUE(MODE, ARG: INTEGER);
VAR ADDR: INTEGER;
BEGIN
  CASE MODE OF
    MODE1, MODE3, MODE4, MODE5:
      WRITE2(PUSHLOCAL2, DISPL(ARG));
    MODE2:
      BEGIN
        ADDR:= DISPL(ARG);
        IF ADDR > 0 THEN ADDR:= ADDR + WORDLENGTH;
        WRITE2(PUSHGLOB2, ADDR)
      END;
    MODE6, MODE7, MODE8:
      WRITE2(PUSHGLOB2, ARG);
    MODE10:
  END;
  PUSHWORD;
END;

PROCEDURE PUSHADDRESS(MODE, ARG: INTEGER);
VAR ADDR: INTEGER;
BEGIN
  CASE MODE OF
    MODE0:
      WRITE2(CONSTADDR2, ARG);
    MODE1, MODE3, MODE4, MODE5:
      WRITE2(LOCALADDR2, DISPL(ARG));
    MODE2:
      BEGIN
        ADDR:= DISPL(ARG);
        IF ADDR > 0 THEN ADDR:= ADDR + WORDLENGTH;
        WRITE2(GLOBADDR2, ADDR)
      END;
    MODE6, MODE7, MODE8:
      WRITE2(GLOBADDR2, ARG);
    MODE10:
  END;
  PUSHWORD;
END;

PROCEDURE PUSHINDIRECT(VARTYPE: INTEGER);
BEGIN
  CASE VARTYPE OF
    BYTETYPE:
      WRITE1(PUSHBYTE2);
    WORDTYPE:
      WRITE1(PUSHIND2);
    REALTYPE:
      BEGIN WRITE1(PUSHREAL2);
        POPWORD; PUSHREAL;
      END;
    SETTYPE:
      BEGIN WRITE1(PUSHSET2);
        POPWORD; PUSHSET;
      END
  END;
END;

{#####################}
{COMPARISON PROCEDURES}
{#####################}
PROCEDURE COMPAREWORD(ARG: INTEGER);
BEGIN
  CASE ARG OF
    LESS:        WRITE1(LSWORD2);
    EQUAL:       WRITE1(EQWORD2);
    GREATER:     WRITE1(GRWORD2);
    NOTLESS:     WRITE1(NLWORD2);
    NOTEQUAL:    WRITE1(NEWORD2);
    NOTGREATER:  WRITE1(NGWORD2)
  END;
  POPWORD;
END;

PROCEDURE COMPAREREAL(ARG: INTEGER);
BEGIN
  CASE ARG OF
    LESS:        WRITE1(LSREAL2);
    EQUAL:       WRITE1(EQREAL2);
    GREATER:     WRITE1(GRREAL2);
    NOTLESS:     WRITE1(NLREAL2);
    NOTEQUAL:    WRITE1(NEREAL2);
    NOTGREATER:  WRITE1(NGREAL2)
  END;
  POPREAL; POPREAL; PUSHWORD;
END;

PROCEDURE COMPARESET(ARG: INTEGER);
BEGIN
  CASE ARG OF
    EQUAL:       WRITE1(EQSET2);
    NOTLESS:     WRITE1(NLSET2);
    NOTEQUAL:    WRITE1(NESET2);
    NOTGREATER:  WRITE1(NGSET2);
    INSET:       WRITE1(INSET2)
  END;
  POPSET;
  IF ARG <> INSET THEN
  BEGIN POPSET; PUSHWORD END;
END;

PROCEDURE COMPARESTRUCT(ARG1, ARG2: INTEGER);
BEGIN
  CASE ARG1 OF
    LESS:        WRITE1(LSSTRUCT2);
    EQUAL:       WRITE1(EQSTRUCT2);
    GREATER:     WRITE1(GRSTRUCT2);
    NOTLESS:     WRITE1(NLSTRUCT2);
    NOTEQUAL:    WRITE1(NESTRUCT2);
    NOTGREATER:  WRITE1(NGSTRUCT2)
  END;
  WRITEARG(ARG2 DIV WORDLENGTH);
  POPWORD;
END;

{################}
{TABLE PROCEDURES}
{################}
PROCEDURE ALLOCATE(VAR T: TABLEPTR; ENTRIES: INTEGER);
VAR PORTION: TABLEPTR; I: INTEGER;
BEGIN
  NEW(T); PORTION:= T;
  I:= ENTRIES - MAXWORD;
  WHILE I > 0 DO
  WITH PORTION^ DO
  BEGIN
    NEW(NEXTPORTION); PORTION:= NEXTPORTION;
    I:= I - MAXWORD;
  END;
END;

PROCEDURE ENTER(T: TABLEPTR; I, J: INTEGER);
VAR PORTION: TABLEPTR; K: INTEGER;
BEGIN
  PORTION:= T; K:= I;
  WHILE K > MAXWORD DO
  BEGIN
    PORTION:= PORTION^.NEXTPORTION;
    K:= K - MAXWORD;
  END;
  PORTION^.CONTENTS(.K.):= J;
END;

FUNCTION ENTRY(T: TABLEPTR; I: INTEGER): INTEGER;
VAR PORTION: TABLEPTR; J: INTEGER;
BEGIN
  PORTION:= T; J:= I;
  WHILE J > MAXWORD DO
  BEGIN
    PORTION:= PORTION^.NEXTPORTION;
    J:= J - MAXWORD;
  END;
  ENTRY:= PORTION^.CONTENTS(.J.);
END;

{###############
{LINE PROCEDURES"}
{###############}
PROCEDURE NEWLINE(ARG: INTEGER);
BEGIN
  LINE:= ARG;
  AFTERERROR:=FALSE;
  IF NUMBER AND AFTERBEGIN THEN WRITE2(NEWLINE2,LINE)
END;

PROCEDURE INITLINE;
BEGIN
  LINE:=0; AFTERBEGIN:=FALSE
END;

{################}
{BLOCK PROCEDURES}
{################}
PROCEDURE ENTERBLOCK(I, J, K, L: INTEGER);
BEGIN
  BLOCK:= I; PARAMLENGTH:= J; VARLENGTH:= K; STACKLENGTH:=L;
  POPLENGTH:= PARAMLENGTH + FOURWORDS;
  TEMP:= 0; MAXTEMP:= 0;
  IF BLOCK=INITIALBLOCK THEN ENTER(JUMPTABLE,BLOCK,LOCATION)
    ELSE ENTER(BLOCKTABLE,BLOCK,LOCATION);
  {THE INITIAL BLOCK IS ONLY REFERENCED BY THE FIRST JUMP INSTRUCTION
  IN A PROGRAM, BUT NOT BY ANY CALL OR INIT INSTRUCTION}
  AFTERBEGIN:=TRUE
END;

PROCEDURE EXITBLOCK;
BEGIN
  IF STACKLENGTH < STACK_LIMIT - MAXTEMP - VARLENGTH THEN
    STACKLENGTH:= STACKLENGTH + MAXTEMP + VARLENGTH + FIVEWORDS
  ELSE ERROR(THIS_PASS, STACK_ERROR);
  ENTER(STACKTABLE, BLOCK, STACKLENGTH);
  AFTERBEGIN:=FALSE
END;

{#########################################}
{INITIALIZATION AND TERMINATION PROCEDURES}
{#########################################}
PROCEDURE BEGINPASS;
BEGIN
  IF TEST THEN PRINTFF(THIS_PASS);
  WITH LINK^ DO
  BEGIN
    SUMMARY:= SUMMARYOPTION IN OPTIONS;
    CHECK:= CHECKOPTION IN OPTIONS;
    NUMBER:= NUMBEROPTION IN OPTIONS;
    GENERATE:= TRUE;
    {MARK(RESETPOINT); }
    ALLOCATE(JUMPTABLE, LABELS);
    ALLOCATE(BLOCKTABLE, BLOCKS);
    ALLOCATE(STACKTABLE, BLOCKS);
    ALLOCATE(CONSTTABLE, CONSTANTS DIV WORDLENGTH);
  END;
  LOCATION:= 0; CONSTANTS:= 0;
  INITLINE;
  IF TEST THEN PRINTFF(THIS_PASS);
END;

PROCEDURE ENDPASS;
BEGIN
  WITH LINK^ DO
  BEGIN
    IF GENERATE THEN OPTIONS:= OPTIONS + (.CODEOPTION.);
    NEW(TABLES);
    TABLES^.PROGLENGTH:= FOURWORDS + LOCATION + CONSTANTS;
    TABLES^.CODELENGTH:= LOCATION;
    TABLES^.STACKLENGTH:= STACKLENGTH;
    TABLES^.VARLENGTH:= VARLENGTH;
    TABLES^.JUMPTABLE:=JUMPTABLE;
    TABLES^.BLOCKTABLE:=BLOCKTABLE;
    TABLES^.STACKTABLE:=STACKTABLE;
    TABLES^.CONSTTABLE:=CONSTTABLE;
  END;
END;

{#########}
{OPERATORS}
{#########}
PROCEDURE SCAN;
var
  I,J:integer;
BEGIN
  DONE:=FALSE;
  REPEAT
    get_l(outfile5, OP);
    CASE OP OF
PUSHCONST1 {(VALUE)}:
  BEGIN READ1ARG;
    WRITE2(PUSHCONST2, ARG1);
    PUSHWORD;
  END;
PUSHVAR1{(TYPE, MODE, DISPL)}:
  BEGIN READ3ARG;
    IF ARG1 = WORDTYPE
      THEN PUSHVALUE(ARG2, ARG3)
      ELSE BEGIN
             PUSHADDRESS(ARG2, ARG3);
             PUSHINDIRECT(ARG1);
           END;
  END;
PUSHIND1 {TYPE)}:
  BEGIN
   READ1ARG;
   PUSHINDIRECT(ARG1) END;
PUSHADDR1{(MODE, DISPL)}:
  BEGIN
  READ2ARG;
  PUSHADDRESS(ARG1, ARG2)
  END;
FIELD1{(DISPL)}:
  BEGIN
   READ1ARG;
   IF ARG1<>0 THEN
    WRITE2(FIELD2,ARG1) END;
INDEX1{(MIN, MAX, LENGTH)}:
  BEGIN READ3ARG;
    WRITE4(INDEX2, ARG1, ARG2 - ARG1, ARG3);
    POPWORD;
  END;
POINTER1:
  IF CHECK THEN WRITE1(POINTER2);
VARIANT1{(TAGSET, DISPL)}:
  BEGIN READ2ARG;
    IF CHECK THEN WRITE3(VARIANT2, ARG2, ARG1);
  END;
RANGE1{(MIN, MAX)}:
  BEGIN READ2ARG;
    IF CHECK THEN
     WRITE3(RANGE2, ARG1, ARG2);
  END;
ASSIGN1{(TYPE)}:
  BEGIN READ1ARG;
    CASE ARG1 OF
      BYTETYPE:
        BEGIN
         WRITE1(COPYBYTE2);
         POPWORD
        END;
      WORDTYPE:
        BEGIN
         WRITE1(COPYWORD2);
         POPWORD
        END;
      REALTYPE:
        BEGIN
        WRITE1(COPYREAL2);
        POPREAL
        END;
      SETTYPE:
        BEGIN
         WRITE1(COPYSET2);
         POPSET
        END
    END;
    POPWORD;
  END;
ASSIGNTAG1{(LENGTH)}:
  BEGIN  READ1ARG;
    IF ARG1 = 0 THEN
     WRITE1(COPYWORD2)
    ELSE
     WRITE2(COPYTAG2, ARG1 DIV WORDLENGTH);
     POPWORD;
     POPWORD;
  END;
COPY1{(LENGTH)}:
  BEGIN READ1ARG; WRITE2(COPYSTRUC2, ARG1 DIV WORDLENGTH);
    POPWORD;
    POPWORD;
  END;
NEW1{(LENGTH, INITIALIZE)}:
  BEGIN READ2ARG;
    IF (ARG2 = 1) and CHECK
      THEN WRITE3(NEWINIT2, BLOCK, ARG1)
      ELSE WRITE3(NEW2, BLOCK, ARG1);
    POPWORD;
  END;
NOT1:
  WRITE1(NOT2);
AND1 {(TYPE)}:
  BEGIN READ1ARG;
    IF ARG1 = WORDTYPE
      THEN BEGIN
       WRITE1(ANDWORD2);
       POPWORD END
      ELSE BEGIN
       WRITE1(ANDSET2);
       POPSET END;
  END;
OR1{(TYPE)}:
  BEGIN READ1ARG;
    IF ARG1 = WORDTYPE
      THEN BEGIN WRITE1(ORWORD2);
                 POPWORD END
      ELSE BEGIN WRITE1(ORSET2);
                 POPSET END;
  END;
NEG1{(TYPE)}:
  BEGIN READ1ARG;
    IF ARG1 = WORDTYPE THEN WRITE1(NEGWORD2)
                       ELSE WRITE1(NEGREAL2);
  END;
ADD1{(TYPE)}:
  BEGIN READ1ARG;
    IF ARG1 = WORDTYPE
      THEN BEGIN WRITE1(ADDWORD2);
                 POPWORD END
      ELSE BEGIN WRITE1(ADDREAL2);
                 POPREAL END;
  END;
SUB1{(TYPE)}:
  BEGIN READ1ARG;
    CASE ARG1 OF
      WORDTYPE:
        BEGIN WRITE1(SUBWORD2);
              POPWORD END;
      REALTYPE:
        BEGIN WRITE1(SUBREAL2);
              POPREAL END;
      SETTYPE:
        BEGIN WRITE1(SUBSET2);
              POPSET END
    END;
  END;
MUL1{(TYPE)}:
  BEGIN READ1ARG;
    IF ARG1 = WORDTYPE
      THEN BEGIN WRITE1(MULWORD2);
      POPWORD END
      ELSE BEGIN WRITE1(MULREAL2);
      POPREAL END;
  END;
DIV1{(TYPE)}:
  BEGIN READ1ARG;
    IF ARG1 = WORDTYPE
      THEN BEGIN WRITE1(DIVWORD2);
      POPWORD END
      ELSE BEGIN WRITE1(DIVREAL2);
      POPREAL END;
  END;
MOD1{(TYPE)}:
  BEGIN READ1ARG;
   WRITE1(MODWORD2);
   POPWORD END;
   {END; }
  {(NOT USED)}
{(NOT USED)}
FUNCTION1{(STANDARDFUNC, TYPE)}:
  BEGIN READ2ARG;
    IF (ARG1 >= MIN_FUNC) AND (ARG1 <= MAX_FUNC) THEN
    CASE ARG1 OF
      TRUNC1:
        BEGIN WRITE1(TRUNCREAL2);
              POPREAL;
              PUSHWORD END;
      ABS1:
        IF ARG2 = WORDTYPE THEN WRITE1(ABSWORD2)
                           ELSE WRITE1(ABSREAL2);
      SUCC1:
        WRITE1(SUCCWORD2);
      PRED1:
        WRITE1(PREDWORD2);
      CONV1:
        BEGIN WRITE1(CONVWORD2);
              POPWORD;
              PUSHREAL END;
      EMPTY1:
        WRITE1(EMPTY2);
      ATTRIBUTE1:
        WRITE1(ATTRIBUTE2);
      REALTIME1:
        BEGIN WRITE1(REALTIME2);
              PUSHWORD END
    END;
  END;
BUILDSET1:
  BEGIN WRITE1(BUILDSET2);
        POPWORD END;
COMPARE1{COMPARISON, TYPE)}:
  BEGIN READ2ARG;
    CASE ARG2 OF
      WORDTYPE:
        COMPAREWORD(ARG1);
      REALTYPE:
        COMPAREREAL(ARG1);
      SETTYPE:
        COMPARESET(ARG1)
    END;
  END;
COMPSTRUC1{COMPARISON, LENGTH)}:
  BEGIN READ2ARG;
  COMPARESTRUCT(ARG1, ARG2) END;
FUNCVALUE1{MODE)}:
  BEGIN READ2ARG;
    CASE ARG1 OF
      MODE1, MODE3:
      IF ARG2 = WORDTYPE THEN BEGIN
        WRITE2(FUNCVALUE2, 0);
        PUSHWORD
      END ELSE BEGIN
        WRITE2(FUNCVALUE2, 8);
        PUSHREAL
      END;
      MODE4, MODE5:
      IF ARG2 = WORDTYPE THEN BEGIN
        WRITE2(FUNCVALUE2, 16);
        PUSHWORD
      END ELSE BEGIN
        WRITE2(FUNCVALUE2, 24);
        PUSHREAL
      END;
        MODE9, MODE10:
    END;
  END;
DEFLABEL1 {(LABEL)}:
  BEGIN READ1ARG;
    ENTER(JUMPTABLE, ARG1, LOCATION);
    IF NUMBER THEN WRITE2(NEWLINE2,LINE)
  END;
JUMP1{LABEL)}:
  BEGIN READ1ARG;
    WRITE1(JUMP2);
    WRITELOCATION;
    WRITEARG(ARG1);
  END;
FALSEJUMP1{(LABEL)}:
  BEGIN READ1ARG;
    WRITE1(FALSEJUMP2);
    WRITELOCATION;
    WRITEARG(ARG1);
    POPWORD;
  END;
CASEJUMP1{(MIN, MAX, LABELS)}:
  BEGIN
    READ2ARG;
    ARG2:=  ARG2 - ARG1;
    {I:=ARG3;}
    WRITE3(CASEJUMP2, ARG1, ARG2);
     WRITELOCATION;
    FOR I:= 0 TO ARG2 DO
    BEGIN
     READ1ARG;
     WRITEARG(ARG1) END;
    POPWORD;
  END;
INITVAR1{(LENGTH)}:
  BEGIN READ1ARG;
    IF CHECK THEN WRITE2(INITVAR2, ARG1 DIV WORDLENGTH);
  END;
CALL1{(MODE, LABEL1, PARAMLENGTH)"}:
  BEGIN READ3ARG;
    IF ARG1 = MODE3 THEN
    BEGIN WRITE2(CALLSYS2, (ARG2 - 2) * WORDLENGTH);
      ARG1:= WORDLENGTH;
    END ELSE
    BEGIN
      WRITE1(CALL2);
      WRITELOCATION;
      WRITEARG(ARG2);
      IF ARG1<>MODE1 THEN ARG3:=ARG3+WORDLENGTH;
      {INCLUDES COMPONENT ADDRESS IN PARAMLENGTH}
      IF CONCURRENT
        THEN ARG1:= ENTRY(STACKTABLE, ARG2)
        ELSE ARG1:= WORDLENGTH;
    END;
    PUSH(ARG1); POP(ARG1 + ARG3);
  END;
ENTER1 {(MODE, LABEL, PARAMLENGTH, VARLENGTH, TEMPLENGTH)}:
  BEGIN READ5ARG;
    ENTERBLOCK(ARG2, ARG3, ARG4, ARG5);
    CASE ARG1 OF
      MODE1:
        WRITE5(ENTER2, BLOCK, POPLENGTH, LINE, VARLENGTH);
      MODE2:
        WRITE5(ENTERPROG2, POPLENGTH + WORDLENGTH, LINE, BLOCK, VARLENGTH);
      MODE3:
        WRITE5(ENTERPROC2, BLOCK, POPLENGTH, LINE, VARLENGTH);
      MODE4:
        WRITE5(ENTERCLAS2, BLOCK, POPLENGTH + WORDLENGTH, LINE, VARLENGTH);
      MODE5:
        WRITE5(ENTERMON2, BLOCK, POPLENGTH + WORDLENGTH, LINE, VARLENGTH);
      MODE6:
        WRITE2(BEGINPROC2, LINE);
      MODE7:
        WRITE5(BEGINCLAS2, BLOCK, FIVEWORDS, LINE, 0);
      MODE8:
        WRITE5(BEGINMON2, BLOCK, FIVEWORDS, LINE, 0);
      MODE10:
    END;
  END;
RETURN1{(MODE)}:
  BEGIN READ1ARG;
    CASE ARG1 OF
      MODE1:  WRITE1(EXIT2);
      MODE2:  WRITE1(EXITPROG2);
      MODE3:  WRITE1(EXITPROC2);
      MODE4:  WRITE1(EXITCLASS2);
      MODE5:  WRITE1(EXITMON2);
      MODE6:  WRITE1(ENDPROC2);
      MODE7:  WRITE1(ENDCLASS2);
      MODE8:  WRITE1(ENDMON2);
      MODE10:
    END;
    EXITBLOCK;
  END;
POP1 {(LENGTH)}:
  BEGIN READ1ARG;
    WRITE2(POP2, ARG1);
    POP(ARG1);
  END;
NEWLINE1{(NUMBER)}:
  BEGIN READ1ARG;
  NEWLINE(ARG1) END;
ERROR1:
  GENERATE:= FALSE;
CONSTANT1 {(LENGTH, VALUE)}:
  BEGIN
    READ1ARG;
    J:=ARG1 DIV WORDLENGTH;
    FOR {ARG3} I:= 1  TO {ARG1 DIV WORDLENGTH}J DO
    BEGIN CONSTANTS:= CONSTANTS + 1;
      READ1ARG; ENTER(CONSTTABLE, CONSTANTS, ARG1);
    END;
  END;
MESSAGE1{(PASS, ERROR)}:
  BEGIN READ2ARG;
    ERROR(ARG1, ARG2)
  END;
INCREMENT1:
  BEGIN WRITE1(INCRWORD2);
  POPWORD END;
DECREMENT1:
  BEGIN WRITE1(DECRWORD2);
  POPWORD END;
PROCEDURE1{(STANDARDPROCEDURE)}:
  BEGIN READ1ARG;
    IF (ARG1 >= MIN_PROC) AND (ARG1 <= MAX_PROC) THEN
    CASE ARG1 OF
      DELAY1:
        BEGIN WRITE1(DELAY2);
        POPWORD END;
      CONTINUE1:
        BEGIN WRITE1(CONTINUE2);
        POPWORD END;
      IO1:
        BEGIN WRITE1(IO2);
        POP(THREEWORDS) END;
      START1:
        WRITE1(START2);
      STOP1:
        BEGIN WRITE1(STOP2);
        POP(TWOWORDS) END;
      SETHEAP1:
        BEGIN WRITE1(SETHEAP2);
        POPWORD END;
      WAIT1:
        WRITE1(WAIT2)
    END;
  END;
INIT1{(MODE, LABEL, PARAMLENGTH, VARLENGTH)}:
  BEGIN READ4ARG;
    IF ARG1 = MODE6 THEN
    BEGIN WRITE4(INITPROC2, ARG3, ARG4, ARG2);
      PUSH(FOURWORDS);
      POP(ARG3 + FIVEWORDS);
    END ELSE
    BEGIN
      IF ARG1 = MODE7
        THEN WRITE2(INITCLASS2, ARG3)
        ELSE WRITE2(INITMON2, ARG3);
      ARG1:= ENTRY(STACKTABLE, ARG2);
      POP(ARG3);
      PUSH(ARG1);
      POP(ARG1 + WORDLENGTH);
    END;
    WRITELOCATION;
    WRITEARG(ARG2);
  END;
PUSHLABEL1{(LABEL)}:
  BEGIN READ1ARG;
    WRITE1(PUSHLABEL2);
    WRITELOCATION;
    WRITEARG(ARG1);
    PUSHWORD;
  END;
CALLPROG1:
  BEGIN WRITE1(CALLPROG2);
  PUSHWORD END;
EOM1{(VARLENGTH)}:
  BEGIN
    DONE:=TRUE;
    READ1ARG; VARLENGTH:=ARG1;
    COMMENT(WORDLENGTH);
    WRITE1(EOM2)
  END
    END
  UNTIL DONE
END;

begin
  first(outfile5);
  init(outfile6);
  BEGINPASS;
  SCAN;
  ENDPASS;
end;

procedure pass7(var OK: boolean; VAR LINK: PASSPTR);

CONST

(*INPUT OPERATORS*)

CONSTADDR1 = 0;    LOCALADDR1 = 1;     GLOBADDR1 = 2;      PUSHCONST1 = 3;
PUSHLOCAL1 = 4;    PUSHGLOB1 = 5;      PUSHIND1 = 6;       PUSHBYTE1 = 7;
PUSHREAL1 = 8;     PUSHSET1 = 9;       FIELD1 = 10;        INDEX1 = 11;
POINTER1 = 12;     VARIANT1 = 13;      RANGE1 = 14;        COPYBYTE1 = 15;
COPYWORD1 = 16;    COPYREAL1 = 17;     COPYSET1 = 18;      COPYTAG1 = 19;
COPYSTRUC1 = 20;   NEW1 = 21;          NEWINIT1 = 22;      NOT1 = 23;
ANDWORD1 = 24;     ANDSET1 = 25;       ORWORD1 = 26;       ORSET1 = 27;
NEGWORD1 = 28;     NEGREAL1 = 29;      ADDWORD1 = 30;      ADDREAL1 = 31;
SUBWORD1 = 32;     SUBREAL1 = 33;      SUBSET1 = 34;       MULWORD1 = 35;
MULREAL1 = 36;     DIVWORD1 = 37;      DIVREAL1 = 38;      MODWORD1 = 39;
BUILDSET1 = 40;    INSET1 = 41;        LSWORD1 = 42;       EQWORD1 = 43;
GRWORD1 = 44;      NLWORD1 = 45;       NEWORD1 = 46;       NGWORD1 = 47;
LSREAL1 = 48;      EQREAL1 = 49;       GRREAL1 = 50;       NLREAL1 = 51;
NEREAL1 = 52;      NGREAL1 = 53;       EQSET1 = 54;        NLSET1 = 55;
NESET1 = 56;       NGSET1 = 57;        LSSTRUCT1 = 58;     EQSTRUCT1 = 59;
GRSTRUCT1 = 60;    NLSTRUCT1 = 61;     NESTRUCT1 = 62;     NGSTRUCT1 = 63;
FUNCVALUE1 = 64;   JUMP1 = 65;         FALSEJUMP1 = 66;    CASEJUMP1 = 67;
INITVAR1 = 68;     CALL1 = 69;         CALLSYS1 = 70;      ENTER1 = 71;
EXIT1 = 72;        ENTERPROG1 = 73;    EXITPROG1 = 74;     BEGINCLAS1 = 75;
ENDCLASS1 = 76;    ENTERCLAS1 = 77;    EXITCLASS1 = 78;    BEGINMON1 = 79;
ENDMON1 = 80;      ENTERMON1 = 81;     EXITMON1 = 82;      BEGINPROC1 = 83;
ENDPROC1 = 84;     ENTERPROC1 = 85;    EXITPROC1 = 86;     POP1 = 87;
NEWLINE1 = 88;     INCRWORD1 = 89;     DECRWORD1 = 90;     INITCLASS1 = 91;
INITMON1 = 92;     INITPROC1 = 93;     PUSHLABEL1 = 94;    CALLPROG1 = 95;
TRUNCREAL1 = 96;   ABSWORD1 = 97;      ABSREAL1 = 98;      SUCCWORD1 = 99;
PREDWORD1 = 100;   CONVWORD1 = 101;    EMPTY1 = 102;       ATTRIBUTE1 = 103;
REALTIME1 = 104;   DELAY1 = 105;       CONTINUE1 = 106;    IO1 = 107;
START1 = 108;      STOP1 = 109;        SETHEAP1 = 110;     WAIT1 = 111;
MESSAGE1=112;      EOM1=113;
(*OUTPUT OPERATORS*)
CONSTADDR2 = 2;    LOCALADDR2 = 4;     GLOBADDR2 = 6;      PUSHCONST2 = 8;
PUSHLOCAL2 = 10;   PUSHGLOB2 = 12;      PUSHIND2 = 14;     PUSHBYTE2 = 16;
PUSHREAL2 = 18;    PUSHSET2 = 20;       FIELD2 = 22;        INDEX2 = 24;
POINTER2 = 26;     VARIANT2 = 28;      RANGE2 = 30;        COPYBYTE2 = 32;
COPYWORD2 = 34;    COPYREAL2 = 36;     COPYSET2 = 38;      COPYTAG2 = 40;
COPYSTRUC2 = 42;    NEW2 = 44;          NEWINIT2 = 46;     NOT2 = 48;
ANDWORD2 = 50;      ANDSET2 = 52;       ORWORD2 = 54;      ORSET2 = 56;
NEGWORD2 = 58;     NEGREAL2 = 60;      ADDWORD2 = 62;      ADDREAL2 = 64;
SUBWORD2 = 66;     SUBREAL2 = 68;      SUBSET2 = 70;       MULWORD2 = 72;
MULREAL2 = 74;      DIVWORD2 = 76;     DIVREAL2 = 78;      MODWORD2 = 80;
BUILDSET2 = 82;     INSET2 = 84;       LSWORD2 = 86;       EQWORD2 = 88;
GRWORD2 = 90;       NLWORD2 = 92;       NEWORD2 = 94;       NGWORD2 = 96;
LSREAL2 = 98;      EQREAL2 = 100;      GRREAL2 = 102;      NLREAL2 = 104;
NEREAL2 = 106;     NGREAL2 = 108;      EQSET2 = 110;       NLSET2 = 112;
NESET2 = 114;       NGSET2 = 116;       LSSTRUCT2 = 118;   EQSTRUCT2 = 120;
GRSTRUCT2 = 122;   NLSTRUCT2 = 124;    NESTRUCT2 = 126;     NGSTRUCT2 = 128;
FUNCVALUE2 = 130;  JUMP2 = 132;        FALSEJUMP2 = 134;   CASEJUMP2 = 136;
INITVAR2 = 138;     CALL2 = 140;       CALLSYS2 = 142;     ENTER2 = 144;
EXIT2 = 146;        ENTERPROG2 = 148;   EXITPROG2 = 150;   BEGINCLAS2 = 152;
ENDCLASS2 = 154;   ENTERCLAS2 = 156;   EXITCLASS2 = 158;   BEGINMON2 = 160;
ENDMON2 = 162;     ENTERMON2 = 164;     EXITMON2 = 166;     BEGINPROC2 = 168;
ENDPROC2 = 170;    ENTERPROC2 = 172;   EXITPROC2 = 174;    POP2 = 176;
NEWLINE2 = 178;     INCRWORD2 = 180;   DECRWORD2 = 182;    INITCLASS2 = 184;
INITMON2 = 186;     INITPROC2 = 188;    PUSHLABEL2 = 190;   CALLPROG2 = 192;
TRUNCREAL2 = 194;  ABSWORD2 = 196;     ABSREAL2 = 198;      SUCCWORD2 = 200;
PREDWORD2 = 202;   CONVWORD2 = 204;    EMPTY2 = 206;        ATTRIBUTE2 = 208;
REALTIME2 = 210;   DELAY2 = 212;       CONTINUE2 = 214;    IO2 = 216;
START2 = 218;       STOP2 = 220;       SETHEAP2 = 222;     WAIT2 = 224;
(*"OTHER CONSTANTS*)
STACKMARGIN = 20 (*BYTES EXTRA PER PROCEDURE CALL*);
PDP11 = TRUE;
CONCURRENT = TRUE;
INITIALBLOCK = 1;
THIS_PASS=7;

TYPE
SHORTTEXT = ARRAY (.1..8.) OF CHAR;
MEDTEXT = ARRAY (.1..16.) OF CHAR;
LONGTEXT = ARRAY (.1..24.) OF CHAR;

VAR
SUMMARY, GENERATE: BOOLEAN;

JUMPTABLE, BLOCKTABLE, STACKTABLE, CONSTTABLE: TABLEPTR;
CONSTANTS: INTEGER;
PROGLENGTH, CODELENGTH, STACKLENGTH, VARLENGTH: INTEGER;
BLOCK: INTEGER;
DONE: BOOLEAN;
objfile:file of integer;
i:integer;


 (*#######################*)
(*INPUT/OUTPUT PROCEDURES*)
(*#######################*)
PROCEDURE WRITEOP(OP: INTEGER);
BEGIN
  IF GENERATE THEN add(OP,outfile7);
  IF TEST THEN PRINTOP(OP);
END;

PROCEDURE WRITEARG(ARG: INTEGER);
BEGIN
  IF GENERATE THEN add(ARG,outfile7);
  IF TEST THEN PRINTARG(ARG);
END;

PROCEDURE COPYARG;
VAR ARG: INTEGER;
BEGIN
  get_L (outfile6, ARG);
  IF GENERATE THEN add(ARG,outfile7);
  IF TEST THEN PRINTARG(ARG);
END;

PROCEDURE COPY1(OP: INTEGER);
VAR ARG: INTEGER;
BEGIN
  get_L (outfile6, ARG);
  IF GENERATE THEN
  BEGIN
    add(OP, outfile7);
    add(ARG,outfile7) END;
  IF TEST THEN
  BEGIN PRINTOP(OP); PRINTARG(ARG) END;
END;

PROCEDURE COPY2(OP: INTEGER);
VAR ARG1, ARG2: INTEGER;
BEGIN
  get_l(outfile6,ARG1 ); get_l(outfile6,ARG2);
  IF GENERATE THEN
  BEGIN add(OP,outfile7); add(ARG1,outfile7); add(ARG2,outfile7);
  END;
  IF TEST THEN
  BEGIN PRINTOP(OP);
    PRINTARG(ARG1); PRINTARG(ARG2);
  END;
END;

PROCEDURE COPY3(OP: INTEGER);
VAR ARG1, ARG2, ARG3: INTEGER;
BEGIN
   get_l(outfile6,ARG1 ); get_l(outfile6,ARG2);
   get_l(outfile6,ARG3);
IF GENERATE THEN
  BEGIN
   add(OP,outfile7); add(ARG1,outfile7); add(ARG2,outfile7);
   add(ARG3,outfile7);
  END;
  IF TEST THEN
  BEGIN
    PRINTOP(OP); PRINTARG(ARG1);
    PRINTARG(ARG2); PRINTARG(ARG3);
  END;
END;

(*################*)
(*TABLE PROCEDURES*)
(*################*)
FUNCTION ENTRY(T: TABLEPTR; I: INTEGER): INTEGER;
VAR PORTION: TABLEPTR; J: INTEGER;
BEGIN
 IF I=0 THEN ENTRY:=0 (*REFERENCE TO UNDEFINED ROUTINE*) ELSE BEGIN
  PORTION:= T; J:= I;
  WHILE J > MAXWORD DO
  BEGIN
    PORTION:= PORTION^.NEXTPORTION;
    J:= J - MAXWORD;
  END;
  ENTRY:= PORTION^.CONTENTS(.J.);
 END
END;

(*########################*)
(*JUMP AND CALL PROCEDURES*)
(*########################*)
PROCEDURE WRITEJUMP(OP: INTEGER);
VAR LOCATION, JUMPLABEL: INTEGER;
var
 m, k:integer;
BEGIN
  WRITEOP(OP);
  get_l(outfile6,LOCATION);
  get_l(outfile6,JUMPLABEL);
 { M:= (ENTRY(JUMPTABLE, JUMPLABEL) - LOCATION) MOD 2;
  IF M=0 then K:= ((ENTRY(JUMPTABLE, JUMPLABEL) - LOCATION) DIV 2)
  else K:= ((ENTRY(JUMPTABLE, JUMPLABEL) - LOCATION) DIV 2) +1;  }
  K:=  (ENTRY(JUMPTABLE, JUMPLABEL) - LOCATION);
 { WRITEARG((ENTRY(JUMPTABLE, JUMPLABEL) - LOCATION)); }
  WRITEARG(K);
END;

PROCEDURE WRITECASE(OP: INTEGER);
VAR DIFF, LOCATION, CASELABEL, I: INTEGER;
    WARG:integer;
BEGIN
  WRITEOP(OP);
  COPYARG;
   get_l(outfile6,DIFF); WRITEARG(DIFF);
   get_l(outfile6,LOCATION);
  FOR I:= 0 TO DIFF DO
  BEGIN
    get_l(outfile6,CASELABEL);
    WARG:=ENTRY(JUMPTABLE, CASELABEL);
    WARG:=WARG-LOCATION;
    {WRITEARG(ENTRY(JUMPTABLE, CASELABEL) - LOCATION);}
    WRITEARG(WARG);
    LOCATION:= LOCATION + WORDLENGTH;
  END;
END;

PROCEDURE WRITECALL(OP: INTEGER);
VAR LOCATION, BLOCK: INTEGER;
BEGIN
  WRITEOP(OP);
  get_l(outfile6,LOCATION);
  get_l(outfile6,BLOCK);
  WRITEARG(ENTRY(BLOCKTABLE, BLOCK) - LOCATION);
END;

(*###############################*)
(*NEW, ENTER, AND EXIT PROCEDURES*)
(*##############################*)
PROCEDURE WRITENEW(OP: INTEGER);
VAR BLOCK, LENGTH: INTEGER;
BEGIN
  WRITEOP(OP);
  get_l(outfile6,BLOCK);  get_l(outfile6,LENGTH);
  WRITEARG(STACKLENGTH + LENGTH);
  WRITEARG(LENGTH);
END;

PROCEDURE COPYBLOCK;
BEGIN
  get_l(outfile6,BLOCK);
  STACKLENGTH:= ENTRY(STACKTABLE, BLOCK) + STACKMARGIN;
  WRITEARG(STACKLENGTH);
END;

PROCEDURE WRITEENTER(OP: INTEGER);
BEGIN
  WRITEOP(OP);
  COPYBLOCK;
  COPYARG;
  COPYARG;
  COPYARG;
END;

PROCEDURE WRITEEXIT(OP: INTEGER);
BEGIN
  WRITEOP(OP);
END;

PROCEDURE WRITEPROG(OP: INTEGER);
BEGIN
  WRITEOP(OP);
  COPYARG;
  COPYARG;
  COPYBLOCK;
  COPYARG;
END;

{###############}
{INIT PROCEDURES}
{###############}
PROCEDURE WRITEINIT(OP: INTEGER);
VAR LOCATION, BLOCK: INTEGER;
BEGIN
  WRITEOP(OP); COPYARG;
  get_l(outfile6,LOCATION);
  get_l(outfile6,BLOCK);
  WRITEARG(ENTRY(BLOCKTABLE, BLOCK) - LOCATION);
END;

PROCEDURE WRITEPROC(OP: INTEGER);
VAR LOCATION, BLOCK: INTEGER;
BEGIN WRITEOP(OP);
  COPYARG;
  COPYARG;
  COPYBLOCK;
  get_l(outfile6,LOCATION); get_l(outfile6,BLOCK);
  WRITEARG(ENTRY(BLOCKTABLE, BLOCK) - LOCATION);
END;

(*########################*)
(*HEAD AND TAIL PROCEDURES*)
(*########################*)
PROCEDURE WRITEHEAD;
BEGIN
  IF TEST THEN
  BEGIN PRINTFF(THIS_PASS);
    WRITE(tiskarna,'('); WRITE(tiskarna,'#'); WRITELN(tiskarna);
  END;
  WRITEARG(PROGLENGTH); WRITEARG(CODELENGTH);
  WRITEARG(STACKLENGTH); WRITEARG(VARLENGTH);
END;

PROCEDURE WRITETAIL;
VAR I: INTEGER;
BEGIN
  FOR I:= 1 TO CONSTANTS DIV WORDLENGTH DO
    WRITEARG(ENTRY(CONSTTABLE, I));
    IF TEST THEN
    BEGIN
      WRITELN(tiskarna); WRITE(tiskarna,'#'); WRITE(tiskarna,')');
    END;
END;

{###################}
{PRINTING PROCEDURES"}
{###################}
PROCEDURE PRINTSHORT(T: SHORTTEXT);
VAR I: INTEGER; C: CHAR;
BEGIN
  I:= 1; C:= T(.I.);
  WHILE C <> '.' DO
  BEGIN WRITE(tiskarna,C); I:= I + 1; C:= T(.I.) END;
END;

PROCEDURE PRINTMED(T: MEDTEXT);
VAR I: INTEGER; C: CHAR;
BEGIN
  I:= 1; C:= T(.I.);
  WHILE C <> '.' DO
  BEGIN WRITE(tiskarna, C); I:= I + 1; C:= T(.I.) END;
END;

PROCEDURE PRINTLONG(T: LONGTEXT);
VAR I: INTEGER; C: CHAR;
BEGIN
  I:= 1; C:= T(.I.);
  WHILE C <> '.' DO
  BEGIN WRITE(tiskarna,C); I:= I + 1; C:= T(.I.) END;
END;

{################"}
{ERROR PROCEDURES}
{################}
PROCEDURE PRINTHEAD(PASS, LINE: INTEGER);
VAR M: MEDTEXT; S: SHORTTEXT;
BEGIN
  PRINTEOL;
  M:= '****** PASS .   '; PRINTMED(M);
  PRINTABS(PASS);
  S:= ' LINE . '; PRINTSHORT(S);
  PRINTABS(LINE);
 WRITE(' ');
END;

PROCEDURE PASS1ERROR(NO, LINE: INTEGER);
CONST
COMMENT_ERROR=1;   NUMBER_ERROR=2;     INSERT_ERROR=3;     STRING_ERROR=4;
CHAR_ERROR=5;
BEGIN
  PRINTHEAD(1, LINE);
  CASE NO OF
    COMMENT_ERROR: PRINTMED('ENDLESS COMMENT.');
    NUMBER_ERROR:  PRINTMED('INVALID NUMBER. ');
    INSERT_ERROR:  PRINTMED('TABLE OVERFLOW. ');
    STRING_ERROR:  PRINTMED('INVALID STRING. ');
    CHAR_ERROR:    PRINTMED('BAD CHARACTER.  ')
  END;
  PRINTEOL;
END;

PROCEDURE PASS2ERROR(NO, LINE: INTEGER);
CONST
PROG_ERROR=1;      DEC_ERROR=2;        CONSTDEF_ERROR=3;   TYPEDEF_ERROR=4;
TYPE_ERROR=5;      ENUM_ERROR=6;       SUBR_ERROR=7;       SET_ERROR=8;
ARRAY_ERROR=9;     RECORD_ERROR=10;    STACK_ERROR=11;     VAR_ERROR=12;
ROUTINE_ERROR=13;  PROC_ERROR=14;      FUNC_ERROR=15;      WITH_ERROR=16;
PARM_ERROR=17;     BODY_ERROR=18;      STATS_ERROR=19;     STAT_ERROR=20;
IDSTAT_ERROR=21;   ARG_ERROR=22;       COMP_ERROR=23;      IF_ERROR=24;
CASE_ERROR=25;     LABEL_ERROR=26;     WHILE_ERROR=27;     REPEAT_ERROR=28;
FOR_ERROR=29;      CYCLE_ERROR=30;     EXPR_ERROR=31;      VARIABLE_ERROR=32;
CONSTANT_ERROR=33; INIT_ERROR=34;      MPROG_ERROR=35;     POINTER_ERROR=36;
PREFIX_ERROR=37;   INTERFACE_ERROR=38;
BEGIN
  PRINTHEAD(2, LINE);
  CASE NO OF
    PROG_ERROR:        PRINTMED('SEQL PROGRAM.   ');
    DEC_ERROR:         PRINTMED('DECLARATION.    ');
    CONSTDEF_ERROR:    PRINTMED('CONSTANT DFN.   ');
    TYPEDEF_ERROR:     PRINTMED('TYPE DFN.       ');
    TYPE_ERROR:        PRINTMED('TYPE.           ');
    ENUM_ERROR:        PRINTMED('ENUMERATION TYP.');
    SUBR_ERROR:        PRINTMED('SUBRANGE TYPE.  ');
    SET_ERROR:         PRINTMED('SET TYPE.       ');
    ARRAY_ERROR:       PRINTMED('ARRAY TYPE.     ');
    RECORD_ERROR:      PRINTMED('RECORD TYPE.    ');
    STACK_ERROR:       PRINTMED('STACK LENGTH.   ');
    VAR_ERROR:         PRINTMED('VAR DECLARATION.');
    ROUTINE_ERROR:     PRINTMED('ROUTINE.        ');
    PROC_ERROR:        PRINTMED('PROCEDURE.      ');
    FUNC_ERROR:        PRINTMED('FUNCTION.       ');
    WITH_ERROR:        PRINTMED('WITH STMT.      ');
    PARM_ERROR:        PRINTMED('PARAMETER.      ');
    BODY_ERROR:        PRINTMED('BODY.           ');
    STATS_ERROR:       PRINTMED('STMT LIST.      ');
    STAT_ERROR:        PRINTMED('STATEMENT.      ');
    IDSTAT_ERROR:      PRINTMED('ID STMT.        ');
    ARG_ERROR:         PRINTMED('ARGUMENT.       ');
    COMP_ERROR:        PRINTMED('COMPOUND STMT.  ');
    IF_ERROR:          PRINTMED('IF STMT.        ');
    CASE_ERROR:        PRINTMED('CASE STMT.      ');
    LABEL_ERROR:       PRINTMED('LABEL LIST.     ');
    WHILE_ERROR:       PRINTMED('WHILE STMT.     ');
    REPEAT_ERROR:      PRINTMED('REPEAT STMT.    ');
    FOR_ERROR:         PRINTMED('FOR STMT.       ');
    CYCLE_ERROR:       PRINTMED('CYCLE STMT.     ');
    EXPR_ERROR:        PRINTMED('EXPRESSION.     ');
    VARIABLE_ERROR:    PRINTMED('VARIABLE.       ');
    CONSTANT_ERROR:    PRINTMED('CONSTANT.       ');
    INIT_ERROR:        PRINTMED('INIT STMT.      ');
    MPROG_ERROR:       PRINTMED('TERMINATION.    ');
    PREFIX_ERROR:      PRINTMED('PREFIX.         ');
    INTERFACE_ERROR:   PRINTMED('INTERFACE.      ');
    POINTER_ERROR:     PRINTMED('POINTER TYPE.   ')
  END;
  PRINTSHORT(' SYNTAX.');
  PRINTEOL;
END;

PROCEDURE PASS3ERROR(NO, LINE: INTEGER);
CONST
UNRES_ERROR=1;     AMBIGUITY_ERROR=2;  ABORT_ERROR=3;      CONSTID_ERROR=4;
SUBR_ERROR=5;      FEW_ARGS_ERROR=6;   ARG_LIST_ERROR=7;   MANY_ARGS_ERROR=8;
CASERANGE_ERROR=9; CASETYPE_ERROR=10;  AMBICASE_ERROR=11;  WITH_ERROR=12;
INIT_ERROR=13;     PROC_USE_ERROR=14;  NAME_ERROR=15;      COMP_ERROR=16;
SUB_ERROR=17;      INTERFACE_ERROR=18; CALL_NAME_ERROR=19; ARROW_ERROR=20;
RESOLVE_ERROR=21;
BEGIN
  PRINTHEAD(3, LINE);
  CASE NO OF
    UNRES_ERROR:       PRINTLONG ('UNRESOLVED ROUTINE.     ');
    AMBIGUITY_ERROR:   PRINTLONG ('AMBIGUOUS IDENTIFIER.   ');
    ABORT_ERROR:       PRINTLONG ('COMPILER ABORT.         ');
    CONSTID_ERROR:     PRINTLONG ('INVALID CONSTANT.       ');
    SUBR_ERROR:        PRINTLONG ('INVALID SUBRANGE.       ');
    FEW_ARGS_ERROR:    PRINTLONG ('MISSING ARGUMENT.       ');
    ARG_LIST_ERROR:    PRINTLONG ('NOT A ROUTINE.          ');
    MANY_ARGS_ERROR:   PRINTLONG ('TOO MANY ARGUMENTS.     ');
    CASERANGE_ERROR:   PRINTLONG ('LABEL VALUE TOO LARGE.  ');
    CASETYPE_ERROR:    PRINTLONG ('INVALID LABEL.          ');
    AMBICASE_ERROR:    PRINTLONG ('AMBIGUOUS LABEL.        ');
    WITH_ERROR:        PRINTLONG ('INVALID WITH VARIABLE.  ');
    INIT_ERROR:        PRINTLONG ('INVALID INITIALIZATION. ');
    PROC_USE_ERROR:    PRINTLONG ('NOT A FUNCTION.         ');
    NAME_ERROR:        PRINTLONG ('INVALID NAME USAGE.     ');
    COMP_ERROR:        PRINTLONG ('INVALID SELECTION.      ');
    SUB_ERROR:         PRINTLONG ('INVALID SUBSCRIPTING.   ');
    INTERFACE_ERROR:   PRINTLONG ('INVALID INTERFACE.      ');
    CALL_NAME_ERROR:   PRINTLONG ('INVALID CALL.           ');
    ARROW_ERROR:       PRINTLONG ('INVALID POINTING.       ');
    RESOLVE_ERROR:     PRINTLONG ('INVALID RESOLUTION.     ')
  END;
  PRINTEOL;
END;

PROCEDURE PASS4ERROR(NO, LINE: INTEGER);
CONST
NESTING_ERROR=1;   ADDRESS_ERROR=2;    ACTIVE_ERROR=3;     QUEUE_ERROR=4;
PROCESS_ERROR=5;   ENTRY_ERROR=6;      FUNCTYPE_ERROR=7;   TYPEID_ERROR=8;
ENUM1_ERROR=9;     ENUM2_ERROR=10;     INDEX_ERROR=11;     MEMBER_ERROR=12;
STACK_ERROR=13;    PARM1_ERROR=14;     PARM2_ERROR=15;     PARM3_ERROR=16;
PARM4_ERROR=17;    PARM5_ERROR=18;     PARM6_ERROR=19;     PARM7_ERROR=20;
COMPILER_ERROR=21; STRING_ERROR=22;    RESOLVE_ERROR=23;   TAG_ERROR=24;
POINTER_ERROR=25;
BEGIN
  PRINTHEAD(4, LINE);
  CASE NO OF
    NESTING_ERROR:     PRINTLONG ('INVALID NESTING.        ');
    ADDRESS_ERROR:     PRINTLONG ('ADDRESS OVERFLOW.       ');
    ACTIVE_ERROR:      PRINTLONG ('ACTIVE VARIABLE.        ');
    QUEUE_ERROR:       PRINTLONG ('QUEUE VARIABLE.         ');
    PROCESS_ERROR:     PRINTLONG ('NESTED PROCESS.         ');
    ENTRY_ERROR:       PRINTLONG ('INVALID ENTRY VARIABLE. ');
    FUNCTYPE_ERROR:    PRINTLONG ('INVALID FUNCTION TYPE.  ');
    TYPEID_ERROR: ;
    ENUM1_ERROR:       PRINTLONG ('RECORD ENUMERATION.     ');
    ENUM2_ERROR:       PRINTLONG ('LONG ENUMERATION.       ');
    INDEX_ERROR:       PRINTLONG ('INVALID INDEX TYPE.     ');
    MEMBER_ERROR:      PRINTLONG ('INVALID MEMBER TYPE.    ');
    STACK_ERROR:       PRINTLONG ('PROCESS STACK USAGE.    ');
    PARM1_ERROR,PARM2_ERROR,PARM3_ERROR,PARM4_ERROR,
    PARM5_ERROR,PARM6_ERROR,
    PARM7_ERROR:       PRINTLONG ('INVALID PARAMETER.      ');
    COMPILER_ERROR:    PRINTLONG ('COMPILER ABORT.         ');
    STRING_ERROR:      PRINTLONG ('ODD LENGTH STRING TYPE. ');
    RESOLVE_ERROR:     PRINTLONG ('INVALID RESOLUTION.     ');
    TAG_ERROR:         PRINTLONG ('INVALID TAG TYPE.       ');
    POINTER_ERROR:     PRINTLONG ('RECORD POINTER TYPE.    ')
  END;
  PRINTEOL;
END;

PROCEDURE PASS5ERROR(NO, LINE: INTEGER);
CONST
COMPILER_ERROR=1;  TYPE_ERROR=2;       ADDRESS_ERROR=3;    ASSIGN_ERROR=4;
INIT_ERROR = 5;
BEGIN
  PRINTHEAD(5, LINE);
  CASE NO OF
    COMPILER_ERROR:    PRINTMED('COMPILER ABORT. ');
    TYPE_ERROR:        PRINTMED('OPERAND TYPE.   ');
    ADDRESS_ERROR:     PRINTMED('NOT A VARIABLE. ');
    ASSIGN_ERROR:      PRINTMED('NOT ASSIGNABLE. ');
    INIT_ERROR:        PRINTLONG ('INVALID INITIALIZATION. ')
  END;
  PRINTEOL;
END;

PROCEDURE PASS6ERROR(NO, LINE: INTEGER);
CONST STACK_ERROR = 1;  CODE_ERROR = 2;
BEGIN
  PRINTHEAD(6, LINE);
  CASE NO OF
    STACK_ERROR: PRINTMED('TOO MUCH STACK. ');
    CODE_ERROR: PRINTMED('TOO MUCH CODE.  ')
  END;
  PRINTEOL;
END;

PROCEDURE PRINTMESSAGE;
VAR PASS, ERROR, LINE: INTEGER;
BEGIN
  OK:= TEST;
  get_L(outfile6, pass);
  get_L(outfile6, error);
  get_L(outfile6, line);

  CASE PASS OF
    1:  PASS1ERROR(ERROR, LINE);
    2:  PASS2ERROR(ERROR, LINE);
    3:  PASS3ERROR(ERROR, LINE);
    4:  PASS4ERROR(ERROR, LINE);
    5:  PASS5ERROR(ERROR, LINE);
    6:  PASS6ERROR(ERROR, LINE)
  END;
END;

{##################}
{SUMMARY PROCEDURES}
{##################}
PROCEDURE PRINTSUMMARY;
BEGIN
  WRITELN(tiskarna);
  PRINTLONG('PROCEDURE PRINTSUMMARY .');
  PRINTSHORT('CALLED. ')
END;

 {#########################################}
{INITIALIZATION AND TERMINATION PROCEDURES}
{#########################################"}
PROCEDURE BEGINPASS;
BEGIN
  {INIT_PASS(LINK); }
  WITH LINK^  DO
  BEGIN
    Summary:=true;
    TEST:= TEST OR GENERATE;
    GENERATE:= {FALSE} TRUE;
    PROGLENGTH:= TABLES^.PROGLENGTH;
    CODELENGTH:= TABLES^.CODELENGTH;
    STACKLENGTH:= TABLES^.STACKLENGTH + STACKMARGIN;
    VARLENGTH:= TABLES^.VARLENGTH;
    JUMPTABLE:= TABLES^.JUMPTABLE;
    BLOCKTABLE:= TABLES^.BLOCKTABLE;
    STACKTABLE:= TABLES^.STACKTABLE;
    CONSTTABLE:= TABLES^.CONSTTABLE;
  END;
  CONSTANTS:= LINK^.CONSTANTS;
  WRITEHEAD;
END;

PROCEDURE ENDPASS;
BEGIN
  WRITETAIL;
  IF SUMMARY THEN PRINTSUMMARY;
  {RELEASE(LINK^.RESETPOINT);}
END;

{#################}
{OPERATOR SCANNING}
{#################}
PROCEDURE SCAN;
VAR OP: INTEGER;
BEGIN
DONE:= FALSE;
REPEAT
get_l(outfile6, OP);
CASE OP OF
CONSTADDR1 {(DISPL)}:
  COPY1(CONSTADDR2);
LOCALADDR1{(DISPL)}:
  COPY1(LOCALADDR2);
GLOBADDR1{(DISPL)}:
  COPY1(GLOBADDR2);
PUSHCONST1{(VALUE)}:
  COPY1(PUSHCONST2);
PUSHLOCAL1{(DISPL)}:
  COPY1(PUSHLOCAL2);
PUSHGLOB1{(DISPL)}:
  COPY1(PUSHGLOB2);
PUSHIND1:
  WRITEOP(PUSHIND2);
PUSHBYTE1:
  WRITEOP(PUSHBYTE2);
PUSHREAL1:
  WRITEOP(PUSHREAL2);
PUSHSET1:
  WRITEOP(PUSHSET2);
FIELD1{(DISPL)}:
  COPY1(FIELD2);
INDEX1{(MIN, MAX-MIN, LENGTH)}:
  COPY3(INDEX2);
POINTER1:
  WRITEOP(POINTER2);
VARIANT1{(DISPL, TAGSET)}:
  COPY2(VARIANT2);
RANGE1{(MIN, MAX)}:
  COPY2(RANGE2);
COPYBYTE1:
  WRITEOP(COPYBYTE2);
COPYWORD1:
  WRITEOP(COPYWORD2);
COPYREAL1:
  WRITEOP(COPYREAL2);
COPYSET1:
  WRITEOP(COPYSET2);
COPYTAG1{(LENGTH DIV WORDLENGTH)}:
  COPY1(COPYTAG2);
COPYSTRUC1{(LENGTH DIV WORDLENGTH)}:
  COPY1(COPYSTRUC2);
NEW1{(BLOCK, LENGTH)}:
  WRITENEW(NEW2);
NEWINIT1{(BLOCK, LENGTH)}:
  WRITENEW(NEWINIT2);
NOT1:
  WRITEOP(NOT2);
ANDWORD1:
  WRITEOP(ANDWORD2);
ANDSET1:
  WRITEOP(ANDSET2);
ORWORD1:
  WRITEOP(ORWORD2);
ORSET1:
  WRITEOP(ORSET2);
NEGWORD1:
  WRITEOP(NEGWORD2);
NEGREAL1:
  WRITEOP(NEGREAL2);
ADDWORD1:
  WRITEOP(ADDWORD2);
ADDREAL1:
  WRITEOP(ADDREAL2);
SUBWORD1:
  WRITEOP(SUBWORD2);
SUBREAL1:
  WRITEOP(SUBREAL2);
SUBSET1:
  WRITEOP(SUBSET2);
MULWORD1:
  WRITEOP(MULWORD2);
MULREAL1:
  WRITEOP(MULREAL2);
DIVWORD1:
  WRITEOP(DIVWORD2);
DIVREAL1:
  WRITEOP(DIVREAL2);
MODWORD1:
  WRITEOP(MODWORD2);
BUILDSET1:
  WRITEOP(BUILDSET2);
INSET1:
  WRITEOP(INSET2);
LSWORD1:
  WRITEOP(LSWORD2);
EQWORD1:
  WRITEOP(EQWORD2);
GRWORD1:
  WRITEOP(GRWORD2);
NLWORD1:
  WRITEOP(NLWORD2);
NEWORD1:
  WRITEOP(NEWORD2);
NGWORD1:
  WRITEOP(NGWORD2);
LSREAL1:
  WRITEOP(LSREAL2);
EQREAL1:
  WRITEOP(EQREAL2);
GRREAL1:
  WRITEOP(GRREAL2);
NLREAL1:
  WRITEOP(NLREAL2);
NEREAL1:
  WRITEOP(NEREAL2);
NGREAL1:
  WRITEOP(NGREAL2);
EQSET1:
  WRITEOP(EQSET2);
NLSET1:
  WRITEOP(NLSET2);
NESET1:
  WRITEOP(NESET2);
NGSET1:
  WRITEOP(NGSET2);
LSSTRUCT1 {(LENGTH DIV WORDLENGTH)}:
  COPY1(LSSTRUCT2);
EQSTRUCT1{(LENGTH DIV WORDLENGTH)}:
  COPY1(EQSTRUCT2);
GRSTRUCT1 {(LENGTH DIV WORDLENGTH)}:
  COPY1(GRSTRUCT2);
NLSTRUCT1{(LENGTH DIV WORDLENGTH)}:
  COPY1(NLSTRUCT2);
NESTRUCT1 {(LENGTH DIV WORDLENGTH)}:
  COPY1(NESTRUCT2);
NGSTRUCT1{(LENGTH DIV WORDLENGTH)}:
  COPY1(NGSTRUCT2);
FUNCVALUE1{(KIND)}:
  COPY1(FUNCVALUE2);
JUMP1 {(LOCATION, LABEL)}:
  WRITEJUMP(JUMP2);
FALSEJUMP1{(LOCATION, LABEL)}:
  WRITEJUMP(FALSEJUMP2);
CASEJUMP1{(MIN, MAX-MIN, LOCATION, LABELS)}:
  WRITECASE(CASEJUMP2);
INITVAR1 {(LENGTH DIV WORDLENGTH)}:
  COPY1(INITVAR2);
CALL1{(LOCATION, BLOCK)}:
  WRITECALL(CALL2);
CALLSYS1 {(ENTRY * WORDLENGTH)}:
  COPY1(CALLSYS2);
ENTER1 {(BLOCK, POPLENGTH, LINE, VARLENGTH)}:
  WRITEENTER(ENTER2);
EXIT1:
  WRITEEXIT(EXIT2);
ENTERPROG1 {(POPLENGTH, LINE, BLOCK, VARLENGTH)}:
  WRITEPROG(ENTERPROG2);
EXITPROG1:
  WRITEEXIT(EXITPROG2);
BEGINCLAS1 {(BLOCK, POPLENGTH, LINE, VARLENGTH)}:
  WRITEENTER(BEGINCLAS2);
ENDCLASS1:
  WRITEEXIT(ENDCLASS2);
ENTERCLAS1 {(BLOCK, POPLENGTH, LINE, VARLENGTH)}:
  WRITEENTER(ENTERCLAS2);
EXITCLASS1:
  WRITEEXIT(EXITCLASS2);
BEGINMON1 {(BLOCK, POPLENGTH, LINE, VARLENGTH)}:
  WRITEENTER(BEGINMON2);
ENDMON1:
  WRITEEXIT(ENDMON2);
ENTERMON1 {(BLOCK, POPLENGTH, LINE, VARLENGTH)}:
  WRITEENTER(ENTERMON2);
EXITMON1:
  WRITEEXIT(EXITMON2);
BEGINPROC1 {(LINE)}:
  COPY1(BEGINPROC2);
ENDPROC1:
  WRITEEXIT(ENDPROC2);
ENTERPROC1 {(BLOCK, POPLENGTH, LINE, VARLENGTH)}:
  WRITEENTER(ENTERPROC2);
EXITPROC1:
  WRITEEXIT(EXITPROC2);
POP1 {(LENGTH)}:
  COPY1(POP2);
NEWLINE1 {(NUMBER)}:
  COPY1(NEWLINE2);
INCRWORD1:
  WRITEOP(INCRWORD2);
DECRWORD1:
  WRITEOP(DECRWORD2);
INITCLASS1 {(PARAMLENGTH, LOCATION, BLOCK)}:
  WRITEINIT(INITCLASS2);
INITMON1 {(PARAMLENGTH, LOCATION, BLOCK)}:
  WRITEINIT(INITMON2);
INITPROC1 {(PARAMLENGTH, VARLENGTH, BLOCK, LOCATION, BLOCK)}:
  WRITEPROC(INITPROC2);
PUSHLABEL1 {(LOCATION, BLOCK)}:
  WRITECALL(PUSHLABEL2);
CALLPROG1:
  WRITEOP(CALLPROG2);
TRUNCREAL1:
  WRITEOP(TRUNCREAL2);
ABSWORD1:
  WRITEOP(ABSWORD2);
ABSREAL1:
  WRITEOP(ABSREAL2);
SUCCWORD1:
  WRITEOP(SUCCWORD2);
PREDWORD1:
  WRITEOP(PREDWORD2);
CONVWORD1:
  WRITEOP(CONVWORD2);
EMPTY1:
  WRITEOP(EMPTY2);
ATTRIBUTE1:
  WRITEOP(ATTRIBUTE2);
REALTIME1:
  WRITEOP(REALTIME2);
DELAY1:
  WRITEOP(DELAY2);
CONTINUE1:
  WRITEOP(CONTINUE2);
IO1:
  WRITEOP(IO2);
START1:
  WRITEOP(START2);
STOP1:
  WRITEOP(STOP2);
SETHEAP1:
  WRITEOP(SETHEAP2);
WAIT1:
  WRITEOP(WAIT2);
MESSAGE1:  { (PASS, ERROR, LINE)}
PRINTMESSAGE;
EOM1: DONE:=TRUE
END
UNTIL DONE;
END {OF SCAN};

{ pass7}

begin
  GENERATE:=true;
  first(outfile6);
  INIT(outfile7);
  first(chyby);
  BEGINPASS;
  SCAN;
  ENDPASS;
  Assign (objfile, Nazev_Vstupniho_Souboru+ '.obj');
  rewrite(objfile);
  for i:=0 to outfile7.Length-1 do
  begin
    write(objfile,outfile7.Data[i])
  end;
 close(objfile);
  end;




begin

  { TODO -oUser -cConsole Main : Insert code here }
  INIT_OPTIONS;
  test:=true; DEBUG:=true; GENERATE:=true;
  writeln('Zadej název souboru: ');
  readln(Nazev_Vstupniho_Souboru);
  ASSIGN(ERRORS, Nazev_Vstupniho_Souboru + '_E.TXT');
  rewrite(ERRORS);
  Assign(source,Nazev_Vstupniho_Souboru +'.txt');
  Reset(source);
  Assign(tiskarna,Nazev_Vstupniho_Souboru+ '.lst');
  Rewrite(tiskarna);
  OK:=true;
  Pass1(OK);
  Close (source);
  if OK then Pass2(OK);
  if OK then  Pass3(OK);
  if OK then  Pass4(OK);
  if OK then  Pass5(OK);
  if OK then  Pass6(OK,INTER_PASS_PTR);
  if OK then  Pass7(OK,INTER_PASS_PTR);

  close(tiskarna);
  close(ERRORS);

 end.
