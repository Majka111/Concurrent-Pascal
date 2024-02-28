program Sequential_P_1;

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
 end;

procedure getreal(dual: splitreal;
  var value: real);
begin
  dual.split := true;
 { get(dual.b);
  get(dual.c);  }
  value := dual.a;
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
   { END_LINE; }
    NEW(INTER_PASS_PTR);
    WITH INTER_PASS_PTR^ DO BEGIN
      OPTIONS:=(.LISTOPTION,CHECKOPTION,NUMBEROPTION.);
     { MARK(RESETPOINT); }
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
  {PRINTEOL; FOR I:=1 TO 130 DO WRITE(tiskarna, '2');}
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
CHAR_ERROR=5;


 LISTOPTION = 0;    SUMMARYOPTION = 1;  TESTOPTION = 2;     CHECKOPTION = 3;
 CODEOPTION = 4;    NUMBEROPTION = 5;



(* OUTPUT OPERATORS *)

EOM2=0;            BEGIN2=1;           IF2=2;              CASE2=3;
WHILE2=4;          REPEAT2=5;          FOR2=6;             WITH2=7;
ID2=8;             REAL2=9;            STRING2=10;         INTEGER2=11;
CHAR2=12;          OPEN2=13;           NOT2=14;            SUB2=15;
SET2=16;           ARRAY2=17;          RECORD2=18;         ARROW2=19;
PERIOD2=20;        STAR2=21;           SLASH2=22;          DIV2=23;
MOD2=24;           AND2=25;            PLUS2=26;           MINUS2=27;
OR2=28;            EQ2=29;             NE2=30;             LE2=31;
GE2=32;            LT2=33;             GT2=34;             IN2=35;
CONST2=36;         TYPE2=37;           VAR2=38;            PROCEDURE2=39;
FUNCTION2=40;      PROGRAM2=41;        SEMICOLON2=42;      CLOSE2=43;
UP_TO2=44;         OF2=45;             COMMA2=46;          BUS2=47;
COLON2=48;         END2=49;            FORWARD2=50;        UNIV2=51;
BECOMES2=52;       THEN2=53;           ELSE2=54;           DO2=55;
UNTIL2=56;         TO2=57;             DOWNTO2=58;         LCONST2=59;
MESSAGE2=60;       NEW_LINE2=61;

(* STANDARD SPELLING/NOUN INDICES*)

XUNDEF=0;          XFALSE=1;           XTRUE=2;            XINTEGER=3;
XBOOLEAN=4;        XCHAR=5;            XNIL=6;             XABS=7;
XATTRIBUTE=8;      XCHR=9;             XCONV=10;           XORD=11;
XPRED=12;          XSUCC=13;           XTRUNC=14;          XNEW=15;
XREAL=16;

type
  PAGE = ARRAY (.1..PAGELENGTH.) OF INTEGER;


 {PIECE = string[10]; }
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

 { SPLITREAL = ARRAY (.1..SPLITLENGTH.) OF INTEGER; }

  TREAT=(CANCEL,IGNORE);

{GETCHTYP=record
         CC:0..line_length;
         LL:0..line_length;
         LINE:array(.1..line_length.) of char;
 end;   }

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
 {G:getchtyp; }

 procedure GETCH({var G:GETCHTYP;}var CH:char);
  begin
  read(source, ch);
  write (tiskarna, ch);
  {inc(g.CC);}
  if eoln(source) then
  begin
   if eof(source) then
     END_SCAN:=true;;
     writeln(tiskarna);
  { inc(g.LL);
   g.CC:=0;   }
  end;

  end;


  PROCEDURE PUT0NC(OP:INTEGER);
  BEGIN
    (*WRITE_IFL(OP);   *)
    IF TEST THEN STORE_TEST(OP);
    {Inc(line_count);
    outfile[line_count].OPER:=op; }
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
    STD_ID('FORWARD   ',-FORWARD2);
    STD_ID('UNIV      ',-UNIV2);
    STD_ID('PROGRAM   ',-PROGRAM2);
    STD_ID('FALSE     ',XFALSE);
    STD_ID('TRUE      ',XTRUE);
    STD_ID('INTEGER   ',XINTEGER);
    STD_ID('BOOLEAN   ',XBOOLEAN);
    STD_ID('CHAR      ',XCHAR);
    STD_ID('NIL       ',XNIL);
    STD_ID('NEW       ',XNEW);
    STD_ID('ABS       ',XABS);
    STD_ID('ATTRIBUTE ',XATTRIBUTE);
    STD_ID('CHR       ',XCHR);
    STD_ID('CONV      ',XCONV);
    STD_ID('ORD       ',XORD);
    STD_ID('PRED      ',XPRED);
    STD_ID('SUCC      ',XSUCC);
    STD_ID('TRUNC     ',XTRUNC);
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
          {REPEAT GETCH (G,CH) UNTIL CH = EOL; }
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
        '@': PUT0NC(ARROW2);
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
WHILE1=4;          REPEAT1=5;          FOR1=6;             WITH1=7;
ID1=8;             REAL1=9;            STRING1=10;         INTEGER1=11;
CHAR1=12;          OPEN1=13;           NOT1=14;            SUB1=15;
SET1=16;           ARRAY1=17;          RECORD1=18;         ARROW1=19;
PERIOD1=20;        STAR1=21;           SLASH1=22;          DIV1=23;
MOD1=24;           AND1=25;            PLUS1=26;           MINUS1=27;
OR1=28;            EQ1=29;             NE1=30;             LE1=31;
GE1=32;            LT1=33;             GT1=34;             IN1=35;
CONST1=36;         TYPE1=37;           VAR1=38;            PROCEDURE1=39;
FUNCTION1=40;      PROGRAM1=41;        SEMICOLON1=42;      CLOSE1=43;
UP_TO1=44;         OF1=45;             COMMA1=46;          BUS1=47;
COLON1=48;         END1=49;            FORWARD1=50;        UNIV1=51;
BECOMES1=52;       THEN1=53;           ELSE1=54;           DO1=55;
UNTIL1=56;         TO1=57;             DOWNTO1=58;         LCONST1=59;
MESSAGE1=60;       NEW_LINE1=61;


{OUTPUT OPERATORS}

EOM2=1;            CONST_ID2=2;        CONST_DEF2=3;       TYPE_ID2=4;
TYPE_DEF2=5;       VAR_ID2=6;          VAR_LIST2=7;        PROC_ID2=8;
PROC_DEF2=9;       LBL_END2=10;        FORWARD2=11;        FUNC_ID2=12;
FUNC_DEF2=13;      POINTER2=14;        FUNC_TYPE2=15;      PROG_ID2=16;
PROG_DEF2=17;      VARNT_END2=18;      TYPE2=19;           ENUM2=20;
ENUM_ID2=21;       ENUM_DEF2=22;       SUBR_DEF2=23;       SET_DEF2=24;
ARRAY_DEF2=25;     REC2=26;            FIELD_ID2=27;       FIELDLIST2=28;
REC_DEF2=29;       VARNT2=30;          PARM_ID2=31;        PARM_TYPE2=32;
UNIV_TYPE2=33;     CPARMLIST2=34;      VPARMLIST2=35;      BODY2=36;
BODY_END2=37;      ANAME2=38;          STORE2=39;          CALL_NAME2=40;
CALL2=41;          ARG_LIST2=42;       ARG2=43;            FALSEJUMP2=44;
DEF_LABEL2=45;     JUMP_DEF2=46;       DEF_CASE2=47;       CASE2=48;
JUMP2=49;          END_CASE2=50;       ADDRESS2=51;        FOR_STORE2=52;
FOR_LIM2=53;       FOR_UP2=54;         FOR_DOWN2=55;       WITH_VAR2=56;
WITH_TEMP2=57;     WITH2=58;           VALUE2=59;          LT2=60;
EQ2=61;            GT2=62;             LE2=63;             NE2=64;
GE2=65;            IN2=66;             UPLUS2=67;          UMINUS2=68;
PLUS2=69;          MINUS2=70;          OR2=71;             STAR2=72;
SLASH2=73;         DIV2=74;            MOD2=75;            AND2=76;
FNAME2=77;         NOT2=78;            EMPTY_SET2=79;      INCLUDE2=80;
FUNCTION2=81;      CALL_FUNC2=82;      NAME2=83;           COMP2=84;
SUB2=85;           ARROW2=86;          CONSTANT2=87;       REAL2=88;
FREAL2=89;         INTEGER2=90;        FINTEGER2=91;       CHAR2=92;
FCHAR2=93;         STRING2=94;         FSTRING2=95;        NEW_LINE2=96;
LCONST2=97;        MESSAGE2=98;        TAG_ID2=99;         TAG_TYPE2=100;
PART_END2=101;     TAG_DEF2=102;       LABEL2=103;         CASE_JUMP2=104;


{OTHER CONSTANTS}
TEXT_LENGTH = 18;
INFILE = 2;        OUTFILE = 1;
THIS_PASS=2;       SPELLING_MAX=700;
COMP_BLOCK=TRUE;   ROUTINE_BLOCK=FALSE;



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
CASE_ERROR=25;     POINTER_ERROR=36;   WHILE_ERROR=27;     REPEAT_ERROR=28;
FOR_ERROR=29;      PREFIX_ERROR=37;    EXPR_ERROR=31;      VARIABLE_ERROR=32;
CONSTANT_ERROR=33; INTERFACE_ERROR=38;



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

TYPE ARGTAG =
  (NILTYPE, BOOLTYPE, INTTYPE, IDTYPE, PTRTYPE);
TYPE ARGTYPE = RECORD
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
QPROG_END,         QFBLOCK,            QPARM_LIST,         QSTAT,
QBODY_END,         QENTRY,             QSTAT_LIST,         QID_END,
QARGUMENT,         QARG_END,           QIF_END,            QTHEN_END,
QCASES,            QCASE_END,          QLABEL_LIST,        QDO_TAIL,
QUNARY,            QFACTOR,            QEXPR,              QUNTIL_TAIL,
QFOR_END,          QFORB_END,          QEXPR_OP,           QSEXPR_OP,
QTERM_OP,          QTERM_LIST,         QFACTOR_LIST,       QSET_EXPR,
QSELECT,           QSUB_END,           QARG,               QCOMMA,
QVARIANT_PART,     QTYPE_LIST,         QWITH_LIST,         QFIELD_LIST,
QTO_TAIL,          QFIELD_PACK,        QID_SEMI,           QVARIANT,
                   QPROGRAM,           QID_OPEN,           QID_CASE,
QSEMI_CASE,        QLABEL_TAIL:        SETS;


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
            FOR I:=1 TO LLENGTH{LENGTH DIV 2} DO BEGIN
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

  PROCEDURE  PROGRAM_ ;    FORWARD;
  PROCEDURE  PREFIX(KEYS: SETS); FORWARD;
  PROCEDURE  INTERFACE_(KEYS: SETS); FORWARD;
  PROCEDURE  PROG_HEADING (KEYS: SETS); FORWARD;
  PROCEDURE  BLOCK (KEYS: SETS); FORWARD;
  PROCEDURE  DECLARATIONS (KEYS: SETS); FORWARD;
  PROCEDURE  CONST_DEC (KEYS: SETS); FORWARD;
  PROCEDURE  TYPE_DEC (KEYS: SETS); FORWARD;
  PROCEDURE  TYPE_ (KEYS: SETS); FORWARD;
  PROCEDURE  ENUM_TYPE (KEYS: SETS);  FORWARD;
  PROCEDURE  SUBR_TYPE (KEYS: SETS); FORWARD;
  PROCEDURE  SET_TYPE  (KEYS: SETS); FORWARD;
  PROCEDURE  ARRAY_TYPE (KEYS: SETS); FORWARD;
  PROCEDURE  RECORD_TYPE (KEYS: SETS); FORWARD;
  PROCEDURE  FIELD_LIST (KEYS: SETS); FORWARD;
  PROCEDURE  VARIANT_PART (KEYS: SETS); FORWARD;
  PROCEDURE  VARIANT (KEYS: SETS); FORWARD;
  PROCEDURE  LABEL_LIST (KEYS: SETS; OP, ERROR_NUM: INTEGER); FORWARD;
  PROCEDURE  POINTER_TYPE (KEYS: SETS); FORWARD;
  PROCEDURE  VAR_DEC (KEYS: SETS); FORWARD;
  PROCEDURE  ID_LIST (KEYS: SETS; OP,ERROR_NUM: INTEGER; VAR ID_COUNT: INTEGER);
  FORWARD;
  PROCEDURE  IDENTIFIER_(KEYS: SETS; OP, ERROR_NUM: INTEGER); FORWARD;
  PROCEDURE  ROUTINE_DEC (KEYS: SETS); FORWARD;
  PROCEDURE  PROC_DEC (KEYS: SETS); FORWARD;
  PROCEDURE  PROC_HEADING  (KEYS: SETS); FORWARD;
  PROCEDURE  FUNC_DEC  (KEYS: SETS); FORWARD;
  PROCEDURE  FUNC_HEADING  (KEYS: SETS); FORWARD;
  PROCEDURE  PARM_LIST (KEYS: SETS); FORWARD;
  PROCEDURE  BODY  (KEYS: SETS); FORWARD;
  PROCEDURE  STAT_LIST (KEYS: SETS); FORWARD;
  PROCEDURE  STAT  (KEYS: SETS); FORWARD;
  PROCEDURE  ID_STAT  (KEYS: SETS); FORWARD;
  PROCEDURE  ARG_LIST  (KEYS: SETS); FORWARD;
  PROCEDURE  COMPOUND_STAT (KEYS: SETS); FORWARD;
  PROCEDURE  IF_STAT  (KEYS: SETS); FORWARD;
  PROCEDURE  CASE_STAT (KEYS: SETS); FORWARD;
  PROCEDURE  WHILE_STAT (KEYS: SETS); FORWARD;
  PROCEDURE  REPEAT_STAT (KEYS: SETS); FORWARD;
  PROCEDURE  FOR_STAT  (KEYS: SETS); FORWARD;
  PROCEDURE  WITH_STAT (KEYS: SETS); FORWARD;
  PROCEDURE  EXPR  (KEYS: SETS); FORWARD;
  PROCEDURE  SEXPR  (KEYS: SETS); FORWARD;
  PROCEDURE  TERM  (KEYS: SETS); FORWARD;
  PROCEDURE  FACTOR  (KEYS: SETS); FORWARD;
  PROCEDURE  FACTOR_ID  (KEYS: SETS); FORWARD;
  PROCEDURE  VARIABLE  (KEYS: SETS); FORWARD;
  PROCEDURE  CONSTANT  (KEYS: SETS); FORWARD;

 {INITIALIZE}
 PROCEDURE INITIALIZE;
  BEGIN
    CURRENT_LABEL:=1  {"THE MAIN PROGRAM"};
    IF TEST THEN PRINTFF(THIS_PASS);
    Test:=true;
    FIRST(outfile1);
    INIT (outfile2);
    IF TEST THEN PRINTFF(THIS_PASS);
    QIGNORE:=(.LCONST1,MESSAGE1,NEW_LINE1.);
    QCOMMA:=(.COMMA1.);
    QOPEN:=(.OPEN1.); QCLOSE:=(.CLOSE1.);
    QEOM:=(.EOM1.); QEND:=(.END1.);
    QSEMICOLON:=(.SEMICOLON1.);
    QBODY:=(.BEGIN1.); QID:=(.ID1.);
    QDEFINITIONS:=(.CONST1,TYPE1.);
    QROUTINES:=(.PROCEDURE1,FUNCTION1.);
    QDECLARATIONS:=QDEFINITIONS + (.VAR1.) + QROUTINES;
    QDEF:=(.ID1,SEMICOLON1,EQ1.);
    QDEC:=(.ID1,SEMICOLON1,COLON1.);
    QCONSTANT:=(.ID1,INTEGER1,REAL1,CHAR1,STRING1.);
    QCONST_DEF:=QDEF + QCONSTANT;
    QTYPE:=(.OPEN1,SET1,ARRAY1,RECORD1,ARROW1.) + QCONSTANT;
    QTYPE_DEF:=QDEF + QTYPE;
    QTYPE_LIST:=QTYPE + QCOMMA;
    QSUBR_LIMIT:=(.UP_TO1.) + QCONSTANT;
    QDIMENSION:=QTYPE + (.COMMA1,BUS1,OF1.);
    QOF_TYPE:=QTYPE + (.OF1.);
    QVAR_DEF:=QDEC + QTYPE;
    QBLOCK:=QDECLARATIONS + QBODY;
    QPARM_END:=QSEMICOLON + QBLOCK;
    QID_LIST:=(.ID1,COMMA1.);
    QPROC_END := (.ID1, OPEN1.) + QPARM_END;
    QARG:=(.ID1,INTEGER1,CHAR1,STRING1.);
    QPROC_PARMS:=QPROC_END-QID;
    QFUNC_END:=QPROC_END + (.COLON1.);
    QFUNC_TYPE:=QPARM_END + QID;
    QPROG_END:=QPROC_END-QBLOCK;
    QPARM_LIST:=QDEC + (.UNIV1,VAR1.);
    QSTAT:=(.ID1,BEGIN1,IF1,CASE1,WHILE1,REPEAT1,FOR1,WITH1.);
    QBODY_END:=QSTAT + QEND;
    QSTAT_LIST :=QSTAT + QSEMICOLON;
    QID_END:=(.BECOMES1,OPEN1.);
    QIF_END:=(.THEN1,ELSE1.) + QSTAT;
    QTHEN_END:=QIF_END-(.THEN1.);
    QCASES:=QCONSTANT + QSTAT + (.COLON1,COMMA1,SEMICOLON1.);
    QCASE_END:=QCASES + (.OF1,END1.);
    QLABEL_LIST:=QCONSTANT + QCOMMA;
    QLABEL_TAIL:=QLABEL_LIST + (.COLON1.);
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
    QTERM_LIST:=QFACTOR + QSEXPR_OP;
    QFACTOR_LIST:=QFACTOR + QTERM_OP;
    QSET_EXPR:=QARGUMENT + (.BUS1.);
    QSELECT:=(.PERIOD1,SUB1,ARROW1.);
    QSUB_END:=QARGUMENT + (.BUS1.);
    QWITH_LIST:=QDO_TAIL + QCOMMA;
    QTO_TAIL:=QDO_TAIL + QEXPR;
    QPROGRAM := (.PROGRAM1.);
    QID_SEMI := (.ID1, SEMICOLON1.);
    QID_OPEN := (.ID1, OPEN1.);
    QID_CASE := (.ID1, CASE1.);
    QSEMI_CASE := (.SEMICOLON1, CASE1.);
    QFIELD_LIST := QVAR_DEF + QID_CASE;
    QVARIANT_PART := QCONSTANT + (.COLON1, OF1, SEMICOLON1.);
    QVARIANT := QCONSTANT + QSEMICOLON;
    QFIELD_PACK := QID_CASE + (.OPEN1, CLOSE1.);
    QFBLOCK := QBLOCK + (.FORWARD1.);
    GET
  END;

  { PROGRAM }

  PROCEDURE ERROR(NUMBER:INTEGER; KEYS:SETS);
  BEGIN
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
    PREFIX(QBLOCK + QEOM);
    BLOCK(QEOM);
    IF SY=PERIOD1 THEN GET ELSE ERROR(PROG_ERROR,QEOM);
    IF SY<>EOM1 THEN ERROR(PROG_ERROR,QEOM);
    PUT0(EOM2)
  END;

  PROCEDURE PREFIX;
  VAR LKEYS1: SETS;
  BEGIN
    LKEYS1:=KEYS + QDEFINITIONS + QROUTINES + QPROGRAM;
    CHECK(PREFIX_ERROR, LKEYS1);
    WHILE SY IN QDEFINITIONS DO BEGIN
      IF SY=CONST1 THEN CONST_DEC(LKEYS1) ELSE TYPE_DEC(LKEYS1);
      CHECK(PREFIX_ERROR, LKEYS1)
    END;
    INTERFACE_(KEYS + QPROGRAM);
    PROG_HEADING(KEYS)
  END;

  PROCEDURE INTERFACE_;
  VAR LKEYS1: SETS;
  BEGIN
    LKEYS1:=KEYS + QROUTINES;
    CHECK(INTERFACE_ERROR, LKEYS1);
    WHILE SY IN QROUTINES DO BEGIN
      IF SY=PROCEDURE1 THEN PROC_HEADING(LKEYS1) ELSE FUNC_HEADING (LKEYS1);
        CHECK(INTERFACE_ERROR, LKEYS1)
    END
  END;

  PROCEDURE PROG_HEADING;
  BEGIN
    IF SY=PROGRAM1 THEN GET
                   ELSE ERROR(PROG_ERROR, KEYS + QID_OPEN + QSEMICOLON);
    IDENTIFIER_(KEYS + QOPEN + QSEMICOLON, PROG_ID2, PROG_ERROR);
    PARM_LIST(KEYS + QSEMICOLON);
    PUT0(PROG_DEF2);
    IF SY=SEMICOLON1 THEN GET ELSE ERROR(PROG_ERROR, KEYS);
  END;

 PROCEDURE BLOCK;
  BEGIN
    DECLARATIONS(KEYS + QBODY);
    BODY(KEYS)
  END;

  {############}
  {DECLARATIONS}
  {############}

  PROCEDURE DECLARATIONS;
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


  PROCEDURE CONST_DEC;
  VAR LKEYS1,LKEYS2:SETS;
  BEGIN
    LKEYS1:=KEYS + QCONST_DEF;
    LKEYS2:=KEYS-QCONST_DEF;
    GET;
    REPEAT
      IDENTIFIER_(LKEYS1,CONST_ID2,CONSTDEF_ERROR);
      IF SY=EQ1 THEN GET ELSE ERROR(CONSTDEF_ERROR,LKEYS1);
      CONSTANT(LKEYS1);
      PUT0(CONST_DEF2);
      IF SY=SEMICOLON1 THEN GET ELSE ERROR(CONSTDEF_ERROR,LKEYS1);
      CHECK(CONSTDEF_ERROR,LKEYS1)
    UNTIL SY IN LKEYS2
  END;

  PROCEDURE TYPE_DEC;
  VAR LKEYS1,LKEYS2:SETS;
  BEGIN
    LKEYS1:=KEYS + QTYPE_DEF;
    LKEYS2:=KEYS-QTYPE_DEF;
    GET;
    REPEAT
      IDENTIFIER_(LKEYS1,TYPE_ID2,TYPEDEF_ERROR);
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

  PROCEDURE TYPE_;
  BEGIN
    CHECK(TYPE_ERROR,KEYS + QTYPE);
    IF SY IN QTYPE THEN
      CASE SY OF
        OPEN1: ENUM_TYPE(KEYS);
        ID1,INTEGER1,REAL1,CHAR1,STRING1: SUBR_TYPE(KEYS);
        SET1: SET_TYPE(KEYS);
        ARRAY1: ARRAY_TYPE(KEYS);
        RECORD1: RECORD_TYPE(KEYS);
        ARROW1:  POINTER_TYPE(KEYS)
      END
    ELSE BEGIN
      ERROR(TYPE_ERROR,KEYS);
      PUT1(TYPE2,XUNDEF)
    END
  END;

  PROCEDURE ENUM_TYPE;
  VAR NUMBER:INTEGER;
  BEGIN
    PUT0(ENUM2); GET;
    ID_LIST(KEYS + QCLOSE,ENUM_ID2,ENUM_ERROR,NUMBER);
    IF SY=CLOSE1 THEN GET ELSE ERROR(ENUM_ERROR,KEYS);
    PUT0(ENUM_DEF2)
  END;

  PROCEDURE SUBR_TYPE;
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

  PROCEDURE SET_TYPE;
  BEGIN
    GET;
    IF SY=OF1 THEN GET ELSE ERROR(SET_ERROR,KEYS + QTYPE);
    TYPE_(KEYS);
    PUT0(SET_DEF2)
  END;

  PROCEDURE ARRAY_TYPE;
  VAR LKEYS1:SETS; I,DIMENSIONS:INTEGER; DONE:BOOLEAN;
  BEGIN
    LKEYS1:=KEYS + QDIMENSION;
    GET;
    IF SY=SUB1 THEN GET ELSE ERROR(ARRAY_ERROR,LKEYS1);
    DIMENSIONS:=0; DONE:=FALSE;
    REPEAT
      {INDEX}TYPE_(LKEYS1); DIMENSIONS:=DIMENSIONS+1;
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

  PROCEDURE RECORD_TYPE;
  BEGIN
    PUT0(REC2); GET;
    FIELD_LIST(KEYS + QEND);
    PUT0(REC_DEF2);
    IF SY=END1 THEN GET ELSE ERROR(RECORD_ERROR,KEYS);
  END;

  PROCEDURE FIELD_LIST;
  VAR LKEYS1: SETS; NUMBER: INTEGER; DONE: BOOLEAN;
  BEGIN
    LKEYS1 := KEYS + QFIELD_LIST;
    DONE := FALSE;
    REPEAT
      CHECK(RECORD_ERROR, LKEYS1);
      IF SY<>CASE1 THEN BEGIN
        ID_LIST(LKEYS1, FIELD_ID2, RECORD_ERROR, NUMBER);
        IF SY=COLON1 THEN GET ELSE ERROR(RECORD_ERROR, LKEYS1);
        TYPE_(LKEYS1);
        PUT1(FIELDLIST2, NUMBER);
        CHECK(RECORD_ERROR, LKEYS1);
        IF SY IN QFIELD_LIST THEN
          IF SY=SEMICOLON1 THEN GET ELSE ERROR(RECORD_ERROR, LKEYS1)
        ELSE DONE := TRUE
      END ELSE DONE := TRUE
    UNTIL DONE;
    IF SY=CASE1 THEN VARIANT_PART(KEYS);
  END;

  PROCEDURE VARIANT_PART;
  VAR LKEYS1, LKEYS2: SETS; DONE: BOOLEAN;
  BEGIN
    LKEYS1 := KEYS + QVARIANT_PART; LKEYS2 := KEYS + QVARIANT;
    GET;
    IDENTIFIER_(LKEYS1, TAG_ID2, RECORD_ERROR);
    IF SY=COLON1 THEN GET ELSE ERROR(RECORD_ERROR, LKEYS1);
    IDENTIFIER_(LKEYS1, TAG_TYPE2, RECORD_ERROR); PUT0(TAG_DEF2);
    IF SY=OF1 THEN GET ELSE ERROR(RECORD_ERROR, LKEYS2);
    DONE := FALSE;
    REPEAT
      VARIANT(LKEYS2);
      CHECK(RECORD_ERROR, LKEYS2);
      IF SY IN QVARIANT THEN
        IF SY=SEMICOLON1 THEN GET ELSE ERROR(RECORD_ERROR, LKEYS2)
      ELSE DONE := TRUE
    UNTIL DONE;
    PUT0(PART_END2)
  END;

  PROCEDURE VARIANT;
  BEGIN
    PUT0(VARNT2);
    LABEL_LIST(KEYS + QFIELD_PACK, LABEL2, RECORD_ERROR);
    IF SY=OPEN1 THEN GET ELSE ERROR(RECORD_ERROR, KEYS + QID_CASE + QCLOSE);
    FIELD_LIST(KEYS + QCLOSE);
    PUT0(VARNT_END2);
    IF SY=CLOSE1 THEN GET ELSE ERROR(RECORD_ERROR, KEYS);
  END;

  PROCEDURE LABEL_LIST;
  VAR LKEYS1: SETS; DONE: BOOLEAN;
  BEGIN
    LKEYS1 := KEYS + QLABEL_TAIL; DONE := FALSE;
    REPEAT
      CONSTANT(LKEYS1);
      PUT0(OP);
      CHECK(ERROR_NUM, LKEYS1);
      IF SY IN QLABEL_LIST THEN
        IF SY=COMMA1 THEN GET ELSE ERROR(ERROR_NUM, LKEYS1)
      ELSE DONE := TRUE
    UNTIL DONE;
    IF OP=LABEL2 THEN PUT0(LBL_END2);
    IF SY=COLON1 THEN GET ELSE ERROR(ERROR_NUM, KEYS)
  END;

  PROCEDURE POINTER_TYPE;
  BEGIN
    GET;
    IDENTIFIER_(KEYS, POINTER2, POINTER_ERROR)
  END;

   {#########}
   {VARIABLES}
   {#########}

  PROCEDURE VAR_DEC;
  VAR    NUMBER:INTEGER; LKEYS1:SETS;
  BEGIN
    LKEYS1:=KEYS + QVAR_DEF;
    GET;
    REPEAT
      ID_LIST(LKEYS1,VAR_ID2,VAR_ERROR,NUMBER);
      IF SY=COLON1 THEN GET ELSE ERROR(VAR_ERROR,LKEYS1);
      {VAR} TYPE_(LKEYS1);
      PUT1(VAR_LIST2, NUMBER);
      IF SY=SEMICOLON1 THEN GET ELSE ERROR(VAR_ERROR,LKEYS1);
      CHECK(VAR_ERROR,LKEYS1)
    UNTIL NOT(SY IN QVAR_DEF);
  END;

  PROCEDURE ID_LIST;
  VAR LKEYS1:SETS; DONE:BOOLEAN;
  BEGIN
    LKEYS1:=KEYS + QID_LIST;
    ID_COUNT:=0; DONE:=FALSE;
    REPEAT
      IDENTIFIER_(LKEYS1,OP,ERROR_NUM);
      ID_COUNT:=ID_COUNT+1;
      CHECK(ERROR_NUM,LKEYS1);
      IF SY IN QID_LIST THEN
        IF SY=COMMA1 THEN GET ELSE ERROR(ERROR_NUM,LKEYS1)
      ELSE DONE:=TRUE
    UNTIL DONE
  END;

  PROCEDURE IDENTIFIER_;
  BEGIN
    IF SY=ID1 THEN BEGIN PUT1(OP,ARG); GET END
    ELSE BEGIN
      ERROR(ERROR_NUM,KEYS);
      PUT1(OP,XUNDEF)
    END
  END;


  {########}
  {ROUTINES}
  {########}

 PROCEDURE ROUTINE_DEC;
  VAR LKEYS1:SETS;
  BEGIN
    LKEYS1:=KEYS + QROUTINES;
    REPEAT
      CASE SY OF
        PROCEDURE1: PROC_DEC(LKEYS1);
        FUNCTION1: FUNC_DEC(LKEYS1)
      END;
      IF SY=SEMICOLON1 THEN GET ELSE ERROR(ROUTINE_ERROR, LKEYS1);
      CHECK(ROUTINE_ERROR,LKEYS1);
    UNTIL NOT(SY IN QROUTINES)
  END;

  PROCEDURE PROC_DEC;
  BEGIN
    PROC_HEADING(KEYS + QFBLOCK);
    CHECK(PROC_ERROR, KEYS + QFBLOCK);
    IF SY=FORWARD1 THEN BEGIN
      PUT0(FORWARD2); GET;
    END ELSE BLOCK(KEYS);
  END;

  PROCEDURE PROC_HEADING;
  BEGIN
    GET;
    IDENTIFIER_(KEYS + QDEC, PROC_ID2, PROC_ERROR);
    PARM_LIST(KEYS + QSEMICOLON);
    PUT0(PROC_DEF2);
    IF SY=SEMICOLON1 THEN GET ELSE ERROR(PROC_ERROR, KEYS);
  END;

  PROCEDURE FUNC_DEC;
  BEGIN
    FUNC_HEADING(KEYS + QFBLOCK);
    CHECK(FUNC_ERROR, KEYS + QFBLOCK);
    IF SY=FORWARD1 THEN BEGIN
      PUT0(FORWARD2); GET
    END ELSE BLOCK(KEYS)
  END;

  PROCEDURE FUNC_HEADING;
  VAR LKEYS1: SETS;
  BEGIN
    LKEYS1 := KEYS + QDEC + QOPEN;
    GET;
    IDENTIFIER_(LKEYS1, FUNC_ID2, FUNC_ERROR);
    CHECK(FUNC_ERROR, LKEYS1);
    IF SY<>SEMICOLON1 THEN BEGIN
      PARM_LIST(KEYS + QDEC);
      IF SY=COLON1 THEN GET ELSE ERROR(FUNC_ERROR, KEYS + QID_SEMI);
      IDENTIFIER_(KEYS + QSEMICOLON, FUNC_TYPE2, FUNC_ERROR)
    END;
    PUT0(FUNC_DEF2);
    IF SY=SEMICOLON1 THEN GET ELSE ERROR(FUNC_ERROR, KEYS);
  END;

  PROCEDURE PARM_LIST;
  VAR LIST_OP,TYPE_OP,NUMBER:INTEGER; DONE:BOOLEAN; LKEYS1:SETS;
  BEGIN
    LKEYS1:=KEYS + QPARM_LIST + QCLOSE;
    CHECK(PARM_ERROR,KEYS + QOPEN);
    IF SY=OPEN1 THEN BEGIN
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
        {TYPE} IDENTIFIER_(LKEYS1,TYPE_OP,PARM_ERROR);
        PUT1(LIST_OP,NUMBER);
        CHECK(PARM_ERROR,LKEYS1);
        IF SY IN QPARM_LIST THEN
          IF SY=SEMICOLON1 THEN GET ELSE ERROR(PARM_ERROR,LKEYS1)
        ELSE DONE:=TRUE
      UNTIL DONE;
      IF SY=CLOSE1 THEN GET ELSE ERROR(PARM_ERROR,KEYS)
    END
  END;


  {####}
  {BODY}
  {####}

  PROCEDURE BODY;
  BEGIN
    PUT0(BODY2);
    IF SY=BEGIN1 THEN GET ELSE ERROR(BODY_ERROR,KEYS + QBODY_END);
    STAT_LIST (KEYS + QEND);
    PUT0(BODY_END2);
    IF SY=END1 THEN GET ELSE ERROR(BODY_ERROR,KEYS)
  END;

  PROCEDURE STAT_LIST;
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

  PROCEDURE STAT;
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
        WITH1: WITH_STAT(KEYS)
      END
  END;

  PROCEDURE ID_STAT;
  VAR LKEYS1: SETS;
  BEGIN
    LKEYS1:=KEYS + QID_END;
    VARIABLE(LKEYS1);
    CHECK(IDSTAT_ERROR,LKEYS1);
    IF SY=BECOMES1 THEN BEGIN
      PUT0(ANAME2); GET;
      EXPR(KEYS); PUT0(STORE2)
    END ELSE BEGIN
      PUT0(CALL_NAME2);
      ARG_LIST(KEYS);
      PUT0(CALL2)
    END
  END;

  PROCEDURE ARG_LIST;
  VAR DONE:BOOLEAN; LKEYS1:SETS;
  BEGIN
    CHECK(ARG_ERROR,KEYS + QOPEN);
    IF SY=OPEN1 THEN BEGIN
      PUT0(ARG_LIST2); GET; DONE:=FALSE; LKEYS1:=KEYS + QARG_END;
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

  PROCEDURE COMPOUND_STAT;
  BEGIN
    GET;
    STAT_LIST (KEYS);
    IF SY=END1 THEN GET ELSE ERROR(COMP_ERROR,KEYS)
  END;

  PROCEDURE IF_STAT;
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

  PROCEDURE CASE_STAT;
  VAR L0,LI,LN:LABEL1; DONE:BOOLEAN; LKEYS1:SETS;
  BEGIN
    LKEYS1:=KEYS + QCASES;
    GET; NEW_LABEL(L0); NEW_LABEL(LN);
    EXPR(KEYS + QCASE_END);
    PUT1(CASE_JUMP2,L0); DONE:=FALSE;
    IF SY=OF1 THEN GET ELSE ERROR(CASE_ERROR,LKEYS1);
    REPEAT
      NEW_LABEL(LI); PUT1(DEF_CASE2,LI);
      LABEL_LIST(LKEYS1, CASE2, CASE_ERROR);
      STAT(LKEYS1); PUT1(JUMP2,LN);
      CHECK(CASE_ERROR,LKEYS1);
      IF SY IN QCASES THEN
        IF SY=SEMICOLON1 THEN GET ELSE ERROR(CASE_ERROR,LKEYS1)
      ELSE DONE:=TRUE
    UNTIL DONE;
    PUT2(END_CASE2,L0,LN);
    IF SY=END1 THEN GET ELSE ERROR(CASE_ERROR,KEYS);
  END;

  PROCEDURE WHILE_STAT;
  VAR L1,L2:LABEL1;
  BEGIN
    NEW_LABEL(L1);  NEW_LABEL(L2);
    PUT1(DEF_LABEL2,L1);
    GET;
    EXPR(KEYS + QDO_TAIL);
    PUT1(FALSEJUMP2,L2);
    IF SY=DO1 THEN GET ELSE ERROR(WHILE_ERROR,KEYS + QSTAT);
    STAT(KEYS);
    PUT2(JUMP_DEF2,L1,L2)
  END;

  PROCEDURE REPEAT_STAT;
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

  PROCEDURE FOR_STAT;
  CONST UP=5; DOWN=3;
  VAR L1,L2:LABEL1; LKEYS1:SETS; OP,DIRECTION:INTEGER;
  BEGIN
    LKEYS1:=KEYS + QFORB_END;
    GET; NEW_LABEL(L1); NEW_LABEL(L2);
    IDENTIFIER_(KEYS + QFOR_END,NAME2,FOR_ERROR); PUT0(ADDRESS2);
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

  PROCEDURE WITH_STAT;
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
    LKEYS1:=KEYS  + QSELECT;
    IDENTIFIER_(LKEYS1,NAME2,VARIABLE_ERROR);
    CHECK(VARIABLE_ERROR,LKEYS1);
    WHILE SY IN QSELECT DO BEGIN
      CASE SY OF
      PERIOD1:
        BEGIN
        PUT0(ADDRESS2);
        GET;
        IDENTIFIER_(LKEYS1,COMP2,VARIABLE_ERROR)
        END;
      SUB1:
        BEGIN
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
      ARROW1:
        BEGIN
        PUT0(ARROW2); GET
        END
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
TYPE_DEF1=5;       VAR_ID1=6;          VAR_LIST1=7;        PROC_ID1=8;
PROC_DEF1=9;       LBL_END1=10;        FORWARD1=11;        FUNC_ID1=12;
FUNC_DEF1=13;      POINTER1=14;        FUNC_TYPE1=15;      PROG_ID1=16;
PROG_DEF1=17;      VARNT_END1=18;      TYPE1=19;           ENUM1=20;
ENUM_ID1=21;       ENUM_DEF1=22;       SUBR_DEF1=23;       SET_DEF1=24;
ARRAY_DEF1=25;     REC1=26;            FIELD_ID1=27;       FIELDLIST1=28;
REC_DEF1=29;       VARNT1=30;          PARM_ID1=31;        PARM_TYPE1=32;
UNIV_TYPE1=33;     CPARMLIST1=34;      VPARMLIST1=35;      BODY1=36;
BODY_END1=37;      ANAME1=38;          STORE1=39;          CALL_NAME1=40;
CALL1=41;          ARG_LIST1=42;       ARG1=43;            FALSEJUMP1=44;
DEF_LABEL1=45;     JUMP_DEF1=46;       DEF_CASE1=47;       CASE1=48;
JUMP1=49;          END_CASE1=50;       ADDRESS1=51;        FOR_STORE1=52;
FOR_LIM1=53;       FOR_UP1=54;         FOR_DOWN1=55;       WITH_VAR1=56;
WITH_TEMP1=57;     WITH1=58;           VALUE1=59;          LT1=60;
EQ1=61;            GT1=62;             LE1=63;             NE1=64;
GE1=65;            IN1=66;             UPLUS1=67;          UMINUS1=68;
PLUS1=69;          MINUS1=70;          OR1=71;             STAR1=72;
SLASH1=73;         DIV1=74;            MOD1=75;            AND1=76;
FNAME1=77;         NOT1=78;            EMPTY_SET1=79;      INCLUDE1=80;
FUNCTION1=81;      CALL_FUNC1=82;      NAME1=83;           COMP1=84;
SUB1=85;           ARROW1=86;          CONSTANT1 =87;       REAL1=88;
FREAL1=89;         INTEGER1=90;        FINTEGER1=91;       CHAR1=92;
FCHAR1=93;         STRING1=94;         FSTRING1=95;        NEW_LINE1=96;
LCONST1=97;        MESSAGE1=98;        TAG_ID1=99;         TAG_TYPE1=100;
PART_END1=101;     TAG_DEF1=102;       LABEL1=103;         CASE_JUMP1=104;

{OUTPUT OPERATORS}
EOM2=1;            PROG_DEF2=2;        TYPE_DEF2=3;        TYPE2=4;
ENUM_DEF2=5;       SUBR_DEF2=6;        SET_DEF2=7;         ARRAY_DEF2=8;
POINTER2=9;        REC2=10;            REC_DEF2=11;        NEW_NOUN2=12;
FIELDLIST2=13;     TAG_DEF2=14;        PART_END2=15;       CASE_JUMP2=16;
VARNT_END2=17;     VAR_LIST2=18;       FORWARD2=19;        PROC_DEF2=20;
PROCF_DEF2=21;     LCONST2=22;         FUNC_DEF2=23;       FUNCF_DEF2=24;
PARM_TYPE2=25;     UNIV_TYPE2=26;      CPARMLIST2=27;      VPARMLIST2=28;
BODY2=29;          BODY_END2=30;       ADDRESS2=31;        RESULT2=32;
STORE2=33;         CALL_PROC2=34;      PARM2=35;           FALSEJUMP2=36;
DEF_LABEL2=37;     JUMP_DEF2=38;       JUMP2=39;           CHK_TYPE2=40;
CASE_LIST2=41;     FOR_STORE2=42;      FOR_LIM2=43;        FOR_UP2=44;
FOR_DOWN2=45;      WITH_VAR2=46;       WITH_TEMP2=47;      WITH2=48;
VALUE2=49;         LT2=50;             EQ2=51;             GT2=52;
LE2=53;            NE2=54;             GE2=55;             IN2=56;
UPLUS2=57;         UMINUS2=58;         PLUS2=59;           MINUS2=60;
OR2=61;            STAR2=62;           SLASH2=63;          DIV2=64;
MOD2=65;           AND2=66;            NOT2=67;            EMPTY_SET2=68;
INCLUDE2=69;       FUNCTION2=70;       CALL_FUNC2=71;      ROUTINE2=72;
VAR2=73;           ARROW2=74;          VCOMP2=75;          SUB2=76;
INDEX2=77;         REAL2=78;           STRING2=79;         NEW_LINE2=80;
MESSAGE2=81;       CALL_NEW2=82;       UNDEF2=83;          VARIANT2=84;
MODE2=85;

{OTHER CONSTANTS}
MIN_CASE=0;        MAX_CASE=127;       THIS_PASS=3;        SPELLING_MAX=700;
TEXT_LENGTH = 18;
INFILE = 1;        OUTFILE = 2;
NOUN_MAX=700;
OPERAND_MAX=150;   UPDATE_MAX=100;     UPDATE_MAX1=101;    MAX_LEVEL=15;
MAX_TAG=15;        MIN_TAG=0;          TAG_STACK_MAX=5;


{MODES}
PROC_MODE=1;       FUNC_MODE=2;        PROGRAM_MODE=3;

{STANDARD SPELLING/NOUN INDICES}
XUNDEF=0;          XFALSE=1;           XTRUE=2;            XINTEGER=3;
XBOOLEAN=4;        XCHAR=5;            XNIL=6;             XABS=7;
XATTRIBUTE=8;      XCHR=9;             XCONV=10;           XORD=11;
XPRED=12;          XSUCC=13;           XTRUNC=14;          XNEW=15;
XREAL=16;

{STANDARD NOUN INDICES}
ZARITHMETIC=17;    ZINDEX=18;          ZPASSIVE=19;        ZPOINTER=20;
ZVPARM=21;         ZCPARM=22;          ZSPARM=23;          ZNPARM=24;
ZWITH=25;

{ERRORS}
UNRES_ERROR=1;     AMBIGUITY_ERROR=2;  ABORT_ERROR=3;      CONSTID_ERROR=4;
SUBR_ERROR=5;      FEW_ARGS_ERROR=6;   ARG_LIST_ERROR=7;   MANY_ARGS_ERROR=8;
LBLRANGE_ERROR=9;  LBLTYPE_ERROR=10;   AMBILBL_ERROR=11;   WITH_ERROR=12;
ARROW_ERROR=20;    PROC_USE_ERROR=14;  NAME_ERROR=15;      COMP_ERROR=16;
SUB_ERROR=17;      CALL_NAME_ERROR=19; RESOLVE_ERROR=21;

{MISCELANEOUS}
NOT_POSSIBLY_FORWARD=FALSE;            POSSIBLY_FORWARD=TRUE;
OUTPUT=TRUE;       RETAIN=FALSE;       PROC_TYPE=NIL;      STD_LEVEL=0;
PREFIX_LEVEL=1;    GLOBAL_LEVEL=2;


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
    PARAMETER,FIELD,SCALAR_KIND,ROUTINE_KIND,SET_KIND,
    POINTER_KIND,ARRAY_KIND,RECORD_KIND,WITH_KIND,UNDEF_KIND);
  OPERAND_CLASS=(VAR_CLASS,ROUTINE_CLASS,ICONST_CLASS,RCONST_CLASS,SCONST_CLASS,
    DEF_CLASS,UNDEF_CLASS,FCONST_CLASS,CASE_LABEL);

ERROR_NOTE=(YES,NO,SUPPRESS);

SPELLING_INDEX=0..SPELLING_MAX;
NOUN_INDEX = 0..NOUN_MAX;
STACK_INDEX=-1{0} ..OPERAND_MAX;
UNIV_SET = ARRAY (.1..8.) OF INTEGER;
UPDATE_INDEX=-1 {0}..UPDATE_MAX;
NAME_PTR=^NAME_REC;
VARIANT_PTR=^VARIANT_REC;
TAG_INDEX=0..TAG_STACK_MAX;

PACKED_SET=INTEGER;

ENTRY_PTR=^ENTRY_REC;
ENTRY_REC=
    RECORD
      NOUN:NOUN_INDEX;
      CASE KIND:ENTRY_KIND OF
        INDEX_CONST:(CONST_TYPE:NOUN_INDEX; CONST_VAL:INTEGER);
        REAL_CONST:(REAL_DISP:INTEGER);
        STRING_CONST:(STRING_LENGTH,STRING_DISP:INTEGER);
        VARIABLE:(VAR_TYPE:ENTRY_PTR);
        PARAMETER:(PARM_TYPE:ENTRY_PTR);
        FIELD:(FIELD_TYPE:ENTRY_PTR; VARIANT:VARIANT_PTR);
        SCALAR_KIND:(RANGE_TYPE:NOUN_INDEX);
        ROUTINE_KIND:(ROUT_PARM: NAME_PTR; ROUT_TYPE:ENTRY_PTR);
        POINTER_KIND:(OBJECT_TYPE,NEXT_FWD:ENTRY_PTR);
        ARRAY_KIND:(INDEX_TYPE:NOUN_INDEX; EL_TYPE:ENTRY_PTR);
        WITH_KIND:(WITH_TYPE:NOUN_INDEX);
        RECORD_KIND:(FIELD_NAME:NAME_PTR)
    END;
  OPERAND=
    RECORD
      CASE CLASS_:OPERAND_CLASS OF
        VAR_CLASS:(VTYPE:ENTRY_PTR);
        ROUTINE_CLASS:(ROUT:ENTRY_PTR; PARM:NAME_PTR);
        ICONST_CLASS:(ICONST_TYPE:NOUN_INDEX; ICONST_VAL:INTEGER);
        RCONST_CLASS:(RCONST_DISP:INTEGER);
        SCONST_CLASS:(SCONST_LENGTH,SCONST_DISP:INTEGER);
        CASE_LABEL:(LABEL1,INDEX:INTEGER);
        DEF_CLASS:(DEF_ENTRY:ENTRY_PTR; DEF_SPIX:SPELLING_INDEX)
    END;

    NAME_ACCESS=(GENERAL,INCOMPLETE,
    UNRES_TYPE,UNRES_ROUTINE,QUALIFIED,UNDEFINED);

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
      PREV_HEAD,PREV_TAIL: NAME_PTR
    END;

   UPDATE_REC=
    RECORD
      UPDATE_SPIX:SPELLING_INDEX;
      OLD_ENTRY:SPELLING_ENTRY
    END;


  VARIANT_REC=
    RECORD
      TAG_NOUN:NOUN_INDEX;
      LABEL_SET:PACKED_SET;
      PARENT_VARIANT:VARIANT_PTR
    END;

  NAME_REC=
    RECORD
      NAME_SPIX:SPELLING_INDEX;
      NAME_ENTRY:ENTRY_PTR;
      NEXT_NAME:NAME_PTR
    END;

    TAG_SET=SET OF MIN_TAG..MAX_TAG;
VAR
  PARAMETERIZED,CONSTANTS: SET OF OPERAND_CLASS;

  QUALIFIABLE,TYPES,CONST_KINDS: SET OF ENTRY_KIND;
  NAME_LIST, OLD_NAMES: NAME_PTR;
  HALT, RESOLUTION: BOOLEAN;
  OPS:ARRAY (.STACK_INDEX.) OF OPERAND;
  UENTRY,FIRST_PARM,THIS_PARM,POINTER_TYPE: ENTRY_PTR;
  INACCESSIBLE,ENTRY_ACCESS,OP_ACCESS: SET OF NAME_ACCESS;
  LABELS: ARRAY (.MIN_CASE..MAX_CASE.) OF INTEGER;
  THIS_UPDATE: -1..UPDATE_MAX{UPDATE_INDEX};
  T:-1..OPERAND_MAX{STACK_INDEX};
  ENUM_VAL,THIS_LABEL,SY,CONST_DISP: INTEGER;
  ENUM_TYPE,THIS_NOUN: NOUN_INDEX;
  UPDATES:ARRAY (.UPDATE_INDEX.) OF UPDATE_REC;
  DISPLAY:ARRAY (.LEVEL_INDEX.) OF DISPLAY_REC;
  SYSCOMP_LEVEL,THIS_LEVEL,BODY_LEVEL: LEVEL_INDEX;
  SPELLING_TABLE:ARRAY (.SPELLING_INDEX.) OF SPELLING_ENTRY;
  UNRESOLVED,TAG_TOP,RESET_POINT: INTEGER;
  NEW_TYPE,LABEL_TYPE,TAG_FIELD,NEW_TAG_FIELD,
  RESET_NOUN: NOUN_INDEX;
  FUNC_TYPE_SW,UPDATE_SW,PREFIX_SW: BOOLEAN;
  THIS_FUNCTION:ENTRY_PTR;
  NAME_HEAD,NAME_TAIL: NAME_PTR;
  THIS_VARIANT:VARIANT_PTR;

   TAG_STACK: ARRAY (.TAG_INDEX.) OF
    RECORD
      PREV_LABELS:TAG_SET;
      PREV_TAG,PREV_TYPE:NOUN_INDEX
    END;

   VARIANT_LABELS,TAG_LABELS: TAG_SET;

  PROCEDURE PACK(LONG_SET: {Univ_SET} TAG_SET; VAR SHORT_SET: PACKED_SET);
  var
  i: Integer;

  BEGIN

   //SHORT_SET:= LONG_SET(.1.);
  Short_Set := 0;
  for i := 0 to 15 do
    if i in Long_Set then
      Short_Set := Short_Set or (1 shl i);
  END;

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

  PROCEDURE STD_PARM(VAR PARM_ENTRY: NAME_PTR; PARMTYPE:ENTRY_PTR;
    PARM_INDEX:NOUN_INDEX);
  BEGIN
    NEW(PARM_ENTRY);
    WITH PARM_ENTRY^ DO BEGIN
      NAME_SPIX:=XUNDEF;
      NEW(NAME_ENTRY);
      WITH NAME_ENTRY^ DO BEGIN
        NOUN:=PARM_INDEX;
        KIND:=PARAMETER;
        PARM_TYPE:=PARMTYPE
      END;
      NEXT_NAME:=NIL
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

  PROCEDURE STD_ROUT (ROUT_INDEX: NOUN_INDEX; ROUTTYPE: ENTRY_PTR;
    FIRST_PARM: NAME_PTR);
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
  VAR I:INTEGER; INT_TYPE,REAL_TYPE,BOOL_TYPE,CHAR_TYPE,POINTER_TYPE,
    INDEX_TYPE,ARITH_TYPE,PASSIVE_TYPE: ENTRY_PTR;
    ARITH_SPARM,INT_CPARM,PTR_VPARM,CHAR_CPARM,INDEX_CPARM,REAL_CPARM,
    INDEX_SPARM:  NAME_PTR;
  BEGIN
   TEST:=true;
   FIRST(outfile2);
   INIT (outfile3);
   IF TEST THEN PRINTFF(THIS_PASS);
   THIS_NOUN:=ZWITH;   NEW_TYPE:=XUNDEF;
    HALT:=FALSE; RESOLUTION:=FALSE; FUNC_TYPE_SW:=FALSE;
    PREFIX_SW:=TRUE; THIS_FUNCTION:=NIL;
    CONST_DISP:=0;
    UNRESOLVED:=0 {UNRESOLVED IDENTIFIERS};
    THIS_VARIANT:=nil;
    tag_TOP:=0;
    CONSTANTS:=(.ICONST_CLASS,RCONST_CLASS,SCONST_CLASS.);
    TYPES:=(.SCALAR_KIND,ARRAY_KIND,RECORD_KIND,POINTER_KIND,SET_KIND,
      UNDEF_KIND.);
    OP_ACCESS:=(.GENERAL,UNRES_ROUTINE,QUALIFIED.);
    CONST_KINDS:=(.INDEX_CONST,REAL_CONST,STRING_CONST.);
    INACCESSIBLE:=(.UNDEFINED,INCOMPLETE,UNRES_TYPE.);
    THIS_UPDATE:= -1; T:= -1; THIS_LEVEL:= PREFIX_LEVEL;
    FOR I:=0 TO SPELLING_MAX DO
      SPELLING_TABLE(.I.).ACCESS:=UNDEFINED;
    {STANDARD ENTRYS}
    STD_CONST(XFALSE,XBOOLEAN,0);
    STD_CONST(XTRUE,XBOOLEAN,1);
    STD_CONST(XNIL,ZPOINTER,0);
    STD_ENTRY(UENTRY,XUNDEF);
    STD_ENTRY(INDEX_TYPE,ZINDEX);
    STD_ENTRY(ARITH_TYPE,ZARITHMETIC);
    STD_ENTRY(PASSIVE_TYPE,ZPASSIVE);
    STD_ENTRY(POINTER_TYPE,ZPOINTER);
    STD_SCALAR(INT_TYPE,XINTEGER);
    STD_SCALAR(REAL_TYPE,XREAL);
    STD_SCALAR(BOOL_TYPE,XBOOLEAN);
    STD_SCALAR(CHAR_TYPE,XCHAR);
    STD_PARM(ARITH_SPARM,ARITH_TYPE,ZSPARM);
    STD_PARM(INT_CPARM,INT_TYPE,ZCPARM);
    STD_PARM(CHAR_CPARM,CHAR_TYPE,ZCPARM);
    STD_PARM(INDEX_CPARM,INDEX_TYPE,ZCPARM);
    STD_PARM(INDEX_SPARM,INDEX_TYPE,ZSPARM);
    STD_PARM(REAL_CPARM,REAL_TYPE,ZCPARM);
    STD_PARM(PTR_VPARM,POINTER_TYPE,ZNPARM);
    STD_ROUT(XABS, ARITH_TYPE, ARITH_SPARM);
    STD_ROUT(XATTRIBUTE, INT_TYPE, INT_CPARM);
    STD_ROUT(XCHR, CHAR_TYPE, INT_CPARM);
    STD_ROUT(XCONV, REAL_TYPE, INT_CPARM);
    STD_ROUT(XORD, INT_TYPE, CHAR_CPARM);
    STD_ROUT(XPRED, INDEX_TYPE, INDEX_SPARM);
    STD_ROUT(XSUCC, INDEX_TYPE, INDEX_SPARM);
    STD_ROUT(XTRUNC, INT_TYPE, REAL_CPARM);
    STD_ROUT(XNEW, PROC_TYPE, PTR_VPARM);
   END;

 {NESTING }
 {"#######}

  PROCEDURE UPDATE_CHECK;
  BEGIN
    UPDATE_SW:= (THIS_LEVEL > GLOBAL_LEVEL) OR (THIS_LEVEL = GLOBAL_LEVEL)
      AND PREFIX_SW;
  END;

  PROCEDURE PUSH_LEVEL(E:ENTRY_PTR);
  BEGIN
    IF THIS_LEVEL>=MAX_LEVEL THEN ABORT ELSE THIS_LEVEL:=THIS_LEVEL+1;
    UPDATE_CHECK;
    WITH DISPLAY(.THIS_LEVEL.) DO BEGIN
      BASE:=THIS_UPDATE+1;
      LEVEL_ENTRY:=E;
      PREV_HEAD:=NAME_HEAD; PREV_TAIL:=NAME_TAIL; NAME_HEAD:=NIL
    END
  END;

  PROCEDURE POP_LEVEL;
  VAR U:UPDATE_INDEX;
  BEGIN
    WITH DISPLAY (.THIS_LEVEL.) DO BEGIN
      NAME_HEAD:=PREV_HEAD; NAME_TAIL:=PREV_TAIL;
      FOR U:=THIS_UPDATE DOWNTO BASE DO
        WITH UPDATES(.U.) DO BEGIN
          SPELLING_TABLE(.UPDATE_SPIX.):=OLD_ENTRY
        END;
      THIS_UPDATE:=BASE-1
    END;
    THIS_LEVEL:= THIS_LEVEL - 1;
    UPDATE_CHECK
  END;

{#############}
{NAME HANDLING}
{#############}
 PROCEDURE PUSH;
  BEGIN
    IF T>= OPERAND_MAX THEN
     ABORT
    ELSE
    T:=T+1
  END;

  PROCEDURE NEW_ENTRY(VAR E:ENTRY_PTR);
  BEGIN
    IF THIS_NOUN>=NOUN_MAX THEN
     ABORT
    ELSE
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
      CLASS_:=DEF_CLASS;
      DEF_ENTRY:=E; DEF_SPIX:=XUNDEF
    END
  END;

  PROCEDURE UPDATE(SPIX:SPELLING_INDEX; E:ENTRY_PTR; A:NAME_ACCESS);
  BEGIN
    IF UPDATE_SW THEN
     BEGIN
      {SAVE OLD ENTRY}
      IF THIS_UPDATE>=UPDATE_MAX THEN
       ABORT
      ELSE
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
  VAR SPIX:{SPELLING_INDEX} INTEGER; E:ENTRY_PTR;
  BEGIN
    GET_L(outfile2, SPIX);
    IF SPIX<>XUNDEF THEN
      WITH SPELLING_TABLE(.SPIX.) DO
        IF (ACCESS<>UNDEFINED) AND (LEVEL=THIS_LEVEL) THEN
          IF RESOLVE AND (ACCESS=UNRES_ROUTINE) THEN
          BEGIN
            E:=ENTRY; ACCESS:=GENERAL;
            RESOLUTION:=TRUE; UNRESOLVED:=UNRESOLVED-1
          END ELSE
          BEGIN
            ERROR(AMBIGUITY_ERROR); SPIX:=XUNDEF;
          END
        ELSE BEGIN
          NEW_ENTRY(E);
          UPDATE(SPIX,E,A)
        END;
    PUSH;
    WITH OPS(.T.) DO
      IF SPIX=XUNDEF THEN BEGIN
        CLASS_:=UNDEF_CLASS;
        IF OUTPUT THEN PUT1(NEW_NOUN2,XUNDEF)
      END ELSE BEGIN
        CLASS_:=DEF_CLASS; DEF_ENTRY:=E; DEF_SPIX:=SPIX;
        IF OUTPUT THEN PUT1(NEW_NOUN2,E^.NOUN)
      END
  END;

  PROCEDURE PUSH_OLD_NAME;
  VAR SPIX:{SPELLING_INDEX} INTEGER;
  BEGIN
    PUSH; GET_L(outfile2, SPIX);
     WITH OPS(.T.),SPELLING_TABLE(.SPIX.) DO
      IF ACCESS IN INACCESSIBLE THEN
      BEGIN
        ERROR(NAME_ERROR);
        CLASS_:=UNDEF_CLASS
      END ELSE BEGIN
        CLASS_:=DEF_CLASS;
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
      NEXT_NAME:=NIL;
      IF NAME_HEAD=NIL THEN BEGIN NAME_HEAD:=N; NAME_TAIL:=N END
      ELSE BEGIN NAME_TAIL^.NEXT_NAME:=N; NAME_TAIL:=N END
    END
  END;

  PROCEDURE SET_ACCESS(SPIX:SPELLING_INDEX; A:NAME_ACCESS);
  BEGIN
    SPELLING_TABLE(.SPIX.).ACCESS:=A;
    T:=T-1
  END;

  PROCEDURE ENTER_NAMES(LIST:NAME_PTR; ACCESS:NAME_ACCESS);
  VAR THIS_NAME:NAME_PTR;
  BEGIN
    THIS_NAME:=LIST;
    WHILE THIS_NAME<>NIL DO
      WITH THIS_NAME^ DO BEGIN
        UPDATE(NAME_SPIX,NAME_ENTRY,ACCESS);
        THIS_NAME:=NEXT_NAME
      END
  END;

  FUNCTION DEFINED:BOOLEAN;
  BEGIN
    DEFINED:=OPS(.T.).CLASS_<>UNDEF_CLASS
  END;

  FUNCTION TOP:ENTRY_PTR;
  BEGIN
    TOP:=OPS(.T.).DEF_ENTRY
  END;

  PROCEDURE DEFINE (VAR E: ENTRY_PTR);
  BEGIN
    WITH OPS(.T.) DO
      IF CLASS_ = DEF_CLASS THEN
       E:= DEF_ENTRY
      ELSE E:= UENTRY
  END;

{"#####################}
{CONSTANT DECLARATIONS}
{#####################}
 PROCEDURE CONST_ID;
  BEGIN
    PUSH_NEW_NAME(NOT_POSSIBLY_FORWARD,RETAIN,INCOMPLETE);
    IF DEFINED THEN THIS_NOUN:=THIS_NOUN-1{"CONST IDS DON'T HAVE NOUNS}
  END;
  PROCEDURE CONST_DEF;
  BEGIN
    WITH OPS(.T-1.) DO
      IF CLASS_=DEF_CLASS THEN BEGIN
        WITH DEF_ENTRY^, OPS(.T.) DO
          IF CLASS_ IN CONSTANTS THEN
            CASE CLASS_ OF
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
  VAR SPIX:{SPELLING_INDEX} integer; ERROR_SW:BOOLEAN;
  BEGIN
   GET_L(outfile2, spix);
  ERROR_SW:=FALSE;
    IF SPIX<>XUNDEF THEN
      WITH SPELLING_TABLE(.SPIX.) DO
        CASE ACCESS OF
          GENERAL:
            IF LEVEL=THIS_LEVEL THEN
             ERROR_SW:=TRUE
            ELSE UPDATE(SPIX,NIL,INCOMPLETE);
          UNDEFINED:
            UPDATE(SPIX,NIL,INCOMPLETE);
          UNRES_TYPE:
            IF LEVEL<>THIS_LEVEL THEN
             ERROR_SW:=TRUE
            ELSE UNRESOLVED:=UNRESOLVED-1;
          UNRES_ROUTINE:
            ERROR_SW:=TRUE
        END
    ELSE ERROR_SW:=TRUE;
    IF ERROR_SW THEN
     ERROR(NAME_ERROR);
    PUSH;
    WITH OPS(.T.) DO
      IF ERROR_SW THEN CLASS_:=UNDEF_CLASS
      ELSE BEGIN
        CLASS_:=DEF_CLASS; DEF_SPIX:=SPIX
      END
  END;

  PROCEDURE TYPE_DEF;
  VAR TYP,FWD_REF:ENTRY_PTR;
  BEGIN
    WITH OPS(.T-1.) DO
      IF CLASS_=DEF_CLASS THEN
        WITH SPELLING_TABLE(.DEF_SPIX.) DO BEGIN
          DEFINE(TYP);
          IF ACCESS=UNRES_TYPE THEN
          BEGIN {RESOLVE}
            FWD_REF:=ENTRY;
            REPEAT
              WITH FWD_REF^ DO BEGIN
                OBJECT_TYPE:=TYP;
                FWD_REF:=NEXT_FWD
              END
            UNTIL FWD_REF=NIL
          END;
          ENTRY:=TYP;
          ACCESS:=GENERAL
        END;
    T:=T-2; PUT0(TYPE_DEF2)
  END;

  PROCEDURE TYPE_(OUTPUT:BOOLEAN; OP:INTEGER);
  VAR TYP: ENTRY_PTR;
  BEGIN
    PUSH_OLD_NAME;
    IF DEFINED THEN
      IF NOT(TOP^.KIND IN TYPES) THEN
      BEGIN
        ERROR(NAME_ERROR);
        OPS(.T.).CLASS_:=UNDEF_CLASS
      END;
    IF OUTPUT THEN BEGIN
      DEFINE(TYP);
      PUT1(OP, TYP^.NOUN)
    END
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
      RANGE_TYPE:=NOUN;
      ENUM_TYPE:=NOUN
    END
  END;

  PROCEDURE SUBR_DEF;
  VAR MIN,MAX:INTEGER; TYPE1:NOUN_INDEX; E:ENTRY_PTR;
  BEGIN
    MIN:=0; MAX:=1; TYPE1:=XUNDEF;
    WITH OPS(.T.) DO
      IF CLASS_=ICONST_CLASS THEN
       BEGIN
        MAX:=ICONST_VAL; TYPE1:=ICONST_TYPE
      END
      ELSE ERROR(SUBR_ERROR);
    WITH OPS(.T-1.) DO
      IF CLASS_=ICONST_CLASS THEN
      BEGIN
        MIN:=ICONST_VAL;
        IF (MIN>MAX) OR (ICONST_TYPE<>TYPE1) THEN
         ERROR(SUBR_ERROR)
      END ELSE
       ERROR(SUBR_ERROR);
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
    DEFINE(EL);
    T:=T-1;
    IF DEFINED THEN INDEX:=TOP^.NOUN
    ELSE INDEX:=XUNDEF;
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

  PROCEDURE FIELD_DEF(NUMBER:INTEGER; VAR TYP:ENTRY_PTR);
  VAR I:INTEGER;
  BEGIN
    IF DEFINED THEN TYP:=TOP
    ELSE TYP:=UENTRY;
    T:=T-1;
    FOR I:=1 TO NUMBER DO
      IF DEFINED THEN
        WITH OPS(.T.) DO BEGIN
          WITH DEF_ENTRY^ DO
          BEGIN
            KIND:=FIELD;
            FIELD_TYPE:=TYP;
            VARIANT:=THIS_VARIANT
          END;
          CHAIN_NAME(DEF_ENTRY,DEF_SPIX);
          SET_ACCESS(DEF_SPIX,GENERAL)
        END ELSE T:=T-1;
  END;

  PROCEDURE FIELD_LIST;
  VAR NUMBER:INTEGER; TYP:ENTRY_PTR;
  BEGIN
     GET_L(outfile2, number);
    FIELD_DEF(NUMBER,TYP);
    PUT1(FIELDLIST2,NUMBER)
  END;

  PROCEDURE TAG_DEF;
  VAR TYP:ENTRY_PTR;

  BEGIN
    FIELD_DEF(1,TYP);
    IF TAG_TOP>TAG_STACK_MAX
    THEN ABORT
    ELSE WITH TAG_STACK(.TAG_TOP.) DO
     BEGIN
      PREV_LABELS:=TAG_LABELS;
      TAG_LABELS:=(..);
      PREV_TAG:=TAG_FIELD;
      TAG_FIELD:=NEW_TAG_FIELD;
      PREV_TYPE:=LABEL_TYPE;
      WITH TYP^ DO
        IF KIND=SCALAR_KIND THEN
        LABEL_TYPE:=RANGE_TYPE
        ELSE LABEL_TYPE:=XUNDEF
    END;
    TAG_TOP:=TAG_TOP+1
  END;


  PROCEDURE VARNT;
  VAR
   VARNT_PTR:VARIANT_PTR;
  BEGIN
    VARIANT_LABELS:=(..);
    NEW(VARNT_PTR);
    WITH VARNT_PTR^ DO
    BEGIN
      TAG_NOUN:=TAG_FIELD;
      PARENT_VARIANT:=THIS_VARIANT;
      THIS_VARIANT:=VARNT_PTR
    END
  END;

  PROCEDURE TAG_ID;
  BEGIN
    PUSH_NEW_NAME(NOT_POSSIBLY_FORWARD,OUTPUT,INCOMPLETE);
    IF DEFINED THEN
     NEW_TAG_FIELD:=OPS(.T.).DEF_ENTRY^.NOUN
    ELSE NEW_TAG_FIELD:=XUNDEF
  END;

  PROCEDURE LBL_END;

  BEGIN
    IF (VARIANT_LABELS * TAG_LABELS <> [] )
     THEN ERROR(AMBILBL_ERROR);
    TAG_LABELS:=TAG_LABELS - VARIANT_LABELS;
    WITH THIS_VARIANT^ DO
      PACK(VARIANT_LABELS,LABEL_SET);
  END;

  PROCEDURE VARNT_END;

  BEGIN
    THIS_VARIANT:=THIS_VARIANT^.PARENT_VARIANT;
    PUT0(VARNT_END2)
  END;

  PROCEDURE PART_END;
  BEGIN
    PUT0(PART_END2);

    TAG_TOP:=TAG_TOP-1;
    IF TAG_TOP<=TAG_STACK_MAX THEN
      WITH TAG_STACK(.TAG_TOP.) DO
       BEGIN
        TAG_LABELS:=PREV_LABELS;
        TAG_FIELD:=PREV_TAG;
        LABEL_TYPE:=PREV_TYPE
      END
  END;

  PROCEDURE LABEL_;
   BEGIN
    IF DEFINED THEN WITH OPS(.T.) DO
      IF CLASS_=ICONST_CLASS THEN
      BEGIN
        IF (ICONST_VAL<MIN_TAG) OR (ICONST_VAL>MAX_TAG)
          THEN ERROR(LBLRANGE_ERROR)
        ELSE VARIANT_LABELS:=VARIANT_LABELS + [ICONST_VAL];
        IF ICONST_TYPE<>LABEL_TYPE
         THEN ERROR(LBLTYPE_ERROR)
      END ELSE
      ERROR(LBLTYPE_ERROR);
    T:=T-1
  END;

  PROCEDURE REC_DEF;
  VAR E:ENTRY_PTR;
  BEGIN
    WITH TOP^ DO BEGIN
      KIND:=RECORD_KIND;
      FIELD_NAME:=NAME_HEAD;
      PUT1(REC_DEF2,NOUN)
    END;
    POP_LEVEL
  END;

  PROCEDURE POINTER_;
  VAR SPIX:{SPELLING_INDEX} integer; OBJ_TYP,PTR_TYP,FWD_REF:ENTRY_PTR;
  BEGIN
    GET_L(outfile2, spix);
    OBJ_TYP:=UENTRY; PUSH_NEW_ENTRY(PTR_TYP);
    IF SPIX<>XUNDEF THEN
      WITH SPELLING_TABLE(.SPIX.) DO
        CASE ACCESS OF
          GENERAL:
            IF ENTRY^.KIND IN TYPES THEN OBJ_TYP:=ENTRY
            ELSE ERROR(NAME_ERROR);
          UNDEFINED:
            BEGIN
              UPDATE(SPIX,PTR_TYP,UNRES_TYPE);
              UNRESOLVED:=UNRESOLVED+1
            END;
          INCOMPLETE,UNRES_ROUTINE:
            ERROR(NAME_ERROR);
          UNRES_TYPE:
            IF LEVEL=THIS_LEVEL THEN BEGIN
              FWD_REF:=ENTRY;
              WHILE FWD_REF^.NEXT_FWD<>NIL DO
                FWD_REF:=FWD_REF^.NEXT_FWD;
              FWD_REF^.NEXT_FWD:=PTR_TYP
            END ELSE ERROR(NAME_ERROR)
        END;
    WITH PTR_TYP^ DO BEGIN
      KIND:=POINTER_KIND;
      OBJECT_TYPE:=OBJ_TYP;
      NEXT_FWD:=NIL;
      PUT1(POINTER2,NOUN)
    END
  END;

{#####################}
{VARIABLE DECLARATIONS}
{#####################}
  PROCEDURE VAR_LIST;
  VAR I,NUMBER:INTEGER; TYP:ENTRY_PTR;
  BEGIN
    GET_L(outfile2, number);
    PUT1(VAR_LIST2,NUMBER);
    DEFINE(TYP);
    T:=T-1;
    FOR I:=1 TO NUMBER DO
      WITH OPS(.T.) DO
       IF DEFINED THEN BEGIN
        WITH DEF_ENTRY^ DO BEGIN
          KIND:=VARIABLE;
          VAR_TYPE:=TYP
        END;
        SET_ACCESS(DEF_SPIX,GENERAL)
       END ELSE T:=T-1
  END;

{###################}
{ROUTINE DECLARATIONS}
{###################}
 PROCEDURE ROUTINE_ID(ACCESS:NAME_ACCESS; MODE:INTEGER);
  BEGIN
    PUSH_NEW_NAME(POSSIBLY_FORWARD,RETAIN,ACCESS);
    PUT1(MODE2,MODE);
    PUSH_LEVEL(UENTRY);
  END;

  PROCEDURE PROC_DEF(OP:INTEGER);
  BEGIN
    RESET_NOUN:=THIS_NOUN;
    IF DEFINED THEN
      WITH TOP^ DO
        IF RESOLUTION THEN BEGIN
          RESOLUTION:=FALSE; PUT1(PROCF_DEF2,NOUN);
          ENTER_NAMES(ROUT_PARM,GENERAL)
        END ELSE BEGIN
          KIND:=ROUTINE_KIND; ROUT_PARM:=NAME_HEAD;
          ROUT_TYPE:=PROC_TYPE; PUT1(OP,NOUN)
        END
      ELSE PUT1(OP,XUNDEF);
    IF PREFIX_SW THEN BEGIN POP_LEVEL; T:=T-1 END
  END;

  PROCEDURE FUNC_TYPE;
  BEGIN
    TYPE_(RETAIN,0);
    FUNC_TYPE_SW:=TRUE
  END;

  PROCEDURE FUNC_DEF;
  VAR TYP: ENTRY_PTR;
  BEGIN
    RESET_NOUN:=THIS_NOUN;
    IF FUNC_TYPE_SW THEN BEGIN
      DEFINE(TYP);
      T:=T-1
    END ELSE TYP:= UENTRY;
    IF DEFINED THEN BEGIN
      THIS_FUNCTION:=TOP;
      WITH THIS_FUNCTION^ DO
        IF RESOLUTION THEN BEGIN
          IF FUNC_TYPE_SW THEN ERROR(RESOLVE_ERROR);
          RESOLUTION:=FALSE; PUT1(FUNCF_DEF2,NOUN);
          ENTER_NAMES(ROUT_PARM,GENERAL)
        END ELSE BEGIN
          KIND:=ROUTINE_KIND; ROUT_PARM:=NAME_HEAD;
          ROUT_TYPE:= TYP;  PUT2(FUNC_DEF2, TYP^.NOUN, NOUN)
        END
    END ELSE PUT2(FUNC_DEF2,XUNDEF,XUNDEF);
    FUNC_TYPE_SW:=FALSE;
    IF PREFIX_SW THEN BEGIN POP_LEVEL; T:=T-1 END
  END;

  PROCEDURE PARMLIST(OP:INTEGER);
  VAR I,NUMBER:INTEGER; PTYPE:ENTRY_PTR;
  BEGIN
    DEFINE(PTYPE);
    GET_L(outfile2, number);
    PUT1(OP,NUMBER);
    FOR I:=NUMBER DOWNTO 1 DO
      WITH OPS(.T-I.) DO
       IF CLASS_=DEF_CLASS THEN BEGIN
        WITH DEF_ENTRY^ DO BEGIN
          KIND:=PARAMETER;
          PARM_TYPE:=PTYPE;
        END;
        CHAIN_NAME(DEF_ENTRY,DEF_SPIX);
        SPELLING_TABLE(.DEF_SPIX.).ACCESS:=GENERAL
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

  PROCEDURE BODY_END;
  BEGIN
    THIS_NOUN:=RESET_NOUN;
    THIS_FUNCTION:=NIL;
    T:=T-1; POP_LEVEL;
    PUT0(BODY_END2)
  END;

  PROCEDURE FORWARD_;
  BEGIN
    PUT0(FORWARD2);
    IF DEFINED THEN BEGIN
      SET_ACCESS(OPS(.T.).DEF_SPIX,UNRES_ROUTINE);
      UNRESOLVED:=UNRESOLVED+1
    END ELSE T:=T-1;
    POP_LEVEL
  END;

  PROCEDURE ANAME;
  BEGIN
    WITH OPS(.T.) DO
      IF CLASS_=ROUTINE_CLASS THEN
        IF ROUT = THIS_FUNCTION THEN
          PUT1(RESULT2, THIS_FUNCTION^.ROUT_TYPE^.NOUN)
        ELSE PUT0(ADDRESS2)
      ELSE PUT0(ADDRESS2)
  END;

  PROCEDURE CALL_NAME;
  VAR ERR:BOOLEAN;
  BEGIN
    ERR:=FALSE;
    WITH OPS(.T.) DO BEGIN
      IF CLASS_=ROUTINE_CLASS THEN
        IF ROUT^.ROUT_TYPE<>PROC_TYPE THEN ERR:=TRUE ELSE {OK}
      ELSE ERR:=TRUE;
      IF ERR THEN BEGIN
        ERROR(CALL_NAME_ERROR);
        CLASS_:=UNDEF_CLASS
      END
    END
  END;

  PROCEDURE CALL(OP:INTEGER);
  BEGIN
    WITH OPS(.T.) DO
      IF CLASS_=ROUTINE_CLASS THEN BEGIN
        IF PARM<>NIL THEN ERROR(FEW_ARGS_ERROR);
        WITH ROUT^ DO
          IF OP = CALL_FUNC2 THEN BEGIN
            PUT0(CALL_FUNC2);
            CLASS_:= VAR_CLASS;  VTYPE:= ROUT_TYPE
          END
          ELSE IF NOUN=XNEW THEN PUT1(CALL_NEW2,NEW_TYPE)
          ELSE PUT0(OP)
      END ELSE PUT0(OP);
    IF OP<>CALL_FUNC2 THEN T:=T-1
  END;

  PROCEDURE ARG_LIST;
  BEGIN
    WITH OPS(.T.) DO
      IF CLASS_<>ROUTINE_CLASS THEN BEGIN
        ERROR(ARG_LIST_ERROR);
        CLASS_:=UNDEF_CLASS
      END
  END;

  PROCEDURE ARG;
  VAR THIS_PARM:ENTRY_PTR; ERR:ERROR_NOTE;
  BEGIN
    ERR:=NO;
    WITH OPS(.T-1.) DO
      IF CLASS_=ROUTINE_CLASS THEN BEGIN
        IF PARM=NIL THEN ERR:=YES ELSE
          WITH PARM^ DO BEGIN
            THIS_PARM:=NAME_ENTRY;
            PARM:=NEXT_NAME
          END
      END ELSE ERR:=SUPPRESS;
    IF ERR<>NO THEN BEGIN
      IF ERR=YES THEN ERROR(MANY_ARGS_ERROR);
      PUT2(PARM2,XUNDEF,XUNDEF)
    END ELSE
      WITH THIS_PARM^ DO BEGIN
        PUT2(PARM2,NOUN,PARM_TYPE^.NOUN);
        IF NOUN=ZNPARM THEN
          WITH OPS(.T.) DO
            IF CLASS_=VAR_CLASS THEN
              WITH VTYPE^ DO
                IF KIND=POINTER_KIND THEN NEW_TYPE:=OBJECT_TYPE^.NOUN
      END;
    T:=T-1 {POP ARGUMENT}
  END;

  PROCEDURE DEF_CASE;
  BEGIN
    get_l(outfile2,THIS_LABEL);
    PUT1(DEF_LABEL2,THIS_LABEL)
  END;

  PROCEDURE CASE_;
  VAR VAL:INTEGER;
  BEGIN
    WITH OPS(.T.) DO
      IF CLASS_=ICONST_CLASS THEN BEGIN
        PUT1(CHK_TYPE2,ICONST_TYPE);
        VAL:=ICONST_VAL;
        CLASS_:=CASE_LABEL;
        LABEL1:=THIS_LABEL;
        IF (VAL>=MIN_CASE) AND (VAL<=MAX_CASE) THEN
          INDEX:=VAL ELSE BEGIN
            ERROR(LBLRANGE_ERROR);
            T:=T-1
          END
      END ELSE BEGIN
        T:=T-1;
        ERROR(LBLTYPE_ERROR)
      END
  END;

  PROCEDURE END_CASE;
  VAR L0,LN,MIN,MAX,I:INTEGER;
  BEGIN
    get_l(outfile2,L0);
    get_l(outfile2,LN);
    FOR I:=MIN_CASE TO MAX_CASE DO LABELS(.I.):=LN;
    IF OPS(.T.).CLASS_=CASE_LABEL THEN BEGIN
     MIN:=OPS(.T.).INDEX; MAX:=MIN;
    END ELSE BEGIN MIN:=0; MAX:=0 END;
    WHILE OPS(.T.).CLASS_=CASE_LABEL DO BEGIN
        WITH OPS(.T.) DO BEGIN
          IF LABELS(.INDEX.)=LN THEN
            LABELS(.INDEX.):=LABEL1
          ELSE ERROR(AMBILBL_ERROR);
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
      IF CLASS_=VAR_CLASS THEN
        WITH VTYPE^ DO
          IF KIND=RECORD_KIND THEN BEGIN
            NEW_ENTRY(TEMP);
            WITH TEMP^ DO BEGIN
              PUT1(WITH_TEMP2,NOUN);
              KIND:=WITH_KIND;
              WITH_TYPE:=VTYPE^.NOUN
            END;
            PUSH_LEVEL(TEMP);
            ENTER_NAMES(FIELD_NAME,QUALIFIED)
          END ELSE ERR:=TRUE
      ELSE ERR:=TRUE;
    IF ERR THEN BEGIN
      ERROR(WITH_ERROR);
      PUSH_LEVEL(UENTRY); PUT1(WITH_TEMP2,XUNDEF)
    END;
    T:=T-1
  END;

{##########}
{"EXPRESSION}
{##########}

 PROCEDURE FNAME;
  VAR TYP: ENTRY_PTR;
  BEGIN
    WITH OPS(.T.) DO
      IF CLASS_=ROUTINE_CLASS THEN
        WITH ROUT^ DO BEGIN
          IF ROUT_TYPE=PROC_TYPE THEN BEGIN
            ERROR(PROC_USE_ERROR);
            TYP:= UENTRY
          END ELSE TYP:=ROUT_TYPE;
          PUT1(FUNCTION2, TYP^.NOUN);
          IF PARM<>NIL THEN ERROR(FEW_ARGS_ERROR);
          PUT0(CALL_FUNC2);
          CLASS_:= VAR_CLASS;  VTYPE:= TYP
        END
  END;

  PROCEDURE FUNCTION_ERROR(ERROR_NUM:INTEGER);
  BEGIN
    ERROR(ERROR_NUM);
    OPS(.T.).CLASS_:=UNDEF_CLASS
  END;

  PROCEDURE FUNCTION_;
  VAR FUNC_TYPE: NOUN_INDEX;
  BEGIN
    FUNC_TYPE:= XUNDEF;
    WITH OPS(.T.) DO
      IF CLASS_ = ROUTINE_CLASS THEN
        WITH ROUT^ DO
          IF ROUT_TYPE = PROC_TYPE THEN
            FUNCTION_ERROR(PROC_USE_ERROR)
          ELSE FUNC_TYPE:= ROUT_TYPE^.NOUN
      ELSE FUNCTION_ERROR(NAME_ERROR);
    PUT1(FUNCTION2, FUNC_TYPE)
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

 PROCEDURE PUSH_OPERAND(OP_ENTRY:ENTRY_PTR; COMP:BOOLEAN);
  VAR OP:INTEGER; VARNT_PTR:VARIANT_PTR;
  BEGIN
    NEW(varnt_ptr);
    IF NOT COMP THEN PUSH;
    WITH OPS(.T.) , OP_ENTRY^ DO
      CASE KIND OF
        INDEX_CONST: BEGIN
          CLASS_:=FCONST_CLASS;
          PUT2(INDEX2,CONST_VAL,CONST_TYPE)
        END;
        REAL_CONST: BEGIN
          CLASS_:=FCONST_CLASS;
          PUT1(REAL2,REAL_DISP)
        END;
        STRING_CONST: BEGIN
          CLASS_:=FCONST_CLASS;
          PUT2(STRING2,STRING_LENGTH,STRING_DISP)
        END;
        VARIABLE,FIELD,PARAMETER: BEGIN
          CLASS_:=VAR_CLASS;
          CASE KIND OF
            VARIABLE:VTYPE:=VAR_TYPE;
            FIELD: VTYPE:=FIELD_TYPE;
            PARAMETER: VTYPE:=PARM_TYPE
          END;
          IF COMP THEN BEGIN
            OP:=VCOMP2;
            VARNT_PTR:=VARIANT;
            WHILE VARNT_PTR<>NIL DO
              WITH VARNT_PTR^ DO BEGIN
                PUT2(VARIANT2,LABEL_SET,TAG_NOUN);
                VARNT_PTR:=PARENT_VARIANT
              END
          END ELSE OP:=VAR2;
          PUT2(OP,NOUN,VTYPE^.NOUN)
        END;
        ROUTINE_KIND: BEGIN
          CLASS_:=ROUTINE_CLASS;
          ROUT:=OP_ENTRY;
          PARM:=ROUT_PARM;
          PUT1(ROUTINE2,NOUN)
        END;
        SCALAR_KIND,POINTER_KIND,ARRAY_KIND,RECORD_KIND,SET_KIND,
        UNDEF_KIND: BEGIN
          ERROR(NAME_ERROR);
          CLASS_:=UNDEF_CLASS;
          IF NOT COMP THEN PUT0(UNDEF2)
        END
      END
  END;

  PROCEDURE NAME;
  VAR SPIX:{SPELLING_INDEX} integer; COMP,ERR:BOOLEAN; NAME_ENTRY:ENTRY_PTR;
  BEGIN
    get_l(outfile2,Spix);
    ERR:=FALSE; COMP:=FALSE;
    WITH SPELLING_TABLE(.SPIX.) DO
      IF ACCESS IN OP_ACCESS THEN BEGIN
        NAME_ENTRY:=ENTRY;
        CASE ACCESS OF
          GENERAL,UNRES_ROUTINE: ;
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
    PUSH_OPERAND(NAME_ENTRY,COMP)
  END;

  PROCEDURE COMP;
  CONST QUALIFIED=TRUE;
  VAR SPIX:{SPELLING_INDEX} integer; COMPONENT:ENTRY_PTR; NAME_LIST:NAME_PTR;
    ERR:BOOLEAN;
  BEGIN
    get_l(outfile2, SPIX);
    ERR:=FALSE;
    WITH OPS(.T.) DO
      IF CLASS_=VAR_CLASS THEN BEGIN
        WITH VTYPE^ DO
          IF KIND=RECORD_KIND THEN NAME_LIST:=FIELD_NAME
          ELSE BEGIN ERR:=TRUE; NAME_LIST:=NIL END;
        FIND_NAME(NAME_LIST,SPIX,COMPONENT)
      END ELSE ERR:=TRUE;
    IF ERR THEN ERROR(COMP_ERROR)
    ELSE PUSH_OPERAND(COMPONENT,QUALIFIED)
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
      IF CLASS_=VAR_CLASS THEN
        WITH VTYPE^ DO
          IF KIND=ARRAY_KIND THEN BEGIN
            PUT2(SUB2,INDEX_TYPE,EL_TYPE^.NOUN);
            VTYPE:=EL_TYPE
          END ELSE SUB_ERR
      ELSE SUB_ERR
  END;

  PROCEDURE ARROW_ERR;
  BEGIN
    ERROR(ARROW_ERROR);
    PUT1(ARROW2,XUNDEF)
  END;
  PROCEDURE ARROW;
  BEGIN
    FNAME {CALL PARAMETERLESS POINTER-VALUED FUNCTION, IF ANY} ;
    WITH OPS(.T.) DO
      IF CLASS_=VAR_CLASS THEN
        WITH VTYPE^ DO
          IF KIND=POINTER_KIND THEN BEGIN
            VTYPE:=OBJECT_TYPE;
            PUT1(ARROW2,VTYPE^.NOUN)
          END ELSE ARROW_ERR
      ELSE ARROW_ERR
  END;

{########}
{CONSTANT}
{########}

PROCEDURE CONSTANT;
  BEGIN
    PUSH_OLD_NAME;
    IF DEFINED THEN
      WITH OPS(.T.), DEF_ENTRY^ DO
          IF KIND IN CONST_KINDS THEN
            CASE KIND OF
              INDEX_CONST: BEGIN
                CLASS_:=ICONST_CLASS;
                ICONST_TYPE:=CONST_TYPE;
                ICONST_VAL:=CONST_VAL
              END;
              REAL_CONST: BEGIN
                CLASS_:=RCONST_CLASS; RCONST_DISP:=REAL_DISP
              END;
              STRING_CONST:BEGIN
                CLASS_:=SCONST_CLASS;
                SCONST_LENGTH:=STRING_LENGTH;
                SCONST_DISP:=STRING_DISP
              END
            END
          ELSE BEGIN CLASS_:=UNDEF_CLASS; ERROR(CONSTID_ERROR) END
  END;

  PROCEDURE REAL_;
  BEGIN
    PUSH;
    WITH OPS(.T.) DO BEGIN
      CLASS_:=RCONST_CLASS; RCONST_DISP:=CONST_DISP
    END
  END;

  PROCEDURE FREAL;
  BEGIN
    PUSH; OPS(.T.).CLASS_:=FCONST_CLASS;
    PUT1(REAL2,CONST_DISP)
  END;

  PROCEDURE INDEX(TYP:NOUN_INDEX);
  BEGIN
    PUSH;
    WITH OPS(.T.) DO BEGIN
      CLASS_:=ICONST_CLASS;
      ICONST_TYPE:=TYP;
      get_l(outfile2, ICONST_VAL);
    END
  END;

  PROCEDURE FINDEX(TYP:NOUN_INDEX);
  VAR VALUE:INTEGER;
  BEGIN
    PUSH; OPS(.T.).CLASS_:=FCONST_CLASS;
    get_l(outfile2,VALUE);
    PUT2(INDEX2,VALUE,TYP)
  END;

  PROCEDURE STRING_;
  BEGIN
    PUSH;
    WITH OPS(.T.) DO BEGIN
      CLASS_:=SCONST_CLASS;
     get_l(outfile2,SCONST_LENGTH);
     SCONST_DISP:=CONST_DISP
    END
  END;

  PROCEDURE FSTRING;
  VAR LENGTH:INTEGER;
  BEGIN
    PUSH; OPS(.T.).CLASS_:=FCONST_CLASS;
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
 ARROW1: ARROW;
 BODY_END1: BODY_END;
 BODY1: BODY;
 CALL_FUNC1: CALL(CALL_FUNC2);
 CALL_NAME1: CALL_NAME;
 CALL1: CALL(CALL_PROC2);
 CASE1: CASE_;
 CASE_JUMP1: IGNORE1(CASE_JUMP2);
 CHAR1: INDEX(XCHAR);
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
 FORWARD1: FORWARD_;
 FREAL1: FREAL;
 FSTRING1: FSTRING;
 FUNC_DEF1: FUNC_DEF;
 FUNC_ID1: ROUTINE_ID(GENERAL,FUNC_MODE);
 FUNC_TYPE1: FUNC_TYPE;
 FUNCTION1: FUNCTION_;
 GE1: BINARY(GE2);
 GT1: BINARY(GT2);
 INCLUDE1: BINARY(INCLUDE2);
 INTEGER1: INDEX(XINTEGER);
 IN1: BINARY(IN2);
 JUMP_DEF1: IGNORE2(JUMP_DEF2);
 JUMP1: IGNORE1(JUMP2);
 LABEL1: LABEL_;
 LBL_END1: LBL_END;
 LCONST1: LCONST;
 LE1: BINARY(LE2);
 LT1: BINARY(LT2);
 MESSAGE1: IGNORE2(MESSAGE2);
 MINUS1: BINARY(MINUS2);
 MOD1: BINARY(MOD2);
 NAME1: NAME;
 NEW_LINE1: IGNORE1(NEW_LINE2);
 NE1: BINARY(NE2);
 NOT1: PUT0(NOT2);
 OR1: BINARY(OR2);
 PARM_TYPE1: TYPE_(OUTPUT,PARM_TYPE2);
 PART_END1: PART_END;
 PLUS1: BINARY(PLUS2);
 POINTER1: POINTER_;
 PROC_DEF1: PROC_DEF(PROC_DEF2);
 PROC_ID1: ROUTINE_ID(GENERAL,PROC_MODE);
 PROG_DEF1: PROC_DEF(PROG_DEF2);
 PROG_ID1: BEGIN PREFIX_SW:= FALSE; ROUTINE_ID(INCOMPLETE, PROGRAM_MODE) END;
 REAL1: REAL_;
 REC_DEF1: REC_DEF;
 REC1: REC;
 SET_DEF1: SET_DEF;
 SLASH1: BINARY(SLASH2);
 STAR1: BINARY(STAR2);
 STORE1: POP2(STORE2);
 STRING1: STRING_;
 SUBR_DEF1: SUBR_DEF;
 SUB1: SUB;
 TAG_DEF1: TAG_DEF;
 TAG_ID1: TAG_ID;
 TAG_TYPE1: TYPE_(OUTPUT,TAG_DEF2);
 TYPE_DEF1: TYPE_DEF;
 TYPE_ID1: TYPE_ID;
 TYPE1: TYPE_(OUTPUT,TYPE2);
 UMINUS1: PUT0(UMINUS2);
 UNIV_TYPE1: TYPE_(OUTPUT,UNIV_TYPE2);
 UPLUS1: PUT0(UPLUS2);
 VALUE1: PUT0(VALUE2);
 VAR_LIST1: VAR_LIST;
 VARNT_END1: VARNT_END;
 VARNT1: VARNT;
 VPARMLIST1: PARMLIST(VPARMLIST2);
 WITH_TEMP1: WITH_TEMP;
 WITH_VAR1: PUT0(WITH_VAR2);
 WITH1: BEGIN POP_LEVEL; PUT0(WITH2) END
   END
  UNTIL HALT;
  IF UNRESOLVED > 0 THEN ERROR(UNRES_ERROR);
  PUT0(EOM2);
  INTER_PASS_PTR^.CONSTANTS:=CONST_DISP;
end;

procedure Pass4(var OK:boolean);

CONST

IDLENGTH = 12;

MAXARG = 10;

PRINTLIMIT = 18;   MAXDIGIT = 6;

{INPUT OPERATORS}
EOM1=1;            PROG_DEF1=2;        TYPE_DEF1=3;        TYPE1=4;
ENUM_DEF1=5;       SUBR_DEF1=6;        SET_DEF1=7;         ARRAY_DEF1=8;
POINTER1=9;        REC1=10;            REC_DEF1=11;        NEW_NOUN1=12;
FIELDLIST1=13;     TAG_DEF1=14;        PART_END1=15;       CASE_JUMP1=16;
VARNT_END1=17;     VAR_LIST1=18;       FORWARD1=19;        PROC_DEF1=20;
PROCF_DEF1=21;     LCONST1=22;         FUNC_DEF1=23;       FUNCF_DEF1=24;
PARM_TYPE1=25;     UNIV_TYPE1=26;      CPARMLIST1=27;      VPARMLIST1=28;
BODY1=29;          BODY_END1=30;       ADDRESS1=31;        RESULT1=32;
STORE1=33;         CALL_PROC1=34;      PARM1=35;           FALSEJUMP1=36;
DEF_LABEL1=37;     JUMP_DEF1=38;       JUMP1=39;           CHK_TYPE1=40;
CASE_LIST1=41;     FOR_STORE1=42;      FOR_LIM1=43;        FOR_UP1=44;
FOR_DOWN1=45;      WITH_VAR1=46;       WITH_TEMP1=47;      WITH1=48;
VALUE1=49;         LT1=50;             EQ1=51;             GT1=52;
LE1=53;            NE1=54;             GE1=55;             IN1=56;
UPLUS1=57;         UMINUS1=58;         PLUS1=59;           MINUS1=60;
OR1=61;            STAR1=62;           SLASH1=63;          DIV1=64;
MOD1=65;           AND1=66;            NOT1=67;            EMPTY_SET1=68;
INCLUDE1=69;       FUNCTION1=70;       CALL_FUNC1=71;      ROUTINE1=72;
VAR1=73;           ARROW1=74;          VCOMP1=75;          SUB1=76;
INDEX1=77;         REAL1=78;           STRING1=79;         NEW_LINE1=80;
MESSAGE1=81;       CALL_NEW1=82;       UNDEF1=83;          VARIANT1=84;
MODE1=85;

{OUTPUT OPERATORS}
EOM2=1;            BODY2=2;            BODY_END2=3;        ADDRESS2=4;
RESULT2=5;         TAG_STORE2=6;       STORE2=7;           CALL_PROC2=8;
CALL_NEW2=9;       CONSTPARM2=10;      VARPARM2=11;        SAVEPARM2=12;
FALSEJUMP2=13;     JUMP2=14;           JUMP_DEF2=15;       DEF_LABEL2=16;
CHK_TYPE2=17;      CASE_LIST2=18;      FOR_STORE2=19;      FOR_LIM2=20;
FOR_UP2=21;        FOR_DOWN2=22;       WITH2=23;           VALUE2=24;
LT2=25;            EQ2=26;             GT2=27;             LE2=28;
NE2=29;            GE2=30;             IN2=31;             UPLUS2=32;
UMINUS2=33;        PLUS2=34;           MINUS2=35;          OR2=36;
STAR2=37;          SLASH2=38;          DIV2=39;            MOD2=40;
AND2=41;           NOT2=42;            EMPTY_SET2=43;      INCLUDE2=44;
FUNCTION2=45;      CALL_FUNC2=46;      CALL_GEN2=47;       ROUTINE2=48;
VAR2=49;           ARROW2=50;          VCOMP2=51;          VARIANT2=52;
SUB2=53;           NEW_LINE2=54;       MESSAGE2=55;        LCONST2=56;
INITVAR2=57;       UNDEF2=58;          RANGE2=59;          CASE_JUMP2=60;

{STANDARD NOUN INDICES}
XUNDEF=0;          XFALSE=1;           XTRUE=2;            XINTEGER=3;
XBOOLEAN=4;        XCHAR=5;            XNIL=6;             XABS=7;
XATTRIBUTE=8;      XCHR=9;             XCONV=10;           XORD=11;
XPRED=12;          XSUCC=13;           XTRUNC=14;          XNEW=15;
XREAL=16;

{ERRORS}
ZARITHMETIC=17;    ZINDEX=18;          ZPASSIVE=19;        ZPOINTER=20;
ZVPARM=21;         ZCPARM=22;          ZSPARM=23;          ZNPARM=24;
ZWITH=25;

{CONTEXT}
FUNC_RESULT=1;     ENTRY_VAR=2;        VARIABLE=3;         VAR_PARM=4;
UNIV_VAR=5;        CONST_PARM=6;       UNIV_CONST=7;       FIELD=8;
EXPR=10;           CONSTANT=11;        SAVE_PARM=12;       NEW_PARM=13;
TAG_FIELD=14;      WITH_CONST = 15;    WITH_VAR = 16;

{TYPE KIND}
INT_KIND=0;        REAL_KIND=1;        BOOL_KIND=2;        CHAR_KIND=3;
ENUM_KIND=4;       SET_KIND=5;         STRING_KIND=6;      NONLIST_KIND=7;
POINTER_KIND=8;    LIST_KIND=9;        GENERIC_KIND=10;    UNDEF_KIND=11;
ROUTINE_KIND=12;

{INPUT_MODES}
PROC1_MODE=1;      FUNC1_MODE=2;       PROGRAM1_MODE=3;    RECORD_MODE=4;
VARIANT_MODE=5;

{OUTPUT_MODES}
SCONST2_MODE=11;   LCONST2_MODE=0;     PROC2_MODE=1;       PROGRAM2_MODE=2;
PE2_MODE=3;        CE2_MODE=4;         ME2_MODE=5;         PROCESS2_MODE=6;
CLASS2_MODE=7;     MONITOR2_MODE=8;    STD2_MODE=9;        UNDEF2_MODE=10;

{"MISCELANEOUS}
MAX_INT=32667;     SET_MIN=0;          SET_MAX=127;        THIS_PASS=4;
STACK_MAX=100;     NOUN_MAX=700;       MAX_LEVEL=15;
TAG_MIN=0;         TAG_MAX=15;
INITIAL_LEVEL=0;   RESOLVE=TRUE;       DONT_RESOLVE=FALSE;
INITIALBLOCK = 1;  BYTELENGTH = 1;
TEXT_LENGTH = 18;
INFILE = 2;        OUTFILE = 1;

 {ERRORS}

NESTING_ERROR=1;   ADDRESS_ERROR=2;    RESOLVE_ERROR=23;   TAG_ERROR=24;
POINTER_ERROR=25;  ENTRY_ERROR=6;      FUNCTYPE_ERROR=7;   TYPEID_ERROR=8;
ENUM1_ERROR=9;     ENUM2_ERROR=10;     INDEX_ERROR=11;     MEMBER_ERROR=12;
STACK_ERROR=13;    PARM1_ERROR=14;     PARM2_ERROR=15;     PARM3_ERROR=16;
PARM4_ERROR=17;    PARM5_ERROR=18;     PARM6_ERROR=19;     PARM7_ERROR=20;
COMPILER_ERROR=21; STRING_ERROR=22;

TYPE
  INPUT_MODE = PROC1_MODE..VARIANT_MODE;
  DISPLACEMENT=INTEGER;
  OUTPUT_MODE=LCONST2_MODE..SCONST2_MODE;
  STACK_INDEX=0..STACK_MAX;
  NOUN_INDEX=0..NOUN_MAX;
  TYPE_KIND=INT_KIND..ROUTINE_KIND;
  TYPE_KINDS=SET OF TYPE_KIND;
  CONTEXT_KIND=FUNC_RESULT..WITH_VAR;
  CONTEXTS=SET OF CONTEXT_KIND;
  PACKED_SET=0..15;
  TEXT_TYPE = ARRAY (.1..TEXT_LENGTH.) OF CHAR;
  ENTRY_CLASS=(UNDEFINED,VALUE,ROUTINE,TEMPLATE);
  ENTRY_PTR=^ENTRY;
  ENTRY=
    RECORD
      CASE CLASS_:ENTRY_CLASS OF
        VALUE:(
          VMODE:OUTPUT_MODE; VDISP,CLEAR_SIZE:DISPLACEMENT;
          CONTEXT:CONTEXT_KIND);
        ROUTINE:(
          RMODE:OUTPUT_MODE; RDISP:DISPLACEMENT;
          PARM_SIZE,VAR_SIZE:{DISPLACEMENT} integer);
        TEMPLATE:(
          NOUN:{NOUN_INDEX} integer; SIZE:{DISPLACEMENT} integer;
          CASE KIND:TYPE_KIND OF
            INT_KIND,BOOL_KIND,CHAR_KIND,ENUM_KIND:(
              MIN,MAX:INTEGER))
    END;
  DISPLAY_INDEX=0..MAX_LEVEL;
  DISPLAY_REC=
    RECORD
      LAST_MODE: OUTPUT_MODE;
      LAST_ADDRESS:DISPLACEMENT;
      LAST_INITIALIZE:BOOLEAN
    END;


 VAR
 SY,PARM_NUMBER,RESET_POINT:INTEGER;
 WITH_CONTEXT:CONTEXT_KIND;
 N:NOUN_INDEX;
 DONE,UNIVERSAL,SAVE_CONTEXT,GENERIC_FUNCTION,PREFIX_SW,INITIALIZE,
 NO_FORWARD: BOOLEAN;
 NOUN_TABLE:ARRAY (.NOUN_INDEX.) OF ENTRY_PTR;
 STACK:ARRAY (.STACK_INDEX.) OF ENTRY_PTR;
 THIS_LEVEL, T: INTEGER;
 DISPLAY: ARRAY (.DISPLAY_INDEX.) OF DISPLAY_REC;
 CURRENT_DISP,CURRENT_LABEL: DISPLACEMENT;
 CHK_MODE:{INPUT_MODE} integer;
 MODE: OUTPUT_MODE;
 PASS_BY_REFERENCE, ASSIGNABLE: CONTEXTS;
 UENTRY,NEW_ENTRY,OLD_ENTRY,UTYPE: ENTRY_PTR;
 SMALLS,LISTS,NONLISTS,FUNC_TYPES,INDEXS,LARGES: TYPE_KINDS;
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


{##########}
{INITIALIZE}
{##########}
 PROCEDURE STD_INDEX(N:NOUN_INDEX; K:TYPE_KIND; L,U:INTEGER);
  VAR E:ENTRY_PTR;
  BEGIN
    NEW(E); NOUN_TABLE(.N.):=E;
    WITH E^ DO BEGIN
      CLASS_:=TEMPLATE; NOUN:=N;
      SIZE:=WORDLENGTH;
      KIND:=K; MIN:=L; MAX:=U
    END
  END;

  PROCEDURE STD_PARM(N:NOUN_INDEX; C:CONTEXT_KIND);
  VAR E:ENTRY_PTR;
  BEGIN
    NEW(E); NOUN_TABLE(.N.):=E;
    WITH E^ DO BEGIN
      CLASS_:=VALUE; VMODE:=UNDEF2_MODE;
      VDISP:= 0;
      CONTEXT:=C
    END
  END;

  PROCEDURE STD_ROUTINE(N:NOUN_INDEX; NO:INTEGER);
  VAR E:ENTRY_PTR;
  BEGIN
    NEW(E); NOUN_TABLE(.N.):=E;
    WITH E^ DO BEGIN
      CLASS_:=ROUTINE; RMODE:=STD2_MODE; RDISP:=NO;
      PARM_SIZE:= 0; VAR_SIZE:= 0;
    END
  END;

  PROCEDURE STD_NONINDEX(N:NOUN_INDEX; K:TYPE_KIND; S:DISPLACEMENT);
  VAR E:ENTRY_PTR;
  BEGIN
    NEW(E); NOUN_TABLE(.N.):=E;
    WITH E^ DO BEGIN
      CLASS_:=TEMPLATE;
      NOUN:=N; SIZE:=S; KIND:=K
    END
  END;

  PROCEDURE INITIALIZE_;
  VAR I:INTEGER;
  BEGIN
    IF DEBUG THEN PRINTFF(THIS_PASS);
    first(outfile3);
    INIT (outfile4);

    GENERIC_FUNCTION:=FALSE;
    CURRENT_DISP:=0;
    PREFIX_SW:=TRUE;
    T:=-1; DONE:=FALSE;
    THIS_LEVEL:=-1;
    SAVE_CONTEXT:=FALSE;
    NO_FORWARD:= FALSE;
    MODE:=PROGRAM2_MODE;
    ASSIGNABLE:= (.FUNC_RESULT, VARIABLE, VAR_PARM, UNIV_VAR, WITH_VAR.);
    NONLISTS:=(.INT_KIND,REAL_KIND,BOOL_KIND,CHAR_KIND,ENUM_KIND,
      SET_KIND,STRING_KIND,NONLIST_KIND,UNDEF_KIND.);
    LISTS:=(.POINTER_KIND,LIST_KIND.);
    CURRENT_LABEL:=INITIALBLOCK;
    NEW(UTYPE);
    WITH UTYPE^ DO BEGIN
      CLASS_:=TEMPLATE;
      NOUN:=XUNDEF; SIZE:=1;
      KIND:=UNDEF_KIND
    END;
    INDEXS:=(.INT_KIND,BOOL_KIND,CHAR_KIND,ENUM_KIND.);
    PASS_BY_REFERENCE:=(.VAR_PARM,UNIV_VAR.);
    LARGES:=(.STRING_KIND,LIST_KIND,NONLIST_KIND.);
    SMALLS:=(.INT_KIND,REAL_KIND,CHAR_KIND,BOOL_KIND,ENUM_KIND,SET_KIND.);
    FUNC_TYPES:= (.INT_KIND, CHAR_KIND, BOOL_KIND, ENUM_KIND,
      POINTER_KIND, REAL_KIND.);
    NEW(UENTRY); UENTRY^.CLASS_:=UNDEFINED; NOUN_TABLE(.XUNDEF.):=UENTRY;
    STD_INDEX(XINTEGER,INT_KIND,-32767,32767);
    STD_NONINDEX(XREAL,REAL_KIND,REALLENGTH);
    STD_INDEX(XBOOLEAN,BOOL_KIND,0,1);
    STD_INDEX(XCHAR,CHAR_KIND,0,127);
    STD_NONINDEX(ZWITH,POINTER_KIND,WORDLENGTH);
    STD_NONINDEX(ZARITHMETIC,GENERIC_KIND,0);
    STD_NONINDEX(ZINDEX,GENERIC_KIND,0);
    STD_NONINDEX(ZPOINTER,POINTER_KIND,WORDLENGTH);
    NOUN_TABLE(.ZPOINTER.)^.NOUN:=XUNDEF {GENERIC POINTERS HAVE UNDEF NOUN};
    STD_PARM(ZVPARM,VAR_PARM);
    STD_PARM(ZCPARM,CONST_PARM);
    STD_PARM(ZSPARM,SAVE_PARM);
    STD_PARM(ZNPARM,NEW_PARM);
    STD_ROUTINE( XNEW,-1);
    STD_ROUTINE( XTRUNC,0);
    STD_ROUTINE( XABS,1);
    STD_ROUTINE( XSUCC,2);
    STD_ROUTINE( XPRED,3);
    STD_ROUTINE( XCONV,4);
    STD_ROUTINE( XATTRIBUTE,6);
    STD_ROUTINE( XORD,8);
    STD_ROUTINE( XCHR,9);
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
      BLOCKS:=CURRENT_LABEL;
    END;
    PUT1(EOM2,0 );{initial PROCESS VAR SIZE}
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
    T:=T-1;
    get_l(outfile3, ARG);
    get_l(outfile3, MIN);
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
    FOR I:=1 TO LENGTH DIV WORDLENGTH DO BEGIN
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

  PROCEDURE PUSH;
  BEGIN
    IF T>=STACK_MAX THEN ABORT ELSE T:=T+1;
    STACK(.T.):=UENTRY {"***** TEMPORARY *****}
  END;

  PROCEDURE PUSH_NEW_ENTRY(VAR E:ENTRY_PTR);
  var
   N:integer;
  BEGIN
    get_l(outfile3, N);
    E:=NOUN_TABLE(.N.);
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
      LAST_ADDRESS:=CURRENT_DISP;
      IF M<>VARIANT_MODE THEN CURRENT_DISP:=0;
      IF MODE<>PROGRAM2_MODE THEN
        IF M< RECORD_MODE THEN ERROR(NESTING_ERROR);
      CASE M OF
        PROC1_MODE,FUNC1_MODE: MODE:=PROC2_MODE;
        PROGRAM1_MODE: MODE:=PROGRAM2_MODE;
        VARIANT_MODE,RECORD_MODE: MODE:=UNDEF2_MODE
      END;
      LAST_INITIALIZE:=INITIALIZE; INITIALIZE:=FALSE
    END
  END;

  PROCEDURE POP_LEVEL;
  BEGIN
    WITH DISPLAY(.THIS_LEVEL.) DO BEGIN
      MODE:=LAST_MODE;
      CURRENT_DISP:=LAST_ADDRESS;
      INITIALIZE:=LAST_INITIALIZE
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
    {ASSERT (A>=0) AND (B>=0);"}
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
      CLASS_:=TEMPLATE;
      NOUN:=N; SIZE:=WORDLENGTH;
      KIND:=ENUM_KIND;
      MIN:=0;
      get_l(outfile3, MAX);
     IF MAX>SET_MAX
        THEN ERROR(ENUM2_ERROR)
    END;
    IF MODE=UNDEF2_MODE THEN ERROR(ENUM1_ERROR)
  END;

  PROCEDURE SUBR_DEF;
  VAR SUBR_ENTRY:ENTRY_PTR;
    //NOUN:integer;
  BEGIN
    PUSH_NEW_ENTRY(SUBR_ENTRY);
    WITH SUBR_ENTRY^ DO BEGIN
      CLASS_:=TEMPLATE;
      get_l(outfile3, NOUN);
      SIZE:=WORDLENGTH;
      IF NOUN=XUNDEF THEN KIND:=ENUM_KIND
        ELSE KIND:=NOUN_TABLE(.NOUN.)^.KIND;
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
      CLASS_:=TEMPLATE;
      NOUN:=SET_NOUN;
      SIZE:=SETLENGTH1;
      KIND:=SET_KIND
    END
  END;

  PROCEDURE ARRAY_DEF;
  VAR SPAN,ARRAY_SIZE:DISPLACEMENT; ARRAY_KIND:TYPE_KIND;
    ARRAY_ENTRY:ENTRY_PTR;
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
        IF KIND IN LISTS THEN ARRAY_KIND:=LIST_KIND
        ELSE ARRAY_KIND:=NONLIST_KIND;
        ARRAY_SIZE:=MULTIPLY(SPAN,SIZE)
      END;
    END;
    T:=T-2 {POP INDEX AND ELEMENT TYPES};
    PUSH_NEW_ENTRY(ARRAY_ENTRY);
    WITH ARRAY_ENTRY^ DO BEGIN
      CLASS_:=TEMPLATE;
      NOUN:=N; SIZE:=ARRAY_SIZE;
      KIND:=ARRAY_KIND
    END
  END;

  PROCEDURE POINTER;
  VAR PTR_ENTRY:ENTRY_PTR;
  BEGIN
    IF MODE=UNDEF2_MODE {IN RECORD} THEN ERROR(POINTER_ERROR);
    PUSH_NEW_ENTRY(PTR_ENTRY);
    WITH PTR_ENTRY^ DO BEGIN
      CLASS_:=TEMPLATE; NOUN:=N;
      SIZE:=WORDLENGTH; KIND:=POINTER_KIND
    END
  END;

  PROCEDURE FIELDLIST;
  VAR THIS_SIZE:DISPLACEMENT; NUMBER,I:INTEGER;
  BEGIN
    WITH STACK(.T.)^ DO BEGIN
      INITIALIZE:=INITIALIZE OR (KIND IN LISTS);
      THIS_SIZE:=SIZE
    END;
    GET_L(outfile3, NUMBER);
    FOR I:=NUMBER DOWNTO 1 DO {ASSIGN ADDRESSES IN FORWARD DIRECTION}
      WITH STACK(.T-I.)^ DO BEGIN
        CLASS_:=VALUE; VMODE:=MODE; CONTEXT:=FIELD;
        VDISP:=CURRENT_DISP; CURRENT_DISP:=ADD(CURRENT_DISP,THIS_SIZE)
      END;
    T:=T-NUMBER-1 {POP DECLARATION LIST}
  END;

  PROCEDURE TAG_DEF;
  VAR THIS_SIZE:DISPLACEMENT;
  BEGIN
    {TAG} TYPE_;
    WITH STACK(.T.)^ DO BEGIN
      IF KIND IN INDEXS THEN
       BEGIN
        IF (MIN<TAG_MIN) OR (MAX>TAG_MAX)
         THEN ERROR(TAG_ERROR)
       END
      ELSE ERROR(TAG_ERROR);
      THIS_SIZE:=SIZE; INITIALIZE:=INITIALIZE OR (KIND IN LISTS)
    END;
    T:=T-1;
    WITH STACK(.T.)^ DO BEGIN
      CLASS_:=VALUE; VMODE:=MODE;
      CONTEXT:=TAG_FIELD; CLEAR_SIZE:=0;
      VDISP:=CURRENT_DISP; CURRENT_DISP:=ADD(CURRENT_DISP,THIS_SIZE)
    END;
    PUSH_LEVEL(VARIANT_MODE)
  END;

  PROCEDURE PART_END;
  VAR VARNT_SIZE:DISPLACEMENT;
  BEGIN
    WITH STACK(.T.)^ {TAG FIELD}, DISPLAY(.THIS_LEVEL.) DO BEGIN
      VARNT_SIZE:=CLEAR_SIZE;
      IF INITIALIZE THEN
        LAST_INITIALIZE:=TRUE ELSE CLEAR_SIZE:=0;
      LAST_ADDRESS:=ADD(CURRENT_DISP,VARNT_SIZE)
    END;
    T:=T-1; POP_LEVEL
  END;

  PROCEDURE VARNT_END;
  VAR VARNT_SIZE:DISPLACEMENT;
  BEGIN
    WITH STACK(.T.)^ {TAG FIELD}, DISPLAY(.THIS_LEVEL.) DO BEGIN
      VARNT_SIZE:=CURRENT_DISP-LAST_ADDRESS;
      IF VARNT_SIZE>CLEAR_SIZE THEN CLEAR_SIZE:=VARNT_SIZE;
      CURRENT_DISP:=LAST_ADDRESS
    END
  END;

  PROCEDURE REC_DEF;
  VAR REC_ENTRY:ENTRY_PTR;
  BEGIN
    PUSH_NEW_ENTRY(REC_ENTRY);
    WITH REC_ENTRY^ DO BEGIN
      CLASS_:=TEMPLATE;
      NOUN:=N; SIZE:=CURRENT_DISP;
      IF INITIALIZE THEN KIND:=LIST_KIND ELSE KIND:=NONLIST_KIND
    END;
    POP_LEVEL
  END;



{#####################}
{"VARIABLE DECLARATIONS}
{#####################}

  PROCEDURE VAR_LIST;
  VAR NUMBER,I:INTEGER; THIS_SIZE:DISPLACEMENT;
  BEGIN
    WITH STACK(.T.)^ {TYPE} DO BEGIN
      THIS_SIZE:=SIZE;
      INITIALIZE:=INITIALIZE OR (KIND IN LISTS)
    END;
    GET_L (outfile3, number);
    FOR I:=NUMBER DOWNTO 1 DO {ASSIGN ADDRESSES IN FORWARD DIRECTION}
      WITH STACK(.T-I.)^ DO BEGIN
        CLASS_:=VALUE; VMODE:=MODE; CONTEXT:=VARIABLE;
        CURRENT_DISP:=ADD(CURRENT_DISP,THIS_SIZE); VDISP:=-CURRENT_DISP
      END;
    T:=T-NUMBER-1 {POP DECLARATION LIST}
  END;

{####################}
{ROUTINE DECLARATIONS}
{####################}

PROCEDURE PEND;
  VAR VSIZE:DISPLACEMENT; I:INTEGER;
  BEGIN
    CURRENT_DISP:=WORDLENGTH; {LEAVE A WORD FOR LINE NUMBER}
    FOR I:=0 TO PARM_NUMBER-1 DO {ASSIGN ADDRESSES IN REVERSE ORDER}
      WITH STACK(.T-I.)^ DO BEGIN
        VSIZE:=VDISP; VDISP:=CURRENT_DISP;
        CURRENT_DISP:=ADD(CURRENT_DISP,VSIZE);
        VMODE:=MODE
      END;
    CURRENT_DISP:=CURRENT_DISP-WORDLENGTH {CENTER};
    T:=T-PARM_NUMBER {POP PARMS};
  END;

PROCEDURE ROUTINE_DEF(RESOLVE:BOOLEAN);
  VAR ROUTINE_ENTRY:ENTRY_PTR;
  BEGIN
    IF RESOLVE THEN BEGIN
      IF PARM_NUMBER>0 THEN BEGIN
        ERROR(RESOLVE_ERROR); PEND
      END;
      NO_FORWARD:= TRUE;
      PUSH_OLD_ENTRY(ROUTINE_ENTRY);
    END ELSE BEGIN
      PEND;
      PUSH_NEW_ENTRY(ROUTINE_ENTRY);
      WITH ROUTINE_ENTRY^ DO BEGIN
        CLASS_:=ROUTINE;
        PARM_SIZE:=CURRENT_DISP;
        VAR_SIZE:= 0;
        IF PREFIX_SW THEN RMODE:=PE2_MODE ELSE RMODE:=MODE;
        CURRENT_LABEL:=CURRENT_LABEL+1; RDISP:=CURRENT_LABEL
      END
    END;
    CURRENT_DISP:=0;
    // MARK(RESET_POINT);
    IF PREFIX_SW THEN BEGIN
      T:=T-1; POP_LEVEL
    END
  END;

  PROCEDURE FORWARD_;
  BEGIN
    IF NO_FORWARD THEN BEGIN
      ERROR(RESOLVE_ERROR);
      NO_FORWARD:= FALSE
    END;
    T:= T- 1;  POP_LEVEL
  END;

  PROCEDURE PROG_DEF;
  VAR SAVE_LABEL:INTEGER;
  BEGIN
    PREFIX_SW:=FALSE;
    SAVE_LABEL:=CURRENT_LABEL; CURRENT_LABEL:=0;
    ROUTINE_DEF(DONT_RESOLVE);
    CURRENT_LABEL:=SAVE_LABEL
  END;

  PROCEDURE FUNC_DEF(RESOLVE:BOOLEAN);
  VAR FUNC_TYPE:ENTRY_PTR;
  BEGIN
    IF NOT RESOLVE THEN BEGIN
      TYPE_;
      IF NOT(STACK(.T.)^.KIND IN FUNC_TYPES) THEN ERROR(FUNCTYPE_ERROR);
      T:=T-1
    END;
    ROUTINE_DEF(RESOLVE)
  END;

  PROCEDURE MODE_;
  BEGIN
    GET_L(outfile3,CHK_MODE);
    PUSH_LEVEL(CHK_MODE);
    PARM_NUMBER:=0
  END;

  PROCEDURE UNIV_TYPE;
  BEGIN
    TYPE_;
    IF STACK(.T.)^.KIND IN LISTS THEN ERROR(PARM6_ERROR);
    UNIVERSAL:=TRUE;
  END;

  PROCEDURE PARMLIST(C:CONTEXT_KIND);
  VAR I,NUMBER:INTEGER; THIS_SIZE:DISPLACEMENT;
  BEGIN
    GET_L(outfile3, NUMBER);
    PARM_NUMBER:=PARM_NUMBER+NUMBER;
    WITH STACK(.T.)^ DO
      IF (C IN PASS_BY_REFERENCE) OR (KIND IN LARGES)
        THEN THIS_SIZE:=WORDLENGTH ELSE THIS_SIZE:=SIZE;
    FOR I:=1 TO NUMBER DO
      WITH STACK(.T-I.)^ DO BEGIN
        CLASS_:=VALUE; VDISP:=THIS_SIZE;
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
    IF CHK_MODE=FUNC1_MODE THEN ERROR(PARM7_ERROR);
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
      PUT4(BODY2,RMODE,RDISP,PARM_SIZE,VAR_SIZE);
      IF INITIALIZE THEN PUT1(INITVAR2,CURRENT_DISP)
    END;
    NO_FORWARD:= FALSE;
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
  VAR N:{NOUN_INDEX} integer;
  BEGIN
    GET_L(outfile3, N);
    WITH NOUN_TABLE(.N.)^ DO
      IF CLASS_=TEMPLATE THEN PUT3_ARG(KIND,NOUN,SIZE)
      ELSE PUT3_ARG(UNDEF_KIND,XUNDEF,1)
  END;

  PROCEDURE RESULT;
  BEGIN
    PUT1(RESULT2, STACK(.T.)^.PARM_SIZE + WORDLENGTH {CENTER});
    PUT_TYPE
  END;

  PROCEDURE STORE;
  BEGIN
    WITH STACK(.T-1.)^ DO
      IF CLASS_=VALUE THEN
        IF CONTEXT=TAG_FIELD THEN PUT1(TAG_STORE2,CLEAR_SIZE)
        ELSE PUT0(STORE2)
      ELSE PUT0(STORE2);
    T:=T-2
  END;

  PROCEDURE PARM;
  VAR PARM_NOUN:{NOUN_INDEX} integer; OP:INTEGER;  PARM_CONTEXT:CONTEXT_KIND;
  BEGIN
    GET_L(outfile3, PARM_NOUN);
    IF PARM_NOUN<>XUNDEF THEN
    WITH NOUN_TABLE(.PARM_NOUN.)^ DO BEGIN
      PARM_CONTEXT:= CONTEXT;
      CASE PARM_CONTEXT OF
        VAR_PARM,UNIV_VAR,NEW_PARM: OP:=VARPARM2;
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
          IF N {TYPE NOUN} <> XINTEGER THEN PUT2(RANGE2,MIN,MAX)
    END;
    T:=T-2
  END;

  PROCEDURE CALL_NEW;
  VAR INITIALIZE:0..1;
  BEGIN
    TYPE_;
    WITH STACK(.T.)^ DO BEGIN
      IF KIND IN LISTS THEN INITIALIZE:=1 ELSE INITIALIZE:=0;
      PUT2(CALL_NEW2,SIZE,INITIALIZE)
    END;
    T:=T-2
  END;

  PROCEDURE FOR_LIM;
  VAR ARG1,ARG2,ARG4:INTEGER;
  BEGIN
    GET_L (outfile3, ARG1);
    GET_L (outfile3, ARG2);
    GET_L (outfile3, ARG4);
    CURRENT_DISP:=ADD(CURRENT_DISP,WORDLENGTH);
    PUT4(FOR_LIM2,ARG1,-CURRENT_DISP,ARG2,ARG4);
    T:=T-3
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
      CLASS_:=VALUE; VMODE:=PROC2_MODE {ALL TEMPS HAVE PROCEDURE MODE};
      CURRENT_DISP:=ADD(CURRENT_DISP,WORDLENGTH);
      VDISP:=-CURRENT_DISP;
      IF WITH_CONTEXT IN ASSIGNABLE THEN CONTEXT:= WITH_VAR
        ELSE CONTEXT:= WITH_CONST
    END;
    T:=T-2;
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

 PROCEDURE CALL_FUNC;
  BEGIN
    IF GENERIC_FUNCTION THEN BEGIN
      PUT0(CALL_GEN2);
      GENERIC_FUNCTION:= FALSE
    END ELSE PUT0(CALL_FUNC2)
  END;

  PROCEDURE FUNCTION_;
  BEGIN
    PUT0(FUNCTION2);
    PUT_TYPE
  END;

  PROCEDURE BINARY(OP:INTEGER);
  BEGIN
    T:=T-1; STACK(.T.):=UENTRY;
    PUT0(OP)
  END;

  PROCEDURE INDEX;
  VAR VALUE:INTEGER;
  BEGIN
    PUSH;
    GET_L(outfile3,VALUE);
    PUT3(VAR2,SCONST2_MODE,VALUE,CONSTANT);
    PUT_TYPE
  END;

  PROCEDURE REAL_;
  VAR DISP:DISPLACEMENT;
  BEGIN
    PUSH;
    GET_L(outfile3, DISP);
    PUT3(VAR2,LCONST2_MODE,DISP,CONSTANT);
    PUT3_ARG(REAL_KIND,XREAL,REALLENGTH)
  END;

  PROCEDURE SSTRING;
  VAR LENGTH:INTEGER;  DISP:DISPLACEMENT;
  BEGIN
    PUSH;
    GET_L(outfile3, LENGTH);
    GET_L(outfile3, DISP);
    PUT3(VAR2,LCONST2_MODE,DISP,CONSTANT);
    PUT3_ARG(STRING_KIND,LENGTH,LENGTH)
  END;

  PROCEDURE VARIANT;
  VAR TAGSET:INTEGER; TAGFIELD:ENTRY_PTR;
  BEGIN
    new(TAGFIELD);
    GET_L(outfile3,TAGSET);
    PUSH_OLD_ENTRY(TAGFIELD); T:=T-1;
    WITH TAGFIELD^ DO
      IF CLASS_=VALUE THEN PUT2(VARIANT2,TAGSET,VDISP)
  END;

  PROCEDURE VCOMP(OP:INTEGER);
  VAR N:NOUN_INDEX;  VAR_ENTRY:ENTRY_PTR;
  BEGIN
    IF OP=VCOMP2 THEN T:=T-1 {POP RECORD};
    PUSH_OLD_ENTRY(VAR_ENTRY);
    WITH VAR_ENTRY^ DO BEGIN
      PUT3(OP,VMODE,VDISP,CONTEXT);
      PUT_TYPE;
      IF SAVE_CONTEXT THEN BEGIN
        WITH_CONTEXT:=CONTEXT; SAVE_CONTEXT:=FALSE
      END
    END
  END;

  PROCEDURE ARROW;
  BEGIN
    PUT0(ARROW2); PUT_TYPE;
    STACK(.T.):=UENTRY
  END;

  PROCEDURE SUB;
  VAR N:NOUN_INDEX; INDEX,ELEMENT:ENTRY_PTR;
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
    WITH ELEMENT^ DO PUT3_ARG(KIND,NOUN,LENGTH);
    T:=T-1; STACK(.T.):=ELEMENT
  END;

  PROCEDURE ROUTINE_;
  VAR ROUT:ENTRY_PTR;
  BEGIN
    PUSH_OLD_ENTRY(ROUT);
    WITH ROUT^ DO
      IF CLASS_=ROUTINE THEN
        PUT4(ROUTINE2,RMODE,RDISP,PARM_SIZE,VAR_SIZE)
      ELSE PUT0(UNDEF2)
  END;


{MAIN LOOP PASS4}

begin
 INITIALIZE_;
 REPEAT
 get_L(outfile3, SY);
CASE SY OF
 ADDRESS1: PUT0(ADDRESS2);
 AND1: BINARY(AND2);
 ARRAY_DEF1: ARRAY_DEF;
 ARROW1: ARROW;
 BODY_END1: BODY_END;
 BODY1: BODY;
 CALL_FUNC1: CALL_FUNC;
 CALL_NEW1: CALL_NEW;
 CALL_PROC1: BEGIN PUT0(CALL_PROC2); T:=T-1 END;
 CASE_JUMP1: IGNORE1(CASE_JUMP2);
 CASE_LIST1: CASE_LIST;
 CHK_TYPE1: BEGIN PUT0(CHK_TYPE2); PUT_TYPE END;
 CPARMLIST1: CPARM_LIST;
 DEF_LABEL1: IGNORE1(DEF_LABEL2);
 DIV1: BINARY(DIV2);
 EMPTY_SET1: BEGIN PUSH; PUT0(EMPTY_SET2) END;
 EOM1: EOM;
 ENUM_DEF1: ENUM_DEF;
 EQ1: BINARY(EQ2);
 FALSEJUMP1: BEGIN IGNORE1(FALSEJUMP2); T:=T-1 END;
 FIELDLIST1: FIELDLIST;
 FOR_DOWN1: FOR_LOOP(FOR_DOWN2);
 FOR_LIM1: FOR_LIM;
 FOR_STORE1: PUT0(FOR_STORE2);
 FOR_UP1: FOR_LOOP(FOR_UP2);
 FORWARD1: FORWARD_;
 FUNC_DEF1: FUNC_DEF(DONT_RESOLVE);
 FUNCF_DEF1: FUNC_DEF(RESOLVE);
 FUNCTION1: FUNCTION_;
 GE1: BINARY(GE2);
 GT1: BINARY(GT2);
 INCLUDE1: BINARY(INCLUDE2);
 INDEX1: INDEX;
 IN1: BINARY(IN2);
 JUMP_DEF1: IGNORE2(JUMP_DEF2);
 JUMP1: IGNORE1(JUMP2);
 LCONST1: LCONST;
 LE1: BINARY(LE2);
 LT1: BINARY(LT2);
 MESSAGE1: IGNORE2(MESSAGE2);
 MINUS1: BINARY(MINUS2);
 MODE1: MODE_;
 MOD1: BINARY(MOD2);
 NEW_LINE1: IGNORE1(NEW_LINE2);
 NEW_NOUN1: PUSH_NEW_ENTRY(NEW_ENTRY);
 NE1: BINARY(NE2);
 NOT1: PUT0(NOT2);
 OR1: BINARY(OR2);
 PARM_TYPE1: TYPE_;
 PARM1: PARM;
 PART_END1: PART_END;
 PLUS1: BINARY(PLUS2);
 POINTER1: POINTER;
 PROC_DEF1: ROUTINE_DEF(DONT_RESOLVE);
 PROCF_DEF1: ROUTINE_DEF(RESOLVE);
 PROG_DEF1: PROG_DEF;
 REAL1: REAL_;
 REC_DEF1: REC_DEF;
 REC1: PUSH_LEVEL(RECORD_MODE);
 RESULT1: RESULT;
 ROUTINE1: ROUTINE_;
 SET_DEF1: SET_DEF;
 SLASH1: BINARY(SLASH2);
 STAR1: BINARY(STAR2);
 STORE1:STORE;
 STRING1: SSTRING;
 SUBR_DEF1: SUBR_DEF;
 SUB1: SUB;
 TAG_DEF1: TAG_DEF;
 TYPE_DEF1: T:=T-1;
 TYPE1: TYPE_;
 UMINUS1: PUT0(UMINUS2);
 UNDEF1: BEGIN PUSH; PUT0(UNDEF2) END;
 UNIV_TYPE1: UNIV_TYPE;
 UPLUS1: PUT0(UPLUS2);
 VALUE1: PUT0(VALUE2);
 VAR_LIST1: VAR_LIST;
 VARIANT1: VARIANT;
 VARNT_END1: VARNT_END;
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
RESULT1=5;         TAG_STORE1=6;       STORE1=7;           CALL_PROC1=8;
CALL_NEW1=9;       CONSTPARM1=10;      VARPARM1=11;        SAVEPARM1=12;
FALSEJUMP1=13;     JUMP1=14;           JUMP_DEF1=15;       DEF_LABEL1=16;
CHK_TYPE1=17;      CASE_LIST1=18;      FOR_STORE1=19;      FOR_LIM1=20;
FOR_UP1=21;        FOR_DOWN1=22;       WITH1=23;           VALUE1=24;
LT1=25;            EQ1=26;             GT1=27;             LE1=28;
NE1=29;            GE1=30;             IN1=31;             UPLUS1=32;
UMINUS1=33;        PLUS1=34;           MINUS1=35;          OR1=36;
STAR1=37;          SLASH1=38;          DIV1=39;            MOD1=40;
AND1=41;           NOT1=42;            EMPTY_SET1=43;      INCLUDE1=44;
FUNCTION1=45;      CALL_FUNC1=46;      CALL_GEN1=47;       ROUTINE1=48;
VAR1=49;           ARROW1=50;          VCOMP1=51;          VARIANT1=52;
SUB1=53;           NEW_LINE1=54;       MESSAGE1=55;        LCONST1=56;
INITVAR1=57;       UNDEF1=58;          RANGE1=59;          CASE_JUMP1=60;

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
EXPR=10;           CONSTANT=11;        SAVE_PARM=12;       NEW_PARM=13;
TAG_FIELD=14;      WITH_CONST = 15;    WITH_VAR = 16;

{TYPE KIND}
INT_KIND=0;        REAL_KIND=1;        BOOL_KIND=2;        CHAR_KIND=3;
ENUM_KIND=4;       SET_KIND=5;         STRING_KIND=6;      NONLIST_KIND=7;
POINTER_KIND=8;    LIST_KIND=9;        GENERIC_KIND=10;    UNDEF_KIND=11;
ROUTINE_KIND=12;

{STANDARD SPELLING/NOUN INDICES}
XUNDEF=0;          XFALSE=1;           XTRUE=2;            XINTEGER=3;
XBOOLEAN=4;        XCHAR=5;            XNIL=6;             XABS=7;
XATTRIBUTE=8;      XCHR=9;             XCONV=10;           XORD=11;
XPRED=12;          XSUCC=13;           XTRUNC=14;          XNEW=15;
XREAL=16;

 {STANDARD NOUN INDICES}
ZARITHMETIC=17;    ZINDEX=18;          ZPASSIVE=19;        ZPOINTER=20;
ZVPARM=21;         ZCPARM=22;          ZSPARM=23;          ZNPARM=24;
ZWITH=25;

{DATA TYPS}
BYTE_TYP=0;        WORD_TYP=1;         REAL_TYP=2;         SET_TYP=3;
STRUCT_TYP=4;

{ADDRESS MODES}
SCONST_MODE=11;    LCONST_MODE=0;      PROC_MODE=1;        PROG_MODE=2;
PE_MODE=3;         CE_MODE=4;          ME_MODE=5;          PROCESS_MODE=6;
CLASS_MODE=7;      MONITOR_MODE=8;     STD_MODE=9;         UNDEF_MODE=10;
TEMP_MODE=PROC_MODE;

{COMPARISONS}
LESS=0;            EQUAL=1;            GREATER=2;          NOTLESS=3;
NOTEQUAL=4;        NOTGREATER=5;       INSET=6;

{ERRORS}
COMPILER_ERROR=1;  TYPE_ERROR=2;       ADDRESS_ERROR=3;    ASSIGN_ERROR=4;

THIS_PASS=5;       BYTELENGTH = 1;
TEXT_LENGTH = 18;
INFILE = 1;        OUTFILE = 2;


TYPE
  TEXT_TYPE = ARRAY (.1..TEXT_LENGTH.) OF CHAR;
  DISPLACEMENT=INTEGER;
  ADDR_STATE=(DIRECT,INDIRECT,ADDR,EXPRESSION);
  ADDR_MODE= LCONST_MODE..SCONST_MODE;
  ADDR_MODES=SET OF ADDR_MODE;
  TYPE_KIND=INT_KIND..ROUTINE_KIND;
  STORE_CLASS=(STORE_FOR,STORE_TAG,STORE_USUAL);
  TYPE_KINDS=SET OF TYPE_KIND;
  CONTEXT_KIND=FUNC_RESULT..WITH_VAR;
  CONTEXTS=SET OF CONTEXT_KIND;
  OPERAND_CLASS=(UNDEFINED,VALUE,ROUTINE);
  OPERAND=
    RECORD
      KIND:TYPE_KIND; NOUN:INTEGER;
      MODE:ADDR_MODE; DISP:DISPLACEMENT; LENGTH:DISPLACEMENT;
      CASE CLASS_:OPERAND_CLASS OF
        VALUE:(CONTEXT:CONTEXT_KIND; STATE:ADDR_STATE);
        ROUTINE:(PARM_SIZE,VAR_SIZE:DISPLACEMENT)
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
  CURRENT_MODE: ADDR_MODE;
  ROUTINE_MODES: ADDR_MODES;
  TOP_STACK,THIS_STACK,EMPTY_STACK:STACK_LINK;
  DONE: BOOLEAN;
  NONLISTS,INDEXS,LARGES,ARITHMETIC,INDIRECTS,SMALLS,POINTERS: TYPE_KINDS;
  UNIVERSAL,ASSIGNS,VAR_PARMS,CNST_PARMS, WITHED: CONTEXTS;


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
EXPR=10;           CONSTANT=11;        SAVE_PARM=12;       NEW_PARM=13;
TAG_FIELD=14;      WITH_CONST = 15;    WITH_VAR = 16;
  }
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
 13: result:=NEW_PARM;
 14: result:= TAG_FIELD;
 15: result:=WITH_CONST;
 16: result:= WITH_VAR;

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
      NEXT_ENTRY:=TOP_STACK;
    END;
    TOP_STACK:=THIS_STACK
  END;


{#########}
{INITIALIZE}
{#########}

PROCEDURE INITIALIZE;
  BEGIN
    DONE:=FALSE;
    IF DEBUG THEN PRINTFF((THIS_PASS));
    first(outfile4);
    INIT(outfile5);

    ARITHMETIC:=(.INT_KIND,REAL_KIND.);
    INDEXS:=(.INT_KIND,BOOL_KIND,CHAR_KIND,ENUM_KIND.);
    SMALLS:=INDEXS + (.REAL_KIND,SET_KIND,POINTER_KIND.);
    NONLISTS:=INDEXS + (.REAL_KIND,SET_KIND,STRING_KIND,NONLIST_KIND.);
    LARGES:=(.STRING_KIND,NONLIST_KIND,LIST_KIND.);
    INDIRECTS:=LARGES;
    ROUTINE_MODES:= (.PROC_MODE,PE_MODE,CE_MODE,ME_MODE.);
    UNIVERSAL:=(.UNIV_VAR,UNIV_CONST.);
    ASSIGNS:=(.FUNC_RESULT,VARIABLE,VAR_PARM,UNIV_VAR, WITH_VAR.);
    POINTERS:=(.POINTER_KIND,UNDEF_KIND.);
    WITHED:= (.WITH_CONST, WITH_VAR.);
    CNST_PARMS:=(.CONST_PARM,UNIV_CONST.);
    VAR_PARMS:=(.VAR_PARM,UNIV_VAR,NEW_PARM.);
    S:=NIL; T:=NIL; NEW(EMPTY_STACK); TOP_STACK:=EMPTY_STACK;
    WITH EMPTY_STACK^ DO BEGIN
      NEXT_ENTRY:=NIL; OPND:=NIL;
    END;
    WITH INT_EXPR DO BEGIN
      KIND:=INT_KIND; NOUN:=XINTEGER; LENGTH:=WORDLENGTH;
      MODE:=UNDEF_MODE;
      CLASS_:=VALUE; CONTEXT:=EXPR; STATE:=EXPRESSION
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
      ELSE
      begin
       write(errors,' ', MESSAGE2,' ',THIS_PASS,' ',ERROR);
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
       write(errors,' ', MESSAGE2,' ',THIS_PASS,' ',ERROR);
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
        UNDEF_KIND: TTYP:=WORD_TYP;
        REAL_KIND: TTYP:=REAL_TYP;
        CHAR_KIND: IF LENGTH=WORDLENGTH THEN TTYP:=WORD_TYP
          ELSE TTYP:=BYTE_TYP;
        SET_KIND: TTYP:=SET_TYP;
        STRING_KIND,NONLIST_KIND,LIST_KIND: TTYP:=STRUCT_TYP;
        GENERIC_KIND,ROUTINE_KIND: BEGIN
          ERROR1(TYPE_ERROR); TTYP:=WORD_TYP END
      END
  END;

  FUNCTION COMPATIBLE:BOOLEAN;
  {VAR RESULT1:BOOLEAN; }
  BEGIN
    IF (T^.CLASS_ <> VALUE) OR (S^.CLASS_ <> VALUE) THEN RESULT:= FALSE ELSE
    IF T^.CONTEXT IN UNIVERSAL THEN
      RESULT:=(S^.KIND IN NONLISTS) AND (T^.LENGTH=S^.LENGTH)
    ELSE
    IF T^.KIND=S^.KIND THEN
      CASE T^.KIND OF
        INT_KIND,REAL_KIND,BOOL_KIND,CHAR_KIND,
        ENUM_KIND,NONLIST_KIND,LIST_KIND: RESULT:=T^.NOUN=S^.NOUN;
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
        ZINDEX: RESULT:=S^.KIND IN INDEXS
      END
    ELSE RESULT:=FALSE;
    IF NOT RESULT THEN ERROR2(TYPE_ERROR);
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
   x:integer;
  BEGIN
    PUSH;
    WITH T^ DO BEGIN
      get_l(outfile4, {MODE} X);
      MODE:=IntToMOde(X);
      get_l(outfile4,DISP);
      CLASS_:=ROUTINE;
      get_l(outfile4, PARM_SIZE);
      get_l(outfile4, VAR_SIZE);
     END

  END;

  PROCEDURE BODY;
  BEGIN
    ROUTINE_;
    WITH T^ DO BEGIN
      PUT5(ENTER2,MODE,DISP,PARM_SIZE,VAR_SIZE,0);
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
      IF class_=VALUE THEN BEGIN
        CASE STATE OF
          DIRECT:
          BEGIN
            IF MODE=SCONST_MODE THEN ADDR_ERROR
            ELSE PUT2(PUSHADDR2,MODE,DISP);
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
      class_:=VALUE;
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

  PROCEDURE STORE(STORE_WHAT:STORE_CLASS);
  VAR TYP:INTEGER; SIMILAR:BOOLEAN; CLEAR_LENGTH:DISPLACEMENT;
  BEGIN
    IF STORE_WHAT=STORE_TAG THEN
    GET_L (outfile4,CLEAR_LENGTH);
    {EXPRESSION} VALUE_;
    SIMILAR:=COMPATIBLE;
    POP {EXPRESSION};
    IF SIMILAR THEN WITH T^ DO
      IF CONTEXT IN ASSIGNS THEN BEGIN
        TYP:=TTYP;
        IF STORE_WHAT<>STORE_TAG THEN
        IF TYP=STRUCT_TYP THEN PUT1(COPY2,LENGTH)
        ELSE PUT1(ASSIGN2,TYP)
        ELSE PUT1(ASSIGNTAG2,CLEAR_LENGTH)
      END ELSE ERROR1(ASSIGN_ERROR);
    IF STORE_WHAT<>STORE_FOR THEN POP {VARIABLE}
   END;

  {#########}
{STATEMENTS}
{#########}
  PROCEDURE VAR_REF;
  var
   X,Y:integer;
  BEGIN
    WITH T^ DO BEGIN
      class_:=VALUE;
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
      IF class_=ROUTINE THEN
        IF MODE=STD_MODE THEN PUT1(PROCEDURE2,DISP)
        ELSE PUT3(CALL2,MODE,DISP,PARM_SIZE);
    POP
  END;

  PROCEDURE CALL_NEW;
  BEGIN
    IGNORE2(NEW2);
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
      IF NOT (S^.CONTEXT IN ASSIGNS) THEN ERROR2(ASSIGN_ERROR);
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
    STORE(STORE_FOR);
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
        STRING_KIND,NONLIST_KIND,LIST_KIND: PUT2(COMPSTRCT2,OP,T^.LENGTH);
        GENERIC_KIND,UNDEF_KIND,ROUTINE_KIND: ERROR2(TYPE_ERROR)
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
        POINTER_KIND,GENERIC_KIND,LIST_KIND,NONLIST_KIND,
        UNDEF_KIND,ROUTINE_KIND: ERROR2(TYPE_ERROR)
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
        SET_KIND,POINTER_KIND,LIST_KIND,NONLIST_KIND,
        ROUTINE_KIND,UNDEF_KIND: ERROR2(TYPE_ERROR)
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
      IF (class_ = ROUTINE) AND (MODE <> STD_MODE)
      THEN PUT2(FUNCVALUE2, MODE, TTYP)
  END;

  PROCEDURE CALL_FUNC;
  BEGIN
    WITH S^ DO
     IF class_ = ROUTINE THEN
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
    {RECORD OR CLASS} ADDRESS;
    SAVE_CONTEXT:=T^.CONTEXT;
    VAR_REF;
    TYPE_;
    WITH T^ DO BEGIN
      PUT1(FIELD2,DISP);
      STATE:=ADDR;
      IF CONTEXT=VARIABLE THEN
       CONTEXT:=ENTRY_VAR
      ELSE
       CONTEXT:=SAVE_CONTEXT;
     END;
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
 CALL_NEW1: CALL_NEW;
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
 INITVAR1: IGNORE1(INITVAR2);
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
 RANGE1: IGNORE2(RANGE2);
 RESULT1: RESULT;
 ROUTINE1: ROUTINE_;
 SAVEPARM1: CONSTPARM(TRUE);
 SLASH1: SLASH;
 STAR1: PLUS_MINUS_STAR(MUL2);
 STORE1: STORE(STORE_USUAL);
 SUB1: SUB;
 TAG_STORE1: STORE(STORE_TAG);
 UMINUS1: UMINUS;
 UNDEF1: UNDEF;
 UPLUS1: UPLUS;
 VALUE1: VALUE_;
 VARIANT1: IGNORE2(VARIANT2);
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
