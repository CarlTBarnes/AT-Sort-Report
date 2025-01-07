! (c) 2018 by Carl Barnes

!Better file name  CBClwCode ???
!Better file name  CBClwCode ???
!Better file name  CBClwCode ???
!Better file name  CBClwCode ???

CBFlatLineMapQType  QUEUE,TYPE  !Map of input UN-Flat Lines Line1 to Line2
Line1           LONG
Line2           LONG 
Txt             STRING(100)
            END
            
!--------------------------------------------------------------------------------
CBCodeFlattenClass    Class(),Type,Module('CBCodeParse.Clw'),DLL(0),Link('CBCodeParse.Clw',1)
Init            PROCEDURE ()  
TrimText1310    PROCEDURE(*STRING Txt, BOOL AddOne1310toEnd=1)  !Remove 13,10,32,9 Leading or Trailing
TrimText1310NoLeft PROCEDURE(*STRING Txt, BOOL AddOne1310toEnd=1)  !Remove Leading "<13,10>" leave leading spaces
TabsToSpaces    PROCEDURE(*STRING Txt, BOOL AddOne1310toEnd=1)  !Change Tabs to Spaces

!Flatten 2 made for AtSort to not remove Leading spaces
Flatten2         PROCEDURE(*STRING WinTxt)
Flatten2         PROCEDURE(*STRING WinTxt, CBFlatLineMapQType LnMapQ )
             End

CBCodeParseClass Class(),Type,Module('CBCodeParse.Clw'),DLL(0),Link('CBCodeParse.Clw',1) 
!FindAttrParen       PROCEDURE(STRING CodeTxt, STRING AttName, *LONG BegAttPos, *LONG BegParen, *LONG EndParen),BOOL
!FindAttrParen       PROCEDURE(STRING CodeTxt, STRING AttName, *LONG BegAttPos, *LONG BegParen, *LONG EndParen, *LONG BegQuote, *LONG EndQuote),BOOL
FindAttrParen   PROCEDURE(STRING CodeTxt, STRING AttName, <*LONG BegAttPos>, <*LONG BegParen>, <*LONG EndParen>, <*LONG BegQuote>, <*LONG EndQuote>),BOOL
FindCommaOrParen    PROCEDURE(STRING CodeTxt, <*STRING OutCharFound>),LONG 
ParseTokenStringIntoGroup FUNCTION (String InTxt, *GROUP OutGroup, STRING Token, SHORT Max, BYTE bSkipBlanks),LONG      !Returns count  
ParseAttributes   PROCEDURE(CONST *STRING InTxt, LONG BegParen, LONG EndParen, *STRING[] Parms, *LONG[,] Slices, <*LONG[] CommaPos>),LONG  !Returns Slice Count
ParseAttributes   PROCEDURE(CONST *STRING InTxt, LONG BegParen, LONG EndParen,                  *LONG[,] Slices, <*LONG[] CommaPos>),LONG  !Returns Slice Count
            END
