! (c) 2018 by Carl Barnes
      Member()
      Include('Equates.CLW'),ONCE
      Include('Keycodes.CLW'),ONCE
      Include('Errors.CLW'),ONCE

      Map
        module ('win32')
          ppOutputDebugString(*CString), raw, pascal, name('OutputDebugStringA'),Dll(dll_mode)
          ppGetwindowRect(Long hWnd, *Group pRect),bool,raw,pascal,proc,name('GetWindowRect')
        end
      End ! map
      Include('CBCodeParse.inc'),ONCE

!--------------------------------------------------------------------------------------------------
CBCodeFlattenClass.Init    PROCEDURE  ()
  CODE  
  Message('CodeFlattenCls.Init')   
  RETURN
!--------------------------------------------------------------------------------------------------
CBCodeFlattenClass.TrimText1310     PROCEDURE(*STRING Txt, BOOL AddOne1310toEnd=1)  !Remove 13,10,32,9 Leading or Trailing
Ndx LONG,AUTO
    CODE
    LOOP Ndx=1 TO SIZE(Txt)                     !10/14/17 remove all tabs seems like a good idea
         IF VAL(Txt[Ndx]) = 9 THEN Txt[Ndx]=''. !10/14/17 I think this is done later and NOT needed here
    END

    LOOP Ndx=1 TO SIZE(Txt)             !Remove leading 13,10,9,spaces
         IF VAL(Txt[Ndx]) > 32 THEN     !Found first non-space,9,13,10
            Ndx -= 1                    !back up to last 13,10,9,32
            IF Ndx > 0 THEN
               CLEAR(Txt[1 : Ndx])      !Blank out leading  13,10,9,32
            END
            BREAK
         END
    END
    Txt=LEFT(Txt)                       !Remove leading spaces
    LOOP Ndx=SIZE(Txt) TO 1 BY -1       !Remove trailing 13,10
         IF VAL(Txt[Ndx]) > 32 THEN     !Found the last good character > Space
            Ndx += 1                    !move to the space after
            IF Ndx < SIZE(Txt) AND AddOne1310toEnd THEN
               Txt[Ndx : SIZE(Txt)]='<13,10>'
            ELSE   
               Txt[Ndx : SIZE(Txt)]='' 
            END
            BREAK
         END
    END
    RETURN
!--------------------------------------------------------------------------------------------------
CBCodeFlattenClass.TrimText1310NoLeft PROCEDURE(*STRING Txt, BOOL AddOne1310toEnd=1)  !Remove Leading "<13,10>" leave leading spaces
Ndx         LONG,AUTO
LenTxt      LONG,AUTO
Last1310    LONG
    CODE
    LenTxt = LEN(CLIP(Txt)) 
    LOOP Ndx=1 TO LenTxt                        !remove all tabs seems like a good idea
         IF VAL(Txt[Ndx]) = 9 THEN Txt[Ndx]=''.
    END
    IF ~LenTxt OR ~Txt THEN RETURN.
    
    LOOP Ndx=1 TO LenTxt                !Remove leading Blanl<13,10> lines
         CASE VAL(Txt[Ndx]) 
         OF 13 OROF 10 
            Last1310 = Ndx 
            Txt[Ndx]=''                 !Blankleading 13,10 incse we never find a character
         END
         IF VAL(Txt[Ndx]) > 32 THEN     !Found first non-space,9,13,10
!            Ndx -= 1                    !back up to last 13,10,9,32
!            IF Ndx > 0 THEN
!               CLEAR(Txt[1 : Ndx])      !Blank out leading  13,10,9,32
!            END

            IF Last1310 THEN 
               Txt = Txt[ Last1310+1 : LenTxt]  !shift left to remove leading lines
            END 
            BREAK
         END
    END
    !NO Txt=LEFT(Txt)                       !Remove leading spaces - LEFT
    LOOP Ndx=LenTxt TO 1 BY -1       !Remove trailing 13,10
         IF VAL(Txt[Ndx]) > 32 THEN     !Found the last good character > Space
            Ndx += 1                    !move to the space after
            IF Ndx < SIZE(Txt) AND AddOne1310toEnd THEN
               Txt[Ndx : SIZE(Txt)]='<13,10>'
            ELSE   
               Txt[Ndx : SIZE(Txt)]='' 
            END
            BREAK
         END
    END
    RETURN    
!--------------------------------------------------------------------------------------------------
CBCodeFlattenClass.TabsToSpaces     PROCEDURE(*STRING Txt, BOOL AddOne1310toEnd=1)  !Remove 13,10,32,9 Leading or Trailing
Ndx LONG,AUTO
    CODE
    LOOP Ndx=1 TO SIZE(Txt)                     !10/14/17 remove all tabs seems like a good idea
         IF VAL(Txt[Ndx]) = 9 THEN Txt[Ndx]=''. !10/14/17 I think this is done later and NOT needed here
    END
    IF ~AddOne1310toEnd THEN RETURN.
    LOOP Ndx=SIZE(Txt) TO 1 BY -1       !Remove trailing 13,10
         IF VAL(Txt[Ndx]) > 32 THEN     !Found the last good character > Space
            Ndx += 1                    !move to the space after
            IF Ndx < SIZE(Txt) AND AddOne1310toEnd THEN
               Txt[Ndx : SIZE(Txt)]='<13,10>'
            ELSE   
               Txt[Ndx : SIZE(Txt)]='' 
            END
            BREAK
         END
    END
    RETURN
!--------------------------------------------------------------------------------------------------

CBCodeFlattenClass.Flatten2    PROCEDURE(*STRING WinTxt)
LnMapQ  QUEUE(CBFlatLineMapQType)
        END
    CODE
    SELF.Flatten2(WinTxt, LnMapQ)
    RETURN
!--------------------------------------------------------------------------------------------------
CBCodeFlattenClass.Flatten2    PROCEDURE(*STRING WinTxt, CBFlatLineMapQType LnMapQ )
LenTxt     LONG,AUTO
Ndx        LONG,AUTO
OutNdx     LONG
InQuote             BOOL            !Am I between 'Quotes'?
InContinuation      BOOL            !Did I run into a |
InComment           BOOL            !Did I run into a !    
NdxLine1    LONG(1)          !10/25 this will not get good text because overwrites input, would need to save original
Is13        BOOL            !10/25 
WinOrig     &STRING !was--> LIKE(WinTxtInput),AUTO          !KILL ME
    CODE
    WinOrig &= NEW(STRING(SIZE(WinTxt)))
    WinOrig = WinTxt                       !KILL ME
    FREE(LnMapQ)                !10/25
    CLEAR(LnMapQ)               !10/25
    LnMapQ.Line1 = 1
    LnMapQ.Line2 = 1

!10/02/18 removing leading 13,10 not good for Just a Bunch of Lines
!10/02/18 IF VAL(WinTxt[1])<=32 THEN SELF.TrimText1310(WinTxt).   !10/25 be sure no leading 13,10,32,9
!10/02/18 
    IF VAL(WinTxt[1])<=32 THEN SELF.TrimText1310NoLeft(WinTxt).   !10/02/18 want to leave leading spaces
!10/02/18 done by above    SELF.TabsToSpaces(WinTxt)   !10/02/18 just want to fix spaces and not have blank on the end
    
    !--Prepare
    LenTxt = LEN(CLIP(WinTxt))

    !--Flatten out the Line Continuations and extra spaces
    InQuote = 0
    OutNdx  = 0  
    LOOP Ndx = 1 TO LenTxt
         IF VAL(WinTxt[Ndx]) = 9 THEN WinTxt[Ndx]=''.       !Changes tabs to spaces

         CASE WinTxt[Ndx]
         OF '|' 
            IF InQuote THEN GOTO BreakCaseLabel: .          !20171017 have | in FORMAT('|')
            InContinuation = True
         OF '!'
            IF InQuote THEN GOTO BreakCaseLabel: .          !20171017 comment in quotes is not a comment
            InComment = True

!wrong place, Flat should have &
!         OF '&'
!            !Crude, toss out & of hot keys, gets some & it should not but who cares, this is for spell check
!            IF InQuote AND Ndx < LenTxt AND VAL(WinTxt[Ndx+1]) > 32 THEN CYCLE.   !if & is not next to space then toss it
!            !Better might be to do this from just PROMPT RADIO OPTION BUTTON

         OF CHR(39)                     !a Quote e.g. PROMPT('Don''t fear the reaper'),AT(1,1),USE(?feq1),HIDE
            IF InComment THEN CYCLE.    ! a comment with Quotes just confuses
            IF ~InQuote                 !Not currently inbetween 'quotes'
                InQuote = True          !   then set flag to true
                IF OutNdx > 1 THEN
                   !db('flatten',WinTxt[OutNdx - 3 : OutNdx] )
                   IF WinTxt[OutNdx - 1 : OutNdx] = '''&' THEN      !change 'xxxx'&'yyyyy' to 'xxxxyyyyy'
                      OutNdx -= 2
                      CYCLE
                   END
                END
!                IF OutNdx > 3 THEN
!                   !db('flatten',WinTxt[OutNdx - 3 : OutNdx] )
!                   IF WinTxt[OutNdx - 3 : OutNdx] = ''' & ' THEN      !change 'xxxx' & 'yyyyy' to 'xxxxyyyyy'
!                      OutNdx -= 4
!                      !db('flatten','New end:' & WinTxt[OutNdx - 3 : OutNdx] )
!                      CYCLE
!                   ELSIF WinTxt[OutNdx - 2 : OutNdx] = '''& '    |      !change 'xxxx'& 'yyyyy' to 'xxxxyyyyy'
!                      OR WinTxt[OutNdx - 2 : OutNdx] = ''' &' THEN      !change 'xxxx' &'yyyyy' to 'xxxxyyyyy'
!                      OutNdx -= 3
!                      CYCLE
!                   END
!               END
            ELSE                    !I am in 'quotes now'
                IF Ndx < LenTxt AND WinTxt[Ndx+1] = CHR(39) THEN   !is is a Doubled quote e.g.  PROMPT('Don''t fear the reaper')
                   OutNdx += 1
                   WinTxt[OutNdx] = WinTxt[Ndx]                    !Take this Quote
                   Ndx += 1                                        !and below keep the next quote
                   InQuote = True                                  !just to make it clear we remain in quotes
                ELSE
                   InQuote = False
                END
            END

         OF CHR(32)
            IF ~InQuote                               !do not keep some spaces outside quotes
                IF OutNdx THEN
                   CASE VAL(WinTxt[OutNdx])     !look at Last character we saved
                   OF 32  ; CYCLE               !Double space then toss it
                   OF 2Ch ; CYCLE               !comma space then toss it
                   OF 28h ; CYCLE               !( space then toss it
                   OF 29h ; CYCLE               !) space then toss it
                   OF 39  ; CYCLE               !' space then toss it
                   OF 38  ; CYCLE               !& space then toss it
                   !preserve 1 space so indented  OF 10  ; CYCLE               !<13,10> space then toss it
                   END
                ELSE
                   !10/02/18 CYCLE   !leading spaces get trimmed
                   !I want leading spaces
                END
            END

         OF CHR(13) OROF CHR(10)
            InComment = False
            InQuote = False                                         !Quotes cannot span lines so force this off cause it must be base code
            IF ~OutNdx THEN Message('Leading 13,10 cannot happen??').                                  !Leading 13,10s get trimmed
            IF ~OutNdx THEN CYCLE.                                  !Leading 13,10s get trimmed

            Is13 = CHOOSE(VAL(WinTxt[Ndx])= 13)           !10/25
            IF Is13 THEN                                  !10/25
               LnMapQ.Txt = LEFT(SUB(WinTxt,NdxLine1,99))       !10/25
               LnMapQ.Txt = LEFT(SUB(WinOrig,NdxLine1,99))       !10/25   !KILLME
               NdxLine1 = Ndx+2
               ADD(LnMapQ)                                !10/25
               LnMapQ.Line1  += 1                         !10/25
               LnMapQ.Line2  += 1                         !10/25
            END 
            IF VAL(WinTxt[Ndx])= 10 THEN NdxLine1 = Ndx+1. 
           
            IF OutNdx > 2 AND Ndx + 1 < LenTxt THEN
                IF  WinTxt[OutNdx-1 : OutNdx] = '<13,10>' |         !have 13,10 at end of good text
                AND WinTxt[Ndx      : Ndx+1 ] = '<13,10>' THEN      ! and 13,10 at start
                   Ndx += 1                                         !Skip this 13,10 and catch it next time 
                   NdxLine1 = Ndx+1
                   CYCLE
                END
            END

!            IF Ndx + 3 < LenTxt THEN
!                IF WinTxt[Ndx+1 : Ndx+3] = '<10,13,10>' THEN        !is it 13,10,13,10
!                   Ndx += 1                                         !Skip this 13,10 and catch it next time
!                   CYCLE
!                END
!            END

            IF InContinuation THEN                          !I had a |
                InContinuation = False
                IF Ndx < LenTxt THEN                        !is there more text
                   CASE VAL(WinTxt[Ndx+1])                  ! look at Next character
                   OF 13 OROF 10                            !   is it the 10 after the 13
                      Ndx += 1                              !     then skip it too
                   END
                END
                NdxLine1 = Ndx+1
                LnMapQ.Line2  -= 1                         !10/25
                CYCLE                                       !toss the 13/10 character after the |
            END
            
            !LnMapQ.Line2 += 1       !10/25
         END
         
BreakCaseLabel:
         IF InContinuation OR InComment THEN CYCLE.             !toss these characters
        

         OutNdx += 1
         WinTxt[OutNdx] = WinTxt[Ndx]                    !Take this Quote

    END !LOOP LOOP Ndx

    IF OutNdx < LenTxt THEN CLEAR( WinTxt[OutNdx+1 : LenTxt]).   

    LnMapQ.Txt = LEFT(SUB(WinTxt,NdxLine1,99))      !10/25
    LnMapQ.Txt = LEFT(SUB(WinOrig,NdxLine1,99))     !10/25   !KILLME
    ADD(LnMapQ)                                     !10/25  !The Last Line
    
    SORT(LnMapQ,LnMapQ.Line2,LnMapQ.Line1)               !10/25
    DISPOSE(WinOrig)
    RETURN

!====================================================================================
!!CBCodeParseClass    Class(),Type,Module('CBCodeParse.Clw'),DLL(0),Link('CBCodeParse.Clw',1) 
!CBCodeParseClass.FindAttrParen   PROCEDURE(STRING CodeTxt, STRING AttName, *LONG BegAttPos, *LONG BegParen, *LONG EndParen) !,BOOL
!BegQuote  LONG,AUTO
!EndQuote  LONG,AUTO
!    CODE
!    RETURN SELF.FindAttrParen(CodeTxt, AttName, BegAttPos, BegParen, EndParen, BegQuote, EndQuote) 
!-----------------------------
!CBCodeParseClass.FindAttrParen   PROCEDURE(STRING CodeTxt, STRING AttName, *LONG BegAttPos, *LONG BegParen, *LONG EndParen, *LONG BegQuote, *LONG EndQuote) !,BOOL
CBCodeParseClass.FindAttrParen   PROCEDURE(STRING CodeTxt, STRING AttName, <*LONG _BegAttPos>, <*LONG _BegParen>, <*LONG _EndParen>, <*LONG _BegQuote>, <*LONG _EndQuote>) !,BOOL
LenCodeTxt  LONG,AUTO 
LenAttName  LONG,AUTO 
RetBool     BOOL
InQuote     LONG
XX          LONG,AUTO
AttBeg      LONG,AUTO
AttEnd      LONG,AUTO
BegAttPos   LONG
BegParen    LONG
EndParen    LONG 
BegQuote    LONG
EndQuote    LONG
    CODE
    CodeTxt=UPPER(CodeTxt)
    AttName=UPPER(AttName)
    LenCodeTxt=LEN(CLIP(CodeTxt))
    LenAttName=LEN(CLIP(AttName))
!    BegAttPos=0 ; BegParen=0 ; EndParen=0 ; BegQuote=0 ; EndQuote=0
    
! LIST,AT(4,16,447,257),USE(?Browse:1),IMM,HVSCROLL,MSG('Browsing Records'),FORMAT('25R(6)|M~Code~C(0)@n3@[160L(2)|M@s40@/160L(2)|M@s40@](167)|M~Description~44C|M~A' &|
!     'ssess Pen.~@s1@32C(2)|M~Pen. Cd.~C(0)@n3@35C|M~Rev. Cat.~@s1@41C|M~Pymt Alloc~@s' &|
!     '1@26C|M~Active~@s1@4C|M~Add To Bal.~@s1@'),FROM(Queue:Browse:1),#SEQ(1),#ORIG(?List),#FIELDS(BillCds:BillCode,BillCds:Desc1,BillCds:Desc2,BillCds:AssessPenalty,BillCds:PenaltyCode,BillCds:RevCategory,PymtAlloc,BillCds:Active,BillCds:AddToBalan
       
    
    LOOP XX = 1 TO LenCodeTxt
        ! IF _Line[LnX] = ')' AND ~InQuote THEN BREAK.   
         IF CodeTxt[XX] = CHR(39) THEN       !is it a Quote
            InQuote = 1 - InQuote
            IF BegAttPos AND ~BegQuote THEN BegQuote = XX.
            IF BegAttPos               THEN EndQuote = XX.
         END
!         EndPos = LnX
         IF InQuote THEN CYCLE. 
         
         IF ~BegAttPos THEN                     ! 1234567
             IF CodeTxt[XX]<>'(' THEN CYCLE.    !,FORMAT( 
             AttEnd = XX     - 1                !      ^  -1 
             LOOP
                IF AttEnd<1 OR CodeTxt[AttEnd]<>'' THEN BREAK.  !move past space between ATTNAME and (  e.g  FORMAT (
                AttEnd -= 1
             END 
             AttBeg = AttEnd + 1 - LenAttName           ! 654321 
             IF AttBeg < 1 THEN CYCLE.          !^        need 2 so room for the Delim               
             IF LEFT( CodeTxt[ AttBeg : AttEnd ] ) <> AttName THEN CYCLE.
             IF AttBeg=1 OR CodeTxt[AttBeg-1]=',' OR CodeTxt[AttBeg-1]=' ' THEN
                BegAttPos = AttBeg 
                BegParen  = XX
             END 
             CYCLE 
         END 
         !We are inside (
         IF CodeTxt[XX]<>')' THEN CYCLE. 
         EndParen = XX
         RetBool = True 
         BREAK
    END !Loop

    IF ~OMITTED(_BegAttPos) THEN _BegAttPos=BegAttPos.
    IF ~OMITTED(_BegParen ) THEN _BegParen =BegParen.
    IF ~OMITTED(_EndParen ) THEN _EndParen =EndParen. 
    IF ~OMITTED(_BegQuote ) THEN _BegQuote =BegQuote.
    IF ~OMITTED(_EndQuote ) THEN _EndQuote =EndQuote.
    RETURN RetBool
!--------------------------------------------------------------------------------------------------
CBCodeParseClass.FindCommaOrParen   PROCEDURE(STRING CodeTxt, <*STRING OutCharFound>)!,LONG
Comma1    LONG    
Paren1    LONG 
    CODE
    Comma1=INSTRING(',',CodeTxt,1)       !LINE,AT(0
    Paren1=INSTRING('(',CodeTxt,1)       !STRING(@D1),
    IF Paren1 AND Paren1 < Comma1 THEN 
       Comma1 = Paren1
    END
    IF ~OMITTED(OutCharFound) THEN
       OutCharFound = SUB(Codetxt,Comma1,1)
    END
    RETURN Comma1
!--------------------------------------------------------------------------------------------------
CBCodeParseClass.ParseAttributes   PROCEDURE(CONST *STRING InTxt, LONG BegParen, LONG EndParen, *STRING[] Parms, *LONG[,] Slices, <*LONG[] CommaPos>)!,LONG  !Returns Slice Count
SliceCnt    LONG,AUTO
Idx        LONG,AUTO
    CODE
    IF ~OMITTED(CommaPos[]) THEN
        SliceCnt=SELF.ParseAttributes(InTxt, BegParen, EndParen, Slices[], CommaPos[])
    ELSE         
        SliceCnt=SELF.ParseAttributes(InTxt, BegParen, EndParen, Slices[])
    END           
    CLEAR(Parms[])
    LOOP Idx = 1 TO SliceCnt
         IF Slices[Idx,1] AND Slices[Idx,1]<=Slices[Idx,2] THEN     !AT(,,w,h) would return bad slices
            Parms[Idx] = InTxt[ Slices[Idx,1] : Slices[Idx,2] ]
         END 
    END
    RETURN SliceCnt
!--------------------------------------------------------------------------------------------------
CBCodeParseClass.ParseAttributes   PROCEDURE(CONST *STRING InTxt, LONG BegParen, LONG EndParen, *LONG[,] Slices, <*LONG[] CommaPos>) !,LONG Returns Slice Count
SlicesMax   SHORT,AUTO   !There is 1 more Slice than Comma
SliceCnt    LONG
SliceBeg    LONG,AUTO
LenInTxt    LONG,AUTO
CommaCnt    LONG
CommaMax    LONG(0)
InQuote     SHORT
Idx         LONG,AUTO 
    CODE
    CLEAR(Slices[])    ; SlicesMax=MAXIMUM(Slices[],1) 
    IF ~OMITTED(CommaPos[]) THEN 
        CLEAR(CommaPos[])  ; CommaMax=MAXIMUM(CommaPos[],1)  
    END         
    LenInTxt=LEN(CLIP(InTxt))
    IF MAXIMUM(Slices[],1) < 2 THEN MESSAGE('CBCodeParseClass.ParseAttributes Slices[] must has 2 dimensons').
    IF BegParen < 1 OR BegParen >= EndParen                   |     !If Sent AT() return Zero and no slices
    OR EndParen > LenInTxt  THEN 
        RETURN(0)
    END        
    SliceBeg = BegParen + 1
    LOOP Idx= BegParen + 1 TO EndParen 
         IF InTxt[Idx]=CHR(39) THEN  InQuote=1-InQuote.
         IF InQuote THEN CYCLE.
         !IF Idx=SliceBeg AND InTxt[Idx]<=CHR(32) THEN SliceBeg=Idx.     !Remove leading spaces before value  NO be perfect
         IF Idx=EndParen OR InTxt[Idx]=',' THEN 
            !What if you have ,, then SLICE is invalid, caller better check
            SliceCnt += 1 
            Slices[SliceCnt,1] = SliceBeg
            Slices[SliceCnt,2] = Idx-1          !this could be invalid slice if AT(,,100,200)
            SliceBeg = Idx + 1
            IF CommaMax THEN        !return ending ")" as a Comma   AND Idx < EndParen THEN 
               CommaCnt += 1
               CommaPos[CommaCnt] = Idx
            END    
         END 
    WHILE SliceCnt < SlicesMax AND (CommaCnt=0 OR CommaCnt < CommaMax) 
    RETURN SliceCnt
!Idx     LONG,AUTO
!NextToken   LONG,AUTO
!LastToken   LONG,AUTO
!GroupIdx    LONG,AUTO
!FieldRef    ANY
!TokenLen    LONG,AUTO
!  CODE                                                     ! Begin processed code
!
!    LastToken = BegParen + 1
!    SliceIdx  = 0
!    TokenLen  = LEN(CLIP(Token))
!?   ASSERT(TokenLen,'ParseTokenStringIntoGroup - No Token')
!    LOOP
!        IF ~SUB(InTxt, LastToken, 999) THEN BREAK.
!        SliceIdx += 1
!        FieldRef &= WHAT(OutGroup, SliceIdx)
!
!        IF Max AND SliceIdx = Max                                       !--If this is the Last Field in the Group
!           FieldRef = SUB(InTxt, LastToken, 999)                     !  take what's left and leave
!           BREAK
!        END
!
!        NextToken = INSTRING( Token[1 : TokenLen] , InTxt , 1 , LastToken )
!        IF ~NextToken                                                   !--if this is the last token
!           FieldRef = SUB(InTxt, LastToken, 999)                     !  take what's left and leave
!           BREAK
!        END
!
!        FieldRef = LEFT(SUB(InTxt, LastToken, NextToken - LastToken ))          !  take what's left and leave
!        LastToken = NextToken + TokenLen
!        IF bSkipBlanks AND ~FieldRef THEN  SliceIdx -= 1.                       !Is this a blank line that I am skipping
!
!    END



!--------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------
CBCodeParseClass.ParseTokenStringIntoGroup FUNCTION (String InTxt, *GROUP OutGroup, STRING Token, SHORT Max, BYTE bSkipBlanks)!,LONG,PROC !rtns count
Idx     LONG,AUTO
NextToken   LONG,AUTO
LastToken   LONG,AUTO
GroupIdx    LONG,AUTO
FieldRef    ANY
TokenLen    LONG,AUTO
  CODE                                                     ! Begin processed code
!(*String InString, *GROUP OutGroup, STRING Token, SHORT Max)
!
    LastToken = 1
    GroupIdx  = 0
    TokenLen  = LEN(CLIP(Token))
?   ASSERT(TokenLen,'ParseTokenStringIntoGroup - No Token')
    LOOP
        IF ~SUB(InTxt, LastToken, 999) THEN BREAK.
        GroupIdx += 1
        FieldRef &= WHAT(OutGroup, GroupIdx)

        IF Max AND GroupIdx = Max                                       !--If this is the Last Field in the Group
           FieldRef = SUB(InTxt, LastToken, 999)                     !  take what's left and leave
           BREAK
        END

        NextToken = INSTRING( Token[1 : TokenLen] , InTxt , 1 , LastToken )
        IF ~NextToken                                                   !--if this is the last token
           FieldRef = SUB(InTxt, LastToken, 999)                     !  take what's left and leave
           BREAK
        END

        FieldRef = LEFT(SUB(InTxt, LastToken, NextToken - LastToken ))          !  take what's left and leave
        LastToken = NextToken + TokenLen
        IF bSkipBlanks AND ~FieldRef THEN  GroupIdx -= 1.                       !Is this a blank line that I am skipping

    END
    RETURN  GroupIdx
