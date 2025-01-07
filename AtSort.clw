! AtSort by Carl T. Barnes - Copyright (c) 2000 - Released on GitHub under the MIT License   
!---------------------------------------------------------------------------------------------------
!Region ---- About and History ---------------------------------------------------------------------
! REPORT's made in the Designer by dragging fields often end up with the Fields in random order.
! This makes the Tab Order Assistant hard to use. It takes tedious work to get them in Y,X order.
! This utility will sort those controls by Y,X or X,Y for you.
!
! The AT() and USE() can be Aligned which makes it easier to read and review the code. 
!
! This is a tool I wrote over a long time . It started out simple, then was enhanced as I used
! it and formed new ideas. So there was no plan, it just grew so could be "grew-some" code.
!---------------------------------------------------------------------------------------------------
! Change History:
! Circa  2000  Written a long time ago in C5
! 05-Jan-2025  Release on GitHub 
! 
! Future changes:
!
!EndRegion -------------------------------------------------------------------------------
!=============================================================================================
!
![ ] The TEXT control has a Max Line Width of 1024. Can I protect against it during Flatten?  
![ ] Parse find FULL and add to Sort Queue so know Blank W or H missing in AT(X,Y,,) are FULL versus Default  
!#########################################################################
  PROGRAM
  INCLUDE('KEYCODES.CLW')
  INCLUDE('CBCodeParse.INC'),ONCE          

!Region --- To build with WindowPreview class UnComment **WndPrv Lines ---
!!!_CbWndPreview_  EQUATE(1)
!!!    INCLUDE('CbWndPreview.inc'),ONCE       !Download https://github.com/CarlTBarnes/WindowPreview
!!!    COMPILE('!**WndPrv',_CbWndPreview_)
!!!WndPrvCls CBWndPreviewClass,THREAD         !Init puts Secret Flat Button at Window Top
!EndRegion    !**WndPrv 
  
    MAP 
AtSortReport        PROCEDURE(STRING StartConfigGrp, STRING StartReRunGrp)
DB                  PROCEDURE(STRING Info)
PopupUnder          PROCEDURE(LONG CtrlFEQ, STRING PopMenu),LONG    !Position Popup neatly under a button
TextLineCount       PROCEDURE(LONG TextFEQ),LONG  !TEXT Prop:LineCount without trailing blanks
        module ('win32')
          OutputDebugString(*CString), raw, pascal, name('OutputDebugStringA'),Dll(dll_mode)
        end  
    END
  INCLUDE('ATSort_DATA.clw','GLOBAL DATA')  ! All Global Data Declarations    
    CODE
    AtSortReport('','')
    RETURN 

AtSortReport PROCEDURE(STRING StartConfigGrp, STRING StartReRunGrp)
  INCLUDE('ATSort_DATA.clw','AtSortReport Procedure DATA')  ! All Procedure Data Declarations
  INCLUDE('ATSort_Window.clw')                              ! Window WINDOW('AT() Sort REPORT ...) 
  CODE
  IF THREAD()=1 
     IF COMMAND('/paste') THEN
        OrigCode=CLIPBOARD()
     ELSE
        DOO.SetTestRtn
     END
  END
  SYSTEM{7A58h}=1                 !  SYSTEM{PROP:PropVScroll}=1
  SYSTEM{7A7Dh}=MSGMODE:CANCOPY   !  SYSTEM{PROP:MsgModeDefault}=MSGMODE:CANCOPY
  SYSTEM{PROP:FontName}='Segoe UI' ; SYSTEM{PROP:FontSize}=10       !System is Message() Font

  OPEN(Window)
  COMPILE('!** EndWndPrv **',_CbWndPreview_) ; WndPrvCls.Init()  !** EndWndPrv **
  0{PROP:MinWidth}  = 0{PROP:Width}  / 2
  0{PROP:MinHeight} = 0{PROP:Height} / 2
  IF THREAD()>1 THEN UNHIDE(?HaltBtn).
 ! IF StartConfigGrp <> '' THEN ConfigGrp = StartConfigGrp.    !ReRun passes Config in START()
  IF StartConfigGrp <> ''  THEN
     ConfigGrp  = StartConfigGrp
     IF Cfg:HistoryNo THEN 
        GET(HistoryQ,Cfg:HistoryNo)
        OrigCode = HisQ:OrigAny
     END
  END
  IF StartReRunGrp <> ''  THEN
     ReRunGrp  = StartReRunGrp
     SETPOSITION(0,ReRn:WinX+20,ReRn:WinY+20,ReRn:WinW,ReRn:WinH)
  END  
  ?Sheet1{PROP:TabSheetStyle}=1
  ?LIST:HistoryQ{PROP:LineHeight} = 2 + ?LIST:HistoryQ{PROP:LineHeight}
  Cfg:AlignAt_BtnTipOriginal = ?AlignAtBtn{PROP:Tip}
  IF 1 THEN                   !IF 0 leaves Consolas 9
     DOO.FontForCodeRtn(0)    !(0) = Set Default Font for TEXT with Code. This does Consolas 10. So can change Font without changing Window
  END
  ACCEPT
    CASE EVENT() 
    OF EVENT:OpenWindow ; SELECT(?OrigCode) 
    END
    CASE ACCEPTED()
    OF ?PasteBtn    ; PasteANY=CLIPBOARD()
                      IF LEN(PasteANY) >= SIZE(OrigCode) THEN
                         Message('Pasted code was '& LEN(PasteANY) &' bytes||Tool is limited to '& size(OrigCode)-1 &' bytes',0{PROP:Text},icon:asterisk)
                         PasteANY=''
                         CYCLE
                      END 
                      OrigCode=PasteANY    ; POST(EVENT:Accepted, ?SortBtn) ; PasteANY=''
    OF ?CopyOrigBtn ; IF OrigCode THEN SETCLIPBOARD(OrigCode).
    OF ?ClearOrigBtn ; CLEAR(OrigCode) ; DISPLAY ; SELECT(?OrigCode)
    OF ?SortBtn     ;  DISABLE(?CopySortedQBtn) !DISABLE(?CopySortedBtn) ; 
                      DOO.SortRtn()
                      ?AlignAtFlatBtn{PROP:Tip} = Cfg:AlignAt_BtnTipOriginal
                      ?AlignAtBtn{PROP:Tip}     = Cfg:AlignAt_BtnTipOriginal
                      DOO.AddHistoryRtn                     
                      IF RECORDS(SortQ) THEN POST(EVENT:Accepted, ?MakeSortedBtn). ! SELECT(?TabSortQ).
    OF ?MakeSortedBtn ;  DOO.MakeSortedRtn
                         IF SortedCode THEN 
                            ENABLE(?CopySortedQBtn) !ENABLE(?CopySortedBtn) ; 
                         END   
    OF ?CopySortedBtn OROF ?CopySortedQBtn ;  IF SortedCode THEN SETCLIPBOARD(SortedCode).
    OF ?StripSortedBtn  ; DOO.StripClutterRtn(?,?SortedCode,SortedCode)
    OF ?StripFlatBtn    ; DOO.StripClutterRtn(?,?FlatCode  ,FlatCode)
    OF ?AlignAtFlatBtn  ; DOO.AlignAtRtn(?FlatCode,?)    
    OF ?AlignAtBtn      ; DOO.AlignAtRtn(?SortedCode, ?)
    OF ?FontForCodeBtn  ; DOO.FontForCodeRtn(?)                 
    OF ?SortQCopyBtn    ; DOO.SortQCopyBtn() 
    OF ?SortQSortBtn    ; DOO.SortQSortBtn() 
    OF ?AlignQCopyBtn   ; DOO.AlignQCopyBtn() 
    OF ?ReportTestFlatBtn   ; RptTestCls.GenerateCode(?,FlatCode) 
    OF ?ReportTestSortedBtn ; RptTestCls.GenerateCode(?,SortedCode) 
    
    OF ?LIST:HistoryQ
        GET(HistoryQ, CHOICE(?LIST:HistoryQ)) ; IF ERRORCODE() THEN CYCLE.
        CASE KEYCODE()
        OF MouseLeft2 
           OrigCode = HisQ:OrigAny
           SELECT(?OrigCode)
           POST(EVENT:Accepted,?SortBtn)
        OF MouseRight
            SETKEYCODE(0)
            CASE popup('Set Original Tab to History Code and Process|-|Copy History Code to Clipboard|Copy History Code and Run Another At() Sort|-|Start New Thread with History Code')
            OF 1 ; SETKEYCODE(MouseLeft2) ; POST(EVENT:Accepted,?LIST:HistoryQ)             
            OF 2 ; SETCLIPBOARD(HisQ:OrigAny) 
            OF 3 ; SETCLIPBOARD(HisQ:OrigAny) ; RUN(COMMAND('0')&' /paste')
            OF 4 ; DOO.NewThreadRun(POINTER(HistoryQ))
            END 
        END 

    OF ?RunAgainBtn  ; RUN(COMMAND('0'))
    OF ?NewThreadBtn ; DOO.NewThreadRun()
    OF ?HaltBtn      ; IF 1=Message('Terminate AT() Sort Tool? {20}',0{PROP:Text},ICON:Hand,'Keep Open|Halt Tool') THEN CYCLE.
                       LOOP Ndx=1 TO 64 ; POST(EVENT:CloseWindow,,Ndx,1) ; END        

    END
  END
  RETURN 
  
!-------------------------------------------------------------------
DOO.SortRtn PROCEDURE()   !Flatten code, then parse into SortQ 
INX         LONG    
Comma1      LONG    
Paren1      LONG 
CbCodeCls   CBCodeParseClass
AtTxt       STRING(64)
ATPos1      LONG
ATParen1    LONG
ATParen2    LONG
InParent    SHORT   

!MaxLevel    EQUATE(10)                  !more levels 
ParentQ     QUEUE,PRE(ParQ)
pSort            PSTRING(12*20)  !ParQ:pSort   !20 levels deep max
            END
ParentCntReport LONG            
ParentCntWindow LONG            
DbSrt           SHORT       !Debug msg Sort            
ALineWasLEFT    BOOL
ATPOS       LONG,DIM(4,2) 
ATparms     STRING(5),DIM(4) 
ATcma       LONG,DIM(4) 
ParmCnt     LONG         
Dbg         BSTRING 
RightX      LONG
BottomY     LONG
     CODE
    CLEAR(SortedCode) 
    FlatCode=OrigCode  
    FlattenCls.Flatten2(FlatCode, FlatMapQ)  
    DISPLAY
    FREE(SortQ)
    SortByCls.Init()
    CLEAR(RptTestCls.BoundBox)     
    LOOP LineX=1 TO ?FlatCode{PROP:LineCount}
         ALine = ?FlatCode{PROP:Line, LineX}
         IF ~ALine THEN CYCLE.
         ALineWasLEFT=CHOOSE(ALine[1]>=':')        !If line starts with 
         ALine = LEFT(ALine)
         
        CLEAR(SortQ)
        SortQ:LineNo      = LineX
        SortQ:CodeLine    = ALine  
        SortQ:SortCodeLne = lower(ALine)
        ULine=UPPER(ALine)

        !Region: Find Control type as first thing on line, or after column 1 label
        Comma1=INSTRING(',',ULine,1)       !LINE,AT(0
        Paren1=INSTRING('(',ULine,1)       !STRING(@D1),
        IF ~Paren1 AND ~Comma1 THEN Comma1=INSTRING(' ',ULine,1).  !This would be END or .
        IF Paren1 AND Paren1 < Comma1 THEN  !Found ( before , e.g. GROUP(), 
           Comma1 = Paren1
        END
        IF ALineWasLEFT THEN                         !Have a Label in Column 1? Detail1 DETAIL,AT(,,,1740)
           INX=INSTRING(' ',ULine,1)                 !Start After Label
           SortQ:Control = LEFT(SUB( SUB(ULine,1,Comma1-1) ,Inx,99) )     !Inner SUB() takes to , or (, then outside SUB() cuts off Label
        ELSE
           SortQ:Control = SUB(ULine,1,Comma1-1)     !No Label in Column 1
        END
        IF SortQ:Control='.' THEN SortQ:Control='END'.
        !endRegion
        !Region: Find AT()
!CBCodeParseClass.FindAttrParen   PROCEDURE(STRING CodeTxt, STRING AttName, *LONG BegAttPos, *LONG BegParen, *LONG EndParen, *LONG BegQuote, *LONG EndQuote) !,BOOL
        !STRING('ALL PAY'),AT(3708,448)
        IF CbCodeCls.FindAttrParen(ULine, 'AT', ATPos1, ATParen1, ATParen2, Q1#, Q2#)
           IF ATParen1 AND ATParen1+1 <= ATParen2-1 THEN     !Make sure not empty AT()
              AtTxt = ULine[ATParen1+1 : ATParen2-1]
              SortQ:ATCode = AtTxt 
              SortQ:ATSort = AtTxt 
              ParmCnt = CbCodeCls.ParseAttributes(ULine, ATParen1, ATParen2, ATparms[], ATPOS[], ATcma[]) 
              IF ParmCnt >= 1 THEN SortQ:ATX = ATparms[1].
              IF ParmCnt >= 2 THEN SortQ:ATY = ATparms[2].
              IF ParmCnt >= 3 THEN SortQ:ATW = CLIP(LEFT(ATparms[3])). ; SortQ:ATW=RIGHT(SortQ:ATW,8)   !Right String to Sort right
              IF ParmCnt >= 4 THEN SortQ:ATH = CLIP(LEFT(ATparms[4])). ; SortQ:ATH=RIGHT(SortQ:ATH,8)
!TODO: parse find FULL and add to Queue so know Blank W or H missing in AT(X,Y,,) are FULL ?              
           END !IF AT( )  
        END !IF FindAttr AT()
        IF CbCodeCls.FindAttrParen(ULine, 'USE', ATPos1, ATParen1, ATParen2)        !Find USE(xxx) 
           IF ATParen1 AND ATParen1+1 <= ATParen2-1 THEN        !Make sure not empty USE()
              SortQ:UseFEQ = ALine[ATParen1+1 : ATParen2-1]     !Take ALine UpLow USE
              SortQ:SortUseFEQ = lower(SortQ:UseFEQ)
           END   
        END !IF FindAttr
        SortQ:Area = ABS( CHOOSE(SortQ:ATW=0,1,0+SortQ:ATW) * CHOOSE(SortQ:ATH=0,1,0+SortQ:ATH) ) 
        RightX  = SortQ:ATX + CHOOSE(SortQ:ATW<=0,100,0+SortQ:ATW )
        BottomY = SortQ:ATY + CHOOSE(SortQ:ATY<=0,100,0+SortQ:ATH )
        IF RptTestCls.BoundBox.MaxX < RightX  THEN RptTestCls.BoundBox.MaxX = RightX .
        IF RptTestCls.BoundBox.MaxY < BottomY THEN RptTestCls.BoundBox.MaxY = BottomY.

        SortQ:ATSort = SortByCls.FmtSortBy(SortQ:ATX, SortQ:ATY)
        !endRegion
        !Region: Code with Label in Column1 
        CASE SortQ:Control
        OF   'DETAIL'
        OROF 'HEADER' 
        OROF 'FOOTER'
        OROF 'BREAK'        
        OROF 'REPORT'
              ParentCntReport += 1
              SortQ:ATSort  =             '     -     -'
              SortQ:ATSort  =             '!Par! '&FORMAT(LineX,@n_6)   !Sort together?
        END                             ! '123456123456'
        !endRegion
        ADD(SortQ)
    END !Loop LineX=1 TO ?FlatCode
    IF Dbg ; SETCLIPBOARD(Dbg) ; Message('Slice data on Clp in DOO.SortRtn()') ; end
    IF ParentCntReport AND Cfg:SortByXYCU <> 0 THEN
       LOOP 1 TIMES 
          CASE Message('Your code contains Report stuctures HEADER DETAIL FOOTER.' & |
                       '||This tool can only process the Controls from ONE Report' & |
                       '|Stucture unless you select "No Sort" as the Sort Method.' & |
                       '','AT() Sort',ICON:Asterisk,'Close|No Sort|Ignore')
          OF 2 ; Cfg:SortByXYCU=0
          OF 3 ; BREAK
          END
          SORT(SortQ,SortQ:LineNo)     !Force No Sort so it does not destroy, not really needed?  
          DISPLAY
          RETURN 
       END
    END
    
    IF Cfg:SortByXYCU <> 0 THEN             !Sort by Original do not do Parents, but it does work to Indent by Level
       DO ParentSort_Experiment_Rtn
    END 
    IF Cfg:SortByXYCU THEN SORT(SortQ,SortQ:ATSort,SortQ:LineNo).
    SELECT(?FlatCode)
    DISPLAY 

ParentSort_Experiment_Rtn ROUTINE
    !Region Nested Sort ----------- Sort -- Not Sure this works, but seems to
    DbSrt = 0  ;     IF DbSrt ; SELECT(?TabSortQ) ; DISPLAY ; END   !TODO change these Message() to DB() ?
    FREE(ParentQ)     
    CLEAR(ParentQ)        
    LOOP QNdx=1 TO RECORDS(SortQ)
        GET(SortQ,Qndx) 
        CASE SortQ:Control 
        OF 'END'
             IF ~InParent THEN
                IF DbSrt THEN |
                   Message('At line '& Qndx &' in END but InParent=' &  InParent &'|QRecs=' & RECORDS(SortQ) &'|pSort=' & ParQ:pSort ). 
                CYCLE
             END
             SortQ:ATSort='End___End___'
             SortQ:ATSort=ParQ:pSort & SortQ:ATSort 
             SortQ:Level = InParent - 1
             PUT(SortQ)

             IF DbSrt THEN Message('END InParent=' & InParent &'  |QNdx=' & QNdx & |
                     '|SortQ:Control=' & SortQ:Control & |
                     '|SortQ:ATSort=' & SortQ:ATSort & |
                     '|ParQ:Sort=' & ParQ:pSort & |   
                     '|Records ParentQ=' & records(ParentQ) & |
                     '','END Parent') .

             GET(ParentQ, InParent) ; DELETE(ParentQ)         !POP the current Parent
             InParent -= 1  
             CLEAR(ParentQ)  
                IF DbSrt THEN Message('After -=1, line '& Qndx &' in END InParent=' &  InParent &'|QRecs=' & RECORDS(SortQ) &'|pSort=' & ParQ:pSort ).
             IF InParent > 0 THEN 
                GET(ParentQ, InParent)
                IF DbSrt THEN Message('Popped Q END |InParent=' & InParent &'  |QNdx=' & QNdx & |
                         '|SortQ:Control=' & SortQ:Control & |
                         '|SortQ:ATSort=' & SortQ:ATSort & |
                         '|ParQ:Sort=' & ParQ:pSort & |   
                         '|Records ParentQ=' & records(ParentQ) & |
                         '','END Parent Popped Q').
                
             END
             IF InParent < 0 THEN InParent = 0.         !Bad Parents
             CYCLE
        END             
        IF InParent THEN 
           SortQ:ATSort=ParQ:pSort & SortQ:ATSort
           IF SortQ:CodeLine[1]='' THEN                             !Does not have LABEL like a DETAIL would
              SortQ:CodeLine=ALL(' ',InParent) & SortQ:CodeLine     !Indent Child e.g. STRING
           END
           SortQ:Level = InParent
           PUT(SortQ)
        END 
        CASE SortQ:Control 
        OF   'GROUP'
        OROF 'OPTION'
        OROF 'SHEET'
        OROF 'TAB'
        OROF 'DETAIL' OROF 'BREAK' OROF 'HEADER' OROF 'FOOTER'  !Added, but will Indent Column 1 Label and mess up
             InParent  += 1
             ParQ:pSort  = CLIP(SortQ:ATSort)     !Push this in front  NO  ParQ:pSort &
             ADD(ParentQ)  
             IF DbSrt THEN Message('InParent=' & InParent &'  |QNdx=' & QNdx & |
                     '|SortQ:Control=' & SortQ:Control & |
                     '|SortQ:ATSort=' & SortQ:ATSort & |
                     '|ParQ:Sort=' & ParQ:pSort & |   
                     '|Records ParentQ=' & records(ParentQ) & |
                     '','New Parent:' & SortQ:Control) .
        END
    END 
    EXIT
    !endRegion
     
!---------------------------------------------
DOO.MakeSortedRtn PROCEDURE()  !Concat FLatCode into SortedCode in SortQ Order
    CODE
    SortedCode='' 
    OutX = 0 
    LOOP QNdx= 1 TO RECORDS(SortQ)
         GET(SortQ,QNdx)
         LineX = SortQ:LineNo
         OutX += 1
         SortedCode = CHOOSE(OutX=1,'',CLIP(SortedCode)&'<13,10>') & | 
                      CHOOSE(SortQ:Control AND SortQ:Control[1]='','  ','') & | !Indent Child  
                      ALL(' ',SortQ:level) & |
                        ?FlatCode{PROP:Line, LineX} 
    END
    SortedCode = CLIP(SortedCode)&'<13,10>'
    DISPLAY
    SELECT(?SortedCode) 
    RETURN 
!---------------------------------------------------
DOO.StripClutterRtn PROCEDURE(LONG BtnFEQ, LONG CodeTextFEQ, *STRING CodeTextUseString)  !Remove from SortedCode clutter #ORIG,#ORD,USE(?STRING also DECIMAL to RIGHT 
StripCode   CSTRING(SIZE(CodeTextUseString)+201)     !e.g. SortedCode or FlatCode
ReplaceCode PSTRING(16)   
ReplaceHow  SHORT       ! 3=String Equates,  5=TRN Add, 6=TRN Remove,  11=Button LEFT or FLAT
StripTxt    PSTRING(16)
StripLen    LONG
ALineAfterStripTxt STRING(64)   !
BegComma    LONG     
EndParen    LONG 
PopNo       USHORT
NotIfSEQ    BOOL    !if #SEQ() then do not strip
StrippedTooBig LONG !Some Add Text. Unlikely to be a problem, but it could exceed size allowed
    CODE
    PopNo=POPUPunder(BtnFEQ, |
                'Remove #ORIG(?xxxx) - Usually Clutter' & |             !#1
               '|Remove #LINK(?xxxx) - Usually Clutter' & |             !#2 
               '|Remove #ORDINAL(##) - If copy or duping controls' & |  !#3 
             '|-' & |               
               '|Remove USE(?String:#) - Numeric Equates' & |           !#4
               '|Lowercase USE(?STRING:#) - Numeric Equates' & |        !#5
             '|-' & |
               '|Change DECIMAL() to RIGHT' & |                         !#6
             '|-' & | 
               '|Add TRN to all STRINGs' & |                            !#7
               '|Remove all TRN'  & |                                   !#8
             '|-|Remove AT() - For Compare Without AT() Changes'   & |  !#9
             '|-|Remove FONT()' & |                                     !#10 
             '|-|Remove MSG(''status message'')' & |                    !#11
               '|Remove TIP(''tool tip'')'         & |                  !#12
             '|-' & |
               '|Remove ICON(''icon.ico'')'        & |                  !#13
               '|Remove LEFT on BUTTON'            & |                  !#14
               '|Remove FLAT on BUTTON'            & |                  !#15
               '')
    IF ~PopNo THEN RETURN.  
    CASE PopNo        !123456789012
    OF  1 ; StripTxt = ',#ORIG('        ; NotIfSEQ=True 
    OF  2 ; StripTxt = ',#LINK('
    OF  3 ; StripTxt = ',#ORDINAL('
    OF  4 ; StripTxt = ',USE(?STRING'   ; ReplaceCode=''             ; ReplaceHow=3
    OF  5 ; StripTxt = ',USE(?STRING'   ; ReplaceCode=',USE(?string' ; ReplaceHow=-3
    OF  6 ; StripTxt = ',DECIMAL('      ; ReplaceCode=',right'
    OF  7 ; StripTxt = ',TRN'           ; ReplaceCode=',TRN'         ; ReplaceHow=5     !Add TRN to all STRINGs
    OF  8 ; StripTxt = ',TRN'           ; ReplaceCode=''             ; ReplaceHow=6     !Remove TRN
    OF  9 ; StripTxt = ',AT('           !Added so I could strip a Window/Report down to what will Compare well i.e. No AT()
    OF 10 ; StripTxt = ',FONT('         !ToDo keep an ANY of the FONT() removed to show in a Message after, and a Count
    OF 11 ; StripTxt = ',MSG('
    OF 12 ; StripTxt = ',TIP('
    OF 13 ; StripTxt = ',ICON('
    OF 14 ; StripTxt = ',LEFT'          ; ReplaceCode=''             ; ReplaceHow=11    !Remove LEFT on BUTTON (had Icon)
    OF 15 ; StripTxt = ',FLAT'          ; ReplaceCode=''             ; ReplaceHow=11    !Remove FLAT on BUTTON (had Icon)
    ELSE  ; Message('StripClutterRtn ||Case unknown PopNo='& PopNo) ; RETURN 
    END  
 
    StripLen = LEN(StripTxt)
    LOOP LineX=1 TO TextLineCount(CodeTextFEQ)  !was ?SortedCode{PROP:LineCount}
         ALine = CodeTextFEQ{PROP:Line, LineX}  !was ?SortedCode{PROP:Line, LineX}
         IF ~ALine THEN CYCLE.
         ULine = UPPER(ALine)
         ULeftLine = LEFT(ULine)
         
         BegComma=INSTRING(StripTxt,ULine,1)
         ALineAfterStripTxt=CHOOSE(~BegComma,'',SUB(ALine,BegComma+StripLen,64))    !E.g. Strip=',TRN' with will be '?' in ',TRN?'
         IF NotIfSEQ AND INSTRING('#SEQ',ULine,1) THEN
            !Skip line change that has #SEQ. Many #Control templates use %ControlOriginal so leave #ORIG if #SEQ. E.g. Browse Buttons require #ORIG(?Insert) etc
         ELSIF ReplaceHow=5 THEN                         !Add TRN if it not there to STRING and PROMPT
            IF ~BegComma AND INLIST(SUB(ULeftLine,1,6),'STRING','PROMPT') THEN
                ALine=CLIP(ALine) & ',TRN'
            END

         ELSIF BegComma = 0 THEN        !Above here can have BegComma=0
               !Fall thru & Append      !Below here cannot have BegComma=0
                                    
         ELSIF ReplaceHow=6 THEN                      !Remove TRN
!!!            CASE SUB(ULine,BegComma,5)
!!!            OF ',TRN,' OROF ',TRN '
!!!               ALine=SUB(ALine,1,BegComma-1) & ReplaceCode & SUB(ALine,BegComma+4,9999)
!!!            END
            IF INLIST(ALineAfterStripTxt[1],' ',',') THEN   !Is it  ',TRN,' or ',TRN '
               ALine=SUB(ALine,1,BegComma-1) & ReplaceCode & SUB(ALine,BegComma+StripLen,9999)
            END            
         ELSIF ReplaceHow=11 THEN                      !Remove LEFT or FLAT on BUTTON
            IF SUB(ULeftLine,1,6)='BUTTON' THEN
!!! stop('button BegComma=' & BegComma &'<13,10>SUB=' & SUB(ULine,BegComma,6) &'<13,10>ALineAfterStripTxt=' & ALineAfterStripTxt  &'<13,10>ALine=' & ALine)   
!!!               CASE SUB(ULine,BegComma,6)
!!!               OF ',LEFT,' OROF ',LEFT '
!!!                  ALine=SUB(ALine,1,BegComma-1) & ReplaceCode & SUB(ALine,BegComma+5,9999)
!!!               END
  IF SUB(ULine,BegComma,StripLen) <> StripTxt THEN STOP('Bug ULine <<> StripTxt ' & StripTxt  &'<13,10>ALine=' & ALine ).   !DEBUG HACK
               IF INLIST(ALineAfterStripTxt[1],' ',',') THEN   !Trailing Space or Comma after ,XXXX so ",LEFT," or ",LEFT "
                  ALine=SUB(ALine,1,BegComma-1) & ReplaceCode & SUB(ALine,BegComma+StripLen,9999)
               END
            END

         ELSIF BegComma THEN
            EndParen=INSTRING(')',ALine,1, BegComma)
            IF EndParen THEN
               CASE ReplaceHow      ! +123456789012
               OF 3 OROF -3         !',USE(?STRING'       ,USE(?STRING[0-9:]
                    IF ~MATCH(ULine[BegComma : EndParen], |
                        ',USE(\?STRING[0-9: ]*)', Match:Regular+Match:NoCase) THEN EndParen=0.
               END
               IF EndParen THEN
                  IF ReplaceHow = -3 THEN       !<
                     ALine=SUB(ALine,1,BegComma-1) & LOWER(SUB(ALine,BegComma,EndParen-BegComma+1)) & SUB(ALine,EndParen+1,9999)
                  ELSE   
                     ALine=SUB(ALine,1,BegComma-1) & ReplaceCode & SUB(ALine,EndParen+1,9999)
                  END   
               END    
            END 
         END  
         StripCode = CLIP(StripCode) & CLIP(ALine) &'<13,10>'
    END
    CodeTextUseString = StripCode
    DISPLAY
    StrippedTooBig = LEN(CLIP(StripCode)) - SIZE(CodeTextUseString)
    IF StrippedTooBig > 0 THEN
       Message('The Stripped code is '& StrippedTooBig &' bytes longer |than fit in the '& SIZE(CodeTextUseString) &' size of the TEXT String','Strip Clutter')
    END    
    RETURN 
!---------------------------------------------------      
DOO.AlignAtRtn PROCEDURE(LONG TxtFEQ, LONG AlignBtnFEQ) !In SortedCode align AT() so can see in source BUTTON('Alight AT()'),USE(?AlignAtBtn)
CbCodeCls   CBCodeParseClass    
AlignedCode CSTRING(SIZE(SortedCode)+1000),AUTO
AlignedTooBig   LONG 
AtNow       SHORT
AtMax       SHORT   
AtHigh      SHORT       !Unlimited High Max
AtMax4Line  SHORT       !If Sorted by Control Type then Align BOX and LINE the same
L_AlignMax  USHORT      !Local AlignMax Here incase Ignored in message by User to Align to Max   
AtHigh_LineX LONG       !Line with AT() the Highest
OverAlnMaxCount LONG
OverAlnMaxSum   LONG
OverAlnAverage  SHORT
IgnoreMAX   BOOL
ATFQ:AtArray STRING(10),DIM(4),OVER(ATFQ:At_XYWH_Grp)
MaxLenAtXYWH   LONG,DIM(4)              !Maximum LenAtXYWH[] so can space out
MaxLenAt_CSV   PSTRING(8*4)             !X,Y,W,H 
MaxValueAtXYWH PSTRING(8),DIM(4)        !Maximum Value that was LenAtXYWH[] so can space out, this is when Len is Max not the Max XYWH
MaxAtXYWH   LONG,DIM(4)                 !The Maxiumum seen in AT() to show in Message() and at end of List
MinAtXYWH   LONG,DIM(4)                 !The Minimu m seen in AT() to show in Message() 
UseLenHigh  LONG                        !The Maximum USE() contents Length for Align ending Paren  
UseLenLine  LONG                        !The Line No with the Max
AX          USHORT
CmaXYW      STRING(3)
L_NoShowMsg BOOL                 !Cfg:AlignAt_NoShowMsg           
MsgAtInfo   ANY
MsgButtons  PSTRING(128)
NoMsgBtn    BYTE             
    CODE                                                        
    L_AlignMax  = Cfg:AlignAt_Max              !Local Max Here incase Ignored
    L_NoShowMsg = Cfg:AlignAt_NoShowMsg
ReAlignLabel:
    DO FindAtMaxAndHighRtn
    IF AtMax THEN DO AlignAtToAtMaxFoundRtn.
    DISPLAY 

    MsgAtInfo='AT() Aligned at Column: ' & AtMax

    IF AtHigh <= AtMax THEN                     
!       MsgAtInfo='AT() aligned at column: ' & AtMax
       NoMsgBtn = 2 ! #1   #2
       MsgButtons='&Close|No &Message'
    ELSE 
       NoMsgBtn = 4  ! #1  #2                   #3                      #4
       MsgButtons='&Close|&Align @'& AtHigh &'|&Set Max='& AtHigh !&'|&No Message'
!       MsgAtInfo='AT() aligned to column: <9>' & AtMax & |
       MsgAtInfo=MsgAtInfo & '<13,10>' & |
          '<13,10>Limited by Maximum:  <9>'    & L_AlignMax & | !'  (change Max on window)'& |
          '<13,10>Highest AT() Column:   <9>'  & AtHigh &' <9>Line: '& AtHigh_LineX  
       IF OverAlnMaxCount > 1 THEN
          OverAlnAverage = INT(OverAlnMaxSum/OverAlnMaxCount + .5)
          MsgAtInfo=MsgAtInfo & '<13,10>Average AT() Column:   <9>'  & OverAlnAverage & |
                                ' <9>For '& OverAlnMaxCount &' Controls Over Max '& L_AlignMax 
          IF OverAlnAverage >1 AND OverAlnAverage < AtHigh THEN 
             MsgButtons=MsgButtons &'|Alig&n @'& OverAlnAverage &'|S&et Max='& OverAlnAverage
             NoMsgBtn += 2
          END 
!          MsgAtInfo=MsgAtInfo & '<13,10>Controls Over '& L_AlignMax &' Max: <9>' & OverAlnMaxCount & |
!                            ' <9>Average AT() Column:'& INT(OverAlnMaxSum/OverAlnMaxCount + .5)       
       END 
       MsgButtons=MsgButtons &'|No &Message'
    END                 
    MsgAtInfo=MsgAtInfo & '<13,10>' & |
              '<13,10>Maximum AT(X,Y,W,H) Widths [' & MaxLenAt_CSV &']' & |
              '<13,10>' & |
             '<13,10>Minimum  AT( ' & MinAtXYWH[1] &' , '& MinAtXYWH[2] &' , '& MinAtXYWH[3] &' , '& MinAtXYWH[4] &' )' & |
              '<13,10>Maximum AT( ' & MaxAtXYWH[1] &' , '& MaxAtXYWH[2] &' , '& MaxAtXYWH[3] &' , '& MaxAtXYWH[4] &' )'
    
    IF Cfg:AlignUSE_Checked AND UseLenHigh THEN
       MsgAtInfo=MsgAtInfo & '<13,10>' & |
                '<13,10>Maximum USE() Width '& UseLenHigh &' in Line '& UseLenLine &'  (Limit: '& Cfg:AlignUSE_Max &')'
    END 
    AlignBtnFEQ{PROP:Tip}=Cfg:AlignAt_BtnTipOriginal & '<13,10>={40}' & |
                '<13,10>Last Aligned '& FORMAT(CLOCK(),@t6) & |
                '<13,10>' & MsgAtInfo

    IF L_NoShowMsg THEN RETURN.
    CASE Message(MsgAtInfo,'Align AT()',ICON:VCRbottom,MsgButtons,,MSGMODE:CANCOPY)
    OF NoMsgBtn ; Cfg:AlignAt_NoShowMsg = 1 ; DISPLAY   !Could be Btn 2 or 4
    OF 2        ; L_AlignMax=AtHigh                                          ; L_NoShowMsg=1 ; GOTO ReAlignLabel:   !Align @  AtHigh (aka Ignore Max)
    OF 3        ; L_AlignMax=AtHigh         ; Cfg:AlignAt_Max=AtHigh         ; L_NoShowMsg=1 ; GOTO ReAlignLabel:   !Set Max= AtHigh
    OF 4        ; L_AlignMax=OverAlnAverage                                  ; L_NoShowMsg=1 ; GOTO ReAlignLabel:   !Align @  OverAlnAverage
    OF 5        ; L_AlignMax=OverAlnAverage ; Cfg:AlignAt_Max=OverAlnAverage ; L_NoShowMsg=1 ; GOTO ReAlignLabel:   !Set Max= OverAlnAverage
    END 

    RETURN
    
FindAtMaxAndHighRtn ROUTINE   
    FREE(ATFindQ)
    LOOP LineX=1 TO TxtFEQ{PROP:LineCount}
         ALine = TxtFEQ{PROP:Line, LineX}
         CLEAR(ATFindQ)
         ATFQ:LineX    = LineX
         ATFQ:CodeLine = LEFT(ALine)   
         ATFQ:CodeAT   = ''

         IF CbCodeCls.FindAttrParen(ALine, 'USE', ATFQ:UsePos1, ATFQ:UseParen1, ATFQ:UseParen2) THEN
            ATFQ:UseLen = ATFQ:UseParen2 - ATFQ:UseParen1 - 1
            ATFQ:UsePosInfo='('& ATFQ:UsePos1 &','& ATFQ:UseLen + 5 &') '& |
                            '['& ATFQ:UseParen1 &':'& ATFQ:UseParen2 &','& ATFQ:UseLen &'] = '& SUB(ALine, ATFQ:UsePos1 , ATFQ:UseLen + 5 )
            IF NOT(ATFQ:UsePos1 AND ATFQ:UseParen1 AND ATFQ:UseParen1+1 <= ATFQ:UseParen2-1) THEN
               ATFQ:UsePosInfo='? '& ATFQ:UsePosInfo 
            ELSIF UseLenHigh < ATFQ:UseLen AND ATFQ:UseLen <= Cfg:AlignUSE_Max THEN 
                  UseLenHigh = ATFQ:UseLen
                  UseLenLine = LineX
            END 
         END
         
         IF ~CbCodeCls.FindAttrParen(ALine, 'AT', ATFQ:AtPos1, ATFQ:AtParen1, ATFQ:AtParen2) THEN CYCLE.
         IF NOT(ATFQ:AtPos1 AND ATFQ:AtParen1 AND ATFQ:AtParen1+1 <= ATFQ:AtParen2-1) THEN CYCLE.    !Make sure not empty AT()
         ATFQ:CodeAT=ALine[ATFQ:AtPos1 : ATFQ:AtParen2]
         ATFQ:AtParmCnt = CbCodeCls.ParseAttributes(ALine, ATFQ:AtParen1, ATFQ:AtParen2, ATFQ:AtArray[], ATFQ:AtParmPos[], ATFQ:AtCommas[]) 
         ATFQ:Xbytes = ATFQ:AtCommas[1] - ATFQ:AtParen1 - 1 
         ATFQ:XLen   = LEN(CLIP(ATFQ:AtArray[1]))
         LOOP AX=1 TO 4
              ATFQ:AtLenXYWH[AX]=LEN(CLIP(ATFQ:AtArray[AX]))
              ATFQ:LenzAT=CHOOSE(AX=1,'',ATFQ:LenzAT&'-') &  ATFQ:AtLenXYWH[AX]     !Debug 
              IF MaxLenAtXYWH[AX] < ATFQ:AtLenXYWH[AX] THEN  
                 MaxLenAtXYWH[AX] = ATFQ:AtLenXYWH[AX]
                 MaxValueAtXYWH[AX] = CLIP(ATFQ:AtArray[AX])
              END
              IF MaxAtXYWH[AX] < ATFQ:AtArray[AX] THEN 
                 MaxAtXYWH[AX] = ATFQ:AtArray[AX]
              END
              IF MinAtXYWH[AX] > ATFQ:AtArray[AX] THEN 
                 MinAtXYWH[AX] = ATFQ:AtArray[AX]
              END              
              ATFQ:ParmZPos=CHOOSE(AX=1,'',ATFQ:ParmZPos&' - ') &  ATFQ:AtParmPos[AX,1] &','& ATFQ:AtParmPos[AX,2]
              ATFQ:CommaZ  =CHOOSE(AX=1,'',ATFQ:CommaZ&'-')     &  ATFQ:AtCommas[AX]
         END 

         ADD(ATFindQ,ATFQ:LineX)

         AtNow=ATFQ:AtPos1 ; IF AtNow > AtMax AND (~L_AlignMax OR AtNow <= L_AlignMax) THEN  
                                AtMax = AtNow
                             ELSIF AtNow > L_AlignMax AND L_AlignMax
                                OverAlnMaxCount += 1
                                OverAlnMaxSum   += AtNow
                             END
                             IF AtNow > AtHigh THEN  
                                AtHigh = AtNow ; AtHigh_LineX = LineX
                             END
    END
!    MaxLenAt_CSV = MaxLenAtXYWH[1] &','& MaxLenAtXYWH[2] &','& MaxLenAtXYWH[3] &','& MaxLenAtXYWH[4]
    MaxLenAt_CSV = MaxLenAtXYWH[1] &'-'& MaxLenAtXYWH[2] &'-'& MaxLenAtXYWH[3] &'-'& MaxLenAtXYWH[4]

    !-- Show Max Align Data at bottom of List as a row -------------------
    CLEAR(ATFindQ)
    ATFQ:LineX    = 9999
    ATFQ:AtPos1   = AtHigh
    ATFQ:CodeAT   = 'Max(' & MaxValueAtXYWH[1] &','& MaxValueAtXYWH[2] &','& MaxValueAtXYWH[3] &','& MaxValueAtXYWH[4] &')'
    ATFQ:LenzAT   = MaxLenAt_CSV &' Max'
    ATFQ:CodeLine = 'MAXIMUM Widths: ' & CLIP(ATFQ:LenzAT) &'  AT '& ATFQ:CodeAT 
    ATFQ:At_X     = MaxAtXYWH[1]        !A way to see the maximum values for XYWH
    ATFQ:At_Y     = MaxAtXYWH[2]
    ATFQ:At_W     = MaxAtXYWH[3]
    ATFQ:At_H     = MaxAtXYWH[4] 
    ATFQ:UsePosInfo = UseLenHigh & ' = Max Use() Length in Line '& UseLenLine
    ADD(ATFindQ,ATFQ:LineX)
    EXIT

AlignAtToAtMaxFoundRtn ROUTINE       !Align the AT() to the Max Found
    OutX = 0
    CLEAR(AlignedCode) 
    LOOP LineX=1 TO TextLineCount(TxtFEQ) !was {PROP:LineCount}
         ALine = TxtFEQ{PROP:Line, LineX}
         IF ~ALine THEN CYCLE.
         ATFQ:LineX = LineX
         GET(ATFindQ,ATFQ:LineX)
         IF ERRORCODE() THEN CLEAR(ATFindQ).
         
         !-- Align the USE() here -------------------------------------------------
         IF Cfg:AlignUSE_Checked |
         AND ATFQ:UsePos1    AND ATFQ:UsePos1 > ATFQ:AtPos1       |       !The USE() must be after AT()
         AND ATFQ:UseParen1  AND ATFQ:UseParen2 > ATFQ:UseParen1  |       !Sanity check
         AND ATFQ:UseLen > 0 AND ATFQ:UseLen <= Cfg:AlignUSE_Max  THEN
               ALine=SUB(ALine,1,ATFQ:UseParen2-1)       & |             !AT()... USE(xxx...         without end ")"
                     ALL(' ',UseLenHigh - ATFQ:UseLen)   & |             !               spaces
                     SUB(ALine,ATFQ:UseParen2,9999)                      !                     ),rest....
         END 
         
         !-- Align the AT() here -------------------------------------------------
         AtNow=ATFQ:AtPos1                  !AtNow=INSTRING(',AT(',UPPER(ALine),1) 
         IF IsSortByControl AND INLIST(ATFQ:CodeLine[1:3],'BOX','LIN') THEN      !Sorted by Control and BOX or LINE 
            AtMax4Line = 8   !Align BOX / LINE at 8 because often STRING's will have AT() way out
         ELSE
            AtMax4Line = AtMax
         END                   !; DB('Line='& SortQ:LineNo &'  AtMax='& AtMax &'  AtMax4Line='& AtMax4Line &'  UseLenHigh='& UseLenHigh &'ATFQ:Code='& ATFQ:CodeLine[1:9] &'  IsSortByControl='& IsSortByControl  )        
         IF AtNow AND AtNow <= AtMax4Line THEN
            IF ~Cfg:AlignAt_XYWH AND ATFQ:AtParen1        !Just Align the 1st comma for X Width
 IF 0  !No X Align
               ALine=SUB(ALine,1,ATFQ:AtParen1)                    & |     !Control AT(
                     ALL(' ',MaxLenAtXYWH[1] - ATFQ:AtLenXYWH[1])  & |     !           spaces
                     SUB(ALine,ATFQ:AtParen1+1,9999)                       !                 xxx,yy,w,h),rest....    
 END                                                                

            ELSIF Cfg:AlignAt_XYWH AND ATFQ:AtParen1 AND ATFQ:AtParen2 THEN               
               CmaXYW=SUB(',,,',1,ATFQ:AtParmCnt-1)
               ALine=SUB(ALine,1,ATFQ:AtParen1) & |                            !Control AT(
                     RIGHT(ATFQ:AtArray[1],MaxLenAtXYWH[1]) & CmaXYW[1] & |    !           xxx,
                     RIGHT(ATFQ:AtArray[2],MaxLenAtXYWH[2]) & CmaXYW[2] & |    !               yy,
                     RIGHT(ATFQ:AtArray[3],MaxLenAtXYWH[3]) & CmaXYW[3] & |    !                  ww,
                     RIGHT(ATFQ:AtArray[4],MaxLenAtXYWH[4])             & |    !                     hh
                     SUB(ALine,ATFQ:AtParen2,9999)                             !                       ),rest...
            END
            IF AtNow < AtMax4Line THEN
               ALine=SUB(ALine,1,AtNow-1) & ALL(' ',AtMax4Line - AtNow) & SUB(ALine,AtNow,9999)
            END
         END    
         OutX += 1
         AlignedCode = CLIP(AlignedCode) & CLIP(ALine) & |
                       '<13,10>'
    END 

    CHANGE(TxtFEQ,CLIP(AlignedCode))
    DISPLAY
    AlignedTooBig = LEN(CLIP(AlignedCode)) - SIZE(SortedCode)
    IF AlignedTooBig > 0 THEN
       Message('The Aligned code is '& AlignedTooBig &' bytes longer |than fit in the '& SIZE(SortedCode) &' size of the TEXT String','Align AT')
    END
    EXIT
!---------------------------------------------------
DOO.AlignQCopyBtn PROCEDURE()
CX LONG 
QX LONG 
CbAny ANY
Tb9   EQUATE('<9>')
    CODE 
    CbAny = 'Index <9>LineX <9>AT Pos1 <9>Paren1 <9>Paren2 <9>ParmCnt <9>At_X <9>At_Y <9>At_W <9>At_H <9>Xbytes <9>XLen <9>CodeAT <9>LenzAT <9>ParmZPos <9>CommaZ ' & |
             '<9>USE() Pos Info <9>UsePos1 <9>Use_( <9>Use_) <9>UseLen <9>CodeLine <13,10>'
    LOOP QX=1 TO RECORDS(ATFindQ)                                                        ! <9>Level
         GET(ATFindQ,QX)
         CbAny = CbAny & LEFT( QX & | 
         Tb9 & ATFQ:LineX       & |     ! LONG        
         Tb9 & ATFQ:AtPos1      & |     ! LONG        
         Tb9 & ATFQ:AtParen1    & |     ! LONG        
         Tb9 & ATFQ:AtParen2    & |     ! LONG        
         Tb9 & ATFQ:AtParmCnt   & |     ! LONG        
         Tb9 & CLIP(ATFQ:At_X)  & |     !   STRING(10)
         Tb9 & CLIP(ATFQ:At_Y)  & |     !   STRING(10)
         Tb9 & CLIP(ATFQ:At_W)  & |     !   STRING(10)
         Tb9 & CLIP(ATFQ:At_H)  & |     !   STRING(10)
         Tb9 & ATFQ:Xbytes      & |     ! SHORT       
         Tb9 & ATFQ:XLen        & |     ! SHORT       
         Tb9 & CLIP(ATFQ:CodeAT)& |     ! STRING(32)  
         Tb9 & ATFQ:LenzAT      & |     ! PSTRING(32) 
         Tb9 & ATFQ:ParmZPos    & |     ! PSTRING(64) 
         Tb9 & ATFQ:CommaZ      & |     ! PSTRING(32) 
         Tb9 & ATFQ:UsePosInfo  & |     ! PSTRING(64)
         Tb9 & ATFQ:UsePos1     & |     ! LONG
         Tb9 & ATFQ:UseParen1   & |     ! LONG
         Tb9 & ATFQ:UseParen2   & |     ! LONG
         Tb9 & ATFQ:UseLen      & |     ! LONG
         Tb9 & ' ' & CLIP(ATFQ:CodeLine) & '<13,10>' )     ! STRING(255)    !Leading Space so POS are correct         
    END
    SETCLIPBOARD(CbAny)
    RETURN 
!---------------------------------------------------
DOO.AddHistoryRtn PROCEDURE() !Add FlatCode to HistoryQ so can recall original code 
QIdx   LONG,AUTO
    CODE 
    IF ~OrigCode OR ~FlatCode THEN RETURN.
    LOOP QIdx=RECORDS(HistoryQ) TO 1 BY -1
        GET(HistoryQ,QIdx)
        IF HisQ:OrigAny=OrigCode THEN       !If its already in History then Delete the current one
           HisQ:OrigAny &= NULL
           CLEAR(HistoryQ)
           DELETE(HistoryQ)
        END            
    END                   
    CLEAR(HistoryQ)
    HisQ:Time        = FORMAT(CLOCK(),@t1)
    HisQ:Date        = LEFT(SUB(FORMAT(today(),@d1),1,5))
    HisQ:CodeSnip    = FlatCode
    HisQ:CodeTip     = FlatCode
    HisQ:OrigAny     = OrigCode               !ANY
    ADD(HistoryQ,1)
    RETURN 

!---------------------------------------------------
DOO.FontForCodeRtn PROCEDURE(LONG ButtonFEQ) !Change TEXT control Font that usually Consolas 9
!TODO refactor this out into a Class so can reuse on other tools
NowName STRING(64)
NowSize LONG
NowStyle LONG
FName   STRING(64)
FSize   LONG
FColor  LONG(Color:None)     !Not used, but could be
FStyle  LONG
X       USHORT
Pick    USHORT
PopMenu ANY
PipeB4  PSTRING(4)                      !Pipe alone | or |+ Checked
MapFont2Pick GROUP,DIM(6+5+3+9),PRE()   !Popup Pick # Mapped to Font Arrays
MapName         STRING(64)
MapSize         LONG
MapStyle        LONG
             END
MapCnt     USHORT 
    CODE
    IF ButtonFEQ = 0 THEN  !Pass (0) at Window Open to set Default Font you desire
       IF THREAD()>1 AND Cfg:FontName THEN 
          FName=Cfg:FontName ; FSize=Cfg:FontSize ; FStyle=Cfg:FontStyle
       ELSE     
          FName='Consolas' ; FSize=10 ; FStyle=FONT:regular
       END
       DO SetFontOnText
       RETURN
    END

    GETFONT(?OrigCode ,NowName,NowSize,,NowStyle) ; IF NowStyle=0 THEN NowStyle=FONT:regular.
    LOOP X=1 TO 9      !       1          2              3                4             5          6     7
         FName = CHOOSE(X,'Consolas','Cascadia Code','Lucida Console','Courier New','FixedSys', NowName,'')
         IF ~FName THEN BREAK.
         PipeB4 = CHOOSE(X<2,'','|')            !1st line no Pipe |
         IF UPPER(FName)=UPPER(NowName) THEN    !If = Current Font Name on Window
            PipeB4 = PipeB4 & '+'               !   then +Check Mark the Now Name
            NowName=''                          !   and Blank so do not find at the End
         END
         PopMenu = PopMenu & LEFT(PipeB4 & |
                   '['& PROP:FontName & '(' & CLIP(FName) &')'  & |    !e.g. [31760(Consolas)
                   ','& PROP:FontSize &                 '(10)]' & |    !e.g. ,31761(10)      ]
                   CLIP(FName) & '<9>*(),;: ABC ilo ILO 012' )          !Sample want to see Zero
         MapCnt += 1 ; MapName[MapCnt]=FName
    END

    PopMenu = PopMenu &'|-'
    LOOP X=1 TO 9      !  1  2  3  4  5        6
         FSize = CHOOSE(X,9,10,11,12, NowSize, 0)
         IF ~FSize THEN BREAK.
         IF FSize=NowSize THEN PipeB4 = '|+' ; NowSize=0  !If = Now Font Size +Check it
                          ELSE PipeB4 = '|' .
         PopMenu = PopMenu   & PipeB4 &'Size '& FSize
         MapCnt += 1 ; MapSize[MapCnt]=FSize         
    END

    PopMenu = PopMenu &'|-'        
    LOOP X=1 TO 9       !       1            2     3        4
         FStyle = CHOOSE(X,Font:Regular,Font:Bold, NowStyle,0)
         IF ~FStyle THEN BREAK.
         IF FStyle=NowStyle THEN PipeB4 = '|+' ; NowStyle=0  !If = Now Font Style +Check it
                            ELSE PipeB4 = '|' .
         PopMenu = PopMenu & PipeB4 & DOO.FontStyleName(FStyle)
         MapCnt += 1 ; MapStyle[MapCnt]=FStyle         
    END
    
    PopMenu = PopMenu & '|-|Select Font ...'   ! ; setclipboard(PopMenu)
    Pick = PopupUnder(ButtonFEQ, PopMenu)
    CASE Pick
    OF 0 ; RETURN 
    OF 1 TO MapCnt      ; FName  = MapName[Pick]
                          FSize  = MapSize[Pick]
                          FStyle = MapStyle[Pick]  
    ELSE
        GETFONT(?OrigCode ,FName,FSize,FColor,FStyle)
        IF ~FONTDIALOG('Select Font for Code ' & ' - '& CLIP(FName) &' '& FSize &' '& FStyle & |
                       '',FName,FSize,FColor,FStyle)  THEN
            RETURN
        END
        SETCLIPBOARD(' Selected Font: '''& CLIP(FName) &''' Size: '& FSize &'  Style: '& FStyle )   !Top Help Code New
    END
    DO SetFontOnText
    RETURN
SetFontOnText ROUTINE
    IF FSize<>9 AND FName='FixedSys' THEN FSize = 9.
    DOO.FontSetOnText(FName,FSize,FStyle)
    EXIT
!---------------------------------------------------
DOO.FontSetOnText PROCEDURE(<STRING FntName>,LONG FntSize=0,LONG FntStyle=0)   !Change TEXT controls Font
Fld LONG(0)
    CODE
    LOOP
        Fld=0{PROP:NextField,Fld} ; IF ~Fld THEN BREAK.
        IF Fld{PROP:Type} <> CREATE:Text |              !All are TEXT,HVSCROLL
        OR ~Fld{PROP:VScroll}            |
        OR ~Fld{PROP:HScroll}       THEN CYCLE.
        IF ~OMITTED(FntName) |
            AND FntName      THEN Fld{PROP:FontName }=FntName.
        IF FntSize           THEN Fld{PROP:FontSize }=FntSize.
        IF FntStyle          THEN Fld{PROP:FontStyle}=FntStyle.
    END
    GETFONT(?OrigCode ,Cfg:FontName,Cfg:FontSize,,Cfg:FontStyle)
    FntStyle=Cfg:FontStyle
    ?FontForCodeBtn{PROP:Tip}=' '& CLIP(Cfg:FontName) &'  <13,10> '& Cfg:FontSize &' Point' & |
                                                         '<13,10> '& DOO.FontStyleName(FntStyle)
    RETURN
!---------------------------------------------------
DOO.FontStyleName PROCEDURE(LONG FntStyle) !,STRING   !Describe Font Style
Style   PSTRING(40)
Weight  LONG,AUTO
    CODE 
    Weight = BAND(FntStyle,FONT:Weight)
    CASE Weight
    OF Font:Regular OROF 0  ; Style='Regular'
    OF Font:Bold            ; Style='Bold'
    ELSE
        Style=CHOOSE(INT(Weight/100),'Thin','Extra Light','Light','Regular','Medium', | !100-500
                                     'Semi Bold','Bold','Extra Bold','Black (Heavy)', | !600-900
                                     'Weight '& Weight )                                !Uknown
    END
    Style = LEFT(Style & |
            CHOOSE(~BAND(FntStyle,FONT:italic)    ,'',' Italic')    & |
            CHOOSE(~BAND(FntStyle,FONT:underline) ,'',' Underline') & |
            CHOOSE(~BAND(FntStyle,FONT:strikeout) ,'',' Strikeout') )
    RETURN Style    
!---------------------------------------------------
DOO.NewThreadRun PROCEDURE(LONG StartHistoryNo=0)
    CODE
    GETPOSITION(0,ReRn:WinX,ReRn:WinY,ReRn:WinW,ReRn:WinH)
    Cfg:HistoryNo = StartHistoryNo
    UNHIDE(?HaltBtn)    
    START(AtSortReport, ,ConfigGrp,ReRunGrp)
    RETURN 
!---------------------------------------------------
DOO.SortQCopyBtn PROCEDURE()
CX LONG 
QX LONG 
CbAny ANY
Tb9   EQUATE('<9>')
    CODE 
    CbAny = 'Index <9>LineNo <9>ATSortHow <9>AT(X) <9>AT(Y) <9>AT(W) <9>AT(H) <9>Area <9>Control <9>USE <9>Lvl <9>AT(X,Y,H,W) Code  <9>Code Line <13,10>'
    LOOP QX=1 TO RECORDS(SortQ)                                                        ! <9>Level
         GET(SortQ,QX)
         LOOP CX=1 TO LEN(CLIP(SortQ:ATSort))                       !Spaces to Underscore so Sort AT works in Excel
              IF SortQ:ATSort[CX]=' ' THEN SortQ:ATSort[CX]='_'. 
         END   
         CbAny = CbAny & |
           LEFT( QX      & | 
           Tb9 & SortQ:LineNo  & |
           Tb9 & CLIP(SortQ:ATSort)  & |
           Tb9 & SortQ:ATX     & |
           Tb9 & SortQ:ATY     & |
           Tb9 & CLIP(LEFT(SortQ:ATW)) & |            !Strings Right Just so make Left, can be blank
           Tb9 & CLIP(LEFT(SortQ:ATH)) & |
           Tb9 & CLIP(LEFT(SortQ:Area)) & |
           Tb9 & CLIP(SortQ:Control) & | 
           Tb9 & CLIP(SortQ:UseFEQ) & | 
           Tb9 & CHOOSE(~SortQ:LEVEL,'',''&SortQ:LEVEL)   & |               
           Tb9 &'AT('& CLIP(SortQ:AtCode)  &')'& |    !Add AT() around or Excel may see as Number with Commas
           Tb9 & CLIP(SortQ:CodeLine) & '<13,10>' )            
    END
    SETCLIPBOARD(CbAny)
    RETURN       
!---------------------------------------------------
DOO.SortQSortBtn PROCEDURE()
    CODE 
    EXECUTE POPUPunder(?,'Line (Original Order)|AT Sort How|-|At() X , Y|At() Y , X|At() Width|At() Height |Area + Y' & |
                         '|-|Control + Line Number|Control + At(X , Y)|Control + At(Y , X)|Control + Width|Control + Height|Control + Area' & |
                         '|-|USE() FEQ|Control Code Line')
        SORT(SortQ,SortQ:LineNo )                            !Sequence
        SORT(SortQ,SortQ:ATSort,SortQ:LineNo )               !At Sort How
        SORT(SortQ,SortQ:ATX,SortQ:ATY,  SortQ:LineNo )      !X,Y
        SORT(SortQ,SortQ:ATy,SortQ:ATx,  SortQ:LineNo )      !Y,X
        SORT(SortQ,SortQ:ATW, SortQ:ATX, SortQ:LineNo )      !Width  then X
        SORT(SortQ,SortQ:ATH, SortQ:ATy, SortQ:LineNo )      !Height then Y
        SORT(SortQ,SortQ:Area, SortQ:ATy, SortQ:LineNo )     !Area then Y

        SORT(SortQ,SortQ:Control,                       SortQ:LineNo )      !Control Line
        SORT(SortQ,SortQ:Control, SortQ:ATX, SortQ:ATY, SortQ:LineNo )      !Control X,Y
        SORT(SortQ,SortQ:Control, SortQ:ATy, SortQ:ATx, SortQ:LineNo )      !Control Y,X
        SORT(SortQ,SortQ:Control, SortQ:ATW,            SortQ:LineNo )      !Control Width 
        SORT(SortQ,SortQ:Control, SortQ:ATH,            SortQ:LineNo )      !Control Height 
        SORT(SortQ,SortQ:Control, SortQ:Area, SortQ:ATY, SortQ:LineNo )     !Control Area
        
        SORT(SortQ,SortQ:SortUseFEQ )
        SORT(SortQ,SortQ:SortCodeLne ) 
    END          
!---------------------------------------------------
DOO.SetTestRtn PROCEDURE()   !Set OrigCode to Test Window
    CODE
 OrigCode = '    STRING(@s38),AT(2031,0,3438,208),USE(CTL:Installation),FONT(,10,,FONT:bold,CHARSET:ANSI), |' &|
     '<13,10>       CENTER,#ORIG(CTL:Installation)' &|
     '<13,10>    STRING(''EMPLOYEE TYPES WITH TRS RATES''),AT(2552,219),USE(?String6),FONT(,10,,FONT:bold, |' &|
     '<13,10>       CHARSET:ANSI),#ORIG(?String6)' &|
     '<13,10>    LINE,AT(0,417,7500,0),USE(?Line1),COLOR(COLOR:Black),LINEWIDTH(4),#ORIG(?Line1)' &|
     '<13,10>    STRING(''RUN:''),AT(10,219),USE(?RunPrompt)' &|
     '<13,10>    STRING(@D1),AT(292,219),USE(ReportRunDate),FONT(,10),LEFT' &|
     '<13,10>    STRING(@T3-),AT(958,219),USE(ReportRunTime),LEFT' &|
     '<13,10>    STRING(''EMPLOYEE NAME  (CODE)  TIER BUILDING''),AT(10,458),USE(?String21),#ORIG(?String21)' &|
     '<13,10>    STRING(''ALL PAY''),AT(3708,448),USE(?String22:3),TRN,#ORIG(?String22)' &|
     '<13,10>    STRING(''CONTRACT PAY''),AT(4896,448),USE(?String22),TRN,#ORIG(?String22)' &|
     '<13,10>    STRING(''HOURLY/OTHER PAY''),AT(6260,458),USE(?String22:2),TRN,#ORIG(?String22)' &|
     '<13,10>    STRING(''EMPLOYEE''),AT(4021,604),USE(?String14),TRN,#ORIG(?String14)' &|
     '<13,10>    STRING(''TYPE''),AT(10,760),USE(?String5),#ORIG(?String5)' &|
     '<13,10>    STRING(''DESCRIPTION''),AT(354,760),USE(?String5:2),#ORIG(?String5)' &|
     '<13,10>    LINE,AT(0,938,7500,0),USE(?Line1:2),COLOR(COLOR:Black),LINEWIDTH(4),#ORIG(?Line1)' &|
     '<13,10>    STRING(''EMPLOYEE'') ,AT(5375,604),USE(?String9:4),RIGHT,TRN,#ORIG(?String9)' &|
     '<13,10>    STRING(''EMPLOYEE'') ,AT(6844,604,290),USE(?String9:8),RIGHT,TRN,#ORIG(?String9:4)' &|
     '<13,10>    STRING(''BOARD'')    ,AT(6229,604,,167),USE(?String9:7),RIGHT,TRN,#ORIG(?String9)' &|
     '<13,10>    STRING(''BOARD'')    ,AT(3417,604,233,167),USE(?String13),TRN,#ORIG(?String13)' &|
     '<13,10>    STRING(''TRS RATE'') ,AT(3354,760),USE(?String7:1),RIGHT,TRN,#ORIG(?String7)' &|
     '<13,10>    STRING(''TRS RATE'') ,AT(4063,760,521),USE(?String7:2),RIGHT,TRN,#ORIG(?String7)' &|
     '<13,10>    STRING(''THIS RATE''),AT(6177,760),USE(?String9:5),RIGHT,TRN,#ORIG(?String9:2)' &|
     '<13,10>    STRING(''THIS RATE''),AT(6865,760),USE(?String9:6),RIGHT,TRN,#ORIG(?String9:5)' &|
     '<13,10>    STRING(''THIS RATE''),AT(5396,760,542),USE(?String9:3),RIGHT,TRN,#ORIG(?String9:2)' &|
     '<13,10>    STRING(''THIS RATE''),AT(4729,760),USE(?String9:2),RIGHT,TRN,#ORIG(?String9)' &|
     '<13,10>    STRING(''BOARD'')    ,AT(4760,604),USE(?String9),RIGHT,TRN,#ORIG(?String9)'   ! Length = 2110
!===========================================
RptTestCls.GenerateCode PROCEDURE(LONG ButtonFEQ, CONST *STRING ReportCode)
Stamp_MDY_HHMM  PSTRING(16)
Stamp_Pretty    PSTRING(32)
CodeAny         ANY
CRLF            EQUATE('<13,10>')
PaperWidth      LONG
PaperHeight     LONG
PaperDesc       PSTRING(20)
IsLandscape     BOOL      !1=',LANDSCAPE'  0=Blank for Portrait
ReportWidth     LONG      !Report AT(,, Width )   Print Area Width  for DETAIL Bands = Paper Width  - Margins Left/Right
ReportHeight    LONG      !Report AT(,,, Height ) Print Area Height for DETAIL Bands = Paper Height - Margins Top/Bot - Header Height  - Footer
MarginTB        LONG(250) !Always 1/4" only THOUS support
MarginLR        LONG(250) !Always 1/4"
HeaderHeight    LONG(500) !Always 1/2"
DetailWidth     LONG      !Set = ReportWidth
DetailHeight    LONG(500) !Minimum 1/2" but increased by actual Details lowest Y+H
FooterHeight    LONG(0)   !No Footer

!MM_Divide      EQUATE(25)              !Use /25 not /25.4 for Integer numbers -- Future
Rpt_Unit        EQUATE('THOUS')         !Maybe allow MM ... or POINTS someday
Rpt_Font        EQUATE('FONT(''Arial'',9,,FONT:regular,CHARSET:ANSI)')
Rpt_Paper       PSTRING(32)     !e.g. PAPER:LETTER
Rpt_At_XYWH     PSTRING(32)     !e.g. AT(250,750,...)   REPORT,AT defines Page 1/4" Margins Details at 
Hdr_At_XYWH     PSTRING(32)     !e.g. AT(250,250,...)   HEADER,AT 1/4" down and 1/2" high
Det_At_XYWH     PSTRING(32)     !e.g. AT(0,0,...)       DETAIL
Det2_At_XYWH    PSTRING(32)     !e.g. AT(0,0,...)       DETAIL Detail 2 and 3
    CODE
    PaperWidth=8500 ; PaperHeight=11000
    Rpt_Paper   = 'PAPER:LETTER'
    
    EXECUTE PopupUnder(ButtonFEQ , |
            '|Letter '            &'<9> 8.50 x 11 in' & |     ! PAPER:LETTER   
            '|Letter - Landscape '&'<9> 8.50 x 11 in' & |     ! PAPER:LETTER Landscape  
          '|-|Legal '             &'<9> 8.50 x 14 in' & |     ! PAPER:LEGAL    
            '|Legal - Landscape ' &'<9> 8.50 x 14 in' & |     ! PAPER:LEGAL  Landscape  
          '|-|Tabloid '           &'<9> 11 x 17 in'   & |     ! PAPER:TABLOID  
            '|Ledger '            &'<9> 17 x 11 in'   )       ! PAPER:LEDGER 
!         '|-|A4 '                &'<9> 210 x 297 mm'         ! PAPER:A4 210 x 297 mm = 8.26 x 11.7 in
    BEGIN ; PaperWidth=  8500 ; PaperHeight= 11000 ; Rpt_Paper='PAPER:LETTER ' ; END                    ! Letter 8 1/2 x 11 in 
    BEGIN ; PaperWidth= 11000 ; PaperHeight=  8500 ; Rpt_Paper='PAPER:LETTER ' ; IsLandscape = 1 ; END  ! Letter 8 1/2 x 11 in Landscape
    BEGIN ; PaperWidth=  8500 ; PaperHeight= 14000 ; Rpt_Paper='PAPER:LEGAL  ' ; END                    ! Legal  8 1/2 x 14 in 
    BEGIN ; PaperWidth= 14000 ; PaperHeight=  8500 ; Rpt_Paper='PAPER:LEGAL  ' ; IsLandscape = 1 ; END  ! Legal  8 1/2 x 14 in  Landscape
    BEGIN ; PaperWidth= 11000 ; PaperHeight= 17000 ; Rpt_Paper='PAPER:TABLOID' ; END                    ! Tabloid 11 x 17 in   
    BEGIN ; PaperWidth= 17000 ; PaperHeight= 11000 ; Rpt_Paper='PAPER:LEDGER ' ; END                    ! Ledger  17 x 11 in   
!    BEGIN ; PaperWidth= 210 ; PaperHeight= 297 ; Rpt_Paper='PAPER:A4 ' ; Rpt_Unit='MM' ; END           ! A4 210 x 297 mm  = 8.26 x 11.7 in  
    ELSE
        RETURN 
    END 
    Stamp_MDY_HHMM = FORMAT(TODAY(),@d11) &'_'& FORMAT(CLOCK(),@t02)
    Stamp_Pretty   = FORMAT(TODAY(),@D3)  &' @ '& FORMAT(CLOCK(),@T3)

    !Todo: IF Rpt_Unit='MM' THEN call function and convert all numbers to MM except Paper
    ReportWidth  = PaperWidth  - (MarginLR * 2)      !E.g. 8500 - 2*250 => 8000
    ReportHeight = PaperHeight - (MarginTB * 2) - HeaderHeight - FooterHeight     !E.g. 11000 - 2*250 - 500 => 10000
    DetailWidth  = ReportWidth
    IF DetailHeight < RptTestCls.BoundBox.MaxY * 1.10 THEN  !The Parsed Detail has Y+Ht > 500 default Ht ?
       DetailHeight = RptTestCls.BoundBox.MaxY * 1.10       !Take that Plus 10%
    END

    Rpt_At_XYWH = 'AT('& MarginLR &','& (MarginTB + HeaderHeight) |                                 !Print Area for Details so - (Margins+Header+Footer)
                                                     &','& ReportWidth &','&  ReportHeight &')'     !E.g. AT(250,750,8000,10000) Letter
    Hdr_At_XYWH = 'AT('& MarginLR &','& MarginTB     &','& ReportWidth &','&  HeaderHeight &')'     !E.g. AT(250,250,8000,500)   Letter
    Det_At_XYWH = 'AT('& 0        &','& 0            &','& DetailWidth &','&  DetailHeight &')'     !E.g. AT(  0,  0,8000,500+)  Letter
    Det2_At_XYWH= 'AT('& 0        &','& 0            &','& DetailWidth &','&  MarginTB &')'         !E.g. AT(  0,  0,8000,250)  Letter
    
    PaperDesc=CLIP(SUB(Rpt_Paper,7,10)) & CHOOSE(~IsLandscape,'','_Landscape')
    CodeAny = '!Region ########### Report Test -- Generated by AtSort() -- '& Stamp_Pretty & ' ########### '& Stamp_MDY_HHMM & |
       CRLF & 'Test_Report_' & Stamp_MDY_HHMM &' ROUTINE ' & | 
       CRLF & '    DATA  !' & |
       CRLF & 'Report_'& PaperDesc &'  REPORT' & |
                      ','& Rpt_At_XYWH & |            !AT(250,750,8000,10000)
                      ','& Rpt_Unit    & |            !THOUS  someday MM ... also POINTS
 CHOOSE(IsLandscape=1,',LANDSCAPE','') & |
                      ','& Rpt_Font    & |            !FONT('Arial',9,,FONT:regular,CHARSET:ANSI)
                      ',PRE(RPT)'      & |            !PRE(RPT)
                      ',PAPER('& Rpt_Paper &')'& |    !PAPER(PAPER:LETTER)
                      '' & |            
       CRLF & '        HEADER,'& Hdr_At_XYWH &',USE(?Header) ' &  |       !e.g. HEADER,AT(250,250,8000,500)
       CRLF & '        END ' &  |
       CRLF & 'Detail1 DETAIL,'& Det_At_XYWH & ',USE(?Detail1) ' &  |     !e.g. DETAIL,AT(0,0,8000,500)
       CRLF & |             
       CRLF & CLIP(ReportCode) &  |
       CRLF & '        END ' &  |                                         !End Detail1
       CRLF & 'Detail2 DETAIL,'& Det2_At_XYWH & ',USE(?Detail2) ' &  |
       CRLF & '        END ' &  |                                         !End Detail2
       CRLF & 'Detail3 DETAIL,'& Det2_At_XYWH & ',USE(?Detail3) ' &  |
       CRLF & '        END ' &  |                                         !End Detail3
       CRLF & '    END ' & |                                              !End Report
       CRLF & '    CODE' & |
       CRLF & |          
       CRLF & ' OMIT(''** Code to Keep **'')'         & |
       CRLF & '            Paste Code to Keep here'   & |
       CRLF & ' !end of OMIT(''** Code to Keep **'')' & |
       CRLF & '!EndRegion '& Stamp_MDY_HHMM &' ={40}' & |
       CRLF

    SETCLIPBOARD(CodeAny) 
    RETURN 

 OMIT('** Code to Keep **')
            Paste Code to Keep here
 !end of OMIT('** Code to Keep **')
    
    OMIT('** Example Rpt **')
ReportLetterPortrait REPORT,AT(250,750,8000,10000),FONT('Arial',9,,FONT:regular,CHARSET:ANSI),PRE(RPT),PAPER(PAPER:LETTER),THOUS
        HEADER,AT(250,250,8000,500),USE(?Header)
        END
Detail  DETAIL,AT(0,0,8000,500),USE(?Detail)
        END
    END
    !end of OMIT('** Example Rpt **')
    
!===========================================
SortByCls.Init PROCEDURE()
    CODE
    SortByLast = Cfg:SortByXYCU
    IsSortByControl = INRANGE(Cfg:SortByXYCU,10,19)      !By Control in FROM is #10 - #19
    RETURN
!-------------------------------------------    
SortByCls.FmtSortBy PROCEDURE(LONG XP, LONG YP) !,STRING with Sort
    CODE      !Format Sort as a String which I think I will need to Sort with Levels and Parents       
    CASE Cfg:SortByXYCU
    OF  1 ; RETURN SELF.FmtXY(XP) & SELF.FmtXY(YP)                                  !X,Y
    OF  2 ; RETURN SELF.FmtXY(YP) & SELF.FmtXY(XP)                                  !Y,X
    OF 11 ; RETURN lower(SUB(SortQ:Control,1,6)) & SELF.FmtXY(XP) & SELF.FmtXY(YP)  !Control + X,Y
    OF 12 ; RETURN lower(SUB(SortQ:Control,1,6)) & SELF.FmtXY(YP) & SELF.FmtXY(XP)  !Control + Y,X 
    OF 13 ; RETURN lower(SUB(SortQ:Control,1,4)) & FORMAT(SortQ:Area,@n08) & SELF.FmtXY(YP) & SELF.FmtXY(XP)         !Control + Area   + Y,X 
    OF 14 ; RETURN lower(SUB(SortQ:Control,1,6)) & SELF.FmtXY(SortQ:ATW)   & SELF.FmtXY(SortQ:ATH) & SELF.FmtXY(YP)  !Control + Width  + Height + Y
    OF 15 ; RETURN lower(SUB(SortQ:Control,1,6)) & SELF.FmtXY(SortQ:ATH)   & SELF.FmtXY(SortQ:ATW) & SELF.FmtXY(YP)  !Control + Height + Width  + Y    
    OF 16 ; RETURN lower(SUB(SortQ:Control,1,6)) & SELF.FmtXY(SortQ:ATW)   & SELF.FmtXY(YP) & SELF.FmtXY(XP)         !Control + Width  + Y,X
    OF 17 ; RETURN lower(SUB(SortQ:Control,1,6)) & SELF.FmtXY(SortQ:ATH)   & SELF.FmtXY(YP) & SELF.FmtXY(XP)         !Control + Height + Y,X
    OF 21 ; RETURN lower(SortQ:UseFEQ)                                              !UseFEQ
    OF 22 ; RETURN lower(Left(SortQ:CodeLine))                                      !Code Line
    OF 99 ; RETURN FORMAT(Random(1,999999),@N06) & SELF.FmtXY(SortQ:LineNo)         !Random for Testing. Could also mess up for Job Security, pull some's chain, sabotage
    OF  0 ; RETURN SELF.FmtXY(SortQ:LineNo)                                         !No Sort so by Original Line#
    END   
    RETURN '?Sort='& Cfg:SortByXYCU &' '& SELF.FmtXY(XP) & SELF.FmtXY(YP) 
!-------------------------------------------
SortByCls.FmtXY PROCEDURE(LONG XYPos)  !Return STRING as Format(N06) of X or Y with + or - as *
N6  STRING(6),AUTO
    CODE
    IF    XYPos > 0 THEN N6 = '+' & XYPos           !e.g. '+123'
    ELSIF XYPos < 0 THEN N6 = '*' & XYPos * -1      !e.g. '*-456'   a Dash- sorts after Plus+ so add *
    ELSE                 N6 = '+0'                  !just '+0'      wanted '0' but does not sort after *-456
    END     
    RETURN RIGHT(N6)    !RIGHT justify in 6 bytes for Sort
!===========================================
CleanupCls.DESTRUCT        PROCEDURE()
    CODE
!    SELF.HistoryQFree()
    RETURN
CleanupCls.HistoryQFree    PROCEDURE()
    CODE
!    LOOP 
!        GET(HistoryQ,1) ; IF ERRORCODE() THEN BREAK.
!        CLEAR(HistoryQ)     !deal with ANY
!        DELETE(HistoryQ)
!    END         
    RETURN
!------------------- 
TextLineCount PROCEDURE(LONG TextFEQ)!,LONG  !TEXT Prop:LineCount without trailing blanks
LastLineNo LONG,AUTO
    CODE
    LOOP LastLineNo=TextFEQ{PROP:LineCount} TO 1 BY -1
    WHILE ~TextFEQ{PROP:Line,LastLineNo}
    RETURN LastLineNo
!------------------- 
PopupUnder PROCEDURE(LONG CtrlFEQ, STRING PopMenu)!,LONG
X LONG,AUTO
Y LONG,AUTO
H LONG,AUTO
    CODE
    GETPOSITION(CtrlFEQ,X,Y,,H)
    IF CtrlFEQ{PROP:InToolBar} THEN Y -= (0{PROP:ToolBar}){PROP:Height}.
    RETURN POPUP(PopMenu,X,Y+H+1,1) 
!------------------- 
DB   PROCEDURE(STRING xMessage)
Prfx EQUATE('AtSort: ')
sz   CSTRING(SIZE(Prfx)+SIZE(xMessage)+3),AUTO
  CODE 
  sz  = Prfx & CLIP(xMessage) & '<13,10>'
  OutputDebugString( sz )
  RETURN
