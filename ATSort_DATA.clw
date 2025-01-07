!ATSort_DATA.clw -- Make it easier to work on an All Source Program with Data open in Separate Tab --  INCLUDE('ATSort_DATA.clw')

    SECTION('GLOBAL DATA')                      !------------------------------------ GLobal DATA
HistoryQ    QUEUE,PRE(HisQ)     !Shared by all threads. No thread protection but should not need it since only accessed by user action
Time            STRING(5)       !HisQ:Time       
Date            STRING(5)       !HisQ:Date       
CodeSnip        STRING(1000)    !HisQ:CodeSnip  A reasonable Len to show in List Column
CodeTip         STRING(2000)    !HisQ:CodeTip   Tool Tip to see with in lines
OrigAny         ANY             !HisQ:OrigAny
            END                     
                     
    SECTION('AtSortReport Procedure DATA')      !------------------------------------ Procedure DATA
FlattenCls      CBCodeFlattenClass     

DOO  CLASS
SortRtn         PROCEDURE()     !Flatten code, then parse into SortQ
MakeSortedRtn   PROCEDURE()     !Concat FLatCode into SortedCode in SortQ Order
StripClutterRtn PROCEDURE(LONG BtnFEQ, LONG CodeTextFEQ, *STRING CodeTextUseString)     !Remove #ORIG,#ORD,USE(?STRING from SortedCode
AlignAtRtn      PROCEDURE(LONG TxtFEQ, LONG AlignBtnFEQ) !In SortedCode align AT() so can see in source BUTTON('Alight AT()'),USE(?AlignAtBtn)
AlignQCopyBtn   PROCEDURE()
AddHistoryRtn   PROCEDURE()     !Add FlatCode to HistoryQ so can recall original code 
FontForCodeRtn  PROCEDURE(LONG ButtonFEQ)           !Change TEXT control Font that usually Consolas 9 - Pass (Zero) for Window Open
FontSetOnText   PROCEDURE(<STRING FntName>,LONG FntSize=0,LONG FntStyle=0)
FontStyleName   PROCEDURE(LONG FntStyle),STRING     !Describe Font Style
NewThreadRun    PROCEDURE(LONG TakeHistory=0)
SortQSortBtn    PROCEDURE()     !Popup to Sort the Debug List Q
SortQCopyBtn    PROCEDURE()     !Copy the Debug List Q to clip to paste into Excel
SetTestRtn      PROCEDURE()     !Set OrigCode to Test Window 
    END

RptTestCls  CLASS               !For the Button ReportTestSortedBtn to Generate a REPORT
GenerateCode    PROCEDURE(LONG ButtonFEQ, CONST *STRING ReportCode)     !Generate a Report to tets the Lines
BoundBox    GROUP
MaxX            LONG  !RptTestCls.BoundBox.MaxX
MaxY            LONG  !RptTestCls.BoundBox.MaxY
            END
            END

CleanupCls  CLASS
DESTRUCT        PROCEDURE()
HistoryQFree    PROCEDURE()
            END                                         

FlatMapQ    QUEUE(CBFlatLineMapQType),PRE(FlatMapQ)
!Line1           LONG            !FlatMapQ:Line1
!Line2           LONG            !FlatMapQ:Line2
!Txt             STRING(100)     !FlatMapQ:Txt  
            END
!Cfg:SortByXYCU      BYTE(2)    !1=(X,Y); 2=(Y,X); 11=Control+X,Y; 12=Control+Y,X; 21=USE FEQ; 22=Code Line; 99=Random; 0=No Sort (LineNo)
SortByLast      BYTE       !Last Sorted By XYCU = Cfg:SortByXYCU each 
IsSortByControl BOOL       !Last Sorted was Control 10,19
SortByCls   CLASS
Init            PROCEDURE()
FmtSortBy       PROCEDURE(LONG XPos, LONG YPos),STRING  !Format Sort per Cfg:SortByXYCU
FmtXY           PROCEDURE(LONG XYPos),STRING            !Format X or Y number as String(6) with +/*- so sorts right
            END 
SortQ   QUEUE,PRE(SortQ)
LineNo      USHORT              !SortQ:LineNo
ATSort      STRING((8+6*2)*16)  !SortQ:ATSort   How to Sort as Text formatted that Sorts ((8+6*2)*16 room for Max Levels *16 .. not sure works perfectly
ATX         LONG                !SortQ:ATX
ATY         LONG                !SortQ:ATY
ATW         PSTRING(9)          !SortQ:ATW      String Because can be blank when omitted for Full or Default
ATH         PSTRING(9)          !SortQ:ATH
Area        LONG                !SortQ:Area     ABS(W * H)  a way to see largest controls and put them 1st or last
Control     STRING(8)           !SortQ:Control  longest ELLIPSE
UseFEQ      STRING(48)          !SortQ:UseFEQ
Level       USHORT              !SortQ:LEVEL    Someday support nested Parent Levels
AtCode      STRING(32)          !SortQ:AtCode    ??? Add FULL to this ???
CodeLine    STRING(1024)        !SortQ:CodeLine
SortCodeLne STRING(128)         !SortQ:SortCodeLne  Lower CodeLine to sort well
SortUseFEQ  STRING(48)          !SortQ:SortUseFEQ   Lower to sort well
        END
            
OrigCode    STRING(60000)
FlatCode    LIKE(OrigCode)
SortedCode  LIKE(OrigCode)

QNdx        LONG   
!!!AlignAt_Max             USHORT(40)
!!!AlignAt_NoShowMsg       BOOL
!!!AlignAt_XYWH            BOOL(1)
!!!AlignAt_BtnTipOriginal  PSTRING(128)
!!!AlignUSE_Max            USHORT(30)
!!!AlignUSE_Checked        BOOL(1)

ATFindQ     QUEUE,PRE(ATFQ)         !Finds the Column to Align AT() and stores in Queue
LineX           LONG                !ATFQ:LineX  
AtPos1          LONG                !ATFQ:AtPos1   
AtParen1        LONG                !ATFQ:AtParen1 
AtParen2        LONG                !ATFQ:AtParen2 
AtParmCnt       LONG                !ATFQ:AtParmCnt 
At_XYWH_Grp     GROUP,PRE()         !ATFQ:At_XYWH_Grp   FYI ATFQ:AtArray DIM(4) Below
At_X              STRING(10)        !ATFQ:At_X          Setup in Queue as 4 fields so can see in LIST
At_Y              STRING(10)        !ATFQ:At_Y   
At_W              STRING(10)        !ATFQ:At_W   
At_H              STRING(10)        !ATFQ:At_H   
                END
Xbytes          SHORT               !ATFQ:Xbytes        !Parm 1 bytes
XLen            SHORT               !ATFQ:XLen
CodeAT          STRING(32)          !ATFQ:CodeAT        for seeing in LIST to debug
LenzAT          PSTRING(32)         !ATFQ:LenzAT        for debug ATFQ:LenAtXYWH[] lengths of X,Y,W,H
CommaZ          PSTRING(32)         !ATFQ:CommaZ        for debug ATFQ:Commas[]    comma position X,Y,W,H
ParmZPos        PSTRING(64)         !ATFQ:ParmZPos      for debug ATFQ:ParmPos[]   Comma and Lengs from class call
UsePosInfo      PSTRING(64)         !ATFQ:UsePosInfo    for debug USE() location Info
CodeLine        STRING(1024)        !ATFQ:CodeLine      original Code that was parsed
AtParmPos       LONG,DIM(4,2)       !ATFQ:AtParmPos[]   AT() positions and lengths
AtCommas        LONG,DIM(4)         !ATFQ:AtCommas[]    AT() comma postion 
AtLenXYWH       LONG,DIM(4)         !ATFQ:AtLenXYWH[]   AT() length of X,Y,W,H  

UsePos1         LONG                !ATFQ:UsePos1   
UseParen1       LONG                !ATFQ:UseParen1 
UseParen2       LONG                !ATFQ:UseParen2 
UseLen          LONG                !ATFQ:UseLen
            END

ALine       STRING(2048)    !Usually A Source Code line from TEXT
ULine       STRING(2048)    !Upper() of ALine
ULeftLine   STRING(2048)    !Left(Upper()) of ALine
LastALine   LIKE(ALine)
LastALineLen     LONG
LineX       LONG
OutX        LONG
Ndx         LONG
PasteANY    ANY

ReRunGrp   GROUP,PRE(ReRn) 
WinX            LONG    !ReRn:WinX
WinY            LONG    !ReRn:WinY
WinW            LONG    !ReRn:WinW
WinH            LONG    !ReRn:WinH
            END

ConfigGrp   GROUP,PRE(CFG) !TODO: Button to Save these to Registry or INI
SortByXYCU              BYTE(2)         !Cfg:SortByXYCU         !1=(X,Y); 2=(Y,X); 11=Control+X,Y; 12=Control+Y,X; 21=USE FEQ; 22=Code Line; 99=Random; 0=No Sort (LineNo)
AlignAt_Max             USHORT(40)      !Cfg:AlignAt_Max           
AlignAt_NoShowMsg       BOOL            !Cfg:AlignAt_NoShowMsg     
AlignAt_XYWH            BOOL(1)         !Cfg:AlignAt_XYWH          
AlignAt_BtnTipOriginal  PSTRING(128)    !Cfg:AlignAt_BtnTipOriginal
AlignUSE_Max            USHORT(30)      !Cfg:AlignUSE_Max          
AlignUSE_Checked        BOOL(1)         !Cfg:AlignUSE_Checked      

HistoryNo               LONG            !Cfg:HistoryNo  Load at New Start
FontName                STRING(64)      !Cfg:FontName
FontSize                LONG            !Cfg:FontSize
FontStyle               LONG            !Cfg:FontStyle
            END