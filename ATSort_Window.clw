!ATSort_Window.clw -- Make it easier to work on an All Source Program with Window open in Separate Tab --  INCLUDE('ATSort_Window.clw')

Window WINDOW('AT() Sort REPORT - Version 2.1 - December 2023'),AT(,,435,300),GRAY,SYSTEM, |
            ICON('ATIconDA.ico'),FONT('Seogo UI',9),RESIZE
        BUTTON('New'),AT(350,2,22,10),USE(?NewThreadBtn),SKIP,FONT(,8),TIP('Start a New Thread of Tool')
        BUTTON('Halt'),AT(377,2,22,10),USE(?HaltBtn),SKIP,FONT(,8),HIDE,TIP('Halt All Threads')
        BUTTON('ReRun'),AT(403,2,28,10),USE(?RunAgainBtn),SKIP,FONT(,8),TIP('Run another of this tool EXE')
        SHEET,AT(2,4),FULL,USE(?Sheet1),NOSHEET,BELOW
            TAB(' &Original Code '),USE(?TabWith)
                BUTTON('&Sort by AT'),AT(66,19,43,14),USE(?SortBtn),SKIP,TIP('Process the code below' & |
                        ' to sort by AT().')
                LIST,AT(116,22,64,10),USE(Cfg:SortByXYCU),SKIP,VSCROLL,TIP('Normally "Sort Y,X" to s' & |
                        'ort by Y then X for Reports <13,10>Select "Not Sorted" just to Flatten Code' & |
                        ' and Align AT()<13,10>Select "Control+Y,X" to Group LINEs and BOXs the STRI' & |
                        'NGs<13,10>USE FEQ or Code Line may be useful for Source Compare<13,10>* Ind' & |
                        'icates Sorts used most '),DROP(15),FROM('Sort X,Y |#1|Sort Y,X  * |#2|Contr' & |
                        'ol+X,Y |#11|Control+Y,X  * |#12|Control+Area |#13|Control+Wd,Ht |#14|Contro' & |
                        'l+Ht,Wd |#15|Control+Wd,Y |#16|Control+Ht,Y |#17|USE( FEQ ) |#21|Code Line ' & |
                        '|#22|Random |#99|Not Sorted |#0'),FORMAT('999L(2)F')
                BUTTON('&Paste && Sort'),AT(186,19,,14),USE(?PasteBtn),SKIP,TIP('Paste Report Detail' & |
                        ' code on the Clipboard <13,10>into the below TEXT control <13,10>then proce' & |
                        'ss to Sort by AT()')
                BUTTON('&Copy'),AT(258,19,40,14),USE(?CopyOrigBtn),SKIP,ICON(ICON:Copy),TIP('Copy Or' & |
                        'ginal Code below to the Clipboard'),LEFT
                BUTTON('Clear'),AT(305,19,33,14),USE(?ClearOrigBtn),TIP('Clear Code below')
                BUTTON('Code Font'),AT(360,19,,14),USE(?FontForCodeBtn),SKIP,FONT('Consolas',9), |
                        TIP('Change the Font for TEXT showing Code')
                PROMPT('DETAIL Controls:'),AT(4,22),USE(?OrigCode:Prompt)
                TEXT,AT(4,36),FULL,USE(OrigCode),HVSCROLL,FONT('Consolas',9)
            END
            TAB(' &Flat Code '),USE(?TabFlatCode)
                BUTTON('&Copy'),AT(33,19,40,14),USE(?CopyFlatBtn),SKIP,ICON(ICON:Copy),TIP('Copy Fla' & |
                        't Unsorted code below to Clipboard'),LEFT
                BUTTON('Clean ...'),AT(85,19,37,14),USE(?StripFlatBtn),SKIP,TIP('Choose to remove cl' & |
                        'utter e.g. #ORIG(), #ORIDINAL(), USE(?String:#)<13,10>Change code like DECI' & |
                        'MAL to RIGHT')
                BUTTON('Align AT()'),AT(130,19,44,14),USE(?AlignAtFlatBtn),SKIP,TIP('Align the AT() ' & |
                        'on all lines the same <13,10>to make it easier to view and change')
                PROMPT('Max:'),AT(181,21),USE(?Cfg:AlignAt_Max:Prompt_FLAT)
                ENTRY(@n2),AT(197,21,14,9),USE(Cfg:AlignAt_Max,, ?Cfg:AlignAt_Max_FLAT),SKIP, |
                        TIP('Maximum AT() Alignment Column')
                CHECK('X,Y,H,W'),AT(216,17),USE(Cfg:AlignAt_XYWH,, ?Cfg:AlignAt_XYWH_FLAT),SKIP,FONT(,8), |
                        TIP('Align the AT(X,Y,W,H) to maximum width of each <0Dh,0Ah>Uncheck to alig' & |
                        'n AT() and X only ')
                CHECK('NoMsg'),AT(216,25),USE(Cfg:AlignAt_NoShowMsg,, ?Cfg:AlignAt_NoShowMsg_FLAT),SKIP, |
                        FONT(,8),TIP('Do Not show the Message after Align AT<13,10>FYI a summary app' & |
                        'ears as tip on the Align button')
                CHECK('Align USE()'),AT(263,20),USE(Cfg:AlignUSE_Checked,, ?Cfg:AlignUSE_Checked_FLAT), |
                        SKIP,TRN,TIP('Align the USE() to the Maximum Width found')
                PROMPT('Max:'),AT(314,21),USE(?Cfg:AlignUSE_Max:Prompt_FLAT),TRN
                ENTRY(@n2),AT(330,21,14,9),USE(Cfg:AlignUSE_Max,, ?Cfg:AlignUSE_Max_FLAT),SKIP, |
                        TIP('Maximum USE() Alignment Width')
                BUTTON('Report Test'),AT(357,19,,14),USE(?ReportTestFlatBtn),SKIP,ICON(ICON:Print1), |
                        TIP('Generate a REPORT to Test with the Flat lines in a DETAIL'),LEFT
                PROMPT('Flat:'),AT(4,22),USE(?FixdCode:Prompt)
                TEXT,AT(4,36),FULL,USE(FlatCode),HVSCROLL,FONT('Consolas',9),COLOR(0EAFFFEH)
            END
            TAB(' &AT() Sorted Code '),USE(?TabSortedCode)
                BUTTON('&Copy'),AT(33,19,40,14),USE(?CopySortedBtn),SKIP,ICON(ICON:Copy),TIP('Copy S' & |
                        'orted Report code to Clipboard'),LEFT
                BUTTON('Clean ...'),AT(85,19,37,14),USE(?StripSortedBtn),SKIP,TIP('Choose to remove ' & |
                        'clutter e.g. #ORIG(), #ORIDINAL(), USE(?String:#)<13,10>Change code like DE' & |
                        'CIMAL to RIGHT')
                BUTTON('Align AT()'),AT(130,19,44,14),USE(?AlignAtBtn),SKIP,TIP('Align the AT() on a' & |
                        'll lines the same <13,10>to make it easier to view and change')
                PROMPT('Max:'),AT(180,21),USE(?Cfg:AlignAt_Max:Prompt)
                ENTRY(@n2),AT(196,21,14,9),USE(Cfg:AlignAt_Max),SKIP,TIP('Maximum AT() Alignment Column')
                CHECK('X,Y,H,W'),AT(216,17),USE(Cfg:AlignAt_XYWH),SKIP,FONT(,8),TIP('Align the AT(X,' & |
                        'Y,W,H) to maximum width of each <0Dh,0Ah>Uncheck to align AT() and X only ')
                CHECK('NoMsg'),AT(216,25),USE(Cfg:AlignAt_NoShowMsg),SKIP,FONT(,8),TIP('Do Not show ' & |
                        'the Message after Align AT<13,10>FYI a summary appears as tip on the Align ' & |
                        'button')
                CHECK('Align USE()'),AT(263,20),USE(Cfg:AlignUSE_Checked),SKIP,TRN,TIP('Align the US' & |
                        'E() to the Maximum Width found')
                PROMPT('Max:'),AT(314,21),USE(?Cfg:AlignUSE_Max:Prompt),TRN
                ENTRY(@n2),AT(330,21,14,9),USE(Cfg:AlignUSE_Max),SKIP,TIP('Maximum USE() Alignment Width')
                BUTTON('Report Test'),AT(357,19,,14),USE(?ReportTestSortedBtn),SKIP,ICON(ICON:Print1), |
                        TIP('Generate a REPORT to Test with the Sorted lines in a DETAIL'),LEFT
                PROMPT('Sorted:'),AT(4,22),USE(?SortedCode:Prompt)
                TEXT,AT(4,36),FULL,USE(SortedCode),HVSCROLL,FONT('Consolas',9),COLOR(0EAFFFEH)
            END
            TAB(' &History '),USE(?TabHistory)
                STRING('In case something goes wrong to can recover the Original Code just double-cl' & |
                        'ick on it in the List.  Right-Click for more options.'),AT(4,20)
                LIST,AT(4,36),FULL,USE(?LIST:HistoryQ),VSCROLL,FONT('Consolas',9),FROM(HistoryQ), |
                        FORMAT('24R(2)|FM~Time~C(0)@s5@24R(2)|FM~Date~C(0)@s5@20L(1)FP~Original Code' & |
                        ' (Double click on Line to Reload or Right-Click)~C(0)S(1000)')
            END
            TAB(' Sort Q  '),USE(?TabSortQ),TIP('View Queue used to Sort Code for Debug')
                BUTTON('Make Sorted Code'),AT(4,21,,14),USE(?MakeSortedBtn),SKIP,TIP('Create AT() So' & |
                        'rted code using the below sort order.')
                BUTTON('Copy Sorted'),AT(97,21,,14),USE(?CopySortedQBtn),DISABLE,SKIP,TIP('Copy Sort' & |
                        'ed Window code to Clipboard')
                BUTTON('&Sort Q...'),AT(165,21,41,14),USE(?SortQSortBtn),SKIP,TIP('Sort below list d' & |
                        'ifferent ways')
                BUTTON('Copy Q'),AT(214,21,,14),USE(?SortQCopyBtn),SKIP,TIP('Copy below List Queue t' & |
                        'ab delimited to paste into Excel')
                LIST,AT(4,40),FULL,USE(?LIST:SortQ),VSCROLL,FONT('Consolas',9),FROM(SortQ), |
                        FORMAT('18R(2)|M~Line~C(0)@n3@60L(2)|M~At Sort How~C(0)@s255@24R(2)|M~At X~C' & |
                        '(0)@n-_7@24R(2)|M~At Y~C(0)@n-_7@22R(2)|M~Width~C(0)@s8@22R(2)|M~Ht~C(0)@s8' & |
                        '@28R(2)|M~Area~C(0)@n_8@34L(2)|M~Control~@s8@50L(2)|M~USE()~@s48@16R(1)|M~L' & |
                        'vl~C(0)@n2b@Q'' Future Level ''50L(2)|M~AT(X,Y,W,H)~C(0)@s32@80L(2)~Control' & |
                        ' Code Line~')
            END
            TAB(' Align Q  '),USE(?TabAlignQ),TIP('View Queue used to Align AT() and USE() for Debug')
                BUTTON('Copy Q'),AT(212,20,,14),USE(?AlignQCopyBtn),SKIP,TIP('Copy below List Queue ' & |
                        'tab delimited to paste into Excel')
                LIST,AT(4,40),FULL,USE(?LIST:ATFindQ),VSCROLL,FONT('Consolas',9),FROM(ATFindQ), |
                        FORMAT('25R(2)|M~Line~C(0)@n-8@20R(2)|M~Pos1~C(0)@n-8@Q'' Pos1 of AT() ''18R' & |
                        '(2)|M~(1~C(0)@n-8@Q''Paren1 Open ( Pos ''18R(2)|M~2)~C(0)@n-8@Q''Paren2 Clo' & |
                        'se ) Pos ''20R(2)|M~PCnt~C(0)@n-5b@Q''AT() Parm Count ''25L(2)|M~At_X~L(2)@' & |
                        's10@#7#25L(2)|M~At_Y~L(2)@s10@25L(2)|M~At_W~L(2)@s10@25L(2)|M~At_H~L(2)@s10' & |
                        '@20R(2)|M~Xbyt~C(0)@n-7@Q'' XBytes - X Parm Bytes  ''20R(2)|M~XLen~C(0)@n-7' & |
                        '@Q'' XLen - X Parm Len ''60L(2)|M~Code AT()~L(2)@s32@40L(2)|M~Len AT(x,y,z,' & |
                        'h)~L(2)@s31@Q'' Lenz AT X,Y,W,H ''30L(2)|M~Commas AT()~L(2)@s31@Q''ATFQ:Com' & |
                        'mas AT X,Y,W,H ''40L(2)|M~Parm Pos~L(2)@s63@Q''ATFQ:ParmPos ''40L(2)|M~USE(' & |
                        ') Pos~L(2)@s63@Q''ATFQ:UsePosZ ''200L(2)~Code Line ~L(2)@s255@')
            END
        END
    END
