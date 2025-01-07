!From WinGDI.h all Weights
Font:FW_Thin         EQUATE(100)  !'Thin'         !Clarion has FONT:thin
Font:FW_ExtraLight   EQUATE(200)  !'Extra Light'
Font:FW_Light        EQUATE(300)  !'Light'
Font:FW_Regular      EQUATE(400)  !'Regular'      !Clarion has FONT:Regular
Font:FW_Medium       EQUATE(500)  !'Medium'
Font:FW_SemiBold     EQUATE(600)  !'Semi Bold'
Font:FW_Bold         EQUATE(700)  !'Bold'         !Clarion has FONT:Bold
Font:FW_ExtraBold    EQUATE(800)  !'Extra Bold'
Font:FW_Black        EQUATE(900)  !'Black'        !See BLACK used in Font Names more than HEAVY

! Font:FW_Heavy      EQUATE(900)  !'Heavy'
! Font:FW_Normal     EQUATE(400)  !'Normal'
!Font:FW_UltraLight  EQUATE(Font:FW_Extralight)
!Font:FW_Regular     EQUATE(Font:FW_Normal)
!Font:FW_DemiBold    EQUATE(Font:FW_SemiBold)
!Font:FW_UltraBold   EQUATE(Font:FW_ExtraBold)
!Font:FW_Black       EQUATE(Font:FW_Heavy)


!        Style=CHOOSE(INT(Weight/100),'Thin','Extra Light','Light','Regular','Medium', | !100-500
!                                     'Semi Bold','Bold','Extra Bold','Black (Heavy)', | !600-900
!                                     'Weight '& Weight )                                !Uknown