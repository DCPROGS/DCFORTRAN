subroutine add_new_text(main,icall,mtext1,mtext2,iy,itext_entry,new_text_entry,&
			ivalx,rxx,ivaly,ryy,indx)
use menu_f90
integer:: itext_entry
character*80 mtext1
character*200 mtext2
	
	itext_entry = gmCreateComplexDialogueBox(Main, 27, 7, 17, iy+7, GALL, mtext1, &
				 gmMaxWidth=32,gmMaxHeight=20,gmvpos=gtop,&
              	 gmIconFormat=GDLLICON,gmIconFile='MBIG1040')

	k=4
    if(indx.eq.1.or.indx.eq.3.or.indx.eq.4.or.indx.eq.5.or.indx.gt.80) then
	new_text_entry=gmCreateTextEntry(itext_entry,1,1,15,iy,mtext2,100,gedit,&
	gmentrytype=grichedit,gmhpos=gleft,gmvpos=gtop,gmExpand = GON,&
                                        gmSelect = 350,gmChange = 350) 	
	endif
	ixt=gmCreateTextEntry(itext_entry,1,iy+2,1,1,'x =',60,gdisplay,gmhpos=gleft,gmVpos=GTOP)
	
	ivalx=gmCreateValueEntry(itext_entry,3 , iy+2, 4, 1, rxx, 20, k, gedit,&
	gscientific,gmhpos=gleft,&
	gmVpos=GTOP)
	IF(rxx.EQ.0.) CALL GMSETVALUESETTING(Ivalx,0)
	iyt=gmCreateTextEntry(itext_entry,8,iy+2,1,1,'y=',60,gdisplay,gmhpos=gleft,gmVpos=GTOP)
	ivaly=gmCreateValueEntry(itext_entry, 10, iy+2, 4, 1, ryy, 20, k, gedit,gmentryformat=gscientific,gmhpos=gleft,&
	gmVpos=GTOP)
	IF(ryy.EQ.0.) CALL GMSETVALUESETTING(Ivaly,0)
	it=gmCreateTextEntry(itext_entry,1,iy+3,15,1,'For thext attributes please click on the icons on the toolbar',60,gdisplay,gmhpos=gleft,gmVpos=GTOP)
	it=gmCreateTextEntry(itext_entry,1,iy+4,15,1,'When you finished click OK button',60,gdisplay,gmhpos=gleft,gmVpos=GTOP)
	
	ivb=gmCreatePushButton(itext_entry,13,0, 2, 1,'OK',&
              	gmType=GSTANDARD, gmSize=100,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=290)
	itoolbar1_2 = gmCreatetoolbar(itext_entry,GTOP,9) 
	itext_arrayp=gmCreatePanel(itext_entry, 0, 0, 10,1, &
              	gmHpos=Gleft, gmVpos=Gbottom, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

	Icon1_16 = gmcreateicon(itext_arrayp, 0,0,1,1,'Mstd1009',  &
              	GDLLICON,0,&
            gmHpos=Gright, gmVpos=GTOP,&
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Bold text',&
                 gmcallback = 301)

! Create icon Icon25 child of Toolbar1_3
   Icon1_17 = gmcreateicon(itext_arrayp,1,0,1,1 ,'Mstd1010',  &
              	GDLLICON,0,&
            gmHpos=Gright, gmVpos=GTOP,&
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Italic ',&
                 gmcallback = 302)

! Create icon Icon26 child of Toolbar1_3
   Icon1_18 = gmcreateicon(itext_arrayp,2,0,1,1 ,'Mstd1011',  &
              	GDLLICON,0,&
            gmHpos=Gright, gmVpos=GTOP,&
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Underline ',&
                 gmcallback = 303)
! Create icon Icon19 child of Toolbar1_3
   Icon1_19 = gmcreateicon(itext_arrayp,3,0,1,1 ,'Mstd1012',  &
              	GDLLICON,0,&
            gmHpos=Gright, gmVpos=GTOP,&
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Left Justify ',&
                 gmcallback = 304)

! Create icon Icon20 child of Toolbar1_3
   Icon1_20 = gmcreateicon(itext_arrayp, 4,0,1,1,'Mstd1013',  &
              	GDLLICON,0,&
            gmHpos=Gright, gmVpos=GTOP,&
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Centre Justify ',&
                 gmcallback = 305)
! Create icon Icon27 child of Toolbar1_3
   Icon1_21 = gmcreateicon(itext_arrayp,5,0,1,1 ,'Mstd1014',  &
              	GDLLICON,0,&
            gmHpos=Gright, gmVpos=GTOP,&
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Right Justify ',&
                 gmcallback = 306)
! Create icon Icon27 child of Toolbar1_3
   Icon1_22 = gmcreateicon(itext_arrayp, 6,0,1,1,'Msml1035',  &
              	GDLLICON,0,&
            gmHpos=Gright, gmVpos=GTOP,&
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Color Selector ',&
                 gmcallback = 307)

! Create button Button4 child of Toolbar1_3
   Icon1_23 = gmcreateicon(itext_arrayp,7,0,1,1 , 'Msml1004', &
   GDLLICON,0,&
            gmHpos=Gright, gmVpos=GTOP,&
              gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Delete ',&
                 gmcallback = 308)	
! Create button Button5 child of Toolbar1_3
   Icon1_24 = gmcreateicon(itext_arrayp, 8,0,1,1, 'Msml1022', &
   GDLLICON,0,&
            gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Move ',&
                gmcallback = 309)
	call gmdrawwindow(itext_entry)

end