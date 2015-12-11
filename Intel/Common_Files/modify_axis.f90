subroutine modify_axis(main,imodax,mtext1,mtext3,rxx,ryy,aminax,amaxax,atic,&
		imtext3,irxx,iryy,iaminax,iamaxax,iatic,jindex,logscale)

use menu_f90
character*80 mtext1
character*200 mtext3
logical logscale

 call gDefineRGB(101, 0.831372, 0.901960, 0.901960)
   call gDefineRGB(102, 0.772549, 0.850980, 0.850980)
   call gDefineRGB(103, 0.745098, 0.819607, 0.819607)
   call gDefineRGB(104, 0.704117, 0.760784, 0.760784)
   call gDefineRGB(105, 0.662745, 0.729411, 0.729411)
iy=13
imodax = gmCreateComplexDialogueBox(Main, 2, 4, 17,iy , GALL, mtext1, &
				 gmMaxWidth=32,gmMaxHeight=20,gmvpos=gtop,&
              	 gmIconFormat=GDLLICON,gmIconFile='MBIG1040')
imodax1=gmCreatePanel(imodax, 0, 0, 17, iy, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle=' ',gmExpand=GOff, gmType=GNOBOUNDARY, &
            	gmLineCol=0, gmLineBorder=0, gmFillCol=102, gmFillBorder=0)

	if(jindex.eq.3) then		
		ixt=gmCreateTextEntry(imodax1,1,1,2,1,'X min =',60,gdisplay,gmhpos=gleft,gmVpos=GTOP)
		ixt=gmCreateTextEntry(imodax1,9,1,2,1,'X max =',60,gdisplay,gmhpos=gleft,gmVpos=GTOP)
		ixt=gmCreateTextEntry(imodax1,1,3,2,1,'X tic =',60,gdisplay,gmhpos=gleft,gmVpos=GTOP)
	else if(jindex.eq.4) then
		ixt=gmCreateTextEntry(imodax1,1,1,2,1,'Y min =',60,gdisplay,gmhpos=gleft,gmVpos=GTOP)
		ixt=gmCreateTextEntry(imodax1,9,1,2,1,'Y max =',60,gdisplay,gmhpos=gleft,gmVpos=GTOP)
		ixt=gmCreateTextEntry(imodax1,1,3,2,1,'Y tic =',60,gdisplay,gmhpos=gleft,gmVpos=GTOP)
	endif	
	
	
	ndp=3
	if(logscale) then
	   if(aminax.ge.1.0) then 
	    ndp=1
	   else
	     npd=-alog10(aminax)	!e.g. 0.001
	   endif
	endif
	
	if(aminax.ge.1.e5.or.aminax.le.1.e-4) then		
		iaminax=gmCreateValueEntry(imodax1,3 , 1, 5, 1, aminax, 20, ndp, gedit,&
		  gscientific,gmhpos=gleft,gmVpos=GTOP)
	else
		iaminax=gmCreateValueEntry(imodax1,3 , 1, 5, 1, aminax, 20, ndp, gedit,&
		  gdecimal,gmhpos=gleft,gmVpos=GTOP)
	endif

	call gmsetvaluesetting(iaminax,aminax)
		
	if(amaxax.ge.1.e5.or.amaxax.le.1.e-4) then		
		iamaxax=gmCreateValueEntry(imodax1,11 , 1, 5, 1, amaxax, 20, ndp, gedit,&
		gscientific,gmhpos=gleft,gmVpos=GTOP)
	else		
		iamaxax=gmCreateValueEntry(imodax1,11 , 1, 5, 1, amaxax, 20, ndp, gedit,&
		gdecimal,gmhpos=gleft,gmVpos=GTOP)
	endif
	
	call gmsetvaluesetting(iamaxax,amaxax)
	
	if(logscale) ndp=0
	if(atic.ge.1.e5.or.atic.le.1.e-4) then		
		iatic=gmCreateValueEntry(imodax1,3 , 3, 4, 1, atic, 20, ndp, gedit,&
		gscientific,gmhpos=gleft,gmVpos=GTOP)
	else
		iatic=gmCreateValueEntry(imodax1,3 , 3, 4, 1, atic, 20, ndp, gedit,&
		gdecimal,gmhpos=gleft,gmVpos=GTOP)
	endif

	call gmsetvaluesetting(iatic,atic)

	ixt=gmCreateTextEntry(imodax1,1,5,2,1,'Label =',60,gdisplay,gmhpos=gleft,gmVpos=GTOP)
	
	imtext3=gmCreateTextEntry(imodax1,3,5,13,1,mtext3,100,gedit,&
	gmentrytype=grichedit,gmhpos=gleft,gmvpos=gtop,gmExpand = GON,&
                                        gmSelect = 350,gmChange = 350) 	
	ixt=gmCreateTextEntry(imodax1,1,7,2,1,'x =',60,gdisplay,gmhpos=gleft,gmVpos=GTOP)
		irxx=gmCreateValueEntry(imodax1,3 , 7, 5, 1, rxx, 20, 5, gedit,&
		gscientific,gmhpos=gleft,gmVpos=GTOP)

		call gmsetvaluesetting(irxx,rxx)
	
	ixt=gmCreateTextEntry(imodax1,9,7,2,1,'y =',60,gdisplay,gmhpos=gleft,gmVpos=GTOP)
		
	

		iryy=gmCreateValueEntry(imodax1,11 , 7, 5, 1, ryy, 20, 5, gedit,&
		gscientific,gmhpos=gleft,gmVpos=GTOP)

		call gmsetvaluesetting(iryy,ryy)

	it=gmCreateTextEntry(imodax1,1,9,15,1,'To change label colour,font,etc, click on the toolbar icons',60,gdisplay,gmhpos=gleft,gmVpos=GTOP)
	it=gmCreateTextEntry(imodax1,1,10,15,1,'When finished, click OK button.',60,gdisplay,gmhpos=gleft,gmVpos=GTOP)


	ivb=gmCreatePushButton(imodax1,1,0, 2, 1,'OK',&
    gmType=GSTANDARD, gmSize=100,gmTextCol=0, &
    gmVpos=Gbottom, gmHpos=Gright,gmExpand=GOFF,gmCallback=360)
	Icon1_16 = gmcreateicon(imodax1, 0,0,1,1,'Mstd1009',  &
              	GDLLICON,0,&
            gmHpos=Gleft, gmVpos=Gbottom,&
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Bold text',&
                 gmcallback = 301)

! Create icon Icon25 child of Toolbar1_3
   Icon1_17 = gmcreateicon(imodax1,1,0,1,1 ,'Mstd1010',  &
              	GDLLICON,0,&
            gmHpos=Gleft, gmVpos=Gbottom,&
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Italic ',&
                 gmcallback = 302)

! Create icon Icon26 child of Toolbar1_3
   Icon1_18 = gmcreateicon(imodax1,2,0,1,1 ,'Mstd1011',  &
              	GDLLICON,0,&
            gmHpos=Gleft, gmVpos=Gbottom,&
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Underline ',&
                 gmcallback = 303)
! Create icon Icon19 child of Toolbar1_3
   Icon1_19 = gmcreateicon(imodax1,3,0,1,1 ,'Mstd1012',  &
              	GDLLICON,0,&
            gmHpos=Gleft, gmVpos=Gbottom,&
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Left Justify ',&
                 gmcallback = 304)

! Create icon Icon20 child of Toolbar1_3
   Icon1_20 = gmcreateicon(imodax1, 4,0,1,1,'Mstd1013',  &
              	GDLLICON,0,&
            gmHpos=Gleft, gmVpos=Gbottom,&
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Centre Justify ',&
                 gmcallback = 305)
! Create icon Icon27 child of Toolbar1_3
   Icon1_21 = gmcreateicon(imodax1,5,0,1,1 ,'Mstd1014',  &
              	GDLLICON,0,&
            gmHpos=Gleft, gmVpos=Gbottom,&
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Right Justify ',&
                 gmcallback = 306)
! Create icon Icon27 child of Toolbar1_3
   Icon1_22 = gmcreateicon(imodax1, 6,0,1,1,'Msml1035',  &
              	GDLLICON,0,&
            gmHpos=Gleft, gmVpos=Gbottom,&
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Color Selector ',&
                 gmcallback = 307)

! Create button Button4 child of Toolbar1_3
   Icon1_23 = gmcreateicon(imodax1,7,0,1,1 , 'Msml1004', &
   GDLLICON,0,&
            gmHpos=Gleft, gmVpos=Gbottom,&
              gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Delete ',&
                 gmcallback = 308)	
! Create button Button5 child of Toolbar1_3
   Icon1_24 = gmcreateicon(imodax1, 8,0,1,1, 'Msml1022', &
   GDLLICON,0,&
            gmHpos=Gleft, gmVpos=Gbottom,gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Move ',&
                gmcallback = 309)
	call gmdrawwindow(imodax)

end


