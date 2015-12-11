subroutine rise_time(main,irise,fc1,fc2,trise,nset,irise1,irise2,iTogg1,iTogg2,iTogg3)
use menu_f90
integer panel1
real*4 fc1(10),fc2(10),trise(10)
type (GLIMIT) :: Graphics_frame
type(GARRAYCELL) arrayattribs
call gDefineRGB(38,1.,0.5,0.25)	!orange
 irise = gmCreateComplexDialogueBox(main,10, 3, 12, 20, GALL,'Risetime setting: ' , &
   gmVpos=GTOP, gmIconFormat=GDLLICON, gmIconFile='Gee')


! Create main panel for form
  panel1=gmCreatePanel(irise, 0, 0, 12, 20, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmFillCol=220, gmScrollMode=Gnobars)

	   
             	
  iTogg1 = gmCreateToggleButton(Panel1, 1, 1, 1, 1, &
				'', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=-31) 
  iStatic1_14_1 = gmCreateTextEntry(Panel1, 3, 1, 6, 1,'Nominal filter setting',&
				 32768, GDISPLAY, &
             	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)
				call gmSetToggleSwitch(	itogg1,Gon)
  iTogg2 = gmCreateToggleButton(Panel1, 1, 3, 1, 1, &
				'', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=-32) 
   iStatic1_14_1 = gmCreateTextEntry(Panel1, 3, 3, 6, 1,'Effect of prefilter+tape', 32768, GDISPLAY, &
             	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)
   irise1 = gmCreateTextArray(Panel1, 1, 5, 10,5 , 2, nset, &
      gmXtext='F rec [kHz]'//char(124)//'F amp [kHz] ', &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=Gvertical, gmVpos=GTOP)
   call gmSetGuiGridMode(GOFF)
   call gmEnqCellAttribs(irise1, 1, 1, arrayattribs)
   arrayattribs%height=22
   arrayattribs%width=100
   arrayattribs%backcol=101
   arrayattribs%display=gedit
   arrayattribs%ndp=5
   do i=1,nset
		call gmSetCellAttribs(irise1, 1, i, arrayattribs)
		call gmSetCellSetting(irise1, 1, i,gmValue=fc1(i))
		call gmSetCellAttribs(irise1, 2, i, arrayattribs)
		call gmSetCellSetting(irise1, 2, i,gmValue=fc2(i))
   enddo
   
  call gmSetGuiGridMode(GOn)
  
  
   iTogg3 = gmCreateToggleButton(Panel1, 1, 11, 1, 1, &
				'', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=-33) 
  
   iStatic1_14_1 = gmCreateTextEntry(Panel1, 3, 11, 6, 1,'Specify value here',32768, GDISPLAY, &
             	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

	irise2 = gmCreateTextArray(Panel1, 3, 13, 7,5 , 1, nset, gmXtext='T rise [us]', &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=Gvertical, gmVpos=GTOP)
   call gmSetGuiGridMode(GOFF)
   call gmEnqCellAttribs(irise2, 1, 1, arrayattribs)
   arrayattribs%height=22
   arrayattribs%width=100
   arrayattribs%backcol=101
   arrayattribs%display=gedit
   arrayattribs%ndp=5
   do i=1,nset
		call gmSetCellAttribs(irise2, 1, i, arrayattribs)
		call gmSetCellSetting(irise2, 1, i,gmValue=trise(i))
		
   enddo
   
  call gmSetGuiGridMode(GOn)
 
   iradio_Button1 = gmCreatePushButton(Panel1,0,0 , 12, 1, 'Continue',gmType=GUSERDEFINED, &
   gmhpos=gleft,gmVpos=Gbottom,gmOffcol=38, gmTextCol=1,&
   gmcallback=-118)
call gmdrawwindow(irise)

end