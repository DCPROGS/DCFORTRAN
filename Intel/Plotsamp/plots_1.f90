subroutine plots(ipanel2,ipanelp,ipanel10,ival1,tval1,iTogg11,itogg12,iTogg13,itogg14,iTogg15,&
ibutt1,itext3,itext4,itext5,itext6,itoggle01,itoggle02,itoggle03,itoggle04,&
poplot,ngrid,ibutton11,ibutton21)
use menu_f90 
 
 logical poplot
integer*4 iedit(20),ival1(20)
character*60 tedit(20)
real tval1(20)

 call gDefineRGB(227, 1.000000, 0.458824, 0.458824)
 call gDefineRGB(228, 1.000000, 1.000000, 0.517647)
  call gDefineRGB(225, 1.000000, 0.866667, 0.733333)
   call gDefineRGB(226, 0.996078, 0.780392, 0.705882)
    call gDefineRGB(220, 1.000000, 1.000000, 0.807843)
   call gDefineRGB(221, 0.796078, 1.000000, 0.592157)
   call gDefineRGB(222, 0.682353, 0.843137, 1.000000)
   call gDefineRGB(223, 1.000000, 0.909804, 0.909804)
  call gDefineRGB(141, 0.192157, 0.984314, 0.964706)
  call gDefineRGB(229, 0.836078, 0.950000, 0.592157)

ipanelp=gmCreateComplexDialogueBox(ipanel2, 21, 11, 20, 21, GALL, ' ', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
!iPanelp=gmCreatePanel(ipanel2, 1, 9, 20, 18, &
!              	gmHpos=GLEFT, gmVpos=Gtop, gmExpand=GON, gmType=GINVERSECHISEL, &
!              	gmtitle='',gmLineCol=0, gmLineBorder=0, gmFillCol=229, gmFillBorder=0)
iPanel10=gmCreatePanel(ipanelp, 1, 1, 18, 13, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmExpand=GON, gmType=GINVERSECHISEL, &
              	gmtitle='',gmLineCol=0, gmLineBorder=0, gmFillCol=220, gmFillBorder=0)


iStatic = gmCreateTextEntry(iPanel10, 1, 1, 4, 1,'Draw from point:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival1(1) = gmCreateValueEntry(iPanel10, 5, 1, 4, 1,tval1(1), 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(iPanel10, 10, 1, 3, 1,'To point:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival1(2) = gmCreateValueEntry(iPanel10, 13, 1, 4, 1,tval1(2), 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)



iStatic = gmCreateTextEntry(iPanel10, 1, 2, 10, 1,'Interleave traces with plot of P(open) :', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

iRadio11 = gmCreateRadioBox(iPanel10, 11, 2, 8,1 , gmType=GFREEFORM, gmBorderType=Gnone, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle=' ', gmFillCol=220,gmVpos=GTOP)

if(poplot) then
iTogg11 = gmCreateToggleButton(iradio11, 1, 0, 3, 1, 'Yes', 1, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmCallback=101)

iTogg12 = gmCreateToggleButton(iradio11, 4, 0, 3, 1, 'No', 0, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmCallback=102)
else
iTogg11 = gmCreateToggleButton(iradio11, 1, 0, 3, 1, 'Yes', 0, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmCallback=101)

iTogg12 = gmCreateToggleButton(iradio11, 4, 0, 3, 1, 'No', 1, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmCallback=102)
endif

iStatic = gmCreateTextEntry(iPanel10, 1, 3, 11, 1,'P(open) measured over a segment of [ms]:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival1(7) = gmCreateValueEntry(iPanel10, 12, 3, 4, 1,tval1(7), 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
			
iStatic = gmCreateTextEntry(iPanel10, 1, 4, 3, 1,'Gridlines:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
iRadio10 = gmCreateRadioBox(iPanel10, 1, 5, 13,1 , gmType=GFREEFORM, gmBorderType=Gnone, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle=' ', gmFillCol=220,gmVpos=GTOP)

!iToggle01 = gmCreateToggleButton(iRadio10, 1, 0, 2, 1, &
!  'none', 1, gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmCallback=36)
iToggle02 = gmCreateToggleButton(iRadio10, 0, 0, 4, 1, &
  ' 1 (P=0,5)', 0, gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmCallback=103)
  iToggle03 = gmCreateToggleButton(iRadio10, 4, 0, 6, 1, &
  ' 3 (P=0.25,0.5,0.75)', 0, gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmCallback=104)
  iToggle04 = gmCreateToggleButton(iRadio10, 10, 0, 3, 1, &
  'specify:', 0, gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)
ival1(11) = gmCreateValueEntry(iPanel10, 14, 5, 3, 1,tval1(11), 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF) 
 
  if(poplot) then
	if(ngrid.eq.1) then
		call gmsetwidgetstatus(itoggle02,GCHECKED)
	else if(ngrid.eq.3) then
		call gmsetwidgetstatus(itoggle03,GCHECKED)
	else
		call gmsetwidgetstatus(itoggle04,GCHECKED)
	endif
  else
	call gmsetwidgetstatus(ival1(7),GUNSELECTABLE)
	call gmsetwidgetstatus(ival1(11),GUNSELECTABLE)
!call gmsetwidgetstatus(itoggle01,GUNSELECTABLE)
call gmsetwidgetstatus(itoggle02,GUNSELECTABLE)
call gmsetwidgetstatus(itoggle03,GUNSELECTABLE)
call gmsetwidgetstatus(itoggle04,GUNSELECTABLE)

endif
iStatic = gmCreateTextEntry(iPanel10, 1, 6, 5, 1,'Seconds per line', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival1(3) = gmCreateValueEntry(iPanel10, 7, 6, 4, 1,tval1(3), 10,2, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
				              
iStatic = gmCreateTextEntry(iPanel10, 1, 7, 5, 1,' Traces per page ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival1(4) = gmCreateValueEntry(iPanel10, 7, 7, 4, 1,tval1(4), 32768,0, GEDIT, &

              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(iPanel10, 1, 8, 7, 1,'Draw frame round traces :', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
				iRadio101 = gmCreateRadioBox(iPanel10, 8, 8, 5,1 , gmType=GFREEFORM, gmBorderType=Gnone, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle=' ', gmFillCol=220,gmVpos=GTOP)

iTogg13 = gmCreateToggleButton(iradio101, 0, 0, 2, 1, 'Yes', 0, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)

iTogg14 = gmCreateToggleButton(iradio101, 3, 0, 2, 1, 'No', 1, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)

iStatic = gmCreateTextEntry(iPanel10, 1, 9, 6, 1,' Plot every nth point: ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival1(10) = gmCreateValueEntry(iPanel10, 7, 9, 4, 1,tval1(10), 10,0, GEDIT, &

              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
!iTogg15 = gmCreateToggleButton(ipanel10, 10, 12, 1, 1, '', 0, &
!   gmType=G3DCHECKBOX, gmHpos=Gleft, gmVpos=GTOP)

iStatic = gmCreateTextEntry(iPanel10, 1, 11, 13, 1,' Check this section and press the button to continue: ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ibutt1 = gmCreatePushButton(ipanel10, 14, 11, 2, 1, 'OK', gmType=GUSERDEFINED, &
  gmoffcol=225, gmTextCol=0,gmHpos=Gleft, gmVpos=GTOP,gmCallback=105)
call gmsetwidgetstatus(ibutt1,GUNSELECTABLE)
!call gmsetwidgetstatus(itogg15,GUNSELECTABLE)

!ibutt2 = gmCreatePushButton(ipanel10, 10, 7, 3, 1, 'Change', gmType=GUSERDEFINED, &
!  gmoffcol=38, gmTextCol=0, gmVpos=GTOP,gmCallback=29)
 
iStatic = gmCreateTextEntry(iPanelp, 1, 15, 10, 1,' Sample duration (seconds) ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival1(8) = gmCreateValueEntry(iPanelp, 11, 15, 4, 1,tval1(8), 20,3, GEDIT, &

              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(iPanelp, 1, 16, 5, 1,' Number of pages: ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival1(6) = gmCreateValueEntry(iPanelp, 6, 16, 4, 1,tval1(6), 10,0, GEDIT, &
gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
iStatic = gmCreateTextEntry(iPanelp, 11, 16, 4, 1,'Points per line :', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival1(9) = gmCreateValueEntry(iPanelp, 15, 16, 4, 1,tval1(9), 10,0, GEDIT, &
gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
iStatic = gmCreateTextEntry(iPanelp, 1, 17, 10, 1,' Vertical separation of traces (pA) ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival1(5) = gmCreateValueEntry(iPanelp, 11, 17, 4, 1,tval1(5), 20,10, GEDIT, &

              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
!ival1(8) = gmCreateValueEntry(iPanelp, 16, 16, 4, 1,0, 20,0, Gdisplay, &

!              	gmType=GSTANDARD, gmhpos=GLEFT, &
!              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
!              	gmVpos=GTOP, gmExpand=GOFF)
!ival1(9) = gmCreateValueEntry(iPanelp, 16, 17, 4, 1,0, 20,0, Gdisplay, &

!              	gmType=GSTANDARD, gmhpos=GLEFT, &
!              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
!              	gmVpos=GTOP, gmExpand=GOFF)
				! Create main panel for form

itext3 = gmCreateTextEntry(iPanelp, 16, 17, 4, 1,' ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

itext4 = gmCreateTextEntry(iPanelp, 16, 18, 4, 1,' ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

!itext5 = gmCreateTextEntry(iPanelp, 16, 19, 3, 1,' ', 32768, GDISPLAY, &
!              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
!             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
!				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

!itext6 = gmCreateTextEntry(iPanelp, 16, 20, 3, 1,' ', 32768, GDISPLAY, &
!              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
!             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
!				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

!iButton11 = gmCreatePushButton(iPanelp, 1, 19, 9, 1, 'Go to previous analysis', &
!              	gmType=GUSERDEFINED, gmAccel=0, &
!              	gmOncol=0, gmOffCol=225, gmTextCol=0, &
!              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=109)
!call gmsetwidgetstatus(ibutton11,GUNSELECTABLE)
iButton21 = gmCreatePushButton(iPanelp, 10, 19, 9, 1, 'Continue', &
              	gmType=GUSERDEFINED, gmAccel=0, &
              	gmOncol=0, gmOffCol=225, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=108) ! 110

call gmsetwidgetstatus(ibutton21,GUNSELECTABLE)
call gmdrawwindow(ipanelp)
end