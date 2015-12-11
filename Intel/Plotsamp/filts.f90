subroutine filts(ipanel2,ipanelf,ipanel20,ipanel21,iedit,tedit,ival2,tval2,itogg17,itogg18,itogg19,&
itogg16,itogg21,itogg22,ibutt3,ibutt4,ibutt5,icallid,ibuttonf)
use menu_f90 
 
 
integer*4 iedit(10),ival2(20)
character*60 tedit(10)
character*11 cnum
real tval2(20)
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
ipanelf=gmCreateComplexDialogueBox(ipanel2, 21, 10, 20, 22, GALL, ' ', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
!iPanelf=gmCreatePanel(ipanel2, 1, 9, 20, 18, &
!              	gmHpos=GLEFT, gmVpos=Gtop, gmExpand=GON, gmType=GINVERSECHISEL, &
!              	gmtitle='',gmLineCol=0, gmLineBorder=0, gmFillCol=229, gmFillBorder=0)

iPanel20=gmCreatePanel(ipanelf, 1, 1, 18, 9, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmExpand=GON, gmType=GINVERSECHISEL, &
              	gmtitle='',gmLineCol=0, gmLineBorder=0, gmFillCol=220, gmFillBorder=0)


iStatic = gmCreateTextEntry(iPanel20, 1, 1, 4, 1,'Filter from point:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival2(1) = gmCreateValueEntry(iPanel20, 5, 1, 4, 1,tval2(1), 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(iPanel20, 10, 1, 3, 1,'To point:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival2(2) = gmCreateValueEntry(iPanel20, 13, 1, 4, 1,tval2(2), 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)


iStatic = gmCreateTextEntry(iPanel20, 1, 2, 4, 1,'Invert the data :', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
iTogg21 = gmCreateToggleButton(ipanel20, 6, 2, 2, 1, 'Yes', 0, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP, gmCallback=201)

iTogg22 = gmCreateToggleButton(ipanel20, 9, 2, 2, 1, 'No', 1, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP, gmCallback=202)

iStatic = gmCreateTextEntry(iPanel20, 1, 3, 6, 1,'Amplify data by gain:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival2(3) = gmCreateValueEntry(iPanel20, 7, 3, 4, 1,tval2(3), 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
iStatic = gmCreateTextEntry(iPanel20, 1, 4, 4, 1,'Points in buffer:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival2(4) = gmCreateValueEntry(iPanel20, 5, 4, 4, 1,tval2(4), 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(iPanel20, 10, 4, 5, 1,'Points in overlap:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival2(5) = gmCreateValueEntry(iPanel20, 15, 4, 2, 1,tval2(5), 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)	

iStatic = gmCreateTextEntry(iPanel20, 1, 5, 4, 1,'Output File:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   iEdit(9) = gmCreateTextEntry(iPanel20, 5, 5, 8, 1,tedit(9), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0,gmscrollable=gon, &
				gmVpos=GTOP)
 ibutt3 = gmCreatePushButton(ipanel20, 14, 5, 3, 1, 'Browse', gmType=GUSERDEFINED, &
  gmoffcol=27, gmTextCol=0, gmVpos=GTOP,gmCallback=203)
             
!iTogg16 = gmCreateToggleButton(ipanel20, 10, 8, 1, 1, '', 0, &
 !  gmType=G3DCHECKBOX, gmHpos=Gleft, gmVpos=GTOP)
iStatic = gmCreateTextEntry(iPanel20, 1, 7, 13, 1,' First check this section and press OK to continue: ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ibutt4 = gmCreatePushButton(ipanel20, 14, 7, 2, 1, 'OK', gmType=GUSERDEFINED, &
  gmoffcol=225, gmTextCol=0,gmHpos=Gleft, gmVpos=GTOP,gmCallback=204)
  call gmsetwidgetstatus(ibutt4,GSELECTABLE)
!call gmsetwidgetstatus(itogg16,GUNSELECTABLE)

  	iStatic = gmCreateTextEntry(iPanelf, 1, 17, 3, 1,' New rate: ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
	ival2(9) = gmCreateValueEntry(iPanelf, 4, 17, 4, 1,tval2(9), 20,3, GEDIT, &
				gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
	iStatic = gmCreateTextEntry(iPanelf, 9, 17, 3, 1,' New calfac: ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
	ival2(11) = gmCreateValueEntry(iPanelf, 13, 17, 4, 1,tval2(11), 20,10, GEDIT, &
				gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
iStatic = gmCreateTextEntry(iPanelf, 1, 18, 7, 1,' Output sample length (sec) : ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
	ival2(12) = gmCreateValueEntry(iPanelf, 9, 18, 4, 1,tval2(12), 20,3, GEDIT, &
				gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
	if(icallid.eq.21) then
		iStatic = gmCreateTextEntry(iPanel20, 1, 6, 8, 1,' -3 dB frequency for filter (Hz) = ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
		ival2(6) = gmCreateValueEntry(iPanel20, 9, 6, 4, 1,tval2(6), 20,3, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)	
		
			iPanel21=gmCreatePanel(ipanelf, 1, 10, 18, 6, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmExpand=GON, gmType=GINVERSECHISEL, &
              	gmtitle='',gmLineCol=0, gmLineBorder=0, gmFillCol=220, gmFillBorder=0)

			iTogg17 = gmCreateToggleButton(ipanel21, 1, 1, 5, 1, 'Filter at fc [Hz]=', 1, &
			gmType=G3dradio, gmHpos=Gleft, gmVpos=GTOP, gmCallback=208)
			ival2(7) = gmCreateValueEntry(iPanel21, 6, 1, 4, 1,tval2(7),20,5, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)	
				call realtoch(tval2(6),cnum,11)
iStatic = gmCreateTextEntry(iPanel21, 10, 1, 4, 1,' to give final fc =', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
		!		tval2(17)=tval2(6)
ival2(17) = gmCreateValueEntry(iPanel21, 14, 1, 3, 1,tval2(17), 20,5, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

			iTogg18 = gmCreateToggleButton(ipanel21, 1, 2, 4, 1, 'OR at fc [Hz] =', 0, &
			gmType=G3dradio, gmHpos=Gleft, gmVpos=GTOP, gmCallback=209)
			ival2(8) = gmCreateValueEntry(iPanel21, 5, 2, 4, 1,tval2(8), 20,5, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)	

			iStatic = gmCreateTextEntry(iPanel21, 10, 2, 4, 1,' overall fc =', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
				ival2(18) = gmCreateValueEntry(iPanel21, 14, 2, 3, 1,tval2(18), 20,5, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

			iStatic = gmCreateTextEntry(iPanel21, 1, 3, 9, 1,' Keep every nth point = ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
			ival2(10) = gmCreateValueEntry(iPanel21, 11, 3, 4, 1,tval2(10), 20,0, GEDIT, &

              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
!		iTogg19 = gmCreateToggleButton(ipanel21, 16, 2, 1, 1, '', 0, &
!   gmType=G3DCHECKBOX, gmHpos=Gleft, gmVpos=GTOP)
iStatic = gmCreateTextEntry(iPanel21, 1, 4, 13, 1,' Next check this section and press OK to continue: ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ibutt5 = gmCreatePushButton(ipanel21, 14, 4, 2, 1, 'OK', gmType=GUSERDEFINED, &
  gmoffcol=225, gmTextCol=0,gmHpos=Gleft, gmVpos=GTOP,gmCallback=205)
		
		call gmsetwidgetstatus(ibutt5,GUNSELECTABLE)
!call gmsetwidgetstatus(itogg19,GUNSELECTABLE)

		
		else if(icallid.eq.22) then
		
			
		
		else
		
		
			iStatic = gmCreateTextEntry(iPanel20, 1, 6, 6, 1,' Keep every nth point: ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
			ival2(10) = gmCreateValueEntry(iPanel20, 7, 6, 5, 1,tval2(10), 20,0, GEDIT, &

              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
		endif

			
!ibutt5 = gmCreatePushButton(ipanel20, 11, 9, 3, 1, 'Change', gmType=GUSERDEFINED, &
 ! gmoffcol=38, gmTextCol=0, gmVpos=GTOP,gmCallback=49)

iButtonf = gmCreatePushButton(iPanelf, 8, 1, 5, 1, 'Continue', &
              	gmType=GUSERDEFINED, gmAccel=0, &
              	gmOncol=0, gmOffCol=225, gmTextCol=0, &
              	gmVpos=Gbottom, gmExpand=GOFF, gmCallback=211)
	 call gmsetwidgetstatus(ibuttonf,GUNSELECTABLE)
call gmdrawwindow(ipanelf)
end