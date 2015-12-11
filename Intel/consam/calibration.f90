subroutine calibration(iForm1,iForm2,iForm2MainPanel,Panel2,nchan,iall,edit2_2, & 
		  edit2_3a,edit2_3,edit2_4,edit2_5,edit2_7,edit2_8,edit2_12,edit2_13a, & 
		  edit2_13,edit2_14, edit2_15, edit2_17, edit2_18,Toggle2_6, &
          Toggle2_16)
use gino_f90
use menu_f90

integer :: iform1
integer :: iform2
integer :: iform2MainPanel
integer :: Panel2
integer :: Static2_00
integer :: Static2_01
integer :: Static2_1 
integer :: Static2_2 
integer :: Static2_3 
integer :: Static2_3a 
integer :: Static2_4 
integer :: Static2_5 
integer :: Static2_6 
integer :: Static2_7 
integer :: Static2_8 
integer :: Static2_9 
integer :: Static2_11 
integer :: Static2_12
integer :: Static2_13 
integer :: Static2_14 
integer :: Static2_15 
integer :: Static2_16 
integer :: Static2_17 
integer :: Static2_18 
integer :: Static2_19 
integer :: Edit2_1 
integer :: Edit2_2 
integer :: Edit2_3 
integer :: Edit2_3a 
integer :: Edit2_4 
integer :: Edit2_5
integer :: Edit2_6 
integer :: Edit2_7 
integer :: Edit2_8 
integer :: Edit2_9 
integer :: Edit2_11 
integer :: Edit2_12 
integer :: Edit2_13 
integer :: Edit2_13a 
integer :: Edit2_14 
integer :: Edit2_15
integer :: Edit2_16 
integer :: Edit2_17 
integer :: Edit2_18 
integer :: Edit2_19 

integer :: Toggle2_11(2)
integer :: Toggle2_6(2)
integer :: Toggle2_16(2)
integer :: Button2(2)
real :: tedit2_2=1., tedit2_12=1.
real :: tedit2_3a=50.0, tedit2_13a=50.
real :: tedit2_3=10, tedit2_13=10
real :: tedit2_4=1.0, tedit2_14=1.0
real :: tedit2_5=50., tedit2_15=50.
real :: tedit2_7=1, tedit2_17=1
real :: tedit2_8=1, tedit2_18=1

! Set up complex dialogue box Form2 child of Form1
   iform2 = gmCreateComplexDialogueBox(iform1,15, 9, 24, 15, GALL, 'Calibration and filter settings', &
              	gmVpos=GTOP, gmIconFormat=GDLLICON,gmIconFile='MBIG1033')


!
! Create main panel for form2 - CALIBRATION 
!
   iform2MainPanel=gmCreatePanel(iform2, 0, 0, 24, 13, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

! Create panel Panel2 child of Form2MainPanel
   Panel2 = gmCreatePanel(iform2MainPanel, 4, 5, 24, 15, &
              	gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=12, gmFillBorder=0, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GOFF)

! Create text entry Static2_00 child of Panel2
   Static2_00 = gmCreateTextEntry(Panel2, 4, 1, 4, 1,'ADC0 ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=2, gmBack2Col=0, gmTextCol=4, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static2_2 child of Panel2
   Static2_2 = gmCreateTextEntry(Panel2, 1, 3, 6, 1,'Calibration [V/pA] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)
   
! Create text entry Edit2_2 child of Panel2
   Edit2_2 = gmCreateValueEntry(Panel2, 7, 3, 4, 1,tedit2_2, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=202)

! Create text entry Static2_2 child of Panel2
   Static2_3a = gmCreateTextEntry(Panel2, 1, 4, 6, 1,'Clamp setting [mV/pA] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)
   
! Create text entry Edit2_2 child of Panel2
!   Edit2_3a = gmCreateValueEntry(Panel2, 7, 4, 4, 1,tedit2_3a, 32768,3, GEDIT, &
!              	gmType=GSTANDARD, gmJustify=GLEFT,gmhpos=gleft, &
!              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
!              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=2031)
Edit2_3a = gmCreateValueEntry(Panel2, 7, 4, 4, 1,tedit2_3a, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=2031)

! Create text entry Static2_3 child of Panel2
   Static2_3 = gmCreateTextEntry(Panel2, 1, 5, 4, 1,'Amplifier gain ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_3 child of Panel2
   Edit2_3 = gmCreateValueEntry(Panel2, 5, 5, 6, 1,tedit2_3, 32768, 3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=203)

! Create text entry Static2_4 child of Panel2
   Static2_4 = gmCreateTextEntry(Panel2, 1, 6, 4, 1,'Error factor ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_4 child of Panel2
   Edit2_4 = gmCreateValueEntry(Panel2, 5, 6, 6, 1,tedit2_4, 32768, 3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=204)

! Create text entry Static2_5 child of Panel2
   Static2_5 = gmCreateTextEntry(Panel2, 1, 7, 6, 1,'Filter setting (-3dB) [Hz] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_6 child of Panel2
   Edit2_5 = gmCreateVAlueEntry(Panel2, 7, 7, 4, 1,tedit2_5, 32768, 3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=205)

! Create text entry Static2_6 child of Panel2
   Static2_6 = gmCreateTextEntry(Panel2, 1, 8, 5, 1,'Sample from tape ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle button Toggle2_6 child of Panel2
   Toggle2_6(1) = gmCreateToggleButton(Panel2, 1, 9, 2, 1, 'Yes', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=261)

! Create toggle button Toggle2_6 child of Panel2
   Toggle2_6(2) = gmCreateToggleButton(Panel2, 4, 9, 2, 1, 'No', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=262)

! Create text entry Static2_7 child of Panel2
   Static2_7 = gmCreateTextEntry(Panel2, 1, 10, 4, 1,'Tape recorder ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_7 child of Panel2
   Edit2_7 = gmCreateValueEntry(Panel2, 5, 10, 6, 1,tedit2_7, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=207)

! Create text entry Static2_8 child of Panel2
   Static2_8 = gmCreateTextEntry(Panel2, 1, 11, 5, 1,'Tape speed factor ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_8 child of Panel2
   Edit2_8 = gmCreateValueEntry(Panel2, 6, 11, 5, 1,tedit2_8, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=208)
!
!	ADC1
!

! Create text entry Static2_01 child of Panel2
   Static2_01 = gmCreateTextEntry(Panel2, 15, 1, 4, 1,'ADC1 ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=2, gmBack2Col=0, gmTextCol=4, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static2_12 child of Panel2
   Static2_12 = gmCreateTextEntry(Panel2, 13, 3, 6, 1,'Calibration [V/pA] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_12 child of Panel2
   Edit2_12 = gmCreateValueEntry(Panel2, 19, 3, 4, 1,tedit2_12, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=212)

! Create text entry Static2_12 child of Panel2
   Static2_13a = gmCreateTextEntry(Panel2, 13, 4, 6, 1,'Clamp setting [mV/pA] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_12 child of Panel2
   Edit2_13a = gmCreateValueEntry(Panel2, 19, 4, 4, 1,tedit2_13a, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=2131)

! Create text entry Static2_13 child of Panel2
   Static2_13= gmCreateTextEntry(Panel2, 13, 5, 4, 1,'Amplifier gain ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_13 child of Panel3
   Edit2_13 = gmCreateValueEntry(Panel2, 17, 5, 6, 1,tedit2_13, 32768, 3,GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=213)

! Create text entry Static2_14 child of Panel2
   Static2_14 = gmCreateTextEntry(Panel2, 13, 6, 4, 1,'Error factor ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_14 child of Panel2
   Edit2_14 = gmCreateValueEntry(Panel2, 17, 6, 6, 1,tedit2_14, 32768, 3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=214)

! Create text entry Static2_15 child of Panel2
   Static2_15 = gmCreateTextEntry(Panel2, 13, 7, 6, 1,'Filter setting (-3dB) [Hz] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_15 child of Panel2
   Edit2_15 = gmCreateValueEntry(Panel2, 19, 7, 4, 1,tedit2_15, 32768, 3,GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=215)

! Create text entry Static2_16 child of Panel2
   Static2_16 = gmCreateTextEntry(Panel2, 13, 8, 5, 1,'Sample from tape ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle button Toggle2_16 child of Panel2
   Toggle2_16(1) = gmCreateToggleButton(Panel2, 13, 9, 2, 1, 'Yes', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=261) 
				

! Create toggle button Toggle2_16 child of Panel2
   Toggle2_16(2) = gmCreateToggleButton(Panel2, 16, 9, 2, 1, 'No', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=262)

! Create text entry Static2_17 child of Panel2
   Static2_17 = gmCreateTextEntry(Panel2, 13, 10, 4, 1,'Tape recorder ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_17 child of Panel2
   Edit2_17 = gmCreateValueEntry(Panel2, 17, 10, 6, 1,tedit2_17, 32768, 3,GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=217)

! Create text entry Static2_18 child of Panel2
   Static2_18 = gmCreateTextEntry(Panel2, 13, 11, 5, 1,'Tape speed factor ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_18 child of Panel2
   Edit2_18 = gmCreateValueEntry(Panel2, 18, 11, 5, 1,tedit2_18, 32768, 3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=218)

! Create button Button2_1 child of Panel2
   Button2(1) = gmCreatePushButton(Panel2, 17, 13, 3, 1, 'Reset', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=21)

! Create button Button6 child of Panel2
   Button2(2) = gmCreatePushButton(Panel2, 20, 13, 3, 1, 'Continue', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=22)

   call gmSetWidgetStatus(Edit2_7,GUNSELECTABLE)
   call gmSetWidgetStatus(Edit2_8,GUNSELECTABLE)
   call gmSetWidgetStatus(Edit2_17,GUNSELECTABLE)
   call gmSetWidgetStatus(Edit2_18,GUNSELECTABLE)
  
   if(iall.eq.100) then
      call gmSetWidgetStatus(Edit2_3,GUNSELECTABLE)
	  call gmSetWidgetStatus(Edit2_3a,GUNSELECTABLE)
	  call gmSetWidgetStatus(Edit2_4,GUNSELECTABLE)
	 
	  call gmSetWidgetStatus(Toggle2_6(1),GUNSELECTABLE)
	  call gmSetWidgetStatus(Toggle2_6(2),GUNSELECTABLE)
	  call gmSetWidgetStatus(Edit2_13,GUNSELECTABLE)
	  call gmSetWidgetStatus(Edit2_13a,GUNSELECTABLE)
	  call gmSetWidgetStatus(Edit2_14,GUNSELECTABLE)
	  
	  call gmSetWidgetStatus(Toggle2_16(1),GUNSELECTABLE)
	  call gmSetWidgetStatus(Toggle2_16(2),GUNSELECTABLE)
	  if(nchan.eq.1) then
	     call gmSetWidgetStatus(Edit2_12,GUNSELECTABLE)
		 call gmSetWidgetStatus(Edit2_15,GUNSELECTABLE)
	  endif
   else
      call gmSetWidgetStatus(Edit2_2,GUNSELECTABLE)
	  call gmSetWidgetStatus(Edit2_12,GUNSELECTABLE)
	  if(nchan.eq.1) then
		call gmSetWidgetStatus(Edit2_13,GUNSELECTABLE)
		call gmSetWidgetStatus(Edit2_13a,GUNSELECTABLE)
		call gmSetWidgetStatus(Edit2_14,GUNSELECTABLE)
		call gmSetWidgetStatus(Edit2_15,GUNSELECTABLE)
		call gmSetWidgetStatus(Toggle2_16(1),GUNSELECTABLE)
		call gmSetWidgetStatus(Toggle2_16(2),GUNSELECTABLE)
      endif
   endif
   
   return
   end
   
   
