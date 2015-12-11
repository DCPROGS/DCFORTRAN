subroutine ini_page(main)
use menu_f90
type (GLIMIT) :: Graphics_frame
type(GARRAYCELL) arrayattribs
call gmSetGuiGridMode(GOFF)
! Set up master window ini
   ini =gmCreateComplexDialogueBox(Main, 100, 100,978, 790, GALL,&
    'Settings in HJCFIT.INI',gmvpos=gtop, gmhpos=gleft,&
    gmIconFormat=GDLLICON,gmIconFile='MBIG1085')
	

! Create main panel for form
   ini_MainPanel=gmCreatePanel(ini, 0, 0, 978, 790, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillCol=10, gmFillBorder=GOUTEREDGE)

! Create panel ini_Panel26 child of ini_MainPanel
   ini_Panel26 = gmCreatePanel(ini_MainPanel, 0, 0, 978, 790, gmTitle='Panel 26', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create panel ini_Panel3 child of ini_Panel26
   ini_Panel3 = gmCreatePanel(ini_Panel26, 705, 0, 220, 485, gmTitle='Initial guesses for rates', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=120, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create panel ini_Panel10 child of ini_Panel3
   ini_Panel10 = gmCreatePanel(ini_Panel3, 0, 485, 175, 125, gmTitle='Panel 10', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text array ini_TxtArray2 child of ini_Panel3
   ini_TxtArray2 = gmCreateTextArray(ini_Panel3, 27, 55, 165, 415, 2, 20, gmAxisW=25, gmAxisH=25, gmXtext='*digits', &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=GVERTICAL, gmHpos=GCENTRE, gmVpos=GTOP)
   call gmEnqCellAttribs(ini_TxtArray2, 1, 1, arrayattribs)
   arrayattribs%width=44
   arrayattribs%height=22
   call gmSetCellAttribs(ini_TxtArray2, 0, 0, arrayattribs)
   call gmSetFontAttribs(ini_TxtArray2, gmPointSize=8)

! Create text widget ini_Text31 child of ini_Panel3
   ini_Text31 = gmCreateTextEntry(ini_Panel3, 15, 25, 100, 25,'File 73: good guesses ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create button ini_Button30 child of ini_Panel3
   ini_Button30 = gmCreatePushButton(ini_Panel3, 175, 5, 30, 20, 'OK', gmType=GUSERDEFINED, gmOffCol=127, gmTextCol=0, &
      gmVpos=GTOP)

! Create button ini_Button31 child of ini_Panel3
   ini_Button31 = gmCreatePushButton(ini_Panel3, 175, 30, 30, 20, 'Edit', gmType=GUSERDEFINED, gmOnCol=4, gmOffCol=4, &
      gmTextCol=0, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Button31, gmPointSize=8)

! Create panel ini_Panel2 child of ini_Panel26
   ini_Panel2 = gmCreatePanel(ini_Panel26, 465, 0, 245, 540, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmFillCol=126, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create button ini_Button3 child of ini_Panel2
   ini_Button3 = gmCreatePushButton(ini_Panel2, 185, 35, 30, 20, 'Edit', gmType=GUSERDEFINED, gmOnCol=4, gmOffCol=4, &
      gmTextCol=0, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Button3, gmPointSize=8)

! Create value entry ini_Value5 child of ini_Panel2
   ini_Value5 = gmCreateValueEntry(ini_Panel2, 110, 5, 20, 25, 2.00000, 8, 0, GDISPLAY, gmType=GDECIMAL, gmJustify=GCENTRE, &
      gmBack1Col=0, gmBack2Col=0, gmTextCol=7, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Value5, gmPointSize=8, gmBold=GON)

! Create text widget ini_Text14 child of ini_Panel2
   ini_Text14 = gmCreateTextEntry(ini_Panel2, 10, 10, 95, 20,'Number of data sets ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create panel ini_Panel15 child of ini_Panel2
   ini_Panel15 = gmCreatePanel(ini_Panel2, 12, 210, 200, 310, gmTitle='SET 1', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=125, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmHpos=GCENTRE, gmVpos=GTOP)

! Create text widget ini_Text15 child of ini_Panel15
   ini_Text15 = gmCreateTextEntry(ini_Panel15, 10, 120, 100, 25,'Use CHS vectors ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Text15, gmPointSize=8)

! Create text widget ini_Text16 child of ini_Panel15
   ini_Text16 = gmCreateTextEntry(ini_Panel15, 10, 90, 105, 25,'Critical shut time (mus) ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Text16, gmPointSize=8)

! Create text widget ini_Text17 child of ini_Panel15
   ini_Text17 = gmCreateTextEntry(ini_Panel15, 10, 150, 120, 25,'Bad shutting ends group ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Text17, gmPointSize=8)

! Create text widget ini_Text18 child of ini_Panel15
   ini_Text18 = gmCreateTextEntry(ini_Panel15, 10, 185, 150, 25,'Set intervals ''bad'' if longer than ', 255, GDISPLAY, &
      gmBack1Col=0, gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text19 child of ini_Panel15
   ini_Text19 = gmCreateTextEntry(ini_Panel15, 5, 50, 95, 25,'One channel only ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text20 child of ini_Panel15
   ini_Text20 = gmCreateTextEntry(ini_Panel15, 30, 240, 80, 20,'open times (ms) ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create button ini_Button15 child of ini_Panel15
   ini_Button15 = gmCreatePushButton(ini_Panel15, 115, 275, 65, 20, 'Next set', gmType=GUSERDEFINED, gmOffCol=4, &
      gmTextCol=0, gmVpos=GTOP)

! Create panel ini_Panel19 child of ini_Panel15
   ini_Panel19 = gmCreatePanel(ini_Panel15, 130, 80, 60, 100, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmFillCol=150, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create value entry ini_Value9 child of ini_Panel19
   ini_Value9 = gmCreateValueEntry(ini_Panel19, 10, 10, 40, 25, 40.0000, 8, 1, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create toggle button ini_Toggle6 child of ini_Panel19
   ini_Toggle6 = gmCreateToggleButton(ini_Panel19, 20, 40, 20, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button ini_Toggle7 child of ini_Panel19
   ini_Toggle7 = gmCreateToggleButton(ini_Panel19, 20, 70, 30, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button ini_Toggle8 child of ini_Panel15
   ini_Toggle8 = gmCreateToggleButton(ini_Panel15, 130, 50, 20, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button ini_Toggle9 child of ini_Panel15
   ini_Toggle9 = gmCreateToggleButton(ini_Panel15, 120, 210, 20, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button ini_Toggle10 child of ini_Panel15
   ini_Toggle10 = gmCreateToggleButton(ini_Panel15, 120, 240, 20, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create value entry ini_Value10 child of ini_Panel15
   ini_Value10 = gmCreateValueEntry(ini_Panel15, 150, 210, 40, 25, 10000.0, 8, 0, GEDIT, gmVmin=0.000000, gmVmax=10000.0, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Value10, gmPointSize=8)

! Create value entry ini_Value11 child of ini_Panel15
   ini_Value11 = gmCreateValueEntry(ini_Panel15, 150, 240, 40, 25, 0.000000, 8, 0, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Value11, gmPointSize=8)

! Create text widget ini_Text21 child of ini_Panel15
   ini_Text21 = gmCreateTextEntry(ini_Panel15, 30, 210, 80, 20,'shut times  (ms) ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text30 child of ini_Panel15
   ini_Text30 = gmCreateTextEntry(ini_Panel15, 15, 25, 135, 25,'Concentration (muM) = 30 ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Text30, gmPointSize=8)

! Create text widget ini_Text22 child of ini_Panel2
   ini_Text22 = gmCreateTextEntry(ini_Panel2, 10, 35, 75, 25,'Data file names ', 255, GDISPLAY, gmBack1Col=125, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create button ini_Button27 child of ini_Panel2
   ini_Button27 = gmCreatePushButton(ini_Panel2, 185, 10, 30, 20, 'OK', gmType=GUSERDEFINED, gmOffCol=127, gmTextCol=0, &
      gmVpos=GTOP)

! Create text array ini_TxtArray3 child of ini_Panel2
   ini_TxtArray3 = gmCreateTextArray(ini_Panel2, 20, 65, 205, 135, 1, 30, gmAxisW=25, gmAxisH=25, gmXtext='*digits', &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=GVERTICAL, gmHpos=GCENTRE, gmVpos=GTOP)
   call gmEnqCellAttribs(ini_TxtArray3, 1, 1, arrayattribs)
   arrayattribs%width=44
   arrayattribs%height=22
   call gmSetCellAttribs(ini_TxtArray3, 0, 0, arrayattribs)
   call gmSetFontAttribs(ini_TxtArray3, gmPointSize=8)

! Create panel ini_Panel1 child of ini_Panel26
   ini_Panel1 = gmCreatePanel(ini_Panel26, 0, 0, 470, 540, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmFillCol=150, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create button ini_Button1 child of ini_Panel1
   ini_Button1 = gmCreatePushButton(ini_Panel1, 230, 50, 30, 20, 'Edit', gmType=GUSERDEFINED, gmOnCol=4, gmOffCol=4, &
      gmTextCol=0, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Button1, gmPointSize=8)

! Create panel ini_Panel4 child of ini_Panel1
   ini_Panel4 = gmCreatePanel(ini_Panel1, 5, 155, 265, 0, gmType=GSINGLE, gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, &
      gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget ini_Text1 child of ini_Panel1
   ini_Text1 = gmCreateTextEntry(ini_Panel1, 5, 160, 170, 20,'Microscopic reversibility constraints ', 255, GDISPLAY, &
      gmBack1Col=0, gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text2 child of ini_Panel1
   ini_Text2 = gmCreateTextEntry(ini_Panel1, 90, 0, 60, 20,'Mechanism ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create button ini_Button7 child of ini_Panel1
   ini_Button7 = gmCreatePushButton(ini_Panel1, 230, 190, 30, 20, 'Edit', gmType=GUSERDEFINED, gmOnCol=4, gmOffCol=4, &
      gmTextCol=0, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Button7, gmPointSize=8)

! Create button ini_Button8 child of ini_Panel1
   ini_Button8 = gmCreatePushButton(ini_Panel1, 230, 300, 30, 20, 'Edit', gmType=GUSERDEFINED, gmOnCol=4, gmOffCol=4, &
      gmTextCol=0, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Button8, gmPointSize=8)

! Create button ini_Button9 child of ini_Panel1
   ini_Button9 = gmCreatePushButton(ini_Panel1, 230, 440, 30, 20, 'Edit', gmType=GUSERDEFINED, gmOnCol=4, gmOffCol=4, &
      gmTextCol=0, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Button9, gmPointSize=8)

! Create button ini_Button10 child of ini_Panel1
   ini_Button10 = gmCreatePushButton(ini_Panel1, 230, 495, 30, 20, 'Edit', gmType=GUSERDEFINED, gmOnCol=4, gmOffCol=4, &
      gmTextCol=0, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Button10, gmPointSize=8)

! Create text widget ini_Text7 child of ini_Panel1
   ini_Text7 = gmCreateTextEntry(ini_Panel1, 0, 275, 90, 10,'Constrained rates ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text8 child of ini_Panel1
   ini_Text8 = gmCreateTextEntry(ini_Panel1, 5, 420, 60, 10,'Fixed rates ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text9 child of ini_Panel1
   ini_Text9 = gmCreateTextEntry(ini_Panel1, 5, 485, 130, 10,'rates constrained by EC50 ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create value entry ini_Value1 child of ini_Panel1
   ini_Value1 = gmCreateValueEntry(ini_Panel1, 175, 160, 20, 25, 1.00000, 8, 0, GDISPLAY, gmType=GDECIMAL, &
      gmJustify=GCENTRE, gmBack1Col=0, gmBack2Col=0, gmTextCol=7, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Value1, gmPointSize=8, gmBold=GON)

! Create table box view ini_TableBox1 child of ini_Panel1
   ini_TableBox1 = gmCreateTableBox(ini_Panel1, 5, 295, 175, 100, gmVpos=GTOP)
   call gmAddTableBoxColumn(ini_TableBox1, 1, 62, gmLabel='rate 1 ')
   call gmAddTableBoxColumn(ini_TableBox1, 2, 49, gmLabel=' =factor')
   call gmAddTableBoxColumn(ini_TableBox1, 3, 64, gmLabel='x rate 2')
   call gmSetFontAttribs(ini_TableBox1, gmPointSize=8)

! Create panel ini_Panel12 child of ini_Panel1
   ini_Panel12 = gmCreatePanel(ini_Panel1, 0, 265, 265, 0, gmType=GSINGLE, gmLineBorder=GOUTEREDGE, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create panel ini_Panel13 child of ini_Panel1
   ini_Panel13 = gmCreatePanel(ini_Panel1, 0, 405, 265, 0, gmType=GSINGLE, gmLineBorder=GOUTEREDGE, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create value entry ini_Value2 child of ini_Panel1
   ini_Value2 = gmCreateValueEntry(ini_Panel1, 85, 270, 20, 25, 6.00000, 8, 0, GDISPLAY, gmType=GDECIMAL, &
      gmJustify=GCENTRE, gmBack1Col=0, gmBack2Col=0, gmTextCol=7, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Value2, gmPointSize=8, gmBold=GON)

! Create value entry ini_Value3 child of ini_Panel1
   ini_Value3 = gmCreateValueEntry(ini_Panel1, 65, 415, 20, 25, 0.000000, 8, 0, GDISPLAY, gmType=GDECIMAL, &
      gmJustify=GCENTRE, gmBack1Col=0, gmBack2Col=0, gmTextCol=7, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Value3, gmPointSize=8, gmBold=GON)

! Create value entry ini_Value4 child of ini_Panel1
   ini_Value4 = gmCreateValueEntry(ini_Panel1, 145, 480, 20, 25, 0.000000, 8, 0, GDISPLAY, gmType=GDECIMAL, &
      gmJustify=GCENTRE, gmBack1Col=0, gmBack2Col=0, gmTextCol=7, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Value4, gmPointSize=8, gmBold=GON)

! Create graphics frame ini_Graphics1 child of ini_Panel1

! Define Graphics Frame limits
   Graphics_frame%xmin = 0.000000
   Graphics_frame%xmax = 1000.000000
   Graphics_frame%ymin = 0.000000
   Graphics_frame%ymax = 750.000000
   ini_Graphics1 = gmCreateGraphicsFrame(ini_Panel1, 10, 20, 210, 130, Graphics_frame, gmFrameType=GGDI, gmVpos=GTOP)
   call gmClearGraphicsFrame(ini_Graphics1)

! Create panel ini_Panel14 child of ini_Panel1
   ini_Panel14 = gmCreatePanel(ini_Panel1, 0, 465, 265, 0, gmType=GSINGLE, gmLineBorder=GOUTEREDGE, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text array ini_TxtArray1 child of ini_Panel1
   ini_TxtArray1 = gmCreateTextArray(ini_Panel1, 5, 180, 200, 75, 2, 10, gmAxisW=25, gmAxisH=25, gmXtext='*letters', &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=GVERTICAL, gmVpos=GTOP)
   call gmEnqCellAttribs(ini_TxtArray1, 1, 1, arrayattribs)
   arrayattribs%width=44
   arrayattribs%height=22
   call gmSetCellAttribs(ini_TxtArray1, 0, 0, arrayattribs)
   call gmSetFontAttribs(ini_TxtArray1, gmPointSize=8)

! Create button ini_Button16 child of ini_Panel1
   ini_Button16 = gmCreatePushButton(ini_Panel1, 230, 20, 30, 25, 'OK', gmType=GUSERDEFINED, gmOffCol=127, gmTextCol=0, &
      gmVpos=GTOP)

! Create button ini_Button20 child of ini_Panel1
   ini_Button20 = gmCreatePushButton(ini_Panel1, 230, 160, 30, 25, 'OK', gmType=GUSERDEFINED, gmOffCol=127, gmTextCol=0, &
      gmVpos=GTOP)

! Create button ini_Button21 child of ini_Panel1
   ini_Button21 = gmCreatePushButton(ini_Panel1, 230, 270, 30, 25, 'OK', gmType=GUSERDEFINED, gmOffCol=127, gmTextCol=0, &
      gmVpos=GTOP)

! Create button ini_Button22 child of ini_Panel1
   ini_Button22 = gmCreatePushButton(ini_Panel1, 230, 405, 30, 25, 'OK', gmType=GUSERDEFINED, gmOffCol=127, gmTextCol=0, &
      gmVpos=GTOP)

! Create button ini_Button23 child of ini_Panel1
   ini_Button23 = gmCreatePushButton(ini_Panel1, 230, 470, 30, 20, 'OK', gmType=GUSERDEFINED, gmOffCol=127, gmTextCol=0, &
      gmVpos=GTOP)

! Create panel ini_Panel11 child of ini_Panel26
   ini_Panel11 = gmCreatePanel(ini_Panel26, 510, 485, 215, 55, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget ini_Text12 child of ini_Panel11
   ini_Text12 = gmCreateTextEntry(ini_Panel11, 45, 5, 95, 10,'All checked: now ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=2 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Text12, gmBold=1)

! Create button ini_Button11 child of ini_Panel11
   ini_Button11 = gmCreatePushButton(ini_Panel11, 40, 20, 55, 30, 'Fit now', gmType=GUSERDEFINED, gmOnCol=2, gmOffCol=2, &
      gmTextCol=0, gmVpos=GTOP)

! Create button ini_Button12 child of ini_Panel11
   ini_Button12 = gmCreatePushButton(ini_Panel11, 125, 20, 55, 30, 'Display (no fit)', gmType=GUSERDEFINED, gmOnCol=28, &
      gmOffCol=28, gmTextCol=0, gmVpos=GTOP)

call gmdrawwindow(ini)
call gmSetGuiGridMode(GOn)
return
end
