subroutine ini_page(main)
use menu_f90

type (GLIMIT) :: Graphics_frame
type (gpoint) :: pts(4)
type(GARRAYCELL) arrayattribs
! Define user defined colour indices used
   call gDefinergb(220, 0.992157, 1.000000, 0.749020)
   call gDefinergb(221, 0.819608, 1.000000, 0.800000)
   call gDefinergb(222, 0.792157, 0.874510, 1.000000)
   call gDefinergb(223, 0.874510, 1.000000, 0.866667)
   call gDefinergb(224, 1.000000, 0.501961, 0.501961)
   call gDefinergb(225, 0.501961, 1.000000, 0.501961)
   call gDefinergb(226, 0.909804, 1.000000, 0.909804)
   call gDefinergb(227, 1.000000, 1.000000, 0.713726)
   call gDefinergb(228, 0.674510, 0.992157, 0.956863)
   call gDefinergb(229, 0.576471, 0.992157, 0.839216)
   call gDefinergb(230, 0.501961, 1.000000, 0.619608)
   call gDefinergb(231, 0.549020, 1.000000, 0.658824)
   call gDefinergb(232, 0.713726, 1.000000, 0.784314)
   call gDefinergb(233, 0.686275, 1.000000, 1.000000)
   call gDefinergb(234, 0.694118, 0.901961, 0.992157)
   call gDefinergb(235, 0.866667, 1.000000, 0.866667)
   call gDefinergb(236, 0.839216, 0.992157, 0.858824)
   call gDefinergb(237, 1.000000, 0.274510, 0.274510)
   call gDefinergb(238, 1.000000, 0.658824, 0.658824)
   call gDefinergb(239, 1.000000, 0.556863, 0.121569)
   call gDefinergb(240, 0.949020, 0.792157, 0.549020)
   call gDefinergb(241, 0.176471, 0.584314, 1.000000)
   call gDefinergb(242, 0.000000, 0.501961, 0.749020)
   call gDefinergb(243, 0.000000, 0.576471, 0.847059)
   call gDefinergb(244, 0.000000, 0.764706, 0.764706)
   call gDefinergb(245, 1.000000, 1.000000, 0.501961)
   call gDefinergb(246, 1.000000, 1.000000, 0.392157)
   call gDefinergb(247, 0.921569, 0.937255, 0.937255)
   call gDefinergb(248, 1.000000, 0.909804, 0.866667)
call gmSetGuiGridMode(GOFF)
! Set up master window ini
   ini = gmCreateComplexDialogueBox(main,128, 128, 907, 754, GALL, 'Initial settings', &
   gmVpos=GTOP, gmInitState=GNORMAL, gmIconFormat=GDLLICON, &
      gmIconFile='Gee')

! Create main panel for form
   ini_MainPanel=gmCreatePanel(ini, 0, 0, 907, 754, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)

! Create panel ini_Panel1 child of ini_MainPanel
   ini_Panel1 = gmCreatePanel(ini_MainPanel, 0, 0, 420, 750, gmTitle='Mechanism', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create panel ini_Panel5 child of ini_Panel1
   ini_Panel5 = gmCreatePanel(ini_Panel1, 10, 40, 400, 290, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmfillcol=227, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create graphics frame ini_Graphics2 child of ini_Panel5

! Define Graphics Frame limits
call gDefineRGB(0,0.,0.,1.)		!blue
   Graphics_frame%xmin = 0.000000
   Graphics_frame%xmax = 310.000000
   Graphics_frame%ymin = 0.000000
   Graphics_frame%ymax = 270.000000
   ini_Graphics2 = gmCreateGraphicsFrame(ini_Panel5, 10, 10, 310, 270, Graphics_frame, gmFrameType=GGDI, gmVpos=GTOP)
   call gmClearGraphicsFrame(ini_Graphics2)

! Create radio box child ini_Radio1 child of ini_Panel5
   ini_Radio1 = gmCreateRadioBox(ini_Panel5, 330, 210, 60, 70, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmfillcol=244, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

! Create toggle button ini_Toggle6 child of ini_Radio1
   ini_Toggle6 = gmCreateToggleButton(ini_Radio1, 10, 10, 40, 25, 'OK', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button ini_Toggle7 child of ini_Radio1
   ini_Toggle7 = gmCreateToggleButton(ini_Radio1, 10, 40, 40, 25, 'Edit', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create panel ini_Panel7 child of ini_Panel1
   ini_Panel7 = gmCreatePanel(ini_Panel1, 10, 480, 400, 160, gmTitle='Constrained rates :', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmfillcol=227, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text array ini_TxtArray2 child of ini_Panel7
   ini_TxtArray2 = gmCreateTextArray(ini_Panel7, 10, 40, 310, 110, 10, 10, gmAxisW=50, gmAxisH=25, gmXtext='*digits', &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmVpos=GTOP)
   call gmEnqCellAttribs(ini_TxtArray2, 1, 1, arrayattribs)
   arrayattribs%width=44
   call gmSetCellAttribs(ini_TxtArray2, 1, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray2, 2, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray2, 3, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray2, 4, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray2, 5, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray2, 6, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray2, 7, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray2, 8, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray2, 9, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray2, 10, 1, arrayattribs)
   call gmEnqCellAttribs(ini_TxtArray2, 1, 1, arrayattribs)
   arrayattribs%height=22
   call gmSetCellAttribs(ini_TxtArray2, 1, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray2, 1, 2, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray2, 1, 3, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray2, 1, 4, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray2, 1, 5, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray2, 1, 6, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray2, 1, 7, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray2, 1, 8, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray2, 1, 9, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray2, 1, 10, arrayattribs)

! Create value entry ini_Value3 child of ini_Panel7
   ini_Value3 = gmCreateValueEntry(ini_Panel7, 180, 0, 50, 25, 0.000000, 8, 0, GDISPLAY, gmType=GDECIMAL, gmBack1Col=0, &
      gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create radio box child ini_Radio3 child of ini_Panel7
   ini_Radio3 = gmCreateRadioBox(ini_Panel7, 320, 210, 60, 70, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

! Create toggle button ini_Toggle10 child of ini_Radio3
   ini_Toggle10 = gmCreateToggleButton(ini_Radio3, 10, 10, 40, 25, 'OK', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button ini_Toggle11 child of ini_Radio3
   ini_Toggle11 = gmCreateToggleButton(ini_Radio3, 10, 40, 40, 25, 'Edit', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create radio box child ini_Radio6 child of ini_Panel7
   ini_Radio6 = gmCreateRadioBox(ini_Panel7, 330, 80, 60, 70, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmfillcol=244, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

! Create toggle button ini_Toggle16 child of ini_Radio6
   ini_Toggle16 = gmCreateToggleButton(ini_Radio6, 10, 10, 40, 25, 'OK', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button ini_Toggle17 child of ini_Radio6
   ini_Toggle17 = gmCreateToggleButton(ini_Radio6, 10, 40, 40, 25, 'Edit', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create panel ini_Panel9 child of ini_Panel1
   ini_Panel9 = gmCreatePanel(ini_Panel1, 10, 640, 400, 100, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmfillcol=227, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget ini_Text1 child of ini_Panel9
   ini_Text1 = gmCreateTextEntry(ini_Panel9, 10, 10, 80, 25,'Fixed rates : ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create value entry ini_Value4 child of ini_Panel9
   ini_Value4 = gmCreateValueEntry(ini_Panel9, 10, 40, 20, 25, 0.000000, 8, 0, GDISPLAY, gmType=GDECIMAL, gmBack1Col=0, &
      gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create value entry ini_Value5 child of ini_Panel9
   ini_Value5 = gmCreateValueEntry(ini_Panel9, 170, 40, 20, 25, 0.000000, 8, 0, GDISPLAY, gmType=GDECIMAL, gmBack1Col=0, &
      gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create radio box child ini_Radio5 child of ini_Panel9
   ini_Radio5 = gmCreateRadioBox(ini_Panel9, 100, 10, 60, 70, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmfillcol=244, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

! Create toggle button ini_Toggle14 child of ini_Radio5
   ini_Toggle14 = gmCreateToggleButton(ini_Radio5, 10, 10, 40, 25, 'OK', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button ini_Toggle15 child of ini_Radio5
   ini_Toggle15 = gmCreateToggleButton(ini_Radio5, 10, 40, 40, 25, 'Edit', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create radio box child ini_Radio7 child of ini_Panel9
   ini_Radio7 = gmCreateRadioBox(ini_Panel9, 330, 10, 60, 70, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmfillcol=244, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

! Create toggle button ini_Toggle18 child of ini_Radio7
   ini_Toggle18 = gmCreateToggleButton(ini_Radio7, 10, 10, 40, 25, 'OK', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button ini_Toggle19 child of ini_Radio7
   ini_Toggle19 = gmCreateToggleButton(ini_Radio7, 10, 40, 40, 25, 'Edit', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create text widget ini_Text12 child of ini_Panel9
   ini_Text12 = gmCreateTextEntry(ini_Panel9, 170, 10, 140, 25,'Constrained by EC50 : ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create panel ini_Panel6 child of ini_Panel1
   ini_Panel6 = gmCreatePanel(ini_Panel1, 10, 330, 400, 150, gmTitle='Microscopic reversibility :', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmfillcol=227, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text array ini_TxtArray1 child of ini_Panel6
   ini_TxtArray1 = gmCreateTextArray(ini_Panel6, 10, 30, 310, 110, 10, 10, gmAxisW=50, gmAxisH=25, gmXtext='*digits', &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmVpos=GTOP)
   call gmEnqCellAttribs(ini_TxtArray1, 1, 1, arrayattribs)
   arrayattribs%width=44
   call gmSetCellAttribs(ini_TxtArray1, 1, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1, 2, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1, 3, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1, 4, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1, 5, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1, 6, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1, 7, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1, 8, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1, 9, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1, 10, 1, arrayattribs)
   call gmEnqCellAttribs(ini_TxtArray1, 1, 1, arrayattribs)
   arrayattribs%height=22
   call gmSetCellAttribs(ini_TxtArray1, 1, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1, 1, 2, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1, 1, 3, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1, 1, 4, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1, 1, 5, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1, 1, 6, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1, 1, 7, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1, 1, 8, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1, 1, 9, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1, 1, 10, arrayattribs)

! Create value entry ini_Value2 child of ini_Panel6
   ini_Value2 = gmCreateValueEntry(ini_Panel6, 190, 0, 50, 25, 0.000000, 8, 0, GDISPLAY, gmType=GDECIMAL, gmBack1Col=0, &
      gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create radio box child ini_Radio2 child of ini_Panel6
   ini_Radio2 = gmCreateRadioBox(ini_Panel6, 320, 210, 60, 70, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

! Create toggle button ini_Toggle8 child of ini_Radio2
   ini_Toggle8 = gmCreateToggleButton(ini_Radio2, 10, 10, 40, 25, 'OK', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button ini_Toggle9 child of ini_Radio2
   ini_Toggle9 = gmCreateToggleButton(ini_Radio2, 10, 40, 40, 25, 'Edit', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create radio box child ini_Radio4 child of ini_Panel6
   ini_Radio4 = gmCreateRadioBox(ini_Panel6, 330, 70, 60, 70, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmfillcol=244, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

! Create toggle button ini_Toggle12 child of ini_Radio4
   ini_Toggle12 = gmCreateToggleButton(ini_Radio4, 10, 10, 40, 25, 'OK', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button ini_Toggle13 child of ini_Radio4
   ini_Toggle13 = gmCreateToggleButton(ini_Radio4, 10, 40, 40, 25, 'Edit', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create panel ini_Panel2 child of ini_MainPanel
   ini_Panel2 = gmCreatePanel(ini_MainPanel, 420, 0, 260, 750, gmTitle='Data', gmType=GPROJECTED, gmLineBorder=GOUTEREDGE, &
      gmfillcol=229, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text array ini_TxtArray3 child of ini_Panel2
   ini_TxtArray3 = gmCreateTextArray(ini_Panel2, 10, 70, 170, 120, 10, 10, gmAxisW=50, gmAxisH=25, gmXtext='*digits', &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmVpos=GTOP)
   call gmEnqCellAttribs(ini_TxtArray3, 1, 1, arrayattribs)
   arrayattribs%width=44
   call gmSetCellAttribs(ini_TxtArray3, 1, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray3, 2, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray3, 3, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray3, 4, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray3, 5, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray3, 6, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray3, 7, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray3, 8, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray3, 9, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray3, 10, 1, arrayattribs)
   call gmEnqCellAttribs(ini_TxtArray3, 1, 1, arrayattribs)
   arrayattribs%height=22
   call gmSetCellAttribs(ini_TxtArray3, 1, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray3, 1, 2, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray3, 1, 3, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray3, 1, 4, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray3, 1, 5, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray3, 1, 6, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray3, 1, 7, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray3, 1, 8, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray3, 1, 9, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray3, 1, 10, arrayattribs)

! Create panel ini_Panel10 child of ini_Panel2
   ini_Panel10 = gmCreatePanel(ini_Panel2, 10, 200, 240, 540, gmType=GPRESSED, gmLineBorder=GOUTEREDGE, gmfillcol=232, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create value entry ini_Value1 child of ini_Panel10
   ini_Value1 = gmCreateValueEntry(ini_Panel10, 150, 30, 50, 25, 0.000000, 8, 3, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
   call gmSetWidgetStatus(ini_Value1, GUNSELECTABLE)

! Create text widget ini_Text3 child of ini_Panel10
   ini_Text3 = gmCreateTextEntry(ini_Panel10, 10, 30, 130, 25,'Concentration (muM) ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text4 child of ini_Panel10
   ini_Text4 = gmCreateTextEntry(ini_Panel10, 10, 60, 130, 25,'One channel only ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text5 child of ini_Panel10
   ini_Text5 = gmCreateTextEntry(ini_Panel10, 10, 90, 130, 25,'Critical shut time (mus) ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text6 child of ini_Panel10
   ini_Text6 = gmCreateTextEntry(ini_Panel10, 10, 120, 130, 25,'Use CHS vector ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text7 child of ini_Panel10
   ini_Text7 = gmCreateTextEntry(ini_Panel10, 10, 150, 160, 25,'Bad shutting ends group ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text8 child of ini_Panel10
   ini_Text8 = gmCreateTextEntry(ini_Panel10, 10, 180, 130, 25,'Set intervals "bad" ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text9 child of ini_Panel10
   ini_Text9 = gmCreateTextEntry(ini_Panel10, 10, 10, 50, 25,'SET 1 ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create toggle button ini_Toggle1 child of ini_Panel10
   ini_Toggle1 = gmCreateToggleButton(ini_Panel10, 150, 60, 40, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create value entry ini_Value6 child of ini_Panel10
   ini_Value6 = gmCreateValueEntry(ini_Panel10, 150, 90, 60, 25, 0.000000, 8, 3, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create toggle button ini_Toggle2 child of ini_Panel10
   ini_Toggle2 = gmCreateToggleButton(ini_Panel10, 150, 120, 40, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button ini_Toggle3 child of ini_Panel10
   ini_Toggle3 = gmCreateToggleButton(ini_Panel10, 180, 150, 40, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button ini_Toggle4 child of ini_Panel10
   ini_Toggle4 = gmCreateToggleButton(ini_Panel10, 10, 200, 110, 25, 'shut times (ms)', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button ini_Toggle5 child of ini_Panel10
   ini_Toggle5 = gmCreateToggleButton(ini_Panel10, 10, 220, 120, 25, 'open times (ms)', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create value entry ini_Value7 child of ini_Panel10
   ini_Value7 = gmCreateValueEntry(ini_Panel10, 140, 200, 80, 25, 0.000000, 8, 3, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create value entry ini_Value8 child of ini_Panel10
   ini_Value8 = gmCreateValueEntry(ini_Panel10, 140, 220, 80, 25, 0.000000, 8, 3, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create button ini_Button15 child of ini_Panel10
   ini_Button15 = gmCreatePushButton(ini_Panel10, 80, 510, 75, 25, 'Previous', gmVpos=GTOP)

! Create button ini_Button16 child of ini_Panel10
   ini_Button16 = gmCreatePushButton(ini_Panel10, 160, 510, 75, 25, 'Next', gmVpos=GTOP)

! Create text widget ini_Text10 child of ini_Panel2
   ini_Text10 = gmCreateTextEntry(ini_Panel2, 20, 20, 100, 25,'Number of sets : ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text11 child of ini_Panel2
   ini_Text11 = gmCreateTextEntry(ini_Panel2, 20, 40, 100, 25,'File names: ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create radio box child ini_Radio8 child of ini_Panel2
   ini_Radio8 = gmCreateRadioBox(ini_Panel2, 190, 120, 60, 70, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmfillcol=244, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

! Create toggle button ini_Toggle20 child of ini_Radio8
   ini_Toggle20 = gmCreateToggleButton(ini_Radio8, 10, 10, 40, 25, 'OK', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button ini_Toggle21 child of ini_Radio8
   ini_Toggle21 = gmCreateToggleButton(ini_Radio8, 10, 40, 40, 25, 'Edit', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create value entry ini_Value9 child of ini_Panel2
   ini_Value9 = gmCreateValueEntry(ini_Panel2, 140, 20, 100, 25, 0.000000, 8, 0, GDISPLAY, gmType=GDECIMAL, gmBack1Col=0, &
      gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create panel ini_Panel3 child of ini_MainPanel
   ini_Panel3 = gmCreatePanel(ini_MainPanel, 680, 0, 220, 640, gmTitle='Initial Guesses', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmfillcol=234, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text array ini_TxtArray4 child of ini_Panel3
   ini_TxtArray4 = gmCreateTextArray(ini_Panel3, 10, 90, 200, 530, 10, 10, gmAxisW=50, gmAxisH=25, gmXtext='*digits', &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmVpos=GTOP)
   call gmEnqCellAttribs(ini_TxtArray4, 1, 1, arrayattribs)
   arrayattribs%width=44
   call gmSetCellAttribs(ini_TxtArray4, 1, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray4, 2, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray4, 3, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray4, 4, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray4, 5, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray4, 6, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray4, 7, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray4, 8, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray4, 9, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray4, 10, 1, arrayattribs)
   call gmEnqCellAttribs(ini_TxtArray4, 1, 1, arrayattribs)
   arrayattribs%height=22
   call gmSetCellAttribs(ini_TxtArray4, 1, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray4, 1, 2, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray4, 1, 3, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray4, 1, 4, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray4, 1, 5, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray4, 1, 6, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray4, 1, 7, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray4, 1, 8, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray4, 1, 9, arrayattribs)
   arrayattribs%height=106
   call gmSetCellAttribs(ini_TxtArray4, 1, 10, arrayattribs)

! Create radio box child ini_Radio9 child of ini_Panel3
   ini_Radio9 = gmCreateRadioBox(ini_Panel3, 150, 10, 60, 70, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmfillcol=244, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

! Create toggle button ini_Toggle22 child of ini_Radio9
   ini_Toggle22 = gmCreateToggleButton(ini_Radio9, 10, 10, 40, 25, 'OK', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button ini_Toggle23 child of ini_Radio9
   ini_Toggle23 = gmCreateToggleButton(ini_Radio9, 10, 40, 40, 25, 'Edit', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create panel ini_Panel4 child of ini_MainPanel
   ini_Panel4 = gmCreatePanel(ini_MainPanel, 680, 640, 220, 110, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmfillcol=248, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create button ini_Button13 child of ini_Panel4
   ini_Button13 = gmCreatePushButton(ini_Panel4, 20, 50, 75, 40, 'Fit now', gmType=GUSERDEFINED, gmoffcol=237, gmTextCol=0, &
      gmVpos=GTOP)

! Create button ini_Button14 child of ini_Panel4
   ini_Button14 = gmCreatePushButton(ini_Panel4, 110, 50, 75, 40, 'Display only', gmType=GUSERDEFINED, gmoffcol=238, &
      gmTextCol=0, gmVpos=GTOP)

! Create text widget ini_Text13 child of ini_Panel4
   ini_Text13 = gmCreateTextEntry(ini_Panel4, 10, 10, 200, 25,'All checked: proceed now ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=2 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Text13, gmBold=1)
    call gmdrawwindow(ini)
	call gmSetGuiGridMode(GOFF)
	call gDefineRGB(0,1.,1.,0.94)
return
end
