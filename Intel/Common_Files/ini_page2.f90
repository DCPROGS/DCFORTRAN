subroutine ini_page(main)
use menu_f90

type (GLIMIT) :: Graphics_frame
type (gpoint) :: pts(4)
type(GARRAYCELL) arrayattribs
common/ini_pag/ ini,ini_panel1,ini_panel1_1
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
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmScrollMode=GnoBARS, gmVpos=GTOP)

! Create panel ini_panel1_1 child of ini_Panel1
   ini_panel1_1 = gmCreatePanel(ini_Panel1, 10, 40, 400, 290, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE,     &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GbothBARS, gmVpos=GTOP)

! Create graphics frame ini_Graphics2 child of ini_panel1_1

! Define Graphics Frame limits
!call gDefineRGB(0,0.,0.,1.)		!blue
!   Graphics_frame%xmin = 0.000000
!   Graphics_frame%xmax = 310.000000
!   Graphics_frame%ymin = 0.000000
!   Graphics_frame%ymax = 270.000000
!   ini_Graphics2 = gmCreateGraphicsFrame(ini_panel1_1, 10, 10, 380, 270, Graphics_frame, gmFrameType=GGDI, gmVpos=GTOP)
!   call gmClearGraphicsFrame(ini_Graphics2)


  
  ini_Button1_1 = gmCreatePushButton(ini_panel1, 350, 10, 60, 25, 'Change', gmType=GUSERDEFINED, gmoffcol=46, &
      gmTextCol=0, gmVpos=GTOP)
! Create panel ini_panel1_2 child of ini_Panel1
   ini_panel1_2 = gmCreatePanel(ini_Panel1, 10, 330, 400, 150, gmTitle='Microscopic reversibility :', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmfillcol=227, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text array ini_TxtArray1_2 child of ini_panel1_2
   ini_TxtArray1_2 = gmCreateTextArray(ini_panel1_2, 10, 30, 310, 110, 10, 10, gmAxisW=50, gmAxisH=25, gmXtext='*digits', &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmVpos=GTOP)
   call gmEnqCellAttribs(ini_TxtArray1_2, 1, 1, arrayattribs)
   arrayattribs%width=44
   call gmSetCellAttribs(ini_TxtArray1_2, 1, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_2, 2, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_2, 3, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_2, 4, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_2, 5, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_2, 6, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_2, 7, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_2, 8, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_2, 9, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_2, 10, 1, arrayattribs)
   call gmEnqCellAttribs(ini_TxtArray1_2, 1, 1, arrayattribs)
   arrayattribs%height=22
   call gmSetCellAttribs(ini_TxtArray1_2, 1, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_2, 1, 2, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_2, 1, 3, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_2, 1, 4, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_2, 1, 5, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_2, 1, 6, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_2, 1, 7, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_2, 1, 8, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_2, 1, 9, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_2, 1, 10, arrayattribs)

! Create value entry ini_Value2 child of ini_panel1_2
 ini_Value1_2 = gmCreateValueEntry(ini_panel1_2, 190, 0, 50, 25, 0.000000, 8, 0, GDISPLAY, gmType=GDECIMAL, gmBack1Col=0, &
      gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

ini_Button1_2 = gmCreatePushButton(ini_panel1_2, 330, 30, 60, 25, 'Change', gmType=GUSERDEFINED, gmoffcol=46, &
      gmTextCol=0, gmVpos=GTOP)


! Create panel ini_panel1_3 child of ini_Panel1
   ini_panel1_3 = gmCreatePanel(ini_Panel1, 10, 480, 400, 160, gmTitle='Constrained rates :', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmfillcol=227, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text array ini_TxtArray1_3 child of ini_panel1_3
   ini_TxtArray1_3 = gmCreateTextArray(ini_panel1_3, 10, 40, 310, 110, 10, 10, gmAxisW=50, gmAxisH=25, gmXtext='*digits', &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmVpos=GTOP)
   call gmEnqCellAttribs(ini_TxtArray1_3, 1, 1, arrayattribs)
   arrayattribs%width=44
   call gmSetCellAttribs(ini_TxtArray1_3, 1, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_3, 2, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_3, 3, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_3, 4, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_3, 5, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_3, 6, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_3, 7, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_3, 8, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_3, 9, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_3, 10, 1, arrayattribs)
   call gmEnqCellAttribs(ini_TxtArray1_3, 1, 1, arrayattribs)
   arrayattribs%height=22
   call gmSetCellAttribs(ini_TxtArray1_3, 1, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_3, 1, 2, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_3, 1, 3, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_3, 1, 4, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_3, 1, 5, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_3, 1, 6, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_3, 1, 7, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_3, 1, 8, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_3, 1, 9, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray1_3, 1, 10, arrayattribs)

! Create value entry ini_Value3 child of ini_panel1_3
   ini_Value1_3 = gmCreateValueEntry(ini_panel1_3, 180, 0, 50, 25, 0.000000, 8, 0, GDISPLAY, gmType=GDECIMAL, gmBack1Col=0, &
      gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
 
 ini_Button1_3 = gmCreatePushButton(ini_panel1_3, 330, 40, 60, 25, 'Change', gmType=GUSERDEFINED, gmoffcol=46, &
      gmTextCol=0, gmVpos=GTOP)



! Create panel ini_panel1_4 child of ini_Panel1
   ini_panel1_4 = gmCreatePanel(ini_Panel1, 10, 640, 400, 100, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmfillcol=227, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget ini_Text1 child of ini_panel1_4
   ini_Text1_4 = gmCreateTextEntry(ini_panel1_4, 10, 10, 80, 25,'Fixed rates : ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
! Create text widget ini_Text12 child of ini_panel1_4
   ini_Text1_5 = gmCreateTextEntry(ini_panel1_4, 170, 10, 140, 25,'Constrained by EC50 : ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)


! Create value entry ini_Value4 child of ini_panel1_4
   ini_Value1_4 = gmCreateValueEntry(ini_panel1_4, 90, 10, 20, 25, 0.000000, 8, 0, GDISPLAY, gmType=GDECIMAL, gmBack1Col=0, &
      gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create value entry ini_Value5 child of ini_panel1_4
   ini_Value1_5 = gmCreateValueEntry(ini_panel1_4, 330, 10, 20, 25, 0.000000, 8, 0, GDISPLAY, gmType=GDECIMAL, gmBack1Col=0, &
      gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
ini_Button1_4 = gmCreatePushButton(ini_panel1_4, 10, 40, 60, 25, 'Change', gmType=GUSERDEFINED, gmoffcol=46, &
      gmTextCol=0, gmVpos=GTOP)
ini_Button1_5 = gmCreatePushButton(ini_panel1_4, 170, 40, 60, 25, 'Change', gmType=GUSERDEFINED, gmoffcol=46, &
      gmTextCol=0, gmVpos=GTOP)




! Create panel ini_Panel2 child of ini_MainPanel
   ini_Panel2 = gmCreatePanel(ini_MainPanel, 420, 0, 260, 750, gmTitle='Data', gmType=GPROJECTED, gmLineBorder=GOUTEREDGE, &
      gmfillcol=229, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)
! Create text widget ini_Text10 child of ini_Panel2

ini_Button2 = gmCreatePushButton(ini_Panel2, 190, 5, 60, 25, 'Change', gmType=GUSERDEFINED, gmoffcol=46, &
      gmTextCol=0, gmVpos=GTOP)

   ini_Text2 = gmCreateTextEntry(ini_Panel2, 20, 20, 100, 25,'Number of sets : ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
	! Create value entry ini_Value9 child of ini_Panel2
   ini_Value2 = gmCreateValueEntry(ini_Panel2, 130, 20, 50, 25, 0.000000, 8, 0, GDISPLAY, gmType=GDECIMAL, gmBack1Col=0, &
      gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create text widget ini_Text11 child of ini_Panel2
   ini_Text2_00 = gmCreateTextEntry(ini_Panel2, 20, 40, 100, 25,'File names: ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
! Create text array ini_TxtArray2 child of ini_Panel2
   ini_TxtArray2 = gmCreateTextArray(ini_Panel2, 10, 70, 170, 120, 10, 10, gmAxisW=50, gmAxisH=25, gmXtext='*digits', &
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

! Create panel ini_panel2_1 child of ini_Panel2
   ini_panel2_1 = gmCreatePanel(ini_Panel2, 10, 200, 240, 540, gmType=GPRESSED, gmLineBorder=GOUTEREDGE, gmfillcol=232, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget ini_Text9 child of ini_panel2_1
   ini_Text2_0 = gmCreateTextEntry(ini_panel2_1, 10, 10, 50, 25,'SET 1 ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
ini_Button2_1 = gmCreatePushButton(ini_Panel2_1, 10, 510, 60, 25, 'Change', gmType=GUSERDEFINED, gmoffcol=46, &
      gmTextCol=0, gmVpos=GTOP)

! Create text widget ini_Text3 child of ini_panel2_1
   ini_Text2_1 = gmCreateTextEntry(ini_panel2_1, 10, 30, 130, 25,'Concentration (muM) ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text4 child of ini_panel2_1
   ini_Text2_2 = gmCreateTextEntry(ini_panel2_1, 10, 60, 130, 25,'One channel only ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text5 child of ini_panel2_1
   ini_Text2_3 = gmCreateTextEntry(ini_panel2_1, 10, 90, 130, 25,'Critical shut time (mus) ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text6 child of ini_panel2_1
   ini_Text2_4 = gmCreateTextEntry(ini_panel2_1, 10, 120, 130, 25,'Use CHS vector ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text7 child of ini_panel2_1
   ini_Text2_5 = gmCreateTextEntry(ini_panel2_1, 10, 150, 160, 25,'Bad shutting ends group ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text8 child of ini_panel2_1
   ini_Text2_6 = gmCreateTextEntry(ini_panel2_1, 10, 180, 130, 25,'Set intervals "bad" ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

 ini_Value2_1 = gmCreateValueEntry(ini_panel2_1, 150, 30, 50, 25, 0.000000, 8, 3, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
   

! Create toggle button ini_Toggle1 child of ini_panel2_1
   ini_Toggle2_2 = gmCreateToggleButton(ini_panel2_1, 150, 60, 40, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)
 
 !Create value entry ini_Value1 child of ini_panel2_1
  
! Create value entry ini_Value6 child of ini_panel2_1
   ini_Value2_3 = gmCreateValueEntry(ini_panel2_1, 150, 90, 60, 25, 0.000000, 8, 3, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create toggle button ini_Toggle2 child of ini_panel2_1
   ini_Toggle2_4 = gmCreateToggleButton(ini_panel2_1, 150, 120, 40, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button ini_Toggle3 child of ini_panel2_1
   ini_Toggle2_5 = gmCreateToggleButton(ini_panel2_1, 180, 150, 40, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button ini_Toggle4 child of ini_panel2_1
   ini_Toggle2_7 = gmCreateToggleButton(ini_panel2_1, 10, 200, 110, 25, 'shut times (ms)', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button ini_Toggle5 child of ini_panel2_1
   ini_Toggle2_8 = gmCreateToggleButton(ini_panel2_1, 10, 220, 120, 25, 'open times (ms)', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create value entry ini_Value7 child of ini_panel2_1
   ini_Value2_7 = gmCreateValueEntry(ini_panel2_1, 140, 200, 80, 25, 0.000000, 8, 3, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create value entry ini_Value8 child of ini_panel2_1
   ini_Value2_8 = gmCreateValueEntry(ini_panel2_1, 140, 220, 80, 25, 0.000000, 8, 3, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create button ini_Button15 child of ini_panel2_1
   ini_Button2_3 = gmCreatePushButton(ini_panel2_1, 80, 510, 75, 25, 'Previous', gmVpos=GTOP)

! Create button ini_Button16 child of ini_panel2_1
   ini_Button2_4 = gmCreatePushButton(ini_panel2_1, 160, 510, 75, 25, 'Next', gmVpos=GTOP)




! Create panel ini_Panel3 child of ini_MainPanel
   ini_Panel3 = gmCreatePanel(ini_MainPanel, 680, 0, 220, 640, gmTitle='Initial Guesses', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmfillcol=234, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text array ini_Txtarray3 child of ini_Panel3
   ini_Txtarray3 = gmCreateTextArray(ini_Panel3, 10, 90, 200, 530, 10, 10, gmAxisW=50, gmAxisH=25, gmXtext='*digits', &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmVpos=GTOP)
   call gmEnqCellAttribs(ini_Txtarray3, 1, 1, arrayattribs)
   arrayattribs%width=44
   call gmSetCellAttribs(ini_Txtarray3, 1, 1, arrayattribs)
   call gmSetCellAttribs(ini_Txtarray3, 2, 1, arrayattribs)
   call gmSetCellAttribs(ini_Txtarray3, 3, 1, arrayattribs)
   call gmSetCellAttribs(ini_Txtarray3, 4, 1, arrayattribs)
   call gmSetCellAttribs(ini_Txtarray3, 5, 1, arrayattribs)
   call gmSetCellAttribs(ini_Txtarray3, 6, 1, arrayattribs)
   call gmSetCellAttribs(ini_Txtarray3, 7, 1, arrayattribs)
   call gmSetCellAttribs(ini_Txtarray3, 8, 1, arrayattribs)
   call gmSetCellAttribs(ini_Txtarray3, 9, 1, arrayattribs)
   call gmSetCellAttribs(ini_Txtarray3, 10, 1, arrayattribs)
   call gmEnqCellAttribs(ini_Txtarray3, 1, 1, arrayattribs)
   arrayattribs%height=22
   call gmSetCellAttribs(ini_Txtarray3, 1, 1, arrayattribs)
   call gmSetCellAttribs(ini_Txtarray3, 1, 2, arrayattribs)
   call gmSetCellAttribs(ini_Txtarray3, 1, 3, arrayattribs)
   call gmSetCellAttribs(ini_Txtarray3, 1, 4, arrayattribs)
   call gmSetCellAttribs(ini_Txtarray3, 1, 5, arrayattribs)
   call gmSetCellAttribs(ini_Txtarray3, 1, 6, arrayattribs)
   call gmSetCellAttribs(ini_Txtarray3, 1, 7, arrayattribs)
   call gmSetCellAttribs(ini_Txtarray3, 1, 8, arrayattribs)
   call gmSetCellAttribs(ini_Txtarray3, 1, 9, arrayattribs)
   arrayattribs%height=106
   call gmSetCellAttribs(ini_Txtarray3, 1, 10, arrayattribs)

ini_Button3 = gmCreatePushButton(ini_Panel3, 150, 10, 60, 25, 'Change', gmType=GUSERDEFINED, gmoffcol=46, &
      gmTextCol=0, gmVpos=GTOP)



! Create panel ini_Panel4 child of ini_MainPanel
   ini_Panel4 = gmCreatePanel(ini_MainPanel, 680, 640, 220, 110, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmfillcol=248, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create button ini_Button13 child of ini_Panel4
   ini_Button4_1 = gmCreatePushButton(ini_Panel4, 20, 50, 75, 40, 'Fit now', gmType=GUSERDEFINED, gmoffcol=237, gmTextCol=0, &
      gmVpos=GTOP)

! Create button ini_Button14 child of ini_Panel4
   ini_Button4_2 = gmCreatePushButton(ini_Panel4, 110, 50, 75, 40, 'Display only', gmType=GUSERDEFINED, gmoffcol=238, &
      gmTextCol=0, gmVpos=GTOP)

! Create text widget ini_Text13 child of ini_Panel4
   ini_Text4 = gmCreateTextEntry(ini_Panel4, 10, 10, 200, 25,'All checked: proceed now ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=2 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Text4, gmBold=1)
    call gmdrawwindow(ini)
	call gmSetGuiGridMode(GOFF)
!	call gDefineRGB(0,1.,1.,0.94)
return
end
