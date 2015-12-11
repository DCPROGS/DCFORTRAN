subroutine ini_page(main)
use menu_f90
character  xtext*100
type (GLIMIT) :: Graphics_frame
type(GARRAYCELL) arrayattribs

common/ini_pag/ ini,ini_panel1,ini_panel1_1,ini_Text1_1,ini_Value2_1,ini_TxtArray2,&
ini_Value3_1,ini_TxtArray3,ini_Value4_1 ,ini_Value4_2,ini_Value5_1,ini_TxtArray5, &
ini_Value6_1 ,ini_Value6_3, ini_Value6_7 ,ini_Value6_8, ini_TxtArray7,&
ini_Toggle1_1,ini_Toggle2_1,ini_Toggle3_1,ini_Toggle4_1,ini_Toggle4_2,ini_Toggle5_1,&
ini_Toggle6_1,ini_Toggle6_2,ini_Toggle6_4,ini_Toggle6_5,ini_Toggle6_7,ini_Toggle6_8,&
ini_Toggle7_1
 

! Define user defined colour indices used
   call gDefineRGB(220, 1.000000, 1.000000, 0.807843)
   call gDefineRGB(221, 0.796078, 1.000000, 0.592157)
   call gDefineRGB(222, 0.682353, 0.843137, 1.000000)
   call gDefineRGB(223, 1.000000, 0.909804, 0.909804)
   call gDefineRGB(224, 0.792157, 1.000000, 0.792157)
   call gDefineRGB(225, 1.000000, 0.866667, 0.733333)
   call gDefineRGB(226, 0.996078, 0.780392, 0.705882)
   call gDefineRGB(227, 1.000000, 0.458824, 0.458824)
   call gDefineRGB(228, 1.000000, 1.000000, 0.517647)
   call gDefineRGB(229, 0.576471, 0.992157, 0.839216)
   call gDefineRGB(230, 0.501961, 1.000000, 0.619608)
   call gDefineRGB(231, 0.549020, 1.000000, 0.658824)
   call gDefineRGB(232, 0.713726, 1.000000, 0.784314)
   call gDefineRGB(233, 0.686275, 1.000000, 1.000000)
   call gDefineRGB(234, 0.694118, 0.901961, 0.992157)
   call gDefineRGB(235, 0.866667, 1.000000, 0.866667)
   call gDefineRGB(236, 0.839216, 0.992157, 0.858824)
   call gDefineRGB(237, 1.000000, 0.274510, 0.274510)
   call gDefineRGB(238, 1.000000, 0.658824, 0.658824)
   call gDefineRGB(239, 1.000000, 0.556863, 0.121569)
   call gDefineRGB(240, 0.949020, 0.792157, 0.549020)
   call gDefineRGB(241, 0.176471, 0.584314, 1.000000)
   call gDefineRGB(242, 0.000000, 0.501961, 0.749020)
   call gDefineRGB(243, 0.000000, 0.576471, 0.847059)
   call gDefineRGB(244, 0.000000, 0.764706, 0.764706)
   call gDefineRGB(245, 1.000000, 1.000000, 0.501961)
   call gDefineRGB(246, 1.000000, 1.000000, 0.392157)
   call gDefineRGB(247, 0.921569, 0.937255, 0.937255)
   call gDefineRGB(248, 1.000000, 0.909804, 0.866667)


call gmSetGuiGridMode(GOFF)
! Set up master window ini
   ini = gmCreateComplexDialogueBox(main,48, 48, 918, 774, GALL, 'Initial settings', &
   gmVpos=GTOP, gmInitState=GNORMAL, gmIconFormat=GDLLICON, &
      gmIconFile='Gee')



! Create main panel for form
   ini_MainPanel=gmCreatePanel(ini, 0, 0, 918, 774, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmScrollMode=GbothBARS)

! Create panel ini_Panel1 child of ini_MainPanel
   ini_Panel1 = gmCreatePanel(ini_MainPanel, 0, 0, 430, 330, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmFillcol=220, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create button ini_Button1 child of ini_Panel1
   ini_Button1_2 = gmCreatePushButton(ini_Panel1, 360, 290, 60, 25, 'Change', gmType=GUSERDEFINED, gmOffcol=228, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-112)

! Create panel ini_Panel9 child of ini_Panel1
   ini_Panel1_1 = gmCreatePanel(ini_Panel1, 10, 40, 340, 280, gmTitle='', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmScrollMode=GbothBARS, gmVpos=GTOP)

! Create button ini_Button2 child of ini_Panel1
   ini_Button1_1 = gmCreatePushButton(ini_Panel1, 390, 250, 30, 25, 'OK', gmType=GUSERDEFINED, gmOffcol=224, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-111)

! Create toggle button ini_Toggle1 child of ini_Panel1
   ini_Toggle1_1 = gmCreateToggleButton(ini_Panel1, 360, 250, 25, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create text widget ini_Text1 child of ini_Panel1
   ini_Text1 = gmCreateTextEntry(ini_Panel1, 20, 10, 100, 25,'Mechanism ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text15 child of ini_Panel1
   ini_Text1_1 = gmCreateTextEntry(ini_Panel1, 110, 10, 300, 25,' ', 255, GEDIT, gmBack1Col=0, gmBack2Col=0, gmTextCol=1 , &
     gmScrollable=Gon, gmVpos=GTOP)

! Create panel ini_Panel2 child of ini_MainPanel
   ini_Panel2 = gmCreatePanel(ini_MainPanel, 0, 330, 430, 150, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillcol=220, gmFillBorder=GOUTEREDGE, gmScrollMode=GnoBARS, gmVpos=GTOP)

! Create text array ini_TxtArray2 child of ini_Panel2
xtext='Rate name '//char(124)//'Qij'
   ini_TxtArray2 = gmCreateTextArray(ini_Panel2, 10, 40, 340, 100, 2, 10, gmAxisW=50, gmAxisH=25, gmXtext=xtext, &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmVpos=GTOP)
   call gmEnqCellAttribs(ini_TxtArray2, 1, 1, arrayattribs)
   arrayattribs%height=22
   arrayattribs%width=150
   call gmSetCellAttribs(ini_TxtArray2, 1, 1, arrayattribs)
   arrayattribs%width=100
   call gmSetCellAttribs(ini_TxtArray2, 2, 1, arrayattribs)
!   call gmSetCellAttribs(ini_TxtArray2, 3, 1, arrayattribs)
   
   call gmEnqCellAttribs(ini_TxtArray2, 1, 1, arrayattribs)
  
   
   call gmSetCellAttribs(ini_TxtArray2, 1, 1, arrayattribs)
  

! Create text widget ini_Text2 child of ini_Panel2
   ini_Text2 = gmCreateTextEntry(ini_Panel2, 20, 10, 170, 25,'Microscopic reversibility ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create toggle button ini_Toggle2 child of ini_Panel2
   ini_Toggle2_1 = gmCreateToggleButton(ini_Panel2, 360, 70, 25, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create button ini_Button3 child of ini_Panel2
   ini_Button2_1 = gmCreatePushButton(ini_Panel2, 380, 70, 30, 25, 'OK', gmType=GUSERDEFINED, gmOffcol=224, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-121)

! Create button ini_Button4 child of ini_Panel2
   ini_Button2_2 = gmCreatePushButton(ini_Panel2, 360, 110, 60, 25, 'Change', gmType=GUSERDEFINED, gmOffcol=228, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-122)

! Create value entry ini_Value1 child of ini_Panel2
   ini_Value2_1 = gmCreateValueEntry(ini_Panel2, 240, 10, 50, 25, 0.000000, 8, 0, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create panel ini_Panel3 child of ini_MainPanel
   ini_Panel3 = gmCreatePanel(ini_MainPanel, 0, 480, 430, 160, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillcol=220, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text array ini_TxtArray2 child of ini_Panel3
xtext='Rate 1'//char(124)//'Factor'//char(124)//'Rate 2'
   ini_TxtArray3 = gmCreateTextArray(ini_Panel3, 10, 40, 340, 110, 3, 20, gmAxisW=50, gmAxisH=25, gmXtext=xtext, &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmVpos=GTOP)
   call gmEnqCellAttribs(ini_TxtArray3, 1, 1, arrayattribs)
   arrayattribs%width=90
   call gmSetCellAttribs(ini_TxtArray3, 1, 1, arrayattribs)
   
   call gmSetCellAttribs(ini_TxtArray3, 3, 1, arrayattribs)
   arrayattribs%width=80
   call gmSetCellAttribs(ini_TxtArray3, 2, 1, arrayattribs)
   call gmEnqCellAttribs(ini_TxtArray3, 1, 1, arrayattribs)
   arrayattribs%height=22
   do j=1,20
   call gmSetCellAttribs(ini_TxtArray3, 1, j, arrayattribs)
   enddo
   

! Create text widget ini_Text3 child of ini_Panel3
   ini_Text3 = gmCreateTextEntry(ini_Panel3, 20, 10, 170, 25,'Constrained rates ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create toggle button ini_Toggle3 child of ini_Panel3
   ini_Toggle3_1 = gmCreateToggleButton(ini_Panel3, 360, 90, 25, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create button ini_Button5 child of ini_Panel3
   ini_Button3_1 = gmCreatePushButton(ini_Panel3, 380, 90, 30, 25, 'OK', gmType=GUSERDEFINED, gmOffcol=224, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-131)

! Create button ini_Button6 child of ini_Panel3
   ini_Button3_2 = gmCreatePushButton(ini_Panel3, 360, 120, 60, 20, 'Change', gmType=GUSERDEFINED, gmOffcol=228, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-132)

! Create value entry ini_Value2 child of ini_Panel3
   ini_Value3_1 = gmCreateValueEntry(ini_Panel3, 250, 10, 50, 25, 0.000000, 8, 0, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create panel ini_Panel4 child of ini_MainPanel
   ini_Panel4 = gmCreatePanel(ini_MainPanel, 0, 640, 430, 130, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillcol=220, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget ini_Text4 child of ini_Panel4
   ini_Text4_1 = gmCreateTextEntry(ini_Panel4, 20, 20, 130, 25,'Fixed rates ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text5 child of ini_Panel4
   ini_Text4_2 = gmCreateTextEntry(ini_Panel4, 20, 70, 140, 25,'Constrained by EC50 ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create toggle button ini_Toggle4 child of ini_Panel4
   ini_Toggle4_1 = gmCreateToggleButton(ini_Panel4, 360, 10, 25, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button ini_Toggle5 child of ini_Panel4
   ini_Toggle4_2 = gmCreateToggleButton(ini_Panel4, 360, 70, 25, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create button ini_Button7 child of ini_Panel4
   ini_Button4_11 = gmCreatePushButton(ini_Panel4, 380, 10, 30, 25, 'OK', gmType=GUSERDEFINED, gmOffcol=224, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-141)

! Create button ini_Button8 child of ini_Panel4
   ini_Button4_12 = gmCreatePushButton(ini_Panel4, 360, 40, 60, 25, 'Change', gmType=GUSERDEFINED, gmOffcol=228, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-142)

! Create button ini_Button9 child of ini_Panel4
   ini_Button4_21 = gmCreatePushButton(ini_Panel4, 380, 70, 30, 25, 'OK', gmType=GUSERDEFINED, gmOffcol=224, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-143)

! Create button ini_Button10 child of ini_Panel4
   ini_Button4_22 = gmCreatePushButton(ini_Panel4, 360, 100, 60, 25, 'Change', gmType=GUSERDEFINED, gmOffcol=228, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-144)

! Create value entry ini_Value3 child of ini_Panel4
   ini_Value4_1 = gmCreateValueEntry(ini_Panel4, 180, 20, 50, 25, 0.000000, 8, 0, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create value entry ini_Value4 child of ini_Panel4
   ini_Value4_2 = gmCreateValueEntry(ini_Panel4, 180, 70, 50, 25, 0.000000, 8, 0, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
call gmsetvaluesetting(ini_Value4_1,nfix)
! Create panel ini_Panel5 child of ini_MainPanel
   ini_Panel5 = gmCreatePanel(ini_MainPanel, 430, 0, 260, 280, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillcol=221, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text array ini_TxtArray5 child of ini_Panel5
xtext='File name '
   ini_TxtArray5 = gmCreateTextArray(ini_Panel5, 10, 90, 240, 130, 1, 10, gmAxisW=50, gmAxisH=25, gmXtext=xtext, &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmVpos=GTOP)
   call gmEnqCellAttribs(ini_TxtArray5, 1, 1, arrayattribs)
   arrayattribs%width=250
   call gmSetCellAttribs(ini_TxtArray5, 1, 1, arrayattribs)
     arrayattribs%height=22
	 do j=1,10
   call gmSetCellAttribs(ini_TxtArray5, 1, j, arrayattribs)
  enddo
! Create text widget ini_Text6 child of ini_Panel5
   ini_Text5 = gmCreateTextEntry(ini_Panel5, 20, 10, 100, 25,'Data ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text7 child of ini_Panel5
   ini_Text5_1 = gmCreateTextEntry(ini_Panel5, 20, 50, 130, 25,'Number of sets ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create toggle button ini_Toggle6 child of ini_Panel5
   ini_Toggle5_1 = gmCreateToggleButton(ini_Panel5, 10, 240, 25, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create button ini_Button11 child of ini_Panel5
   ini_Button5_1 = gmCreatePushButton(ini_Panel5, 40, 240, 30, 25, 'OK', gmType=GUSERDEFINED, gmOffcol=224, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-151)

! Create button ini_Button12 child of ini_Panel5
   ini_Button5_2 = gmCreatePushButton(ini_Panel5, 180, 240, 60, 25, 'Change', gmType=GUSERDEFINED, gmOffcol=228, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-152)

! Create value entry ini_Value5 child of ini_Panel5
   ini_Value5_1 = gmCreateValueEntry(ini_Panel5, 180, 50, 50, 25, 0.000000, 8, 0, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create panel ini_Panel6 child of ini_MainPanel
   ini_Panel6 = gmCreatePanel(ini_MainPanel, 430, 290, 260, 480, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillcol=221, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget ini_Text8 child of ini_Panel6
   ini_Text6 = gmCreateTextEntry(ini_Panel6, 20, 10, 100, 25,'Set 1 ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Text6, gmBold=1)

! Create toggle button ini_Toggle9 child of ini_Panel6
   ini_Toggle6_1 = gmCreateToggleButton(ini_Panel6, 20, 360, 25, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create button ini_Button13 child of ini_Panel6
   ini_Button6_1 = gmCreatePushButton(ini_Panel6, 50, 360, 30, 25, 'OK', gmType=GUSERDEFINED, gmOffcol=224, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-161)
! Create button ini_Button16 child of ini_Panel6
   ini_Button6_2 = gmCreatePushButton(ini_Panel6, 180, 360, 60, 25, 'Change', gmType=GUSERDEFINED, gmOffcol=228, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-162)

! Create text widget ini_Text9 child of ini_Panel6
   ini_Text6_1 = gmCreateTextEntry(ini_Panel6, 20, 50, 140, 25,'Concentration (muM) ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text10 child of ini_Panel6
   ini_Text6_2 = gmCreateTextEntry(ini_Panel6, 20, 90, 160, 25,'One channel only ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text11 child of ini_Panel6
   ini_Text6_3 = gmCreateTextEntry(ini_Panel6, 20, 130, 160, 25,'Critical shut time (mus) ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text12 child of ini_Panel6
   ini_Text6_4 = gmCreateTextEntry(ini_Panel6, 20, 170, 150, 25,'Use CHS vectors ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text13 child of ini_Panel6
   ini_Text6_5 = gmCreateTextEntry(ini_Panel6, 20, 210, 160, 25,'Bad shutting ends group ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text14 child of ini_Panel6
   ini_Text6_6 = gmCreateTextEntry(ini_Panel6, 20, 250, 160, 25,'Set intervals bad ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create toggle button ini_Toggle7 child of ini_Panel6
   ini_Toggle6_7 = gmCreateToggleButton(ini_Panel6, 20, 280, 140, 25, 'shutt times (ms)', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button ini_Toggle8 child of ini_Panel6
   ini_Toggle6_8 = gmCreateToggleButton(ini_Panel6, 20, 310, 140, 25, 'open times (ms)', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)


! Create button ini_Button14 child of ini_Panel6
   ini_Button16_3 = gmCreatePushButton(ini_Panel6, 180, 450, 75, 25, 'Next', gmVpos=GTOP,gmCallback=-163)
	
! Create button ini_Button15 child of ini_Panel6
   ini_Button16_4 = gmCreatePushButton(ini_Panel6, 100, 450, 75, 25, 'Previous', gmVpos=GTOP,gmCallback=-164)



! Create toggle button ini_Toggle10 child of ini_Panel6
   ini_Toggle6_2 = gmCreateToggleButton(ini_Panel6, 190, 90, 30, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button ini_Toggle11 child of ini_Panel6
   ini_Toggle6_4 = gmCreateToggleButton(ini_Panel6, 190, 170, 30, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button ini_Toggle12 child of ini_Panel6
   ini_Toggle6_5 = gmCreateToggleButton(ini_Panel6, 190, 210, 30, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create value entry ini_Value6 child of ini_Panel6
   ini_Value6_1 = gmCreateValueEntry(ini_Panel6, 190, 50, 60, 25, 0.000000, 8, 3, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=Gscientific, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create value entry ini_Value7 child of ini_Panel6
   ini_Value6_3 = gmCreateValueEntry(ini_Panel6, 190, 130, 60, 25, 0.000000, 8, 3, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=Gscientific, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create value entry ini_Value8 child of ini_Panel6
   ini_Value6_7 = gmCreateValueEntry(ini_Panel6, 180, 280, 70, 25, 0.000000, 8, 3, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=Gscientific, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create value entry ini_Value9 child of ini_Panel6
   ini_Value6_8 = gmCreateValueEntry(ini_Panel6, 180, 310, 70, 25, 0.000000, 8, 3, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=Gscientific, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create panel ini_Panel7 child of ini_MainPanel
   ini_Panel7 = gmCreatePanel(ini_MainPanel, 690, 0, 220, 630, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillcol=222, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text array ini_TxtArray7 child of ini_Panel7
	xtext='Rate name '//char(124)//'Value'
   ini_TxtArray7 = gmCreateTextArray(ini_Panel7, 10, 90, 200, 460, 2, 20, gmAxisW=50, gmAxisH=25, gmXtext=xtext, &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmVpos=GTOP)
   call gmEnqCellAttribs(ini_TxtArray7, 1, 1, arrayattribs)
   arrayattribs%width=80
   call gmSetCellAttribs(ini_TxtArray7, 1, 1, arrayattribs)
   call gmSetCellAttribs(ini_TxtArray7, 2, 1, arrayattribs)
   arrayattribs%height=22
   do j=1,20
   call gmSetCellAttribs(ini_TxtArray7, 1, j, arrayattribs)
   enddo
! Create text widget ini_Text16 child of ini_Panel7
   ini_Text7 = gmCreateTextEntry(ini_Panel7, 10, 10, 170, 25,'Initial guesses ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create toggle button ini_Toggle13 child of ini_Panel7
   ini_Toggle7_1 = gmCreateToggleButton(ini_Panel7, 10, 590, 25, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create button ini_Button17 child of ini_Panel7
   ini_Button7_1 = gmCreatePushButton(ini_Panel7, 40, 590, 30, 25, 'OK', gmType=GUSERDEFINED, gmOffcol=224, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-171)

! Create button ini_Button18 child of ini_Panel7
   ini_Button7_2 = gmCreatePushButton(ini_Panel7, 140, 590, 60, 25, 'Change', gmType=GUSERDEFINED, gmOffcol=228, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-172)

! Create text widget ini_Text17 child of ini_Panel7
   ini_Text7_1 = gmCreateTextEntry(ini_Panel7, 10, 50, 190, 25,'File ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create panel ini_Panel8 child of ini_MainPanel
   ini_Panel8 = gmCreatePanel(ini_MainPanel, 690, 630, 220, 140, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillcol=223, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget ini_Text18 child of ini_Panel8
   ini_Text8 = gmCreateTextEntry(ini_Panel8, 10, 20, 190, 25,'All checked: proceed now ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextcol=22 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Text8, gmBold=1)

! Create button ini_Button19 child of ini_Panel8
   ini_Button8_1 = gmCreatePushButton(ini_Panel8, 10, 70, 90, 40, 'Fit now', gmType=GUSERDEFINED, gmOffcol=227, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-101)

! Create button ini_Button20 child of ini_Panel8
   ini_Button8_2 = gmCreatePushButton(ini_Panel8, 120, 70, 80, 40, 'Display only', gmType=GUSERDEFINED, gmOffcol=226, &
      gmTextCol=0, gmVpos=GTOP,gmCallback=-102)
  call gmdrawwindow(ini)
!	call gmSetGuiGridMode(GOn)
return
end
