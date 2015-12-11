subroutine initialize(main,initwin,initwin_Toggle3,initwin_Toggle4,initwin_Toggle5,&
initwin_Toggle9,initwin_Toggle10)

use menu_f90


! Widget identifier definitions
integer :: initwin
integer :: initwin_MainPanel
integer :: initwin_Panel1
integer :: initwin_Text1
integer :: initwin_Radio3
integer :: initwin_Toggle12
integer :: initwin_Toggle13
integer :: initwin_Combo1
integer :: initwin_Text3
integer :: initwin_Text4
integer :: initwin_Radio1
integer :: initwin_Toggle3
integer :: initwin_Toggle4
integer :: initwin_Toggle5
integer :: initwin_Radio2
integer :: initwin_Toggle9
integer :: initwin_Toggle10
integer :: initwin_Button5
integer :: initwin_Button6
 call gmSetGuiGridMode(GOFF)
  initwin = gmCreateMDIComplexDialogueBox(Main,200,100,381,440, GALL, 'Initial Settings ', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052')

! Create main panel for form
   initwin_MainPanel=gmCreatePanel(initwin, 0, 0, 381, 428, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)

! Create panel initwin_Panel1 child of initwin_MainPanel
   initwin_Panel1 = gmCreatePanel(initwin_MainPanel, 0, 0, 380, 50, gmType=GPROJECTED, gmLineBorder=GOUTEREDGE, gmFillCol=123, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget initwin_Text1 child of initwin_Panel1
   initwin_Text1 = gmCreateTextEntry(initwin_Panel1, 20, 10, 150, 25,'Initialisation file : ', 255, GDISPLAY, gmBack1Col=124, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   
   call gmSetFontAttribs(initwin_Text1, gmFontFace='PosterBodoni BT', gmPointSize=12)
! Create combo box initwin_Combo1 child of initwin_Panel1
  initwin_Combo1 = gmCreateComboBox(initwin_Panel1, 180, 10, 170, 100, NONE, 1, gmSort=GUNSORTED, gmVpos=GTOP)
 call gmSetListEntry(initwin_Combo1, GADD, gmString='hjcfit.ini')
   call gmSetListEntry(initwin_Combo1, GADD, gmString='browse')
   call gmSetListEntry(initwin_Combo1, GADD, gmString='none')
   call gmSetListEntry(initwin_Combo1,GSELECT,gmEntry=1)
initwin_Panel2 = gmCreatePanel(initwin_MainPanel, 0, 51, 380, 120, gmType=GPROJECTED, gmLineBorder=GOUTEREDGE, gmFillCol=123, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

  ! Create text widget initwin_Text3 child of initwin_Panel1
   initwin_Text3 = gmCreateTextEntry(initwin_Panel2, 20, 10, 140, 25,'Printout file : ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   call gmSetFontAttribs(initwin_Text3, gmFontFace='PosterBodoni BT', gmPointSize=12)

! Create text widget initwin_Text4 child of initwin_Panel1
   initwin_Text4 = gmCreateTextEntry(initwin_Panel2, 180, 10, 170, 25,'hjcfit.txt ', 255, GEDIT, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   call gmSetFontAttribs(initwin_Text4, gmPointSize=12)

! Create radio box child initwin_Radio3 child of initwin_Panel1
   initwin_Radio3 = gmCreateRadioBox(initwin_Panel2, 10, 40, 360, 70, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=124, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)


! Create toggle button initwin_Toggle12 child of initwin_Radio3
   initwin_Toggle12 = gmCreateToggleButton(initwin_Radio3, 20, 5, 100, 25, 'Append', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button initwin_Toggle13 child of initwin_Radio3
   initwin_Toggle13 = gmCreateToggleButton(initwin_Radio3, 20, 35, 100, 25, 'New', 0, gmType=G3DRADIO, gmVpos=GTOP)


! Create radio box child initwin_Radio1 child of initwin_MainPanel
   initwin_Radio1 = gmCreateRadioBox(initwin_MainPanel, 0, 171, 380, 120, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=122, gmFillBorder=GOUTEREDGE, gmTitle='Options:', gmVpos=GTOP)
 call gmSetFontAttribs(initwin_radio1, gmFontFace='PosterBodoni BT')
! Create toggle button initwin_Toggle3 child of initwin_Radio1
   initwin_Toggle3 = gmCreateToggleButton(initwin_Radio1, 40, 20, 100, 25, &
   'Fit data', 0, gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-11)

! Create toggle button initwin_Toggle4 child of initwin_Radio1
   initwin_Toggle4 = gmCreateToggleButton(initwin_Radio1, 40, 50, 220, 25, 'Show data and distributions', 0, gmType=G3DRADIO, &
      gmVpos=GTOP,gmcallback=-12)

! Create toggle button initwin_Toggle5 child of initwin_Radio1
   initwin_Toggle5 = gmCreateToggleButton(initwin_Radio1, 40, 80, 230, 25, 'Show only HJC distributions', 0, gmType=G3DRADIO, &
      gmVpos=GTOP,gmcallback=-13)

! Create radio box child initwin_Radio2 child of initwin_MainPanel
   initwin_Radio2 = gmCreateRadioBox(initwin_MainPanel, 0, 291, 380, 120, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=122, gmFillBorder=GOUTEREDGE, gmTitle='Data type:', gmVpos=GTOP)
call gmSetFontAttribs(initwin_radio1, gmFontFace='PosterBodoni BT')
!
! Create toggle button initwin_Toggle9 child of initwin_Radio2
   initwin_Toggle9 = gmCreateToggleButton(initwin_Radio2, 40, 30, 270, 25, 'Use experimental data (.scn file)', 0, gmType=G3DRADIO, &
      gmVpos=GTOP)
   call gmSetWidgetStatus(initwin_Toggle9, GUNSELECTABLE)

! Create toggle button initwin_Toggle10 child of initwin_Radio2
   initwin_Toggle10 = gmCreateToggleButton(initwin_Radio2, 40, 60, 270, 25, 'Use simulated data', 0, gmType=G3DRADIO, gmVpos=GTOP)
   call gmSetWidgetStatus(initwin_Toggle10, GUNSELECTABLE)

! Create button initwin_Button5 child of initwin_MainPanel
   initwin_Button5 = gmCreatePushButton(initwin_MainPanel, 0, 411, 190, 25, 'Advanced settings', &
   gmVpos=GTOP)
   call gmSetFontAttribs(initwin_Button5, gmFontFace='PosterBodoni BT')
 call gmSetWidgetStatus(initwin_button5, GSELECTABLE)

! Create button initwin_Button6 child of initwin_MainPanel
   initwin_Button6 = gmCreatePushButton(initwin_MainPanel, 190, 411, 190, 25, 'OK', &
   gmVpos=GTOP,gmcallback=-10)
   call gmSetFontAttribs(initwin_Button6, gmFontFace='PosterBodoni BT')
   call gmSetWidgetStatus(initwin_button6, GSELECTABLE)
call gmSetGuiGridMode(GOn)
return
 
end
