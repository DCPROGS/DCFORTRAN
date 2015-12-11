subroutine initial(main,initialw,initialw_Toggle1,initialw_Toggle2, initialw_Text1)
 
 use menu_f90
call gDefineRGB(38,1.,0.5,0.25)	!orange
 call gmSetGuiGridMode(GOff)
! Set up complex dialogue box initialw child of imecform1
 initialw = gmCreateComplexDialogueBox(Main,150,20,330,188, GALL, 'Initial Settings ', &
              	gmhpos=gmiddle,gmvpos=gmiddle,gmIconFormat=GDLLICON,gmIconFile='MBIG1052')

! Create panel initialw_Panel1 child of initialw_MainPanel
   initialw_Panel1 = gmCreatePanel(initialw, 0, 0, 330, 188, gmTitle='Initialization file', gmType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=140, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget initialw_Text1 child of initialw_Panel1
   initialw_Text1 = gmCreateTextEntry(initialw_Panel1, 20, 30, 200, 25,'hjcfit.ini ', 255, GEDIT, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create button initialw_Button1 child of initialw_Panel1
   initialw_Button1 = gmCreatePushButton(initialw_Panel1, 240, 30, 75, 25, 'Browse', &
   gmcallback=-5,gmVpos=GTOP)
 call gmSetWidgetStatus(initialw_button1, GSELECTABLE) 
 
!Create radio box child initialw_Radio1 child of initialw_Panel1
   initialw_Radio1 = gmCreateRadioBox(initialw_Panel1, 20, 70, 330, 70, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
     gmLineBorder=GOUTEREDGE, gmFillCol=141, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

! Create toggle button initialw_Toggle1 child of initialw_Radio1
   initialw_Toggle1 = gmCreateToggleButton(initialw_Radio1, 20, 10, 220, 25, 'Read existing ini file', 1, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initialw_Toggle2 child of initialw_Radio1
   initialw_Toggle2 = gmCreateToggleButton(initialw_Radio1, 20, 40, 220, 25, 'No ini file', 0, gmType=G3DRADIO, gmVpos=GTOP)

  initialw_Button6 = gmCreatePushButton(initialw_Panel1, 10, 160, 310, 25, 'Continue', &
   gmtype=guserdefined,gmoffcol=38,gmVpos=GTOP,gmcallback=-4)
  call gmSetWidgetStatus(initialw_button6, GSELECTABLE)
  ! call gmSetWidgetStatus(initwin_Button6, GCHECKED)
   call gmSetGuiGridMode(GOn)
   call gmdrawwindow(initialw)
   end