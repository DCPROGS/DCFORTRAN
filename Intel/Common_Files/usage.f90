subroutine usage(main,iusage,iUsage_Toggle1,iUsage_Toggle2)

use menu_f90

! Widget identifier definitions
integer :: iUsage
integer :: iUsage_MainPanel
integer :: iUsage_Radio1
integer :: iUsage_Toggle1
integer :: iUsage_Toggle2
integer :: iUsage_Panel1
integer :: iUsage_Text1
integer :: iUsage_Text2




   call gmSetGuiGridMode(goff)




! Set up master window iUsage
   iUsage =&
gmCreateComplexDialogueBox(Main,250,100,533,264, GALL, 'Menu', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)

! Create main panel for form
   iUsage_MainPanel=gmCreatePanel(iUsage, 0, 0, 533, 264, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)

! Create radio box child iUsage_Radio1 child of iUsage_MainPanel
   iUsage_Radio1 = gmCreateRadioBox(iUsage_MainPanel, 0, 0, 190, 260, gmType=GFREEFORM, gmBorderType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillCol=72, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

! Create toggle button iUsage_Toggle1 child of iUsage_Radio1
   iUsage_Toggle1 = gmCreateToggleButton(iUsage_Radio1, 30, 40, 100, 25, 'Typical', &
   0, gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-20)
   call gmSetFontAttribs(iUsage_Toggle1, gmFontFace='Courier', gmBold=GON)

! Create toggle button iUsage_Toggle2 child of iUsage_Radio1
   iUsage_Toggle2 = gmCreateToggleButton(iUsage_Radio1, 30, 120, 100, 25, 'Advanced',&
    0, gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-30)
   call gmSetFontAttribs(iUsage_Toggle2, gmFontFace='Courier', gmBold=GON)

! Create panel iUsage_Panel1 child of iUsage_MainPanel
   iUsage_Panel1 = gmCreatePanel(iUsage_MainPanel, 180, 0, 350, 260, gmType=GNOBOUNDARY, gmLineBorder=GOUTEREDGE, gmFillCol=72, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget iUsage_Text1 child of iUsage_Panel1
   iUsage_Text1 = gmCreateTextEntry(iUsage_Panel1, 10, 40, 300, 70,'Guide the users through the HJCFIT process with a set of se'// &
     'quential windows ', 30000, GDISPLAY, gmBack1Col=73, gmBack2Col=12, gmTextCol=16, gmScrollMode=GNOBARS, &
      gmBorderType=GNOBORDER, gmVpos=GTOP)
   call gmSetFontAttribs(iUsage_Text1, gmFontFace='Courier')

! Create text widget iUsage_Text2 child of iUsage_Panel1
   iUsage_Text2 = gmCreateTextEntry(iUsage_Panel1, 10, 120, 270, 130,'For experienced users who are familiar with HJCFIT proces'// &
     's. Also gives the user the opportunity to Visualize/Print previous plots and  Visualize/Create/Print mechanisms straighta'// &
     'way ', 30000, GDISPLAY, gmBack1Col=72, gmBack2Col=12, gmTextCol=16, gmScrollMode=GNOBARS, gmBorderType=GNOBORDER, &
      gmVpos=GTOP)
   call gmSetFontAttribs(iUsage_Text2, gmFontFace='Courier')
   call gmSetGuiGridMode(gon)
   call gmdrawwindow(iusage)
return
end
