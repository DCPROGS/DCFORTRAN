subroutine qec50(main,iqec,iqec_Toggle1,iqec_Toggle2,iqec_Toggle3,iqec_Toggle4,ligname)
use menu_f90


! Widget identifier definitions
integer :: iqec
integer :: iqec_MainPanel
integer :: iqec_Radio1
integer :: iqec_Toggle1
integer :: iqec_Toggle2
integer :: iqec_Toggle3
integer :: iqec_Toggle4
character*20 ligname(10)
 call gmSetGuiGridMode(GOff)

! Set up master window iqec
   iqec =	gmCreateComplexDialogueBox(Main,200,30, 270, 230, GALL, '', &
              	 gmIconFormat=GDLLICON,gmIconFile='MBIG1011',gmvpos=gtop)


! Create main panel for form
   iqec_MainPanel=gmCreatePanel(iqec, 0, 0, 270, 230, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)

! Create radio box child iqec_Radio1 child of iqec_MainPanel
   iqec_Radio1 = gmCreateRadioBox(iqec_MainPanel, 0, 0, 270, 200, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=30, gmFillBorder=GOUTEREDGE, gmTitle='Specify EC50 as constraint for:', gmVpos=GTOP)

! Create toggle button iqec_Toggle1 child of iqec_Radio1
   iqec_Toggle1 = gmCreateToggleButton(iqec_Radio1, 30, 30, 150, 25, 'Agonist 1 :'//ligname(1), 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button iqec_Toggle2 child of iqec_Radio1
   iqec_Toggle2 = gmCreateToggleButton(iqec_Radio1, 30, 70, 150, 25, 'Agonist 2 :'//ligname(2), 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button iqec_Toggle3 child of iqec_Radio1
   iqec_Toggle3 = gmCreateToggleButton(iqec_Radio1, 30, 110, 150, 25, 'Agonist 1 and 2', 0, gmType=G3DRADIO, gmVpos=Gtop)

! Create toggle button iqec_Toggle4 child of iqec_Radio1
   iqec_Toggle4 = gmCreateToggleButton(iqec_Radio1, 30, 150, 150, 25, 'No EC50', 0, gmType=G3DRADIO, gmVpos=GTOP)
   
   iec50_Button1 = gmCreatePushButton(iqec_mainpanel, 0, 205, 270, 25, 'OK', gmVpos=GTOP,gmcallback=709)
 call gmSetGuiGridMode(GOn)
call gmdrawwindow(iqec)
return
end
