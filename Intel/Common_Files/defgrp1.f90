
subroutine defgrp1(main,nset)
use menu_f90

! Widget identifier definitions
integer :: group
integer :: group_MainPanel
integer :: group_Panel1
integer :: group_Button1
integer :: group_Button2
integer :: group_Static2
integer :: group_Static1
integer :: group_Static3
integer :: group_Static4
integer :: group_Static5
integer :: group_Static6
integer :: group_Panel4
integer :: group_Panel8

integer :: group_Toggle15
integer :: group_Toggle16
integer :: group_Toggle6
integer :: group_Editval2
integer :: group_Toggle7
integer :: group_Editval3
integer :: group_Toggle3
integer :: group_Panel3
integer :: group_Panel2
integer :: group_Editval1
integer :: group_Toggle4
integer :: group_Toggle5
integer :: group_Toggle19
integer :: group_Toggle20
integer :: group_Toggle21
integer :: group_Editval13
integer :: group_Editval14
integer :: group_Panel12
integer :: group_Panel13
integer :: group_Editval15
integer :: group_Toggle27
integer :: group_Toggle28
integer :: group_Toggle29
integer :: group_Toggle30
integer :: group_Toggle31
integer :: group_Editval16
integer :: group_Editval17
integer :: group_Panel14
integer :: group_Panel15
integer :: group_Editval18
integer :: group_Toggle32
integer :: group_Toggle33
integer :: group_Toggle34
integer :: group_Toggle35
integer :: group_Toggle36
integer :: group_Editval19
integer :: group_Editval20
integer :: group_Button7
integer :: callid
integer :: group_Editval8
 call gmSetGuiGridMode(GOff)
if(nset.eq.1) iwid=300
if(nset.eq.2) iwid=400
if(nset.eq.3) iwid=500
if(nset.eq.4) iwid=600
! Set up master window group
   group = gmCreateMDIComplexDialogueBox(Main,72, 117, iwid, 330, Gall, 'Groups', gmIconFormat=GDLLICON, &
      gmIconFile='Gee')

! Create main panel for form
   group_MainPanel=gmCreatePanel(group, 0, 0, iwid, 330, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)

! Create panel group_Panel1 child of group_MainPanel
   group_Panel1 = gmCreatePanel(group_MainPanel, 0, 0,iwid, 330, gmTitle='DEFINE GROUPS OF OPENINGS', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=150, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create button group_Button1 child of group_Panel1
   group_Button1 = gmCreatePushButton(group_Panel1, 40, 280, 75, 25, 'Help', gmVpos=GTOP)

! Create button group_Button2 child of group_Panel1
   group_Button2 = gmCreatePushButton(group_Panel1, 130, 280, 75, 25, 'All done', gmVpos=GTOP)

! Create text widget group_Static2 child of group_Panel1
   group_Static2 = gmCreateTextEntry(group_Panel1, 30, 110, 100, 25,'Use CHS vectors ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget group_Static1 child of group_Panel1
   group_Static1 = gmCreateTextEntry(group_Panel1, 30, 80, 120, 25,'Critical shut time (mus) ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget group_Static3 child of group_Panel1
   group_Static3 = gmCreateTextEntry(group_Panel1, 30, 140, 120, 25,'Bad shutting ends group ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget group_Static4 child of group_Panel1
   group_Static4 = gmCreateTextEntry(group_Panel1, 10, 190, 150, 25,'Set long open times ''bad'' (ms) ', 255, GDISPLAY, &
      gmBack1Col=0, gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget group_Static5 child of group_Panel1
   group_Static5 = gmCreateTextEntry(group_Panel1, 10, 40, 150, 25,'All openings from one channel ', 255, GDISPLAY, &
      gmBack1Col=0, gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget group_Static6 child of group_Panel1
   group_Static6 = gmCreateTextEntry(group_Panel1, 10, 220, 140, 20,'Set long shut times ''bad'' (ms) ', 255, GDISPLAY, &
      gmBack1Col=0, gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create panel group_Panel3 child of group_Panel1
   group_Panel3 = gmCreatePanel(group_Panel1, 190, 20, 90, 240, gmTitle='SET 1', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=150, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create panel group_Panel2 child of group_Panel3
   group_Panel2 = gmCreatePanel(group_Panel3, 10, 50, 60, 100, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmFillCol=152, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create value entry group_Editval1 child of group_Panel2
   group_Editval1 = gmCreateValueEntry(group_Panel2, 10, 10, 40, 25, 40.000000, 8, 1, GEDIT, gmVmin=0.000000, &
      gmVmax=1000.000000, gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create toggle button group_Toggle4 child of group_Panel2
   group_Toggle4 = gmCreateToggleButton(group_Panel2, 20, 40, 20, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button group_Toggle5 child of group_Panel2
   group_Toggle5 = gmCreateToggleButton(group_Panel2, 20, 70, 30, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button group_Toggle19 child of group_Panel3
   group_Toggle19 = gmCreateToggleButton(group_Panel3, 10, 20, 20, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button group_Toggle20 child of group_Panel3
   group_Toggle20 = gmCreateToggleButton(group_Panel3, 10, 170, 20, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button group_Toggle21 child of group_Panel3
   group_Toggle21 = gmCreateToggleButton(group_Panel3, 10, 200, 20, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create value entry group_Editval13 child of group_Panel3
   group_Editval13 = gmCreateValueEntry(group_Panel3, 40, 170, 40, 25, 10000.000000, 8, 0, GEDIT, gmVmin=0.000000, &
      gmVmax=10000.000000, gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create value entry group_Editval14 child of group_Panel3
   group_Editval14 = gmCreateValueEntry(group_Panel3, 40, 200, 40, 25, 0.000000, 8, 0, GEDIT, gmVmin=0.000000, &
      gmVmax=1000.000000, gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create panel group_Panel12 child of group_Panel1
if(nset.gt.1) then
! Create panel group_Panel4 child of group_Panel1
   group_Panel4 = gmCreatePanel(group_Panel1, 290, 20, 90, 240, gmTitle='SET 2', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=150, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create panel group_Panel8 child of group_Panel4
   group_Panel8 = gmCreatePanel(group_Panel4, 10, 50, 60, 100, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmFillCol=152, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create value entry group_Editval8 child of group_Panel8
   group_Editval8 = gmCreateValueEntry(group_Panel8, 10, 10, 40, 25, 40.000000, 8, 1, GEDIT, gmVmin=0.000000, &
      gmVmax=1000.000000, gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create toggle button group_Toggle15 child of group_Panel8
   group_Toggle15 = gmCreateToggleButton(group_Panel8, 20, 40, 20, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button group_Toggle16 child of group_Panel8
   group_Toggle16 = gmCreateToggleButton(group_Panel8, 20, 70, 30, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button group_Toggle6 child of group_Panel4
   group_Toggle6 = gmCreateToggleButton(group_Panel4, 10, 170, 20, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create value entry group_Editval2 child of group_Panel4
   group_Editval2 = gmCreateValueEntry(group_Panel4, 40, 170, 40, 25, 0.000000, 8, 0, GEDIT, gmVmin=0.000000, &
      gmVmax=1000.000000, gmType=GDECIMAL, gmBack1Col=8, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP, gmHelp='length of open time '// &
     'in ms')

! Create toggle button group_Toggle7 child of group_Panel4
   group_Toggle7 = gmCreateToggleButton(group_Panel4, 10, 200, 20, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP, gmHelp='Enter du'// &
     'ration in ms')

! Create value entry group_Editval3 child of group_Panel4
   group_Editval3 = gmCreateValueEntry(group_Panel4, 40, 200, 40, 25, 0.000000, 8, 0, GEDIT, gmVmin=0.000000, &
      gmVmax=1000.000000, gmType=GDECIMAL, gmBack1Col=8, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create toggle button group_Toggle3 child of group_Panel4
   group_Toggle3 = gmCreateToggleButton(group_Panel4, 10, 20, 30, 25, '', 1, gmType=G3DCHECKBOX, gmVpos=GTOP, gmHelp='and all s'// &
     'tates are in model')
endif
if(nset.gt.2) then
   group_Panel12 = gmCreatePanel(group_Panel1, 390, 20, 90, 240, gmTitle='SET 3', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=150, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create panel group_Panel13 child of group_Panel12
   group_Panel13 = gmCreatePanel(group_Panel12, 10, 50, 60, 100, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmFillCol=152, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create value entry group_Editval15 child of group_Panel13
   group_Editval15 = gmCreateValueEntry(group_Panel13, 10, 10, 40, 25, 35.000000, 8, 1, GEDIT, gmVmin=0.000000, &
      gmVmax=1000.000000, gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create toggle button group_Toggle27 child of group_Panel13
   group_Toggle27 = gmCreateToggleButton(group_Panel13, 20, 40, 20, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button group_Toggle28 child of group_Panel13
   group_Toggle28 = gmCreateToggleButton(group_Panel13, 20, 70, 30, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button group_Toggle29 child of group_Panel12
   group_Toggle29 = gmCreateToggleButton(group_Panel12, 10, 20, 20, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button group_Toggle30 child of group_Panel12
   group_Toggle30 = gmCreateToggleButton(group_Panel12, 10, 170, 20, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button group_Toggle31 child of group_Panel12
   group_Toggle31 = gmCreateToggleButton(group_Panel12, 10, 200, 20, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create value entry group_Editval16 child of group_Panel12
   group_Editval16 = gmCreateValueEntry(group_Panel12, 40, 170, 40, 25, 0.000000, 8, 0, GEDIT, gmVmin=0.000000, &
      gmVmax=1000.000000, gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create value entry group_Editval17 child of group_Panel12
   group_Editval17 = gmCreateValueEntry(group_Panel12, 40, 200, 40, 25, 0.000000, 8, 0, GEDIT, gmVmin=0.000000, &
      gmVmax=1000.000000, gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
endif
if(nset.gt.3) then
! Create panel group_Panel14 child of group_Panel1
   group_Panel14 = gmCreatePanel(group_Panel1, 490, 20, 90, 240, gmTitle='SET 4', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=150, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create panel group_Panel15 child of group_Panel14
   group_Panel15 = gmCreatePanel(group_Panel14, 10, 50, 60, 100, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmFillCol=152, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create value entry group_Editval18 child of group_Panel15
   group_Editval18 = gmCreateValueEntry(group_Panel15, 10, 10, 40, 25, 30.000000, 8, 1, GEDIT, gmVmin=0.000000, &
      gmVmax=1000.000000, gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create toggle button group_Toggle32 child of group_Panel15
   group_Toggle32 = gmCreateToggleButton(group_Panel15, 20, 40, 20, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button group_Toggle33 child of group_Panel15
   group_Toggle33 = gmCreateToggleButton(group_Panel15, 20, 70, 30, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button group_Toggle34 child of group_Panel14
   group_Toggle34 = gmCreateToggleButton(group_Panel14, 10, 20, 20, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button group_Toggle35 child of group_Panel14
   group_Toggle35 = gmCreateToggleButton(group_Panel14, 10, 170, 20, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button group_Toggle36 child of group_Panel14
   group_Toggle36 = gmCreateToggleButton(group_Panel14, 10, 200, 20, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create value entry group_Editval19 child of group_Panel14
   group_Editval19 = gmCreateValueEntry(group_Panel14, 40, 170, 40, 25, 0.000000, 8, 0, GEDIT, gmVmin=0.000000, &
      gmVmax=1000.000000, gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create value entry group_Editval20 child of group_Panel14
   group_Editval20 = gmCreateValueEntry(group_Panel14, 40, 200, 40, 25, 0.000000, 8, 0, GEDIT, gmVmin=0.000000, &
      gmVmax=1000.000000, gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
endif
if(nset.gt.4) then
! Create button group_Button7 child of group_Panel1
   group_Button7 = gmCreatePushButton(group_Panel1, 320, 280, 75, 25, 'Next 3 sets', gmVpos=GTOP)
endif
call gmSetGuiGridMode(GOff)
return
end
