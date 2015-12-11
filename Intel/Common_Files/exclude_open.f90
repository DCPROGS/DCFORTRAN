subroutine exclude_open(main,igroup,nset,vigroup1,vigroup2,igroup_editval1,igroup_editval2,&
			igroup_Toggle0,igroup_Toggle1)

use menu_f90

! Widget identifier definitions
integer :: igroup
integer :: igroup_MainPanel
integer :: igroup_Panel1
integer :: igroup_Panel2(10)
integer :: igroup_Panel3(10)
integer :: igroup_Button1
integer :: igroup_Button2
integer :: igroup_Static2
integer :: igroup_Static1
integer :: igroup_Static3
integer :: igroup_Static4

integer :: igroup_Editval1(10)
integer :: igroup_Editval2(10)

integer :: igroup_Toggle0(10)
integer :: igroup_Toggle1(10)

real vigroup1(10)
real vigroup2(10)

 call gmSetGuiGridMode(GOff)
if(nset.eq.1) iwid=300
if(nset.eq.2) iwid=400
if(nset.eq.3) iwid=500
if(nset.eq.4) iwid=600
iwid=300+(nset-1)*100
! Set up master window igroup
   igroup = gmCreateComplexDialogueBox(Main,72, 117, iwid, 330, Gall, ' Exclude open times ', gmIconFormat=GDLLICON, &
      gmIconFile='Gee',gmvpos=gmiddle)

! Create main panel for form
   igroup_MainPanel=gmCreatePanel(igroup, 0, 0, iwid, 330, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)

! Create panel igroup_Panel1 child of igroup_MainPanel
   igroup_Panel1 = gmCreatePanel(igroup_MainPanel, 0, 0,iwid, 330, gmTitle=' ', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=150, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)
! Create text widget igroup_Static2 child of igroup_Panel1
 
   igroup_Static1 = gmCreateTextEntry(igroup_Panel1, 10, 40, 100, 25,'-shut time on BOTH SIDES', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget igroup_Static2 child of igroup_Panel1
   igroup_Static2 = gmCreateTextEntry(igroup_Panel1, 10, 70, 100, 25,'-groups containing ONE opening', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget igroup_Static1 child of igroup_Panel1
   igroup_Static3 = gmCreateTextEntry(igroup_Panel1, 10, 100, 120, 25,'Lower limit (ms) ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget igroup_Static3 child of igroup_Panel1
   igroup_Static4 = gmCreateTextEntry(igroup_Panel1, 10, 130, 120, 25,'Upper limit (ms) ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)


do j=1,nset
! Create panel igroup_Panel3 child of igroup_Panel1
   igroup_Panel2(j) = gmCreatePanel(igroup_Panel1, 190+100*(j-1), 20, 90, 240, gmTitle='SET: '//char(48+j), gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=150, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)
	! Create panel igroup_Panel2 child of igroup_Panel3
! Create toggle button igroup_Toggle4 child of igroup_Panel2
   igroup_Toggle0(j) = gmCreateToggleButton(igroup_Panel2(j), 10, 20, 30, 25, '', 0, &
   gmType=G3DCHECKBOX, gmVpos=GTOP,gmcallback=3170+j)

   igroup_Toggle1(j) = gmCreateToggleButton(igroup_Panel2(j), 10, 50, 30, 25, '', 0, &
   gmType=G3DCHECKBOX, gmVpos=GTOP,gmcallback=3180+j)

  

! Create value entry igroup_Editval1 child of igroup_Panel2
   igroup_Editval1(j) = gmCreateValueEntry(igroup_Panel2(j), 10, 80, 40, 25, vigroup1(j), 8, 1, GEDIT,  &
      gmType=Gdecimal, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
	call gmSetWidgetStatus(igroup_editval1(j), Gunselectable)



! Create value entry igroup_Editval13 child of igroup_Panel3
   igroup_Editval2(j) = gmCreateValueEntry(igroup_Panel2(j), 10, 110, 40, 25, vigroup2(j), 8, 0, GEDIT,  &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
	call gmSetWidgetStatus(igroup_editval2(j), Gunselectable)


enddo
! Create button igroup_Button1 child of igroup_Panel1
   igroup_Button1 = gmCreatePushButton(igroup_Panel1, 40, 280, 75, 25, 'Help', gmVpos=GTOP,&
   gmcallback=3164)

! Create button igroup_Button2 child of igroup_Panel1
   igroup_Button2 = gmCreatePushButton(igroup_Panel1, 130, 280, 75, 25, 'All done', &
   gmVpos=GTOP,gmcallback=3165)


call gmSetGuiGridMode(GOff)
call gmdrawwindow(igroup)
return



end