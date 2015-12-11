subroutine ini_file(main,initwin,initwin_Toggle3,initwin_Toggle4,&
initwin_Toggle5,initwin_Toggle6,initwin_Toggle7,initwin_Toggle8,&
initwin_Toggle9,initwin_Toggle10,initwin_Toggle11,pfilem,icallid,initwin_Text2)


use menu_f90
character*40 pfilem

! Widget identifier definitions

integer :: initwin
integer :: initwin_MainPanel
integer :: initwin_Panel1
integer :: initwin_Text1
integer :: initwin_Button1
integer :: initwin_Button2
integer :: initwin_Radio1
integer :: initwin_Toggle1
integer :: initwin_Toggle2
integer :: initwin_Panel2
integer :: initwin_Text2
integer :: initwin_Button3
integer :: initwin_Button4
integer :: initwin_Radio2
integer :: initwin_Toggle3
integer :: initwin_Toggle4
integer :: initwin_Toggle5
integer :: initwin_Radio3
integer :: initwin_Toggle6
integer :: initwin_Toggle7
integer :: initwin_Toggle8
integer :: initwin_Radio4
integer :: initwin_Toggle9
integer :: initwin_Toggle10
integer :: initwin_Button5
integer :: initwin_Button6
logical nodata,nofit,autosim,curvonly,discprt,append
common/iniset/nodata,nofit,autosim,curvonly
common/dp/discprt,append

call gmsetguigridmode(goff)



ihwin=480
ihwin=450
if(initwin_Toggle11.eq.-1) ihwin=260

! Set up complex dialogue box initwin child of imecform1
 initwin = gmCreateComplexDialogueBox(Main,100,150,421,ihwin, GALL, 'Initial Settings ', &
              	gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='MBIG1052')

! Create main panel for form
   initwin_MainPanel=gmCreatePanel(initwin, 0, 0, 421, ihwin-30, gmHpos=GCENTRE, gmVpos=Gtop, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillCol=141, gmFillBorder=GOUTEREDGE)

 !Create panel initwin_Panel2 child of initwin_MainPanel
   initwin_Panel2 = gmCreatePanel(initwin_MainPanel, 0, 0, 420, 190, gmTitle='Printout file', gmType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=140, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget initwin_Text2 child of initwin_Panel2
   initwin_Text2 = gmCreateTextEntry(initwin_Panel2, 20, 30, 200, 25,pfilem, 255, GEDIT, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create button initwin_Button3 child of initwin_Panel2
   initwin_Button3 = gmCreatePushButton(initwin_Panel2, 240, 30, 75, 25, 'Browse', &
   gmVpos=GTOP,gmcallback=-35)
call gmSetWidgetStatus(initwin_button3, GSELECTABLE)
! Create button initwin_Button4 child of initwin_Panel2
   initwin_Button4 = gmCreatePushButton(initwin_Panel2, 330, 30, 75, 25, 'View', &
   gmVpos=GTOP,gmcallback=-34)
call gmSetWidgetStatus(initwin_button4, GSELECTABLE)
! Create radio box child initwin_Radio2 child of initwin_Panel2
   initwin_Radio2 = gmCreateRadioBox(initwin_Panel2, 20, 70, 380, 110, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=141, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)
if(discprt) then
if(append) then
! Create toggle button initwin_Toggle3 child of initwin_Radio2
   initwin_Toggle3 = gmCreateToggleButton(initwin_Radio2, 70, 10, 230, 25, 'Append to existing print file', 1, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initwin_Toggle4 child of initwin_Radio2
   initwin_Toggle4 = gmCreateToggleButton(initwin_Radio2, 70, 40, 230, 25, 'Overwrite/new printout file', 0, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initwin_Toggle5 child of initwin_Radio2
   initwin_Toggle5 = gmCreateToggleButton(initwin_Radio2, 70, 70, 230, 25, 'No printout file', 0, gmType=G3DRADIO, gmVpos=GTOP)
else
initwin_Toggle3 = gmCreateToggleButton(initwin_Radio2, 70, 10, 230, 25, 'Append to existing print file', 0, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initwin_Toggle4 child of initwin_Radio2
   initwin_Toggle4 = gmCreateToggleButton(initwin_Radio2, 70, 40, 230, 25, 'Overwrite/new printout file', 1, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initwin_Toggle5 child of initwin_Radio2
   initwin_Toggle5 = gmCreateToggleButton(initwin_Radio2, 70, 70, 230, 25, 'No printout file', 0, gmType=G3DRADIO, gmVpos=GTOP)
endif
else
initwin_Toggle3 = gmCreateToggleButton(initwin_Radio2, 70, 10, 230, 25, 'Append to existing print file', 0, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initwin_Toggle4 child of initwin_Radio2
   initwin_Toggle4 = gmCreateToggleButton(initwin_Radio2, 70, 40, 230, 25, 'Overwrite/new printout file', 0, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initwin_Toggle5 child of initwin_Radio2
   initwin_Toggle5 = gmCreateToggleButton(initwin_Radio2, 70, 70, 230, 25, 'No printout file', 1, gmType=G3DRADIO, gmVpos=GTOP)
endif
if(initwin_Toggle11.eq.-1) goto 3 
! Create radio box child initwin_Radio3 child of initwin_MainPanel
   initwin_Radio3 = gmCreateRadioBox(initwin_MainPanel, 0, 190, 420, 95, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=140, gmFillBorder=GOUTEREDGE, gmTitle='Options', gmVpos=GTOP)
if(curvonly) then
 initwin_Toggle6 = gmCreateToggleButton(initwin_Radio3, 40, 30, 290, 25, 'Fit data', 0, &
   gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-25)

! Create toggle button initwin_Toggle7 child of initwin_Radio3
!!   initwin_Toggle7 = gmCreateToggleButton(initwin_Radio3, 40, 60, 270, 25, 'Show data and HJC distributions', 0, &
 !!     gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-26)

! Create toggle button initwin_Toggle8 child of initwin_Radio3
   initwin_Toggle8 = gmCreateToggleButton(initwin_Radio3, 40, 60, 290, 25, 'Show only curves for specified model', 1, gmType=G3DRADIO, &
      gmVpos=GTOP,gmcallback=-27)
else
	if(nofit) then
! Create toggle button initwin_Toggle6 child of initwin_Radio3
   initwin_Toggle6 = gmCreateToggleButton(initwin_Radio3, 40, 30, 290, 25, 'Fit data', 1, &
   gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-25)

! Create toggle button initwin_Toggle7 child of initwin_Radio3
 !!  initwin_Toggle7 = gmCreateToggleButton(initwin_Radio3, 40, 60, 270, 25, 'Show data and HJC distributions', 1, &
  !!    gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-26)

! Create toggle button initwin_Toggle8 child of initwin_Radio3
   initwin_Toggle8 = gmCreateToggleButton(initwin_Radio3, 40, 60, 290, 25, 'Show only curves for specified model', 0, gmType=G3DRADIO, &
      gmVpos=GTOP,gmcallback=-27)
	  ! Create toggle button initwin_Toggle8 child of initwin_Radio3
	else
initwin_Toggle6 = gmCreateToggleButton(initwin_Radio3, 40, 30, 290, 25, 'Fit data', 1, &
   gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-25)

! Create toggle button initwin_Toggle7 child of initwin_Radio3
 !!  initwin_Toggle7 = gmCreateToggleButton(initwin_Radio3, 40, 60, 270, 25, 'Show data and HJC distributions', 0, &
  !!    gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-26)

! Create toggle button initwin_Toggle8 child of initwin_Radio3
   initwin_Toggle8 = gmCreateToggleButton(initwin_Radio3, 40, 60, 290, 25, 'Show only curves for specified model', 0, gmType=G3DRADIO, &
      gmVpos=GTOP,gmcallback=-27)
	endif
 endif  

! Create radio box child initwin_Radio4 child of initwin_MainPanel
   initwin_Radio4 = gmCreateRadioBox(initwin_MainPanel, 0, 290, 420, 100, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=140, gmFillBorder=GOUTEREDGE, gmTitle='Data type', gmVpos=GTOP)

! Create toggle button initwin_Toggle9 child of initwin_Radio4

if(autosim) then
initwin_Toggle9 = gmCreateToggleButton(initwin_Radio4, 40, 30, 290, 25, 'Use experimental data (scn files)', 0, &
      gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-28)

! Create toggle button initwin_Toggle10 child of initwin_Radio4
   initwin_Toggle10 = gmCreateToggleButton(initwin_Radio4, 40, 70, 290, 25, &
   'Use simulated data', 1, gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-29)
else
   initwin_Toggle9 = gmCreateToggleButton(initwin_Radio4, 40, 30, 290, 25, 'Use experimental data (scn files)', 1, &
      gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-28)

! Create toggle button initwin_Toggle10 child of initwin_Radio4
   initwin_Toggle10 = gmCreateToggleButton(initwin_Radio4, 40, 70, 290, 25, &
   'Use simulated data', 0, gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-29)
endif
3 continue
initwin_Button8 = gmCreatePushButton(initwin_MainPanel, 210, 3, 200, 25,&
    'Save as default', gmVpos=Gbottom,gmcallback=-4)
call gmSetWidgetStatus(initwin_button8, GSELECTABLE)
! Create button initwin_Button5 child of initwin_MainPanel
   initwin_Button5 = gmCreatePushButton(initwin_MainPanel, 5, 3, 200, 25,&
    'Advanced settings', gmVpos=Gbottom,gmcallback=-5)
call gmSetWidgetStatus(initwin_button5, GSELECTABLE)
! Create button initwin_Button6 child of initwin_MainPanel
   initwin_Button6 = gmCreatePushButton(initwin, 10, 0, 400, 25, 'Continue', &
   gmVpos=Gbottom,gmType=GDEFAULTBUTTON,gmcallback=icallid) !-40)
  call gmSetWidgetStatus(initwin_button6, GSELECTABLE)
  ! call gmSetWidgetStatus(initwin_Button6, GCHECKED)
   call gmSetGuiGridMode(GOn)
   call gmdrawwindow(initwin)
return
end

