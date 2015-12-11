subroutine ini_file(main,initwin,initwin_Toggle3,initwin_Toggle4,&
initwin_Toggle5,initwin_Toggle6,initwin_Toggle7,initwin_Toggle8,&
initwin_Toggle9,initwin_Toggle10,initwin_Toggle11,pfilem,icallid,initwin_Text2,&
initwin_Toggle_1,initwin_Toggle_2,initwin_TxtArray,initwin_text,nset,itogrec,text)


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
integer :: initwin_Button6,initwin_Toggle_1(5),initwin_Toggle_2(5)
character*80 text1(5),text2(5)
character*60 text
logical nodata,nofit,autosim,curvonly,discprt,append,readini
common/iniset/nodata,nofit,autosim,curvonly
common/dp/discprt,append

type (GLIMIT) :: Graphics_frame
type(GARRAYCELL) arrayattribss
integer nfileb(10)

!character*100  xtext1
character*2 cnum
text1(1)=' (1) Read data from *.SCN file (from SCAN)'
text1(3)=' (3) Simulate a one or more scan.scn file'
text1(2)=' (2) Read data from old SCANDAT.DAT disc'
text1(4)=' (4) Read data from a Dempster (*.EDE) file'
text1(5)=' (5) Read data from an AXON (*.EVL) file'

text2(1)= ' (1) Use same experiments again'
nl=nblank1(text)
text2(3)=' (3) Change path only '
text2(2)=' (2) Use different experiments'

if(itogrec.eq.2) then
	ihwin=530
	iwwin=421
else if(itogrec.eq.3) then
	ihwin=270
	iwwin=421
else
	ihwin=485
	iwwin=841
endif

call gmsetguigridmode(goff)



! Set up complex dialogue box initwin child of imecform1
 initwin = gmCreateComplexDialogueBox(Main,100,100,iwwin,ihwin, GALL, 'Initial Settings ', &
              	gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='MBIG1052')

! Create main panel for form
   initwin_MainPanel=gmCreatePanel(initwin, 0, 0, iwwin, ihwin, gmHpos=GCENTRE, gmVpos=Gtop, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillCol=141, gmFillBorder=GOUTEREDGE)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
   initwin_Toggle3 = gmCreateToggleButton(initwin_Radio2, 70, 10, 230, 25, ' (1) Append to existing print file', 1, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initwin_Toggle4 child of initwin_Radio2
   initwin_Toggle4 = gmCreateToggleButton(initwin_Radio2, 70, 40, 230, 25, ' (2) Overwrite/new printout file', 0, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initwin_Toggle5 child of initwin_Radio2
   initwin_Toggle5 = gmCreateToggleButton(initwin_Radio2, 70, 70, 230, 25, ' (3) No printout file', 0, gmType=G3DRADIO, gmVpos=GTOP)
else
initwin_Toggle3 = gmCreateToggleButton(initwin_Radio2, 70, 10, 230, 25, ' (1) Append to existing print file', 0, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initwin_Toggle4 child of initwin_Radio2
   initwin_Toggle4 = gmCreateToggleButton(initwin_Radio2, 70, 40, 230, 25, ' (2) Overwrite/new printout file', 1, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initwin_Toggle5 child of initwin_Radio2
   initwin_Toggle5 = gmCreateToggleButton(initwin_Radio2, 70, 70, 230, 25, ' (3) No printout file', 0, gmType=G3DRADIO, gmVpos=GTOP)
endif
else
initwin_Toggle3 = gmCreateToggleButton(initwin_Radio2, 70, 10, 230, 25, ' (1) Append to existing print file', 0, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initwin_Toggle4 child of initwin_Radio2
   initwin_Toggle4 = gmCreateToggleButton(initwin_Radio2, 70, 40, 230, 25, ' (2) Overwrite/new printout file', 0, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initwin_Toggle5 child of initwin_Radio2
   initwin_Toggle5 = gmCreateToggleButton(initwin_Radio2, 70, 70, 230, 25, ' (3) No printout file', 1, gmType=G3DRADIO, gmVpos=GTOP)
endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

if(itogrec.ne.3) then

! Create radio box child initwin_Radio3 child of initwin_MainPanel
initwin_Radio3 = gmCreateRadioBox(initwin_MainPanel, 0, 190, 420, 95, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=140, gmFillBorder=GOUTEREDGE, gmTitle='Options', gmVpos=GTOP)
if(curvonly) then
 initwin_Toggle6 = gmCreateToggleButton(initwin_Radio3, 40, 30, 290, 25, ' (1) Fit data', 0, &
   gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-25)

! Create toggle button initwin_Toggle7 child of initwin_Radio3
!!   initwin_Toggle7 = gmCreateToggleButton(initwin_Radio3, 40, 60, 270, 25, 'Show data and HJC distributions', 0, &
 !!     gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-26)

! Create toggle button initwin_Toggle8 child of initwin_Radio3
   initwin_Toggle8 = gmCreateToggleButton(initwin_Radio3, 40, 60, 290, 25, ' (2) Show only curves for specified model', 1, gmType=G3DRADIO, &
      gmVpos=GTOP,gmcallback=-27)
else
	if(nofit) then
! Create toggle button initwin_Toggle6 child of initwin_Radio3
   initwin_Toggle6 = gmCreateToggleButton(initwin_Radio3, 40, 30, 290, 25, ' (1) Fit data', 1, &
   gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-25)

! Create toggle button initwin_Toggle7 child of initwin_Radio3
 !!  initwin_Toggle7 = gmCreateToggleButton(initwin_Radio3, 40, 60, 270, 25, 'Show data and HJC distributions', 1, &
  !!    gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-26)

! Create toggle button initwin_Toggle8 child of initwin_Radio3
   initwin_Toggle8 = gmCreateToggleButton(initwin_Radio3, 40, 60, 290, 25, ' (2) Show only curves for specified model', 0, gmType=G3DRADIO, &
      gmVpos=GTOP,gmcallback=-27)
	  ! Create toggle button initwin_Toggle8 child of initwin_Radio3
	else
initwin_Toggle6 = gmCreateToggleButton(initwin_Radio3, 40, 30, 290, 25, ' (1) Fit data', 1, &
   gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-25)

! Create toggle button initwin_Toggle7 child of initwin_Radio3
 !!  initwin_Toggle7 = gmCreateToggleButton(initwin_Radio3, 40, 60, 270, 25, 'Show data and HJC distributions', 0, &
  !!    gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-26)

! Create toggle button initwin_Toggle8 child of initwin_Radio3
   initwin_Toggle8 = gmCreateToggleButton(initwin_Radio3, 40, 60, 290, 25, ' (2) Show only curves for specified model', 0, gmType=G3DRADIO, &
      gmVpos=GTOP,gmcallback=-27)
	endif
 endif  

! Create radio box child initwin_Radio4 child of initwin_MainPanel
initwin_Radio4 = gmCreateRadioBox(initwin_MainPanel, 0, 285, 420, 195, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=140, gmFillBorder=GOUTEREDGE, gmTitle='Data type', gmVpos=GTOP)

! Create toggle button initwin_Toggle9 child of initwin_Radio4
!icall=-28,-29
do i=1,5
	initwin_Toggle_1(i) = gmCreateToggleButton(initwin_Radio4, 40, 30*i, 290, 25, text1(i), 0, &
      gmType=G3DRADIO, gmVpos=GTOP)
	  if(itogrec.eq.4.or.itogrec.eq.5.and.i.gt.1) &
	  call gmSetWidgetStatus(initwin_Toggle_1(i), GunSELECTABLE)
enddo

endif




if(itogrec.ne.2.and.itogrec.ne.3) then

   initwin_Panel4 = gmCreatePanel(initwin_MainPanel, 420, 0, 420, 350, gmTitle='Settings', gmType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=140, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

  

   
   initwin_Radio5 = gmCreateRadioBox(initwin_Panel4, 20,30, 380, 110, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=141, gmFillBorder=GOUTEREDGE, gmTitle=' ', gmVpos=GTOP)
   
	initwin_Toggle_2(1) = gmCreateToggleButton(initwin_Radio5, 40, 10, 290, 25, text2(1), 1, &
      gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-227)

	initwin_Toggle_2(2) = gmCreateToggleButton(initwin_Radio5, 40, 40, 290, 25, text2(2), 0, &
      gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-228)

	initwin_Toggle_2(3) = gmCreateToggleButton(initwin_Radio5, 40, 70, 290, 25, text2(3), 0, &
      gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-229)

if(itogrec.eq.4.or.itogrec.eq.5) then
	  call gmSetWidgetStatus(initwin_Toggle_2(2), GunSELECTABLE)
endif
	initText = gmCreateTextEntry(initwin_Panel4, 20, 160, 50, 25,'Path:', 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 , gmVpos=GTOP)
	initwin_Text = gmCreateTextEntry(initwin_Panel4, 80, 160, 300, 25,text, 255, gedit, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 , gmVpos=GTOP)
    initwin_TxtArray = gmCreateTextArray(initwin_Panel4, 10, 190, 400, 140, 2, 10, gmAxisW=30, gmAxisH=25, &
	  gmXtext='File 1'//char(124)//'File 2', &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmVpos=GTOP)
   call gmEnqCellAttribs(initwin_TxtArray, 1, 1, arrayattribss)
  
   arrayattribss%width=200
  
     arrayattribss%height=22
	 arrayattribss%display=gdisplay
	 
	  do i=1,2
	   arrayattribss%backcol=0
	 do j=1,10
	    
	   
		call gmSetCellAttribs(initwin_TxtArray, i, j, arrayattribss)
     enddo
   enddo

	initwin_Button8 = gmCreatePushButton(initwin_MainPanel, 630, 50, 195, 25,&
    'Save as default', gmVpos=Gbottom,gmcallback=-4)

    initwin_Button5 = gmCreatePushButton(initwin_MainPanel, 430, 50, 195, 25,&
    'Advanced settings', gmVpos=Gbottom,gmcallback=-5)
   
    initwin_Button6 = gmCreatePushButton(initwin_MainPanel, 430, 20, 400, 25, 'Continue', &
    gmVpos=Gbottom,gmType=GDEFAULTBUTTON,gmcallback=icallid) !-40)

else


	initwin_Button8 = gmCreatePushButton(initwin_MainPanel, 215, 30, 195, 25,&
    'Save as default', gmVpos=Gbottom,gmcallback=-4)

    initwin_Button5 = gmCreatePushButton(initwin_MainPanel, 10, 30, 195, 25,&
    'Advanced settings', gmVpos=Gbottom,gmcallback=-5)
   
    initwin_Button6 = gmCreatePushButton(initwin_MainPanel, 10, 0, 400, 25, 'Continue', &
    gmVpos=Gbottom,gmType=GDEFAULTBUTTON,gmcallback=icallid) !-40)
 
endif
call gmSetWidgetStatus(initwin_button8, GSELECTABLE)
call gmSetWidgetStatus(initwin_button5, GSELECTABLE)
call gmSetWidgetStatus(initwin_button6, GSELECTABLE)
  


   call gmSetGuiGridMode(GOn)
   call gmdrawwindow(initwin)

call gmSetWidgetStatus(initwin_text, GunSELECTABLE)

return
end

