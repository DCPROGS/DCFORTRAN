subroutine ini_file(main,initwin,nset,itogrec,&
pfiles,nfileb,qmec,title,trate,pfilem,icallid,initwin_Toggle3,initwin_Toggle4,&
initwin_Toggle5,initwin_Toggle6,initwin_Toggle7,initwin_Toggle8,&
initwin_Toggle9,initwin_Toggle10,initwin_Toggle11,initwin_Text2,&
initwin_Toggle_1,initwin_Toggle_2,initwin_TxtArray,initwin_text1)


use menu_f90
!use dflib
!use dfport
character*40 pfilem,qfilem
character*74 trate,title
character*33 pfileb(20,10)
character*60 pfiles(20,10),qmec

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
integer :: initwin_Button6,initwin_Toggle_1(10),initwin_Toggle_2(10)
character*80 text1(10),text2(10)
character*60 texta
logical nodata,nofit,autosim,curvonly,discprt,append,readini
common/iniset/nodata,nofit,autosim,curvonly
common/dp/discprt,append
!character*100  xtext1
character*2 cnum
type (GLIMIT) :: Graphics_frame
type(GARRAYCELL) arrayatr
integer nfileb(10)
do j=1,nset
do i=1,nfileb(j)	
    if(nfileb(j).gt.nmax) nmax=nfileb(j)
enddo
enddo 


text1(1)=' Use the same data (*.scn) files'
text1(2)=' Specify data (*.scn) files'
text1(3)=' Read data from old SCANDAT.DAT disc'
text1(4)=' Simulate and fit one or more scan.scn files'
text1(5)=' Plot existing simulated fit results'
text1(6)=' Read data from an AXON (*.EVL) file'
text1(7)=' Read data from a Dempster (*.EDE) file'

text2(1)= ' Use same again'

text2(2)=' Modify this mechanism '
text2(4)=' Read mechanism already defined'
text2(3)=' Create new mechanism '

if(itogrec.eq.2.or.itogrec.eq.7) then
	ihwin=380
	iwwin=841
	ih1=290

else
	ihwin=560
	iwwin=841
	ih1=470
endif
	call gDefineRGB(171, 0.6, 0.99, 0.99)
	call gDefineRGB(172, 0.44, 0.73, 0.73)
    call gDefineRGB(161,0.80,1.,0.8)
	 call gDefineRGB(162,0.80,1.,0.8)
	 call gDefineRGB(163,0.44,0.87,0.44)
	 call gDefineRGB(38,1.,0.5,0.25)	!orange
call gmsetguigridmode(goff)

	if(nmax.eq.1) then
		
			texta='file 1'
	else if(nmax.eq.2) then
			texta='file 1'//char(124)//'file 2'
	else if(nmax.eq.3) then
			texta='file 1'//char(124)//'file 2'//char(124)//'file 3'
	
	endif

! Set up complex dialogue box initwin child of imecform1
initwin = gmCreateComplexDialogueBox(Main,100,100,iwwin,ihwin, gminimize, 'Data and Mechanism ', &
              	gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='MBIG1052')

! Create main panel for form
initwin_MainPanel=gmCreatePanel(initwin, 0, 0, iwwin, ihwin, gmHpos=GCENTRE, gmVpos=Gtop, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillCol=150, gmFillBorder=GOUTEREDGE)

initwin_Panel4 = gmCreatePanel(initwin_MainPanel, 0, 0, 420, ih1, gmTitle=' ', gmType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=163, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)
! goto 12
if(itogrec.ne.2.and.itogrec.ne.7) then
nmax1=nmax

!if(nmax1.eq.1) nmax1=nmax+1
nset1=nset
nd=40+30*nset1
initwin_TxtArray = gmCreateTextArray(initwin_Panel4, 20, 20, 380, nd, nmax1, 10, gmAxisW=30, gmAxisH=25, &
	gmXtext=texta,gmYtext='*digits', gmXjust=Gcentre, gmYjust=GCENTRE, &
	gmScrollMode=GBOTHBARS, gmhpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
 
   call gmEnqCellAttribs(initwin_TxtArray, 1, 1, arrayatr)
  
   arrayatr%width=200
  
   arrayatr%height=22
   arrayatr%display=gdisplay
	 
	  do i=1,2
	   arrayatr%backcol=0
	 do j=1,10
	    
	   
		call gmSetCellAttribs(initwin_TxtArray, i, j, arrayatr)
     enddo
   enddo
   do j=1,nset
				do i=1,nfileb(j)	
				call gmSetCellSetting(initwin_TxtArray, i,j ,gmString=pfiles(i,j))
				enddo
  enddo

endif
12 continue
init=1
ino=0
ihe=230
ih2=220
ih3=350

if(itogrec.eq.2.or.itogrec.eq.7) then
init=2
ino=1
ihe=210
ih2=40
ih3=190
endif
 if(init.eq.1) text1(2)=' Specify different data (*.scn) files'
initwin_Radio4 = gmCreateRadioBox(initwin_Panel4, 20, ih2, 380, ihe, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=161, gmFillBorder=GOUTEREDGE, gmTitle=' ', gmVpos=GTOP)

! Create toggle button initwin_Toggle9 child of initwin_Radio4
!icall=-28,-29
do i=init,7
	initwin_Toggle_1(i) = gmCreateToggleButton(initwin_Radio4, 20, 30*(i-ino)-20, 330, 25, text1(i), 0, &
      gmType=G3DRADIO, gmVpos=GTOP)
	  
enddo
initwin_Panel1 = gmCreatePanel(initwin_MainPanel,420, 0, 420, ih3, gmTitle='', gmType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=172, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

if(itogrec.ne.2.and.itogrec.ne.7) then

   
	initText = gmCreateTextEntry(initwin_Panel1, 20, 20, 360, 25,'Mechanism:'//title, 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 , gmVpos=GTOP)


	initText = gmCreateTextEntry(initwin_Panel1, 20, 50, 360, 25,'Rate:'//trate, 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 , gmVpos=GTOP)


initText = gmCreateTextEntry(initwin_Panel1, 20, 80, 100, 25,'Mechanism file', 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 , gmVpos=GTOP)
	initwin_Text2 = gmCreateTextEntry(initwin_Panel1, 160, 80, 240, 25,qmec, 255, gedit, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 , gmVpos=GTOP)

!	initText = gmCreateTextEntry(initwin_Panel1, 20, 110, 120, 25,'Path:(e.g. D:\hjcfit\ )', 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
 !     gmTextCol=1 , gmVpos=GTOP)
!	initwin_Text2 = gmCreateTextEntry(initwin_Panel1, 140, 110, 180, 25,textm, 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
 !     gmTextCol=1 , gmVpos=GTOP)
!	  initwin_Button_m = gmCreatePushButton(initwin_Panel1, 330, 110, 80, 25, 'Browse', &
 !   gmVpos=Gtop,gmType=GDEFAULTBUTTON,gmcallback=-237) !-40)
endif

init=1
ino=0
ihe=135
ih4=160
if(itogrec.eq.2.or.itogrec.eq.7) then
init=3
ino=2
ihe=100
ih4=20
endif

initwin_Radio5 = gmCreateRadioBox(initwin_Panel1, 20,ih4, 380, ihe, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=171, gmFillBorder=GOUTEREDGE, gmTitle=' ', gmVpos=GTOP)
if(itogrec.eq.2.or.itogrec.eq.7) then   
	initwin_Toggle_2(3) = gmCreateToggleButton(initwin_Radio5, 40, 10, 290, 25, text2(3), 0, &
      gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-228)

	initwin_Toggle_2(4) = gmCreateToggleButton(initwin_Radio5, 40, 40, 290, 25, text2(4), 1, &
      gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-228)
else
    initwin_Toggle_2(1) = gmCreateToggleButton(initwin_Radio5, 40, 10, 290, 25, text2(1), 1, &
      gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-227)

	initwin_Toggle_2(2) = gmCreateToggleButton(initwin_Radio5, 40, 40, 290, 25, text2(2), 0, &
      gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-229)
	initwin_Toggle_2(3) = gmCreateToggleButton(initwin_Radio5, 40, 70, 290, 25, text2(3), 0, &
      gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-228)
	  initwin_Toggle_2(4) = gmCreateToggleButton(initwin_Radio5, 40, 100, 290, 25, text2(4), 0, &
      gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-228)
endif


ih5=150
if(itogrec.eq.2.or.itogrec.eq.7) ih5=130
!goto 99
initwin_Radio3 = gmCreateRadioBox(initwin_MainPanel,420, ih3, 420, ih5, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=152, gmFillBorder=GOUTEREDGE, gmTitle='Options', gmVpos=GTOP)

if(curvonly) then
 initwin_Toggle6 = gmCreateToggleButton(initwin_Radio3, 40, 40, 290, 25, ' (1) Fit data', 0, &
   gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-25)

! Create toggle button initwin_Toggle7 child of initwin_Radio3
!!   initwin_Toggle7 = gmCreateToggleButton(initwin_Radio3, 40, 60, 270, 25, 'Show data and HJC distributions', 0, &
 !!     gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-26)

! Create toggle button initwin_Toggle8 child of initwin_Radio3
   initwin_Toggle8 = gmCreateToggleButton(initwin_Radio3, 40, 70, 290, 25, ' (2) Show only curves for specified model  (no data)', 1, gmType=G3DRADIO, &
      gmVpos=GTOP,gmcallback=-27)
else
	if(nofit) then
! Create toggle button initwin_Toggle6 child of initwin_Radio3
   initwin_Toggle6 = gmCreateToggleButton(initwin_Radio3, 40, 40, 290, 25, ' (1) Fit data', 1, &
   gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-25)

! Create toggle button initwin_Toggle7 child of initwin_Radio3
 !!  initwin_Toggle7 = gmCreateToggleButton(initwin_Radio3, 40, 60, 270, 25, 'Show data and HJC distributions', 1, &
  !!    gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-26)

! Create toggle button initwin_Toggle8 child of initwin_Radio3
   initwin_Toggle8 = gmCreateToggleButton(initwin_Radio3, 40, 70, 290, 25, ' (2) Show only curves for specified model  (no data)', 0, gmType=G3DRADIO, &
      gmVpos=GTOP,gmcallback=-27)
	  ! Create toggle button initwin_Toggle8 child of initwin_Radio3
	else
initwin_Toggle6 = gmCreateToggleButton(initwin_Radio3, 40, 40, 290, 25, ' (1) Fit data', 1, &
   gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-25)

! Create toggle button initwin_Toggle7 child of initwin_Radio3
 !!  initwin_Toggle7 = gmCreateToggleButton(initwin_Radio3, 40, 60, 270, 25, 'Show data and HJC distributions', 0, &
  !!    gmType=G3DRADIO, gmVpos=GTOP,gmcallback=-26)

! Create toggle button initwin_Toggle8 child of initwin_Radio3
   initwin_Toggle8 = gmCreateToggleButton(initwin_Radio3, 40, 70, 290, 25, ' (2) Show only curves for specified model  (no data)', 0, gmType=G3DRADIO, &
      gmVpos=GTOP,gmcallback=-27)
	endif
 endif  
 99 continue 

	initwin_Button8 = gmCreatePushButton(initwin_MainPanel, 10, 20, 195, 25,&
    'Save to ini file', gmVpos=Gbottom,gmcallback=8004)

    initwin_Button5 = gmCreatePushButton(initwin_MainPanel, 220, 20, 195, 25,&
    'Advanced settings', gmVpos=Gbottom,gmcallback=-5)
   
    initwin_Button6 = gmCreatePushButton(initwin_MainPanel, 430, 20, 400, 25, 'Continue', &
    gmVpos=Gbottom,gmType=Guserdefined,gmOffcol=38,gmcallback=icallid) !-40)

 
   call gmSetGuiGridMode(GOn)
   call gmdrawwindow(initwin)
  
   	do i=2,7
	call gmSetWidgetStatus(initwin_toggle_1(i), GunSELECTABLE)
	enddo
	if(itogrec.eq.2) then
		call gmSetWidgetStatus(initwin_toggle_1(4), GSELECTABLE)
		call gmSetWidgetStatus(initwin_toggle_1(2), GSELECTABLE)
		call gmSetWidgetStatus(initwin_toggle_1(5), GSELECTABLE)
	else if(itogrec.eq.7) then
		call gmSetWidgetStatus(initwin_toggle_1(4), GSELECTABLE)
		call gmSetWidgetStatus(initwin_toggle_1(5), GSELECTABLE)
	else
   		if(autosim) then
			call gmSetWidgetStatus(initwin_toggle_1(4), GSELECTABLE)

		else
			call gmSetWidgetStatus(initwin_toggle_1(2), GSELECTABLE)
		endif
	endif
return
end