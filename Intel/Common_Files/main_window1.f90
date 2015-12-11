!     Last change:  D    15 Jan 104    4:41 pm
subroutine main_window(program_type,Main,imainpanel,cDATEW,modelw,eqfit,Status_bar1)
		


!***********************************************
!	CALLBACKS:

!   
!	-100 TO -10 INITIALIZATION
!	-1	EXIT

!	1:1000 - ATTRIBUTES ( LIKE IN VPLOT AND VHIST)
!---------------------------------------------------------

!   1:500 2d attributes
!   501-800 3D ATTRIBUTES
!	801-850  SAVE IMAGE
!	851-900  SAVE FILE ON DISC /EXPORT (PLOTQ)
!	901-950  PRINT
!	951-1000  HELP
!---------------------------------------------------------	
!   1001-2000	   MECHANISMS
!	2001-2300	   constraints
!	2301-2400      move mouse
!   2401-2500      select mouse
!---------------------------------------------------------

!	3001- 3300 READING HJCFIT FILES (SCN)
!	3301- 3500 simulate data
!   3501- 4000 stability plots

!	4001- 5000 FITTING FOR HJCFIT
!----------------------------------------------------------
!	5001- 6000 READING AND DRAWING AUTPLOT AND CVFIT FILES
!	6001- 7000 FITTING FOR CVFIT
!----------------------------------------------------------
!   7001-7100 graph callback
!	7100-7200 graph move
!   7201-7300 graph select
!----------------------------------------------------------- 

! Using GINOMENU Version 2.0c
use menu_f90
integer program_type
! Widget identifier definitions
integer ::  Main
integer ::  MainMenubar
integer ::  File
integer ::  MainPop1_1
integer ::  NEW_file
integer ::  Open_file
integer ::  Import_file
integer ::  Save_file
integer ::  Export_file
integer ::  Print_file
integer ::  Exit_file
integer ::  Edit
integer ::  MainPop1_2
integer ::  Title_record
integer ::  Parameters
integer ::  Labels
integer ::  Label_x
integer ::  Label_y
integer ::  Label_z
integer ::  Number_x
integer ::  Number_y
integer ::  Number_z
integer ::  Numbers
integer ::  Text_record

integer ::  Viewer
integer ::  MainPop1_3
integer ::  view_Record
integer ::  view_Data
integer ::  view_Equations
integer ::  view_Another
integer ::  Select
integer ::  MainPop1_4
integer ::  Axis
integer ::  Frame
integer ::  Lines
integer ::  Arrows
integer :: raw_data
integer :: icurves

integer ::  Image
integer ::  MainPop1_5
integer ::  iZoom
integer ::  Interpolate
integer ::  Help
integer ::  MainPop1_6
integer ::  MainPop1_7
integer ::  MainPop1_8
integer ::  MainPop1_9
integer ::  MainPop1_10
integer ::  MainPop1_11
integer ::  MainPop1_12
integer ::  MainPop1_13
integer ::  Studio
integer ::  About
integer :: MainPanel,toggle_main(20)
integer :: Panel1_1
integer :: Panel1_2
integer :: Icon1_1
integer :: Icon1_2
integer :: Icon1_3
integer :: Icon1_4
integer :: Icon1_5
integer :: Icon1_6
integer :: Icon1_7
integer :: Icon1_8
integer :: Icon1_9
integer :: Icon1_10
integer :: Icon1_11
integer :: Icon1_12
integer :: Icon1_13
integer :: Icon1_14
integer :: Icon1_15
integer :: Icon1_16
integer :: Icon1_17
integer :: Icon1_18
integer :: Icon1_19
integer :: Icon1_20 
integer :: Icon1_21
integer :: Icon1_22
integer :: Icon1_23
integer :: Icon1_24
integer :: Icon1_25
integer :: Icon1_26
integer :: Icon1_27
integer :: Icon1_28
integer :: Icon1_29
 
integer :: Combo1_1,Combo1_11,combo1_12
integer :: Combo1_2
integer :: Combo1_3
integer :: Combo1_4
integer :: Combo1_5
integer :: Combo1_6
integer :: Combo1_8
integer :: Combo1_9
integer :: Combo1_10
integer :: toolbar1_1
integer :: toolbar1_2
integer :: toolbar1_3
integer :: toolbar1_4
integer :: toolbar1_5
integer :: separator1_1
integer :: separator1_2
integer :: separator1_3
integer :: Static1_1
integer :: Status_bar1
integer :: Fit
integer :: eqfit(10),modelw(20)
character*100 titleprogram
character cDATEW*11,qDATEW*9
INTEGER :: ipans(5)
DATA ipans/50,170,230,290,-1/
character*20 toggle_text(20)
logical nodata,nofit,autosim,curvonly

common/plotatr/new_file,open_file,import_file,izoomy, &
           isave_bmp,isave_wmf,export_file,print_file,exit_file,view_record,&
		   view_data,title_record,iparameters,labels,&
		   jtitle_record,jparameters,jlabels,jnewtext,jnumbers,jaxis,jframe,&
		   jlines,jarrows,jraw_data,jcurves,label_x,label_y, &
		   label_z,number_x,number_y,number_z,Icon1_1,Icon1_2,Icon1_3,Icon1_4,&
		   Icon1_5,Icon1_6,Icon1_7,Icon1_8,Icon1_9,Icon1_10,Icon1_11,Icon1_12,&
		   Combo1_1,Combo1_2,Combo1_3,Combo1_4,Combo1_5,Combo1_6, combo1_8,combo1_9,combo1_10,&
		   toolbar1_1,toolbar1_2,toolbar1_3,Toolbar1_4,&
		   new_text,new_lines,new_arrows,new_lines_v,new_lines_h,&
		   i3d,irot,iview3d,iaratxy,iarathb,igridd,isurfdr,icross,ifill3d,imarkbad
	common/iniset/nodata,nofit,autosim,curvonly
	
!common/widgets/
call gSetEscapeChar('£')
toggle_text(1)='Model'
toggle_text(2)='File'
toggle_text(3)='Risetime'
toggle_text(4)='Resolution'
ixpos=1
iypos=3
iwidth=40
iheight=32

	if(program_type.eq.-1) then
	titleprogram='Data Analysis Studio (Copyright 2003 D. Colquhoun & I. Vais)'
else if(program_type.eq.1) then
	titleprogram='Curve Fitting'
else if(program_type.eq.2) then
	titleprogram='Automatic printing from plot queues (Copyright 2003 D. Colquhoun & I. Vais)'
else if(program_type.eq.3) then
	titleprogram='Maximum likelihood fit to open-shut time sequence by HJC method'
else if(program_type.eq.4) then
	titleprogram='Ekdist'
	endif
! Set up master window Main
   Main = gmCreateMasterWindow(ixpos, iypos, iwidth, iheight, GALL,titleprogram , &
              	gmmaxwidth=iwidth,gmmaxheight=iheight&
				,gmVpos=GTOP, gmIconFormat=GDLLICON,gmIconFile='MBIG1068')
			
! Create menu structure for Main

! Create menu bar MainMenubar child of Main
   MainMenubar = gmCreateMenuBar(Main)
 imFile = gmCreateMenuEntry(MainMenubar, 'File', gmAccel=70)
  ! Create pull-down menu MainPop1 child of File
   MainPop1_1 = gmCreatePullDownMenu(imFile)
   IF(PROGRAM_TYPE.EQ.1) THEN
		NEW_file = gmCreateMenuEntry(MainPop1_1, 'New', gmAccel=78, gmCallBack=5003) !11)
		Open_file = gmCreateMenuEntry(MainPop1_1, 'Open', gmAccel=79, gmCallBack=5001) !12)
   else if(program_type.eq.2) then
		Open_file = gmCreateMenuEntry(MainPop1_1, 'Open', gmAccel=79, gmCallBack=5001) !12)
		 separator1_3 = gmCreateMenuSeparator(MainPop1_1)
		Import_file = gmCreateMenuEntry(MainPop1_1, 'Import', gmAccel=73, gmCallBack=6000) 
		Mainimp = gmCreatePullDownMenu(import_file)
		i1=gmCreateMenuEntry(mainimp, 'Cjump',gmAccel=90, gmCallBack=6001) !732)
	    !i2=gmCreateMenuEntry(mainimp, 'Axon',gmAccel=90, gmCallBack=6002) !706)
		!call gmSetWidgetStatus(i1, GunSELECTABLE)
   else if(program_type.eq.3) then
		new_file = gmCreateMenuEntry(MainPop1_1, 'Simulate data', gmAccel=79, gmCallBack=3301) !11)
		Open_file = gmCreateMenuEntry(MainPop1_1, 'Open', gmAccel=79, gmCallBack=3005) !12)
		!Import_file = gmCreateMenuEntry(MainPop1_1, 'Import', gmAccel=73, gmCallBack=5001) !13)
   endif
  
   if(program_type.eq.1) then
	separator1_1 = gmCreateMenuSeparator(MainPop1_1)
 
    iSave_bmp = gmCreateMenuEntry(MainPop1_1, 'Save as bmp/wmf', gmAccel=83, gmCallBack=5200) !14)
    Export_file = gmCreateMenuEntry(MainPop1_1, 'Export as plotq', gmAccel=69, gmCallBack=5100) !15)
    separator1_2 = gmCreateMenuSeparator(MainPop1_1)
    Print_file = gmCreateMenuEntry(MainPop1_1, 'Print', gmAccel=100, gmCallBack=5150) !16)
   endif
separator1_3 = gmCreateMenuSeparator(MainPop1_1)
  modelw(3) = gmCreateMenuEntry(MainPop1_1, 'Use ini file', gmAccel=84,gmCallBack=-10)

   separator1_3 = gmCreateMenuSeparator(MainPop1_1)
   Exit_file = gmCreateMenuEntry(MainPop1_1, 'Exit', gmAccel=69, gmCallBack=-1000) !19)

   if(program_type.eq.3) then
	!	call gmSetWidgetStatus(new_file, GunSELECTABLE)
	!	call gmSetWidgetStatus(open_file, GunSELECTABLE)
	


   endif
IF(PROGRAM_TYPE.EQ.-1.OR.PROGRAM_TYPE.EQ.3) THEN
   Mechanism = gmCreateMenuEntry(MainMenubar, 'Mechanism', gmAccel=84)
   
   MainPop1_15 = gmCreatePullDownMenu(Mechanism)
   modelw(1) = gmCreateMenuEntry(MainPop1_15, 'Create New', gmAccel=84,gmCallBack=1100 )!35)
   modelw(2) = gmCreateMenuEntry(MainPop1_15, 'Display Old', gmAccel=84,gmCallBack=1200) !36)
    separator1_15 = gmCreateMenuSeparator(MainPop1_15) 
   modelw(4) = gmCreateMenuEntry(MainPop1_15, 'View/Edit state properties ', gmAccel=84,gmCallBack=1300) !3501)
   modelw(5) = gmCreateMenuEntry(MainPop1_15, 'View/Edit rate constants', gmAccel=84,gmCallBack=1400) !3502)
   modelw(6) = gmCreateMenuEntry(MainPop1_15, 'Mechanism Title', gmAccel=84,gmCallBack=1950) !6502)
   modelw(7) = gmCreateMenuEntry(MainPop1_15, 'Rate Title', gmAccel=84,gmCallBack=1960) !6504)
 
   separator1_15 = gmCreateMenuSeparator(MainPop1_15) 
   modelw(8) = gmCreateMenuEntry(MainPop1_15, 'Microscopic reversibility ', gmAccel=84,gmCallBack=-122) !1005)
  ! MainPopmr= gmCreatePullDownMenu(modelw(8))
	! eqfit5 = gmCreateMenuEntry(mainpopmr, 'Spaning tree method', gmAccel=84,gmCallBack=2010) !1001)
     !eqfit6 = gmCreateMenuEntry(mainpopmr, 'Ordered cycles method', gmAccel=84,gmCallBack=2020) !3521)

   modelw(9) = gmCreateMenuEntry(MainPop1_15, 'Constraints', gmAccel=84,gmCallBack=-132) !701)
   modelw(10) = gmCreateMenuEntry(MainPop1_15, 'EC50', gmAccel=84,gmCallBack=2035) !701)
   modelw(11) = gmCreateMenuEntry(MainPop1_15, 'Fix parameters', gmAccel=84,gmCallBack=-142) !701)   
   separator1_15 = gmCreateMenuSeparator(MainPop1_15) 	
   modelw(12) = gmCreateMenuEntry(MainPop1_15, 'Show curve', gmAccel=84,gmCallBack=-148) !3509)
   separator1_15 = gmCreateMenuSeparator(MainPop1_15) 
   modelw(13) = gmCreateMenuEntry(MainPop1_15, 'Save mechanism on disc', gmAccel=84,gmCallBack=4430) !3503)
    modelw(16) = gmCreateMenuEntry(MainPop1_15, 'Save mechanism with fitted rates',gmaccel=84,gmCallBack=4432) 
   modelw(14) = gmCreateMenuEntry(MainPop1_15, 'Save image as bmp/wmf',gmaccel=84,gmCallBack=1900)  
    modelw(15) = gmCreateMenuEntry(MainPop1_15, 'Print',gmaccel=84,gmCallBack=1850) 
 
   if(program_type.eq.3) then
	do i=4,16
		call gmSetWidgetStatus(modelw(i), GunSELECTABLE)
	
   enddo
   endif
ENDIF


  

   IF(PROGRAM_TYPE.EQ.1.or.program_type.ge.4) THEN
	Fit = gmCreateMenuEntry(MainMenubar, 'Fit', gmAccel=73)
	MainPop1_14 = gmCreatePullDownMenu(Fit)
	eqfit(1)=gmCreateMenuEntry(MainPop1_14, 'Fit one dat set',gmAccel=90, gmCallBack=6001) !701)
	eqfit(2)=gmCreateMenuEntry(MainPop1_14, 'Fit selected sets separately - same equation',gmAccel=90, gmCallBack=6002) !702)
	eqfit(3)=gmCreateMenuEntry(MainPop1_14, 'Fit selected sets separately - different equations',gmAccel=90, gmCallBack=6010) ! 710)
	eqfit(4)=gmCreateMenuEntry(MainPop1_14, 'Fit selected sets simultaneously',gmAccel=90, gmCallBack=6003) !703)
	eqfit(5)=gmCreateMenuEntry(MainPop1_14, 'Fit selected sets as one set',gmAccel=90, gmCallBack=6006) !704)
	eqfit(6)=gmCreateMenuEntry(MainPop1_14, 'Estimate relative potencies',gmAccel=90, gmCallBack=6004) !705)
	eqfit(7)=gmCreateMenuEntry(MainPop1_14, 'Estimate antagonist KB',gmAccel=90, gmCallBack=6005) !707
	do j=5,7
	call gmSetWidgetStatus(eqfit(j), GunSELECTABLE)

	enddo
   else if(PROGRAM_TYPE.EQ.3) THEN
	Fit = gmCreateMenuEntry(MainMenubar, 'Fit', gmAccel=73)
 	MainPop1_14 = gmCreatePullDownMenu(Fit)
	eqfit(1)=gmCreateMenuEntry(MainPop1_14, 'Fit',gmAccel=90, gmCallBack=-147) !732)
	eqfit(2)=gmCreateMenuEntry(MainPop1_14, 'Show guesses model',gmAccel=90, gmCallBack=-148) !706)
	eqfit(3)=gmCreateMenuEntry(MainPop1_14, 'Repeat fit',gmAccel=90, gmCallBack=4020) !706)
	
	do j=1,3
	call gmSetWidgetStatus(eqfit(j), GunSELECTABLE)

	enddo
   ENDIF
 
 	IF(PROGRAM_TYPE.EQ.3) then 

	iplot = gmCreateMenuEntry(MainMenubar, 'Plot', gmAccel=86)
    MainPop1_12 = gmCreatePullDownMenu(iplot)

	! iselect = gmCreateMenuEntry(MainPop1_12, 'Select Plot', gmAccel=90, gmCallBack=4506)
	! MainPop1_121 = gmCreatePullDownMenu(iselect)

	 if(.not.nodata) then
	  
     ipl1= gmCreateMenuEntry(MainPop1_12,' (1) Plot histogram of apparent OPEN periods, with fit',gmCallBack=4501)
    !  MainPop1_1211 = gmCreatePullDownMenu(ipl1)
	 ! ipl1_1=gmCreateMenuEntry(MainPop1_1211,' (1) Distribution of log durations',gmCallBack=4511)
     ! ipl1_2=gmCreateMenuEntry(MainPop1_1211,' (2) Distribution of durations- set bins manually',gmCallBack=4512)
     ! ipl1_3=gmCreateMenuEntry(MainPop1_1211,' (3) Distribution of durations- 20 bins',gmCallBack=4513)
     ! ipl1_4=gmCreateMenuEntry(MainPop1_1211,' (4) Distribution of durations- 40 bins',gmCallBack=4514)
     ! ipl1_5=gmCreateMenuEntry(MainPop1_1211,' (5) Distribution of durations- 60 bins',gmCallBack=4515)

	 ipl2=gmCreateMenuEntry(MainPop1_12,' (2) Plot histogram of apparent SHUT times, with fit',gmCallBack=4502)
   ! MainPop1_1212 = gmCreatePullDownMenu(ipl2)
!	  ipl1_1=gmCreateMenuEntry(MainPop1_1212,' (1) Distribution of log durations',gmCallBack=4521)
   !   ipl1_2=gmCreateMenuEntry(MainPop1_1212,' (2) Distribution of durations- set bins manually',gmCallBack=4522)
   !   ipl1_3=gmCreateMenuEntry(MainPop1_1212,' (3) Distribution of durations- 20 bins',gmCallBack=4523)
  !    ipl1_4=gmCreateMenuEntry(MainPop1_1212,' (4) Distribution of durations- 40 bins',gmCallBack=4524)
  !    ipl1_5=gmCreateMenuEntry(MainPop1_1212,' (5) Distribution of durations- 60 bins',gmCallBack=4525)

	 ipl3= gmCreateMenuEntry(MainPop1_12,' (3) Open time pdf conditional on adjacent gap',gmCallBack=4503)
    !  MainPop1_1213 = gmCreatePullDownMenu(ipl3)
!	  ipl3_1=gmCreateMenuEntry(MainPop1_1213,' (1) Show open times conditional on PRECEDING shut time',gmCallBack=4531)
	!  ipl3_2=gmCreateMenuEntry(MainPop1_1213,' (2) Show open times conditional on FOLLOWING shut time',gmCallBack=4532)
    !  ipl3_3=gmCreateMenuEntry(MainPop1_1213,' (3) Show open times conditional on EITHER shut time',gmCallBack=4533)
	 ipl4=gmCreateMenuEntry(MainPop1_12, ' (4) Mean open vs adjacent gap plot, with fit',gmCallBack=4504)
     ipl5=gmCreateMenuEntry(MainPop1_12,' (5) Dependency plot',gmCallBack=4505)
   !  MainPop1_1215 = gmCreatePullDownMenu(ipl5)
	!  ipl5_1=gmCreateMenuEntry(MainPop1_1215,' (1) Distributions of open period-shut times',gmCallBack=4551)
	 ! ipl5_2=gmCreateMenuEntry(MainPop1_1215,' (2) Distributions of shut time-shut time',gmCallBack=4552)
	  !ipl5_3=gmCreateMenuEntry(MainPop1_1215,' (3) Distributions of open period-open period',gmCallBack=4553)
 
	 ipl6=gmCreateMenuEntry(MainPop1_12,' (6) P(open) curves (ideal and HJC)',gmCallBack=4506)
     
	 else if (nodata) then
	 
     ipl1=gmCreateMenuEntry(MainPop1_12, ' (1) Plot HJC distribution of apparent OPEN periods',gmCallBack=4501)
    	 ipl2=gmCreateMenuEntry(MainPop1_121, ' (2) Plot HJC distribution of apparent SHUT  times',gmCallBack=4502)
     
	 ipl3=gmCreateMenuEntry(MainPop1_12, ' (3) Open time pdf conditional on adjacent gap',gmCallBack=4503)
    	
	 ipl4=gmCreateMenuEntry(MainPop1_12, ' (4) Mean open vs adjacent gap plot',gmCallBack=4504)
     ipl5=gmCreateMenuEntry(MainPop1_12, ' (5) Dependency plot',gmCallBack=4505)
	 
	 
	 ipl6=gmCreateMenuEntry(MainPop1_12,' (6) P(open) curves (ideal and HJC)',gmCallBack=4506)
     
  
	 endif
	
!	 separator1_1 = gmCreateMenuSeparator(MainPop1_12)
	endif
	 IF(PROGRAM_TYPE.lE.1) THEN
  
    Viewer = gmCreateMenuEntry(MainMenubar, 'View', gmAccel=86)
    MainPop1_6 = gmCreatePullDownMenu(Viewer)
    view_Record = gmCreateMenuEntry(MainPop1_6, 'Records List', gmAccel=82, gmCallBack=8001) !31)
   ! view_Data = gmCreateMenuEntry(MainPop1_6, 'Current Data', gmAccel=69, gmCallBack=5502) !32)
   
   ! view_Another = gmCreateMenuEntry(MainPop1_6, 'Current graph', gmAccel=65, gmCallBack=5503) !34)
    view_Equations = gmCreateMenuEntry(MainPop1_6, 'Equations', gmAccel=69, gmCallBack=8002) !33)
	iview_text = gmCreateMenuEntry(MainPop1_6, 'Text file', gmAccel=69, gmCallBack=8003) !33)
   else if(PROGRAM_TYPE.EQ.3) THEN
	Viewer = gmCreateMenuEntry(MainMenubar, 'View', gmAccel=86)
  
	MainPop1_6 = gmCreatePullDownMenu(Viewer)
	iview_text = gmCreateMenuEntry(MainPop1_6, 'Text file', gmAccel=69, gmCallBack=8003) !33)

	view_Record = gmCreateMenuEntry(MainPop1_6, 'Parameters', gmAccel=82, gmCallBack=3501) !31)
	view_Data = gmCreateMenuEntry(MainPop1_6, 'Stability plot', gmAccel=69, gmCallBack=3510) !32)
	istab = gmCreatePullDownMenu(view_data)
	istab1 = gmCreateMenuEntry(istab, 'Amplitude', gmAccel=69, gmCallBack=3511) !32)
	istab2 = gmCreateMenuEntry(istab, 'Open,shut time and Popen', gmAccel=69, gmCallBack=3611) !33)
    call gmSetWidgetStatus(view_record, GunSELECTABLE)
	call gmSetWidgetStatus(view_data, GunSELECTABLE)
  ENDIF

 	igrf = gmCreateMenuEntry(MainMenubar, 'Graph', gmAccel=86)
	MainPop1_120 = gmCreatePullDownMenu(igrf)
    jTitle_record = gmCreateMenuEntry(MainPop1_120, 'Title', gmAccel=84, gmCallBack=1)
   if(program_type.ne.3) jParameters = gmCreateMenuEntry(MainPop1_120, 'Parameters', gmAccel=100, gmCallBack=2)
   jLabels = gmCreateMenuEntry(MainPop1_120, 'Axis Labels', gmAccel=76)
  
   MainPop1_7_1=gmCreatePullDownMenu(jLabels)
   Label_X = gmCreateMenuEntry(MainPop1_7_1, 'X', gmAccel=88, gmCallBack=3)
   Label_Y = gmCreateMenuEntry(MainPop1_7_1, 'Y', gmAccel=89, gmCallBack=4)
   Label_Z = gmCreateMenuEntry(MainPop1_7_1, 'Z', gmAccel=90, gmCallBack=5)
   
   jNumbers = gmCreateMenuEntry(MainPop1_120, 'Axis Numbers', gmAccel=78)
   MainPop1_7_2 = gmCreatePullDownMenu(jNumbers)
   Number_X = gmCreateMenuEntry(MainPop1_7_2, 'X', gmAccel=88, gmCallBack=6)
   Number_Y = gmCreateMenuEntry(MainPop1_7_2, 'Y', gmAccel=89, gmCallBack=7)
   Number_Z = gmCreateMenuEntry(MainPop1_7_2, 'Z', gmAccel=90, gmCallBack=8)
   
   jnewText = gmCreateMenuEntry(MainPop1_120, 'Text', gmAccel=84,gmCallBack=80)
   jAxis = gmCreateMenuEntry(MainPop1_120, 'Axis', gmAccel=65, gmCallBack=245)
   jFrame = gmCreateMenuEntry(MainPop1_120, 'Frame/Grid', gmAccel=70, gmCallBack=244)
    
	separator1_1 = gmCreateMenuSeparator(MainPop1_120)
  iZoomy = gmCreateMenuEntry(MainPop1_120, 'Rescale', gmAccel=90, gmCallBack=263) !51
 ! Interpolate = gmCreateMenuEntry(MainPop1_12, 'Interpolate', gmAccel=73, gmCallBack=265) ! 53
 ! if(program_type.ne.1) then
	separator1_1 = gmCreateMenuSeparator(MainPop1_120)
 
    iSave_bmp = gmCreateMenuEntry(MainPop1_120, 'Save as bmp/wmf', gmAccel=83, gmCallBack=5200) !14)
 !  iSave_wmf = gmCreateMenuEntry(MainPop1_1, 'Save as wmf', gmAccel=83, gmCallBack=5250) !20)
    Export_file = gmCreateMenuEntry(MainPop1_120, 'Export as plotq', gmAccel=69, gmCallBack=5100) !15)
    separator1_2 = gmCreateMenuSeparator(MainPop1_120)
    Print_file = gmCreateMenuEntry(MainPop1_120, 'Print', gmAccel=100, gmCallBack=5150) !16)
!  endif
   !---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
  if(program_type.eq.3) then
			call gmSetWidgetStatus(jtitle_record, GunSELECTABLE)
	!	call gmSetWidgetStatus(jparameters, GunSELECTABLE)
		call gmSetWidgetStatus(jlabels, GunSELECTABLE)
		call gmSetWidgetStatus(jnewtext, GunSELECTABLE)
		call gmSetWidgetStatus(jnumbers, GunSELECTABLE)
		call gmSetWidgetStatus(jaxis, GunSELECTABLE)
		call gmSetWidgetStatus(jframe, GunSELECTABLE)
	
		call gmSetWidgetStatus(izoomy, GunSELECTABLE)
	!	call gmSetWidgetStatus(interpolate, GunSELECTABLE)
		call gmSetWidgetStatus(isave_bmp, GunSELECTABLE)
		call gmSetWidgetStatus(export_file, GunSELECTABLE)
		call gmSetWidgetStatus(print_file, GunSELECTABLE)
	    call gmSetWidgetStatus(ipl1, GunSELECTABLE)
		call gmSetWidgetStatus(ipl2, GunSELECTABLE)
		call gmSetWidgetStatus(ipl3, GunSELECTABLE)
		call gmSetWidgetStatus(ipl4, GunSELECTABLE)
		call gmSetWidgetStatus(ipl5, GunSELECTABLE)
		call gmSetWidgetStatus(ipl6, GunSELECTABLE)


   endif
   
  
 

   Edit = gmCreateMenuEntry(MainMenubar, 'Insert', gmAccel=69)
   MainPop1_2 = gmCreatePullDownMenu(Edit)
  ! Title_record = gmCreateMenuEntry(MainPop1_2,'Title', gmAccel=84, gmCallBack=251)
  ! iParameters = gmCreateMenuEntry(MainPop1_2, 'Parameters', gmAccel=100, gmCallBack=252)
  ! Labels = gmCreateMenuEntry(MainPop1_2, 'Axis Labels', gmAccel=76)
  ! MainPop1_3 = gmCreatePullDownMenu(Labels)
  ! Label_X = gmCreateMenuEntry(MainPop1_3, 'X', gmAccel=88, gmCallBack=253)
  ! Label_Y = gmCreateMenuEntry(MainPop1_3, 'Y', gmAccel=89, gmCallBack=254)
  ! Label_Z = gmCreateMenuEntry(MainPop1_3, 'Z', gmAccel=90, gmCallBack=255)
   new_Text = gmCreateMenuEntry(MainPop1_2, 'Text', gmAccel=88, gmCallBack=250)
   new_Arrows = gmCreateMenuEntry(MainPop1_2, 'Arrows', gmAccel=65, gmCallBack=257 ) !943)
   new_Lines = gmCreateMenuEntry(MainPop1_2, 'Lines', gmAccel=76, gmCallBack=258) !944)
   new_Lines_h = gmCreateMenuEntry(MainPop1_2, 'Horizontal Lines', gmAccel=76, gmCallBack=259)
   new_Lines_v = gmCreateMenuEntry(MainPop1_2, 'Vertical Lines', gmAccel=76, gmCallBack=260)
  
   if(program_type.eq.3) then
	!	call gmSetWidgetStatus(title_record, GunSELECTABLE)
	!	call gmSetWidgetStatus(iparameters, GunSELECTABLE)
	!	call gmSetWidgetStatus(labels, GunSELECTABLE)
		call gmSetWidgetStatus(new_text, GunSELECTABLE)
		call gmSetWidgetStatus(new_arrows, GunSELECTABLE)
		call gmSetWidgetStatus(new_lines, GunSELECTABLE)
		call gmSetWidgetStatus(new_lines_h, GunSELECTABLE)
		call gmSetWidgetStatus(new_lines_v, GunSELECTABLE)
   endif
  if(program_type.ne.100) then
   Select = gmCreateMenuEntry(MainMenubar, 'Edit 2D', gmAccel=83)
   MainPop1_7 = gmCreatePullDownMenu(Select)
  
   jArrows = gmCreateMenuEntry(MainPop1_7, 'Arrows', gmAccel=65, gmCallBack=271 ) !943)
 
   jLines = gmCreateMenuEntry(MainPop1_7, 'Lines', gmAccel=76, gmCallBack=272) !944)
    
   jLines_h = gmCreateMenuEntry(MainPop1_7, 'Horizontal Lines', gmAccel=76, gmCallBack=273)
    jLines_v = gmCreateMenuEntry(MainPop1_7, 'Vertical Lines', gmAccel=76, gmCallBack=274)
 
   jraw_Data = gmCreateMenuEntry(MainPop1_7, 'Raw Data', gmAccel=65, gmCallBack=275)
   jCurves = gmCreateMenuEntry(MainPop1_7, 'Calculated Curves', gmAccel=70, gmCallBack=276)
  
  endif
if(program_type.eq.3) then
		
		call gmSetWidgetStatus(jlines, GunSELECTABLE)
		call gmSetWidgetStatus(jlines_h, GunSELECTABLE)
		call gmSetWidgetStatus(jlines_V, GunSELECTABLE)
		call gmSetWidgetStatus(jarrows, GunSELECTABLE)
		call gmSetWidgetStatus(jraw_data, GunSELECTABLE)
		call gmSetWidgetStatus(jcurves, GunSELECTABLE)
  
  endif
  
IF(PROGRAM_TYPE.ge.2) THEN
   ig3d = gmCreateMenuEntry(MainMenubar, 'Edit 3D', gmAccel=84)
      MainPop1_16 = gmCreatePullDownMenu(ig3d)
    icross= gmCreateMenuEntry(MainPop1_16, 'Cross section', gmAccel=84,gmCallBack=501)
    call gmSetWidgetStatus(icross, GunSELECTABLE)

    i3d= gmCreateMenuEntry(MainPop1_16, '3D projection', gmAccel=84,gmCallBack=502)
	irot= gmCreateMenuEntry(MainPop1_16, 'Rotation angle', gmAccel=84,gmCallBack=503)
    iview3d= gmCreateMenuEntry(MainPop1_16, 'View direction', gmAccel=84,gmCallBack=504)

    iaratxy= gmCreateMenuEntry(MainPop1_16, 'Aspect ratio X:Y', gmAccel=84,gmCallBack=505)
    iarathb= gmCreateMenuEntry(MainPop1_16, 'Heigh:Base', gmAccel=84,gmCallBack=506)
	igridd= gmCreateMenuEntry(MainPop1_16, 'Grid(net) density', gmAccel=84,gmCallBack=507)
    
	isurfdr= gmCreateMenuEntry(MainPop1_16, 'Surface drawn', gmAccel=84,gmCallBack=510)
	MainPop1_20 = gmCreatePullDownMenu(isurfdr)
	isurf1= gmCreateMenuEntry( MainPop1_20, 'Draw base', gmAccel=84,gmCallBack=511)
    isurf2= gmCreateMenuEntry( MainPop1_20, 'Upper', gmAccel=84,gmCallBack=512)
	isurf3= gmCreateMenuEntry( MainPop1_20, 'Both', gmAccel=84,gmCallBack=513)
	isurf4= gmCreateMenuEntry( MainPop1_20, 'Lower', gmAccel=84,gmCallBack=514)

	ifill3d= gmCreateMenuEntry(MainPop1_16, 'Fill', gmAccel=84,gmCallBack=520)
     call gmSetWidgetStatus(icross, GunSELECTABLE)
 call gmSetWidgetStatus(i3d, GunSELECTABLE)
 call gmSetWidgetStatus(irot, GunSELECTABLE)
 call gmSetWidgetStatus(iview3d, GunSELECTABLE)
 call gmSetWidgetStatus(iaratxy, GunSELECTABLE)
 call gmSetWidgetStatus(iarathb, GunSELECTABLE)
 call gmSetWidgetStatus(igridd, GunSELECTABLE)
  call gmSetWidgetStatus(isurfdr, GunSELECTABLE)
  call gmSetWidgetStatus(ifill3d, GunSELECTABLE) 


	MainPop1_21 = gmCreatePullDownMenu(ifill3d)
	
	ifill1= gmCreateMenuEntry(mainpop1_21, 'One color', gmAccel=84,gmCallBack=521)
	MainPop1_211 = gmCreatePullDownMenu(ifill1)
    idef= gmCreateMenuEntry( MainPop1_211, 'Default', gmAccel=84,gmCallBack=521)
    ichange= gmCreateMenuEntry( MainPop1_211, 'Change colour', gmAccel=84,gmCallBack=522)
	
	ifill2= gmCreateMenuEntry( MainPop1_21, 'Upper surface', gmAccel=84,gmCallBack=523)
	MainPop1_211 = gmCreatePullDownMenu(ifill2)
	idef= gmCreateMenuEntry( MainPop1_211, 'Default', gmAccel=84,gmCallBack=523)
    ichange= gmCreateMenuEntry( MainPop1_211, 'Change colour', gmAccel=84,gmCallBack=524)
	
	ifill3= gmCreateMenuEntry( MainPop1_21, 'Lower surface', gmAccel=84,gmCallBack=525)
	MainPop1_211 = gmCreatePullDownMenu(ifill3)
	idef= gmCreateMenuEntry( MainPop1_211, 'Default', gmAccel=84,gmCallBack=525)
    ichange= gmCreateMenuEntry( MainPop1_211, 'Change colour', gmAccel=84,gmCallBack=526)
	
	ifill4= gmCreateMenuEntry( MainPop1_21, 'Pos/neg 2 colours', gmAccel=84,gmCallBack=527)
	MainPop1_211 = gmCreatePullDownMenu(ifill4)
	idef= gmCreateMenuEntry( MainPop1_211, 'Default', gmAccel=84,gmCallBack=527)
    ichange= gmCreateMenuEntry( MainPop1_211, 'Change colour', gmAccel=84,gmCallBack=528)
	
	ifill5= gmCreateMenuEntry( MainPop1_21, 'Pos/neg 4 colours', gmAccel=84,gmCallBack=529)
	MainPop1_211 = gmCreatePullDownMenu(ifill5)
	idef= gmCreateMenuEntry( MainPop1_211, 'Default', gmAccel=84,gmCallBack=529)
    ichange= gmCreateMenuEntry( MainPop1_211, 'Change colour', gmAccel=84,gmCallBack=530)
	
	ifill6= gmCreateMenuEntry( MainPop1_21, 'Fill contours', gmAccel=84,gmCallBack=531)
	MainPop1_211 = gmCreatePullDownMenu(ifill6)
	idef= gmCreateMenuEntry( MainPop1_211, 'Default', gmAccel=84,gmCallBack=531)
    ichange= gmCreateMenuEntry( MainPop1_211, 'Change colour', gmAccel=84,gmCallBack=532)
	
	ifill7= gmCreateMenuEntry( MainPop1_21, 'No fill ', gmAccel=84,gmCallBack=533)

	imarkbad= gmCreateMenuEntry(MainPop1_16, 'Mark bad regions', gmAccel=84,gmCallBack=508)
    MainPop1_22 = gmCreatePullDownMenu(imarkbad)
	idef= gmCreateMenuEntry( MainPop1_22, 'Default', gmAccel=84,gmCallBack=508)
    ichange= gmCreateMenuEntry( MainPop1_22, 'Change colour', gmAccel=84,gmCallBack=509)
	call gmSetWidgetStatus(ifill3d, GunSELECTABLE)
	call gmSetWidgetStatus(imarkbad, GunSELECTABLE)

endif

   Help = gmCreateMenuEntry(MainMenubar, 'Help', gmAccel=72)

! Create pull-down menu MainPop6 child of Help
   MainPop1_13 = gmCreatePullDownMenu(Help)
    IF(PROGRAM_TYPE.EQ.3) then
	Studio = gmCreateMenuEntry(MainPop1_13, 'Graph settings', gmAccel=83, gmCallBack=951) !611)
  
   imeca = gmCreateMenuEntry(MainPop1_13, 'New mechanism', gmAccel=73, gmCallBack=961) !613)
 ! iHowto=gmCreateMenuEntry(MainPop1_13, 'How to .....', gmAccel=65)
  ! !MainPop1_17 = gmCreatePullDownMenu(ihowto)
   istart1 = gmCreateMenuEntry(MainPop1_13, 'Modify mechanism', gmAccel=90, gmCallBack=971) !610)
 !  iselect = gmCreateMenuEntry(MainPop1_13, 'Select a graph', gmAccel=73, gmCallBack=611)
	 else IF(PROGRAM_TYPE.EQ.2) then
	 	Studio = gmCreateMenuEntry(MainPop1_13, 'Graph settings', gmAccel=83, gmCallBack=951) !611)
  
	else
 Studio = gmCreateMenuEntry(MainPop1_13, 'Curve fitting', gmAccel=83, gmCallBack=981) !602)
  
   endif
   About = gmCreateMenuEntry(MainPop1_13, 'About', gmAccel=65, gmCallBack=999) !601)
   
     IF(PROGRAM_TYPE.EQ.-1.OR.PROGRAM_TYPE.EQ.1) THEN
   ipaste = gmCreateMenuEntry(MainPop1_13, 'Paste data from Excel file', gmAccel=73, gmCallBack=991) !612)
   
   
   endif
 toolbar1_1 = gmCreatetoolbar(Main,GTOP,22)


! Create icon Icon1 child of toolbar1_1
if(program_type.eq.3) then
Icon1_1 = gmCreateToolBarButton(toolbar1_1,GICONBUTTON,'MBIG1001', &
              	
				gmHFlag = GBUBBLEANDSTATUSBAR,&
                gmHelp = 'Open file',gmcallback =3001) !12)
else
   Icon1_1 = gmCreateToolBarButton(toolbar1_1,GICONBUTTON,'MBIG1001', &
              	
				gmHFlag = GBUBBLEANDSTATUSBAR,&
                gmHelp = 'Open file',gmcallback =5001) !12)
endif
! Create icon Icon2 child of toolbar1_1
   Icon1_2 = gmCreateToolBarButton(toolbar1_1,GICONBUTTON ,'MBIG1055',  &
              
				gmHFlag = GBUBBLEANDSTATUSBAR,&
                gmHelp = 'Print',gmcallback = 901) !16)
! Create icon Icon4 child of toolbar1_1
   Icon1_3 = gmCreateToolBarButton(toolbar1_1, GICONBUTTON,'MBIG1033', &
              
				gmHFlag = GBUBBLEANDSTATUSBAR,&
                gmHelp = 'BMP/WMF',gmcallback = 801) !14)
! Create icon Icon5 child of toolbar1_1
   Icon1_4 = gmCreateToolBarButton(toolbar1_1,GICONBUTTON ,'MSML1039',  &
              	
				gmHFlag = GBUBBLEANDSTATUSBAR,&
                gmHelp = 'Zoom out',gmcallback = 261)
! Create icon Icon6 child of toolbar1_1
   Icon1_5 = gmCreateToolBarButton(toolbar1_1, GICONBUTTON,'MSML1038',  &
              
				gmHFlag = GBUBBLEANDSTATUSBAR,&
                gmHelp = 'Zoom in',gmcallback = 263)

! Create combo box Combo1 child of Panel1
   Combo1_1 = gmCreateComboBox(toolbar1_1, 1, 0, 5, 5, GNONE, 1, &
              	gmSort=GUNSORTED, gmVpos=GTOP, gmExpand=GOFF, gmcallback=401)

! Create entries for combo box Combo1
call gmSetListEntry(Combo1_1, GADD, gmString='Orientation')
   call gmSetListEntry(Combo1_1, GADD, gmString='Landscape')
   call gmSetListEntry(Combo1_1, GADD, gmString='Portrait')
   call gmSetListEntry(Combo1_1, GADD, gmString='Square')
   call gmSetListEntry(Combo1_1, GADD, gmString='Full width')
   call gmSetListEntry(Combo1_1, GADD, gmString='I-V Plot')

! Select first entry in list for display
   call gmSetListEntry(Combo1_1,GSELECT,gmEntry=1)

! Create combo box combo1_2 child of Panel1
   combo1_2 = gmCreateComboBox(toolbar1_1, 6, 0, 5, 10, GNONE, 1, &
              	gmSort=GUNSORTED, gmVpos=GTOP, gmExpand=GOFF, gmcallback=402)

! Create entries for combo box combo1_2
	 call gmSetListEntry(combo1_2, GADD, gmString='Scale')
   call gmSetListEntry(combo1_2, GADD, gmString='Linear')
   call gmSetListEntry(combo1_2, GADD, gmString='Log X - Y')
   call gmSetListEntry(combo1_2, GADD, gmString='X - Log Y')
   call gmSetListEntry(combo1_2, GADD, gmString='Log X - Log Y')
   call gmSetListEntry(combo1_2, GADD, gmString='Hill Plot')
   call gmSetListEntry(combo1_2, GADD, gmString='X - Square Y')
   call gmSetListEntry(combo1_2, GADD, gmString='Log X - Square Y')
  

! Select first entry in list for display
   call gmSetListEntry(combo1_2,GSELECT,gmEntry=1)

 IF(PROGRAM_TYPE.EQ.-1) THEN

! Create combo box combo1_3 child of Panel1
   combo1_3 = gmCreateComboBox(Toolbar1_1, 12, 0, 6, 6, GNONE, 1, &
              	gmSort=GUNSORTED, gmVpos=GTOP, gmExpand=GOFF, gmcallback=5303)

! Create entries for combo box combo1_3
   call gmSetListEntry(combo1_3, GADD, gmString='All data sets')
   call gmSetListEntry(combo1_3, GADD, gmString='Selected data sets ')
   
! Select first entry in list for display
   call gmSetListEntry(combo1_3,GSELECT,gmEntry=1)

ENDIF

toolbar1_3 = gmCreatetoolbar(Main,GTOP,22)
 
 Icon1_13 = gmCreateToolBarButton(toolbar1_3, GICONBUTTON,'MSML1017',  &
              	
				gmHFlag = GBUBBLEANDSTATUSBAR,&
                gmHelp = 'Insert arrow ',gmcallback = 257 )
! Create icon Icon9 child of toolbar1_1
    Icon1_12 = gmCreateToolBarButton(toolbar1_3, GICONBUTTON,'MSML1009',  &
              		gmHFlag = GBUBBLEANDSTATUSBAR,&
                gmHelp = 'Insert new line',gmcallback = 258)
 Icon1_121 = gmCreateToolBarButton(toolbar1_3, GICONBUTTON,'MSML1050',  &
              	
				gmHFlag = GBUBBLEANDSTATUSBAR,&
                gmHelp = 'Insert hline',gmcallback = 259)
 Icon1_122 = gmCreateToolBarButton(toolbar1_3, GICONBUTTON,'MSML1073',  &
              	
				gmHFlag = GBUBBLEANDSTATUSBAR,&
                gmHelp = 'Insert vline',gmcallback = 260)

! Create icon Icon13 child of toolbar1_1
  
! Create icon Icon14 child of toolbar1_1

combo1_9 = gmCreateComboBox(Toolbar1_3, 0, 0, 4, 5, GNONE, 1, &
             	gmSort=GUNSORTED, gmVpos=GTOP, gmExpand=GOFF, gmcallback=313)

call gmSetListEntry(Combo1_9, GADD, gmString='Line width')
   call gmSetListEntry(Combo1_9, GADD, gmString='Normal')
   call gmSetListEntry(Combo1_9, GADD, gmString='Thin lines')
   call gmSetListEntry(Combo1_9, GADD, gmString='Thick lines')
     ! Select first entry in list for display
   call gmSetListEntry(Combo1_9,GSELECT,gmEntry=1)

combo1_10 = gmCreateComboBox(Toolbar1_3, 0, 0, 4, 12, GNONE, 1, &
             	gmSort=GUNSORTED, gmVpos=GTOP, gmExpand=GOFF, gmcallback=312)

call gmSetListEntry(Combo1_10, GADD, gmString='Line type')
   
   call gmSetListEntry(Combo1_10, GADD, gmString='-------------------------')
   call gmSetListEntry(Combo1_10, GADD, gmString='- - - - - - - - - - - - - -')
    call gmSetListEntry(Combo1_10, GADD, gmString='-  -  -  -  -  -  -  -  -  -')
	call gmSetListEntry(Combo1_10, GADD, gmString='-- - -- - -- - -- - -- - -- - -- -')

   call gmSetListEntry(Combo1_10, GADD, gmString='...............................')
    call gmSetListEntry(Combo1_10, GADD, gmString='. . . . . . . . . . . . . . . .')
     call gmSetListEntry(Combo1_10, GADD, gmString='_ _ _ _ _ _ _ _ _ _ _ _')
   call gmSetListEntry(Combo1_10, GADD, gmString='_ . _ . _ . _ . _ . _ . _')
    call gmSetListEntry(Combo1_10, GADD, gmString='__ __ __ __ __ __ __ __')
	 call gmSetListEntry(Combo1_10, GADD, gmString='__ . __ . __ . __ . __')
     ! Select first entry in list for display
   call gmSetListEntry(Combo1_10,GSELECT,gmEntry=1)
! Create icon Icon27 child of Toolbar1_3
combo1_11 = gmCreateComboBox(Toolbar1_3, 0, 0, 4, 5, GNONE, 1, &
             	gmSort=GUNSORTED, gmVpos=GTOP, gmExpand=GOFF, gmcallback=361)

call gmSetListEntry(Combo1_11, GADD, gmString='Symbol size')
   call gmSetListEntry(Combo1_11, GADD, gmString='Normal')
   call gmSetListEntry(Combo1_11, GADD, gmString='Small')
   call gmSetListEntry(Combo1_11, GADD, gmString='Big')
     ! Select first entry in list for display
   call gmSetListEntry(Combo1_11,GSELECT,gmEntry=1)
combo1_12 = gmCreateComboBox(Toolbar1_3, 0, 0, 5, 14, GNONE, 1, &
             	gmSort=GUNSORTED, gmVpos=GTOP, gmExpand=GOFF, gmcallback=370)

call gmSetListEntry(Combo1_12, GADD, gmString='Symbol type')
   call gmSetListEntry(Combo1_12, GADD, gmString='point')
   call gmSetListEntry(Combo1_12, GADD, gmString='triangle up')
   call gmSetListEntry(Combo1_12, GADD, gmString='triangle down')
    call gmSetListEntry(Combo1_12, GADD, gmString='square')
   call gmSetListEntry(Combo1_12, GADD, gmString='diamond')
   call gmSetListEntry(Combo1_12, GADD, gmString='pentagon')
    call gmSetListEntry(Combo1_12, GADD, gmString='hexagon')
    call gmSetListEntry(Combo1_12, GADD, gmString='circle')
   call gmSetListEntry(Combo1_12, GADD, gmString='+')
   call gmSetListEntry(Combo1_12, GADD, gmString='x')
    call gmSetListEntry(Combo1_12, GADD, gmString='triangle up filled')
   call gmSetListEntry(Combo1_12, GADD, gmString='triangle down filled')
    call gmSetListEntry(Combo1_12, GADD, gmString='square filled')
   call gmSetListEntry(Combo1_12, GADD, gmString='diamond filled')
   call gmSetListEntry(Combo1_12, GADD, gmString='pentagon filled')
    call gmSetListEntry(Combo1_12, GADD, gmString='hexagon filled')
    call gmSetListEntry(Combo1_12, GADD, gmString='circle filled')
     ! Select first entry in list for display
   call gmSetListEntry(Combo1_12,GSELECT,gmEntry=1)
   
!Icon1_24 = gmCreateToolBarButton(Toolbar1_3, GICONBUTTON,'MSML1061',  &
!              	
!				 gmHFlag = GBUBBLEANDSTATUSBAR,&
!                 gmHelp = 'Symbol type ',&
!                 gmcallback = 370)
Icon1_22 = gmCreateToolBarButton(Toolbar1_3, GICONBUTTON,'MSML1035',  &
              	
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Color Selector ',&
                 gmcallback = 307)
! Create button Button4 child of Toolbar1_3
   Icon1_23 = gmCreateToolBarButton(Toolbar1_3,GICONBUTTON , 'MSML1004', &
              gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Delete ',&
                 gmcallback = 308)	

 toolbar1_2 = gmCreatetoolbar(Main,GTOP,22) 


   Icon1_6 = gmCreateToolBarButton(toolbar1_2,GICONBUTTON ,'title.ico', &
              	
				gmHFlag = GBUBBLEANDSTATUSBAR,&
                gmHelp = 'Edit Title',gmcallback = 1 )
! Create icon Icon8 child of toolbar1_1
   if(program_type.ne.3) Icon1_7 = gmCreateToolBarButton(toolbar1_2,GICONBUTTON ,'param.ico',  &
              
				gmHFlag = GBUBBLEANDSTATUSBAR,&
                gmHelp = 'Edit parameters',gmcallback = 2)
! Create icon Icon10 child of toolbar1_1
   Icon1_9 = gmCreateToolBarButton(toolbar1_2,GICONBUTTON ,'xaxis.ico',  &
              
				gmHFlag = GBUBBLEANDSTATUSBAR,&
                gmHelp = 'Edit x axis',gmcallback = 353)
! Create icon Icon11 child of toolbar1_1
   Icon1_10 = gmCreateToolBarButton(toolbar1_2,GICONBUTTON ,'yaxis.ico',  &
              	
				gmHFlag = GBUBBLEANDSTATUSBAR,&
                gmHelp = ' Edit y axis',gmcallback = 354)
! Create icon Icon11 child of toolbar1_1
   Icon1_11 = gmCreateToolBarButton(toolbar1_2,GICONBUTTON ,'zaxis.ico', &
              	
				gmHFlag = GBUBBLEANDSTATUSBAR,&
                gmHelp = 'Edit z axis',gmcallback = 355)
! Create icon Icon12 child of toolbar1_1
  Icon1_8 = gmCreateToolBarButton(toolbar1_2,GICONBUTTON ,'MSML1083',  &
				gmHFlag = GBUBBLEANDSTATUSBAR,&
                gmHelp = 'Insert new text',gmcallback = 250)
 

!toolbar1_3 = gmCreatetoolbar(Main,GTOP,22)
! Create combo box Combo1_4 child of Toolbar1_3
   Combo1_4 = gmCreateComboBox(Toolbar1_2, 0, 0, 6, 14, Gnone, gdisplay, &
              	gmSort=GunSORTED, gmVpos=GTOP, gmExpand=GOFF, gmcallback=319)

! Create entries for combo box Combo1_5
 call gmSetListEntry(Combo1_4, GADD, gmString='Gino Font')
   call gmSetListEntry(Combo1_4, GADD, gmString='Roman Simplex')
   call gmSetListEntry(Combo1_4, GADD, gmString='Roman Duplex')
   call gmSetListEntry(Combo1_4, GADD, gmString='Roman Complex')
   call gmSetListEntry(Combo1_4, GADD, gmString='Roman Triplex')
   call gmSetListEntry(Combo1_4, GADD, gmString='Italic Complex')
   call gmSetListEntry(Combo1_4, GADD, gmString='Italic Triplex')
   call gmSetListEntry(Combo1_4, GADD, gmString='Script Simplex') 
   call gmSetListEntry(Combo1_4, GADD, gmString='Script Complex')
   call gmSetListEntry(Combo1_4, GADD, gmString='Greek Simplex')
   call gmSetListEntry(Combo1_4, GADD, gmString='Greek Complex')
   call gmSetListEntry(Combo1_4, GADD, gmString='Gothic English')
   call gmSetListEntry(Combo1_4, GADD, gmString='Gothic German')
   call gmSetListEntry(Combo1_4, GADD, gmString='Gothic Italian')
   call gmSetListEntry(Combo1_4, GADD, gmString='Chirilic')
   call gmSetListEntry(Combo1_4, GADD, gmString='Swiss Solid')
   call gmSetListEntry(Combo1_4, GADD, gmString='Duch Solid')
   call gmSetListEntry(Combo1_4, GADD, gmString='Western')
   call gmSetListEntry(Combo1_4, GADD, gmString='Computer')
   call gmSetListEntry(Combo1_4, GADD, gmString='Display')
   call gmSetListEntry(Combo1_4, GADD, gmString='Latin')
   call gmSetListEntry(Combo1_4,GSELECT,gmEntry=1)

!101 - 108 
  Combo1_8 = gmCreateComboBox(Toolbar1_2, 0, 0, 5, 10, Gnone, gdisplay, &
              	gmSort=GunSORTED, gmVpos=GTOP, gmExpand=GOFF, gmcallback=318)

! Create entries for combo box Combo1_5
 call gmSetListEntry(Combo1_8, GADD, gmString='TT Font')
 call gmSetListEntry(Combo1_8, GADD, gmString='Courier')
   call gmSetListEntry(Combo1_8, GADD, gmString='Helvetica')
   call gmSetListEntry(Combo1_8, GADD, gmString='Roman')
  ! call gmSetListEntry(Combo1_8, GADD, gmString='Avant Garde')
 !  call gmSetListEntry(Combo1_8, GADD, gmString='Lublin')
 !  call gmSetListEntry(Combo1_8, GADD, gmString='New century')
 !  call gmSetListEntry(Combo1_8, GADD, gmString='Souvenir')
 !  call gmSetListEntry(Combo1_8, GADD, gmString='Palatino')
 !  call gmSetListEntry(Combo1_8, GADD, gmString='Chancery')
 !  call gmSetListEntry(Combo1_8, GADD, gmString='Arial')
 !  call gmSetListEntry(Combo1_8, GADD, gmString='Times New Roman')
 !  call gmSetListEntry(Combo1_8, GADD, gmString='Symbol')
 
! Select first entry in list for display
   call gmSetListEntry(Combo1_8,GSELECT,gmEntry=1)

! Create combo box Combo1_5 child of Toolbar1_3
   combo1_5 = gmCreateComboBox(Toolbar1_2, 0, 0, 3, 8, GNONE, 1, &
              	gmSort=GUNSORTED, gmVpos=GTOP, gmExpand=GOFF, gmcallback=320)

! Create entries for combo box Combo1_5
    call gmSetListEntry(Combo1_5, GADD, gmString='Size')
   
   call gmSetListEntry(Combo1_5, GADD, gmString='14')
   call gmSetListEntry(Combo1_5, GADD, gmString='16')
   call gmSetListEntry(Combo1_5, GADD, gmString='18')
   call gmSetListEntry(Combo1_5, GADD, gmString='20')
   

! Select first entry in list for display
   call gmSetListEntry(Combo1_5,GSELECT,gmEntry=1)
 
 combo1_6 = gmCreateComboBox(Toolbar1_2, 0, 0, 3, 8, GNONE, 1, &
             	gmSort=GUNSORTED, gmVpos=GTOP, gmExpand=GOFF, gmcallback=311)

! Create entries for combo box Combo1_5
 call gmSetListEntry(Combo1_6, GADD, gmString='Angle')
   call gmSetListEntry(Combo1_6, GADD, gmString=' 0')
  ! call gmSetListEntry(Combo1_6, GADD, gmString='15')
   call gmSetListEntry(Combo1_6, GADD, gmString='30')
   !call gmSetListEntry(Combo1_6, GADD, gmString='45')
   call gmSetListEntry(Combo1_6, GADD, gmString='60')
   call gmSetListEntry(Combo1_6, GADD, gmString='90')
   ! Select first entry in list for display
   call gmSetListEntry(Combo1_6,GSELECT,gmEntry=1)

! Create icon Icon24 child of Toolbar1_3
   Icon1_16 = gmCreateToolBarButton(Toolbar1_2, GICONBUTTON,'MSTD1009',  &
              	
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Bold text',&
                 gmcallback = 321)

! Create icon Icon25 child of Toolbar1_3
   Icon1_17 = gmCreateToolBarButton(Toolbar1_2,GICONBUTTON ,'MSTD1010',  &
              	
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Italic ',&
                 gmcallback = 322)

! Create icon Icon26 child of Toolbar1_3
   Icon1_18 = gmCreateToolBarButton(Toolbar1_2,GICONBUTTON ,'MSTD1011',  &
              	
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Underline ',&
                 gmcallback = 323)
! Create icon Icon19 child of Toolbar1_3
   Icon1_19 = gmCreateToolBarButton(Toolbar1_2,GICONBUTTON ,'MSTD1012',  &
              	
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Left Justify ',&
                 gmcallback = 324)

! Create icon Icon20 child of Toolbar1_3
   Icon1_20 = gmCreateToolBarButton(Toolbar1_2, GICONBUTTON,'MSTD1013',  &
              	
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Centre Justify ',&
                 gmcallback = 325)
! Create icon Icon27 child of Toolbar1_3
   Icon1_21 = gmCreateToolBarButton(Toolbar1_2,GICONBUTTON ,'MSTD1014',  &
              	
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Right Justify ',&
                 gmcallback = 326)
! Create icon Icon27 child of Toolbar1_3
   Icon1_22 = gmCreateToolBarButton(Toolbar1_2, GICONBUTTON,'MSML1035',  &
              	
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Color Selector ',&
                 gmcallback = 327)

! Create button Button4 child of Toolbar1_3
   Icon1_23 = gmCreateToolBarButton(Toolbar1_2,GICONBUTTON , 'MSML1004', &
              gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Delete ',&
                 gmcallback = 328)	
! Create button Button5 child of Toolbar1_3
   Icon1_24 = gmCreateToolBarButton(Toolbar1_2, GICONBUTTON, 'MSML1022', &
              	gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Move ',&
                gmcallback = 329)  !309)
! Create button Button6 child of Toolbar1_3
  ! Icon1_25 = gmCreateToolBarButton(Toolbar1_3, GICONBUTTON, 'MSML1010', &
   !           	gmHFlag = GBUBBLEANDSTATUSBAR,&
    !             gmHelp = 'Box ',&
     !            gmcallback = 310)

				 ! Create button Button6 child of Toolbar1_3
  ! Icon1_26 = gmCreateToolBarButton(Toolbar1_3, GICONBUTTON, 'MSML1090', &
  !           	gmHFlag = GBUBBLEANDSTATUSBAR,&
  !             gmHelp = 'Rotate ',&
  !               gmcallback = 311)
				 ! Create combo box Combo1_5 child of Toolbar1_3
 

				  IF(PROGRAM_TYPE.EQ.-1.OR.PROGRAM_TYPE.EQ.3) THEN
	toolbar1_4 = gmCreatetoolbar(Main,GTOP,22)

 !  Icon1_29 = gmCreateToolBarButton(Toolbar1_4, GICONBUTTON, 'open.ico', &
 !            	gmHFlag = GBUBBLEANDSTATUSBAR,&
 !                gmHelp = 'Add an Open state ',&
 !                gmcallback = 1161) !314)
 Icon1_29 = gmCreateToolBarButton(Toolbar1_4, GICONBUTTON, 'Ostate.ICO', &
              	gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Add an Open state ',&
                 gmcallback = 1161)

   !Icon1_30 = gmCreateToolBarButton(Toolbar1_4, GICONBUTTON, 'close.ico', &
   !          	gmHFlag = GBUBBLEANDSTATUSBAR,&
   !            gmHelp = 'Add a Closed State ',&
   !           gmcallback = 1162) !315)
   Icon1_30 = gmCreateToolBarButton(Toolbar1_4, GICONBUTTON, 'cstate.ICO', &
              	gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Add a Closed State ',&
                 gmcallback = 1162) !315)
   Icon1_31 = gmCreateToolBarButton(Toolbar1_4, GICONBUTTON, 'MSML1054', &
              	gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Create Link between States ',&
                 gmcallback = 1163) !316)
	Icon1_35 = gmCreateToolBarButton(Toolbar1_4,GICONBUTTON , 'MSML1004', &
              gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Delete state/link',&
                 gmcallback = 1165)	
! Create button Button5 child of Toolbar1_3
   Icon1_34 = gmCreateToolBarButton(Toolbar1_4, GICONBUTTON, 'MSML1022', &
              	gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Move state/link',&
                 gmcallback = 1164)
	Icon1_32 = gmCreateToolBarButton(Toolbar1_4, GICONBUTTON,'indie.ico ', &
              	gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Create Independent model',&
                 gmcallback =1150) ! 317)
!	Icon1_33 = gmCreateToolBarButton(Toolbar1_4, GICONBUTTON, 'MSML1060', &
 !             	gmHFlag = GBUBBLEANDSTATUSBAR,&
  !               gmHelp = 'Reset',&
   !              gmcallback = 1170) !318)	'MSML1069'
	
	ENDIF
	
   toolbar1_5 = gmCreatetoolbar(Main,GTOP,22)
   
   Icon1_14 = gmCreateToolBarButton(toolbar1_5, GICONBUTTON,'MSML1064',  &
              gmHFlag = GBUBBLEANDSTATUSBAR,gmHelp = 'Help',gmcallback =999) 
   
   Icon1_15 = gmCreateToolBarButton(toolbar1_5,GICONBUTTON,'MSML1007',  &
              gmHFlag = GBUBBLEANDSTATUSBAR, gmHelp = 'Exit',gmcallback = -1000) 
			
			
 
	call DATE(qDATEW)
   cDATEW=qDATEW(1:7)//'20'//qDATEW(8:9)
   Status_bar1 = gmCreateStatusBar(Main,gmPanes = 5, gmSizes = IPANS)
   call gmSetStatusBarText(Status_bar1,1,'DATEW :'//cDATEW)
  ! call gmSetStatusBarText(Status_bar1,2,'File')
   call gmSetStatusBarText(Status_bar1,4,&
   'When multple graphs on screen, press the left mouse button on the title bar to select a specific one')
	call gmSetStatusBarText(Status_bar1,2,'0.0')				
   call gmSetStatusBarText(Status_bar1,3,'0.0')
 
imainPanel=gmCreatePanel(main, 1, 1, iwidth-1, iheight-2,&
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

end