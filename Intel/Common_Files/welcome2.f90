subroutine welcome(main,initialw,initialw_MainPanel,inifile, initialw_Text5,&
initial_toggle,initialw_Toggle3,initialw_Toggle4,initialw_Toggle5,initialw_Text2,pfilem,&
textcomp,textid,drives)

use menu_f90
use IFPORT
!use dfport
character*26 drives
character*40 pfilem,textcomp,textid
character*40 inifile
! Widget identifier definitions
character*60 radio_text(10) 
integer :: initial_toggle(10)
integer :: initialw_Panel1
integer :: initialw_Text1
integer :: initialw_Text2
integer :: initialw_Text3
integer :: initialw_Button1
integer :: initialw_Button2
integer :: initialw_Panel2
integer :: initialw_Text4
integer :: initialw_Text5
integer :: initialw_Button3
integer :: initialw_Button4

logical discprt,append

common/dp/discprt,append
	radio_text(1)='Browse for *.ini file (including autosim)'
	radio_text(2)='Do not use initiatialization file '
	radio_text(3)='Create new mechanism only '
	radio_text(4)='View or modify old mechanism only '
	radio_text(5)='Run DEMO fit (nicotinic,1 set) '
	radio_text(6)='Run DEMO fit (glycine, 4 sets) '
	radio_text(7)='Run DEMO repeated simulations (C-K) '
  	radio_text(8)='Use simulated data'

imes= hostnam(textcomp)
call getlog(textid)

drives=getdrivesqq()   
! Create main panel for form
 call gmSetGuiGridMode(GOff)
  call gDefineRGB(222, 0.682353, 0.843137, 1.000000)
 call gDefineRGB(41,0.5,0.5,0.75)!lilac
 	call gDefineRGB(42,0.65,0.65,0.85)!lilac
	call gDefineRGB(43,0.,0.25,0.)	!dark green
	call gDefineRGB(34,0.,0.4,0.)	!dark green
! Set up complex dialogue box initialw child of imecform1
 initialw = gmCreateComplexDialogueBox(Main,150,200,760,460, GALL, 'Welcome ', &
              	gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='MBIG1052')

   initialw_MainPanel=gmCreatePanel(initialw, 0, 0, 760, 460, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)

! Create panel initialw_Panel1 child of initialw_MainPanel
   initialw_Panel1 = gmCreatePanel(initialw_MainPanel, 0, 0, 760, 460, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillCol=150, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget initialw_Text1 child of initialw_Panel1
   initialw_Text1 = gmCreateTextEntry(initialw_Panel1, 140, 10, 90, 25,'HJCFIT ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=151 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   call gmSetFontAttribs(initialw_Text1, gmFontFace='Times New Roman', gmPointSize=22)

! Create text widget initialw_Text2 child of initialw_Panel1
   initialw_Text2 = gmCreateTextEntry(initialw_Panel1, 90, 40, 0, 25,'Intel Fortran. Version 0.8 (beta) ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget initialw_Text3 child of initialw_Panel1
   initialw_Text3 = gmCreateTextEntry(initialw_Panel1, 90, 60, 200, 25,'Copyright 2006 D. Colquhoun & I. Vais ', 255, GDISPLAY, &
      gmBack1Col=0, gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   call gmSetFontAttribs(initialw_Text3, gmPointSize=10)



! Create panel initialw_Panel2 child of initialw_Panel1
   initialw_Panel2 = gmCreatePanel(initialw_Panel1, 20, 100, 350, 350, &
   gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmFillCol=152, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget initialw_Text4 child of initialw_Panel2
   initialw_Text4 = gmCreateTextEntry(initialw_Panel2, 20, 10, 150, 25,'Name of initialisation file: ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget initialw_Text5 child of initialw_Panel2
   initialw_Text5 = gmCreateTextEntry(initialw_Panel2, 20, 40, 220, 25,inifile, 255, GEDIT, gmBack1Col=0, gmBack2Col=0, gmTextCol=16 , &
     gmVpos=GTOP,gmScrollMode=gon)

! Create button initialw_Button3 child of initialw_Panel2
   initialw_Button3 = gmCreatePushButton(initialw_Panel2, 250, 40, 75, 25, 'Browse', &
   gmVpos=GTOP,gmcallback=-13)
   
  ! call gmDefineCallback(initialw_Button3,-13 ) ! -50)
   call gmSetWidgetStatus(initialw_button3, GSELECTABLE)
	
  ! initialw_Panel0 = gmCreatePanel(initialw_Panel1, 30, 200, 310, 190, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmFillCol=152, &
   !   gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)
	initialw_Panel21 = gmCreatePanel(initialw_Panel2, 20, 80, 300, 260, &
   gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmFillCol=150, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)
   initialradio= gmCreateRadioBox(initialw_Panel21, 2, 2, 30, 250, gmType=GFREEFORM, gmBorderType=Gnone, &
      gmFillCol=150,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='', gmVpos=GTOP)
	initialw_Panel22 = gmCreatePanel(initialw_Panel21, 32, 2, 260, 250, &
   gmType=Gnone, gmLineBorder=GOUTEREDGE, gmFillCol=150, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)
	do i=1,8
		initial_Toggle(i) = gmCreateToggleButton(initialradio, 10, (i-1)*30+10 , 20, 25,' ', 0, &
		gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP,gmcallback=-58+i)
	enddo
	do i=1,4
	 iText = gmCreateTextEntry(initialw_Panel22, 10, (i-1)*30+10, 230, 25, radio_text(i), 255, &
	 Gdisplay, gmBack1Col=150, gmBack2Col=150, gmTextCol=1 , &
     gmVpos=GTOP)
	enddo
	do i=5,7
	 iText = gmCreateTextEntry(initialw_Panel22, 10, (i-1)*30+10, 230, 25, radio_text(i), 255,&
	  Gdisplay, gmBack1Col=150, gmBack2Col=150, gmTextCol=7 , &
     gmVpos=GTOP)

	enddo
	
		 iText = gmCreateTextEntry(initialw_Panel22, 10, 220, 230, 25, radio_text(8), 255, &
		 Gdisplay, gmBack1Col=150, gmBack2Col=150, gmTextCol=2 , &
     gmVpos=GTOP)
!	initial_Toggle(8) = gmCreateToggleButton(initialradio, 10, 220 , 250, 25, radio_text(8), 0, &
!		gmType=Gbeveldiamond,gmoffcol=41,gmtextcol=41, gmHpos=GCENTRE, gmVpos=GTOP,gmcallback=-50)

! Create button initialw_Button4 child of initialw_Panel2
  
!Create panel initialw_Panel2 child of initialw_MainPanel
   initialw_Panel2 = gmCreatePanel(initialw_Panel1, 390, 10, 350, 225, gmTitle='Printout file', gmType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=152, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget initialw_Text2 child of initialw_Panel2
   initialw_Text2 = gmCreateTextEntry(initialw_Panel2, 20, 30, 230, 25,pfilem, 255, GEDIT, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=Gon, gmVpos=GTOP)

! Create button initialw_Button3 child of initialw_Panel2
   initialw_Button3 = gmCreatePushButton(initialw_Panel2, 260, 30, 60, 25, 'Browse', &
   gmVpos=GTOP,gmcallback=-35)
call gmSetWidgetStatus(initialw_button3, GSELECTABLE)
! Create button initialw_Button4 child of initialw_Panel2
   initialw_Button4 = gmCreatePushButton(initialw_Panel2, 260, 60, 60, 25, 'View', &
   gmVpos=GTOP,gmcallback=8003)
call gmSetWidgetStatus(initialw_button4, GSELECTABLE)
! Create radio box child initialw_Radio2 child of initialw_Panel2
   initialw_Radio2 = gmCreateRadioBox(initialw_Panel2, 20, 100, 310, 110, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=150, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)
if(discprt) then
if(append) then
! Create toggle button initialw_Toggle3 child of initialw_Radio2
   initialw_Toggle3 = gmCreateToggleButton(initialw_Radio2, 10, 10, 230, 25, ' (1) Append to existing print file', 1, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initialw_Toggle4 child of initialw_Radio2
   initialw_Toggle4 = gmCreateToggleButton(initialw_Radio2, 10, 40, 230, 25, ' (2) Overwrite/new printout file', 0, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initialw_Toggle5 child of initialw_Radio2
   initialw_Toggle5 = gmCreateToggleButton(initialw_Radio2, 10, 70, 230, 25, ' (3) No printout file', 0, gmType=G3DRADIO, gmVpos=GTOP)
else
initialw_Toggle3 = gmCreateToggleButton(initialw_Radio2, 10, 10, 230, 25, ' (1) Append to existing print file', 0, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initialw_Toggle4 child of initialw_Radio2
   initialw_Toggle4 = gmCreateToggleButton(initialw_Radio2, 10, 40, 230, 25, ' (2) Overwrite/new printout file', 1, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initialw_Toggle5 child of initialw_Radio2
   initialw_Toggle5 = gmCreateToggleButton(initialw_Radio2, 10, 70, 230, 25, ' (3) No printout file', 0, gmType=G3DRADIO, gmVpos=GTOP)
endif
else
initialw_Toggle3 = gmCreateToggleButton(initialw_Radio2, 10, 10, 230, 25, ' (1) Append to existing print file', 0, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initialw_Toggle4 child of initialw_Radio2
   initialw_Toggle4 = gmCreateToggleButton(initialw_Radio2, 10, 40, 230, 25, ' (2) Overwrite/new printout file', 0, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initialw_Toggle5 child of initialw_Radio2
   initialw_Toggle5 = gmCreateToggleButton(initialw_Radio2, 10, 70, 230, 25, ' (3) No printout file', 1, gmType=G3DRADIO, gmVpos=GTOP)
endif
 initialw_Panel3 = gmCreatePanel(initialw_Panel1, 390,250, 350, 105, gmTitle=' ', gmType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=152, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget initialw_Text2 child of initialw_Panel2
 iniTextq = gmCreateTextEntry(initialw_Panel3, 20, 20, 280, 25,'Computer : '//textcomp, 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=Ghorizontal, gmVpos=GTOP)
  
 iniTextq = gmCreateTextEntry(initialw_Panel3, 20, 45, 280, 25,'User : '//textid, 255, GDIsplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=Ghorizontal, gmVpos=GTOP)
 
! iniTextq = gmCreateTextEntry(initialw_Panel3, 20, 90, 280, 25,'Drives : '//drives, 255, GDIsplay, gmBack1Col=0, gmBack2Col=0, &
 !     gmTextCol=16 ,gmScrollMode=Ghorizontal, gmVpos=GTOP)
 !iniTextq = gmCreateTextEntry(initialw_Panel3, 20, 70, 280, 25,'OS : ', 255, GDIsplay, gmBack1Col=0, gmBack2Col=0, &
 !     gmTextCol=16 ,gmScrollMode=Ghorizontal, gmVpos=GTOP)
 ! Create button initialw_Button1 child of initialw_Panel1
   initialw_Button1 = gmCreatePushButton(initialw_Panel1, 390, 40, 165, 25, 'Help', &
   gmVpos=Gbottom,gmcallback=999) !-7)
call gmSetWidgetStatus(initialw_button1, GSELECTABLE)

initialw_Button8 = gmCreatePushButton(initialw_Panel1, 570, 40, 165, 25,&
    'Save as default', gmVpos=Gbottom,gmcallback=-4)
! Create button initialw_Button2 child of initialw_Panel1
   initialw_Button2 = gmCreatePushButton(initialw_Panel1, 390, 10, 350, 25, 'Continue', &
   gmVpos=Gbottom,gmType=Guserdefined,gmOffcol=38 ,gmcallback=-11) !-40)
   call gmSetWidgetStatus(initialw_button2, GSELECTABLE)
 
 call gmSetGuiGridMode(GOn)
  call gmdrawwindow(initialw)
return
end
