subroutine welcome(main,initialw,initialw_MainPanel,inifile, initialw_Text5,initial_toggle)
use menu_f90
use dflib
use dfport
character	drives*26,text1*40,text2*40
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
character*40 inifile
common/details/text1,text2,drives

	radio_text(1)='Browse for initialization file'
	radio_text(2)='Do not use initiatialization file '
	radio_text(3)='Create/View mechanism only '
	radio_text(4)='Run DEMO fit (nicotinic,1 set) '
	radio_text(5)='Run DEMO fit (glycine, 4 sets) '

imes= hostnam(text1)
call getlog(text2)

drives=getdrivesqq()   
! Create main panel for form
 call gmSetGuiGridMode(GOff)
  call gDefineRGB(222, 0.682353, 0.843137, 1.000000)
 
! Set up complex dialogue box initialw child of imecform1
 initialw = gmCreateComplexDialogueBox(Main,100,150,370,560, GALL, 'Initial Settings ', &
              	gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='MBIG1052')

   initialw_MainPanel=gmCreatePanel(initialw, 0, 0, 370, 560, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)

! Create panel initialw_Panel1 child of initialw_MainPanel
   initialw_Panel1 = gmCreatePanel(initialw_MainPanel, 0, 0, 370, 560, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillCol=150, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget initialw_Text1 child of initialw_Panel1
   initialw_Text1 = gmCreateTextEntry(initialw_Panel1, 140, 10, 90, 25,'HJCFIT ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=151 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   call gmSetFontAttribs(initialw_Text1, gmFontFace='Times New Roman', gmPointSize=22)

! Create text widget initialw_Text2 child of initialw_Panel1
   initialw_Text2 = gmCreateTextEntry(initialw_Panel1, 140, 40, 0, 25,'Version 0.8 (beta) ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget initialw_Text3 child of initialw_Panel1
   initialw_Text3 = gmCreateTextEntry(initialw_Panel1, 90, 60, 200, 25,'Copyright 2006 D. Colquhoun & I. Vais ', 255, GDISPLAY, &
      gmBack1Col=0, gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   call gmSetFontAttribs(initialw_Text3, gmPointSize=8)



! Create panel initialw_Panel2 child of initialw_Panel1
   initialw_Panel2 = gmCreatePanel(initialw_Panel1, 30, 100, 310, 260, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmFillCol=152, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget initialw_Text4 child of initialw_Panel2
   initialw_Text4 = gmCreateTextEntry(initialw_Panel2, 30, 10, 150, 25,'Name of initialisation file: ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget initialw_Text5 child of initialw_Panel2
   initialw_Text5 = gmCreateTextEntry(initialw_Panel2, 30, 40, 150, 25,inifile, 255, GEDIT, gmBack1Col=0, gmBack2Col=0, gmTextCol=16 , &
     gmVpos=GTOP,gmScrollMode=goff)

! Create button initialw_Button3 child of initialw_Panel2
   initialw_Button3 = gmCreatePushButton(initialw_Panel2, 210, 40, 75, 25, 'Browse', &
   gmVpos=GTOP)
   
   call gmDefineCallback(initialw_Button3,-13 ) ! -50)
   call gmSetWidgetStatus(initialw_button3, GSELECTABLE)

  ! initialw_Panel0 = gmCreatePanel(initialw_Panel1, 30, 200, 310, 190, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmFillCol=152, &
   !   gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

   initialradio= gmCreateRadioBox(initialw_Panel2, 20, 80, 270, 165, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmFillCol=150,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='', gmVpos=GTOP)
	do i=1,5
		initial_Toggle(i) = gmCreateToggleButton(initialradio, 10, (i-1)*30+10 , 250, 25, radio_text(i), 0, &
		gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP,gmcallback=-56+i)
	enddo

	
! Create button initialw_Button4 child of initialw_Panel2
  
  
 initwin_Panel3 = gmCreatePanel(initialw_Panel1, 30,370, 310, 130, gmTitle='Setup for this computer:', gmType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=152, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget initwin_Text2 child of initwin_Panel2
 iniTextq = gmCreateTextEntry(initwin_Panel3, 20, 30, 280, 25,'Computer : '//text1, 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=Ghorizontal, gmVpos=GTOP)
  
 iniTextq = gmCreateTextEntry(initwin_Panel3, 20, 60, 280, 25,'User : '//text2, 255, GDIsplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=Ghorizontal, gmVpos=GTOP)
 
! iniTextq = gmCreateTextEntry(initwin_Panel3, 20, 90, 280, 25,'Drives : '//drives, 255, GDIsplay, gmBack1Col=0, gmBack2Col=0, &
 !     gmTextCol=16 ,gmScrollMode=Ghorizontal, gmVpos=GTOP)
 iniTextq = gmCreateTextEntry(initwin_Panel3, 20, 90, 280, 25,'OS : ', 255, GDIsplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=Ghorizontal, gmVpos=GTOP)
 ! Create button initialw_Button1 child of initialw_Panel1
   initialw_Button1 = gmCreatePushButton(initialw_Panel1, 30, 10, 75, 25, 'Help', &
   gmVpos=Gbottom,gmcallback=-14) !-7)
call gmSetWidgetStatus(initialw_button1, GSELECTABLE)

! Create button initialw_Button2 child of initialw_Panel1
   initialw_Button2 = gmCreatePushButton(initialw_Panel1, 265, 10, 75, 25, 'Continue', &
   gmVpos=Gbottom,gmType=GDEFAULTBUTTON ,gmcallback=-11) !-40)
   call gmSetWidgetStatus(initialw_button2, GSELECTABLE)
 
 call gmSetGuiGridMode(GOn)
  call gmdrawwindow(initialw)
return
end
