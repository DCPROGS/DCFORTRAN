
subroutine ini_mechanism(main,imecform1,qmec,im,imecform1_Toggle1,imecform1_Toggle2,&
	imecform1_Toggle3,imecform1_Toggle4,imecform1_Toggle5,imecform1_Toggle6,readmec,&
	imod,title,rtitle,imod11,mtitle,rtitle1)
use menu_f90


integer :: imecform1
integer :: imecform1_MainPanel
integer :: imecform1_Panel1
integer :: imecform1_Text1
integer :: imecform1_Text2
integer :: imecform1_Text3
integer :: imecform1_Text4
integer :: imecform1_Radio1
integer :: imecform1_Toggle1
integer :: imecform1_Toggle2
integer :: imecform1_Toggle3
integer :: imecform1_Radio2
integer :: imecform1_Toggle4
integer :: imecform1_Toggle5
integer :: imecform1_Toggle6
integer :: imecform1_Button1
character*74 title,mtitle,rtitle1,rtitle
character*60 qmec
logical readmec
! Define user defined colour indices used
call gDefineRGB(38,1.,0.5,0.25)	!orange
	call gmsetguigridmode(goff)

if(readmec) then
	ihmp=260
else
	ihmp=160
endif
if(im.eq.-1) then
	ihmec=ihmp+30
else
	ihmec=2*ihmp+30
endif

! Set up master window imecform1
   imecform1 = gmCreateComplexDialogueBox(Main,100,150,424,ihmec, Gminimize, 'Mechanism', &
              	gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='MBIG1052')

! Create main panel for form
   imecform1_MainPanel=gmCreatePanel(imecform1, 0, 0, 424, ihmec, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, &
      gmType=GNOBOUNDARY, gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)

! Create panel imecform1_Panel1 child of imecform1_MainPanel
   imecform1_Panel1 = gmCreatePanel(imecform1_MainPanel, 0, 0, 420, ihmp, gmTitle='Reaction mechanism', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=144, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)
if(readmec) then
! Create text widget imecform1_Text1 child of imecform1_Panel1
   imecform1_Text1 = gmCreateTextEntry(imecform1_Panel1, 20, 30, 50, 25,'File ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget imecform1_Text2 child of imecform1_Panel1
   imecform1_Text2 = gmCreateTextEntry(imecform1_Panel1, 20, 60, 40, 25,'Model ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
imecform1_Text2 = gmCreateTextEntry(imecform1_Panel1, 20, 90, 40, 25,'Rate: ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget imecform1_Text3 child of imecform1_Panel1
   imecform1_Text3 = gmCreateTextEntry(imecform1_Panel1, 80, 30, 320, 25,qmec, 255, gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget imecform1_Text4 child of imecform1_Panel1
   imecform1_Text4 = gmCreateTextEntry(imecform1_Panel1, 80, 60, 320, 25,mtitle , 255, gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
	 !  Create text widget imecform1_Text4 child of imecform1_Panel1
   imecform1_Text4 = gmCreateTextEntry(imecform1_Panel1, 80, 90, 320, 25,rtitle1 , 255, gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
endif
! Create radio box child imecform1_Radio1 child of imecform1_Panel1
   imecform1_Radio1 = gmCreateRadioBox(imecform1_Panel1, 40, ihmp-120, 340, 110, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=146, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)
if(readmec) then
	ib1=gon
	ib2=goff
else
	ib1=goff
	ib2=gon
		

endif
! Create toggle button imecform1_Toggle1 child of imecform1_Radio1
if(readmec) then
   imecform1_Toggle1 = gmCreateToggleButton(imecform1_Radio1, 40, 10, 270, 25, &
   'Use mechanism as in last run', 1, &
      gmType=G3DRADIO, gmVpos=GTOP)
	  ! Create toggle button imecform1_Toggle2 child of imecform1_Radio1
   imecform1_Toggle2 = gmCreateToggleButton(imecform1_Radio1, 40, 40, 270, 25, &
   'Choose a mechanism from those defined', &
      0, gmType=G3DRADIO, gmVpos=GTOP)
else
 imecform1_Toggle1 = gmCreateToggleButton(imecform1_Radio1, 40, 10, 270, 25, &
   'Use mechanism as in last run', 0, &
      gmType=G3DRADIO, gmVpos=GTOP)
	  ! Create toggle button imecform1_Toggle2 child of imecform1_Radio1
   imecform1_Toggle2 = gmCreateToggleButton(imecform1_Radio1, 40, 40, 270, 25, &
   'Choose a mechanism from those defined', &
      1, gmType=G3DRADIO, gmVpos=GTOP)
	  	call gmSetWidgetStatus(imecform1_Toggle1, Gunselectable)

endif


! Create toggle button imecform1_Toggle3 child of imecform1_Radio1
   imecform1_Toggle3 = gmCreateToggleButton(imecform1_Radio1, 40, 70, 280, 25, &
   'Define a completely new mechanism', 0, &
      gmType=G3DRADIO, gmVpos=GTOP)
	  


if(im.ne.-1) then
ihmp2=ihmp

  imecform1_Panel2 = gmCreatePanel(imecform1_MainPanel, 0, ihmp2, 420, ihmp, gmTitle='Mechanism to be fitted', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=144, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)
if(readmec) then
! Create text widget imecform1_Text1 child of imecform1_Panel1
   imecform1_Text1 = gmCreateTextEntry(imecform1_Panel2, 20, 30, 50, 25,'File: ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget imecform1_Text2 child of imecform1_Panel1
   imecform1_Text2 = gmCreateTextEntry(imecform1_Panel2, 20, 60, 40, 25,'Model: ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
!Create text widget imecform1_Text2 child of imecform1_Panel1
   imecform1_Text2 = gmCreateTextEntry(imecform1_Panel2, 20, 90, 40, 25,'Rate: ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
! Create text widget imecform1_Text3 child of imecform1_Panel1
   imecform1_Text3 = gmCreateTextEntry(imecform1_Panel2, 80, 30, 320, 25,qmec, 255, gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget imecform1_Text4 child of imecform1_Panel1
   imecform1_Text4 = gmCreateTextEntry(imecform1_Panel2, 80, 60, 320, 25,title , 255, gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
	  imecform1_Text4 = gmCreateTextEntry(imecform1_Panel2, 80, 90, 320, 25,rtitle , 255, gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
endif
! Create radio box child imecform1_Radio2 child of imecform1_MainPanel
   imecform1_Radio2 = gmCreateRadioBox(imecform1_Panel2, 40, ihmp-120, 340, 110, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=146, gmFillBorder=GOUTEREDGE, gmTitle='', gmVpos=GTOP)

! Create toggle button imecform1_Toggle4 child of imecform1_Radio2
if(readmec) then
   imecform1_Toggle4 = gmCreateToggleButton(imecform1_Radio2, 40, 10, 280, 25, &
   'Use the same as above', 1, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button imecform1_Toggle5 child of imecform1_Radio2
   imecform1_Toggle5 = gmCreateToggleButton(imecform1_Radio2, 40, 40, 280, 25, &
   'Choose a mechanism from those defined', &
      0, gmType=G3DRADIO, gmVpos=GTOP)
else
 imecform1_Toggle4 = gmCreateToggleButton(imecform1_Radio2, 40, 10, 280, 25, &
   'Use the same as above', 0, gmType=G3DRADIO, &
      gmVpos=GTOP)
	call gmSetWidgetStatus(imecform1_Toggle4, Gunselectable)
! Create toggle button imecform1_Toggle5 child of imecform1_Radio2
   imecform1_Toggle5 = gmCreateToggleButton(imecform1_Radio2, 40, 40, 280, 25, &
   'Choose a mechanism from those defined', &
      1, gmType=G3DRADIO, gmVpos=GTOP)
endif
! Create toggle button imecform1_Toggle6 child of imecform1_Radio2
   imecform1_Toggle6 = gmCreateToggleButton(imecform1_Radio2, 40, 70, 280, 25, 'Define a completely new mechanism', 0, &
      gmType=G3DRADIO, gmVpos=GTOP)
	
	 
endif
! Create button imecform1_Button1 child of imecform1_MainPanel
if(im.eq.-1) then
ihb=235
else
ihb=360
endif
   imecform1_Button1 = gmCreatePushButton(imecform1_MainPanel, 2, 0, 420, 25, &
   'Continue', gmVpos=Gbottom,gmType=Guserdefined,gmoffcol=38,gmcallback=-291)
  call gmSetWidgetStatus(imecform1_button1, GSELECTABLE)
   call gmsetguigridmode(gon)
call gmdrawwindow(imecform1)
return
end
