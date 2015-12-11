subroutine mechw(main,imecform1,title,im,imecform1_Toggle1,imecform1_Toggle2,&
	imecform1_Toggle3,imecform1_Toggle4,imecform1_Toggle5)
! Last created on Wed Aug 06 17:35:39 2003

! Using GINOMENU Studio Version  v4.0
! Global Definitions

use menu_f90

character title*60
call gmSetGuiGridMode(GOff)

! Set up master window imecform1
   imecform1 = gmCreateMDIComplexDialogueBox(Main,270,70,251,294, GALL, 'Mechanism', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052')

! Create main panel for form
   imecform1_MainPanel=gmCreatePanel(imecform1, 0, 0, 231, 254, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)

! Create radio box child imecform1_Radio1 child of imecform1_MainPanel
   imecform1_Radio1 = gmCreateRadioBox(imecform1_MainPanel, 0, 0, 250, 110, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=130, gmFillBorder=GOUTEREDGE, gmTitle=title, gmVpos=GTOP)

! Create toggle button imecform1_Toggle1 child of imecform1_Radio1
   imecform1_Toggle1 = gmCreateToggleButton(imecform1_Radio1, 40, 30, 100, 25, 'Create new', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button imecform1_Toggle2 child of imecform1_Radio1
   imecform1_Toggle2 = gmCreateToggleButton(imecform1_Radio1, 40, 70, 100, 25, 'Read old', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create radio box child imecform1_Radio2 child of imecform1_MainPanel
   imecform1_Radio2 = gmCreateRadioBox(imecform1_MainPanel, 0, 110, 250, 160, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=131, gmFillBorder=GOUTEREDGE, gmTitle='Define mechanism for fitting', gmVpos=GTOP)

! Create toggle button imecform1_Toggle3 child of imecform1_Radio2
   imecform1_Toggle3 = gmCreateToggleButton(imecform1_Radio2, 30, 30, 100, 25, 'Use the same as above', 0, gmType=G3DRADIO, gmVpos=GTOP)
   if(im.eq.-1) then
   call gmSetWidgetStatus(imecform1_Toggle3, GUNSELECTABLE)
	else
	 call gmSetWidgetStatus(imecform1_Toggle3, GSELECTABLE)

	endif
! Create toggle button imecform1_Toggle4 child of imecform1_Radio2
   imecform1_Toggle4 = gmCreateToggleButton(imecform1_Radio2, 30, 70, 100, 25, 'Create new ', 0, gmType=G3DRADIO, gmVpos=GTOP)
   if(im.eq.-1) then
   call gmSetWidgetStatus(imecform1_Toggle4, GUNSELECTABLE)
else
 call gmSetWidgetStatus(imecform1_Toggle4, GSELECTABLE)

endif

 imecform1_Toggle5 = gmCreateToggleButton(imecform1_Radio2, 30, 110, 100, 25, 'Read old', 0, gmType=G3DRADIO, gmVpos=GTOP)
if(im.eq.-1) then
   call gmSetWidgetStatus(imecform1_Toggle5, GUNSELECTABLE)
else
 call gmSetWidgetStatus(imecform1_Toggle5, GSELECTABLE)

endif
! Create button imecform1_Button1 child of imecform1_MainPanel
   imecform1_Button1 = gmCreatePushButton(imecform1_MainPanel, 0, 270, 250, 25, 'OK',&
    gmVpos=GTOP,gmcallback=-15)
call gmSetWidgetStatus(imecform1_button1, GSELECTABLE)

call gmSetGuiGridMode(GOn)

return

end
