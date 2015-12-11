subroutine print_options(iform,initwin)

use menu_f90

 
 
character*(100) text_tog(100)
integer intoggle(100)
call gDefineRGB(140, 0.011765, 0.827451, 0.803922)
call gDefineRGB(141, 0.192157, 0.984314, 0.964706)
call gDefineRGB(142, 0.286275, 0.784314, 0.803922)
call gmsetguigridmode(goff)
! Set up complex dialogue box initwin child of imecform1
 initwin = gmCreateComplexDialogueBox(iform,300,200,321,270, GALL, 'Options ', &
              	gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='MBIG1052')

! Create main panel for form
   inip=gmCreatePanel(initwin, 0, 0, 321, 270, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE,gmFillCol=140)


	  initwin_Radio1 = gmCreateRadioBox(inip, 20, 10, 280, 75, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=141, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

! Create toggle button initwin_Toggle3 child of initwin_Radio2
   inToggle(1) = gmCreateToggleButton(initwin_Radio1, 70, 10, 130, 25, 'Colour', 1, gmType=G3DRADIO, &
      gmVpos=GTOP,gmcallback=131)

! Create toggle button initwin_Toggle4 child of initwin_Radio2
   intoggle(2) = gmCreateToggleButton(initwin_Radio1, 70, 40, 130, 25, 'Black and white', 0, gmType=G3DRADIO, &
      gmVpos=GTOP,gmcallback=132)

initwin_Radio2 = gmCreateRadioBox(inip, 20, 90, 280, 110, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=141, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

! Create toggle button initwin_Toggle3 child of initwin_Radio2
   inToggle(3) = gmCreateToggleButton(initwin_Radio2, 70, 10, 130, 25, 'Current page', 1, gmType=G3DRADIO, &
      gmVpos=GTOP,gmcallback=133)

! Create toggle button initwin_Toggle4 child of initwin_Radio2
   intoggle(4) = gmCreateToggleButton(initwin_Radio2, 70, 40, 130, 25, 'All pages', 0, gmType=G3DRADIO, &
      gmVpos=GTOP,gmcallback=135)

! Create toggle button initwin_Toggle5 child of initwin_Radio2
   inToggle(5) = gmCreateToggleButton(initwin_Radio2, 70, 70, 130, 25, 'Select page', 0, &
   gmType=G3DRADIO, gmVpos=GTOP,gmcallback=134)

 iStatic = gmCreateTextEntry(inip, 20, 210, 300, 25,'Please select Landscape from printer Setup', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
 
 itogButton = gmCreatePushButton(inip, 160, 240, 140, 25, 'Continue', &
   gmVpos=GTOP,gmType=GDEFAULTBUTTON,gmcallback=136) !-40)
  call gmSetWidgetStatus(itogbutton, GSELECTABLE)
   itogButton = gmCreatePushButton(inip, 20, 240, 140, 25, 'Exit', &
   gmVpos=GTOP,gmType=GDEFAULTBUTTON,gmcallback=140) !-40)
  call gmSetWidgetStatus(itogbutton, GSELECTABLE)
 call gmSetGuiGridMode(GOn)
   call gmdrawwindow(initwin)

end