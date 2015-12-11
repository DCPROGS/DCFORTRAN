! Last created on Fri Aug 08 15:28:35 2003

! Using GINOMENU Studio Version  v4.0
! Global Definitions
Module mstudio
use menu_f90


! Widget identifier definitions
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
integer :: callid
End Module

!Program D:\Compaq\hjcfit\initialize2
use mstudio
use menu_f90


! Initialise Gino, Device & Menu
   call gOpenGino
   call gGuiwin
   call gmInitializeMenu

! Define required grid
   call gmDefineGuiGrid(36,27)
   call gmSetGuiGridMode(0)

! Define user defined colour indices used
   call gDefineRGB(20, 0.011765, 0.827451, 0.803922)
   call gDefineRGB(21, 0.192157, 0.984314, 0.964706)
   call gDefineRGB(22, 0.286275, 0.784314, 0.803922)
   call gDefineRGB(23, 0.011765, 0.827451, 0.764706)
   call gDefineRGB(24, 1.000000, 0.611765, 0.431373)
   call gDefineRGB(25, 1.000000, 0.737255, 0.611765)
   call gDefineRGB(26, 0.992157, 0.839216, 0.729412)

! Generate Forms
   call Generateimecform1
   call Generateinitwin

! Start management
   call gmManage

! Action loop
   do
      callid=gmAction(callid)

! Code to handle callbacks

      if(callid == -1) exit
   end do

! Close device MENU and GINO, and end program
   call gmCloseMenu
   call gCloseDevice
   call gCloseGino

stop
end
subroutine Generateimecform1
use menu_f90
use mstudio

! Set up master window imecform1
   imecform1 = gmCreateMasterWindow(512, 456, 384, 387, GALL, 'Mechanism', gmVpos=GTOP, gmInitState=GNORMAL, &
      gmIconFormat=GDLLICON, gmIconFile='Gee')

! Create main panel for form
   imecform1_MainPanel=gmCreatePanel(imecform1, 0, 0, 384, 387, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, &
      gmType=GNOBOUNDARY, gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)

! Create panel imecform1_Panel1 child of imecform1_MainPanel
   imecform1_Panel1 = gmCreatePanel(imecform1_MainPanel, 0, 0, 380, 230, gmTitle='Reaction mechanism', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=24, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget imecform1_Text1 child of imecform1_Panel1
   imecform1_Text1 = gmCreateTextEntry(imecform1_Panel1, 20, 30, 50, 25,'File ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget imecform1_Text2 child of imecform1_Panel1
   imecform1_Text2 = gmCreateTextEntry(imecform1_Panel1, 20, 60, 40, 25,'Title ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget imecform1_Text3 child of imecform1_Panel1
   imecform1_Text3 = gmCreateTextEntry(imecform1_Panel1, 80, 30, 280, 25,' ', 255, GEDIT, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget imecform1_Text4 child of imecform1_Panel1
   imecform1_Text4 = gmCreateTextEntry(imecform1_Panel1, 80, 60, 280, 25,' ', 255, GEDIT, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create radio box child imecform1_Radio1 child of imecform1_Panel1
   imecform1_Radio1 = gmCreateRadioBox(imecform1_Panel1, 20, 100, 340, 110, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=26, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

! Create toggle button imecform1_Toggle1 child of imecform1_Radio1
   imecform1_Toggle1 = gmCreateToggleButton(imecform1_Radio1, 40, 10, 270, 25, 'Use mechanism as in last run', 1, &
      gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button imecform1_Toggle2 child of imecform1_Radio1
   imecform1_Toggle2 = gmCreateToggleButton(imecform1_Radio1, 40, 40, 270, 25, 'Choose a mechanism from those already defined', &
      0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button imecform1_Toggle3 child of imecform1_Radio1
   imecform1_Toggle3 = gmCreateToggleButton(imecform1_Radio1, 40, 70, 280, 25, 'Define a completely new mechanism', 0, &
      gmType=G3DRADIO, gmVpos=GTOP)

! Create radio box child imecform1_Radio2 child of imecform1_MainPanel
   imecform1_Radio2 = gmCreateRadioBox(imecform1_MainPanel, 0, 230, 380, 125, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=24, gmFillBorder=GOUTEREDGE, gmTitle='Mechanism to be fitted', gmVpos=GTOP)

! Create toggle button imecform1_Toggle4 child of imecform1_Radio2
   imecform1_Toggle4 = gmCreateToggleButton(imecform1_Radio2, 60, 30, 290, 25, 'Use the same as above', 1, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button imecform1_Toggle5 child of imecform1_Radio2
   imecform1_Toggle5 = gmCreateToggleButton(imecform1_Radio2, 60, 60, 300, 25, 'Choose a mechanism from those already defined', &
      0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button imecform1_Toggle6 child of imecform1_Radio2
   imecform1_Toggle6 = gmCreateToggleButton(imecform1_Radio2, 60, 90, 300, 25, 'Define a completely new mechanism', 0, &
      gmType=G3DRADIO, gmVpos=GTOP)

! Create button imecform1_Button1 child of imecform1_MainPanel
   imecform1_Button1 = gmCreatePushButton(imecform1_MainPanel, 0, 360, 380, 25, 'Continue', gmVpos=GTOP)
   call gmSetWidgetStatus(imecform1_Button1, GCHECKED)
return
end
subroutine Generateinitwin
use menu_f90
use mstudio

! Set up complex dialogue box initwin child of imecform1
, GALL, 'Setup', gmVpos=GTOP, gmQuitMode=GHIDE, gmInitState=GNORMAL, gmIconFormat=GDLLICON, gmIconFile='Gee')

! Create main panel for form
   initwin_MainPanel=gmCreatePanel(initwin, 0, 0, 421, 608, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)

! Create panel initwin_Panel1 child of initwin_MainPanel
   initwin_Panel1 = gmCreatePanel(initwin_MainPanel, 0, 0, 420, 160, gmTitle='Initialization file', gmType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=20, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget initwin_Text1 child of initwin_Panel1
   initwin_Text1 = gmCreateTextEntry(initwin_Panel1, 20, 30, 200, 25,'hjcfit.ini ', 255, GEDIT, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create button initwin_Button1 child of initwin_Panel1
   initwin_Button1 = gmCreatePushButton(initwin_Panel1, 240, 30, 75, 25, 'Browse', gmVpos=GTOP)

! Create button initwin_Button2 child of initwin_Panel1
   initwin_Button2 = gmCreatePushButton(initwin_Panel1, 330, 30, 75, 25, 'View', gmVpos=GTOP)

! Create radio box child initwin_Radio1 child of initwin_Panel1
   initwin_Radio1 = gmCreateRadioBox(initwin_Panel1, 20, 70, 380, 70, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=21, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

! Create toggle button initwin_Toggle1 child of initwin_Radio1
   initwin_Toggle1 = gmCreateToggleButton(initwin_Radio1, 70, 10, 220, 25, 'Read existing ini file', 1, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initwin_Toggle2 child of initwin_Radio1
   initwin_Toggle2 = gmCreateToggleButton(initwin_Radio1, 70, 40, 220, 25, 'No ini file', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create panel initwin_Panel2 child of initwin_MainPanel
   initwin_Panel2 = gmCreatePanel(initwin_MainPanel, 0, 160, 420, 190, gmTitle='Printout file', gmType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=20, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget initwin_Text2 child of initwin_Panel2
   initwin_Text2 = gmCreateTextEntry(initwin_Panel2, 20, 30, 200, 25,'hjcfit.txt ', 255, GEDIT, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create button initwin_Button3 child of initwin_Panel2
   initwin_Button3 = gmCreatePushButton(initwin_Panel2, 240, 30, 75, 25, 'Browse', gmVpos=GTOP)

! Create button initwin_Button4 child of initwin_Panel2
   initwin_Button4 = gmCreatePushButton(initwin_Panel2, 330, 30, 75, 25, 'View', gmVpos=GTOP)

! Create radio box child initwin_Radio2 child of initwin_Panel2
   initwin_Radio2 = gmCreateRadioBox(initwin_Panel2, 20, 70, 380, 110, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=21, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

! Create toggle button initwin_Toggle3 child of initwin_Radio2
   initwin_Toggle3 = gmCreateToggleButton(initwin_Radio2, 70, 10, 230, 25, 'Append to existing print file', 1, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initwin_Toggle4 child of initwin_Radio2
   initwin_Toggle4 = gmCreateToggleButton(initwin_Radio2, 70, 40, 230, 25, 'Overwrite (new) printout file', 0, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initwin_Toggle5 child of initwin_Radio2
   initwin_Toggle5 = gmCreateToggleButton(initwin_Radio2, 70, 70, 230, 25, 'No printout file', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create radio box child initwin_Radio3 child of initwin_MainPanel
   initwin_Radio3 = gmCreateRadioBox(initwin_MainPanel, 0, 350, 420, 125, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=20, gmFillBorder=GOUTEREDGE, gmTitle='Options', gmVpos=GTOP)

! Create toggle button initwin_Toggle6 child of initwin_Radio3
   initwin_Toggle6 = gmCreateToggleButton(initwin_Radio3, 40, 30, 290, 25, 'Fit data', 1, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button initwin_Toggle7 child of initwin_Radio3
   initwin_Toggle7 = gmCreateToggleButton(initwin_Radio3, 40, 60, 270, 25, 'Show data and HJC distributions', 0, &
      gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button initwin_Toggle8 child of initwin_Radio3
   initwin_Toggle8 = gmCreateToggleButton(initwin_Radio3, 40, 90, 290, 25, 'Show only HJC distributions', 0, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create radio box child initwin_Radio4 child of initwin_MainPanel
   initwin_Radio4 = gmCreateRadioBox(initwin_MainPanel, 0, 480, 420, 100, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=20, gmFillBorder=GOUTEREDGE, gmTitle='Data type', gmVpos=GTOP)

! Create toggle button initwin_Toggle9 child of initwin_Radio4
   initwin_Toggle9 = gmCreateToggleButton(initwin_Radio4, 40, 30, 290, 25, 'Use experimental data (scn files)', 1, &
      gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button initwin_Toggle10 child of initwin_Radio4
   initwin_Toggle10 = gmCreateToggleButton(initwin_Radio4, 40, 70, 290, 25, 'Use simulated data', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create button initwin_Button5 child of initwin_MainPanel
   initwin_Button5 = gmCreatePushButton(initwin_MainPanel, 0, 580, 210, 25, 'Advanced settings', gmVpos=GTOP)

! Create button initwin_Button6 child of initwin_MainPanel
   initwin_Button6 = gmCreatePushButton(initwin_MainPanel, 210, 580, 210, 25, 'Continue', gmVpos=GTOP)
   call gmSetWidgetStatus(initwin_Button6, GCHECKED)
return
end
