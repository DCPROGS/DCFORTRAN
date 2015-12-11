! Last created on Wed Dec 22 16:16:02 2004

! Using GINOMENU Studio Version  v4.0e
! Global Definitions
Module mstudio
use menu_f90


! Widget identifier definitions
integer :: Form1
integer :: Form1_MainPanel
integer :: Form1_Radio1
integer :: Form1_Toggle1
integer :: Form1_Toggle2
integer :: Form1_Toggle3
integer :: Form1_Toggle4
integer :: Form1_Toggle5
integer :: Form1_Panel1
integer :: Form1_Text1
integer :: Form1_Text2
integer :: Form1_Text3
integer :: Form1_Value1
integer :: Form1_Value2
integer :: Form1_Value3
integer :: Form1_Radio2
integer :: Form1_Toggle7
integer :: Form1_Toggle8
integer :: Form1_Radio3
integer :: Form1_Toggle9
integer :: Form1_Toggle10
integer :: Form1_Button1
integer :: Form1_Button2
integer :: callid
End Module

!Program D:\FortWin\common_files\plotprop
use mstudio
use menu_f90


! Initialise Gino, Device & Menu
   call gOpenGino
   call gGuiwin
   call gmInitializeMenu

! Define required grid
   call gmDefineGuiGrid(36,27)
   call gmSetGuiGridMode(0)

! Generate Forms
   call GenerateForm1

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
subroutine GenerateForm1
use menu_f90
use mstudio

! Set up master window Form1
   Form1 = gmCreateMasterWindow(323, 199, 383, 467, GALL, 'Form 1', gmVpos=GTOP, gmInitState=GNORMAL, gmIconFormat=GDLLICON, &
      gmIconFile='Gee')

! Create main panel for form
   Form1_MainPanel=gmCreatePanel(Form1, 0, 0, 383, 467, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)

! Create radio box child Form1_Radio1 child of Form1_MainPanel
   Form1_Radio1 = gmCreateRadioBox(Form1_MainPanel, 0, 0, 380, 180, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Radio Box 1', gmVpos=GTOP)

! Create toggle button Form1_Toggle1 child of Form1_Radio1
   Form1_Toggle1 = gmCreateToggleButton(Form1_Radio1, 20, 30, 350, 25, 'Toggle 1', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button Form1_Toggle2 child of Form1_Radio1
   Form1_Toggle2 = gmCreateToggleButton(Form1_Radio1, 20, 60, 350, 25, 'Toggle 2', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button Form1_Toggle3 child of Form1_Radio1
   Form1_Toggle3 = gmCreateToggleButton(Form1_Radio1, 20, 90, 350, 25, 'Toggle 3', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button Form1_Toggle4 child of Form1_Radio1
   Form1_Toggle4 = gmCreateToggleButton(Form1_Radio1, 20, 120, 350, 25, 'Toggle 4', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button Form1_Toggle5 child of Form1_Radio1
   Form1_Toggle5 = gmCreateToggleButton(Form1_Radio1, 20, 150, 350, 25, 'Toggle 5', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create panel Form1_Panel1 child of Form1_MainPanel
   Form1_Panel1 = gmCreatePanel(Form1_MainPanel, 0, 180, 380, 120, gmTitle='Panel 1', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget Form1_Text1 child of Form1_Panel1
   Form1_Text1 = gmCreateTextEntry(Form1_Panel1, 20, 20, 230, 25,'Text Entry 1 ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget Form1_Text2 child of Form1_Panel1
   Form1_Text2 = gmCreateTextEntry(Form1_Panel1, 20, 50, 240, 25,'Text Entry 2 ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget Form1_Text3 child of Form1_Panel1
   Form1_Text3 = gmCreateTextEntry(Form1_Panel1, 20, 80, 230, 25,'Text Entry 3 ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create value entry Form1_Value1 child of Form1_Panel1
   Form1_Value1 = gmCreateValueEntry(Form1_Panel1, 270, 20, 100, 25, 0.000000, 8, 3, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create value entry Form1_Value2 child of Form1_Panel1
   Form1_Value2 = gmCreateValueEntry(Form1_Panel1, 270, 50, 100, 25, 0.000000, 8, 3, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create value entry Form1_Value3 child of Form1_Panel1
   Form1_Value3 = gmCreateValueEntry(Form1_Panel1, 270, 80, 100, 25, 0.000000, 8, 3, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create radio box child Form1_Radio2 child of Form1_MainPanel
   Form1_Radio2 = gmCreateRadioBox(Form1_MainPanel, 0, 300, 380, 70, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Radio Box 2', gmVpos=GTOP)

! Create toggle button Form1_Toggle7 child of Form1_Radio2
   Form1_Toggle7 = gmCreateToggleButton(Form1_Radio2, 20, 30, 100, 25, 'Toggle 7', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button Form1_Toggle8 child of Form1_Radio2
   Form1_Toggle8 = gmCreateToggleButton(Form1_Radio2, 150, 30, 100, 25, 'Toggle 8', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create radio box child Form1_Radio3 child of Form1_MainPanel
   Form1_Radio3 = gmCreateRadioBox(Form1_MainPanel, 0, 370, 380, 70, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Radio Box 3', gmVpos=GTOP)

! Create toggle button Form1_Toggle9 child of Form1_Radio3
   Form1_Toggle9 = gmCreateToggleButton(Form1_Radio3, 20, 30, 100, 25, 'Toggle 9', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button Form1_Toggle10 child of Form1_Radio3
   Form1_Toggle10 = gmCreateToggleButton(Form1_Radio3, 150, 30, 100, 25, 'Toggle 10', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create button Form1_Button1 child of Form1_MainPanel
   Form1_Button1 = gmCreatePushButton(Form1_MainPanel, 190, 440, 190, 25, 'Button 1', gmVpos=GTOP)

! Create button Form1_Button2 child of Form1_MainPanel
   Form1_Button2 = gmCreatePushButton(Form1_MainPanel, 0, 440, 190, 25, 'Button 2', gmVpos=GTOP)
return
end
