! Last created on Wed Oct 20 18:05:06 2004

! Using GINOMENU Studio Version  v4.0e
! Global Definitions
Module mstudio
use menu_f90


! Widget identifier definitions
integer :: Form1
integer :: Form1_MainPanel
integer :: Form1_Panel1
integer :: Form1_Text1
integer :: Form1_Text2
integer :: Form1_Text3
integer :: Form1_Text4
integer :: Form1_Text5
integer :: Form1_Text6
integer :: Form1_Button1
integer :: callid
End Module

!Program D:\FortWin\common_files\edit
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
   Form1 = gmCreateMasterWindow(314, 234, 343, 284, GALL, '', gmVpos=GTOP, gmInitState=GNORMAL, gmIconFormat=GDLLICON, &
      gmIconFile='MBIG1036')

! Create main panel for form
   Form1_MainPanel=gmCreatePanel(Form1, 0, 0, 343, 284, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)

! Create panel Form1_Panel1 child of Form1_MainPanel
   Form1_Panel1 = gmCreatePanel(Form1_MainPanel, 0, 0, 340, 180, gmTitle='Edit/Modify :', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget Form1_Text1 child of Form1_Panel1
   Form1_Text1 = gmCreateTextEntry(Form1_Panel1, 10, 30, 320, 25,' ', -255, GEDIT, gmBack1Col=0, gmBack2Col=0, gmTextCol=16 , &
     gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget Form1_Text2 child of Form1_Panel1
   Form1_Text2 = gmCreateTextEntry(Form1_Panel1, 20, 70, 20, 25,'x = ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget Form1_Text3 child of Form1_Panel1
   Form1_Text3 = gmCreateTextEntry(Form1_Panel1, 50, 70, 100, 25,' ', 255, GEDIT, gmBack1Col=0, gmBack2Col=0, gmTextCol=16 , &
     gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget Form1_Text4 child of Form1_Panel1
   Form1_Text4 = gmCreateTextEntry(Form1_Panel1, 180, 70, 20, 25,'y = ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget Form1_Text5 child of Form1_Panel1
   Form1_Text5 = gmCreateTextEntry(Form1_Panel1, 210, 70, 100, 25,' ', 255, GEDIT, gmBack1Col=0, gmBack2Col=0, gmTextCol=16 , &
     gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget Form1_Text6 child of Form1_Panel1
   Form1_Text6 = gmCreateTextEntry(Form1_Panel1, 10, 110, 310, 25,'For attributes click on the icons on the toolbar ', 255, &
      GDISPLAY, gmJustify=GCENTRE, gmBack1Col=0, gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create button Form1_Button1 child of Form1_Panel1
   Form1_Button1 = gmCreatePushButton(Form1_Panel1, 260, 150, 75, 25, 'OK', gmVpos=GTOP)
return
end
