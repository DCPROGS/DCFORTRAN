! Last created on Wed Mar 31 12:35:37 2004

! Using GINOMENU Studio Version  v4.0c
! Global Definitions
Module mstudio
use menu_f90


! Widget identifier definitions
integer :: Form1
integer :: Form1_MainPanel
integer :: Form1_Panel1
integer :: Form2
integer :: Form2_MainPanel
integer :: Form2_TabDiaBox1
integer :: Form2_Panel2
integer :: callid
End Module

!Program D:\FortWin\common_files\forms
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
   call GenerateForm2

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
   Form1 = gmCreateMasterWindow(237, 164, 400, 550, GALL, 'Form 1', gmVpos=GTOP, gmInitState=GNORMAL, gmIconFormat=GDLLICON, &
      gmIconFile='Gee')

! Create main panel for form
   Form1_MainPanel=gmCreatePanel(Form1, 0, 0, 400, 550, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)

! Create panel Form1_Panel1 child of Form1_MainPanel
   Form1_Panel1 = gmCreatePanel(Form1_MainPanel, 0, 0, 175, 125, gmTitle='Panel 1', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)
return
end
subroutine GenerateForm2
use menu_f90
use mstudio

! Set up complex dialogue box Form2 child of Form1
   Form2 = gmCreateComplexDialogueBox(Form1, 390, 239, 400, 550, GALL, 'Form 2', gmVpos=GTOP, gmQuitMode=GHIDE, &
      gmInitState=GNORMAL, gmIconFormat=GDLLICON, gmIconFile='Gee')

! Create main panel for form
   Form2_MainPanel=gmCreatePanel(Form2, 0, 0, 400, 550, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)

! Create tabdded dialogue box Form2_TabDiaBox1 child of Form2_MainPanel
   Form2_TabDiaBox1 = gmCreateTabDialogueBox(Form2_MainPanel, 290, 220, 175, 125, gmVpos=GTOP)

! Create panel Form2_Panel2 child of Form2_TabDiaBox1
   Form2_Panel2 = gmCreatePanel(Form2_TabDiaBox1, 290, 220, 175, 125, gmTitle='Panel 2', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)
return
end
