subroutine about
! Last created on Tue Sep 11 12:49:01 2001

! Using GINOMENU Version 2.0c
use menu_f90

! Widget identifier definitions
integer :: Form1
integer :: Form1MainPanel
integer :: Panel1
integer :: Static1
integer :: Static2
integer :: Static3
integer :: Static4
integer :: Icon1
type(GARRAYCELL) arrayattribs
integer :: ISTBAR(32)
integer :: callid

! Initialise Gino, Device & Menu
   call gOpenGino
   call gGuiwin
   call gmInitializeMenu
   call gmSetGuiGridMode(GON)

! Set up master window Form1
   Form1 = gmCreateMasterWindow(9, 5, 16, 22, GALL, 'Form 1', &
              	gmVpos=GTOP, gmIconFormat=GDLLICON,gmIconFile='Gee')

! Create main panel for form
   Form1MainPanel=gmCreatePanel(Form1, 0, 0, 16, 22, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

! Create panel Panel1 child of Form1MainPanel
   Panel1 = gmCreatePanel(Form1MainPanel, 0, 0, 14, 8, &
              	gmTitle='                  CONSAM  FOR  WINDOWS  V1.0', gmType=GPROJECTED, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=12, gmFillBorder=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static1 child of Panel1
   Static1 = gmCreateTextEntry(Panel1, 1, 1, 12, 1,'General-purpose Continuous Sampling to Disk ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static2 child of Panel1
   Static2 = gmCreateTextEntry(Panel1, 1, 4, 12, 1,'David Colquhoun and Ioana Vais ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static3 child of Panel1
   Static3 = gmCreateTextEntry(Panel1, 1, 5, 12, 1,'University College London ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=12, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static4 child of Panel1
   Static4 = gmCreateTextEntry(Panel1, 1, 6, 12, 1,'http://www.ucl.ac.uk/Pharmacology ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create icon Icon1 child of Panel1
   Icon1 = gmCreateIcon(Panel1, 6, 2, 2, 2,'MBIG1035', 4, 1, &
              	gmType=GBUTTON, gmVpos=GTOP, gmExpand=GOFF)

! Start management
   call gmManage

! Set required callbacks for forms

! Action loop
   do while (gmAction(callid) /= -1)

! Code to handle callbacks

   end do

! Close device MENU and GINO, and end program
   call gmCloseMenu
   call gCloseDevice
   call gCloseGino

stop
end
