subroutine help(Main,Help_Dialog,HelpMainPanel,indhelp)

use menu_f90

integer :: Main
integer :: Help_Dialog
integer :: HelpMainPanel
integer :: Panel5_1
integer :: Icon5_1
integer :: Static5_1
integer :: Static5_2
integer :: Static5_3
integer :: Static5_4

if(indhelp.eq.0) then
! Set up complex dialogue box Form8 child of Main
   Help_Dialog = gmCreateComplexDialogueBox(Main, 9, 5, 16, 12, GALL, 'About', &
              	gmVpos=GTOP, gmIconFormat=GDLLICON,gmIconFile='MBIG1061')

! Create main panel for form
   HelpMainPanel=gmCreatePanel(Help_Dialog, 0, 0, 16, 12, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

! Create panel Panel5_1 child of Form8MainPanel
   Panel5_1 = gmCreatePanel(HelpMainPanel, 0, 0, 16, 12, &
              	gmType=GCHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static5 child of Panel5_1
   Static5_1 = gmCreateTextEntry(Panel5_1, 1, 1, 14, 1,'Data Analysis Studio 1.0 ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static6 child of Panel5_1
   Static5_2 = gmCreateTextEntry(Panel5_1, 1, 6, 14, 1,'David Colquhoun & Ioana L.Vais ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static7 child of Panel5_1
   Static5_3 = gmCreateTextEntry(Panel5_1, 1, 7, 14, 1,'University College London ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static8 child of Panel5_1
   Static5_4 = gmCreateTextEntry(Panel5_1, 1, 9, 14, 1,'http://www.ucl.ac.uk/Pharmacology ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create icon Icon23 child of Panel5_1
   Icon5_1 = gmCreateIcon(Panel5_1, 7, 3, 2, 2,'MBIG1032', 4, 1, &
              	gmType=GBUTTON, gmVpos=GTOP, gmExpand=GOFF)

else
endif
call gmdrawwindow(help_dialog)
end