subroutine weighting(weightform)
! Using GINOMENU Version 2.0c
use menu_f90

! Widget identifier definitions
integer :: weightForm
integer :: Form8MainPanel
integer :: Panel8
integer :: Toggle8(5)
integer :: Editval8(4)
integer :: Button8(2)

   Weightform = gmCreateComplexDialogueBox(Main, 3, 3, 16, 9, GALL,'Weighting methods:' , &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1067',gmvpos=gtop,&
				gmCallBack=-81,gmSelect=-82)

! Create main panel for form
   Form8MainPanel=gmCreatePanel(Weightform, 0, 0, 16, 9, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

! Create panel panel8 child of Form1MainPanel
   Panel8 = gmCreatePanel(Form8MainPanel, 0, 0, 16, 9, &
              	gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle button Toggle1 child of panel8
   Toggle8(1) = gmCreateToggleButton(panel8, 1, 1, 14, 1, 'Weights constant.Errors from residuals', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle button Toggle2 child of panel8
   Toggle8(2) = gmCreateToggleButton(panel8, 1, 2, 14, 1, 'Weights from specified s(y)', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle button Toggle3 child of panel8
   Toggle8(3) = gmCreateToggleButton(panel8, 1, 3, 14, 1, 'Calculate weights from s(y)=a+bx', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create value entry Editval1 child of panel8
   Editval8(1) = gmCreateValueEntry(panel8, 1, 4, 4, 1, 0.000000, 8, 3, GEDIT, &
              	
              	gmType=GSTANDARD, gmSign=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create value entry Editval2 child of panel8
   Editval8(2) = gmCreateValueEntry(panel8, 8, 4, 4, 1, 0.000000, 8, 3, GEDIT, &
              	
              	gmType=GSTANDARD, gmSign=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle button Toggle4 child of panel8
   Toggle8(4) = gmCreateToggleButton(panel8, 1, 5, 14, 1, 'Calculate weights from s(y)=a+by', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create value entry Editval3 child of panel8
   Editval8(3) = gmCreateValueEntry(panel8, 1, 6, 4, 1, 0.000000, 8, 3, GEDIT, &
              	
              	gmType=GSTANDARD, gmSign=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create value entry Editval4 child of panel8
   Editval8(4) = gmCreateValueEntry(panel8, 8, 6, 4, 1, 0.000000, 8, 3, GEDIT, &
              	
              	gmType=GSTANDARD, gmSign=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle button Toggle5 child of panel8
   Toggle8(5) = gmCreateToggleButton(panel8, 1, 7, 14, 1, 'Use specified weights for fit but error from residuals', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create button Button1 child of panel8
   Button8(1) = gmCreatePushButton(panel8, 9, 8, 3, 1, 'OK', &
              	gmType=GSTANDARD, gmSize=80, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create button Button2 child of panel8
   Button8(2) = gmCreatePushButton(panel8, 12, 8, 3, 1, 'Cancel', &
              	gmType=GSTANDARD, gmSize=80, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

call gmdrawwindow(weightform)
end
