! Create main panel for form
   Form1MainPanel=gmCreatePanel(Form1, 0, 0, 36, 27, &
              	gmHpos=GLEFT, gmVpos=Gmiddle, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=15, gmFillBorder=0)

! Create panel Panel1 child of Form1MainPanel
   Panel1 = gmCreatePanel(Form1MainPanel, 0, 0, 36, 27, &
              	gmType=GCHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=12, gmFillBorder=0, &
              	gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static1 child of Panel1
   Static1 = gmCreateTextEntry(Panel1, 1, 7, 8, 1,'Experiment', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

   Edit1 = gmCreateTextEntry(Panel1, 11, 8, 12, 1,tedit1, 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=101)

! Create text entry Static1_1 child of Panel1
   Static1_1 = gmCreateTextEntry(Panel1, 1, 8, 10, 1,'Date ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
! Create text entry Edit1_1 child of Panel1
  
   Edit1_1 = gmCreateTextEntry(Panel1, 11, 8, 12, 1,tedit1_1, 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=101)


! Create text entry Static1_2 child of Panel1
   Static1_2 = gmCreateTextEntry(Panel1, 1, 9, 4, 1,'Tape details ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
! Create text entry Edit1-2 child of Panel1
   
   Edit1_2 = gmCreateTextEntry(Panel1, 5, 9, 18, 1,tedit1_2, 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=102)

! Create text entry Static1_3 child of Panel1
   Static1_3 = gmCreateTextEntry(Panel1, 1, 10, 4, 1,'Patch number ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit1_3 child of Panel1
  	
   val1_3 = gmCreateValueEntry(Panel1, 5, 10, 18, 1,tval1_3, 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=103)

! Create text entry Static1_4 child of Panel1
   Static1_4 = gmCreateTextEntry(Panel1, 1, 11, 4, 1,'Patch name ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit1_4 child of Panel1
   
   Edit1_4 = gmCreateTextEntry(Panel1, 5, 11, 18, 1,tedit1_4, 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=104)


! Create text entry Static1_5 child of Panel1
   Static1_5 = gmCreateTextEntry(Panel1, 1, 12, 3, 1,'Patch type ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)


! Create toggle button Toggle1_5 child of Panel1
   Toggle1_5(1) = gmCreateToggleButton(Panel1, 1, 13, 4, 1, 'Outside-out', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=151)

! Create toggle button Toggle1_5 child of Panel1
   Toggle1_5(2) = gmCreateToggleButton(Panel1, 5, 13, 4, 1, 'Inside-out', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=152)

! Create toggle button Toggle1_5 child of Panel1
   
   Toggle1_5(3) = gmCreateToggleButton(Panel1, 9, 13, 4, 1, 'Cell-attached', itogon, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=153)

! Create toggle button Toggle1_5 child of Panel1
   Toggle1_5(4) = gmCreateToggleButton(Panel1, 14, 13, 4, 1, 'Whole-cell', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=154)

! Create toggle button Toggle1_5 child of Panel1
   Toggle1_5(5) = gmCreateToggleButton(Panel1, 18, 13, 6, 1, 'Simulated data', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=155)

! Create text entry Static1_6 child of Panel1
   Static1_6 = gmCreateTextEntry(Panel1, 1, 14, 7, 1,'Membrane potential [mV] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit16 child of Panel1
   
   val1_6 = gmCreateValueEntry(Panel1, 8, 14, 15, 1,tval1_6, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=106)

! Create text entry Static1_7 child of Panel1
   Static1_7 = gmCreateTextEntry(Panel1, 1, 15, 5, 1,'Temperature [o C] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit1_7 child of Panel1
   
   val1_7 = gmCreateValueEntry(Panel1, 7, 15, 16, 1,tval1_7, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=107)
