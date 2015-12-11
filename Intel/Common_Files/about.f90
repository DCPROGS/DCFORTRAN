subroutine about_ucl(Main,Form12,title,iprogram_type)

use menu_f90

! Widget identifier definitions
integer :: Form12
integer :: Form12MainPanel
integer :: Panel12
integer :: Static12_0
integer :: Static12_1
integer :: Static12_2
integer :: Static12_3
integer :: Static12_4
integer :: Icon12
character title*60, titleprogram*60
iyy=8
if(iprogram_type.eq.-1) then
	titleprogram='Data Analysis Studio'
else if(iprogram_type.eq.1) then
	titleprogram='Curve Fitting'
else if(iprogram_type.eq.2) then
	titleprogram='Automatic printing from plot queues'
else if(iprogram_type.eq.3) then
        iyy=17
	titleprogram='Fitting model using HJC distributions'
endif
   Form12=gmCreateComplexDialogueBox(Main, 13, 5, 15, iyy, GALL, 'About', &
              	gmIconFormat=GDLLICON,gmvpos=gtop,gmIconFile='MBIG1062')


! Create main panel for form
   Form12MainPanel=gmCreatePanel(Form12, 0, 0, 15, iyy, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

! Create panel Panel1 child of Form1MainPanel
   Panel12 = gmCreatePanel(Form12MainPanel, 0, 0, 15, iyy, &
              	gmType=GPROJECTED, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=12, gmFillBorder=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static1 child of Panel1
   Static12_0 = gmCreateTextEntry(Panel12, 1, 1,13, 1,TITLE, 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static1 child of Panel1
   Static12_1 = gmCreateTextEntry(Panel12, 1, 1,13, 1,titleprogram, 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static2 child of Panel1
   Static12_2 = gmCreateTextEntry(Panel12, 1, 4,13, 1,'David Colquhoun and Ioana Vais ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static3 child of Panel1
   Static12_3 = gmCreateTextEntry(Panel12, 1, 5,13, 1,'University College London ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=12, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static4 child of Panel1
   Static12_4 = gmCreateTextEntry(Panel12, 1, 6,13, 1,'http://www.ucl.ac.uk/Pharmacology ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create icon Icon1 child of Panel1
   Icon12 = gmCreateIcon(Panel12, 6, 2, 2, 2,'MBIG1035', 4, 1, &
              	gmType=GBUTTON, gmVpos=GTOP, gmExpand=GOFF)
   if(iprogram_type.eq.3) then
   Static12_4 = gmCreateTextEntry(Panel12, 1, 7,13, 1,'Full maximum likelihood fitting of a mechanism directly', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
              	
   Static12_4 = gmCreateTextEntry(Panel12, 1, 8,13, 1,'to the entire sequence of open and shut times, ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
    Static12_4 = gmCreateTextEntry(Panel12, 1, 9,13, 1,'with exact missed events correction.', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
              	
   Static12_4 = gmCreateTextEntry(Panel12, 1, 10,13, 1,'The input for HJCFIT is the list of idealised ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)          	           	
    
   Static12_4 = gmCreateTextEntry(Panel12, 1, 11,13, 1,'open and shut times from SCAN (scan.scn file).', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF) 
    Static12_4 = gmCreateTextEntry(Panel12, 1, 12,13, 1,'The current version can now also read events list files', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)          	           	
    
   Static12_4 = gmCreateTextEntry(Panel12, 1, 13,13, 1,'from Strathclyde (Dempster) programs (.ede files), ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF) 
   Static12_4 = gmCreateTextEntry(Panel12, 1, 14,13, 1,'and from pClamp (.evl files). ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
              	 Static12_4 = gmCreateTextEntry(Panel12, 1, 15,13, 1,'Use the menu bar help for specific guidance', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=4, &
              	gmVpos=GTOP, gmExpand=GOFF)  



    endif
call gmdrawwindow(form12)

end
