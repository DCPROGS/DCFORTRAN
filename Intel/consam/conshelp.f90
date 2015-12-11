subroutine conshelp(iform1)	

use menu_f90
CHARACTER*255 string
integer form1
	ihelp = gmCreateComplexDialogueBox(iform1, 20,4, 16, &
		20,GALL, 'Help',gmvpos=gtop, gmIconFormat=GDLLICON,&
		gmIconFile='MBIG1052')
	ihelpPanel=gmCreatePanel(ihelp, 0, 0, 16, 20, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0,gmscrollmode=gvertical)

	iStatic = gmCreateTextEntry(ihelppanel, 1, 1, 14, 1,'About Consam', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)
	
	
	iStatic = gmCreateTextEntry(ihelppanel, 3, 2, 14, 2,'Sample data continuously to hard disk via CED1401 &
	interface;', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

	
	iStatic = gmCreateTextEntry(ihelppanel, 1, 4, 14, 1,'Ini File', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 3, 5, 14, 4,'First time you start the program  a name for *.ini file &
(including the path) will be suggested. To change that name you can use the BROWSE button (recommended) or enter it by hand ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 1, 9, 14, 1,'Data File', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 3, 10, 14, 4,'An automatic data file name will be created; this will containt the current date. &
The name will be saved in *.ini file so next time you start the program a counter will increase the last digit(s) &
To change that name you can use the BROWSE button (recommended) or enter it by hand. Using the BROWSER will give the possibility &
to automatically overwrite an existing file or create a new one ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 1, 14, 14, 1,'Experiment + sampling details', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 3, 15, 14, 4,'Before starting the sampling fill/check experiment details, &
if you sampling from tape or fill/check in the required fields,if sampling live ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

call gmdrawwindow(ihelp)
end