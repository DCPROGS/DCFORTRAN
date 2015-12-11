subroutine plothelp(iform)

use menu_f90
CHARACTER*255 string
integer form1
	ihelp = gmCreateComplexDialogueBox(iform, 20,2, 24, &
		28,GALL, 'Help',gmvpos=gtop, gmIconFormat=GDLLICON,&
		gmIconFile='MBIG1052')
	ihelpPanel=gmCreatePanel(ihelp, 0, 0, 20, 35, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0,gmscrollmode=gvertical)

	iStatic = gmCreateTextEntry(ihelppanel, 1, 1, 18, 1,'About Plotsamp', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)
	
	
	iStatic = gmCreateTextEntry(ihelppanel, 3, 2, 18, 3,'To plot all or part of CONSAM type file on multiple pages;&
	or/and to create output file which is modified in any/all of the following ways:&
	(1) Particular start and end points specified in CONSAM ;&
	(2) Filtered by GAUSSIAN filter ;&
	(3) Every nth point omitted/kept', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)


	iStatic = gmCreateTextEntry(ihelppanel, 1, 5, 18, 1,'Initial options', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 3, 6, 18, 5,'The user has &
		the choice to Auto-restart the previous analysis.&
		 When CONTINUE is pressed the program will go to the last &
		 page previously analysed. If No is selected a second window will appear &
		 where the user can make his choices for file types, to use or not to use &
		 the ini file,to write or not to write the output to a text file', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)	




	iStatic = gmCreateTextEntry(ihelppanel, 1, 11, 18, 1,'File type', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 3, 12, 18, 5,'The user must &
		choose between the data file types (Consam, Axon,16 bit binary) and to enter the name for print out file &
 and/or the name of the ini file; to change the default name press the BROWSE button (recommended) or enter it by hand. &
 If the option is 16 bit integer binary file, this file will be converted &
 to a consam type file before further processing; the user must choose the name for the converted file as well as to fill in &
 details about the experiment ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)



iStatic = gmCreateTextEntry(ihelppanel, 1, 17,18, 1,'Display sample', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 3, 18,18, 7,'The window with the default options will appear;&
	the user can change them and then press the OK button. The sample duration, number of pages, &
	vertical separation of traces will be calculated from the input values. &
	If the option "Interleave traces with plot of P(open" has been chosen &
	, a further window will appear where the user could adjust Y open & Y shut. &
	Pressing the arrow (continue) icon will display the first page requested.', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)
iStatic = gmCreateTextEntry(ihelppanel, 1, 24, 18, 1,&
			'Page window', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 3, 25, 18, 3,' On this window the user has the possibility to: &
	1) change Y open, Y shut, 2) omit points, 3) change vertical separation between traces, &
	4) draw frame,  5) go to next page, 6) display previous page, 7) print current page or all pages, &
	8) save as wmf', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 1, 28, 18, 1,&
			'Filter with Gaussian filter, and omit points if nec', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)
iStatic = gmCreateTextEntry(ihelppanel, 3, 29, 18, 3,'The user has the possibility to filter the sample before saving &
		it in the new consam file. After pressing the Continue button , a window will appear showing the progress  ',&
		32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 1, 32, 18, 1,&
			'Write subsection of CONSAM file to disk', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)
iStatic = gmCreateTextEntry(ihelppanel, 3, 33, 18, 2,' Writes bits of consam to output file without any further action', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 1, 35, 18, 1,&
			'Omit points from output file, and write to disk', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 3, 36, 18, 2,'The user has the possibility to omit points from the &
initial file before writing them to the output file ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

call gmdrawwindow(ihelp)
end