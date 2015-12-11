subroutine page_help(main,indmod,iphelp)	

use menu_f90
CHARACTER*255 string
logical indmod
integer main
	iphelp = gmCreateComplexDialogueBox(main, 28,1, 16, &
		34,GALL, 'How to create a new model',gmvpos=gtop, gmIconFormat=GDLLICON,&
		gmIconFile='MBIG1052')
	ihelpPanel=gmCreatePanel(iphelp, 0, 0, 16, 34, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0,gmscrollmode=gvertical)

	iStatic = gmCreateTextEntry(ihelppanel, 1, 0, 14, 1,'1.Add states', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)
	
	
	iStatic = gmCreateTextEntry(ihelppanel, 3, 1, 14, 3,'To add an open/closed state, click once on the &
	icon "O"/"C" on the graph bar, release the mouse and click again on the graph area where you &
	want to insert the new state', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

	
	iStatic = gmCreateTextEntry(ihelppanel, 1, 4, 14, 1,'2.Make links between states', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 3, 5, 14, 3,'Click on the graph bar on the icon &
"=" to select this option.Then click on the first state you want to link,release the button &
and move the mouse cursor to the next state and click again inside it ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 1, 8, 14, 1,'3.Move states', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 3, 9, 14, 3,' Click on the icon &
">>" to select this option.  Click on the state you want to move, release the button, &
move the mouse cursor to the new position, and click again', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 1, 12, 14, 1,'4.Delete states or links', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 3, 13, 14, 2,' Click on the rubber (eraser) icon &
 and then click on the state, or link, that you want to remove', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 1, 15, 14, 1,'5.Fill in states table', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 3, 16, 14, 4," Click on 'States' icon &
and when the table appears fill in the fields 'State name', 'Conductance' (if you want to &
change the default values) and enter the number of ligands bound to each state (last column). DO NOT FORGET then to click &
on STORE CHANGES. ", 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 1, 20, 14, 1,'6.Fill in rates table', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 3, 21, 14, 4," Click on the 'Rates' icon &
and when the table appears fill in the fields 'Rate name', 'Rate value' (if you want to &
change the default values). Under 'Conc dep' choose the appropriate ligand for association &
rate constants, 'none' for all others. Then click &
on STORE CHANGES.", 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iy=24
!if(indmod) then

iStatic = gmCreateTextEntry(ihelppanel, 1, 25, 14, 1,'7.Save on disk', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 1, 26, 14, 2,' Click on the graph bar on the disk icon &
to save the model on the desired file', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 1, 28, 14, 1,'8.Press CONTINUE button on the graph', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)
iStatic = gmCreateTextEntry(ihelppanel, 1,29 , 14, 1,'Create independent model', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gred, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 1, 30, 14, 4,'If independent model the procedure above must be repeated for each subunit.&
When you finish and save the information for &
each subunit, click on the dark blue icon (showing a 4 states mechanism) on the main window menu bar', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iy=28
!endif


call gmdrawwindow(iphelp)
end