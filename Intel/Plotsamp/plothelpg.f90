subroutine plothelpg(iform,isw)

use menu_f90
CHARACTER*255 string
integer form1
if (isw.eq.1) then
ihg=10
else if (isw.eq.2) then
	ihg=10
else
 ihg=6
endif

	ihelpg = gmCreateComplexDialogueBox(iform, 20,2, 20, &
		ihg,GALL, 'Help',gmvpos=gtop, gmIconFormat=GDLLICON,&
		gmIconFile='MBIG1052')

	ihelpPanel=gmCreatePanel(ihelpg, 0, 0, 20, ihg, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0,gmscrollmode=gvertical)

if(isw.eq.1) then
	iStatic = gmCreateTextEntry(ihelppanel, 1, 1, 18, 1,'Reset shut and open levels', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)
	
	


iStatic = gmCreateTextEntry(ihelppanel, 3, 2,18, 5,'The window with the default options is displayed;&
	the user can change : the vertical separation of traces, the number of points displayed, y open &
	and y shut, to draw a frame around traces, etc. If the option to readjust  y open and y shut &
	is choosen, the user will have to select with the cursor the length of cluster for the new &
	calculations; a further window will appear where the changes can be made',&
	32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

else if (isw.eq.2) then


iStatic = gmCreateTextEntry(ihelppanel, 1, 1, 18, 1,&
			'Reset open and shut level and cluster length', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ihelppanel, 3, 2, 18, 4,' Select the line you want to move and press the &
keyboard arrows in the desired direction. When you choose the zoom option a new window will open where &
the area near the choosen line is expanded.When you finish press the icon -> (continue) to accept or &
 X to reject ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)
else

iStatic = gmCreateTextEntry(ihelppanel, 1, 1, 18, 1,&
			'Adjust line', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)
iStatic = gmCreateTextEntry(ihelppanel, 3, 2, 18, 3,'Move with keyboard arrows the line at the desired place and &
	and then press the icon -> (continue) to accept or  X to reject   ',&
		32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

endif
call gmdrawwindow(ihelpg)
end