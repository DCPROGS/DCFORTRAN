subroutine grid(iform,igrid,tval3,ival3)
use menu_f90


integer ival3(11)
real tval3(11)
character*11 cnum
intg=int(tval3(11))
igrid = gmCreateComplexDialogueBox(iform,15,10,11,intg+4, GALL, '', &
              	gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='MBIG1052')

iStatic = gmCreateTextEntry(igrid, 1, 1, 5, 1,'Number of gridlines:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

ival3(11) = gmCreateValueEntry(igrid, 7, 1, 2, 1,tval3(11), 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)



do i=1,intg
call intconv(i,cnum)
iStatic = gmCreateTextEntry(igrid, 1, 2+i, 5, 1,'Line '//cnum(1:3)//' P (open) =', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

ival3(i) = gmCreateValueEntry(igrid, 6, 2+i, 4, 1,tval3(i), 5,3, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
enddo


 itogButton = gmCreatePushButton(igrid, 4, 0, 3, 1, 'OK', &
   gmVpos=Gbottom,gmType=GDEFAULTBUTTON,gmcallback=106)
 call gmdrawwindow(igrid)
end