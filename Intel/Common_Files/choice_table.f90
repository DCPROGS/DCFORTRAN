subroutine choice_table(Main,n,m,choices,text)
use menu_f90	
character*10 choices(n,m),text(n)
integer iq1(n),icombo(n)

ix=8
iy=2*(n+1)

icwin = gmCreateComplexDialogueBox(Main, 5, 5, ix,iy, GALL, ' ', &
              	gmIconFormat=GDLLICON,gmvpos=gtop,gmIconFile='MBIG1052')
icPanel=gmCreatePanel(icwin, 0, 0, ix,iy, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle=' ',gmExpand=GOff, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)


do i=1,n
	iq1(i) = gmCreateTextEntry(icPanel, 1, 2*i-1, 3, 1,text(i), 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
	iCombo(i)= gmCreateComboBox(icpanel, 4, 2*i-1, 3, 3, Gnone, gdisplay, &
              	gmSort=GunSORTED, gmVpos=GTOP, gmExpand=GOFF, gmcallback=319)
	do j=1,m
		 call gmSetListEntry(icombo(i), GADD, gmString=choices(i,j))
		 
	enddo
	call gmSetListEntry(iCombo(i),GSELECT,gmEntry=0)
enddo
call gmdrawwindow(icwin)
end