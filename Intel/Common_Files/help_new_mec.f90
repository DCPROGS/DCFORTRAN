subroutine help_new_mec(main,indmod,inewhelp)	

use menu_f90
CHARACTER*255 string
logical indmod
integer main
 call gDefineRGB(161,0.80,1.,0.8)
	 call gDefineRGB(162,0.80,1.,0.8)
	 call gDefineRGB(163,0.44,0.87,0.44)
	inewhelp = gmCreateComplexDialogueBox(main, 28,4, 16, &
		28,GALL, 'How to create a new model',gmvpos=gtop, gmIconFormat=GDLLICON,&
		gmIconFile='MBIG1052')
	ihelpPanel=gmCreatePanel(inewhelp, 0, 0, 16, 28, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=162, gmFillBorder=0,gmscrollmode=gvertical)

call gmdrawwindow(inewhelp)
end