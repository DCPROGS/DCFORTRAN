	subroutine ajust_line(main,iajust,icall,text,iaval,val)

	use menu_f90	

	character*11 text
	

	iajust = gmCreateComplexDialogueBox(Main, 20, 5, 15, 5, GALL, '', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
	
	iPanel=gmCreatePanel(iajust, 0, 0, 15, 5, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle=' ',gmExpand=GOff, gmType=GNOBOUNDARY, &
            	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
itext= gmCreateTextEntry(ipanel, 1, 1, 6, 1,text,30, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=gtop, gmExpand=GOFF)

  iaval = gmCreateValueEntry(iPanel, 8, 1, 4, 1,val, 20,9, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

itext= gmCreateTextEntry(ipanel, 1, 2, 13, 1,' or use mouse/arrow keys ',30, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=gtop, gmExpand=GOFF)
itext= gmCreateTextEntry(ipanel, 1, 3, 13, 1,' When you finish pres OK ',30, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=gtop, gmExpand=GOFF)
   	
	ivb=gmCreatePushButton(ipanel,1,0, 11, 1,'OK',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall)
	
	call gmdrawwindow(iajust)
	
	end