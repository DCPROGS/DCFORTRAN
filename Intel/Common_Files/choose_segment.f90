subroutine choose_segment(iform,isegment,text1,text2,ival1,ival2,val1,val2,icall)




	use menu_f90	

	character*11 text1,text2
	

	isegment = gmCreateComplexDialogueBox(iform, 2, 2, 16, 7, GALL, '', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
	
	iPanel=gmCreatePanel(isegment, 0, 0, 16, 7, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle=' ',gmExpand=GOff, gmType=GNOBOUNDARY, &
            	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
itext= gmCreateTextEntry(ipanel, 1, 1, 6, 1,text1,30, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=gtop, gmExpand=GOFF)

  iaval1 = gmCreateValueEntry(iPanel, 8, 1, 5, 1,val1, 20,9, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
itext= gmCreateTextEntry(ipanel, 1, 2, 6, 1,text2,30, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=gtop, gmExpand=GOFF)

  iaval2 = gmCreateValueEntry(iPanel, 8, 2, 5, 1,val2, 20,9, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

itext= gmCreateTextEntry(ipanel, 1, 3, 14, 1,' or use mouse to mark the beginning and end of segment ',30, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=gtop, gmExpand=GOFF)
itext= gmCreateTextEntry(ipanel, 1, 4, 13, 1,' When you finish press Continue or Reset ',30, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=gtop, gmExpand=GOFF)
   		ivb=gmCreatePushButton(ipanel,2,0, 4, 1,'Canel',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall+1)
	ivb=gmCreatePushButton(ipanel,6,0, 4, 1,'Reset',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall+2)
		ivb=gmCreatePushButton(ipanel,10,0, 4, 1,'Continue',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall)
	
	call gmdrawwindow(isegment)
	
	end