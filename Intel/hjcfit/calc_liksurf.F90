subroutine calc_liksurf(main,iplotlik,ivalk1,ivalk1_1,ivalk1_2,ivalk2,ivalk2_1,ivalk2_2,&
           ivalk3,iTogglek_1,iTogglek_2)

use menu_f90

call gDefineRGB(220, 1.000000, 1.000000, 0.807843)
call gDefineRGB(221, 0.796078, 1.000000, 0.592157)

iplotlik = gmCreateComplexDialogueBox(main,14, 8, 19, 12, GALL, &
	'Calculation of log(likelihood) for range of values of 2 parameters', &
    gmVpos=GTOP, gmIconFormat=GDLLICON, gmIconFile='Gee')

    iplotPanel1=gmCreatePanel(iplotlik, 0, 0, 19, 12, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
    gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmFillCol=41, gmScrollMode=Gnobars)
	

iText = gmCreateTextEntry(iplotPanel1, 2, 1, 11, 1,&
		'Parameter number for rate to be varied', 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
		gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
	ivalk1=gmCreateValueEntry(iplotpanel1, 15, 1, 2, 1,ip1 , 6, 0, gedit,&
	  gmVpos=GTOP)
	
iText = gmCreateTextEntry(iplotPanel1, 2, 2, 7, 1,&
		'Range of values to be used', 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
		gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
	ivalk1_1=gmCreateValueEntry(iplotpanel1, 9, 2, 4, 1,p1l , 14, 2, gedit,&
	  gmVpos=GTOP)
	 
	ivalk1_2=gmCreateValueEntry(iplotpanel1, 13, 2, 4, 1,p1h, 14, 2, gedit,&
	  gmVpos=GTOP)
	
iText = gmCreateTextEntry(iplotPanel1, 2, 4, 11, 1,&
		'Parameter number for rate to be varied', 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
		gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
	ivalk2=gmCreateValueEntry(iplotpanel1, 15, 4, 2, 1,ip2 , 6, 0, gedit,&
	  gmVpos=GTOP)
	
iText = gmCreateTextEntry(iplotPanel1, 2, 5, 7, 1,&
		'Range of values to be used', 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
		gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
	ivalk2_1=gmCreateValueEntry(iplotpanel1, 9, 5, 4, 1,p2l , 14, 2, gedit,&
	  gmVpos=GTOP)
	 
	ivalk2_2=gmCreateValueEntry(iplotpanel1, 13, 5, 4, 1,p2h, 14, 2, gedit,&
	  gmVpos=GTOP)
	  
iText = gmCreateTextEntry(iplotPanel1, 2, 7, 11, 1,&
		'Number of values for each', 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
		gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
	ivalk3=gmCreateValueEntry(iplotpanel1, 15, 7, 2, 1,ncalc , 6, 0, gedit,&
	  gmVpos=GTOP)	  

iradio3 = gmCreateRadioBox(iplotPanel1, 2, 8, 16, 2, gmType=GFREEFORM, gmBorderType=Gnone, &
				gmFillCol=41,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, &
				gmTitle='Use log(rate)?', gmVpos=GTOP)
			
				iTogglek_1 = gmCreateToggleButton(iRadio3, 4, 1 , 3, 1,'Yes', 1, &
					gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)
				iTogglek_2 = gmCreateToggleButton(iRadio3, 8, 1 , 3, 1,'No', 0, &
					gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)

iButton1 = gmCreatePushButton(iplotPanel1,10,0 , 8, 1, 'Calculate',gmType=GUSERDEFINED, &
	gmhpos=gleft,gmVpos=Gbottom,gmOffcol=38, gmTextCol=1,&
	gmcallback=8050)
iButton1 = gmCreatePushButton(iplotPanel1,1,0 , 8, 1, 'Cancel', gmType=GUSERDEFINED,&
	gmhpos=gleft,gmVpos=Gbottom,gmOffcol=123, gmTextCol=1,&
	gmcallback=-9)
call gmdrawwindow(iplotlik)

end
