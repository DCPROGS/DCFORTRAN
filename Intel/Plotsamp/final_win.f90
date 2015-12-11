subroutine finalwin(iform,ifinwin,adcfil1,srate1,iticks,ivalfin,itextfin,ibutfin1,ibutfin2)

use menu_f90 


character*8 iticks
character*60 adcfil1
 call gDefineRGB(226, 0.996078, 0.780392, 0.705882)
 call gDefineRGB(227, 1.000000, 0.458824, 0.458824)
ifinwin = gmCreateComplexDialogueBox(iform, 15,10 , 12,13 , GALL, '', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
	
ifinPanel=gmCreatePanel(ifinwin, 0, 0, 12, 13, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle=' ',gmExpand=GOff, gmType=GINVERSECHISEL, &
            	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

iStatic = gmCreateTextEntry(ifinpanel, 3, 1, 6, 1,'OUTPUT AS CONSAM FILE', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ifinpanel, 1, 2, 10, 1,adcfil1, 32768, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT,gmscrollable=gon, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ifinPanel, 1, 3, 4, 1,'Sample rate:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

ivalfin1 = gmCreateValueEntry(ifinPanel, 5, 3, 4, 1,srate1, 20,3, Gdisplay, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)


iStatic = gmCreateTextEntry(ifinPanel, 1, 5, 10, 1,'Start writing:'//iticks, 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

itextfin = gmCreateTextEntry(ifinPanel, 1, 7, 10, 1,'PLEASE WAIT....', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ifinPanel, 1, 9, 4, 1,'Points written:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

ivalfin = gmCreateValueEntry(ifinPanel, 5, 9, 4, 1,0, 20,0, Gdisplay, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

ibutfin1=gmCreatePushButton(ifinpanel,0,0, 6, 1,'PLOT SAMPLE',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=222)
ibutfin2=gmCreatePushButton(ifinpanel,6,0, 6, 1,'CONTINUE',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=219)
				call gmsetwidgetstatus(ibutfin1,GunSELECTABLE)
				call gmsetwidgetstatus(ibutfin2,GunSELECTABLE)
call gmdrawwindow(ifinwin)
end