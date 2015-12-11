subroutine display_message(main,idisplay,message,itbutton,icall)

use menu_f90	
character*100 message

idisplay = gmCreateComplexDialogueBox(Main, 5, 5, 10,5, GALL, ' ', &
              	gmIconFormat=GDLLICON,gmvpos=gtop,gmIconFile='MBIG1052')
icPanel=gmCreatePanel(idisplay, 0, 0, 10,5, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle=' ',gmExpand=GOff, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=101, gmFillBorder=0)
itext= gmCreateTextEntry(icPanel, 1, 1, 8, 1,message, 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
itbutton=gmCreatePushButton(icpanel,3,1, 4, 1,'BROWSE',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall)

end