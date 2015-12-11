subroutine popenmax(main,ipopen,fixpmax,pomax,penfac,icall,ipval1,ipval2,ioptog1,ioptog2)

use menu_f90	
real*8 penfac,pomax
logical fixpmax
call gDefineRGB(101, 0.831372, 0.901960, 0.901960)
ipopen = gmCreateComplexDialogueBox(Main,5,5,20,7, GALL, 'Specify maximum Popen', &
              	gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='MBIG1052')


ipopen_MainPanel=gmCreatePanel(ipopen, 0, 0, 20, 7, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmfillcol=101)
iec=gmCreateTextEntry(ipopen_MainPanel, 2, 1, 7, 1,'Constrain Maximum Popen = ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=0 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
iradio_Radio1 = gmCreateRadioBox(ipopen_mainPanel, 10, 1, 6, 1, gmType=GFREEFORM, gmBorderType=Gnone, &
      gmFillCol=101,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='', gmVpos=GTOP)
if(fixpmax)then
ioptog1 = gmCreateToggleButton(iradio_Radio1 , 0, 0, 2, 1, 'yes', 2, &
              	gmType=G3Dradio,gmVpos=GTOP, gmExpand=GOFF,gmCallback=-168)
ioptog2 = gmCreateToggleButton(iradio_Radio1 , 3, 0, 2, 1, 'no', 0, &
              	gmType=G3Dradio,gmVpos=GTOP, gmExpand=GOFF,gmCallback=-169)             	
else
ioptog1 = gmCreateToggleButton(iradio_Radio1 , 0, 0, 2, 1, 'yes', 0, &
              	gmType=G3Dradio,gmVpos=GTOP, gmExpand=GOFF,gmCallback=-168)
ioptog2 = gmCreateToggleButton(iradio_Radio1 , 3, 0, 2, 1, 'no', 1, &
              	gmType=G3Dradio,gmVpos=GTOP, gmExpand=GOFF,gmCallback=-169)
endif
iec=gmCreateTextEntry(ipopen_MainPanel, 2, 3, 6, 1,'Maximum Popen = ', 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=0 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
ipval1=gmCreateValueEntry(ipopen_MainPanel,10 ,3 , 4, 1, sngl(pomax), 10, 5,gedit,&
		gmVpos=GTOP,gmtextcol=1)
call gmsetvaluesetting(ipval1,sngl(pomax))
iec=gmCreateTextEntry(ipopen_MainPanel, 2, 4, 6, 1,'Penalty  function = ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=0 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
ipval2=gmCreateValueEntry(ipopen_MainPanel,10 ,4 , 4, 1, sngl(penfac), 10, 5,gedit,&
		gmVpos=GTOP,gmtextcol=1)
call gmsetvaluesetting(ipval2,sngl(penfac))
ivb=gmCreatePushButton(ipopen_MainPanel,10,0, 10, 1,'OK',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall)
ivb=gmCreatePushButton(ipopen_MainPanel,0,0, 10, 1,'Cancel',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=-167)
call gmdrawwindow(ipopen)


end