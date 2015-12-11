subroutine wrong_mec(main,itogwrong,itogglewr1,itogglewr2,imod,imodold,irecq,qmec,&
	kam,kfm,ka,kf,irecq2,kaq,kfq,kaz1,kfz1,rtitle,itogglewr3,itogglewr4)
use menu_f90	
character*11 cnum1,cnum2,cnum3,cnum4,cnum5,cnum6,cnum7
character*60 qmec,rtitle

call intconv(irecq,cnum1)
call intconv(imodold,cnum2)
call intconv(imod,cnum3)
call intconv(kam,cnum4)
call intconv(kfm,cnum5)
call intconv(ka,cnum6)
call intconv(kf,cnum7)
if(irecq2.eq.-1) then
iho=14
ihr=7
else
iho=20
ihr=13
endif
nb=len_trim(qmec)
 call gDefineRGB(130, 1.000000, 0.792157, 0.584314)
   call gDefineRGB(131, 1.000000, 0.706471, 0.549020)
itogwrong = gmCreateComplexDialogueBox(Main, 10, 10, 16,iho , GALL,title='Find record for mechanism and rates', &
        gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
itogglePanel=gmCreatePanel(itogwrong, 0, 0, 18, 6, &
		gmHpos=GCENTRE, gmVpos=Gtop, gmExpand=GON, gmType=GNOBOUNDARY, &
        gmLineCol=0, gmLineBorder=0, gmFillCol=130, gmFillBorder=0)
if(ka.ne.-1) then
itext = gmCreateTextEntry(itogglePanel, 1, 1, 18, 1,&
		'The ini file specifies record #'//cnum1(1:3)//' but this belongs to a different model in',&
		 32768, Gdisplay, gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
         gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP, gmExpand=GOFF)
else
itext = gmCreateTextEntry(itogglePanel, 1, 1, 18, 1,&
		'The ini file specifies record #'//cnum1(1:3)//' but this does not exist in',&
		 32768, Gdisplay, gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
         gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP, gmExpand=GOFF)
endif
itext = gmCreateTextEntry(itogglePanel, 1, 2, 18, 1,&
		'this mechanism file ('//qmec(1:nb+1)//')',&
		 32768, Gdisplay, gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
         gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP, gmExpand=GOFF)
itext = gmCreateTextEntry(itogglePanel, 1, 3, 17, 1,&
		'In inifile, imod='//cnum2(1:3)//';kA = '//cnum4(1:3)//',kF = '//cnum5(1:3),&
		 32768, Gdisplay, gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
         gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP, gmExpand=GOFF)
if(ka.ne.-1) then
itext = gmCreateTextEntry(itogglePanel, 1, 4, 17, 1,&		
		'In '//qmec(1:nb)//' file, imod='//cnum3(1:3)//';kA = '//cnum6(1:3)//',&
		kF = '//cnum7(1:3)//' and',&
		32768, Gdisplay, gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
         gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP, gmExpand=GOFF)
itext = gmCreateTextEntry(itogglePanel, 1, 5, 17, 1,'rate title ='//rtitle,&		
		32768, Gdisplay, gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
         gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP, gmExpand=GOFF)
endif
if(irecq2.ne.-1) then
call intconv(irecq2,cnum1)
call intconv(imodold,cnum2)
call intconv(imod,cnum3)
call intconv(kaq,cnum4)
call intconv(kfq,cnum5)
call intconv(kaz1,cnum6)
call intconv(kfz1,cnum7)
itogglePanel1=gmCreatePanel(itogwrong, 0, 6, 18, 6, &
		gmHpos=GCENTRE, gmVpos=Gtop,gmtitle='Mechanism to fit:' ,gmExpand=GON, gmType=GNOBOUNDARY, &
        gmLineCol=0, gmLineBorder=0, gmFillCol=130, gmFillBorder=0)
if(kaz1.ne.-1) then
itext = gmCreateTextEntry(itogglePanel1, 1, 1, 18, 1,&
		'The ini file specifies record #'//cnum1(1:3)//' but this belongs to a different model in',&
		 32768, Gdisplay, gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
         gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP, gmExpand=GOFF)
else
itext = gmCreateTextEntry(itogglePanel1, 1, 1, 18, 1,&
		'The ini file specifies record #'//cnum1(1:3)//' but this does not exist in',&
		 32768, Gdisplay, gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
         gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP, gmExpand=GOFF)
endif
itext = gmCreateTextEntry(itogglePanel1, 1, 2, 18, 1,&
		'this mechanism file ('//qmec(1:nb+1)//')',&
		 32768, Gdisplay, gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
         gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP, gmExpand=GOFF)
itext = gmCreateTextEntry(itogglePanel1, 1, 3, 17, 1,&
		'In inifile, imod='//cnum2(1:3)//';kA = '//cnum4(1:3)//',kF = '//cnum5(1:3),&
		 32768, Gdisplay, gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
         gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP, gmExpand=GOFF)
if(kaz1.ne.-1) then
itext = gmCreateTextEntry(itogglePanel1, 1, 4, 17, 1,&		
		'In '//qmec(1:nb)//' file, imod='//cnum3(1:3)//';kA = '//cnum6(1:3)//',&
		kF = '//cnum7(1:3)//' and',&
		32768, Gdisplay, gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
         gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP, gmExpand=GOFF)
itext = gmCreateTextEntry(itogglePanel1, 1, 5, 17, 1,'rate title ='//rtitle,&		
		32768, Gdisplay, gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
         gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP, gmExpand=GOFF)
endif


endif

iradio_Radio1 = gmCreateRadioBox(itogwrong, 2, ihr, 14, 6, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmFillCol=131,gmvpos=gtop,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='')

iTogglewr1 = gmCreateToggleButton(iradio_Radio1 , 1, 1, 12, 1,&
		'Browse for another mechanism file?' , status=GOn, &
         gmType=G3Dradio,gmVpos=GTOP, gmExpand=GOFF)
iTogglewr2 = gmCreateToggleButton(iradio_Radio1 , 1, 2, 12, 1,&
		'Look for the correct model in this file?' , status=GOff, &
        gmType=G3Dradio,gmVpos=GTOP, gmExpand=GOFF)

!goto 55

if(kA.ne.-1) then
  iTogglewr3 = gmCreateToggleButton(iradio_Radio1 , 1, 3, 12, 1,&
		'Use this model?' , status=GOFF, &
         gmType=G3Dradio,gmVpos=GTOP, gmExpand=GOFF)
else
  iTogglewr3 = gmCreateToggleButton(iradio_Radio1 , 1, 3, 12, 1,&
		'Use this model?' , status=GOFF, &
         gmType=G3Dradio,gmVpos=GTOP, gmExpand=GOFF)
		 call gmSetWidgetStatus(iTogglewr3, status=GUNSELECTABLE)
endif
iTogglewr4 = gmCreateToggleButton(iradio_Radio1 , 1, 4, 12, 1,&
		'Go back to Welcome page?' , status=GOFF, &
        gmType=G3Dradio,gmVpos=GTOP, gmExpand=GOFF)
55 continue
		
ivb=gmCreatePushButton(itogwrong,2,0, 14, 1,'Continue',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=-90)
call gmdrawwindow(itogwrong)

end	
		
							