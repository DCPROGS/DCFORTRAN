subroutine setec50(main,iec50,nfixec50,nlig,ligname,ival,val,comboname,npar,icall,&
iecctype,lec50)
	use menu_f90	
integer ival(50), iec50pan(10)
character*20 comboname(10,100), ligname(10)
real val(50)


iy=nfixec50*8+3 
 call gDefineRGB(101, 0.831372, 0.901960, 0.901960)
iec50 = gmCreateComplexDialogueBox(Main,5,5,20,iy, GALL, 'Specify EC50', &
              	gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='MBIG1052')


iec50_MainPanel=gmCreatePanel(iec50, 0, 0, 20, iy, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)

iec5=gmCreateTextEntry(iec50_MainPanel, 2, 0, 6, 1,'Penalty function = ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=0 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
ival(10)=gmCreateValueEntry(iec50_MainPanel,9 ,0 , 4, 1, val(10), 10, 5,gedit,&
		gmVpos=GTOP,gmtextcol=1)
		call gmsetvaluesetting(ival(10),val(10))

do i=1,nfixec50
if(i.eq.1) then
	m=1
else
	m=10
endif
iec50pan(i)=gmCreatePanel(iec50_MainPanel, 1, 8*(i-1)+1, 18, 8, gmHpos=Gleft, gmVpos=Gtop, &
      gmExpand=GON, gmType=GINVERSECHISEL, gmfillCol=101,&
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)
iec50_1=gmCreateTextEntry(iec50pan(i), 1, 1, 12, 1,'Ligand: '//ligname(i), 255, GDISPLAY, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=0 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

iec50_2=gmCreateTextEntry(iec50pan(i), 1, 2, 7, 1,'EC50 (micromolar) = ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=0, &
       gmTextCol=0 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

ival(1)=gmCreateValueEntry(iec50pan(i),8 ,2 , 4, 1, val(1), 10, 5,gedit,&
		gmVpos=GTOP,gmtextcol=1)
		call gmsetvaluesetting(ival(1),val(1))
iec50_3=gmCreateTextEntry(iec50pan(i), 1, 3, 12,1,'Rate constant to be constrained by EC50 =', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=0 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
iecctype= gmCreateComboBox(iec50pan(i),13,3,4,10,Gnone, 1, &
			gmSort=GunSORTED, gmVpos=GTOP, gmExpand=GOFF)
do j=1,npar
	call gmSetListEntry(iecctype, GADD, gmString=comboname(1,j))

enddo
 call gmSetListEntry(iecctype,GSELECT,gmEntry=lec50)
iec50_4=gmCreateTextEntry(iec50pan(i), 1, 4, 12, 1,'Lower limit for specific rate constant', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=0 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
ival(3)=gmCreateValueEntry(iec50pan(i),13 ,4 , 4, 1, val(3), 10, 5,gedit,&
		gmVpos=GTOP,gmtextcol=1)
		call gmsetvaluesetting(ival(3),val(3))
iec50_5=gmCreateTextEntry(iec50pan(i), 1, 5, 12, 1,'Upper limit for specific rate constant' , 255, GDISPLAY, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=0 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
ival(4)=gmCreateValueEntry(iec50pan(i),13 ,5 , 4, 1, val(4), 10, 5,gedit,&
		gmVpos=GTOP,gmtextcol=1)
		call gmsetvaluesetting(ival(4),val(4))
if(nlig.gt.1) then
   if(i.eq.1) then
	iec50_6=gmCreateTextEntry(iec50pan(i), 1, 6, 12, 1,'Concentration of ligand '//&
	 ligname(2)//' (at which EC50 was determinated)', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=0 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
	else
	 iec50_6=gmCreateTextEntry(iec50pan(i), 1, 6, 12, 1,'Concentration of ligand '//&
	 ligname(1)//' (at which EC50 was determinated)', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=0 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
	endif
	ival(2)=gmCreateValueEntry(iec50pan(i),13 ,6 , 4, 1, val(2), 10, 5,gedit,&
		gmVpos=GTOP,gmtextcol=1)
		call gmsetvaluesetting(ival(2),val(2))
endif
enddo
ivb=gmCreatePushButton(iec50_MainPanel,10,0, 10, 1,'OK',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall)
ivb=gmCreatePushButton(iec50_MainPanel,0,0, 10, 1,'Cancel',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall+1)
call gmdrawwindow(iec50)
end