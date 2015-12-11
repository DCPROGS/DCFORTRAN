subroutine val_table(Main,ivwin9,n,text,ival,val,icall,itype,iplot_Toggle0)
! to enter 1 column of values

	use menu_f90	
	integer ival(50),itext(50),itype(50)
	character*60 text(50),title
	real val(50)
	integer iplot_Toggle0(10)
	! itype(i)= 1 k=0 integer
   
	iy=0
	imy=2*(n+1)
	if(icall.eq.4013) then
	iy=2
	imy=2*(n+1)+2
	endif
	ivwin9 = gmCreateComplexDialogueBox(Main, 15, 5, 19, imy, GALL, text(50), &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
	
	
	if(icall.eq.4013) then
mainp=gmCreatePanel(ivwin9, 0, 0, 19, 2, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle='Use log rate constants ?',gmExpand=GOff, gmType=GNOBOUNDARY, &
            	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
	  iradio0 = gmCreateRadioBox(mainp, 0, 0, 17, 2, gmType=GFREEFORM, gmBorderType=Gnoboundary, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Use log rate constants ?',&
	  gmVpos=Gtop)
	  iplot_Toggle0(1) = gmCreateToggleButton(iradio0, 5, 1 , 2, 1, 'Yes', 1, &
			gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)
		iplot_Toggle0(2) = gmCreateToggleButton(iradio0, 8, 1 , 2, 1, 'No', 0, &
			gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)
	endif
	
	iPanel=gmCreatePanel(ivwin9, 0, iy, 19, 2*(n+1), &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle=' ',gmExpand=GOff, gmType=GNOBOUNDARY, &
            	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
	    
	do i=1,n
	itext(i) = gmCreateTextEntry(iPanel, 1, 2*i-1, 12, 1,text(i), 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

	ival(i)=gmCreateValueEntry(ipanel, 14, 2*i-1, 4, 1, val(i), 20, itype(i), gedit,&
	gmVpos=GTOP)
	call gmsetvaluesetting(ival(i),val(i))
	enddo
	
	ivb=gmCreatePushButton(ipanel,10,0, 8, 1,'OK',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall)
	if(icall.eq.3032.or.icall.eq.4002) then
		ivb=gmCreatePushButton(ipanel,1,0, 8, 1,'RECALCULATE',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall+1)
	endif
call gmdrawwindow(ivwin9)
end			
