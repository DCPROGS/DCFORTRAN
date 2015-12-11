subroutine value_table(Main,ivwin,n,text,ival,val,icall,itype)
! to enter 1 column of values

	use menu_f90	
	integer ival(50),itext(50),itype(50)
	character*60 text(50),title
	real val(50)
	! itype(i)= 1 k=0 integer
    k=9
	
	ivwin = gmCreateComplexDialogueBox(Main, 15, 10, 17, 2*(n+1), GALL, text(50), &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
	
	iPanel=gmCreatePanel(ivwin, 0, 0, 17, 2*(n+1), &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle=' ',gmExpand=GOff, gmType=GNOBOUNDARY, &
            	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
	    
	do i=1,n
	itext(i) = gmCreateTextEntry(iPanel, 1, 2*i-1, 10, 1,text(i), 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
	if(itype(i).eq.1) then
		k=0
	else
		k=9
	endif	
	ival(i)=gmCreateValueEntry(ipanel, 12, 2*i-1, 4, 1, val(i), 12, itype(i), gedit,&
	gmVpos=GTOP)
	call gmsetvaluesetting(ival(i),val(i))
	enddo
	
	ivb=gmCreatePushButton(ipanel,10,0, 3, 1,'OK',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall)
	if(icall.eq.3032.or.icall.eq.4002) then
		ivb=gmCreatePushButton(ipanel,1,0, 8, 1,'RECALCULATE',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall+1)
	endif
	if(icall.eq.195) then
	ivb=gmCreatePushButton(ipanel,10,0, 3, 1,'Accept',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall)

		ivb=gmCreatePushButton(ipanel,1,0, 8, 1,'Reject',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall+1)
	endif
call gmdrawwindow(ivwin)

end			
	