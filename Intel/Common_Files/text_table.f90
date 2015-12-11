subroutine text_table(Main,iqwin,title,n,textiq1,iqt1,textiqt1,icall)
! similar with table_value- but 1 coloumn of text
	use menu_f90	
	integer iq1(20),iqt1(20)
	character*60 textiq1(20),textiqt1(20),title			
	     iadd=0
		ixl1=5
		ixl2=3
		
		if(icall.eq.1111.or.icall.eq.1627.or.icall.eq.1628.or.icall.eq.1951.or.icall.eq.1961) then
		ixl1=3
		ixl2=12
		endif
			
		iqwin = gmCreateComplexDialogueBox(Main, 2+iadd, 2+iadd, ixl1+ixl2+3, 2*(n+1), GALL, ' ', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
		iqPanel=gmCreatePanel(iqwin, 0, 0, ixl1+ixl2+3, 2*(n+1), &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle=title,gmExpand=GOff, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
	    do i=1,n
			if(i.gt.1.and.icall.eq.38) then
				ixl1=9
				ixl2=3
			endif
            iq1(i) = gmCreateTextEntry(iqPanel, 1, 2*i-1, ixl1, 1,textiq1(i), 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
			
			iqt1(i) = gmCreateTextEntry(iqPanel, ixl1+2, 2*i-1, ixl2, 1,textiqt1(i), 60, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT,gmscrollable=gon, &
              	gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
		enddo

		iqb=gmCreatePushButton(iqpanel,ixl1+ixl2,1, 2, 1,'OK',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall)
call gmdrawwindow(iqwin)
end