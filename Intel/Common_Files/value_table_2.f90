subroutine value_table_2(Main,iv2win,n,m,text,ititle2,ival2,val_2,icall,itype)
! to enter m colum, n rows of values

	use menu_f90	
	integer ival2(10,20),itype(50)
	character*60 text(50),ititle2(20),title
	real val_2(10,20)

	iv2win = gmCreateComplexDialogueBox(Main, 3, 5, 10+4*m, 2*n+4, GALL, title, &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
	
	i2Panel=gmCreatePanel(iv2win, 0, 0, 4*m+10, 2*n+4, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle=' ',gmExpand=GOff, gmType=GNOBOUNDARY, &
            	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)


	do i=1,n
		itextv= gmCreateTextEntry(i2Panel, 1, 2*i+1, 8, 1,text(i), 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

	enddo

	do j=1,m
			itexth = gmCreateTextEntry(i2Panel, 10+3*(j-1), 1, 3, 1,ititle2(j), 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

	enddo

	do i=1,n
	do j=1,m
		ival2(i,j)=gmCreateValueEntry(i2panel,10+3*(j-1) ,2*i+1 , 3, 1, val_2(i,j), 10, itype(i),gedit,&
		gmVpos=GTOP,gmtextcol=11)
		call gmsetvaluesetting(ival2(i,j),val_2(i,j))
	enddo
	enddo
	ivb=gmCreatePushButton(i2panel,3,0, 3, 1,'OK',gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gright,gmExpand=GOFF,gmCallback=icall)
	call gmdrawwindow(iv2win)
	end