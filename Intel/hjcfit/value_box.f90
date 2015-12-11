subroutine value_box(main,ivalwin,ncol,nrows,title_box,ival_box,val_box,itcall)
use menu_f90


character*60 title_box(10)
integer ival_box(10,20),ititbox(10)
real val_box(10,20)


	ilen=nrows+4
	iwid=4
	itwid=ncol*(iwid+1)+1
	ivalwin = gmCreateComplexDialogueBox(Main, 12, 4, ncol*(iwid+1)+1, nrows+3, GALL, ' ', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
	iPanel=gmCreatePanel(itwin, 0, 1, ncol*(iwid+1)+1,nrows, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle=' ',gmExpand=GOff, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
	
	do j=1,ncol    
	
	ititbox(j)=gmCreateTextEntry(itwin, (iwid+1)*(j-1)+1, 0, iwid, 1,title_box(j), 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=Gcentre, &
              	gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
	do i=1,nrows
	ival_box(j,i)=gmCreateValueEntry(ipanel, (iwid+1)*(j-1)+1 , i-1, iwid, 1, val_box(j,i), 20, 9, gedit,&
	gmVpos=GTOP)
	call gmsetvaluesetting(ival_box(j,i),val_box(j,i))
	enddo
	enddo
	ivb=gmCreatePushButton(ivalwin,0,0, itwid, 1,'OK',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall)
call gmdrawwindow(ivalwin)
end