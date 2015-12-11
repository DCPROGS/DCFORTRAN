subroutine yesno_table(main,iynwin,n,text,iyes,iyn,iyesno,icall)
	use menu_f90	
character*60 text(20)
integer iyes(20),ino(20),iyesno(20,2)
iwid=12
iynwin = gmCreateComplexDialogueBox(Main,25 ,7 ,iwid , 3*n+1, GALL, ' ', &
         gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
iyesnop=gmCreatePanel(iynwin, 0, 0,iwid , 3*n+1, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)
	
do i=1,n
	iyn_Radio = gmCreateRadioBox(iyesnop, 0, 3*(i-1),iwid , 3, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
		gmFillCol=141,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle=text(i), gmVpos=GTOP)
	iyes(i) = gmCreateToggleButton(iyn_Radio, 4, 1 , 2, 1, 'Yes', iyesno(i,1), &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)
	ino(i) = gmCreateToggleButton(iyn_Radio, 6, 1 , 2, 1, 'No', iyesno(i,2), &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)
enddo

ivb=gmCreatePushButton(iyesnop,1,0, iwid-2, 1,'Continue',&
              	gmType=Guserdefined, gmSize=80,gmoffcol=38,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall)
	
call gmdrawwindow(iynwin)
end   