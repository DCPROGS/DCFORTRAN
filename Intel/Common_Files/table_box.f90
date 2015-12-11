subroutine table_box(main,itwin,n,m,title_box,text_boxf,itext_boxf,icall,ivalbox)
! to enter text(i,j)
use menu_f90
integer :: icyc_Form(500)

integer :: iwcell(10),ivalbox(10),ival(10)

character*60 text_boxf(20,10),title_box(10), wtitle
integer itext_boxf(20,10),ititle(10)
iwid=4
if (icall.eq.3011) then
	wtitle='Browse for data files '
iwid=7
endif
	ilen=n+4
	
	 call gDefineRGB(112,0.96,0.82,0.82)
	 call gDefineRGB(113,0.93,0.78,0.78)
	 call gDefineRGB(114,0.90,0.74,0.74)
call gDefineRGB(38,1.,0.5,0.25)	!orange
	itwid=m*(iwid+1)+1
	if(ilen.gt.20) ilen=20
	itwin = gmCreateComplexDialogueBox(Main, 12, 4, m*(iwid+1)+1, n+4, GALL, wtitle, &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
	iPanel=gmCreatePanel(itwin, 0, 0, m*(iwid+1)+1,n+3, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle=' ',gmExpand=GOff, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=112, gmFillBorder=0)
	
	do j=1,m    
	
	ititle(j)=gmCreateTextEntry(ipanel, (iwid+1)*(j-1)+1, 0, iwid, 1,title_box(j), 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=Gcentre, &
              	gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
	do i=1,n

	if(j.eq.1.and.icall.eq.3513) iwid=2
	if(j.eq.2.and.icall.eq.3513) iwid=6
	itext_boxf(i,j) = gmCreateTextEntry(iPanel,(iwid+1)*(j-1)+1 , i, iwid, 1,text_boxf(i,j), 32000, Gedit, &
              	gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmscrollable=gon,gmExpand=GOn)
	
	enddo
	enddo
	if (icall.eq.3011) then
	do i=1,m
		ivb1=gmCreatePushButton(ipanel,(iwid+1)*(i-1)+1,1,iwid/2+1 , 1,'Change #',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=3200+i)
				ix=(iwid+1)*(i-1)+2+iwid/2
				ival(i)=1
		ivalbox(i)=gmCreateValueEntry(ipanel,ix,1,iwid/2,1,ival(i),3,0,&
              	gdisplay, gmType=GDECIMAL,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft)
		ivb1=gmCreatePushButton(ipanel,(iwid+1)*(i-1)+1,0,iwid/2+1 , 1,'Browse',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=3020+i)
	enddo
	!ivb2=gmCreatePushButton(itwin,itwid/3,0, itwid/3, 1,'Reset',&
     !         	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
      !        	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=9515)
	ivb3=gmCreatePushButton(itwin,itwid/2,0, itwid/2, 1,'Continue',&
              	gmType=Guserdefined, gmOffcol=114, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=3014)
				ivb3=gmCreatePushButton(itwin,0,0, itwid/2, 1,'Cancel',&
              	gmType=Guserdefined, gmOffcol=114, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=3015)
	else
	ivb=gmCreatePushButton(itwin,0,0, itwid, 1,'Continue',&
              	gmType=Guserdefined, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall)

	endif
	call gmdrawwindow(itwin)
end
