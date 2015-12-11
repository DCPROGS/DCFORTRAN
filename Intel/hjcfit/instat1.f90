subroutine instat1(main,peq,k,kA,kB,kC,kD,i1,pcum,ist,k1,fixed)
use menu_f90	

real*4 peq(100),peq1(100),pcum(101)
	integer iexc(100),ist(100)
	character*60 text(10)
	character*11 cnum
	logical fixed
	common/rand/ix,iy,iz
	SAVE iexc

integer iyes(20),ino(20)
	k1=k		!no of states from which init state chosen
	do i=1,k
				iexc(i)=0
				ist(i)=i
				peq1(i)=peq(i)
	enddo
text(5)='No'
text(1)='Yes:Start in a long shut state (C state)'
text(2)='Yes:Start in any shut state'
text(3)='Yes:Start in any open state'
text(4)='Yes:Exclude specified states as initial state'
iwid=16
inswin = gmCreateComplexDialogueBox(Main,25 ,7 ,iwid , k+14, GALL, ' ', &
         gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
iyesnop=gmCreatePanel(inswin, 0, 0,iwid , k+14, gmHpos=GCENTRE, gmVpos=GMIDDLE,gmFillCol=142, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)
	

ist_Radio1 = gmCreateRadioBox(iyesnop, 1, 1,iwid-2 , 7, &
	gmType=GFREEFORM, gmBorderType=GPROJECTED, &
		gmFillCol=141,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, &
	gmTitle='Choose initial state at random from initial vector ?', gmVpos=GTOP)

do i=1,5	
	iyes(i) = gmCreateToggleButton(ist_Radio1, 1, i , iwid-4, 1, text(i),0 , &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmCallback=10+i)	

enddo
call gmSetToggleSwitch(	iyes(1),Gon)
ist_Radio2 = gmCreateRadioBox(iyesnop, 1, 9,iwid-2 , k+2, &
	gmType=GFREEFORM, gmBorderType=GPROJECTED, &
		gmFillCol=141,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, &
	gmTitle='States to be excluded:', gmVpos=GTOP)
	
do i=1,k	
	call intconv(i,cnum)
	ino(i) = gmCreateToggleButton(ist_Radio2, 1, i , 4, 1,'State no '//cnum(1:3) ,0 , &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)	
	
enddo
itexth = gmCreateTextEntry(iyesnop, 1, 1, 4, 1,'Starting state #= ', 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=Gbottom, gmExpand=GOFF)


		istart=gmCreateValueEntry(iyesnop,6 ,1 , 4, 1, i1, 10, 0,gedit,&
		gmVpos=Gbottom,gmtextcol=11)


ivb=gmCreatePushButton(iyesnop,1,0, iwid-2, 1,'Continue',&
              	gmType=Guserdefined, gmSize=80,gmoffcol=38,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=10)
	
call gmdrawwindow(inswin)
call gmSetWidgetStatus(istart, GunSELECTABLE)
do i=1,k
		call gmSetWidgetStatus(	ino(i),Gunselectable)
	enddo
1	icall=gmAction(icall)
if(icall.eq.10) then
	
	do i=1,5
		istatus0=gmEnqToggleSwitch(iyes(i))
		if(istatus0.eq.gon) iopt=i

	enddo
	if(iopt.eq.5) then
	    rs=gmenqvaluesetting(istart)
	    i1=int(rs)
		fixed=.true.
		u=random()	 !not used (but leaves ix,iy,iz same as if i1 chosen randomly)
		u=random()
		call gmremovewindow(inswin)
		ist(i1)=-1
		goto 2
	else if(iopt.eq.1) then	!exclude A,B states
	   nexc=kA+kB
	   do i=1,nexc
		iexc(i)=1
	   enddo
	else if(iopt.eq.2) then		!exclude open states
	   nexc=kA
	   do i=1,nexc
		iexc(i)=1
	   enddo
	else if(iopt.eq.3) then		!exclude shut states
	   nexc=kB+kC
	   do i=kA+1,k
		iexc(i)=1
	   enddo
	else if(iopt.eq.4) then
		do i=1,k
		enddo
	endif
	call gmremovewindow(inswin)
	j=0
	s=0.
	do 63 i=1,k
		if(iexc(i).ne.0) goto 63
		j=j+1
		peq1(j)=peq(i)
		ist(j)=i	!state # for peq1(j)
		s=s+peq(i)

63	continue
	do i=1,k1
		peq1(i)=peq1(i)/s	!so sum to 1

	enddo
	pcum(1)=0.
	do i=2,k1+1
		pcum(i)=pcum(i-1) + peq1(i-1)	!so pcum(k+1)=1
	enddo
	u=random()		!uniform(0,1)

	do i=1,k1
	   i1=ist(i)
	   if(u.gt.pcum(i).and.u.le.pcum(i+1)) goto 14	!jump out with i1
	enddo
14	continue
else if (icall.ge.11.and.icall.le.13) then
	do i=1,k
		call gmSetWidgetStatus(	ino(i),Gunselectable)
	enddo
	call gmSetWidgetStatus(istart, GunSELECTABLE)
	goto 1
else if (icall.eq.14) then
	do i=1,k
		call gmSetWidgetStatus(	ino(i),GSELECTABLE)
	enddo
		call gmSetWidgetStatus(istart, GunSELECTABLE)
	goto 1
else if (icall.eq.15) then
	do i=1,k
		call gmSetWidgetStatus(	ino(i),GunSELECTABLE)
	enddo
		call gmSetWidgetStatus(istart, GSELECTABLE)
	goto 1

else
	
	goto 1
endif

2 continue
icall=0
end