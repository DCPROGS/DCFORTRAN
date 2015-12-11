	subroutine CALLINE(ibasevga,calfac1,opendown,ndisp,finter,ibk)
c To draw time and amplitude calibration lines in SCAN
c Default=white ticks/axis = 1 pA/ 5 ms
c If no ticks are on scale then yellow ticks/axis = 0.2 pA/ 1 ms
c Draws calibration if ibk=-1; otherwise erases to background colour=ibk
c
c Modif 11/15/96 03:36pm for case where signal is large (eg junction potential
c jumps) in which case could get idelt=0 giving infinite loop!
c
	logical opendown
	character cnum*11
	character*7 text(2)
	common/datpos/yd1,yd2,iyd1,iyd2	!position of data area
c
c Amplitude calibration: 1 pA ticks starting from baseline position
c	call HLINE(0,639,iyd1-1,14)		!line to mark bottom of data area
c	call HLINE(0,639,iyd2+1,14)	!line to mark top of data area
	if(opendown) then
	   ylen=abs(float(ibasevga-iyd1))
	else
	   ylen=abs(float(ibasevga-iyd2))
	endif
	ytic=1.0 	   			!tic length (pA)
1	continue
	yticp=ytic/calfac1		!tic length (pixels)
	ntic=ifix(ylen/yticp)
	if(ntic.lt.2) then
	   ytic=ytic/5.
	   goto 1
	else if(ntic.gt.20) then
	   ytic=ytic*5.
	   goto 1
	endif
c	itic=ibasevga		!start at baseline (pixel units)
c	idelt=ifixr(1./calfac1)		!1 pA tics
c	if(opendown) then
c	   ok=ibasevga-idelt.gt.iyd1
c	else
c	   ok=ibasevga+idelt.lt.iyd2
c	endif
c	ic=15		!white
c	if(ok) then
c	   ic=15			!white 1pA tics
c	else
c	   ic=14				!yellow
c	   idelt=ifixr(0.2/calfac1)	!0.2 pA
c	endif
c
c
	itic=ibasevga		!start at baseline (pixel units)
	idelt=ifixr(ytic/calfac1)	!tic length (pixels)
	ic=15
	if(ytic.lt.0.9) ic=14
	if(ibk.ge.0) ic=ibk           !erase
c
	call VLINE(0,iyd1,iyd2,ic)	!line at left edge
c
	if(opendown) then
	   do while (itic.gt.iyd1)
		call HLINE(1,6,itic,ic)
		itic=itic-idelt
	   enddo
	else
	   do while (itic.lt.iyd2)
		call HLINE(1,6,itic,ic)
		itic=itic+idelt
	   enddo
	endif
c
c Now Time calibration. Calc from number of points on display=640 pixels
c ndisp*finter/1000.=length of display (ms) =640 pixels
c Draw 5 ms tics normally (yellow), or, if expanded so fewer than 2 tics
c on display, draw 1 ms tics in white
	tdisp=finter*float(ndisp)*0.001	!display length (ms)
	pfac=640./tdisp
	xtic=1.0 	   		!tic length (pA)
2	continue
	ntic=ifix(tdisp/xtic)
	if(ntic.lt.2) then
	   xtic=xtic/5.
	   goto 2
	else if(ntic.gt.20) then
	   xtic=xtic*5.
	   goto 2
	endif
c
	idelt=ifixr(xtic*pfac)	!tic length (pixels)
	ic=15
	if(xtic.lt.0.9) ic=14
	if(ibk.ge.0) ic=ibk           !erase
c
	itic=0	!start at 0
	call HLINE(0,639,iyd1-1,ic)		!line to mark bottom of data area
	do while(itic.lt.639)
	   call VLINE(itic,iyd1,iyd1+6,ic)
	   itic=itic+idelt
	enddo
c
c Write calibration to right of dialog box
	if(ytic.gt.0.9) then
	   iytic=ifixr(ytic)
	   call INTCONV(iytic,cnum)
	else
	   call DCFORMAT(ytic,5,2,cnum)
	   call LJUSTIF(cnum,11)
	endif
	text(1)=charnb(cnum)//' pA'
c	if(ok) then
c	   text(1)='  1 pA'
c	else
c	   text(1)='0.2 pA'
c	endif
	if(xtic.gt.0.9) then
	   ixtic=ifixr(xtic)
	   call INTCONV(ixtic,cnum)
	   text(2)=charnb(cnum)//' ms'
	else if(xtic.ge.0.1) then
	   call DCFORMAT(xtic,4,1,cnum)
	   call LJUSTIF(cnum,11)
	   text(2)=charnb(cnum)//' ms'
	else
	   call DCFORMAT(xtic,5,2,cnum)
	   call LJUSTIF(cnum,11)
	   text(2)=charnb(cnum)//'ms'
	endif
c	if(tdisp.gt.10.) then
c	   text(2)='  5 ms'
c	else
c	   text(2)='  1 ms'
c	endif
	call OPENBOX(5,15,.false.)
	call WRITBOX(5,text,2,15)
	RETURN
	end



