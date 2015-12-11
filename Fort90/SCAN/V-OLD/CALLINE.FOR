	subroutine CALLINE(ibasevga,calfac1,opendown,ndisp,finter,ibk)
c To draw time and amplitude calibration lines in SCAN
c Default=white ticks/axis = 1 pA/ 5 ms
c If no ticks are on scale then yellow ticks/axis = 0.2 pA/ 1 ms
c Draws calibration if ibk=-1; otherwise erases to background colour=ibk
	logical opendown,ok
	common/datpos/yd1,yd2,iyd1,iyd2	!position of data area
c
c Amplitude calibration: 1 pA ticks starting from baseline position
c	call HLINE(0,639,iyd1-1,14)		!line to mark bottom of data area
c	call HLINE(0,639,iyd2+1,14)	!line to mark top of data area
	itic=ibasevga		!start at baseline (pixel units)
	idelt=ifixr(1./calfac1)		!1 pA tics
	if(opendown) then
	   ok=ibasevga-idelt.gt.iyd1
	else
	   ok=ibasevga+idelt.lt.iyd2
	endif
	ic=15		!white
	if(ok) then
	   ic=15			!white 1ms tics
	else
	   ic=14				!yellow
	   idelt=ifixr(0.2/calfac1)	!0.2 pA
	endif
c
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
	if(tdisp.gt.10.) then
	   idelt=ifixr(5.*640./tdisp)	!pixels/5 ms
	   ic=15				!white
	else
	   idelt=ifixr(640./tdisp)	!pixels/1 ms
	   ic=14				!yellow
	endif
	if(ibk.ge.0) ic=ibk           !erase
	itic=0	!start at 0
	call HLINE(0,639,iyd1-1,ic)		!line to mark bottom of data area
	do while(itic.lt.639)
	   call VLINE(itic,iyd1,iyd1+6,ic)	!msec tics
	   itic=itic+idelt
	enddo
c
	RETURN
	end



