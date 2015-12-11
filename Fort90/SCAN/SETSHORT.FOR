	subroutine SETSHORT(ix2,iy2,ylevel,t0,dt,itrans,ntrans,cfac,
     & finter,npfilt,data,if1,if2,opendown,base,avamp,timamax,tmin2)
c
c Called by SETSUBL to insert a short event at point marked by mouse
c click at ix2,iy2, and define dt(), dt() and ylevel(), ylevel()
c
c itrans=current transition number -level before this =ylevel(itrans)
c NB ylevel(nt)=current level (intermed units) BEFORE transition #nt
c    ylevel(nt+1)=current level (intermed units) AFTER transition #nt
c
	integer*2 ix2,iy2
	real*4 data(2048)
	real*4 ylevel(50),dt(49)
	real*4 timamax(49)
	logical opendown,shutting
c
c Locate point to start looking in data from ix2
	t=float(ix2)/cfac		!cfac=dx/finter
	it1=1+ifix(t/finter)		!index in data (=1 when t0=0)
	if(it1.lt.if1) it1=if1
	it2=it1+ifixr(1.8*float(npfilt))
	if(it2.gt.if2) it2=if2
c Search data(it1) to data(it2) for extreme point
	ymin=1.e38
	ymax=-1.e38
	do i=it1,it2
	   if(data(i).gt.ymax) ymax=data(i)
	   if(data(i).lt.ymin) ymin=data(i)
	enddo
c
c define level before 1st of the two trans (could also take from mouse=iy2)
	y0=ylevel(itrans)
	if(abs(ymax-y0).gt.abs(ymin-y0)) then		!upwards on screen
	   shutting=opendown
	   dext=ymax
	else
	   shutting=.not.opendown
	   dext=ymin
	endif
c
	if(shutting) then
	    frac=abs((dext-y0)/(y0-base))
	else
	    frac=abs((dext-y0)/avamp)
	endif
	i=ifixr(frac/0.02)
	if(i.lt.1) i=1
	if(i.gt.49) i=49
	delt=timamax(i)
c
c Define time of first transition of the pair
	if(itrans.eq.1) then
c         NB x0 defines where display of calc curve starts
	   x0=float(ix2)
	   t0=x0/cfac		!cfac=dx/finter
	   if(t0.lt.0.) t0=tmin2
	else		!itrans>1
	   t=float(ix2)/cfac
	   dt(itrans-1)=t-t0
	   if(itrans.eq.2) then
		if(t.le.t0+tmin2) dt(1)=tmin2
	   endif
	   do i=2,ntrans-1
		if(dt(i).le.dt(i-1)+tmin2) then
		   dt(i)=dt(i-1)+tmin2
		endif
	   enddo
	endif
c Define amplitude of first transition of the pair
	if(shutting) then
	   ylevel(itrans+1)=base
	else
	   ylevel(itrans+1)=base+avamp
	endif
c
c Define time of 2nd transition of the pair
	if(itrans.eq.1) then
	   dt(1)=delt
	else
	   dt(itrans)=dt(itrans-1)+delt
	endif
c Define amplitude of 2nd transition of the pair
	ylevel(itrans+2)=y0		!same as initial level
c
	RETURN
	end

