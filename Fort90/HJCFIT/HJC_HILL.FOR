	subroutine HJC_Hill(xcal,ycal,ncalc,nchill,h50,h50hjc,
     & decline,pmax,pop0,ec50,p0hjc,pmaxhjc,ec50hjc,ndc1,ndimc1)
c To calculate values for Hill slope after HJCMEAN called to get apparent
c P(open) values in HJCDISP
c  Input:
c   ycal(i,1) has HJC Popen values (i=1,...,ncalc)
c   ycal(i,2) has ideal Popen values
c	real*8 pmax,pop0,ec50,p0hjc,pmaxhjc,ec50hjc
c Output:
c   ycal(i,3) has Hill slope for HJC Popen values  (i=1,...,nchill)
c   ycal(i,4) has Hill slope for ideal Popen values
c   xcal(i,3)=xcal(i,4) = conc at which nH calculated
c for plot against Popen
c   ycal(i,5) = ycal(i,3)
c   ycal(i,6) = ycal(i,4)
c   xcal(i,5) = ycal(i,1)	!Popen
c   xcal(i,6) = ycal(i,2)
c
c   nchill = number of points in both of above (for Popen=1% to 99% only)
c   h50 = real*4 Hill slope at ec50 (ideal)
c   h50hjc = real*4 Hill slope at ec50 (HJC)
c
	real*4 xcal(ndc1,ndimc1),ycal(ndc1,ndimc1)
	real*8 pmax,pop0,ec50,p0hjc,pmaxhjc,ec50hjc
	logical decline  	!d/r curve goes down with conc
c
c Thus do Hill slopes only for equilib d/r curve (which will not sag normally,
c and will be same as peak curve for cases without desens/block) -in ycal(i,1)
c
c If d/r curve declines with conc, swap p0 and pax (internally in this
c subroutine only at the moment)
	if(decline) then
	   y00=sngl(pmax)
	   yinfeq=sngl(pop0)
	   y00hjc=sngl(pmaxhjc)
	   yinfhjc=sngl(p0hjc)
	else
	   y00=sngl(pop0)
	   yinfeq=sngl(pmax)
	   y00hjc=sngl(p0hjc)
	   yinfhjc=sngl(pmaxhjc)
	endif
	ec50s=sngl(1.d6*ec50)	!micromolar (lik xcal())
	ec50hjcs=sngl(1.d6*ec50hjc)
c
	n=0		!number of calc values
	i50=0		!for ideal
	i50h=0		!for HJC
c skip points above 99% of max (gets lost in rounding errors)
	do i=1,ncalc-1
	   if((ycal(i,2).gt.1.e-10.and.xcal(i,2).gt.1.e-10).and.
     &     (abs((yinfeq-ycal(i+1,2))/yinfeq).ge.0.01).and.
     &     (abs((yinfeq-ycal(i+1,2))/yinfeq).le.0.99)) then
		n=n+1
		y1h=alog(abs((ycal(i,1)-y00hjc)/(yinfhjc-ycal(i,1))))	!HJC
		y2h=alog(abs((ycal(i+1,1)-y00hjc)/(yinfhjc-ycal(i+1,1))))
		y1=alog(abs((ycal(i,2)-y00)/(yinfeq-ycal(i,2))))	!ideal
		y2=alog(abs((ycal(i+1,2)-y00)/(yinfeq-ycal(i+1,2))))
		x1h=xcal(i,1)
		x2h=xcal(i+1,1)
		x1=xcal(i,2)
		x2=xcal(i+1,2)
c==		if(x1.le.ec50eq.and.x2.ge.ec50eq) i50=i
		if(x1h.le.ec50hjcs.and.x2h.ge.ec50hjcs) i50h=n
		if(x1.le.ec50s.and.x2.ge.ec50s) i50=n

		x1=alog(x1)
		x2=alog(x2)
		ycal(n,3)=(y2-y1)/(x2-x1)
		ycal(n,4)=(y2h-y1h)/(x2-x1)
		xcal(n,3)=(xcal(i,1)+xcal(i+1,1))/2.0		!not log
		xcal(n,4)=xcal(n,3)
c
		ycal(n,5) = ycal(n,3)
		ycal(n,6) = ycal(n,4)
c
		xcal(n,5) = ycal(i,1)	!Popen as X axis
		xcal(n,6) = ycal(i,2)
	   endif
	enddo
	nchill=n		!number of calc values
c
c  Interpolate linearly for Hill slope at EC50
	if(i50h.gt.0) then	!interpolate linearly
	   b=(ycal(i50h+1,3)-ycal(i50h,3))/(xcal(i50h+1,3)-xcal(i50h,3))
	   h50hjc=ycal(i50h,3) + b*(ec50hjcs-xcal(i50h,3))
	endif
	if(i50.gt.0) then	!interpolate linearly
	   b=(ycal(i50+1,4)-ycal(i50,4))/(xcal(i50+1,4)-xcal(i50,4))
	   h50=ycal(i50,4) + b*(ec50s-xcal(i50,4))
	endif
c
	RETURN
	end





