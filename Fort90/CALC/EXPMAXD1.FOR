	subroutine EXPMAXd1(tmax,ftmax,yinf,texp,w,tau,yinf1,k,
     &	pon,discprt,debug,km,nerr)
c Expmaxd1 is version if expmaxd with w,tau,yinf1,k as parameters, rather
c than in common.  Also EXPFUNC, EXPFUNC1 now separate to avoid duplicate definition
c
c To find tmax and f(tmax) by bisection when
c   f(t)=Y(inf) + SUM[w(i)*exp(-t/tau(i))]	(i=1,...,k)
c
c Double precision version of expmax
c
c Modif 10/16/97 09:05pm to calculate also an 'effective time constant'
c defined as texp=time to reach (1-1/e) of final value where 'final value'
c is defined as =asymptote if monotonic, or =ypeak if there is a peak.
c
c Possible problem: can run out of sig figs in single precision calc
c of FUNC(t2)-FUNC(t1)
c  Returns
c	nerr=0 if tmax, ftmax found successfully
c	nerr=-5 if ftmax=yinf is returned -no stationary point
c     nerr=-6 if neither defined
c     Any other non-zero value for nerr means error #nerr in BISEC0
c
	real*8 w(km),tau(km),yinf1
	real*8 tmax,ftmax,yinf,texp
	real*8 t,tlast,slast
	real*8 taumin,taumax,t1,t2,dt,slope,zero
	real*8 y,epsx,epsy,yout,f0
	logical pon,discprt,debug
	EXTERNAL EXPFUNC,EXPFUNC1
c
c First deriv is
c	f'(t)=SUM[-(w(i)/tau(i))*exp(-t/tau(i))]
c
	yinf1=yinf	!copy for common
	nerr=0
	if(debug) then
         print 2,yinf
         if(pon) write(7,2) yinf
         if(discprt) write(8,2) yinf
2	format(' Find tmax for f(t) = Y(inf) + SUM[w(i)*exp(-t/tau(i))]',/
     & ,' Y(inf) = ',g13.6,/,'         w(i)        tau(i)')
	   do i=1,k
      	print 3,i,w(i),tau(i)
      	if(pon) write(7,3) i,w(i),tau(i)
      	if(discprt) write(8,3) i,w(i),tau(i)
3		format(i3,2x,g13.6,2x,g13.6)
	   enddo
	endif
c
c
c Find initial guesses, x1,x2
c In this version, try another approach to the problem of zero slope
c at the end of the relaxation.  Start at end (5*taumax say) and step
c down to smaller time until a slope with absolute value that differs
c significantly from zero is found.  Then use this as upper limit for
c the solution, and use t=0 as lower limit.

	zero=0.0d0
c   Find smallest tau
	taumin=1.d37
	taumax=-1.d37
	do i=1,k
	   if(tau(i).lt.taumin) taumin=tau(i)
	   if(tau(i).gt.taumax) taumax=tau(i)
	enddo
c
c Another version -just look through 100 equally spaced steps and
c look for change in sign of slope
	tmax1=3.d0*taumax
	dt=0.01d0*tmax1
	t=0.1d0		!ms
	do n=1,100
	   slope=EXPFUNC1(t)
	   if(n.gt.1) then
		if(slope*slast.lt.zero) then	 !sign changed
		   t1=tlast
		   t2=t
		   goto 10
		endif
	   endif
	   slast=slope
	   tlast=t
	   t=t + dt
	enddo
c If get to here then curve is monotonic
	nerr=-5
	ftmax=yinf		!no min/max except asymptote
	goto 9
c Jump to 10 if slope changes sign between t1 and t2
10	continue
c
c If get to here then t1, t2 should be OK as initial guesses
	y=zero			!1st deriv in EXPFUNC=0 at min/max
	epsx=0.001d0		!error=0.001 ms if tau in ms
	epsy=-1.d0			!ignore
	ndisp=-2  			!silent
	if(debug) ndisp=2
	call BISECd(EXPFUNC1,t1,t2,y,tmax,Yout,epsx,epsy,
     & nerr1,ndisp,.false.)
c
	if(nerr1.eq.0) then
	   ftmax=EXPFUNC(tmax)
	   if(debug) then
	      f0=EXPFUNC(zero)
		print 1,ftmax,tmax,f0
	      if(pon) write(7,1) ftmax,tmax,f0
	      if(discprt) write(8,1) ftmax,tmax,f0
1		format(
     &	' f(tmax) = ',g13.6,' at tmax = ',g13.6,/,
     &	' f(0.0) = ',g13.6)
	   endif
	else
	   nerr=nerr1
	   if(debug) then
      	print 5,nerr1
		if(pon) write(7,5) nerr1
		if(discprt) write(8,5) nerr1
5		format(' ERROR in EXPMAX: nerr1 = ',i5)
	   endif
	endif
c
9	continue
c	Now find texp, defined as above
	if(nerr.eq.-5) then	!monotonic
	   y=(1.0 - dexp(-1.d0))*yinf
	   epsx=0.0001d0		!error=0.0001 ms if tau in ms
	   epsy=-1.d0		!ignore
	   ndisp=-2		!silent
	   if(debug) ndisp=2
	   t1=dt
	   t2=tmax1
c
	   call BISECd(EXPFUNC,t1,t2,y,texp,Yout,epsx,epsy,
     &    nerr1,ndisp,.false.)
	else		!look between t=0 and peak (at tmax)
	   y=(1.0d0 - dexp(-1.d0))*ftmax
	   epsx=0.0001d0		!error=0.0001 ms if tau in ms
	   epsy=-1.d0		!ignore
	   ndisp=-2	 		!silent
	   if(debug) ndisp=2
	   t1=zero
	   t2=tmax
c
	   call BISECd(EXPFUNC,t1,t2,y,texp,Yout,epsx,epsy,
     &    nerr1,ndisp,.false.)
	endif
c
	if(nerr1.ne.0) call BELL(1)
	if(nerr.ne.0.and.nerr.ne.-5) call BELL(1)
	RETURN
	end
