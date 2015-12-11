	subroutine EXPMAX(tmax,ftmax,yinf,texp,pon,discprt,debug,nerr)
c To find tmax and f(tmax) by bisection when
c   f(t)=Y(inf) + SUM[w(i)*exp(-t/tau(i))]	(i=1,...,k)
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
	real*4 w(10),tau(10)
	logical pon,discprt,debug
	common/exp/ w,tau,yinf1,k
	EXTERNAL func,func1
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

c   Find smallest tau
	taumin=1.e37
	taumax=-1.e37
	do i=1,k
	   if(tau(i).lt.taumin) taumin=tau(i)
	   if(tau(i).gt.taumax) taumax=tau(i)
	enddo
c
	tmax=5*taumax
	dt=0.01*tmax
	slope=0.0
	t2=tmax
	do while (abs(slope).lt.1.e-5.and.t2.ge.0.)
	   slope=FUNC1(t2)
	   t2=t2 - dt
	enddo
	if(t2.lt.0.) t2=0.01*dt
	slop2=slope		!slope near end
	if(abs(slop2).lt.1.e-6) slop2=0.0	!guard against sign being wrong by rounding
c
c Next check if slope has same sign at very low conc -and check that
c slope is big enough to differ signif from zero
	slope=0.0
	dt=0.01*taumin
	t1=dt
	do while (abs(slope).lt.1.e-5.and.t1.le.tmax)
	   slope=FUNC1(t1)
	   t1=t1 + dt
	enddo
	slop1=slope		!slope at start
c
	t1sav=t1
c
c If slope is same at start and end then there is no min/max
	if(slop1*slop2.gt.0.0) then	!same sign at start and end
	   nerr=-5
	   ftmax=yinf		!no min/max except asymptote
	   goto 9
	endif
c
c If t2<t1 then there is a serious problem.
	if(t2.le.t1) then
	   nerr=-6		!ypeak undefined
	   RETURN
	endif
c
c If get to here then t1, t2 should be OK as initial guesses
	y=0.0		!1st deriv in func=0 at min/max
	epsx=0.001		!error=0.001 ms if tau in ms
	epsy=-1.		!ignore
	ndisp=-2		!silent
	if(debug) ndisp=2
	call BISEC0(FUNC1,t1,t2,y,tmax,Yout,epsx,epsy,
     & nerr1,ndisp,.false.)
c
	if(nerr1.eq.0) then
	   ftmax=FUNC(tmax)
	   if(debug) then
	      f0=FUNC(0.0)
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
	   y=(1.0 - exp(-1.))*yinf
	   epsx=0.001		!error=0.001 ms if tau in ms
	   epsy=-1.		!ignore
	   ndisp=-2		!silent
	   if(debug) ndisp=2
	   t1=t1sav
	   t2=tmax
c
	   call BISEC0(FUNC,t1,t2,y,texp,Yout,epsx,epsy,
     &    nerr1,ndisp,.false.)
	else
	   y=(1.0 - exp(-1.))*ftmax
	   epsx=0.001		!error=0.001 ms if tau in ms
	   epsy=-1.		!ignore
	   ndisp=-2		!silent
	   if(debug) ndisp=2
	   t1=t1sav
	   t2=tmax
c
	   call BISEC0(FUNC,t1,t2,y,texp,Yout,epsx,epsy,
     &    nerr1,ndisp,.false.)
	endif
c
	RETURN
	end

	function FUNC(t)
c Evaluate exponentials
	common/exp/ w(10),tau(10),yinf,k
c=	common/exp/ w(10),tau(10),k
c
	s=0.0
	do i=1,k
	   s=s + w(i)*exp1(-t/tau(i))
	enddo
	func=s+yinf
	RETURN
	end

	function FUNC1(t)
c First derivative of exponentials
	common/exp/ w(10),tau(10),yinf,k
c
	s=0.0
	do i=1,k
	   s=s - (w(i)/tau(i))*exp1(-t/tau(i))
	enddo
	FUNC1=s
	RETURN
	end

