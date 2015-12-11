	subroutine DSFUNC(theta,gval)
c
c To calculate d(SSD)/d(theta(j)) for DFPMIN fittin in SCAN
c Method is in stepresp.mcd
c (DSFUNC1-test version for use with tdf1.for)
c The earlier version (now called dsfunc0) using srfunc() for amplitude
c derivatives was fast but caused 0/0 crash when stepamp() near zero,
c so now use erfs() method
c
c common/min/method added for test of two methods of calculating derivs
c method=0 is DC method
c method=1 is direct method by evaluating SFUNC at 2 param values
c (latter is about 5 times slower, as tested in tdf1.for)
c
c Problem is probably that exp(-x2) and erf() are centred at zero
c so calc starts at middle of erf, and at peak of exp(x) (see stepresp.mcd)
c rather than at their start need to offset by time from 'start' (tf50, as
c defined in normfilt) to midpoint
c
c Calculated values,ydcalc(i) and stepamp(j) are in common from SFUNC1 at
c present so must ensure they are the current values (from same theta as
c in the call here)
c
c Parameters:
c theta(1)= t0 = time in microsec from 1st point on display TO 1st transition
c theta(2)= time in microsec from 1st point on display TO 2nd transition
c . . .
c theta(nt)= time in microsec from 1st point on display TO (nt)th transition
c theta(nt+1)='amplitude' (floating Y units) before 1st trans
c theta(nt+2)='amplitude' (ditto) between 1st and 2nd trans
c theta(nt+3)='amplitude' (ditto) between 2nd and 3rd trans
c . . .
c theta(2*nt)='amplitude' (ditto) between (nt-1)th and nt'th transition
c theta(2*nt+1)='amplitude' (ditto) after (nt)th transition
c
	real*4 data(2048),theta(50),gval(50)
	real*4 stepamp(50)
	real*4 filt1(1024)
	real*4 ydcalc(2048)	!calc values from SCANFUNC (in common/ycal/)
	allocatable::ydev
	real*4 ydev(:)
	allocatable::time
	real*4 time(:)
	real*4 th1(50),th2(50)
c	real*4 gval1(50)	!for debug
	common/abt/abort			!common with simplex1
	common/fitblk/data,if1,if2,ntrans,neval,nevfix,itype,stepamp,y0
	common/ycal/ydcalc	!calc values from SCANFUNC
	common/filtblk/filt1,nfilt1,dtf1,npfilt,finter,filt(640),filtf
	common/consts/c1,c2,tf50
	common/km/kmax		!for dsfunc
c
	common/min/method
	logical silent
	common/err/nerr,silent	!for dfpmin1,sfunc,dsfunc
c
c Constants (now in common)
c	tf50=204.736
c	filtf=2.0	!kHz
c	c1=5.336446*0.001*filtf		!called k in stepresp.mcd
c	c2=-c1/sqrt(3.141593)
c
	if(method.eq.1) goto 100	!use direct method
c
	x=SFUNC(theta) 	!make sure all values are current
	t0=theta(1)
	y0=theta(ntrans+1)	!initial level before trans #1

	ALLOCATE(ydev(2048),time(2048))
c Define ydev()
c===(keep ydev(i), time(i) in common from sfunc?)
	do i=if1,if2
	   ydev(i)=ydcalc(i)-data(i)
	   time(i)=float(i-1)*finter
	enddo
c	endif
c
c First the derivatives with respect to time parameters
c For the first transition, at time=t0,
c No need to start at if1 since slope is zero up to t0, and zero
c again after step response func has reached max
c NB the deriv falls to zero after the transition so can jump out
c of the loop when get beyond exp(-10) say, to save time
	gval(1)=0.0
	do i=if1,if2
	  if(time(i).ge.t0) then
	   x=c1*(time(i)-t0-tf50)	!time starts at -tf50
	   x2=x*x
	   if(x2.gt.10.) goto 1		!jump out after exp(-10) reached
	   ex=exp2(-x2)	!for debug (-> 0 for large t
	   gval(1)=gval(1) +ydev(i)*c2*stepamp(1)*exp2(-x2)
	  endif
	enddo
1	gval(1)=2.0*gval(1)
c
c For rest of the time parameters (if ntrans>1) these are
	if(ntrans.gt.1) then
	   do j=2,ntrans
		gval(j)=0.0
		delt=theta(j)
		do i=if1,if2
		  if(time(i).ge.delt) then
		   x=c1*(time(i)-delt-tf50)	!time starts at -tf50
		   x2=x*x
		   if(x2.gt.10.) goto 2	!jump out after exp(-10) reached
		   ex=exp2(-x2)
		   gval(j)=gval(j) + ydev(i)*c2*stepamp(j)*exp2(-x2)
		  endif
		enddo
2		gval(j)=2.0*gval(j)
	   enddo
	endif
c
c Now derivatives with respect to amplitude parameters
c Parameter # ntrans+1 is y0, so gval(nt+1) is just sum over observations
c of the deviations
	j=ntrans+1
	g=0.0
	do i=if1,if2
	   g=g + ydev(i)
	enddo
	gval(j)=2.0*g
c
c Now the rest of the amplitude parameters
c After the first amplitude parameter, y0, there are ntrans more, for
c the amplitude following the n'th transition, so first time offset
c is t0=theta(1)
c Slope=0 up to time of the transition (as specified in theta), so can start
c at that point and no need to calculate erf(x) for x>3 since
c erfs(3)=0.999978 so take as 1 for x>3
	j2=1
	do j=ntrans+2,2*ntrans+1
	   gval(j)=0.0
	   delt=theta(j2)		!=t0 first time
	   j2=j2+1
	   do i=if1,if2
	    if(time(i).ge.delt) then
		x=c1*(time(i)-delt-tf50)	!time starts at -tf50
		if(x.le.3.) then
		   term=ydev(i)*0.5*(1.0+erfs(x))
		else
		   term=ydev(i)
		endif
		gval(j)=gval(j) + term
	     endif
	   enddo
	   gval(j)=2.0*gval(j)
	enddo
c
	DEALLOCATE(ydev,time)
	RETURN
c
c Method=1: approx derivative is calculated directly from SFUNC
100	continue
	do j=1,kmax
	   do j1=1,kmax
		if(j.eq.j1) then
		   d=0.001*abs(theta(j))
		   th1(j1)=theta(j1) - d
		   th2(j1)=theta(j1) + d
		else
		   th1(j1)=theta(j1)
		   th2(j1)=theta(j1)
		endif
	   enddo
c	   gval1(j)=(SFUNC(th2)-SFUNC(th1))/(th2(j)-th1(j))
	   anum=(SFUNC(th2)-SFUNC(th1))
	   den=th2(j)-th1(j)
	   gval(j)=anum/den
	enddo
c
	RETURN
	end



