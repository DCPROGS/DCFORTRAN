	subroutine DEF_ONE(kt,t0,delt,dt,shutprev,shut,jfix,kfix,ntrans,
     & scrit,base,prevlevel,theta,itype,y0,stepamp,kmax,opendown,
     & refit,iampl,ntmax,itrigm)
c TEST VERSION WITH IAMPL(NTMAX)
c====needs check to ensure that in output (a) open and shut alternate
c====and (b) never get two consecutive shut times
c===DEFINITION OF OPEN/SHUT IS STILL POOR!
c	subroutine DEF_ONE(kt,t0,delt,dt,shutprev,shut,jfix,kfix,ntrans,
c     & iampl,base,prevlevel,theta,itype,y0,stepamp,kmax)
c   To define amplitude guesses, and stepamp(), needed for 'REFIT -ONE AMP'
c option. In this case openings and shuttings alternate.  If the input
c values (from AUTO-FIT) have sublevel(s) these must be concatenated
c to make a single opening to which the one amplitude is fitted.
c  Need to know when channel shuts for this.  Any deflection in shutting
c direction must be taken as a complete shutting since channel is assumed
c to be either shut or fully open for this fit.
c
c Modified 01/12/94 09:58am.  Now uses iampl()=0 to define which intervals
c are shut when refit=true (ie REFIT 1 AMP), so iampl() is defined, and
c which periods are shut have been specified, if nec, in CHKSHUT.
c   Defining which periods are shut is harder for 'FIT 1 AMP' since iampl()
c is not yet defined and we have only the initial guesses for the FINDALL loop
c to go on.  Shuttings that are detected as being shorter than tshort will
c have amplitude set to baseline in theta, and kfix() will be set, so this
c is used as one criterion for which intervals are shut. Openings with long
c low sublevels are not suitable for 'fit 1 amp' at all, but could have a
c long low sublevel that is so close to baseline that it should be counted
c as shut, so take any interval that is within SCRIT of baseline (or 2*scrit?)
c as being shut, even if it is long.
	real*4 stepamp(50),dt(49),theta(50)
	real*4 delt(0:50),delt1(0:50)
	integer jfix(50),kfix(0:50),kfixnew(0:50),itrigm(50)
	integer*2 iampl(ntmax)
	logical shut,shutprev,shut1,shut2,opendown,refit,shutlast
c
c NB Assume there is enough data somewhere to estimate the one baseline
c and one open level, so no need to fix anything EXCEPT (1) when initially
c open, the open level is fixed at its previous level as in auto-fit: this
c is set below; (2) when open at both ends, the value of BASE is a fixed
c parameter
	do i=1,50		!reset jfix
 	  jfix(i)=0
	  kfixnew(i)=0		!keep input kfix() for now
	enddo
c
c Define itype
	if(shutprev.and.shut) then			!open both ends
	   itype=1              !for use in scnfunc1
	else if(shutprev.and.(.not.shut)) then	!shut at start but not end
	   itype=2
	else if(shut.and.(.not.shutprev)) then	!shut at end but not start
	   itype=3
	else
	   itype=4
	endif
c
c Try to estimate a rough open level, so any level that is far below this
c gets counted as a shutting: try using the 'most open' of the amplitude
c estimates in theta() -NOT NEEDED IN CURRENT VERSION
c	if(opendown) then
c	   oplevel=1.e36
c	   do i=ntrans+1,kmax
c		if(theta(i).lt.oplevel) oplevel=theta(i)
c	   enddo
c	else
c	   oplevel=-1.e36
c	   do i=ntrans+1,kmax
c		if(theta(i).gt.oplevel) oplevel=theta(i)
c	   enddo
c	endif
c
c Look for sublevel transitions in prev auto-fit (also calc mean open level)
c and concatenate open periods, and get lengths for them in delt1()
c NB ASSUMES WE DO NOT HAVE TWO CONSECUTIVE ZERO AMPLITUDES!!
c
c	print*,' ITYPE= ',itype
	s=0.
	n=0
	if(shutprev) then
	   nt=1			!number of shut/open transitions
	else
	   nt=0			!number of shut/open transitions
	endif
	nper=0			!number of open periods
	nop=0 			!number of openings in current open period
	d=0.0 			!duration of open period
c
	do i=kt+1,kt+ntrans			!kt not updated yet
	   j=i-kt				!j starts at 1 for i=kt+1
	   j1=ntrans+j			!index for theta
	   j2=j1+1				!index for theta
	   if(refit) then		!so iampl() is defined
		if(i.eq.1) then
		   shut1=.true.		!shut before 1st opening
		else
		   shut1=iampl(i-1).eq.0
		endif
		shut2=iampl(i).eq.0
	   else			!case where iampl() not yet defined
c define shut1 (like iampl(i-1).eq.0)
		if(j.eq.1) then
c==	         shut1=(abs(oplevel-theta(j1)).gt.scrit).or.	!this is too high!
c==		   shut1=(abs(theta(j1)-base).le.scrit).or.	!like iampl(i-1).eq.0
c		   test1=abs(float(itrigm(j-1))-base)
c		   shut1=(test1.le.scrit).or.	!like iampl(i-1).eq.0
c     &	       kfix(j-1).eq.1
		   shut1=shutprev
		else
		   shut1=shutlast
		endif
c define shut2 (like iampl(i).eq.0)
c==	      shut2=(abs(oplevel-theta(j2)).gt.scrit).or.	!this is too high!
		test2=abs(float(itrigm(j))-base)
c		shut2=(abs(theta(j2)-base).le.scrit).or.	!like iampl(i).eq.0
		shut2=(test2.le.scrit).or.	!like iampl(i).eq.0
     &		kfix(j).eq.1
	   endif
c NB safer to use shutlast in case shut1 or shut2 altered below
c=====above definitions of shut1,shut2 still poor! -better to use iampl()
c=====when called AFTER fit (for 'refit-1-amp'), but before refit
c=====still have problems e.g. if there is a long sublevel (kfix not set)
c=====that is below oplev by more than scrit, it gets counted as shut, and
c=====if it follows a shutting we therefore get 2 successive shut times!
c Check for two successive shuts!
	   if(shut1.and.shut2) then
		if(opendown) then
		   if(theta(j1).lt.theta(j2)) then
			shut1=.false.	!first is 'more' open
		   else
			shut2=.false.	!second is 'more' open
		   endif
		else
		   if(theta(j1).gt.theta(j2)) then
			shut1=.false.	!first is 'more' open
		   else
			shut2=.false.	!second is 'more' open
		   endif
		endif
	   endif
	   shutlast=shut2		!=next shut1
c=	   if(iampl(i).eq.0) then	!open period ended
c==	   if(.not.open1) then 		!open period ended
	   if(shut2) then 		!open period ended
		nper=nper+1			!number of open periods
		delt1(nt)=d		!duration of open period just ended
		if(.not.shut1) nt=nt+1
c==		if(open) nt=nt+1
c=		if(iampl(i-1).ne.0) nt=nt+1	!number of shut/open transitions
c		print*,' nop = ',nop
		d=0.0			!reset
		nop=0			!reset number of openings in current open period
	   else 				!iampl(i).ne.0
c=		if(i.gt.kt+1.and.iampl(i-1).eq.0) then	!not first time
c==		if(i.gt.kt+1.and.(.not.open)) then	!not first time
		if(i.gt.kt+1.and.shut1) then	!not first time
		   if(kfix(j-1).eq.1) kfixnew(nt)=1	!transfer kfix
		   delt1(nt)=delt(j-1)	!insert length of shut interval into delt1
		   nt=nt+1			!number of shut/open transitions
		endif
		n=n+1
		if(refit) then
		   s=s+float(iampl(i))+base	!accum mean open level
		else
		   s=s+theta(j2)	!accum mean open level
		endif
		nop=nop+1			!number of openings in current open period
		d=d+delt(j)			!add durations of indiv open times
	   endif
	enddo
c
c Calc mean open level (not needed if initially open, when open level is fixed
c at prevlevel)
	if(shutprev.and.n.ge.1) then
	   s=s/float(n)	!mean open level
	endif
c
c Set new values
	ntrans=nt		!NEW VALUE
c
	do i=0,ntrans
	   kfix(i)=kfixnew(i)
	enddo
c Use delt1() to set time guesses in theta(1) to theta(nt)
c Initial guesses: time params (none fixed)
	do i=1,ntrans-1
	   if(i.eq.1) then
		dt(i)=delt1(i)
	   else
		dt(i)=dt(i-1) + delt1(i)
	   endif
	enddo
	theta(1)=t0
	if(ntrans.gt.1) then
	   do i=2,ntrans
		theta(i)=t0 + dt(i-1)
	   enddo
	endif
c
c Define guess for open level in theta(nt+1)
	if(shutprev) then       !initially shut
	   theta(ntrans+1)=s		!new guess for amp
	else			!open initially
	   theta(ntrans+1)=prevlevel
	   jfix(ntrans+1)=1
	   kfix(0)=2	!define so print out correct below
	endif
c
c Set # of param (kmax), and define shut levels
	theta(ntrans+2)=base	!guess for baseline
	if(itype.eq.1) then	!shut at both start and end
	   theta(ntrans+3)=base		!guess for baseline at end
	   kmax=ntrans+3
	else
	   kmax=ntrans+2
	   if(itype.eq.4) then	!open at both ends
		 jfix(ntrans+2)=1		!fix shut level
	   endif
	endif
c
c Set y0, and stepamp (to calculate diplay of these init guesses)
	if(itype.eq.1) then	!shut/shut (so ntrans=>2 and even)
	   y0=theta(ntrans+2)	!initial level before trans #1
	   base1=0.5*(theta(ntrans+2)+theta(ntrans+3))	!mean baseline (local var)
	   stepamp(1)=theta(ntrans+1)-theta(ntrans+2)	!use pre-baseline
	   stepamp(ntrans)=theta(ntrans+3)-theta(ntrans+1)	!use post-baseline
	   if(ntrans.gt.2) then
	      sign=-1.0
		do j=2,ntrans-1
		   stepamp(j)=sign*(theta(ntrans+1)-base1)
		   sign=-sign
		enddo
	   endif
	else if(itype.eq.2) then	!shut/open (so ntrans=>1 and odd)
	   y0=theta(ntrans+2)	!initial level before trans #1
	   base1=y0
	   stepamp(1)=theta(ntrans+1)-base1
	   if(ntrans.gt.1) then
	      sign=-1.0
		do j=2,ntrans
		   stepamp(j)=sign*(theta(ntrans+1)-base1)
		   sign=-sign
		enddo
	   endif
	else if(itype.eq.3) then	!open/shut (so ntrans=>1 and odd)
	   y0=theta(ntrans+1)	!initial level open level (will be fixed param)
	   base1=theta(ntrans+2)	!final baseline
	   stepamp(1)=base1-theta(ntrans+1)
	   if(ntrans.gt.1) then
	      sign=1.0
		do j=2,ntrans
		   stepamp(j)=sign*(theta(ntrans+1)-base1)
		   sign=-sign
		enddo
	   endif
	else if(itype.eq.4) then	!open/open (so ntrans=>2 and even)
	   y0=theta(ntrans+1)	!initial level open level (will be fixed param)
	   base1=theta(ntrans+2)	!final baseline (also fixed param)
	   stepamp(1)=base1-theta(ntrans+1)
	   if(ntrans.gt.1) then
	      sign=1.0
		do j=2,ntrans
		   stepamp(j)=sign*(theta(ntrans+1)-base1)
		   sign=-sign
		enddo
	   endif
	endif
c
c  Put new values into delt()
	do i=1,ntrans-1
	   delt(i)=delt1(i)
	enddo
	RETURN
	end

