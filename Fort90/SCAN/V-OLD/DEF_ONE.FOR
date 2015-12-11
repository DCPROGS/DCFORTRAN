	subroutine DEF_ONE(kt,t0,delt,dt,shutprev,shut,jfix,kfix,ntrans,
     & scrit,base,prevlevel,theta,itype,y0,stepamp,kmax,
     & refit,iampl,ntmax,dtrigm)
c====needs check to ensure that in output (a) open and shut alternate
c====and (b) never get two consecutive shut times
c===DEFINITION OF OPEN/SHUT IS STILL POOR!
c
c   To define amplitude guesses, and stepamp(), delt, theta needed for
c the 'FIT -ONE AMP' and 'REFIT -ONE AMP' options.
c In this case openings and shuttings alternate.  If the input
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
	real*4 dtrigm(50)
	integer jfix(50),kfix(0:50),kfixnew(0:50)
	integer*2 iampl(0:ntmax)
	logical shut,shutprev,shut1,shut2,refit,shutlast
	logical setkfix
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
c	if(shutprev) then
c	else
c	   nt=0			!number of shut/open transitions
c	endif
	nt=1				!counts number of shut/open transitions
	npop=0			!number of open periods
	nps=0				!number of shut periods
	nop=0 			!number of openings in current open period
	dop=0.0 			!duration of open period
	ns=0  			!ditto for shut periods
	ds=0.0
	setkfix=.false.
c Loop starts with iampl(kt+1)=amplitude following the first transition
c of the current fit.
c
c Define shut1, shut2 (first time round loop these refer to amplitude
c before and after 1st transition of current fitting, respectively).
	do i=kt+1,kt+ntrans			!kt not updated yet
	   j=i-kt				!j starts at 1 for i=kt+1
	   j1=ntrans+j			!index for theta
	   j2=j1+1				!index for theta
	   if(refit) then		!so iampl() is defined
c		if(i.eq.1) then
c		   shut1=.true.		!shut before 1st opening
c		else
c		   shut1=iampl(i-1).eq.0	!=iampl(kt) for i=1
c		endif
		shut1=iampl(i-1).eq.0	!=iampl(0)=0 for i=1
		shut2=iampl(i).eq.0
	   else			!case where iampl() not yet defined
c define shut1 (like iampl(i-1).eq.0)
		if(j.eq.1) then
		   shut1=shutprev
		else
		   shut1=shutlast
		endif
c define shut2 (like iampl(i).eq.0)
c==	      shut2=(abs(oplevel-theta(j2)).gt.scrit).or.	!this is too high!
		test2=abs(dtrigm(j)-base)
c===try 2*scrit!
		shut2=(test2.le.2.*scrit).or.kfix(j).eq.1	!like iampl(i).eq.0
		if(kfix(j).eq.2) shut2=.false.		!fixed open
	   endif
c
	   shutlast=shut2		!=next shut1
c
c NB safer to use shutlast in case shut1 or shut2 altered below
c=====above definitions of shut1,shut2 still poor! -better to use iampl()
c=====when called AFTER fit (for 'refit-1-amp'), but before refit
c=====still have problems e.g. if there is a long sublevel (kfix not set)
c=====that is below oplev by more than scrit, it gets counted as shut, and
c=====if it follows a shutting we therefore get 2 successive shut times!
c
c Modif 02/05/94 12:05pm. NB
c (1) If fitting starts in shut state:
c    (a) amp after 1st transition could be set shut, in which case all leading
c		shut times must be added to t0, until first open time reached,
c	(b) then concatenate 1st opening with any contigous openings to
c		 define delt(1), etc; when shutting found then
c	(c) concatenate shut times to get delt(2) ...
c (2) If fitting starts in open state:
c    (a) amp after 1st transition may also ne open, in which case all leading
c		open times must be added to t0, until first shutting reached,
c	(b) then concatenate 1st shutting with any contigous shut times to
c		 define delt(1), etc; when opening found then
c	(c) concatenate open times to get delt(2) ...
c
	   if(shut2) then 		!open period ended
		if(j.gt.1) then		!not first time!
c          Record the open period just ended
		   npop=npop+1			!number of open periods
		   delt1(nt)=dop		!duration of open period just ended
		   if(.not.shut1) nt=nt+1
c		   print*,' nop = ',nop
		   dop=0.0			!reset for the next one
		   nop=0			!reset number of openings in current open period
		endif
c        Accumulate durations of shuttings
		if(j.eq.1.and.shutprev) then
		   ns=ns+1
		   t0=t0+delt(j)
		else
		   ns=ns+1			!number of shuttings in current shut period
		   ds=ds+delt(j)		!add durations of indiv shut times
		   if(kfix(j).eq.1) setkfix=.true.	!transfer kfix
		endif
	   else if(.not.shut2) then		!open period ended (iampl(i).ne.0)
c           Record the shut period just ended
		if(j.gt.1) then	!not first time
		   nps=nps+1
		   delt1(nt)=ds		!duration of shut period just ended
		   if(setkfix) then
			kfixnew(nt)=1
			setkfix=.false.
		   endif
		   if(shut1) nt=nt+1	!number of shut/open transitions
		   ns=0 			!reset for next
		   ds=0.0
		endif
c        Accumulate amplitude guess
		n=n+1
		if(refit) then
		   s=s+float(iampl(i))+base	!accum mean open level
		else
		   s=s+theta(j2)	!accum mean open level
		endif
c        Accumulate durations of openings
		if(j.eq.1.and.(.not.shutprev)) then
		   nop=nop+1
		   t0=t0+delt(j)
		else
		   nop=nop+1		!number of openings in current open period
		   dop=dop+delt(j)	!add durations of indiv open times
		endif
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

