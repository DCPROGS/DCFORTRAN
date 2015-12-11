	program TDEF
c Tests for DEF_ONE
	real*4 stepamp(50),dt(49),theta(50)
	real*4 delt(0:50),delt1(0:50)
	integer jfix(50),kfix(0:50)
	integer*2 iampl(1024)
	logical shut,shutprev
c
	base=-5.
	prevlevel=-105.
	t0=10.
c
	iampl(20)=0
	iampl(21)=-100
	iampl(22)=-50
	iampl(23)=0
	iampl(24)=-100
	iampl(25)=-150
	iampl(26)=-50
	iampl(27)=0
	iampl(28)=-150
	iampl(29)=0
c
	delt(1)=1.
	delt(2)=2.
	delt(3)=120.
	delt(4)=1.
	delt(5)=2.
	delt(6)=3.
	delt(7)=90.
	delt(8)=4.
c
	print 1
1	format(' itype = ')
	read*,itype

	if(itype.eq.1) then
c (1) shut at each end
c should give  nper=3 open periods; ntrans=6
c and delt1(i)=3,120,6,90,4 (i=1,ntrans-1)
	  kt=20
	  ntrans=9
c==
c	  kt=0
c	  ntrans=2
c	  iampl(1)=-100
c	  iampl(2)=0
c	  delt(1)=2.3
c==
	  shutprev=.true.
	  shut=.true.

	else if(itype.eq.2) then
c
c (2) shut at start, open at end
c should give  nper=3 open periods; ntrans=5
c and delt1(i)=3,120,6,90 (i=1,ntrans-1)
	  kt=20
	  ntrans=8
	  shutprev=.true.
	  shut=.false.

	else if(itype.eq.3) then
c (3) open at start, shut at end
 	  kt=21
	  ntrans=8
	  shutprev=.false.
	  shut=.true.
	  do i=1,7
	   delt(i)=delt(i+1)
	  enddo
c should give  nper=3 open periods; ntrans=5
c and delt1(i)=120,6,90,4 (i=1,ntrans-1)
c
	else if(itype.eq.4) then
c (4) open at start, open at end
	  kt=21
	  ntrans=7
	  shutprev=.false.
	  shut=.false.
	  do i=1,7
	    delt(i)=delt(i+1)
	  enddo
c should give  nper=3 open periods; ntrans=4
c and delt1(i)=120,6,90 (i=1,ntrans-1)
	endif
c	subroutine DEF_ONE(kt,t0,delt,shutprev,shut,jfix,kfix,ntrans,
c     & iampl,base,prevlevel,theta,itype,stepamp)
c========== Start of DEF_ONE
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
c Look for sublevel transitions in prev auto-fit (also calc mean open level)
c and concatenate open periods, and get lengths for them in delt1()
c NB ASSUMES WE DO HAVE TWO CONSECUTIVE ZERO AMPLITUDES!!
c
	print*,' ITYPE= ',itype
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
	do i=kt+1,kt+ntrans			!kt not updated yet
	   j=i-kt				!j starts at 1 for i=kt+1
	   if(iampl(i).eq.0) then	!open period ended
		nper=nper+1			!number of open periods
		delt1(nt)=d		!duration of open period just ended
		if(iampl(i-1).ne.0) nt=nt+1	!number of shut/open transitions
		print*,' nop = ',nop
		d=0.0			!reset
		nop=0				!number of openings in current open period
	   else 				!iampl(i).ne.0
		if(i.gt.kt+1.and.iampl(i-1).eq.0) then	!not first time
		   delt1(nt)=delt(j-1)	!insert length of shut interval into delt1
		   nt=nt+1			!number of shut/open transitions
		endif
		n=n+1
		s=s+float(iampl(i))	!accum mean open level
		nop=nop+1			!number of openings in current open period
		d=d+delt(j)			!add durations of indiv open times
	   endif
	enddo
c
c Calc mean open level
	s=s/float(n)	!mean open level
c
	ntrans=nt		!NEW VALUE
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
c=================NO -don't reset amps fixed for short events!
c	do i=1,50		!reset jfix
c 	  jfix(i)=0
cc	  kfix(i)=0
c	enddo
c=============
c
c Define guess for open level in theta(nt+1)
	if(shutprev) then       !initially shut
	   theta(ntrans+1)=s+base	!new guess for amp
	else			!open initially
	   theta(ntrans+1)=prevlevel
	   jfix(ntrans+1)=1
	    kfix(0)=2	!define so print out correct below
	endif
c
c Set # of param (kmax), and define, and fix, shut levels
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
	print*,' nt, nper, base, s = ',nt,nper,base,s
	do i=1,nt-1
	   print*,' i, delt1(i), dt(i)= ',i,delt1(i),dt(i)
	enddo
	pause
	do i=1,kmax
	   print*,' i,theta(i)= ',i,theta(i)
	enddo
	do i=1,nt
	   print*,' i,stepamp(i)= ',i,stepamp(i)
	enddo
c
c	RETURN
	end

