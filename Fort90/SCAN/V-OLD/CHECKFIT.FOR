	subroutine CHECKFIT(kt,ntrans,delt,iampl,iprops,jfix,kfix,
     & theta,dt,stepamp,tmin,ifitmode,iampcrit,base,refit,
     & tshort,avamp,opening,opendown,kmax,t0,ntmax)
c Before accept/reject/refit in SCAN, check results of fit to see
c  (1) Whether any fitted durations are too short to be of interest
c	  (below tmin, e.g. 15 microsec)
c  (2) Whether any fixed or constrained amplitudes are, after fitting,
c	actually long enough that they need not have been constrained/fixed
c -if such cases found then offer an automatic refit with ntrans fixed and
c appropriately adjusted init guesses/constraints
	integer*2 iampl(ntmax)
	integer*1 iprops(ntmax)	!properties of ith interval (see SCAN)
	integer*2 iampsav(51)
	integer*1 ipropsav(51)
	real*4 delt(0:50),deltsav(0:50)
	real*4 dt(49),theta(50),stepamp(50)
	integer jfix(50),kfix(0:50)
	logical refit
	logical opening(50),down,opendown
	character ans,defolt*30,cans*30		!to hold text & result of QDIALOG
	character*11 cnum
c
c First look for short gaps (below tmin microsec).
c NB Before kt updated:
c iampl(kt) is the amplitude of the event of length delt(0) (before 1st trans)
c iampl(kt+1) is the amplitude of the event of length delt(1)
c NB If duration is set unusable (iprops=>8) it is supposed to be resolvable
c Save orig values in case refit not wanted!
	ntsav=ntrans
	do i=0,ntrans-1
	   deltsav(i)=delt(i)
	enddo
	do i=1,ntrans
	   iampsav(i)=iampl(kt+i)
	   ipropsav(i)=iprops(kt+i)
	enddo
c
c Save initial level (assume this not changed) whether open or shut
	y0=theta(ntrans+1)	!internal value of y0
c
	call TC1(kt,ntrans,iampl,iprops,delt,iampcrit,tmin,
     &  ifitmode,ntmax)
c
	refit=.false.
c
	if(ntrans.lt.ntsav) then      !short trans found
	   ans='N'
	   call DCFORMAT(tmin,7,1,cnum)		!duration
	   call BELL(1)
	   call DEFOLTa(ans,defolt)
	   if(ntrans.gt.0) then
	      call QDIALOG(1,
     &      'Refit without intervals shorter than'//cnum//' microsec'
     &      ,defolt,12,cans)
	      call GETINPa(cans,ans)
	   else
	      call QDIALOG(1,
     &      'Event shorter than'//cnum//' microsec: skip fit'
     &      ,defolt,12,cans)
	      call GETINPa(cans,ans)
		if(ans.eq.'Y') ans='N'	!so orig values restored, below!
	   endif
	   if(ans.eq.'Y') then	!define new theta and refit
		refit=.true.
c	    doing refit so reset jfix,kfix here -zero them first
		do i=0,ntsav
		   kfix(i)=0
		enddo
		do i=1,kmax
		   jfix(i)=0
		enddo
c
		t0=t0 + delt(0)-deltsav(0)	!set t0 if delt(0) changed
		do i=1,ntrans-1
		   if(i.eq.1) then
			dt(i)=delt(i)
		   else
			dt(i)=dt(i-1) + delt(i)
		   endif
		enddo
		theta(1)=t0
		do i=2,ntrans
		   theta(i)=t0 + dt(i-1)	!theta(1)=t0, unchanged
		enddo
c	Now define amplitude elements of theta() as for ifitmode=0
		j1=ntrans+1		!numbering of theta based on new ntrans
		theta(j1)=y0	!initial value
		if(kfix(0).eq.0) then	!initially open so fix amplitude
		   jfix(j1)=0
		else
		   jfix(j1)=1	!first amp level fixed
		endif
		k=kt
		do i=1,ntrans
		   j1=j1+1		!index for theta =ntrans+2, ntrans+3,...
		   k=k+1		!index for iampl =kt+1, kt+2,...,kt+ntrans
		   theta(j1)=float(iampl(k)) + base
		   stepamp(i)=theta(j1)-theta(j1-1)	!for display of guesses
		   down=theta(j1).lt.theta(j1-1)
	         opening(i)=(down.eqv.opendown)	!needed in scanfunc
		enddo
c       Next redefine jfix,kfix,iprops -(fixamp will be set true after
c	   return to FITSUB if nec).  Here, jfix,kfix,iprops were reset to zero
c	   above -now redefine them by looking for durations< tshort in revised
c	   guesses for durations
c	  NB do not change kfix(0) or jfix(ntrans+1) which were set above
		do i=1,ntrans-1
		   if(delt(i).lt.tshort) then		!short event  -fix amplitude
c============PROBLEM 'not opening' does not imply SHUT!
		      if(opening(i)) then		!=short opening
			   theta(i+1)=base+avamp	!ie fix amplitude at mean open
		         kfix(i)=2  		!fixed at open level
			   jfix(ntrans+i+1)=1
c			   iprops(kt+i)=2    !fixed at mean open level (avamp)
			   iprops(kt+i)=IBSET(iprops(kt+i),1)	!set bit 1 = '2'
			else 			!shutting
			   theta(i+1)=base	!ie fix amplitude at baseline
		         kfix(i)=1  		!fixed at baseline level
			   jfix(ntrans+i+1)=1
c			   iprops(kt+i)=2    !fixed at mean shut level (base)
			   iprops(kt+i)=IBSET(iprops(kt+i),1)	!set bit 1 = '2'
			endif
		   endif
		enddo
c        finally, zero the values above the new ntrans (not now defined)
		do i=ntrans+1,ntsav
		   j=kt+1
		   iampl(j)=0
		   iprops(j)=0
		   opening(i)=.false.
		enddo
c No refit wanted, so restore original values
	   ELSE	!no refit -restore orig values
		refit=.false.
		if(ntrans.eq.0) refit=.true.	!so fit rejected in FITSUB
		ntrans=ntsav
		do i=0,ntrans-1
		   delt(i)=deltsav(i)
		enddo
		do i=1,ntrans
		   iampl(kt+i)=iampsav(i)
		   iprops(kt+i)=ipropsav(i)
		enddo
	   endif
	endif
c
	RETURN
	end

	subroutine TC1(kt,ntrans,iampl,iprops,delt,iampcrit,tmin,
     &  ifitmode,ntmax)
	integer*2 iampl(ntmax)
	integer*1 iprops(ntmax)	!properties of ith interval (see SCAN)
	real*4 delt(0:50)		!temp storage for transition search results
	logical bad,diffamp,btest
c
	bad=.false.
	nt=ntrans
	ki=-1		!counts concat intervals -start at delt(0)
	k=kt-1	!index for iprops(), iampl()
	i=k-kt 	!index for delt()=0,1,...,
	i=i+1
	k=k+1
c First find good interval to start concat group as in RESINT- normally delt(0)
c will be long (though possibly set unusable -but still counted resolvable)
c	if(delt(i).ge.tmin.or.iprops(k).ge.8) goto 2	!found good interval
	if(delt(i).ge.tmin) goto 2
	if(k.eq.0) then
	   goto 2		!undefined shutting before 1st opening assumed good
	else
	   if(BTEST(iprops(k),3)) goto 2	!bit 3 set? (='8')
	endif
1	i=i+1
	k=k+1
	if((delt(i).ge.tmin.or.BTEST(iprops(k),3)).and.
     &	(delt(i-1).ge.tmin.or.BTEST(iprops(k-1),3))) goto 2	!found good interval
	goto 1      !keep looking for start
c    Now start looking for short intervals
2	ki=ki+1
	call SUMAMP(-1,S,iampl,n,ntmax)
	delt(ki)=delt(i)			!start new concat group
	call SUMAMP(k,S,iampl,n,ntmax)
c is next interval resolvable? If so, assign amp and carry on
7	if(i.ge.ntrans-1) goto 4		!last delt() reached
	if(delt(i+1).ge.tmin.or.BTEST(iprops(k+1),3)) goto 4	!yes-resolvable
c Next bit done when NOT resolvable. Has the interval after it got the 'same'
c amp as that before it? NOTE it is safe to refer to iampl(k+2) here
c because amplitude of interval after the last transition is known, even
c though its duration is not yet defined.
	tin=delt(ki)
	bad=BTEST(iprops(k+1),3)	!bit 3 set (='8')
c If ifitmode=0 or 3 then may have openings of diff amplitude on each
c side of an irresolvable gap
	if(ifitmode.eq.1.or.ifitmode.eq.2) goto 9		!all same amp
	if(DIFFAMP(k,k+2,iampl,iampcrit,ntmax)) goto 5
c
c Now bit to be done if delt(i+1) is unresolvable and intervals on each side
c of it have the same amp: concatenate.
9	continue
c NB -DELT(I+2) NOT LEGAL -but if referred to here, delt(i+2) refers
c to the (still unknown) duration of the interval after the last transition
c which is being concatenated with earlier intervals -but do not need
c duration for this (though do need amplitude), so just skip delt(ki) in
c this case
	if(i+2.le.ntrans-1) then
	   delt(ki)=tin+delt(i+1)+delt(i+2)
c	else
c	   print 77,i+2
c77	   format(' DELT(',i2,') NOT LEGAL')
	endif
c   If any interval is unusable set the whole group unusable!
c	bad=iprops(k+1).ge.8.or.iprops(k+2).ge.8
	bad=BTEST(iprops(k+1),3).or.BTEST(iprops(k+2),3)
	call SUMAMP(k+2,S,iampl,n,ntmax)
	i=i+2
	k=k+2
	nt=nt-2	!number of transitions reduced by 2
	goto 7	!continue concat until resolvable interval found
c
c Next bit done if delt(i+1) unresolvable but intervals on each side of it
c have 'different' amps.
5	tin1=delt(i+1)
	delt(ki)=tin+0.5*tin1
c	bad=iprops(k+1).ge.8.or.iprops(k+2).ge.8
	bad=BTEST(iprops(k+1),3).or.BTEST(iprops(k+2),3)
	delt(ki+1)=0.5*tin1+delt(i+2)
c   Concat of delt(ki) now ended so assign amp to it
	if(n.gt.0) iampl(kt+ki)=IFIXR(S/float(n))
C   TINT(KI+1) (APPROX=TINT(I+2)) MAY NEED MORE THINGS CONCAT WITH IT
C   SO INITIALISE SUMS AND START NEW ONES.
	call SUMAMP(-1,S,iampl,n,ntmax)
	call SUMAMP(k+2,S,iampl,n,ntmax)
	ki=ki+1
	i=i+2
	k=k+2
	nt=nt-1	!number of transitions reduced by 1
	goto 7		!CARRY ON CONCAT
c
4	continue
c  Finished a concatenated group
c Assign amp if concatenation finished
	if(n.gt.0) iampl(kt+ki)=IFIXR(S/float(n))
	if(bad) then
c	   iprops(kt+ki)=8
	   iprops(kt+ki)=IBSET(iprops(kt+ki),3)	!set bit 3 (='8')
	   bad=.false.
	else
c	   iprops(kt+ki)=0
	   if(kt+ki.gt.0) then
	      iprops(kt+ki)=IBCLR(iprops(kt+ki),3)	!clear bit 3 (='8')
	   endif
	endif
	i=i+1
	k=k+1
	if(i.le.ntrans-1) goto 2
c
c All done:
c Need to assign amplitude that follows the last transition (as revised here),
c if not already done
	if(ki.lt.nt) iampl(kt+nt)=iampl(kt+ntrans)
	ntrans=nt	!new number of transitions
	RETURN
	end

	Logical function DIFFAMP(i1,i2,iampl,iampcrit,ntmax)
	integer*2 iampl(ntmax)
c
	diffamp=.true.
	if(iampl(i1).eq.0.and.iampl(i2).eq.0) diffamp=.false.
	if(iampl(i1).ne.0.and.iampl(i2).ne.0.and.
     &  iabs(iampl(i1)-iampl(i2)).lt.iampcrit) diffamp=.false.
	RETURN
	end

	subroutine SUMAMP(k,S,iampl,n,ntmax)
	integer*2 iampl(ntmax)
c k=-1 initialises sum
	if(k.lt.1) then
	   n=0
	   S=0.0
	else
	   n=n+1
	   S=S+float(iampl(k))
	endif
	RETURN
	END


