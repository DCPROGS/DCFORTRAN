	subroutine CHECKFIT(kt,ntrans,delt,iampl,iprops,jfix,kfix,
     & theta,dt,stepamp,ylevel,tmin,ifitmode,iampcrit,base,refit,
     & tshort,avamp,opening,opendown,kmax,t0,ntmax)
c Before accept/reject/refit in SCAN, check results of fit to see
c  (1) Whether any fitted durations are too short to be of interest
c	  (below tmin, e.g. 15 microsec)
c  (2) Whether any fixed or constrained amplitudes are, after fitting,
c	actually long enough that they need not have been constrained/fixed
c -if such cases found then offer an automatic refit with ntrans fixed and
c appropriately adjusted init guesses/constraints
c
c Modif 05/20/98 08:37am to ensure that 'bad' does not get cleared
c  when first interval already set bad
c   And weighted mean amplitude now used as in resint5
c Modif 04/02/98 10:09am so refit without intervals below tmin is automatic
c Modif 09/15/94 12:20pm so never sets kfix()=2 in CHECKFIT, to fix short
c openings to average amp -this causes problems particularly when fitting
c an opening larger than AVAMP (but NOT doing it may cause problems too?)
	integer*2 iampl(0:ntmax)
	integer*1 iprops(ntmax)	!properties of ith interval (see SCAN)
	integer*2 iampsav(51)
	integer*1 ipropsav(51)
	real*4 delt(0:50),deltsav(0:50)
	real*4 dt(49),theta(50),stepamp(50)
	real*4 ylevel(50)		!temp storage for transition search results
	integer jfix(50),kfix(0:50)
	logical refit
	logical opening(50),down,opendown
	character UC,ans,defolt*30,cans*30		!to hold text & result of QDIALOG
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
	enddo
	i1=0
	if(kt.eq.0) i1=1
	do i=i1,ntrans
	   ipropsav(i+1)=iprops(kt+i)
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
	   ans='Y'
	   call DCFORMAT(tmin,7,1,cnum)		!duration
	   call TONE2(444,352,20)
c	   call BELL(1)
	   call DEFOLTa(ans,defolt)
	   if(ntrans.gt.0) then
c	      call QDIALOG(1,
c     &      'Refit without intervals shorter than'//cnum//' microsec'
c     &      ,defolt,12,cans)
c	      call GETINPa(cans,ans)
	      call WDIALOG(1,
     &   'Refitting without intervals shorter than'//cnum//' microsec',
     &	12)
		ans='Y'
	   else
	      call QDIALOG(1,
     &      'Event shorter than'//cnum//' microsec: skip fit'
     &      ,defolt,12,cans)
	      call GETINPa(cans,ans)
		if(ans.eq.'Y') ans='N'	!so orig values restored, below!
	   endif
	   if(ans.eq.'Y'.or.UC(ans).eq.'3') then	!define new theta and refit
		refit=.true.
c	    doing refit so reset jfix,kfix here -zero them first
c (also zero iprops() 09/15/94 03:06pm)
		do i=0,ntsav
		   kfix(i)=0
		enddo
		do i=1,kmax
		   jfix(i)=0
		enddo
		do k=kt+1,kt+ntrans
		   iprops(k)=0	!reset iprops() to zero
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
		kmax=2*ntrans+1	!new value
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
c       The new iampl() were defined in TC1 so can use now to say whether
c	   shut or open.
		do i=1,ntrans-1
		   j=ntrans+i+1				!index for theta, kfix
		   if(delt(i).lt.tshort) then		!short event  -fix amplitude
c============PROBLEM 'not opening' does not imply SHUT!
c		      if(opening(i)) then		!=short opening
		      if(iampl(kt+i).ne.0) then		!=short opening
			   theta(j)=base+avamp	!ie fix amplitude at mean open
c== fixing removed for now
c==			   jfix(j)=1
c==		         kfix(i)=2  		!fixed at open level
c==			   iprops(kt+i)=IBSET(iprops(kt+i),1)	!set bit 1 = '2'
			else 			!shutting
			   theta(j)=base		!ie fix amplitude at baseline
			   jfix(j)=1
		         kfix(i)=1  		!fixed at baseline level
c			   iprops(kt+i)=2    !fixed at mean shut level (base)
			   iprops(kt+i)=IBSET(iprops(kt+i),1)	!set bit 1 = '2'
			endif
		   endif
		enddo
		j1=ntrans+1		!numbering of theta based on new ntrans
		do i=1,ntrans	!redefine stepamp, in case theta changed
		   j1=j1+1		!index for theta =ntrans+2, ntrans+3,...
		   stepamp(i)=theta(j1)-theta(j1-1)	!for display of guesses
		enddo
c        and redefine ylevel() because ylevel may be needed in modsubl
c        (assuming ylevel(1) not changed)
		do i=1,ntrans
		   ylevel(i+1)=stepamp(i)+ylevel(i)
		enddo
c        finally, zero the values above the new ntrans (not now defined)
		do i=ntrans+1,ntsav
		   j=kt+i
		   iampl(j)=0
		   iprops(j)=0
		   opening(i)=.false.
		enddo
c No refit wanted, so restore original values
	   ELSE	!no refit
		refit=.false.
		if(ntrans.eq.0) refit=.true.	!so fit rejected in FITSUB
	   endif
	endif
c If refit not wanted, restore original values
	if(.not.refit) then
	   ntrans=ntsav
	   do i=0,ntrans-1
	      delt(i)=deltsav(i)
	   enddo
	   do i=1,ntrans
	      iampl(kt+i)=iampsav(i)
	   enddo
	   do i=i1,ntrans
	      iprops(kt+i)=ipropsav(i+1)
	   enddo
	endif
c
	RETURN
	end

	subroutine TC1(kt,ntrans,iampl,iprops,delt,iampcrit,tmin,
     &  ifitmode,ntmax)
	integer*2 iampl(0:ntmax)
	integer*2 iamp	!dummy arg for sumwamp
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
c Modif 05/20/98 08:37am so next line moved down -otherwise 'bad' does not
c get set sometimes
c	if(delt(i).ge.tmin) goto 2
	if(k.eq.0) then
	   goto 2		!undefined shutting before 1st opening assumed good
	else
	   bad=BTEST(iprops(k),3)	!bit 3 set (='8')
c	   if(bad) goto 2			!bit 3 set? (='8')
	endif
	if(bad.or.delt(i).ge.tmin) goto 2
1	i=i+1
	k=k+1
	if((delt(i).ge.tmin.or.BTEST(iprops(k),3)).and.
     &	(delt(i-1).ge.tmin.or.BTEST(iprops(k-1),3))) goto 2	!found good interval
	goto 1      !keep looking for start
c    Now start looking for short intervals
2	continue
	ki=ki+1
c=	call SUMAMP(-1,S,iampl,n,ntmax)
	call SUMWAMP(-1,j,S,iamp,tin,ampav,ttot)		!initialise sums
	delt(ki)=delt(i)			!start new concat group
c==	call SUMAMP(k,S,iampl,n,ntmax)
	call SUMWAMP(1,j,S,iampl(k),delt(ki),ampav,ttot)
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
c==	call SUMAMP(k+2,S,iampl,n,ntmax)
	call SUMWAMP(1,j,S,iampl(k+2),delt(i+2),ampav,ttot)
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
c***	if(n.gt.0) iampl(kt+ki)=IFIXR(S/float(n))
	if(j.gt.0) then
	   iampl(kt+ki)=ifixr(ampav)		!last mean output from sumwamp
	else
	   print*,' ERROR #21 in CHECKFIT'	!see resint5
	endif
C   TINT(KI+1) (APPROX=TINT(I+2)) MAY NEED MORE THINGS CONCAT WITH IT
C   SO INITIALISE SUMS AND START NEW ONES.
c==	call SUMAMP(-1,S,iampl,n,ntmax)
	call SUMWAMP(-1,j,S,iamp,tin,ampav,ttot)		!initialise sums
	call SUMWAMP(1,j,S,iampl(k+2),delt(i+2),ampav,ttot)
c==	call SUMAMP(k+2,S,iampl,n,ntmax)
	ki=ki+1
	i=i+2
	k=k+2
	nt=nt-1	!number of transitions reduced by 1
	goto 7		!CARRY ON CONCAT
c
4	continue
c  Finished a concatenated group
c Assign amp if concatenation finished
c***	if(n.gt.0) iampl(kt+ki)=IFIXR(S/float(n))
	if(j.gt.0) then
	   iampl(kt+ki)=ifixr(ampav)		!last mean output from sumwamp
	else
	   print*,' ERROR #2 in CHECKFIT'	!see resint5
	endif
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
	integer*2 iampl(0:ntmax)
c
	diffamp=.true.
	if(iampl(i1).eq.0.and.iampl(i2).eq.0) diffamp=.false.
	if(iampl(i1).ne.0.and.iampl(i2).ne.0.and.
     &  i2abs(iampl(i1)-iampl(i2)).lt.iampcrit) diffamp=.false.
	RETURN
	end

	subroutine SUMAMP(k,S,iampl,n,ntmax)
	integer*2 iampl(0:ntmax)
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

	subroutine SUMWAMP(imode,j,S,iamp,tint,ampav,ttot)
c To calculate sum of amplitudes, weighted by their duration (=tint)
c As in resint5, except that ampliudes are integer*2 here
c Returns ampav=current value of average after each call, and ttot=current
c value of denominator=total duration of the intervals used
C IF imode=-1 IT INITIALISES.
	integer*2 iamp
c
	if(imode.lt.0) then   !initialise only
	   j=0
	   S=0.0
	   ttot=0.0
	   RETURN
	endif
c
c Insert precaution in case of 'zero' duration intervals
c   Note that, although avamp is calculated afresh at each call, it is
c not actually used (at present) until concatenation is finished, so
c a value calculated when j=1 (as below) will not be used unless there
c is no concatenation and this is the only amplitude
c   In this version of j>1 and ttot (total length) is still near zero
c then avamp is undefined (this would occur only when all the durations
c in a concatenation are near-zero!)
	j=j+1
	S=S+float(iamp)*tint
	ttot=ttot+tint
	if(ttot.gt.1.e-30*abs(s)) then
	   ampav=s/ttot
	else if(j.eq.1) then
	   ampav=float(iamp)
	endif
c
	RETURN
	end

