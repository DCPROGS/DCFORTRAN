	program TCHECK
	integer*2 iampl(1024)
	integer*1 iprops(1024)	!properties of ith interval (see SCAN)
	real*4 delt(0:50)		!temp storage for transition search results
c
	ntrans=6
c=	ntrans=4
c=	ntrans=3
	tmin=15
	delt(0)=5000.	!amp=0
	delt(1)=1000.	!amp=-500
	delt(2)=10.		!amp=0
	delt(3)=2000.	!amp=-520
	delt(4)=200.	!amp=0
	delt(5)=8.		!amp=-540
c
	kt=20
	iacrit=30
c
	iampl(20)=0
	iampl(21)=-500
	iampl(22)=0
	iampl(23)=-520
	iampl(24)=0
	iampl(25)=-540
c	iampl(26)=0
	iampl(26)=-530

c
	iprops(20)=0
	iprops(21)=0
	iprops(22)=0
	iprops(23)=0
	iprops(24)=0
	iprops(25)=0
	iprops(26)=0
c
	ifitmode=0
c
	print 1,ntrans
1	format(' ntrans = ',i5)
	do i=0,ntrans
	   k=kt+i
	   if(i.lt.ntrans) then
	      print 2,i,delt(i),k,iampl(k),iprops(k)
2	      format(
     &	' delt(',i2,') = ',f9.1,'; k, amp(k), props(k) = ',i3,2i6)
	   else
	      print 3,k,iampl(k),iprops(k)
3	      format(21x,'  k, amp(k), props(k) = ',i3,2i6)
	   endif
	enddo
c
	call TC1(kt,ntrans,iampl,iprops,delt,iacrit,tmin,
     &  ifitmode)
	print 1,ntrans
c1	format(' ntrans = ',i5)
	do i=0,ntrans
	   k=kt+i
	   if(i.lt.ntrans) then
	      print 2,i,delt(i),k,iampl(k),iprops(k)
c2	      format(
c     &	' delt(',i2,') = ',f9.1,'; k, amp(k), props(k) = ',i3,2i6)
	   else
	      print 3,k,iampl(k),iprops(k)
c3	      format(21x,'  k, amp(k), props(k) = ',i3,2i6)
	   endif
	enddo
	end

	subroutine TC1(kt,ntrans,iampl,iprops,delt,iacrit,tmin,
     &  ifitmode)
c Simplified version of CHECKFIT for debugging in TCHECK
c	subroutine CHECKFIT(kt,ntrans,delt,iampl,iprops,jfix,kfix,
c     & theta,dt,stepamp,tmin,ifitmode,iacrit
	integer*2 iampl(1024)
	integer*1 iprops(1024)	!properties of ith interval (see SCAN)
	real*4 delt(0:50)		!temp storage for transition search results
	logical bad,diffamp
c
	bad=.false.
	nt=ntrans
	ki=-1		!counts concat intervals -start at delt(0)
	k=kt-1
	i=k-kt 	!index for delt()=0,1,...,
	i=i+1
	k=k+1
c First find good interval to start concat group as in RESINT- normally delt(0)
c will be long (though possibly set unusable -but still counted resolvable)
	if(delt(i).ge.tmin.or.iprops(k).ge.8) goto 2	!found good interval
1	i=i+1
	k=k+1
	if((delt(i).ge.tmin.or.iprops(k).ge.8).and.
     &	(delt(i-1).ge.tmin.or.iprops(k-1).ge.8)) goto 2	!found good interval
	goto 1      !keep looking for start
c    Now start looking for short intervals
2	ki=ki+1
	call SUMAMP(-1,S,iampl,n)
	delt(ki)=delt(i)			!start new concat group
	call SUMAMP(k,S,iampl,n)
c is next interval resolvable? If so, assign amp and carry on
7	if(i.ge.ntrans-1) goto 4		!last delt() reached
	if(delt(i+1).ge.tmin.or.iprops(k+1).ge.8) goto 4	!yes-resolvable
c Next bit done when NOT resolvable. Has the interval after it got the 'same'
c amp as that before it? NOTE it is safe to refer to iampl(k+2) here
c because amplitude of interval after the last transition is known, even
c though its duration is not yet defined.
	tin=delt(ki)
	bad=iprops(k).ge.8
c If ifitmode=0 or 3 then may have openings of diff amplitude on each
c side of an irresolvable gap
	if(ifitmode.eq.1.or.ifitmode.eq.2) goto 9		!all same amp
	if(DIFFAMP(k,k+2,iampl,iacrit)) goto 5
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
	bad=iprops(k+1).ge.8.or.iprops(k+2).ge.8
	call SUMAMP(k+2,S,iampl,n)
	i=i+2
	k=k+2
	nt=nt-2	!number of transitions reduced by 2
	goto 7	!continue concat until resolvable interval found
c
c Next bit done if delt(i+1) unresolvable but intervals on each side of it
c have 'different' amps.
5	tin1=delt(i+1)
	delt(ki)=tin+0.5*tin1
	bad=iprops(k+1).ge.8.or.iprops(k+2).ge.8
	delt(ki+1)=0.5*tin1+delt(i+2)
c   Concat of delt(ki) now ended so assign amp to it
	if(n.gt.0) iampl(kt+ki)=IFIXR(S/float(n))
C   TINT(KI+1) (APPROX=TINT(I+2)) MAY NEED MORE THINGS CONCAT WITH IT
C   SO INITIALISE SUMS AND START NEW ONES.
	call SUMAMP(-1,S,iampl,n)
	call SUMAMP(k+2,S,iampl,n)
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
	   iprops(kt+ki)=8
	   bad=.false.
	else
	   iprops(kt+ki)=0
	endif
	i=i+1
	k=k+1
c===	if(i.le.ntrans-1) goto 2
	if(i.le.ntrans-2) goto 2
c
c All done:
c Need to assign amplitude that follows the last transition (as revised here),
c if not already done
	if(ki.lt.nt) iampl(kt+nt)=iampl(kt+ntrans)
	ntrans=nt	!new number of transitions
	RETURN
	end

	Logical function DIFFAMP(i1,i2,iampl,iacrit)
	integer*2 iampl(1024)
c
	diffamp=.true.
	if(iampl(i1).eq.0.and.iampl(i2).eq.0) diffamp=.false.
	if(iampl(i1).ne.0.and.iampl(i2).ne.0.and.
     &  iabs(iampl(i1)-iampl(i2)).lt.iacrit) diffamp=.false.
	RETURN
	end

	subroutine SUMAMP(k,S,iampl,n)
	integer*2 iampl(1024)
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

