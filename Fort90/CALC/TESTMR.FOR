	subroutine TESTMR(m,im1,jm1,done,ncycp,nOK,iOK,jOK,ndim)
c To test whether q(im1,jm1) is a member of an cycle before cycle #m
c
c The index m in call is such that isetmr(m)=actual cycle number
c Set m1=isetmr(m)
c In call, m=current cycle number (=>2) and im1=im(m1,1), jm1=jm(m1,1), the
c indices of the rate that it is proposed to set by MR in cycle #m
c
c Returns
c   ncycp=0 if q(im1,jm1) is not a member of any previous cycle
c   ncycp=n if q(im1,jm1) is member of any previous cycle #n
c   nOK=number of routes in cycle #m that are NOT part of a previously
c	defined cycle, and can therefore be set by MR
c   iOK(),jOK() =i,j for these rates
c
	logical done(ndim)	!50 at present
	integer*4 iOK(10),jOK(10)
	logical bad(10)
	logical discprt
	common/dp/discprt
c
	integer NSC(50),IM(50,100),JM(50,100)
c for eqoccd, getqd
	logical obeymr(50),automr(50)
	common/mr/obeymr,automr
	integer isetmr(50)
	common/mr1/isetmr
	COMMON/MPAR/NCYC,NSC,IM,JM
c
	ncycp=0	!initialise
c
	m1=isetmr(m)	!the actual cycle number for mth cycle to be set
	if(ncyc.lt.2.or.m.lt.2) RETURN
	if(.not.obeymr(m1)) RETURN
c
c Check all cycles that have already been set (done=true) BEFORE cycle #m
	do n1=1,m-1
	   n=isetmr(n1)	!cycle number in order they are set
	   if(obeymr(n)) then
c   check all rates in cycle #n
		do j1=1,nsc(n)
		   if(im1.eq.im(n,j1).and.jm1.eq.jm(n,j1).OR.
     &		im1.eq.jm(n,j1).and.jm1.eq.im(n,j1)) then
			ncycp=n
			goto 9
		   endif
		enddo
9		continue
	   endif
	enddo
c If the selected route, im1,jm1 is not available, then find any routes
c in cycle #m that ARE available
c Record in iOK, JOK, the routes that are NOT part of any earlier cycle and are
c therefore available to be set by MR for current cycle, #m
	nOK=nsc(m1)		!all OK unless found otherwise
	if(ncycp.ne.0) then
	   do j2=1,nsc(m1)		!check ALL routes in current cycle, #m
		im2=im(m1,j2)
		jm2=jm(m1,j2)
		bad(j2)=.false.
c check whether this im2, jm2 occur in any previous cycles, as above
		do n1=1,m-1
		   n=isetmr(n1)	!cycle number in order they are set
		   if((obeymr(n).or.automr(n)).and.done(n)) then
c   check all rates in cycle #n
			do j1=1,nsc(n)
			   if(im2.eq.im(n,j1).and.jm2.eq.jm(n,j1).OR.
     &			im2.eq.jm(n,j1).and.jm2.eq.im(n,j1)) then
				bad(j2)=.true. 	!route im2,jm2 bad in cycle m
				print 1,im2,jm2,m1,n
				if(discprt) write(8,1) im2,jm2,m1,n
1				format(
     &'   Route ',i3,'-',i3,' in cycle ',i3,' occurs also in cycle '
     & ,i3,' which has already been set')
			   endif
			enddo
		   endif
		enddo
	   enddo
c bad(i) now defined for all routes in cycle #m, so set good routes
	   nOK=0
	   do j2=1,nsc(m1)		!check ALL routes in current cycle, #m
		if(.not.bad(j2)) then
		   nOK=nOK+1
		   iOK(nOK)=im(m1,j2)
		   jOK(nOK)=jm(m1,j2)
		endif
	   enddo
	endif
c
99	continue
	RETURN
	end

