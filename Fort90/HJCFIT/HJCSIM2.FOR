	subroutine HJCSIM2(thtrue,tint0,ampl0,iprops0,conc,nintt,
     &   irate,jrate,npar,nmax,nset,dgamma,nsims,k)
c
c To generate nset simulated .scn files, as specified in HJCSIM1
c (no questions asked here, so can be put in a loop for repeated simulations)
c
c Must be called with values for model used for simulation -for
c irate,jrate,IQ,npar,dgamma,kA,kB,kC,kD (QT constructed here from
c npar,irate,jrate and commons
c	common/KBLKs/kA,kB,kC,kD
c	common/cpars/ncdep,IX(100),JX(100),x
c	common/LIGs/nlig,IL(100)
c  (IQ not used so removed from args)
c Call from main prog will be
c	call HJCSIM2(thtrue,tint0,ampl0,iprops0,conc,nintt,
c     &   irates,jrates,npars,nmax,nset,dgammas,nsims,ks)
c
c The array iampl0() can be local to this subroutine
c
c Args
	real*8 thtrue(200)
	real*4 tint0(nmax,nset),ampl0(nmax,nset)
	integer*1 iprops0(nmax,nset)
	real*4 conc(10,10)
	real*8 dgamma(100)
	integer irate(200),jrate(200)
	integer nintt(10)
c	common/cpar/ncdep,IX(100),JX(100),x
c	common/LIG/nlig,IL(100)
c	common/KBLK/kA,kB,kC,kD
c for separate mechanism for simulation -commons in getqd are
c	common/KBLKs/kAs,kBs,kCs,kDs
c	common/cpars/ncdeps,IXs(100),JXs(100),xs
c	common/LIGs/nligs,ILs(100)
c but here keep usual notation so no need to change this subroutine
	common/KBLKs/kA,kB,kC,kD
	common/cpars/ncdep,IX(100),JX(100),x
	common/LIGs/nlig,IL(100)
c  Local
	allocatable::QT,QD,Peqtrue,iampl0
	allocatable::pi,picum,peqt,amean,pisim,tsim,nsim1,icur
	integer*2 iampl0(:,:)
	real*8 QT(:,:),QD(:,:),peqtrue(:),zero
	real*4 Peqt(:),pi(:,:),picum(:,:)		!NB real*4 in simsc
	real*4 pisim(:,:),tsim(:)
	real*4 amean(:)
	integer nsim1(:)
	integer*2 icur(:,:),ilast
c
c
	logical discprt
	common/dp/discprt
	common/rand/ix1,iy1,iz1
c
c  Make QT from thtrue (defined in HJCSIM1) and define amean() and peq() for
c true rates which are what is used to do simulation.  If nset=1 these are
c constant, so could be calculated once only in hjcsim1. BUT they are different
c for each set, so perhaps better to calculate them at same time as simulation
c (time taken to do so is negligible compared with that needed for fitting)
c Definitions before call to simsc are much as in scsim.
	km=100
	ALLOCATE(QD(km,km),QT(km,km),peqtrue(km))
	ALLOCATE(pi(km,km),picum(km,km),pisim(km,km),peqt(km),tsim(km))
	ALLOCATE(amean(km),nsim1(km),icur(km,km))
	ALLOCATE(iampl0(nmax,nset))

c NB allocated arrays may not be all zero initially
	zero=0.d0
	do i=1,km
	   peqtrue(i)=zero
	   peqt(i)=zero
	   tsim(i)=zero
	   amean(i)=zero
	   nsim1(i)=0
	   do j=1,km
		QD(i,j)=zero
		QT(i,j)=zero
		PI(i,j)=zero
		PICUM(i,j)=zero
		PISIM(i,j)=zero
		icur(i,j)=0
	   enddo
	enddo
c
c Define icur()
	do i=1,k
	   if(i.le.kA) then
		ia=ifixr(sngl(1.d12*dgamma(i)))*0.1
		icur(i,1)=int2(ia)
	   else
		icur(i,1)=0
	   endif
	enddo
c
c Define QT from thtrue
c    Set constraints and micro rev, in QT only, from rates in thtrue()
	do n=1,npar
	   i=irate(n)
	   j=jrate(n)
	   QT(i,j)=thtrue(n)	!define QT from theta
	enddo

	do jset=1,nset
	   ktmax=nintt(jset)
	   call QNEW_HJC(QT,jset,conc,QD,ncdep,nlig,IL,IX,JX,k,km,km)
	   call EQOC_HJC(QD,peqtrue,k,km,km)
c
	   do i=1,k
		peqt(i)=sngl(peqtrue(i))
		nsim1(i)=0
		tsim(i)=0.
	   enddo
c	   do i=1,k
c		print 36,i,peq(i)
c		if(discprt) write(8,36) i,peq(i)
c36		format(' State #',i3,': equilibrium occupancy = ',g13.6)
c		do j=1,k
c		   Q(i,j)=sngl(QD(i,j))
c		enddo
c	   enddo
c
c calc mean lifetimes
	   do i=1,k
		amean(i)=-sngl(1.d3/qd(i,i))
	   enddo
c
c Calc PI
	   do i=1,k
		do j=1,k
		   pi(i,j)=-sngl(qd(i,j)/qd(i,i))
		enddo
	   enddo
c	   ans='Y'
c	   call DCASK('Type PI matrix',ans,ans)
c	   if(UC(ans).eq.'Y')call ATYPE(pi,' PI     ',k,k,km,km)
c
c Calc cumulative PI matrix
	   do i=1,k
		do j=1,k
		   picum(i,j)=0.
		   do n=1,j
			if(n.ne.i) picum(i,j)=picum(i,j)+pi(i,n)
		   enddo
		enddo
	   enddo
c
c	if(debug())call ATYPE(picum,' PI(cum)',k,k,Km,km)
c
c In call to SIMSC, topen, nopen,nt,nt1,ttot are output
c nchan, k, kA . . ., nbstmax are inputs
c NB in subroutine dummy args are nbmax (nbstmax in call -not needed here)
c ktmax=number required (input); kt= number generated (output)
c This subroutine same as SIMSC (used in SCSIM) except that resolution
c setting removed
	   if(nsims.eq.1) then
		if(discprt) write(8,1) jset,(conc(i,jset),i=1,nlig)
1		format(/,' Simulation for set ',i3,/,' Conc = ',10g13.6)
	   endif
	   call SIM_HJC(tint0(1,jset),iampl0(1,jset),pisim,nsim1,tsim,
     &    picum,icur,amean,peqt,topen,nopen,nt,nmax,nt1,kt,ktmax,
     &    ilast,ttot,nchan,nsims,k,kA,kB,kC,kD,km)

	   do i=1,ktmax
		iprops0(i,jset)=0
		ampl0(i,jset)=float(iampl0(i,jset))
	   enddo
c
	enddo		!jset=1,nset
c
	DEALLOCATE(iampl0)
	DEALLOCATE(QD,QT,peqtrue)
	DEALLOCATE(pi,picum,pisim,peqt,tsim)
	DEALLOCATE(amean,nsim1,icur)
	RETURN
	end

