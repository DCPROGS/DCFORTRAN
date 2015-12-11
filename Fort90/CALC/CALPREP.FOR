	subroutine CALPREP(tpre,QT,p00,pt0,nlig,IL,
     &  conc000,conc00,conc0,conc,nljump,vhold,vref,
     &  km,k,kA,kF,iprint,pon)
c	subroutine CALPREP(tpre,QT,p00,pt0,p00z,p00n,nlig,IL,
c     &  conc000,conc00,conc0,conc,nljump,vhold,vref,
c     &  km,k,kA,kF,iprint,pon)
c	subroutine CALPREP(tpre,QT,p0,pt,pt0,pt1,cA1,cB1,nlig,IL,
c     & xApre,km,k,kA,kF,iprint,pon)
c To calculate things for prepulse in SCALCS
c Input= Q = Q matrix at prepulse concentration
c        p0 =equilib occs at prepulse conc
c        p00 is now a parameter so cannot be allocated here
c
c Declare pt0(),pt1() -parameters? -or allocate locally and ask
c  which one is to be copied to pt()?
	real*8 p00(km)
	real*8 QT(100,100),p0(100),pt(100)
	real*8 pt0(100),pt1(100)
	ALLOCATABLE Q0,Q1,QFF,amat,eigen,bmj,ucol
c=	ALLOCATABLE pt0,pt1
	real*8 Q0(:,:),Q1(:,:),QFF(:,:),amat(:,:,:),eigen(:),bmj(:,:)
	real*8 ucol(:,:)
c=	real*8 pt0(:),pt1(:)
	real*8 bm,tt,one,Pr0,s,s0,s1
	integer IL(100)
	integer FF,FA
	character heading*70
	logical discprt,pon
	common/dp/discprt
	COMMON/CPAR/ncdep,IX(100),JX(100),x
c For revision
	real*4 conc0(10),conc00(10),conc(10)
	real*4 conc000(10)	!conc before prepulse
c
	ALLOCATE(Q0(km,km),Q1(km,km),QFF(km,km),amat(km,km,km),
     & eigen(km),bmj(km,km),ucol(km,1))
c=	ALLOCATE(pt0(km),pt1(km))
c Definitions
	one=1.0d0
	do i=1,km
	   ucol(i,1)=one
	enddo
	xApre=conc00(nljump)
c
c First calculate initial vector (before prepulse) at conc=0
c Should conc be zero for ALL ligands, or just for the one that is jumped?
c This is now decided in main prog, and conc000(i) set accordingly
c==	call QNEWC(Q,cA1,cB1,Q0,0.0,0.0,ncdep,nlig,IL,IX,JX,k,km)
	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	call QSETD(conc000,IL,V1,QT,Q0,k,.false.)
	   call RANK_SVD(Q0,k,k,km,km,irank)
	   if(irank.lt.k-1) then
		print 83, irank
	      if(discprt) write(8,83) irank
83		format(
     & ' The rank of the Q matrix seems to be ',i2,', less than k-1.',/,
     & ' This suggest that the mechanism has disconnected states,',/,
     & ' so equilibrium occupancies can''t be calculated.')
		STOP
	   endif
	call EQOCCd(Q0,k,k-1,km,P00)		!calc equilib occs at conc=0
c
c Calculate spectral expansion of Q at prepulse conc
	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	call QSETD(conc00,IL,V1,QT,Q1,k,.false.)
	call QMAT5(Q1,Amat,k,eigen,ibad,km,km,km)
	if(ibad.ne.0) print 14,ibad
14	format(' ***ERROR IN SPECTRAL EXPANSION: ibad= ',i4)
c
	call PTCALC(pt,tpre,bmj,p00,p0,eigen,amat,k,km)
c Output=pt=occupancies at end of prepulse of length tpre
c
c  Now calculate occupancies at t=tpre for case where no openings
c occur during the prepulse -these are pF(0)*exp(QFF*tpre), where pF(0)
c has been calculated above in last kF elements of P00() -keep these
c occupancies in pt0().  Calc from exp(QFF*tpre)=SUM[A(m)*exp(eig(m)*tpre]
c	FF=55
c	FA=51
	FF=77       !use 7, in case kD>0
	FA=71
	call SUBMAT(Q1,FF,QFF,km,km,km,km)			!QFF in QFF
	call QMAT5(QFF,Amat,kF,eigen,ibad,km,km,km)      !expand QFF
	do j=1,kA
	   pt0(j)=0.0d0		!no open channels at t=tpre
	enddo
	tt=dble(tpre)
	do j=1,kF
	   j1=j+kA		!so elements go into 'F' part of pt0()
	   pt0(j1)=0.0d0
	   do m=1,kF
		bm=0.0d0
		do i=1,kF
		   bm=bm + p00(i+kA)*Amat(i,j,m)
		enddo
		pt0(j1)=pt0(j1) + bm*dexp(eigen(m)*tt)   !pt0(j) not yet normalised
	   enddo
	enddo
c  Now calc, in pt1(), the occupancies at time t=tpre for the case where
c there are one or more openings during the prepulse.  To calculate these
c Now calc Pr0=Prob[no openings during prepulse] = Prob[first latency > tpre]
c i.e. phi(0)*exp(QFF*tpre)*GFA*uA/Pr0, which is cumulative form of the first
c latency distribution.
c Have already got phi(0)*exp(QFF*tpre) in pt0() so need only postmultiply
c this by GFA*uA=uF (NB this shows that pt1(i) will add to 0 when normalised
c with Pr0)
      call VECMUL(pt0,ucol,Pr0,k,km,km)	  !Prob[latency>tpre] in Pr0
c Now normalise pt0(j)
	do j=1,k
	   pt0(j)=pt0(j)/Pr0
	enddo
c And now calc pt1(), from fact that overall occs at t=tpre, in pt(), must
c weighted mean of those given NO opening in pt0() (weight Pr0), and those
c given at least one opening (weight 1-Pr0)
	s=0.0d0
	s0=0.0d0
	s1=0.0d0
	do i=1,k
	   pt1(i)=(pt(i) - Pr0*pt0(i))/(one - Pr0)
	   s=s+pt(i)
	   s0=s0+pt0(i)
	   s1=s1+pt1(i)
	enddo
c
      print 1,Pr0
      if(pon) write(7,1) Pr0
      if(discprt) write(8,1) Pr0
1	format(' Prob[no openings during prepulse] = ',g13.6)
	if(iprint.ge.2) then
	   heading=
     &    '   State   Overall        No openings     '
     &	//char(242)//' 1 opening'
         print 2,tpre*1000.,xapre*1.e6,heading
         if(pon) write(7,2) tpre*1000.,xapre*1.e6,heading
         if(discprt) write(8,2) tpre*1000.,xapre*1.e6,heading
2	   format(/,
     &   ' Occupancies at end of prepulse:',/,' (t = ',g13.6,
     &   ' ms at concentration = ',g13.6,')',/,a70)
	   do j=1,k
		print 60,j,pt(j),pt0(j),pt1(j)
	      if(pon) write(7,60) j,pt(j),pt0(j),pt1(j)
	      if(discprt) write(8,60) j,pt(j),pt0(j),pt1(j)
60		format(1x,i5,3(3x,g13.6))
	   enddo
         print 61,s,s0,s1
         if(pon) write(7,61) s,s0,s1
         if(discprt) write(8,61) s,s0,s1
61	   format(' Total  ',3(3x,g13.6))
	endif
c
	DEALLOCATE(Q0,Q1,QFF,amat,eigen,bmj,ucol)
	RETURN
	end


