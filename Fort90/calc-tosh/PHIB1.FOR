	subroutine PHIb1(QM,Peq,phib,endb1,endb2,debug,km)
c To calculate the initial and final vectors for burst distributions as in CH82
c
c Part of family of subroutines to do single channel calcs via
c set of discrete subroutines that can be called as req.
c   There is a problem in designating submatrices in that notation
c is somewat different in burst and cluster programs, eg F or T for all
c shut states. Here just work directly from kA,kB,kC,kD
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c=	dimension QM(km,10),Peq(km),phi(1,10)
	dimension QM(km,km),Peq(km),phib(1,km)
	dimension endb1(km,1),endb2(km,1)
	allocatable::GBA,Q1,Q2,Q3
	real*8 GBA(:,:),Q1(:,:),Q2(:,:),Q3(:,:)
	integer AB,AC,BA,BC,CA,CB
c	integer AA,AB,AC,AD,BA,BB,BC,CB,CA,EE,AF,FA,FF,FD
c	integer BD,CD,DF,DA,TT,AT,TA,AH,BH,HA,HB,GG
c	common/subblk/AA,AB,AC,AD,BA,BB,BC,CB,CA,EE,AF,FA,FF,FD,
c     & BD,CD,DF,DA,TT,AT,TA,AH,BH,HA,HB,GG
	common/KBLK/KA,KB,KC,KD
	logical debug
	logical discprt
	common/dp/discprt
c
	ALLOCATE(GBA(km,km),Q1(km,km),Q2(km,km),Q3(km,km))
c	k=kA+kB+kC+kD
	kE=kA+kB
C 1,2,3,4= codes for A,B,C,D resp. 5=F=(B+C). 6=E=(A+B). 7=T=(B+C+D).
c 8=H=(C+D), 9=G=(A+B+C)
	one=1.0d0
	zero=0.0d0
	AB=12
	AC=13
	BA=21
	BC=23
	CA=31
	CB=32
c
c   (1) Initial (1 x kA) vector=phi(1,j) sau. Use general form
c	=phib=pC(inf)(QCB*GBA+QCA)/SUM over A if numerator
C	First need GBA=-inv(QBB)*QBA
	if(debug) then
	   print 8
	   if(discprt) write(8,8)
8	   format( /,' Initial vector for burst (phi(b))=')
	endif
	call GMAT1(BA,QM,GBA,km,km)	!GBA in GBA
c      call SUBMAT(QM,BB,Q1,km,km,km,km)		!QBB in Q1
c	call MATINV(Q1,kB,km,Q3,km)			!inv(QBB) in Q3
c	call SUBMAT(QM,BA,Q2,km,km,km,km)		!QBA in Q2
c	call MATMUL(Q3,Q2,GBA,kB,kB,kA,-one,
c     & km,km,km,km,km,km)                      	!GBA in GBA
c
	call SUBMAT(QM,CB,Q1,km,km,km,km)		!QCB IN Q1
	call MATMUL(Q1,GBA,Q2,kC,kB,kA,ONE,km,km,km,km,km,km)	!QCB*GBA IN Q2
	call SUBMAT(QM,CA,Q1,km,km,km,km)		!QCA IN Q1
	do i=1,kC
	   do j=1,kA
		Q1(i,j)=Q2(i,j)+Q1(i,j)	!QCB*GBA+QCA IN Q1
	   enddo
	enddo
	sum=zero
	do j=1,kA		!PREMULT BY PC(INF) TO GET PHI
	   phib(1,j)=zero
	   do L=1,kC
		phib(1,j)=phib(1,j)+Peq(L+kE)*Q1(L,j)
	   enddo
	   sum=sum+phib(1,j)
	enddo
	do j=1,kA
	   phib(1,j)=phib(1,j)/sum	!normalise phi
	enddo
	if(debug) then
	   print 71,(phib(1,j),j=1,kA)
         if(discprt) write(8,71)(phib(1,j),j=1,kA)
71	   format(2x,8g13.6)
	endif
c
c  (2) Calc end vector END1=(QAB*GBC+QAC)*uC where GBC=-inv(QBB)*QBC
	call GMAT1(BC,QM,Q2,km,km)   				!GBC in Q2
	call SUBMAT(QM,AB,Q1,km,km,km,km)			!QAB IN Q1
	call MATMUL(Q1,Q2,Q3,kA,kB,kC,one,km,km,km,km,km,km)	!QAB*GBC IN Q3
	call SUBMAT(QM,AC,Q1,km,km,km,km)			!QAC IN Q1
	do i=1,kA
	   do j=1,kC
		Q2(i,j)=Q3(i,j)+Q1(i,j)		!QAB*GBC+QAC in Q2
	   enddo
	enddo
	do i=1,kA		!POSTMULT BY UC TO GET END1
	   endb1(i,1)=zero
	   do j=1,kC
		endb1(i,1)=endb1(i,1)+Q2(i,j)
	   enddo
	enddo

C  Now end2=GAB*GBC+GAC=-inv(QAA)*end1  (needed for number of ops per burst)
	call MATINV(QM,kA,km,Q3,km)		!inv(QAA) IN Q3
	if(debug) then
	   CALL ATYPD(Q3,'inv(QAA)',kA,kA,km,km)
	endif
	call MATMUL(Q3,endb1,endb2,kA,kA,1,-one,km,km,km,1,km,1)   !-inv(QAA)*end1
	if(debug) then
	   print 12
	   if(discprt) write(8,12)
12	   FORMAT( ' END VECTORS (Eb,-QAA*Eb)=')
	   print 71,(ENDb1(I,1),I=1,KA)
         if(discprt) write(8,71)(ENDb1(i,1),i=1,kA)
	   print 71,(ENDb2(I,1),I=1,KA)
         if(discprt) write(8,71)(ENDb2(i,1),i=1,kA)
	endif
c
	DEALLOCATE(GBA,Q1,Q2,Q3)
	RETURN
	end
