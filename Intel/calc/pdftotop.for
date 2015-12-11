	subroutine PDFtotop(QD,phib,endb1,area,tau,ncomp,debug,km)
c To calc PDF for distribution of total open time per burst (as in CH82)
c f(t)=phib*exp(VAA*t)*(-QAA)*eb
c where VAA=QAA + QAB*GBA
c
c PDF for distribution of burst length
c Input:
c   QD (qith conc set correctly)
c   phib =initial vector for bursts (calc in phib1) (1*kA)
c   endb1=final vector =(-QAA)*eb (calc in phib1) (kA*1)
c
c Output:
c   areac()
c   tau()
c   ncompc

c Outputs tau (ms), area (single prec) and ncomp (as req in PDFIT)
c Outputs eigen,w1 as req for printing of PDFOUT1 etc
c
c Part of family of subroutines to do single channel calcs via
c set of discrete subroutines that can be called as req.
c
c   There is a problem in designating submatrices in that notation
c is somewat different in burst and cluster programs, eg F or T for all
c shut states. Here just work directly from kA,kB,kC,kD
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*4 tau(km),area(km)
	real*8 QD(km,km)
	real*8 phib(1,km),endb1(km,1)
c=	real*8 col(10,1)
	allocatable::Q1,GBA,VAA,amat,eigen,w1,w
	real*8 amat(:,:,:),Q1(:,:),GBA(:,:),VAA(:,:),eigen(:),w1(:)
	real*4 w(:)
	integer AB,BA
	logical debug
c	integer AA,AB,AC,AD,BA,BB,BC,CB,CA,EE,AF,FA,FF,FD
c	integer BD,CD,DF,DA,TT,AT,TA,AH,BH,HA,HB,GG
c	common/subblk/AA,AB,AC,AD,BA,BB,BC,CB,CA,EE,AF,FA,FF,FD,
c     & BD,CD,DF,DA,TT,AT,TA,AH,BH,HA,HB,GG
c	common/calblk/kE,kG,kF,kT,kH,k,zero,one
	common/KBLK/KA,KB,KC,KD
c
c	kE=kA+kB
c	k=kA+kB+kC+kD
c	EE=66		!6=code for E(=BURST=A+B)
c	AA=11
	one=1.0d0
c	zero=0.0d0
	AB=12
	BA=21
c
	ALLOCATE(amat(km,km,km),Q1(km,km),GBA(km,km),VAA(km,km),
     &	eigen(km),w1(km),w(km))
c
	call SUBMAT(QD,AB,Q1,km,km,km,km)	!QAB in Q1
	call GMAT1(BA,QD,GBA,km,km)	!GBA in GBA
      call MATMUL(Q1,GBA,Q1,kA,kB,kA,one,km,km,km,km,km,km)  !QAB*GBA in Q1
	do i=1,kA
	   do j=1,kA
		VAA(i,j)=QD(i,j) + Q1(i,j)	!VAA=QAA + QAB*GBA
	   enddo
	enddo

c   Expand VAA
	CALL QMAT5(VAA,Amat,kA,eigen,ibad,km,km,km)
c	if(debug) print 60,(eigen(i),i=1,kA)
c	IF(IBAD.NE.0) print 14,IBAD
c	do m=1,kdim		W1 is zeroed in RAC3
c	   w1(m)=zero
c	enddo
c	call RAC3(phib,endb1,amat,1,kA,1,kA,kA,w,w1,km,km,km)
	call RAC2(phib,endb1,Amat,kA,w,w1,km,km,km)
cc Alternative calc of end vector as -VAA*uA
c	do i=1,kA
c	   col(i,1)=zero
c	   do j=1,kA
c		col(i,1)=col(i,1) - VAA(i,j)
c	   enddo
c	enddo
c	call RAC2(phib,col,Amat,kA,w,w1,km,km,km)
c
	ncomp=kA
	mbad=0
	do m=1,ncomp
	  x=dabs(eigen(m))
	  if(x.lt.1.0d-35) then
		tau(m)=1.e35
		area(m)=1.0
		mbad=m
	  else
		tau(m)=1000./sngl(x)
		area(m)=sngl(w1(m)/x)
	  endif
	enddo
c area set to 1.0 for bad component, so set others to zero
	if(mbad.ne.0) then
	   do m=1,ncomp
		if(m.ne.mbad) area(m)=0.0		!area(mbad)=1 (above)
	   enddo
	endif
c
	DEALLOCATE(amat,Q1,GBA,VAA,eigen,w1,w)
	RETURN
	end

