	subroutine PDDburst(QD,phib,endb1,area,tau,ncomp,debug,km)
c PDF for distribution of burst length
c PDDburst is version of pdfburst with real*8 tau, area output
c Input:
c   QD (qith conc set correctly)
c   phib -initial vector for bursts (calc in phib1) (1*kA)
c   endb1=final vector ofr bursts (calc in phib1) (kA*1)
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
	real*8 tau(km),area(km)
	real*8 QD(km,km)
	real*8 phib(1,km),endb1(km,1)
	allocatable::Q1,amat,eigen,w1,w
	real*8 amat(:,:,:),Q1(:,:),eigen(:),w1(:)
	real*4 w(:)
	integer EE
	logical debug
c	integer AA
c	integer AA,AB,AC,AD,BA,BB,BC,CB,CA,EE,AF,FA,FF,FD
c	integer BD,CD,DF,DA,TT,AT,TA,AH,BH,HA,HB,GG
c	common/subblk/AA,AB,AC,AD,BA,BB,BC,CB,CA,EE,AF,FA,FF,FD,
c     & BD,CD,DF,DA,TT,AT,TA,AH,BH,HA,HB,GG
c	common/calblk/kE,kG,kF,kT,kH,k,zero,one
	common/KBLK/KA,KB,KC,KD
c
	kE=kA+kB
c	k=kA+kB+kC+kD
c	one=1.0d0
	EE=66		!6=code for E(=BURST=A+B)
c
	ALLOCATE(amat(km,km,km),Q1(km,km),eigen(km),w1(km),w(km))
c
	call SUBMAT(QD,EE,Q1,km,km,km,km)	!QEE in Q1
c   Expand QEE
	CALL QMAT5(Q1,Amat,kE,eigen,ibad,km,km,km)
c	if(debug) print 60,(eigen(i),i=1,kx)
c	IF(IBAD.NE.0) print 14,IBAD
c	do m=1,kdim		W1 is zeroed in RAC3
c	   w1(m)=zero
c	enddo
	call RAC3(phib,endb1,amat,1,kA,1,kA,kE,w,w1,km,km,km)
c
	ncomp=kE
	mbad=0
	do m=1,ncomp
	  x=dabs(eigen(m))
	  if(x.lt.1.0d-307) then
		tau(m)=1.d-307
		area(m)=1.0d0
		mbad=m
	  else
		tau(m)=1000.d0/x
		area(m)=w1(m)/x
	  endif
	enddo
c area set to 1.0 for bad component, so set others to zero
	if(mbad.ne.0) then
	   do m=1,ncomp
		if(m.ne.mbad) area(m)=0.0d0		!area(mbad)=1 (above)
	   enddo
	endif
c
	DEALLOCATE(amat,Q1,eigen,w1,w)
	RETURN
	end

