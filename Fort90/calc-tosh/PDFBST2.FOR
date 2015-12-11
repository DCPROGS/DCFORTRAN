	subroutine PDFbst2(QD,phib,endb1,endb2,area,tau,ncomp,debug,km)
c To calculate pdf of burst lengths for bursts that have 2 or more openings
c The initial and final vectors are calulated in phib1.for
c
c fbst2=phib[exp(QEE*t)AA - exp(QAA*t](-QAA)*endb1/den where
c   den = 1-p(r=1) = 1 - phib*endb2
c NB this involves eigenvalues of QAA as well as those of QEE so there
c are kE+kA components.
c
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
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*4 tau(km),area(km),dens
	real*8 QD(km,km)
	real*8 phib(1,km),endb1(km,1),endb2(km,1),den
	allocatable::Q1,amat,eigen,w1,w
	real*8 amat(:,:,:),Q1(:,:),eigen(:),w1(:)
	real*4 w(:)
	allocatable::eigenA,w1A,wA
	real*8 eigenA(:),w1A(:)
	real*4 wA(:)
	integer EE
	integer AA
	logical debug
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
	AA=11
c
	ALLOCATE(amat(km,km,km),Q1(km,km),eigen(km),w1(km),w(km))
	ALLOCATE(eigenA(km),w1A(km),wA(km))
c
	call SUBMAT(QD,EE,Q1,km,km,km,km)	!QEE in Q1
c   Expand QEE
	CALL QMAT5(Q1,Amat,kE,eigen,ibad,km,km,km)
c Next line gives normal burst length pdf
	call RAC3(phib,endb1,amat,1,kA,1,kA,kE,w,w1,km,km,km)
c To remove the term for r=1 in the LT form of the distribution
c must subtract from this phib.exp(QAA*t)(-QAA)*endb2
c or phib.exp(QAA*t)*endb1
c Get QAA
	call SUBMAT(QD,AA,Q1,km,km,km,km)	!QAA in Q1
c   Expand QEE
	CALL QMAT5(Q1,Amat,kA,eigenA,ibad,km,km,km)
	call RAC3(phib,endb1,amat,1,kA,1,kA,kA,wA,w1A,km,km,km)
c
c  Divide all by normalising factor den = 1-p(r=1) = 1 - phib*endb2
c	subroutine VECMUL(row,col,scalar,k,kd1,kd2)
c	real*8 row(1,kd1),col(kd2,1),scalar
c	real*8 phib(1,km),endb2(km,1),den
	call VECMUL(phib,endb2,den,kA,km,km)
	den=1.d0 - den
	dens=sngl(den)
	do m=1,kE
	   w1(m)=w1(m)/den
	   w(m)=w(m)/dens
	enddo
	do m=1,kA
	   w1A(m)=w1A(m)/den
	   wA(m)=wA(m)/dens
	enddo
c
	ncomp=kE+kA
	mbad=0
	do m=1,kE		!first kE terms as for burst length
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
c Last kA terms from QAA
	do m=1,kA
	  m1=m+kE		!m1=kE+1,...,ncomp
	  x=dabs(eigenA(m))
	  if(x.lt.1.0d-35) then
		tau(m1)=1.e35
		area(m1)=1.0
		mbad=m1
	  else
		tau(m1)=1000./sngl(x)
		area(m1)=-sngl(w1A(m)/x)	!NB last kA terms subtracted
		w(m1)=wA(m)				!fill in rest of real*8 values
	  endif
	enddo
c
c area set to 1.0 for bad component, so set others to zero
	if(mbad.ne.0) then
	   do m=1,ncomp
		if(m.ne.mbad) area(m)=0.0		!area(mbad)=1 (above)
	   enddo
	endif
c Finally sort area, tau, into order of tau
	call SORT2r(tau,area,ncomp,km,.true.)
c
	DEALLOCATE(amat,Q1,eigen,w1,w)
	DEALLOCATE(eigenA,w1A,wA)
	RETURN
	end
