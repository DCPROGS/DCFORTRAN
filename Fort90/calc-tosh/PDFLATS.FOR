	subroutine pdfLATs(QD,phiF0,tau,area,km)
c Subroutine to calc pdf of first latency after step of conc from zero to xA
c (only from zero at present, i.e. assumed never open at t=0).
c   f(t)=phiF0*exp(QFF*t)(-QFF)uF
c Input: phiF0() and Q1=Q(xA,xB) (if Q precalc then do not need
c		need xA0,xA too)
c Output: tau (ms), area (single prec).
c Part of family of subroutines to do single channel calcs via
c set of discrete subroutines that can be called as req.
c Params/commom
	real*8 QD(km,km),phiF0(1,10),x
	real*4 area(km),tau(km)
c Local
	real*8 Q1(10,10),eigen(10),amat(10,10,10)
	real*8 W1(10),ucol(10,1),col1(10,1)
c	integer AF,FF
	integer FF
	common/KBLK/kA,kB,kC,kD
c
	kF=kB+kC+kD       !known as kT in some progs
c	AF=17
	FF=77
	one=1.0d0
	do 130 i=1,km
130	ucol(i,1)=one
c Final vector
	call SUBMAT(QD,FF,Q1,km,km,km,km)	!QFF in Q1
	call MATMUL(Q1,ucol,col1,kF,kF,1,-one,km,km,km,1,km,1)  !-QFF*uF in col1
c Calc pdf
	call QMAT5(Q1,Amat,kF,eigen,ibad,km,km,km)
	do m=1,kF		!calc w1(m)
	   call MATSCL3(phiF0,Amat,m,col1,kF,w1(m),km,km,km)
	enddo
c
	ncomp=kF
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
	RETURN
	end

