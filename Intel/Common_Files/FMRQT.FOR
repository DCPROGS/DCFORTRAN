	subroutine FMRQt(t0,tres,k,kA,kF,FMRQ,
     & Z00F,Z10F,Z11F,XFA,kFm,kAm,km)
c To calculate (F)MR(t)*QFA*exp(QAA*tres), where (F)MR(t) denotes the cumulative
c matrix used in calculate mean shut time given that it is in specified range
c (eg as X value for Magleby-type plots); result returned in FMRQ. (Calcs
c are similar in many ways to those in FCRQt (q.v.))
c (NB to get this without the 'QFA*exp(QAA*tres)' factor would
c have to get the C(i,m,r) (rather than Z00 etc) from HJCEXACT, and similarly
c rather than getting XFA() from HJCASYMP would have to get latter without
c the 'QFA*exp(QAA*tres)' factor.
c (NB functions GAM0 and GAM1 are defined in FCRQT)
c  In the call t=EXCESS time = true time-tres
c
c Corrected 04/04/02 09:22am In calls to FMRQt (from MOPADJ) output array FMRQ
c is 100x100
c
	use menu_f90
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 FMRQ(km,km),t0,t,tres,gam0,gam1,gam2
c=	real*8 FMRQ(kFm,kAm),t0,t,tres,gam0,gam1,gam2
	real*8 ex1(100),tauf(100)
c=	real*8 eigen(10),tauF(10)
c=	real*8 g00A(10),g10A(10),g11A(10),g00F(10),g10F(10),g11F(10)
c next 2 lines in common/fitblk
	real*8 eigen(100)
	real*8 g00A(100),g10A(100),g11A(100),g00F(100),g10F(100),g11F(100)
c
c=	common/ZFblk/Z00F(10,10,10),Z10F(10,10,10),Z11F(10,10,10)	!for FCRTQ
c=	common/xblkF/XFA(10,10,10)				!for FCRTQ
	real*8 Z00F(kFm,kAm,km),Z10F(kFm,kAm,km),Z11F(kFm,kAm,km)
	real*8 XFA(kFm,kAm,kFm)
c
	real*8 dexp1
	common/fitblk/eigen,g00A,g10A,g11A,g00F,g10F,g11F	!for HJCDISP
	common/root/rootA(100),rootF(100)
	logical goodx,good
	common/lim/good,goodx		!in FMRQT
	logical null
	common/dex/null		!in dexp1
c
	t=t0		!to prevent input being changed
	zero=0.d0
	two=2.d0
	do m=1,kF
	   tauF(m)=-1.d0/rootF(m)		!in 1/sec
	enddo
c
	if(t.lt.zero) then
	   if(t.gt.-1.d-6) then		!prob just rounding error
		t=zero
	   else
c		
		
		imes=gmdisplaymessagebox('',' ERROR in FMRQT',ginformation,gok)
		STOP
	   endif
	endif
	if(t.le.tres) then
	   do 1 i=1,kF
	   do 1 j=1,kA
	      FMRQ(i,j)=zero
		do 2 m=1,k
2		   FMRQ(i,j)=FMRQ(i,j) + Z00F(i,j,m)*gam1(t,m,eigen)
1	   continue
	else if(t.gt.tres.and.t.le.two*tres) then
	   do 3 i=1,kF
	   do 3 j=1,kA
	      FMRQ(i,j)=zero
		do 4 m=1,k
4		 FMRQ(i,j)=FMRQ(i,j) + Z00F(i,j,m)*gam1(t,m,eigen)    !m=0,r=0 term
     &	  - tres*Z10F(i,j,m)*gam0(t-tres,m,eigen)       !m=1,r=0 term
     &	  - Z10F(i,j,m)*gam1(t-tres,m,eigen)            !m=1,r=0 term
     &	  - tres*Z11F(i,j,m)*gam1(t-tres,m,eigen)        !m=1,r=1 term
     &	  - Z11F(i,j,m)*gam2(t-tres,m,eigen)             !m=1,r=1 term
3	   continue
	else 			!asymptotic
c        first get exact for t=2*tres
	   t2=two*tres
	   do 5 i=1,kF
	   do 5 j=1,kA
	      FMRQ(i,j)=zero
		do 6 m=1,k
		 FMRQ(i,j)=FMRQ(i,j) + Z00F(i,j,m)*gam1(t2,m,eigen)
     &	  - tres*Z10F(i,j,m)*gam0(t2-tres,m,eigen)
     &	  - Z10F(i,j,m)*gam1(t2-tres,m,eigen)
     &	  - tres*Z11F(i,j,m)*gam1(t2-tres,m,eigen)
     &	  - Z11F(i,j,m)*gam2(t2-tres,m,eigen)
6	   continue
5	   continue
c        next add to FMRQ the asymptotic part
c note that common/dex added to dexp1 (in dclib) so zero=true when dexp1 is
c set to 0 exactly. To get limiting form, all terms but slowest in the
c sum 1,..kF must be zero, i.e. all ex1(m) are zero except ex1(kF), so
	   n0=0
	   do m=1,kF
	      ex1(m)=(tauF(m)+t2)*dexp1(-t2/tauF(m))
     &         - (tauF(m)+t)*dexp1(-t/tauF(m))
	      e2=(tauF(m)+t2)*dexp1(-t2/tauF(m))
		e1=(tauF(m)+t)*dexp1(-t/tauF(m))
		if(null) n0=n0+1		!zero in common with dexp1
		ex1(m)=e2-e1
	   enddo
	   goodx=n0.lt.kF-1 	!good=false as soon as kF-1 are zero, last nonzero
c
c Note that ex1(m) includes the constant term dexp1(-t2/tauF(m)), so does
c not go to zero. Therefore calculate here using ex1(m) as normal -the
c logical 'good' is transmitted to mopadj for action there
c
	   do i=1,kF
	      do j=1,kA
		   do m=1,kF
			FMRQ(i,j)=FMRQ(i,j) + tauF(m)*XFA(i,j,m)*ex1(m)
		   enddo
		enddo
	   enddo
	endif
c
	RETURN
	end


	real*8 function GAM2(t,i,eigen)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 eigen(100)
c
	if(dabs(eigen(i)).lt.1.d-10) then
	   GAM2=t**3/3.d0
	else
	   y=eigen(i)*t
	   x=1.d0 + y + (y*y)/2.d0
	   x=1.d0 - x*dexp1(-y)
	   gam2=x*2.d0/(eigen(i)*eigen(i)*eigen(i))
	endif
	RETURN
	end


