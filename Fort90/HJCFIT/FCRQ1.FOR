	subroutine FCRQ1(t0,tres,k,kA,kF,FCRQ,
     &  Z00F,Z10F,Z11F,XFA,kAm,kFm,km)
c
c FCRQ1 is version of FCRQt which uses simplified form for asymptotic
c case which omits constant terms and so can be used ONLY when two values
c (for tlo, thi) are subtracted from each other, as in MOPADJ.  This makes
c it easier to deal with the limiting case where difference between two
c values is close to zero (at long shut times -see notes below)
c
c To calculate (F)CR(t)*QFA*exp(QAA*tres), where (F)CR(t) denotes the cumulative
c form of (F)R(t), for HJC distributions; result returned in FCRQ.
c This is given in CHS96, p2569.
c (NB to get cumulative form without the 'QFA*exp(QAA*tres)' factor would
c have to get the C(i,m,r) (rather than Z00 etc) from HJCEXACT, and similarly
c rather than getting XFA() from HJCASYMP would have to get latter without
c the 'QFA*exp(QAA*tres)' factor.
c  In the call t=EXCESS time = true time-tres
c For t< 2*tres use the exact form, viz FCR(t)=
c Sum(i=1,k).Sum(m=0,1).Sum(r=0,m){(-1**m)*C(i,m,r)*Gam(t-m*tres;eig(i))}
c where GAM resembles an incomplete gamma function; eig(i)=eigenvalues of -Q
c ie for 0<t<tres
c  (F)CR(t)=Sum(1,k){C(i,0,0)*gam0(t;eig(i))
c for tres<t<2tres
c  (F)CR(t)=Sum(1,k){C(i,0,0)*gam0(t;eig(i)) - C(i,1,0)*gam0(t-tres;eig(i))
c							-C(i,1,1)*gam1(t-tres;eig(i))
c where
c 	gam0(t;eig(i))=[1-exp(-eig(i)*t]/eig(i)
c	gam1(t;eig(i))={1 - exp(-eig(i)*t)[1 + eig(i)*t]}/(eig(i)**2)
c NB Lim(eig=0) [GAMr(t;eig)] = t**r/r
c Note: the gam0,gam1 are scalar so can use values of	Z00F(m)=Ci00(m)*QFA*EXPQA
c calculated in HJCEXACT (and Z10F,Z11F)
c
c For t>2*tres use asymptotic form starting from (F)CR(2*tres) from exact
c calc above to give
c FCR=(F)CR(2tres) + Sum(i=1,kF){(F)R(i)[exp(-2tres/tauF(i) - exp(-t/tauF(i)]}
c where tauF(i)=1/rootF(i) and (F)R(i)=areas for asymptotic shut time pdf, which
c for shut times can be got from the matrices XFA(m) calc in HJCASYMP (which
c also include the factor QFA*exp(QAA*tres) needed for calcs) by multiplying
c by tauF(m)
c	(tauF(m))*XFA(m)=tauF(m)*colvec(m)*rowvec(m)*QFA*exp(QAA*tres)/denom
c where denom=rowvec(m)*W'(s(m))*colvec(m)
c so f(t)=phiF*SUM[XFA(m)*exp(rootF(m)*t1)]uA for shut times
c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 FCRQ(100,100)
	real*8 ex1(100)
	real*8 eigen(100)
	real*8 g00A(100),g10A(100),g11A(100),g00F(100),g10F(100),g11F(100)
	real*8 tauF(100)
c
c-	common/ZFblk/Z00F(10,10,10),Z10F(10,10,10),Z11F(10,10,10)	!for FCRTQ
c=	common/xblkF/XFA(10,10,10)				!for FCRTQ
	real*8 Z00F(kFm,kAm,km),Z10F(kFm,kAm,km),Z11F(kFm,kAm,km)
	real*8 XFA(kFm,kAm,kFm)
	common/fitblk/eigen,g00A,g10A,g11A,g00F,g10F,g11F	!for HJCDISP
	common/root/rootA(100),rootF(100)
	real*8 dexp1
	logical good
	common/lim/good		!in FCRQT
	logical zero
	common/dex/zero		!in dexp1
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
c		call BELL(3)
c		print 11,t
c11		format(' ERROR: t = ',g13.6,' in FCRQT')
		STOP
	   endif
	endif
	if(t.le.tres) then
	   do 1 i=1,kF
	   do 1 j=1,kA
	      FCRQ(i,j)=zero
		do 2 m=1,k
2		   FCRQ(i,j)=FCRQ(i,j) + Z00F(i,j,m)*gam0(t,m,eigen)
1	   continue
	else if(t.gt.tres.and.t.le.two*tres) then
	   do i=1,kF
		do j=1,kA
	         FCRQ(i,j)=zero
		   do m=1,k
		      FCRQ(i,j)=FCRQ(i,j) + Z00F(i,j,m)*gam0(t,m,eigen)
     &	    - Z10F(i,j,m)*gam0(t-tres,m,eigen)
     &	    - Z11F(i,j,m)*gam1(t-tres,m,eigen)
		   enddo
		enddo
	   enddo
	else 			!asymptotic
c   first get exact for t=2*tres
	   t2=two*tres
	   do i=1,kF
	      do j=1,kA
		   FCRQ(i,j)=zero
		   do m=1,k
			term1=Z00F(i,j,m)*gam0(t2,m,eigen)
			term2=Z10F(i,j,m)*gam0(t2-tres,m,eigen)
			term3=Z11F(i,j,m)*gam1(t2-tres,m,eigen)
			FCRQ(i,j)=FCRQ(i,j) + term1 - term2 - term3
c			FCRQ(i,j)=FCRQ(i,j) + Z00F(i,j,m)*gam0(t2,m,eigen)
c     &	      - Z10F(i,j,m)*gam0(t2-tres,m,eigen)
c     &	      - Z11F(i,j,m)*gam1(t2-tres,m,eigen)
		   enddo
		enddo
	   enddo
c        next add to FRCQ the asymptotic part
c note that common/dex added to dexp1 (in dclib) so zero=true when dexp1 is
c set to 0 exactly. To get limiting form, all terms but slowest in the
c sum 1,..kF must be zero, i.e. all ex1(m) are zero except ex1(kF), so 
	   n0=0
	   do m=1,kF
c=	      ex1(m)=dexp1(-t2/tauF(m)) - dexp1(-t/tauF(m))
		e2=dexp1(-t2/tauF(m))	!constant -should always be good
		e1=dexp1(-t/tauF(m))	!will go to 0 for large t
		ex1(m)=e2-e1
		if(zero) n0=n0+1		!zero in common with dexp1
	   enddo
	   good=n0.lt.kF-1 	!good=false as soon as kF-1 are zero, last nonzero
c
c Note that ex1(m) includes the constant term dexp1(-t2/tauF(m)), so does
c not go to zero. Therefore calculate here using ex1(m) as normal -the
c logical 'good' is transmitted to mopadj.
	   do i=1,kF
		do j=1,kA
c		   if(good) then
			do m=1,kF
			   FCRQ(i,j)=FCRQ(i,j) + tauF(m)*XFA(i,j,m)*ex1(m)
			enddo
c		   else
c===
c		   endif
		enddo
	   enddo
	endif
c
	RETURN
	end

	real*8 function GAM0(t,i,eigen)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 eigen(100)
	real*8 dexp1
c NB eigenvalues of -Q are pos
	x=eigen(i)*t
	if(dabs(x).lt.1.e-6) then
	   GAM0=t
	else
	   GAM0=(1.d0-dexp1(-x))/eigen(i)
	endif
c NB orig version can give lethal rounding errors!
c	if(dabs(eigen(i)).lt.1.d-10) then
c	   GAM0=t
c	else
c	   GAM0=(1.d0-dexp1(-eigen(i)*t))/eigen(i)
c	endif
	RETURN
	end

	real*8 function GAM1(t,i,eigen)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 eigen(100)
	real*8 dexp1
c
	x=eigen(i)*t
	if(dabs(x).lt.1.e-6) then
	   GAM1=t**2/2.d0
	else
	   GAM1=(1.d0-dexp1(-x)*(1.d0 + x))/(eigen(i)**2)
	endif
c NB orig version can give lethal rounding errors!
c	if(dabs(eigen(i)).lt.1.d-10) then
c	   GAM1=t**2/2.d0
c	else
c	   GAM1=(1.d0-dexp1(-eigen(i)*t)*(1.d0+eigen(i)*t))/(eigen(i)**2)
c	endif
	RETURN
	end

