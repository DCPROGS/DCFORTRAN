	subroutine DEPEND(tres,k,kA,kF,nshut,nopen,shutt,opent,DEP,
     &  dmin,dmax,
     & Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & XAF,XFA,QexpQA,QexpQF,
     & kAm,kFm,km,irt)

c
c To calculate Bivariate distribution f(tshut,topen), and hence
c   Magleby-Song dependency plot DEP=[f(to,ts) - p]/p where p=f(to)*f(ts)
c     (based on old hjccond with imode=4, and present popadjc, but values
c	calculated for a whole range of 'following shut times')
c
c NB EXPECTS shutt,opent TO CONTAIN LOG10(DURATION IN msec)
c
c NB for 3D plot with Hgraph routines need to have one-dimensional arrays
c for X and Y values, and 2D array, F(j,i), dimensioned (allocated) to exact
c number of X and Y values, containing the function F(j,i)=Func(Y(j),x(i))
c i.e. F(i,j)=Func(Y(i),x(j))
c Therefore calculate the required values of X() and Y() in shutt(), opent()
c in the calling program (log values normally, but need non-log times for
c the calculations done here) after allocating tshut(ns),
c topen(nopen) and DEP(nopen,nshut).
c  Then here calculate DEP(i,j)=func[tshut(i),topen(j)], and also return
c min and max values of dep(i,j) in dmin,dmax.
c See example in THREED.FOR -will need also to allocate the working
c arrays XX(ns,ns) and YY(no,no)
c OR IS IT XX(no,ns), YY(no,ns)???
c
c
c  The bivariate pdf for open time and following shut time is
c  f(topen,tshut) = f(to,ts)
c   = phiA*eGAF(to)*eGFA(ts)*uA =
c   = phiA*(A)R(to-tres)*QAF*exp(QFF*tres)*(F)R(ts-tres)*QFA*exp(QAA*tres)*uA
c (notation in popadj etc: t1=shut,t2=open)
c
c Modified 09/26/01 04:42pm for 100 state/10 ligand version
c
c New declarations
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c	real*4 shutt(nshut),opent(nopen),DEP(nopen,nshut)
	real*4 shutt(nshut),opent(nopen),DEP(nshut,nopen)
	real*4 dmin,dmax,dp
c
	real*8 Q1(100,100)
	real*8 ucol(100,1)
	real*4 t1
	
c extra for imode=2,3,4
	real*8 Z00A(kAm,kFm,km),Z10A(kAm,kFm,km),Z11A(kAm,kFm,km)
	real*8 Z00F(kFm,kAm,km),Z10F(kFm,kAm,km),Z11F(kFm,kAm,km)
	real*8 QEXPQA(kFm,kAM),QEXPQF(kAm,kFm)
	real*8 XAF(kAm,kFm,kAm),XFA(kFm,kAm,kFm)
c
c for common/fitblk
	real*8 eigen(100)
	real*8 g00A(100),g10A(100),g11A(100),g00F(100),g10F(100),g11F(100)
	allocatable::EG,EGFA,ex1,ex2
	real*8 EG(:,:),EGFA(:,:),ex1(:),ex2(:)
c
	common/phiblk/phiF(1,100),phiA(1,100)
	common/root/rootA(100),rootF(100)
	common/fitblk/eigen,g00A,g10A,g11A,g00F,g10F,g11F	!for HJCDISP,FCRTQ
	real*8 dexp1
c
c
	
c
	ALLOCATE(EG(kAm,kFm),EGFA(kFm,kAm),ex1(k),ex2(k))
	one=1.d0
	do i=1,km
	   ucol(i,1)=one
	enddo
c
	tres2=2.d0*tres		!real*8 in this subroutine
	tres3=3.d0*tres		!real*8 in this subroutine
	dmin=1.e37
	dmax=-1.e37
c
c START OUTER LOOP FOR THE SHUT TIMES:
	do ni=1,nshut
	   tshut=dble(10.**shutt(ni))*1.d-3		!in sec
	   if(tshut.lt.tres) tshut=tres 	!in case of rounding error in antilog
c       Get eGFA(t)=(F)R(t-tres)*QFA*exp(QAA*tres); result in EGFA()
	   if(tshut.le.tres2) then		!exact -first dead time
		t1=tshut-tres		!sec
		do m=1,k
		   ex1(m)=dexp1(-eigen(m)*t1)
		enddo
c
		do i=1,kF
	 	   do j=1,kA
			EGFA(i,j)=0.d0
			do m=1,k
			   EGFA(i,j)=EGFA(i,j)+Z00F(i,j,m)*ex1(m)
			enddo
		   enddo
		enddo
c
	   else if(tshut.gt.tres2.and.tshut.le.tres3) then		!exact -2nd deadtime
		t1=tshut-tres			!sec
		t2=t1-tres	    		!sec
		do m=1,k
	 	   ex1(m)=dexp1(-eigen(m)*t1)
	 	   ex2(m)=dexp1(-eigen(m)*t2)
		enddo
c
		do i=1,kF
		   do j=1,kA
			sum=0.d0
			do m=1,k
			   sum=sum+Z00F(i,j,m)*ex1(m)
			   sum=sum - (Z10F(i,j,m)+Z11F(i,j,m)*t2)*ex2(m)
			enddo
			EGFA(i,j)=sum
		   enddo
		enddo
c
	   else				!asymptotic
		t1=tshut-tres		!msec
		do m=1,kA
	 	   ex1(m)=dexp1(rootF(m)*t1)		!NB rootA() already negative
		enddo
		do i=1,kF
		   do j=1,kA
			EGFA(i,j)=0.d0
			do m=1,kF
			   EGFA(i,j)=EGFA(i,j)+XFA(i,j,m)*ex1(m)
			enddo
		   enddo
		enddo
	   endif
c END OF eGFA(t) CALC
c
C For denominator need unconditional pdf of shut time, f(tshut).
c fts=f(ts)=phiF*(F)R(ts-tres)*QFA*exp(QAA*tres)*uA=phiF*GFA(t)*uA
c (notation in popadj etc: t1=shut,t2=open)
c In MATSCL2 it was assumed that EGFA was declared as square matrix, but
c this is no longer true so need new matscl2d in which both declared dimensiond
c of the 2nd argument are specified in call
c=	   call MATSCL2(phiF,EGFA,ucol,kF,kA,fts,km,km,km)	!scalar=phi*QAF*col
c=         call MATSCL2d(phiF,EGFA,ucol,kF,kA,fts,km,kFm,kAm,km)	!scalar=phi*QAF*col
	   zero=0.0d0
	   fts=zero
	   do i=1,kF
		sum=zero
		do j=1,kA
		   sum=sum+EGFA(i,j)*ucol(j,1)
		enddo
		fts=fts+phiF(1,i)*sum
	   enddo
c
c Now loop over range of open times for which pdf is to be calculated
c START OF INNER LOOP
	   do nj=1,nopen
		topen=dble(10.**opent(nj))*1.d-3		!in sec
		if(topen.lt.tres) topen=tres 	!in case of rounding error in antilog
c       Get eGAF(t)=(A)R(to-tres)*QAF*exp(QFF*tres) from XAF; result in EG()
		if(topen.le.tres2) then		!exact -first dead time
		   t1=topen-tres		!sec
		   do m=1,k
			ex1(m)=dexp1(-eigen(m)*t1)
		   enddo
c
		   do i=1,kA
			do j=1,kF
			   EG(i,j)=0.d0
			   do m=1,k
				EG(i,j)=EG(i,j)+Z00A(i,j,m)*ex1(m)
			   enddo
			enddo
		   enddo
		else if(topen.gt.tres2.and.topen.le.tres3) then		!exact -2nd deadtime
		   t1=topen-tres			!sec
		   t2=t1-tres	    		!sec
		   do m=1,k
	 		ex1(m)=dexp1(-eigen(m)*t1)
			ex2(m)=dexp1(-eigen(m)*t2)
		   enddo
c
		   do i=1,kA
			do j=1,kF
			   sum=0.d0
			   do m=1,k
				sum=sum+Z00A(i,j,m)*ex1(m)
				sum=sum - (Z10A(i,j,m)+Z11A(i,j,m)*t2)*ex2(m)
			   enddo
			   EG(i,j)=sum
			enddo
		   enddo
c
		else			!asymptotic
		   t1=topen-tres		!msec
		   do m=1,kA
			ex1(m)=dexp1(rootA(m)*t1)		!NB rootA() already negative
		   enddo
c
		   do i=1,kA
			do j=1,kF
			   EG(i,j)=0.d0
			   do m=1,kA
				EG(i,j)=EG(i,j)+XAF(i,j,m)*ex1(m)
			   enddo
			enddo
		   enddo
		endif
c END OF eGAF(t) CALC in EG(i,j)
c
c Finally need unconditional distribution for current topen
C Finally for denominator need unconditional pdf of current open time, f(topen).
c fto=f(to)=phiA*(A)R(to-tres)*QAF*exp(QFF*tres)*uA=phiA*GAF(t)*uF
c Declaration is EG(kAm,kFm) so matcl2d needed
c=		call MATSCL2(phiA,EG,ucol,kA,kF,fto,km,km,km)	!scalar=phi*QAF*col
c      	call MATSCL2d(phiA,EG,ucol,kA,kF,fto,km,kFm,kAm,km)	!scalar=phi*QAF*col
		zero=0.0d0
		fto=zero
		do i=1,kA
		   sum=zero
		   do j=1,kF
			sum=sum+EG(i,j)*ucol(j,1)
		   enddo
		   fto=fto+phiA(1,i)*sum
		enddo
c And can now calculate the value of f(to)*f(ts) = d say
		d=fto*fts
c
c Get phiA*eGAF(to)*eGFA(ts)*uA in SCAL
      	call MATMUL(EG,EGFA,Q1,kA,kF,kA,one,
     & 	kAm,kFm,kFm,kAm,km,km)
c=      	call MATSCL2d(phiA,Q1,ucol,kA,kA,scal,km,km,km,km)
		zero=0.0d0
		scal=zero
		do i=1,kA
		   sum=zero
		   do j=1,kA
			sum=sum+Q1(i,j)*ucol(j,1)
		   enddo
		   scal=scal+phiA(1,i)*sum
		enddo
c
c and calculate dependence
		DP=sngl((scal - d)/d)
c		DEP(nj,ni)=dp
		DEP(ni,nj)=dp
		if(dp.gt.dmax) dmax=dp
		if(dp.lt.dmin) dmin=dp

c
	   enddo	!end of open times loop
	enddo		!end of shut times loop
c
	DEALLOCATE(EG,EGFA,ex1,ex2)
	RETURN
	end

