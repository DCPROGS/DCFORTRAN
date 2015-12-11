	subroutine POPADJC(tres,k,kA,kF,km,ycal1,xcal1,
     & shutlen,ncalc,ndc1,ndimc1,j1,j2)
	real*4 shutlen 		!in milliseconds
c
c To calculate:
c  Conditional pdf of open time, conditional on previous/next shut
c	time (theoretical version for an exactly-specified shut time)
c     (based on old hjccond with imode=4)
c================
c	subroutine MOPADJc(tres,QD,k,kA,kF,km,ycal1,xcal1,
c     & ncalc,ndc1,ndimc1,j1,j2)
c	subroutine HJCCOND(tres,ylo,yhi,nval,QD,k,kA,kF,km,
c     & ycal,xcal,ncalc,ndimc,den1,imode)
c=============
c Calc pdf of open time given previous/next gap length (continuous)
c Calc in ycal(i,j) (uses j=j1,j2 respectively for these values)
c
c (1) in ycal(i,j1),xcal(i,j1) (i=1,ncalc) the HJC pdf for openings that
c FOLLOW gaps of specified length
c (2) in ycal(i,j2),xcal(i,j2) ditto for openings that PRECEDE spec gaps
c
c For preceding gap (t1=shut,t2=open)
c  f(topen|tshut)=f(tshut,topen)/f(tshut), where the bivariate pdf, f(t1,t2)=
c phiF*(F)R(t1-tres)*QFA*exp(QAA*tres)*(A)R(t2-tres)*QAF*exp(QFF*tres)*uF/den
c where den=f(t1)=phiF*(F)R(t1-tres)*QFA*exp(QAA*tres)*uA=phiF*GFA(t)*uA
c
c For following gap
c  f(topen|tshut)=f(topen,tshut)/f(tshut), where the bivariate pdf, f(t2,t1)=
c phiA*(A)R(t2-tres)*QAF*exp(QFF*tres)*(F)R(t1-tres)*QFA*exp(QAA*tres)*uA/den
c All calc for some fixed specified t1=tshut which is specified in ylo(1)
c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c==	real*8 QD(10,10)
c==	real*8 Q1(10,10),DARS(10,10)
	real*8 Q1(10,10)
c==	real*8 row1(1,10)
	real*8 ucol(10,1)
c==	real*8 ucol(10,1),col1(10,1)
	real*4 XCAL1(ndc1,ndimc1),YCAL1(ndc1,ndimc1)
c=	real*4 xcal(2048,ndimc),ycal(2048,ndimc)
c=	real*4 xcal(2048,3),ycal(2048,3)
	real*4 t1
	logical debug,caplock,deb
c extra for imode=2,3,4
	real*8 Z00A(10,10,10),Z10A(10,10,10),Z11A(10,10,10)
	real*8 Z00F(10,10,10),Z10F(10,10,10),Z11F(10,10,10)
	real*8 eigen(10),EG(10,10),EGFA(10,10),ex1(10),ex2(10)
	real*8 g00A(10),g10A(10),g11A(10),g00F(10),g10F(10),g11F(10)
c
	common/phiblk/phiF(1,10),phiA(1,10)
	common/qexpblk/QEXPQA(10,10),QEXPQF(10,10)
	common/ZAblk/Z00A,Z10A,Z11A		!for POPADJ,ACRTQ
	common/ZFblk/Z00F,Z10F,Z11F		!for FCRTQ
	common/root/rootA(10),rootF(10)
	common/xblkA/XAF(10,10,10)				!for POPADJ,ACRTQ
	common/xblkF/XFA(10,10,10)				!for FCRTQ
	common/fitblk/eigen,g00A,g10A,g11A,g00F,g10F,g11F	!for HJCDISP,FCRTQ
c
c
	debug()=caplock()
c
	one=1.d0
	do i=1,km
	   ucol(i,1)=one
	enddo
c
c For preceding gap (t1=shut,t2=open)
c  f(topen|tshut)=f(tshut,topen)/f(tshut), where the bivariate pdf, f(t1,t2)=
c phiF*(F)R(t1-tres)*QFA*exp(QAA*tres)*(A)R(t2-tres)*QAF*exp(QFF*tres)*uF/den
c =phiF*eGFA(t1)*eGAF(t2)*uF/den
c where den=f(t1)=phiF*(F)R(t1-tres)*QFA*exp(QAA*tres)*uA=phiF*GFA(t)*uA
c
c For following gap
c  f(topen|tshut)=f(topen,tshut)/f(tshut), where the bivariate pdf, f(t2,t1)=
c phiA*(A)R(t2-tres)*QAF*exp(QFF*tres)*(F)R(t1-tres)*QFA*exp(QAA*tres)*uA/den
c =phiA*eGAF(t2)*eGFA(t1)*uA/den
c
c All calc for some fixed specified t1=tshut which is specified in parameter
c shutlen (in milliseconds).
c The matrix (F)R(t1-tres)*QFA*exp(QAA*tres) is constant for the specified
c shut time t1=shutlen, as is the denominator, so calculate these outside
c the loop
	tres2=2.d0*tres		!real*8 in this subroutine
	tres3=3.d0*tres		!real*8 in this subroutine
	tshut=dble(shutlen)*1.d-3		!in sec
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
c===*** following line was in original HJCCOND, but surely WRONG
c	else if(topen.gt.tres2.and.topen.le.tres3) then		!exact -2nd deadtime
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
c den=f(t1)=phiF*(F)R(t1-tres)*QFA*exp(QAA*tres)*uA=phiF*GFA(t)*uA
      call MATSCL2(phiF,EGFA,ucol,kF,kA,den,km,km,km)	!scalar=phi*QAF*col
c
c Now loop over range of open times for which pdf is to be calculated
	ycal1(1,j1)=0.0	!to draw vertical line
	ycal1(1,j2)=0.0	!to draw vertical line
	xcal1(1,j1)=xcal1(1,1)
	xcal1(1,j2)=xcal1(1,1)
	deb=debug()
	do 60 n=2,ncalc
	 xcal1(n,j1)=xcal1(n,1)
	 xcal1(n,j2)=xcal1(n,1)
	 top=dble(xcal1(n,1))	!in ms
	 topen=top*1.d-3		!in sec
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
		do  m=1,k
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
c END OF eGAF(t) CALC
c
c For preceding gap need phiF*eGFA(t1)*eGAF(t2)*uF/den
      call MATMUL(EGFA,EG,Q1,kF,kA,kF,one,
     & km,km,km,km,km,km)
      call MATSCL2(phiF,Q1,ucol,kF,kF,scal,km,km,km)
	ycal1(n,j1)=sngl(scal/den)	!preceding gap
c
c For following gap need phiA*eGAF(t2)*eGFA(t1)*uA/den
      call MATMUL(EG,EGFA,Q1,kA,kF,kA,one,
     & km,km,km,km,km,km)
      call MATSCL2(phiA,Q1,ucol,kA,kA,scal,km,km,km)
	ycal1(n,j2)=sngl(scal/den)	!following gap
60	continue	!end of ncalc loop
c
	RETURN
	end


