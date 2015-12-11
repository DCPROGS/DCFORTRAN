	subroutine MOPADJc(tres,QD,k,kA,kF,
     & ycal1,xcal1,ncalc,ndc1,ndimc1,j1,j2,
     & QEXPQA,QEXPQF,Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,XFA,kAm,kFm,km)
c
c NB ERROR IN ORIGINAL HJCCOND NOW FIXED (see ***)
c To calculate THEORETICAL mean open time given previous/next gap length
c (continuous function) (based in old hjccond with imode=3 from old hjcdist).
c  as function of gap length.  Mean is calc for ncalc values of shut time.
c (MOPADJ does calculation for adjacent gap RANGE)
c
c NB XAF(m) and XFA(m) are the matrices denoted (A)R(m)*QAF*EXPQF and
c (F)R(m)*QFA*EXPQA where (A)R(m) is defined in CHS96 eq.2.29
c and EXPQA=exp(QAA*tres)
c
c For openings conditional on preceding gap, kept in ycal(i,j1),xcal(i,j1),
c  E(topen|tshut=t1) = tres +
c phiF*(F)R(t1-tres)*QFA*exp(QAA*tres)*{[-d(A)R(s)/ds]s=0}*QAF*exp(QFF*tres)*uF/den
c =phiF*eGFA(t)**[-d(A)R(s)/ds]s=0*QAF*exp(QFF*tres)*uF/den
c where den=f(t1)=phiF*(F)R(t1-tres)*QFA*exp(QAA*tres)*uA=phiF*GFA(t)*uA
c
c For following gap, kept in ycal(i,j2),xcal(i,j2),
c E(topen|tshut=t1) = tres +
c phiA*{[-d(A)R(s)/ds]s=0}*QAF*exp(QFF*tres)*(F)R(t1-tres)*QFA*exp(QAA*tres)*uA/den
c  =phiA**[-d(A)R(s)/ds]s=0*QAF*exp(QFF*tres)*eGFA(t)*uA/den
c where den=f(t1)=as above
c
c  Note that for large (shut) time the expression for conditional mean open
c  time tends to 0/0, but empirically the graph gets flat.  Not sure how to
c  to get correct limiting form, but dexp1(x) is set to 0 for x<-708
c  and once all the terms dexp1(rootF(m)*t1) are zero then we get a 0/0 error
c  The slowest rate constant is the last, rootF(kF), and this is the last term
c   to go to zero.  At the point where this is the only non-zero term in the
c  sum 1...kF, we have a single exponential that cancels between numerator
c  and denominator, so in both we can keep only the last term, without the
c  exponential, so in place of
c			do m=1,kF
c			   EG(i,j)=EG(i,j)+XFA(i,j,m)*ex1(m)
c			enddo
c   we have
c			   EG(i,j)=EG(i,j)+XFA(i,j,kF)
c   in both numerator and denominator (eGFA is calculated only once, and used
c   in both numerator and demoninator).  When this is in use, set good=false
c   05/13/02 08:28pm x>-708 allows values that are on verge of running out of
c	precision, so now set good=false as soon as kF-1 exponentials are zero
c  See 'conditional mean.doc' for full algebra
c
c
c Modified 05/09/02 10:11am to correct error in asymptotic (see !!!)
c
c Modified 10/01/01 06:22am for 100 state/10 ligand -last laine of arguments added
c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
	real*8 QD(km,km)
c=	real*8 Q1(10,10),EG(10,10)  !now allocated
	real*8 ex1(100),ex2(100)
	real*8 row1(1,100)
	real*8 ucol(100,1),col1(100,1)
	real*4 XCAL1(ndc1,ndimc1),YCAL1(ndc1,ndimc1)
c=	real*4 xcal(2048,ndimc),ycal(2048,ndimc)
c=	real*4 xcal(2048,3),ycal(2048,3)
c=	real*4 t1
	logical debug,caplock,deb
c
c=	real*8 Z00A(10,10,10),Z10A(10,10,10),Z11A(10,10,10)
c=	real*8 Z00F(10,10,10),Z10F(10,10,10),Z11F(10,10,10)
c
c=	common/qexpblk/QEXPQA(10,10),QEXPQF(10,10)
c=	common/ZAblk/Z00A,Z10A,Z11A		!for POPADJ,ACRTQ
c=	common/ZFblk/Z00F,Z10F,Z11F		!for FCRTQ
c=	common/xblkA/XAF(10,10,10)				!for POPADJ,ACRTQ
c=	common/xblkF/XFA(10,10,10)				!for FCRTQ
c==	real*8 XAF(kAm,kFm,kAm),XFA(kFm,kAm,kFm)
c next 4 lines no longer in commons in 100 state version
	real*8 Z00A(kAm,kFm,km),Z10A(kAm,kFm,km),Z11A(kAm,kFm,km)
	real*8 Z00F(kFm,kAm,km),Z10F(kFm,kAm,km),Z11F(kFm,kAm,km)
	real*8 QEXPQA(kFm,kAM),QEXPQF(kAm,kFm)
	real*8 XFA(kFm,kAm,kFm)

c DARS IS kAm x kAm in darsds subroutine so must allocate here DARS(100,100)
c Also allocate EG and Q1 to save memory
	allocatable::DARS,EG,Q1
	real*8 DARS(:,:),EG(:,:),Q1(:,:)
c
	logical discprt,bad,good,first
	common/dp/discprt
c   Things below still in commons in 100 state version
	common/phiblk/phiF(1,100),phiA(1,100)
	common/root/rootA(100),rootF(100)
	real*8 g00A(100),g10A(100),g11A(100),g00F(100),g10F(100),g11F(100)
	real*8 eigen(100)
	common/fitblk/eigen,g00A,g10A,g11A,g00F,g10F,g11F	!for HJCDISP,FCRTQ
	real*8 dexp1
	logical zero
	common/dex/zero		!in common with dexp1
c
	debug()=caplock()
c
	one=1.d0
	do i=1,km
	   ucol(i,1)=one
	enddo
	ALLOCATE(DARS(kAm,kAm),EG(kFm,kAm),Q1(kAm,kFm))
c
c phiF*(F)R(t1-tres)*QFA*exp(QAA*tres)*[-d(A)R(s)/ds]s=0*QAF*exp(QFF*tres)*uF/den
c where den=f(t1)=phiF*(F)R(t1-tres)*QFA*exp(QAA*tres)*uA=phiF*GFA(t)*uA
103	continue
c	Can first calc [-d(A)R(s)/ds]s=0*QAF*exp(QFF*tres) in Q1; this is indep
c of t1 and is needed for both preceding and following gap
c=	call dARSdS(QD,tres,DARS,kA,kF,km)
	call dARSdS(QD,tres,DARS,kA,kF,kAm,kFm,km)
c Already have QAF*exp(QFF*tres) in QEXPQF (kA*kF)
      call MATMUL(DARS,QEXPQF,Q1,kA,kA,kF,one,kAm,kAm,kAm,kFm,kAm,kFm)
c For preceding gap need Q1*uF -keep in col1 (kA*1)
      call MATMUL(Q1,ucol,col1,kA,kF,1,one,kAm,kFm,km,1,km,1)
c For following gap need phiA*Q1 -keep in row1  (1*kF)
      call MATMUL(phiA,Q1,row1,1,kA,kF,one,1,km,kAm,kFm,1,km)
c Now loop for t1 values to get (F)R(t1-tres)*QFA*exp(QAA*tres)
	tres2=2.d0*tres		!real*8 in this subroutine
	tres3=3.d0*tres		!real*8 in this subroutine
c
c Start loop for calc values
	deb=debug()
	good=.true.
	first=.true.
	do 30 n=1,ncalc
	   xcal1(n,j2)=xcal1(n,j1)
	   tgap=dble(xcal1(n,j1))	!in ms (for F0HJC,F1HJC)
	   tshut=tgap*1.d-3		!in sec
c       Get eGFA(t)=(F)R(t-tres)*QFA*exp(QAA*tres); result in EG()
	   if(tshut.le.tres2) then		!exact -first dead time
		t1=tshut-tres		!sec
		do m=1,k
	 	   ex1(m)=dexp1(-eigen(m)*t1)
		enddo
c
		do i=1,kF
		   do j=1,kA
			EG(i,j)=0.d0
			do m=1,k
			   EG(i,j)=EG(i,j)+Z00F(i,j,m)*ex1(m)
			enddo
		   enddo
		enddo
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
			EG(i,j)=sum
		   enddo
		enddo
c
	   else				!asymptotic
		t1=tshut-tres		!sec
c		good=rootF(kF)*t1.ge.-708.d0		!all exp() now zero
c		if(deb) then
c		   do m=1,kF
c			print 70,tgap,m,rootF(m)*t1
c			if(discprt) write(8,70) tgap,m,rootF(m)*t1
c70			format(' ts = ',f7.3,' t1/tau(',i2,') = ',g13.6)
c		   enddo
c		endif
c=!!!!!	do m=1,kA	!bad error, in old version too
		n0=0
		do m=1,kF
		   ex1(m)=dexp1(rootF(m)*t1)		!NB rootA() already negative
		   if(zero) n0=n0+1	!zero in common with dexp1
		enddo
		good=n0.lt.kF-1 	!good=false as soon as kF-1 are zero, last nonzero
		do i=1,kF
		   do j=1,kA
			EG(i,j)=0.d0
			if(good) then
			   do m=1,kF
				EG(i,j)=EG(i,j)+XFA(i,j,m)*ex1(m)
			   enddo
			else
			   EG(i,j)=EG(i,j)+XFA(i,j,kF)	!see above
			endif
		   enddo
		enddo
	   endif
c END OF eGFA(t) CALC
c (same eGFA(t) used for num and den)
C For denominator need unconditional pdf
c den=f(t1)=phiF*(F)R(t1-tres)*QFA*exp(QAA*tres)*uA=phiF*GFA(t)*uA
         call MATSCL2d(phiF,EG,ucol,kF,kA,den,km,kFm,kAm,km)	!scalar=phi*QAF*col
c For preceding gap need
c =phiF*eGFA(t)**[-d(A)R(s)/ds]s=0*QAF*exp(QFF*tres)*uF/den=phiF*eGFA(t)*col1/den
c For following gap need row1*eGFA(t)*uA/den
c
c For large times may get scal=0 and den=0 so check before dividing
c If the asymptotic form for large t, above, works, then 'bad' should not
c ever be needed
	   bad=.false.
         call MATSCL2d(phiF,EG,col1,kF,kA,scal,km,kFm,kAm,km)	!scalar=phi*QAF*col
	   bad=.false.
	   if(dabs(den).lt.dabs(scal)/1.d230) bad=.true.
c	   if(dabs(den).lt.1.d-200) bad=.true.
	   if(.not.bad) then
	      ycal1(n,j1)=1000.*sngl(tres + scal/den)	!preceding gap; in msec
	   else
		print 1,xcal1(n,j1),ncalc,n-1
		if(discprt) write(8,1) xcal1(n,j1),ncalc,n-1
1		format(' Numerical problem in time = ',g11.4,' ms'
     &	' Reduce Ncalc from ',i4,' to ',i4,/)
		ncalc=n-1
		goto 99
	   endif
c
         call MATSCL2d(row1,EG,ucol,kF,kA,scal,km,kFm,kAm,km)	!scalar=phi*QAF*col
	   bad=.false.
	   if(dabs(den).lt.dabs(scal)/1.d230) bad=.true.
	   if(.not.bad) then
		ycal1(n,j2)=1000.*sngl(tres + scal/den)	!following gap; in msec
	   else
		print 1,xcal1(n,j2),ncalc,n-1
		if(discprt) write(8,1) xcal1(n,j2),ncalc,n-1
c1		format(' Numerical problem in time = ',g11.4,' ms'
c     &	' Reduce Ncalc from ',i4,' to ',i4,/)
		ncalc=n-1
		goto 99
	   endif
	   if(.not.good.and.first) then
		print 2,ycal1(n,j1),xcal1(n,j1)
		if(discprt) write(8,2) ycal1(n,j1),xcal1(n,j1)
2		format(/,' For continuous curve, note that',/,
     &    ' asymptotic mean = ',g13.6,' used above t = ',g11.4,' ms',/)
		first=.false.		!so printed only once
		pause
	   endif
30	continue		!end of n=2,ncalc loop
c
99	DEALLOCATE(DARS,EG,Q1)
	RETURN
	end

