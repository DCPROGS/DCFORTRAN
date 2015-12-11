	subroutine POPADJ(tres,ylo,yhi,ncalc,k,kA,kF,
     & ycal,xcal,j1,j2,den1,
     & QEXPQA,QEXPQF,Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & XAF,XFA,kAm,kFm,kAx,km,nc,irt)
	 use menu_f90
c===================================================================
c To calculate from the current QD etc (values from last HJCLIK call)
c the conditional pdfs (use j=2,3 for these so j=1 can be used for the
c unconditional apparent open time pdf for superimposition)
c
c Modif 05/16/02 03:01pm to use limiting case when ylo is long, as
c  in MOPADJ (q.v. for comments).
c
c Modified 03/10/00 08:59am so that results go into ycal(i,j1), ycal(i,j2)
c rather than always taking j1=2, j2=3, as before
c
c Notation in paper is a bit different from here (F)CR(t) is (F)K(t) in
c paper, and XAF(t) here is the asymptotic (A)R(i)*QAF*exp(QFF*tres)
c where (a)R(i) is defined in eq 2.29 in paper.
c
c (1) in ycal(i,2),xcal(i,2) (i=1,ncalc) the HJC pdf for openings that
c FOLLOW gaps in a specified range ylo(1)=L to yhi(1)=U
c (2) in ycal(i,3),xcal(i,3) ditto for openings that PRECEDE spec gaps
c
c (1) f(to|L<Tc<U)=phiF*{(F)CR(U-tres)-(F)CR(L-tres)}*QFA*exp(QAA*tres)*
c     (A)R(to-tres)*QAF*exp(QFF*tres)*uF/den
c 	den=FTc(U)-FTc(L)
c (2) = phiA*(A)R(to-tres)*QAF*exp(QFF*tres)*{(F)CR(U-tres)
c	-(F)CR(L-tres)}*QFA*exp(QAA*tres)*uA/den
c
c For calc of (A)R(to-tres)*QAF*exp(QFF*tres)=eGAF(t) can use Zir(m)=
c Cir(m)*QAF*EXPQF (ir=00,10,11) from HJCEXACT (via common in HJCLIK) for
c the exact part and for asymptotic part can use rootA(m) and XAF(i,j,m)
c from HFJASYMP (via common in HJCLIK) where
c	XAF(m)=colvec(m)*rowvec(m)*QAF*exp(QFF*tres)/denom
c where denom=rowvec(m)*W'(s(m))*colvec(m) (see HJCLIK for usage)
c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 row0(1,100),row1(1,100)
	real*8 ucol(100,1)
	allocatable::EG,Q1,Q2
	real*8 EG(:,:),Q1(:,:),Q2(:,:)
	real*8 ex1(100),ex2(100)
	real*4 den1(20)
	real*4 ylo(20),yhi(20)
	!real*4 xcal(2048,10),ycal(2048,10)          !for VHIST3
	real*4 xcal(nc,10),ycal(nc,10) 
	real*4 am1,sd1			!for output from pdfoutd
	character*256 string
	logical discprt
c
	common/phiblk/phiF(1,100),phiA(1,100)
c=	common/qexpblk/QEXPQA(10,10),QEXPQF(10,10)
c=	common/ZAblk/Z00A,Z10A,Z11A		!for POPADJ,ACRTQ
c=	common/xblkA/XAF(10,10,10)				!for POPADJ,ACRTQ
c next 4 lines no longer in commons in 100 state version
	real*8 QEXPQA(kFm,kAM),QEXPQF(kAm,kFm)
	real*8 XAF(kAm,kFm,kAm),XFA(kFm,kAm,kFm)
	real*8 Z00A(kAm,kFm,km),Z10A(kAm,kFm,km),Z11A(kAm,kFm,km)
	real*8 Z00F(kFm,kAm,km),Z10F(kFm,kAm,km),Z11F(kFm,kAm,km)

	common/root/rootA(100),rootF(100)
	real*8 eigen(100)
	real*8 g00A(100),g10A(100),g11A(100),g00F(100),g10F(100),g11F(100)
	real*8 dexp1
	common/fitblk/eigen,g00A,g10A,g11A,g00F,g10F,g11F	!for HJCDISP,FCRTQ
	common/dp/discprt
	logical good,goodx
	common/lim/good,goodx		!in FCRQT
	common/tty/ittypanel,itty
c
	
c
	one=1.d0
c	kax=60
	do 4 i=1,km
4	ucol(i,1)=one
	ALLOCATE(EG(kAm,kFm),Q1(km,km),Q2(km,km))
c
	tlo=dble(ylo(1))*1.d-3       !low time (sec)
	thi=dble(yhi(1))*1.d-3       !hi  time (sec)
	den=FTCt(thi,tres,k,kA,kF,ucol,
     &		Z00F,Z10F,Z11F,XFA,kAm,kFm,km)
     &   - FTCt(tlo,tres,k,kA,kF,ucol,
     & 		Z00F,Z10F,Z11F,XFA,kAm,kFm,km)
	den1(1)=sngl(den)		!for output
	if(.not.good.or.den.lt.1.d-200) then
c	Use only slowest m=kF
	   do i=1,kF
		do j=1,kA
		   Q1(i,j)=XFA(i,j,kF)
		enddo
	   enddo
c	   recalculate denominator, without the cancelled bit
	   call MATSCL2d(phiF,Q1,ucol,kF,kA,den,km,km,km,km)
	   den1(1)=0.0		!for output when den->0
	   good=.false.	!in case
	   goto 9	!then all same, using modified Q1
	endif
c Get FCRQ=(F)CR(t-tres)*QFA*exp(QAA*tres)
c NB arg corresponding to Q1 is 100x100 in fcrqt at present
	call FCRQt(tlo-tres,tres,k,kA,kF,Q1,
     &  	Z00F,Z10F,Z11F,XFA,kAm,kFm,km)
	call FCRQt(thi-tres,tres,k,kA,kF,Q2,
     &  	Z00F,Z10F,Z11F,XFA,kAm,kFm,km)
c Subtract them
	do i=1,kF
	   do j=1,kA
		Q1(i,j)=Q2(i,j) - Q1(i,j)
	   enddo
	enddo
c Now Q1={(F)CR(U-tres)-(F)CR(L-tres)}*QFA*exp(QAA*tres)
9	continue		!jump here with modif Q1 for limiting case
c
c Calcs for PRECEDING gap (and for unconditional?) (do calcs for
c following gap separately since may not usually be needed?)
c Note that xcal(i,j) already defined before calling
c  Complete calcs that can be done outside loop for i=1,ncalc
      call MATMUL(phiF,Q1,row0,1,kF,kA,one,1,km,km,km,1,km) !phiF*Q1 in row0
c  Now row0 contains entry probs for each open state (conditional on
c specified shut time range) (must be deivided by den).
c
	tres2=2.d0*tres		!real*8 in this subroutine
	tres3=3.d0*tres		!real*8 in this subroutine
c
c Start loop for calc values
c	ycal(1,1)=0.0	!to draw vertical line
	ycal(1,j1)=0.0	!to draw vertical line
	ycal(1,j2)=0.0	!to draw vertical line
	xcal(1,j1)=xcal(1,1)
	xcal(1,j2)=xcal(1,1)
c	deb=debug()
	do 1 n=2,ncalc
	 xcal(n,j1)=xcal(n,1)
	 xcal(n,j2)=xcal(n,1)
	 top=dble(xcal(n,1))	!in ms (for F0HJC,F1HJC)
	 topen=top*1.d-3		!in sec
	 if(topen.le.tres2) then		!exact -first dead time
	   t1=topen-tres		!sec
	   do 56 m=1,k
56	    ex1(m)=dexp1(-eigen(m)*t1)
c
	    do 57 i=1,kA
	    do 57 j=1,kF
		EG(i,j)=0.d0
		do 58 m=1,k
58	       EG(i,j)=EG(i,j)+Z00A(i,j,m)*ex1(m)
57	    continue
	else if(topen.gt.tres2.and.topen.le.tres3) then		!exact -2nd deadtime
	   t1=topen-tres			!sec
	   t2=t1-tres	    		!sec
	   do 561 m=1,k
	    ex1(m)=dexp1(-eigen(m)*t1)
561	    ex2(m)=dexp1(-eigen(m)*t2)
c
	   do 571 i=1,kA
	   do 571 j=1,kF
		sum=0.d0
		do 581 m=1,k
	       sum=sum+Z00A(i,j,m)*ex1(m)
		 sum=sum - (Z10A(i,j,m)+Z11A(i,j,m)*t2)*ex2(m)
581		continue
	   EG(i,j)=sum
571	   continue
c
	else			!asymptotic
c       Get eGAF(t)=(A)R(to-tres)*QAF*exp(QFF*tres) from XAF; result in EG()
	   t1=topen-tres		!msec
	   do 562 m=1,kA
562	    ex1(m)=dexp1(rootA(m)*t1)		!NB rootA() already negative
c
	    do 572 i=1,kA
	    do 572 j=1,kF
		EG(i,j)=0.d0
		do 582 m=1,kA
582	       EG(i,j)=EG(i,j)+XAF(i,j,m)*ex1(m)
572	    continue
	endif
c END OF eGAF(t) CALC
c Now f(t|L<t<U)=row1*EG*uF/den=phiF*Q1*EG*uF
      call MATMUL(row0,EG,row1,1,kA,kF,one,
     &	1,km,kAm,kFm,1,km)
      call VECMUL(row1,ucol,scal,kF,km,km)	!row*col=scalar
	ycal(n,j1)=sngl(scal/den)
c for following gap:
c (2) = phiA*(A)R(to-tres)*QAF*exp(QFF*tres)*{(F)CR(U-tres)
c	-(F)CR(L-tres)}*QFA*exp(QAA*tres)*uA/den
c     =phiA*EG*Q1*uA
      call MATMUL(phiA,EG,row1,1,kA,kF,one,
     & 1,km,kAm,kFm,1,km)
      call MATMUL(row1,Q1,row1,1,kF,kA,one,1,km,km,km,1,km)
      call VECMUL(row1,ucol,scal,kA,km,km)	!postmult by uA
	ycal(n,j2)=sngl(scal/den)
c Calculation of conditional density, in Ycal(), now finished
1	continue	!end of n=2,ncalc loop
c
c  Print the entry probs in row0()
      
	CALL GMSETTEXTSETTING(ITTY,
     &' Entry probabilities for each open state, conditional on being')
	WRITE(STRING,fmt='(a20,g11.4,a4,g11.4)') 
     & 'preceded by shut time in range ',ylo(1),' to',yhi(1)
	
	CALL GMSETTEXTSETTING(ITTY,string)
      if(discprt) write(7,20) ylo(1),yhi(1)
20	format(/,
     & ' Entry probabilities for each open state, conditional on being'/
     & ,' preceded by shut time in range ',g11.4,' to ',g11.4,' ms')
	do m=1,kA
	WRITE(STRING,fmt='(a10,i3,a3,g13.6)') 'state ',m,
     & ' :',row0(1,m)/den
         
         if(discprt) write(7,21) m,row0(1,m)/den
21	   format('    state ',i3,3x,g13.6)
	enddo
c  Calculate and print, for interest, the AREAS (asymptotic) for the
c  conditional open time pdf (for the specified PRECEDING shut time range)
c -only areas (not tau) depend on shut time range. From eq 4.7, Srodzinski et al
c   a(i)=tau(i)*phiF*{(F)CR(U-tres)-(F)CR(L-tres)}*QFA*exp(QAA*tres)*
c     (A)R(i)*QAF*exp(QFF*tres)*uF/den
c 	den=FTc(U)-FTc(L)
c i.e. a(m)=tau(m)*row0*XAF(m)*uF/den
c or amp(m)=row0*XAF(m)*uF/den -keep amplitude in array ex1(10) here
c or
	do m=1,kA
	   call MATSCL3D(row0,XAF,m,ucol,kA,kF,SCAL,
     &	km, kAm,kFm,kAm, km)
	   ex1(m)=scal/den	     		!amplitude
	enddo
cc	call PDFOUTd(
cc     & ' ASYMPTOTIC OPEN TIME DISTRIBUTION COND ON PRECEDING GAP RANGE',
cc     &	-1,-1,ex1,rootA,kA,am1,sd1,km,.false.,.true.,discprt)
	call ACONVd(ex1,rootA,kA,tres,discprt,km)	!renormalise areas
c
	DEALLOCATE(EG,Q1,Q2)
	RETURN
	end

