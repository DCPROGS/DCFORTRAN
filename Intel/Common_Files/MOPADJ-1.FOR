	subroutine MOPADJ(tres,ylo,yhi,nval,QD,k,kA,kF,
     & ycal1,xcal1,ndc1,ndimc,den1,j1,j2,
     & QEXPQA,QEXPQF,Z00F,Z10F,Z11F,XFA,kAm,kFm,km,
     & EXPQF,irt)
c
c To calculate from the current QD etc (values from last HJCLIK call)
c (1) in ycal1(i,j1),xcal1(i,j1), the HJC mean open time for openings that
c FOLLOW gaps in a specified range ylo(i)=L to yhi(i)=U, for i=1,nval
c (2) in ycal1(i,j2),xcal1(i,j2), ditto for openings that PRECEDE spec gaps
c Modif 09/13/95 09:11pm by adding parameters j1,j2.
c (1) E(To|L<Tc<U)=tres + phiF*{(F)CR(U-tres)-(F)CR(L-tres)}*QFA*exp(QAA*tres)*
c     [-d(A)R(s)/ds]s=0*QAF*exp(QFF*tres)*uF/den
c 	den=FTc(U)-FTc(L)
c (2) = tres + phiA*[-d(A)R(s)/ds]s=0*QAF*exp(QFF*tres)*{(F)CR(U-tres)
c	-(F)CR(L-tres)}*QFA*exp(QAA*tres)*uA/den
c
c Modif 09/27/01 05:05pm for 100 state/10 ligand. Common/phiblk still used
c but QEXPQA, QEXPQF must be arguments now
c
c Notation in paper is a bit different from here (F)CR(t) is (F)K(t) in
c paper, and XAF(t) here is the asymptotic (A)R(i)*QAF*exp(QFF*tres)
c where (a)R(i) is defined in eq 2.29 in paper.
c
c NB XAF(m) and XFA(m) are the matrices denoted (A)R(m)*QAF*EXPQF and
c (F)R(m)*QFA*EXPQA where (A)R(m) is defined in CHS96 eq.2.29
c and EXPQA=exp(QAA*tres)
c
c  Note that for large (shut) time the expression for conditional mean open
c  time tends to 0/0, but empirically the graph gets flat.  This is fixed
c  as in MOPADJC (continuous version).  For ranges, the case is a bit more
c  complicated. For mean open time. a common scalar cancels once all but the
c  slowest exponential(tau-slowest=taus say) has decayed to zero, but here
c  the thing that cancels is
c	 [exp(-ulo/taus)-exp(-uhi/taus)].
c  For the mean X value in the range, this same factor occurs in denominator
c  but in numerator get
c	(taus+ulo)*exp(-ulo/taus)-(taus+uhi)*exp(-uhi/taus)
c  so they don't cancel exactly and result must be multiplied by their ratio
c   which itself is 0/0 in the limit, but has limiting value = taus+ulo.
c  See 'conditional mean.doc' for full algebra
c
c   05/13/02 08:28pm x>-708 allows values that are on verge of running out of
c	precision, so now set good=false as soon as kF-1 exponentials are zero

c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 QD(100,100)
c=	common/qexpblk/QEXPQA(10,10),QEXPQF(10,10)
	real*8 QEXPQA(irt,irt),QEXPQF(irt,irt)
	real*8 Z00F(irt,irt,km),Z10F(irt,irt,km),Z11F(irt,irt,km)
	real*8 XFA(irt,irt,irt)
	real*4 XCAL1(ndc1,ndimc),YCAL1(ndc1,ndimc)
	real*4 den1(20)
	real*4 ylo(20),yhi(20)
c The rest are local
	real*8 Q1(100,100),Q2(100,100)
!	real*8 QAF(20,100),QFA(100,20),QAA(20,20),QFF(100,100)
!	real*8 EXPQA(20,20),EXPQF(100,100)
	real*8 row1(1,100)
	real*8 ucol(100,1)
	real*4 t1
c DARS IS kAm x kAm in darsds subroutine so must allocate here DARS(100,100)
	allocatable::DARS
	real*8 DARS(:,:)
	common/dex/zero		!in common with dexp1
	real*8 EXPQF(100,100)
	logical discprt,bad
	common/dp/discprt
c
	common/phiblk/phiF(1,100),phiA(1,100)
	common/root/rootA(100),rootF(100)	!needed for limiting form of X mean
	logical good
	common/lim/good,goodx		!in FCRQT
	logical goodx
		!in FMRQT
c
	
c
	one=1.d0
	good=.true.
	goodx=.true.
	do i=1,km
	   ucol(i,1)=one
	enddo
	ALLOCATE(DARS(kAm,kAm))
c
	do n=1,nval
	   tlo=dble(ylo(n))*1.d-3       !low time (sec)
	   thi=dble(yhi(n))*1.d-3       !hi  time (sec)
c	   den=FTCt(thi,tres,k,kA,kF,ucol,Z00F,Z10F,Z11F,XFA,kAm,kFm,km)
c     &     - FTCt(tlo,tres,k,kA,kF,ucol,Z00F,Z10F,Z11F,XFA,kAm,kFm,km)
	   dlo=FTCt(tlo,tres,k,kA,kF,ucol,Z00F,Z10F,Z11F,XFA,kAm,kFm,km)
	   dhi=FTCt(thi,tres,k,kA,kF,ucol,Z00F,Z10F,Z11F,XFA,kAm,kFm,km)
	   den=dhi-dlo		!call with yhi 2nd, as most likely to be bad
	   den1(n)=sngl(den)		!for output
c The variable 'good' is in common with FCRQT (called by FTCt) and is set true
c for large t, when all exponentials but the slowest have decayed to zero in
c the asymptotic HJC distribution. Under these conditions den (and numerator,
c below) will be near zero, and limiting form must be used. Since limiting
c form is independent of t, all the rest of the values for ycal can be
c set to it.  Because of the way prog has been written, with subtraction for
c num and den done right at the end, it is not convenient to use FTCT and FCRQT
c to get the limiting form, and as soon as good=False, call special subroutine
c to find limit (c.f.mopadjc, where limit can be found as special version of
c regular code).
	   if(.not.good.or.den.lt.1.d-200) then
c		Use only slowest m=kF
		do i=1,kF
		   do j=1,kA
			Q1(i,j)=XFA(i,j,kF)
		   enddo
		enddo
c		recalculate denominator, without the cancelled bit
	      call MATSCL2d(phiF,Q1,ucol,kF,kA,den,km,km,km,km)
		den1(n)=0.0		!for output when den->0
		good=.false.	!in case
		goto 9	!then all same, using modified Q1
	   endif

51	   call FCRQt(tlo-tres,tres,k,kA,kF,Q1,
     &      Z00F,Z10F,Z11F,XFA,kAm,kFm,km)
	   call FCRQt(thi-tres,tres,k,kA,kF,Q2,
     &      Z00F,Z10F,Z11F,XFA,kAm,kFm,km)
c Subtract them
	   do i=1,kF
		do j=1,kA
		   Q1(i,j)=Q2(i,j) - Q1(i,j)
		enddo
	   enddo
c Now Q1={(F)CR(U-tres)-(F)CR(L-tres)}*QFA*exp(QAA*tres)
9	   continue		!jump here with modif Q1 for limiting case
c for preceding gap:
         call MATMUL(phiF,Q1,row1,1,kF,kA,one,1,km,km,km,1,km) !phiF*Q1 in row1
c Get [-dAR(s)/ds] at s=0  (kA*kA)
	   call dARSdS(QD,tres,DARS,kA,kF,kAm,kFm,km,EXPQF)
     
         call MATMUL(row1,DARS,row1,1,kA,kA,one,1,km,kAm,kAm,1,km)
c Already have QFA*exp(QAA*tres) in QEXPQA (kF*kA)
c Already have QAF*exp(QFF*tres) in QEXPQF (kA*kF)
         call MATMUL(row1,QEXPQF,row1,1,kA,kF,one,1,km,kAm,kFm,1,km)
         call VECMUL(row1,ucol,scal,kF,km,km)	!row*col=scalar
c
c For large times may get scal=0 and den=0 so check before dividing
c (should find the asymptotic form for large t, but for now just omit
c  current, and any higher, ranges)
	   bad=.false.
	   if(dabs(den).lt.dabs(scal)/1.d230) bad=.true.
	   if(dabs(den).lt.1.d-200) bad=.true.
	   if(.not.bad) then
		ycal1(n,j1)=1000.*sngl(tres + scal/den)		!in msec
	   else
	!	print 1,nval,nval-1
		if(discprt) write(7,1) nval,nval-1
1		format(' Numerical problem in calculating range ',i4,/,
     &	' Number of ranges reduced to ',i4,/)
		nval=n-1
	!	pause
		goto 99
	   endif
c
c for following gap:
         call MATMUL(phiA,DARS,row1,1,kA,kA,one,1,km,kAm,kAm,1,km)
c Already have QAF*exp(QFF*tres) in QEXPQF (kA*kF)
         call MATMUL(row1,QEXPQF,row1,1,kA,kF,one,1,km,kAm,kFm,1,km)
         call MATMUL(row1,Q1,row1,1,kF,kA,one,1,km,km,km,1,km)
         call VECMUL(row1,ucol,scal,kA,km,km)	!postmult by uA
	   bad=.false.
	   if(dabs(den).lt.dabs(scal)/1.d230) bad=.true.
	   if(dabs(den).lt.1.d-200) bad=.true.
	   if(.not.bad) then
		ycal1(n,j2)=1000.*sngl(tres + scal/den)		!in msec
	   else
	!	print 1,nval,nval-1
		if(discprt) write(7,1) nval,nval-1
c1		format(' Numerical problem in calculating range ',i4,/,
c     &	' Number of ranges reduced to ',i4,/)
		nval=n-1
		goto 99
	   endif
c Calculation of Y finished
c
c Now calculate MEAN SHUT TIME, given that shut time is between tlo,thi
c to use on X axis for plot of corresp Y values calc above
c Get FMRQ=(F)MR(t-tres)*QFA*exp(QAA*tres) (kF x kA)
c NB den is same as for calulation of Y value.
c 05/14/02 11:47am  Add check that calculation of mean X value is not
c running into 0/0 case -if so use limiting form
c It doesn't matter if thi goves zero exponential so call tlo second, so
c goodx (in common) indicates whether tlo gives zero exponentials, except
c last. In this case mean of values>tlo is essentially the standard formula
c for mean of values > tlo for exponential variable with tau=tau-slow
c  Call with yhi 2nd, as most likely to be bad
	   call FMRQt(tlo-tres,tres,k,kA,kF,Q1,
     &    Z00F,Z10F,Z11F,XFA,kFm,kAm,km)
	   call FMRQt(thi-tres,tres,k,kA,kF,Q2,
     &    Z00F,Z10F,Z11F,XFA,kFm,kAm,km)
	   if(.not.goodx) then
		taus=-1.d0/rootF(kF)		!in 1/sec -slowest
c		xcal1(n,j1)=1000.*sngl(taus + tlo)		!standard for exp var > tlo
c     more accurate (essentially same in practice) version
		e1=dexp1(-(thi-tlo)/taus)
		temp=(taus+tlo-tres-(taus+thi-tres)*e1)/(1.d0-e1)
		xcal1(n,j1)=1000.*sngl(tres+temp)
		xcal1(n,j2)=xcal1(n,j1)
		goto 91	!next range
	   endif
c
c Subtract them
	   do i=1,kF
	      do j=1,kA
		   Q1(i,j)=Q2(i,j) - Q1(i,j)
		enddo
	   enddo
         call MATMUL(phiF,Q1,row1,1,kF,kA,one,1,km,km,km,1,km)  !phiF*Q1 in row1
         call VECMUL(row1,ucol,scal,kA,km,km)	!row*col=scalar
c For large times may get scal=0 and den=0 so check before dividing
c (should find the asymptotic form for large t, but for now just omit
c  current, and any higher, ranges)
c This bit should never be done now -skipped by jump to 91 when bad
	   bad=.false.
	   if(dabs(den).lt.dabs(scal)/1.d230) bad=.true.
	   if(dabs(den).lt.1.d-200) bad=.true.
	   if(.not.bad) then
		xcal1(n,j1)=1000.*sngl(tres + scal/den)	!in msec
		xcal1(n,j2)=xcal1(n,j1)
	   else
	!	print 1,nval,nval-1
		if(discprt) write(7,1) nval,nval-1
c1		format(' Numerical problem in calculating range ',i4,/,
c     &	' Number of ranges reduced to ',i4,/)
		nval=n-1
	!	pause
		goto 99
	   endif
91	   continue
c
	enddo		!end of n=1,nval loop
c
99	DEALLOCATE(DARS)
	RETURN
	end

