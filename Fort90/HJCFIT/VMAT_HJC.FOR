	subroutine VMAT_HJC(theta,delt,finfo,covar,unit,det,badpar,
     & kgood,elmax,kfit,tint,ampl,iprops,nd1,nd2,
     & 	Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & 	XAF,XFA,QexpQA,QexpQF,
     & 	alpha2,beta2,aloglik,kab,
     & 	kAm,kFm,km)
c
c Version of VMATD2 for use in HJCFIT
c Special version needed to pass parameters (from kfit onwards) for
c	 HJCLIK(kfit,THETA,tint,ampl,iprops,nd1,nd2,km)
c Also add ktd=dimension of arrays in here (previoulsy fixed at 20 or 50)
c NB this version is real*8 throughout and deals ONLY with fitted
c param (see hmat_hjc)
c
c 11/13/97 03:56pm Modif throughout for 50 paramters! now calls minvd2
c    theta(50),theta(50,5), jfix(50), titlep(5)
c VMATD2 is version of VMATD which, like VMAT2, has arrays of dimension 20
c rather than 10, but which las likelihood in call (ELIK), rather than
c SSD (as in VMAT2). Calls GMPROD (rather than GMPRDD) accordingly.
c 06/06/89 08:13pm Lahey version
c
C Version for EKDIST
C D COLQUHOUN JAN 1977
C***14-JAN-83. VERSION WITH SCALE FACTORS FOR PARAMETERS AS
C SEEMS THAT THERE MAY BE NUMERICAL PROBLEMS IN INVERTING LARGE MATRICES
C WITH ELEMENTS OF WIDELY VARYING SIZE
C***6-NOV-82. VERSION FOR USE WITH FUNCTION ELIK(K,THETA) THAT
C CALCULATES MINUS LIKELIHOOD RATHER THAN SUM OF SQUARES
C***9-OCT-82. VERSION WITH PARAMETER JFIX(KMAX) WITH ELEMENTS SET
C  TO ZERO EXCEPT IF THETA(I) IS FIXED JFIX(I)=1. NFIX=NUMBER OF
C  FIXED VALUES. K=TOTAL NO OF PARAM (INC FIXED).
C  N.B. FINFO ETC ARE RETURNED AS KFIT*KFIT MATRICES (KFIT=K-NFIX) WITH
C  ROW AND COL FOR FIXED PARAM OMITTED
C VERSION WITH OPTIONAL AUTOMATIC CALC OF STEP SIZE FOR DERIVATIVES
C DOUBLE PRECISION VERSION FOR MATRIX OPS
C FINFO,COVAR,UNIT AND DET MUST BE DOUBLE PREC IN CALLING PROGRAM
C ARRAYD,GMPRDD AND MINVD ARE DOUBLE PRECISION VERSIONS OF THE
C SSP SUBROUTINES ARRAY,GMPRD AND MINV
C
	real*8 FINFO(200,200),COVAR(200,200),UNIT(200,200)
	real*8 det,delt,one
c	real*4 DELTA(50),scal(50)
c	real*4 THETA(ktd),TH1(50),TH2(50),TH3(50),TH4(50)
	real*8 THETA(200)
c	real*8 TH1(50),TH2(50),TH3(50),TH4(50)
c	real*8 DELTA(50),scal(50)
	allocatable::TH1,TH2,TH3,TH4,DELTA,scal
	real*8 TH1(:),TH2(:),TH3(:),TH4(:)
	real*8 DELTA(:),scal(:)
	real*8 elmax,elcrit,el,den,sfac,p,f1,f2,f3,f4
	real*8 hjclik	!function now real*8
c=	integer LWORK(50),MWORK(50)
	LOGICAL DEBUG,caplock
	logical badpar(200),bad
	logical badlik
	common/bad/badlik		!for vmat_hjc/hjclik
	logical debprt,dprt
c	real*8 det1			!not needed for matinv2
c	COMMON/determ/det1 	!for matinv
	common/deb1/debprt
	logical discprt
	common/dp/discprt
c
	real*4 tint(nd1,nd2),ampl(nd1,nd2)
	integer*1 iprops(nd1,nd2)
c Parameters in call only for transmission to HJCLIK
	real*8 Z00A(kAm,kFm,km),Z10A(kAm,kFm,km),Z11A(kAm,kFm,km)
	real*8 Z00F(kFm,kAm,km),Z10F(kFm,kAm,km),Z11F(kFm,kAm,km)
	real*8 QEXPQA(kFm,kAM),QEXPQF(kAm,kFm)
	real*8 XAF(kAm,kFm,kAm),XFA(kFm,kAm,kFm)
  	real*8 alpha2(kab),beta2(kab),aloglik(kab)
c
c For HJCLIK
	logical logfit
	common/logf/logfit
c
	debug()=caplock()
C
C  THETA IS THE ARRAY OF K MAXIMUM LIKELIHOOD ESTIMATES.
C  FRACT=FRACTION BY WHICH EACH PARAMETER CHANGED TO FIND DERIVATIVES
C   IF POSITIVE. IF NEGATIVE, VALUE IS IGNORED AND A VALUE CHOSEN BY
C   STARTING WITH A LOW VALUE AND INCREASING IT UNTIL S IS INCREASED
C   BY AT LEAST 1 PERCENT WHEN ITH PARAMETER INCREASED BY FRACT*THETA(I)
C  FINFO=INFORMATION (HESSIAN) MATRIX (OUTPUT)
C  COVAR=VARIANCE-COVARIANCE MATRIX (OUTPUT)
C  UNIT=INFO*COVAR IS CALC AS CHECK ON INVERSION,AS IS DET(INFO).
C THE SUBROUTINE ELIK CALCULATES THE MINUS-LOG-LIKELIHOOD-THE FUNCTION
C PREVIOUSLY
C MINIMIZED,IN FORM ELIK(K,TH) WHERE TH IS AN ARRAY OF PARAMETER VALUES.
C ARGUMENT SUBSTITUTED FOR ELIK MUST BE DECLARED EXTERNAL IN CALLING PROG.
C
C GET SCALE FACTORS FOR THETA, SO ALL VALUES IN RANGE 1-10 IE DIVIDE
C ACTUAL VALUES BY 10**(SCAL) WHERE SCAL=FLOAT(IFIX(ALOG10(THETA)))
C THIS DEFINES SCAL(I),I=1,KFIT FOR UNFIXED PARAM AS NEEDED FOR FINFO
C
C
	logfit=.false.	!so hjclik expects rates (not log(rates)) in theta()
c
	one=1.d0
	dprt=discprt.and.debprt
	elcrit=elmax - delt
	ktd=200
	ALLOCATE(TH1(ktd),TH2(ktd),TH3(ktd),TH4(ktd),DELTA(ktd),scal(ktd))
c
      do i=1,kfit
	   print 20,i,theta(i)
	   if(dprt) write(8,20) i,theta(i)
20	   format(/,
     & '  Seeking increment in parameter ',i3,' = ',g13.6,
     & ' for calculation of Hessian',/,
     & '    percent change      reduction in',/,
     & '    in param            log(likelihood)')
	   badpar(i)=.false.
	   do m=1,kfit
	    	th1(m)=theta(m)		!RESET TH1 FOR EACH PARAM
	   enddo
	   delta(i)=0.01d0*theta(i)	!START WITH FRACT=0.01
	   th1(i)=theta(i)+delta(i)	!TRY START VALUE
c	   el=-HJCLIK(kfit,TH1,tint,ampl,iprops,nd1,nd2,km)
	   el=-HJCLIK(kfit,TH1,tint,ampl,iprops,nd1,nd2,
     & 	Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & 	XAF,XFA,QexpQA,QexpQF,
     & 	alpha2,beta2,aloglik,kab,
     & 	kAm,kFm,km)
	   if(el.lt.elcrit) then   	!decrease delta
17          delta(i)=0.7*delta(i)	!DECREASE DELTA UNTIL L>ELCRIT
		th1(i)=theta(i)+delta(i)
		el=-HJCLIK(kfit,TH1,tint,ampl,iprops,nd1,nd2,
     & 		Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & 		XAF,XFA,QexpQA,QexpQF,
     & 		alpha2,beta2,aloglik,kab,
     & 		kAm,kFm,km)
		p=100.d0*(1.d0-th1(i)/theta(i))
		print 21,p, elmax-el
		if(dprt) write(8,21) p,elmax-el
21		format(2x,g13.6,6x,g13.6)
		if(el.gt.elcrit) goto 9	!OK NOW: NEXT PARAMETER
		if(badlik) then
		   badpar(i)=.true.
		   goto 9				!next parameter
		endif
		goto 17		!REDUCE DELTA AGAIN
	   else
C NEXT BIT DONE IF L STILL >LCRIT I.E. DELTA TOO SMALL TO REDUCE
C L BELOW LCRIT SO INCREASE DELTA
11	      delta(i)=1.4d0*delta(i)		!INCREASE DELTA UNTIL L<ELCRIT
		th1(i)=theta(i)+delta(i)
		el=-HJCLIK(kfit,TH1,tint,ampl,iprops,nd1,nd2,
     & 		Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & 		XAF,XFA,QexpQA,QexpQF,
     & 		alpha2,beta2,aloglik,kab,
     & 		kAm,kFm,km)
		p=100.d0*(1.d0-th1(i)/theta(i))
		print 21,p, elmax-el
		if(dprt) write(8,21) p,elmax-el
c21		format(2x,g13.6,6x,g13.6)
		if(el.lt.elcrit) goto 9		!O.K. NOW:NEXT PARAMETER
		if(badlik.or.(delta(i).gt.theta(i))) then
		   badpar(i)=.true.
		   goto 9				!next parameter
		endif
		goto 11				!INCREASE DELTA MORE
	   endif
9        continue
	enddo
c
	nbad=0
	do i=1,kfit
	   if(badpar(i)) nbad=nbad+1
	enddo
c
12	continue
c	print 18,(delta(i),i=1,kfit)
c18	format(' delta(i)= ',/,5G13.6,/,5G13.6)
	print 181,delt,(delta(i)/theta(i),i=1,kfit)
	if(discprt) write(8,181) delt,(delta(i)/theta(i),i=1,kfit)
181	format(/,
     &' Step size for each parameter (fraction of ML value) needed ',/,
     &' to achieve change in log(lik) of at least ',f6.3,' units',/,
     &     20(5G13.6,/),/)
C DELTA NOW SET FOR EACH PARAMETER.
C
C NOW ESTIMATE HESSIAN
	ii=0	!INDEX FOR FINFO ETC (=1,2,..,KFIT)
	do i=1,kfit
	   if(badpar(i)) goto 2		!omit row of finfo if parameter bad
	   print 23,i
23	   format('  calculating row ',i3,' of Hessian . . .')
	   ii=ii+1
	   jj=0
	   do j=1,i
	      if(badpar(j)) goto 3		!omit col of finfo if parameter bad
		jj=jj+1
C RESET TH1...TH4 EVERY CYCLE
	      do m=1,kfit
	         TH1(M)=THETA(M)
      	   TH2(M)=THETA(M)
	         TH3(M)=THETA(M)
		   TH4(M)=THETA(M)
		enddo
C  ALTER APPROPRIATE ELEMENTS OF THETA TO FORM DERIVATIVES
	      TH1(I)=TH1(I)+DELTA(I)
	      TH3(I)=TH3(I)-DELTA(I)
      	if(i.eq.j) then
C  NOW DO DIAGONAL ELEMENTS
	         den=delta(i)*delta(i)
		   bad=.false.
		   f1=HJCLIK(kfit,TH1,tint,ampl,iprops,nd1,nd2,
     & 		Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & 		XAF,XFA,QexpQA,QexpQF,
     & 		alpha2,beta2,aloglik,kab,
     & 		kAm,kFm,km)
		   if(badlik) bad=.true.
		   f2=HJCLIK(kfit,theta,tint,ampl,iprops,nd1,nd2,
     & 		Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & 		XAF,XFA,QexpQA,QexpQF,
     & 		alpha2,beta2,aloglik,kab,
     & 		kAm,kFm,km)
		   if(badlik) bad=.true.
		   f3=HJCLIK(kfit,TH3,tint,ampl,iprops,nd1,nd2,
     & 		Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & 		XAF,XFA,QexpQA,QexpQF,
     & 		alpha2,beta2,aloglik,kab,
     & 		kAm,kFm,km)
		   if(badlik) bad=.true.
      	   finfo(ii,jj)=(f1 - 2.0d0*f2 + f3)/den
		   if(bad) then
			if(.not.badpar(i)) nbad=nbad+1
			badpar(i)=.true.
			print 22,i,j
			if(discprt) write(8,22) i,j
22			format(
     &		' Calculation of Hessian failed at i, j = ',2i3)
		   endif
c
		else
C   NOW THE OFF DIAGONALS
		   TH1(J)=TH1(J)+DELTA(J)
	         TH2(I)=TH2(I)+DELTA(I)
      	   TH2(J)=TH2(J)-DELTA(J)
	         TH3(J)=TH3(J)+DELTA(J)
      	   TH4(I)=TH4(I)-DELTA(I)
	         TH4(J)=TH4(J)-DELTA(J)
      	   DEN=4.0d0*DELTA(I)*DELTA(J)
		   bad=.false.
		   f1=HJCLIK(kfit,TH1,tint,ampl,iprops,nd1,nd2,
     & 		Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & 		XAF,XFA,QexpQA,QexpQF,
     & 		alpha2,beta2,aloglik,kab,
     & 		kAm,kFm,km)
		   if(badlik) bad=.true.
		   f2=HJCLIK(kfit,TH2,tint,ampl,iprops,nd1,nd2,
     & 		Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & 		XAF,XFA,QexpQA,QexpQF,
     & 		alpha2,beta2,aloglik,kab,
     & 		kAm,kFm,km)
		   if(badlik) bad=.true.
		   f3=HJCLIK(kfit,TH3,tint,ampl,iprops,nd1,nd2,
     & 		Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & 		XAF,XFA,QexpQA,QexpQF,
     & 		alpha2,beta2,aloglik,kab,
     & 		kAm,kFm,km)
		   if(badlik) bad=.true.
		   f4=HJCLIK(kfit,TH4,tint,ampl,iprops,nd1,nd2,
     & 		Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & 		XAF,XFA,QexpQA,QexpQF,
     & 		alpha2,beta2,aloglik,kab,
     & 		kAm,kFm,km)
		   if(badlik) bad=.true.
      	   finfo(ii,jj)=(f1 - f2 -f3 + f4)/den
      	   FINFO(JJ,II)=FINFO(II,JJ)
		   if(bad) then
			if(.not.badpar(i)) nbad=nbad+1
			badpar(i)=.true.
			if(.not.badpar(j)) nbad=nbad+1
			badpar(j)=.true.
			print 22,i,j
			if(discprt) write(8,22) i,j
c22			format(
c     &		' Calculation of Hessian failed at i, j = ',2i3)
		   endif
		endif
3		continue
	   enddo
2	   continue
	enddo
c
c Print which params are bad
	kgood=kfit-nbad
	if(nbad.gt.0) then
	   print 10,nbad,kfit
	   if(discprt) write(8,10) nbad,kfit
10	   format(/,
     & ' Attempt to find 2nd derivative failed for ',i3,' out of',i3,/,
     & ' free parameters, so bad params removed from covariance matrix',
     & /,'  free param #    new number')
	   i1=0
	   do i=1,kfit
		if(badpar(i)) then
		   print 24,i
		   if(discprt) write(8,24) i
24		   format(1x,i6,'  bad')
		else
		   i1=i1+1
		   print 25,i,i1
		   if(discprt) write(8,25) i,i1
25		   format(1x,i6,'  OK     ',i6)
		endif
	   enddo
	endif
c
C Now scale the info matrix before inversion
c Define scale factors to exclude bad parameters
	i1=0
	do i=1,kfit
	   if(.not.badpar(i)) then
		i1=i1+1
		scal(i1)=10.0**dfloat(int4(dlog10(theta(i))))
	   endif
	enddo
	if(i1.ne.kgood) then
	   print 13,i1,kgood
	   if(discprt) write(8,13) i1,kgood
13	   format(
     &   ' Error in vmat_hjc; i1, kgood = ',2i5)
	   pause
	endif
C
	do i=1,kgood
	   do j=1,kgood
		finfo(i,j)=finfo(i,j)*scal(i)*scal(j)
	   enddo
	enddo
C
c Now use matinv and matmul
c	call MATINV(FINFO,kgood,ktd,COVAR,ktd)
	call MATINV2(FINFO,kgood,ktd,COVAR,ktd,.true.,det,ndscale)
	if(ndscale.ne.0) then
	   call BELL(1)
	   print 1,ndscale
1	   format(' Determinant overflow in VMAT_HJC: ndscale = ',i10)
	   pause
	endif
c	det=det1	!from common, for return to hmat_hjc
	call MATMUL(FINFO,COVAR,UNIT,kgood,kgood,kgood,one,
     &	ktd,ktd,ktd,ktd,ktd,ktd)
C
C RESCALE FINFO AND COVAR TO ORIGINAL UNITS
	if(debug()) then
	   print 700,(scal(i),i=1,kgood)
700	   format(10g8.1,/,10g8.1)
	endif
	do i=1,kgood
	   do j=1,kgood
		sfac=scal(i)*scal(j)
		FINFO(I,J)=FINFO(I,J)/SFAC
		COVAR(I,J)=COVAR(I,J)*SFAC
	   enddo
	enddo
c
	DEALLOCATE(TH1,TH2,TH3,TH4,DELTA,scal)
	RETURN
	end


