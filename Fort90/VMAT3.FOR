	subroutine VMAT3(theta,Kmax,fract,finfo,covar,unit,SSD,
     & smin,nfix,jfix,SCAL,
     & ndth,ndc,Xobs,yobs,w,nj,juse,setx,niobs,njset)
c
c VMAT3 is version of VMAT2 with 2 extra params -ndth=dimension of
c theta() and jfix(), and ndc=dimension of finfo,covar,unit, and
c params for SSD that are needed in CVFIT
c
c 12/29/96 01:14pm -all printout removed
c
c	subroutine VMAT2(THETA,K,FRACT,FINFO,COVAR,UNIT,DET,ELIK,
c     & ELMAX,NFIX,JFIX)
c
c 12/28/89 03:16pm Lahey version
c General purpose calc of Hessian with up to 20 param
c Returns SCALED values (see HESMAT)
c
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
C ARRAYD,GMPROD AND MINVD ARE DOUBLE PRECISION VERSIONS OF THE
C SSP SUBROUTINES ARRAY,GMPRD AND MINV
c ###NB these are still for 10x10 matrices, but work anyway??
C
	dimension  FINFO(ndc,ndc),COVAR(ndc,ndc),UNIT(ndc,ndc),scal(ndc)
	REAL*8 FINFO,COVAR,UNIT,one
	real*4 theta(ndth)
	integer jfix(ndth)
c end of params
	ALLOCATABLE::DELTA,TH1,TH2,TH3,TH4
	real*4 DELTA(:),TH1(:),TH2(:),TH3(:),TH4(:)
	LOGICAL DEBUG,caplock
c Params to be passed to SSDCV and Simplex
	real*4 xobs(niobs,njset),yobs(niobs,njset),w(niobs,njset)
	integer juse(njset),nj(njset)
	real setx(njset)
c	external ELIK
	external SSD
c
	debug()=caplock()
c NB SSDCV gives -2*log likelihood; in version for EKDIST etc use
c ELIK(=explik etc)=-log likelihood, SSD=2*ELIK, or ELIK=0.5*SSD
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
	ALLOCATE(DELTA(kmax),TH1(kmax),TH2(kmax),TH3(kmax),TH4(kmax))
	KFIT=Kmax-NFIX
	elmax=-smin/2.
C
C GET SCALE FACTORS FOR THETA, SO ALL VALUES IN RANGE 1-10 IE DIVIDE
C ACTUAL VALUES BY 10**(SCAL) WHERE SCAL=FLOAT(IFIX(ALOG10(THETA)))
C THIS DEFINES SCAL(I),I=1,KFIT FOR UNFIXED PARAM AS NEEDED FOR FINFO
c
	I1=1
	DO 100 I=1,Kmax
	IF(JFIX(I).EQ.1) GOTO 100
	SCAL(I1)=10.0**FLOAT(IFIX(ALOG10(abs(THETA(I)))))
	I1=I1+1
100	CONTINUE
C
C NEXT CALC FINFO WITH UNSCALED VALUES AS BEFORE. THEN SCALE FINFO
C BEFORE INVERSION.
C
      IF(FRACT.LT.0.) GOTO 8
      DO 1 M=1,Kmax
1     DELTA(M)=FRACT*THETA(M)
      GOTO 12
8	CONTINUE
C -ELIK=LOG LIKELIHOOD
c	IF(DEBUG()) print 82,(THETA(I),I=1,Kmax)
c82	FORMAT( ' THETA= ',5G13.6,/,5G13.6)
c	IF(debug()) print 83,-0.5*SSD(Kmax,THETA),(JFIX(I),I=1,Kmax)
c83	FORMAT (' Lmax: calc in VMATD= ',G13.6,/,
c     & ' JFIX= ',20I3)
C	X=0.005		!DEFAULT
C	X=0.5		!DEFAULT
	ELCRIT=ELMAX-0.005*ABS(ELMAX)	!DEFAULT
C	IF(.NOT.debug()) GOTO 13
c	x=-1.
	if(fract.lt.0.) goto 13		!use default ELCRIT
c	print 81,ELMAX
c81	FORMAT( ' Lmax (in VMATD)= ',G13.6)
c	print 14
c14	FORMAT( ' Lcrit=Lmax-X:   X= ')
c	read 15,X
c15	FORMAT(G13.6)
c	ELCRIT=ELMAX-X
13	CONTINUE
c
c Make max delta=1.e15 (because delta^2 is used below)
c If theta(i) altered by constraint, delta() may not actually increase
c so allow maximum of 10 attempts to increase it
	ndmax=10
	deltmax=1.e15
c
      DO 9 I=1,Kmax
	IF(JFIX(I).EQ.1) GOTO 9
c	print 91,I
cc91	FORMAT(' I= ',I2)
      DO 10 M=1,Kmax
10    TH1(M)=THETA(M)		!RESET TH1 FOR EACH PARAM
      DELTA(I)=0.01*THETA(I)	!START WITH FRACT=0.01
      TH1(I)=THETA(I)+DELTA(I)	!TRY START VALUE
	EL=-0.5*SSD(Kmax,TH1,
     & Xobs,yobs,w,nj,juse,setx,niobs,njset)		! +LOG LIKELIHOOD
c NB if there are constraints on theta(i) then it may be altered
c by call to SSD, so recalculate delta here, in case th1(i) has been altered!
	delta(i)=th1(i)-theta(i)
c	print 93,EL
c93	FORMAT( ' EL= ',G13.6)
      IF (EL.LT.ELCRIT) GOTO 16	!SECTION TO DEC DELTA
C NEXT BIT DONE IF L STILL >LCRIT I.E. DELTA TOO SMALL TO REDUCE
C L BELOW LCRIT SO INCREASE DELTA
	nd=0
11	continue
	nd=nd+1	!number of attempts
	DELTA(I)=2.*DELTA(I)	!INCREASE DELTA UNTIL L<ELCRIT
	TH1(I)=THETA(I)+DELTA(I)
	EL=-0.5*SSD(Kmax,TH1,
     & Xobs,yobs,w,nj,juse,setx,niobs,njset)		! +LOG LIKELIHOOD
c NB if there are constraints on theta(i) then it may be altered
c by call to SSD, so recalculate delta here, in case th1(i) has been altered!
	delta(i)=th1(i)-theta(i)
c	print 94,DELTA(I),EL
c94	FORMAT( ' DELTA,EL= ',2G13.6)
	IF (EL.LT.ELCRIT) GOTO 9	!O.K. NOW:NEXT PARAMETER
	if(abs(delta(i)).gt.deltmax) goto 9	!can't increase it further
	if(nd.gt.ndmax) goto 9
	GOTO 11		!INCREASE DELTA MORE
c
16	CONTINUE
	nd=0
17    continue
	nd=nd+1	!number of attempts
	DELTA(I)=0.5*DELTA(I)	!DECREASE DELTA UNTIL L>ELCRIT
	TH1(I)=THETA(I)+DELTA(I)
	EL=-0.5*SSD(Kmax,TH1,
     & Xobs,yobs,w,nj,juse,setx,niobs,njset)		! +LOG LIKELIHOOD
c NB if there are constraints on theta(i) then it may be altered
c by call to SSD, so recalculate delta here, in case th1(i) has been altered!
	delta(i)=th1(i)-theta(i)
c	print 94,DELTA(I),EL
	IF (EL.GT.ELCRIT) GOTO 9	!OK NOW: NEXT PARAMETER
	if(nd.gt.ndmax) goto 9
	GOTO 17		!REDUCE DELTA AGAIN
c
9     CONTINUE
c
12	continue
c	do 182 i=1,kmax
c	   if(jfix(i).eq.1) goto 182
c	   f=0.
c	   if(abs(theta(i)).gt.1.e-21) f=delta(i)/theta(i)
c	   print 18,i,delta(i),f
c18	   format(' i = ',i4,' delta(i), fract(i) = ',2G13.6)
c182	continue
C DELTA NOW SET FOR EACH PARAMETER.
C
C NOW ESTIMATE HESSIAN
	II=0	!INDEX FOR FINFO ETC (=1,2,..,KFIT)
	DO 2 I=1,Kmax
	IF(JFIX(I).EQ.1) GOTO 2		!OMIT ROW OF FINFO IF PARAM FIXED
	II=II+1
	JJ=0
C      DO 3 J=I,Kmax
	DO 3 J=1,I
	IF(JFIX(J).EQ.1) GOTO 3		!OMIT COLUMN
	JJ=JJ+1
C RESET TH1...TH4 EVERY CYCLE
      DO 4 M=1,Kmax
      TH1(M)=THETA(M)
      TH2(M)=THETA(M)
      TH3(M)=THETA(M)
    4 TH4(M)=THETA(M)
C  ALTER APPROPRIATE ELEMENTS OF THETA TO FORM DERIVATIVES
      TH1(I)=TH1(I)+DELTA(I)
      TH3(I)=TH3(I)-DELTA(I)
      IF(I.NE.J) GOTO 5
C  NOW DO DIAGONAL ELEMENTS
      DEN=DELTA(I)**2
      FINFO(II,JJ)=
     & 0.5*(SSD(Kmax,TH1,Xobs,yobs,w,nj,juse,setx,niobs,njset)-
     & 2.0*SSD(Kmax,THETA,Xobs,yobs,w,nj,juse,setx,niobs,njset)+
     & SSD(Kmax,TH3,Xobs,yobs,w,nj,juse,setx,niobs,njset))/DEN
      GOTO 6
C   NOW THE OFF DIAGONALS
    5 TH1(J)=TH1(J)+DELTA(J)
      TH2(I)=TH2(I)+DELTA(I)
      TH2(J)=TH2(J)-DELTA(J)
      TH3(J)=TH3(J)+DELTA(J)
      TH4(I)=TH4(I)-DELTA(I)
      TH4(J)=TH4(J)-DELTA(J)
      DEN=4.0*DELTA(I)*DELTA(J)
      FINFO(II,JJ)=
     & 0.5*(SSD(Kmax,TH1,Xobs,yobs,w,nj,juse,setx,niobs,njset)-
     & SSD(Kmax,TH2,Xobs,yobs,w,nj,juse,setx,niobs,njset)-
     & SSD(Kmax,TH3,Xobs,yobs,w,nj,juse,setx,niobs,njset)+
     & SSD(Kmax,TH4,Xobs,yobs,w,nj,juse,setx,niobs,njset))/DEN
      FINFO(JJ,II)=FINFO(II,JJ)
    6 CONTINUE
    3 CONTINUE
    2 CONTINUE
C
C NOW SCALE INFO
	DO 101 I=1,KFIT
	DO 101 J=1,KFIT
101	FINFO(I,J)=FINFO(I,J)*SCAL(I)*SCAL(J)
C Now use MATLIB routines
	call MATINV(finfo,kfit,ndc,covar,ndc)
	one=1.0d0
	call MATMUL(finfo,covar,unit,kfit,kfit,kfit,one,
     & ndc,ndc,ndc,ndc,ndc,ndc)
	DEALLOCATE(DELTA,TH1,TH2,TH3,TH4)
	RETURN
	end


