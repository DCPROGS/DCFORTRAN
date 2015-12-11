	SUBROUTINE CORCAL(YVAL,NYVAL,AHIST,OHIST,GHIST,BHIST,OBHIST,
     & IDTYPE,FREQ,XAXIS,XMIN,XMAX,NBIN,FMIN,FMAX,
     & FLO,FHI,XWBASE,IDEST)
C Version of CORCAL for EKDIS3
C SUBROUTINE FOR OVERLAY IN EKDIST TO CALC TESTS FOR CORRELATION
C (A)RUNS TEST
C (B)AUTOCORRELATION
C
	dimension YVAL(20480),FREQ(510),XAXIS(510)
	dimension YVAL1(20480)	!in case some values exc for cor calcs
	real AC(10),ACG(10)
	character*1 ans,UC
	LOGICAL AHIST,OHIST,GHIST,BHIST,OBHIST,AUTO,TRANS,pon,oscor
	logical discprt,slock,caplock,debug
	common/dp/discprt
C
	pon()=slock()
	debug()=caplock()
c
	print 11
	if(pon()) write(7,11)
	if(discprt) write(8,11)
11	FORMAT(/' CORRELATION CALCULATIONS',/)
c define oscor
	OSCOR=OHIST.and.idtype.eq.4	!open-shut correlation
c
	Nyval1=NYVAL	!COPY IN CASE ALTERED BELOW
300	TRANS=.FALSE.	!RETURN HERE FOR ANOTHER TEST
	NYVAL=Nyval1	!RESTORE
c Copy data to YVAL1 in case orig data altered by omission of some
c values, or by transformation (need orig data if another test done)
	do 362 i=1,nyval
362	yval1(i)=yval(i)
	print 35
35	FORMAT('&Limit range of Y values [N] ? ')
	read 101,ans
	if(UC(ans).NE.'Y') GOTO 361
	print 37
37	FORMAT('&Ylow, Yhigh= ')
	call INPUT2r(ylo,yhi)
	if(yhi.gt.ylo) goto 381
	x=yhi
	yhi=ylo
	ylo=x
381	continue
	J=0
	DO 39 I=1,NYVAL
	IF(YVAL(I).LT.YLO.OR.YVAL(I).GT.YHI) GOTO 39
	J=J+1
	YVAL1(J)=YVAL(I)
39	CONTINUE
	NYVAL1=J		!NEW VALUE
	print 50,NYVAL1
50	FORMAT( ' New number of values= ',I8,/)
	if(pon()) write(7,501)nyval1,ylo,yhi
      if(discprt) write(8,501)nyval1,ylo,yhi
501	format(' Correlation calculated on ',i6,' values, from',g13.6,
     & ' to ',g13.6)
361	continue
C
	IF(BHIST.AND.IDTYPE.EQ.11) GOTO 60
	if(OSCOR) goto 70
c
	print 1
1	FORMAT('&Runs test [Y] ? ')
	read 101,ans
101	format(a1)
	if(UC(ans).EQ.'N') GOTO 3
C RUNS TEST NOW
9	continue
	auto=.false.
	if(.not.(ohist.or.ghist.or.bhist)) goto 91
c If X axis is time ask whether default Xcrit to be used
	print 92
92	format('&Use default X(crit) (0.1, 0.3, 1.0,...ms) [Y] ? ')
	read 101,ans
	auto=UC(ans).ne.'N'
	if(auto) goto 7
91	print 5
5	FORMAT('&Xcrit (real; type-1.for HELP)= ')
	call INPUTr(xcrit)
	IF(XCRIT.GT.0.) GOTO 7
	print 8
8	FORMAT(
     & ' Observations are assigned a value 0 if less than Xcrit',/,
     & '  or 1 if equal or greater than Xcrit. The runs tests counts',/,
     & '  runs of 0 values and of 1 values. If the number of runs',/,
     & '  is smaller than expected for a random process then',/,
     & '  small/large values occur in runs i.e. values are ',/,
     & '  positively correlated')
	GOTO 9
7	IF(OBHIST) XCRIT=XCRIT-0.01	!TO PREVENT ROUNDING ERRORS
C
	k=0			!number of xcrit values done if auto
c
94	continue		!return here for next Xcrit if auto
	N0=0		!intialise for current Xcrit
	N1=0
	NRUN=1
	k=k+1
	j=mod(k,2)	!=1 for odd k
	x=10.0**((k-1)/2 - 1)	!=0.1,0.1,1,1,10,10,...
	if(auto) Xcrit=float(j)*x + float(1-j)*3.0*x	!=0.1,.3,1.,3.,...
C
	if(debug()) print 15
15	FORMAT('   i      Yval(i)      m        Nrun',/)
	DO 10 I=1,NYVAL1
	M=0
	IF(YVAL1(I).GT.XCRIT) M=1
	IF(M.EQ.0) N0=N0+1
	IF(M.EQ.1) N1=N1+1
	IF(I.EQ.1) GOTO 13		!mlast not yet defined
	IF(M.EQ.MLAST) GOTO 13		!RUN NOT ENDED YET
	NRUN=NRUN+1
13	MLAST=M
	if(debug()) print 14,I,YVAL1(I),M,NRUN
14	FORMAT(I8,2X,G13.6,2I8)
10	CONTINUE		!end of Yval loop
C
	N=N0+N1
	IF(N.NE.NYVAL1) print 12,n0,n1,n,nyval
12	FORMAT(' ERROR: N NOT EQUAL TO NYVAL n0,n1,n,nyval= ',4i8,/)
	if(.not.auto) goto 93
	if(n0.lt.4) goto 94	!next Xcrit if auto
	if(n1.lt.4) goto 95	!no more Xcrit if auto
93	continue
	A=FLOAT(N)
	A0=FLOAT(N0)
	A1=FLOAT(N1)
	AMEAN=1.0+(2.0*A0*A1/A)
	AVAR=2.0*A0*A1*(2.0*A0*A1-A)/(A**3-A**2)
	Z=(FLOAT(NRUN)-AMEAN)/SQRT(AVAR)
	print 20,xcrit,N0,N1,NRUN,AMEAN,SQRT(AVAR),Z
	if(pon()) write(7,20)xcrit,N0,N1,NRUN,AMEAN,SQRT(AVAR),Z
      if(discprt) write(8,20)xcrit,N0,N1,NRUN,AMEAN,SQRT(AVAR),Z
20	FORMAT(' Runs test: critical value= ',g13.6,/,
     & ' No of 0, 1 values= ',2i8,/,
     & ' Observed no of runs= ',I8,/,
     & ' Expected no of runs= ',g13.6,': SD = ',G13.6,/,
     & ' Normal deviate= ',G13.6)
	if(auto) goto 94	!next Xcrit if auto
c
95	print 69
69	format(' Try another Xcrit value [N] ? ')
	read 101,ans
	if(UC(ans).EQ.'Y') GOTO 9
C
C
3	CONTINUE
C AUTOCORRELATION COEFFICIENT. FIRST CALC FOR LAG=1,2,...,5, BOTH
C WITH ORIG DATA, AND WITH TRANSFORMED DATA (+NORMAL DEVIATE FOR LATTER)
C THEN ASK IF MORE CALCS REQUIRED (E.G. OTHER LAGS, OR LIMITED RANGE
C OF VALUES- N.B. LATTER WILL REMOVE SOME VALUES THUS AFFECTING
C THE EFFECTIVE LAG).
	AUTO=.TRUE.
	print 4
4	FORMAT(' Calculate autocorrelation coefficients [Y] ? ')
	read 101,ans
	if(UC(ans).EQ.'N') RETURN
	print 44
44	FORMAT('&  Lags 1-N (-1=manual) [5]. N= ')
	read 23,NLAG
	IF(NLAG.LE.-1) AUTO=.FALSE.
	IF(NLAG.EQ.0) NLAG=5		!DEFAULT
C
C
	CALL VAR(YVAL1,NYVAL1,YBAR,VARY,1,1,N)  !MEAN AND VAR FOR ORIG DATA
	IF(AUTO) GOTO 46
c
	print 21
21	FORMAT('&Transform to Gaussian? ')
	read 101,ans
	if(UC(ans).EQ.'N') GOTO 46
	TRANS=.TRUE.
C
46	CONTINUE
	print 27,YBAR
27	FORMAT('&Mean (for Gaussian transformation)= ',
     & G13.6,'  O.K.[Y] ? ')
	read 101,ans
	if(UC(ans).NE.'N') GOTO 29
	print 28
28	FORMAT(' mean= ')
	call INPUTr(ybar)
29	CONTINUE
	IF(.NOT.AUTO) GOTO 45
C
C CALC AUTCOR FOR UNTRANSFORMED DATA FOR LAGS 1-NLAG. RESULT IN AC(K)
	DO 40 K=1,NLAG
	k1=k
40	AC(K)=AUTCOR(YVAL1,NYVAL1,K1,VARY,YBAR)
C
C NOW TRANSFORM AND REPEAT CALC FOR NEW DATA- RESULT IN ACG(K)
	CALL GTRANS(YVAL1,NYVAL1,YBAR,1,1)
	TRANS=.TRUE.
	CALL VAR(YVAL1,NYVAL1,YBAR,VARY,1,1,N) !RECALC FOR TRANSFORMED YVAL
	print 33,YBAR,VARY
	if(pon()) write(7,33)YBAR,VARY
      if(discprt) write(8,33)YBAR,VARY
33	FORMAT(
     & ' Mean and var for transformed values (should be 0,1)= ',
     & 2G13.6,/)
	DO 41 K=1,NLAG
	k1=k
41	ACG(K)=AUTCOR(YVAL1,NYVAL1,K1,VARY,YBAR)
C print THESE RESULTS
	if(pon()) print 42
	if(discprt) write(8,42)
	print 42
42	FORMAT(
     & '  Lag   Auto (no trans)   Auto (trans)    Norm.dev.(trans)')
	DO 49 I=1,NLAG
	if(pon()) write(7,43)I,AC(I),ACG(I),ACG(I)*SQRT(FLOAT(NYVAL1))
      if(discprt) write(8,43)I,AC(I),ACG(I),ACG(I)*SQRT(FLOAT(NYVAL1))
	print 43,I,AC(I),ACG(I),ACG(I)*SQRT(FLOAT(NYVAL1))
43	FORMAT(I6,3(3X,G13.6))
49	CONTINUE
	GOTO 47
C
C NOW 'MANUAL' AUTOCORRELATION CALC
45	CONTINUE
	IF(.NOT.TRANS) GOTO 48
	CALL GTRANS(YVAL1,NYVAL1,YBAR,1,1)
	CALL VAR(YVAL1,NYVAL1,YBAR,VARY,1,1,N) !RECALC FOR TRANSFORMED YVAL
	if(pon()) write(7,33)YBAR,VARY
      if(discprt) write(8,33)YBAR,VARY
	print 33,YBAR,VARY
48	CONTINUE
C
	print 22
22	FORMAT(' Lag, K= ')
	read 23,K
23	FORMAT(I8)
	Z=AUTCOR(YVAL1,NYVAL1,K,VARY,YBAR)
	if(pon()) write(7,25)k,Z,Z*SQRT(FLOAT(NYVAL1))
      if(discprt) write(8,25)k,Z,Z*SQRT(FLOAT(NYVAL1))
	print 25,k,Z,Z*SQRT(FLOAT(NYVAL1))
25	FORMAT(' Autocorrelation coeff ( for lag= ',i3,') = ',G13.6,/,
     & ' Normal deviate= ',G13.6)
C
C
47	print 301
301	FORMAT( ' Another test [N] ? ')
	read 101,ans
	if(UC(ans).EQ.'Y') GOTO 300
	IF(.NOT.TRANS) RETURN
	GOTO 61
C
C SECTION TO CALC CORRELATION BETWEEN 1ST AND JTH OPENING LENGTH IN
C A BURST. IN THIS CASE (IDTYPE=11) YVAL CONTAINS ALTERNATELY THE
C LENGTHS OF THE 1ST AND JTH OPENINGS (SO NYVAL SHOULD BE EVEN!)
C FIRST CALC FOR UNTRANSFORMED VALUES.
60	IDEST=21
c	do 3621 i=1,nyval	!already copied
c3621	yval1(i)=yval(i)
701	CALL VAR(YVAL1,NYVAL1,YBAR,VARY,1,2,N)  !MEAN & VAR FOR 1ST OPENING
c (N is output from VAR)
	if(pon()) write(7,62)YBAR,SQRT(VARY),N
      if(discprt) write(8,62)YBAR,SQRT(VARY),N
	print 62,YBAR,SQRT(VARY),N
62	FORMAT(' Mean & SD for Ith opening= ',2G13.6,'  N= ',I6,/)
	CALL VAR(YVAL1,NYVAL1,YBAR1,VARY1,2,2,N)  !DITTO FOR JTH OPENING
	if(pon()) write(7,63)YBAR1,SQRT(VARY1),N
      if(discprt) write(8,63)YBAR1,SQRT(VARY1),N
	print 63,YBAR1,SQRT(VARY1),N
63	FORMAT(' Mean & SD for Jth opening= ',2G13.6,'  N= ',I6,/)
	SP=0.0
	DO 64 I=1,NYVAL1-1,2
64	SP=SP+(YVAL1(I)-YBAR)*(YVAL1(I+1)-YBAR1)
	AC(1)=SP/((N-1)*SQRT(VARY)*SQRT(VARY1))
	if(pon()) write(7,65)AC(1),AC(1)*SQRT(FLOAT(N-2))
      if(discprt) write(8,65)AC(1),AC(1)*SQRT(FLOAT(N-2))
	print 65,AC(1),AC(1)*SQRT(FLOAT(N-2))
65	FORMAT( ' Correlation coeff= ',G13.6,/,
     & ' normal deviate= ',G13.6,/)
	IF(TRANS) GOTO 61
	if(pon()) print 66
	if(discprt) write(8,66)
	print 66
66	FORMAT(/,' After transformation to Gaussian: '/)
	CALL GTRANS(YVAL1,NYVAL1-1,YBAR,1,2)	!TRANSFORM Ith OPENINGS
	CALL GTRANS(YVAL1,NYVAL1,YBAR1,2,2)	!TRANSFORM Jth OPENINGS
	TRANS=.TRUE.
	GOTO 701		!REPEAT CALCS FOR TRANSFORMED VALUES
C
c
c Section to calc correlation between an apparent open time and the
c Kth subsequent shut time. Yval(), as defined in CDIST0 should start with
c a valid open time, followed by a shut time so open/shut alternate. If
c there are contiguous openings the all openings that are not immediately
c followed by a shut period are omitted
c Nyval must be even.
c LAG=1 ONLY AT PRESENT- NEED MEANS & VAR OF JTH SUBSEQUENT GAP
C SEPARATELY FOR EACH LAG??
70	continue
	IDEST=21	!so goes to 'where next' after RETURN
77	CALL VAR(YVAL1,NYVAL1,YBAR,VARY,1,2,N)  !MEAN & VAR FOR OPENING
	if(pon()) write(7,72)YBAR,SQRT(VARY),N
      if(discprt) write(8,72)YBAR,SQRT(VARY),N
	print 72,YBAR,SQRT(VARY),N
72	FORMAT(' Autocorrelation for opening time and following shut time'
     & ,/' Mean & SD for openings= ',2G13.6,'  N= ',I6,/)
	CALL VAR(YVAL1,NYVAL1,YBAR1,VARY1,2,2,N)  !DITTO FOR JTH OPENING
	if(pon()) write(7,73)YBAR1,SQRT(VARY1),N
      if(discprt) write(8,73)YBAR1,SQRT(VARY1),N
	print 73,YBAR1,SQRT(VARY1),N
73	FORMAT(' Mean & SD for shut times= ',2G13.6,'  N= ',I6,/)
	SP=0.0
	DO 74 I=1,NYVAL1-1,2
74	SP=SP+(YVAL1(I)-YBAR)*(YVAL1(I+1)-YBAR1)
	AC(1)=SP/((N-1)*SQRT(VARY)*SQRT(VARY1))
	if(pon()) write(7,65)AC(1),AC(1)*SQRT(FLOAT(N-2))
      if(discprt) write(8,65)AC(1),AC(1)*SQRT(FLOAT(N-2))
	print 75,AC(1),AC(1)*SQRT(FLOAT(N-2))
75	FORMAT(' Correlation coeff= ',G13.6,/,
     & ' normal deviate= ',G13.6,/)
	IF(TRANS) GOTO 61
	if(pon()) print 76
	if(discprt) write(8,76)
	print 76
76	FORMAT(/,' After transformation to Gaussian: '/)
	CALL GTRANS(YVAL1,NYVAL1-1,YBAR,1,2)	!TRANSFORM Ith OPENINGS
	CALL GTRANS(YVAL1,NYVAL1,YBAR1,2,2)	!TRANSFORM Jth OPENINGS
	TRANS=.TRUE.
	GOTO 77		!REPEAT CALCS FOR TRANSFORMED VALUES
c
c
61	print 32
32	FORMAT( ' Display histogram of transformed values [N] ? ')
	read 101,ans
	if(UC(ans).NE.'Y') RETURN
C NOW MUST CALC HISTOGRAM FREQUENCIES FOR TRANSFORMED VALUES
C
	N1=1
	NS=1
	IF(.NOT.(BHIST.AND.IDTYPE.EQ.11)) GOTO 67
	NS=2
	print 68
68	FORMAT('   For Ith (N1=1) or Jth (N1=2) opening. N1= ')
	read 23,N1
67	NBW=1
	print 209
209	FORMAT( ' Start histogram at x= ')
	call INPUTr(xaxis(1))
	K=1		!COUNTS X AXIS ENTRIES
	DO 210 I=1,NBW
	KLAST=K
212	print 104,I
104	FORMAT( ' (',I2,') Bin width, Last x value= ')
	call INPUT2r(dx,xend)
	XN=(XEND-XAXIS(KLAST))/DX
c	IF(ABS(XN-FLOAT(IFIX(0.5+XN))).GT.DX*.01) GOTO 212
C I.E. ASK AGAIN IF NOT AN INTEGRAL NO OF BINS BET X START AND XEND
	IF(ABS(XN-FLOAT(IFIX(0.5+XN))).lt.DX*.01) GOTO 30	!OK
	xend=xaxis(klast) + float(ifix(0.5+xn))*dx
	call bell(1)
	print 31,xend
31	format(' Last x reset to ',g13.6,' O.K.? ')
	read 101,ans
	if(UC(ans).eq.'N') goto 212
30	NBIN=IFIX(0.5+XN)
	IF(NBIN.EQ.0) GOTO 212
	DO 211 J=1,NBIN
	K=K+1
211	XAXIS(K)=XAXIS(KLAST)+FLOAT(J)*DX
210	CONTINUE	!END OF I LOOP
C
	NBIN=K-1		!NOW NBIN=TOTAL NO OF BINS
	XWBASE=DX		!AS ONLY ONE BIN WIDTH
	DO 224 I=1,101
224	FREQ(I)=0.0
	DO 223 J=1,NBIN
	DO 221 I=N1,NYVAL1,NS	!INDEX OF YVAL
	IF(YVAL1(I).LT.XAXIS(J)) GOTO 221
	IF(YVAL1(I).GE.XAXIS(J+1)) GOTO 221
	FREQ(J)=FREQ(J)+1.
221	CONTINUE	!I LOOP
223	CONTINUE	!J LOOP
	FLO=0.
	FHI=0.
	DO 401 I=N1,NYVAL1,NS	!INDEX OF YVAL FOR CURRENT EXPT
	IF(YVAL1(I).LT.XAXIS(1)) FLO=FLO+1.0
	IF(YVAL1(I).GE.XAXIS(NBIN+1)) FHI=FHI+1.0
401	CONTINUE
	IF(FLO.LT.0.1) FLO=-1.	!DO NOT PLOT IF FLO=0
	IF(FHI.LT.0.1) FHI=-1.	!DITTO
	CONTINUE
CD	DO 2201 I=1,NBIN
CD2201	print 106,I,FREQ(I)
CD	print 108
	FMAX=0.
	DO 218 J=1,NBIN
218	IF(FREQ(J).GT.FMAX) FMAX=FREQ(J)
	IF(FLO.GT.FMAX) FMAX=FLO
	IF(FHI.GT.FMAX) FMAX=FHI
C idest=21- correlation only
c idest=22- correlation + fit. But if display of transformed variables
c is req here then cannot do fit as histo is lost
c	IF(IDEST.EQ.21) IDEST=31
	idest=31	!display transformed variables
	RETURN
	END


	FUNCTION AUTCOR(YVAL,NYVAL,K,VARY,YBAR)
C CALCULATES AUTOCORRELATION COEFF, WITH LAG=K, FOR THE ARRAY YVAL
C INPUT MEAN AND VAR OF YVAL CALC BY PREV CALL TO VAR
	dimension YVAL(20480)
C
	SY=FLOAT(NYVAL-1)*VARY		!SUM OF SQUARES
	SP=0.0
	DO 24 I=1,NYVAL-K
	SP=SP+(YVAL(I)-YBAR)*(YVAL(I+K)-YBAR)
24	CONTINUE
	AUTCOR=SP/SY
	RETURN
	END


	SUBROUTINE GTRANS(Y,N,YBAR,N1,NS)
	dimension Y(20480)
C
C TO TRANSFORM EXPONENTIAL VARIATE IN YVAL TO GAUSSIAN
C STARTS AT Y(N1) AND INCREMENTS INDEX BY NS UNTIL Y(N)
	PI=3.1415927
	DO 1 I=N1,N,NS
cD	print 21,I,Y(I)
cD21	FORMAT(I5,2X,G13.6)
	Q=EXP(-Y(I)/YBAR)	!NEAR ZERO FOR LONG INTERVALS!
	X=4.0*Q*(1.0-Q)
	IF(X.LT.1.E-32) X=1.E-32	!TO PREVENT LOG(0)
	X=SQRT(-0.5*PI*ALOG(X))
	IF(Q.GT.0.5) X=-X
	Z=X+7.8365E-3*(X**3)-2.881E-4*(X**5)+4.3728E-6*(X**7)
cD	print 2,Z
cD2	FORMAT(G13.6)
	Y(I)=Z
1	CONTINUE
	RETURN
	END


	SUBROUTINE VAR(Y,N,YBAR,VARY,N1,NS,NVAL)
C	CALCULATES MEAN,YBAR,AND VARIANCE,VARY OF FIRST N
C	ELEMENTS OF Y. SINGLE PASS YOUNGS AND CRAMER METHOD.
C STARTS AT Y(N1) AND INCREMENTS INDEX BY NS UNTIL Y(N)
C
	dimension Y(20480)
C
C	SUMY=Y(1)
	SUMY=Y(N1)	!** MODIF FOR N1,N,NS
	SUMYY=0.
	NVAL=1
C
C	DO 1 I=2,N
	DO 1 I=N1+NS,N,NS	!**
	NVAL=NVAL+1		!**
	YI=Y(I)
	FI=FLOAT(I)
	SUMY=SUMY+YI
	SUMYY=SUMYY+(FI*YI-SUMY)**2/(FI*(FI-1))
1	CONTINUE
C
	YBAR=SUMY/FLOAT(NVAL)
	VARY=SUMYY/FLOAT(NVAL-1)
	RETURN
	END



