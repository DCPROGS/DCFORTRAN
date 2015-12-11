	subroutine TCSUB()
C
C SUBROUTINE VERSION OF TCRIT.FOR
C INCLUDES BISECT IN SAME FILE. LEVENT NEEDED ONLY IF /D
C SOLVES FOR BEST TCRIT FOR DISTINGUISHING BURSTS GIVEN
C TAU FAST,TAU SLOW FROM DISTRIBUTION OF ALL GAPS
C	LINK TCRIT,BISECT,LEVENT
C
C
c	LOGICAL SHORT
	character*1 ans,UC
	logical discprt,pon,slock
	COMMON/TBLK/ TS,TF,AS,AF
	common/dp/discprt
	EXTERNAL TCFUNC,TFUNC1,TFUNC2
c
	pon()=slock()
C
	NDISP=-1	!NO print
c	IF(LEVENT(1).EQ.0) GOTO 102
c	print 4
c4	FORMAT(' Type every Nth iteration (-1 for none) N= ')
c	read 5,NDISP
c5	FORMAT(I8)
c102	CONTINUE
101	FORMAT(A1)
	EPS=1.E-4	! 0.1 MICROSEC ERROR
c	print 20,EPS
c20	FORMAT(' ERROR (MS)= ',G13.6)
C
10	NPASS=1
	print 1
1	FORMAT(' For faster component: Tau (ms), Area (fractional) = ')
	call INPUT2r(tf,af)
c	read 200,TF,TS
c200	FORMAT(2G13.6)
	print 22
c22	FORMAT('&AREA fast, AREA slow= ')
22	FORMAT('&For slower component: Tau (ms), Area (fractional) = ')
	call INPUT2r(ts,as)
      print 2,tf,af,ts,as
      if(pon()) write(7,2) tf,af,ts,as
      if(discprt) write(8,2) tf,af,ts,as
2	format(/,
     & ' Critical time between tau = ',g12.5,'(area = ',g12.5,')',/,
     & '                   and tau = ',g12.5,'(area = ',g12.5,')')
	ntot=100
c	print 23
c3	FORMAT( ' TOTAL NO OF EVENTS (INT)= ')
c	read 24,NTOT
24	FORMAT(I8)
	ENF=FLOAT(NTOT)*AF
	ENS=FLOAT(NTOT)*AS
21	CONTINUE
C
c	X1=0.0		!INIT GUESSES
c	X2=2.*TS
	x1=tf		!init guesses
	x2=ts
C
	CALL BISECT(TCFUNC,X1,X2,0.0,TCRIT,YOUT,EPS,NERR,NDISP)
	if(nerr.eq.1) then
	   print 51,X1,X2
51	   format(' Guesses bad for BISECT. x1,x2= ',2g13.6)
	   RETURN
	else if(nerr.eq.2) then
	   print 61
61	   format(' BISECT aborted.')
	   RETURN
	endif
50	IF(NPASS.EQ.1) then
         print 52
         if(pon()) write(7,52)
         if(discprt) write(8,52)
52	   FORMAT(' Equal % misclassified (DC criterion)')
	else if(npass.eq.2) then
         print 53
         if(pon()) write(7,53)
         if(discprt) write(8,53)
53	   FORMAT(/,
     & ' Equal numbers misclassified (Clapham & Neher criterion)')
	else if(npass.eq.3) then
         print 54
         if(pon()) write(7,54)
         if(discprt) write(8,54)
54	   FORMAT(/,
     & ' Minimun total number misclassified (Jackson et al criterion)')
	endif
      print 6,tcrit
      if(pon()) write(7,6) tcrit
      if(discprt) write(8,6) tcrit
6	FORMAT( ' Tcrit (ms)= ',G11.4)
	BF=EXP(-TCRIT/TF)
	BS=1.0 - EXP(-TCRIT/TS)
      print 30,100.*BF,100.*BS
      if(pon()) write(7,30) 100.*BF,100.*BS
      if(discprt) write(8,30) 100.*BF,100.*BS
30	FORMAT( ' % short misclassified= ',G11.4,
     &	' % long misclassified= ',G11.4)
c	IF(SHORT) GOTO 10
C
      print 31,ENF*BF,ENS*BS
      if(pon()) write(7,31) ENF*BF,ENS*BS
      if(discprt) write(8,31) ENF*BF,ENS*BS
31	FORMAT( ' # short misclassified= ',G11.4,
     &	' # long misclassified= ',G11.4)
	print 32,ENF*BF+ENS*BS
      if(pon()) write(7,32) ENF*BF+ENS*BS
      if(discprt) write(8,32) ENF*BF+ENS*BS
32	FORMAT( ' Total # misclassified (out of 100)= ',G11.4)
	IF(NPASS.EQ.2) GOTO 60	! 2ND METHOD DONE
	IF(NPASS.EQ.3) GOTO 99	! ALL 3 DONE
C
C CLAPHAM & NEHER CRITERION
c	X1=0.0		!INIT GUESSES
c	X2=2.*TS
	x1=tf		!init guesses
	x2=ts
	CALL BISECT(TFUNC1,X1,X2,0.0,TCRIT,YOUT,EPS,NERR,NDISP)
	if(nerr.eq.1) then
	   print 51,X1,X2
c51	   format(' Guesses bad for BISECT. x1,x2= ',2g13.6)
	   RETURN
	else if(nerr.eq.2) then
	   print 61
c61	   format(' BISECT aborted.')
	   RETURN
	endif
	NPASS=2
	GOTO 50
C
C NOW JACKSON ET AL CRITERION
60	continue
c	X1=0.0		!INIT GUESSES
c	X2=2.*TS
	x1=tf		!init guesses
	x2=ts
	CALL BISECT(TFUNC2,X1,X2,0.0,TCRIT,YOUT,EPS,NERR,NDISP)
	if(nerr.eq.1) then
	   print 51,X1,X2
c51	   format(' Guesses bad for BISECT. x1,x2= ',2g13.6)
	   RETURN
	else if(nerr.eq.2) then
	   print 61
c61	   format(' BISECT aborted.')
	   RETURN
	endif
	NPASS=3
	GOTO 50
C
99	print 120
120	FORMAT(' Another Tcrit calculation [N] ? ')
	read 101,ans
	if(UC(ans).eq.'Y') GOTO 10
C
	RETURN
	END



	function TCFUNC(TC)
	COMMON/TBLK/ TS,TF,AS,AF
C
	S=(1.0-EXP(-TC/TS))-EXP(-TC/TF)
	TCFUNC=S
	RETURN
	END

	function TFUNC1(TC)
	COMMON/TBLK/ TS,TF,AS,AF
C
	S=AF*EXP(-TC/TF) - AS*(1.0-EXP(-TC/TS))
	TFUNC1=S
	RETURN
	END

	function TFUNC2(TC)
	COMMON/TBLK/ TS,TF,AS,AF
C
	S=(AF/TF)*EXP(-TC/TF) - (AS/TS)*EXP(-TC/TS)
	TFUNC2=S
	RETURN
	END

	SUBROUTINE BISECT(FUNC,X1,X2,Y,XOUT,YOUT,EPS,NERR,NDISP)
	external FUNC
C
C*** VERSION THAT CAN BE STOPPED BY EVENT 0
C SOLVES FUNC(X)=Y BY BISECTION GIVEN Y AND GUESSES X1,AND
C X2 THAT INCLUDE SOLUTION. EPS=ACCURACY; NERR=1 IF GUESSES
C BAD, nerr=2 if aborted, NERR=0 OTHERWISE
C  PRINT PROGRESS	S EVERY NDISP STEPS (IF NDISP NEGATIVE NO PRINTING
C
	NDSAV=0
	NTRY=1
	IF(NDISP.GE.0) print 12
12	FORMAT(/, '  NSTEP      XOUT       YOUT',/)
982	IF(NTRY.LT.5) GOTO 983
	print 984
984	FORMAT(' New guesses for solution (-1 to abort): x1,x2= ')
	call INPUT2r(x1,x2)
	if(x1.eq.-1.) then
	   nerr=2
	   RETURN
	endif
c	read 985,X1,X2
c985	FORMAT(2G13.6)
983	FX1=FUNC(X1)
	FX2=FUNC(X2)
	NERR=0
	IF((FX1-Y)*(FX2-Y).GT.0.0) GOTO 98
	IF(FX1.LT.Y) GOTO 1
	P=X1
	X1=X2
	X2=P
1	NSTEP=IFIX(ALOG(ABS(X1-X2)/EPS)/ALOG(2.0)+0.5)
c	print 3,NSTEP
c3	FORMAT( ' NSTEP= ',I8)
C
	DO 2 NS=1,NSTEP
	XOUT=0.5*(X1+X2)
	YOUT=FUNC(XOUT)
	IF(NDISP.LT.0.OR.((NS-NDSAV).LT.NDISP)) GOTO 10
	print 4,NS,XOUT,YOUT
4	FORMAT(I5,3X,G13.6,2X,G13.6)
	NDSAV=NS
c	IF(LEVENT(0).EQ.1) GOTO 99	!ABORT WITH EVENT 0
10	CONTINUE
	IF(YOUT-Y) 5,6,7
5	X1=XOUT
	GOTO 2
6	GOTO 99
7	X2=XOUT
2	CONTINUE
	GOTO 99
C
98	NERR=1
	print 200,X1,X2,FX1-Y,FX2-Y
200	FORMAT(' BISECTION ERROR. X1,X2,FX1,FX2= ',/,4G13.6,/)
	IF(NTRY.EQ.1) DX=0.1*(X2-X1)
	X1=X1+DX
	X2=X2-DX	!MAKE GUESSES CLOSER
	NTRY=NTRY+1
	GOTO 982
C
99	IF(NDISP.GT.0) print 20
20	FORMAT(/)
	RETURN
	END
