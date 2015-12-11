	subroutine TCSUB1(tc1,tc2,tc3,noprint)
c TCSUB1 is version of TCSUB that outputs the three tcrit values
c Also if NOPRINT=true, then does no printing at all: assumes that common/TBLK
c already defined in calling prog
c
C SUBROUTINE VERSION OF TCRIT.FOR
C INCLUDES BISECT IN SAME FILE.
C SOLVES FOR BEST TCRIT FOR DISTINGUISHING BURSTS GIVEN
C TAU FAST,TAU SLOW FROM DISTRIBUTION OF ALL GAPS
C
C
c	LOGICAL SHORT
	COMMON/TBLK/ TS,TF,AS,AF
	character*1 ans,UC
	logical discprt,pon,slock,noprint
	common/dp/discprt
	EXTERNAL TCFUNC,TFUNC1,TFUNC2
c
	pon()=slock()
C
	NDISP=-1	!NO print
101	FORMAT(A1)
	EPS=1.E-4	! 0.1 MICROSEC ERROR
10	NPASS=1
c
	if(noprint) goto 80
	print 1
1	FORMAT(' TAU fast, TAU slow (ms)= ')
	call INPUT2r(tf,ts)
200	FORMAT(2G13.6)
2	FORMAT(G13.6)
	print 22
22	FORMAT('&AREA fast, AREA slow= ')
	call INPUT2r(af,as)
      print 82,tf,af,ts,as
      if(pon()) write(7,82) tf,af,ts,as
      if(discprt) write(8,82) tf,af,ts,as
82	format(
     & ' Optimum t(crit) found between tau = ',g13.6,'ms (area = ',
     &   f10.4,'), and',/,
     & ' tau = ',g13.6,'ms (area = ',f10.4,'):')
80	continue
	ntot=100
24	FORMAT(I8)
	ENF=FLOAT(NTOT)*AF
	ENS=FLOAT(NTOT)*AS
21	CONTINUE
C
	X1=0.0		!INIT GUESSES
	X2=2.*TS
c
	CALL BISECT(TCFUNC,X1,X2,0.0,TCRIT,YOUT,EPS,NERR,NDISP)
	tc1=tcrit		!DC criterion
c	IF(NERR.EQ.1) print 51,X1,X2
c51	FORMAT( ' GUESSES BAD FOR BISECT. X1,X2= ',2G13.6)
	if(nerr.eq.1) goto 99
50	continue
	if(noprint) goto 81
	IF(NPASS.EQ.1) then
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
     & ' Minimum total number misclassified (Jackson et al criterion)')
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
c
      print 31,ENF*BF,ENS*BS
      if(pon()) write(7,31) ENF*BF,ENS*BS
      if(discprt) write(8,31) ENF*BF,ENS*BS
31	FORMAT( ' # short misclassified= ',G11.4,
     &	' # long misclassified= ',G11.4)
	print 32,ENF*BF+ENS*BS
      if(pon()) write(7,32) ENF*BF+ENS*BS
      if(discprt) write(8,32) ENF*BF+ENS*BS
32	FORMAT( ' Total # misclassified (out of 100)= ',G11.4)
81	continue
	IF(NPASS.EQ.2) GOTO 60	! 2ND METHOD DONE
	IF(NPASS.EQ.3) GOTO 99	! ALL 3 DONE
C
C CLAPHAM & NEHER CRITERION
	X1=0.0		!INIT GUESSES
	X2=2.*TS
	CALL BISECT(TFUNC1,X1,X2,0.0,TCRIT,YOUT,EPS,NERR,NDISP)
	if(nerr.eq.1) goto 99
	tc2=tcrit
c	IF(NERR.EQ.1) print 51,X1,X2
	NPASS=2
	GOTO 50
C
C NOW JACKSON ET AL CRITERION
60	X1=0.0		!INIT GUESSES
	X2=2.*TS
	CALL BISECT(TFUNC2,X1,X2,0.0,TCRIT,YOUT,EPS,NERR,NDISP)
	if(nerr.eq.1) goto 99
	tc3=tcrit
c	IF(NERR.EQ.1) print 51,X1,X2
	NPASS=3
	GOTO 50
C
99	continue
	if(nerr.eq.1) then
         print 821
         if(pon()) write(7,821)
         if(discprt) write(8,821)
821	   format(' BISECTION FAILED')
	   tc1=-1.		!signal to calling program
	endif
	if(noprint) RETURN
	print 120
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
C BAD,NERR=0 OTHERWISE
C  PRINT PROGRESS	S EVERY NDISP STEPS (IF NDISP NEGATIVE NO PRINTING
C
	NDSAV=0
	NTRY=1
	IF(NDISP.GE.0) print 12
12	FORMAT(/, '  NSTEP      XOUT       YOUT',/)
982	IF(NTRY.LT.5) GOTO 983
	print 984
984	format(
     & ' (1) give new guesses for x1,x2',/,
     & ' (2) abandon bisection',/,
     & ' Option number = ')
	i=2
	call INPUTi(i)
	if(i.eq.2) then
	   nerr=1
	   RETURN
	endif
	print 9841
9841	FORMAT( ' NEW GUESSES X1,X2= ')
	read 985,X1,X2
985	FORMAT(2G13.6)
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
