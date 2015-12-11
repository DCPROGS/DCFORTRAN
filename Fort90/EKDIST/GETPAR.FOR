	SUBROUTINE GETPAR(AMEAN,AREA,SD,ID,ANTRUE,AHIST,THIST,
     & OBHIST,Ylow,yhigh,Ncomp,gamdst,isg)		!ID=IDTYPE
C
	REAL AMEAN(10),AREA(10),SD(10)	!PARAM
	character*1 ans,UC
	LOGICAL AHIST,THIST,OBHIST,GAMDST
C ** MODIF 20 FEB 84 SO THAT OBSERVED NUMBER RATHER THAN ANTRUE CAN
C BE SUPPLIED AND ANTRUE ESTIMATED FROM CUM DISTN BETWEEN SPECIFIED
C TLO,THI. ONLY FOR GAMMA (ETC) DISTN AT PRESENT. WOULD BE USEFUL IN
C ANY CASE WHERE FITTING NOT DONE SO ANTRUE UNKNOWN.
C AMEAN=TAU (FOR THIST) OR MEANS
C (FOR AHIST OR OBHIST),SD=STANDARD DEV (FOR AHIST)
C **11-NOV-82. OBHIST ADDED (NCOMP=1 OR 2)
C SUBROUTINE TO INPUT PRE-DETERMINED PARAM FOR CALC CURVES
C FOR OVERLAY IN SCDIST.FOR. WILL USE VALUES OF AMEAN ETC FROM LAST
C FIT (A)IF NO CHAINING (B)AFTER CHAIN BACK FROM PLOT (BUT NOT YET
C AFTER CHAIN BACK FROM ERRORS)
C
101	format(a1)
108	FORMAT(/)
	GAMDST=.FALSE.
	IF(.NOT.(THIST.AND.ISG.GT.0.and.(id.eq.7.or.id.eq.8))) GOTO 53
	ans='Y'
	call DCASK('Fit gamma distribution',ans,ans)
c	print 521
c521	FORMAT(
c     & ' Fit Gamma distribution (param=those for open time pdf)? ')
c	read 101,ans
	if(UC(ans).EQ.'Y') GAMDST=.TRUE.
53	CONTINUE
	print 1
1	FORMAT( ' Number of components (print 0 to use last fit)= ')
	read 2,N
	IF(N.le.0) RETURN
	NCOMP=N
2	FORMAT(I8)
	IF(AHIST) GOTO 3
	IF(OBHIST) GOTO 100
C SECTION FOR PARAMETERS FOR TIME HISTOGRAMS
	print 52
52	FORMAT( ' Parameters for curve:',/)
	SA=0.
	DO 5 I=1,NCOMP
	print 6,I
6	FORMAT( ' tau(',I1,') (ms)= ')
	call INPUTr(amean(i))
	IF(I.EQ.NCOMP) GOTO 91		!FIRST TIME FOR NCOMP=1
	print 9,I
9	FORMAT( ' area(',I1,')= ')
	call INPUTr(area(i))
	SA=SA+AREA(I)
5	CONTINUE
91	AREA(NCOMP)=1.0-SA
	GOTO 4		!PARAMETERS SET
3	CONTINUE
C NOW SECTION FOR AMPLITUDE HISTOGRAM PARAMETERS
	SA=0.
	DO 35 I=1,NCOMP
	print 39,I
39	FORMAT( ' Mean amp(',I1,') (pA)= ')
	call INPUTr(amean(i))
	print 37,I
37	FORMAT( ' Standard dev(',I1,') (pA)= ')
	call INPUTr(sd(i))
	IF(I.EQ.NCOMP) GOTO 36
	print 38,I
38	FORMAT( ' Area(',I1,')= ')
	call INPUTr(area(i))
	SA=SA+AREA(I)
35	CONTINUE
36	AREA(NCOMP)=1.0-SA
	GOTO 4
C
C SECTION FOR OBHIST PARAMETERS
100	CONTINUE
	SA=0.0
	DO 102 I=1,NCOMP
	print 103,I
103	FORMAT( ' Mean(',I1,') (ops/bst)= ')
	call INPUTr(amean(i))
	IF(I.EQ.NCOMP) GOTO 110
	print 104,I
104	FORMAT( ' area(',I1,')= ')
	call INPUTr(area(i))
	SA=SA+AREA(I)
102	CONTINUE
110	AREA(NCOMP)=1.0-SA
C
4	IF(.NOT.GAMDST) GOTO 45
	print 46
46	FORMAT(' Specify observed [Y] or total (N) number of events? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).EQ.'Y') then
	   if(ncomp.eq.1) then
		print 42
42		FORMAT( ' Tlow,Thigh (ms)= ')
		call INPUT2r(ylow,yhigh)
		print 44
44		FORMAT( ' Number of events between Tlow, Thigh (integer)= ')
		call INPUTi(n)
c		CALL CUMGAM(NCOMP,ISG,YLOW,YHIGH,AMEAN,AREA,F1,F2)
c		ANTRUE=FLOAT(N)/(F1-F2)
		t1=ylow
		t2=yhigh
c		D=GAMMAinc(t1,rate,n)-GAMMAinc(t2,rate,n)	!Prob[ylow < t < yhigh]
		f1=GAMMAinc(t1,rate,n)
		f2=GAMMAinc(t2,rate,n)	
		D=f1-f2		!Prob[ylow < t < yhigh]
		ANTRUE=FLOAT(N)/D
		print 113,ANTRUE,ANTRUE*(1.-F1),ANTRUE*F2
113	FORMAT( ' Predicted true no of events= ',G13.6,/
     & ,' No below Ylow= ',G13.6,' +No above Yhigh= ',G13.6)
		RETURN
	   else if(ncomp.eq.2) then
		print*, ' Not yet done for 2 open time components'
	   endif
	endif
C
45	print 51
51	FORMAT(' Total number (true) of events = ')
	call INPUTr(antrue)
	RETURN
C
	END






