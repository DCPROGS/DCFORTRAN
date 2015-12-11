	SUBROUTINE LIKINT()
c
c 06/07/89 05:06pm Lahey version of LIKIN3
c
C Version of LIKINT for EKDIS3
	dimension XLL(2,20),XUL(2,20),NPFIL(20)
	REAL THETA(20)
	INTEGER JFIX(20)
	character*1 UC,ans
	LOGICAL DEBUG,AHIST,THIST,OBHIST,LOWER,UPPER,omit,pon
	logical discprt,slock,caplock
	common/dp/discprt
	COMMON/LOGBLK/ AHIST,THIST,OBHIST
	COMMON/LIKBLK/Kmax,kfit,elmax,em,delmin,ndisp,theta,npar
	common/FIXBLK/nfix,jfix
c	COMMON/LIKBLK/KMAX,kfit,JFIX,ELMAX,EM,DELMIN,NDISP,THETA,NPAR
	EXTERNAL EXPLIK,GAULIK,OBLIK,ELFUNC
C
C Note that if KFIT=1, i.e. only one param to be estimated then
c when this is fixed at a trial value there are none to be
c estimated so SIMPLEX not needed- just call EXPLIK etc (see ELFUNC)
c
	pon()=slock()
	debug()=caplock()
c
	print 17
17	FORMAT( ' Compute m-unit intervals: m= ')
	call INPUTr(em)
	ELCRIT=ELMAX-EM
C INITIALISE
	DO 49 I=1,2
	DO 49 J=1,20
	XLL(I,J)=-2.E30
49	XUL(I,J)=-2.E30
C
	print 42
42	FORMAT('&No of parameters for which limits to be calc= ')
	read 14,NP
	IF(NP.LE.0) GOTO 999
	if(pon()) print 54
	if(discprt) write(8,54)
54	format(/,' Likelihood intervals')
	DO 43 J=1,NP
	print 13
13	FORMAT('&Parameter number= ')
	read 14,NPFIL(J)
14	FORMAT(I8)
	print 44
44	FORMAT( '  calc lower limit? ')
	read 46,ans
46	format(a1)
	if(UC(ans).EQ.'N') GOTO 47
	print 15
15	FORMAT('&  low and high guesses for lower limit= ')
	call INPUT2r(xlow,xhigh)
	IF(XLOW.LT.XHIGH) GOTO 52
	P=XLOW
	XLOW=XHIGH
	XHIGH=P
52	CONTINUE
	XLL(1,J)=XLOW
	XLL(2,J)=XHIGH
47	print 45
45	FORMAT('& calc upper limit? ')
	read 46,ans
	if(UC(ans).EQ.'N') GOTO 43
	print 33
33	FORMAT('&  low and high guesses for upper limit= ')
	call INPUT2r(xlow,xhigh)
	IF(XLOW.LT.XHIGH) GOTO 53
	P=XLOW
	XLOW=XHIGH
	XHIGH=P
53	CONTINUE
	XUL(1,J)=XLOW
	XUL(2,J)=XHIGH
43	CONTINUE
C
C
C PRECISION IN simplex CONTROLLED BY DELMIN=(CHANGE IN MIN)
C SAY ABSOLUTE ERROR OF 0.002 IN L(THETA) IS CLOSE ENOUGH THEN
	omit=.false.	!so BISEC tests and prints init guesses
	DELMIN=0.005	!defaults used if debug()=false
	NEPSX=1000
c	EPSY=-1.
	epsy=0.005
	ndisp=30
	IF(.NOT.debug()) GOTO 2
	if(kfit.eq.1) goto 21
	print 5
5	FORMAT( ' Abs accuracy for L(theta) in SIMPLEX: delmin= ')
	call INPUTr(del)
	DELMIN=DEL
	print 3
3	FORMAT( '&Print every Nth iteration in SIMPLEX. N= ')
	read 4,NDISP
4	FORMAT(I8)
21	print 28
28	FORMAT(
     & '&Accuracy for param (in Bisection) set to 1 in N. N= ')
	read 14,NEPSX
	print 56
56	FORMAT('&Abs accuracy L-Lmax in bisection (ignore if neg)= ')
	call INPUTr(epsy)
2	CONTINUE
C
C NOW START LOOP FOR THE PARAMETERS TO BE DONE
	DO 48 IP=1,NP
	NPAR=NPFIL(IP)	!CURRENT PARAMETER NUMBER
	JFIX(NPAR)=1	!FIX THIS PARAMETER
	nfix=nfix+1		!explik etc need correct NFIX
	EPSX=THETA(NPAR)/FLOAT(NEPSX)	!ABS ACCURACY FOR BISECT
	LOWER=.FALSE.
	UPPER=.FALSE.
	IF(XLL(1,IP).GT.-1.E30) LOWER=.TRUE.	!CALC LOWER LIMIT
	IF(XUL(1,IP).GT.-1.E30) UPPER=.TRUE.	!CALC UPPER LIMIT
C NOW CALC LOWER LIMIT FIRST IF WANTED
	IF(.NOT.LOWER) GOTO 50
	XLOW=XLL(1,IP)
	XHIGH=XLL(2,IP)
	ND1=1	!PRINT EVERY CYCLE IN BISECT
	IF(debug()) print 55,XLOW,XHIGH
55	FORMAT( ' Initial guess= ',G13.6,2X,G13.6,/)
	CALL BISEC0(ELFUNC,XLOW,XHIGH,0.0,XLIM,YOUT,EPSX,EPSY,
     & NERR,ND1,omit)
	if(pon()) write(7,29)NPAR,theta(npar),EM,XLIM,xlim-theta(npar)
      if(discprt) write(8,29)NPAR,theta(npar),EM,XLIM,xlim-theta(npar)
	print 29,NPAR,theta(npar),EM,XLIM,xlim-theta(npar)
29	FORMAT(
     & ' Parameter',I2,' = ',g13.6,/,
     & '   lower ',F6.2,'-unit interval= ',G13.6,' ( dev= ',g13.6,')')
	if(pon()) write(7,361)ELMAX,elcrit+yout,ELCRIT,YOUT
      if(discprt) write(8,361)ELMAX,elcrit+yout,ELCRIT,YOUT
	print 361,ELMAX,ELCRIT,elcrit+yout,YOUT
361	FORMAT(' L(max)= ',G13.6,'  L(crit)= ',G13.6,/,
     & ' L(limit)= ',g13.6,' (Dev from L(crit)= ',F10.5,')'/,/)
	call flush(7)
C
C LOWER LIMIT DONE (IF REQ). NEXT DO UPPER
50	IF(.NOT.UPPER) GOTO 51		!NEXT PARAMETER
	XLOW=XUL(1,IP)
	XHIGH=XUL(2,IP)
	ND1=1	!PRINT EVERY CYCLE IN BISECT
	IF(debug()) print 55,XLOW,XHIGH
	CALL BISEC0(ELFUNC,XLOW,XHIGH,0.0,XLIM,YOUT,EPSX,EPSY,
     & NERR,ND1,omit)
	if(nerr.eq.0) then
	 if(pon()) write(7,37)NPAR,theta(npar),EM,XLIM,xlim-theta(npar)
       if(discprt) write(8,37)NPAR,theta(npar),EM,XLIM,xlim-theta(npar)
	 print 37,NPAR,theta(npar),EM,XLIM,xlim-theta(npar)
37	 FORMAT(
     & ' Parameter',I2,' = ',g13.6,/,
     & '   upper ',F6.2,'-unit interval= ',G13.6,' ( dev= ',g13.6,')')
	 if(pon()) write(7,361)ELMAX,elcrit+yout,ELCRIT,YOUT
       if(discprt) write(8,361)ELMAX,elcrit+yout,ELCRIT,YOUT
	 print 361,ELMAX,ELCRIT,elcrit+yout,YOUT
	else if(nerr.eq.3) then
	print 371
	  if(pon()) write(7,371)
	  if(discprt) write(8,371)
371	  format(' Bisection aborted')
	else
	print 372,nerr
	  if(pon()) write(7,372) nerr
	  if(discprt) write(8,372) nerr
372	  format(' Guesses do not include result: nerr= ',i4)
	endif
C
51	JFIX(NPAR)=0	!UNFIX THIS PARAM BEFORE GOING TO NEXT
	nfix=nfix-1
48	CONTINUE		!NEXT PARAM
C
999	CONTINUE
	call flush(7)
	RETURN
	END


