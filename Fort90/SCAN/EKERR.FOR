	subroutine EKERR(YLOW,YHIGH,ncomp,nfit,
     & ERRORS,antrue,gamdst,isg)
c
c Modif 06/01/92 10:36am to take 20 parameters (use HESMAT, VMAT2)
c 06/06/89 07:46pm Lahey version
c
C Version of EKERR for EKDIS3
c This is subroutine version for 11-73 of old EKERR1 prog. Now
c linked as subroutine of EKDIST
C EKERR1=VERSION OF SCERR1 FOR EKDIST,EKPLOT
C TO CALCULATE ERRORS IN SINGLE CHANNEL FITTING. CHAINED FROM
C SCDIST AFTER FITTING IN MLFIT. CHAINS BACK TO SCDIST FOR DISPLAY.
C
	real THETA(20)
	INTEGER JFIX(20)
	LOGICAL AHIST,THIST,OBHIST,ERRORS,gamdst
	REAL VAR(20),AREA(20),AMEAN(20),SD(20)
	character*1 ans,UC
c	COMMON/LIKBLK/KMAX,kfit,JFIX,ELMAX,EM,DELMIN,NDISP,THETA,NPAR
	COMMON/LIKBLK/Kmax,kfit,elmax,em,delmin,ndisp,theta,npar
	common/FIXBLK/nfix,jfix
	COMMON/LOGBLK/AHIST,THIST,OBHIST
	EXTERNAL explik,gaulik,oblik,ELFUNC
C
23	continue
	if(ahist.and.kmax.lt.3*ncomp-1) goto 15	!no Hessian if SD equal
	print 14
14	FORMAT('&Likelihood intervals only? ')
	read 11,ans
	if(UC(ans).EQ.'Y') GOTO 15
C CALC COVARIANCE MATRIX
	call HMAT2(THETA,VAR,NFIX,JFIX,ELMAX,KMAX,KFIT,NCOMP)
C
C print RESULTS
	call PTYP2(THETA,VAR,AREA,AMEAN,SD,
     & ncomp,YHIGH,YLOW,NFIT,ANTRUE,.false.)
	goto 21
C
C NOW CALC LIKELIHOOD INTERVALS?
c If Lik intervals ONLY call ptyp1 here to print result of fit.
15	CALL PTYP1(THETA,AREA,AMEAN,SD,
     & KMAX,NCOMP,YHIGH,YLOW,NFIT,ANTRUE,.false.,GAMDST,ISG)
	goto 16
21	print 10
10	FORMAT(' Calculate likelihood intervals? ')
	read 11,ans
11	format(a1)
	if(UC(ans).EQ.'N') GOTO 999
16	CONTINUE
	CALL LIKINT			!all param in COMMON
C
999	CONTINUE
	print 22
22	format(' Another error calculation? ')
	read 11,ans
	if(UC(ans).ne.'N') goto 23
	ERRORS=.FALSE.		!RESET
	RETURN
	END


