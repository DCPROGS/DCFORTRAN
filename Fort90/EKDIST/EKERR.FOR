	subroutine EKERR(YLOW,YHIGH,ncomp,nfit,
     & ERRORS,antrue,gamdst,isg,idtype,ghist,titlep,
     & excreg,yexclow,yexchigh,isdcon,anfit,nsweep,nnull,nfitt)
c
c 11/13/97 03:56pm Modif throughout for 50 paramters!
c    theta(50),theta(50,5), jfix(50), titlep(5)
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
	real THETA(50)
	INTEGER JFIX(50)
	LOGICAL AHIST,THIST,OBHIST,ghist,ERRORS,gamdst,excreg
	REAL VAR(50),AREA(10),AMEAN(10),SD(10)
	character*1 ans,UC
c
	character*20 TITLEP(50)		!names of params for fit
	logical vardef
c
	COMMON/LIKBLK/Kmax,kfit,elmax,em,delmin,ndisp,theta,npar
	common/FIXBLK/nfix,jfix
	COMMON/LOGBLK/AHIST,THIST,OBHIST
	EXTERNAL explik,gaulik,oblik,ELFUNC
C
23	continue
	vardef=.false.
	if(ahist.and.kmax.lt.3*ncomp-1) goto 15	!no Hessian if SD equal
	ans='N'
	call DCASK('Calculate likelihood intervals only',ans,ans)
	if(UC(ans).EQ.'Y') GOTO 15
C CALC COVARIANCE MATRIX
	call HMAT2(THETA,VAR,NFIX,JFIX,ELMAX,KMAX,KFIT,NCOMP)
	vardef=.true.
C
C print RESULTS
	call PTYP2(THETA,VAR,AREA,AMEAN,SD,
     & ncomp,yhigh,ylow,nfit,antrue,idtype,
     & excreg,yexclow,yexchigh,isdcon,anfit)
	goto 21
C
C NOW CALC LIKELIHOOD INTERVALS?
c If Lik intervals ONLY call ptyp1 here to print result of fit.
15	call PTYP1(THETA,AREA,AMEAN,SD,KMAX,NCOMP,YHIGH,YLOW,NFIT,
     & ANTRUE,.false.,GAMDST,ISG,idtype,ahist,thist,obhist,ghist,
     & excreg,yexclow,yexchigh,isdcon,anfit,nsweep,nnull,nfitt)
	goto 16
21	continue
	ans='N'
	call DCASK('Calculate likelihood intervals now',ans,ans)
	if(UC(ans).EQ.'N') GOTO 999
16	CONTINUE
	CALL LIKINT(titlep,vardef,var)	!all other param in COMMON
C
999	CONTINUE
	ans='N'
	call DCASK('Another error calculation',ans,ans)
	if(UC(ans).ne.'N') goto 23
	ERRORS=.FALSE.		!RESET
	RETURN
	END


