	function ELFUNCV(X,Xobs,yobs,w,nj,juse,setx,niobs,njset,
     & theta,jfix,ndth)
	real THETA(ndth)
	integer JFIX(ndth)
	real temp(100)
c Params to be passed to SSDCV and Simplex
	real*4 xobs(niobs,njset),yobs(niobs,njset),w(niobs,njset)
	integer juse(njset),nj(njset)
	real setx(njset)
	COMMON/LIKBLK/kmax,kfit,elmax,em,delmin,ndisp,npar,iw,sres
	EXTERNAL SSDCV
C
c 12/28/89 04:44pm Lahey version of ELFUNC for CVFIT
c
c 11-73 version Sept 1986.
C Note that if KFIT=1, i.e. only one param to be estimated then
c when this is fixed at a trial value there are none to be
c estimated so SIMPLEX not needed- just call EXPLIK etc
C FUNCTION FOR USE IN CALC OF LIKELIHOOD INTERVALS. INPUT X IS
C VALUE FOR THE FIXED PARAM FOR WHICH LIMITS WANTED-IT IS PARAM NUMBER
C NPAR . JFIX SPECIFIES WHICH PARAMETER HAS BEEN
C FIXED (THAT FOR WHICH LIMITS WANTED) (OTHER PARAM WHICH
C HAVE BEEN FIXED THROUGHOUT STAY FIXED OF COURSE).
C	VALUE OF FUNC IS ELMAX1-ELCRIT WHERE ELMAX1= MAX LIK GIVEN THE
C CONSTRAINED PARAMETERS - ELCRIT= L(THETA HAT)-EM. THUS VALUE OF
C FUNC SHOULD BE ZERO AT SOLUTION FOR EM-UNIT INTERVAL
C	NEED INITIAL GUESSES (IN BP) FOR THE PARAM FOR THIS CONSTRAINED
C FIT. THE FIXED PARAM IS SET TO THE CURRENT GUESS FOR ITS (UPPER OR
C LOWER) LIMIT. FOR THE OTHER PARAM PROB CANNOT DO BETTER THAN
C USE ML VALUES.
C	SPECIFICATION OF ERROR OF MINIMISATION TRICKY SINCE IT IS
C THE MIN THAT WE WANT,NOT THE PARAM VALUES AT THE MIN
C
c
	DO 19 I=1,KMAX
	IF(I.EQ.NPAR) GOTO 19
	temp(I)=THETA(I)		!RESET ALL TO ML VALUES
c	print 24,I,temp(I)
c24	FORMAT( ' temp(',I2,')= ',G13.6,' CHANGE IT? ')
c	read 25,ans
c25	format(a1)
c	if(UC(ans).EQ.'N') GOTO 19
c	print 22,I
c22	FORMAT( ' temp(',I2,')= ')
c	read 23,temp(I)
c23	FORMAT(G13.6)
19	CONTINUE
	temp(NPAR)=X		!SET TO CURRENT GUESS (FIXED)
C
	if(kfit.gt.1) goto 30
c
c NB SSDCV gives -2*log likelihood
	elmax1=-0.5*SSDCV(kmax,temp,Xobs,yobs,w,nj,juse,setx,niobs,njset)
	goto 31
c
30	continue
	stpfac=0.1
	errfac=1.e-4
	irestrt=3
	confac=0.5		!parameter for simplex3
c20	FORMAT( ' IN ELFUNC: GUESS= ',G13.6)
C N.B.THETA CONTAINS ML ESTIMATES-NOT TO BE CHANGED- so put TEMP
C in call to SIMPLEX
	call SIMPLEXv(kmax,temp,stpfac,errfac,nev,15000,
     & Smin,SSDCV,ndisp,jfix,delmin,confac,irestrt,iconv,
     & Xobs,yobs,w,nj,juse,setx,niobs,njset,ndth)
c NB SSDCV gives -2*log likelihood
	elmax1=-smin/2.0
c	ELMAX1=-ELMIN
c	print 21,NEV,ELMAX1,(TEMP(I),I=1,KMAX)
c21	FORMAT(' EVALUATIONS,LMAX1= ',I8,G13.6,
c     & '  PARAMETERS=',/,7G10.3)
c	ELFUNCV=ELMAX1-(ELMAX-EM)
31	continue
	if(iw.eq.1.or.iw.eq.5) elmax1=elmax1/(sres**2)	!use Sres as error var
c
	ELFUNCV=ELMAX1-(ELMAX-EM)	!elmax1-elcrit=0, ie elmax1=elcrit
c
	RETURN
	END


