	function ELFUNC(X,thetsav,xlimsav,youtsav,km,nfits,nfmax)
c
	real*4 thetsav(km,nfmax),xlimsav(nfmax),youtsav(nfmax)
	real theta1(50)
c
	LOGICAL AHIST,THIST,OBHIST
	REAL THETA(50)
	INTEGER JFIX(50)
c	character*1 ans,UC
	COMMON/LIKBLK/Kmax,kfit,elmax,em,delmin,ndisp,theta,npar
	common/FIXBLK/nfix,jfix
c	COMMON/LIKBLK/KMAX,kfit,JFIX,ELMAX,EM,DELMIN,NDISP,THETA,NPAR
	COMMON/LOGBLK/ AHIST,THIST,OBHIST
	common/simp/iconv,nevmax	!so convergence testable in likint1
	EXTERNAL EXPLIK,GAULIK,OBLIK
C
c For likelihood intervals in EKDIST
c
c Modif 11/05/97 07:07am to define initial guesses as for elfunv.for, used
c in cvfit
c
c 06/07/89 05:18pm Lahey version of ELFUNC3
c
c Version of ELFUNC for EKDIS3
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
c Choose the best initial guess for the non-fixed parameters from
c those known to converge OK, stored in thetsav()
	dmin=1.e37
	do j=1,nfits
	   d=abs(x - xlimsav(j))
	   if(d.lt.dmin) then
		dmin=d
		jmin=j
	   endif
	enddo
	do i=1,kmax
	   if(i.ne.npar) theta1(i)=thetsav(i,jmin)
	enddo
c
	theta1(NPAR)=X		!SET TO CURRENT GUESS (FIXED)
c
	if(kfit.eq.1) then
	   if(thist) elmax1= -explik(kmax,theta1)
	   if(ahist) elmax1= -gaulik(kmax,theta1)
	   if(obhist) elmax1= -oblik(kmax,theta1)
	   goto 31
	endif
c
30	continue
	stpfac=0.1
	errfac=1.e-4
	confac=0.5		!parameter for simplex3
	irestrt=3		!maximum of 3 restarts
C N.B.THETA CONTAINS ML ESTIMATES-NOT TO BE CHANGED- SO PUT theta1
C IN CALL TO PTSCH3
	if(thist) call SIMPLEX3(Kmax,theta1,stpfac,errfac,NEV,nevmax,
     & ELMIN,EXPLIK,NDISP,JFIX,delmin,confac,irestrt,iconv)
c
	if(ahist) call SIMPLEX3(Kmax,theta1,stpfac,errfac,NEV,nevmax,
     & ELMIN,gaulik,NDISP,JFIX,delmin,confac,irestrt,iconv)
c
	if(obhist) call SIMPLEX3(Kmax,theta1,stpfac,errfac,NEV,nevmax,
     & ELMIN,oblik,NDISP,JFIX,delmin,confac,irestrt,iconv)
c
	ELMAX1=-ELMIN
31	continue
c
	Yout=ELMAX1-(ELMAX-EM)	!elmax1-elcrit=0, ie elmax1=elcrit
c
c Store output to use as initial guesses for next Simplex, IF this one
c converged
c -or better store input? -these are values known to result in convergence!
	if(iconv.ne.6) then
	   nfits=nfits+1
	   if(nfits.le.nfmax) then
		do i=1,kmax
		   thetsav(i,nfits)=theta1(i)		!store output
c		   thetsav(i,nfits)=theta2(i)		!store input
		enddo
		xlimsav(nfits)=theta1(npar)	!the fixed value
		youtsav(nfits)=Yout
	   endif
	endif
c
	ELFUNC=Yout
	RETURN
	END


