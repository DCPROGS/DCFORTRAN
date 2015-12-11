	SUBROUTINE CSIMP(K,THETA,JFIX,AHIST,OBHIST,ELMAX,
     & NEV,ERRFAC,NDISP,errors,idtype)
	REAL THETA(20)
	INTEGER JFIX(20)
	LOGICAL AHIST,OBHIST,errors,pon,slock
	character*1 ans,UC
	logical discprt
	common/dp/discprt
	EXTERNAL EXPLIK,GAULIK,OBLIK
c 06/03/89 02:32pm Lahey version of CSIMP3. Data no longer needed as no
c longer params of SIMPLEX)
c
C Version of CSIMP for EKDIS3
C FOR OVERLAY IN EKDIST TO CALL SIMPLEX
	pon()=slock()
c
	stpfac=0.1	!factor for step size (set in SIMP5V)
c ERRFAC was set in MLFITS)
c
c	DO 1 I=1,K
c	STEP(I)=0.1*theta(I)
c	CRTSTP(I)=ERRFAC*theta(I)
c1	CONTINUE
C
	IF(AHIST) GOTO 17
	if(idtype.eq.14.or.idtype.eq.52) goto 17	!fit gau to Popen dist
	IF(OBHIST) GOTO 401
C LMIN=-LMAX
	nevmax=-30000	!neg so iterations not printed to disc
	call SIMPLEX(k,THETA,stpfac,errfac,NEV,nevmax,
     & ELMIN,EXPLIK,NDISP,JFIX,-1.)
	GOTO 18
C FIT AMPLITUDES
17	CONTINUE
	nevmax=-30000	!neg so iterations not printed to disc
	call SIMPLEX(k,THETA,stpfac,errfac,NEV,nevmax,
     & ELMIN,GAULIK,NDISP,JFIX,-1.)
	GOTO 18
C
C FIT GEOMETRICS AND print RESULTS
401	CONTINUE
	nevmax=-30000	!neg so iterations not printed to disc
	call SIMPLEX(k,THETA,stpfac,errfac,NEV,nevmax,
     & ELMIN,OBLIK,NDISP,JFIX,-1.)
C
18	CONTINUE	!NOW ERRORS?
	print 19
19	FORMAT( ' End of fitting')
	print 20,(theta(i),i=1,k)
20	format(4(5g13.6,/))
c	IF(K.LE.5) print 20,(THETA(I),I=1,K)
c20	FORMAT(1X,5G13.6)
c	IF(K.GT.5) print 21,(THETA(I),I=1,K)
c21	FORMAT(1X,5G13.6,/,5G13.6)
	ELMAX=-ELMIN
	if(pon()) write(7,942) NEV,ELMAX
	if(discprt) write(8,942) NEV,ELMAX
	print 942,NEV,ELMAX
942	FORMAT(I5,' function evaluations.'/'  Maximum=',G13.6)
	errors=.true.
	print 22
22	format(' Calculate errors [N] ? ')
	read 11,ans
11	format(a1)
	if(UC(ans).ne.'Y') errors=.false.
	RETURN
	END

