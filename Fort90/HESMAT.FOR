	subroutine HESMAT(theta,sdev,nfix,jfix,SMIN,SSD,kmax,
     & kfit,fract)
c	subroutine HESMAT(theta,sdev,nfix,jfix,elmax,kmax,kfit,ncomp)
c ARGS are SSD,Smin (not ELIK,ELMAX)
c 06/06/89 08:00pm Lahey version
c Version of HMAT1 that
c (a) takes up to 20 parameters
c (b) scales parameters internally for better calc of derivatives (as
c	in HMAT2.FOR on PDP-11)
c Used with VMAT2 which returns scaled FINFO, COVAR + scaling factors
c in SCAL(i),i-1,...,kfit. (Note that a parameter like g(0)=1.e-30 with
c sd=1.e-31 would have variance=1.e-62 which will underflow, so must keep
c in scaled form until last moment)
c Also returns SDEV= standard dev of params (corrected for scaling) rather
c than variance as in other versions
c
c Modif 06/08/95 12:11pm so that fract is an argument.  If called with
c  fract=-1. then uses default (=1% change in ssd)
c  fract<-1.5 than asks what fraction to use
c  fract>0 then uses this value
c
	real*4 sdev(20),THETA(20)		!param
	real*4 scal(20)
	REAL*8 FINFO(20,20),COVAR(20,20),UNIT(20,20),DET,U,DEN
	integer JFIX(20)
c	integer NP(20)
	LOGICAL DEBUG,pon
      character*1 ans,UC
	logical discprt,slock,caplock,deb,prt
	common/dp/discprt
	external SSD
c
	pon()=slock()
	debug()=caplock()
c
101   format(a1)
	deb=caplock()
	prt=slock()
	if(fract.le.-1.5) then
	   print 31
31	   format('&Use default step size for error calculation [Y] ? ')
         read 101,ans
         if(UC(ans).eq.'N') then
		print 11
11  		FORMAT('&Fraction for calc of Hessian = ')
		call INPUTr(fract)
	   endif
	endif
C
c	CALL VMAT2(THETA,KMAX,FRACT,FINFO,COVAR,UNIT,DET,
c     & explik,ELMAX,NFIX,JFIX)
	call VMAT2(theta,Kmax,fract,finfo,covar,unit,det,SSD,
     & smin,nfix,jfix,SCAL)
c	print 452,(JFIX(I),I=1,KMAX)
c452	FORMAT(2X,10I8)
c	if(.NOT.debug()) GOTO 13
	if(.not.deb) goto 13
	print 14,(scal(i),i=1,kfit)
	if(prt) write(7,14) (scal(i),i=1,kfit)
	if(discprt) write(8,14) (scal(i),i=1,kfit)
14	FORMAT(' Scale factors= ',10g9.3,//,
     & ' Observed information matrix (scaled)= ')
	DO 372 I=1,KFIT
	IF(I.GT.6) GOTO 57
	if(prt) write(7,45)(FINFO(I,J),J=1,I)
      if(discprt) write(8,45)(FINFO(I,J),J=1,I)
	print 45,(FINFO(I,J),J=1,I)
	GOTO 372
57	if(prt) write(7,45)(FINFO(I,J),J=1,I)
      if(discprt) write(8,45)(FINFO(I,J),J=1,I)
	print 45,(FINFO(I,J),J=1,6)
	if(prt) write(7,451)(FINFO(I,J),J=7,I)
      if(discprt) write(8,451)(FINFO(I,J),J=7,I)
	print 451,(FINFO(I,J),J=7,I)
372	CONTINUE
	print 3711
3711	FORMAT(/,' Covariance matrix (scaled)= ')
	DO 371 I=1,KFIT
	IF(I.GT.6) GOTO 58
	if(prt) write(7,45)(COVAR(I,J),J=1,I)
      if(discprt) write(8,45)(COVAR(I,J),J=1,I)
	print 45,(COVAR(I,J),J=1,I)
	GOTO 371
58	if(prt) write(7,45)(COVAR(I,J),J=1,6)
      if(discprt) write(8,45)(COVAR(I,J),J=1,6)
	print 45,(COVAR(I,J),J=1,6)
	if(prt) write(7,451)(COVAR(I,J),J=7,I)
      if(discprt) write(8,451)(COVAR(I,J),J=7,I)
	print 451,(COVAR(I,J),J=7,I)
371	CONTINUE
13	CONTINUE
c
C RE-ASSIGN VAR(I) (BEFORE COVAR REDEFINED AS CORRELATION MATRIX)
C SO THAT ALL KMAX VALUES INCLUDED, WITH VAR SET TO -1.0
C FOR FIXED PARAMETERS. ALSO DEFINE LOCAL ARRAY NP(I),I=1,2,..,KFIT
C WITH NP(I)=PARAM NUMBER CORRESPONDING TO I'TH ROW AND COL OF COVAR.
C E.G. IF KMAX=4 AND PARAM 2 IS FIXED NP=(1,3,4)
c	I1=0
c	DO 40 I=1,KMAX
c	IF(JFIX(I).EQ.1) GOTO 411
c	I1=I1+1
c	sdev(i)=sqrt(covar(i1,i1))*scal(i1)	!unscaled SD
cc	VAR(I)=COVAR(I1,I1)
c	NP(I1)=I
c	GOTO 40
c411	sdev(i)=-1.0
c40	CONTINUE
	i1=0
	do i=1,kmax
	   if(jfix(i).eq.0) then
		i1=i1+1
		c=covar(i1,i1)
		if(c.ge.0.0) then
		   sdev(i)=sqrt(c)*scal(i1)	!unscaled SD
		else
		   sdev(i)=-1.0
		endif
	   else
		sdev(i)=-1.0
	   endif
	enddo
C
C CALC CORRELATION MATRIX IN FINFO AND type it (WITHOUT CHANGING COVAR)
	CONTINUE
	print 421
	if(prt) write(7,421)
	if(discprt) write(8,421)
421	FORMAT(/,' Correlation matrix=')
	DO 431 I=1,KFIT
	DO 431 J=1,KFIT
	DEN=COVAR(I,I)*COVAR(J,J)		!still scaled
	IF(DEN.LT.1.E-30) GOTO 432
	FINFO(I,J)=COVAR(I,J)/SQRT(DEN)	!scale factors cancel
	GOTO 431
432	FINFO(I,J)=9999.	!DEN NEGATIVE SO CANNOT CALC SQRT(DEN)
431	CONTINUE
C
c Print correlation matrix
	DO 43 I=1,KFIT
	IF(I.GT.7) GOTO 56
	if(prt) write(7,45) (FINFO(i,j),j=1,i)	!=correlation matrix
      if(discprt) write(8,45) (FINFO(i,j),j=1,i)
	print 45, (FINFO(i,j),j=1,i)
45	FORMAT(2X,7g14.4)
	GOTO 43
56	if(prt) write(7,45) (FINFO(I,J),J=1,7)
      if(discprt) write(8,45) (FINFO(I,J),J=1,7)
	print 45, (FINFO(I,J),J=1,7)
	if(prt) write(7,451)(FINFO(I,J),j=8,i)
      if(discprt) write(8,451)(FINFO(I,J),j=8,i)
	print 451,(FINFO(I,J),j=8,i)
451	FORMAT(11x,6G11.4,/)	!INDENT REMAINING VALUES IF ANY
43	CONTINUE
	if(prt) write(7,46)DET
      if(discprt) write(8,46)DET
	print 46,DET
46	FORMAT( ' Determinant of INFO matrix=',G13.6//)
	DO 471 I=1,KFIT		!IF UNIT O.K. TO 1 IN 1E9 DO NOT PRINT
	DO 471 J=1,KFIT
	U=0.0D0
	IF(I.EQ.J) U=1.0D0
471	IF(DABS(UNIT(I,J)-U).GT.1.0D-9) GOTO 472
	GOTO 50
472	DO 47 I=1,KFIT
	if(prt) write(7,45)(UNIT(I,J),J=1,KFIT)
      if(discprt) write(8,45)(UNIT(I,J),J=1,KFIT)
	print 45,(UNIT(I,J),J=1,KFIT)
47	CONTINUE
C
50	CONTINUE
C
	RETURN
	END



