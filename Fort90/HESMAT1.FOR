	subroutine HESMAT1(theta,sdev,nfix,jfix,SMIN,SSD,kmax,
     & kfit,kdim,Xobs,yobs,w,nj,juse,setx,niobs,njset)
c Version of HESMAT with extra parameter, kdim,=declared (or allocated)
c dimension of theta,sdev and jfix in calling prog. And internal arrays
c allocated to kmax.
c
c ### ELIK=NAME OF FUNCTION THAT CALCULATES MINUS LOG-LIKELIHOOD
c ### ARGS are SSD,Smin (not ELIK,ELMAX)
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
	real*4 sdev(kdim),THETA(kdim)		!param
	integer jfix(kdim)			!param
	ALLOCATABLE::scal,finfo,covar,unit,np
	real*4 scal(:)
	real*8 finfo(:,:),covar(:,:),unit(:,:)
	integer np(:)
	real*8 det,u,den
c	real*4 scal(20)
c	REAL*8 FINFO(20,20),COVAR(20,20),UNIT(20,20),DET,U,DEN
c	integer NP(20)
	LOGICAL DEBUG,pon
      character*1 ans,UC
	logical discprt,slock,caplock,deb,prt,logyfit,norm
c Params to be passed to SSDCV and Simplex
	real*4 xobs(niobs,njset),yobs(niobs,njset),w(niobs,njset)
	integer juse(njset),nj(njset)
	real setx(njset)
	COMMON/determ/det		!for MATINV
	common/dp/discprt
      COMMON/BLOCK3/logyfit,norm,xnorm,iostat

c###	EXTERNAL EXPLIK,GAULIK,OBLIK
	external SSD
c
	pon()=slock()
	debug()=caplock()
c
101   format(a1)
c	deb=caplock()
	prt=slock()
c
	ALLOCATE(scal(kmax),finfo(kmax,kmax),covar(kmax,kmax),
     & unit(kmax,kmax),np(kmax))
c
	FRACT=-1.0
	print 31
31	format('&Use default step size for error calculation [Y] ? ')
      read 101,ans
      if(UC(ans).ne.'N') goto 10
C
	print 11
11	FORMAT('&Fraction for calc of Hessian = ')
	read 12,FRACT
12	FORMAT(G13.6)
10	CONTINUE
C
c	CALL VMAT2(THETA,KMAX,FRACT,FINFO,COVAR,UNIT,DET,
c     & explik,ELMAX,NFIX,JFIX)
	call VMAT3(theta,Kmax,fract,finfo,covar,unit,det,SSD,
     & smin,nfix,jfix,SCAL,
     & kdim,kmax,Xobs,yobs,w,nj,juse,setx,niobs,njset)
      if(iostat.ge.3.and.iostat.le.9) return
C YCALCV calculat
c
c	print 452,(JFIX(I),I=1,KMAX)
c452	FORMAT(2X,10I8)
c	if(.NOT.debug()) GOTO 13
c	if(.not.deb) goto 13
c
	print 14,(scal(i),i=1,kfit)
	if(prt) write(7,14) (scal(i),i=1,kfit)
	if(discprt) write(8,14) (scal(i),i=1,kfit)
14	FORMAT(' Scale factors= ',10g9.3)
	print 141
	if(prt) write(7,141)
	if(discprt) write(8,141)
141	format(//,
     & ' Observed information matrix = ')
c	DO 372 I=1,KFIT
c	IF(I.GT.6) GOTO 57
c	if(prt) write(7,45)(FINFO(I,J),J=1,I)
c      if(discprt) write(8,45)(FINFO(I,J),J=1,I)
c	print 45,(FINFO(I,J),J=1,I)
c	GOTO 372
c57	if(prt) write(7,45)(FINFO(I,J),J=1,I)
c      if(discprt) write(8,45)(FINFO(I,J),J=1,I)
c	print 45,(FINFO(I,J),J=1,6)
c	if(prt) write(7,451)(FINFO(I,J),J=7,I)
c      if(discprt) write(8,451)(FINFO(I,J),J=7,I)
c	print 451,(FINFO(I,J),J=7,I)
c372	CONTINUE
	do i=1,kfit
	   if(i.le.6) then
		print 45,(FINFO(I,J)/(scal(i)*scal(j)),J=1,I)
		if(prt) write(7,45)(FINFO(I,J)/(scal(i)*scal(j)),J=1,I)
	      if(discprt) write(8,45)(FINFO(I,J)/(scal(i)*scal(j)),J=1,I)
	   else
		if(prt) write(7,45)(FINFO(I,J)/(scal(i)*scal(j)),J=1,I)
      	if(discprt) write(8,45)(FINFO(I,J)/(scal(i)*scal(j)),J=1,I)
		print 45,(FINFO(I,J)/(scal(i)*scal(j)),J=1,6)
		if(prt) write(7,451)(FINFO(I,J)/(scal(i)*scal(j)),J=7,I)
	      if(discprt) write(8,451)(FINFO(I,J)/(scal(i)*scal(j)),J=7,I)
		print 451,(FINFO(I,J)/(scal(i)*scal(j)),J=7,I)
	   endif
	enddo

	print 3711
	if(prt) write(7,3711)
	if(discprt) write(8,3711)
3711	FORMAT(/,' Covariance matrix = ')
	do i=1,kfit
	   if(i.le.6) then
		if(prt) write(7,45)(COVAR(I,J)*scal(i)*scal(j),J=1,I)
	      if(discprt) write(8,45)(COVAR(I,J)*scal(i)*scal(j),J=1,I)
		print 45,(COVAR(I,J)*scal(i)*scal(j),J=1,I)
	   else
		if(prt) write(7,45)(COVAR(I,J)*scal(i)*scal(j),J=1,6)
      	if(discprt) write(8,45)(COVAR(I,J)*scal(i)*scal(j),J=1,6)
		print 45,(COVAR(I,J)*scal(i)*scal(j),J=1,6)
		if(prt) write(7,451)(COVAR(I,J)*scal(i)*scal(j),J=7,I)
	      if(discprt) write(8,451)(COVAR(I,J)*scal(i)*scal(j),J=7,I)
		print 451,(COVAR(I,J)*scal(i)*scal(j),J=7,I)
	   endif
	enddo
c13	CONTINUE
c
C RE-ASSIGN VAR(I) (BEFORE COVAR REDEFINED AS CORRELATION MATRIX)
C SO THAT ALL KMAX VALUES INCLUDED, WITH VAR SET TO -1.0
C FOR FIXED PARAMETERS. ALSO DEFINE LOCAL ARRAY NP(I),I=1,2,..,KFIT
C WITH NP(I)=PARAM NUMBER CORRESPONDING TO I'TH ROW AND COL OF COVAR.
C E.G. IF KMAX=4 AND PARAM 2 IS FIXED NP=(1,3,4)
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
421	format(/,' Correlation matrix=')
	do i=1,kfit
	   do j=1,kfit
		den=covar(i,i)*covar(j,j)		!still scaled
		if(den.gt.1.e-30) then
		   finfo(i,j)=covar(i,j)/sqrt(den)
		else
		   finfo(i,j)=9999.	!den negative
		endif
	   enddo
	enddo
C
c Print correlation matrix
	DO 43 I=1,KFIT
	IF(I.GT.6) GOTO 56
	if(prt) write(7,45) (FINFO(i,j),j=1,i)	!=correlation matrix
      if(discprt) write(8,45) (FINFO(i,j),j=1,i)
	print 45, (FINFO(i,j),j=1,i)
45	FORMAT(2X,6G13.6)
	GOTO 43
56	if(prt) write(7,45) (FINFO(I,J),J=1,6)
      if(discprt) write(8,45) (FINFO(I,J),J=1,6)
	print 45, (FINFO(I,J),J=1,6)
	if(prt) write(7,451)(FINFO(I,J),J=7,I)
      if(discprt) write(8,451)(FINFO(I,J),J=7,I)
	print 451,(FINFO(I,J),J=7,I)
451	FORMAT(15X,5G13.6,/)	!INDENT REMAINING VALUES IF ANY
43	CONTINUE
	if(prt) write(7,46)DET	!det is in common, from matinv
      if(discprt) write(8,46)DET
	print 46,DET
46	FORMAT(/' Determinant of INFO matrix=',G13.6//)
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
	DEALLOCATE(scal,finfo,covar,unit,np)
	RETURN
	END



