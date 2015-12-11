	subroutine HESMAT3(theta,sdev,nfix,jfix,SMIN,SSD,kmax,
     & kfit,ndth,Xobs,yobs,w,nj,juse,setx,niobs,njset,cormat,serr)
c
c For use in CVFIT only
c HESMAT3 is version with extra argument, SERR, used to scale the info matrix,
c and hence the covar matrix, and sdev(i).  If weights are used for
c estimation of errors this factor should be 1.0 in call , but if error is
c from residuals then it should be error SD found from residuals (sres,
c defined in CVSIMP).  In latter case this factor was previously put in only
c  when parameter SD calculated, but not in info matrix or covar matrix
c that is found and printed here
c
c HESMAT2 is version of HESMAT1 with option for output in graphical
c mode (controlled by alpha, in common).  Assumes dialog box one set up
c already.  Outputs correlation matrix for display in cvout1.
c
c Version of HESMAT with extra parameter, ndth,=declared (or allocated)
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
	real*4 sdev(ndth),THETA(ndth)		!param
	integer jfix(ndth)			!param
	real*4 cormat(kmax,kmax)
	ALLOCATABLE::scal,finfo,covar,unit,np
	real*4 scal(:)
	real*8 finfo(:,:),covar(:,:),unit(:,:)
	integer np(:)
	character*40 message
	real*8 det,u,den
	logical discprt
c Params to be passed to SSDCV and Simplex
	real*4 xobs(niobs,njset),yobs(niobs,njset),w(niobs,njset)
	integer juse(njset),nj(njset)
	real setx(njset)
	COMMON/determ/det		!for MATINV
	common/dp/discprt
	external SSD
c
101   format(a1)

	ALLOCATE(scal(kmax),finfo(kmax,kmax),covar(kmax,kmax),
     & unit(kmax,kmax),np(kmax))
c
	fract=-1.0
	call VMAT3(theta,Kmax,fract,finfo,covar,unit,SSD,
     & smin,nfix,jfix,SCAL,
     & ndth,kmax,Xobs,yobs,w,nj,juse,setx,niobs,njset)
c
	if(discprt) write(7,14)  (scal(i),i=1,kfit)
14	FORMAT(' Scale factors= ',10g9.3,/10g9.3)
c Incorporate error SD (=1.0 if errors from weights)
	if(serr.ne.1.0) then
	   vard=dble(serr)*dble(serr)
	   do i=1,kfit
		do j=1,kfit
		   finfo(i,j)=finfo(i,j)/vard
		   covar(i,j)=covar(i,j)*vard
		enddo
	   enddo
	endif
c
	if(discprt) write(7,141)
141	format(//,
     & ' Observed information matrix = ')
	do i=1,kfit
	   if(i.le.6) then
	      if(discprt) write(7,45)(FINFO(I,J)/(scal(i)*scal(j)),J=1,I)
	   else
      	if(discprt) write(7,45)(FINFO(I,J)/(scal(i)*scal(j)),J=1,I)
	      if(discprt) write(7,451)(FINFO(I,J)/(scal(i)*scal(j)),J=7,I)
	   endif
	enddo

	if(discprt) write(7,3711)
3711	FORMAT(/,' Covariance matrix = ')
	do i=1,kfit
	   if(i.le.6) then
	      if(discprt) write(7,45)(COVAR(I,J)*scal(i)*scal(j),J=1,I)
45		FORMAT(2X,6G13.6)
	   else
      	if(discprt) write(7,45)(COVAR(I,J)*scal(i)*scal(j),J=1,6)
	      if(discprt) write(7,451)(COVAR(I,J)*scal(i)*scal(j),J=7,I)
451		FORMAT(15X,5G13.6,/)	!INDENT REMAINING VALUES IF ANY
	   endif
	enddo
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
		if(dabs(covar(i1,i1)).lt.1.d37) then	!avoid overflow!
		   c=sngl(covar(i1,i1))
		else
		   if(covar(i1,i1).lt.0.d0) then
			c=-1.e37
		   else
			c=1.e37
		   endif
		endif
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
	if(discprt) write(7,421)
421	format(/,' Correlation matrix=')
	do i=1,kfit
	   do j=1,kfit
		den=covar(i,i)*covar(j,j)		!still scaled
		if(den.gt.1.e-30) then
		   finfo(i,j)=covar(i,j)/sqrt(den)
		else
		   finfo(i,j)=9999.d0	!den negative
		endif
	   enddo
	enddo
C
c Print correlation matrix
c Fixed 02/14/98 12:53pm so OK up to 32 params
	if(kfit.le.8) then
	   do i=1,kfit
      	if(discprt) write(7,48) (FINFO(i,j),j=1,i)
48		format(2x,8f9.6)
	   enddo
	else
	   do i=1,kfit
		if(i.le.11) then
      	   if(discprt) write(7,49) (FINFO(i,j),j=1,i)
49		   format(2x,11f7.4)
		else if(i.le.21) then
      	   if(discprt) write(7,49) (FINFO(I,J),j=1,11)
	         if(discprt) write(7,491)(FINFO(I,J),j=12,i)
491		   format(15x,10f9.6,/)    !indent
		else
      	   if(discprt) write(7,49) (FINFO(I,J),j=1,11)
	         if(discprt) write(7,491)(FINFO(I,J),j=12,21)
	         if(discprt) write(7,491)(FINFO(I,J),j=22,i)
		endif
	   enddo
	endif
      if(discprt) write(7,46)DET
46	FORMAT(/' Determinant of INFO matrix=',G13.6//)
	if(det.lt.1.d-10) then
	   message='Determinant of INFO matrix near zero'
	endif
	DO 471 I=1,KFIT		!IF UNIT O.K. TO 1 IN 1E9 DO NOT PRINT
	DO 471 J=1,KFIT
	U=0.0D0
	IF(I.EQ.J) U=1.0D0
471	IF(DABS(UNIT(I,J)-U).GT.1.0D-9) GOTO 472
	GOTO 50
472	DO 47 I=1,KFIT
      if(discprt) write(7,45)(UNIT(I,J),J=1,KFIT)
47	CONTINUE
50	continue
C
c If in graphics, output correlation matrix for display in cvout1.
	do i=1,kfit
	   do j=1,kfit
		cormat(i,j)=sngl(finfo(i,j))
	   enddo
	enddo
C
	DEALLOCATE(scal,finfo,covar,unit,np)
	RETURN
	END



