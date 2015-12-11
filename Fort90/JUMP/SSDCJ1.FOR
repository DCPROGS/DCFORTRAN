	function SSDCJ1(theta,xfit,yfit,ndv1)
C FUNC=sum of squares for fitting exponentials with dfpmincj in CJFIT
C No constraints allowed!
	real THETA(20)
	real*4 Xfit(ndv1),Yfit(ndv1)		!now alloc in main prog
	real*8 S,s1,dev
	COMMON/BLOCK1/constr,Xv,kmax,nmod,ncomp,nfit,ip1,ip2,xjoin,slope
	common/err/nerr,silent
	logical silent
c
c
c DFPMIN method now fits rate=1/tau, rather than tau values, to avoid
c problem when tau happens to be near zero, but still get problem if
c rate goes much less than zero, because get overflows at large times
c when exp(-xv*rate) has large positive exponent!
c
c Debug -insert prelim check for near zero tau values -still needed when
c fitting rates (see above)
	do j=1,ncomp
	   i1=2*j
c	   tau=theta(i1-1)
	   rate=theta(i1-1)
	   if(rate.lt.0.0) then
		print 1,j,rate
1		format(' rate (1/tau) (',i2,') = ',g13.6,' in SSDCJ1')
c		if(abs(tau).lt.0.1) then
cc		SSDCJ1=1.e20
cc		RETURN
c		tau=0.1
c		theta(i1-1)=tau
		rate=0.00001
		theta(i1-1)=rate
	   endif
	enddo
c
	S=0.0d0
	do i=1,nfit
	   xv=Xfit(i)		!see new Ycalcj
c amps= theta 2,4,6,8,10; theta(2*i-1) tau=theta 1,3,5,7,9
	   ycalc=0.
	   do j=1,ncomp
		i1=2*j
c		tau=theta(i1-1)
		rate=theta(i1-1)
		amp=theta(i1)
c===		ycalc=ycalc + amp*exp2(-xv/tau)
c Put in error returns in case of overflows
c		ex=exp2(-xv/tau)
		ex=exp2(-xv*rate)
		if(ex.gt.1.) then
		   if(amp.gt.1.e37/ex) then
			nerr=1
			RETURN
		   endif
		else
		   term=amp*ex
		endif
		if(term.gt.1) then
		   if(ycalc.gt.1.e37-term) then
			nerr=2
			RETURN
		   endif
		endif
		ycalc=ycalc + term
	   enddo
	   ycalc=ycalc+theta(kmax)		!add Y(inf)
c=	   Yc=Ycalcj(kmax,THETA,xv1,i1)
	   dev=dble(Yfit(i)-Ycalc)
	   S=S + dev*dev
	enddo
40	continue
c
	s1=dabs(s)
	if(s1.gt.1.18d-38.and.s1.lt.3.4d38) then
	   SSDCJ1=sngl(S)
	else if(s1.le.1.18d-38) then
	   sign=1.0
	   if(s.lt.0.0d0) sign=-1.0
	   SSDCJ1=sign*1.18e-38
	else if(s1.ge.3.4d38) then
	   sign=1.0
	   if(s.lt.0.0d0) sign=-1.0
	   SSDCJ1=sign*3.4e38
	endif
	RETURN
	END

