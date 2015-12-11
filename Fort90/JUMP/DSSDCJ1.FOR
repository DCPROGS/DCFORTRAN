	subroutine dSSDCJ1(theta,g1,xfit,yfit,ndv1)
C dFUNC=first derivative if func (=sum of squares) for fitting exponentials
c with dfpmincj in CJFIT.
c SEE dSSD_dp.mcd for method.  Need only the first derivative of the
c function being fitted since
c
c DFPMIN method now fits rate=1/tau, rather than tau values, to avoid
c problem when tau happens to be near zero, but still get problem if
c rate goes much less than zero, because get overflows at large times
c when exp(-xv*rate) has large positive exponent!
c
c dSSD/dp(j) = 2*SUM[(f(x(i)-y(i))*df(x(i)/dp(j)]
c
c where p(j) is jth parameter, f(x(i)) id the function being fitted,
c y(i) are the observations, and sum is over the observations, 1=1,..,n
c
c Need derivative with respect to each parameter in turn, returned in g(),
c in the order in which parameters occur in theta.
c SSD=SUM[amp(i)*exp(t/tau(i)] so derivative with respect to a(i) or tau(i)
c involves only the ith tern (all others are independent of a(i) etc)
c No constraints allowed!
	real THETA(20),g1(kmax)
	real*4 Xfit(ndv1),Yfit(ndv1)		!now alloc in main prog
	allocatable:: Ycalc,Ydev,expa,expt
	real Ycalc(:),Ydev(:),expa(:,:),expt(:,:)
	COMMON/BLOCK1/constr,Xv,kmax,nmod,ncomp,nfit,ip1,ip2,xjoin,slope
	common/err/nerr,silent
	logical silent
c
c To avoid recalculating the Ycalc(i) for every parameter, get array
c of the the Ycalc(i)=f(x(i),theta) first, and use it in subsequent calcs
c (can accumulate SUM(Ycalc-yobs) in same loop, for dSSD/dYinf)
c Can also sve time by creating arrays of values of
c Ydev(i)=Ycalc(i)-Yobs(i)
c expa(i,m)=exp(-x(i)/tau(m))   (m=1,...,ncomp)
c expt(i,m)=[amp(m)*x(i)/tau(m)**2]*exp(-x(i)/tau(m))
c          =[amp(m)*x(i)/tau(m)**2]*expa(i,m)
	ALLOCATE(Ycalc(nfit),Ydev(nfit),
     &  expa(nfit,ncomp),expt(nfit,ncomp))
c
c amps= theta 2,4,6,8,10; theta(2*i-1) tau=theta 1,3,5,7,9
c
	g=0.0
	do i=1,nfit
	   xv=Xfit(i)		!see new Ycalcj
	   yc=0.
	   do m=1,ncomp
		j2=2*m
c===		tau=theta(j2-1)
		rate=theta(j2-1)
		amp=theta(j2)
c		ex=exp(-xv/tau)
		ex=exp2(-xv*rate)
		yc=yc + amp*ex
		expa(i,m)=ex
cc		expt(i,m)=amp*xv*ex/(tau*tau)
c less prone to overflow if written thus:
c		expt(i,m)=(amp/tau)*(xv/tau)*ex
c NB do not just substitute rate for tau! (this would give deriv w.r.t. tau
c expressed in terms of rate) -we need derives w.r.t. rate viz
c (insert check for overflow here!)
c		expt(i,m)= -amp*xv*ex
		z= -amp*xv
		if(ex.gt.1.) then
		   if(abs(z).gt.1.e37/ex) then
			nerr=7
			expt(i,m)= 1.e37
c			RETURN
		   endif
		else
		   expt(i,m)= z*ex
		endif
	   enddo
	   ycalc(i)=yc + theta(kmax)		!add Yinf
	   ydev(i)=ycalc(i) - Yfit(i)
	   g=g + ydev(i)				!accumulate dSSD/dYinf
	enddo
	g1(kmax)=2.0*g		!dSSD/dYinf
c
c Now the rest of the derivatives
	j=0
	do m=1,ncomp		!go through parameters in turn
	   gt=0.0			!for dSSD/d(tau), for current j1
	   ga=0.0			!for dSSD/d(amp)
	   do i=1,nfit
		gt=gt + ydev(i)*expt(i,m)
		ga=ga + ydev(i)*expa(i,m)
	   enddo
	   j=j+1			!parameter # (1,...,kmax-1)
	   g1(j)=2.0*gt		!dSSD/d(tau(m))
	   j=j+1			!parameter # (1,...,kmax-1)
	   g1(j)=2.0*ga		!dSSD/d(amp(m))
	enddo
c
	DEALLOCATE(Ycalc,Ydev,expa,expt)
	RETURN
	END

