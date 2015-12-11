	subroutine SPLINE(X,Y,n,yp1,ypn,Y2)
c Cubic spline interpolation from Numerical Recipes
c This routine calculates array of 2nd derivatives, Y2(n), for use by
c subroutine SPLINT below, which does the actual interpolation
c Input= X(n),Y(n) where X(n) is strictly increasing
c        YP1,YPN control what is done at 1st and last points: if called
c	   with these set to 1st deriv of interpolating function at points
c	   1 and n, resp., then these values used.  If either or both set
c	   to >1.e30 then boundary is set for a natural spline with zero
c	   2nd deriv on boundary
c
c=	parameter (nmax=2048)
	ALLOCATABLE::U
	real*4 U(:)
c=	real*4 X(n),Y(n),Y2(n),U(ndim)
	real*4 X(n),Y(n),Y2(n)
c
c	ALLOCATE(U(ndim))
	ALLOCATE(U(n))
	if(yp1.gt.0.99e30) then
	   Y2(1)=0.
	   u(1)=0.
	else
	   y2(1)=-0.5
	   u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
	endif
c
	do 11 i=2,n-1
	  sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
	  p=sig*y2(i-1)+2.
	  y2(i)=(sig-1.)/p
	  u(i)=(6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))/
     &  (x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
11	continue
c
	if(ypn.gt.0.99e30) then
	   qn=0.
	   un=0.
	else
	   qn=0.5
	   un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
	endif
c
	y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
	do 12 k=n-1,1,-1
12	y2(k)=y2(k)*y2(k+1)+u(k)
c
	DEALLOCATE(U)
	RETURN
	end


	subroutine SPLINT(X,Y,Y2,n,klo,khi,xin,yout)
c Given arrays X,Y, and Y2 calc from them in SPLINE, interpolates a single
c value of y=yout for the specified x=xin
	real*4 X(n),Y(n),Y2(n)
c
c Find place in table by bisection: This is optimum for random values of x:if
c called repeatedly with several closely-spaced x values better to keep khi,klo
c from last call and test if they are still OK
c Code down to label 1 is DC attempt (original just set klo=1,khi=n). To
c function like original (full bisection at each call) call with klo=0 (at every
c call; the call alters it). Otherwise specify klo,khi (such that xin is between
c x(klo),x(khi).  If it is found that xin is NOT in fact
c between x(klo),x(khi) then klo decremented, and khi incremented until
c it is, so khi-klo>1 and some bisection will still have to be done(this would
c be slower than full bisection for a random input, but faster if specified
c khi, klo are near correct values, e.g. klo,khi taken from previous call with
c similar xin)
c
	if(xin.lt.x(1).or.xin.gt.x(n)) then
c	   call BELL(1)
	   print 21,xin,x(1),x(n)
21	   format(
     & ' X value out of range in SPLINE (SPLINT) -no extrapolation!',/,
     &	' Xin, X(1), X(n) = ',3g13.6)
	   pause
	endif
c
	if(klo.le.0) then
	   klo=1
	   khi=n
	else
3	   if(x(klo).le.xin.and.x(khi).ge.xin) goto 1
	   klo=klo-1
	   if(klo.lt.1) klo=1
	   khi=khi+1
	   if(khi.gt.n) khi=n
	   goto 3
	endif
c
1	continue
	if(khi-klo.gt.1) then
	   k=(khi+klo)/2
	   if(x(k).gt.xin) then
		khi=k
	   else
		klo=k
	   endif
	   goto 1
	endif
c
	h=x(khi)-x(klo)
	if(h.eq.0.) then
c	   call BELL(1)
	   print 2
2	   format(' X values must be distinct in SPLINE')
	   pause
	endif
c
	a=(x(khi)-xin)/h
	b=(xin-x(klo))/h
	yout=a*y(klo)+b*y(khi)+
     & ((a**3-a)*y2(klo)+(b**3-b)*y2(khi))*(h**2)/6.
	RETURN
	end


