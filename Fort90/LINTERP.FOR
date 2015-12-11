	subroutine LINTERP(y,x0,dx,n,y1,dx1,n1)
c For linear interpolation of a whole array
c Input:
c y(n) tabulated at interval=dx, with x=x0 for y(1)
c Output:
c y1(n1) tabulated at interval=dx1, with x=x0 for y1(1)
c
c NB input values MUST satisfy the following condition
c Range of x must be same for both arrays
c so xmax=x0+(n-1)*dx=x0+(n1-1)*dx1, so must have
C dx/dx1=(n1-1)/(n-1) i.e. n1=1+ (n-1)*dx/dx1,
c or dx1=dx*(n-1)/(n1-1)
c
	real*4 y(n),y1(n1)
c
	y1(1)=y(1)
	do i=2,n1-1
	  x=x0+float(i-1)*dx1	!time for new point
	  j=1+ifix(x/dx)	      !index in Y of element just below x
	  x1=x0+float(j-1)*dx	!x value for this element
c
	  f1=y(j)
	  if(j.lt.n) then
		f2=y(j+1)
	  else
		f2=y(n)
	  endif
	  b=(f2-f1)/dx	!slope
	  y1(i)=f1 + b*(x-x1)
	enddo
	y1(n1)=y(n)
c
	RETURN
	end

