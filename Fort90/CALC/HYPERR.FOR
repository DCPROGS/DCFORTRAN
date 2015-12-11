	subroutine HYPERR(Ymax,aK)
c To calculate approx SD from analytical Hessian matrix in case of
c fit of simple hyerbola (K,Ymax)
c Also to calculate likelihood intervals in simpler way than general
c method in CVFIT (needs only bisection, not iterated simplex)
c Methods are as worked out in SSD-LOB.MCD
c
	real*4 finfo(2,2),cov(2,2)
	logical complex
	common/data1/ n,x(100),y(100),w(100)	!for hyperr/func1/2
	common/data2/ Scrit				!for hyper/func1/2
	external func1,funclo,funchi
c
c First Hessian
c d2(SSD)/dYm^2
	s=0.
	do i=1,n
	   s=s + w(i)*(x(i)/(x(i)+aK))**2
	enddo
	d2Y=2.*s
c
c d2(SSD)/dK^2
	s1=0.
	s2=0.
	do i=1,n
	   s1=s1 + w(i)*x(i)*y(i)/((x(i)+aK)**3)
	   s2=s2 + w(i)*x(i)*x(i)/((x(i)+aK)**4)
	enddo
	d2K=-4.*Ymax*s1 + 6.*Ymax*Ymax*s2
c
c Cross derivative d2(SSD)/dYm dK
c
	s1=0.
	s2=0.
	do i=1,n
	   s1=s1 + w(i)*x(i)*y(i)/((x(i)+aK)**2)
	   s2=s2 + w(i)*x(i)*x(i)/((x(i)+aK)**3)
	enddo
	d2YK=2.*s1 - 4.*Ymax*s2
c
c Info matrix
	finfo(1,1)=0.5*d2Y
	finfo(1,2)=0.5*d2YK
	finfo(2,1)=0.5*d2YK
	finfo(2,2)=0.5*d2K
c Covar matrix
	det=finfo(1,1)*finfo(2,2) - finfo(1,2)*finfo(2,1)
	cov(1,1)=finfo(2,2)/det
	cov(1,2)=-finfo(1,2)/det
	cov(2,1)=-finfo(2,1)/det
	cov(2,2)=finfo(1,1)/det
c Errors (SD)
	if(cov(1,1).gt.0.) then
	   sdY=sqrt(cov(1,1))
	else
	   sdY=-1.
	endif
	if(cov(2,2).gt.0.) then
	   sdK=sqrt(cov(2,2))
	else
	   sdK=-1.
	endif
c
c Correlation matrix -only one element!
	den=cov(1,1)*cov(2,2)
	if(den.gt.0.) then
	   cor=cov(1,2)/sqrt(den)
	else
	   cor=-1.
	endif
c
c
c Likelihood intervals for K.  Find as solutions for K when disc=0 so the
c two solutions for Ymax are identical (i.e. at tangents to the scrit ellipse)
	f=0.0
c Lower limit
	x1=0.
	x2=aK
	epsx=-1.	!ignore
	epsy=0.001	!in log-likelihood units
	ndisp=5
	call BISEC0(FUNC1,x1,x2,f,Xout,Yout,epsx,epsy,
     & nerr,ndisp,.false.)
	if(nerr.ne.0) then
	   print 1,nerr
1	   format(' Error ',i2,' in lower limit for K')
	   call BELL(1)
	endif
	aKlo=xout
c Upper limit
	x1=aK
	if(sdK.gt.0.) then
	   x2=aK+6.*sdK
	else
	   x2=10.*aK
	endif
	f=0.0
	call BISEC0(FUNC1,x1,x2,f,Xout,Yout,epsx,epsy,
     & nerr,ndisp,.false.)
	if(nerr.ne.0) then
	   print 2,nerr
2	   format(' Error ',i2,' in upper limit for K')
	   call BELL(1)
	endif
	aKhi=xout
c
c Limits for Ymax.  Look at scrit contour over the range of K values
c just found, and locate points where dY/dK is zero, say at Kyhi on
c upper limb of contour, Kylo on lower limb of countour.  Limits for
c Ymax are the values of Ymax that correspond to these points
c
c Lower limit for Ymax: first find corresponding K values -check only in the
c region of valid K values, aKlo to aKhi = coordinates of the ends of the contour
c -Need to check that aKlo and aKhi are OK (give disc=>0) -aK1 can be reset
c slightly in funclo, funchi if necessary, to correct rounding errors in
c aKlo, aKhi.

	x1=aKlo
	x2=aK
	epsx=-1.	!ignore
	epsy=0.001	!in log-likelihood units
	ndisp=5
	f=0.0
	call BISEC0(FUNClo,x1,x2,f,Xout,Yout,epsx,epsy,
     & nerr,ndisp,.false.)
	if(nerr.ne.0) then
	   print 3,nerr
3	   format(' Error ',i2,' in lower limit for Ymax')
	   call BELL(1)
	endif
	aKylo=xout
	call Yvals(Ylow,Yhigh,aKylo,Scrit,complex)
	Ylo=Ylow
c Upper limit
c -check aKhi OK?
	x1=aK
	x2=aKhi
c	if(sdK.gt.0.) then
c	   x2=aK+6.*sdK
c	else
c	   x2=10.*Ymax
c	endif
	f=0.0
	call BISEC0(FUNChi,x1,x2,f,Xout,Yout,epsx,epsy,
     & nerr,ndisp,.false.)
	if(nerr.ne.0) then
	   print 4,nerr
4	   format(' Error ',i2,' in upper limit for Ymax')
	   call BELL(1)
	endif
	aYhi=xout
	aKyhi=xout
	call Yvals(Ylow,Yhigh,aKyhi,Scrit,complex)
	Yhi=Yhigh
c
	RETURN
	end



	subroutine Yvals(Ylo,Yhi,aK,Scrit,complex)
c	Returns the both roots for Ymax, for specified K and Scrit
	common/data1/ n,x(100),y(100),w(100)		!for func1/2
	logical complex
c
c  Calculate coefficients of quadratic for give K value (aK) and given Scrit value
c
	s=0.
	do i=1,n
	   s=s + w(i)*(x(i)/(x(i)+aK))**2
	enddo
	den=s
c
	s=0.
	do i=1,n
	   s=s + w(i)*(x(i)*y(i)/(x(i)+aK))
	enddo
	b=-2*s
c
	s=0.
	do i=1,n
	   s=s + w(i)*y(i)*y(i)
	enddo
c Coefficients are
	c= (s - Scrit)/den
	b=b/den
c and solutions are
	disc=b*b - 4*c
	if(disc.gt.0) then
	   Yhi=0.5*(-b+sqrt(disc))
	   Ylo=0.5*(-b-sqrt(disc))
	   complex=.false.
	else
	   complex=.true.
	endif
	RETURN
	end



	function FUNC1(aK1)
	common/data1/ n,x(100),y(100),w(100)		!for func1/2
	common/data2/ Scrit					!for func1/2
	s=0.
	do i=1,n
	   s=s + w(i)*(x(i)/(x(i)+aK1))**2
	enddo
	den=s
c
	s=0.
	do i=1,n
	   s=s + w(i)*(x(i)*y(i)/(x(i)+aK1))
	enddo
	b=-2*s
c
	s=0.
	do i=1,n
	   s=s + w(i)*y(i)*y(i)
	enddo
c Coefficients are
	c= (s - Scrit)/den
	b=b/den
c and solutions are
	disc=b*b - 4*c
	func1=disc
	RETURN
	end


	function FUNClo(aK1)
c to get point where dYlo/dK=0 on the lower limb of the S=Scrit contour
	common/data1/ n,x(100),y(100),w(100)		!for func1/2
	common/data2/ Scrit				!for func1
	logical complex
	common/bad/complex
c
	nbadmax=10
	nbad=0
12	continue
c First get db/dK
	s=0.
	do i=1,n
	   s=s + w(i)*(x(i)/(x(i)+aK1))**2
	enddo
	den=s
	den2=den*den
c
	s=0.
	do i=1,n
	   s=s + w(i)*x(i)*y(i)/((x(i)+aK1)**2)
	enddo
	dudx=2*s
c
	s=0.
	do i=1,n
	   s=s + w(i)*x(i)*y(i)/(x(i)+aK1)
	enddo
	ux=-2*s
	bval=ux/den
c
	s=0.
	do i=1,n
	   s=s + w(i)*x(i)*x(i)/((x(i)+aK1)**3)
	enddo
	dvdx=-2*s

c
	dbdK=(dudx*den - ux*dvdx)/den2
c
c Next get dcdx
	s=0.
	do i=1,n
	   s=s + w(i)*y(i)*y(i)
	enddo
	ux=s - Scrit
	cval=ux/den
c
	dcdK=-ux*dvdx/den2
c
	disc=bval*bval - 4*cval
	if(disc.ge.0) then
	   dval=sqrt(disc)
	   complex=.false.
	else
	   complex=.true.
	   aK1=aK1*1.001	!increase a bit, until not complex!
	   nbad=nbad+1
	   if(nbad.lt.nbadmax) goto 12
	endif
c
c denominator (den*den), and factor of 0.5, omitted from slopes
	dYlodK=-(dbdK*(bval+dval) - 2.*dcdK)
	funclo=dYlodK
	RETURN
	end


	function FUNChi(aK1)
c to get point where dYhi/dK=0 on the upper limb of S=Scrit contour
	common/data1/ n,x(100),y(100),w(100)		!for func1/2
	common/data2/ Scrit				!for func1
	logical complex
	common/bad/complex
c
	nbadmax=10
	nbad=0
12	continue
c First get db/dK
	s=0.
	do i=1,n
	   s=s + w(i)*(x(i)/(x(i)+aK1))**2
	enddo
	den=s
	den2=den*den
c
	s=0.
	do i=1,n
	   s=s + w(i)*x(i)*y(i)/((x(i)+aK1)**2)
	enddo
	dudx=2*s
c
	s=0.
	do i=1,n
	   s=s + w(i)*x(i)*y(i)/(x(i)+aK1)
	enddo
	ux=-2*s
	bval=ux/den
c
	s=0.
	do i=1,n
	   s=s + w(i)*x(i)*x(i)/((x(i)+aK1)**3)
	enddo
	dvdx=-2*s

c
	dbdK=(dudx*den - ux*dvdx)/den2
c
c Next get dcdx
	s=0.
	do i=1,n
	   s=s + w(i)*y(i)*y(i)
	enddo
	ux=s - Scrit
	cval=ux/den
c
	dcdK=-ux*dvdx/den2
c
	disc=bval*bval - 4*cval
	if(disc.ge.0) then
	   dval=sqrt(disc)
	   complex=.false.
	else
	   complex=.true.
	   aK1=aK1*0.999	!decrease a bit, until not complex!
	   nbad=nbad+1
	   if(nbad.le.nbadmax) goto 12
	endif
c
c denominator (den*den), and factor of 0.5, omitted from slopes
	dYhidK=dbdK*(bval-dval) - 2.*dcdK
	funchi=dYhidK
	RETURN
	end
