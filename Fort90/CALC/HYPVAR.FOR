	subroutine HYPVAR(Ymax,aK,sdK,sdY,cor)
c To calculate approx SD from analytical Hessian matrix in case of
c fit of simple hyerbola (K,Ymax), i.e OBSERVED info matrix.
c Methods are as worked out in SSD-LOB.MCD
c
	real*4 finfo(2,2),cov(2,2)
	common/data1/ n,x(100),y(100),w(100)	!for hyperr/func1/2
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
	   cor=-2.
	endif
c
	RETURN
	end


