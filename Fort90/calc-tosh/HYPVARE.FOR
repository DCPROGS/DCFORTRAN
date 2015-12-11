	subroutine HYPVARe(Ymax,aK,sdK,sdY,cor)
c To calculate SD from EXPECTED info matrix for Hill-Langmuir fits
c  NB Observations not needed for this, only the current ML estimates
	real*4 finfo(2,2),cov(2,2)
	common/data1/ n,x(100),y(100),w(100)	!for hyperr/func1/2
c
c (dY/dYmax)^2
	s=0.
	do i=1,n
	   s=s + w(i)*(x(i)/(x(i)+aK))**2
	enddo
	finfo(1,1)=s
c
c (dY/dK)^2
	s=0.
	do i=1,n
	   z=x(i)+aK
	   s=s + w(i)*(x(i)/(z*z))**2
	enddo
	finfo(2,2)=Ymax*Ymax*s
c
c (dY/dYmax)(dY/dK)
	s=0.
	do i=1,n
	   z=x(i)+aK
	   s=s + w(i)*((x(i)/z)**2)/z
	enddo
	finfo(1,2)=-Ymax*s
	finfo(2,1)=finfo(1,2)
c
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


