	SUBROUTINE VARV3(Y,N,YBAR,VARY,ymin,ymax,skew)
c VARV3 is version of VARV1 that als returns skewness index in skew
c VARV1.FOR is version of VARV.FOR that also gives ymin,ymax
C	CALCULATES MEAN,YBAR,AND VARIANCE,VARY OF FIRST N
C	ELEMENTS OF Y. SINGLE PASS YOUNGS AND CRAMER METHOD.
	dimension Y(N)
c
	if(n.eq.1) then
	   ybar=y(1)
	   ymin=y(1)
	   ymax=y(1)
	   vary=0.0
	   RETURN
	endif
	ymax=-1.e38
	ymin=1.e38
	SUMY=Y(1)
	SUMYY=0.
c
	do i=2,n
	   YI=Y(I)
	   if(yi.gt.ymax) ymax=yi
	   if(yi.lt.ymin) ymin=yi
	   FI=FLOAT(I)
	   SUMY=SUMY+YI
	   SUMYY=SUMYY+(FI*YI-SUMY)**2/(FI*(FI-1))
	enddo
	Ybar=sumy/float(n)
	vary=sumyy/float(n-1)
c
	yi=y(1)
	if(yi.gt.ymax) ymax=yi
	if(yi.lt.ymin) ymin=yi
c
c Calculate skewness the slow way (see skew.mcd)
	sy3=0.0
	do i=1,n
	   sy3=sy3 + (y(i)-ybar)**3
	enddo
	skew=sqrt(float(n-1))*sy3/(sumyy**1.5)
c
	RETURN
	END
