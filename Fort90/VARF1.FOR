	subroutine VARF1(Y,wfreq,n,ybar,vary,sumw,ymin,ymax)
c Modif of VARF.FOR to calc weighted mean in case where weights are
c integer freqencies, in wfreq(), and also return ymin,ymax.
c If w(i) has arbitrary relative weights (eg number of values averaged
c to get each input Y(i) then VARY1 is
c	VARY1= SUM[w(i)*(Y(i)-Ybar)**2]/(SUM[w(i)]-1)
c where Ybar is the weighted mean- this version makes sense ONLY if the
c w(i) are integers- the numbers averaged (if w(i) were small the
c denom could be zero or negative)
c
C	CALCULATES MEAN,YBAR,AND VARIANCE,VARY OF FIRST N
C	ELEMENTS OF Y. SINGLE PASS YOUNGS AND CRAMER METHOD.
	real*4 Y(n)
	real*4 wfreq(n)
c
	sumw=0.
	sumy=0.
	ymax=-1.e38
	ymin=1.e38
	do i=1,n
	   w=wfreq(i)
	   sumy=sumy+w*Y(i)
	   sumw=sumw+w
	   if(y(i).gt.ymax) ymax=y(i)
	   if(y(i).lt.ymin) ymin=y(i)
	enddo
c
	YBAR=SUMY/sumw
c
	sumyy=0.
	do i=1,n
	   w=wfreq(i)
	   sumyy=sumyy+w*Y(i)*Y(i)
	enddo
c
	sumyy=sumyy - (sumy*sumy)/sumw
c
	if(sumw.gt.0.0) then
		VARY=sumyy/(sumw-1.0)
	else
		vary=-1.
	endif
	RETURN
	END
