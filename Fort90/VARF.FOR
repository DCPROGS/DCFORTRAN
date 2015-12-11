	subroutine VARF(Y,nfreq,n,ybar,vary1)
c Modif of VARF.FOR to calc weighted mean in case where weights are
c integer freqencies, in nfreq().
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
	integer*4 nfreq(n)
c
	sumw=0.
	sumy=0.
	do i=1,n
	   w=float(nfreq(i))
	   sumy=sumy+w*Y(i)
	   sumw=sumw+w
	enddo
c
	YBAR=SUMY/sumw
c
	sumyy=0.
	do i=1,n
	   w=float(nfreq(i))
	   sumyy=sumyy+w*Y(i)*Y(i)
	enddo
c
	sumyy=sumyy - (sumy*sumy)/sumw
c
	if(sumw.gt.0.0) then
		VARY1=sumyy/(sumw-1.0)
	else
		vary1=-1.
	endif
	RETURN
	END
