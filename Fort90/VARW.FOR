	subroutine VARW(Y,W,n,ybar,varbar,vary1)
c Modif of VAR.FOR to calc weighted mean
c (version for old compiler).
c If w(i)=1.0/var(y(i) then variance of result is VARBAR=1/sum(w(i))-this is SEM
c If w(i) is constant then var of result is VARY1- the usual unweighted value
c If w(i) has arbitrary relative weights (eg number of values averaged
c to get each input Y(i) then VARY1 is
c	VARY1= SUM[w(i)*(Y(i)-Ybar)**2]/(SUM[w(i)]-1)
c eg 	VARY1= SUM[n(i)*(Y(i)-Ybar)**2]/(N-1)
c where Ybar is the weighted mean- this version makes sense ONLY if the
c w(i) are integers- the numbers averaged (if w(i) were small the
c denom could be zero or negative)
c
C	CALCULATES MEAN,YBAR,AND VARIANCE,VARY OF FIRST N
C	ELEMENTS OF Y. SINGLE PASS YOUNGS AND CRAMER METHOD.
	dimension Y(n),W(n)
	logical caplock
c
c	SUMY=Y(1)
c	SUMYY=0.
c
c	do 1 i=2,n
c	YI=Y(i)
c	FI=FLOAT(i)
c	SUMY=SUMY+YI
c	SUMYY=SUMYY+(FI*YI-SUMY)**2/(FI*(FI-1))
c1	CONTINUE
c
	sumw=0.
	sumy=0.
	do 2 i=1,n
	sumy=sumy+w(i)*Y(i)
	sumw=sumw+w(i)
2	continue
c
	YBAR=SUMY/sumw
	VARBAR=1.0/sumw
c
	sumyy=0.
	do 3 i=1,n
3	sumyy=sumyy+w(i)*Y(i)*Y(i)
c
	sumyy=sumyy - (sumy*sumy)/sumw
c	if(caplock()) print 4,sumyy,sumw,sumy*sumy/sumw
c4	format(' sumyy,sumw,sumy*sumy/sumw=',3g13.6)
	if(sumw.gt.0.0) then
		VARY1=sumyy/(sumw-1.0)
	else
		vary1=-1.
	endif
	RETURN
	END
