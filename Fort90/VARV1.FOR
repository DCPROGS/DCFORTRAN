	SUBROUTINE VARV1(Y,N,YBAR,VARY,ymin,ymax)
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
	DO 1 I=2,N
	YI=Y(I)
	if(yi.gt.ymax) ymax=yi
	if(yi.lt.ymin) ymin=yi
	FI=FLOAT(I)
	SUMY=SUMY+YI
	SUMYY=SUMYY+(FI*YI-SUMY)**2/(FI*(FI-1))
1	CONTINUE
	YBAR=SUMY/FLOAT(N)
	VARY=SUMYY/FLOAT(N-1)
	yi=y(1)
	if(yi.gt.ymax) ymax=yi
	if(yi.lt.ymin) ymin=yi
	RETURN
	END
