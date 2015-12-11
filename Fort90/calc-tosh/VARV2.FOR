	SUBROUTINE VARV2(Y,N,YBAR,VARY,ymin,ymax,nbad)
c VARV2.FOR is version of VARV1.FOR that uses input value of ymin,ymax
c to restrict range -all values outside this range omitted from calc
c of mean and SD. Returns ymin, ymax as min, max of the values that
c were used, and nbad as the number of y(i) that were omitted.
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
	   nbad=0
	   RETURN
	endif
	ymax1=-1.e38
	ymin1=1.e38
	i1=1
	nbad=0
	n1=0
c
2	if(y(i1).ge.ymin.and.y(i1).le.ymax) then	!look for first good value
	   n1=n1+1
	   sumy=Y(i1)
	   sumyy=0.
	   if(y(i1).gt.ymax1) ymax1=y(i1)
	   if(y(i1).lt.ymin1) ymin1=y(i1)
	else
	   i1=i1+1
	   nbad=nbad+1
	   goto 2
	endif
c
c	do 1 i=2,n
	do i=i1+1,n
	   yi=y(i)
	   if(yi.ge.ymin.and.yi.le.ymax) then
		n1=n1+1
		if(yi.gt.ymax1) ymax1=yi
		if(yi.lt.ymin1) ymin1=yi
c		fi=float(i)
		fi=float(n1)
		sumy=sumy+yi
		sumyy=sumyy+(fi*yi-sumy)**2/(fi*(fi-1.))
	   else
		nbad=nbad+1
	   endif
	enddo
c
	Ybar=sumy/float(n1)
	vary=sumyy/float(n1-1)
	if(n1+nbad.ne.n) then
	   print *, 'ERROR IN VARV2'
	endif
c Set ymin, ymax for output
	ymin=ymin1
	ymax=ymax1
	RETURN
	END
