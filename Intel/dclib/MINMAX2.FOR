	subroutine MINMAX2(xval,yval,ndat,xmin1,xmax1,ymin1,ymax1,
     & logx,logy,ndimd,ncurvd,icurvd,ndv1)
c For Yval want the largest/smallest values in any of the arrays that
c are used -calculated here
c Modif 11/02/96 02:42pm for case of logx or logy.  In this case minmax2
c returns the smallest positive value.  However when x values (say) start
c at negative value and increase in equal increments such that there is
c a 'zero' value, this 'zero' may actually be very small but positive
c so it is returned by minmax2.  Cope with this skipping values that
c are below xcrit (previously 1.e-37) when log=true, where xcrit
c is set to 0.2* difference between last two x values (OK for equally
c spaced x values anyway).  But doesn't work for Y where values may
c have any spacing.
c
c Modif 09/10/94 10:36am for allocatable arrays with ndv1 added to call.
c 07/05/91 09:44pm modified to deal with ndat>2048. Looks only at all points
c (including those skipped when ndelt>1; ndelt value not known here)
c Should be OK if called with Ycal etc (for which ngr=1 always)
c Ndimd=2nd dimension in main prog of xval,yval
	real*4 XVAL(ndv1,ndimd),YVAL(ndv1,ndimd)
	integer ndat(ndimd)	!contain number of points in each array
	integer icurvd(ndimd)	!contain numbers of arrays actually used
	logical logx,logy
c
	ymin1=1.e37
	xmin1=1.e37
	ymax1=-1.e37
	xmax1=-1.e37
	xcrit=1.e-37	!for first x value only
	ycrit=1.e-37
	do j1=1,ncurvd		!scan ncurvd data sets
	   j=icurvd(j1)		!data set to be scanned
	   do i=1,ndat(j)
		xv=xval(i,j)
		yv=yval(i,j)
		if(i.gt.1) then
		   xcrit=0.2*abs(xv-xlast)
		endif
		if(logx.and.xv.lt.xcrit) goto 1
		if(logy.and.yv.lt.ycrit) goto 1
c		if(logy.and.yv.lt.1.e-37) goto 1
c		if(logx.and.xv.lt.1.e-37) goto 1
		if(xv.lt.xmin1) xmin1=xv
		if(xv.gt.xmax1) xmax1=xv
		if(yv.lt.ymin1) ymin1=yv
		if(yv.gt.ymax1) ymax1=yv
1	      continue
		xlast=xv
	   enddo
	enddo
	RETURN
	end
