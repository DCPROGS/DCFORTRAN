	subroutine COUNTDAT(j,ndat,ndelt,xval,yval,logx,logy,logity,sqrty,
     & y0,yinf,ymin,ymax,x0,xmin,xmax,ndimd,NDISP,ndv1)
	logical logx,logy,logity,sqrty
	real*4 XVAL(ndv1,ndimd),YVAL(ndv1,ndimd)
	integer ndat(ndimd)
c
c Subroutine, needed only if ndat(j)>2048, to calculate NDISP=number of points
c in display range when every NDELTth point used (eg call with ndelt=1 to get
c ndisp from which calculate new ndelt in order to keep ndisp =< 2048)
c Modif 09/10/94 10:36am for allocatable arrays with ndv1 added to call.
c
	ndisp=0
c
	do 215 i=1,ndat(j),ndelt
	 xv=xval(i,j)
	 yv=yval(i,j)
	 if(logity) then
	   if(abs(yinf-yv).lt.1.e-30) goto 215	!for case of yv=yinf
	   if(yv.gt.yinf-x0) goto 215	!skip values above ymax!
	   if(yv.lt.y0+x0) goto 215	!skip values below ymin!
	   yv=(yv-y0)/(yinf-yv)	! Hill scale
	   if(yv.gt.x0) then
		yv=alog10(yv)
	   endif
	 endif
	 if(logx) then
	   if(xv.gt.x0) then
		xv=alog10(xv)
	   else
		xv=2.*xmax		!so not plotted
	   endif
	 endif
	 if(logy) then
	   if(yv.gt.x0) then
		yv=alog10(yv)
	   else
		yv=2.*ymax		!so not plotted
	   endif
	 endif
	 if(sqrty.and.yv.ge.0.) yv=sqrt(yv)
c  xv,yv now defined but don't count if outside min,max range
	 if(xv.lt.xmin.or.xv.gt.xmax) goto 215
	 if(yv.lt.ymin.or.yv.gt.ymax) goto 215
	 NDISP=NDISP+1
215	 continue
c
	RETURN
	end
