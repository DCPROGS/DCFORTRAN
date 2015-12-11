	subroutine HYPDISP1(aK,Ym,hill,irun,nrun,x,y,w,n,aKtrue,Ymtrue,
     & aKmax,Ymmax,hillmax)
c To draw a simulated expt in hypfit, with LS fit (and other fits?)
c If irun>0 draws data for expt #irun (+ fitted curve and true curve)
c If irun=-1 draws Ym vs K to show correlation
c
	real*4 x(100),y(100),w(100)
	real*4 aK(nrun,6),Ym(nrun,6),hill(nrun)
c For vplot
	allocatable:: XVAL,YVAL,weight
	real XVAL(:,:),YVAL(:,:),weight(:,:)		!for VPLOT
	allocatable:: ycal,xcal
	real*4 ycal(:,:),xcal(:,:)
	integer ndat(1),icurvd(1),isym(1),ijoin(1)
	integer icurvw(1)		!new for VPLOT5
	real symsiz(1)
c for calc curves
	integer ncal(5),icurvc(5),iline(5)
	logical fitted,doframe,autplt,interp,landscap
	character*40 titlex,titley
	character*44 title1
	real theta(1)	!not used
c
	kwi=100   !dimensions of weight()
	kwj=1
	kth=1		!declared dimension of theta
	ncalc=200
	if(irun.gt.0) then
	   ndc1=ncalc
	   ndimc=5
	   ndv1=n
	   ndimd=1
	   aK1=aK(irun,5)
	   Ym1=Ym(irun,5)
	else if(irun.le.-1) then		!plot Ym vs K etc
	   ndc1=1
	   ndimc=1
	   ndv1=nrun
	   ndimd=1
	endif
c
	ALLOCATE(xval(ndv1,ndimd),yval(ndv1,ndimd),weight(kwi,kwj))
	ALLOCATE(xcal(ndc1,ndimc),ycal(ndc1,ndimc))
c
	if(irun.gt.0) then
	   ncurvd=1
	   ndat(1)=n
	   do i=1,n
		xval(i,1)=x(i)
		yval(i,1)=y(i)
		weight(i,1)=w(i)
	   enddo
	   isym(1)=-7	!circle
	   symsiz(1)=2.5
	   do i=1,1
		icurvd(i)=i
		ijoin(i)=0
		ijoin(i)=-1	!do not join data points
		icurvw(i)=1
	   enddo
c  ISCAL=4 if input values of xmin,xmax,ymin,ymax to be used; rest internal
c	   iscal=1		!scale internally
	   iscal=4
	   xmin=0.01
	   xmax=100.
	   ymin=0.
	   ymax=1.5
c      Calc curves
c
c	   call FIXAX(aK1,aK2,xmin,xmax,xtic,0)	!always non-log
c	   call FIXAX(ymin1,ymax1,ymin,ymax,ytic,0)
	   ncurvc=2
	   do i=1,2
		icurvc(i)=i
		iline(i)=0		!continuous
	 	ncal(i)=ncalc
	   enddo
	   iline(1)=2		!true curve dashed
	   iline(2)=0		!continuous
	   ncalc=200
	   dl=(alog10(xmax)-alog10(xmin))/float(ncalc-1)
	   do i=1,ncal(1)
		x1=10**(alog10(xmin)+float(i-1)*dl)
		xcal(i,1)=x1
	 	xcal(i,2)=x1
		ycal(i,1)=Ymtrue*x1/(x1+aKtrue)
		ycal(i,2)=Ym1*x1/(x1+aK1)
	   enddo
	   titlex='concentration'
	   titley='occupancy'
	   ilog=1	!y vs log x
	else if(irun.le.-1) then		!plot Ym vs K etc
	   ncurvd=1
c=	   ndat(1)=nrun
	   isym(1)=0	!point
	   symsiz(1)=2.5
	   do i=1,1
		icurvd(i)=i
c		ijoin(i)=0
		ijoin(i)=-1	!do not join data points
		icurvw(i)=-1
	   enddo
	   ilog=0
	   iscal=1		!scale internally
c	   iscal=4
c	   xmin=0.01
c	   xmax=100.
c	   ymin=0.
c	   ymax=1.5
c      Calc curves
	   ncurvc=0
	   if(irun.eq.-1) then		!plot Ym vs K for hyperbola
		nv=0
		do i=1,nrun
		   aK1=aK(i,5)
		   ym1=Ym(i,5)
		   if(aK1.lt.aKmax.and.Ym1.lt.Ymmax) then
			nv=nv+1
			xval(nv,1)=aK1
			yval(nv,1)=Ym1
c=			weight(i,1)=
		   endif
		enddo
		ndat(1)=nv
		titlex='K value'
		titley='Ymax value'
		title1='K vs Ymax for hyperbola fit'
	   else if(irun.eq.-2) then		!plot Ym vs K for Hill eqn
		nv=0
		do i=1,nrun
		   aK1=aK(i,6)
		   ym1=Ym(i,6)
		   if(aK1.lt.aKmax.and.Ym1.lt.Ymmax) then
			nv=nv+1
			xval(nv,1)=aK1
			yval(nv,1)=Ym1
c=			weight(i,1)=
		   endif
		enddo
		ndat(1)=nv
		titlex='K value'
		titley='Ymax value'
		title1='K vs Ymax for Hill fit'
	   else if(irun.eq.-3) then		!plot Ym vs K for Hill eqn
		nv=0
		do i=1,nrun
		   aK1=aK(i,6)
		   hill1=hill(i)
		   if(aK1.lt.aKmax.and.hill1.lt.hillmax) then
			nv=nv+1
			xval(nv,1)=aK1
			yval(nv,1)=hill1
c=			weight(i,1)=
		   endif
		enddo
		ndat(1)=nv
		titlex='K value'
		titley='Hill coeff'
		title1='K vs nHill for Hill fit'
	   else if(irun.eq.-4) then		!plot Ym vs K for Hill eqn
		nv=0
		do i=1,nrun
		   Ym1=Ym(i,6)
		   hill1=hill(i)
		   if(Ym1.lt.Ymmax.and.hill1.lt.hillmax) then
			nv=nv+1
			xval(nv,1)=Ym1
			yval(nv,1)=hill1
c=			weight(i,1)=
		   endif
		enddo
		ndat(1)=nv
		titlex='Ymax value'
		titley='Hill coeff'
		title1='Ymax vs nHill for Hill fit'
	   endif
	endif
c
	isetcol=0		!default colours
c	isetcol=1		!default if -1 on entry
c	do i=1,100
c	   icol(i)=-1
c	enddo
	ixlo=-1		!whole screen
	ntx=5
	nty=5
	itx=1		!ticks orientated normally
	ity=1
	doframe=.true.
	landscap=.true.
	autplt=.false.
	ncjump=0
	nvjump=0
	autplt=.false.
	fitted=.false.
	itrace=0
	cbig=2.5
	ifont=4
	itit=1
	ilabel=1
c	iask=2
	iask=-2
c
	call VPLOT5(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,symsiz,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & iXLO,iXHI,iYLO,iYHI,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,plotonly,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,interp,
     & isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kth)
c
	DEALLOCATE(xval,yval,weight)
	DEALLOCATE(xcal,ycal)
	RETURN
	end




