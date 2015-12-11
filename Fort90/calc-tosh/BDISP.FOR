	subroutine BDISP(pvalue,irun,nrun,x,y,w,n,cmin,cmax,
     &  nctrue,thetrue,theta3,theta4,kmtrue,kmax3,kmax4,ieqn)
c To draw a simulated expt in betzfit, with LS fit -modif from hypdisp1
c
	real*4 x(100),y(100),w(100)
	real theta(20),thetrue(20),theta3(20),theta4(20)
c	real*4 aK(nrun,6),Ym(nrun,6),hill(nrun)
	character*11 cnum1,cnum2
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
	character*64 title1
c
	kwi=100   !dimensions of weight()
	kwj=1
	kth=20		!declared dimension of theta
	ncalc=512
	ndc1=ncalc
	ndimc=5
	ndv1=n
	ndimd=1
c
	ALLOCATE(xval(ndv1,ndimd),yval(ndv1,ndimd),weight(kwi,kwj))
	ALLOCATE(xcal(ndc1,ndimc),ycal(ndc1,ndimc))
c
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
c	iscal=1		!scale internally
	iscal=4
	xmin=cmin
	xmax=cmax
	ymin=0.
	ymax=1.0
c   Calc curves
c
	ncurvc=3
	do i=1,ncurvc
	   icurvc(i)=i
	   iline(i)=0		!continuous
	   ncal(i)=ncalc
	enddo
	iline(1)=0		!continuous
	iline(2)=0		!continuous
	iline(3)=2		!true curve dashed
	dl=(alog10(xmax)-alog10(xmin))/float(ncalc-1)
	if(ieqn.eq.1) then
	   ncomp1=3
	   ncomp2=4
	else if(ieqn.eq.2) then
	   ncomp1=1
	   ncomp2=2
	endif
	do i=1,ncal(1)
	   x1=10**(alog10(xmin)+float(i-1)*dl)
	   xcal(i,1)=x1
	   xcal(i,2)=x1
	   xcal(i,3)=x1
	   ycal(i,1)=BCALC(x1,ncomp1,theta3,kmax3)
	   ycal(i,2)=BCALC(x1,ncomp2,theta4,kmax4)
	   ycal(i,3)=BCALC(x1,nctrue,thetrue,kmtrue)
c	   ycal(i,1)=Ymtrue*x1/(x1+aKtrue)
c	   ycal(i,2)=Ym1*x1/(x1+aK1)
	enddo
	titlex='concentration'
	titley='response'
	ilog=1	!y vs log x
c
	isetcol=0		!default colours
c	isetcol=1		!default if -1 on entry
c	do i=1,100
c	   icol(i)=-1
c	enddo
	xlo=-1		!whole screen
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
	call INTCONV(nctrue,cnum1)
	call DCFORMAT(pvalue,7,4,cnum2)
	if(ieqn.eq.1) then
	   title1='3 (blue), 4 (red) comp fits. True=green='//
     &    charnb(cnum1)//' comp.  P='//charnb(cnum2)
	else if(ieqn.eq.2) then
	   title1='1 (blue), 2 (red) comp fits. True=green='//
     &    charnb(cnum1)//' comp.  P='//charnb(cnum2)
	endif
	ilabel=1
c	iask=2
	iask=-2
	ifitype=0
c
	call VPLOT5(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,symsiz,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,plotonly,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,interp,
     & isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kth,iver)
c
	DEALLOCATE(xval,yval,weight)
	DEALLOCATE(xcal,ycal)
	RETURN
	end




