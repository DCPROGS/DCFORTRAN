	subroutine SIMDISP(ntj,ttsoj,itsoj,nds,njump,
     & kt1,kt2,tint,iampl,ndim,icur,tval,totcur,nval,
     & calcurv,imode,alpha,QT,p0,vhold,vref,km)
c For display in SCSIM
c imode=1: state transition diagram for present jump + one channel intervals
c imode=2: summed channels
c
c Parameters
c Intervals in the current jump are tint(kt1) to tint(kt2)
	integer itsoj(nds)
	real*4 ttsoj(nds)
	real*4 tint(ndim)
	integer*2 iampl(ndim)		!allocated in main
	integer*2 icur(100,100)
	real*4 tval(nval),totcur(nval)
	real*8 QT(km,km),p0(km)
c
	character*11 cnum
	ALLOCATABLE::tcum
	real*4 tcum(:)
c variables for vplot
	ALLOCATABLE::XVAL,YVAL,XCAL,YCAL,weight,theta
	real*4 XVAL(:,:),YVAL(:,:)		!for VPLOT
	real*4 XCAL(:,:),YCAL(:,:)		!for VPLOT/VHIST
	real*4 weight(:,:)
	real*4 theta(:)
c for data
c	integer ndat(ndimd),icurvd(ndimd),icurvw(ndimd),isym(ndimd),ijoin(ndimd)
	ALLOCATABLE::ndat,icurvd,icurvw,isym,ijoin,syms
	integer ndat(:),icurvd(:),icurvw(:),isym(:),ijoin(:)
	real*4 syms(:)
c for calc curves
	ALLOCATABLE::ncal,icurvc,iline
	integer ncal(:),icurvc(:),iline(:)
	LOGICAL fitted,doframe,plotonly,autplt,interp,landscap,ivplot
	character*40 titlex,titley
	character*64 title1
c
	COMMON/KBLK/KA,KB,KC,KD
	COMMON/cols/icol(100),mono
c
c For mouse
	logical mouse_on
c	logical mous_set
	common/mousval/mouse_on,nbutton
c
	logical alpha,calcurv
c  for drawing conc profile
	logical step,pulse,varconc
c 	common/cconc/ split into 2 so conc0 etc can be common with main prog
	common/cconc1/conc0(10),conc1(10),conc2(10),nljump !for funcd etc and main
	common/cconc2/step,pulse,varconc,trise2,tdec2,tpulse2,
     & cmax,nshape,u,arat,brat,tabs				!for funcd etc
c=	common/cconc/step,pulse,varconc,trise,tdec,tpulse,
c=     & cmax,xa0,xa1,xa2,nshape,u,arat,brat,tabs	!for funcd
c
	if(alpha) then		!go into graphics
	   pause 'hit any key for graph'
	   call SETMOUSE()	!define values in common\mousval\ (in IVLIB)
	   call GINO
	   call VGA
	   alpha=.false.
	endif
c
	k=kA+kB+kC+kD
c
	isetcol=0
	itrace=0
	cbig=2.5
	ifont=4
	ifitype=0		!no display of fitted parameters
	idiskq=-1
	ilog=0	!arithmetic
	ilabel=1	!default pos for axis labels
c	iask=-2
	iask=2	!leave on screen
	ncjump=0
	nvjump=0
	autplt=.false.
	fitted=.false.
	plotonly=.false.
	interp=.false.
	ivplot=.false.
	doframe=.true.
	landscap=.true.
	iscal=1		!scale internally
	xlo=-1.		!whole screen
	ntx=5
	nty=5
	itx=1		!ticks orientated normally
	ity=1
c
	if(imode.eq.1) then
c
	   ndimd=1		!no data
	   ndv1=1
	   kwi=1		!no weights
	   kwj=1
	   ndth=1		!no parameters
	   ndimc=2			!2 curves (state trace and single channel)
	   ndc1=2*(ntj-1)+2	!number of points for state trace
	   ALLOCATE(xval(ndv1,ndimd),yval(ndv1,ndimd))
	   ALLOCATE(xcal(ndc1,ndimc),ycal(ndc1,ndimc))
	   ALLOCATE(weight(kwi,kwj),theta(ndth))
	   ALLOCATE(ndat(ndimd),icurvd(ndimd),icurvw(ndimd),
     &	isym(ndimd),ijoin(ndimd),syms(ndimd))
	   ALLOCATE(ncal(ndimc),icurvc(ndimc),iline(ndimc))
c Generate points for the state transition curve (j=1)
c       make cumulative version of ttsoj
	   ALLOCATE(tcum(ntj))
	   tcum(1)=ttsoj(1)
	   do i=2,ntj
		tcum(i)=tcum(i-1)+ttsoj(i)
	   enddo
	   off=float(k)+1.		!trace offset=number of states+1
	   n1=2*(ntj-1)+2		!must be even
	   j=1
	   m=0
	   xcal(1,j)=0.0			!first one
	   ycal(1,j)=-float(itsoj(1))+off
	   do i=2,n1-2,2	!n1-2 is even
		m=m+1
		xcal(i,j)=tcum(m)	!cumulative time
		xcal(i+1,j)=tcum(m)
		ycal(i,j)=-float(itsoj(m))+off
		ycal(i+1,j)=-float(itsoj(m+1))+off
	   enddo
	   xcal(n1,j)=tcum(ntj)			!last one
	   ycal(n1,j)=ycal(n1-1,j)		!last one
c
c Generate points for single channel trace (j=2)
c       make cumulative version of relevant elements of tint()
	   amax=float(icur(1,1))
	   if(kA.gt.1) then
		do i=2,kA
		   a=float(icur(i,1))
		   if(a.gt.amax) amax=a
		enddo
	   endif
	   ktj=kt2-kt1+1	!number of intervals in this jump
	   tcum(1)=tint(kt1)
	   do i=2,ktj
		tcum(i)=tcum(i-1)+tint(kt1+i-1)
	   enddo
	   n2=2*(ktj-1)+2
	   off=float(k)+1.		!trace offset=number of states+1
	   j=2
	   m=0
	   m1=kt1-1
	   xcal(1,j)=0.0			!first one
	   ycal(1,j)=off+float(iampl(kt1))/amax
	   do i=2,n2-2,2	!n2-2 is even
		m=m+1
		m1=m1+1
		xcal(i,j)=tcum(m)	!cumulative time
		xcal(i+1,j)=tcum(m)
		ycal(i,j)=off+float(iampl(m1))/amax	!so channel amp=0 to 1
		ycal(i+1,j)=off+float(iampl(m1+1))/amax
	   enddo
	   xcal(n2,j)=tcum(ktj)			!last one
	   ycal(n2,j)=ycal(n2-1,j)		!last one
c
	   ncurvd=0
	   ncurvc=2
	   ncal(1)=n1
	   ncal(2)=n2
	   do j=1,ncurvc
		icurvc(j)=j
		iline(j)=0		!0 for continuous
	   enddo
c
 	   titlex='time (ms)'
	   titley='state'
	   itit=1		!so prev title used
	   call INTCONV(njump,cnum)
	   title1='Jump #'//charnb(cnum)//
     &	': state transitions (blue) and channel (red)'
	   nty=1		!one tick/unit
	   xmin1=0.
	   xmax1=tcum(ktj)
	   call FIXAX(xmin1,xmax1,xmin,xmax,xtic,0)	!always non-log
	   call FIXAX(ymin1,ymax1,ymin,ymax,ytic,0)
	   xmin=xmin-0.5*xtic
	   xmax=xmax+0.5*xtic
c  ISCAL=2 if input values of xmin,xmax only to be used; others internally set
	   iscal= 2
c
	   call VPLOT5(xval,yval,ndat,icurvd,ncurvd,ijoin,syms,ndimd,
     & xcal,ycal,ncal,icurvc,ncurvc,iline,ndimc,isym,ilog,iscal,
     & xmin,xmax,ymin,ymax,xtic,ytic,xcross,ycross,ntx,nty,itx,ity,
     & xlo,xhi,ylo,yhi,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,plotonly,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,interp,
     & isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kmax,iver)
	   DEALLOCATE(tcum)
	   DEALLOCATE(xval,yval,xcal,ycal,weight,theta)
	   DEALLOCATE(ndat,icurvd,icurvw,isym,ijoin,syms)
	   DEALLOCATE(ncal,icurvc,iline)
	endif
c
	if(imode.eq.2) then
	   ndimd=1
	   ndv1=nval	!=number of data points
	   kwi=1		!no weights
	   kwj=1
	   ndth=1		!no parameters
	   ndimc=1		!for conc profile
	   if(calcurv) ndimc=2	!for theoretical curve
	   ncalc=512
	   ndc1=ncalc+1		!may need extra point for pulse
	   ALLOCATE(xval(ndv1,ndimd),yval(ndv1,ndimd))
	   ALLOCATE(xcal(ndc1,ndimc),ycal(ndc1,ndimc))
	   ALLOCATE(weight(kwi,kwj),theta(ndth))
	   ALLOCATE(ndat(ndimd),icurvd(ndimd),icurvw(ndimd),
     &	isym(ndimd),ijoin(ndimd),syms(ndimd))
	   ALLOCATE(ncal(ndimc),icurvc(ndimc),iline(ndimc))
c
	   j=1
	   ymin=1.e37
	   ymax=-1.e37
	   do i=1,nval
		xval(i,j)=tval(i)
		y=totcur(i)
		yval(i,j)=y
		if(y.gt.ymax) ymax=y
		if(y.lt.ymin) ymin=y
	   enddo
c scale/offset the conc profile and set colours
c no point in drawing conc profile for step
	   if(step) then
		tmin=tval(1)
		tmax=tval(nval)
		goto 10
	   else if(pulse) then
		xcal(1,j)=0.
		ycal(1,j)=xA0
		xcal(2,j)=0.
		ycal(2,j)=xA1
		xcal(3,j)=tpulse
		ycal(3,j)=xA1
		xcal(4,j)=tpulse
		ycal(4,j)=xA2
		tmin=tpulse
		tmax=tval(nval)
		dt=(tmax-tmin)/float(ncalc-1)
		do i=5,ncalc
		   t=tmin+float(i-1)*dt
		   xcal(i,j)=t
		   ycal(i,j)=xA2
		enddo
		ycmin=xA0
		ycmax=xA1
		if(ycmin.gt.ycmax) then
		   ycmin=xA1
		   ycmax=xA0
		endif
	   else if(varconc) then
		tmin=tval(1)
		tmax=tval(nval)
		dt=(tmax-tmin)/float(ncalc-1)
		ycmin=1.e37
		ycmax=-1.e37
		do i=1,ncalc
		   t=tmin+float(i-1)*dt
		   xcal(i,j)=t
		   y=calconc(t)
		   ycal(i,j)=y
		   if(y.gt.ycmax) ycmax=y
		   if(y.lt.ycmin) ycmin=y
		enddo
	   endif
c scale/offset the conc profile and set colours; for conc profile make the
c amplitude say 20% that of current
	   scal=0.2*(ymax-ymin)/(ycmax-ycmin)
	   off=ymax+0.1*(ymax-ymin)
	   ymax=-1.e37
	   do i=1,ncalc
		y=off + scal*ycal(i,j)
		ycal(i,j)=y
		if(y.gt.ymax) ymax=y	!new ymax to leave room for conc profile
	   enddo
	   isetcol=1
	   do i=1,100
		icol(i)=-1
	   enddo
	   icol(11)=12	!red for conc
	   icol(12)=11	!pale blue for calc curve
	   ymin1=ymin
	   ymax1=ymax
	   call FIXAX(ymin1,ymax1,ymin,ymax,ytic,0)
10	   continue
	   xmin1=0.
	   xmax1=tmax
	   call FIXAX(xmin1,xmax1,xmin,xmax,xtic,0)	!always non-log
	   xmin=xmin-0.5*xtic
	   xmax=xmax+0.5*xtic
c  ISCAL=4 if input values of xmin,xmax,ymin,ymax to be used; rest internal
	   iscal=4
c  ISCAL=2 if input values of xmin,xmax only to be used; others internally set
	   if(step) iscal=2
c
	   ncurvd=1
	   if(calcurv) then
		jcal=2	!put values in xcal(i,2)
		tmin=0.0	!tmin was reset above
		call CALCVAL(xcal,ycal,jcal,ndc1,ndimc,tmin,tmax,
     &	  njump,ncalc,icur,QT,p0,km,ncalj,vhold,vref)
		pause
		ncurvc=2
		if(varconc) then
		   ncal(2)=ncalj		!from ODEINT: may not be same as ncalc
		else
	         ncal(2)=ncalc
		endif
	      icurvc(2)=2
	      iline(2)=0		!0 for continuous
	   else
	      ncurvc=1
	   endif
	   if(step) then
		ncurvc=0 	!no conc profile for step
		if(calcurv) then
		   ncurvc=1
		   icurvc(1)=2
		   ncal(1)=ncalc
		   iline(1)=0		!0 for continuous
		endif
	   else
		ncurvc=1
	      icurvc(1)=1
		ncal(1)=ncalc
		iline(1)=0		!0 for continuous
		if(calcurv) then
		   ncurvc=2
		   icurvc(2)=2
		endif
	   endif
	   ndat(1)=nval
	   icurvd(1)=1
	   icurvw(1)=-1
	   isym(1)=0		!point
	   syms(1)=3.
c	   ijoin(j)=-1		!points not joined
	   ijoin(1)=0
c
 	   titlex='time (ms)'
	   titley='total current'
	   itit=1		!so prev title used
	   call INTCONV(njump,cnum)
	   title1='Total current from '//charnb(cnum)//' channels'
	   if(.not.step) title1=charnb(title1)//' (conc in red)'
c  ISCAL=4 if input values of xmin,xmax,ymin,ymax to be used; rest internal
	   iscal=4
c
	   call VPLOT5(xval,yval,ndat,icurvd,ncurvd,ijoin,syms,ndimd,
     & xcal,ycal,ncal,icurvc,ncurvc,iline,ndimc,isym,ilog,iscal,
     & xmin,xmax,ymin,ymax,xtic,ytic,xcross,ycross,ntx,nty,itx,ity,
     & xlo,xhi,ylo,yhi,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,plotonly,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,interp,
     & isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kmax,iver)
	   DEALLOCATE(xval,yval,xcal,ycal,weight,theta)
	   DEALLOCATE(ndat,icurvd,icurvw,isym,ijoin,syms)
	   DEALLOCATE(ncal,icurvc,iline)
	endif
c
c==	call MODE(3)
	RETURN
	end

