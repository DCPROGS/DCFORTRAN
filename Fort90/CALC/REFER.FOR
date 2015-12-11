	program REFER

	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 QFF(100,100),invQFF(100,100),qsav(100,100)
	real*8 q(100),r(100),p0(1,100),uF(100,1),row(1,100),phi(1000,5)
	real*8 ydisp(1000,5),xdisp(1000,5)
	character*40 mtitle1*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle1 !for WINPRINT,ENDPRINT,DISCNUM
	logical discprt
	common/dp/discprt
	character*11 cdate,ctime
	logical caplock,debug
c for graphs
	logical allocated
	ALLOCATABLE::Xval,Yval,Xcal,Ycal
	real*4 Xval(:,:),Yval(:,:)		!for VPLOT
	real*4 Xcal(:,:),Ycal(:,:)		!for VPLOT
	real*4 XLO,XHI,YLO,YHI,y00,yinfeq
	real*4 XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross
	real*4 cbig
c for data
	integer*4 ndat(1),icurvd(1),isym(1),ijoin(1)	!for data
	real*4 syms(1)				!for data
c for calc curves ndimc=5
	dimension ncal(5),icurvc(5),iline(5)	!for calc curve
	LOGICAL fitted,doframe,plotonly,autplt
	character*40 titlex,titley
	character*64 title1
	logical landscap,ivplot,interp
	character*1 ans
	integer*4 icurvw(1)		!for VPLOT5
	real*4 wght(1,1)
	real*4 theta(200)
c
	common/KBLK/kA,kB,kC,kD
	integer icol(100)
	logical mono
	COMMON/cols/icol,mono

c
	debug()=caplock()
c
	call SETMOUSE()		!define values in common\mousval\ (in IVLIB)
      OPEN(unit=10,file='ERROR.GIN')
	call errdev(10)
	call GINO
	call errdev(10)
	call vga
	call errdev(10)
	call mode(3)
	filnam='REFER.PRT'
	call WINPRINT	!print file control
      OPEN(unit=7,file=prtport,iostat=nerr)		!open printer
	print 1
	if(discprt) write(8,1)
1	FORMAT(' REFER: LFER calculations ',/)
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 2,cdate,ctime(1:8),mtitle1
	if(discprt) write(8,2) cdate,ctime(1:8),mtitle1
2	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 3
3	format(
     & ' CAPS LOCK on for debugging')

	zero=0.0d0
	one=1.0d0
c
	k=6
	q(1)=0.35d6
	q(12)=0.012d6
	q(2)=5.d6
	do i=3,11
	   q(i)=q(2)
	enddo
c
	do j=1,5
	   j2=2*j
	   r(j)=q(j2)/q(j2+1)
	enddo
c
	do i=1,6
	   do j=1,6
		QFF(i,j)=zero
	   enddo
	enddo
c

	call SETQ(QFF,q)
c
	kF=6
	km=100
c
	call ATYPD(QFF,'   QCC  ',kF,kF,km,km)
c
c Exact mean shut time, given start in state 1
	call MATINV(-QFF,kF,km,invQFF,km)
	do i=1,6
	   p0(1,i)=zero
	   uF(i,1)=one
	enddo
	p0(1,1)=one
	call MATMUL(p0,invQFF,row,1,kF,kF,ONE,1,km,km,km,1,km)
	call MATMUL(row,uF,s,1,kF,1,ONE,1,km,km,1,1,1)
	ts=s
	beta=one/ts
c
c Approx mean shut
	em=one+r(1)+r(1)*r(2)+r(1)*r(2)*r(3) + r(1)*r(2)*r(3)*r(4) +
     & r(1)*r(2)*r(3)*r(4)*r(5)
	ts1=(em)/q(1)
	beta1=one/ts1
c Overall equilibrium constant

	eqK=(q(1)/q(12))/(r(1)*r(2)*r(3)*r(4)*r(5))

	print 10,beta,beta1,eqK
10	format(3g13.6)

	do i=1,6
	   do j=1,6
		Qsav(i,j)=QFF(i,j)
	   enddo
	enddo
c Now perturb each r in turn -calc beta and eqK for range of r values
c With exact solution, the mean shut time is function of the q(i), not of
c r(i) only, so for each r(i) change only the forward rate eg q(3)=q(2)/r1

c	ndisp=1000
c	rmin=0.01
c	rmax=100.
	ndisp=10
	rmin=0.2d0
	rmax=2.d0
	dx=(dlog10(rmax)-dlog10(rmin))/float(ndisp-1)

	do ir=1,5
	   do i=1,ndisp
		ri=rmin*10.d0**((i-1)*dx)
		q(2*ir+1)=q(2*ir)/ri
c or change both forward and backward rates by factor sqrt(r)
c		q(2*ir+1)=q(2*ir+1)/sqrt(ri)
c		q(2*ir)=q(2*ir)*sqrt(ri)

		do j=1,5		!recalculate all r(j)
		   j2=2*j
		   r(j)=q(j2)/q(j2+1)
		enddo
		call SETQ(QFF,q)
		call MATINV(-QFF,kF,km,invQFF,km)
		call MATMUL(p0,invQFF,row,1,kF,kF,ONE,1,km,km,km,1,km)
		call MATMUL(row,uF,s,1,kF,1,ONE,1,km,km,1,1,1)
		ts=s
		beta=one/ts
		eqK=(q(1)/q(12))/(r(1)*r(2)*r(3)*r(4)*r(5))

		ydisp(i,ir)=beta
		xdisp(i,ir)=eqK
		if(i.gt.1) then
		   phi(i-1,ir)=(dlog10(ydisp(i,ir))-dlog10(ydisp(i-1,ir)))/
     &			(dlog10(xdisp(i,ir))-dlog10(xdisp(i-1,ir)))
		endif
		rlast=ri
c        restore QFF
		do i1=1,6
		   do j1=1,6
			QFF(i1,j1)=Qsav(i1,j1)
		   enddo
		enddo
		if(ir.eq.1.and.i.lt.20) then
		   if(i.eq.1) then
			print 11,i,r(1),beta,eqK
			if(discprt) write(8,11) i,r(1),beta,eqK
11			format(i4,3g13.6)
		   else
			print 111,i,r(1),beta,eqK,phi(i-1,ir)
			if(discprt) write(8,111) i,r(1),beta,eqK,phi(i-1,ir)
111			format(i4,4g13.6)
		   endif
		endif
	   enddo
c        restore QFF
	   do i=1,6
	      do j=1,6
	   	   QFF(i,j)=Qsav(i,j)
	      enddo
	   enddo
	enddo
c
c Now display
	ndv1=1 		!xval not used
	ndimd=1
	ndc1=ndisp
	ndimc=5
	ALLOCATE(Xval(ndv1,ndimd),Yval(ndv1,ndimd))
	ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
	iscal=1		!scale internally
	inumx=-1		!X axis in fixed (Fn.d) format
	inumy=-1
c	cbig=2.5
	cbig=2.0
	ifont=4		!default is duplex
	itit=0		!no title yet
	itx=1			!normal tic orientation
	ity=1			!normal tic orientation
	ilabel=1
	xlo=-1.		!whole screen
      doframe=.true.
	landscap=.true.
	autplt=.false.
	ivplot=.false.
	interp=.false.
	itrace=0	!no longer used!
	kwi=1		!dimension of weight()
	kwj=1		!dimension of weight()
	ifitype=0
	kmax=200	!dimension of theta
c
	iopt=1
13	print 12,iopt
12	format(
     & '(1) Display REFER curves',/,
     & '(2) Display phi curves',/,
     & '(3) Finish',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(iopt)
	if(iopt.eq.3) goto 999
c
c Misc settings for vplot
	iask=-2		!leave with graph erased, no query
	ISCAL=1 	!scaling to be done internally with FIXAX.
	do i=1,100
	   icol(i)=-1
	enddo
	isetcol=1	!use default colours except when icol set to valid colour
	plotonly=.true.
c
	fitted=.false.
	ncurvd=0
	ncurvc=5
	if(iopt.eq.1) then
	   titlex='   E      '
	   titley='  beta    '
	   ilog=3		!log/log initially
	   do i=1,ncurvc
		icurvc(i)=i
		ncal(i)=ndisp
		iline(i)=0
	   enddo
	   do i=1,ndisp
		do j=1,5
		   xcal(i,j)=sngl(xdisp(i,j))
		   ycal(i,j)=sngl(ydisp(i,j))
		enddo
	   enddo
	else if(iopt.eq.2) then
	   titlex='   E      '
	   titley='   phi    '
	   ilog=1		!phi vs log(E) initially
	   do i=1,ncurvc
		icurvc(i)=i
		ncal(i)=ndisp-1
		iline(i)=0
	   enddo
c note: phi = derivative is located at x half way between calc points
	   do i=1,ndisp-1
		do j=1,5
		   xcal(i,j)=sngl(sqrt(xdisp(i,j)*xdisp(i+1,j)))
		   ycal(i,j)=sngl(phi(i,j))
		enddo
	   enddo
	endif

	call VPLOT5(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,syms,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,y00,yinfeq,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,plotonly,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,interp,
     & isetcol,itrace,ndv1,ndc1,wght,kwi,kwj,icurvw,kmax,iver)
c
	if(iopt.eq.1) iopt=2
	goto 13
c
999	if(allocated(xval)) deallocate(xval,yval,xcal,ycal)
	call ENDPRINT		!flush + form feed
	call NUMCLR()

c
	end

	subroutine SETQ(QFF,q)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 QFF(100,100),q(100)
c
	zero=0.0d0
c
	QFF(1,2)=q(1)
	QFF(2,1)=q(2)
	QFF(2,3)=q(3)
	QFF(3,2)=q(4)
	QFF(3,4)=q(5)
	QFF(4,3)=q(6)
	QFF(4,5)=q(7)
	QFF(5,4)=q(8)
	QFF(5,6)=q(9)
	QFF(6,5)=q(10)

	do i=1,6
	   s=zero
	   do j=1,6
		if(i.ne.j) then
		   s=s+QFF(i,j)
		endif
	   enddo
	   QFF(i,i)=-s
	enddo
	QFF(6,6)=QFF(6,6)-q(11)
	end

