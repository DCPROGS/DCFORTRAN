	program tvplot
c To test vplot2
	real*4 t1v(10),t2v(10),t1c(10),t2c(10),vstep(10)
c
c for plots
	integer itype(10),ifst(10),ilast(10)
	dimension xcal(2048,10),ycal(2048,10)
	real*4 XVAL(2048,10),YVAL(2048,10)
	real*4 theta(20)
c for data
c	dimension ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd)
c for calc curves
c	dimension ncal(ndimc),icurvc(ndimc),iline(ndimc)
	integer ifstcol(10)
	dimension ndat(10),icurvd(10),isym(10),ijoin(10)	!for data
	dimension ncal(10),icurvc(10),iline(10)		!for calc curve
	real symsiz(10)				!for data
c	integer*2 videotyp
	character*40 titlex,titley
	character*44 title
	logical fitted,dcurve,doframe,draft,autplt,interp
	logical landscap,ivplot
	logical discprt,pon,debug,deb,caplock,slock
	character*1 ans,UC
c
c
	ndimd=10		!dimension of Xval,Yval
	ndimc=10		!dimension of xcal,ycal
c Define x,y
	f=1.0
	pi=3.14159
	r=f*pi
c
10	continue
	n1=1024
	n2=1024
c=	print 1
c=1	format(' Number of points (n1,n2) = ')
cc	input 3,i
c=	read 31,n1,n2
31	format(2i8)
	if(n1.eq.0) goto 99
	ndat(1)=n1
3	format(i8)
	ncurvd=2
	icurvd(1)=1
	ifstcol(1)=1		!start col for curve 1
	ijoin(1)=-1
	isym(1)=0
	xmin=0.
	xmax=100.
	dx=100./float(ndat(1))
	do 5 i=1,ndat(1)
	   xval(i,1)=xmin+float(i-1)*dx
	   yval(i,1)=sin(xval(i,1)/r)
5	continue
	ngr=1 + (ndat(1)-1)/2048    !# of cols of yval for data #1
	j=ngr+1		!col to start next data
	icurvd(2)=j
	ifstcol(2)=j
	ndat(j)=n2
	ijoin(j)=-1
	isym(j)=0
	dx=100./float(ndat(j))
	do 51 i=1,ndat(j)
	   xval(i,j)=xmin+float(i-1)*dx
	   yval(i,j)=sin(xval(i,j)/(2.*r))
51	continue
c
c NB to plot 2nd curve want icurvd=start col (which may be >2 if 1st curve
c has > 2048 points), so if n1,n2 =5000,3000 so data for curve 2 starts
c at Yval(1,4) then must specify curve number=4 for 2nd curve
c	print 11,1,j
c11	format(' Start cols = ',2i4,' :number of curves to plot = ')
c	read 3,ncurvd
c	do 12 i=1,ncurvd
c	   print 13,i
c13	   format(1x,i2,'. curve number = ')	!give 1 or j in this version
c	   read 3,icurvd(i)
c12	continue
c Another, more convenient, version in which array ifstcol() is defined that
c contains the col # where data starts for each curve (defined above). Then
c icurvd() can be defined as usual, but the loop at 'do 14' replaces icurvd()
c icurvd() values with start-col numbers before calling VPLOT
	ncurvd=2
	icurvd(1)=1
	icurvd(2)=2
c=	print 11,1,j
c=11	format(' Start cols = ',2i4,' :number of curves to plot = ')
c=	read 3,ncurvd
c=	do 12 i=1,ncurvd
c=	   print 13,i
c=13	   format(1x,i2,'. curve number = ')	!give 1 or 2 in this version
c=	   read 3,icurvd(i)
c=12	continue
	do 14 k=1,ncurvd
	ncv=icurvd(k)	!curve number
	icurvd(k)=ifstcol(ncv)		!now use icurvd in call
	print 15,k,icurvd(k)
15	format(' k,icurvd(k) = ',2i8)
14	continue
	pause
c
c Misc inputs
	titlex='abscissa'
	titley='ordinate'
	cbig=2.5
	ifont=4
	ifitype=0		!no display of fitted parameters
	ilog=0
	itx=1			!normal tick orientation
	ity=1
	ntx=5
	nty=5
	ixlo=-1		!whole screen
c	symsiz(1)=-1.0 	!default symbol sizes
c	do 41 i=1,10
c41	symsiz(i)=0.4*cbig
	doframe=.true.
c	doframe=.false.		!to show jump logos
	draft=.false.
	autplt=.false.
	ivplot=.false.
	interp=.false.
	itit=0
	fitted=.false.
	landscap=.true.
c calc curves:
	ncurvc=0		!none here
c
	iask=-2
c
c	if(debug()) then
c	   print 31,ncurvd,ndimd,ndimc,(icurvd(i),i=1,8),(ndat(i),i=1,8),
c     &   (isym(i),i=1,8)
c31	   format(' ncurvd,ndimd,ndimc= ',3i4,/,
c     & ' icurvd= ',8i5,/,
c     & ' ndat  = ',8i5,/,
c     & ' isym  = ',8i5,/)
c	   pause '00'
c	endif
c
	itit=0
	iscal=1	!scale internally
	isdev=-1
c
	call VPLOT2(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,syms,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & iXLO,iXHI,iYLO,iYHI,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,draft,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,isdev,interp)
c	call VPLOTR(Xval,Yval,ndat,icurvd,ncurvd,ijoin,symsiz,ndimd,
c     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
c     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
c     & iXLO,iXHI,iYLO,iYHI,y0,yinf,inumx,inumy,
c     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,
c     & titlex,titley,ilabel,doframe,idiskq,
c     & autplt,draft,itit,title,cbig,ifont,landscap,fitted,iask,
c     & theta,ifitype,ncomp)
c
	goto 10
99	continue
	end

