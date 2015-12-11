	program TFILT
c To test interpolation and normalisation of step response functions
c For Getfilt
	real*4 filt(100)
	real*4 filt1(1024)
	character*1 ans
	character ftitle*80,filtfile*20,filnam*12
	logical pon,slock,discprt,present
c For vplot
	dimension xcal(2048,1),ycal(2048,1)
	dimension XVAL(2048,1),YVAL(2048,1)
	dimension w(100,10)
	real*4 theta(20)
c for data
c	dimension ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd)
c for calc curves
c	dimension ncal(ndimc),icurvc(ndimc),iline(ndimc)
	dimension ndat(1),icurvd(1),isym(1),ijoin(1)	!for data
	real symsiz(1)				!for data
	dimension ncal(1),icurvc(1),iline(1)	!for calc curve
	LOGICAL fitted,doframe,draft,autplt,interp,landscap,ivplot
	character*40 titlex,titley
	character*44 title1
c Declarations for common/tpos/
	real RX(100),RY(100)
	real rxbox(4,100),rybox(4,100)
	integer IXJUS(100),IYJUS(100)
	integer iangle(100),idraw(100),icol(100)
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25)
	integer ifnt(30)
	real csfac(30)
	real xb(10),yb(10),xe(10),ye(10)		!up to 10 arrows
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	integer iltype(10)				!line type for ditto
	character*80 newtext(20)		!extra text
	logical mono
C Commons:
	COMMON/JLOGO/t1c(10),t2c(10),t1v(10),t2v(10),xoff1,iy1v,iy2v,
     & iy1c,iy2c
	common/DMENU/csizb,ifonb,csize,ifont2,nboxlast,nblast
	common/hgv/ivxlo,ivxhi,ivylo,ivyhi,ixlo,ixhi,iylo,iyhi
	COMMON/TPOS/rx,ry,rxbox,rybox,ixjus,iyjus,iangle,idraw,
     & ifnt,csfac,ilth,thbig,narrow,xb,yb,xe,ye,
     & nline,xlb,xle,ylb,yle,iltype,ntext,newtext,
     & cnumx,cnumy,cexpx,cexpy,numbx,numby,icol,mono	!need also PARVAL?
	COMMON/SD/w		  !set isdev=-1 in call if not needed
c
c Commons
	common/dp/discprt
c
	call GETFILT(nfilt,dtf,filt,nsetup,filtf,filtfile,ftitle)
c
	ndimd=1
	ndimc=1
c
	cbig=2.5
	ifont=4
	ifitype=0		!no display of fitted parameters
	idiskq=-1
	isdev=-1		!no SD available
	ilog=0	!arithmetic
	ilabel=1
	titlex='X axis'
	titley='Y axis'
	itit=1		!so prev title used
	title1='TITLE'
	i=NBLANK2(titlex,40)
	i=NBLANK2(titley,40)
	i=NBLANK2(title1,44)
	iask=-2
	ncjump=0
	nvjump=0
	autplt=.false.
	fitted=.false.
	draft=.false.
	interp=.false.
	ivplot=.false.
	doframe=.true.
	landscap=.true.
	iscal=1		!scale internally
	ixlo=-1		!whole screen
	ntx=5
	nty=5
	itx=1		!ticks orientated normally
	ity=1
c
	ncurvd=1
	do j=1,ncurvd
	  ndat(j)=nfilt
	  icurvd(j)=j
	  isym(j)=-7
	  symsiz(j)=2.0
	  ijoin(j)=-1		!points not joined
	  do i=1,ndat(j)
		xval(i,j)=float(i-1)*dtf
		yval(i,j)=filt(i)
	   enddo
	enddo
c
	ncurvc=0
	iline(1)=0		!curve continuous
c
10	call VPLOT2(xval,yval,ndat,icurvd,ncurvd,ijoin,symsiz,ndimd,
     & xcal,ycal,ncal,icurvc,ncurvc,iline,ndimc,isym,ilog,iscal,
     & xmin,xmax,ymin,ymax,xtic,ytic,xcross,ycross,ntx,nty,itx,ity,
     & ixlo,ixhi,iylo,iyhi,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,draft,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,isdev,interp)
c
	if(nfilt1.gt.100) goto 999
	call NORMFILT(nfilt,dtf,filt,dtf1,nfilt1,filt1,tf0,tf50,tf1)
	print 13,tf0,tf50,tf1
13	format(' tf0, tf50, tf1 = ',3g13.6)
	pause
	do j=1,ncurvd
	  ndat(j)=nfilt1
	  icurvd(j)=j
	  isym(j)=0
	  symsiz(j)=2.0
	  ijoin(j)=-1		!points not joined
	  do i=1,ndat(j)
		xval(i,j)=float(i-1)*dtf1
		yval(i,j)=filt1(i)
	   enddo
	enddo
	goto 10	!display
999	continue
	end
