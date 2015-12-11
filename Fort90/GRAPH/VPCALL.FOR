c VPCALL : typical VPLOT5 call, for use as basis for inserting
c into real programs
c  Weight() must be declared/allocated as weight(kwi,kwj) (eg kwi=niobs, kwj=njset)
c  before calling, but if not needed can be allocated as W(1,1).  Also include
c  icurvw(ndimd) (like icurvd) such that icurvw(i)=1 if curve i (as defined
c  by icurvd) should have error bars, =0 if w(i,j) defined but error bars not
c  to be drawn initially; and =-1 if weights not defined (replaces isdev).
c ISYM=positive for hollow symbol; neg for filled symbol (where applic)
c ISYM=0  point (now one pixel (I think); was a small pentagon)
c ISYM=1  triangle (point up)
c ISYM=2  triangle (point down)
c ISYM=3  square
c ISYM=4  diamond
c ISYM=5  pentagon
c ISYM=6  hexagon
c ISYM=7  circle (20 side poly)
c ISYM=8  +
c ISYM=9  x
c ISYM > 99		no symbol
c
c  ISCAL=-1 if input value of xcross,ycross to be used, but xmin,..,ymax,
c		and xtic,ytic found internally
c  ISCAL=0 if input values of xmin,...,ymax,xtic,ytic,xcross.. to be used.
c  ISCAL=1 if scaling to be done internally with FIXAX.
c  ISCAL=2 if input values of xmin,xmax only to be used; others internally set
c  ISCAL=3 if input values of ymin,ymax only to be used; others internally set
c  ISCAL=4 if input values of xmin,xmax,ymin,ymax to be used; rest internal
c  ISCAL set to 0 if NDAT=<0
c
c    IASK=1 or -1 to ask 'are you sure' before leaving VHIST; =2,-2 to not ask
c    IASK=pos to leave graph on screen (1 or 2); neg (-1,-2) to delete before
c		exit (orig version is iask=-2)
c    IASK=3 to draw graph and leave at once (no boxes; no chance to alter or
c		 plot or queue it)
c
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
c new arrays for horizontal lines
	real*4 yhline(10)		!record y value
	real*4 xhlb(10),xhle(10)     !start/end of HLINE
	integer ilhtype(10)	!line type for horizontal lines
	integer*1 ihlinrel(10)	!=1 if line pos rel to line 1
c new arrays for vertical lines
	real*4 xvline(10)		!record x value
	real*4 yvlb(10),yvle(10)     !start/end of VLINE
	integer ilvtype(10)	!line type for vertical lines
	integer*1 ivlinrel(10)	!=1 if line pos rel to line 1
C Commons:
	common/dp/discprt
	common/devnum/idev	!for textbox,gstring
	COMMON/JLOGOS/t1c(10),t2c(10),t1v(10),t2v(10),xoff1,y1v,y2v,
     & y1c,y2c
	common/DMENU/ifonb,csizem,ifont2,nboxlast,nblast		!for DCMENU
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi
	COMMON/TPOS/rx,ry,rxbox,rybox,ijus,angle,idraw,
     & ifnt,size,rlth,thbig,narrow,xb,yb,xe,ye,
     & nline,xlb,xle,ylb,yle,iltype,ntext,newtext,
     & cnumx,cnumy,cexpx,cexpy,numbx,numby,
     & nhline,yhline,xhlb,xhle,ilhtype,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel
	character adcfil*33
	COMMON/mtrace/ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil
	COMMON/cols/icol,mono
	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
	common/logval/logx,logy,sqrty
	common/queue/qfile
	common/hlp/help		!for QDIALOG
	common/help1/ihtype	!for VHELP
c
c For mouse
	logical mouse_on,mous_set
	common/mousval/mouse_on,nbutton
c
	call SETMOUSE()		!define values in common\mousval\ (in IVLIB)
	call GINO
	call vga
c	ndimd=1
c	ndimc=1
	ndv1=ndat(1)
	ndimd=1
	ndc1=1
	ndimc=1
	ALLOCATE(xval(ndv1,ndimd),yval(ndv1,ndimd))
	ALLOCATE(xcal(ndc1,ndimc),ycal(ndc1,ndimc))
	ALLOCATE(weight(kwi,kwj),theta(ndth))
	ALLOCATE(ndat(ndimd),icurvd(ndimd),icurvw(ndimd),
     &	isym(ndimd),ijoin(ndimd),syms(ndimd))
	ALLOCATE(ncal(ndimc),icurvc(ndimc),iline(ndimc))
c
	cbig=2.5
	ifont=4
	ifitype=0		!no display of fitted parameters
	idiskq=-1
	ilog=0	!arithmetic
	ilabel=1	!default pos for axis labels
	titlex='X axis'
	titley='Y axis'
	n=NBLANK2(titlex,40)
	n=NBLANK2(titley,40)
	itit=1		!so prev title used
	title1='TITLE'
	n=NBLANK2(title1,44)
	iask=-2
	ncjump=0
	nvjump=0
	autplt=.false.
	fitted=.false.
	plotonly=.false.
	interp=.false.
	ivplot=.false.
	doframe=.true.
	landscap=.true.
	iline(1)=0		!total curve continuous
	iscal=1		!scale internally
	xlo=-1.		!whole screen
	ntx=5
	nty=5
	itx=1		!ticks orientated normally
	ity=1
c
	ncurvd=nset
	do j=1,ncurvd
	  ndat(j)=n
	  icurvd(j)=j
	  icurvw(j)=1
	  isym(j)=-(j+2)	!filled symbols
	  syms(j)=3.0
	  ijoin(j)=-1		!points not joined
	  do i=1,ndat(j)
		xval(i,j)=xobs(i,j)
		yval(i,j)=yobs(i,j)
	   enddo
	enddo
c
	ncurvc=nc
	do j=1,ncurvc
	  ncal(j)=n
	  icurvc(j)=j
	  iline(j)=j-1	!0 for continuous
	  do i=1,ncal(j)
		xcal(i,j)=xc(i,j)
		ycal(i,j)=yc(i,j)
	   enddo
	enddo
	isetcol=0
	itrace=0
c
c
	DEALLOCATE(xval,yval,xcal,ycal,weight,theta)
	DEALLOCATE(ndat,icurvd,icurvw,isym,ijoin,syms)
	DEALLOCATE(ncal,icurvc,iline)
	call mode(3)

