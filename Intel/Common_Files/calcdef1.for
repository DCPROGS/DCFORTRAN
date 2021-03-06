	subroutine calc_default(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,syms,
     & ndimd,XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
     & XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,
     & titlex,titley,ilabel,doframe,autplt,itit,title1,
     & ISHP,ifont,landscap,ndv1,ndc1,weight,kwi,kwj,icurvw,kmax,iver,
     & redrawn,plot,redo,pbmp,pwmf,npint,hdisp,n1,tlenx,tleny,nx1,ny1,
     & izoom,xtitle,ytitle,rescale,dxs,dys,ifitype,parval,theta,ncomp,
     & isval,sval)
c=====================================================================
c
c	xmin,xmax,ymin,ymax		- coordinates depending on scale ! except sqrty
c	xmin1,xmax1,ymin1,ymax1	- absolute (arithmetic) coordinates
c	xmin2,xmax2,ymin2,ymax2	- fixed absolute (arithmetic) coordinates
c	xmin3,xmax3,ymin3,ymax3	- coordinates depending on scale
c
c
c=====================================================================
	integer*4 IFNT(100),IJUS(100)
	real*4 RX(100),RY(100),RXBOX(4,100),RYBOX(4,100)
      integer*4 ICOL(250),ITYPE(250)
      real*4 sizetext(100),ANGLE(100),THICK(250),IDRAW(250)
	real*4 XBEG(50),YBEG(50),XEND(50),YEND(50),C_THICK(250)
	integer*1 ihlinrel(10)	!=1 if line pos rel to line 1
	integer*1 ivlinrel(10)	!=1 if line pos rel to line 1

	real*4 XVAL(n1:ndv1,ndimd),YVAL(n1:ndv1,ndimd)
	real*4 XCAL(ndc1,ndimc),YCAL(ndc1,ndimc)
	integer*4 ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd)
	integer*4 icurvw(ndimd)
	real*4 syms(ndimd)		!symbol size
	real*4 weight(kwi,kwj)
	integer*4 ncal(ndimc),icurvc(ndimc),iline(ndimc)
	
	allocatable::xscale,yscale,yoffset
	real*4 xscale(:),yscale(:),yoffset(:)
	logical xdone,ydone,meta,wmeta,wbmp,nojump,redo
	
	logical ivplot,onjump,tload,calbarX,calbarY,calbar,blank
	logical vert,mono,monsav,redrawn,pbmp,pwmf
	logical plot,plotcols,rescale
	real theta(100)
	character*40 titlex,titley,titxsav,titysav
	character*75 xtitle,ytitle	!output from LAXES
	character*75 oldtext
	character*64 TITLE1
	character*200 parval,text1		!to hold any string (for fixtext call)
      character kpchar*11, ch*1,cnum1*11		!must have *11 for INTCONV
	LOGICAL logx,logy,logity,down,fitted,croset,EQUAL,sqrty
	logical doframe,autplt,landscap
	logical allpnt,bigplot !for when one or more plots have large # of points
	integer icolsav(250)
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25),cexpz(25)
	character*10 cnumz(25)
	character*80 newtext(20)		!extra text
	integer  nreal(20)
	character*78 text(30),strings(10),str2(10),titles,titles1	!for DCMENU
	character adcfil*33
	logical hdisp
	character tring*11
	character*20 strnum(100)
	character*75 xtitlea
	character*11 string		!for components of titles
	character*80 str		!for components of titles
	character*6 frmty
	character*5 x10x,x10y		!for ' x10' on x,y axes
	character*15 newfnt
	character*2 up,down1,font
	real*8 xlog ,ylog

      INTEGER LEFT, CENTER, RIGHT
      DATA LEFT, CENTER, RIGHT /-1, 0, 1 /     !new definition
	logical bold,italik,underline
	common/text_attr/bold,italik,underline,ipow,idev,nolabx,ifontrue

      COMMON/TPOS/IDRAW,ICOL,THICK,C_THICK,ITYPE,IFNT,ANGLE,IJUS,
     & SIZEtext,RXBOX,RYBOX,
     & RX,RY,NARROW,NLINE,NHLINE,NVLINE, XBEG,YBEG,XEND,YEND,
     & NTEXT,NEWTEXT,CNUMX,CNUMY,CNUMZ,CEXPX,CEXPY,CEXPZ,
     & NUMBX,NUMBY,NUMBZ,IHLINREL,IVLINREL
	
	COMMON/JLOGOS/t1c(10),t2c(10),t1v(10),t2v(10),xoff1,y1v,y2v,
     & y1c,y2c,ncjump,nvjump,ivplot
c	COMMON/mtrace/ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil
	common/logval/logx,logy,sqrty,logity
	common/callback/icl1,icl2,icl3
	COMMON/mtrace/ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil
	common/limit/wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,
     &	xmin,xmax,ymin,ymax

	common/calib/calbarx,calbary,xbeg4,ybeg4,xend4,yend4,
     &     xbeg5,ybeg5,xend5,yend5
	common/ptext/ameant(100),areat(100)
	common/other/iparfirst	
	EQUAL(x,y)=abs(x-y).lt.0.00001
	isetcol=0
	plotcols=.true.
	ndims=ndimd
	if(ndimc.gt.ndimd) ndims=ndimc
	ALLOCATE(xscale(ndims),yscale(ndims),yoffset(ndims))
	
	
	select case(ishp)
	   case(0)
		disfacx=0.7
		disfacy=0.7
!		disfacx=1.
!		disfacy=1.
	   case(1)
		disfacx=0.45 !0.4
		disfacy=0.7
	   case(2)
		disfacx=0.55 ! 0.5
		disfacy=0.7
	   case(3)
		disfacx=0.85
		disfacy=0.85
	   case(4)
		disfacx=0.45
		disfacy=0.7
	end select
	mono=.false.
	if(.not.rescale) then
		if(autplt) isetcol=1
		call set_colours(mono,icol,.false.,plotcols,isetcol)
	endif
	jfirst=0
	scalfac=1.0
	just=0
	ihead=2
	wmeta=.false.
	wbmp=.false.
	nbig=10000	!for bigplot/allpnt options
	present=.true.
c	xtic=xtic*1.00001
	xtsav=xtic
	ytsav=ytic
	if(.not.autplt) iver=1100
	xlo=xlo1		!named ixlo1 in call so ixlo can be in common/hgv/
	xhi=xhi1		!ditto
	ylo=ylo1 !named iylo1 in call so iylo can be in common/hgv/
	yhi=yhi1		!ditto
	do i=1,ndims
	   xscale(i)=1.0
	   yscale(i)=1.0
	   yoffset(i)=0.0
	enddo
	xdone=.false.
	ydone=.false.
	if(inumx.eq.0) inumx=-1		!but should be defined in call
	if(inumy.eq.0) inumy=-1		!but should be defined in call
	plotcols=.true.
	cbig=3.0
      calbarX=ntx.eq.-1000
	calbarY=nty.eq.-1000
	calbar=calbarX.or.calbarY
	ntxsav=ntx		!keep input value
	ntysav=nty		!keep input value
	i=NBLANK1(titlex)               !ensure it ends with char(0)
	j=NBLANK1(titley)               !ensure it ends with char(0)
	if(ntx.ne.-1000) titxsav=titlex
	if(nty.ne.-1000) titysav=titley
	nojump=ncjump.eq.0.and.nvjump.eq.0
	xcalib=rx(2)-rx(1)
	ycalib=ry(4)-ry(3)
	croset=.false.		!cross position has not been reset
	if(iscal.eq.0.or.iscal.le.-1) croset=.true.	!use input cross position
c	landplot=.true.
	x0=1.e-36		!smallest value for taking logs
	logx=ilog.eq.1.or.ilog.eq.3.or.ilog.eq.4.or.ilog.eq.6
	logy=ilog.eq.2.or.ilog.eq.3
	logity=ilog.eq.4		!for Hill plot
	sqrty=ilog.eq.5.or.ilog.eq.6
	if(logx.and.xval(1,1).lt.0.001) inumx=1
	do i=1,100
	   if(sizetext(i).le.0.0) sizetext(i)=3.
	enddo
	do i=1,ndimd
	   if(syms(i).le.0.001.or.syms(i).gt.5.)  syms(i)=2.5
	enddo
	if(hdisp) then
		if(ncurvd.gt.0) then
			n1=0	!lower bound for xval, yval
			call MINMAX0(xval,yval,ndat,xmin1,xmax1,ymin1,
     &		ymax1,logx,logy,ndimd,ncurvd,icurvd,n1,ndv1)

		endif
		if(ncurvd.le.0)then
			n1=1	!lower bound for xcal, ycal
			call MINMAX0(xcal,ycal,ncal,xmin1,xmax1,ymin1,
     &		ymax1,logx,logy,ndimc,ncurvc,icurvc,n1,ndc1)

		endif
	else
		bigplot=.false.		!now superfluous
		do j1=1,ncurvd
			j=icurvd(j1)
			if(ndat(j).gt.nbig) bigplot=.true.
		enddo
		plotcols=.true.
		mono=.false.
		linetype=0		!continuous line
		if(autplt.or.izoom.eq.1) then
			fitted=ncurvc.gt.0
			allpnt=.true.		!plot all points if ndat>2048
			xmin1=xmin
			xmax1=xmax
			ymin1=ymin
			ymax1=ymax
			if(logx) then
					xmin1=10**xmin
					xmax1=10**xmax
			endif
			if(sqrty) then
				!	ymin1=ymin**2
				!	ymax1=ymax**2
			endif
	        xmin1=xmin1*1.00001	!so does not round down
			if(logy) then
					ymin1=10**ymin
					ymax1=10**ymax
			endif
			xmin1=xmin1*1.00001
	        ymin1=ymin1*1.00001	!so does not round down
	   		if(logity) then
				y0=0
				yinf=ymax1
			endif
		else
			if(cbig.lt.2.) cbig=3.5
			if(ifont.lt.0) ifont=2
			allpnt=.not.bigplot	!now start with allpnt=false if any curve has >nbig points
			thfacsml=0.6		!line thickness factor for small plots
				csize=3.0
			idraw(241)=1		!c-jump logo
			idraw(242)=1		!v-jump logo
			if((.not.fitted).or.ifitype.eq.0) idraw(2)=0	!no param values
			ijus(3)=0		!for x-axis label
			ijus(4)=0
			ANGLE(4)=90.
	if(ncurvd.gt.0)call MINMAX2(xval,yval,ndat,xmin1,xmax1,ymin1,
     & ymax1,logx,logy,ndimd,ncurvd,icurvd,ndv1)
	if(ncurvd.le.0)call MINMAX2(xcal,ycal,ncal,xmin1,xmax1,ymin1,
     & ymax1,logx,logy,ndimc,ncurvc,icurvc,ndc1)
		xmin=xmin1
		xmax=xmax1
		ymin=ymin1
		ymax=ymax1
	if(logx) then
					xmin=alog10(xmin1)
					xmax=alog10(xmax1)
			endif
	if(logy) then
					ymin=alog10(ymin1)
					ymax=alog10(ymax1)
			endif
		endif
		
		xcross=xmin		!crossing point for axes
		ycross=ymin
	
	endif
	
	if(iscal.ne.0.and.autplt.eq..false.) then 
		xmins=xmin		!save input values
		xmaxs=xmax		!save input values
		ymins=ymin		!save input values
		ymaxs=ymax		!save input values
		if(logy.and.inumy.eq.0) then	!not defined on input
			inumy=1	!exponent form of numbering for log scale initially
			amin=abs(alog10(ymin1))
			amax=abs(alog10(ymax1))
			if((amax.le.3.5).and.amin.le.3.5) inumy=-1	!non-exponent
		endif
		if(itx.lt.-1.or.itx.gt.1) then
			itx=1	!default axis tic orientation in LAXES
			ity=1
		endif
		if(ntx.eq.0) ntx=5		!label every 5th tic
		if(nty.eq.0) nty=5		!label every 5th tic
		if(izoom.eq.1)then
			xmin2=xmin1
			xmax2=xmax1
			ymin2=ymin1
			ymax2=ymax1
		else
			call FIXAX(xmin1,xmax1,xmin2,xmax2,xtic,0)	!always non-log
			call FIXAX(ymin1,ymax1,ymin2,ymax2,ytic,0)
			if(iscal.eq.2.or.iscal.eq.4) then
				xmin2=xmins		!restore input value
				xmax2=xmaxs		!restore input value
			endif
			if(iscal.eq.3.or.iscal.eq.4) then
				ymin2=ymins		!restore input value
				ymax2=ymaxs		!restore input value
			endif
			if(sqrty) then
			ytic=4.0
			nty=4
			if(ymax2.gt.49.) then
				ytic=16.0
				nty=16
			endif
			if(ymin2.lt.0.) ymin2=0.
			endif
			if(iscal.ge.0) then
			xcross=xmin2		!crossing point for axes
			ycross=ymin2
			endif
			if(ivplot) then
			xtic=50.		!mV (or tics are too close with short X axis)
			itx=0		!tics central
			ity=0
			xcross=0.
			ycross=0.
			landscap=.false.
			doframe=.false.
			endif
			if((.not.nojump).and.(.not.autplt)) then
			y1c=ymin+0.96*(ymax-ymin)         !initial positions for jump logos
			y2c=ymin+1.00*(ymax-ymin)
			y1v=ymin+0.94*(ymax-ymin)
			y2v=ymin+0.90*(ymax-ymin)
			endif
			xtsav=xtic
			ytsav=ytic
		endif
	
	endif


	xlo2=xlo		!save screen GRAPHBOUNDARY
	xhi2=xhi
	ylo2=ylo
	yhi2=yhi


	if(ymin.eq.0.0.and.ymax.eq.0.0) then
	   y1c=ymin2+0.96*(ymax2-ymin2)         !initial positions for jump logos
	   y2c=ymin2+1.00*(ymax2-ymin2)
	   y1v=ymin2+0.94*(ymax2-ymin2)
	   y2v=ymin2+0.90*(ymax2-ymin2)
	endif
	
	if(iscal.eq.5) then
	   if(ilog.ne.0) then
		logx=ilog.eq.1.or.ilog.eq.3.or.ilog.eq.4.or.ilog.eq.6
		logy=ilog.eq.2.or.ilog.eq.3
		logity=ilog.eq.4		!for Hill plot
		sqrty=ilog.eq.5.or.ilog.eq.6
	      x0=1.e-36		!smallest value for log
		if(izoom.eq.1.and.logx) then
		   xmin=alog10(xmin1)
		   xmax=alog10(xmax1)
		endif
		if(izoom.eq.1.and.logy) then
		   ymin=alog10(ymin1)
		   ymax=alog10(ymax1)
		endif
	    if(sqrty.and.ymin1.ge.0.) goto 200
		if(logx.and.xmin1.le.0.) then
		   xmin1=1.0
		   if(xmin1.gt.xmax) then
		   if(xmin.gt.0) then
			 xmin1=xmin
		   else
		     xmin1=0.1*xmax
		   endif
		  endif
		  xmin1=xmin1*1.00001	!so does not round down
	    endif
	      
		if(logy.and.ymin1.le.x0) then
	   	  ymin1=1.0
		  if(ymin1.gt.ymax) then
		  if(ymin.gt.0) then
			ymin1=ymin
		  else
		     ymin1=0.1*ymax
		  endif
	        endif
		  ymin1=ymin1*1.00001	!so does not round down
		endif
200		continue
		if(logity) then
		   y0=0
		   yinf=ymax1
		endif
		if(logx) then
	   	   xmin3=alog10(xmin1)
	   	   xmax3=alog10(xmax1)
		else
	   	   xmin3=xmin1	!xmin1 etc always non-log values
	   	   xmax3=xmax1
		endif
		if(logy) then
	   	   ymin3=alog10(ymin1)
	   	   ymax3=alog10(ymax1)
		else
	   	   ymin3=ymin1	!xmin1 etc always non-log values
	   	   ymax3=ymax1
		endif
	      if(logity) then
	         if(ymin1.gt.y0+x0.and.ymax1.lt.(yinf-x0).and.
     &	   abs(yinf-ymax1).gt.1.e-30) then
		      ymin3=alog10((ymin1-y0)/(yinf-ymin1))
		      ymax3=alog10((ymax1-y0)/(yinf-ymax1))
	   	   else
		      ymin3=-2.99999    !rounds to -3 to +3
		      ymax3=2.99999
	   	   endif
		endif
		if(ilog.eq.7) then	!1/y vs 1/x
		   xmin3=1./xmax1
		   xmax3=1./xmin1
		   ymin3=1./ymax1
		   ymax3=1./ymin1
		else if(ilog.eq.8) then
		   xmin3=ymin1
		   xmax3=ymax3
		   ymin3=ymax1/xmax1
		   ymax3=ymin1/xmin1
		else if(ilog.eq.9) then
		   ymin3=ymin1
		   ymax3=ymax3
		   xmin3=ymax1/xmax1
		   xmax3=ymin1/xmin1
		else if(ilog.eq.10) then
		   xmin3=xmin1
		   xmax3=xmax1
		   ymin3=xmin1/ymin3
		   ymax3=xmax3/ymax3
		endif
		if(izoom.eq.1) goto 223
		il=1
		if(.not.logx) il=0
		call FIXAX(xmin3,xmax3,xmin,xmax,xtic,il)
		il=1
		if(.not.logy) il=0
		call FIXAX(ymin3,ymax3,ymin,ymax,ytic,il)
223		if(sqrty) then
	   	   ytic=1.0
	   	   nty=1
	   	   if(ymax.gt.49.) then
			ytic=4.0
			nty=4
	   	   endif
	   	   if(ymin.lt.0.) ymin=0.
	   	   xcross=xmin
	   	   ycross=ymin
		endif
		croset=.false.	!so sets xcross=xmin etc
	   else
		if(.not.autplt) then
		xmin=xmin2
		xmax=xmax2
		ymin=ymin2
		ymax=ymax2
		endif
	   endif
	else
		if(.not.autplt) then
	  !  xmin=xmin2
	!	xmax=xmax2
	!	ymin=ymin2
	!	ymax=ymax2
		endif
	endif
c*********************************************************************

100	continue
	redrawn=.true.	!graph will have xtitle etc drawn in LAXES
	if((.not.croset).and.(.not.ivplot)) then
	   xcross=xmin		!crossing point for axes
	   ycross=ymin
	endif

c     Scale now
	ymaxq=(ymax)
	yminq=(ymin)
	if(sqrty) then
	   ymaxq=sqrt(ymax)
	   yminq=sqrt(ymin)
	else
	   ymaxq=(ymax)
	   yminq=(ymin)
	endif
	xlth=xmax-xmin
	ylth=ymaxq-yminq
	deltax=xlth/disfacx
	deltay=ylth/disfacy
	dx=(deltax-xlth)/2.
	dy=(deltay-ylth)/2.

	wxmin=xmin-1.5*dx
	wxmax=xmax+1.5*dx
	wymin=yminq-1.5*dy
	wymax=ymaxq+1.5*dy

	dxs=xlth*0.007
	dys=ylth*0.010
	
	wid=0.5*dys*1.000001

	do i=1,250
	   thick(i)=wid*c_thick(i)
	enddo
	xsps=xmax+0.2*dx
	ysps=ymax



C	calibration bars and axis
	
	calbarX=ntx.eq.-1000
	calbarY=nty.eq.-1000
	tlenx=1.45*dys
	tleny=1.4*dxs
	ipow=-10000
	left=-1	!ditto
	right=1		!ditto
	center=0
	if(logx) xtic=1.0
	if(logy) ytic=1.0

	up='�E'
	down='�I'
	font='�F'
	x10x=' x 10'	!'x10' for x axis
	x10y=' x 10'	!'x10' for x axis
c	ymax1=ymax
c	ymin1=ymin
	ycros1=ycross
c	dxs=abs(xmax-xmin)*0.007
c	dys=abs(ymax-ymin)*0.010

	if(sqrty) then
!	   ymax1=sqrt(ymax)	!for comparison with pos
!	   ymin1=sqrt(ymin)	!display thinks this is ymin,ymax
	   ycros1=sqrt(ycross)
	endif
	yoffset1=0.14*abs(ymax-ymin)
	if(sqrty) yoffset1=sqrt(yoffset1)
	xoffset=0.14*abs(xmax-xmin)
	nxf=0		!used if numbering omitted (ntx<0)
	if(logx.and.inumx.eq.-1) nxf=4	!temp fix to allow down to 0.0001
	nx=0		!ditto, so no scale factor written
	if(logx) then
		amax=abs(xmax)
		if(abs(xmin).gt.amax) amax=abs(xmin)
		nxf=2-ifix(alog10(amax))
		if(nxf.ge.0.and.nxf.le.2) then
			nx=0
		else
			nx=nxf-2
			nxf=2
		endif
	endif

	nyf=0		!used if numbering omitted (nty<0)
	if(logy.and.inumy.eq.-1) nyf=2	!temp fix to allow 0.01,0.1
	ny=0		!ditto, so no scale factor written
	if(nty.lt.0) goto 33
	amax=abs(ymax)
	if(abs(ymin).gt.amax) amax=abs(ymin)
	nmax=3	!max number of sig figs after decimal point
	y=float(ifix(amax/ytic))*ytic	!=largest number drawn
	call FIXDEC1(y,m0,n,nmax)	!try this 03/07/91 06:55pm
	idmax=m0+n+3
	if(n.eq.0) idmax=m0+n+1		!no dec point
	if(logy.and.inumy.eq.1.and.ymin.lt.0.) idmax=idmax+1	!for neg sign in exp
	if (logy) nd=1+ifix(alog10(amax)+0.001)
	if(ymin.lt.0.) nd=-nd	!signal to MAKEXP to leave room for minus sign
	if(logy) goto 33
	if(.not.logy.and.inumy.eq.-1) goto 33
	nyf=2-(iabs(nd)-1)
	if(nyf.ge.0.and.nyf.le.2) then
	   ny=0
	else
	   ny=nyf-2
	   nyf=2
	endif
	if(ny.ne.0) idmax=idmax-iabs(ny)+2	!numbers take less space if scaled
c
c Make format into string for use in setxlabel call

33	continue

	nmax=3	!max number of sig figs after decimal point
	frmty='7.'//char(nyf+48)	!='7.nyf'
	if(logy) frmty='3.1'		!does this control title offset?-YES
	nx1=iabs(ntx)	!mod Jan 88
	ny1=iabs(nty)
	
	if(.not.logx) goto 40
	nx1=9		!major tic every 10th for log axis
	if(abs(amod(xmin,1.)).lt.0.0001) goto 40	!xmin is integer
	n=ifix(xmin)
	if(xmin.lt.0.) n=n-1	!find nearest integer below xmin
40	continue
	
	if(.not.logy) goto 41
	ny1=9		!major tic every 10th for log axis
	if(abs(amod(ymin,1.)).lt.0.0001) goto 41	!ymin is integer
	n=ifix(ymin)
	if(ymin.lt.0.) n=n-1	!find nearest integer below ymin
41	continue



C		LABELS

	do 19 i=1,75
	xtitle(i:i)=char(32)
19	ytitle(i:i)=char(32)
	nlx=NBLANK1(titlex)
	if(ilabel.ne.0.and.nlx.gt.0) then
	   k=0
	   do 18 i=1,nlx
	   ch=titlex(i:i)
	   ic=ichar(ch)
	   if(ic.ne.242) then
	      k=k+1
	      xtitle(k:k)=ch
	   else if(ic.eq.242) then
		newfnt=font//'010'//'>'//font//'10'//char(ifnt(3)+48)
		do 181 j=1,5
		k=k+1
181		xtitle(k:k)=newfnt(j:j)
	   endif
18	   continue
	endif
	if(nx.eq.0.or.logx.or.calbarX) goto 20
c 	add 10**nx to xtitle
	nlx=NBLANK1(xtitle)
	string=char(iabs(nx)+48)		!ascii for number NX
	if(nx.lt.0) string='-'//string
	nmax=3	!max number of sig figs after decimal point
	call FIXDEC1(float(nx),m0,n,nmax)
	n=0
	call DCFORMAT(float(nx),m0+n+3,n,string)
	ns=NBLANK1(string)
c	if(nlx.ge.1.and.ns.ge.2) then
c	   xtitle=xtitle(1:nlx)//x10x//up//string(2:ns)//down
c	endif
20	continue
	if(logx) then
	   nl=NBLANK1(xtitle)
	   if(nl.ge.1) xtitle=xtitle(1:nl)//' (log scale)'
	endif
21	continue
c
c	 and YTITLE
	
	if(ilabel.eq.0) goto 22		!add scale factor?
	nly=NBLANK1(titley)
	if(nly.ge.1) ytitle=titley(1:nly)
	if(calbarY) goto 22
c  	If xwbase pos (eg=10.) add 'per 10 ms' to Y label for freq dens plots
c  	If xwbase neg (eg=-10.) add 'per 10 pA' to Y label
c  	No addition if xwbase=0.
	if(xwbase.gt.-1.e-20.and.xwbase.lt.1.e-20) goto 22	!skip
	nmax=3	!max number of sig figs after decimal point
	call FIXDEC1(abs(xwbase),m0,n,nmax)
	call DCFORMAT(abs(xwbase),m0+n+3,n,string)
	ns=NBLANK1(string)
	if(nly.ge.1.and.ns.ge.1) then
	   ytitle=ytitle(1:nly)//' per'//string(1:ns)
	endif
	nly=NBLANK1(ytitle)
	if(nly.ge.1) then
	   if(xwbase.gt.0.) ytitle=ytitle(1:nly)//' ms'
	   if(xwbase.lt.0.) ytitle=ytitle(1:nly)//' pA'
	endif
22	continue
c     Now scale factor
	if(ny.eq.0.or.logy.or.calbarY) goto 23
c     add 10**ny to ytitle
	nly=NBLANK1(ytitle)
	if(ny.gt.0) then
	   string(1:1)=char(ny+48)
	   ns=1
	else if(ny.lt.0) then
	   string(1:1)='-'
	   string(2:2)=char(iabs(ny)+48)
	   ns=2
	endif
	if(nly.ge.1.and.ns.ge.1) then
	   ytitle=ytitle(1:nly)//x10y//up//string(1:ns)//down1
	endif
23	continue
	nly=NBLANK1(ytitle)
!	if(logy) ytitle=ytitle(1:nly)//' (log scale)'
!	if(sqrty) ytitle=ytitle(1:nly)//' (square root scale)'
!	if(logity) ytitle=ytitle(1:nly)//' (logit scale)'
c
c Rescale: if nx not zero need to scale the numbers plotted on
c the axes by 10.**nx- can do this here by temporarily changing
c the scaling, then revert to original as soon as axes drawn (see Hgraph p14)
	if(logx.or.calbarX) nx=0	!to be sure- done above?
	if(logy.or.calbarY) ny=0	!to be sure- done above?
	sx=10.**nx
	sy=10.**ny
	if(sqrty) sy=1.0


	idr=idraw(245)
!	if(hdisp) goto 77


	if(calbarX.or.calbarY) then
	  
	   if (idr.le.-2) then	!draw at default position if idraw=-2,-3
		idraw(245)=1			!reset
	   endif
	   if (idr.eq.-3) then		!use current length but default posn
		   xcalib=xend(44)-xbeg(44)		!xcalib=current length
		   ycalib=yend(45)-ybeg(45)
	   else
		  if(calbarX) then
		    call FIXAX(xmin,xmax,x1,x2,x,0)		!default length
		    xcalib=x		!1 major 'tic'
		  endif
		  if(calbarY) then
		    call FIXAX(ymin,ymax,x1,x2,x,0)
		    ycalib=x		!1 major 'tic'
		  endif
	   endif
		xbeg(44)=xmin+0.02*(xmax-xmin)		!x-origin
		ybeg(44)=ymin+0.02*(ymax-ymin)
		xbeg(45)=xbeg(44)			!y origin
		ybeg(45)=ybeg(44)			!y origin
		yend(44)=ybeg(44)			!horizontal
		xend(45)=xbeg(45)			!vertical
c	   Set bar lengths
		if(calbarX) then
		  xend(44)=xbeg(44)+xcalib
	        call FIXCALIB(titlex,xcalib)		!change number in title
		endif
		if(calbarY) then
		  yend(45)=ybeg(45)+ycalib
	        call FIXCALIB(titley,ycalib)		!change number in title
		endif
      endif
c        
c        First x-label
	   
         xtitle(1:40)=titlex
         ytitle(1:40)=titley
	   nlx=NBLANK1(xtitle)
	   
	   if(idraw(3).eq.-2.or.izoom.eq.1.or.iscal.eq.5) then
		ymid=0.5*(ymin + ymax)
		x=(0.5*(xbeg(44) + xend(44)))
	  	angle(3)=0			!set during initialisation
	  	ijus(3)=0
		y=ybeg(44)		!Y posn of X bar
		y=y-yoffset1/2
		y=y-yoffset1/4
	  	rx(3)=x		!keep start pos for x axis label
	  	ry(3)=y

		idraw(3)=1
		if(nlx.lt.1) then
			nlx=1
			idraw(3)=0
		endif
	   endif

	   if(sqrty) ytic1=sqrt(ytic1)
	   nly=NBLANK1(ytitle)
	   if(nly.lt.1) nly=1
	   idr9=idraw(4)
	   if(idraw(4).eq.-2.or.izoom.eq.1.or.iscal.eq.5) then
		xmid=0.5*(xmin + xmax)
		y=(0.5*(ybeg(45)+yend(45)))
	  	angle(4)=90.0		!set during initialisation
	  	ijus(4)=0 !!!!13/03     not -1
		roff=xoffset/2
		if(xend(45).gt.xmid) Roff=-Roff
		x=rx(4)		!X posn of Y bar
		x=x-roff
	  	rx(4)=x
	  	ry(4)=y
	    idraw(4)=1
		if(nly.lt.1) idraw(4)=0
	endif		!end of calibration bar drawing
	

c===========================================================
c                        X AXIS
c===========================================================
	
77	xtic1=xtic
	nlx=NBLANK1(xtitle)
	if(nlx.lt.1) nlx=1
	if(calbarX.or.calbary) goto 52
	ipower=0
	if(.not.logx) sx=1
	tmin=abs(xmin*sx)
	tmax=abs(xmax*sx)

	if(tmin.gt.999.) nolabx=1
	if(tmax.gt.999.) nolabx=1
	if(tmin.gt.99999.) ipower=1
	if(tmax.gt.99999.) ipower=1
	if(tmin.gt.0.and.tmin.lt.0.01) ipower=1
	if(tmax.gt.0.and.tmax.lt.0.01) ipower=1
      if(ipower.eq.1) then
     		call pow(xmax*sx,newz,ipow)
	      call intconv(ipow,string)
	      call CHAEXI(0.8,0.8,0.6,0.3)
	      nl=nblank1(string)
	      string='x10'//char(42)//char(69)//string(1:nl)
		nlx=nblank1(xtitle)

	endif
	if(logx.and.(tmax-tmin).ge.5) inumx=1
	   xmid=0.5*(xmin + xmax)
	   x=xmid
	   angle(3)=0			!set during initialisation
	   ijus(3)=0
	   if(.not.equal(xcross,xmin)) then
		if(xcross.gt.xmin.and.xcross.le.xmid) then
		   x=xcross+xoffset
		   ijus(3)=-1
		endif
		if(xcross.gt.xmin.and.xcross.gt.xmid) then
		   x=xcross-xoffset
		   ijus(3)=1
		endif
	   endif
	   y=ycros1	!Y posn of X axis (see SETXAXIS)
	   y=y-yoffset1
	
	   rx(3)=x
	   ymin2=ymin
	ymax2=ymax
	if(sqrty) ymin2=sqrt(ymin)
	if(sqrty) ymax2=sqrt(ymax)
	
!	y=ymin2-0.05*(ymax2-ymin2)
			!keep start pos for x axis label
	   ry(3)=y
	   if(sqrty) ry(3)=ymin2-0.15*(ymax2-ymin2)
C---------------------------------------------------------
c 				 Y AXIS
C---------------------------------------------------------

	
	ytic1=ytic
	if(sqrty) ytic1=sqrt(ytic1)
	
	if(nly.lt.1) nly=1
	
	
	ymid=0.5*(ymax+ymin)
	y=(ymid)
	if(sqrty) then
		y=0.5*(sqrt(ymax)+sqrt(ymin))
		ymid=y
	endif
	angle(4)=90.		!set during initialisation
	ijus(4)=0
	  ry(4)=y
	if(.not.equal(ycros1,ymin)) then
		if(ycros1.gt.ymin.and.ycros1.le.ymid) then
		   y=y+yoffset1
		   ijus(4)=-1
		endif
		if(ycross.gt.ymin.and.ycross.gt.ymid) then
		   y=y-yoffset1
		   ijus(4)=1
		endif
	   endif
	   x=(xcross)	!X posn of Y axis (see SETYAXIS)
	   i=ichar(frmty(1:1))-48
	   x=x-xoffset
	   rx(4)=x
	 
		
c **********************************************************************
c 			DRAW IN AXIS NUMBERS
c----------------------------------------------------------------------

	if(nx1.gt.15) nolabx=1
	
	cs=sizetext(6)
	
	
	if(idraw(6).eq.-2.or.izoom.eq.1.or.iscal.eq.5) then
	   inx=1		!counts numbers for textbox etc
	   if(logx) then
		x=AFIXR(xmin)
		if(x.lt.xmin) x=x+1.0
	   else
		sx=1.
		z=amod(xmin,xtic1)
		if(xmin.lt.0.) X=xmin-z
		if(xmin.gt.0.) X=xmin-z+xtic1
		if(xmin.eq.0.) X=xmin
	   endif
14	   continue		!return here for next number
	   j=inx+5		!index=31-55 for x axis
	   if(j.gt.30) goto 42
	   z=x*sx		!number actually drawn is scaled
	   x1=x  		!number position in world coord
	   y1=ycros1		!number position in world coord
	   nd10=nd
	   if (logx.and.inumx.eq.1) then
		if((nolabx.eq.1).and.mod(inx,2).eq.0) nd10=-100
		call make_number(z,x1,y1,.true.,ij,itx,dxs,dys,ipow,
     &	string,.true.,cnumx(inx),cexpx(inx),strnum(j),logx,logy)
		ijus(6)=ij
	
	   else
	      if(logx) z=10.**x
	      if(ipow.ne.-10000) then
		   k=abs(ipow)
		   if(ipow.gt.0) then
		     do lm=1,k
			  z=z/10.
		     enddo
		   else
		      do lm=1,k
			   z=z*10.
		      enddo
		   endif
		endif
		if(mod(inx,2).eq.0) then
               if(nolabx.eq.1.or.ipow.ne.-10000) nd10=-100
		endif
		call make_number(z,x1,y1,.true.,ij,itx,dxs,dys,ipow,
     &	string,.false.,cnumx(inx),cexpx(inx),strnum(j),logx,logy)
	
		ijus(6)=ij
	   endif
c  	   NB must redetermine box positions after rescale
	   rx(j)=x1
	   ry(j)=y1
	   ijus(j)=ij	!also predefine???
		
	   x=x+xtic1
	   inx=inx+1
	   if(x.le.xmax.and.inx.le.25) goto 14
	   if((x-xmax).lt.0.0000000001) goto 14
	   NUMBX=inx-1 !x axis at predefined position
	endif
	   do j=1,numbx
	   idraw(j+5)=1	!so drawn below next time, now CNUM defined
	   enddo

c--------------------------------------------------------
c 			Numbers for Y axis
c--------------------------------------------------------

	
	


42	continue

	
	if(idraw(31).eq.-2.or.izoom.eq.1.or.iscal.eq.5) then
	   half=0
	   iny=1
	   if(logy) then
		y=AFIXR(ymin)
		if(y.lt.ymin) y=y+1.0
		if(ymin1.eq.0.) ymax1=ymax1+1
	   else
		z=amod(ymin1,ytic1)
		if(ymin1.lt.0.) Y=ymin1-z
		if(ymin1.gt.0.) Y=ymin1-z+ytic1
		if(ymin1.eq.0.) Y=ymin1
		if(abs((ymax1-ymin1)/ytic1).gt.25) half=1
	   endif
	   ylast=ymax1
	   if(logy) ylast=ymax1
15	   continue		!return here for next number
	   j=iny+30	!index=56-80 for y axis
	   i=iny
	   if(j.gt.80.and.half.eq.0) goto 43
	   idraw(j)=1	!so drawn below next time, now CNUM defined
	   z=y*sy			!number actually drawn is scaled
	   x1=xcross     		!number position in world coord
	   y1=y		!number position in world coord
	   if (logy.and.inumy.eq.1) then
		nd10=nd
		
		call make_number(z,x1,y1,.false.,ij,ity,dxs,dys,ipow,
     &	string,.true.,cnumy(iny),cexpy(iny),strnum(j),logx,logy)
	   else
		if(sqrty) z=y**2
	      if(logy) z=10.**y
		nd10=nd
		if(half.eq.1.and.mod(iny,2).eq.0) nd10=-100
		
		call make_number(z,x1,y1,.false.,ij,ity,dxs,dys,ipow,
     &	string,.false.,cnumy(iny),cexpy(iny),strnum(j),logx,logy)
	
	   endif
	!   numby=iny
		if(z.gt.ylast) then
		!numby=iny-1
		!goto 43
		endif
9876	   rx(j)=x1
	   ry(j)=y1
	   ijus(j)=ij	!predefine???
	  
	   y=y+ytic1
	   if(y.gt.0.0.and.y.lt.0.0001) y=0.0
	   iny=iny+1
		if(sqrtY) then
			if(z.lt.ylast.and.iny.le.25) goto 15	
		else
			if(z.le.ylast.and.iny.le.25) goto 15	!next number
		endif
	   NUMBY=iny-1
43	   continue
	
	endif
	do j=31,31+numby
		ijus(j)=1
	enddo
	
C		TITLE

	ymin2=ymin
	ymax2=ymax
	if(sqrty) ymin2=sqrt(ymin)
	if(sqrty) ymax2=sqrt(ymax)
	xt=0.5*(xmin+xmax)
	yt=ymax2+0.025*(ymax2-ymin2)
	yt=ymax2+0.05*(ymax2-ymin2)!new
	if(idraw(1).eq.-2.or.izoom.eq.1.or.iscal.eq.5) then
	rx(1)=xt
	ry(1)=yt
	
	  idraw(1)=1		!title position now defined
	  ANGLE(1)=0.
	  ijus(1)=0
	  imode=0
c	  call fbox1_1(xt,yt,title1(1:nl),ANGLE(1),sizetext(1),
c     &     ijus(1),rxbox(1,1),rybox(1,1),imode)
	  rx0=rx(1)
	  ry0=ry(1)
	endif
	
c	Parameters
	if(theta(1).eq.-10000) then
	idraw(2)=-2
	parval=' '
	else 
	if(ncomp.gt.0) then
	if(idraw(2).ne.0.and.ifitype.ne.0.and.iparfirst.eq.-100)then
    
     	call PARTEXT(parval,ifitype,theta,ncomp,ifnt(2),sizetext(2),
     &    kmax)
	    nl=nblank1(parval)
	
		if(nl.gt.2) idraw(2)=-1	!so drawn with box by default
		endif
		if(idraw(2).eq.-2) then
		rx(2)=xmax-0.025*(xmax-xmin)
	    ymax2=ymax
	    if(sqrty) ymax2=sqrt(ymax)
	    ry(2)=ymax2-0.042*(ymax2-ymin)
	    if(ifitype.eq.4) ry(2)=ymax2-0.5*(ymax2-ymin)  !lower (for jump logos)
	    ijus(2)=right
		ANGLE(2)=0.
		endif
		
		
	else
		idraw(2)=-2
	parval=' '
	endif
	endif	
		iparfirst=0
		
	    
		doframe=.true.
52	continue
	
	xbeg4=xbeg(44)
	xend4=xend(44)
	ybeg4=ybeg(44)
	yend4=yend(44)
	xbeg5=xbeg(45)
	xend5=xend(45)
	ybeg5=ybeg(45)
	yend5=yend(45)

	end