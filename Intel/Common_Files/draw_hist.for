	subroutine draw_hist(xval,yval,icurvd,ncurvd,ndelt,logity,logx,
     &logy,sqrty,y0,yinf,x0,ilog,xmin,xmax,ymin,ymax,idev,ndat,ijoin,
     &icol,thick,ndv1,ndimd,xwbase,lt2)

	use gino_f90
	DIMENSION ICOL(250),ITYPE(250),THICK(250),IDRAW(250)
	real XVAL(0:ndv1,ndimd),YVAL(0:ndv1,ndimd)
	integer ndat(ndimd),icurvd(ndimd),ijoin(ndimd)
	real xvert(4),yvert(4)	!vertices for Hgraph FILL call
	real xsvert(4),ysvert(4)	!vertices for Hgraph FIL

	logical off,bad,logx,logy,sqrty,logity,landscape
	common/pixpos/ixgrid,iygrid,ixpix,iypix,wid,landscape
	call broken(0)
	ymaxq=ymax
	yminq=ymin
	if(sqrty) ymaxq=sqrt(ymax)
	if(sqrty) yminq=sqrt(ymin)
	do 50 j1=1,ncurvd		!plot ncurvd histograms
	j=icurvd(j1)			!histogram to be plotted
	call LINWID(thick(j+100))
	
	if(idev.gt.0) then
!	call gSetLineWidth(0.0)
!	call gEnqSelectedPen(icola,wid,itypea) ! This is the actual thickness
!	call gSetLineWidth(wid*5.0*100.0)
	endif
	call glincols(icol(j+100),idev)		!colour for jth histo bars
	

	ybase=ymin
	if(logy) ybase=10.**ymin  !need non-log value (log taken in logvl if req)
	ybase=ybase+1.e-5*abs(ybase)

	flo=Yval(0,j)
	if(flo.lt.0.0001) goto 20	!skip lo bin if not needed
	if(logx.or.logy.or.flo.le.0.) goto 20	!no LO bin

	dxlo=xval(1,j)-xval(0,j)   !define width in calling prog now!
	xlow=xval(1,j)-dxlo	!=xval(0,j) now (symbol XLO is used for locate!)
	if(xlow+1.e-6.ge.xmin) goto 22
	xlow=xmin
	dxlo=xval(1,j)-xmin
22	continue
c Express FLO as freq density if dxlo.ne.xwbase ? Problem here, because
c may call with xwbase=0 to suppress 'per 10 ms' etc in LAXES2
	if((xwbase.gt.1.e-5).and.abs(xwbase-dxlo).gt.1.e-4) then
	  if(dxlo.ne.0.) flo=flo*xwbase/dxlo	!use FLO as number not density
c		  if(pon()) write(7,21) dxlo,flo
c	  if(discprt) write(7,21) dxlo,flo
21	  format(' Bin width for LO bin= ',g13.6,'. Freq density= ',g13.6)
c	  call DCFORMAT(dxlo,8,3,cans)
c	  call WDIALOG(1,'Bin width for LO bin = '//CHARNB(cans),ict)
c	  call DCFORMAT(flo,8,3,cans)
c	  call WDIALOG(1,'Frequency density = '//CHARNB(cans),ict)
	endif
c
	call logvl(xlow,xv,xmin,xmax,logx,off,bad,.false.) !ixlow on scale?
c=	if(off.or.bad) goto 20		!no lo bin if not
 	call logvl(flo,yv,yminq,ymaxq,logy,off,bad,sqrty)
c if Y off scale it is plotted as ymaxq (set in logvl)
c=	if(off.or.bad) goto 20		!omit LO bin if Y off scale
	call glincols(icol(110),idev)		!colour end bins
	linetype=lt2			!line type for FLO bin
c========
	call broken(linetype)
	if(xlow.eq.xmin) then		!omit 1st vert
	   call movto2(xv,yv)
	else
	   call movto2(xv,0.)
	   call linto2(xv,yv)
	endif
	xvert(1)=xv
	yvert(1)=0.
	xvert(2)=xv
	yvert(2)=yv
	call logvl(xval(1,j),xv,xmin,xmax,logx,off,bad,.false.)
	call linto2(xv,yv)		!draw top
	xvert(3)=xv
	yvert(3)=yv
c   go back to baseline
	call logvl(ybase,yv,yminq,ymaxq,logy,off,bad,sqrty)
	if(yval(1,j).gt.flo) then
	   linetype=ijoin(j)	!contin line
	   call broken(linetype)
	endif
	call linto2(xv,yv)		!draw vert
	xvert(4)=xv
	yvert(4)=yv
	
	call movto2(xvert(1),yvert(1))
	call POFTO2(0,0,0,xvert,yvert,4)

c	
20	continue
c    start from baseline
c	call LINWID(rlth(j))
	call glincols(icol(j+100),idev)		!colour for jth histo bars
	linetype=ijoin(j)
	k=0
38	continue 
      k=k+1
	call logvl(xval(k,j),xv,xmin,xmax,logx,off,bad,.false.)
	if(off.or.bad) goto 38	!look for 1st good X if X off scale
	call logvl(ybase,yv,yminq,ymaxq,logy,off,bad,sqrty)
	call movto2(xv,yv)
	call logvl(yval(k,j),yv,yminq,ymaxq,logy,off,bad,sqrty)
c	if(off.or.bad) goto 25
	call broken(linetype)
	call linto2(xv,yv)	!draw 1st vert
25	continue
c  Now loop to draw top and 2nd vert. Starts at point x1,y1
	do 215 i=k,ndat(j)		!ndat=nbin; x=x(1) to x(nbin+1)
	  call logvl(xval(i+1,j),xv,xmin,xmax,logx,off,bad,.false.)
c already checked that x > xmin. Once X > Xmax can leave loop
c altogther as all subsequent X will be off scale too
c	  if(off.or.bad) goto 215     !skip unless x is on scale
	  if(xv.gt.xmax) goto 39
	  call broken(linetype)
	  call linto2(xv,yv)		!draw top
	  ynext=ybase   !for last bin- xval(ndat+1) is defined but not yval
 	  if(i.lt.ndat(j)) ynext=yval(i+1,j)
	  call logvl(ynext,yv,yminq,ymaxq,logy,off,bad,sqrty)
c if Y off scale it is plotted as yminq or ymaxq (set in logvl)
c	  if(off.or.bad) goto 215     !skip unless x is on scale
	  call linto2(xv,yv)		!draw vertical
215	continue
c
c  lastly HI bin. Could be improved later to plot (with recalc FHI)
c even after rescaling to reduce Xmax (no data to recalc FHI if Xmax
c is increased by rescaling)
c If regular histo went above xmax, OFF will be true here
39	continue
	fhi=Yval(ndat(j)+1,j)
	if(fhi.lt.0.0001) goto 36	!no hi bin
	xhigh=xval(ndat(j)+2,j)
	call logvl(xhigh,xv1,xmin,xmax,logx,off,bad,.false.)
	call logvl(xval(ndat(j)+1,j),xv,xmin,xmax,logx,off,bad,.false.)
	call logvl(FHI,yv,yminq,ymaxq,logy,off,bad,sqrty)
c  plot as ymaxq if FHI > ymaxq
	linetype=lt2
	call glincols(icol(110),idev)		!colour end bins
!	call broken(linetype)
	call broken(0)
	call linto2(xv,yv)		!1st vert
	if(xv1.eq.0.0) xv1=xmax
	call linto2(xv1,yv)	!hor
	call logvl(ybase,yv2,yminq,ymaxq,logy,off,bad,sqrty)
	call linto2(xv1,yv2)	!last vert
	xvert(1)=xv
	yvert(1)=0.
	xvert(2)=xv
	yvert(2)=yv
	xvert(3)=xv1
	yvert(3)=yv
	xvert(4)=xv1
	yvert(4)=yv2
	
c	call movto2(xvert(1),yvert(1))
c	call POFTO2(0,0,0,xvert,yvert,4)
c	call FILL1(xvert,yvert,4,12,0,0,0)
	linetype=0
	call glincols(icol(j+100),idev)		!colour for jth histo bars
36	continue
c
50	continue


	call gFlushGraphics()

	end