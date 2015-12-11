	subroutine VPLOTQ5(XVAL,YVAL,NDAT,ncurvd,ijoin,syms,ndv1,ndimd,
     & XCAL,YCAL,NCAL,ncurvc,iline,ndc1,ndimc,ISYM,ILOG,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & y0,yinf,titlex,titley,ilabel,doframe,draft,itit,title1,
     & csize,ifont,theta,ifitype,ncomp,weight,isdev,quarter,iptype,
     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ivplot,
     & cjump,idev,interp,iplot,idelt,
     & kwi,kwj,icurvw,kmax,iver)
c
c VERSION OF VPLOT1 FOR AUTOMATIC PLOTTING FROM QUEUE (NO QUESTIONS ASKED)
c WITH DATA FROM ANY VPLOT VERSION.
c
c VPLOTQ5 (04/04/95 05:28pm) is version for use with VPLOT5, in which
c   theta() and weight() are also allocated, and icurvw() used for SD.
c VPLOTQ4 (09/23/94 03:50pm) is version for use with VPLOT4 and assumes
c that xval,yval,xcal,ycal have been ALLOCATED in calling prog as
c  ALLOCATE(Xval(ndv1,ndimd),Yval(ndv1,ndimd),Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
c  (ndv1, ndc1 added to parameters)
c Modif 02/11/95 12:56pm Added idelt to params (defined in PLOTOPT)
c Modif 02/15/93 02:44pm for Lahey V5.x. NB must be called with mono=true
c if idev.gt.6 because colour plots not possible yet (icol, mono added
c to common/tpos/)
c Modif 09/06/94 02:17pm to write plot queue number (=iplot)
c Modif 07/12/92 09:23pm so that quarter page plots use smaller symbol
c   size, by factor=symfac
c Modif 03/10/92 09:44am so all plotting done as for VPLOT2 so need only
c  data arrays that are (2048,10). ILINE definitions altered as for VPLOT2
c Modif 07/10/91 03:42pm: ifstcol(10) added as parameter to cope with plots
c   from VPLOTR that have more than 2048 data points
c Modif 04/25/91 09:51am so points not plotted if weight(i)=0 (unless isdev=-1
c   in which case weight not defined) (as in VPLOT1)
c
c INIPLT called with appropriate IDEV in PLOTAUT before call to VPLOTQ
c Fixed to work also for VPLOTR data 11/29/90 06:14pm
c Modif 01/21/91 03:08pm so that idev is a parameter (aRLTHough INIPLT already
c called with appropriate idev in PLOTAUT, still need idev here to control
c whether line thickness commands inserted)
c    For cjump data, reset logo positions to scaled units (not abs screen
c units) for VPLOTQ so they are OK for quarter page plots. Keep them
c within graph boundary since title is automatically written. At present
c v-jump logo is usually omitted, so put this below -c-jump logo at top
c (thus input values of iy1c,..,iy2v are ignored at present, except that
c iy1c=-1 still used to signal omission)
c
c	dimension XVAL(2048,ndimd),YVAL(2048,ndimd)
c	dimension XCAL(2048,ndimc),YCAL(2048,ndimc)
	real*4 Xval(ndv1,ndimd),Yval(ndv1,ndimd)
	real*4 Xcal(ndc1,ndimc),Ycal(ndc1,ndimc)
c for data
	integer*4 ndat(ndimd),isym(ndimd),ijoin(ndimd)
c for calc curves
	integer*4 ncal(ndimc),iline(ndimc)
	real*4 syms(ndimd),xrp(4),yrp(4)		!symbol size
	logical cjump,mono,interp,vert
c
c For vplotr:
	real*4 t1v(10),t2v(10),t1c(10),t2c(10)	!for jump logos and t=0 def.
	logical ivplot,dobar
c=	real theta(20),weight(100,ndimd)
	real theta(kmax),weight(kwi,kwj)
	integer icurvw(ndimd)
	dimension zseq(10)
c	character*1 ans,UC
	character*40 titlex,titley
	character*75 xtitle,ytitle	!output from LAXES
	character*64 TITLE1
	character parval*200		!to hold param values
	LOGICAL logx,logy,down,pon,zoomed
	logical sqrty,slock
	logical doframe,draft,landscape,quarter
	logical caplock,debug
	real*4 RLTH(100),rlt 		!for line thickness
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
c
c Arrays for arrows, extra text etc
c posn etc for 20 bits of new text + 6=param values
c NB start posn for text and arrows (and axis labels in LAXES)must be
c kept in world coord (real) not device coord (integer) or they come
c out in wrong place if graph outline is changed (as in 'FIX ON VDU')
c so rx(),ry() must all hold world coord
c	real RX(50),RY(50)
c	real rxbox(4,50),rybox(4,50)
c	integer IXJUS(50),IYJUS(50)
c	integer ANGle(50),idraw(50)
c	character*10 cnumx(20),cnumy(20),cexpx(20),cexpy(20)
c
	real RX(100),RY(100),ANGle(100),size(30),asize(30)
	real rxbox(4,100),rybox(4,100)
	integer IJUS(100)
	integer idraw(100),icol(100)
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25)
	character*80 newtext(20)		!extra text
	integer ifnt(30)

	real xb(10),yb(10),xe(10),ye(10)		!up to 10 arrows
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	integer iltype(10)				!line type for ditto
	logical discprt,colseq
	common/dp/discprt
	COMMON/TPOS/rx,ry,rxbox,rybox,ijus,ANGle,idraw,
     & ifnt,SIZE,RLTH,thbig,narrow,xb,yb,xe,ye,
     & nline,xlb,xle,ylb,yle,iltype,ntext,newtext,
     & cnumx,cnumy,cexpx,cexpy,numbx,numby,
     & nhline,yhline,xhlb,xhle,ilhtype,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel
	COMMON/cols/icol,mono
	common/logval/logx,logy,sqrty
	common/ekplot/jbff1,jbff2,colseq,ice,icpe

      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi

c
c values below are from c:\hgraph\CONSTS.FOR
      INTEGER LEFT, CENTER, RIGHT

      DATA LEFT, CENTER, RIGHT /-1, 0, 1 /     !new definition

	pon()=slock()
	debug()=caplock()
      EQUAL(x,y)=abs(x-y).lt.0.00001
c
c	if(debug()) pause '4'
3	format(i8)
4	format(g13.6)
	if(syms(1).lt.0.001) syms(1)=0.5
	zoomed=.false.	!needed for draw_arrow, draw_lines calls?
	just=0
	ihead=2
	if(idraw(6).ne.0.and.ifitype.ne.0) call PARTEXT(parval,ifitype,
     & theta,ncomp,ifnt(6),size(6),kmax)
c Initialisations:
	if(iptype.eq.1) then
	   ncjump=0
	   nvjump=0
	   ivplot=.false.
	else if(iptype.eq.11) then
	   isdev=-1
	endif
	if(iptype.ge.15) then
c Set isdev = 1 if ANY of the icurvw() are 0 or 1 so at least some weights
c are defined, otherwise set to -1
	   isdev=-1		!no weights at all
	   do i=1,ncurvd
		if(icurvw(i).ge.0) isdev=1
	   enddo
	endif
	landscape=.true.
	if(quarter) then
	   symfac=0.7		!1/sqrt(2) appropriate for 1/4 page plots
	else
	   symfac=1.0		!symbol size factor
	endif
c Set default line thickness
	thick=thbig 	!value (called thbig in common) set in plotaut
	rlt=0.4	!default thickness, unless reset
	if(draft) rlt=0
	do 6 i=1,100
6	RLTH(i)=rlt
	if(.not.draft) then
 	   RLTH(22)=0.3	!frame
	   RLTH(26)=0.2	!param value text
	   RLTH(27)=0.2	!symbols
	   RLTH(28)=0.2	!C-jump logo
	   RLTH(29)=0.2	!V-jump logo
	endif
c     rem out !!
c     call GINO
c	if(VIDEOTYP().ne.18) call VGA
c	call errswi(-1)
c	call brkswi(1)
c	call gsetcols(0)
c	call mode(18)

	call papenq(xp,yp,ipap)
	xlo1=xlo          !named ixlo1 in call so ixlo can be in common/hgv/
	xhi1=xhi		!ditto
	ylo1=ylo		!named iylo1 in call so iylo can be in common/hgv/
	yhi1=yhi		!ditto
      vxlo=0	! for VIEWPORT
	vxhi=xp
	vylo=0
	vyhi=yp

c Fill background colour
	if (quarter) then
	   x1=xlo-0.06*xp
	   x2=xhi+0.06*xp
	   y1=ylo-0.06*yp
	   y2=yhi+0.06*yp
	else
	   x1=vxlo
	   x2=vxhi
	   y1=vylo
	   y2=vyhi
	endif
	xrp(1)=x1
	yrp(1)=y1
	xrp(2)=x2
	yrp(2)=y1
	xrp(3)=x2
	yrp(3)=y2
	xrp(4)=x1
	yrp(4)=y2
	if(.not.mono.and.idev.eq.0) then
          call lincols(icol(71),idev)
c	    call RFILL(0,0,x1,x2,y1,y2)
	    call movto2(xrp(1),yrp(1))
	    call POFTO2(0,0,0,xrp,yrp,4)
	    call lincols(15,idev)		!bright white
	endif
	ic=15
	x20=x2-0.13*xp
	if(idev.eq.6) then
         ic=0
	   x20=x2-0.17*xp
	endif
	if(.not.cjump) then
	  call WRITQNUM(iplot,x20-0.03*xp,y2-0.03*yp,ic,idev)
	endif

	do i=1,30
		if(size(i).lt.2.) size(i)=3.
		asize(i)=size(i)
	      if(quarter) size(i)=size(i)*0.6
	enddo

c
c
c END OF INITIALISATIONS
c
	call setfnt(ifont)
c	call SETCSIZE(csize,isize)    !sets isize too
c NB cannot yet define position, box coordinates, justification, font etc
c for all strings that are defined on entry: viz Plot Title, axis
c labels,(axis numbers if eventually drawn separately), and
c parameter values. Cannot do here at start (so done once only unless
c text altered), because must be done after CALL SCALE, so must be done
c every time graph is redrawn
c
183	logx=ilog.eq.1.or.ilog.eq.3.or.ilog.eq.4.or.ilog.eq.6
	logy=ilog.eq.2.or.ilog.eq.3
	sqrty=ilog.eq.5.or.ilog.eq.6
	is=1
309	if(.not.sqrty) call gscale(xmin,xmax,ymin,ymax,is)
	if(sqrty) call gscale(xmin,xmax,sqrt(ymin),sqrt(ymax),is)
c NB FRAME call is now in LAXES
c Last line of param is so that position of text strings (for axis
c labels etc) can be defined when they are, at the first call,
c calculated internally.
	tlen=4.	!tick length
	if(quarter) tlen=2.
	xcalib=rx(2)-rx(1)
	ycalib=ry(4)-ry(3)
	if(iver.ne.1100.and.just.eq.0) then
 		do k=7,10
 		  idraw(k)=-2
 		enddo
		do k=31,80
		  idraw(k)=-2
		enddo
		call convstring(title1,64)
		call convstring(titlex,40)
		call convstring(titley,40)
		if(ntext.gt.0) then
		   do i=1,ntext
		      call convstring(newtext(i),80)
		   enddo
		endif
		if(ntext.gt.0) then
		   do i=1,ntext
		      call convstring(newtext(i),80)
		   enddo
		endif
	     if (nline.gt.0) then
		do i=1,nline
		  if(logx) then
		  	xle(i)=10**xle(i)
		  	xlb(i)=10**xlb(i)
		  endif
		  if(logy) then
		  	yle(i)=10**yle(i)
		  	ylb(i)=10**ylb(i)
		  endif
	       if(sqrty) then
			ylb(i)=ylb(i)**2
			yle(i)=yle(i)**2
	     	 endif
	      enddo
	     endif
	     if (nhline.gt.0) then
		do i=1,nhline
		if(logy) yhline(i)=10**yhline(i)
		if (logx) then
		   xhlb(i)=10**xhlb(i)
		   xhle(i)=10**xhle(i)
		endif
		if(sqrty) then
		   yhline(i)=yhline(i)**2
		endif
		enddo
	     endif
	     if (nvline.gt.0) then
		do i=1,nvline
		if(logx) xvline(i)=10**xvline(i)
		if (logy) then
		   yvlb(i)=10**yvlb(i)
		   yvle(i)=10**yvle(i)
		endif
		if(sqrty) then
		   yvlb(i)=yvlb(i)**2
		   yvle(i)=yvle(i)**2
		endif
	     enddo
	     endif
	     if (narrow.gt.0) then
		 do i=1,narrow
	   	if(logx) then
			xe(i)=10**xe(i)
			xb(i)=10**xb(i)
	  	 endif
	   	if(logy) then
		ye(i)=10**ye(i)
		yb(i)=10**yb(i)
	   	endif
	   	if(sqrty) then
		yb(i)=yb(i)**2
		ye(i)=ye(i)**2
	   	endif
	    	enddo
	     endif
	endif
	call broken(0)
	call LAXES2(xmin,xmax,ymin,ymax,xtic,ytic,xcross,ycross,
     & ntx,nty,itx,ity,tlen,titlex,titley,xtitle,ytitle,ilabel,idev,
     & size(7),size(8),size(9),ifnt(7),ifnt(8),ifnt(9),
     & thick,RLTH,inumx,inumy,0.0,ilog,doframe,mono,icol,xcalib,ycalib,
     & rx,ry,ANGle,ijus,idraw,rxbox,rybox,
     & numbx,numby,cnumx,cnumy,cexpx,cexpy)
	just=1
	if(.not.mono) call lincols(15,idev)		!bright white
c
136	continue
 	xabs1=xmin
      yabs1=ymin
 	xabs2=xmax
      yabs2=ymax
c	if(debug()) pause '11'
c
c========================================================================
c 					DO TITLE IF ANY
c========================================================================
	call broken(0)
	if(itit.ne.1) goto 1361
c	if(idraw(10).eq.0) goto 1361
	nl=NBLANK1(title1)	!terminate with ASCII 0
      call setfnt(ifnt(10))
	call setsize(size(10))
	ymin2=ymin
	ymax2=ymax
	if(sqrty) ymin2=sqrt(ymin)
	if(sqrty) ymax2=sqrt(ymax)
c position for title:
	if(idraw(10).eq.-2) then
	  idraw(10)=1		!title position now defined
	  rx(10)=0.5*(xmin+xmax)
	  ry(10)=ymax2+0.025*(ymax2-ymin2)
	  ANGle(10)=0.
	  ijus(10)=center
c       define rxbox,rybox for posn of TITLE1
	  call graspa(rx(10),ry(10),xsc,ysc)
	  call TEXTBOX(xsc,ysc,title1(1:nl),ANGle(10),size(10),
     &     ijus(10),rxbox(1,10),rybox(1,10),0)
	endif
	if(idev.gt.6) then
	   call LINWID(2*RLTH(25))
	else
	   call LINWID(RLTH(25))
	endif
c	if(.not.mono)
      call lincols(icol(25),idev)		!colour for title
	call graspa(rx(10),ry(10),xsc,ysc)
	call JUSTIFYSTRING(xsc,ysc,title1(1:nl),ANGle(10),size(10),
     & ijus(10))
c 	draw the box round title if necessary- could call TEXTBOX again, but
c	position already defined so quicker to call FRAME- but safer to use
c	former, in case graph size changed
	if(idraw(10).eq.-1) call TEXTBOX(xsc,ysc,title1(1:nl),
     &   ANGle(10),size(10),ijus(10),rxbox(1,10),rybox(1,10),1)
	   call setfnt(ifont)		!reset


1361	continue
	call broken(0)
c
c
c Draw param values, if req. NB need to set box position etc only the first
c time here (when ixt()=-1 still). If PARAM values are altered (in position,
c size etc) the relevant parameters will be adjusted at time of alteration.
	if(idraw(6).ne.0.and.ifitype.ne.0) then
      call setfnt(ifnt(6))
	call SETSIZE(size(6))	!this defines ISIZE
	  nl=NBLANK1(parval)	!terminate with ASCII 0
        call graspa(rx(6),ry(6),xsc,ysc)

	  if(idev.gt.6) then
	    call Linwid(2*RLTH(26))
	  else
	    call LINWID(RLTH(26))
	  endif
	  if(.not.mono) call lincols(icol(26),idev)		!colour for params
	  call JUSTIFYSTRING(xsc,ysc,parval(1:nl),ANGle(6),size(6),
     &	ijus(6))
c draw the box round PARVAL if necessary- could call TEXTBOX again, but
c position already defined so quicker to call IFRAMER
c	if(idraw(6).eq.-1.and.(.not.quarter)) call
c	if(idraw(6).eq.-1) call
c     &  IFRAMER(ixbox(1,6),iybox(1,6))
	  if(idraw(6).eq.-1.and.(.not.quarter)) then
	   call TEXTBOX(xsc,ysc,parval(1:nl),
     &   ANGle(6),size(6),ijus(6),rxbox(1,6),rybox(1,6),1)
	  endif
	  call setfnt(ifont)		!reset font
c	  call SETCSIZE(csize,isize)	!reset csize,isize
	endif
c
c Draw extra text if any
	call broken(0)
	if(ntext.gt.0) then
	  do 223 i=1,ntext
	   j=i+10
	   if1=ifnt(j)
c	   cs1=size(j)*csize
	   if(idev.gt.6) then
	      call LINWID(2*RLTH(i+30))
	   else
	      call LINWID(RLTH(i+30))
	   endif
	   call setfnt(ifnt(j))
c	   call setsize(size(j))
c	   if(.not.mono)
         call lincols(icol(i+30),idev)		!colour for text(i)
	   call graspa(rx(j),ry(j),xsc,ysc)
	   call JUSTIFYSTRING(xsc,ysc,newtext(i),ANGle(j),
     &    size(j),ijus(j))
	   if (idraw(j).eq.-1) then
		call TEXTBOX(xsc,ysc,newtext(i),
     &      ANGle(j),size(j),ijus(j),rxbox(1,j),rybox(1,j),1)
	   endif
c	   if(idraw(j).eq.-1) call FRAMER(rxbox(1,j),rybox(1,j))
223	   continue
	  call setfnt(ifont)	!reset current values
	endif
c
c==========================================================================
c                        DRAW ARROWS IF ANY
c==========================================================================
	call broken(0)
	ikey=0
	call draw_arr(ikey,narrow,xe,xb,ye,yb,ixfix,iyfix,
     &xv,yv,csize,idev,xabs1,yabs1,xabs2,yabs2,RLTH,zoomed,iver,ihead)

c ==============================================================================
c                       DRAW LINES IF ANY
c ==============================================================================
	call broken(0)
	ikey=0
	call draw_lin(ikey,nline,xlb,xle,ylb,yle,iltype,
     & nhline,yhline,xhlb,xhle,ilhtype,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel,
     & csize,idev,xabs1,yabs1,xabs2,yabs2,RLTH,
     & xmin,xmax,ymin,ymax,zoomed,iver)
	call broken(0)

3333  continue
	call broken(0)

c
c Draw jump logos if req (need to keep, and queue, the definitions eg
c whether to draw, ylevel for each and height of deflections), and
c for V-jump better read in voltages to make deflections proportional to them
c Define logo positions in scaled units (see top), and call JLOGO1=
c appropriately modified version of JLOGO to plot them
c	iy1c=6600   !initial positions for jump logos -must give when iscal=0
c	iy2c=6800
c	iy1v=6550
c	iy2v=6350
c NB idraw(28),(29) not queued -iy1=-1 if logo to be omitted (see VPLQR1)
c	ay1cg=ymax-0.03*(ymax-ymin)
c	ay2cg=ymax
c	ay1vg=ymax-0.04*(ymax-ymin)
c	ay2vg=ymax-0.07*(ymax-ymin)
c	call graspa(0.,ay1cg,dummy,ay1c)
c	call graspa(0.,ay2cg,dummy,ay2c)
c	call graspa(0.,ay1vg,dummy,ay1v)
c	call graspa(0.,ay2vg,dummy,ay2v)
	if(.not.IVplot) then
	   if(ncjump.gt.0.and.y1c.ge.-10000.) then
	      if(.not.mono) call lincols(icol(28),idev)		!colour for c-logo
		call JLOGO(xmin,xmax,ncjump,t1c,t2c,
     &           xoff1,idev,RLTH(28),1,y1c,y2c,icol(71))
	   endif
	   if(nvjump.gt.0.and.y1v.ge.-10000.) then
	      if(.not.mono) call lincols(icol(29),idev)		!colour for v-logo
		call JLOGO(xmin,xmax,nvjump,t1v,t2v,
     &         xoff1,idev,RLTH(29),1,y1v,y2v,icol(71))
	   endif
	endif
	tlen=4
c
c

c PLOT THE GRAPH
c     call broken(0)
	barleng=0.01*(xmax-xmin)	!bar on top/bottom of error bars
c Control which curves are plotted via Icurvd() now
	if(ncurvd.le.0) goto 303	!calc curve only

	do 50 j=1,ncurvd		!plot ncurvd data sets
	icolj0=icol(j)
	isd=isdev
	if(iptype.ge.15) isd=icurvw(j)
	if(.not.mono) call lincols(icol(j),idev)		!colour for jth data curve
	jflag=0
	linetype=ijoin(j)		!line to join data points
	iud=1				!draw line between points
	if(linetype.eq.-1) iud=0	!don't
	do 215 i=1,ndat(j),idelt
	 if(colseq.and.i.le.jbff2.and.i.ge.jbff1) then
	    icol(j)=icpe
	 else
	    icol(j)=icolj0
	 endif
	if(.not.mono) call lincols(icol(j),idev)		!colour for jth data curve
c	if(isdev.ge.0.and.weight(i,j).eq.0.) goto 215	!skip points with w=0
	if(isd.ge.0.and.weight(i,j).eq.0.) goto 215	!skip points with w=0
	xv=xval(i,j)
	yv=yval(i,j)
	if(ilog.eq.4) then
	   if(abs(yinf-yv).lt.1.e-30) goto 215	!for case of yv=yinf
	   if(yv.gt.yinf-x0) goto 215		!skip values above ymax!
	   if(yv.lt.y0+x0) goto 215		!skip values below ymin!
	   yv=(yv-y0)/(yinf-yv)	! Hill scale
	   if(yv.gt.x0) then
		yv=alog10(yv)
	   endif
	endif
	if(logx.and.xv.gt.x0) xv=alog10(xv)
	if(logy.and.yv.gt.x0) yv=alog10(yv)
	if(sqrty.and.yv.ge.0.) yv=sqrt(yv)
c start of modif #1 07/23/96 10:52am
c      =7 for 1/y vs 1/x
c      =8 for y/x vs y 		!Scatchard
c      =9 for y vs y/x		!Eadie Hofstee
c      =10 for x/y vs x		!eponym?
	 if(ilog.eq.7) then
		xv=1./xv
		yv=1./yv
	 else if(ilog.eq.8) then
		xv=yval(i,j)
		yv=yval(i,j)/xval(i,j)
	 else if(ilog.eq.9) then
		yv=yval(i,j)
		xv=yval(i,j)/xval(i,j)
	 else if(ilog.eq.10) then
		xv=xval(i,j)
		yv=xval(i,j)/yval(i,j)
	 endif
c end of modif #1 07/23/96 10:52am
	if(xv.lt.xmin.or.xv.gt.xmax) goto 215
	if(yv.lt.ymin.or.yv.gt.ymax) goto 215
	if(jflag.eq.0) call graMOV(xv,yv)   !move to 1st point in range
	if(iud.eq.1) then
	   if(idev.gt.6) then
	     call Linwid(2*RLTH(j))
	   else
	     call LINWID(RLTH(j))
	   endif
	endif
c==========temp for debug of cjump plots
	if(yv.gt.ymax) then
	   print 700,i,yv,ymax
700	   format(' yv(',i8,') = ',g13.6,' is bigger than ymax = ',g13.6)
	endif
c=============end of temp
	if(jflag.gt.0) then
	   call linvis(iud)
         call broken(linetype)
         call gralin(xv,yv)   !join with line from last point
      endif
c need thinner lines to draw symbols? and for error bars?
	 if(idev.gt.6) then
	     call Linwid(2*RLTH(27))
	   else
	     call LINWID(RLTH(27))
	   endif
	 call graspa(xv,yv,xsp,ysp)
	call jSYMBOL(xsp,ysp,isym(j),syms(j)*symfac,icol(j),idev)     !symbol if isym>0
	if(.not.mono) call lincols(icol(j),idev)	!restore colour after SYMBOL
c
c start of modif #1 07/23/96 10:52am
c Plot error bars
	if(isd.eq.1) then
	   if(idev.gt.6) then
	     call Linwid(2*RLTH(30))
	   else
	     call LINWID(RLTH(30))
	   endif
	   sdmax=5.0*abs(ymax-ymin)	!largest sd to be plotted
	   if(logy) sdmax=5.0*abs(10.**ymax - 10.**ymin)
	   if(weight(i,j).gt.(1.0/sdmax**2)) then
	      sdev=sqrt(1.0/weight(i,j))
c Do lower limit first
		yv1=yval(i,j)-sdev			!lower limit
		dobar=.true.		!draw bar at top/bottom of sd bar
		linetype=0		!continuous
		if(logy) then
		   if(yv1.gt.x0) then
			yv1=alog10(yv1)
			linetype=0		!continuous
		   else
			yv1=ymin		!ymin is already log
			linetype=2		!short dash if lower limit undefined
			dobar=.false.
		   endif
		else if(ilog.eq.4) then
		   yv1=(yv1-y0)/(yinf-yv1)	! Hill scale
		   if(yv1.gt.x0) then
			yv1=alog10(yv1)
			linetype=0		!continuous
		   else
			yv1=ymin		!ymin is already log
			linetype=2		!short dash if lower limit undefined
			dobar=.false.
		   endif
		else if(ilog.eq.7) then
		   yv1=1./yv1
c  for double reciprocal plot, y-sdev gives upper limit -if neg set to ymax
		   if(yv1.lt.0.) then
			yv1=ymax
			linetype=2
		   endif
		else if(ilog.eq.8) then
		   yv1=yv1/xval(i,j)
		   if(yv1.lt.0.) then
			yv1=ymin
			linetype=2
		   endif
		else if(ilog.eq.9) then
		   yv1=yv1
		   if(yv1.lt.0.) then
			yv1=ymin
			linetype=2
		   endif
		else if(ilog.eq.10) then
		   yv1=xval(i,j)/yv1
		   if(yv1.lt.0.) then
			yv1=ymax
			linetype=2
		   endif
		endif
		if(yv1.le.ymin) then
		   dobar=.false.
		   yv1=ymin
		else if(yv1.ge.ymax) then
		   dobar=.false.
		   yv1=ymax
		endif
            call broken(linetype)
		call gralin(xv,yv1)
		if(dobar) then
		   call gramov(xv-barleng,yv1)
		   call broken(0)
		   call gralin(xv+barleng,yv1)
		endif
		call graMOV(xv,yv)
c Now upper limit
22		yv1=yval(i,j) + sdev		!upper limit
		dobar=.true.		!draw bar at top/bottom of sd bar
		linetype=0		!continuous
		if(logy) then
		   if(yv1.gt.x0) then
			yv1=alog10(yv1)
			linetype=0		!continuous
		   else
			yv1=ymin		!ymin is already log
			linetype=2		!short dash if lower limit undefined
			dobar=.false.
		   endif
		else if(ilog.eq.4) then
		   yv1=(yv1-y0)/(yinf-yv1)	! Hill scale
		   if(yv1.gt.x0) then
			yv1=alog10(yv1)
			linetype=0		!continuous
		   else
			yv1=ymax		!ymax is already log
			linetype=2		!short dash if upper limit undefined
			dobar=.false.
		   endif
		else if(ilog.eq.7) then
		   yv1=1./yv1
		else if(ilog.eq.8) then
		   yv1=yv1/xval(i,j)
		else if(ilog.eq.9) then
		   yv1=yv1
		else if(ilog.eq.10) then
		   yv1=xval(i,j)/yv1
		endif
		if(yv1.le.ymin) then
		   dobar=.false.
		   yv1=ymin
		else if(yv1.ge.ymax) then
		   dobar=.false.
		   yv1=ymax
		endif
            call broken(linetype)
		call gralin(xv,yv1)
		if(dobar) then
		   call graMOV(xv-barleng,yv1)
		   call broken(0)
		   call gralin(xv+barleng,yv1)
		endif
		call graMOV(xv,yv)
c ilog =8 for y/x vs y 		!Scatchard
c      =9 for y vs y/x		!Eadie Hofstee
c For ilog=8 or 9 plot horizontal error bars too
	 	if(ilog.eq.8.or.ilog.eq.9) then
c Do lower limit for horizontal error
		   dobar=.true.		!draw bar at top/bottom of sd bar
		   linetype=0		!continuous
		   if(ilog.eq.8) then
			xv1=yval(i,j)-sdev		!lower limit
		   else if(ilog.eq.9) then
			xv1=(yval(i,j)-sdev)/xval(i,j)
		   endif
		   if(xv1.lt.0.) then
			xv1=xmin
			linetype=2
		   endif
		   if(xv1.le.xmin) then
			dobar=.false.
			xv1=xmin
		   else if(xv1.ge.xmax) then
			dobar=.false.
			xv1=xmax
		   endif
               call broken(linetype)
		   call gralin(xv1,yv)
		   if(dobar) then
			call graMOV(xv1,yv-barleng)
			call broken(0)
			call gralin(xv1,yv+barleng)
		   endif
		   call gramov(xv,yv)
c Now upper limit for horizontal error
		   if(ilog.eq.8) then
			xv1=yval(i,j)+sdev		!upper limit
		   else if(ilog.eq.9) then
			xv1=(yval(i,j)+sdev)/xval(i,j)
		   endif
		   dobar=.true.		!draw bar at top/bottom of sd bar
		   linetype=0		!continuous
		   if(xv1.le.xmin) then
			dobar=.false.
			xv1=xmin
		   else if(xv1.ge.xmax) then
			dobar=.false.
			xv1=xmax
		   endif
               call broken(linetype)
		   call gramov(xv1,yv)
		   if(dobar) then
			call graMOV(xv1,yv-barleng)
			call broken(0)
			call gralin(xv1,yv+barleng)
		   endif
		   call graMOV(xv,yv)
		endif
	   endif
	 endif
	jflag=1
215	continue
50	continue

c
303	continue

	if(ncurvc.le.0) goto 999

	linetype=0		!always cont line- dashes synthesized via zseq
	do 54 j=1,ncurvc		!plot ncurvc curves
	   if (iline(j).lt.0) goto 54
	   ij=iabs(iline(j))
	if(idev.gt.6) then
	     call Linwid(2*RLTH(j+10))
	else
	     call LINWID(RLTH(j+10))
	endif
	if(.not.mono) call lincols(icol(j+10),idev)		!colour for jth calc curve
	if(ij.gt.0) goto 310		!dashed calc curve
c
	Jflag=0
	do 220 k=1,ncal(j)
	xv=xcal(k,j)
	yv=ycal(k,j)
	if(logx.and.xv.ge.x0) xv=alog10(xv)
	if(logy.and.yv.ge.x0) yv=alog10(yv)
	if(sqrty.and.yv.ge.0.) yv=sqrt(yv)
	if(ilog.eq.4) then
	   if(abs(yinf-yv).lt.1.e-30) goto 220	!for case of yv=yinf
	   if(yv.gt.yinf-x0) goto 220	!skip values above ymax!
	   if(yv.lt.y0+x0) goto 220	!skip values below ymin!
	   yv=(yv-y0)/(yinf-yv)	! Hill scale
	   if(yv.gt.x0) then
		yv=alog10(yv)
	   endif
	endif
c start of modif #1 07/23/96 10:52am
	x0=1.e-36		!smallest value for taking logs
	if(ilog.eq.7) then
	   if(xv.lt.x0.or.yv.lt.x0) goto 220
	   xv=1./xv
	   yv=1./yv
	else if(ilog.eq.8) then
	   xv=ycal(k,j)
	   yv=ycal(k,j)/xcal(k,j)
	else if(ilog.eq.9) then
	   if(xv.lt.x0) goto 220
	   xv=ycal(k,j)/xcal(k,j)
	   yv=ycal(k,j)
	else if(ilog.eq.10) then
	   if(yv.lt.x0) goto 220
	   xv=xcal(k,j)
	   yv=xcal(k,j)/ycal(k,j)
	endif
c end of modif #1 07/23/96 10:52am
	if(xv.lt.xmin.or.xv.gt.xmax) goto 220
	if(yv.lt.ymin.or.yv.gt.ymax) goto 220
	if(jflag.eq.0) call graMOV(xv,yv)	!1st point in range
	if(jflag.gt.0) then
         call broken(linetype)
         call gralin(xv,yv)	!rest of points
      endif
	jflag=1
220	continue
	goto 311
c
c now section to draw dashed etc calc curves
310	continue
c  define dash sequences: specify lengths of each repeating sequence
c  starting with bright (down=true). Lengths specified as percent
c  of length of the X axis.
	goto(702,703,704,705,706),ij
702	kseq=2
	zseq(1)=0.3
	zseq(2)=0.5
	goto 312
703	kseq=2
	zseq(1)=1.5
	zseq(2)=2.
	goto 312
704	kseq=2
	zseq(1)=5.
	zseq(2)=2.5
	goto 312
705	kseq=4
	zseq(1)=6.
	zseq(2)=2.
	zseq(3)=1.5
	zseq(4)=2.
	goto 312
706	kseq=6
	zseq(1)=6.
	zseq(2)=2.
	zseq(3)=1.5
	zseq(4)=2.
	zseq(5)=1.5
	zseq(6)=2.
	goto 312
c
312	continue
	sfac=(yhi-ylo)/(xhi-xlo)		!O.K.?
	xr=0.01*(xmax-xmin)
	yr=0.01*(ymax-ymin)
	if(sqrty) yr=0.01*(sqrt(ymax)-sqrt(ymin))
	k=1
 	zleft=zseq(k)
	down=.true.
	x0=1.e-36		!smallest value for logs
	Jflag=0
	do 313 i=1,ncal(j)
	xv=xcal(i,j)
	yv=ycal(i,j)
	if(ilog.eq.4) then
	   if(abs(yinf-yv).lt.1.e-30) goto 313	!for case of yv=yinf
	   if(yv.gt.yinf-x0) goto 313	!skip values above ymax!
	   if(yv.lt.y0+x0) goto 313	!skip values below ymin!
	   yv=(yv-y0)/(yinf-yv)	! Hill scale
	   if(yv.gt.x0) then
		yv=alog10(yv)
	   else
		goto 313
	   endif
	endif
	if(logx) then
	   if(xv.gt.x0) then
		xv=alog10(xv)
	   else
		goto 313
	   endif
	endif
	if(logy) then
	   if(yv.gt.x0) then
		yv=alog10(yv)
	   else
		goto 313
	   endif
	endif
	if(sqrty.and.yv.ge.0.) yv=sqrt(yv)
c 	start of modif #1 07/23/96 10:52am
	x0=1.e-36		!smallest value for taking logs
c
	if(sqrty.and.yv.ge.0.) yv=sqrt(yv)
	if(ilog.eq.7) then
	   if(xv.lt.x0.or.yv.lt.x0) goto 313
	   xv=1./xv
	   yv=1./yv
	else if(ilog.eq.8) then
	   xv=ycal(k,j)
	   yv=ycal(k,j)/xcal(k,j)
	else if(ilog.eq.9) then
	   if(xv.lt.x0) goto 313
	   xv=ycal(k,j)/xcal(k,j)
	   yv=ycal(k,j)
	else if(ilog.eq.10) then
	   if(yv.lt.x0) goto 313
	   xv=xcal(k,j)
	   yv=xcal(k,j)/ycal(k,j)
	endif
c 	end of modif #1 07/23/96 10:52am
c	if(xv.lt.xmin.or.xv.gt.xmax) goto 313
c	if(yv.lt.ymin.or.yv.gt.ymax) goto 313
	if(jflag.eq.0) then
	   if(xv.lt.xmin) xv=xmin
	   if(xv.gt.xmax) xv=xmax
	   if(yv.lt.ymin) yv=ymin
	   if(yv.gt.ymax) yv=ymax
	   call graMOV(xv,yv)	!move to 1st point in range
	   jflag=1
	   goto 317
	else
	   if(xv.lt.xmin.or.xv.gt.xmax) goto 313
	   if(yv.lt.ymin.or.yv.gt.ymax) goto 313
	endif
c     Now part done for all points after 1st in range
314	dxn=(xv-xvlast)/xr
	dyn=sfac*(yv-yvlast)/yr
	vert=(dxn.eq.0.).or.abs(dxn).lt.(1.e-20*abs(dyn))    !line is vertical
	if(vert) then
	   b=1.0
	   if(dyn.lt.0.) b=-1.0
	else
	   b=dyn/dxn	!slope of line
	endif
c     calc zn=dist from last point to current one in units= percent
c     of length of X axis
318	zn=sqrt(dxn*dxn + dyn*dyn)
c 	next bit when amount left to draw extends beyond (or exactly
c 	up to) the current point- go to this point
	if(zleft.ge.zn) then
315	   if(.not.down) call graMOV(xv,yv)
	   if(down) then
		call broken(linetype)
            call gralin(xv,yv)
	   endif
	   zleft=zleft-zn	!amount of this segment still to be drawn
	   if(zleft.gt.0.) goto 317
	   down=.not.down	!zleft=0 i.e.segment reaches current point exactly
	   k=k+1
	   if(k.gt.kseq) k=1
	   zleft=zseq(k)
	else
c  	   next bit done when amount of line remaining to be drawn does
c 	   not extend as far as current point- keep drawing segments (with
c 	   slope as between last point and current one) until it does.
	   if(vert) then
		xv1=xvlast
	      yv1=yvlast + b*zleft*yr/sfac	!b=-1 or +1 here
	   else
		xv1=xvlast + zleft*xr/sqrt(1.+b*b)
		yv1=yvlast + b*(xv1-xvlast)*yr/(sfac*xr)
	   endif
	   if(xv1.lt.xmin) xv1=xmin
	   if(xv1.gt.xmax) xv1=xmax
	   if(yv1.lt.ymin) yv1=ymin
	   if(yv1.gt.ymax) yv1=ymax
	   if(.not.down) call graMOV(xv1,yv1)
	   if(down) then
		call broken(linetype)
            call gralin(xv1,yv1)
	   endif
	   dxn=(xv-xv1)/xr		!for dist from xv1,yv1 to current point
	   dyn=sfac*(yv-yv1)/yr
	   xvlast=xv1
	   yvlast=yv1
	   down=.not.down		!prepare for next segment
	   k=k+1
	   if(k.gt.kseq) k=1
	   zleft=zseq(k)
	   goto 318	!repeat until current point reached
	endif
c
317	xvlast=xv
	yvlast=yv
313	continue	!end of loop for points
311	continue
54	continue	!loop for each calc curve
c
999	continue
	do i=1,30
	   size(i)=asize(i)
	enddo
	RETURN
	END


