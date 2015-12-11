	subroutine VPLOTQ4(XVAL,YVAL,NDAT,ncurvd,ijoin,syms,ndv1,ndimd,
     & XCAL,YCAL,NCAL,ncurvc,iline,ndc1,ndimc,ISYM,ILOG,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & y0,yinf,titlex,titley,ilabel,doframe,draft,itit,title1,
     & csize,ifont,theta,ifitype,ncomp,weight,isdev,quarter,iptype,
     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,
     & cjump,idev,ifstcol,interp,iplot,idelt)
c
c VERSION OF VPLOT1 FOR AUTOMATIC PLOTTING FROM QUEUE (NO QUESTIONS ASKED)
c WITH DATA FROM ANY VPLOT VERSION.
c
c VPLOTQ4 (09/23/94 03:50pm) is version for use with VPLOT4 and assumes
c that xval,yval,xcal,ycal have been ALLOCATED in calling prog as
c  ALLOCATE(Xval(ndv1,ndimd),Yval(ndv1,ndimd),Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
c  (ndv1, ndc1 added to parameters)
c Modif 02/11/95 12:56pm Added idelt to params (defined in PLOTOPT)
c Modif 02/15/93 02:44pm for Lahey V5.x. NB must be called with mono=true
c if idev.ge.5 because colour plots not possible yet (icol, mono added
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
c Modif 01/21/91 03:08pm so that idev is a parameter (although INIPLT already
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
	dimension ndat(ndimd),isym(ndimd),ijoin(ndimd),ifstcol(ndimd)
c for calc curves
	dimension ncal(ndimc),iline(ndimc)
	real syms(ndimd)		!symbol size
	logical cjump,mono,interp
c
c For vplotr:
	real*4 t1v(10),t2v(10),t1c(10),t2c(10)	!for jump logos and t=0 def.
	logical ivplot
	real theta(20),weight(100,ndimd)
	dimension zseq(10)
c	character*1 ans,UC
	character*40 titlex,titley
	character*75 xtitle,ytitle	!output from LAXES
	character*44 TITLE1
	character parval*150		!to hold param values
	LOGICAL logx,logy,down,pon
	logical sqrty,slock
	logical doframe,draft,landscape,quarter
	logical caplock,debug
	integer*2 ilt,lth(100),lth 		!for line thickness
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
c	integer iangle(50),idraw(50)
c	character*10 cnumx(20),cnumy(20),cexpx(20),cexpy(20)
c
	real RX(100),RY(100)
	real rxbox(4,100),rybox(4,100)
	integer IXJUS(100),IYJUS(100)
	integer iangle(100),idraw(100),icol(100)
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25)
	character*80 newtext(20)		!extra text
	integer ifnt(30)
	real csfac(30)
	real xb(10),yb(10),xe(10),ye(10)		!up to 10 arrows
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	integer iltype(10)				!line type for ditto
	logical discprt
	common/dp/discprt
	COMMON/TPOS/rx,ry,rxbox,rybox,ixjus,iyjus,iangle,idraw,
     & ifnt,csfac,lth,thbig,narrow,xb,yb,xe,ye,
     & nline,xlb,xle,ylb,yle,iltype,ntext,newtext,
     & cnumx,cnumy,cexpx,cexpy,numbx,numby,
     & nhline,yhline,xhlb,xhle,ilhtype,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel
	COMMON/cols/icol,mono
	common/hgv/ivxlo,ivxhi,ivylo,ivyhi,ixlo,ixhi,iylo,iyhi

c
c        INCLUDE 'c:\hgraph\CONSTS.FOR'
c*      INCLUDE 'c:\hgraph\HGRGLB.DEC'
c*      INCLUDE 'c:\f77l3\SPINATT.HDR		!modif version of SPINATTR.HDR
c values below are from c:\hgraph\CONSTS.FOR
      INTEGER LEFT, CENTER, RIGHT, BASE
c      INTEGER BELOW, BOTTOM, ABOVE, TOP
      DATA LEFT, CENTER, RIGHT, BASE /0, 2, 1, 3/     !new definition
c      DATA BELOW, BOTTOM, ABOVE, TOP /0, 0, 1, 1/     !new definition
c
	pon()=slock()
	debug()=caplock()
	LTH(i)=int2(ifixr(thbig*float(int4(lth(i)))))	!sets line thickness
c
c	if(debug()) pause '4'
3	format(i8)
4	format(g13.6)
c Initialisations:
	if(iptype.eq.1) then
	   ncjump=0
	   nvjump=0
	   ivplot=.false.
	else if(iptype.eq.11) then
	   isdev=-1
	endif
	landscape=.true.
	if(quarter) then
	   symfac=0.7		!1/sqrt(2) appropriate for 1/4 page plots
	else
	   symfac=1.0		!symbol size factor
	endif
c Set default line thickness
	thick=thbig 	!value (called thbig in common) set in plotaut
	ilt=20		!default thickness, unless reset
	if(draft) ilt=0
	do 6 i=1,100
6	lth(i)=ilt
	if(.not.draft) then
 	   lth(22)=25	!frame
	   lth(26)=10	!param value text
	   lth(27)=15	!symbols
	   lth(28)=20	!C-jump logo
	   lth(29)=20	!V-jump logo
	endif
c
c Fill background colour
	if(quarter) then
	   ix1=ixlo-600
	   ix2=ixhi+570
	   iy1=iylo-420
	   iy2=iyhi+440
	else
	   ix1=ivxlo
	   ix2=ivxhi
	   iy1=ivylo
	   iy2=ivyhi
	endif
	if(.not.mono) then
	    call FILLRECT(ix1,ix2,iy1,iy2,icol(71))
	    call COLTYP1(15)		!bright white
	endif
	ic=15
	if(.not.cjump) then
	  call WRITQNUM(iplot,ix2-1300,iy2-250,ic,idev)	!write plot queue # on screen/plot
	endif
c
	if(idraw(6).ne.0.and.ifitype.ne.0) call PARTEXT(parval,ifitype,
     & theta,ncomp,ifnt(6),csfac(6)*csize)  !define text string for param values
c
c END OF INITIALISATIONS
c
	call SETFONT1(ifont)
	call SETCSIZE(csize,isize)    !sets isize too
c NB cannot yet define position, box coordinates, justification, font etc
c for all strings that are defined on entry: viz Plot Title, axis
c labels,(axis numbers if eventually drawn separately), and
c parameter values. Cannot do here at start (so done once only unless
c text altered), because must be done after CALL SCALE, so must be done
c every time graph is redrawn
c
183	logx=ilog.eq.1.or.ilog.eq.3.or.ilog.eq.6
	logy=ilog.eq.2.or.ilog.eq.3
	sqrty=ilog.eq.5.or.ilog.eq.6
309	if(.not.sqrty) call scale(xmin,xmax,ymin,ymax)
	if(sqrty) call scale(xmin,xmax,sqrt(ymin),sqrt(ymax))
c NB FRAME call is now in LAXES
c Last line of param is so that position of text strings (for axis
c labels etc) can be defined when they are, at the first call,
c calculated internally.
	itlen=200	!tick length
	if(quarter) itlen=100
	xcalib=rx(2)-rx(1)
	ycalib=ry(4)-ry(3)
	call LAXES2(xmin,xmax,ymin,ymax,xtic,ytic,xcross,ycross,
     & ntx,nty,itx,ity,itlen,titlex,titley,xtitle,ytitle,ilabel,idev,
     & csfac(7),csfac(8),csfac(9),csize,ifnt(7),ifnt(8),ifnt(9),
     & thick,lth,inumx,inumy,0.0,ilog,doframe,mono,icol,xcalib,ycalib,
     & rx,ry,iangle,ixjus,iyjus,idraw,rxbox,rybox,
     & numbx,numby,cnumx,cnumy,cexpx,cexpy)
	if(.not.mono) call COLTYP1(15)		!bright white
c
136	continue
c	if(debug()) pause '11'
c
c Do title if any
	if(itit.ne.1) goto 1361
c	if(idraw(10).eq.0) goto 1361
	nl=NBLANK1(title1)	!terminate with ASCII 0
	call SETFONT1(ifnt(10))
	call SETCSIZE(csfac(10)*csize,isize)    !sets isize too
	ymin2=ymin
	ymax2=ymax
	if(sqrty) ymin2=sqrt(ymin)
	if(sqrty) ymax2=sqrt(ymax)
c position for title:
	if(idraw(10).eq.-2) then
	  ix=iscrx(0.5*(xmin+xmax))
	  iy=iscry(ymax2+0.025*(ymax2-ymin2))
	  rx(10)=xworld(ix)
	  ry(10)=yworld(iy)
	  iangle(10)=0
	  ixjus(10)=center
	  iyjus(10)=below
	else
	  ix=iscrx(rx(10))
	  iy=iscry(ry(10))
	endif
	if(idev.ge.5) then
	   call LOADVTB('2'//'W'//char(lth(25)))
	else
	   call LINWID(int4(lth(25)))
	endif
	if(.not.mono) call COLTYP1(icol(25))		!colour for title
	call JUSTIFYSTRING(ix,iy,title1(1:nl),iangle(10),isize,
     & ixjus(10),iyjus(10))
c draw the box round title if necessary- could call TEXTBOX again, but
c position already defined so quicker to call IFRAMER
c	if(idraw(10).eq.-1.and.(.not.quarter)) call
c	if(idraw(10).eq.-1) call
c     &  IFRAMER(ixbox(1,10),iybox(1,10))
	if(idraw(10).eq.-1) call TEXTBOX(ix,iy,title1(1:nl),iangle(10),
     & isize,ixjus(10),iyjus(10),rxbox(1,10),rybox(1,10),1)
	call SETFONT1(ifont)		!reset
	call SETCSIZE(csize,isize)    !reset
1361	continue
c
c
c Draw param values, if req. NB need to set box position etc only the first
c time here (when ixt()=-1 still). If PARAM values are altered (in position,
c size etc) the relevant parameters will be adjusted at time of alteration.
	if(idraw(6).ne.0.and.ifitype.ne.0) then
	  call SETFONT1(ifnt(6))
	  call SETCSIZE(csfac(6)*csize,isize)	!this defines ISIZE
	  nl=NBLANK1(parval)	!terminate with ASCII 0
	  ix=iscrx(rx(6))	!posn for param val
	  iy=iscry(ry(6))
	  if(idev.ge.5) then
	    call LOADVTB('2'//'W'//char(lth(26)))
	  else
	    call LINWID(int4(lth(26)))
	  endif
	  if(.not.mono) call COLTYP1(icol(26))		!colour for params
	  call JUSTIFYSTRING(ix,iy,parval(1:nl),iangle(6),isize,
     &	ixjus(6),iyjus(6))
c draw the box round PARVAL if necessary- could call TEXTBOX again, but
c position already defined so quicker to call IFRAMER
c	if(idraw(6).eq.-1.and.(.not.quarter)) call
c	if(idraw(6).eq.-1) call
c     &  IFRAMER(ixbox(1,6),iybox(1,6))
	  if(idraw(6).eq.-1.and.(.not.quarter)) then
	   call TEXTBOX(ix,iy,parval(1:nl),
     &   iangle(6),isize,ixjus(6),iyjus(6),rxbox(1,6),rybox(1,6),1)
	  endif
	  call SETFONT1(ifont)		!reset font
	  call SETCSIZE(csize,isize)	!reset csize,isize
	endif
c
c Draw extra text if any
	if(ntext.gt.0) then
	 do 223 i=1,ntext
	   j=i+10
	   if1=ifnt(j)
	   cs1=csfac(j)*csize
	   ix=iscrx(rx(j))
	   iy=iscry(ry(j))
	 call SETFONT1(if1)
	 call setcsize(cs1,isize)    !sets isize too
c 	if(debug()) print 85,(newtext(i)(j:j),j=1,30)
c85	format(/,' TEXT= ',/,5X,30a1)
	 if(idev.ge.5) then
	    call LOADVTB('2'//'W'//char(lth(i+30)))
	 else
	    call LINWID(int4(lth(i+30)))
	 endif
	 if(.not.mono) call COLTYP1(icol(i+30))		!colour for text(i)
	 call JUSTIFYSTRING(ix,iy,newtext(i),iangle(i),
     & isize,base,base)
c draw the box round NEWTEXT if necessary- could call TEXTBOX again, but
c position already defined so quicker to call IFRAMER
c	if(idraw(i).eq.-1.and.(.not.quarter)) call
c	if(idraw(i).eq.-1) call
c     &  IFRAMER(ixbox(1,i),iybox(1,i))
	   if(idraw(j).eq.-1) call TEXTBOX(ix,iy,newtext(i),iangle(j),
     &   isize,ixjus(j),iyjus(j),rxbox(1,j),rybox(1,j),1)
223	 continue
	 call SETFONT1(ifont)	!reset current values
	 call setcsize(csize,isize)    !sets isize too
	endif
c
c Draw arrows if any
	if(narrow.gt.0) then
	 do 222 i=1,narrow
	 if(idev.ge.5) then
	    call LOADVTB('2'//'W'//char(lth(i+50)))
	 else
	    call LINWID(int4(lth(i+50)))
	 endif
c Call to ARROW includes colour (with Hgraph numbering)
	   if(mono) then
		ic=0
	   else
		ic=15-icol(i+50)		!convert to Hgraph numbering
		if(ic.eq.15) ic=-1	!black (for icol=0)
	   endif
         call ARROW(xb(i),yb(i),xe(i),ye(i),ic,2,.true.)
222	 continue
	endif
c
c DRAW LINES IF ANY
	if(nline.gt.0) then
	   do 224 i=1,nline
	   if(idev.ge.5) then
	     call LOADVTB('2'//'W'//char(lth(i+60)))
	   else
	     call LINWID(int4(lth(i+60)))
	   endif
	   if(.not.mono) call COLTYP1(icol(i+60))		!colour for line(i)
	   call MOVETO(xlb(i),ylb(i),0,iltype(i))
	   call MOVETO(xle(i),yle(i),1,iltype(i))
224	   continue
	endif
	if(nhline.gt.0) then
	   do i=1,nhline
	     if(idev.ge.5) then
	      call LOADVTB('2'//'W'//char(lth(i+71)))
	     else
	      call LINWID(int4(lth(i+70)))
	     endif
	     if(.not.mono) call COLTYP1(icol(i+71))		!colour for line(i)
	     call MOVETO(xhlb(i),yhline(i),0,ilhtype(i))
	     call MOVETO(xhle(i),yhline(i),1,ilhtype(i))
	   enddo
	endif
	if(nvline.gt.0) then
	   do i=1,nvline
	     if(idev.ge.5) then
	      call LOADVTB('2'//'W'//char(lth(i+81)))
	     else
	      call LINWID(int4(lth(i+81)))
	     endif
	     if(.not.mono) call COLTYP1(icol(i+81))		!colour for line(i)
	     call MOVETO(xvline(i),yvlb(i),0,ilvtype(i))
	     call MOVETO(xvline(i),yvle(i),1,ilvtype(i))
	   enddo
	endif
c
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
	ay1c=ymax-0.03*(ymax-ymin)
	ay2c=ymax
	ay1v=ymax-0.04*(ymax-ymin)
	ay2v=ymax-0.07*(ymax-ymin)
	if(.not.IVplot) then
c NB idraw(28),(29) not queued -iy1=-1 if logo to be omitted (see VPLQR1)
	   if(ncjump.gt.0.and.iy1c.ge.0) then
	      if(.not.mono) call COLTYP1(icol(28))		!colour for c-logo
		call JLOGO1(xmin,xmax,ncjump,t1c,t2c,
     &         xoff1,idev,LTH(28),1,ay1c,ay2c)
	   endif
	   if(nvjump.gt.0.and.iy1v.ge.0) then
	      if(.not.mono) call COLTYP1(icol(29))		!colour for v-logo
		call JLOGO1(xmin,xmax,nvjump,t1v,t2v,
     &         xoff1,idev,LTH(29),1,ay1v,ay2v)
	   endif
	endif
c
c
c PLOT THE GRAPH
c
	barleng=0.01*(xmax-xmin)	!bar on top/bottom of error bars
c Control which curves are plotted via Icurvd() now
	if(ncurvd.le.0) goto 303	!calc curve only
	do 50 j1=1,ncurvd		!plot ncurvd data sets
	j=ifstcol(j1)		!=j1 unless there are plots with >2048 points
	if(.not.mono) call COLTYP1(icol(j))		!colour for jth data curve
	jflag=0
	linetype=ijoin(j)		!line to join data points
	iud=1				!draw line between points
	if(linetype.eq.-1) iud=0	!don't
	do 215 i=1,ndat(j),idelt
	if(isdev.ge.0.and.weight(i,j).eq.0.) goto 215	!skip points with w=0
	xv=xval(i,j)
	yv=yval(i,j)
	if(ilog.eq.4) yv=(yv-y0)/(yinf-yv)	! Hill scale
	if(logx.and.xv.gt.1.e-37) xv=alog10(xv)
	if(logy.and.yv.gt.1.e-37) yv=alog10(yv)
	if(xv.lt.xmin.or.xv.gt.xmax) goto 215
	if(yv.lt.ymin.or.yv.gt.ymax) goto 215
	if(jflag.eq.0) call MOVETO(xv,yv,0,0)   !move to 1st point in range
	if(iud.eq.1) then
	   if(idev.ge.5) then
	     call LOADVTB('2'//'W'//char(lth(j)))
	   else
	     call LINWID(int4(lth(j)))
	   endif
	endif
	if(jflag.gt.0) call MOVETO(xv,yv,iud,linetype)   !join with line from last point
c need thinner lines to draw symbols? and for error bars?
	if(idev.ge.5) then
	  call LOADVTB('2'//'W'//char(lth(27)))
	else
	  call LINWID(int4(lth(27)))
	endif
	call SYMBOL(xv,yv,isym(j),syms(j)*symfac,icol(j))     !symbol if isym>0
	if(.not.mono) call COLTYP1(icol(j))	!restore colour after SYMBOL
c plot error bars
	if(isdev.eq.1) then
	   if(idev.ge.5) then
	     call LOADVTB('2'//'W'//char(lth(30)))
	   else
	     call LINWID(int4(lth(30)))
	   endif
	   sdmax=0.5*abs(ymax-ymin)	!largest sd to be plotted
	   if(weight(i,j).gt.(1.0/sdmax**2)) then
	      sdev=sqrt(1.0/weight(i,j))
		yv1=yv-sdev
		if(logy) yv1=10.**yv-sdev
		if(logy.and.yv1.lt.1.e-37)  goto 25
		if(logy) yv1=alog10(yv1)	!yv already logged
		call MOVETO(xv-barleng,yv1,0,0)
		call MOVETO(xv+barleng,yv1,1,0)
		call MOVETO(xv,yv1,0,0)
25		yv1=yv+sdev
		if(logy) yv1=alog10(10.**yv+sdev)
		call MOVETO(xv,yv1,1,0)
		call MOVETO(xv-barleng,yv1,0,0)
		call MOVETO(xv+barleng,yv1,1,0)
		call MOVETO(xv,yv,0,0)
	   endif
	endif
	jflag=1
215	continue
50	continue
c
303	continue
	if(ncurvc.le.0) goto 999
	linetype=0		!always cont line- dashes synthesized via zseq
c
c	do 54 j1=1,ncurvc		!plot ncurvc curves
c	j=icurvc(j1)			!curve # to be plotted
	do 54 j=1,ncurvc		!plot ncurvc curves
	if(iline(j).lt.0) goto 54
	ij=iabs(iline(j))
	if(idev.ge.5) then
	     call LOADVTB('2'//'W'//char(lth(j+10)))
	else
	     call LINWID(int4(lth(j+10)))
	endif
	if(.not.mono) call COLTYP1(icol(j+10))		!colour for jth calc curve
	if(ij.gt.0) goto 310		!dashed calc curve
c
	Jflag=0
	do 220 k=1,ncal(j)
	xv=xcal(k,j)
	yv=ycal(k,j)
	if(logx.and.xv.ge.1.e-37) xv=alog10(xv)
	if(logy.and.yv.ge.1.e-37) yv=alog10(yv)
	if(sqrty.and.yv.ge.0.) yv=sqrt(yv)
	if(xv.lt.xmin.or.xv.gt.xmax) goto 220
	if(yv.lt.ymin.or.yv.gt.ymax) goto 220
	if(jflag.eq.0) call MOVETO(xv,yv,0,0)	!1st point in range
	if(jflag.gt.0) call MOVETO(xv,yv,1,linetype)	!rest of points
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
312	sfac=ifixr(float(iyhi-iylo)/float(ixhi-ixlo))		!O.K.? ###
	xr=0.01*(xmax-xmin)
	yr=0.01*(ymax-ymin)
	if(sqrty) yr=0.01*(sqrt(ymax)-sqrt(ymin))
	k=1
 	zleft=zseq(k)
	down=.true.
c
	x0=1.e-37		!smallest value for logs
	Jflag=0
	do 313 i=1,ncal(j)
	xv=xcal(i,j)
	yv=ycal(i,j)
	if(ilog.eq.4) then
	   yv=(yv-y0)/(yinf-yv)	! Hill scale
	   if(yv.gt.x0) then
		yv=alog10(yv)
c	   else
c	      nbadc=nbadc+1
c	      if(nbadc.le.25) xcbad(nbadc)=xv
c	      if(nbadc.le.25) ycbad(nbadc)=yv
	   endif
	endif
	if(logx) then
	   if(xv.gt.x0) then
		xv=alog10(xv)
c	   else
c	      nbadc=nbadc+1
c	      if(nbadc.le.25) xcbad(nbadc)=xv
c	      if(nbadc.le.25) ycbad(nbadc)=yv
	   endif
	endif
	if(logy) then
	   if(yv.gt.x0) then
		yv=alog10(yv)
c	   else
c	      nbadc=nbadc+1
c	      if(nbadc.le.25) xcbad(nbadc)=xv
c	      if(nbadc.le.25) ycbad(nbadc)=yv
	   endif
	endif
	if(sqrty.and.yv.ge.0.) yv=sqrt(yv)
	if(xv.lt.xmin.or.xv.gt.xmax) goto 313
	if(yv.lt.ymin.or.yv.gt.ymax) goto 313
	if(jflag.gt.0) goto 314
	call MOVETO(xv,yv,0,0)	!move to 1st point in range
	jflag=1
	goto 317
c     now part done for all points after 1st in range
314	dxn=(xv-xvlast)/xr
	dyn=sfac*(yv-yvlast)/yr
	if(abs(dxn).lt.1.e-5) goto 317
	b=dyn/dxn		!normalised slope
c     calc zn=dist from last point to current one in units= percent
c     of length of X axis
318	zn=sqrt(dxn*dxn + dyn*dyn)
	if(zleft.ge.zn) goto 315
	goto 316
c next bit when amount left to draw extends beyond (or exactly
c up to) the current point- go to this point
315	if(.not.down) call MOVETO(xv,yv,0,0)
	if(down) call MOVETO(xv,yv,1,linetype)
	zleft=zleft-zn	!amount of this segment still to be drawn
	if(zleft.gt.0.) goto 317
	down=.not.down	!zleft=0 i.e.segment reaches current point exactly
	k=k+1
	if(k.gt.kseq) k=1
	zleft=zseq(k)
	goto 317
c
316	continue
c  next bit done when amount of line remaining to be drawn does
c not extend as far as current point- keep drawing segments (with
c slope as between last point and current one) until it does.
c	b=dyn/dxn		!normalised slope
	xv1=xvlast + zleft*xr/sqrt(1.+b*b)
c	yv1=yvlast + b*(xv1-xvlast)*yr/xr
	yv1=yvlast + b*(xv1-xvlast)*yr/(sfac*xr)
	if(.not.down) call MOVETO(xv1,yv1,0,0)
	if(down) call MOVETO(xv1,yv1,1,linetype)
	dxn=(xv-xv1)/xr		!for dist from xv1,yv1 to current point
	dyn=sfac*(yv-yv1)/yr
	xvlast=xv1
	yvlast=yv1
	down=.not.down		!prepare for next segment
	k=k+1
	if(k.gt.kseq) k=1
	zleft=zseq(k)
	goto 318	!repeat until current point reached
c
317	xvlast=xv
	yvlast=yv
313	continue	!end of loop for points
311	continue
54	continue	!loop for each calc curve
c
999	continue
	RETURN
	END


