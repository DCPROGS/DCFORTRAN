	subroutine VHISTQ(XVAL,YVAL,NDAT,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,ncurvc,iline,ndimc,ILOG,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & xwbase,lt2,isval,sval,titlex,titley,ilabel,doframe,
     & draft,itit,title1,csize,ifont,theta,ifitype,ncomp,quarter,idev,
     & ndv1,ndc1,kmax,iver,iplot)
c     & iver,iplot)
c
c VERSION OF VHIST1 FOR AUTOMATIC PLOTTING FROM QUEUE (NO QUESTIONS ASKED)
c Modif 11/14/97 11:44am so ndv1,ndc1,kmax are parameters
c Modif 03/24/94 10:23am Xcal, Ycal increased to 2048 as VHIST3
c Modif 02/15/93 02:44pm for Lahey V5.x. NB must be called with mono=true
c if idev.ge.5 because colour plots not possible yet (icol, mono added
c to common/tpos/)
c Modif 03/10/92 10:31am for new definition of ILINE
c INIPLT called with appropriate IDEV in PLOTAUT before call to VPLOTQ
c Modif 01/21/91 03:08pm so that idev is a parameter (although INIPLT already
c called with appropriate idev in PLOTAUT, still need idev here to control
c whether line thickness commands inserted)
c
c 02/07/90 08:46am FIXED SO NO BOXES DRAWN WHEN QUARTER=TRUE (quarter page
c plots) because cannot be drawn in correct posn with IFRAME (device coord)
c when size reduced (prob could be drawn by call to TEXTBOX with imode=1,
c but this not yet fixed)
c
c
c	ICURVD,ICURVC not used since histo,curves are renumbered 1,2,..,n
c	when queued

c VPLOT1 is version of VPLOT0 that uses ONLY the FIXTEXT routine to
c alter text strings ('manual' move,delete etc now removed)
c Also uses LAXES1 that allows axis numbering to be adjusted like other
c text, and Xcross,Ycross added to parameters. 01/01/90 08:10pm
c Modif 01/15/90 12:48pm so that text, numbers and their positions are
c all taken from COMMON/TPOS/
c
c 12/05/89 03:06pm
c VPLOT0 is completely revamped Lahey version for use
c (1) As graph plotting program in CVFIT  etc
c (2) for doing posh plots in AUTPLT, for which have many extra options
c e.g. add/alter/move all text; add arrows or linetype legends;
c control line thickness (necessitates use of VTRANS at present).
c (Note that automatic plotting in AUTPLT is done by VPLOTQ, not
c by this routine, which is used only for posh plots.)
c When used in AUTPLT several parameters are specified on entry that
c are defined only internally when used in CVFIT  etc (e.g. ITIT,TITLE1,
c DOFRAME,...) so parameter AUTPLT added: when TRUE
c (1) input values of itit,title1,doframe,... (from queue) used
c (2) queueing of plot not allowed
c MODIFS:
c (1) Superimposition on existing plot not possible with raster printers
c	so must do via multiple data/curve inputs.
c	This also means that 'plot curve only' and 'plot no axes' are
c	no use now. ISUP and IPDIS removed. PLOT is now internal logical
c	variable only. MPOS,MLAST no longer args.
c (2) Doframe added as parameter (true=draws box round graph), also
c	itit,title1
c (3) Fonts specified on 0-11 scale so set by call setfnt(if) here.
c (4) Input logical FITTED used to control if calc curve is queued
c	so the 'example' exponential drawn on log(time) histo is
c	not queued for plotting in AUTPLT
c (5) NB Multiple data and fitted curves are now
c     only way to get superimposition now)
c	(1)Ndimd,ndimc =dimensions in calling prog ie Yval,Xval(1024,ndimd),
c		Ycal,Xcal(1024,ndimc)
c	ncurvd (=<ndimd)=number of data sets to be plotted
c	ncurvc (=<ndimc)=no of calc curves to be plotted
c	icurvd(1)...icurvd(ncurvd)=data set numbers to be
c		 plotted. Eg if ndimd=3, ncurvd=2
c		icurvd=(2,3) then Yval(i,2) and Yval(i,3)
c		plotted, but Yval(i,1) is omitted
c	icurvc(1)...icurvc(ncurvc)=calc curve numbers to be
c		 plotted
c	(2)SYMS() = symbol size (in ###) : added to args
c		and if SYMS(1)=<0 on entry then set internally.
c (6) Xcross,Ycross added to args so if altered new values are
c		used for next call (if ISCAL=1 then xc,yc set internally
c		to xmin,ymin)
c
c IFITYPE=0 for no display of param
c IFITYPE=1 for time histos (THIST)
c IFITYPE=2 for op/bst histos (OBHIST)
c IFITYPE=3 for amp  histos (AHIST)
c IFITYPE=31 for amp  histos (AHIST) if SD are proportional to mean
c IFITYPE=32 for amp  histos (AHIST) if SD are constrained to be equal
c   (older version used IFITYPE=-3 to indicate equal SD -this still recognized
c	in PARTEXT)
c IFITYPE=4 for exponentials fitted to relaxations (CJFIT,VPLOTR)
c IFITYPE=5 for Lorentzians fitted to spectra (PSFIT,VPLOTR)
c IFITYPE=-5 for Lorentzians + line fitted to spectra (PSFIT,VPLOTR)
c
c 05/28/89 11:02pm Lahey/Hgraph version of VHIST3
c (1) ixlo etc now integers
c (2) For Hgraph Xtic,Ytic are major, not minor ticks- fixed in FIXAX
c (3) NB call to INIPLT with device=0 or 1 will erase screen, but
c	not erased if idev>1. (Can get from alpha mode to graphics
c	without erasing screen by call to LOADCRTC() but must
c	reset BIOS=18 to BIOS=146 (=18+128) in SCREEN.CFG to do
c	this (will not prevent INIPLT(0 from erasing screen)
c (4) No need for CONFIRM of plot position (if all plotting done
c	from queue, no need to specify position at all- but may need
c	'plot now' for fancy superimposed plots, or for plot mixed
c	with text so leave in for now)
c
c (5) LAXES allows log scales to be labelled with non-log values in Fn.d format
c	rather than with exponent notation (10**n) ( controlled by new
c	parameters INUMX, INUMY)- note values diff from DEC version
c	INUMX=-1		X axis in fixed (Fn.d) format
c	INUMX=1		X axis in exponent format (if log)
c	INUMY=-1		Y axis in fixed (Fn.d) format
c	INUMY=1		Y axis in exponent format (if log)
c
c
c
c VHIST3 is version of VHIST2 that calls LAXES5 for nice log scales
c and for sqrt(y) scale (ilog=5,6); but takes more memory.
c
C General purpose display/plot subroutine for DATA that is in graph
c form.
c    LT2=straight line type for lo,hi bins (no LT1 now)
c    IJOIN(j) gives line type for joining data points: for Hgraph types 0-8
c
c	     (-1= do not join data points)
c		0=solid line
c		1=dotted line
c		2-6=dashed line with increasing dash length
c		7=dash-dot
c		8=dash-dot-dot
c
c	ILINE(j) defines line type for the jth calculated curve, via ZSEQ
c         (prev numbered 1-5, but now make 0-5 so 0=continuous as in Hgraph)
c		0=continuous line
c		1='dotted'
c		2=short dash
c		3=long dash
c		4=long,short dash
c		5=long,short,short dash
c(previously: 0=continuous line; 1=long dash; 2=short dash; 3=long,short dash;
c 4=long,short,short dash)
c
c
c  NDAT(j)=no of obs points. If ndat(j)=0 no display of data,only curve
c
c  NCAL(j)=no of points for calc curve. If NCAL=0 no display of calc curve.
c
c  XTIC,YTIC=minor tics (numbered at every 5 minor tics now-see LAXES)
c
C  ILOG  =0 for arithmetic plot,
C	 =1 for plot Y vs log(x)
C	 =2 for plot log(Y) vs x
C	 =3 for plot log(Y) vs log(x)
c	 =4 for Hill plot (not done here- only in VPLOT)
c	 =5 for sqrt(y) vs x
c	 =6 for sqrt(y) vs log(x)
c
c  ISCAL=0 if input values of xmin,...,ymax,xtic,ytic to be used.
c  ISCAL=1 if scaling to be done internally with FIXAX.
c	Set to 0 if NDAT=<0
c
c  iXLO etc=coordinates in device units(X=0-10000, Y=0-7000) for position
c   of display. If iXLO is negative others
c   are ignored and whole screen display used. Plots will be as
c   on screen if iXLO etc specified.
c
c NOTES ON MANIPULATION OF TEXT STRINGS.
c Use array CSFAC(10) to hold char size (0.5-18.) for text, specified
c as multiple of default size for whole page plots, CSIZE (=cbig initially)
c (so all can be scaled if latter altered): csfac(i), i=1-5 for newtext;
c csfac(6) for param values;
c MODIF: 01/25/90 09:55am csfac(7)=axis numbers (same for both axes);
c csfac(8)=x axis label; csfac(9)=y axis label; csfac(10)=title
c and similarly fonts defined by ifnt(1) to ifnt(10)
c ###OLD:  [NB for size and font,8,9 =axis labels,axis numbers resp,]
c but for position(rx,itx,iangle,ixjus,iyjus,ixbox) and for idraw()
c use 8=x-axis label, 9=y-axis label.
c NOTE that values of IDRAW(i) that refer to things that are drawn in LAXES
c are defined rather differently from others (for i=8,9, and 11-50) thus:
c    IDRAW(8)=0 if x-axis label to be omitted; =1 if to be drawn separately
c by JUSTIFYSTRING starting at posn calc internally
c in LAXES, or =2 if posn defined by the coords in RX(8),RY(8) (eg as defined
c by cursors); =-1 axis label to be drawn by DRAWAXIS.
c IDRAW(9) same but for Y axis
c
c VPLOT1/LAXES1 version. Position arrays, IANGLE to IYBOX all increased
c to dimension of 50, where
c #11-30 defines position of numbers 1-20 on X axis
c #31-50 defines position of numbers 1-20 on Y axis
c
C CSFAC(10) defines character size, as just defined
c IFNT(10)   defines default font for text strings similarly
c IANGLE(10) defines default ANGLE for text strings similarly
c IXJUS(10),IYJUS(10) keeps justification params similarly
c IXT(10),IYT(10) keep position (ix,iy,in call JUSTIFYSTRING) for drawing each
c RX(10),RY(10) are same, but in world coord.
c IDRAW(10) =1 to draw  (without box); =0 to omit text; =-1 to draw with box
c IXBOX(4,10),IYBOX(4,10) keep the coords of the 4 corners of the
c rectangle that encloses each text string (1-10) as found
c (and drawn if req) by TEXTBOX.  This allows a particular text string
c to be identified by crosshairs (by call to LOCTEXT)
c NB can use IXBOX(1,j) in call to TEXTBOX etc, with dummy arg being
c IX(4) within the subroutine, to return the 4 coord for jth text string
c (would NOT work if declared as ixbox(10,4))
c	NOTE potential problem: need to keep positions for text etc as
c world coord rather than device coord if they are to come out in right
c place when graph shape changed (eg via FIX ON VDU option). However symbols
c etc, with position specified in world coord, will not plot outside
c the graphboundary so must be within axes!
c 	LAXES0 altered by inclusion as a parameter of array RX(8),RY(8)=world
c coord for position of x-label, and RX(9),RY(9) for y-label.
c
c	real XVAL(0:511,ndimd),YVAL(0:511,ndimd)
c	real XCAL(2048,ndimc),YCAL(2048,ndimc)
c	real theta(20)
	real XVAL(0:ndv1,ndimd),YVAL(0:ndv1,ndimd)
	real XCAL(ndc1,ndimc),YCAL(ndc1,ndimc)
	real theta(kmax)
c for histo data
	integer ndat(ndimd),ijoin(ndimd)
c for calc curves
	integer ncal(ndimc),iline(ndimc)
c
	real xvert(4),yvert(4)	!vertices for Hgraph FILL call
	dimension zseq(10)
c	character*1 ans,UC
	character*40 titlex,titley
	character*75 xtitle,ytitle	!output from LAXES
	character*64 TITLE1
	character parval*200		!to hold param values
	LOGICAL logx,logy,down,pon,zoomed,vert
	logical sqrty,slock,off,bad
	logical doframe,draft,quarter,equal,mono
	logical caplock,debug
	real  RLTH(100),rlt		!for line thickness
c arrays for arrows, extra text etc
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
	integer IJUS(100)
      REAL ANGLE(100)
	integer idraw(100),icol(100)
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25)
c	integer ifnt(10)
c	real size(10)
c	real xb(5),yb(5),xe(5),ye(5)		!up to 5 arrows
c	character*80 newtext(5)		!extra text
	integer ifnt(30)
	real size(30),asize(30)
	real*4 xrp(4),yrp(4)		!symbol size
	real xb(10),yb(10),xe(10),ye(10)		!up to 10 arrows
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	integer iltype(10)				!line type for ditto
	character*80 newtext(20)		!extra text
	logical discprt
	character cans*30		!to hold text & result of WDIALOG
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
	common/dp/discprt
	COMMON/TPOS/rx,ry,rxbox,rybox,ijus,angle,idraw,
     & ifnt,size,Rlth,thbig,narrow,xb,yb,xe,ye,
     & nline,xlb,xle,ylb,yle,iltype,ntext,newtext,
     & cnumx,cnumy,cexpx,cexpy,numbx,numby,		!need also PARVAL?
     & nhline,yhline,xhlb,xhle,ilhtype,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel
	COMMON/cols/icol,mono
	common/logval/logx,logy,sqrty
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu
     & ,XLO,xhi,ylo,yhi
c
c        INCLUDE 'c:\hgraph\CONSTS.FOR'
c*      INCLUDE 'c:\hgraph\HGRGLB.DEC'
c*      INCLUDE 'c:\f77l3\SPINATT.HDR		!modif version of SPINATTR.HDR
c values below are from c:\hgraph\CONSTS.FOR
      INTEGER LEFT, CENTER, RIGHT

      DATA LEFT, CENTER, RIGHT/-1, 0, 1/     !new definition

	pon()=slock()
	debug()=caplock()
      EQUAL(x,y)=abs(x-y).lt.0.00001
c
c	if(debug()) pause '4'
3	format(i8)
4	format(g13.6)
c Initialisations:
c
	icmono=15 	!line colour for mono plots (laserjet etc)
	zoomed=.false.
	 just=0
	 ihead=2
	if(idraw(6).ne.0.and.ifitype.ne.0) call PARTEXT(parval,ifitype,
     & theta,ncomp,ifnt(6),size(6),20)  !define text string for param values
c
c Set default line thickness
	thick=thbig 	!value (called thbig in common) set in plotaut
	rlt=0.20		!default thickness, unless reset
	if(draft) rlt=0.
	do 6 i=1,100
6	RLTH(i)=rlt
	if(.not.draft) then
 	   RLTH(22)=0.25	!frame
	   RLTH(26)=0.10	!param value text
	   RLTH(27)=0.15	!symbols
	   RLTH(28)=0.20	!C-jump logo
	   RLTH(29)=0.20	!V-jump logo
	endif
c
c Fill background colour
      call papenq(xp,yp,ipap)
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
	call WRITQNUM(iplot,x20-0.03*xp,y2-0.03*yp,ic,idev)	!write plot queue # on screen/plot
	do i=1,30
	   if(size(i).lt.2.) size(i)=3.
         asize(i)=size(i)
	   if(quarter) size(i)=size(i)*0.6
	enddo


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
183	logx=ilog.eq.1.or.ilog.eq.3.or.ilog.eq.6.or.ilog.eq.4
	logy=ilog.eq.2.or.ilog.eq.3
	sqrty=ilog.eq.5.or.ilog.eq.6
309	if(.not.sqrty) call gSCALE(xmin,xmax,ymin,ymax,is)
	if(sqrty) call gSCALE(xmin,xmax,sqrt(ymin),sqrt(ymax),is)
c NB FRAME call is now in LAXES
c Last line of param is so that position of text strings (for axis
c labels etc) can be defined when they are, at the first call,
c calculated internally.
	tlen=4.	!tic length
	if(quarter) tlen=2.
	xcalib=rx(2)-rx(1)
	ycalib=ry(4)-ry(3)
	inumx=-1		!prevent superfluous scale factors?
	inumy=-1
	if(iver.ne.1100.and.just.eq.0) then
c		do k=7,10
c		  idraw(k)=-2
c		enddo
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
     & size(7),size(8),size(9),ifnt(7),ifnt(8),ifnt(9),thick,
     & RLTH,inumx,inumy,xwbase,ilog,doframe,mono,icol,xcalib,ycalib,
     & rx,ry,angle,ijus,idraw,rxbox,rybox,
     & numbx,numby,cnumx,cnumy,cexpx,cexpy)
	if(.not.mono) call lincols(15,idev)		!bright white
	just=1
c
136	continue
 	xabs1=xmin
      yabs1=ymin
 	xabs2=xmax
      yabs2=ymax
c	if(debug()) pause '11'
c
c Do title if any
	call broken(0)
	if(idraw(10).eq.0.or.itit.eq.0) goto 1361		!no title
	nl=NBLANK1(title1)
	call setfnt(ifnt(10))
	call SETSIZE(size(10))    !sets isize too
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
	if(idev.ge.6) then
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
	   call setfnt(ifont)		!res

1361	continue
	call broken(0)
c
c Draw param values, if req. NB need to set box position etc only the first
c time here (when ixt()=-1 still). If PARAM values are altered (in position,
c size etc) the relevant parameters will be adjusted at time of alteration.
	if(idraw(6).ne.0.and.ifitype.ne.0) then
	  call setfnt(ifnt(6))
	  call SETSIZE(size(6))	!this defines ISIZE
	  nl=nblank1(parval)
        call graspa(rx(6),ry(6),xsc,ysc)
	  if(idev.gt.6) then
	    call linwid((rlth(26)))
	  else
	    call LINWID((RLTH(26)))
	  endif
         if(.not.mono) call lincols(icol(26),idev)		!colour for params
	  call JUSTIFYSTRING(xsc,ysc,parval(1:nl),ANGle(6),size(6),
     &	ijus(6))

c draw the box round PARVAL if necessary- could call TEXTBOX again, but
c position already defined so quicker to call IFRAME1
	  if(idraw(6).eq.-1.and.(.not.quarter)) then
   	     call TEXTBOX(xsc,ysc,parval(1:nl),
     &     ANGle(6),size(6),ijus(6),rxbox(1,6),rybox(1,6),1)
	  endif
	  call setfnt(ifont)		!reset font
	endif
c
c Draw extra text if any
	call broken(0)
	if(ntext.gt.0) then
	 do 223 i=1,ntext
	   j=i+10
	   if1=ifnt(j)

	 call setfnt(ifnt(j))

c	   call setsize(size(j))
	 if(idev.gt.6) then
	    call linwid((rlth(i+30)))
	 else
	    call LINWID((RLTH(i+30)))
	 endif
	 if(.not.mono) call lincols(icol(i+30),idev)		!colour for text(i)
          call graspa(rx(j),ry(j),xsc,ysc)
	   call JUSTIFYSTRING(xsc,ysc,newtext(i),ANGle(j),
     &    size(j),ijus(j))

c draw the box round NEWTEXT if necessary- could call TEXTBOX again, but
c position already defined so quicker to call IFRAME1
c	if(idraw(i).eq.-1.and.(.not.quarter)) call
c     &  IFRAME1(ixbox(1,i),iybox(1,i))
c	   if(idraw(j).eq.-1) call FRAMER(rxbox(1,j),rybox(1,j))
	   if(idraw(i).eq.-1) call TEXTBOX(xsc,ysc,newtext(i),angle(j),
     &   size(j),ijus(j),rxbox(1,j),rybox(1,j),1)
223	 continue
	 call setfnt(ifont)	!reset current values
c	 call setcsize(csize,isize)    !sets isize too
	endif
c Draw arrows if any
	if(mono) then
	   call lincol(icmono)		!colour for jth histo bars
	endif
	call broken(0)
      ikey=0
      call draw_arr(ikey,narrow,xe,xb,ye,yb,ixfix,iyfix,
     &xv,yv,csize,idev,xabs1,yabs1,xabs2,yabs2,RLTH,zoomed,iver,ihead)


c
c DRAW LINES IF ANY
	call broken(0)
      	ikey=0
	call draw_lin(ikey,nline,xlb,xle,ylb,yle,iltype,
     & nhline,yhline,xhlb,xhle,ilhtype,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel,
     & csize,idev,xabs1,yabs1,xabs2,yabs2,RLTH,
     & xmin,xmax,ymin,ymax,zoomed,iver)
	call broken(0)

c PLOT THE HISTOGRAM
c
	call broken(0)
	do 50 j=1,ncurvd		!plot ncurvd histograms
	if(idev.gt.6) then
	    call linwid((rlth(j)))
	else
	    call LINWID((RLTH(j)))
	endif
	if(.not.mono) then
	   call lincols(icol(j),idev)		!colour for jth histo bars
	else if(mono) then
	   call lincol(icmono)		!colour for jth histo bars
	endif
c
	if(ndat(j).le.0) goto 303	!calc curve only
c  first LO bin
	flo=Yval(0,j)
	ybase=ymin
	if(logy) ybase=10.**ymin		!need non-log value
	ybase=ybase+1.e-6+.00001*abs(ymin)	!so not out of range
c	write(7,29) ymin,ybase
c29	format(' ymin,ybase= ',2g13.6)
	if(flo.lt.0.0001) goto 20	!skip lo bin if not needed
	if(logx.or.logy.or.flo.le.0.) goto 20	!no LO bin
c	dxlo=xval(2)-xval(1)   !same as first bin unless goes below xmin
	dxlo=xval(1,j)-xval(0,j)   !define width in calling prog
	xlow=xval(1,j)-dxlo	!symbol XLO is used for locate!
	if(xlow+1.e-6.ge.xmin) goto 22
	xlow=xmin
	dxlo=xval(1,j)-xmin
22	continue
cc Express FLO as freq density if dxlo.ne.xwbase ?
c	if(abs(xwbase-dxlo).gt.1.e-4.and.dxlo.gt.0.) then
c	  flo=flo*xwbase/dxlo		!use FLO as number not density
c	  if(pon()) write(7,21) dxlo,flo
c	  if(discprt) write(8,21) dxlo,flo
c	  call LOCATE(0,0)		!row=0,col=0
c	  print 21,dxlo,flo
c21	  format(' Bin width for LO bin= ',g13.6,'. Freq density= ',g13.6)
c	endif
c Express FLO as freq density if dxlo.ne.xwbase ? Problem here, because
c may call with xwbase=0 to suppress 'per 10 ms' etc in LAXES2
	if((xwbase.gt.1.e-5).and.abs(xwbase-dxlo).gt.1.e-4) then
	  if(dxlo.ne.0.) flo=flo*xwbase/dxlo	!use FLO as number not density
	  if(pon()) write(7,21) dxlo,flo
	  if(discprt) write(8,21) dxlo,flo
c	  call LOCATE(0,0)		!row=0,col=0
c	  print 21,dxlo,flo
21	  format(' Bin width for LO bin= ',g13.6,'. Freq density= ',g13.6)
	  call DCFORMAT(dxlo,8,3,cans)
	  call WDIALOG(1,'Bin width for LO bin = '//CHARNB(cans),11)
	  call DCFORMAT(flo,8,3,cans)
	  call WDIALOG(1,'Frequency density = '//CHARNB(cans),11)
	endif
c
	call logvl(xlow,xv,xmin,xmax,logx,off,bad,.false.) !ixlow on scale?
	if(off.or.bad) goto 20		!no lo bin if not
 	call logvl(flo,yv,ymin,ymax,logy,off,bad,sqrty)
c if Y off scale it is plotted as Ymax (set in logvl)
c	if(off.or.bad) goto 20		!omit LO bin if Y off scale
	linetype=lt2			!line type for FLO bin
	if(.not.mono) call lincols(icol(10),idev)		!colour end bins
	if(xlow.eq.xmin) then		!omit 1st vert
	   call graMOV(xv,yv)
	else
	   call graMOV(xv,0.)
         call broken(linetype)
	   call gralin(xv,yv)
	endif
	xvert(1)=xv
	yvert(1)=0.
	xvert(2)=xv
	yvert(2)=yv
	call logvl(xval(1,j),xv,xmin,xmax,logx,off,bad,.false.)
       call broken(linetype)
	   call gralin(xv,yv)

	xvert(3)=xv
	yvert(3)=yv
c   go back to baseline
	call logvl(ybase,yv,ymin,ymax,logy,off,bad,sqrty)
c	if(yval(1,j).gt.flo) linetype=ijoin(j)	!contin line
      call broken(linetype)
	call gralin(xv,yv)		!draw vert
	xvert(4)=xv
	yvert(4)=yv
	do kap=1,4
	   call graspa(xvert(kap),yvert(kap),xrp(kap),yrp(kap))
	enddo
	    call gramov(xvert(1),yvert(1))
	    call POFTO2(0,0,0,xrp,yrp,4)
c	call FILL1(xvert,yvert,4,12,0,0,0)
c	goto 25
c  next regular histogram-
c  first do 1st vertical of regular histo
20	continue
c    start from baseline
	if(idev.gt.6) then
	    call linwid((rlth(j)))
	else
	    call LINWID((RLTH(j)))
	endif
	if(.not.mono) call lincols(icol(j),idev)		!colour for jth histo bars
	linetype=ijoin(j)
	k=0
27	k=k+1
	call logvl(xval(k,j),xv,xmin,xmax,logx,off,bad,.false.)
	if(off.or.bad) goto 27	!look for 1st good X if X off scale
	call logvl(ybase,yv,ymin,ymax,logy,off,bad,sqrty)
c if Y off scale it is plotted as Ymax (set in logvl)
	call graMOV(xv,yv)
	call logvl(yval(k,j),yv,ymin,ymax,logy,off,bad,sqrty)
c if Y off scale it is plotted as Ymax (set in logvl)
c	if(off.or.bad) goto 25
      call broken(linetype)
	call gralin(xv,yv)	!draw 1st vert
25	continue
c  Now loop to draw top and 2nd vert. Starts at point x1,y1
	do 215 i=k,ndat(j)		!ndat=nbin; x=x(1) to x(nbin+1)
	call logvl(xval(i+1,j),xv,xmin,xmax,logx,off,bad,.false.)
c already checked that x > xmin. Once X > Xmax can leave loop
c altogther as all subsequent X will be off scale too
c	if(off.or.bad) goto 215     !skip unless x is on scale
	if(xv.gt.xmax) goto 28
        call broken(linetype)
	call gralin(xv,yv)	!draw 1s
		!draw top
	ynext=ybase   !for last bin- xval(ndat+1) is defined but not yval
 	if(i.lt.ndat(j)) ynext=yval(i+1,j)
	call logvl(ynext,yv,ymin,ymax,logy,off,bad,sqrty)
c if Y off scale it is plotted as Ymin or Ymax (set in logvl)
c	if(off.or.bad) goto 215     !skip unless x is on scale
        call broken(linetype)
	call gralin(xv,yv)	!draw 1s

215	continue
c
c  lastly HI bin. Could be improved later to plot (with recalc FHI)
c even after rescaling to reduce Xmax (no data to recalc FHI if Xmax
c is increased by rescaling)
c If regular histo went above xmax, OFF will be true here
28	continue
c=	fhi=Yval(ndat(j)+2,j)
	fhi=Yval(ndat(j)+1,j)
c=	if(logx.or.logy.or.fhi.lt.0.) goto 26	!no hi bin
	if(fhi.lt.0.0001) goto 26	!no hi bin
c	dxhi=xval(ndat+2,j)-xval(ndat+1,j)	!define in calling prog
c	xhigh=xval(ndat+1)+dxhi
	xhigh=xval(ndat(j)+2,j)
	call logvl(xhigh,xv1,xmin,xmax,logx,off,bad,.false.)
c=	if(off.or.bad) goto 26		!no hi bin if Xhigh off scale
	call logvl(xval(ndat(j)+1,j),xv,xmin,xmax,logx,off,bad,.false.)
	call logvl(FHI,yv,ymin,ymax,logy,off,bad,sqrty)
c  plot as Ymax if FHI > Ymax
c	if(off.or.bad) goto 26
	if(.not.mono) call lincols(icol(10),idev)		!colour end bins
	linetype=lt2
        call broken(linetype)
	call gralin(xv,yv)	!draw 1s
      call gralin(xv1,yv)

	call logvl(ybase,yv2,ymin,ymax,logy,off,bad,sqrty)
	call gralin(xv1,yv2)
	xvert(1)=xv
	yvert(1)=0.
	xvert(2)=xv
	yvert(2)=yv
	xvert(3)=xv1
	yvert(3)=yv
	xvert(4)=xv1
	yvert(4)=yv2
	do kap=1,4
	   call graspa(xvert(kap),yvert(kap),xrp(kap),yrp(kap))
	enddo
	    call gramov(xvert(1),yvert(1))
	    call POFTO2(0,0,0,xrp,yrp,4)
c	call FILL1(xvert,yvert,4,12,0,0,0)
	linetype=0
	if(.not.mono) call lincols(icol(j),idev)		!colour for jth histo bars
26	continue
c
50	continue
c Histograms finished
303	continue
	if(ncurvc.le.0) goto 999
	linetype=0		!always cont line- dashes synthesized via zseq
c
	do 54 j=1,ncurvc		!plot ncurvc curves
	if(iline(j).lt.0) goto 54	!skip curve
	ij=iabs(iline(j))
	if(idev.gt.6) then
	    call linwid((rlth(j+10)))
	else
	    call LINWID((RLTH(j+10)))
	endif
	if(.not.mono) call lincols(icol(j+10),idev)		!colour for jth calc curve
	if(ij.gt.0) goto 310		!dashed calc curve
c
c	Jflag=0
	do 220 k=1,ncal(j)
	xv=xcal(k,j)
	yv=ycal(k,j)
	if(logx.and.xv.ge.1.e-32) xv=alog10(xv)
	if(logy.and.yv.ge.1.e-32) yv=alog10(yv)
	if(sqrty.and.yv.ge.0.) yv=sqrt(yv)
	if(k.eq.1) then
	   call graMOV(xv,yv)	!1st point in range
	else
        call broken(linetype)
	  call gralin(xv,yv)

	endif
c	if(xv.lt.xmin.or.xv.gt.xmax) goto 220
c	if(yv.lt.ymin.or.yv.gt.ymax) goto 220
c	if(jflag.eq.0) call MOVETO(xv,yv,0,0)	!1st point in range
c	if(jflag.gt.0) call MOVETO(xv,yv,1,linetype)	!rest of points
c	jflag=1
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
312	sfac=ifixr((yhi-ylo)/(xhi-xlo))		!O.K.? ###
	xr=0.01*(xmax-xmin)
	yr=0.01*(ymax-ymin)
	if(sqrty) yr=0.01*(sqrt(ymax)-sqrt(ymin))
	k=1
 	zleft=zseq(k)
	down=.true.
c
      x0=1.e-36               !smallest value for logs
	Jflag=0
	do 313 i=1,ncal(j)
	xv=xcal(i,j)
	yv=ycal(i,j)
	if(logx) then
	   if(xv.gt.x0) then
		xv=alog10(xv)
	   else
c	      nbadc=nbadc+1
c	      if(nbadc.le.25) xcbad(nbadc)=xv
c	      if(nbadc.le.25) ycbad(nbadc)=yv
		goto 313
	   endif
	endif
	if(logy) then
	   if(yv.gt.x0) then
		yv=alog10(yv)
	   else
c	      nbadc=nbadc+1
c	      if(nbadc.le.25) xcbad(nbadc)=xv
c	      if(nbadc.le.25) ycbad(nbadc)=yv
		goto 313
	   endif
	endif
	if(sqrty.and.yv.ge.0.) yv=sqrt(yv)
c
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
c    Next bit when amount left to draw extends beyond (or exactly
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
c  	   Next bit done when amount of line remaining to be drawn does
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


