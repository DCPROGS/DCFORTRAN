	subroutine VHIST2(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & iXLO1,iXHI1,iYLO1,iYHI1,xwbase,lt2,inumx,inumy,
     & titlex,titley,ilabel,doframe,idiskq,
     & autplt,draft,itit,title1,cbig,ifont,landscap,fitted,
     & theta,ifitype,ncomp,sval,isval,iask)
c
c
c Lahey V5.n version 02/10/93 09:19am
c Updated to use dialog boxes 06/29/93 03:38pm
C General purpose display/plot subroutine for DATA that is in histogram
c form.
c   (1) Uses DCMENU in place of DRAWBOX (see TDCBOX). Names of ixlo etc altered
c	to ixlo1 etc in call, so ixlo can be in common/hgv/.  This common can
c	be used for any subroutine that need to change viewport and/or
c	graphboundary, to allow restoration of current values before leaving.
c   (2) Colour added.  Note that COLTYP1 and FILL1 use the Lahey graphics
c	 colour numbering. MONO=true for monochrome display (except for DCMENU).
c	 ICOL,MONO added to common/tpos/ (so queued colours accessible in AUTPLOT)
c Lahey colours:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c
c Hgraph colours
c -1=black; 0=white; 1=yellow (bright); 2=purple (bright); 3=red (bright)
c 4= pale blue (bright); 5=green (bright); 6=dark blue (bright);
c 7=dark gray; 8=pale gray; 9=brown (bright); 10=purple; 11=brown;
c 12=pale blue; 13=green; 14=dark blue
c	   call FILL1(x,y,n,icol2,0,0,1)
c	   call COLTYP1(icol1)
c 07/19/90 10:50am MINMAX1 removed as sep subroutine to reduce memory
c needed in Multi-Edit
c VHIST1 made by modif of VPLOT1 to do histograms
c syms,isym isdev,weight not needed;
c   xwbase,lt2,sval(for randomisation tests),iask added, and ntx,itx for laxes2
c    NTX,NTY label every NTXth (major) tic
c    ITX,ITY=1 for normal tic orientation,=0 for centered,=-1 for opp of normal
c	(if ISCAL=1 default values set for ntx,nty)
c    IASK=1 or -1 to ask 'are you sure' before leaving VHIST; =2,-2 to not ask
c    IASK=pos to leave graph on screen (1 or 2); neg (-1,-2) to delete before
c		exit (orig version is iask=-2)
c    IASK=3 to draw graph and leave at once (no boxes; no chance to alter or
c		 plot or queue it)
c    LT2=straight line type for lo,hi bins (no LT1 now)
c    SVAL= value of X to put arrow at for randomisation tests (may be neg!)
c    ISVAL=0 if no arrow; =1 if arrow to be drawn
c    IJOIN(j) gives line type for main histo bars: for Hgraph types 0-8
c
c		0=solid line
c		1=dotted line
c		2-6=dashed line with increasing dash length
c		7=dash-dot
c		8=dash-dot-dot
c   Input logical FITTED used to control if calc curve is queued
c	so the 'example' exponential drawn on log(time) histo is
c	not queued for plotting in AUTPLT.
c   When AUTPLT=true, then FITTED is set true if ncurvc>1, ie if there
c	is a calc curve to be plotted (and stored in POSHPLOT.DAT).
c	For ops/bst etc the calc data is stored as data, not curve, so
c	if ifitype=2 then set fitted=true if ncurvd>1
c   Multiple histos and fitted curves now allowed as in VPLOT (as this
c	is only way to get superimposition now)
c	(a)Ndimd,ndimc =dimensions in calling prog of Yval,Xval(0:511,ndimd)
c		(histo data), and of Ycal,Xcal(512,ndimc) (for calc curves)
c BIN DEFINITION
c	ndat(j)=nbin.
c	LO bin is from Xval(0,j) to Xval(1,j); freq in Yval(0,j)
c	First bin is from Xval(1,j) to Xval(2,j); freq in Yval(1,j)
c	ith bin is from Xval(i,j) to Xval(i+1,j); freq in Yval(i,j)
c	last bin is from Xval(nbin,j) to Xval(nbin+1,j); freq in Yval(nbin,j)
c	HI bin is from Xval(nbin+1,j) to Xval(nbin+2,j); freq in Yval(nbin+1,j)
c
c	 Note that FLO,FHI not used as param to define lo and hi bins
c		but Xval,Yval numbered from 0 rather than 1, and FLO in Xval(0),
c		and FHI kept in Yval(ndat+2)
c
c Version that uses ONLY the FIXTEXT routine to
c alter text strings ('manual' move,delete etc now removed)
c Also uses LAXES1 that allows axis numbering to be adjusted like other
c text, and Xcross,Ycross added to parameters. 01/01/90 08:10pm
c Modif 01/15/90 12:48pm so that text, numbers and their positions are
c all taken from COMMON/TPOS/ if AUTPLT=true (for use e.g.in AUTPLT.FOR
c in which their values are all specified in queue)
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
c (3) Fonts specified on 0-11 scale so set by call SETFONT1(if) here.
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
c	(2)SYMS() = symbol size (in ### units) : added to args
c		and if SYMS(1)=<0 on entry then set internally.
c (6) Xcross,Ycross added to args so if altered new values are
c		used for next call (if ISCAL=1 then xc,yc set internally
c		to xmin,ymin)
c
c IFITYPE=0 for no display of param
c IFITYPE=1 for time histos (THIST)
c IFITYPE=2 for op/bst histos (OBHIST)
c IFITYPE=3 for amp  histos (AHIST)
c IFITYPE=-3 for amp  histos (AHIST) if SD are constrained to be equal
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
c	parameters INUMX, INUMY)- note values diff from PDP version
c Before 06/18/93 09:01am inumx, inumy were used ONLY for log scales,
c but now use also for non-log scales to control whether numbers are
c (1) inum=-1 SCALED automatically, as in orig version, by multiplying numbers
c by 10**n and adding 'x10**n' to the axis label (still shown in fixed
c format though)
c (2) inum=-1 -forces no such scaling
c (this creates possible problem with queued plots in which the axis
c numbers that are displayed are already fixed in CNUMx/y -if they have
c already been scaled up/down before queuing then numbers will not
c be scaled according to value of SX calculated here, so in AUTPLOT check
c that inumx,inumy=1 always; then can reset to non-scaled values in
c VPLOT/VHIST via NEW AXES option)
c
c If log scale then:
c	INUMX=-1		X axis numbers shown in fixed (Fn.d) format
c	INUMX=1		X axis numbers shown in exponent format (if log)
c	INUMY=-1		Y axis shown in fixed (Fn.d) format
c	INUMY=1		Y axis shown in exponent format (if log)
c If NOT log scale then numbers are displayed in fixed format, but:
c	INUMX/Y=-1		No scaling
c	INUMX/Y=1		Allows automatic scaling (as in original)
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
c Modif 11/01/90 10:20am to include neg values for ISCAL
c NB always uses input values of itx,ity (tic orientation) if valid value
c itx (-1,0,+1) given, otherwise both set to 1.
c  ISCAL=-1 if input value of xcross,ycross to be used, but xmin,..,ymax,
c		and xtic,ytic found internally
c  ISCAL=0 if input values of xmin,...,ymax,xtic,ytic to be used.
c  ISCAL=1 if scaling to be done internally with FIXAX.
c	Set to 0 if NDAT=<0
c
c  iXLO etc=coordinates in device units(X=0-10000, Y=0-7000) for position
c   of display. If iXLO is negative others
c   are ignored and whole screen display used. Plots will be as
c   on screen if iXLO etc specified.
c
c   LANDSCAP If true get normal shape plot, if false get portrait shape
c	plot (as long as IXLO negative on input, otherwise input IXLO etc
c	are used). Shape can be altered again with GRAPH SHAPE option.
c	Note that however LANDSCAP is set graph will be plotted in
c	landscape orientation on plotter, unless portrait plot option
c	chosen (LANDPLOT=false)
c
c NOTES ON MANIPULATION OF TEXT STRINGS.
c Use array CSFAC() to hold char size (0.5-18.) for text, specified
c as multiple of default size for whole page plots, CSIZE (=cbig initially)
c (so all can be scaled if latter altered): csfac(1)-(5) spare[old-for newtext];
c csfac(6) for param values; csfac(7) spare; csfac(8)=axis labels;
c csfac(9)=axis numbers; csfac(10)=title; csfac(11)-(30) for newtext
c and same numbering for IFNT(i)
c MODIF: 01/25/90 09:55am csfac(7)=axis numbers (same for both axes);
c csfac(8)=x axis label; csfac(9)=y axis label;
c and similarly fonts defined by ifnt(1) to ifnt(10)
c
c ARRAY INDEX FOR POSITIONS (IDRAW,IANGLE,RX,RY,RXBOX,RYBOX) all now
c dimension=100:  1-5=spare [were newtext]; 6=param values; 7=spare;
c 8=axis labels; 9=axis numbers; 10=title; 11-30=newtext; 31-55=numbers
c (#1-25) on Xaxis; 56-80=numbers (#1-25) on Y axis; 81-100 spare for future
c
c Line thickness for whole page plots in lth(i) as follows ( actual
c thickness =THICK*lth(i) were THICK=THBIG=1.0 for whole page)
c Colours in icol() are numbered the same way.
c 1-10= data sets (#1-10) (histo bars); Use icol(10) for end (dashed) bins
c 11-20=calc curves (#1-10);
c 21=axes; 22=frame; 23=axis labels (same for X,Y)
c 24=axis numbers (same for X,Y); 25=plot title; 26=param value text;
c 27=symbols; 28=SD bars; 29-30=spare;
c 31-50=extra text (#1-20); 51-60=arrows (#1-10); 61-70=lines (#1-10)
c Use lth(51), icol(51) for Sval arrow, if any (always arrow #1)
c icol(71)=background colour for graph
c 72-100 =spare for future
C [OLD- 31-35=extra text (#1-5); 36-40=arrows (#1-5)]
c
C CSFAC() defines character size, as just defined
c IFNT()   defines default font for text strings similarly
c IANGLE() defines default ANGLE for text strings similarly
c IXJUS(),IYJUS() keeps justification params similarly
c RX(i),RY(i) keep position world coord) for drawing each bit of text
c RXBOX(4,j),RYBOX(4,j) keep the coords of the 4 corners of the
c rectangle that encloses each text string (1-10) as found
c (and drawn if req) by TEXTBOX.  This allows a particular text string
c to be identified by crosshairs (by call to LOCTEXT)
c NB can use rxbox(1,j) in call to TEXTBOX etc, with dummy arg being
c RXB(4) within the subroutine, to return the 4 coord for jth text string
c (would NOT work if declared as rxbox(j,4))
c	NOTE potential problem: need to keep positions for text etc as
c world coord rather than device coord if they are to come out in right
c place when graph shape changed (eg via FIX ON VDU option). However symbols
c etc, with position specified in world coord, will not plot outside
c the graphboundary so must be within axes!
c
c IDRAW(i) =1 to draw (without box) at the position that has been already
c			defined in rx,ry,rxbox,rybox (no need to define box again)
c		=0 to omit text
c		=-1 to draw with box, as for idraw=1
c		=-2 when text position not yet defined; there are two sorts
c		      of text for which idraw=-2 has somwhat different effects,viz
c		 (a) those for which a default position is calc internally ie
c			axis labels, all axis numbers (both done in LAXES),
c			title and parval (and Sval arrow in VHIST). For these
c			when idraw=-2, the text is drawn at the default position
c			(kept in rx,ry), and the boxes that enclose it defined
c			by call to TEXTBOX and kept in rxbox,rybox; then set idraw=1
c			(or -1 for parval which has box by default)
c		 (b) those for which there is no default position (NEWTEXT, and
c			extra arrows/lines though lattter are not text and have no
c			boxes defined for them). In this case nothing done if idraw=-2
c			(idraw gets set to 1 or -1 when newtext defined via 'add new
c			text'
c Special problems for text location
c   (1) After rescale
c After rescale that affects numbering on axes (xmin,xmax,xtic,xcross) must
c redraw all numbers on the affected axis at their default positions (idraw=-2)
c####also do axis labels at default posn??
c   (2) After change of log/sqrt axes
c AFTER CHANGE OF LOG AXES NEED TO REDRAW EVERYTHING AT DEFAULT POSN
c ####When, as for NEWTEXT and ARROWS then could take log/sqrt/antilog etc
c of rx,ry but this involves checking how axis CHANGES eg might change
c from logy/logx to logy/x so Y axis is log, but not changed so no
c need to alter RY(). This not yet fixed, so log axes should be changed
c before any newtext/arrows added.
c#######
c   (3) After change of graph shape (by call to graphboundary)
c After change of shape by call to graphboundary must redraw ALL text
c at the same world coordinates in rx,ry. ie do not want any of the idraw=-2
c (all should be -1,0,1). But boxes may be in wrong positions
c or distorted,so all boxes redefined now, from the current rx,ry.
c
	real XVAL(0:511,ndimd),YVAL(0:511,ndimd)
	real XCAL(512,ndimc),YCAL(512,ndimc)
c for histo data
	integer ndat(ndimd),icurvd(ndimd),ijoin(ndimd)
c for calc curves
	integer ncal(ndimc),icurvc(ndimc),iline(ndimc)
c
	integer ndat1(10)
	real theta(20)
	dimension zseq(10)
	character*1 ans,UC
	character*40 titlex,titley
	character*75 xtitle,ytitle	!output from LAXES
	character*44 TITLE1
	character parval*150		!to hold param values
	character text1*150		!to hold any string (for fixtext call)
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	LOGICAL PLOT,logx,logy,logity,down,pon,fitted,croset,equal
	logical sqrty,slock,bad,off,vert
	logical doframe,autplt,draft,landplot,landscap,zoomed,blank
	logical caplock,debug,mono,monsav,colplotter
	integer*2 lrow,int2,videotyp
	integer*2 ilt,lth(100),lth 		!for line thickness
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
	integer IXJUS(100),IYJUS(100)
	integer iangle(100),idraw(100),icol(100),icolsav(100)
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25)
c	integer ifnt(10)
c	real csfac(10)
c	real xb(5),yb(5),xe(5),ye(5)		!up to 5 arrows
c	character*80 newtext(5)		!extra text
	integer ifnt(30)
	real csfac(30)
	real xb(10),yb(10),xe(10),ye(10)		!up to 10 arrows
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	integer iltype(10)				!line type for ditto
	character*80 newtext(20)		!extra text
c      character*1 ch, fndcur   !fndcur is now integer
      character ch*1,cnum1*11,cnum2*11	!must have *11 for INTCONV
      integer fndcur
	integer Lb(18)
	character*78 text(18)
	logical discprt
	common/dp/discprt
	common/DMENU/csizb,ifonb,csize,ifont2,nboxlast,nblast
	common/hgv/ivxlo,ivxhi,ivylo,ivyhi,ixlo,ixhi,iylo,iyhi
	COMMON/TPOS/rx,ry,rxbox,rybox,ixjus,iyjus,iangle,idraw,
     & ifnt,csfac,lth,thbig,narrow,xb,yb,xe,ye,
     & nline,xlb,xle,ylb,yle,iltype,ntext,newtext,
     & cnumx,cnumy,cexpx,cexpy,numbx,numby,icol,mono	!need also PARVAL?
c
c        INCLUDE 'c:\hgraph\CONSTS.FOR'
c*      INCLUDE 'c:\hgraph\HGRGLB.DEC'
c*      INCLUDE 'c:\f77l3\SPINATT.HDR		!modif version of SPINATTR.HDR
c values below are from c:\hgraph\CONSTS.FOR
c values below are from c:\hgraph\CONSTS.FOR
      INTEGER LEFT, CENTER, RIGHT, BASE
      INTEGER BELOW, BOTTOM, ABOVE, TOP
      DATA LEFT, CENTER, RIGHT, BASE /0, 2, 1, 3/     !new definition
      DATA BELOW, BOTTOM, ABOVE, TOP /0, 0, 1, 1/     !new definition
c
	pon()=slock()
	debug()=caplock()
	lth(i)=ifixr(thick*float(lth(i)))	!sets line thickness
	EQUAL(x,y)=abs(x-y).lt.0.00001
c
3	format(i8)
4	format(g13.6)
c Initialisations:
c (1) Initialisation whether AUTPLT or not
c	if(VIDEOTYP().ne.18) call INIPLT(idev,.false.,1.0)
	scalfac=1.0
	if(VIDEOTYP().ne.18) call INIPLT(0,.false.,scalfac)
c
	plot=.false.
	croset=.false.		!cross position has not been reset
	iret=0		!for return after help screen
	landplot=.true.
	draft=.false.
	zoomed=.false.
	cfacsml=0.6		!character size factor for small plots (0.6*2.5=1.5)
	thfacsml=0.6	!line thickness factor for small plots
	ifsav=ifont		!save input value (if there is one)
	itlen=200		!tic length
	ixlo=ixlo1		!named ixlo1 in call so ixlo can be in common/hgv/
	ixhi=ixhi1		!ditto
	iylo=iylo1		!named iylo1 in call so iylo can be in common/hgv/
	iyhi=iyhi1		!ditto
	ifont2=ifont	!copy for common/dmenu/
	nboxlast=10		!in case attempt to delete boxes before any drawn
	nblast=4
	icol1=14	!yellow text/border for boxes in DRAWBOX
	icol2=8	!grey background for boxes
	icol0=7	!white text/border for altered boxes
	icb=0	!background colour for dialog box 1
	icb2=0	!background colour for dialog box 1,2
	icf=7		!frame colour for dialog box 1
	icf2=12	!frame colour for dialog box 2
	ict=11	!text colour for dialog box 1
	ict2=12	!text colour for dialog box 2
c	subroutine DEFDIALOG(ibox,irhi,iclo,nrow,ncol,icb)
	call DEFDIALOG(1,1,2,4,60,icb2)	!define dialog box #1
	call DEFDIALOG(2,1,65,4,14,icb2)	!define dialog box #2
	if(.not.autplt) then	!for autplot, mono set on entry
c	   mono=.true.		!monochrome display (except DCMENU)
	   mono=.false.		!colour display with default colours
	endif
	if(inumx.eq.0) inumx=-1		!but should be defined in call
	if(inumy.eq.0) inumy=-1		!but should be defined in call
c Default colours set if mono=false on entry and not AUTPLOT (for autplot
c use the queued colours if mono=false)
	call SETCOLS(mono,icol,autplt,plot)
	ifonb=0     !font for boxes
	csizb=1.7   !character size for boxes
	i=NBLANK1(titlex)               !ensure it ends with char(0)
	j=NBLANK1(titley)               !ensure it ends with char(0)
c
c (2) Initialisation ONLY when AUTPLT=true
	if(autplt) then
	   csize=cbig
	   thick=thbig
	   if(ifitype.ne.2) then
	      fitted=ncurvc.gt.0
	   else if(ifitype.eq.2) then
		fitted=ncurvd.gt.1
	   endif
	endif
c
c (3) Initialisation only when AUTPLT=false- none defined on entry
	if(AUTPLT) goto 89
c	mono=.true.			!monochrome display (except DCMENU)
	mono=.false.		!colour display with default colours
	linetype=0		!continuous line
	narrow=0	!number of arrows added
	nline=0	!number of lines added
	ntext=0	!number of extra text strings
	if(ifont.lt.0) ifont=2		!simplex (on 0-11 scale)
	ifont2=ifont	!copy for common/dmenu/
	cbig=2.5
	itit=0		!no title yet
	ifsav=ifont		!save input value (if there is one)
	thbig=1.0		!line thickness factor for whole page
	csize=cbig
	thick=thbig
	thfacsml=0.6		!line thickness factor for small plots
c
	do 8 i=1,30
	ifnt(i)=ifont
	csfac(i)=1.0
8	continue
	do 81 i=1,100
	  iangle(i)=0
	  idraw(i)=-2		!until defined
c	  rx(i)=0.0		!NB world coord may be neg so cant tell if defined yet
81	continue
	do 83 i=1,10
83	iltype(i)=0		!continuous line for extra lines
c
	csfac(6)=0.7		!csize for param values=0.7*csize
	csfac(10)=1.2		!for title
	do i=11,30
	   csfac(i)=1.2		!for newtext (simplifies QDIALOG call!)
	enddo
c	iangle(8)=0			!0 set above
	ixjus(8)=center		!for x-axis label
	iyjus(8)=above
	iangle(9)=90		!for y-axis label
	ixjus(9)=center
	iyjus(9)=center
	if((.not.fitted).or.ifitype.eq.0) idraw(6)=0	!no param values
c
c Default line thickness
	ilt=20		!default thickness, unless reset
c	if(autplt.and.draft) ilt=0	!'draft' set below
	do i=1,100
	  lth(i)=ilt
	enddo
c	lth(21)=25	!axes (and axis labels at present!)
 	lth(22)=25	!frame
	lth(26)=15	!param value text
c
	if(isval.eq.1) then
	   narrow=1
	   isval=-1		!to indicate that position not yet defined
	endif
c
89	continue	!jump here if autplt
	if(idraw(6).ne.0.and.ifitype.ne.0) call PARTEXT(parval,ifitype,
     & theta,ncomp,ifnt(6),csfac(6)*csize)  !define text string for param values
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
c Establish top 3 lines of screen as scrolling region using utility library
c routines (windows lib works similarly but will not draw borders while
c in graphics mode, as far as I can tell, so no advantage really). See
c \fortran\TSCROL.FOR for tests.
	lastrow=2			!NB args of SCROLL=integer
	lrow=int2(lastrow)	!but args of LOCATE are int*2
	call SETATTR(112)		!'inverse' attrib seems nec while in graphics
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c
	idev=0			!screen
c 	if(debug()) pause 'call iniplt'
c	if(VIDEOTYP().ne.18) call INIPLT(idev,.false.,1.0)	!above
180	continue			!return here after plot to repeat
	call COLTYP1(15)		!bright white
c 	if(debug()) pause '7'
183	continue
	logx=ilog.eq.1.or.ilog.eq.3.or.ilog.eq.4.or.ilog.eq.6
	logy=ilog.eq.2.or.ilog.eq.3
	logity=ilog.eq.4		!for Hill plot
	sqrty=ilog.eq.5.or.ilog.eq.6
c If ncurvd=0 use ycal for scaling
c Note that min and max data values ALWAYS found here whatever ISCAL, and
c they are always the non-log values. Modified so that
c if logx or logy requested in call to VPLOT then negative values
c omitted when looking for min.
c 07/27/92 10:48am Small problem -for histo DATA (but not calc curves)
c have ndat()=nbin, but data goes up to nbin+2 (if 'hi bin' present), or
c up to nbin+1 (otherwise), so when looking for max need to go up to
c ndat()+2.  To keep call the same when searching xcal(), easiest to redefine
c ndat() here
	if(ncurvd.gt.0) then
	   do 182 j=1,ncurvd
182	   ndat1(j)=ndat(j)+2
	   call MINMAX1(xval,yval,ndat1,xmin1,xmax1,ymin1,
     &    ymax1,logx,logy,ndimd,ncurvd,icurvd)
	endif
c	if(debug()) then
c	   print 870,1,xmin,xmin1,iscal,logx
c	   pause
c	endif
870	format(' pos,xmin,xmin1,iscal,logx=',i3,2g13.6,i3,l4)
	if(ncurvd.le.0)call MINMAX1(xcal,ycal,ncal,xmin1,xmax1,ymin1,
     & ymax1,logx,logy,ndimc,ncurvc,icurvc)
C
c  If iscal.ne.0 use FIXAX to get new xmin,xtic etc
	if(iscal.eq.0) goto 108
	if(logy.and.inumy.eq.0) then	!not defined on input
	   inumy=1	!exponent form of numbering for log scale initially
	   amin=abs(alog10(ymin))
	   amax=abs(alog10(ymax))
	   if((amax.le.3.5).and.amin.le.3.5) inumy=-1	!non-exponent
	endif
	if(itx.lt.-1.or.itx.gt.1) then
	   itx=1	!default axis tic orientation in LAXES2
	   ity=1
	endif
	if(ntx.eq.0) ntx=5		!label every 5th tic
	if(nty.eq.0) nty=5		!label every 5th tic
	call FIXAX(xmin1,xmax1,xmin,xmax,xtic,0)	!always non-log
c	if(debug()) then
c	   print 870,2,xmin,xmin1,iscal,logx
c	   pause
c	endif
	call FIXAX(ymin1,ymax1,ymin,ymax,ytic,0)
	if(sqrty) then
	   ytic=1.0
	   nty=1
	   if(ymax.gt.49.) then
		ytic=4.0
		nty=4
	   endif
	   if(ymin.lt.0.) ymin=0.
	endif
	if(iscal.gt.-1) then
	   xcross=xmin		!crossing point for axes
	   ycross=ymin
	endif
108	continue
c
	ivxlo=0		! for VIEWPORT
	ivxhi=10000
	ivylo=0
	ivyhi=7000
	if(ixlo.ge.0) goto 1082
	if(landscap) then
c	   ixlo=1400		! set		dx=8100
c	   ixhi=9500		! display
c	   iylo=1600		! location  dy=4900
c	   iyhi=6500		! screen    dx/dy=1.653
c Make iyhi,ixhi lower to leave room for dialog box (also for narrow
c vertical box ar RHS of screen?)
c -proportions are close to golden section = 1:1.618
	   ixlo=1300		! set		dx=6800
	   ixhi=8100		! display
	   iylo=1500		! location	dy=4200
	   iyhi=5700		! screen    dx/dy=1.619
	else
	   ixlo=2200            !portrait
	   ixhi=7800
	   iylo=1480
	   iyhi=6650
	   if(ycross.gt.ymin) then
	      iylo=1000 	!make lower as no need for room for numbers/title
	      iyhi=6600
	   endif
c	   ixlo=2500            !portrait
c	   ixhi=7500
c	   iylo=1499
c	   iyhi=6999
	endif
1082	continue
	xticsav=xtic
	yticsav=ytic
	ixlo2=ixlo		!save screen GRAPHBOUNDARY
	ixhi2=ixhi
	iylo2=iylo
	iyhi2=iyhi
1081	call VIEWPORT(ivxlo,ivxhi,ivylo,ivyhi)
	call GRAPHBOUNDARY(ixlo,ixhi,iylo,iyhi)
c	if(debug()) pause '1'
c NB uses default viewport=whole screen
c
c Now the graphics. Return to 306 to plot with new line type etc
	if(iscal.gt.0.and.ilog.ne.0) goto 133	!scale for logs first
306	continue						!and return here
c	if(debug()) then
c	print 82,xmin,xmax,ymin,ymax,
c     & xtic,ytic,xcross,ycross,NCURVD,NCURVC,ndat(1),croset,
c     & titlex,titley,csize,ilabel,inumx,inumy,ivxlo,ixlo,
c     & logx,logy,sqrty,doframe,idraw(6),parval
c82	format(' xmin,xmax,ymin,ymax= ',4g13.6,/,
c     & ' xtic,ytic,xcross,ycross,NCURVD,NCURVC,NDAT(1),croset= ',/,
c     & 4g13.6,3I8,l4,/,
c     & ' titlex,titley= ',a40,/,1x,a40,/,
c     & ' csize,ilabel,inumx,inumy='g13.6,3i8,/,
c     & ' ivxlo,ixlo= ',2i8,/,/
c     & ' logx,logy,sqrty,doframe = ',4l4,/,
c     & ' idraw(6),parval=',i4,1x,a150)
c	pause
c	endif		!end debug
	if(.not.croset) then
	   xcross=xmin		!crossing point for axes
	   ycross=ymin
	endif
c Scale now
	if(.not.sqrty) call SCALE(xmin,xmax,ymin,ymax)
	if(sqrty) call SCALE(xmin,xmax,sqrt(ymin),sqrt(ymax))
c	if(debug()) pause '4'
	call ERASCR
104	continue			!return here to do plot
c Fill background colour
c	if(debug()) then
c	    print *,' xmin,. . .,ymax,vid = ',
c     &	xmin,xmax,ymin,ymax,videotyp()
c	endif
	if(.not.mono) then
c	    call FILLRECT(0,10000,800,5770,icol(71))
c	    call FILLRECT(0,10000,800,6150,icol(71))
c	    call FILLRECT(0,8300,800,6150,icol(71))	!graph area
c	    call FILLWIN(0,55,531,387,icol(71))		!graph area
	    call FILLWIN(0,55,639,387,icol(71))		!graph area
	    call FILLWIN(0,388,639,479,1)			!dialog area
	    call COLTYP1(15)		!bright white
	endif
c	subroutine OPENDIALOG(ibox,icf)
	call OPENDIALOG(1,icf,.true.)		!draw dialog box #1
c	if(debug()) pause '5'
c
	if(ilabel.lt.0) then
	   titlex='   X   '
	   titley=' Frequency '
	   if(logx) titlex='log(X) '
	   if(logy) titley='log(frequency) '
c	   if(ilog.eq.4) titley='logit(Y) '
	   if(ilog.eq.4) titley='log((Y-Y0)/(Ymax-Y)) '
	endif
c NB FRAME call is now in LAXES
c Last line of param is so that position of text strings (for axis
c labels etc) can be defined when they are, at the first call,
c calculated internally.
	call LAXES2(xmin,xmax,ymin,ymax,xtic,ytic,xcross,ycross,
     & ntx,nty,itx,ity,itlen,titlex,titley,xtitle,ytitle,ilabel,idev,
     & csfac(7),csfac(8),csfac(9),csize,ifnt(7),ifnt(8),ifnt(9),
     & thick,lth,inumx,inumy,xwbase,ilog,doframe,mono,icol,xc,yc,
     & rx,ry,iangle,ixjus,iyjus,idraw,rxbox,rybox,
     & numbx,numby,cnumx,cnumy,cexpx,cexpy)
	call COLTYP1(15)		!bright white
c
136	continue
c	if(debug()) pause '11'
c
c DO TITLE IF ANY
	if(idraw(10).eq.0.or.itit.eq.0) goto 1361		!no title
	call SETFONT1(ifnt(10))
	call SETCSIZE(csfac(10)*csize,isize)	!this defines ISIZE
	nl=NBLANK1(title1)
	ymin2=ymin
	ymax2=ymax
	if(sqrty) ymin2=sqrt(ymin)
	if(sqrty) ymax2=sqrt(ymax)
c position for title:
c Draw param values, if req. NB need to set box position etc only the first
c time here (when idraw()=-2 still). If TITLE values are altered (in position,
c size etc) the relevant parameters will be adjusted at time of alteration.
c#	if(ixt(10).eq.-1) then
	if(idraw(10).eq.-2) then
	  idraw(10)=1		!title position now defined
	  ix=iscrx(0.5*(xmin+xmax))
	  iy=iscry(ymax2+0.025*(ymax2-ymin2))
	  rx(10)=xworld(ix)
	  ry(10)=yworld(iy)
	  iangle(10)=0
	  ixjus(10)=center
	  iyjus(10)=below
c     define rxbox,rybox for posn of TITLE1
	  call TEXTBOX(ix,iy,title1(1:nl),iangle(10),isize,
     &     ixjus(10),iyjus(10),rxbox(1,10),rybox(1,10),0)
c	  if(debug())print 2324,2,(rxbox(i,10),i=1,4),(rybox(i,10),i=1,4)
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
c position already defined so quicker to call FRAME. But if graph size
c changed better redefine box!
c	if(idraw(10).eq.-1) call FRAMER(rxbox(1,10),rybox(1,10))
	if(idraw(10).eq.-1) call TEXTBOX(ix,iy,title1(1:nl),iangle(10),
     & isize,ixjus(10),iyjus(10),rxbox(1,10),rybox(1,10),1)
	call SETFONT1(ifont)		!reset
	call SETCSIZE(csize,isize)    !reset
1361	continue		!end of title
c
c
c DRAW PARAMETER VALUES, if req. NB need to set box position etc only the first
c time here (when idraw(6)=-2 still). If PARAM values are altered (in position,
c size etc) the relevant parameters will be adjusted at time of alteration.
	if(idraw(6).eq.0) goto 309	!no parameters to draw
	if(.not.fitted.or.ifitype.eq.0) goto 309	!no parameters to draw
	call SETFONT1(ifnt(6))
	call SETCSIZE(csfac(6)*csize,isize)	!this defines ISIZE
	nl=nblank1(parval)
	if(idraw(6).eq.-2) then
	    idraw(6)=-1	!so drawn with box by default
	    rx(6)=xmax-0.025*(xmax-xmin)
	    ymax2=ymax
	    if(sqrty) ymax2=sqrt(ymax)
	    ry(6)=ymax2-0.042*(ymax2-ymin)
	    ix=iscrx(rx(6))
	    iy=iscry(ry(6))
	    ixjus(6)=right
	    iyjus(6)=above
c define rxbox,rybox for posn of PARVAL
	    call TEXTBOX(ix,iy,parval(1:nl),iangle(6),isize,
     &     ixjus(6),iyjus(6),rxbox(1,6),rybox(1,6),0)
	else
	    ix=iscrx(rx(6))	!pos as defined by cursors or FIXTEXT
	    iy=iscry(ry(6))
	endif
	if(idev.ge.5) then
	   call LOADVTB('2'//'W'//char(lth(26)))
	else
	   call LINWID(int4(lth(26)))
	endif
	if(.not.mono) call COLTYP1(icol(26))		!colour for params
	call JUSTIFYSTRING(ix,iy,parval(1:nl),iangle(6),isize,
     & ixjus(6),iyjus(6))
c draw the box round PARVAL if necessary- could call TEXTBOX again, but
c position already defined so quicker to call IFRAME1
c	 if(idraw(6).eq.-1) call FRAMER(rxbox(1,6),rybox(1,6))
	  if(idraw(6).eq.-1) call TEXTBOX(ix,iy,parval(1:nl),
     &   iangle(6),isize,ixjus(6),iyjus(6),rxbox(1,6),rybox(1,6),1)
	call SETFONT1(ifont)		!reset font
	call SETCSIZE(csize,isize)	!reset csize,isize
309	continue		!end of par val
c
c DRAW EXTRA TEXT IF ANY. Note that there is no default position for new
c text so its position must already be defined if ntext>0
	if(ntext.gt.0) then
	  do 223 i=1,ntext
	   j=i+10
	   if1=ifnt(j)
	   cs1=csfac(j)*csize
	   ix=iscrx(rx(j))
	   iy=iscry(ry(j))
	   call SETFONT1(if1)
	   call setcsize(cs1,isize)    !sets isize too
c 	   if(debug()) print 85,(newtext(i)(j:j),j=1,30)
85	   format(/,' TEXT= ',/,5X,30a1)
	   if(idev.ge.5) then
	      call LOADVTB('2'//'W'//char(lth(i+30)))
	   else
	      call LINWID(int4(lth(i+30)))
	   endif
	   if(.not.mono) call COLTYP1(icol(i+30))		!colour for text(i)
	   call JUSTIFYSTRING(ix,iy,newtext(i),iangle(j),
     &    isize,base,base)
c draw the box round NEWTEXT if necessary- could call TEXTBOX again, but
c position already defined so quicker to call FRAME
	   if(idraw(j).eq.-1) call FRAMER(rxbox(1,j),rybox(1,j))
c	   if(idraw(i).eq.-1) call TEXTBOX(ix,iy,newtext(i),iangle(j),
c     &   isize,ixjus(j),iyjus(j),rxbox(1,j),rybox(1,j),1)
223	   continue
	  call SETFONT1(ifont)	!reset current values
	  call setcsize(csize,isize)    !sets isize too
	endif
c Draw arrows if any
	if(narrow.gt.0) then
	 if(isval.eq.-1) then	!define pos of Sval arrow
	   xb(1)=Sval
	   yb(1)=ymax-0.1*(ymax-ymin)
	   xe(1)=Sval
	   ye(1)=ymax-0.4*(ymax-ymin)
	 endif
	 do 222 i=1,narrow
	 if(idev.ge.5) then
	      call LOADVTB('2'//'W'//char(lth(i+50)))
	 else
	      call LINWID(int4(lth(i+50)))
	 endif
c Call to ARROW includes colour (with Hgraph numbering) (Sval=arrow #1 if
c present)
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
c
c PLOT THE HISTOGRAM
c
c For line thickness need separate control if there a several histos (ncurvd>1)
c just as for calc curves
c 1-10= data sets (#1-10) (histo bars); 11-20=calc curves (#1-10);
c 21=axes; 22=frame; 23=axis labels (same for X,Y)
c 24=axis numbers (same for X,Y); 25=plot title; 26-30=spare;
c 31-35=extra text (#1-5); 36-40=arrows (#1-5)
c	if(debug()) pause '50'
	do 50 j1=1,ncurvd		!plot ncurvd histograms
	j=icurvd(j1)			!histogram to be plotted
	if(idev.ge.5) then
	      call LOADVTB('2'//'W'//char(lth(j)))
	else
	      call LINWID(int4(lth(j)))
	endif
	if(.not.mono) call COLTYP1(icol(j))		!colour for jth histo bars
	if(ndat(j).le.0) goto 303	!calc curve only
c Set baseline value to ensure it is on scale
	ybase=ymin
	if(logy) ybase=10.**ymin  !need non-log value (log taken in LOGVAL if req)
	ybase=ybase+1.e-5*abs(ybase)
c	ybase=ybase+1.e-6+.00001*abs(ymin)	!so not out of range
c	if(debug()) print 29,ymin,ybase
c29	format(' ymin,ybase= ',2g13.6)
c  first LO bin
	flo=Yval(0,j)
	if(flo.lt.0.0001) goto 20	!skip lo bin if not needed
	if(logx.or.logy.or.flo.le.0.) goto 20	!no LO bin
c	dxlo=xval(2)-xval(1)   !same as first bin unless goes below xmin
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
	  if(pon()) write(7,21) dxlo,flo
	  if(discprt) write(8,21) dxlo,flo
c	  call LOCATE(0,0)		!row=0,col=0
c	  print 21,dxlo,flo
21	  format(' Bin width for LO bin= ',g13.6,'. Freq density= ',g13.6)
	  call DCFORMAT(dxlo,8,3,cans)
	  call WDIALOG(1,'Bin width for LO bin = '//CHARNB(cans),ict)
	  call DCFORMAT(flo,8,3,cans)
	  call WDIALOG(1,'Frequency density = '//CHARNB(cans),ict)
	endif
c
	call LOGVAL(xlow,xv,xmin,xmax,logx,off,bad,.false.) !ixlow on scale?
	if(off.or.bad) goto 20		!no lo bin if not
 	call LOGVAL(flo,yv,ymin,ymax,logy,off,bad,sqrty)
c if Y off scale it is plotted as Ymax (set in logval)
c	if(off.or.bad) goto 20		!omit LO bin if Y off scale
	if(.not.mono) call COLTYP1(icol(10))		!colour end bins
	linetype=lt2			!line type for FLO bin
	if(xlow.eq.xmin) goto 34		!omit 1st vert
	call MOVETO(xv,0.,0,0)
	call MOVETO(xv,yv,1,linetype)
	goto 35
34	call MOVETO(xv,yv,0,0)
35	call LOGVAL(xval(1,j),xv,xmin,xmax,logx,off,bad,.false.)
	call MOVETO(xv,yv,1,linetype)		!draw top
c   go back to baseline
	call LOGVAL(ybase,yv,ymin,ymax,logy,off,bad,sqrty)
c	if(yval(1,j).gt.flo) linetype=ijoin(j)	!contin line
	call MOVETO(xv,yv,1,linetype)		!draw vert
c	goto 25
c
c  next regular histogram-
c  first do 1st vertical of regular histo
20	continue
c    start from baseline
	if(idev.ge.5) then
	      call LOADVTB('2'//'W'//char(lth(j)))
	else
	      call LINWID(int4(lth(j)))
	endif
	if(.not.mono) call COLTYP1(icol(j))		!colour for jth histo bars
	linetype=ijoin(j)
	k=0
38	k=k+1
	call LOGVAL(xval(k,j),xv,xmin,xmax,logx,off,bad,.false.)
c	if(bad) print 381,k,j,xval(k,j)
c381	format('&Bad value is Xval(',i3,',',i3,') = ',g13.6)
	if(off.or.bad) goto 38	!look for 1st good X if X off scale
	call LOGVAL(ybase,yv,ymin,ymax,logy,off,bad,sqrty)
c	if(bad) print 382,ybase,ymin,k,j
c382	format('&Bad value is ybase = ',g13.6,' ymin,k,j=',g13.6,2i4)
c if Y off scale it is plotted as Ymax (set in logval)
	call MOVETO(xv,yv,0,0)
	call LOGVAL(yval(k,j),yv,ymin,ymax,logy,off,bad,sqrty)
c	if(bad) print 383,k,j,yval(k,j)
c383	format('&Bad value is Yval(',i3,',',i3,') = ',g13.6)
c if Y off scale it is plotted as Ymax (set in logval)
c	if(off.or.bad) goto 25
	call MOVETO(xv,yv,1,linetype)	!draw 1st vert
25	continue
c  Now loop to draw top and 2nd vert. Starts at point x1,y1
	do 215 i=k,ndat(j)		!ndat=nbin; x=x(1) to x(nbin+1)
	  call LOGVAL(xval(i+1,j),xv,xmin,xmax,logx,off,bad,.false.)
c	  if(bad) print 381,i+1,j,xval(i+1,j)
c already checked that x > xmin. Once X > Xmax can leave loop
c altogther as all subsequent X will be off scale too
c	  if(off.or.bad) goto 215     !skip unless x is on scale
	  if(xv.gt.xmax) goto 39
	  call MOVETO(xv,yv,1,linetype)		!draw top
	  ynext=ybase   !for last bin- xval(ndat+1) is defined but not yval
 	  if(i.lt.ndat(j)) ynext=yval(i+1,j)
	  call LOGVAL(ynext,yv,ymin,ymax,logy,off,bad,sqrty)
c if Y off scale it is plotted as Ymin or Ymax (set in logval)
c	  if(off.or.bad) goto 215     !skip unless x is on scale
	  call MOVETO(xv,yv,1,linetype)		!draw vertical
215	continue
c
c  lastly HI bin. Could be improved later to plot (with recalc FHI)
c even after rescaling to reduce Xmax (no data to recalc FHI if Xmax
c is increased by rescaling)
c If regular histo went above xmax, OFF will be true here
39	continue
	fhi=Yval(ndat(j)+2,j)
	if(logx.or.logy.or.fhi.lt.0.0001) goto 36	!no hi bin
c	dxhi=xval(ndat+2,j)-xval(ndat+1,j)	!define in calling prog
c	xhigh=xval(ndat+1)+dxhi
	xhigh=xval(ndat(j)+2,j)
	call LOGVAL(xhigh,xv1,xmin,xmax,logx,off,bad,.false.)
	if(off.or.bad) goto 36		!no hi bin if Xhigh off scale
	call LOGVAL(xval(ndat(j)+1,j),xv,xmin,xmax,logx,off,bad,.false.)
	call LOGVAL(FHI,yv,ymin,ymax,logy,off,bad,sqrty)
c  plot as Ymax if FHI > Ymax
c	if(off.or.bad) goto 36
	linetype=lt2
	if(.not.mono) call COLTYP1(icol(10))		!colour end bins
	call MOVETO(xv,yv,1,linetype)		!1st vert
	call MOVETO(xv1,yv,1,linetype)	!hor
	call LOGVAL(ybase,yv,ymin,ymax,logy,off,bad,sqrty)
	call MOVETO(xv1,yv,1,linetype)	!last vert
	linetype=0
	if(.not.mono) call COLTYP1(icol(j))		!colour for jth histo bars
36	continue
c
50	continue
c Histograms finished
c
	if(ncurvc.gt.0) goto 303	!do calc curve before rescale option
	if(idev.le.4.and.ncurvc.le.0.and.PLOT) goto 1812
	if(idev.ge.5.and.ncurvc.le.0.and.PLOT) goto 1813	!do vtrans
15	continue
c Special returns that follow redrawing after HELP screen
	if(iret.ne.0) then
	 i=iret
	 iret=0
	 goto (15,221,11,13,221,235,236) i		!return after HELP screen
	endif
c
	if(iabs(iask).eq.3) goto 999		!straight out after drawing
c
150	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
151	continue
	Lb(13)=0
	Lb(14)=0
	text(1)='1: RESCALE     '
	text(2)='2:  ZOOM      '
	text(3)='3: GRAPH SHAPE'
	text(4)='4: CHANGE AXES  '
	text(5)='5:POSH OPTIONS'
	text(6)='6: GIVE TITLE '
	text(7)='7: PLOT NOW   '
	if(autplt) then
	   text(8)='8: STORE PLOT '
	else
	   text(8)='8: QUEUE PLOT '
	endif
	text(9)='9:END DISPLAY'
	text(10)='10:  REDRAW    '
	text(11)='+:X AXIS LABEL'
	text(12)='-:Y AXIS LABEL'
	text(15)='.:DEFAULT OPTIONS'
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
152	call CKEY(ch,ikey)
	if(ikey.lt.-1.or.ikey.gt.15) goto 151
	if(ikey.eq.-1) then	!F1 key
	   iret=1
	   goto 237		!help
	endif
c
	goto(11,140,40,13,221,991,155,301,999,306,401,401,152,152,154)ikey
c
c DEFAULT OPTIONS
154	continue
	nbox=7	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	if(mono) then
	   text(1)='1:COLOUR DISPLAY'
	else
	   text(1)='1: MONOCHROME  '
	endif
	text(2)='2:SET COLOURS (SCREEN)'
	text(3)='3:SET COLOURS (PLOT)'
	text(4)='4:SET COLOURS (MANUAL)'
	text(5)='5:USE THICKER LINES '
	text(6)='6:USE THINNER LINES '
	text(7)='7:SET LINE THICKNESS'
	call DCMENU(nbox,4,Lb,text,icol1,icol2)
	call CKEY(ch,ikey)
	if(ikey.lt.-1.or.ikey.gt.nbox) goto 154
	if(ikey.eq.1) then
	   mono=.not.mono
	   if(mono) call COLTYP1(15)		!bright white
	else if(ikey.eq.2) then
c set default colours even if AUTPLOT for now
	  mono=.false.
c	  call SETCOLS(mono,icol,autplt,plot)
	  call SETCOLS(mono,icol,.false.,.false.)
	else if(ikey.eq.3) then
	  mono=.false.
	  call SETCOLS(mono,icol,.false.,.true.)
	else if(ikey.ge.4.and.ikey.le.7) then
	   if(ikey.eq.4) imode=-2		!for histo colours
	   if(ikey.eq.5) imode=11		!increase line thickness
	   if(ikey.eq.6) imode=12		!decrease line thickness
	   if(ikey.eq.7) imode=-1	      !set histo line thickness
	   call SETATT(narrow,nline,ntext,isdev,
     &   lth,icol,icol0,icol1,icol2,ncurvd,icurvd,ncurvc,icurvc,
     &   ndimd,ndimc,ifitype,imode)
	endif
	goto 306	!redraw
c
c
C READ IN TITLE
c With plotter on, MOVE goes only to plotter. To get 'Enter title..'
c at right position on screen need sep MOVE with plotter off!
c Get label but do not draw until decided if plot is to be queued
991	continue
	call DCMENU(-4,4,Lb,text,0,0)	!delete box 4 only
	Lb(4)=-1
	call DCMENU(-4,4,Lb,text,icol1,icol2)	!draw box 4 only (italic)
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row=lrow, col 0
cc	call LABENT('Enter title for plot:',TITLE1,44)
	blank=itit.eq.0
c	call TITENT1('Enter title for plot:',title1,44,blank)
c	subroutine TDIALOG(ibox,query,title,nlen,blank,ict)
	call TDIALOG(1,'Enter title for plot:',title1,44,blank,ict)
	itit=1		!for plotq
	idraw(10)=-2	!so title inserted at default pos when graph redrawn
c	if(pon()) write(7,1003) title1
c	if(discprt) write(8,1003) title1
c1003	format(/,' Plot title: ',a44,'********************')
	call flush(7)
c	goto 306	!redraw so title seen (and its position defined)
c	goto 151	!main menu
	goto 152
c
c Section to read in new axis labels (and define boxes for them)
401	continue
	call DCMENU(-ikey,4,Lb,text,0,0)	!delete box #ikey only
	Lb(ikey)=-1
	call DCMENU(-ikey,4,Lb,text,icol1,icol2)	!draw box #ikey only (italic)
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row=lrow, col 0
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row=lrow, col 0
	if(ikey.eq.11) then
	   blank=ilabel.eq.0
c	   call TITENT1('Enter label for X axis:',titlex,40,blank)
	   call TDIALOG(1,'Enter label for X axis:',titlex,40,blank,ict)
	   titlex=charnb(titlex)
	   idraw(8)=-2	!so new label drawn at default posn in LAXES
	else if(ikey.eq.12) then
	   blank=ilabel.eq.0
c	   call TITENT1('Enter label for Y axis:',titley,40,blank)
	   call TDIALOG(1,'Enter label for Y axis:',titley,40,blank,ict)
	   idraw(9)=-2	!so new label drawn at default posn in LAXES
	endif
	ilabel=1		!so internal default labels not used
c	goto 151	!main menu
	goto 152
c
c ZOOM section
140	continue
	ikey=1		!if not zoomed yet
	if(zoomed) then
	   nbox=2
	   call SETLBOX(nbox,Lb,1)
	   call NUMSET			!set num lock on ready for response
	   call DCMENU(0,4,Lb,text,0,0)		!delete all
	   if(.not.zoomed) Lb(2)=0
	   text(1)='1. ZOOM IN    '
	   text(2)='2.RESTORE ORIG'
	   call DCMENU(nbox,4,Lb,text,icol1,icol2)
	   ikey=nkey()
	endif
	if(ikey.eq.2) then
	   zoomed=.false.
	   xmin=xminsav
	   xmax=xmaxsav
	   ymin=yminsav
	   ymax=ymaxsav
	   xcross=xcsav
	   ycross=ycsav
	   xtic=xtsav
	   ytic=ytsav
	else if(ikey.eq.1) then
	   if(.not.zoomed) then
		xminsav=xmin
		xmaxsav=xmax
		yminsav=ymin
		ymaxsav=ymax
		xcsav=xcross
		ycsav=ycross
		xtsav=xtic
		ytsav=ytic
	   endif
	   zoomed=.true.
147	   continue
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row lrow,col 0
c	   print 141
c141	   format('&Mark BOTTOM LEFT corner of new display with cursors')
	   call WDIALOG(1,
     &      'Mark BOTTOM LEFT corner of new display with cursors',ict)
	   call PUTCUR(2000,1500)
c         ch=FNDCUR(ix1,iy1)
         ch=char(FNDCUR(ix1,iy1))	!note new usage
         xmin=XWORLD(ix1)
         ymin=YWORLD(iy1)
	   call SYMBOL(xmin,ymin,4,csize,12)   !mark bottom left corner with diamond
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row lrow,col 0
c	   print 142
c142	   format('&Mark TOP RIGHT corner of new display with cursors  ')
	   call WDIALOG(1,
     &      'Mark TOP RIGHT corner of new display with cursors',ict)
	   ix1=ix1+2000
	   iy1=iy1+1500
	   if(ix1.gt.10000) ix1=9900
	   if(iy1.gt.7000) iy1=6900
	   call PUTCUR(ix1,iy1)
         ch=char(FNDCUR(ix1,iy1))	!note new usage
         xmax=XWORLD(ix1)
         ymax=YWORLD(iy1)
	   if(xmin.gt.xmax.or.ymin.gt.ymax) then
		call BELL(2)
c	      call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	      call LOCATE(lrow,0)		!row lrow,col 0
c		print 146
c146		format(
c     &    '&Maximum x,y must be greater than minimum -try again    ',/)
	      call WDIALOG(1,
     &      'Maximum x,y must be greater than minimum -try again',12)
		goto 147
	   endif
c	Fix tic length only (xminz etc not used)
	   call FIXAX(xmin,xmax,xminz,xmaxz,xtic,ilog)
	   call FIXAX(ymin,ymax,yminz,ymaxz,ytic,ilog)
	   if(xcross.lt.xmin) xcross=xmin
	   if(xcross.gt.xmax) xcross=xmax
	   if(ycross.lt.ymin) ycross=ymin
	   if(ycross.gt.ymax) ycross=ymax
	else
	   goto 140		!no valid key
	endif
	idraw(6)=-2       !Parameter values drawn at default posn
	idraw(10)=-2	!so title, if present, drawn at default posn
	idraw(8)=-2       !X axis label drawn at default posn
	idraw(9)=-2       !Y axis label drawn at default posn
	do 341 i=31,80
341	idraw(i)=-2		!all numbers at default posn
	goto 306		!redraw
c
c RESCALE/AXES section:
11	continue
	nbox=10	!number of boxes for DCMENU
	call SETLBOX(10,Lb,1)
113	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
111	continue
	Lb(6)=0
	Lb(7)=0
	Lb(8)=0
	Lb(9)=0
	text(1)='1. Xmin,Xmax  '
	text(2)='2. Ymin,Ymax  '
	text(3)='3. Xtic,Ytic  '
	text(4)='4. X,Y crossing'
	text(5)='5. TIC LAYOUT'
C	text(7)='7. TIC LAYOUT'
	text(10)='10. REDRAW    '
c
c	call NUMSET			!set num lock on ready for response
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
112	ikey=nkey()
	if(ikey.eq.-1) then	!F1 key
	   iret=3
	   goto 237		!help
	endif
c After rescale that affects numbering on axes (xmin,xmax,xtic,xcross) must
c redraw all numbers on the affected axis at their default positions (idraw=-2)
c Also do axis labels at default posn??
	idraw(6)=-2       !Parameter values drawn at default posn
	idraw(10)=-2	!so title, if present, drawn at default posn
c	if(ikey.eq.1.or.ikey.eq.3) then	!X axis altered
	if(ikey.ge.1.and.ikey.le.4) then
	   idraw(8)=-2       !X axis label drawn at default posn
	   do 30 i=31,55
30	   idraw(i)=-2
c	else if(ikey.eq.2.or.ikey.eq.4) then	!Y axis altered
	   idraw(9)=-2       !Y axis label drawn at default posn
	   do 31 i=56,80
31	   idraw(i)=-2
	endif
c
c	goto(16,17,18,19,13,40,14,11,11,115) ikey
	goto(16,17,18,19,14,112,112,112,112,115) ikey
	goto 11
c
14	continue
	nbox=10	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	text(1)='1.X tic ABOVE '
	text(2)='2.X tic CENTRE'
	text(3)='3.X tic BELOW '
	text(4)='4.Y tic RIGHT '
	text(5)='5.Y tic CENTRE'
	text(6)='6.Y tic LEFT  '
	text(7)='7. TIC LENGTH '
	text(8)='8.MINOR X TICS'
	text(9)='9.MINOR Y TICS'
	text(10)='10. DONE      '
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
	ikey=nkey()
	call ENDDIALOG(2,1)	!remove error box (if any)
	if(ikey.eq.1) itx=1
	if(ikey.eq.2) itx=0
	if(ikey.eq.3) itx=-1
	if(ikey.eq.4) ity=1
	if(ikey.eq.5) ity=0
	if(ikey.eq.6) ity=-1
	if(ikey.ge.7.and.ikey.le.9) then
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row=lrow,col 0
	   if(ikey.eq.7) then
c		print 144,itlen
c144		format('&Tic length now',i4,'; new value= ')
c	   	call INPUTi(itlen)
 		call DEFOLTi(itlen,defolt)
		call QDIALOG(1,'Tic length',defolt,ict,cans)
		call GETINPi(cans,itlen)
	   else
		if(ikey.eq.8) then
c		   print 143,ntx
c143		format('&Major tic for every',i4,' minor tics; new value= ')
c	   	   call INPUTi(ntx)
 		   call DEFOLTi(ntx,defolt)
		   call QDIALOG(1,'Major tic for every n minor: n',
     &		defolt,ict,cans)
		   call GETINPi(cans,ntx)
		else if(ikey.eq.9) then
c		   print 143,nty
c	   	   call INPUTi(nty)
 		   call DEFOLTi(nty,defolt)
		   call QDIALOG(1,'Major tic for every n minor: n',
     &		defolt,ict,cans)
		   call GETINPi(cans,nty)
		endif
	   endif
	endif
	if(ikey.eq.10) goto 11
	goto 14
c
115	continue
	goto 306	!redraw
16	continue
C move to suitable place for typing xmin,max- use Utility Lib LOCATE
c routine to move to 2nd line down (line 1=lower line of the 2-line
c scrolling region
3041	continue
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row=lrow,col 0
c	print 304,xmin,xmax
c304	FORMAT('&Xmin, Xmax [',f9.2,1x,f9.2,'] = ')
c	call INPUT2r(xmin,xmax)
c	subroutine QDIALOG(ibox,text,defolt,ict,cans)
	call DEFOLT2r(xmin,xmax,defolt)
	call QDIALOG(1,'Xmin, Xmax',defolt,ict,cans)
	call GETINP2r(cans,xmin,xmax)
	if(xmax.le.xmin) then
	endif
	call DCMENU(-1,4,Lb,text,0,0)	!delete box 1 only
	Lb(1)=-1
	call DCMENU(-1,4,Lb,text,icol0,icol2)	!draw box 1 only (italic)
c	Lb(1)=-1
c	call DELBOX1(1)	!delete single box
c	call DRAWBOX1(-1,text(1),icol0,icol2)	!redraw single box (italic)
	goto 112
c	goto 111
209	format(2g13.6)
17	continue
3051	continue
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row=lrow, col 0
c 	print 305,ymin,ymax
c305	FORMAT('&Ymin,Ymax [',f9.2,1x,f9.2,'] = ')
c	call INPUT2r(ymin,ymax)
	call DEFOLT2r(ymin,ymax,defolt)
	call QDIALOG(1,'Ymin, Ymax',defolt,ict,cans)
	call GETINP2r(cans,ymin,ymax)
	if(ymax.le.ymin) then
	   call BELL(1)
c	subroutine OPENDIALOG(ibox,icf)
	   call OPENDIALOG(2,icf2,.true.)	!draw dialog box #2
	   call WDIALOG(2,'Ymin<Ymax!',12)
	   goto 3051
	endif
	call DCMENU(-2,4,Lb,text,0,0)	!delete box 2 only
	Lb(2)=-1
	call DCMENU(-2,4,Lb,text,icol0,icol2)	!draw box 2 only (italic)
	goto 112
c
18	continue
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row=lrow, col 0
c 	print 308,xtic,ytic
c308	FORMAT('&Xtic,Ytic [',f9.2,1x,f9.2,']= ')
c	call INPUT2r(xtic,ytic)
	call DEFOLT2r(xtic,ytic,defolt)
	call QDIALOG(1,'Xtic, Ytic',defolt,ict,cans)
	call GETINP2r(cans,xtic,ytic)
c	if((xtic.le.0.0).or.(ytic.le.0.0)) goto 18
	if((xtic.le.0.0).or.(ytic.le.0.0)) then
	   call BELL(1)
	   call OPENDIALOG(2,icf2,.true.)	!draw dialog box #2
	   call WDIALOG(2,'Xtic,Ytic > 0!',12)
	   goto 18
	endif
	call DCMENU(-3,4,Lb,text,0,0)	!delete box 3 only
	Lb(3)=-1
	call DCMENU(-3,4,Lb,text,icol0,icol2)	!draw box 3 only (italic)
	goto 112
c
19	continue
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row=lrow, col 0
c	print 307,xcross,ycross
c307	FORMAT('&Xcross,Ycross [',f9.2,1x,f9.2,'] = ')
c	call INPUT2r(xcross,ycross)
	call DEFOLT2r(xcross,ycross,defolt)
	call QDIALOG(1,'Axes crossing point: X, Y',defolt,ict,cans)
	call GETINP2r(cans,xcross,ycross)
	croset=.true.		!cross position has been reset
	Lb(4)=-1
	goto 113		!redraw menu
c
c
c GRAPH SHAPE: Section to move graphboundary (eg if Y axis on right hand edge)
40	continue
	nbox=4	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	text(1)='1.DEFINE WITH CURSORS'
	text(2)='2.GIVE NUMBERS '
	text(3)='3. DEFAULT SHAPE'
	text(4)='4. SQUARE SHAPE'
	call DCMENU(nbox,4,Lb,text,icol1,icol2)
405	call CKEY(ch,ikey)
	if(ikey.eq.-16) goto 150	!ESC returns to main menu
c===CHECK IRET VALUE?
	if(ikey.eq.-1) then	!F1 key
	   iret=4
	   goto 237		!help
	endif
	if(ikey.lt.1.or.ikey.gt.4) goto 405
	goto(406,406,406,406) ikey
c MARK NEW SHAPE WITH CURSORS
406	if(ikey.eq.1) then
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row lrow,col 0
c	   print 4061
c4061	   format('&Mark BOTTOM LEFT corner of new graph with cursors')
	   call WDIALOG(1,
     &    'Mark BOTTOM LEFT corner of new graph with cursors',ict)
	   call PUTCUR(2000,1500)
         ch=char(FNDCUR(ixlo,iylo))		!note new usage
         x1=XWORLD(ixlo)
         y1=YWORLD(iylo)
	   call SYMBOL(x1,y1,4,csize,12)   !mark bottom left corner with diamond
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row lrow,col 0
c	   print 4062
c4062	   format('&Mark TOP RIGHT corner of new graph with cursors  ')
	   call WDIALOG(1,
     &      'Mark TOP RIGHT corner of new graph with cursors',ict)
	   ixhi=9000
	   iyhi=6000
	   call PUTCUR(ixhi,iyhi)
         ch=char(FNDCUR(ixhi,iyhi))		!note new usage
	else if(ikey.eq.2) then		!define shape by typing in numbers
	   rxlo=float(ixlo)/100.	!as percent
	   rxhi=float(ixhi)/100.	!as percent
         rylo=float(iylo)/70.	!as percent
	   ryhi=float(iyhi)/70.	!as percent
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row=lrow, col 0
c	   print 41,rxlo,rxhi,rylo,ryhi
c41	   format(
c     &'&Present graph boundary (% page): x= ',2f7.1,' y= ',2f7.1)
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row=lrow, col 0
c	   print 42
c42	   format('&Give four new values (with dec point) (%) = ')
c	   read 43,rxlo,rxhi,rylo,ryhi
c43	   format(4g13.6)
	   call DEFOLT2r(rxlo,rxhi,defolt)
	   call QDIALOG(1,'Graph boundaries for X (% of page)'
     &	,defolt,ict,cans)
	   call GETINP2r(cans,rxlo,rxhi)
	   call DEFOLT2r(rylo,ryhi,defolt)
	   call QDIALOG(1,'Graph boundaries for Y (% of page)'
     &	,defolt,ict,cans)
	   call GETINP2r(cans,rylo,ryhi)
	   ixlo=ifixr(100.*rxlo)
	   ixhi=ifixr(100.*rxhi)
	   iylo=ifixr(70.*rylo)
	   iyhi=ifixr(70.*ryhi)
	else if(ikey.eq.3) then		!restore default shape
	   xtic=xticsav
	   ytic=yticsav
	   ixlo=ixlo2		!restore screen GRAPHBOUNDARY
	   ixhi=ixhi2
	   iylo=iylo2
	   iyhi=iyhi2
	   itx=1
	   ity=1
	   xcross=xmin
	   ycross=ymin
	   landscap=.true.
	   doframe=.true.
	else if(ikey.eq.4) then		!square shape
	   ixhi=ixlo + (iyhi-iylo)	!reduce ixhi as nec
	endif
	call GRAPHBOUNDARY(ixlo,ixhi,iylo,iyhi)
c After change of shape by call to graphboundary must redraw ALL text
c at the same world coordinates in rx,ry. ie do not want any of the idraw=-2
c (all should be -1,0,1). But boxes may be in wrong positions
c or distorted,so all boxes redefined now, from the current rx,ry.
c Do this for newtext,parval(i=6),axis labels i=8,9), title (i=10)
c and for all numbers (i=31,..,31+numbx; i=56,...,56+numby)
c NB must call SCALE before doing this
	if(.not.sqrty) call SCALE(xmin,xmax,ymin,ymax)
	if(sqrty) call SCALE(xmin,xmax,sqrt(ymin),sqrt(ymax))
c is following reset of idraw() needed?
	idraw(6)=-2       !Parameter values drawn at default posn
	idraw(10)=-2	!so title, if present, drawn at default posn
	idraw(8)=-2       !X axis label drawn at default posn
	idraw(9)=-2       !Y axis label drawn at default posn
	idraw(1)=-2		!calibration bars drawn at default position and length
	do i=31,80
	  idraw(i)=-2		!all numbers at default posn
	enddo
	do 33 i=1,80	!i=index in iangle,...,rybox arrays
c skip non-existent text
	if(idraw(i).eq.-2) goto 33
	if(idraw(i).eq.0) goto 33	!necessary?
c (so all can be scaled if latter altered): csfac(i), i=1-5 for newtext;
c csfac(6) for param values; csfac(7) spare; csfac(8)=axis labels;
c csfac(9)=axis numbers; csfac(10)=title
c MODIF: 01/25/90 09:55am csfac(7)=axis numbers (same for both axes);
c csfac(8)=x axis label; csfac(9)=y axis label;
c and similarly fonts defined by ifnt(1) to ifnt(10)
	if(i.ge.11.and.i.le.30) j=i	!size/font for newtext
	if(i.eq.6) j=i	!size/font for parval
	if(i.eq.8.or.i.eq.9) j=i	!size/font for axis labels
	if(i.ge.31) j=7	!size/font for axis numbers
	if(i.eq.10) j=10	!size/font for title
	call SETFONT1(ifnt(j))		!set appropriate size and font
	call setcsize(csize*csfac(j),isize)    !sets isize too
c Initialise text1
	text1(1:50)='                                                 '
	text1(51:100)='                                                 '
	text1(101:150)='                                                 '
c	k=ichar(text1(10:10))	!==debug
c	if(i.ge.1.and.i.le.5) text1=newtext(i)
	if(i.ge.11.and.i.le.30) text1(1:80)=newtext(i-10)
c	k=ichar(text1(10:10))	!==debug
	if(i.eq.6) text1=parval
	if(i.eq.8) text1=xtitle
	if(i.eq.9) text1=ytitle
	if(i.eq.10) text1=title1
c	if(i.ge.11.and.i.le.30) text1=cnumx(i-10)
c	if(i.ge.31.and.i.le.50) text1=cnumy(i-30)
	if(i.ge.31.and.i.le.55) text1=cnumx(i-30)
	if(i.ge.56.and.i.le.80) text1=cnumy(i-55)
	ix=ISCRX(rx(i))
	iy=ISCRY(ry(i))
	nt=NBLANK1(text1)
	call TEXTBOX(ix,iy,text1(1:nt),iangle(i),isize,
     & ixjus(i),iyjus(i),rxbox(1,i),rybox(1,i),0)
33	continue
c Reset current values
	call SETFONT1(ifont)	!reset current values
	call setcsize(csize,isize)    !sets isize too
	call ERASCR
	goto 104		!redraw (SCALE already called)
c END OF GRAPH SHAPE OPTIONS
c
c
C SECTION TO CHANGE LOG SCALES
C  ILOG  =0 for arithmetic plot,
C	 =1 for plot Y vs log(x)
C	 =2 for plot log(Y) vs x
C	 =3 for plot log(Y) vs log(x)
c	 =4 for Hill plot
c	 =5 for sqrt(y) vs x
c	 =6 for sqrt(y) vs log(x)
13	continue
	nbox=10	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
134	call DCMENU(0,4,Lb,text,0,0)		!delete all
	text(1)='1. ARITHMETIC '
	text(2)='2.Y vs log(X) '
	text(3)='3.log(Y) vs X '
	text(4)='4.log(Y)/log(X)'
	text(5)='5. HILL PLOT  '
	text(6)='6.sqrt(Y) / X '
	text(7)='7.sqrt(Y)/logX'
	if(logx) then
	   if(inumx.eq.-1) then
		text(8)='8.EXPONENT X NUM'
	   else if(inumx.eq.1) then
		text(8)='8.FIXED X NUMBERS'
	   endif
	else
	   if(inumx.eq.-1) then
		text(8)='8.ALLOW X SCALING'
	   else if(inumx.eq.1) then
		text(8)='8.NO X SCALING'
	   endif
	endif
	if(logy) then
	   if(inumy.eq.-1) then
		text(9)='9.EXPONENT Y NUM'
	   else if(inumy.eq.1) then
		text(9)='9.FIXED Y NUMBERS'
	   endif
	else
	   if(inumy.eq.-1) then
		text(9)='9.ALLOW Y SCALING'
	   else if(inumy.eq.1) then
		text(9)='9.NO Y SCALING'
	   endif
	endif
	text(10)='10. REDRAW    '
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
	ikey=nkey()
	if(ikey.eq.-1) then	!F1 key
	   iret=4
	   goto 237		!help
	endif
	if(ikey.lt.1.or.ikey.gt.10) goto 13
	if(ikey.eq.10) goto 306		!redraw
	if(ikey.eq.8) then
		inumx=-inumx
		idraw(31)=-2   !so x axis numbers redrawn at default posn in LAXES
		Lb(8)=-1
		goto 134
	endif
	if(ikey.eq.9) then
		inumy=-inumy
		idraw(56)=-2   !so y axis numbers redrawn at default posn in LAXES
		Lb(9)=-1
		goto 134
	endif
	if(ikey.le.7) then
	   ilogsav=ilog
	   ilog=ikey-1
	endif
c
133	continue
	logx=ilog.eq.1.or.ilog.eq.3.or.ilog.eq.4.or.ilog.eq.6
	logy=ilog.eq.2.or.ilog.eq.3
	logity=ilog.eq.4		!for Hill plot
	sqrty=ilog.eq.5.or.ilog.eq.6
	if(ilog.eq.0) goto 131
	x0=1.e-37		!smallest value for log
	if(logx.and.xmin1.ge.x0) goto 131
	if((logy.or.logity).and.ymin1.ge.x0) goto 131
	if(sqrty.and.ymin1.ge.0.) goto 131
	   call BELL(1)
	   call OPENDIALOG(2,icf2,.true.)	!draw dialog box #2
	   call WDIALOG(2,'Negative log!',12)
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row=lrow, col 0
c	   print 132,xmin1,ymin1
c132	   format('&Cannot take log or sqrt: xmin1,ymin1= ',2g13.6,/,
c     &    ' New xmin,ymin (give -1 to abandon)= ')
c	   call INPUT2r(xmin1,ymin1)
 	   call DEFOLT2r(xmin1,ymin1,defolt)
	   call QDIALOG(1,'New Xmin, Ymin values [abandon]',defolt,
     &	ict,cans)
	   call GETINP2r(cans,xmin1,ymin1)
	if(xmin1.le.0.) then
	   ilog=ilogsav		!restore orig values and abandon
	   logx=ilog.eq.1.or.ilog.eq.3.or.ilog.eq.4.or.ilog.eq.6
	   logy=ilog.eq.2.or.ilog.eq.3
	   logity=ilog.eq.4		!for Hill plot
	   sqrty=ilog.eq.5.or.ilog.eq.6
	   goto 15
	endif
131	continue
	if(logity) then
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row=lrow, col 0
c	   print 130,y0,yinf
c130	   format('&Y(0), Ymax = ',2g13.6,'  O.K. [Y] ? ')
c	   read 101,ans
 	   call DEFOLT2r(y0,yinf,defolt)
	   call QDIALOG(1,'Y(0), Ymax',defolt,ict,cans)
	   call GETINP2r(cans,y0,yinf)
c	   if(UC(ans).eq.'N') then
c		call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c		call LOCATE(lrow,0)		!row=lrow, col 0
c		print 129
c129		format('&Y(0), Ymax = ')
c		read 209,y0,yinf
		if(yinf.lt.ymax1) then
 		   call BELL(1)
	   	   call WDIALOG(1,
     &	    'Biggest observation(s) are greater than Ymax!',12)
c		   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c		   call LOCATE(lrow,0)		!row=lrow, col 0
c		   print 127
c127		   format('& Biggest observation(s) are greater than Ymax')
c		   call BELL(1)
		   goto 131
		endif
		if(pon()) write(7,128) y0,yinf
		if(discprt) write(8,128) y0,yinf
128		format(' For Hill plot take Y(0), Ymax = ',2g13.6)
c	   endif
	endif
	xmin2=xmin1	!xmin1 etc always non-log values
	xmax2=xmax1
	ymin2=ymin1
	ymax2=ymax1
	if(logx) xmin2=alog10(xmin1)
	if(logx) xmax2=alog10(xmax1)
	if(logy) ymin2=alog10(ymin1)
	if(logy) ymax2=alog10(ymax1)
	if(logity) ymin2=alog10((ymin1-y0)/(yinf-ymin1))
	if(logity) ymax2=alog10((ymax1-y0)/(yinf-ymax1))
	il=1
	if(.not.logx) il=0
c if logx then following call converts xmin,xmax to log scale
	call FIXAX(xmin2,xmax2,xmin,xmax,xtic,il)
	il=1
	if(.not.logy) il=0
	call FIXAX(ymin2,ymax2,ymin,ymax,ytic,il)
	if(sqrty) then
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
c AFTER CHANGE OF LOG AXES NEED TO REDRAW EVERYTHING AT DEFAULT POSN
c When, as for NEWTEXT and ARROWS then could take log/sqrt/antilog etc
c of rx,ry but this involves checking how axis CHANGES eg might change
c from logy/logx to logy/x so Y axis is log, but not changed so no
c need to alter RY(). This not yet fixed, so log axes should be changed
c before any newtext/arrows added.
	do i=31,80
	  idraw(i)=-2		!all numbers at default posn
	enddo
	idraw(6)=-2       !Parameter values drawn at default posn
	idraw(8)=-2       !X axis label drawn at default posn
	idraw(9)=-2       !Y axis label drawn at default posn
	idraw(10)=-2	!so title, if present, drawn at default posn
	croset=.false.	!so sets xcross=xmin etc
c
	goto 306
c
c Section to change line type
cc ikey=6 for lines joining data points; ikey=7 for calc curve lines
cc ikey=8 for symbol type; ikey=9 for symbol size
c ikey=6 for symbol type; ikey=7 for symbol size
c ikey=8 for lines joining data points; ikey=9 for calc curve lines
116	continue
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row lrow,col 0
c ask which data set/calc curve (j value) if more than one
	j1=1
	if(ikey.eq.6.or.ikey.eq.7.or.ikey.eq.8) then	!data
	   if(ncurvd.gt.1) then
c		print 117,ncurvd
c117	 	format('&Histogram data number (1 to ',i2, ') [0=all] = ')
c	 	read 3,j1
		j1=0
		call INTCONV(ncurvd,cnum1)
		call QDIALOG(1,'Histogram data number (1 to '
     &	  //CHARNB(cnum1)//') [all] ',' ',ict,cans)
		call GETINPi(cans,j1)
	   endif
	   if(j1.gt.0) then
		j2=icurvd(j1)	!selected set
	   else
		j2=icurvd(1)	!use 1st set plotted for 'present value'
	   endif
	else if(ikey.eq.9) then		!calc curve
	   if(ncurvc.gt.1) then
c		print 118,ncurvc
c118		format('&Curve number (1 to ',i2, ') [0=all] = ')
c		read 3,j1
		j1=0
		call INTCONV(ncurvc,cnum1)
		call QDIALOG(1,'Curve number (1 to '
     &	  //CHARNB(cnum1)//') [all] ',' ',ict,cans)
		call GETINPi(cans,j1)
	   endif
	   if(j1.gt.0) then
		j2=icurvc(j1)	!selected set
	   else
		j2=icurvc(1)	!use 1st set plotted for 'present value'
	   endif
	endif
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row lrow,col 0
	if(ikey.eq.8) then
c	   print 119,ijoin(j2)
c119	   format('&Line type (0 to 8) (now= ',i3,') = ')
c	   read 3,i
	   i=ijoin(j2)
	   call DEFOLTi(i,defolt)
	   call QDIALOG(1,'Line type (0 to 8)',defolt,ict,cans)
	   call GETINPi(cans,i)
	   if(j1.gt.0) then
		ijoin(j2)=i
	   else
		do 1191 j2=1,ncurvd
		j=icurvd(j2)
1191		ijoin(j)=i
	   endif
	else if(ikey.eq.9) then
c	   print 120,iline(j2)
c120	   format(
c     & '&Line type (0 to 5, or 10-18,-1=skip)  (now= ',i3,')= ')
c	   read 3,i
	   i=iline(j2)
	   call DEFOLTi(i,defolt)
	   call QDIALOG(1,'Line type (0-5, 10-18)',defolt,ict,cans)
	   call GETINPi(cans,i)
	   if(j1.gt.0) then
		iline(j2)=i
	   else
		do 1201 j2=1,ncurvc
		j=icurvc(j2)
1201		iline(j)=i
	   endif
	endif
c	goto 306	!redraw
	call DCMENU(-ikey,4,Lb,text,0,0)	!delete box #ikey only
	Lb(ikey)=-1
	call DCMENU(-ikey,4,Lb,text,icol0,icol2)	!redraw (italic)
	goto 221	!back to menu
c
c POSH PLOT SECTION
c e.g.add/alter/move all text; add arrows or linetype legends;
c control line thickness (necessitates use of VTRANS at present)
c NB Need to store number of bits of extra text, arrows and their
c positions, so they can be reproduced when graph redrawn.
c Keep numbering as in VPLOT (though leaves a lot of blank boxes in menu)
221	continue
	iret=0
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	Lb(4)=0
	if(ncurvd.le.0) then
	   Lb(6)=0
	   Lb(7)=0
	   Lb(8)=0
	endif
	if(ncurvc.le.0) Lb(9)=0
	Lb(12)=0
	Lb(13)=0
	Lb(14)=0
	text(1)='1: ADD NEW TEXT'
 	text(2)='2: FIX TEXT   '
 	text(3)='3: FIX NUMBERS '
c	text(4)='4.FIX CAL BAR(s)'
	text(5)='5:ADD ARROW/LINE'
c	text(6)='6: SYMBOL TYPE'
c	text(7)='7: SYMBOL SIZE'
	text(8)='8:FIX HISTO LINE'
	text(9)='9:FIX CALC LINE'
	text(10)='10. REDRAW   '
	if(doframe) then
	 text(11)='+:OMIT FRAME'
	else
       text(11)='+:DRAW FRAME'
	endif
c	if(isdev.eq.0) then
c	   text(12)='-:SHOW SD BARS'
c	else if(isdev.eq.1) then
c	   text(12)='-:NO SD BARS '
c	endif
c	text(13)='*:FIX C-JUMP LOGO'
c	text(14)='/:FIX V-JUMP LOGO'
	text(15)='.:LINE THICKNESS'
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
219	call CKEY(ch,ikey)
	if(ikey.eq.-1) then	!F1 key
	   iret=5
	   goto 237		!help
	endif
c===		1   2   3   4   5  6   7   8   9   10  11 12
	goto(241,232,235,219,234,219,219,116,116,306,12,219,
     &	219,219,236) ikey
	goto 221	!no valid key
c
12	doframe=.not.doframe	!toggle frame
	goto 221	!more changes?
c
c
c USE FIXTEXT OPTION HERE
232	continue
c Decide which of the 50 text boxes are to be modified by putting
c crosshairs within the req box (undefined boxes have idraw=-2 and
c are skipped)
	call LOCTEXT(ib,80,iangle,rxbox,rybox,idraw)
	if(ib.eq.0) goto 221	!give up
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	call SETLBOX(2,Lb,1)
	text(1)=
     & 'Move=ARROWS(or F2=cursor); Size=+,-; Font=/*; Rotate=R,r;'
      text(2)=
     & 'BOX on/off=b; Delete/undel=DEL; hi/low sens=INS; end=ENTER'
	call DCMENU(2,1,Lb,text,icol1,icol2)
c Now alter text of text string #ib (also adjusts rxbox and other
c position parameters.
	if(ib.ge.11.and.ib.le.30) j=ib	!size/font for newtext
	if(ib.eq.6) j=ib				!size/font for parval
	if(ib.eq.8.or.ib.eq.9) j=ib		!size/font for axis labels
	if(ib.ge.31) j=7			!size/font for axis numbers
	if(ib.eq.10) j=10	!size/font for title
	call SETFONT1(ifnt(j))		!set appropriate size and font
	call setcsize(csize*csfac(j),isize)    !sets isize too
c Initialise text1
	text1(1:50)='                                                 '
	text1(51:100)='                                                 '
	text1(101:150)='                                                 '
c	k=ichar(text1(10:10))	!==debug
c	if(ib.ge.1.and.ib.le.5) text1=newtext(i)
	if(ib.ge.11.and.ib.le.30) text1=newtext(ib-10)
c	k=ichar(text1(10:10))	!==debug
	if(ib.eq.6) text1=parval
	if(ib.eq.8) text1=xtitle
	if(ib.eq.9) text1=ytitle
	if(ib.eq.10) text1=title1
c	if(i.ge.11.and.i.le.30) text1=cnumx(i-10)
c	if(i.ge.31.and.i.le.50) text1=cnumy(i-30)
	if(ib.ge.31.and.ib.le.55) text1=cnumx(ib-30)
	if(ib.ge.56.and.ib.le.80) text1=cnumy(ib-55)
	ix=ISCRX(rx(ib))
	iy=ISCRY(ry(ib))
	call FIXTEXT(ix,iy,text1,iangle(ib),csfac(j),csize,ifnt(j),
     & ixjus(ib),iyjus(ib),idraw(ib),rxbox(1,ib),rybox(1,ib),
     & ib,ifitype,theta,ncomp,ntdel)
	if(ib.eq.-1) goto 221		!not a text box
	rx(ib)=xworld(ix)	!adjust these too, in case text moved
	ry(ib)=yworld(iy)
c	if(debug()) print 801,ib,j,csfac(j),ifnt(j),iangle(ib)
c801	format(' ib,j,csf,ifnt,iang= ',2i3,f5.2,2i4)
c	if(debug()) then
c	   call locate(12,1)
c	   print 995,idraw(9),rx(9),ry(9)
c995	   format(' After FIXTEXT',i4,2g13.6)
c	endif
c If NTDEL set then newtext #n has been deleted so reduce ntext and
c rearrange others
	if(ntdel.eq.1.and.ib.ge.11.and.ib.le.30) then
	  ntdel=0	!reset
c	  idraw(i)=-2	!so skipped by LOCTEXT
	  j1=0  		!for newtext
	  j=10		!for position,font,size
	  do 2444 n1=1,ntext
	  n=n1+10
	  if(n.eq.ib) goto 2444	!skip deleted one
	  j=j+1
	  j1=j1+1
	  newtext(j1)=newtext(n1)
	  idraw(j)=idraw(n)
	  rx(j)=rx(n)
	  ry(j)=ry(n)
	  ifnt(j)=ifnt(n)
	  csfac(j)=csfac(n)
	  iangle(j)=iangle(n)
	  ixjus(j)=ixjus(n)
	  iyjus(j)=iyjus(n)
	  lth(j1+30)=lth(n+30)
	  do 2445,m=1,4
	  rxbox(m,j)=rxbox(m,n)
2445	  rybox(m,j)=rybox(m,n)
2444	  continue
	  idraw(ntext+10)=-2		!so skipped by LOCTEXT
	  ntext=ntext-1
	endif
c If font for parameter values changed within FIXTEXT, the new font
c cannot be changed within FIXTEXT because to do so requires that
c the string PARVAL be completely regenerated- can do this here, ready
c for next redraw (or could do within FIXTEXT if call to PARTEXT
c incorp in it. NOW PUT IN FIXTEXT VIA EXTRA PARAMETERS (LAST ROW)
c	if(ifnt(6).ne.ifsav) call PARTEXT(parval,ifitype,theta,ncomp,
c     &  ifnt(6),csfac(6)*csize)		!define text string for param values
	if(ib.eq.6) parval=text1		!in case font altered in FIXTEXT
c If size or font of X axis label has been changed then that
c of Y axis label will be similarly changed when redrawn, but at present
c box position for latter not updated (and similarly with x,y reversed)
	if(ib.eq.8.and.idraw(8).ne.0) then  !do not change if 0, so deleted
	   idraw(8)=1	!so x-axis label at new posn in rx(8),ry(8)
	endif
	if(ib.eq.9.and.idraw(9).ne.0) then
	   idraw(9)=1	!so y-axis label at new posn in rx(9),ry(9)
	endif
	if(ib.ge.31) idraw(ib)=1	 !so axis numbers at new posn in rx(),ry()
c	if(debug()) print 2324,2,(rxbox(i,n),i=1,4),(rybox(i,n),i=1,4)
2324	format(' #',i2,':  ',4g13.6,3x,4g13.6)
	goto 221
c
c FIXNUMB OPTION
235	continue
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	call SETLBOX(5,Lb,1)
	text(1)='1. ONE NUMBER '
	text(2)='2. ALL X AXIS '
	text(3)='3. ALL Y AXIS '
	text(4)='4.ALL NUMBERS '
	text(5)='5.  DONE      '
	call DCMENU(5,5,Lb,text,icol1,icol2)
	imode=nkey()
	if(imode.eq.-1) then	!F1 key
	   iret=6
	   goto 237		!help
	endif
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	call SETLBOX(2,Lb,1)
	if(imode.eq.1) then		!define which number to alter
	   call LOCTEXT(n,80,iangle,rxbox,rybox,idraw)
	   if(n.eq.0) goto 221	!give up
	   text(1)=
     &   'ARROWS=move; R,r=rotate; '
         text(2)=
     & ' DEL=toggle delete; INS=toggle sensitivity; ESC/ENTER=end'
	else if(imode.eq.2.or.imode.eq.3) then
         text(1)=
     & 'ARROWS/F2=move; DEL=delete/undel; INS=sensit; ESC/ENTER=end'
	   Lb(2)=0
	else if(imode.eq.4) then
	   Lb(2)=0
         text(1)=
     & '+,-=size; /*=font;  INS=sensitivity; ESC/ENTER=end'
	else if(imode.eq.5) then
	   goto 221
	else
	   goto 235		!no valid imode
	endif
	call DCMENU(2,1,Lb,text,icol1,icol2)
c	if(debug()) then
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row=lrow,col 0
c	   print 700,(rx(i),i=11,16),(rx(i),i=31,36)
c	endif
	call FIXNUMB(imode,n,csfac(7),csize,ifnt(7),inumx,inumy,
     & logx,logy,rx,ry,iangle,ixjus,iyjus,idraw,rxbox,rybox,
     & numbx,numby,cnumx,cnumy,cexpx,cexpy)
c	if(debug()) print 701,(idraw(i),i=31,40),(idraw(i),i=56,65)
c701	format(10i3,/,10i3)
	if(n.eq.-1) goto 235		!not an axis number box
c All idraw() for numbers to be drawn should be no longer set to -2
c so numbers should be redrawn at new posn in LAXES
c	if(debug()) then
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row=lrow,col 0
c	   print 700,(rx(i),i=11,16),(rx(i),i=31,36)
c	endif
	goto 221
c
c233	continue
c	goto 221
c
c Add/delete Arrow
234	continue
c start of arrow- need to store start/end points, and number of arrows,
c so they can be reproduced when graph redrawn
	ixfix=0		!x not fixed
	iyfix=0		!y not fixed
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	nbox=10		!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	if(isval.eq.0) Lb(8)=0
	Lb(9)=0
2340	if(narrow.eq.0) Lb(2)=0
	if(nline.eq.0) Lb(4)=0
	text(1)='1.ADD AN ARROW '
	text(2)='2.DELETE ARROW '
	text(3)='3.ADD A LINE   '
	text(4)='4.DELETE LINE  '
	text(5)='5.LINE TYPE    '
	text(6)='6.SPECIFY X    '
	text(7)='7.SPECIFY Y    '
	text(8)='8.S(obs) ARROW '
c	text(9)='9.             '
	text(10)='10.  DONE      '
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
	ikey=nkey()
	if(ikey.lt.1.or.ikey.gt.10) goto 221	!no valid key
c start of arrow- need to store start/end points, and number of arrows,
c so they can be reproduced when graph redrawn
	goto(26,26,27,27,27,28,28,26,234,221) ikey
c
28	continue
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row lrow,col 0
	if(ikey.eq.6) then
c	   print 2800,xmin,xmax
c2800	   format('&Draw at X (between',2g10.3,' = ')
c	   call INPUTr(xv)
	   call REALTOCH(xmin,cnum1,11)
	   call REALTOCH(xmax,cnum2,11)
	   call QDIALOG(1,'Draw at X (between'//
     &    CHARNB(cnum1)//' and '//CHARNB(cnum2)//')',' ',ict,cans)
	   call GETINPr(cans,xv)
	   ixfix=1		!x fixed
	   Lb(6)=-1
	else if(ikey.eq.7) then
c	   print 2801,ymin,ymax
c2801	   format('&Draw at Y (between',2g10.3,') = ')
c	   call INPUTr(yv)
	   call REALTOCH(ymin,cnum1,11)
	   call REALTOCH(ymax,cnum2,11)
	   call QDIALOG(1,'Draw at Y (between'//
     &    CHARNB(cnum1)//' and '//CHARNB(cnum2)//')',' ',ict,cans)
	   call GETINPr(cans,yv)
	   iyfix=1		!y fixed
	   Lb(7)=-1
	endif
	goto 2340
26	continue
	if(ikey.eq.2.and.narrow.gt.0) then		!delete
2354	   continue
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row lrow,col 0
c	   print 2351,narrow
c2351	   format('&Delete arrow number (1-',i2,') = ')
c	   read 3,k
	   call INTCONV(narrow,cnum1)
	   call QDIALOG(1,
     &'Delete arrow number (1 to '//CHARNB(cnum1)//')',' ',ict,cans)
	   call GETINPi(cans,k)
	   if(k.le.0.or.k.gt.narrow) goto 2354
         call ARROW(xb(k),yb(k),xe(k),ye(k),-1,2,.true.)	!delete orig
	   j=0
	   do 2352 i=1,narrow
	   if(i.eq.k) goto 2352	!skip deleted one
	   j=j+1
	   xb(j)=xb(i)
	   yb(j)=yb(i)
	   xe(j)=xe(i)
	   ye(j)=ye(i)
2352	   continue
	   narrow=narrow-1
	else if(ikey.eq.1.or.ikey.eq.8) then
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row lrow,col 0
	   if(ikey.eq.1) then
		narrow=narrow+1			!count arrows
	      k=narrow
c		print 2341,narrow
c2341		format(
c     &      '&Arrow #',i2,': define start and end points with cursors')
	      call INTCONV(k,cnum1)
		cnum1=CHARNB(cnum1)
	      call WDIALOG(1,'Arrow #'//CHARNB(cnum1)
     &	//': define start and end points with cursors',ict)
	   else if(ikey.eq.8) then		!for Sval
		isval=1	!so new pos used when redrawn
c		print 2343
c2343	     format('&Define top and bottom of S(obs) arrow with cursors')
	      call WDIALOG(1,
     &	 'Define top and bottom of S(obs) arrow with cursors',14)
		k=1		!Sval arrow is #1 if present
            call ARROW(xb(k),yb(k),xe(k),ye(k),-1,2,.true.)	!delete orig
	      ixfix=1		!x fixed
	      xv=Sval
	   endif
	   call PUTCUR(5000,3500)		!start with cursor central
         ch=char(FNDCUR(ix1,iy1))		!define start of arrow
         ch=char(FNDCUR(ix2,iy2))		!define end of arrow
         xb(k) = XWORLD(ix1)
         yb(k) = YWORLD(iy1)
         xe(k) = XWORLD(ix2)
         ye(k) = YWORLD(iy2)
	   if(ixfix.eq.1) xb(k)=xv       ! x specified
	   if(ixfix.eq.1) xe(k)=xv
	   if(iyfix.eq.1) yb(k)=yv       ! y specified
	   if(iyfix.eq.1) ye(k)=yv
c Call to ARROW includes colour (with Hgraph numbering)
	   if(mono) then
		ic=0
	   else
		ic=15-icol(k+50)		!convert to Hgraph numbering
		if(ic.eq.15) ic=-1	!black (for icol=0)
	   endif
         call ARROW(xb(k),yb(k),xe(k),ye(k),ic,2,.true.)
	   ixfix=0		!clear again
	   iyfix=0		!clear again
	endif
	goto 221
c
27	continue
	if(ikey.eq.5.and.nline.gt.0) then		!line type
	   k=1
	   if(nline.gt.1) then
c		call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c		call LOCATE(lrow,0)		!row lrow,col 0
c		print 2700,nline
c2700	   	format('&Alter line number (1-',i2,') = ')
c	      read 3,k
		call INTCONV(nline,cnum1)
		call QDIALOG(1,'Histogram data number (1 to '
     &	  //CHARNB(cnum1)//')',' ',ict,cans)
		call GETINPi(cans,k)
	   endif
	   call MOVETO(xlb(k),ylb(k),0,iltype(k))
	   call MOVETO(xle(k),yle(k),-1,iltype(k))	!delete
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row lrow,col 0
c	   print 119,iltype(k)
c	   read 3,iltype(k)
	   call DEFOLTi(iltype(k),defolt)
	   call QDIALOG(1,'Line type (0 to 8)',defolt,ict,cans)
	   call GETINPi(cans,iltype(k))
	   call MOVETO(xlb(k),ylb(k),0,iltype(k))
	   call MOVETO(xle(k),yle(k),1,iltype(k))	!redraw
	   goto 221
	endif
	if(ikey.eq.4.and.nline.gt.0) then		!delete
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row lrow,col 0
c	   print 2701,nline
c2701	   format('&Delete line number (1-',i2,') = ')
c	   read 3,k
	   call INTCONV(nline,cnum1)
	   call QDIALOG(1,'Delete line number (1 to '
     &     //CHARNB(cnum1)//')',' ',ict,cans)
	   call GETINPi(cans,k)
	   call MOVETO(xlb(k),ylb(k),0,iltype(k))
	   call MOVETO(xle(k),yle(k),-1,iltype(k))
	   j=0
	   do 2702 i=1,nline
	   if(i.eq.k) goto 2702	!skip deleted one
	   j=j+1
	   xlb(j)=xlb(i)
	   ylb(j)=ylb(i)
	   xle(j)=xle(i)
	   yle(j)=yle(i)
	   iltype(j)=iltype(i)
2702	   continue
	   nline=nline-1
	else if(ikey.eq.3) then             !add line
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row lrow,col 0
	   nline=nline+1			!count lines
c	   print 2342,nline
c2342	   format(
c     &     '&Line #',i2,': define start and end points with cursors')
	   call INTCONV(nline,cnum1)
	   call WDIALOG(1,'Line #'//CHARNB(cnum1)
     &	//': define start and end points with cursors',ict)
	   call PUTCUR(5000,3500)		!start with cursor central
         ch=char(FNDCUR(ix1,iy1))		!define start of line
         ch=char(FNDCUR(ix2,iy2))		!define end of line
	   i=nline
         xlb(i) = XWORLD(ix1)
         ylb(i) = YWORLD(iy1)
         xle(i) = XWORLD(ix2)
         yle(i) = YWORLD(iy2)
	   if(ixfix.eq.1) xlb(i)=xv       ! x specified
	   if(ixfix.eq.1) xle(i)=xv
	   if(iyfix.eq.1) ylb(i)=yv       ! y specified
	   if(iyfix.eq.1) yle(i)=yv
	   call MOVETO(xlb(i),ylb(i),0,iltype(i))
	   call MOVETO(xle(i),yle(i),1,iltype(i))
	   ixfix=0		!clear again
	   iyfix=0		!clear again
	endif
	goto 221
c
c
c Line type legend
C###	NOT DONE
c	continue
c	goto 221
c
c Line thickness
236	continue
	imode=-1	!for line thickness
	call SETATT(narrow,nline,ntext,isdev,
     & lth,icol,icol0,icol1,icol2,ncurvd,icurvd,ncurvc,icurvc,
     & ndimd,ndimc,ifitype,imode)
	goto 306	!redraw
c
c Change default size/font
c===MOVE THIS TO MAIN MENU 'DEFAULTS'
37	continue
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row=lrow, col 0
c	print 371,ifont
c371	format(
c     & '&present font number is ',i3,': new value (0-11,A,B) = ')
c	read 3,ifont
	call DEFOLTi(ifont,defolt)
	call QDIALOG(1,'New font number (0-11,A,B)',defolt,ict,cans)
	call GETINPi(cans,ifont)
	ifsav=ifont
	ifont2=ifont	!copy for common/dmenu/
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row=lrow, col 0
c	print 372,csize
c372	format(
c     & '&present size is ',f5.1,': new value (0.5-18) = ')
c	call INPUTr(csize)
	call DEFOLTr(csize,defolt)
	call QDIALOG(1,'New font size (0.5-18)',defolt,ict,cans)
	call GETINPr(cans,csize)
c Reset all defaults too (will override existing alterations): try changing
c only those that are set to current default
	do 84 i=1,30
	if(ifnt(i).eq.ifont) then
	   ifnt(i)=ifont1
	   csfac(i)=csize1/csize
	endif
84	continue
	ifont=ifont1
	ifont2=ifont	!copy for common/dmenu/
	csize=csize1
	call SETFONT1(ifont)
	cbig=csize				!change default 'whole page' size
	call SETCSIZE(csize,isize)    !sets isize too
	goto 221
c
c ADD TEXT
241	continue
c	if(ntext.le.19) goto 2411	!OK- room for another one
c All 20 newtext arrays used; must delete one in order to add more
c Delete again specified bits of text that have been added
c (This bit deletes NEWTEXT for good and frees the space for another bit
c of new text, whereas FIXTEXT just sets idraw=0 so not drawn (rather
c than deleted)
	if(ntext.gt.19) then
c	   call LOCATE(0,0)
c	   print 226	!' CURRENT NEW TEXT STRINGS:
c	   do 2271 i=1,ntext
c2271	   print 2372,i,newtext(i)(1:75)
c2443	   print 244
c244	   format('&Before adding new text must delete one: delete no= ')
c        read 3,it
	   call WDIALOG(1,'CURRENT NEW TEXT STRINGS:',ict)
	   do i=1,ntext
		call INTCONV(i,defolt)
		n=NBLANK(newtext(i))
	      call WDIALOG(1,CHARNB(defolt)//':'// newtext(i)(1:n),ict)
	   enddo
2443	   call QDIALOG(1,
     &   'Must delete one existing text: delete # ',' ',ict,cans)
	   call GETINPi(cans,it)
	   if(it.lt.1.or.it.ge.20) goto 2443
	   j1=0  		!for newtext
	   j=10		!for position,font,size
	   do 2441 n1=1,ntext
		n=n1+10
		if(n.eq.it) goto 2441	!skip deleted one
		j=j+1
		j1=j1+1
		newtext(j1)=newtext(n1)
		idraw(j)=idraw(n)
		rx(j)=rx(n)
		rx(j)=ry(n)
		ifnt(j)=ifnt(n)
		csfac(j)=csfac(n)
		iangle(j)=iangle(n)
		ixjus(j)=ixjus(n)
		iyjus(j)=iyjus(n)
		lth(j1+30)=lth(n+30)
		do m=1,4
		  rxbox(m,j)=rxbox(m,n)
		  rybox(m,j)=rybox(m,n)
		enddo
2441	   continue
	   idraw(ntext+10)=-2		!so skipped by LOCTEXT
	   ntext=ntext-1
	endif
c
c Continue to add new text:
2411	continue
	ntext=ntext+1		!count titles
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row lrow,col 0
c Better to use boxes/fixtext to change angle,size,font (+help button)
c remove for now
	n=ntext+10
c	print 451,ntext,iangle(n),ifnt(n),csfac(n)*csize
c451	format(
c     & '&Text #',i2,': angle, font, size= ',i4,',',i3,',',f5.1,
c     & ': alter these [N] ? ')
c	read 101,ans
c101	format(a1)
c	if(UC(ans).eq.'Y') then
c	  call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	  call LOCATE(lrow,0)		!row lrow,col 0
c	  print 4511,ntext
c4511	  format(
c     &  '&Text #',i2,': angle (0-359), font (0-11), size (0.5-18.)= ')
c	  read 4522,iangle(n),if1,cs1
c4522	  format(2i8,g13.6)
c	  ifnt(n)=if1
c	  csfac(n)=cs1/csize
c	endif
c Fonts if=0=draft; 1=duplex; 2=complex; 3=bold (triplex); 4=script; 5=greek,
c but for ^F command use ifont=2*if for upright, or ifont=2*if+1 for italic
c ( for greek use 'A', 'B' in place of 10,11)
	call SETFONT1(ifnt(n))
	call setcsize(csfac(n)*csize,isize)    !sets isize too
c NB Readstring is equivalent to call to FNDCUR followed by GETSRING
	call PUTCUR(5000,3500)		!start with cursor central
	call READSTRING(ix,iy,newtext(ntext),
     & iangle(n),isize)
c Define all text string values (do once only, unless text altered), not
c everytime graph redrawn
	rx(n)=XWORLD(ix)
	ry(n)=YWORLD(iy)
	ixjus(n)=base
	iyjus(n)=base
	idraw(n)=1
	nl=NBLANK1(newtext(ntext))
	call TEXTBOX(ix,iy,newtext(ntext)(1:nl),iangle(n),isize,
     & ixjus(n),iyjus(n),rxbox(1,n),rybox(1,n),0)
c
c Reset current values
	call SETFONT1(ifont)	!reset current values
	call setcsize(csize,isize)    !sets isize too
	goto 221
c
c HELP (need to delete graph, or make window ?)
237	continue
c Fonts if=0=draft; 1=duplex; 2=complex; 3=bold (triplex); 4=script; 5=greek,
c but for ^F command use ifont=2*if for upright, or ifont=2*if+1 for italic
c ( for greek use 'A', 'B' in place of 10,11)
c At present font=0-5 input; better use 0-11 so italics can be controlled
c	call SCROLL(0,0,24,79,1)
	call ERASCR
c Go to alpha mode for help?
 	call VIDEOMOD(3)
	call LOCATE(0,0)
	print 2370
2370	format(
     & ' GENERAL INFORMATION',/,
     & ' (1) If LOG AXES or GRAPH SHAPE (both under RESCALE/AXES)',/,
     & '    are to be changed, this should be done first, before',/,
     & '    adding extra text, arrows, lines etc',/,
     & ' ',/,
     & ' ENTER for help with text entry: ESC to return')
	i=nkey()
	if(i.eq.-16) goto 306
	print 2371
2371	format(
     & ' FONTS:',/,
     & '    0,1 = draft; 2,3 = duplex; 4,5 = complex; 6,7 = bold',/,
     & '    8,9 = script; 10,11=greek/maths (p36),but use A,B with ^F'
     & ,/,'     (use 2nd digit for italic form)',/,
     & ' SIZE:',/,
     & '    =0.5 to 18.0, but values other than integers 1-9 give a',/,
     & '    a scale factor (0.5-2.0) which may alter other text.',/,
     & ' TEXT STRINGS:',/,
     & '    Starting point for new text marked by cursors. ',/,
     & '    As well as characters, text may contain control codes',/,
     & '    (''CTRL'', denoted ''^''), viz. ^N=new line;',/,
     & '    ^U=up half line; ^D=down half line; ^B=backspace;',/,
     & '    ^Fn=font n (n=1,..9,A,B); ^Sn=size n (n=1-9 only)',/,
     & ' FIX TEXT/ FIX AXIS NUMBERS',/,
     & '    First mark bit to be altered with cursors. Then use keys',/,
     & '    to move etc, as shown at bottom of screen. F2 puts up',/,
     & '    cursors again to mark new position for text (for big moves)'
     & ,/,' LINE TYPES:',/,
     & '	LINES BETWEEN POINTS: 0=solid; 1=dotted;',/,
     & '	     2-6=inc length dashes; 7=dash-dot; 8=dash-dot-dot.',/,
     & '	CALC CURVES: -1=skip; 0=solid; 1=''dots''; 2=short dash;',/
     & ,'      3=long dash; 4=long-short dash; 4=long-short-short.',/,
     & ' SYMBOLS:   (pos=hollow; neg=filled)',/,
     & '     0=point; 1=triangle up; 2=triangle down; 3=square',/,
     & '     4=diamond; 5=pentagon; 6=hexagon; 7=circle; 8= +; 9= x')
c     & '',/,
c Type strings with their numbers (font/size not shown in ASCII printout!)
	if(ntext.ge.1) then
	   print 228
228	   format('&Print the current NEW TEXT strings [N] ? ')
	   read 101,ans
101	   format(a1)
	   if(UC(ans).eq.'Y') then
		print 226
226	 	format(' CURRENT NEW TEXT STRINGS:',/)
	 	do 227 i=1,ntext
227	 	print 2372,i,newtext(i)(1:75)
2372	 	format(' #',i2,1x,a75)
	   endif
	endif
	print 2373
2373	format(' Press any key to return')
	call ANYKEY()
	call INIPLT(idev,.false.,scalfac)	!back to graphics mode after help
	goto 306
c
c
C SECTION FOR GRAPH PLOTTER
c NB DESKJET DRAWS 1000 DEVICE UNITS/INCH IN LANDSCAPE
C AND ABOUT 1250 UNITS/INCH IN PORTRAIT. Thus landscape gives better resolution
c even for tall narrow graphs- use portrait only if text needed on same page
c Note that this prog is used in AUTPLT only to make single posh plots
c (for automatic plotting of many graphs VHISTQ is used). But when
c used in EKDIST can also make posh plots, or may want quick (draft- ie
c line thickness=0) plots, but only one per page in either case.
c   mpos=1 upper left
c   mpos=2 upper right
c   mpos=3 lower left
c   mpos=4 lower right
c   mpos=5 whole page
c   mpos=6 top half page
c   mpos=7 bottom half page
c   mpos=8 X cm by Y cm
c ----- set plotter parameters
155	continue
	plot=.true.
	monsav=mono			!keep value before plot
	do i=1,100
	   icolsav(i)=icol(i)
	enddo
	call HGDRIVER(iplotter,colplotter)
	if(colplotter) then
	   ans='Y'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'Plot the graph in colour',
     &    defolt,ict,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'N') then
		mono=.true.
	   else
		mono=.false.
	   endif
	else
	   mono=.true.
	endif
	landplot=.true.		!landscape is default
	draft=.false.
c set whole page as default
	csize=cbig		!csize=default size for whole page plots
	thick=thbig		!line thickness=default thickness for whole page plots
c NB ixlo already set (and possibly reset by 'MOVE GRAPH') so do set here
c NB ixlo already set (and possibly reset by 'MOVE GRAPH') so do set here
	ixlo2=ixlo		!save screen GRAPHBOUNDARY
	ixhi2=ixhi
	iylo2=iylo
	iyhi2=iyhi
	ivxlo=0		! for VIEWPORT
	ivxhi=10000
	ivylo=0
	ivyhi=7000
	nbox=10		!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	Lb(1)=-1		!whole page is default
	call DCMENU(0,4,Lb,text,0,0)		!delete all
1551	continue
	text(1)='1.WHOLE PAGE  '
	text(2)='2. TOP LEFT   '
	text(3)='3. TOP RIGHT  '
	text(4)='4.LOWER LEFT  '
	text(5)='5.LOWER RIGHT '
	text(6)='6.FIX ON VDU  '
 	text(7)='7.            '
	if(draft) then
	  text(7)='7.THICK LINES  '
	else
	  text(7)='7. DRAFT       '
	endif
	if(landplot) then
	  text(8)='8.PLOT PORTRAIT'
	else
C	  text(8)='8. LANDSCAPE   '
	  text(8)='8.PLOT LANDSCAPE'
	endif
 	text(9)='9.CANCEL PLOT '
	text(10)='10. PLOT NOW   '
c	subroutine DRAWBOX(nbline,Lbox,text,csize,ifont)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
1552	ikey=nkey()
	if(ikey.eq.1) mpos=5
	if(ikey.ge.2.and.ikey.le.5) mpos=ikey-1	!=1-4
	if(ikey.eq.6) mpos=8
	if(ikey.ge.2.and.ikey.le.7) then
	  call DCMENU(-1,5,Lb,text,0,0)	!delete box #1 only
	  Lb(1)=1
	  call DCMENU(-1,5,Lb,text,icol0,icol2)	!redraw (not italic)
	endif
	if(ikey.eq.10) then
	   call DCMENU(-10,5,Lb,text,0,0)	!delete box 4 only
	   Lb(10)=-1
	   call DCMENU(-10,5,Lb,text,icol1,icol2)	!draw box 4 only (italic)
	   scalfac=1.15
	   call DEFOLTr(scalfac,defolt)
	   call QDIALOG(1,'Scale up to fill paper?: scale factor',
     &	defolt,ict,cans)
	   call GETINPr(cans,scalfac)
	endif
c
	goto(71,72,72,72,72,73,75,76,1814,1041) ikey
	goto 1552	!no valid key
c
75	continue
	draft=.not.draft
	if(draft) ifont=0		!draft font
	if(.not.draft) ifont=ifsav
	ifont2=ifont	!copy for common/dmenu/
	call DCMENU(-7,4,Lb,text,0,0)		!delete box 7 only
	goto 1551
c
76	continue
	landplot=.not.landplot
	call DCMENU(-8,4,Lb,text,0,0)		!delete box 8 only
	goto 1551
c
71	continue	!whole page
	csize=cbig		!csize=default size for whole page
	thick=thbig		!line thickness=default thickness for whole page plots
c	thick=1.0		!line thickness multiplier
	ixlo=1400		! for GRAPHBOUNDARY
	ixhi=9500
	iylo=1600
	iyhi=6500
	ivxlo=0		! for VIEWPORT
	ivxhi=10000
	ivylo=1
	ivyhi=7000
	Lb(1)=-1
	call DCMENU(-1,4,Lb,text,0,0)		!delete box 1 only
	do 711 i=2,7
	if(Lb(i).eq.-1) then
	  Lb(i)=1
	  call DCMENU(-i,4,Lb,text,0,0)		!delete box i only
	endif
711	continue
	goto 1551
c
72	continue	!quarter page positions
	csize=csize*cfacsml
	thick=thick*thfacsml
	thick=0.5		!line thickness multiplier
	ixlo=1000		! for GRAPHBOUNDARY- same for all
	ixhi=4750
	iylo=800
	iyhi=3250
	if(mpos.eq.1) then
	  ivxlo=0		! for VIEWPORT: top left
	  ivxhi=5000
	  ivylo=3500
	  ivyhi=7000
	else if(mpos.eq.2) then
	  ivxlo=5000	!for VIEWPORT: top right
	  ivxhi=10000
	  ivylo=3500
	  ivyhi=7000
	else if(mpos.eq.3) then
	  ivxlo=0		!for VIEWPORT: bottom left
	  ivxhi=5000
	  ivylo=0
	  ivyhi=3500
	else if(mpos.eq.4) then
	  ivxlo=5000	!for VIEWPORT: bottom right
	  ivxhi=10000
	  ivylo=0
	  ivyhi=3500
	endif
	Lb(ikey)=-1
	call DCMENU(-ikey,4,Lb,text,0,0)		!delete box #ikey only
	goto 1551
c
c Define position of plot on screen
73	continue
c A4 paper is about 210*297 mm (8.3*1.7 inch) and 297/210=sqrt(2) nearly
c 7000*sqrt(2)=9900 nearly; 7000/sqrt(2)=4950 nearly
c NB DESKJET DRAWS 1000 DEVICE UNITS/INCH IN LANDSCAPE
C AND ABOUT 1250 UNITS/INCH IN PORTRAIT. Thus landscape gives better resolution
c even for tall narrow graphs- use portrait only if text needed on same page
c But manual (p9) states that on 8.5*11 inch paper landscape uses a 7*10 inch
c region, and portrait uses a 5.6 (vert) by 8 (horizontal) inch region
c (8/5.6 = sqrt(2) approx still, so put up same box even if PORTRAIT)
	call ERASCR
c	call LOCATE(5,10)		!row=5,col 10
c	print 731
c731	format(' Define bottom left and top right corners with cursors')
	call WDIALOG(1,
     & 'Define bottom left and top right corners with cursors',ict)
c	if(landplot) then
c	  call IFRAME(ixlo,ixhi,iylo,iyhi)
	  call IFRAME(50,9950,0,7000)		!9950-50=9900
c	else
c	  call LOCATE(6,10)		!row=5,col 10
c	  print 732
c732	  format(' (dashed area is not usable)')
c	  call IFRAME(2525,7475,3500,7000)     !7475-2525=4950
cc	  top half of box=solid line, bottom half (not usable) dashed
c	  call FRAME(xworld(2525),xworld(7475),yworld(0),yworld(3500),2)
c	endif
	call PUTCUR(2000,2000)
      ch=char(FNDCUR(ix1,iy1))
	call PUTCUR(8000,5500)
      ch=char(FNDCUR(ix2,iy2))
	if(ix1.gt.ix2) then
	  i=ix1
	  ix1=ix2
	  ix2=i
	endif
	if(iy1.gt.iy2) then
	  i=iy1
	  iy1=iy2
	  iy2=i
	endif
	call IFRAME(ix1,ix2,iy1,iy2)	!show defined region-viewport
	ix3=ifixr(float(ix1)+0.14*float(ix2-ix1))	!bottom left of axes
	ix4=ifixr(float(ix1)+0.95*float(ix2-ix1))	!bottom right of axes
	iy3=ifixr(float(iy1)+0.16*float(iy2-iy1))	!bottom left of axes
	iy4=ifixr(float(iy1)+0.93*float(iy2-iy1))	!top left of axes
	call IFRAME(ix3,ix4,iy3,iy4)	!show region of axes- graphboundary
c	if(debug()) then
c	  call LOCATE(lrow,0)		!row=lrow,col 0
c	  print 733,ix1,ix2,iy1,iy2,ix3,ix4,iy3,iy4
c	  pause
c	endif
	ivxlo=0		! for VIEWPORT
	ivxhi=10000
	ivylo=0
	ivyhi=7000
	ixlo=ix3		! for GRAPHBOUNDARY
	ixhi=ix4
	iylo=iy3
	iyhi=iy4
c
c
c Calc size in cm
c NB DESKJET DRAWS 1000 DEVICE UNITS/INCH IN LANDSCAPE
c AND ABOUT 1250 UNITS/INCH IN PORTRAIT. Thus landscape gives better resolution
	fac=2.54/1000.
	if(.not.landplot) fac=2.54/1250.	!portrait
	x1=float(ix2-ix1)*fac		!outer boundary
	y1=float(iy2-iy1)*fac
	x2=float(ix4-ix3)*fac		!axes boundary
	y2=float(iy4-iy3)*fac
c Could do output below with WDIALOG but not yet fixed
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row=lrow,col 0
c	print 734,x1,y1,x2,y2
c734	format('&Outer boundary (X by Y) = ',f7.2,' by ',f7.2,' cm',/,
c     & ' Axes boundary  (X by Y) = ',f7.2,' by ',f7.2,' cm')
cc option to make an exact number of centimetres?
cc	if(debug()) then
cc	  call LOCATE(lrow,0)		!row=lrow,col 0
cc	  print 733,ivxlo,ivxhi,ivylo,ivyhi,ixlo,ixhi,iylo,iyhi
cc733	  format('&view= ',4i6,' graph= ',4i6)
cc	endif
	Lb(6)=-1
	call DCMENU(-6,4,Lb,text,0,0)		!delete box #6 only
c scale down char size to new mean length of x and y axes
	fac=0.5*(float(ix2-ix1)/10000.+float(iy2-iy1)/7000.)
	csize=csize*sqrt(fac)
	thick=fac		!line thickness multiplier
	goto 1551
c
c
c DO THE PLOT- if draft mode it can be done straight away but if posh
c (or default line thickness used) it must be archived in PLOT99.DAT
c for plotting via VTRANS.
1041	continue
c	if(debug()) pause '1041'
	idev=4
	if(.not.landplot) idev=3	!portrait
	call REVCOL(icol)	!reverse black/white
	iview=0
c	if(.not.autplt) then
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row=lrow,col 0
c	   print 1555
c1555	   format('&Preview plot on screen [N] ? ')
c	   read 101,ans
c	   if(UC(ans).eq.'Y') iview=1		!preview
c	endif
	if(draft) then
		goto 1042			!plot at once
	else
c		idev=99
c		idev=4	!direct to plotter with new Hgraph (idev set above)
c		call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c		call LOCATE(lrow,0)		!row=lrow,col 0
c		print 1553
c1553		format('&Assembling the plot...')
c		goto 1042	!go through plot again with idev=99, and return to 1813
	      call WDIALOG(1,
     &      'Assembling the plot . . .',ict)
		goto 1042	!go through plot again with idev=4, and return to 1812
	endif
c
c Now call INIPLT with idev=3 or 4 to assemble the plot
c (calling iniplt with idev=4 does not erase screen?)
1042	continue
	call INIPLT(idev,.false.,scalfac)		!idev=4 set at 1551
	call VIEWPORT(ivxlo,ivxhi,ivylo,ivyhi)
	call GRAPHBOUNDARY(ixlo,ixhi,iylo,iyhi)
	if(.not.sqrty) call SCALE(xmin,xmax,ymin,ymax)
	if(sqrty) call SCALE(xmin,xmax,sqrt(ymin),sqrt(ymax))
	call SETCSIZE(csize,isize)    !sets isize too
	call SETFONT1(ifont)
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row=lrow,col 0
	call ERASCR
c	print 1553
c1553	format('&Plotting in progress...')
	call WDIALOG(1,'Plotting in progress . . .',12)
c###################need to redefine box positions here (at least for bits
c###################of text that have boxes on final graph) as done after
c###################'GRAPH SHAPE'
	goto 104		!do plot and return to 1812
c
c Return to 1813 to plot via VTRANS
1813	continue
	call ERASCR
c device for final plot
	idev=4
	if(.not.landplot) idev=3	!portrait
	call VTPLOT(99,iview,idev)		!plot PLOT99.DAT
	plot=.false.
c
c After plot finished:
1812	continue
c	print 81
c81	format(' Type <ENTER> to continue')		!needed by ENDPLT-no!
	call ENDPLT
c To eject page without picking up another, send 'Esc E' to laserjet (OK for
c deskjet too?).  If this is NOT done then another plot can be put on same page.
c      write(7,7)
c7	format('1')		!form feed
	write(7,*) char(27)//'E'
      call FLUSH(7)
1814	continue		!jump here if plot cancelled
	plot=.false.
	mono=monsav		!restore
	do i=1,100
	   icol(i)=icolsav(i)
	enddo
c?	itit=0		!no title yet
c Restore idev=0 (and scalfac, if altered)
	idev=0
	scalfac=1.0
	call INIPLT(idev,.false.,scalfac)
	call SETFONT1(ifont)
	csize=cbig		!character size
	thick=thbig		!line thickness=default thickness for whole page plots
c	thick=1.0
	call setcsize(csize,isize)    !sets isize too
	ixlo=ixlo2		!restore screen GRAPHBOUNDARY
	ixhi=ixhi2
	iylo=iylo2
	iyhi=iyhi2
	ivxlo=0		! for VIEWPORT
	ivxhi=10000
	ivylo=0
	ivyhi=7000
	goto 1081	!redraw whole screen
c
c
c PLOT QUEUE SECTION
301	continue
	call VHSQ2(xval,yval,xcal,ycal,ndimd,ndimc,
     & ncurvd,ndat,icurvd,ijoin,ncurvc,ncal,icurvc,iline,
     & xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ntx,nty,itx,ity,
     & ixlo,ixhi,iylo,iyhi,itit,title1,csize,ifont,ilog,iscal,doframe,
     & titlex,titley,ilabel,inumx,inumy,idiskq,sval,isval,
     & theta,ifitype,ncomp,idest,icol,mono,xwbase,lt2,
     & lth,RX,RY,rxbox,rybox,IXJUS,IYJUS,iangle,idraw,ifnt,
     & csfac,cnumx,cnumy,cexpx,cexpy,numbx,numby,narrow,
     & xb,yb,xe,ye,ntext,nline,xlb,ylb,xle,yle,iltype,newtext)
	if(idest.eq.1551) goto 1551
c	goto 1812		!return to screen coordinates?
	goto 306		!redraw
c end of plot queue section
c
c CALC CURVE SECTION.
303	continue
	if(ncurvc.le.0) goto 999
	linetype=0		!always cont line- dashes synthesized via zseq
c
	do 54 j1=1,ncurvc		!plot ncurvc curves
	j=icurvc(j1)			!curve # to be plotted
	if(iline(j).lt.0) goto 54	!skip this curve (added 06/24/90 05:29pm)
	ij=iabs(iline(j))
	if(idev.ge.5) then
	      call LOADVTB('2'//'W'//char(lth(j+10)))
	else
	      call LINWID(int4(lth(j+10)))
	endif
	if(.not.mono) call COLTYP1(icol(j+10))		!colour for jth calc curve
	if(ij.gt.0) goto 310		!dashed calc curve
c
c problem with calc curve not drawn right up to edge of graph -need
c interpolation, or provide a calc point on boundary? Try drawing JUST
c past edge: if x was out of range on last cycle, but in range on this
c cycle, then draw from last to current pos. ie move (no line) to each
c point (whether in range or not, in case next point is in range), and
c when 1st point in range is found, draw to it. Should be OK for 1st point;
c for last point draw to the first point out of range- not fixed yet
c Also not fixed for dashed lines
	Jflag=0
	do 220 k=1,ncal(j)
	xv=xcal(k,j)
	yv=ycal(k,j)
	if(logx.and.xv.ge.1.e-32) xv=alog10(xv)
	if(logy.and.yv.ge.1.e-32) yv=alog10(yv)
	if(sqrty.and.yv.ge.0.) yv=sqrt(yv)
	if(jflag.eq.0) call MOVETO(xv,yv,0,0)	!1st point in range
	if(xv.lt.xmin.or.xv.gt.xmax) goto 220
	if(yv.lt.ymin.or.yv.gt.ymax) goto 220
	call MOVETO(xv,yv,1,linetype)
c
c#	if(xv.lt.xmin.or.xv.gt.xmax) goto 220
c#	if(yv.lt.ymin.or.yv.gt.ymax) goto 220
c#	if(jflag.eq.0) call MOVETO(xv,yv,0,0)	!1st point in range
c#	if(jflag.gt.0) call MOVETO(xv,yv,1,linetype)	!rest of points
	jflag=1
220	continue
	goto 311
c
c now section to draw dashed etc calc curves
310	continue
c  define dash sequences: specify lengths of each repeating sequence
c  starting with bright (down=true). Lengths specified as percent
c  of length of the X axis.
c		1='dotted'
c		2=short dash
c		3=long dash
c		4=long,short dash
c		5=long,short,short dash
c(previously: 0=continuous line; 1=long dash; 2=short dash; 3=long,short dash;
c 4=long,short,short dash)
	goto(702,703,704,705,706),ij
702	kseq=2
	zseq(1)=0.30
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
	sfac=ifixr(float(iyhi-iylo)/float(ixhi-ixlo))		!O.K.? ###
	xr=0.01*(xmax-xmin)
	yr=0.01*(ymax-ymin)
	if(sqrty) yr=0.01*(sqrt(ymax)-sqrt(ymin))
	k=1
 	zleft=zseq(k)
	down=.true.		!start with 'pen down'
c
      x0=1.e-37               !smallest value for logs
	Jflag=0
	do 313 i=1,ncal(j)
	xv=xcal(i,j)
	yv=ycal(i,j)
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
c	if(debug().and.abs(dxn).lt.1.e-4) then
c	   print 3141,i,xv,xvlast,dxn
c3141	   format(' i,xv,xvlast,dxn = ',i5,3g13.6)
c	   pause
c	endif
c	if(abs(dxn).lt.1.e-5) goto 317
c	b=dyn/dxn		!normalised slope
	vert=abs(dxn).lt.1.e-28*abs(dyn)    !line is vertical
	if(.not.vert) b=dyn/dxn		!normalised slope
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
	if(vert) then
	   xv1=xvlast
	   yv1=yvlast + zleft*yr/sfac
	else
	   xv1=xvlast + zleft*xr/sqrt(1.+b*b)
c	   yv1=yvlast + b*(xv1-xvlast)*yr/xr
	   yv1=yvlast + b*(xv1-xvlast)*yr/(sfac*xr)
	endif
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
	if(.not.plot) goto 15		!another chance to plot it
	if(idev.ge.5.and.PLOT) goto 1813	!do vtrans
	if(plot) goto 1812		!return with plot=true?
c
c TIDY UP AND EXIT
999	continue
c    IASK=1 or -1 to ask 'are you sure' before leaving VHIST; =2,-2 to not ask
c    IASK=pos to leave graph on screen (1 or 2); neg (-1,-2) to delete
	if(iabs(iask).eq.1) then
	   call BELL(1)
	   ans='Y'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'ARE YOU SURE',defolt,ict,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'N') goto 151
c	   call LOCATE(12,1)
c	   print 9991
c9991	   format(' ARE YOU SURE [Y] ? ')
c	   read 101,ans
c	   if(UC(ans).eq.'N') goto 151
	endif
c
	if(iask.gt.0) call DCMENU(0,4,Lb,text,0,0)	!delete boxes before exit
 	if(iask.lt.0)call VIDEOMOD(3)	 !this makes graph go for good!
	RETURN
	END

