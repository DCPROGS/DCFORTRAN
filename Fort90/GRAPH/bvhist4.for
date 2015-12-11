      subroutine VHIST4(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO1,XHI1,YLO1,YHI1,xwbase,lt2,inumx,inumy,
     & titlex,titley,ilabel,doframe,idiskq,
     & autplt,draft,itit,title1,cbig,ifont,landscap,fitted,
     & theta,ifitype,ncomp,sval,isval,iask,iver)
c
c
c Lahey 90 version 19/09/96
c Lahey V5.n version 02/10/93 09:19am
c VHIST4 08/06/96 06:00am has
c   (1) modifs to label dragging/input as in vplot5
c   (2) if itit=1 on entry uses input title1
c==TO BE DONE on VHIST4
c   Fix label moving and entry as in VPLOT5
c
c Modif 10/27/95 10:02pm: qfile declared and call of VHSQ3 altered
c Modif 12/20/94 11:20am
c  Separate common/cols/icol,mono created to communicate icol() to program
c	and these removed from common/tpos.  Thus if want only to control
c	colours then need only have common/cols/ in calling prog, not the
c	whole /tpos/ (as for VPLOT4).
c
c VHIST3 03/01/94 03:19pm has
c  (1) XCAL(), YCAL() arrays increased to 2048 (prev 512)
c  (2) Input parameters added to control colours, as follows:
c   =========
c  (2) Popup help added
c  (3) Dialog box moved to bottom -NO leave it
c Updated to use dialog boxes 06/29/93 03:38pm
C General purpose display/plot subroutine for DATA that is in histogram
c form.
c   (1) Uses DCMENU in place of DRAWBOX (see TDCBOX). Names of ixlo etc altered
c	to ixlo1 etc in call, so ixlo can be in common/hgv/.  This common can
c	be used for any subroutine that need to change viewport and/or
c	graphboundary, to allow restoration of current values before leaving.
c   (2) Colour added.  Note that lincols and FILL1 use the Lahey graphics
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
c	   call lincols(icol1)
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
c		(histo data), and of Ycal,Xcal(2048,ndimc) (for calc curves)
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
c		and FHI kept in Yval(ndat+1)
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
c thickness =THICK*Irlth(i) were THICK=THBIG=1.0 for whole page)
c Colours in icol() are numbered the same way.
c 1-10= data sets (#1-10) (histo bars); Use icol(10) for end (dashed) bins
c 11-20=calc curves (#1-10);
c 21=axes; 22=frame; 23=axis labels (same for X,Y)
c 24=axis numbers (same for X,Y); 25=plot title; 26=param value text;
c 27=symbols; 28=SD bars; 29-30=spare;
c 31-50=extra text (#1-20); 51-60=arrows (#1-10); 61-70=lines (#1-10)
c Use irlth(51), icol(51) for Sval arrow, if any (always arrow #1)
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
	real XCAL(2048,ndimc),YCAL(2048,ndimc)
c for histo data
	integer ndat(ndimd),icurvd(ndimd),ijoin(ndimd)
c for calc curves
	integer ncal(ndimc),icurvc(ndimc),iline(ndimc)
c
	real xvert(4),yvert(4)	!vertices for Hgraph FILL call
	real xsvert(4),ysvert(4)	!vertices for Hgraph FILL call
	integer ndat1(10)
	real theta(20)
	dimension zseq(10)
	character*1 ans,UC
	character*33 qfile,oldfile,printfil,metafil
	character*40 titlex,titley
	character*75 xtitle,ytitle	!output from LAXES
	character*75 oldtext
	character*64 TITLE1
	character parval*200		!to hold param values
	character text1*150		!to hold any string (for fixtext call)
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	LOGICAL PLOT,logx,logy,logity,down,pon,fitted,croset,equal,allpnt
	logical sqrty,slock,bad,off,vert,bigplot,present,plotcols
	logical doframe,autplt,draft,landplot,landscap,zoomed,blank
	logical caplock,debug,mono,monsav,redrawn,meta
	integer*2 lrow,int2,videotyp
c	 		!for line thickness
	real rlth(100)
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
	real angle(100),wpots(30)
      integer idraw(100),icol(100),icolsav(100)
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25)
c	integer ifnt(10)
c	real csfac(10)
c	real xb(5),yb(5),xe(5),ye(5)		!up to 5 arrows
c	character*80 newtext(5)		!extra text
	integer ifnt(30)
	real size(30)
	real xb(10),yb(10),xe(10),ye(10)		!up to 10 arrows
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	real*4 yhline(10)		!record y value
	real*4 xhlb(10),xhle(10)     !start/end of HLINE
	real*4 xvline(10)		!record x value
	real*4 yvlb(10),yvle(10)     !start/end of VLINE
	integer iltype(10)				!line type for ditto
	integer ilhtype(10)	!line type for horizontal lines
	integer*1 ihlinrel(10)	!=1 if line pos rel to line 1
	integer ilvtype(10)	!line type for vertical lines
	integer*1 ivlinrel(10)	!=1 if line pos rel to line 1
	character*80 newtext(20)		!extra text
c      character*1 ch, fndcur   !fndcur is now integer
      character kpchar*11, ch*1,cnum1*11		!must have *11 for INTCONV
	integer Lb(30)
	character*78 text(18),strings(10),str2(10),titles,titles1
	logical discprt,wmeta,wbmp
	common/dp/discprt
	common/hlp/help		!for QDIALOG
	common/DMENU/ifonb,csize,ifont2,nboxlast,nblast
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi
	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
	COMMON/TPOS/rx,ry,rxbox,rybox,ijus,angle,idraw,
     & ifnt,size,rlth,thbig,narrow,xb,yb,xe,ye,
     & nline,xlb,xle,ylb,yle,iltype,ntext,newtext,
     & cnumx,cnumy,cexpx,cexpy,numbx,numby,	!need also PARVAL?
     & nhline,yhline,xhlb,xhle,ilhtype,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel
	COMMON/cols/icol,mono
	common/logval/logx,logy,sqrty
c
c        INCLUDE 'c:\hgraph\CONSTS.FOR'
c*      INCLUDE 'c:\hgraph\HGRGLB.DEC'
c*      INCLUDE 'c:\f77l3\SPINATT.HDR		!modif version of SPINATTR.HDR
c values below are from c:\hgraph\CONSTS.FOR
c values below are from c:\hgraph\CONSTS.FOR
      INTEGER LEFT, CENTER, RIGHT
      DATA LEFT, CENTER, RIGHT /-1, 0, 1 /     !new definition
c
	pon()=slock()
	debug()=caplock()
c	rlth(i)=ifixr(thick*float(irlth(i)))	!sets line thickness
	EQUAL(x,y)=abs(x-y).lt.0.00001
c
3	format(i8)
4	format(g13.6)
c Initialisations:
c (1) Initialisation whether AUTPLT or not
c	if(VIDEOTYP().ne.18) call INIPLT(idev,.false.,1.0)
	scalfac=1.0
c	if(VIDEOTYP().ne.18) call INIPLT(0,.false.,scalfac)
c
c	call GINO
c	if(VIDEOTYP().ne.18) call VGA
      call VGA
	call errswi(-1)
	call brkswi(1)
	call chaswi(1)
	call gsetcols(0)
	call grfmod (1)
	call mode(18)
	plot=.false.
	wmeta=.false.
	wbmp=.false.
	croset=.false.		!cross position has not been reset
	iret=0		!for return after help screen
	landplot=.true.
	draft=.false.
	zoomed=.false.
	cfacsml=0.6		!character size factor for small plots (0.6*2.5=1.5)
	thfacsml=0.6	!line thickness factor for small plots
	ifsav=ifont		!save input value (if there is one)
	itlen=200		!tic length
c	ixlo=ixlo1		!named ixlo1 in call so ixlo can be in common/hgv/
c	ixhi=ixhi1		!ditto
c	iylo=iylo1		!named iylo1 in call so iylo can be in common/hgv/
c	iyhi=iyhi1		!ditto
	xtsav=xtic
	ytsav=ytic
	call papenq(xp,yp,ipap)
	vxlo=0	! for VIEWPORT
	vxhi=xp
	vylo=0
	vyhi=yp
	xlo=xlo1		!named ixlo1 in call so ixlo can be in common/hgv/
	xhi=xhi1		!ditto
	ylo=ylo1 !named iylo1 in call so iylo can be in common/hgv/
	yhi=yhi1		!ditto
c	ifont2=ifont	!copy for common/dmenu/
	ifont2=0 !copy for common/dmenu/
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
	call DEFDIALOG(1,1,2,4,60,icb2)	!define dialog box #1 at top
c	call DEFDIALOG(1,22,2,4,60,icb2)	!dialog box at bottom
	call DEFDIALOG(2,1,65,4,14,icb2)	!define dialog box #2
	if(.not.autplt) then	!for autplot, mono set on entry
c	   mono=.true.		!monochrome display (except DCMENU)
	   mono=.false.		!colour display with default colours
	endif
	if(inumx.eq.0) inumx=-1		!but should be defined in call
	if(inumy.eq.0) inumy=-1		!but should be defined in call
c Default colours set if mono=false on entry and not AUTPLOT (for autplot
c use the queued colours if mono=false)
c	call SETCOLS(mono,icol,autplt,plot)
c	call setcls(mono,icol,autplt,plotcols,isetcol)
	plotcols=.true.
	call setcls(mono,icol,.false.,plotcols,0)
	ifonb=0     !font for boxes
	csizb=1.7   !character size for boxes
	cbig=3.0

	i=NBLANK1(titlex)               !ensure it ends with char(0)
	j=NBLANK1(titley)               !ensure it ends with char(0)
c
c (2) Initialisation ONLY when AUTPLT=true
	if(autplt) then
	   csize=cbig
	   thick=thbig
	   size0=csize
	   if(ntext.gt.0) then
		ntx3=ntext+11
	      do i=ntx3,30
		   size(i)=size(ntext)
		enddo
	   else
		do i=11,30
		   size(i)=1.3*csize
		enddo
	   endif
	   do i=1,30
		if(size(10).eq.0.0) size(10)=1.3*csize
		if(size(6).eq.0.0) size(6)=0.7*csize
		if(size(7).eq.0.0) size(7)=csize
		if(size(8).eq.0.0) size(8)=csize
		if(size(9).eq.0.0) size(9)=csize
		wpots(i)=size(i)*4.
	   enddo
	   if(ifitype.ne.2) then
	      fitted=ncurvc.gt.0
	   else if(ifitype.eq.2) then
		fitted=ncurvd.gt.1
	   endif
	endif
	just=0
	linetype=0
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
	if(cbig.lt.2.) cbig=3.5
c	itit=0		!no title yet
	ifsav=ifont		!save input value (if there is one)
	thbig=1.0		!line thickness factor for whole page
	csize=cbig
	thick=thbig
	thfacsml=0.6		!line thickness factor for small plots
c
	do 81 i=1,100
	  angle(i)=0.
	  idraw(i)=-2		!until defined
c	  rx(i)=0.0		!NB world coord may be neg so cant tell if defined yet
81	continue
	do 83 i=1,10
83	iltype(i)=0		!continuous line for extra lines
c
c	angle(8)=0			!0 set above
	ijus(8)=center		!for x-axis label
	angle(9)=90.		!for y-axis label
	ijus(9)=center
	do 8 i=1,30
	ifnt(i)=ifont
	size(i)=1.3
8	continue
	do i=1,30
	   ifnt(i)=ifont
	   size(i)=1.3		!for newtext (simplifies QDIALOG call!)
	enddo
	size(6)=0.7		!csize for param values=0.7*csize
      csize=3.0
	do i=1,30
		size(i)=size(i)*csize
		wpots(i)=size(i)*4.
	enddo
	if((.not.fitted).or.ifitype.eq.0) idraw(6)=0	!no param values
c
c Default line thickness
	rlt=0.20		!default thickness, unless reset
c	if(autplt.and.draft) ilt=0	!'draft' set below
	do i=1,100
	  rlth(i)=0.2
	enddo
c	irlth(21)=25	!axes (and axis labels at present!)
 	rlth(22)=0.3     !frame
	rlth(26)=0.3	!param value text
c
	do i=1,30
		size(i)=size(i)*csize
	enddo
 	RLTH(23)=0.5		!labels
 	RLTH(24)=0.5		!numbers
 	RLTH(25)=0.5		!title
 	RLTH(26)=0.25	!param value text
	do i=31,50
		rlth(i)=0.5
	enddo
	if(isval.eq.1) then
	   narrow=1
	   isval=-1		!to indicate that position not yet defined
	endif
c
89	continue	!jump here if autplt
	if(idraw(6).ne.0.and.ifitype.ne.0) call PARTEXT(parval,ifitype,
     & theta,ncomp,ifnt(6),size(6),20)  !define text string for param values
c END OF INITIALISATIONS
c
c	call SETFONT1(ifont)
	call setfnt(ifont)
c	call SETCSIZE(csize,isize)    !sets isize too
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
	call lincols(15,idev)		!bright white
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
	xminsav=xmin
	yminsav=ymin
	xmaxsav=xmax
	ymaxsav=ymax
	xtsav=xtic
	ytsav=ytic
	xcsav=xcross
	ycsav=ycross

108	continue

	xmin1z=xmin1
	ymin1z=ymin1
	xmax1z=xmax1
	ymax1z=ymax1
c
	if(xlo.ge.0) goto 1082
c     Make iyhi,ixhi lower to leave room for dialog box (also for narrow
c     vertical box ar RHS of screen?)
c     -proportions are close to golden section = 1:1.618
c NB to have dialog box at bottom need to redfine these (iylo=1866 ?)
c====================================================================
	is=1
1083	if(landscap) then
	   xlo=0.2*xp		! set
c	   if(calbarY) xlo=0.10*xp	!make wider
	   xhi=0.8*xp		! display
	   ylo=0.2*yp! location
	   yhi=0.75*yp		! screen
	else
	   xlo=0.22*xp         !portrait
	   xhi=0.78*xp	!axis length x=3500 y=4200
	   ylo=0.2*yp
	   yhi=0.85*yp
	   if(ycross.gt.ymin) then
	      ylo=0.15*yp 	!make lower as no need for room for numbers/title
	      yhi=0.85*yp
c		xlo=0.3*xp	!make bigger to match
c		xhi=0.7*xp
	   endif
	endif

1082	continue
	xticsav=xtic
	yticsav=ytic
	xlo2=xlo		!save screen GRAPHBOUNDARY
	xhi2=xhi
	ylo2=ylo
	yhi2=yhi
c=== si ptr plot now!


c*********************************************************************
1081	continue
c     Now the graphics. Return to 306 to plot with new line type etc
	call axiset
	call GRAPHBOUNDARY(xlo,xhi,ylo,yhi)
	if(iscal.ne.0.and.ilog.ne.0) goto 133	!scale for logs first

c*********************************************************************
306	continue
	redrawn=.true.	!graph will have xtitle etc drawn in LAXES2
	if(.not.croset) then
	   xcross=xmin		!crossing point for axes
	   ycross=ymin
	endif
c     Scale now
	if(.not.sqrty) call gSCALE(xmin,xmax,ymin,ymax,is)
	if(sqrty) call gSCALE(xmin,xmax,sqrt(ymin),sqrt(ymax),is)
	call spagra(vxlo,vylo,wxmin,wymin)
	call spagra(vxhi,vyhi,wxmax,wymax)
	call linwid(0.2)

c=======================================================================
104	continue			!return here to do plot
	call broken(0)
	call pixpos(0,53,xsc,ysc)
	ymenu=yp-ysc
c     Fill background colour
	if(.not.mono) then
	    call FILLWIN(0,55,639,387,icol(71))		!graph area
	    call FILLWIN(0,388,639,479,1)			!dialog area
	endif
	if(.not.plot) then
	   call HLINE(0,639,54,15)		!line to mark bottom of data area
	   call HLINE(0,639,389,15)	!line to mark top of data area
	endif
	call OPENDIALOG(1,icf,.true.)		!draw dialog box #1
	call OPENDIALOG(2,icf2,.true.)		!draw dialog box #2
	call WDIALOG(2,'F1=HELP',ict)
	call WDIALOG(2,'F2=HELP INDEX',ict)
	if(plot) then
	   call dhline(1,639,1,13,22)
	   call dhline(1,639,479,13,22)
	   call dvline(1 ,1 ,479,13,22)
	   call dvline(639 ,1 ,479 ,13,22)
	   call dhline(ixlo,ixhi,iylo,13,44)
	   call dhline(ixlo,ixhi,iyhi,13,44)
	   call dvline(ixlo,iylo,iyhi,13,44)
	   call dvline(ixhi,iylo,iyhi,13,44)
	   if(mpos.ge.1.and.mpos.le.4) then
		size(7)=0.8*size(7)
		size(8)=0.8*size(8)
		size(9)=0.8*size(9)
		size(10)=0.8*size(10)
	   endif
	endif
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
	if(autplt.and.(iver.ne.1100.and.just.eq.0)) then
		do k=6,10
		  idraw(k)=-2
		enddo
		do k=31,80
		  idraw(k)=-2
		enddo
		call convstring(title1)
		call convstring(titlex)
		call convstring(titley)
		if(ntext.gt.0) then
		   do i=1,ntext
		      call convstring(newtext(i))
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
     & thick,rlth,inumx,inumy,xwbase,ilog,doframe,mono,icol,xc,yc,
     & rx,ry,angle,ijus,idraw,rxbox,rybox,
     & numbx,numby,cnumx,cnumy,cexpx,cexpy)
	call lincols(15,idev)		!bright white
	just=1
c
136	continue
c	if(debug()) pause '11'
c=========================================================================
c DO TITLE IF ANY
	if(idraw(10).eq.0.or.itit.eq.0) goto 1361		!no title
	call setfnt(ifnt(10))
	call setsize(size(10))
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
	  rx(10)=0.5*(xmin+xmax)
	  ry(10)=ymax2+0.025*(ymax2-ymin2)
	  angle(10)=0
	  ijus(10)=center
c       define rxbox,rybox for posn of TITLE1
	  call graspa(rx(10),ry(10),xsc,ysc)
	  call TEXTBOX(xsc,ysc,title1(1:nl),angle(10),size(10),
     &     ijus(10),rxbox(1,10),rybox(1,10),0)
	endif
	if(idev.ge.5) then
	   call LINWID(2*rlth(25))
	else
	   call LINWID(rlth(25))
	endif
	if(.not.mono) call lincols(icol(25),idev)		!colour for title
	call graspa(rx(10),ry(10),xsc,ysc)
	call JUSTIFYSTRING(xsc,ysc,title1(1:nl),angle(10),size(10),
     & ijus(10))
c 	draw the box round title if necessary- could call TEXTBOX again, but
c	position already defined so quicker to call FRAME- but safer to use
c	former, in case graph size changed
	if(idraw(10).eq.-1) call TEXTBOX(xsc,ysc,title1(1:nl),
     &   angle(10),size(10),ijus(10),rxbox(1,10),rybox(1,10),1)
	   call setfnt(ifont)		!reset
c=======================================================================
c=========================================================================
c 				DRAW PARAMETER VALUES,
c=========================================================================

c if req. NB need to set box position etc only the first
c time here (when idraw(6)=-2 still). If PARAM values are altered (in position,
c size etc) the relevant parameters will be adjusted at time of alteration.
1361	continue
	call broken(0)
	if(idraw(6).eq.0) goto 309	!no parameters to draw
	if(.not.fitted.or.ifitype.eq.0) goto 309	!no parameters to draw
	call setfnt(ifnt(6))
	call SETSIZE(size(6))	!this defines ISIZE
	nl=nblank1(parval)
	if(idraw(6).eq.-2) then
	    idraw(6)=-1	!so drawn with box by default
	    rx(6)=xmax-0.025*(xmax-xmin)
	    ymax2=ymax
	    if(sqrty) ymax2=sqrt(ymax)
	    ry(6)=ymax2-0.042*(ymax2-ymin)
	    if(ifitype.eq.4) ry(6)=ymax2-0.5*(ymax2-ymin)  !lower (for jump logos)
	    ijus(6)=right
c 	    define rxbox,rybox for posn of PARVAL
	    call graspa(rx(6),ry(6),xsc,ysc)
	    call TEXTBOX(xsc,ysc,parval(1:nl),angle(6),size(6),
     &     ijus(6),rxbox(1,6),rybox(1,6),0)
	else
	    x=rx(6)	!pos as defined by cursors or FIXTEXT
	    y=ry(6)
	endif
	if(idev.ge.5) then
	   call LINWID(2*rlth(26))
	else
	   call LINWID(rlth(26))
	endif
	if(.not.mono) call lincols(icol(26),idev)		!colour for params
	call graspa(rx(6),ry(6),xsc,ysc)
	call JUSTIFYSTRING(xsc,ysc,parval(1:nl),angle(6),size(6),
     & ijus(6))
c 	draw the box round PARVAL if necessary- could call TEXTBOX again, but
c 	position already defined so quicker to call IFRAME1
	call graspa(rx(6),ry(6),xsc,ysc)
	if(idraw(6).eq.-1) call TEXTBOX(xsc,ysc,parval(1:nl),
     &   angle(6),size(6),ijus(6),rxbox(1,6),rybox(1,6),1)
	call setfnt(ifont)		!reset font

c====================================================================
309	continue		!end of par val
c====================================================================
c
c DRAW EXTRA TEXT IF ANY. Note that there is no default position for new
c text so its position must already be defined if ntext>0
	call broken(0)
	if(ntext.gt.0) then
	  do 223 i=1,ntext
	   j=i+10
	   if1=ifnt(j)
c	   cs1=size(j)*csize
	   if(idev.ge.5) then
	      call LINWID(2*rlth(i+30))
	   else
	      call LINWID(rlth(i+30))
	   endif
	   call setfnt(ifnt(j))
c	   call setsize(size(j))
	   if(.not.mono) call lincols(icol(i+30),idev)		!colour for text(i)
	   call graspa(rx(j),ry(j),xsc,ysc)
	   call JUSTIFYSTRING(xsc,ysc,newtext(i),angle(j),
     &    size(j),ijus(j))
	   if (idraw(j).eq.-1) then
		call TEXTBOX(xsc,ysc,newtext(i),
     &      angle(j),size(j),ijus(j),rxbox(1,j),rybox(1,j),1)
	   endif
c	   if(idraw(j).eq.-1) call FRAMER(rxbox(1,j),rybox(1,j))
223	   continue
	  call setfnt(ifont)	!reset current values
	endif
c
c==========================================================================
c                        DRAW ARROWS IF ANY
c==========================================================================
	ikey=0
	call draw_arrow(ikey,narrow,xe,xb,ye,yb,ixfix,iyfix,
     &xv,yv,csize,idev,xabs1,yabs1,xabs2,yabs2,rlth,zoomed,iver)

c ==============================================================================
c                       DRAW LINES IF ANY
c ==============================================================================
	ikey=0
	call broken(0)
	call draw_lines(ikey,nline,xlb,xle,ylb,yle,iltype,
     & nhline,yhline,xhlb,xhle,ilhtype,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel,
     & csize,idev,xabs1,yabs1,xabs2,yabs2,rlth,
     & xmin,xmax,ymin,ymax,zoomed,iver)
	call broken(0)

c
c ==============================================================================
c PLOT THE HISTOGRAM
c ==============================================================================
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
c	      call LOADVTB('2'//'W'//char(rlth(j)))
	else
	      call LINWID(rlth(j))
	endif
	if(.not.mono) call lincols(icol(j),idev)		!colour for jth histo bars
	if(ndat(j).le.0) goto 303	!calc curve only
c Set baseline value to ensure it is on scale
	ybase=ymin
	if(logy) ybase=10.**ymin  !need non-log value (log taken in logvl if req)
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
	call logvl(xlow,xv,xmin,xmax,logx,off,bad,.false.) !ixlow on scale?
c=	if(off.or.bad) goto 20		!no lo bin if not
 	call logvl(flo,yv,ymin,ymax,logy,off,bad,sqrty)
c if Y off scale it is plotted as Ymax (set in logvl)
c=	if(off.or.bad) goto 20		!omit LO bin if Y off scale
	if(.not.mono) call lincols(icol(10),idev)		!colour end bins
	linetype=lt2			!line type for FLO bin
c========
	 call broken(linetype)
	if(xlow.eq.xmin) then		!omit 1st vert
	   call gramov(xv,yv)
	else
	   call gramov(xv,0.)
	   call gralin(xv,yv)
	endif
	xvert(1)=xv
	yvert(1)=0.
	xvert(2)=xv
	yvert(2)=yv
	call logvl(xval(1,j),xv,xmin,xmax,logx,off,bad,.false.)
	call gralin(xv,yv)		!draw top
	xvert(3)=xv
	yvert(3)=yv
c   go back to baseline
	call logvl(ybase,yv,ymin,ymax,logy,off,bad,sqrty)
c=================???????????
	if(yval(1,j).gt.flo) then
		linetype=ijoin(j)	!contin line
		call broken(linetype)
	endif
	call gralin(xv,yv)		!draw vert
	xvert(4)=xv
	yvert(4)=yv
	do i=1,4
	   call graspa(xvert(i),yvert(i),xsvert(i),ysvert(i))
	enddo
	call movto2(xsvert(1),ysvert(1))
	call POFTO2(0,0,0,xsvert,ysvert,4)

c	call FILL1(xvert,yvert,4,12,0,0,0)
c###	call graFI2(xvert,yvert,4,12,0,0,0)
c========
c	if(xlow.eq.xmin) goto 34		!omit 1st vert
c	call gramov(xv,0.,0,0)
c	call gramov(xv,yv,1,linetype)
c	goto 35
c34	call gramov(xv,yv,0,0)
c35	call logvl(xval(1,j),xv,xmin,xmax,logx,off,bad,.false.)
c	call MOVETO(xv,yv,1,linetype)		!draw top
cc   go back to baseline
c	call logvl(ybase,yv,ymin,ymax,logy,off,bad,sqrty)
cc	if(yval(1,j).gt.flo) linetype=ijoin(j)	!contin line
c	call MOVETO(xv,yv,1,linetype)		!draw vert
cc	goto 25
c
c  next regular histogram-
c  first do 1st vertical of regular histo
20	continue
c    start from baseline
	if(idev.ge.5) then
c	      call LOADVTB('2'//'W'//char(rlth(j)))
	else
	      call LINWID(rlth(j))
	endif
	if(.not.mono) call lincols(icol(j),idev)		!colour for jth histo bars
	linetype=ijoin(j)
	k=0
38	k=k+1
	call logvl(xval(k,j),xv,xmin,xmax,logx,off,bad,.false.)
c	if(bad) print 381,k,j,xval(k,j)
c381	format('&Bad value is Xval(',i3,',',i3,') = ',g13.6)
	if(off.or.bad) goto 38	!look for 1st good X if X off scale
	call logvl(ybase,yv,ymin,ymax,logy,off,bad,sqrty)
c	if(bad) print 382,ybase,ymin,k,j
c382	format('&Bad value is ybase = ',g13.6,' ymin,k,j=',g13.6,2i4)
c if Y off scale it is plotted as Ymax (set in logvl)
	call gramov(xv,yv)
	call logvl(yval(k,j),yv,ymin,ymax,logy,off,bad,sqrty)
c	if(bad) print 383,k,j,yval(k,j)
c383	format('&Bad value is Yval(',i3,',',i3,') = ',g13.6)
c if Y off scale it is plotted as Ymax (set in logvl)
c	if(off.or.bad) goto 25
	call broken(linetype)
	call gralin(xv,yv)	!draw 1st vert
25	continue
c  Now loop to draw top and 2nd vert. Starts at point x1,y1
	do 215 i=k,ndat(j)		!ndat=nbin; x=x(1) to x(nbin+1)
	  call logvl(xval(i+1,j),xv,xmin,xmax,logx,off,bad,.false.)
c	  if(bad) print 381,i+1,j,xval(i+1,j)
c already checked that x > xmin. Once X > Xmax can leave loop
c altogther as all subsequent X will be off scale too
c	  if(off.or.bad) goto 215     !skip unless x is on scale
	  if(xv.gt.xmax) goto 39
	  call broken(linetype)
	  call gralin(xv,yv)		!draw top
	  ynext=ybase   !for last bin- xval(ndat+1) is defined but not yval
 	  if(i.lt.ndat(j)) ynext=yval(i+1,j)
	  call logvl(ynext,yv,ymin,ymax,logy,off,bad,sqrty)
c if Y off scale it is plotted as Ymin or Ymax (set in logvl)
c	  if(off.or.bad) goto 215     !skip unless x is on scale
	  call gralin(xv,yv)		!draw vertical
215	continue
c
c  lastly HI bin. Could be improved later to plot (with recalc FHI)
c even after rescaling to reduce Xmax (no data to recalc FHI if Xmax
c is increased by rescaling)
c If regular histo went above xmax, OFF will be true here
39	continue
c==	fhi=Yval(ndat(j)+2,j)
	fhi=Yval(ndat(j)+1,j)
c=	if(logx.or.logy.or.fhi.lt.0.0001) goto 36	!no hi bin
	if(fhi.lt.0.0001) goto 36	!no hi bin
c	dxhi=xval(ndat+2,j)-xval(ndat+1,j)	!define in calling prog
c	xhigh=xval(ndat+1)+dxhi
	xhigh=xval(ndat(j)+2,j)
	call logvl(xhigh,xv1,xmin,xmax,logx,off,bad,.false.)
c=	if(off.or.bad) goto 36		!no hi bin if Xhigh off scale
	call logvl(xval(ndat(j)+1,j),xv,xmin,xmax,logx,off,bad,.false.)
	call logvl(FHI,yv,ymin,ymax,logy,off,bad,sqrty)
c  plot as Ymax if FHI > Ymax
c=	if(off.or.bad) goto 36
	linetype=lt2
	if(.not.mono) call lincols(icol(10),idev)		!colour end bins
	call broken(linetype)
	call gralin(xv,yv)		!1st vert
	call gralin(xv1,yv)	!hor
	call logvl(ybase,yv2,ymin,ymax,logy,off,bad,sqrty)
	call gralin(xv1,yv2)	!last vert
	xvert(1)=xv
	yvert(1)=0.
	xvert(2)=xv
	yvert(2)=yv
	xvert(3)=xv1
	yvert(3)=yv
	xvert(4)=xv1
	yvert(4)=yv2
	do i=1,4
	   call graspa(xvert(i),yvert(i),xsvert(i),ysvert(i))
	enddo
	call movto2(xsvert(1),ysvert(1))
	call POFTO2(0,0,0,xsvert,ysvert,4)
c	call FILL1(xvert,yvert,4,12,0,0,0)
	linetype=0
	if(.not.mono) call lincols(icol(j),idev)		!colour for jth histo bars
36	continue
c
50	continue
c Histograms finished
c
	if(ncurvc.gt.0) goto 303	!do calc curve before rescale option
	if(meta.and.ncurvc.le.0) goto 5555
	if(wmeta.and.ncurvc.le.0) goto 5555
	if(wbmp.and.ncurvc.le.0) goto 5555
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
	goto 150


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
c	      call LOADVTB('2'//'W'//char(rlth(j+10)))
	else
	      call LINWID(rlth(j+10))
	endif
	if(.not.mono) call lincols(icol(j+10),idev)		!colour for jth calc curve
	if(ij.gt.0) goto 310		!dashed calc curve
c
c Problem with calc curve not drawn right up to edge of graph -need
c Now have many more calc points so MOVE rather than DRAW if next point
c is out of range (so won't draw right to edge).
c	Jflag=0
	do 220 k=1,ncal(j)
	xv=xcal(k,j)
	yv=ycal(k,j)
	if(logx.and.xv.ge.1.e-32) xv=alog10(xv)
	if(logy.and.yv.ge.1.e-32) yv=alog10(yv)
	if(sqrty.and.yv.ge.0.) yv=sqrt(yv)
	if(k.eq.1) then
	   call gramov(xv,yv)	!1st point in range
	else
	   call broken(linetype)
	   call gralin(xv,yv)
	endif
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
	sfac=(yhi-ylo)/(xhi-xlo)		!O.K.?
	xr=0.01*(xmax-xmin)
	yr=0.01*(ymax-ymin)
	if(sqrty) yr=0.01*(sqrt(ymax)-sqrt(ymin))
	k=1
 	zleft=zseq(k)
	down=.true.		!start with 'pen down'
c
      x0=1.e-36               !smallest value for logs
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
	call gramov(xv,yv)	!move to 1st point in range
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
	goto 317
c
316	continue
c  next bit done when amount of line remaining to be drawn does
c not extend as far as current point- keep drawing segments (with
c slope as between last point and current one) until it does.
	call broken(linetype)
	if(vert) then
	   xv1=xvlast
	   yv1=yvlast + zleft*yr/sfac
	else
	   xv1=xvlast + zleft*xr/sqrt(1.+b*b)
c	   yv1=yvlast + b*(xv1-xvlast)*yr/xr
	   yv1=yvlast + b*(xv1-xvlast)*yr/(sfac*xr)
	endif
	if(.not.down) call gramov(xv1,yv1)
	if(down) call gramov(xv1,yv1)
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
	if(meta) goto 5555
	if(wmeta.or.wbmp) goto 5558
	if(.not.plot) goto 15		!another chance to plot it
	if(idev.ge.5.and.PLOT) goto 1813	!do vtrans
	if(plot) goto 1812		!return with plot=true?
c
c=======================================================================
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
	text(15)='.:MORE OPTIONS'
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
	call broken(0)

152	call CKEY(ch,ikey)
	if(ikey.lt.-2.or.ikey.gt.15) goto 151
	if(ikey.eq.-1) then	!F1 key
	   call VHELP(1)   !?????????????????????????///
	   goto 152	!help
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-2)
	   goto 152		!another menu choice
	endif
c
	goto(11,140,40,13,221,991,155,301,999,306,401,401,152,152,154)ikey
c
c MORE OPTIONS
154	continue
	nbox=12	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	Lb(11)=0
	Lb(12)=0
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
	text(8)='8: SET DEFAULT TEXT SIZE'
	text(9)='9: SET DEFAULT FONT  '
	text(10)='0: REDRAW         '
	call DCMENU(nbox,4,Lb,text,icol1,icol2)
	call CKEY(ch,ikey)
	if(ikey.lt.-1.or.ikey.gt.nbox) goto 154
	if(ikey.eq.10) goto 150	!main menu
	if(ikey.eq.-16) goto 150	!0 or ESC to main menu also
	if(ikey.eq.1) then
	   mono=.not.mono
	   if(mono) call lincols(15,idev)		!bright white
	else if(ikey.eq.2) then
c set default colours even if AUTPLOT for now
	  mono=.false.
	  call setcls(mono,icol,.false.,.false.,0)
	else if(ikey.eq.3) then
	  mono=.false.
	  call setcls(mono,icol,.false.,.true.,0)
	else if(ikey.ge.4.and.ikey.le.7) then
	   if(ikey.eq.4) imode=-2		!for histo colours
	   if(ikey.eq.5) imode=11		!increase line thickness
	   if(ikey.eq.6) imode=12		!decrease line thickness
	   if(ikey.eq.7) imode=-1	      !set histo line thickness
	   call SETATT(narrow,nline,ntext,isdev,
     &   rlth,icol,icol0,icol1,icol2,ncurvd,icurvd,ncurvc,icurvc,
     &   ndimd,ndimc,ifitype,imode)
	else if(ikey.eq.8) then
	   csfac=1.0
 	   call DEFOLTr(csfac,defolt)
	   call QDIALOG(1,
     &	'Size factor for all text',
     &	defolt,ict,cans)
	   call GETINPr(cans,csfac)
	   do i=1,30
		size(i)=size(i)*csfac
		wpots(i)=size(i)*4.
	   enddo
	else if(ikey.eq.9) then
 	   call DEFOLTi(ifont,defolt)
	   call QDIALOG(1,
     &	'Font number for all text',
     &	defolt,ict,cans)
	   call GETINPi(cans,ifont)
	   do i=1,30
		ifnt(i)=ifont
	   enddo
	else
	   goto 154		!try again
	endif
	goto 306	!redraw
c
c
C READ IN TITLE
c With plotter on, MOVE goes only to plotter. To get 'Enter title..'
c at right position on screen need sep MOVE with plotter off!
c Get label but do not draw until decided if plot is to be queued
991	continue
c now use HGSTRING here
c With plotter on, MOVE goes only to plotter. To get 'Enter title..'
c at right position on screen need sep MOVE with plotter off!
c Get label but do not draw until decided if plot is to be queued
	call DCMENU(-6,4,Lb,text,0,0)	!delete box 4 only
	Lb(6)=-1
	call DCMENU(-6,4,Lb,text,icol1,icol2)	!draw box 4 only (italic)
	blank=itit.eq.0
	call setfnt(ifnt(10))
	call SETSIZE(size(10))	!this defines ISIZE
	nl=NBLANK1(title1)
	blank=itit.eq.0
	if(nl.eq.1.and.title1(1:1).eq.char(0)) blank=.true.
	ymin2=ymin
	ymax2=ymax
	if(sqrty) ymin2=sqrt(ymin)
	if(sqrty) ymax2=sqrt(ymax)
 	if(idraw(10).eq.-2) then
	   idraw(10)=1		!title position now defined
	   x=(0.5*(xmin+xmax))
	   y=(ymax2+0.025*(ymax2-ymin2))
	   rx(10)=x
	   ry(10)=y
	   ANGLE(10)=0.
	   ijus(10)=center
c        define rxbox,rybox for posn of TITLE1
	   call graspa(rx(10),ry(10),xsc,ysc)
	   call TEXTBOX(xsc,ysc,title1(1:nl),ANGLE(10),size(10),
     &   ijus(10),rxbox(1,10),rybox(1,10),0)
	else
	   x=rx(10)
	   y=ry(10)
	endif
c	if(.not.mono)
      call lincols(icol(25),idev)		!colour for title
	if(itit.eq.1) then	!delete existing title first
	   call lincols(icol(71),idev)		!colour for title
	   call graspa(rx(10),ry(10),xsc,ysc)
	   call JUSTIFYSTRING(xsc,ysc,title1,ANGLE(10),
     &	 size(10),ijus(10))
	endif
	call graspa(rx(10),ry(10),xsc,ysc)
	call GSTRING(xsc,ysc,title1,ifnt(10),ANGLE(10),size(10),
     &   ijus(10),icol(25),icol(71),64,blank,nrealt)
	nlg=nblank1(title1)
	if(idraw(10).eq.0.and.nlg.gt.1) idraw(10)=1
	itit=1		!for plotq
	call flush(7)
	goto 152
c
c Section to read in new axis labels (and define boxes for them)
401	continue
	call DCMENU(-ikey,4,Lb,text,0,0)	!delete box #ikey only
	Lb(ikey)=-1
	call DCMENU(-ikey,4,Lb,text,icol1,icol2)	!draw box #ikey only (italic)
	if(ikey.eq.11.or.ikey.eq.12) then	!x,y axis labels
	   call WDIALOG(1,'Now write text: hit F1 anytime for HELP  ',ict)
	   if(ikey.eq.11) then
		n=8		!index for rx() etc
		if(redrawn) then
		   oldtext=xtitle
		else
		   oldtext=titlex
		endif
	   else if(ikey.eq.12) then
		n=9		!index for rx() etc
		if(redrawn) then
		   oldtext=ytitle
		else
		   oldtext=titley
		endif
	   endif
	   x=(rx(n))
	   y=(ry(n))
	   call lincols(icol(71),idev)	!delete existing title
	   nlx=NBLANK1(oldtext)
	   call setfnt(ifnt(n))
	   call SETSIZE(size(n))	!this defines ISIZE
	   if(nlx.gt.0) then
	      call graspa(rx(n),ry(n),xsc,ysc)
	      call JUSTIFYSTRING(xsc,ysc,oldtext(1:nlx),angle(n),
     &		SIZE(n),ijus(n))
	   endif
	   redrawn=.false.	!until hit REDRAW
	   if(ikey.eq.11) then
	      call graspa(rx(n),ry(n),xsc,ysc)
	      call GSTRING(xsc,ysc,titlex,ifnt(n),angle(n),size(n),
     &	 ijus(n),icol(23),icol(71),40,.true.,nrealx)	!40=size of titlex
	   else if(ikey.eq.12) then
	      call graspa(rx(n),ry(n),xsc,ysc)
	      call GSTRING(xsc,ysc,titley,ifnt(n),angle(n),size(n),
     &	 ijus(n),icol(23),icol(71),40,.true.,nrealy)
	   endif
	   idraw(n)=-2	!so new label drawn at default posn in LAXES
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
c		xminsav=xmin
c		xmaxsav=xmax
c		yminsav=ymin
c		ymaxsav=ymax
c		xcsav=xcross
c		ycsav=ycross
c		xtsav=xtic
c		ytsav=ytic
	   endif
	   zoomed=.true.
147	   continue
	   call WDIALOG(1,
     &      'Mark BOTTOM LEFT corner of new display with cursors',ict)
	   call CURPOS(xlo,ylo)
	   call CURSOR(ikey,x1,y1)
	   ch=char(ikey)
         call spagra(x1,y1,xmin,ymin)
	   XMIN1z=XMIN
	   ymin1z=ymin
	   if(logx) xmin1z=10**xmin
	   if(logy) ymin1z=10**ymin
	   xmin2=xmin
	   ymin2=ymin
	   call jSYMBOL(x1,y1,4,csize,12)   !mark bottom left with red diamond
	   call WDIALOG(1,
     &      'Mark TOP RIGHT corner of new display with cursors',ict)
	   x1=x1+200
	   y1=y1+150
	   if(x1.gt.xhi) x1=xhi
	   if(y1.gt.yhi) y1=yhi
	   call CURPOS(x1,y1)
	   call CURSOR(ikey,x1,y1)
	   ch=char(ikey)
         call spagra(x1,y1,xmax,ymax)
	   XMax1z=XMax
	   ymax1z=ymax
	   if(logx) xmax1z=10**(xmax)
	   if(logy) ymax1z=10**(ymax)
	   xmax2=xmax
	   ymax2=ymax
	   call jSYMBOL(x1,y1,4,csize,12)   !mark bottom left with red diamond
	   if(xmin.gt.xmax.or.ymin.gt.ymax) then
		call BELL(2)
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
	if (idraw(6).ne.0) idraw(6)=-2       !Parameter values drawn at default posn
	if (idraw(10).ne.0) idraw(10)=-2	!so title, if present, drawn at default posn
	if (idraw(8).ne.0) idraw(8)=-2       !X axis label drawn at default posn
	if (idraw(9).ne.0) idraw(9)=-2       !Y axis label drawn at default posn
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
	   call VHELP(3)
	   goto 112		!another menu choice
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
	   call VHELP(5)
	   goto 405		!another menu choice
	endif
	if(ikey.lt.1.or.ikey.gt.4) goto 405
	goto(406,406,406,406) ikey
c MARK NEW SHAPE WITH CURSORS

406	if(ikey.eq.1) then
	   call WDIALOG(1,
     &    'Mark BOTTOM LEFT corner of new graph with cursors',ict)
	   call CURPOS(xlo,ylo)
	   call CURSOR(ikey,xlo,ylo)
         ch=char(ikey)		!note new usage
         x1=xlo
         y1=ylo
	   call SYMBOL(x1,y1,4,csize,12)   !mark bottom left corner with diamond
	   call WDIALOG(1,
     &      'Mark TOP RIGHT corner of new graph with cursors',ict)
	   call CURPOS(xhi,yhi)
         call cursor(ikey,xhi,yhi) !note new usage
         ch=char(ikey)		!note new usage
         x1=xhi
         y1=yhi
	   call SYMBOL(x1,y1,4,csize,12)   !mark bottom left corner with diamond
	else if(ikey.eq.2) then		!define shape by typing in numbers
c	   rxlo=xlo/100.	!as percent
c	   rxhi=xhi/100.	!as percent
c         rylo=ylo/70.	!as percent
c	   ryhi=yhi/70.	!as percent
c	   call DEFOLT2r(rxlo,rxhi,defolt)
c	   call QDIALOG(1,'Low, high graph boundaries for X (% of page)'
c     &	,defolt,ict,cans)
c	   call GETINP2r(cans,rxlo,rxhi)
c	   call DEFOLT2r(rylo,ryhi,defolt)
c	   call QDIALOG(1,'Low, high graph boundaries for Y (% of page)'
c     &	,defolt,ict,cans)
c	   call GETINP2r(cans,rylo,ryhi)
c	   xlo=(100.*rxlo)
c	   xhi=(100.*rxhi)
c	   ylo=(70.*rylo)
c	   yhi=(70.*ryhi)
 	   rxlo=100.*xlo/xp	!as percent
 	   rxhi=100.*xhi/xp	!as percent
         rylo=100.*ylo/yp	!as percent
 	   ryhi=100.*yhi/yp	!as percent
	   call WDIALOG(1,'Low, high graph boundaries for X',ict)
	   call DEFOLT2r(rxlo,rxhi,defolt)
	   call QDIALOG(1,'% of page : ',defolt,ict,cans)
	   call GETINP2r(cans,rxlo,rxhi)
	   call WDIALOG(1,'Low, high graph boundaries for Y',ict)
	   call DEFOLT2r(rylo,ryhi,defolt)
	   call QDIALOG(1,'% of page : ',defolt,ict,cans)
	   call GETINP2r(cans,rylo,ryhi)
	   xlo=(rxlo/100.)*xp
	   xhi=(rxhi/100.)*xp
	   ylo=(rylo/100.)*yp
	   yhi=(ryhi/100.)*yp
	else if(ikey.eq.3) then		!restore default shape
	   xtic=xtsav
	   ytic=ytsav
	   xlo=xlo2		!restore screen GRAPHBOUNDARY
	   xhi=xhi2
	   ylo=ylo2
	   yhi=yhi2
	   itx=1
	   ity=1
	   xcross=xmin
	   ycross=ymin
	   landscap=.true.
	   doframe=.true.
	else if(ikey.eq.4) then		!square shape
	   xhi=xlo + (yhi-ylo)	!reduce ixhi as nec
	endif

	call GRAPHBOUNDARY(xlo,xhi,ylo,yhi)
c After change of shape by call to graphboundary must redraw ALL text
c at the same world coordinates in rx,ry. ie do not want any of the idraw=-2
c (all should be -1,0,1). But boxes may be in wrong positions
c or distorted,so all boxes redefined now, from the current rx,ry.
c Do this for newtext,parval(i=6),axis labels i=8,9), title (i=10)
c and for all numbers (i=31,..,31+numbx; i=56,...,56+numby)
c NB must call SCALE before doing this
	if(.not.sqrty) call gSCALE(xmin,xmax,ymin,ymax,is)
	if(sqrty) call gSCALE(xmin,xmax,sqrt(ymin),sqrt(ymax),is)
	call spagra(vxlo,vylo,wxmin,wymin)
	call spagra(vxhi,vyhi,wxmax,wymax)

c is following reset of idraw() needed?
c=========================ioana
	if (idraw(6).ne.0) idraw(6)=-2       !Parameter values drawn at default posn
	if (idraw(10).ne.0) idraw(10)=-2	!so title, if present, drawn at default posn
	if (idraw(8).ne.0) idraw(8)=-2       !X axis label drawn at default posn
	if (idraw(9).ne.0) idraw(9)=-2       !Y axis label drawn at default posn
	if (idraw(1).ne.0) idraw(1)=-2		!calibration bars drawn at default position and length
	do i=31,80
	  idraw(i)=-2		!all numbers at default posn
	enddo
	do 33 i=1,80	!i=index in angle,...,rybox arrays
c skip non-existent text
	if(idraw(i).eq.-2) goto 33
	if(idraw(i).eq.0) goto 33	!necessary?
c (so all can be scaled if latter altered): size(i), i=1-5 for newtext;
c size(6) for param values; size(7) spare; size(8)=axis labels;
c size(9)=axis numbers; size(10)=title
c MODIF: 01/25/90 09:55am size(7)=axis numbers (same for both axes);
c size(8)=x axis label; size(9)=y axis label;
c and similarly fonts defined by ifnt(1) to ifnt(10)
	if(i.ge.11.and.i.le.30) j=i	!size/font for newtext
	if(i.eq.6) j=i	!size/font for parval
	if(i.eq.8.or.i.eq.9) j=i	!size/font for axis labels
	if(i.ge.31) j=7	!size/font for axis numbers
	if(i.eq.10) j=10	!size/font for title
	call setfnt(ifnt(j))		!set appropriate size and font
c	call setcsize(csize*size(j),isize)    !sets isize too
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
	x=(rx(i))
	y=(ry(i))
	nt=NBLANK1(text1)
		call graspa(rx(i),ry(i),xsc,ysc)
	call TEXTBOX(xsc,ysc,text1(1:nt),angle(i),size(i),
     & ijus(i),rxbox(1,i),rybox(1,i),0)
33	continue
c Reset current values
	call setfnt(ifont)	!reset current values
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
c	ikey=nkey()
1311	call CKEY(ch,ikey)
	if(ikey.lt.-16.or.ikey.gt.15) goto 1311
	if(ikey.eq.-1) then	!F1 key
	   call VHELP(4)
	   goto 1311		!another menu choice
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-2)
	   goto 1311		!another menu choice
	endif
c	if(ikey.lt.1.or.ikey.gt.10) goto 13
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
	x0=1.e-36		!smallest value for log
	if(sqrty.and.ymin1.ge.0.) goto 131
c---
c New bit for dealing with logs of negative numbers
	if(logx.and.xmin1.le.0.) then
	   call BELL(1)
	   call WDIALOG(1,'Xmin is zero or negative',12)
	   iopt=1
	   call DEFOLTi(iopt,defolt)
	   call QDIALOG(1,
     &	'(1) Give new Xmin; (2) Abandon',
     &	defolt,ict,cans)
	   call GETINPi(cans,iopt)
c	   if(iopt.eq.2) then
c		ikey=6
c		goto 114	!for SCALCURV
	   if(iopt.eq.1) then

c		xmin1=0.01
		xmin1=1.0
		if(xmin1.gt.xmax) then
		  if(xmin.gt.0) then
			xmin1=xmin
		  else
		     xmin1=0.1*xmax
		  endif
		endif
 		call DEFOLTr(xmin1,defolt)
		call QDIALOG(1,'New Xmin value',defolt,
     &	ict,cans)
		call GETINPr(cans,xmin1)
		xmin1=xmin1*1.00001	!so does not round down
	   else
		ilog=ilogsav		!restore orig values and abandon
		logx=ilog.eq.1.or.ilog.eq.3.or.ilog.eq.4.or.ilog.eq.6
		logy=ilog.eq.2.or.ilog.eq.3
		logity=ilog.eq.4		!for Hill plot
		sqrty=ilog.eq.5.or.ilog.eq.6
		goto 15
	   endif
	endif
	if(logy.and.ymin1.le.x0) then
	   call BELL(1)
	   call WDIALOG(1,'Ymin is zero or negative',12)
	   iopt=1
c	   ymin1=0.01
	   ymin1=1.0
		if(ymin1.gt.ymax) then
		  if(ymin.gt.0) then
			ymin1=ymin
		  else
		     ymin1=0.1*ymax
		  endif
		endif
	   call DEFOLTi(iopt,defolt)
	   call QDIALOG(1,
     &	'(1) Give new Ymin; (2) Abandon',
     &	defolt,ict,cans)
	   call GETINPi(cans,iopt)
c	   if(iopt.eq.2) then
c		ikey=6
c		goto 114	!for SCALCURV
	   if(iopt.eq.1) then
 		call DEFOLTr(ymin1,defolt)
		call QDIALOG(1,'New Ymin value',defolt,
     &	ict,cans)
		call GETINPr(cans,ymin1)
		ymin1=ymin1*1.00001	!so does not round down
	   else
		ilog=ilogsav		!restore orig values and abandon
		logx=ilog.eq.1.or.ilog.eq.3.or.ilog.eq.4.or.ilog.eq.6
		logy=ilog.eq.2.or.ilog.eq.3
		logity=ilog.eq.4		!for Hill plot
		sqrty=ilog.eq.5.or.ilog.eq.6
		goto 15
	   endif
	endif

131	continue
	if(logity) then
 	   call DEFOLT2r(y0,yinf,defolt)
	   call QDIALOG(1,'Specify values for Y(0), Ymax',defolt,ict,cans)
	   call GETINP2r(cans,y0,yinf)
	   if(yinf.lt.ymax1) then
 		   call BELL(1)
	   	   call WDIALOG(1,
     &	    'Observation(s) greater than Ymax omitted!',12)
	   endif
		if(pon()) write(7,128) y0,yinf
		if(discprt) write(8,128) y0,yinf
128		format(' For Hill plot take Y(0), Ymax = ',2g13.6)
	endif
	xmin2=xmin1	!xmin1 etc always non-log values
	xmax2=xmax1
	ymin2=ymin1
	ymax2=ymax1
	if(logx) xmin2=alog10(xmin1)
	if(logx) xmax2=alog10(xmax1)
	if(logy) ymin2=alog10(ymin1)
	if(logy) ymax2=alog10(ymax1)
	if(logity) then
	   if(ymin1.gt.y0+x0.and.ymax1.lt.(yinf-x0).and.
     &	abs(yinf-ymax1).gt.1.e-30) then
		ymin2=alog10((ymin1-y0)/(yinf-ymin1))
		ymax2=alog10((ymax1-y0)/(yinf-ymax1))
	   else
		ymin2=-2.99999    !rounds to -3 to +3
		ymax2=2.99999
	   endif
	endif
c	if(logity) ymin2=alog10((ymin1-y0)/(yinf-ymin1))
c	if(logity) ymax2=alog10((ymax1-y0)/(yinf-ymax1))
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
	  if(idraw(i).ne.0) idraw(i)=-2		!all numbers at default posn
	enddo
	if (idraw(6).ne.0) idraw(6)=-2       !Parameter values drawn at default posn
	if (idraw(8).ne.0) idraw(8)=-2       !X axis label drawn at default posn
	if (idraw(9).ne.0) idraw(9)=-2       !Y axis label drawn at default posn
	if (idraw(10).ne.0) idraw(10)=-2	!so title, if present, drawn at default posn
	croset=.false.	!so sets xcross=xmin etc
	xmin1z=xmin
	xmax1z=xmax
	ymin1z=ymin
	ymax1z=ymax
	if(logx) then
		xmin1z=10**xmin
		xmax1z=10**xmax
	endif
	if(logy) then
		ymin1z=10**ymin
		ymax1z=10**ymax
	endif
	if(sqrty) then
		ymin1z=2**ymin
		ymax1z=2**ymax
	endif
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
	text(5)='5:ADD ARROW/LINE'
	text(8)='8:FIX HISTO LINE'
	text(9)='9:FIX CALC LINE'
	text(10)='10. REDRAW   '
	if(doframe) then
	 text(11)='+:OMIT FRAME'
	else
       text(11)='+:DRAW FRAME'
	endif
	text(15)='.:LINE THICKNESS'
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
219	call CKEY(ch,ikey)
	if(ikey.eq.-1) then	!F1 key
	   CALL VHELP(1)
	   GOTO 219
	endif
c===		1   2   3   4   5  6   7   8   9   10  11 12
	goto(241,232,235,219,234,219,219,116,116,306,12,219,
     &	219,219,236) ikey
	goto 219	!no valid key
c
12	doframe=.not.doframe	!toggle frame
	goto 221	!more changes?
c
c===================================================================
232	continue

	call DCMENU(0,4,Lb,text,0,0)		!delete all

	imd=0             ! move frame only
      call GTEXT(parval,xtitle,ytitle,title1,cnumx,cnumy,cexpx,
     &  cexpy,newtext,numbx,numby,ntext,inumx,inumy,logx,logy,
     &  rx,ry,ijus,icol,ANGLE,ifnt,idraw,size,icol(71),imd,RLTH,
     &  nrealt,nrealx,nrealy,nreal)

	goto 221

c===================================================================
c
c FIXNUMB OPTION


c===========================================================================
c				FIXNUMB
c===========================================================================

235	continue
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	call SETLBOX(10,Lb,1)
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	call SETLBOX(5,Lb,1)
	Lb(5)=0
 	text(1)='1 : X axis numbers'
 	text(2)='2 : Y axis numbers'
	text(3)='3 : All numbers'
	text(4)='4 : Done'
	call DCMENU(5,5,Lb,text,icol1,icol2)
2351  continue
	call CKEY(ch,ikey)
	if(ikey.ge.4) goto 221
	call DCMENU(0,4,Lb,text,0,0)		!delete all

      call GNUMB(cnumx,cnumy,cexpx,cexpy,numbx,numby,inumx,inumy,
     &  logx,logy,rx,ry,ijus,icol(24),angle,ifnt(7),idraw,size(7),
     &  icol(71),imd,rlth(24),ikey)
c All idraw() for numbers to be drawn should be no longer set to -2
c so numbers should be redrawn at new posn in LAXES
	goto 221

c Add/delete Arrow
234	continue

c start of arrow- need to store start/end points, and number of arrows,
c so they can be reproduced when graph redrawn
	ixfix=0		!x not fixed
	iyfix=0		!y not fixed
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	nbox=15		!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
c	Lb(11)=0
	Lb(5)=0
	Lb(13)=0
	Lb(12)=0
	Lb(14)=0
	Lb(15)=0
2340	continue
	if(narrow.eq.0) Lb(6)=0
	if(nline.eq.0) Lb(7)=0
	if(nhline.eq.0) Lb(8)=0
	if(nvline.eq.0) Lb(9)=0
	text(1)=' 1: ADD AN ARROW  '
	text(2)=' 2: ADD FREE LINE '
	text(3)=' 3: ADD HOR. LINE '
	text(4)=' 4: ADD VERT. LINE'
	text(6)=' 6: DELETE ARROW  '
	text(7)=' 7: FIX FREE LINE'
	text(8)=' 8: FIX HOR. LINE '
	text(9)=' 9: FIX VERT. LINE'
	text(10)='10 : DONE'
	text(11)=' +:S(obs) ARROW '
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
	call CKEY(ch,ikey)
	if(ikey.lt.1.or.ikey.gt.15) goto 221	!no valid key
c start of arrow- need to store start/end points, and number of arrows,
c so they can be reproduced when graph redrawn
	if(ikey.eq.1.or.ikey.eq.6.or.ikey.eq.11) then
	   call draw_arrow(ikey,narrow,xe,xb,ye,yb,ixfix,iyfix,
     &   xv,yv,csize,idev,xabs1,yabs1,xabs2,yabs2,rlth,zoomed)
	call broken(0)
	else if (ikey.ge.2.and.ikey.le.4) then
	   call draw_lines(ikey,nline,xlb,xle,ylb,yle,iltype,
     &   nhline,yhline,xhlb,xhle,ilhtype,
     &   nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel,
     &   csize,idev,xabs1,yabs1,xabs2,yabs2,rlth,
     &   xmin,xmax,ymin,ymax,zoomed)
	call broken(0)

	else if(ikey.eq.7.or.ikey.eq.8.or.ikey.eq.9) then           !fix lines
	   if(ikey.eq.7.and.nline.eq.0) goto 234	!nothing to fix
	   if(ikey.eq.8.and.nhline.eq.0) goto 234		!nothing to fix
	   if(ikey.eq.9.and.nvline.eq.0) goto 234		!nothing to fix
	   call FIXLINES(ikey,nline,xlb,xle,ylb,yle,iltype,
     &	 nhline,yhline,xhlb,xhle,ilhtype,ihlinrel,
     &	 nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,
     & 	 rlth,icol,icol(71))
	call broken(0)
	else if(ikey.eq.10) then
	     goto 221
	endif

	goto 234

c************************************************************
c
c Line thickness
236	continue
	imode=-1	!for line thickness
	call SETATT(narrow,nline,ntext,isdev,
     & rlth,icol,icol0,icol1,icol2,ncurvd,icurvd,ncurvc,icurvc,
     & ndimd,ndimc,ifitype,imode)
	call broken(0)
	goto 306	!redraw
c
c==================================================================

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
		size(j)=size(n)
		size(j)=size(n)
		angle(j)=angle(n)
		ijus(j)=ijus(n)
		rlth(j1+30)=rlth(n+30)
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
	n=ntext+10
c Fonts if=0=draft; 1=duplex; 2=complex; 3=bold (triplex); 4=script; 5=greek,
c but for ^F command use ifont=2*if for upright, or ifont=2*if+1 for italic
c ( for greek use 'A', 'B' in place of 10,11)
	call setfnt(ifnt(n))
	call setsize(size(n))    !sets isize too
	call WDIALOG(1,'Define start position for text with cursors',-ict)
	call CURPOS(xlo,ylo)
	call cursor(ikey,xc,yc)
      ch=char(ikey)	!note new usage
c Define all text string values (do once only, unless text altered), not
c everytime graph redrawn
	call spagra(xc,yc,rx(n),ry(n))
	ijus(n)=left
	idraw(n)=1
	ic=icol(30+ntext)
	if(ic.eq.0) then
	   ic=14
	   icol(30+ntext)=ic
	endif
	call WDIALOG(1,'Now write text: hit F1 anytime for HELP     ',ict)
	call GSTRING(xc,yc,newtext(ntext),ifont,angle(n),size(n),
     &	ijus(n),ic,icol(71),80,.true.,nreal)
	nl=NBLANK(newtext(ntext))
	if(nl.ge.1) then
	   call graspa(rx(n),ry(n),xsc,ysc)
	   call TEXTBOX(xsc,ysc,newtext(ntext)(1:nl),angle(n),size(n),
     &   ijus(n),rxbox(1,n),rybox(1,n),0)
	else
	   ntext=ntext-1		!don't count empty strings!
	endif
c
c Reset current values
	call setfnt(ifont)	!reset current values
	call setsize(size0)    !sets isize too
	goto 221

c##########################################################################
c                    SECTION FOR GRAPH PLOTTER
c##########################################################################

c NB DESKJET DRAWS 1000 DEVICE UNITS/INCH IN LANDSCAPE
C AND ABOUT 1250 UNITS/INCH IN PORTRAIT. Thus landscape gives better resolution
c even for tall narrow graphs- use portrait only if text needed on same page
c Note that this prog is used in AUTPLOT only to make single posh plots
c (for automatic plotting of many graphs VHISTQ is used).
c   mpos=1 upper left
c   mpos=2 upper right
c   mpos=3 lower left
c   mpos=4 lower right
c   mpos=5 whole page
c   mpos=6 top half page
c   mpos=7 bottom half page
c   mpos=8 X cm by Y cm
c ----- set plotter parameters

c==================================================================
155	continue
c 	open a 2 row dialog box top=row 23; cols 2-58
	monsav=mono			!keep value before plot
	do i=1,100
	   icolsav(i)=icol(i)
	enddo
	landplot=.true.		!landscape is default
c set whole page as default
	csize=cbig		!csize=default size for whole page plots
	thick=thbig		!line thickness=default thickness for whole page plots
c NB ixlo already set (and possibly reset by 'MOVE GRAPH') so do set here
	xlo2=xlo		!save screen GRAPHBOUNDARY
	xhi2=xhi
	ylo2=ylo
	yhi2=yhi
	call papenq(xp,yp,itype)
	vxloq=0		! for VIEWPORT
	vxhiq=xp
	vyloq=0
	vyhiq=yp
	nbox=10		!number of boxes for DCMENU
	s1=0.2
	s2=0.8
	xloq=s1*xp		! set normal shape
	xhiq=s2*xp		! display
	yloq=s1*yp		! location
	yhiq=s2*yp		! screen
	call SETLBOX(nbox,Lb,1)
	Lb(1)=-1		!whole page is default
	call DCMENU(0,4,Lb,text,0,0)		!delete all
1551	continue
	nxlo=8
	nylo=200
	nyhi=-1
	ictm=15		!text white
	ibkm=8		!background dark grey
	icfm=1		!frame dark blue
	icupm=12		!upper case red
	TITLEs     ='   PLOT OPTIONS    '
	strings(1)='1. Black&white postscript '
	strings(2)='2. Black&white laserjet '
	strings(3)='3. Color deskjet'
	strings(4)='4. Metafile color (*.cgm)'
	strings(5)='5. metafile Grey shades (*.cgm)'
	strings(6)='6. Windows metafile (*.wmf)'
	strings(7)='7. bitmap Format (*.bmp) '
	strings(8)='8. End          '
	nval=8

	TITLEs1    ='POSITION    '
	str2(1)='1. Whole page'
	str2(2)='2. Top left'
	str2(3)='3. Top right '
	str2(4)='4. Lower left'
	str2(5)='5. Lower right '
	str2(6)='6. Fix on VDU'
	str2(7)='7. Cancel '
	nval1=7

777	nhelp=0
	call POPMENU(nxlo,nylo,nyhi,strings,nval,ictm,icfm,
     & icupm,ibkm,titles,helps,nhelp,line1,charout,ival)
      if(line1.ge.1.and.line1.le.nval) then	!iline=0 for ESC=cance
	   ihelp=line1
	else
	   ihelp=8
	endif

	if(ihelp.eq.1.or.ihelp.eq.2.or.ihelp.eq.3) then
	   plot=.true.
	   if(ihelp.eq.3) call WDIALOG(1,
     &	'NOW AVAILABLE ONLY FOR GREY SHADES',ict)
	   call DEFOLTr(scalfac,defolt)
	   call QDIALOG(1,'Scale up/down : scale factor [0.8 to 1.1]',
     &   defolt,ict,cans)
	   call GETINPr(cans,scalfac)
	   if(scalfac.lt.0.8) scalfac=0.8
	   if(scalfac.gt.1.2) scalfac=1.1
	   s1=1.0-s2*scalfac
	   s2=s2*scalfac
         if(ihelp.eq.1.or.ihelp.eq.2) then
		call setcls(.true.,icol,autplt,plot,isetcol)
	   endif
	   call POPMENU(nxlo,nylo,nyhi,str2,nval1,ictm,icfm,
     &   icupm,ibkm,titles1,helps,nhelp,jline,charout,ival)
	   if(jline.ge.1.and.jline.le.nval) then	!jline=0 for ESC=cancel
		ih1=jline
	   else
		ih1=7
	   endif
	   select case(ih1)
	    case(1)
		  mpos=5
	        csize=cbig		!csize=default size for whole page
	        thick=thbig		!line thickness=default thickness for whole page plots
	        vxloq=0		! for VIEWPORT
	        vxhiq=xp
	        vyloq=0
	        vyhiq=yp
	        xloq=s1*xp		! set normal shape
	        xhiq=s2*xp		! display
	        yloq=s1*yp		! location
	        yhiq=s2*yp		! screen
	    case(2)
		  mpos=ih1-1
	        vxloq=0		! for VIEWPORT: top left
	        vxhiq=xp/2
	        vyloq=yp/2
	        vyhiq=yp
	    case(3)
		  mpos=ih1-1
	        vxloq=xp/2	!for VIEWPORT: top right
	        vxhiq=xp
	        vyloq=yp/2
	  	  vyhiq=yp
	    case(4)
		  mpos=ih1-1
	  	  vxloq=0		!for VIEWPORT: bottom left
	        vxhiq=xp/2
	        vyloq=0
	        vyhiq=yp/2
	    case(5)
		  mpos=ih1-1
	        vxloq=xp/2	!for VIEWPORT: bottom right
	        vxhiq=xp
	        vyloq=0
	        vyhiq=yp/2
	    case(6)
		  mpos=8
	        call FILLWIN(0,0,639,479,0)			!dialog area
		  call dFRAME(3.,210.,3.,150.,0)	!show defined region-viewport
		  call WDIALOG(1,
     &    'Define bottom left and top right corners with cursors',ict)
		  call CURPOS(xlo,ylo)
		  call CURSOR(ikey,x1,y1)
		  ch=char(ikey)
		  call jSYMBOL(x1,y1,4,csize,12)   !mark bottom left with red diamond
		  call CURpos(xhi,yhi)
		  call cursor(ikey,x2,y2)
		  ch=char(ikey)
		  call jSYMBOL(x1,y1,4,csize,12)   !mark bottom left with red diamond
	        if(x1.gt.x2) then
	  		ti=x1
	  		x1=x2
	  		x2=ti
		 endif
		 if(y1.gt.y2) then
	  		ti=y1
	  		y1=y2
	  		y2=ti
		endif
		call dFRAME(x1,x2,y1,y2,0)	!show defined region-viewport
		x3=x1+0.14*(x2-x1)	!bottom left of axes
		x4=x1+0.95*(x2-x1)	!bottom right of axes
		y3=y1+0.16*( y2- y1)	!bottom left of axes
		y4=y1+0.93*( y2- y1)	!top left of axes
		call dFRAME(x3,x4,y3,y4,0)	!show region of axes- graphboundary
c 	      Define ixlo etc from cursor values
		vxloq=0		! for VIEWPORT
		vxhiq=xp
		vyloq=0
		vyhiq=yp
		xloq=x3		! for GRAPHBOUNDARY
		xhiq=x4
		yloq=y3
		yhiq=y4
		fac=2.54/1000.
		if(.not.landplot) fac=2.54/1250.	!portrait
		x1=(x2-x1)*fac		!outer boundary
		y1=(y2-y1)*fac
		x2=(x4-x3)*fac		!axes boundary
		Lb(6)=-1
		fac=0.5*((x2-x1)/xp+(y2-y1)/yp)
		csize=csize*sqrt(fac)
		thick=fac		!line thickness multiplier
	    case(7)
	        plot=.false.
	        meta=.false.
		  goto 777
	   end select
	   if(mpos.eq.1.or.mpos.eq.2.or.mpos.eq.3.or.mpos.eq.4) then
	      thick=0.5		!line thickness multiplier
	      csize=csize*cfacsml
	      thick=thick*thfacsml
	      xloq=vxloq+0.5*s1*xp	! for GRAPHBOUNDARY- same for all
	      xhiq=vxloq+0.5*s2*xp
	      yloq=vyloq+0.5*s1*yp
	      yhiq=vyloq+0.5*s2*yp
	   endif
	else if(ihelp.eq.4.or.ihelp.eq.5) then
	   meta=.true.
	   idev=2
 	   if(ihelp.eq.4) then
c     		call setcls(.false.,icol,autplt,plot,2)
	   else
          	call setcls(.true.,icol,autplt,plot,2)
 	   endif
	   metafil='cvfit.cgm'
	   CALL WDIALOG(1,'OPTIONS FOR FILES NAMES ',ict)
765	   continue
	   nc=nblank1(metafil)
	   call qdialog(1,'Name for metafile ',
     &   ' ['//metafil(1:nc-4)//'] = ',ict,cans)
	   if(cans.eq.' ') goto 654
	   nc1=nblank1(cans)
	   if(nc1.le.20) then
		metafil=cans(1:nc1)//'.cgm'
		goto 654
	   endif
	   call wdialog(1,'YOU ENTERED AN INVALID FILE NAME',ict)
	   goto 765
654      continue
	else if(ihelp.eq.6) then
c	   call WDIALOG
c     &(1,'Sorry this option is not available for computers with less
c     &then 32MB ',ict)
c	   goto 777
	     wmeta=.true.
	     idev=1
      else if(ihelp.eq.7) then
c	   call WDIALOG
c     &(1,'Sorry this option is not available for computers with less
c     &then 32MB ',ict)
c	   goto 777
	     wbmp=.true.
	     idev=1
	else if(ihelp.eq.8) then
	   plot=.false.
	   meta=.false.
	   wmeta=.false.
	   wbmp=.false.
	   goto 1814
	else
	   goto 777
	endif

c=====================================================================
	jopt=1
	call DEFOLTi(jopt,defolt)
	call QDIALOG(1,'Plot now [1] / Cancel [2]',
     &  defolt,ict,cans)
	call GETINPi(cans,jopt)
	if(jopt.eq.1) then
	   if(ihelp.eq.1.or.ihelp.eq.2) then
	     nopt=1
	     call DEFOLTi(nopt,defolt)
	     call QDIALOG(1,'Landscape [1] / Portrait [2]',
     &     defolt,ict,cans)
	     call GETINPi(cans,nopt)
	     if (nopt.eq.1) then
			idev=4
			landplot=.true.
	     else if (nopt.eq.2) then
			idev=3
			landplot=.false.
	     endif
	   endif
	else
	   plot=.false.
	   meta=.false.
	   goto 777
	endif

c=====================================================================
	if (meta) then
	   call WDIALOG(1,
     &   'Assembling the metafile. . .',ict)
	   call devend
	   META=.TRUE.
	   ICGMDV=14
	   OPEN(UNIT=ICGMDV,FILE=metafil,STATUS='UNKNOWN')
	   CALL CGMBi
	   CALL DEVICE(ICGMDV,0)
	   call rgbdef(0,0.0,0.0,0.0)
	   call errswi(-1)
	   call brkswi(1)
	   call gsetcols(0)
c	   idev=7
	   call papenq(xpm,ypm,itype)
	   vxlo=0.
	   vxhi=xpm
	   vylo=0.
	   vyhi=ypm
	   xlo=s1*xpm	! for GRAPHBOUNDARY- same for all
	   xhi=s2*xpm
	   dym=((s2-s1)*xpm*yp)/(ypm*xp)
	   ylo=((1-dym)/2)*ypm
	   yhi=ylo+dym*ypm
	   call GRAPHBOUNDARY(xlo,xhi,ylo,yhi)
	   if(.not.sqrty) call gSCALE(xmin,xmax,ymin,ymax,is)
	   if(sqrty) call gSCALE(xmin,xmax,sqrt(ymin),sqrt(ymax),is)
	   call spagra(vxlo,vylo,wxmin,wymin)
	   call spagra(vxhi,vyhi,wxmax,wymax)
	   call mode(18)
	   call WDIALOG(1,'Metafile in progress . . .',12)
	else if(wmeta) then
	   call WDIALOG(1,
     &   'Assembling the wmetafile. . .',ict)
	   call devend
c	   call wmf(60,640,480)
	   call rgbdef(0,0.0,0.0,0.0)
	   call errswi(-1)
	   call brkswi(1)
	   call gsetcols(0)
	else if(wbmp) then
	   call WDIALOG(1,
     &   'Assembling the bmp. . .',ict)
	   call devend
c	   call bmp
	   call rgbdef(0,0.0,0.0,0.0)
	   call errswi(-1)
	   call brkswi(1)
	   call gsetcols(0)
	else if (plot) then
	   call pospix(xloq,yloq,ixlo,iylo)
	   iylo=479-iylo
	   call pospix(xhiq,yhiq,ixhi,iyhi)
	   iyhi=479-iyhi
	   if(ihelp.eq.2.or.ihelp.eq.3) then
	   	oldfile='HPLJ.OUT'
	   else
	   	oldfile='EPS.OUT'
	   endif
	   INQUIRE(file=oldfile,exist=present,flen=len)
	   if(present) call ERASE(oldfile)
c	   if(.not.landplot) idev=3	!portrait
c	   call REVCOL(icol)	!reverse black/white
	   if(bigplot.and.(.not.allpnt)) then
	      call INTCONV(ndelt,cnum1)
	      ans='Y'
	      call DEFOLTa(ans,defolt)
	      call QDIALOG(1,'Every nth point displayed, n = '
     &   	  //CHARNB(cnum1)//'; plot ALL points',defolt,ict,cans)
	      call GETINPa(cans,ans)
	      if(ans.ne.'N') allpnt=.true.
	   endif
	   call WDIALOG(1,
     &      'Assembling the plot . . .',ict)
	   call devend
	   if(ihelp.eq.1) then
	      if(landplot) then
	   	   call eps(1,5.,5.,297.,210.,297.,210.)
	      else
	   	   call eps(1,5.,5.,210.,297.,210.,297.)
	      endif
	   else if(ihelp.eq.2) then
	   	   call hplj
	   else
	      if(landplot) then
	   	   call hplj
c		   call hppj
	      else
	   	   call hplj
c		   call hppj
	      endif
	   endif
	   call errswi(-1)
	   call brkswi(1)
1042	   continue
	   call papenq(xpp,ypp,itype)
	   xhi=xhiq*xpp/xp
	   yhi=yhiq*ypp/yp
	   xlo=xhi-(xhiq-xloq)*xpp/xp
	   ylo=yhi-(yhiq-yloq)*ypp/yp
	   vxhi=vxhiq*xpp/xp
	   vyhi=vyhiq*ypp/yp
	   vxlo=vxhi-(vxhiq-vxloq)*xpp/xp
	   vylo=vyhi-(vyhiq-vyloq)*ypp/yp
	   call GRAPHBOUNDARY(xlo,xhi,ylo,yhi)
	   if(.not.sqrty) call gSCALE(xmin,xmax,ymin,ymax,is)
	   if(sqrty) call gSCALE(xmin,xmax,sqrt(ymin),sqrt(ymax),is)
	   call spagra(vxlo,vylo,wxmin,wymin)
	   call spagra(vxhi,vyhi,wxmax,wymax)
	   call WDIALOG(1,'Plotting in progress . . .',12)
c	   need to redefine box positions here (at least for bits
c	   of text that have boxes on final graph) as done after
c	   'GRAPH SHAPE'
	   call mode(18)
	endif
	goto 104		!do plot and return to 1812

c=================================================================================7

5555  continue
	call WDIALOG(1,'Metafile done. . .',12)
	meta=.false.
	CLOSE(UNIT=ICGMDV)
	CALL DEVEND
c==========================================
	goto 9876
5558  continue
	wmeta=.false.
	wbmp=.false.
c	CLOSE(UNIT=IDV)
	CALL DEVEND
c==========================================
9876	call VGA
	call gsetcols(0)
	call mode(18)
	call errswi(-1)
	call brkswi(1)
	idev=0
	scalfac=1.0
	call papenq(xp,yp,itype)
      call setcls(.false.,icol,.false.,.false.,0)
	xlo=xlo2		!restore screen GRAPHBOUNDARY
	xhi=xhi2
	ylo=ylo2
	yhi=yhi2
	vxlo=0		! for VIEWPORT
	vxhi=xp
	vylo=0
	vyhi=yp
	goto 1081


c==========================================================================
c 	Return to 1813 to plot via VTRANS
1813	continue
	plot=.false.
c	mono=.false.

c====================================================================================
c After plot finished:
1812	continue
c	goto 1043
	call DEVEND
c	call COPY('hplj.out','lpt1')
	call COPY(oldfile,'lpt1')
c	call VGA
c	call gsetcols(0)
c	call mode(18)
	if(idev.eq.2) then
	   printfil='cvfit.out'
	   CALL WDIALOG(1,'OPTIONS FOR FILES NAMES ',ict)
7651	   continue
	   nc=nblank1(printfil)
	   call qdialog(1,'Name for printfile ',
     &   ' ['//printfil(1:nc-4)//'] = ',ict,cans)
	   if(cans.eq.' ') goto 6541
	   nc1=nblank1(cans)
	   if(nc1.le.20) then
		printfil=cans(1:nc1)//'.out'
		goto 6541
	   endif
	   call wdialog(2,'YOU ENTERED AN INVALID FILE NAME',ict)
	   call COPY(oldfile,printfil)
	   goto 7651
6541	   continue
	endif
	if(idev.eq.4.or.idev.eq.3) then
	   kp=kp+1
	   call intconv(kp,kpchar)
	   nc1=nblank1(kpchar)
	   printfil='fit'//kpchar(1:nc1)//'.out'
	   call COPY(oldfile,printfil)
	endif
c 	To eject page without picking up another, send 'Esc E' to laserjet (OK for
c     deskjet too?).  If this is NOT done then another plot can be put on same page.
c	write(7,*) char(27)//'E'
      call FLUSH(7)
	if(mpos.ge.1.and.mpos.le.4) then
		size(7)=1.25*size(7)
		size(8)=1.25*size(8)
		size(9)=1.25*size(9)
		size(10)=1.25*size(10)
	endif

	call VGA
	call errswi(-1)
	call brkswi(1)
	call gsetcols(0)
	call mode(18)
1814	continue
	idev=0
	call papenq(xp,yp,itype)
	plot=.false.
c	if(mono) then
c		mono=.false.
		call setcls(mono,icol,autplt,plot,isetcol)
c	endif
	mono=monsav		!restore
	do i=1,100
	   icol(i)=icolsav(i)
	enddo
c?	itit=0		!no title yet
c 	Restore idev=0 (and scalfac, if altered)
	scalfac=1.0
	call setfnt(ifont)
	csize=cbig		!character size
	thick=thbig		!line thickness=default thickness for whole page plots
	xlo=xlo2		!restore screen GRAPHBOUNDARY
	xhi=xhi2
	ylo=ylo2
	yhi=yhi2
	vxlo=0		! for VIEWPORT
	vxhi=xp
	vylo=0
	vyhi=yp
1043	continue
	if (ih1.eq.7.or.jopt.eq.2) then
		call WDIALOG(1,'Plot cancelled',12)
	else
		call WDIALOG(1,'Plot done. . .',12)
	endif
	goto 1081



c ===================================================================
c
c PLOT QUEUE SECTION
301	continue
	call VHSQ3(xval,yval,xcal,ycal,ndimd,ndimc,
     & ncurvd,ndat,icurvd,ijoin,ncurvc,ncal,icurvc,iline,
     & xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ntx,nty,itx,ity,
     & xlo,xhi,ylo,yhi,itit,title1,ifont,ilog,iscal,doframe,
     & titlex,titley,ilabel,inumx,inumy,idiskq,qfile,sval,isval,
     & theta,ifitype,ncomp,idest,icol,mono,xwbase,lt2,
     & rlth,RX,RY,rxbox,rybox,IJUS,angle,idraw,ifnt,
     & size,cnumx,cnumy,cexpx,cexpy,numbx,numby,narrow,
     & xb,yb,xe,ye,ntext,nline,xlb,ylb,xle,yle,iltype,newtext)
	if(idest.eq.1551) goto 1551
c	goto 1812		!return to screen coordinates?
	goto 306		!redraw
c end of plot queue section
c
c====================================

c TIDY UP AND EXIT
999	continue
	if(iabs(iask).eq.1) then
	   call BELL(1)
	   ans='Y'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'ARE YOU SURE',defolt,ict,cans)
	   call GETINPa(cans,ans)
	   if(UC(ans).eq.'N') goto 151
	endif
c
	if(iask.gt.0) call DCMENU(0,5,Lb,text,0,0)	!delete boxes before exit
 	if(iask.lt.0)call VIDEOMOD(3)	 !this makes graph go for good!
	RETURN
	END

