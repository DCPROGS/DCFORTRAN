	subroutine VPLOT4(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,syms,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & iXLO1,iXHI1,iYLO1,iYHI1,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,draft,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,isdev,interp,
     & isetcol,itrace,ndv1,ndc1)
c
cTO BE DONE=======================
c==========SHOULD LINES/MULT TRACE HAVE SEPARATE COMMONS OR SHOULD THEY
c============GO IN /TPOS/
c
c (*) Make general input IDEFAULT=0,1,2,3 to control various options
c	about which variables are set internally, and which taken from
c	TPOS etc, when called with autplt=.false. (can replace itrace -
c	-ans could replace autplt too!) eg initialise ntrace=1?, nhline=0?
c (*)  Fix so ESC returns to previous menu
c (*) add more HELP (eg F2 for tree structure of drawbox calls?) or
c	reproduce VPLOT.HLP notes on screen?
c     Could get help on particular questions eg make new version of INPUTr
c	and INPUTi that will recognize Fn characters or other invalid char and
c	return which was hit: if asking for a number can then hit F1 etc to
c	get help on the answer. Also would be nice to have some way to get
c	help on eg how to fix CLOGO or whatever
c (*) Make dummy call as in VPCALL.for
c Modif 09/28/94 07:29pm so ivyhi=7484 because this, rather than 7000, is
c the largest displayable value (Hgraph units) on VGA screen (see TCOORD)
c
c VPLOT4 is new version (09/10/94 10:24am) with more parameters in call
c  and with both dimensions adjustable for xval, yval, xcal, ycal.
c (1)!!! XVAL etc MUST BE ALLOCATABLE IN CALLING PROGRAM AND ALLOCATED AS
c     XVAL(ndv1,ndimd), YVAL(ndv1,ndimd), XCAL(ndc1,ndimc), YCAL(ndc1,ndimc),
c     BEFORE CALLING VPLOT4 !!!
c     Problem: ndat() etc were previously all dimensioned as ndat(10); for
c	 complete generality need to make these allocatable too, as ndat(ndimd)
c	 etc, but this makes them difficult to declare before they are read
c	 from PLOTQ.  Simplest solution is probably to impose max dimemsion
c	 for them (not likely to have more than 10 or 20 curves on one graph!)
c	 so arrays can be declared this max size in AUTPLOT and are therefore
c	 sure to be big enough for queued data -see VPLQ4 for details
c (2) Stuff for multiple line traces (esp for SC plots) added in common/tpos
c	(ignored if itrace=0 in call)
c	ntrace(i), (i=1,ndimd), = number of traces into which ith data set is
c	   split (normally =1)
c	ytsep(i), (i=1,ndimd), = vertical distance between the traces for ith set
c
c (3) Separate common/cols/icol,mono created to communicate icol() to program
c	and these removed from common/tpos.  Thus if want only to control
c	colours then need only have common/cols/ in calling prog, not the 
c	whole /tpos/.
c
c (4) New parameters in the call:
c	ndv1,ndc1=allocated dimensions of Xval etc.
c	nhline,nvline= # of horizontal/vertical lines -if either is =>1
c		details are taken from arrays in common/tpos/
c	itrace=0 if no multiple traces used, =1 if values are in common
c
c VPLOT3 is new version (03/16/94 07:48am) that has following changes
c (1) Control of colours from calling program; still has ICOL,MONO in
c	common (now in /cols/ rather than /tpos/), but has extra parameter
c	 ISETCOL in call. If this is zero then functions as before (colours set
c	 internally unless autplt=true). If ISETCOL=1 then all colours to
c	 default if ICOL(i)=-1 on entry, but if ICOL(i) is a valid colour on
c	 entry, default not set.  Subroutine SETCOL does this (replaces SETCOLS).
c (2) Modif 09/03/94 09:25pm to add horizontal lines/vertical lines/grid
c	nhline=number of horizontal lines
c	nvline=number of vertical lines
c	Arrays defining their position are in common/TPOS rather than param, viz
c      yhline,xhlb,xhle,ilhtype,
c      xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel.
c     Colour/line thickness in icol(72-81) horizontal,icol(82-91) vertical
c
c
c Lahey V5.n version 01/21/93 01:24pm
c Updated to use dialog boxes 06/29/93 03:38pm
c VPLOT2 is version of VPLOTR that includes SD bars and aims to be universal
c plotting routine.
c   (1) Uses DCMENU in place of DRAWBOX (see TDCBOX). Names of ixlo etc altered
c	to ixlo1 etc in call, so ixlo can be in common/hgv/.  This common can
c	be used for any subroutine that need to change viewport and/or
c	graphboundary, to allow restoration of current values before leaving.
c   (2) Colour added.  Note that COLTYP1 and FILL1 use the Lahey graphics
c	 colour numbering. MONO=true for monochrome display (except for DCMENU).
c	 ICOL,MONO added to common/tpos/ (so queued colours accessible in AUTPLOT)
c      At present, when called with autplt=false, uses default colours
c	automatically, despite fact that mono and icol are available in
c	main prog (via common/tpos/).
c   (3) added INTERP as a parameter rather than using ijoin(10) to control
c	interpolation
c TO BE DONE
c (*) add more HELP (eg F2 for tree structure of drawbox calls?) or
c	reproduce VPLOT.HLP notes on screen?
c     Could get help on particular questions eg make new version of INPUTr
c	and INPUTi that will recognize Fn characters or other invalid char and
c	return which was hit: if asking for a number can then hit F1 etc to
c	get help on the answer. Also would be nice to have some way to get
c	help on eg how to fix CLOGO or whatever
c (*) Last line of HELP still not showing.
c
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
c Unlike previous versions, VPLOT2 has some parameters in
c COMMON blocks, rather than as arguments.  This makes it easier in cases where,
c for example, SD bars are not needed, because the WEIGHT array need not be
c declared in the calling program at all (i.e. omit the COMMON/SD/ block
c from the calling program, and set ISDEV=-1 in the call so no attempt is made
c to refer to it. The common blocks are as follows.
c (1) COMMON/SD/weight(100,10)  (set isdev=-1 in call if not needed)
c (2) COMMON/JLOGO/t1c(10),t2c(10),t1v(10),t2v(10),xoff1,iy1v,iy2v,iy1c,iy2c
c	(set ncjump=0,nvjump=0 in call if logos not needed; this causes iy1v to
c	be set to	-1000 and so prevent 'reset t=0' option)
c Other differences:
c (1) SD bar thickness now in ilth(30)
c (2) ISCAL=2,3,4 added
c (3) iline=10 to 18 joins calc points with Hgraph line type iline-10
c
c Modif 02/06/93 10:59pm so that the number and text in calibration bar
c  labels are kept separately (as for regular axes), rather than having the
c  number as part of the axis label as previously.
c Modif 11/15/91 06:33pm to allow calibration bar on one axis and regular
c axis on the other:
c	ntx=-1000, nty=-1000: -both axes are calibration bars
c	ntx=-1000, nty=nty:   -X axis has calibration bars
c	ntx=ntx  , nty=-1000: -Y axis has calibration bars
c Modif 09/03/91 10:06am to allow calibration bars (rather than normal axes)
c	to be drawn.  Signalled by setting ntx=-1000. Call with titlex,
c	titley containing units ONLY eg =' ms ', for construction of labels
c	on calibration bars when called with ntx,nty=-1000
c Modif 07/05/91 03:31pm
c	(1) Fixed for display of more than 2048 data points (calculated points
c	 as before).
c	NDAT(j) may be more than 2048 points now, so data for one graph may occupy
c	more than 1 column of yval(i,j) -say ngr(j)=number of columns used for jth
c	graph; ngr=1+(ndat(j)-1)/2048 eg if graph #1 has 5000 points ngr(1)=3.
c	Thus data for graph #2 start in Yval(1,4) ie Yval(1,ngr(1)+1).
c	Each graph must start at the beginning of a column of Yval(i,j), but have
c	problem in defining where data starts for each graph now it is not
c	necessarilly in column #j -these really needs an extra input to specify
c	it, but this means changing plot queue etc.  Therefore now use convention
c	that ICURVD(j1), j1=1,..,ncurvd, gives the column (j value) at which data
c	starts for data curve #j1, ie 1st data point is in Yval(1,j) where
c	j=icurvd(j1). When all ndat(j)=<2048 this will be as normal.
c	At present every NDELT'th point only plotted initially, with ndelt>1 when
c	nec to keep the number of points on display =<2048
c	NB if more than 2048 points are written to Yval(i,2) say, then the
c	extra ones (when i>2048) do not cause error but just continue into
c	Yval(i,3) i.e. Yval(2049,2) is interpreted as Yval(1,3) and so on
c	-no problem as long as we do not go beyond end of Yval, ie Yval(2048,10);
c	(see TEQUIV.FOR and TVPR1.FOR).  This simplifies things greatly, and
c	means, for example, that queue subroutine VPLQR1 should not need changing!
c	(2) To control interpolation without adding an extra parameter use
c	 as signal IJOIN(10); to interpolate from start subtract 100 from it;
c	 If this done the 100 is added again at start of prog and INTERP set true.
c	 If interp=true on exit, 100 is subtracted again before exit. NOW REPLACED
c	 by extra parameter = INTERP in the call.
c
c 01/15/91 02:26pm Fixed so that, if in graphics mode on entry (videotyp()=18)
c then call INIPLT is not done (but still need call ERASCR
c so any text that was on screen before call is deleted)
c
c VPLOTR Initially same as vplot1 except:
c (1) array size increased to 2048
c (2) SD bars
c     WEIGHT(100,10) added for error bars; SDEV calc as sqrt(1/w)
c	 Set ISDEV=-1 if no WEIGHT=1/s*s supplied so bars cannot be plotted
c	     ISDEV=0 if WEIGHT supplied, but not plotted unless req
c	     ISDEV=1 if WEIGHT supplied and bars to be plotted initially
c    Bars drawn SDEV above and below point (eg can set SDEV=1.96*s(y) before call)
c    Modif 04/25/91 09:51am so points not plotted if weight(i)=0 (unless isdev=-1
c     in which case weight not defined)
c (3) 11/08/90 09:26am Option to draw diagrammatic C-jump and/or V-jump logo
c	on graph (times for start,end of jumps in t1c,t2c,t1v,t2v); upper and
c	lower levels (in screen units, 0-7000) in iy1v,iy2v,iy1c,iy2c; set iy1c
c	and/or iy1v =-1 to omit the respective logo. Set iy1v=-1000 to signify
c	that VPLOTR is not plotted against time at all (eg used for power
c	spectra at present -because it is only VPLOT that takes 2048 points) -in
c	this case the 'redefine t=0' option is omitted.
c (4) and option to move t=0 to start of a jump, or to cursor-position
c	[if moved to start of jump, assumes all xval(*,j) are same such
c	that Xval(*,1) is at the same time origin as t1v(),t2v(),...]
c (5) Add input param XOFF1=value of any offset applied to time axis
c	values supplied in XVAL (eg when only part of sweep displayed with
c	t=0 at start of jump) -needed to draw jump logos correctly since t1,t2
c	always in time from trigger.
c (6) Add logical input IVPLOT=true when I/V plot being done (do not want
c	options 3,4 above in this case)
c (7) Management of Xcross,Ycross changed. ISCAL=-1 added (see below) and
c	now, if xmin,ymin changed then xcross,ycross reset to new xmin,ymin
c	IF old xmin,ymin=old xcross,ycross. But when this not true then
c	xcross,ycross NOT reset (eg so xmin,ymin can be changed on an
c	I/V plot without resetting xcross,ycross)
c
c Modif 01/15/90 12:48pm so that text, numbers and their positions are
c all taken from COMMON/TPOS/ if AUTPLT=true (for use e.g.in AUTPLT.FOR
c in which their values are all specified in queue)
c    IASK=1 or -1 to ask 'are you sure' before leaving VHIST; =2,-2 to not ask
c    IASK=pos to leave graph on screen (1 or 2); neg (-1,-2) to delete before
c		exit (orig version is iask=-2)
c    IASK=3 to draw graph and leave at once (no boxes; no chance to alter or
c		 plot or queue it)
c
c When used in AUTPLT several parameters are specified on entry that
c are defined only internally when used in CVFIT  etc (e.g. ITIT,TITLE1,
c DOFRAME,...) so parameter AUTPLT added: when TRUE
c (1) input values of itit,title1,doframe,... (from queue) used
c (2) Doframe added as parameter (true=draws box round graph), also
c	itit,title1
c (3) Fonts specified on 0-11 scale so set by call SETFONT1(if) here.
c (4) Input logical FITTED used to control if calc curve is queued
c	so the 'example' exponential drawn on log(time) histo is
c	not queued for plotting in AUTPLT
c   When AUTPLT=true, then FITTED is set true if ncurvc>1, ie if there
c	is a calc curve to be plotted (and stored in POSHPLOT.DAT)
c (5) NB Multiple data and fitted curves are now
c     only way to get superimposition now)
c	(1)Ndimd,ndimc =dimensions in calling prog ie Yval,Xval(2048,ndimd),
c		Ycal,Xcal(2048,ndimc)
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
c	    ILABEL=-1 to use default axis labels
c	    ILABEL=0 no labels put on either axis
c	    ILABEL=1 labels drawn at left/bottom wherever axes are
c		with writing starting from XMIN/YMIN
c	    ILABEL=2 labels drawn on axes starting to right of xcross
c		and above ycross (so labels should not be too long)
c	    ILABEL=3 labels on axes but starting on left for X axis
c		and at bottom for Y axis
c
c
c NB call to INIPLT with device=0 or 1 will erase screen, but
c	not erased if idev>1. (Can get from alpha mode to graphics
c	without erasing screen by call to LOADCRTC() but must
c	reset BIOS=18 to BIOS=146 (=18+128) in SCREEN.CFG to do
c	this (will not prevent INIPLT(0 from erasing screen)
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
c    NTX,NTY label every NTXth (major) tic
c    ITX,ITY=1 for normal tic orientation,=0 for centered,=-1 for opp of normal
c	(if ISCAL=1 default values set for ntx,nty)
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
c         (prev numbered 1-5, but now make 0-4 so 0=continuous as in Hgraph)
c          (iline=-1=skip this curve)
c		iline=10+n joins calculated points with straight (Hgraph) lines-
c		  of type n (as listed for ijoin) n=0 to 8
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
c	 =4 for Hill plot
c	 =5 for sqrt(y) vs x
c	 =6 for sqrt(y) vs log(x)
c
c NB always uses input values of itx,ity (tic orientation) if valid value
c itx (-1,0,+1) given, otherwise both set to 1.
c  ISCAL=-1 if input value of xcross,ycross to be used, but xmin,..,ymax,
c		and xtic,ytic found internally
c  ISCAL=0 if input values of xmin,...,ymax,xtic,ytic,xcross.. to be used.
c  ISCAL=1 if scaling to be done internally with FIXAX.
c  ISCAL=2 if input values of xmin,xmax only to be used; others internally set
c  ISCAL=3 if input values of ymin,ymax only to be used; others internally set
c  ISCAL=4 if input values of xmin,xmax,ymin,ymax to be used; rest internal
c  ISCAL set to 0 if NDAT=<0
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
c ARRAY INDEX FOR POSITIONS (IDRAW,IANGLE,RX,RY,RXBOX,RYBOX)
c (all now dimension=100):  1-5=spare [were newtext]; 6=param values; 7=spare;
c 8=X-axis label; 9=Y-axis label; 10=title; 11-30=newtext; 31-55=numbers
c (#1-25) on Xaxis; 56-80=numbers (#1-25) on Y axis; 81-100 spare for future
c
c 09/03/91 11:16am Now use:
c Length of the bars (in world units, eg pA,ms) kept thus:
c Horizontal and vertical so ry(1)=ry(2), and rx(3)=rx(4)
c idraw(1) controls whether bars are drawn or not
c rx(1),ry(1)=origin of X-calibration bars (world units),
c rx(2),ry(2)=end of X calibration bars
c rx(3),ry(3)=origin of Y-calibration bars (world units),
c rx(4),ry(4)=end of Y calibration bars
c
c Line thickness for whole page plots in ILTH(i) as follows ( actual
c thickness =THICK*ILTH(i) were THICK=THBIG=1.0 for whole page).
c Colours in icol() are numbered the same way, except colours for symbols,
c SD bars and lines joining data points are all set to 'data set' colours
c (icol(1)-icol(10))
c 1-10= data sets (#1-10) (and colour for symbols etc);
c 11-20=calc curves (#1-10);
c 21=axes (or calibration bars); 22=frame; 23=axis labels (same for X,Y)
c 24=axis numbers (same for X,Y); 25=plot title; 26=param value text;
c 27=symbols (line thickness only; colour as for data sets);
c 28=jump bar (logo) for C-jumps
c 29=jump bar (logo) for V-jumps
c 30=SD bars
c 31-50=extra text (#1-20); 51-60=arrows (#1-10); 61-70=lines (#1-10)
c icol(71)=background colour for graph
c icol(72-81)= horizontal lines; icol(82-91) vertical lines
c 92-100 =spare for future
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
c		Elements of IDRAW() defined as for ILTH() above
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
c			extra arrows/lines though latter are not text and have no
c			boxes defined for them). In this case nothing done if idraw=-2
c			(idraw gets set to 1 or -1 when newtext defined via 'add new
c			text'
c		 (c) Jump logos, idraw(28),idraw(29) for c-, v-jump are special
c			case. Set to 1 to draw, 0 to not draw.
c		 (d) Calibration bars, idraw(1) is also special case. Have an
c			extra value, idraw(1)=-3 which causes calibration bars to
c			be drawn at the default position, but with the
c			currently-defined length (rather than default length) e.g. for
c			use after rescale.
c Special problems for text location
c   (1) After rescale
c After rescale that affects numbering on axes (xmin,xmax,xtic,xcross) must
c redraw all numbers on the affected axis at their default positions (idraw=-2)
c####also do axis labels at default posn??
c   (2) After change of log/sqrt axes
c AFTER CHANGE OF LOG AXES NEED TO REDRAW EVERYTHING AT DEFAULT POSN
c When, as for NEWTEXT and ARROWS then could take log/sqrt/antilog etc
c of rx,ry but this involves checking how axis CHANGES eg might change
c from logy/logx to logy/x so Y axis is log, but not changed so no
c need to alter RY(). This not yet fixed, so log axes should be changed
c before any newtext/arrows added.
c
c   (3) After change of graph shape (by call to graphboundary)
c After change of shape by call to graphboundary must redraw ALL text
c at the same world coordinates in rx,ry. ie do not want any of the idraw=-2
c (all should be -1,0,1). But boxes may be in wrong positions
c or distorted,so all boxes redefined now, from the current rx,ry.
c
	real*4 XVAL(ndv1,ndimd),YVAL(ndv1,ndimd)
	real*4 XCAL(ndc1,ndimc),YCAL(ndc1,ndimc)
c for data
	integer*4 ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd)
	real*4 syms(ndimd)		!symbol size
c for calc curves
	integer*4 ncal(ndimc),icurvc(ndimc),iline(ndimc)
c for interpolation
	real xint(2048),yint(2048),Y2int(2048)	!for interpolation
c	real xdbad(25),ydbad(25),xcbad(25),ycbad(25)	!keep 1st 25 bad values
c
	logical ivplot,onjump,tload,calbarX,calbarY,calbar,blank
	logical vert,mono,monsav,colplotter,redrawn
	logical relval,delete
	real theta(20)
	dimension zseq(10)
	character*1 ans,UC,ch1,ch2
	character*40 titlex,titley,titxsav,titysav
	character*75 xtitle,ytitle	!output from LAXES
	character*75 oldtext
	character*44 TITLE1
	character parval*150		!to hold param values
	character text1*150		!to hold any string (for fixtext call)
	character defolt*30,cans*30		!to hold text & result of QDIALOG
      character ch*1,cnum1*11		!must have *11 for INTCONV
	LOGICAL PLOT,logx,logy,logity,down,fitted,croset,equal,sqrty
	logical doframe,autplt,draft,landplot,landscap,zoomed,interp
	logical allpnt,bigplot		!for when one or more plots >2048 points
	logical caplock,debug,pon,slock
C=	integer*2 lrow
	integer*2 int2,videotyp
	integer*2 ilt,ilth(100),lth 		!for line thickness
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
	integer ifnt(30)
	real csfac(30)
	real xb(10),yb(10),xe(10),ye(10)		!up to 10 arrows
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	integer iltype(10)				!line type for ditto
	character*80 newtext(20)		!extra text
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
c      character*1 ch, fndcur   !fndcur is now integer
      integer fndcur
	integer Lb(18)		!for DCMENU
	character*78 text(18)
	logical discprt
	common/dp/discprt
	COMMON/SD/weight(100,10)  !set isdev=-1 in call if not needed
	COMMON/JLOGO/t1c(10),t2c(10),t1v(10),t2v(10),xoff1,iy1v,iy2v,
     & iy1c,iy2c
	common/DMENU/csizb,ifonb,csize,ifont2,nboxlast,nblast		!for DCMENU
	common/hgv/ivxlo,ivxhi,ivylo,ivyhi,ixlo,ixhi,iylo,iyhi
	COMMON/TPOS/rx,ry,rxbox,rybox,ixjus,iyjus,iangle,idraw,
     & ifnt,csfac,ilth,thbig,narrow,xb,yb,xe,ye,
     & nline,xlb,xle,ylb,yle,iltype,ntext,newtext,
     & cnumx,cnumy,cexpx,cexpy,numbx,numby,
     & nhline,yhline,xhlb,xhle,ilhtype,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel,
     & ntrace,ytsep
	COMMON/cols/icol,mono
c lines 5,6 added to TPOS 09/04/94 08:29pm for hor and vert lines
c  line 7 added to TPOS 09/12/94 07:11am multiple traces
c NB integer LINWID renamed as LINWID1 in commons, so does not clash
c with LINWID calls.
      COMMON /CHGRAPH/ penstatus, nocheck, chwid, chhght, small, ixcur,
     .                 iycur, maxvdc, ixminf, ixmaxf, iyminf, iymaxf,
     .                 curfont, fonts, maxfont, maxsoft, ixorg, iyorg,
     .                 ixbond, iybond, sclfx, sclfy, iascr, ibscr,
     .                 icscr, idscr, xminw, xmaxw, yminw, ymaxw, xslope,
     .                 yslope, jxbond, jybond, irot, maxplen, maxpwid,
     .                 scrx, scry, icolh, idisp, jpfile, ixlas, iylas,
     .                 inside, strtnew, linntp, lin1, dash, space,
     .                 iystart, ixxpos, jxlpos, jylpos, jxtpos, jytpos,
     .                 jxtics, jytics, jgrxmrk, jgrymrk, jxsiz, jysiz,
     .                 xftic, xltic, xintv, yftic, yltic, yintv, jxlen,
     .                 jxsub1, jxsub2, jylen, jysub1, jysub2, jxacol,
     .                 jxlcol, jyacol, jylcol, xstart, xinter, xend,
     .                 ystart, yinter, yend, jxtype, jytype, jpair,
     .                 jxshift, jyshift, xproc, yproc, xfrmt, yfrmt,
     .                 hgrdir
      INTEGER   penstatus, nocheck, ixcur, iycur, maxvdc, ixminf, ixmaxf
      INTEGER   iyminf, iymaxf, maxfont, maxsoft, ixorg, iyorg, ixbond
      INTEGER   iybond, iascr, ibscr, icscr, idscr, jxbond, jybond, irot
      INTEGER   maxplen, maxpwid, icolh, idisp, ixlas, iylas
      INTEGER   inside, strtnew, linntp, lin1, iystart, ixxpos(2)
      INTEGER   jxlpos, jylpos, jxtpos, jytpos, jxtics, jytics, jgrxmrk
      INTEGER   jgrymrk, jxsiz, jysiz, jxlen, jxsub1, jxsub2, jylen
      INTEGER   jysub1, jysub2, jxacol, jxlcol, jyacol, jylcol
      INTEGER   jxtype, jytype, jpair, jxshift, jyshift
      INTEGER*4 curfont, fonts, xproc, yproc, jpfile
      REAL      chwid, chhght, small, sclfx, sclfy, xminw, xmaxw, yminw
      REAL      ymaxw, xslope, yslope, scrx, scry, dash, space
      REAL      xftic, xltic, xintv, yftic, yltic, yintv, xstart, xinter
      REAL      xend, ystart, yinter, yend
      CHARACTER hgrdir*61, xfrmt*256, yfrmt*256
c
      COMMON /CRESOL/ GCON, IXXG, IYYG, IXGG, IYGG,
     .                GRAFBASE, CRTCREG, COLREG, MODEREG, XMODEREG,
     .                GRAFPAGESIZE, NUMOFCOLS, HORSCRLEN, VERSCRLEN,
     .                PIXELPERBYTE, BYTEPERROW, NUMGRAFPAGE, REGVAL,
     .                COLORVAL, MODEVAL, XMODEVAL, BIOSMODE, PAGENUM,
     .                NUMREGS, NGPLN2, PPBLN2, BPPLN2, LWIDTHTT,
     .                LHEIGHTTT, LASTMODE, ASPECTRATIO, DPIH, DPIV, DPL,
     .                DPP, NPINS, NUMCOLS, NUMROWS, NPLANES, CURGRAY,
     .                LINWID1, NUMGRAY, MAXCOL, SCLF, FWD, FLN
      INTEGER   GCON, IXXG, IYYG, IXGG, IYGG
      INTEGER   GRAFBASE, CRTCREG, COLREG, MODEREG, XMODEREG
      INTEGER   GRAFPAGESIZE, NUMOFCOLS, HORSCRLEN, VERSCRLEN
      INTEGER   PIXELPERBYTE, BYTEPERROW, NUMGRAFPAGE
      INTEGER   NGPLN2, PPBLN2, BPPLN2, LWIDTHTT, LHEIGHTTT, LASTMODE
      INTEGER   DPIH, DPIV, DPL, DPP, NPINS, NUMCOLS, NUMROWS, NPLANES
      INTEGER   CURGRAY, LINWID1, NUMGRAY, MAXCOL
      INTEGER*1 REGVAL(26), COLORVAL, MODEVAL, XMODEVAL, BIOSMODE
      INTEGER*1 PAGENUM, NUMREGS
      REAL*4 ASPECTRATIO, SCLF, FWD, FLN

      COMMON /HCDNAM/ RASTEMPDIR, RASDRVNAME, RASOUTNAME,
     .                VECOUTNAME, VECDRVNAME, VECPRENAME
      CHARACTER*81 RASTEMPDIR, RASDRVNAME, RASOUTNAME
      CHARACTER*81 VECOUTNAME, VECDRVNAME, VECPRENAME
c        INCLUDE 'c:\hgraph\CONSTS.FOR'
c*      INCLUDE 'c:\hgraph\HGRGLB.DEC'
c*      INCLUDE 'c:\f77l3\SPINATT.HDR		!modif version of SPINATTR.HDR
c values below are from c:\hgraph\CONSTS.FOR
      INTEGER LEFT, CENTER, RIGHT, BASE
      INTEGER BELOW, BOTTOM, ABOVE, TOP
      DATA LEFT, CENTER, RIGHT, BASE /0, 2, 1, 3/     !new definition
      DATA BELOW, BOTTOM, ABOVE, TOP /0, 0, 1, 1/     !new definition
c
c Define functions
	pon()=slock()
	debug()=caplock()
	LTH(i)=int2(ifixr(thick*float(int4(ilth(i)))))	!sets line thickness
	EQUAL(x,y)=abs(x-y).lt.0.00001
c
3	format(i8)
4	format(g13.6)
c Initialisations:
c (1) Initialisation whether AUTPLT or not
c	if(VIDEOTYP().ne.18) call INIPLT(idev,.false.,1.0)
	scalfac=1.0
	if(VIDEOTYP().ne.18) call INIPLT(0,.false.,scalfac)
	xticsav=xtic
	ixlo=ixlo1		!named ixlo1 in call so ixlo can be in common/hgv/
	ixhi=ixhi1		!ditto
	iylo=iylo1		!named iylo1 in call so iylo can be in common/hgv/
	iyhi=iyhi1		!ditto
	ifont2=ifont	!copy for common/dmenu/
	nboxlast=10		!in case attempt to delete boxes before any drawn
	nblast=4
c	nbox=10	!number of boxes for DCMENU
	icol1=14	!yellow text/border for boxes in DRAWBOX
	icol2=8	!grey background for boxes
	icol0=7	!white text/border for altered boxes
	icb2=0	!background colour for dialog box 1,2
	icf=7		!frame colour for dialog box 1
	icf2=12	!frame colour for dialog box 2
	ict=11	!text colour for dialog box 1
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
c	call SETCOLS(mono,icol,autplt,plot)
	call SETCOL(mono,icol,autplt,plot,isetcol)
	ifonb=0     !font for boxes
	csizb=1.7   !character size for boxes
	calbarX=ntx.eq.-1000
	calbarY=nty.eq.-1000
	calbar=calbarX.or.calbarY
	ntxsav=ntx		!keep input value
	ntysav=nty		!keep input value
	i=NBLANK1(titlex)               !ensure it ends with char(0)
	j=NBLANK1(titley)               !ensure it ends with char(0)
	if(ntx.ne.-1000) titxsav=titlex
	if(nty.ne.-1000) titysav=titley
	if(ncjump.eq.0.and.nvjump.eq.0) iy1v=-1000	!so no 'set t=0' option
	xcalib=rx(2)-rx(1)
	ycalib=ry(4)-ry(3)
	plot=.false.
	croset=.false.		!cross position has not been reset
	if(iscal.eq.0.or.iscal.le.-1) croset=.true.	!use input cross position
	iret=0		!for return after help screen
	landplot=.true.
	draft=.false.
	zoomed=.false.
c	interp=.false.			!now a parameter
c	if(ijoin(10).lt.-10) then	!signal to interpolate
c	   ijoin(10)=ijoin(10)+100	!restore proper value
c	   interp=.true.
c	   yp1=0.
c	   ypn=0.
c	endif
	cfacsml=0.6		!character size factor for small plots (0.6*2.5=1.5)
	thfacsml=0.6	!line thickness factor for small plots
	ifsav=ifont		!save input value (if there is one)
	itlen=200
	bigplot=.false.
	do 803 j1=1,ncurvd
	j=icurvd(j1)
	if(ndat(j).gt.2048) bigplot=.true.
803	continue
c
c (2) Initialisation ONLY when AUTPLT=true.
c Note: PLOTQ/POSHPLOT now hold values for things are already defined at
c time plot is queued/stored. Thus default angle,line thickness etc not
c defined for new things that are added. This is done in  RDVPLQ
	if(autplt) then
	   csize=cbig
	   thick=thbig
	   fitted=ncurvc.gt.0
	   allpnt=.true.		!plot all points if ndat>2048
	endif
c
c (3) Initialisation only when AUTPLT=false- none defined on entry
	if(AUTPLT) goto 89
	linetype=0		!continuous line
	narrow=0	!number of arrows added
	nline=0	!number of free lines added
c========for now use nhline/nvline from TPOS even if AUTPLT=false
c	nhline=0	!number of horizontal lines added
c	nvline=0	!number of vertical lines added
c=================
	ntext=0	!number of extra text strings
	if(cbig.lt.2.) cbig=2.5
	if(ifont.lt.0) ifont=2	! Simplex, upright on 0-11 scale (manual, p57)
	ifont2=ifont	!copy for common/dmenu/
	allpnt=.true.
	if(bigplot) allpnt=.false.
c	itit=0		!no title yet
	ifsav=ifont		!save input value (if there is one)
	thbig=1.0		!line thickness factor for whole page
	csize=cbig
	thick=thbig
	thfacsml=0.6		!line thickness factor for small plots
	s=0.8*csize		!default symbol size
	if(syms(1).le.0.) then
	   do 9 i=1,ndimd
9	   syms(i)=s
	endif
c
	do i=1,10
	  ifnt(i)=ifont
	  csfac(i)=1.0
	enddo
	do i=11,30
	  ifnt(i)=ifont
	  csfac(i)=1.3    !larger for newtext
	enddo
	do 81 i=1,100
	  iangle(i)=0
	  idraw(i)=-2		!until defined
c	  rx(i)=0.0		!NB world coord may be neg so cant tell if defined yet
81	continue
	idraw(28)=1		!c-jump logo
	idraw(29)=1		!v-jump logo
	do i=1,10
	   iltype(i)=0		!continuous line for extra lines
c========for now use nhline/nvline from TPOS even if AUTPLT=false
c	   ilhtype(i)=0		!continuous line for horizontal lines
c	   ilvtype(i)=0		!continuous line for vertical lines
c	   ihlinrel(i)=0		!=1 line pos rel to line 1
c	   ivlinrel(i)=0		!=1 line pos rel to line 1
	enddo
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
	  ilth(i)=ilt
	enddo
c	ilth(21)=25	!axes (and axis labels at present!)
c	ilth(1)=10		!*****test: less than 15 gives problems!
 	ilth(22)=25	!frame
	ilth(26)=10	!param value text
	ilth(27)=15	!symbols
	ilth(28)=20	!C-jump logo
	ilth(29)=20	!V-jump logo
	ilth(30)=12	!SD bars
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
c	lastrow=2			!NB args of SCROLL=integer
c	lrow=int2(lastrow)	!but args of LOCATE are int*2
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
	if(ncurvd.gt.0)call MINMAX2(xval,yval,ndat,xmin1,xmax1,ymin1,
     & ymax1,logx,logy,ndimd,ncurvd,icurvd,ndv1)
	if(ncurvd.le.0)call MINMAX2(xcal,ycal,ncal,xmin1,xmax1,ymin1,
     & ymax1,logx,logy,ndimc,ncurvc,icurvc,ndc1)
c
c  If iscal.ne.0 use FIXAX to get new xmin,xtic etc
	if(iscal.eq.0) goto 108
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
	   itx=1	!default axis tic orientation in LAXES2
	   ity=1
	endif
	if(ntx.eq.0) ntx=5		!label every 5th tic
	if(nty.eq.0) nty=5		!label every 5th tic
	call FIXAX(xmin1,xmax1,xmin,xmax,xtic,0)	!always non-log
	call FIXAX(ymin1,ymax1,ymin,ymax,ytic,0)
c	if(debug()) then
c	  print 821,xmin1,xmax1,ymin1,ymax1,xmin,xmax,ymin,ymax
c821	  format(' xmin1,xmax1,ymin1,ymax1 = ',4g13.6,/,
c     &  ' xmin,xmax,ymin,ymax = ',4g13.6)
c	endif
c  ISCAL=2 if input values of xmin,xmax only to be used; others internally set
c  ISCAL=3 if input values of ymin,ymax only to be used; others internally set
c  ISCAL=4 if input values of xmin,xmax,ymin,ymax to be used; rest internal
	if(iscal.eq.2.or.iscal.eq.4) then
	   xmin=xmins		!restore input value
	   xmax=xmaxs		!restore input value
	endif
	if(iscal.eq.3.or.iscal.eq.4) then
	   ymin=ymins		!restore input value
	   ymax=ymaxs		!restore input value
	endif
	if(sqrty) then
	   ytic=1.0
	   nty=1
	   if(ymax.gt.49.) then
		ytic=4.0
		nty=4
	   endif
	   if(ymin.lt.0.) ymin=0.
	endif
	if(iscal.ge.0) then
	   xcross=xmin		!crossing point for axes
	   ycross=ymin
	endif
	if(IVplot) then
	   xtic=50.		!mV (or tics are too close with short X axis)
	   itx=0		!tics central
	   ity=0
	   xcross=0.
	   ycross=0.
	   landscap=.false.
	   doframe=.false.
	endif
	if(iy1v.ne.-1000) then
c	   iy1c=6350         !initial positions for jump logos
c	   iy2c=6500         !that do not overlap title
c	   iy1v=6300
c	   iy2v=6150
	   iy1c=5500         !initial positions for jump logos
	   iy2c=5640         !that do not overlap title
	   iy1v=5460
	   iy2v=5320
	endif
c	xoff1=0.0		!for drawing jump logo- now input
108	continue
c
	ivxlo=0		! for VIEWPORT
	ivxhi=10000
	ivylo=0
	ivyhi=7484
	if(ixlo.ge.0) goto 1082
1083	if(landscap) then
c	   ixlo=1400		! set
c	   if(calbarY) ixlo=500	!make wider
c	   ixhi=9500		! display
c	   iylo=1600		! location
c	   iyhi=6500		! screen
c Make iyhi,ixhi lower to leave room for dialog box (also for narrow
c vertical box ar RHS of screen?)
c -proportions are close to golden section = 1:1.618
	   ixlo=1300		! set
	   if(calbarY) ixlo=500	!make wider
	   ixhi=8200		! display
	   iylo=1500		! location
	   iyhi=5700		! screen
	else
	   ixlo=3250            !portrait
	   ixhi=6750	!axis length x=3500 y=4200
	   iylo=1500
	   iyhi=5700
	   if(ycross.gt.ymin) then
	      iylo=1000 	!make lower as no need for room for numbers/title
	      iyhi=6000
		ixlo=3000	!make bigger to match
		ixhi=7000
	   endif
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
	if(iscal.ne.0.and.ilog.ne.0) goto 133	!scale for logs first
306	continue
	redrawn=.true.	!graph will have xtitle etc drawn in LAXES2
c	if(debug()) then
c	print 82,xmin,xmax,ymin,ymax,
c     & xtic,ytic,xcross,ycross,lb(4),rescal,
c     & titlex,titley,csize,ilabel,inumx,inumy,ivxlo,ixlo,
c     & logx,logy,sqrty,doframe
c82	format(' xmin,xmax,ymin,ymax= ',4g13.6,/,
c     & ' xtic,ytic,xcross,ycross,Lb(4),rescal= ',4g13.6,i4,l4,/,
c     & ' titlex,titley= ',a40,/,1x,a40,/,
c     & ' csize,ilabel,inumx,inumy='g13.6,3i8,/,
c     & ' ivxlo,ixlo= ',2i8,/,/
c     & ' logx,logy,sqrty,doframe = ',4l4)
c	pause '2'
c	endif		!end debug
c	if(iscal.eq.0) goto 309		!use input xcross,ycross
	if((.not.croset).and.(.not.ivplot)) then
	   xcross=xmin		!crossing point for axes
	   ycross=ymin
	endif
c Scale now
	if(.not.sqrty) call SCALE(xmin,xmax,ymin,ymax)
	if(sqrty) call SCALE(xmin,xmax,sqrt(ymin),sqrt(ymax))
c	if(debug()) then
c	   print 777,xmin,xmax,ymin,ymax
c777	   format(' after SCALE xmin,...,ymax = ',4g13.6)
c	   pause '4'
c	endif
	call ERASCR
104	continue			!RETURN HERE TO DO PLOT
c Fill background colour
	if(.not.mono) then
	    call FILLWIN(0,55,639,387,icol(71))		!graph area
	    call FILLWIN(0,388,639,479,1)			!dialog area
	    call COLTYP1(15)		!bright white
	endif
	call HLINE(0,639,54,15)		!line to mark bottom of data area
	call HLINE(0,639,389,15)		!line to mark top of data area
	call OPENDIALOG(1,icf,.true.)		!draw dialog box #1
	call OPENDIALOG(2,icf2,.true.)		!draw dialog box #2
	call WDIALOG(2,'F1=HELP',ict)
	call WDIALOG(2,'F2=HELP INDEX',ict)
c	if(debug()) pause '5'
c
	if(ilabel.lt.0) then
	   titlex='   X   '
	   titley='   Y   '
	   if(logx) titlex='log(X) '
	   if(logy) titley='log(Y) '
c	   if(ilog.eq.4) titley='logit(Y) '
	   if(ilog.eq.4) titley='log[(Y-Y(0)]/(Ymax-Y))'
	endif
c NB FRAME call is now in LAXES
c Last line of param is so that position of text strings (for axis
c labels etc) can be defined when they are, at the first call,
c calculated internally. Colours set internally in LAXES.
	call LAXES2(xmin,xmax,ymin,ymax,xtic,ytic,xcross,ycross,
     & ntx,nty,itx,ity,itlen,titlex,titley,xtitle,ytitle,ilabel,idev,
     & csfac(7),csfac(8),csfac(9),csize,ifnt(7),ifnt(8),ifnt(9),
     & thick,ilth,inumx,inumy,0.0,ilog,doframe,mono,icol,xcalib,ycalib,
     & rx,ry,iangle,ixjus,iyjus,idraw,rxbox,rybox,
     & numbx,numby,cnumx,cnumy,cexpx,cexpy)
	call COLTYP1(15)		!bright white
c
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
	   call LINWID(int4(ilth(25)))
	 endif
	if(.not.mono) call COLTYP1(icol(25))		!colour for title
	call JUSTIFYSTRING(ix,iy,title1(1:nl),iangle(10),isize,
     & ixjus(10),iyjus(10))
c draw the box round title if necessary- could call TEXTBOX again, but
c position already defined so quicker to call FRAME- but safer to use
c former, in case graph size changed
c	if(idraw(10).eq.-1) call FRAMER(rxbox(1,10),rybox(1,10))
	if(idraw(10).eq.-1) call TEXTBOX(ix,iy,title1(1:nl),iangle(10),
     & isize,ixjus(10),iyjus(10),rxbox(1,10),rybox(1,10),1)
	call SETFONT1(ifont)		!reset
	call SETCSIZE(csize,isize)    !reset
1361	continue		!end of title
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
c	    if(ifitype.eq.4) ry(6)=ymax2-0.2*(ymax2-ymin)  !lower (for jump logos)
	    if(ifitype.eq.4) ry(6)=ymax2-0.5*(ymax2-ymin)  !lower (for jump logos)
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
	   call LINWID(int4(ilth(26)))
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
c85	   format(/,' TEXT= ',/,5X,30a1)
	   if(idev.ge.5) then
	      call LOADVTB('2'//'W'//char(lth(i+30)))
	   else
	      call LINWID(int4(ilth(i+30)))
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
c
c DRAW ARROWS IF ANY
	if(narrow.gt.0) then
	  do 222 i=1,narrow
	   if(idev.ge.5) then
	      call LOADVTB('2'//'W'//char(lth(i+50)))
	   else
	      call LINWID(int4(ilth(i+50)))
	   endif
c	   if(.not.mono) call COLTYP1(icol(i+50))		!colour for arrow(i)
c Call to ARROW includes colour (with Hgraph numbering)
	   if(mono) then
		ic=0
	   else
		ic=15-icol(i+50)		!convert to Hgraph numbering
		if(ic.eq.15) ic=-1	!black (for icol=0)
	   endif
         call ARROW(xb(i),yb(i),xe(i),ye(i),ic,2,.true.)
222	  continue
	endif
c
c DRAW LINES IF ANY
	if(nline.gt.0) then
	   do 224 i=1,nline
	   if(idev.ge.5) then
	      call LOADVTB('2'//'W'//char(lth(i+60)))
	   else
	      call LINWID(int4(ilth(i+60)))
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
	      call LINWID(int4(ilth(i+70)))
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
	      call LINWID(int4(ilth(i+81)))
	     endif
	     if(.not.mono) call COLTYP1(icol(i+81))		!colour for line(i)
	     call MOVETO(xvline(i),yvlb(i),0,ilvtype(i))
	     call MOVETO(xvline(i),yvle(i),1,ilvtype(i))
	   enddo
	endif
c
c Draw jump logos if req (need to keep, and queue, the definitions eg
c whether to draw, ylevel for each and height of deflections), and
c for V-jump better read in voltages to make deflections proportional to them
c	if(debug()) print 802,ivplot,ncjump,iy1c,idraw(28),
c     &	nvjump,iy1v,idraw(29)
c802	format(' ivplot,ncjump,iy1c,idraw(28),nvjump,iy1v,idraw(29)=',/,
c     & l4,6i8)
	if(.not.IVplot) then
c 28=jump bar (logo) for C-jumps
c 29=jump bar (logo) for V-jumps
	   if(ncjump.gt.0.and.iy1c.ge.0.and.idraw(28).eq.1) then
	      if(.not.mono) call COLTYP1(icol(28))		!colour for c-logo
		call JLOGO(xmin,xmax,ncjump,t1c,t2c,
     &           xoff1,idev,LTH(28),1,iy1c,iy2c)
	   endif
	   if(nvjump.gt.0.and.iy1v.ge.0.and.idraw(29).eq.1) then
	      if(.not.mono) call COLTYP1(icol(29))		!colour for v-logo
		call JLOGO(xmin,xmax,nvjump,t1v,t2v,
     &         xoff1,idev,LTH(29),1,iy1v,iy2v)
	   endif
	endif
c
c
c PLOT THE GRAPH
c
	barleng=0.01*(xmax-xmin)	!bar on top/bottom of error bars
c Control which curves are plotted via Icurvd() now
	if(ncurvd.le.0) goto 303	!calc curve only
	nbadd=0
	x0=1.e-37		!smallest value for taking logs
c
	do 50 j1=1,ncurvd		!plot ncurvd data sets
	jflag=0
	j=icurvd(j1)		!data set to be plotted (col # where data starts)
	if(.not.mono) call COLTYP1(icol(j))	 !col for jth data set: symbols+line+SD
c	if(debug()) print 805,j1,j,ndat(j),ijoin(j),isym(j)
c805	format(' j1,j,ndat(j),ijoin(j),isym(j)= ',5i5)
	linetype=ijoin(j)		!line to join data points
	iud=1				!draw line between points
	if(linetype.eq.-1) iud=0	!don't
	nint=0			!points for interpolation
c  Definitions to cope with ndat>2048; define ngr(j)=number of cols of
c Yval(i,j) occupied by each of the data graphs (all =1 in normal case
c where ndat(j)=<2048).  If number of points > 2048 then display only
c every ndelt-th point -must first check how many values lie within the
c current xmin,xmax,ymin,ymax range by call to COUNTDAT.
	if(ndat(j).le.2048.or.allpnt) then
	   ndelt=1
c	   ngr=1
c	   nlast=ndat(j)
	else
	   ndelt=1
	   call COUNTDAT(j,ndat,ndelt,xval,yval,logx,logy,logity,sqrty,
     &    y0,yinf,ymin,ymax,x0,xmin,xmax,ndimd,NDISP,ndv1)
	   if(ndisp.gt.2048) then	!calc ndelt>1
		ndelt=1 + (ndisp-1)/2048
	   endif
	endif
	if(ndelt.gt.1) then
c	   call LOCATE(0,0)
c	   print 801,ndelt
c801	   format('&Every nth point plotted, n = ',i3)
	   call INTCONV(ndelt,cnum1)
	   if(ndelt.eq.2) then
		cnum1(1:3)='2nd'
	   else if(ndelt.eq.3) then
		cnum1(1:3)='3rd'
	   else
		cnum1=CHARNB(cnum1)//'th'
	   endif
c	   call OPENDIALOG(2,icf2,.true.)		!draw dialog box #2
	   call WDIALOG(2,'Every '//CHARNB(cnum1)//' point',12)
	endif
c
c NB 09/06/91 11:50am seem to get some problems with line thickness if have
c many points, and solvable if line thickness not changed for every point
c Change is only needed if want BOTH symbols (other than point=0) AND
c data points joined with line.  Mostly both are not needed and if so load
c line thickness here, before the 'do 215' loop for each point
	 tload=.true.					!thickness to be loaded in loop
	 if((iud.eq.1).and.(isym(j).eq.0)) then
	    if(idev.ge.5) then
	      call LOADVTB('2'//'W'//char(lth(j)))      !data line
	    else
	      call LINWID(int4(ilth(j)))
	    endif
	    tload=.false.					!thickness already loaded
	 else if(isym(j).ne.0.and.ijoin(j).eq.-1) then
	    if(idev.ge.5) then
	      call LOADVTB('2'//'W'//char(lth(27)))      !symbol
	    else
	      call LINWID(int4(ilth(27)))
	    endif
	    tload=.false.					!thickness already loaded
	 endif
c Put do 215 loop + interp drawing into DRAWDAT, to draw curve # j
	   if(j.eq.1.and.ntrace.gt.1) then
	      call MULTRACE(yval,mono,xmin,xmax,ymin,ymax,idev,
     &      ndat(1),ijoin(1),icol(1),ilth(1),ndv1,ndimd,ntrace,
     & 	ytsep,reclen,idraw(1),icol(71),2)	!draw only, no clear
c     &	ytsep,icol(71),2)		!draw only, no clear
	   else
	      call DRAWDAT(j,xval,yval,ndelt,isdev,
     &       logity,logx,logy,sqrty,y0,yinf,x0,mono,
     &       xmin,xmax,ymin,ymax,iud,tload,idev,
     &       ndat(j),ijoin(j),icol(j),isym(j),syms(j),lth(j),ilth(j),
     &       lth(27),ilth(27),lth(30),ilth(30),barleng,interp,npint,
     &       Xint,Yint,nint,yp1,ypn,Y2int,ndv1,ndimd)
	   endif
50	continue		!end of sets loop
c
c	 do 215 i=1,ndat(j),ndelt
c	if(isdev.ge.0.and.weight(i,j).eq.0.) goto 215	!skip points with w=0
c	 xv=xval(i,j)
c	 yv=yval(i,j)
c	 if(logity) then
c	   yv=(yv-y0)/(yinf-yv)	! Hill scale
c	   if(yv.gt.x0) then
c		yv=alog10(yv)
cc	   else
cc	      nbadd=nbadd+1
cc	      if(nbadd.le.25) xdbad(nbadd)=xv
cc	      if(nbadd.le.25) ydbad(nbadd)=yv
c	   endif
c	 endif
c	 if(logx) then
c	   if(xv.gt.x0) then
c		xv=alog10(xv)
c	   else
c		xv=2.*xmax		!so not plotted
cc	      nbadd=nbadd+1
cc	      if(nbadd.le.25) xdbad(nbadd)=xv
cc	      if(nbadd.le.25) ydbad(nbadd)=yv
c	   endif
c	 endif
c	 if(logy) then
c	   if(yv.gt.x0) then
c		yv=alog10(yv)
c	   else
c		yv=2.*ymax		!so not plotted
cc	      nbadd=nbadd+1
cc	      if(nbadd.le.25) xdbad(nbadd)=xv
cc	      if(nbadd.le.25) ydbad(nbadd)=yv
c	   endif
c	 endif
cc	 if(((logy.or.logity).and.yv.le.x0).or.(logx.and.xv.le.x0)) then
cc	   nbadd=nbadd+1
cc	   xdbad(nbadd)=xv
cc	   ydbad(nbadd)=yv
cc	   if(nbad.eq.1) call BELL(1)
cc	   call LOCATE(0,0)		!row=0,col 0
cc	   print 2161,nbad,xv,yv
cc2161	   format(' OBSERVED VALUE < 1.E-37 FOR LOG: n,xv,yv= ',i4,2g13.6)
cc	 endif
c	 if(sqrty.and.yv.ge.0.) yv=sqrt(yv)
cc  xv,yv now defined but don't plot if outside min,max range
c	 if(xv.lt.xmin.or.xv.gt.xmax) goto 215
c	 if(yv.lt.ymin.or.yv.gt.ymax) goto 215
cc Define arrays for interpolation (whether INTERP true or not, in case req below)
cc	 if(nint.le.255) then
c	 if(nint.le.2047) then
c	   nint=nint+1
c	   xint(nint)=xv
c	   yint(nint)=yv
c	 endif
c	 if(jflag.eq.0) call MOVETO(xv,yv,0,0)   !move to 1st point in range
c	 if(iud.eq.1.and.tload) then
c	    if(idev.ge.5) then
c	      call LOADVTB('2'//'W'//char(lth(j)))      !data line
c	    else
c	      call LINWID(int4(ilth(j)))
c	    endif
c	 endif
c	 if(jflag.gt.0) then
c	   if(interp.and.nint.le.2048) then
c	     call MOVETO(xv,yv,0,linetype)   !join with line below
c	   else
c	     call MOVETO(xv,yv,iud,linetype)   !join with line from last point
c	   endif
c	 endif
cc need thinner lines to draw symbols? and for error bars?
c	 if(isym(j).ne.0.and.tload) then
c	    if(idev.ge.5) then
c	      call LOADVTB('2'//'W'//char(lth(27)))      !symbol thickness
c	    else
c	      call LINWID(int4(ilth(27)))
c	    endif
c	 endif
c	 ic=icol(j)
c	 if(mono) ic=15		!bright white
c	 call SYMBOL(xv,yv,isym(j),syms(j),ic)     !symbol if isym>0
c	 if(.not.mono) call COLTYP1(icol(j))	!restore colour after SYMBOL
cc plot error bars
c	 if(isdev.eq.1) then
c	   if(idev.ge.5) then
c	      call LOADVTB('2'//'W'//char(lth(30)))
c	   else
c	      call LINWID(int4(ilth(30)))
c	   endif
c	   sdmax=0.5*abs(ymax-ymin)	!largest sd to be plotted
c	   if(logy) sdmax=0.5*abs(10.**ymax - 10.**ymin)
c	   if(weight(i,j).gt.(1.0/sdmax**2)) then
c	      sdev=sqrt(1.0/weight(i,j))
c		yv1=yv-sdev
c		if(logy) yv1=10.**yv-sdev
c		if(logy.and.yv1.lt.1.e-37) goto 22
c		if(logy) yv1=alog10(yv1)	!yv already logged
c		call MOVETO(xv-barleng,yv1,0,0)
c		call MOVETO(xv+barleng,yv1,1,0)
c		call MOVETO(xv,yv1,0,0)
c22		yv1=yv+sdev
c		if(logy) yv1=alog10(10.**yv+sdev)
c		call MOVETO(xv,yv1,1,0)
c		call MOVETO(xv-barleng,yv1,0,0)
c		call MOVETO(xv+barleng,yv1,1,0)
c		call MOVETO(xv,yv,0,0)
c	   endif
c	 endif
c	 jflag=1
c215	 continue
cc
cc Add interpolated points here if interp=true (so they get plotted!)
c	if(interp) then
cc	   print 700,6
c	   call SPLINE(Xint,Yint,nint,yp1,ypn,Y2int)
c	   call MOVETO(xint(1),yint(1),0,0)   !move to 1st point in range
c	   x=float(n+1)
c	   do 4035 i=1,nint-1
c	   do 4035 k=1,n
c	   xin=xint(i) + (xint(i+1)-xint(i))*float(k)/x	!divide the interval
c	   klo=0		!full bisection for now
c	   call SPLINT(Xint,Yint,Y2int,nint,klo,khi,xin,yout)
c	   if(iud.eq.1) then
c	     if(idev.ge.5) then
c	      call LOADVTB('2'//'W'//char(lth(j))) 	!data line
c	     else
c	      call LINWID(int4(ilth(j)))
c	     endif
c	   endif
cc if interp then do not join lines here, but join interp points below
c	   call MOVETO(xin,yout,iud,linetype)   !join with line from last point
c	   if(idev.ge.5) then
c	      call LOADVTB('2'//'W'//char(lth(27)))
c	   else
c	      call LINWID(int4(ilth(27)))
c	   endif
c	   ic=icol(j)
c	   if(mono) ic=15		!bright white
c	   call SYMBOL(xin,yout,isym(j),syms(j),ic)     !symbol if isym>0
c4035	   continue
c	endif
c50	continue		!end of sets loop
	if(nbadd.gt.0) call BELL(2)	!add option to print bad values?
c
c CALC CURVE (before rescale options)
	if(ncurvc.gt.0) goto 303	!do calc curve before rescale option
c
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
	if(.not.bigplot) Lb(14)=0
151	continue
	text(1)='1: RESCALE     '
	text(2)='2:  ZOOM      '
	text(3)='3: GRAPH SHAPE'
	text(4)='4: GRAPH AXES '
	text(5)='5:POSH OPTIONS'
	text(6)='6: GIVE TITLE '
	text(7)='7: PLOT NOW   '
c	if(autplt) then
c	   text(8)='8: STORE PLOT '
c	else
	   text(8)='8: QUEUE PLOT '
c	endif
	text(9)='9:END DISPLAY'
	text(10)='10:  REDRAW    '
	text(11)='+:X AXIS LABEL'
	text(12)='-:Y AXIS LABEL'
	text(13)='*:INTERPOLATE '
	if(allpnt) then
	   text(14)='/:OMIT POINTS '
	else
	   text(14)='/: ALL POINTS  '
	endif
	text(15)='.: MORE OPTIONS  '
c	subroutine DRAWBOX(nbline,Lbox,text,csize,ifont)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
c152	ikey=nkey()
152	call CKEY(ch,ikey)
	if(ikey.lt.-2.or.ikey.gt.15) goto 151
	if(ikey.eq.-1) then	!F1 key
	   call VHELP(1)
	   goto 152		!another menu choice
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-2)
	   goto 152		!another menu choice
c	   iret=1
c	   goto 237		!help
	endif
	if(ikey.eq.14) then
	   allpnt=.not.allpnt
	   goto 306		!redraw
	endif
c
c	goto(11,140,301,991,221,401,403,155,999,306) ikey
	goto(11,140,40,13,221,991,155,301,999,306,401,401,403,152,154)ikey
c
c MORE OPTIONS
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
c	  call SETCOLS(mono,icol,autplt,plot)
	  mono=.false.
c	  call SETCOLS(mono,icol,.false.,.false.)
	  call SETCOL(mono,icol,.false.,.false.,0)
	else if(ikey.eq.3) then
	  mono=.false.
	  call SETCOL(mono,icol,.false.,.true.,0)
	else if(ikey.ge.4.and.ikey.le.7) then
	   if(ikey.eq.4) imode=2		!for graph colours
	   if(ikey.eq.5) imode=11		!increase line thickness
	   if(ikey.eq.6) imode=12		!decrease line thickness
	   if(ikey.eq.7) imode=1	      !set graph line thickness
	   call SETATT(narrow,nline,ntext,isdev,
     &   ilth,icol,icol0,icol1,icol2,ncurvd,icurvd,ncurvc,icurvc,
     &   ndimd,ndimc,ifitype,imode)
	endif
	goto 306	!redraw
c
c READ IN TITLE
c=========use HGSTRING here
c With plotter on, MOVE goes only to plotter. To get 'Enter title..'
c at right position on screen need sep MOVE with plotter off!
c Get label but do not draw until decided if plot is to be queued
991	continue
	call DCMENU(-6,4,Lb,text,0,0)	!delete box 4 only
	Lb(6)=-1
	call DCMENU(-6,4,Lb,text,icol1,icol2)	!draw box 4 only (italic)
	blank=itit.eq.0
c	subroutine TDIALOG(ibox,query,title,nlen,blank,ict)
c	call TDIALOG(1,'Enter title for plot:',title1,44,blank,ict)
c===TDIALOG replace by all down to HGSTRING
	call SETFONT1(ifnt(10))
	call SETCSIZE(csfac(10)*csize,isize)	!this defines ISIZE
	nl=NBLANK1(title1)
	ymin2=ymin
	ymax2=ymax
	if(sqrty) ymin2=sqrt(ymin)
	if(sqrty) ymax2=sqrt(ymax)
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
	else
	  ix=iscrx(rx(10))
	  iy=iscry(ry(10))
	endif
	if(.not.mono) call COLTYP1(icol(25))		!colour for title
	if(itit.eq.1) then	!delete existing title first
	   call COLTYP1(icol(71))		!colour for title
	   call JUSTIFYSTRING(ix,iy,title1(1:nl),iangle(10),isize,
     &	 ixjus(10),iyjus(10))
	endif
	call HGSTRING(ix,iy,title1,ifont,iangle(10),isize,
     &   ixjus(10),iyjus(10),icol(25),icol(71),44,blank)
c=========
	itit=1		!for plotq
c==	idraw(10)=-2	!so title inserted at default pos when graph redrawn
	call flush(7)
c	goto 306	!redraw so title seen (and its position defined)
c	goto 151	!main menu
	goto 152
c
c Section to read in new axis labels
401	continue
	call DCMENU(-ikey,4,Lb,text,0,0)	!delete box #ikey only
	Lb(ikey)=-1
	call DCMENU(-ikey,4,Lb,text,icol1,icol2)	!draw box #ikey only (italic)
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row=lrow, col 0
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
	   ix=ISCRX(rx(n))
	   iy=ISCRY(ry(n))
	   call SETCSIZE(csfac(n)*csize,isize)	!this defines ISIZE
	   call COLTYP1(icol(71))	!delete existing title
	   nlx=NBLANK1(oldtext)
	   if(nlx.gt.0) then
	      call JUSTIFYSTRING(ix,iy,oldtext(1:nlx),iangle(n),isize,
     &		ixjus(n),iyjus(n))
	   endif
	   redrawn=.false.	!until hit REDRAW
	   if(ikey.eq.11) then
	      call HGSTRING(ix,iy,titlex,ifont,iangle(n),isize,
     &	 ixjus(n),iyjus(n),icol(23),icol(71),40,.true.)	!40=size of titlex
	   else if(ikey.eq.12) then
	      call HGSTRING(ix,iy,titley,ifont,iangle(n),isize,
     &	 ixjus(n),iyjus(n),icol(23),icol(71),40,.true.)
	   endif
	   idraw(n)=-2	!so new label drawn at default posn in LAXES
	endif
	ilabel=1		!so internal default labels not used
c	goto 151	!main menu
	goto 152
c
c INTERPOLATION SECTION. Interpolate first 256 points of display- now 2048
403	continue
	n=5
	yp1=0.
	ypn=0.
	interp=.true.
c	call LOCATE(0,0)
c	print 4036
c4036	format(
c     &'&Use defaults (interp 5 points, end slopes=0) [Y] = ')
c	read 101,ans
	ans='Y'
 	call DEFOLTa(ans,defolt)
	call QDIALOG(1,'Use defaults (interp 5 points, end slopes=0)'
     &	,defolt,ict,cans)
	call GETINPa(cans,ans)
	if(ans.eq.'N') then
c	   call LOCATE(0,0)
c	   print 4032
c4032	   format(
c     &'&No of points between each existing point (-1=no interp) [5] = ')
c	   read 3,n
c	   if(n.eq.0) n=5
	   n=5
 	   call DEFOLTi(n,defolt)
	   call QDIALOG(1,
     &'# of points between each existing point (-1=cancel)',
     &	defolt,ict,cans)
	   call GETINPi(cans,n)
	   npint=n
	   if(npint.lt.0) then
		interp=.false.
		goto 152
	   endif
	   yp1=0.0
	   ypn=0.0
	   call DEFOLT2r(yp1,ypn,defolt)
	   call QDIALOG(1,
     & 'Slope at each end (enter u if both unknown): s1,s2',
     &   defolt,ict,cans)
	   call GETINP2r(cans,yp1,ypn)
c GETINP2r fixed so entering 'u' or 'U' returns 1.e35, 1.e35
	endif
	if(ncurvd.gt.1) then
c	   print 700,2
	   goto 306		!redraw completely
	else			!draw in interp points now
c	  print 700,3
	  j=icurvd(1)
	  call SPLINE(Xint,Yint,nint,yp1,ypn,Y2int)
	  call MOVETO(xint(1),yint(1),0,0)   !move to 1st point in range
	  x=float(npint+1)
c	  print 700,4
	  do 4033 i=1,nint-1
	  do 4033 k=1,npint
	   xin=xint(i) + (xint(i+1)-xint(i))*float(k)/x	!divide the interval
   	   klo=0		!full bisection for now
	   call SPLINT(Xint,Yint,Y2int,nint,klo,khi,xin,yout)
	   call MOVETO(xin,yout,iud,linetype)   !join with line from last point
	   ic=icol(j)
	   if(mono) ic=15		!bright white
	   call SYMBOL(xin,yout,isym(j),syms(j),ic)     !symbol if isym>0
4033	  continue
c	  print 700,5
	  goto 152
	endif
c
c ZOOM section
140	continue
	interp=.false.	!must redo interpolation after rescaling
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
	   call CKEY(ch,ikey)
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
	   call SYMBOL(xmin,ymin,4,csize,12)   !mark bottom left with red diamond
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row lrow,col 0
c	   print 142
c142	   format('&Mark TOP RIGHT corner of new display with cursors  ')
	   call WDIALOG(1,
     &      'Mark TOP RIGHT corner of new display with cursors',ict)
	   ix1=ix1+2000
	   iy1=iy1+1500
	   if(ix1.gt.10000) ix1=10000
	   if(iy1.gt.7484) iy1=7484
	   call PUTCUR(ix1,iy1)
c         ch=FNDCUR(ix1,iy1)
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
	idraw(1)=-2		!calibration bars drawn at default position and length
	do 34 i=31,80
34	idraw(i)=-2		!all numbers at default posn
	goto 306		!redraw
c
c RESCALE section:
11	continue
	interp=.false.	!must redo interpolation after rescaling
	nbox=10	!number of boxes for DCMENU
	call SETLBOX(10,Lb,1)
113	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
111	continue
	if(IVplot.or.iy1v.eq.-1000) then
	   Lb(8)=0
	   Lb(9)=0
	endif
	text(1)='1. Xmin,Xmax  '
	text(2)='2. Ymin,Ymax  '
	if(calbarX.and.calbarY) then
	   text(3)='3.X-bar, Y-bar'
	   text(4)='4.FIX CAL BARS'
	else if(calbarX.and.(.not.calbarY)) then
	   text(3)='3.X-bar, Y-tic'
	   text(4)='4.FIX CAL BAR '
	else if(calbarY.and.(.not.calbarX)) then
	   text(3)='3.X-tic, Y-bar'
	   text(4)='4.FIX CAL BAR '
	else
	   text(3)='3.X-tic,Y-tic '
	   text(4)='4. X,Y crossing'
	endif
	text(5)='5. TIC LAYOUT '
	Lb(6)=0
	Lb(7)=0
	if(ncjump.eq.0.and.nvjump.eq.0) then
	   Lb(8)=0
	   text(9)='9. DEFINE t=0 '
	else
	   text(8)='8.ON-JUMP t=0 '
	   text(9)='9.OFF-JUMP t=0'
	endif
	text(10)='10. REDRAW    '
c
c	call NUMSET			!set num lock on ready for response
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
112	call CKEY(ch,ikey)
	if(ikey.eq.-1) then	!F1 key
	   call VHELP(3)
	   goto 112		!another menu choice
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-2)
	   goto 112		!another menu choice
	endif
c	if(ikey.eq.-1) then	!F1 key
c	   iret=3
c	   goto 237		!help
c	endif
c After rescale that affects numbering on axes (xmin,xmax,xtic,xcross) must
c redraw all numbers on the affected axis at their default positions (idraw=-2)
c Also do axis labels at default posn??
	idraw(6)=-2       !Parameter values drawn at default posn
	idraw(10)=-2	!so title, if present, drawn at default posn
c Calib bars drawn at default position, but current length
c (if this is defined ie idraw(1).ne.-2), otherwise default length
	if(calbar.and.(ikey.eq.1.or.ikey.eq.2.or.ikey.eq.3)) then
	   if(idraw(1).ne.-2) idraw(1)=-3
	endif
	if((ikey.ge.1.and.ikey.le.4).or.ikey.eq.8.or.ikey.eq.9) then
	   idraw(8)=-2       !X axis label drawn at default posn
	   do 30 i=31,55
30	   idraw(i)=-2
	   idraw(9)=-2       !Y axis label drawn at default posn
	   do 31 i=56,80
31	   idraw(i)=-2
	endif
c
c	goto(16,17,18,19,13,40,14,25,25,115) ikey
	goto(16,17,18,19,14,112,112,25,25,115) ikey
	goto 11
c
14	continue	!FIX TICS
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
	call CKEY(ch,ikey)
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
c If xcross=xmin,ycross=ymin at present then set croset=.false. so xcross
c reset to new xmin to keep them equal
	if(equal(xmin,xcross).and.equal(ymin,ycross)) croset=.false.
3041	continue
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row=lrow,col 0
c	print 304,xmin,xmax
c304	FORMAT('&Xmin, Xmax [',f9.2,1x,f9.2,'] = ')
c	call INPUT2r(xmin,xmax)
	call DEFOLT2r(xmin,xmax,defolt)
	call QDIALOG(1,'Xmin, Xmax',defolt,ict,cans)
	call GETINP2r(cans,xmin,xmax)
	if(xmax.le.xmin) goto 3041
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
	if(equal(xmin,xcross).and.equal(ymin,ycross)) croset=.false.
3051	continue
	call DEFOLT2r(ymin,ymax,defolt)
	call QDIALOG(1,'Ymin, Ymax',defolt,ict,cans)
	call GETINP2r(cans,ymin,ymax)
	if(ymax.le.ymin) goto 3051
	call DCMENU(-2,4,Lb,text,0,0)	!delete box 2 only
	Lb(2)=-1
	call DCMENU(-2,4,Lb,text,icol0,icol2)	!draw box 2 only (italic)
	goto 112
c
18	continue
	if(.not.calbar) then
	   call DEFOLT2r(xtic,ytic,defolt)
	   call QDIALOG(1,'Xtic sep, Ytic sep',defolt,ict,cans)
	   call GETINP2r(cans,xtic,ytic)
	   if((xtic.lt.0.0).or.(ytic.lt.0.0)) goto 18
	else
c	   rx(1),ry(1)=origin of X-calibration bars (world units),
c	   rx(2),ry(2)=end of X calibration bars
c	   rx(3),ry(3)=origin of Y-calibration bars (world units),
c	   rx(4),ry(4)=end of Y calibration bars
c 	   Horizontal and vertical so ry(1)=ry(2), and rx(3)=rx(4)
	   if(calbarX.and.calbarY) then
	     call DEFOLT2r(xcalib,ycalib,defolt)
	     call QDIALOG(1,'Length of calibration bars: X,Y',
     &	defolt,ict,cans)
	     call GETINP2r(cans,xcalib,ycalib)
	   else if(calbarX.and.(.not.calbarY)) then
	     call DEFOLT2r(xcalib,ytic,defolt)
	     call QDIALOG(1,'Length of X-bar, Y-tic sep',
     &	defolt,ict,cans)
	     call GETINP2r(cans,xcalib,ytic)
	   else if(calbarY.and.(.not.calbarX)) then
	     call DEFOLT2r(xtic,ycalib,defolt)
	     call QDIALOG(1,'X-tic sep, length of Y-bar',
     &	defolt,ict,cans)
	     call GETINP2r(cans,xtic,ycalib)
	   endif
	   rx(2)=rx(1)+xcalib
	   ry(4)=ry(3)+ycalib
	   call FIXCALIB(titlex,xcalib)		!change number in title
	   call FIXCALIB(titley,ycalib)		!change number in title
	endif
	call DCMENU(-3,4,Lb,text,0,0)	!delete box 3 only
	Lb(3)=-1
	call DCMENU(-3,4,Lb,text,icol0,icol2)	!draw box 3 only (italic)
	goto 112
c
19	continue
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row=lrow, col 0
	if(.not.calbar) then
c	   print 307,xcross,ycross
c307	   FORMAT('&Xcross,Ycross [',f9.2,1x,f9.2,']= ')
c	   call INPUT2r(xcross,ycross)
	   call DEFOLT2r(xcross,ycross,defolt)
	   call QDIALOG(1,'Axes crossing point: X, Y',defolt,ict,cans)
	   call GETINP2r(cans,xcross,ycross)
	   croset=.true.		!cross position has been reset
	   Lb(4)=-1
	   goto 113		!redraw menu
	else
	   call FIXCBAR(rx,ry,lth(21),idev,itx,ity,ntx,nty,itlen,idraw(1))
	   goto 306		!redraw
	endif
c
c Redefine t=0.  If this section is done several times need to keep the
c accumulated X offset that has been applied in XOFF1
25	continue
	if(ncjump.eq.0.and.nvjump.eq.0.and.ikey.eq.9) goto 2501	!t=0 at cursor
	onjump=ikey.eq.8
	if(ncjump+nvjump.gt.8) call BELL(2)	!not enough boxes!
	call SETLBOX(10,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	text(9)='9. DEFINE t=0 '
	text(10)=' START AT t=0 '
	   if(ncjump.gt.0) then
		do 263 i=1,ncjump
		if(i.gt.9) goto 267
		ch1=char(i+48)
263	      text(i)=ch1//'.C-jump #'//ch1
	   endif
	   if(nvjump.gt.0) then
		do 264 i=1,nvjump
		j=i+ncjump
		if(j.gt.9) goto 267
		ch1=char(i+48)
		ch2=char(j+48)
264	      text(j)=ch2//'.V-jump #'//ch1
	   endif
	   if(ncjump+nvjump.lt.8) then
	     do 265 i=ncjump+nvjump+1,8
265	     Lb(i)=0
	   endif
267	call DCMENU(nbox,5,Lb,text,icol1,icol2)
	call CKEY(ch,ikey)
2501	continue
	if(ikey.eq.10) then
	   do 262 j1=1,ncurvd		!ncurvd data sets
	   j=icurvd(j1)		!data set to be scanned
	   t0=xval(1,j)
	   do 262 i=1,ndat(j)
262	   xval(i,j)=xval(i,j) - t0		!so 1st point at t=0
c and similarly for Xcal (if there are any calc curves)
	   if(ncurvc.gt.0) then
		do 2621 j1=1,ncurvc		!plot ncurvc curves
		j=icurvc(j1)			!curve # to be plotted
		do 2621 i=1,ncal(j)
2621		xcal(i,j)=xcal(i,j) - t0
	   endif
	   xmin=xmin-t0
	   xmax=xmax-t0
	   xoff1=0.0	!for drawing jump logo
	else if(ikey.le.9) then
	   if(ikey.eq.9) then    		!mark t=0 with cursor
		call PUTCUR(5000,7484)
            ch=char(FNDCUR(ix1,iy1))	!note new usage
      	t0=XWORLD(ix1)
		xoff=0.0
	   else if(ikey.ge.1.and.ikey.le.8) then
	   j=icurvd(1)		!all Xval(*,j) shifted by same amount
	   xoff=xval(1,j)	!to shift origin to 0 before applying t1v etc shift
	    if(ikey.le.ncjump) then
		if(onjump) then
		   t0=t1c(ikey)		!start time for cjump #ikey
		else
		   t0=t2c(ikey)		!end time=start of off-jump #ikey
		endif
	    else
		if(onjump) then
		   t0=t1v(ikey-ncjump)	!ditto for Vjump
		else
		   t0=t2v(ikey-ncjump)	!end time=start of off-jump #ikey
		endif
	    endif
	   endif
	   do 266 j1=1,ncurvd		!ncurvd data sets
	   j=icurvd(j1)		!data set to be scanned
	   do 266 i=1,ndat(j)
266	   xval(i,j)=xval(i,j)-xoff-t0
c and similarly for Xcal (if there are any calc curves)
	   if(ncurvc.gt.0) then
		do 2622 j1=1,ncurvc		!plot ncurvc curves
		j=icurvc(j1)			!curve # to be plotted
		do 2622 i=1,ncal(j)
2622		xcal(i,j)=xcal(i,j)-xoff-t0
	   endif
	   xmin=xmin-xoff-t0
	   xmax=xmax-xoff-t0
	   xoff1=t0		!for drawing jump logo
	endif
c	goto 11
	croset=.false.	!so xcross also reset to new xmin
	goto 306		!redraw straight away
c
c GRAPH SHAPE: Section to move graphboundary (eg if Y axis on right hand edge)
40	continue
	nbox=10	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	Lb(8)=0
	call DCMENU(0,5,Lb,text,0,0)		!delete all
	text(1)='1. USE CURSORS'
	text(2)='2.GIVE NUMBERS '
	text(3)='3. DEFAULT SHAPE'
	text(4)='4. SQUARE SHAPE'
	text(5)='5. PORTRAIT SHAPE'
	text(6)='6. FULL WIDTH  '
	if(.not.ivplot) then
	  text(7)='7.I-V PLOT SHAPE'
	else
	  text(7)='7. NOT I-V PLOT '
	endif
	text(9)='9. PAGE PREVIEW '
	text(10)='10. REDRAW       '
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
	call WDIALOG(2,'F1=HELP',ict)
	call WDIALOG(2,'F2=HELP INDEX',ict)
405	call CKEY(ch,ikey)
	if(ikey.eq.-16) goto 150	!ESC returns to main menu
	if(ikey.eq.-1) then	!F1 key
	   call VHELP(5)
	   goto 405		!another menu choice
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-2)
	   goto 405		!another menu choice
	endif
c	if(ikey.eq.-1) then	!F1 key
c	   iret=4
c	   goto 237		!help
c	endif
	if(ikey.lt.1.or.ikey.gt.10) goto 405
	if(ikey.eq.7) goto 408
	if(ikey.eq.10) goto 306		!redraw
	if(ikey.eq.9) then
	   call DCMENU(0,5,Lb,text,0,0)		!delete all
	   call WDIALOG(1,'NB Anything outside red box NOT plotted',12)
	   call PAGEV(landscap,title1,csize,icol(71))
	   goto 306		!redraw
	endif
c	goto(406,406,406,406,406,406,408) ikey
c MARK NEW SHAPE WITH CURSORS
406	if(ikey.eq.1) then
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
	   call DEFOLT2r(rxlo,rxhi,defolt)
	   call QDIALOG(1,'Low, high graph boundaries for X (% of page)'
     &	,defolt,ict,cans)
	   call GETINP2r(cans,rxlo,rxhi)
	   call DEFOLT2r(rylo,ryhi,defolt)
	   call QDIALOG(1,'Low, high graph boundaries for Y (% of page)'
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
	else if(ikey.eq.5) then		!portrait shape as above
	   ixlo=3250            !portrait
	   ixhi=6750	!axis length x=3500 y=4200
	   iylo=1500
	   iyhi=5700
	   if(ycross.gt.ymin) then
	      iylo=1000 	!make lower as no need for room for numbers/title
	      iyhi=6000
		ixlo=3000	!make bigger to match
		ixhi=7000
	   endif
	else if(ikey.eq.6) then		!full width
	   ixlo=50
	   ixhi=9950
	   iylo=350 		!make taller too (so graphboundary
	   iyhi=6000		! fills whole plot area on screen)
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
c
c Change to/from I-V plot shape (ikey=7)
408	continue
	if(ikey.eq.7) then
	   if(.not.ivplot) then
		ivplot=.true.
		xticsav=xtic
		xtic=50.	!mV (or tics are too close with short X axis)
		itx=0		!tics central
		ity=0
		xcross=0.
		ycross=0.
		landscap=.false.
		doframe=.false.
	   else
		ivplot=.false.
		xtic=xticsav
		itx=1
		ity=1
		xcross=xmin
		ycross=ymin
		landscap=.true.
		doframe=.true.
	   endif
	   idraw(6)=-2       !Parameter values drawn at default posn
	   idraw(10)=-2	!so title, if present, drawn at default posn
	   idraw(8)=-2       !X axis label drawn at default posn
	   idraw(9)=-2       !Y axis label drawn at default posn
	   idraw(1)=-2		!calibration bars drawn at default position and length
	   do i=31,80
	    idraw(i)=-2		!all numbers at default posn
	   enddo
c	   goto 306
	   goto 1083		!so ixlo, viewport etc reset
	endif
c END OF GRAPH SHAPE OPTIONS
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
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	Lb(9)=0	!not yet used
	if(.not.logx) Lb(12)=0
	if(.not.logy) Lb(13)=0
	Lb(14)=0	!not yet used
	Lb(15)=0	!not yet used
134	call DCMENU(0,5,Lb,text,0,0)		!delete all
	if(ilog.ne.0) then
	   text(1)='1: ARITHMETIC '
	else if(ilog.eq.0) then
	   if(.not.calbar) then
	      text(1)='1:DRAW CAL BAR(s)'
	   else
	      text(1)='1: DRAW AXES  '
	  endif
	endif
	text(2)='2:Y vs log(X) '
	text(3)='3:log(Y) vs X '
	text(4)='4:log(Y)/log(X)'
	text(5)='5: HILL PLOT  '
	text(6)='6:sqrt(Y) / X '
	text(7)='7:sqrt(Y)/logX'
	text(8)='8: QUEUE PLOT '
	text(10)='10: REDRAW   '
	text(11)='+ :MULTIPLE TRACE'
	if(logx) then
	   if(inumx.eq.-1) then
		text(12)='-:EXPONENT X NUM'
	   else if(inumx.eq.1) then
		text(12)='-:FIXED X NUMBERS'
	   endif
	else
	   if(inumx.eq.-1) then
		text(12)='-:ALLOW X SCALING'
	   else if(inumx.eq.1) then
		text(12)='-:NO X SCALING'
	   endif
	endif
	if(logy) then
	   if(inumy.eq.-1) then
		text(13)='*:EXPONENT Y NUM'
	   else if(inumy.eq.1) then
		text(13)='*:FIXED Y NUMBERS'
	   endif
	else
	   if(inumy.eq.-1) then
		text(13)='*:ALLOW Y SCALING'
	   else if(inumy.eq.1) then
		text(13)='*:NO Y SCALING'
	   endif
	endif
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
1311	call CKEY(ch,ikey)
	if(ikey.lt.-16.or.ikey.gt.15) goto 1311
	if(ikey.eq.-1) then	!F1 key
	   call VHELP(4)
	   goto 1311		!another menu choice
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-2)
	   goto 1311		!another menu choice
	else if(ikey.eq.-16) then	!ESC
	   goto 306			!redraw
	else if(ikey.eq.8) then	!queue plot
	   goto 301
	endif
	if(ikey.eq.10) goto 306		!redraw
	if(ikey.eq.11) then
	   reclen=xval(ndat(1),1)-xval(1,1)	!length of trace
	   call MULTRACE(yval,mono,xmin,xmax,ymin,ymax,idev,
     &      ndat(1),ijoin(1),icol(1),ilth(1),ndv1,ndimd,ntrace,
     & 	ytsep,reclen,idraw(1),icol(71),0)	!draw only, no clear
c     &	ytsep,icol(71),0)
c	   goto 13
	   goto 306		!redraw straight away
	endif
	if(ikey.eq.12) then		!move somewhere else
		inumx=-inumx
		idraw(31)=-2   !so x axis numbers redrawn at default posn in LAXES
		Lb(12)=-1
		goto 134
	endif
	if(ikey.eq.13) then
		inumy=-inumy
		idraw(56)=-2   !so y axis numbers redrawn at default posn in LAXES
		Lb(13)=-1
		goto 134
	endif
	if(ikey.eq.1.and.ilog.eq.0) then	!cal bars
	   calbar=.not.calbar
	   if(calbar) then
	      idraw(1)=-2		!calibration bars drawn at default position/length
		doframe=.false.
		i=3
 		call DEFOLTi(i,defolt)
44		call QDIALOG(1,
     & 'Calibration bar on (1) X axis (2) Y axis (3) both: n',
     &	   defolt,ict,cans)
		call GETINPi(cans,i)
c		if(i1.ge.1.and.i1.le.3) i=i1
		if(i.lt.1.or.i.gt.3) goto 44
		if(i.eq.1.or.i.eq.3) then
		   ntx=-1000
		   if(ntxsav.ne.-1000) then		!units not yet defined
c	subroutine TDIALOG(ibox,query,title,nlen,blank,ict)
			call TDIALOG(1,'Units for X axis (eg ms):',
     &		   titlex,40,.false.,ict)
		   endif
		endif
		if(i.eq.2.or.i.eq.3) then
		   nty=-1000
		   if(ntysav.ne.-1000) then
			call TDIALOG(1,'Units for Y axis (eg pA):',
     &		   titley,40,.false.,ict)
		   endif
		endif
	   else if(.not.calbar) then        !revert to axes
	      ixlo=1400		! reset
		call GRAPHBOUNDARY(ixlo,ixhi,iylo,iyhi)
		doframe=.true.
c		If originally called with cal bars then xtic,ytic may not be defined
c		 so define them now (but leave xmin,xmax,ymin,ymax unchanged)
	      if(xtic.le.0.) call FIXAX(xmin1,xmax1,x1,x2,xtic,0)	!get xtic
		if(ytic.le.0.) call FIXAX(ymin1,ymax1,x1,x2,ytic,0)
		ntx=5
		nty=5
		if(ntxsav.ne.-1000) titlex=titxsav		!restore axis label
		if(ntysav.ne.-1000) titley=titysav		!restore axis label
	   endif
c	   idraw(12)=-2       !X axis label drawn at default posn -set at 135 now
c	   idraw(13)=-2       !Y axis label drawn at default posn
	   calbarX=ntx.eq.-1000
	   calbarY=nty.eq.-1000
	   calbar=calbarX.or.calbarY
	   if(calbarY) then
		ixlo=500	!make wider
		call GRAPHBOUNDARY(ixlo,ixhi,iylo,iyhi)
	   endif
	   goto 135		!revert to default positions and redraw
	endif
c
c If ikey>1 then now define ILOG
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
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row=lrow, col 0
c	print 132,xmin1,ymin1
c132	format('&Cannot take log or sqrt: xmin1,ymin1= ',2g13.6,/,
c     & ' New xmin,ymin (give -1 to abandon)= ')
c	call INPUT2r(xmin1,ymin1)
 	call DEFOLT2r(xmin1,ymin1,defolt)
	call QDIALOG(1,'New Xmin, Ymin values [abandon]',defolt,
     &	ict,cans)
	call GETINP2r(cans,xmin1,ymin1)
	if(xmin1.lt.0.) then
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
135	continue
	do i=31,80
	  idraw(i)=-2		!all numbers at default posn
	enddo
	idraw(6)=-2       !Parameter values drawn at default posn
	idraw(8)=-2       !X axis label drawn at default posn
	idraw(9)=-2       !Y axis label drawn at default posn
	idraw(10)=-2	!so title, if present, drawn at default posn
	croset=.false.	!so sets xcross=xmin etc
c
	goto 306		!redraw
c
c Section to change line type
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
c117	 	format('&Data set number (1 to ',i2, ') [0=all] = ')
c	 	read 3,j1
		j1=0
		call INTCONV(ncurvd,cnum1)
		call QDIALOG(1,'Data set number (1 to '
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
	else if(ikey.eq.6) then
c	   print 121,isym(j2)
c121	   format('&Symbol type (0 to 9) (now= ',i3,') = ')
c	   i=isym(j2)
c	   call INPUTi(i)
	   i=isym(j2)
	   call DEFOLTi(i,defolt)
	   call QDIALOG(1,'Symbol type (0 to 9)',defolt,ict,cans)
	   call GETINPi(cans,i)
	   if(j1.gt.0) then
		isym(j2)=i
	   else
		do 1211 j2=1,ncurvd
		j=icurvd(j2)
1211		isym(j)=i
	   endif
	else if(ikey.eq.7) then
	   s=syms(1)
	   if(j2.gt.0) s=syms(j2)
c	   print 123,syms(j2)
c123	   format('&Symbol size (0.5 to 18.) [now= ',f6.1,'] = ')
c	   call INPUTr(syms(j2))
	   call DEFOLTr(s,defolt)
	   call QDIALOG(1,'Symbol size (0.5 to 18)',defolt,ict,cans)
	   call GETINPr(cans,s)
	   if(j1.le.0) then
		do j2=1,ncurvd
		 j=icurvd(j2)
		 syms(j)=s
		enddo
	   else
		syms(j2)=s
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
221	continue
	iret=0
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	if(.not.calbar) Lb(4)=0
	if(ncurvd.le.0) then
	   Lb(6)=0
	   Lb(7)=0
	   Lb(8)=0
	endif
	if(ncurvc.le.0) Lb(9)=0
	if(isdev.eq.-1) Lb(12)=0
	if(ncjump.eq.0.or.IVplot) Lb(13)=0
	if(nvjump.eq.0.or.IVplot) Lb(14)=0
	text(1)='1: ADD NEW TEXT'
 	text(2)='2: FIX TEXT   '
 	text(3)='3: FIX NUMBERS '
	text(4)='4.FIX CAL BAR(s)'
	text(5)='5:ARROWS & LINES '
	text(6)='6: SYMBOL TYPE'
	text(7)='7: SYMBOL SIZE'
	text(8)='8:FIX DATA LINE'
	text(9)='9:FIX CALC LINE'
	text(10)='10. REDRAW   '
	if(doframe) then
	 text(11)='+:OMIT FRAME'
	else
       text(11)='+:DRAW FRAME'
	endif
	if(isdev.eq.0) then
	   text(12)='-:SHOW SD BARS'
	else if(isdev.eq.1) then
	   text(12)='-:NO SD BARS '
	endif
	text(13)='*:FIX C-JUMP LOGO'
	text(14)='/:FIX V-JUMP LOGO'
	text(15)='.:LINE THICKNESS'
c	text(4)='4.ARROWS/LINES'
c	if(doframe) then
c	 text(5)='5.OMIT FRAME '
c	else
c       text(5)='5.DRAW FRAME '
c	endif
c	text(6)='6.SYMBOL/LINES'
c	text(7)='7.LINE THICK  '
c	text(8)='8.SIZE/FONT   '
c	text(9)='9.HELP (or F1)'
c	text(10)='10. REDRAW    '
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
	call CKEY(ch,ikey)
	if(ikey.eq.-1) then	!F1 key
	   iret=5
	   goto 237		!help
	endif
c	goto(241,232,235,234,12,1161,236,37,237,306) ikey
c===		1   2   3   4   5  6   7   8   9   10  11 12
	goto(241,232,235,233,234,116,116,116,116,306,12,404,
     &	402,402,236) ikey
	goto 221	!no valid key
c
c1161	continue
c	iret=0
c	call NUMSET			!set num lock on ready for response
c	call DCMENU(0,4,Lb,text,0,0)		!delete all
c	nbox=10		!number of boxes for DCMENU
c	call SETLBOX(nbox,Lb,1)
c	if(ncurvc.le.0) Lb(2)=0
c	if(ncurvd.le.0) then
c	   Lb(1)=0
c	   Lb(3)=0
c	   Lb(4)=0
c	endif
c	if(ncjump.eq.0.or.IVplot) Lb(5)=0
c	if(nvjump.eq.0.or.IVplot) Lb(6)=0
c	if(.not.bigplot) Lb(7)=0
c	if(isdev.eq.-1) Lb(8)=0
c	Lb(9)=1
c	Lb(10)=1
c	text(1)='1.DATA LINE   '
c	text(2)='2.CALC LINE   '
c	text(3)='3.SYMBOL TYPE '
c	text(4)='4.SYMBOL SIZE '
c	text(5)='5.C-JUMP LOGO '
c	text(6)='6.V-JUMP LOGO '
c	if(allpnt) then
c	   text(7)='7.OMIT POINTS '
c	else
c	   text(7)='7.ALL POINTS  '
c	endif
c	if(isdev.eq.0) then
c		text(8)='8.SHOW SD BARS'
c	else if(isdev.eq.1) then
c	   text(8)='8.NO SD BARS  '
c	endif
c	text(9)='9.HELP (or F1)'
c	text(10)='10. DONE      '
c	call DCMENU(nbox,5,Lb,text,icol1,icol2)
c	ikey=nkey()
c	if(ikey.eq.-1.or.ikey.eq.9) then	!F1 key
c	   iret=9
c	   goto 237		!help
c	endif
c	if(ikey.eq.7) then
c	   allpnt=.not.allpnt
c	   goto 221
c	endif
c	goto(116,116,116,116,402,402,1161,404,1161,221) ikey
c	goto 1161	!no valid key
c
c Section to alter bars (logos) that represent jumps
402	continue
	if(ikey.eq.13) call FIXLOGO(text,xmin,xmax,ncjump,t1c,t2c,
     & xoff1,idev,LTH(28),iy1c,iy2c,idraw(28))
	if(ikey.eq.14) call FIXLOGO(text,xmin,xmax,nvjump,t1v,t2v,
     & xoff1,idev,LTH(29),iy1v,iy2v,idraw(29))
	goto 221
c
c Toggle error bars
404	continue
	call DCMENU(-8,4,Lb,text,0,0)		!delete box 8 only
c	call DELBOX1(8)	!delete single box
	if(isdev.eq.0) then
	   isdev=1
	else if(isdev.eq.1) then
	   isdev=0
	endif
	goto 221	!more options?
c
12	doframe=.not.doframe	!toggle frame
	goto 221	!more changes?
c
233	continue
	call FIXCBAR(rx,ry,lth(21),idev,itx,ity,ntx,nty,itlen,idraw(1))
	idraw(8)=-2       !X axis label drawn at default posn
	idraw(9)=-2       !Y axis label drawn at default posn
	goto 306		!redraw
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
	  j1=0  		!index for newtext
	  j=10		!index for position,font,size
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
	  ilth(j1+30)=ilth(n+30)
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
	call CKEY(ch,imode)
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
	nbox=15		!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	Lb(11)=0
	Lb(12)=0
	Lb(13)=0
	Lb(14)=0
	Lb(15)=0
2340	continue
	if(narrow.eq.0) Lb(2)=0
	if(nline.eq.0) Lb(7)=0
	if(nhline.eq.0) Lb(8)=0
	if(nvline.eq.0) Lb(9)=0
	text(1)='1:ADD AN ARROW  '
	text(2)='2:DELETE ARROW  '
	text(3)='3:ADD FREE LINE '
	text(4)='4:ADD HOR. LINE '
	text(5)='5:ADD VERT. LINE'
	text(6)='6:ADD GRID LINES'
	text(7)='7:DEL. FREE LINE  '
	text(8)='8:FIX HOR. LINE   '
	text(9)='9: FIX VERT. LINE '
	text(10)='10:  FIX GRID     '
c===line type/colour to be fixed with IV's new routines
c===old:
c	text(3)='3.ADD A LINE    '
c	text(4)='4.DELETE LINE   '
c	text(5)='5.LINE TYPE     '
c	text(6)='6.SPECIFY X     '
c	text(7)='7.SPECIFY Y     '
c===
c	text(8)='8: Sval ARROW   '	!for VHIST
c=	text(15)='15:  DONE       '	!not used now
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
	call CKEY(ch,ikey)
	if(ikey.lt.1.or.ikey.gt.15) goto 221	!no valid key
c start of arrow- need to store start/end points, and number of arrows,
c so they can be reproduced when graph redrawn
	if(ikey.ge.3.and.ikey.le.10) goto 27
c
26	continue	!ARROWS
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
	else if(ikey.eq.1) then
	   narrow=narrow+1			!count arrows
	   call INTCONV(k,cnum1)
	   cnum1=CHARNB(cnum1)
	   call WDIALOG(1,'Arrow #'//CHARNB(cnum1)
     &	//': define start and end points with cursors',ict)
	   call PUTCUR(5000,3500)		!start with cursor central
         ch=char(FNDCUR(ix1,iy1))		!define start of arrow
         ch=char(FNDCUR(ix2,iy2))		!define end of arrow
	   i=narrow
         xb(i) = XWORLD(ix1)
         yb(i) = YWORLD(iy1)
         xe(i) = XWORLD(ix2)
         ye(i) = YWORLD(iy2)
	   if(ixfix.eq.1) xb(i)=xv       ! x specified
	   if(ixfix.eq.1) xe(i)=xv
	   if(iyfix.eq.1) yb(i)=yv       ! y specified
	   if(iyfix.eq.1) ye(i)=yv
c Call to ARROW includes colour (with Hgraph numbering)
	   if(mono) then
		ic=0
	   else
		ic=15-icol(i+50)		!convert to Hgraph numbering
		if(ic.eq.15) ic=-1	!black (for icol=0)
	   endif
         call ARROW(xb(i),yb(i),xe(i),ye(i),ic,2,.true.)
	   ixfix=0		!clear again
	   iyfix=0		!clear again
	endif
	goto 221
c
27	continue
c========next bit will be subsumed in 'fix line' (also col/line thick?)
c	if(ikey.eq.5.and.nline.gt.0) then		!line type
c	   k=1
c	   if(nline.gt.1) then
c		call INTCONV(nline,cnum1)
c		call QDIALOG(1,'Histogram data number (1 to '
c     &	  //CHARNB(cnum1)//')',' ',ict,cans)
c		call GETINPi(cans,k)
c	   endif
c	   call MOVETO(xlb(k),ylb(k),0,iltype(k))
c	   call MOVETO(xle(k),yle(k),-1,iltype(k))	!delete
c	   call DEFOLTi(iltype(k),defolt)
c	   call QDIALOG(1,'Line type (0 to 8)',defolt,ict,cans)
c	   call GETINPi(cans,iltype(k))
c	   call MOVETO(xlb(k),ylb(k),0,iltype(k))
c	   call MOVETO(xle(k),yle(k),1,iltype(k))	!redraw
c	   goto 221
c	endif
	if(ikey.eq.3) then             !add freehand line
	   nline=nline+1			!count lines
	   call INTCONV(nline,cnum1)
	   call WDIALOG(1,'Line #'//CHARNB(cnum1)
     &	//': define start and end points with cursors',ict)
	   call PUTCUR(5000,3500)		!start with cursor central
         ch=char(FNDCUR(ix1,iy1))		!define start of line
         ch=char(FNDCUR(ix2,iy2))		!define end of line
	   i=nline
         xlb(i)=XWORLD(ix1)
         ylb(i)=YWORLD(iy1)
         xle(i)=XWORLD(ix2)
         yle(i)=YWORLD(iy2)
	   call MOVETO(xlb(i),ylb(i),0,iltype(i))
	   call MOVETO(xle(i),yle(i),1,iltype(i))
	else if(ikey.eq.4.or.ikey.eq.5) then     !add horizontal/vert line
	   if(ikey.eq.4) then
		nhline=nhline+1
	      call INTCONV(nhline,cnum1)
	   else
		nvline=nvline+1
	      call INTCONV(nvline,cnum1)
	   endif
	   call WDIALOG(1,'Horizontal line # '//charnb(cnum1)//':',ict)
	   iopt=1
	   call DEFOLTi(iopt,defolt)
	   call QDIALOG(1,'Specify (1) with cursor (2) numerically;',
     &	defolt,ict,cans)
	   call GETINPi(cans,iopt)
	   call WDIALOG(1,'-Hit ENTER/ESC when done: F1 for help',ict)
	   if(iopt.eq.1) then
	    if(ikey.eq.4) then	!horizontal
		val=0.5*(ymin+ymax)
		eps=0.003*val
		amin=xmin
		amax=xmax
		call SETLINE(.true.,val,eps,6,2,amin,amax,12,6,70,
     &	icol(71),delete,ik)     !format=F6.2 at present
		if(delete) goto 221	!should not use delete here!
		yhline(nhline)=val		!record y value
		xhlb(nhline)=amin        !start/end of HLINE
		xhle(nhline)=amax
	      if(pon()) write(7,35) nhline,val
	      if(discprt) write(8,35) nhline,val
35		format(' Horizontal line # ',i3,' added at y = ',g13.6)
	    else if(ikey.eq.5) then	!vertical
		val=0.5*(xmin+xmax)
		eps=0.001*val
		amin=ymin
		amax=ymax
		call SETLINE(.false.,val,eps,6,2,amin,amax,12,6,70,
     &	icol(71),delete,ik)     !format=F6.2 at present
		if(delete) goto 221
		xvline(nvline)=val		!record x value
		yvlb(nvline)=amin          !start/end of HLINE (or define with cursor?)
		yvle(nvline)=amax
	      if(pon()) write(7,351) nvline,val
	      if(discprt) write(8,351) nvline,val
351		format(' Vertical line # ',i3,' added at x = ',g13.6)
	    endif
	   else if(iopt.eq.2) then	!specify numerically
	      if(ikey.eq.4) then	!horizontal
		   relval=.false.
		   if(nhline.gt.1) then
			ans='Y'
	   		call DEFOLTa(ans,defolt)
			call QDIALOG(1,' Specify RELATIVE to line 1',
     &		 defolt,ict,cans)
			call GETINPa(cans,ans)
			relval=ans.eq.'Y'
			if(relval) then
			   ihlinrel(nhline)=1
			else
			   ihlinrel(nhline)=0
			endif
		   endif
		   y=ymin
	   	   call DEFOLTr(ymin,defolt)
		   call QDIALOG(1,' Y value for horizontal line ',
     &	defolt,ict,cans)
		   call GETINPr(cans,y)
		   if(relval) then
			yhline(nhline)=y + yhline(1)
			ilhtype(nhline)=2
		   else
			yhline(nhline)=y
		   endif
		   xhlb(nhline)=xmin    !start/end of HLINE (or define with cursor?)
		   xhle(nhline)=xmax
		   i=nhline
		   if(.not.mono) call COLTYP1(icol(i+71))		!draw it
		   call MOVETO(xhlb(i),yhline(i),0,ilhtype(i))
		   call MOVETO(xhle(i),yhline(i),1,ilhtype(i))
	         if(pon()) write(7,35) nhline,y
	         if(discprt) write(8,35) nhline,y
		   if(relval) then
	            if(pon()) write(7,36)
	            if(discprt) write(8,36)
36			format(' (relative to line 1)')
		   endif
	      else if(ikey.eq.5) then	!vertical
		   relval=.false.
		   if(nvline.gt.1) then
			ans='Y'
	   		call DEFOLTa(ans,defolt)
			call QDIALOG(1,' Specify RELATIVE to line 1',
     &		 defolt,ict,cans)
			call GETINPa(cans,ans)
			relval=ans.eq.'Y'
			if(relval) then
			   ivlinrel(nvline)=1
			else
			   ivlinrel(nvline)=0
			endif
		   endif
		   x=xmin
	   	   call DEFOLTr(x,defolt)
		   call QDIALOG(1,' X value for vertical line ',
     &	defolt,ict,cans)
		   call GETINPr(cans,x)
		   if(relval) then
			xvline(nvline)=x + xvline(1)
			ilvtype(nvline)=2
		   else
			xvline(nvline)=x
		   endif
		   yvlb(nvline)=ymin    !start/end of VLINE (or define with cursor?)
		   yvle(nvline)=ymax
		   i=nvline
		   if(.not.mono) call COLTYP1(icol(i+81))		!draw it
		   call MOVETO(xvline(i),yvlb(i),0,ilvtype(i))
		   call MOVETO(xvline(i),yvle(i),1,ilvtype(i))
	         if(pon()) write(7,351) nvline,x
	         if(discprt) write(8,351) nvline,x
		   if(relval) then
	            if(pon()) write(7,36)
	            if(discprt) write(8,36)
c36			format(' (relative to line 1)')
		   endif
	      endif
	   endif
	else if(ikey.eq.6.or.ikey.eq.10) then    !add/fix grid (hor + vert lines)
	   call BELL(2)
	   call WDIALOG(1,'Grid not yet fixed',12)
	else if(ikey.eq.7.or.ikey.eq.8.or.ikey.eq.9) then           !fix lines
	   if(ikey.eq.7.and.nline.eq.0) goto 221		!nothing to fix
	   if(ikey.eq.8.and.nhline.eq.0) goto 221		!nothing to fix
	   if(ikey.eq.9.and.nvline.eq.0) goto 221		!nothing to fix
	   call FIXLINES(ikey,nline,xlb,xle,ylb,yle,iltype,
     &	 nhline,yhline,xhlb,xhle,ilhtype,ihlinrel,
     &	 nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,icol(71))
c====
c	   call FIXHVLIN(nhline,yhline,xhlb,xhle,ilhtype,ihlinrel,
c     &    nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,icol(71))
	endif
	goto 221
c
c
c Line type legend
C###	continue
c not yet done
c	goto 221
c
c Line thickness
236	continue
	imode=1	!for line thickness
	call SETATT(narrow,nline,ntext,isdev,
     & ilth,icol,icol0,icol1,icol2,ncurvd,icurvd,ncurvc,icurvc,
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
	if(ntext.le.19) goto 2411	!OK- room for another one
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
		ilth(j1+30)=ilth(n+30)
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
	call SETFONT1(ifnt(n))
	call setcsize(csfac(n)*csize,isize)    !sets isize too
c NB Readstring is equivalent to call to FNDCUR followed by GETSTRING
c	call PUTCUR(5000,3500)		!start with cursor central
c	call READSTRING(ix,iy,newtext(ntext),
c     & iangle(n),isize)
	call WDIALOG(1,'Define start position for text with cursors',-ict)
	call PUTCUR(3000,3000)
      ch=char(FNDCUR(ix,iy))	!note new usage
c Define all text string values (do once only, unless text altered), not
c everytime graph redrawn
	rx(n)=XWORLD(ix)
	ry(n)=YWORLD(iy)
	ixjus(n)=base
	iyjus(n)=base
	idraw(n)=1
	ic=icol(30+ntext)
	if(ic.eq.0) then
	   ic=14
	   icol(30+ntext)=ic
	endif
	call WDIALOG(1,'Now write text: hit F1 anytime for HELP     ',ict)
	call HGSTRING(ix,iy,newtext(ntext),ifont,iangle(n),isize,
     &	ixjus(n),iyjus(n),ic,icol(71),80,.true.)
	nl=NBLANK(newtext(ntext))
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
c	call INIPLT(0,.false.,1.0)	!erase for help?
c	call SCROLL(0,0,28,79,1)
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
	call CKEY(ch,i)
	if(i.eq.-16) goto 306
c	call SCROLL(0,0,24,79,24)
c	call LOCATE(0,0)		!row=0,col 0
c	call ERASCR
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
     & '	LINES BETWEEN POINTS- 0=solid; 1=dotted;',/,
     & '	     2-6=inc length dashes; 7=dash-dot; 8=dash-dot-dot.',/,
     & '	CALC CURVES: -1=skip; 0=solid; 1=dotted; 2=short dash;',/,
     & '      3=long dash; 4=long-short dash; 5=long-short-short.',/,
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
	call ANYKEY
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
c open a 2 row dialog box top=row 23; cols 2-58
	plot=.true.
	monsav=mono			!keep value before plot
	do i=1,100
	   icolsav(i)=icol(i)
	enddo
	call HGDRIVER(iplotter,colplotter)
	if(colplotter) then
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
	ixlo2=ixlo		!save screen GRAPHBOUNDARY
	ixhi2=ixhi
	iylo2=iylo
	iyhi2=iyhi
	ivxlo=0		! for VIEWPORT
	ivxhi=10000
	ivylo=0
	ivyhi=7484
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
1552	call CKEY(ch,ikey)
	if(ikey.eq.1) mpos=5
	if(ikey.ge.2.and.ikey.le.5) mpos=ikey-1	!=1-4
	if(ikey.eq.6) mpos=8
	if(ikey.ge.2.and.ikey.le.7) then
	  call DCMENU(-1,5,Lb,text,0,0)	!delete box #1 only
	  Lb(1)=1
	  call DCMENU(-1,5,Lb,text,icol0,icol2)	!redraw (not italic)
	endif
	if(ikey.eq.10) then
	   scalfac=1.15
c If scaled up too much, program hangs up without output to printer: try this
	   s1=7484./float(iyhi)
	   s2=10000./float(ixhi)
	   if(s1.gt.scalfac.or.s2.gt.scalfac) then	!scalfac too big
		if(s1.lt.s2) then					!so use min(s1,s2)
		   scalfac=s1
		else
		   scalfac=s2
		endif
	   endif
	   call DEFOLTr(scalfac,defolt)
	   call QDIALOG(1,'Scale up to fill paper?: scale factor',
     &	defolt,ict,cans)
	   call GETINPr(cans,scalfac)
	   call DCMENU(-10,5,Lb,text,0,0)	!delete box 10 only
	   Lb(10)=-1
	   call DCMENU(-10,5,Lb,text,icol1,icol2)	!draw box 10 only (italic)
	endif
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
	ixlo=1300		! set normal shape
	if(calbarY) ixlo=500	!make wider
	ixhi=8200		! display
	iylo=1500		! location
	iyhi=5700		! screen
	ivxlo=0		! for VIEWPORT
	ivxhi=10000
	ivylo=1
	ivyhi=7484
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
	  ivyhi=7484
	else if(mpos.eq.2) then
	  ivxlo=5000	!for VIEWPORT: top right
	  ivxhi=10000
	  ivylo=3500
	  ivyhi=7484
	else if(mpos.eq.3) then
	  ivxlo=0		!for VIEWPORT: bottom left
	  ivxhi=5000
	  ivylo=0
c	  ivyhi=3500
	  ivyhi=3745
	else if(mpos.eq.4) then
	  ivxlo=5000	!for VIEWPORT: bottom right
	  ivxhi=10000
	  ivylo=0
	  ivyhi=3745
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
	  call IFRAME(50,9950,0,7484)		!9950-50=9900
c	else
c	  call LOCATE(6,10)		!row=5,col 10
c	  print 732
c732	  format(' (dashed area is not usable)')
c	  call IFRAME(2525,7475,3500,7000)     !7475-2525=4950
cc	  top half of box=solid line, bottom half (not usable) dashed
c	  call DFRAME(xworld(2525),xworld(7475),yworld(0),yworld(3500),2)
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
c Define ixlo etc from cursor values
	ivxlo=0		! for VIEWPORT
	ivxhi=10000
	ivylo=0
	ivyhi=7484
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
c=========
c	y2=float(iy4-iy3)*fac
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
	fac=0.5*(float(ix2-ix1)/10000.+float(iy2-iy1)/7484.)
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
	if(bigplot.and.(.not.allpnt)) then
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row=lrow,col 0
c	   print 1554,ndelt
c1554	   format('&Every nth point displayed, n = ',i3,
c     &   '; plot ALL points [Y] ? ')
c	   read 101,ans
	   call INTCONV(ndelt,cnum1)
	   ans='Y'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'Every nth point displayed, n = '
     &	  //CHARNB(cnum1)//'; plot ALL points',defolt,ict,cans)
	   call GETINPa(cans,ans)
	   if(ans.ne.'N') allpnt=.true.
	endif
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
1814	continue
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
	ivyhi=7484
	goto 1081
c
c
c PLOT QUEUE SECTION
301	continue
c For VPLOT restore weight,isdev; add (for jump logos):
c ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,
	call VPLQ4(xval,yval,xcal,ycal,ndimd,ndimc,
     & ncurvd,ndat,icurvd,isym,ijoin,ncurvc,ncal,icurvc,iline,syms,
     & xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ntx,nty,itx,ity,
     & ixlo,ixhi,iylo,iyhi,itit,title1,csize,ifont,ilog,iscal,doframe,
     & titlex,titley,ilabel,inumx,inumy,idiskq,sval,
     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,
     & theta,ifitype,ncomp,idest,autplt,icol,mono,interp,
     & ilth,RX,RY,rxbox,rybox,IXJUS,IYJUS,iangle,idraw,ifnt,
     & csfac,cnumx,cnumy,cexpx,cexpy,numbx,numby,
     & narrow,xb,yb,xe,ye,ntext,nline,xlb,ylb,xle,yle,iltype,
     & newtext,isdev,weight,y0,yinf,
     & nhline,yhline,xhlb,xhle,ilhtype,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel,
     & itrace,ntrace,ytsep,ndv1,ndc1)
	goto 306		!redraw
c end of plot queue section
c
c CALC CURVE SECTION.
303	continue
	if(ncurvc.le.0) goto 999
	linetype=0		!always cont line- dashes synthesized via zseq
c
c	nbadc=0
	x0=1.e-37		!smallest value for logs
	do 54 j1=1,ncurvc		!plot ncurvc curves
	j=icurvc(j1)			!curve # to be plotted
	if(iline(j).lt.0) goto 54	!skip this curve (added 06/24/90 05:29pm)
	ij=iabs(iline(j))
	if(idev.ge.5) then
	      call LOADVTB('2'//'W'//char(lth(j+10)))
	else
	      call LINWID(int4(ilth(j+10)))
	endif
	if(.not.mono) call COLTYP1(icol(j+10))		!colour for jth calc curve
	if(ij.gt.0.and.ij.le.9) then
c	   straight=.false.
	   goto 310		!dashed calc curve
	else if(ij.ge.10) then
c	   straight=.true.
	   linetype=ij-10			!join points with straight line type #ij
	endif
c
	Jflag=0
	do 220 k=1,ncal(j)
	xv=xcal(k,j)
	yv=ycal(k,j)
	if(logity) then
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
312	continue
	sfac=float(iyhi-iylo)/float(ixhi-ixlo)		!O.K.?
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
	if(logity) then
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
c	if(debug().and.abs(dxn).lt.1.e-4) then
c	   print 3141,i,xv,xvlast,dxn
c3141	   format(' i,xv,xvlast,dxn = ',i5,3g13.6)
c	   pause
c	endif
	vert=abs(dxn).lt.(1.e-28*abs(dyn))    !line is vertical
	if(.not.vert) b=dyn/dxn
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
c	if(nbadc.gt.0) call BELL(2)	!add option to print bad values?
	if(.not.plot) goto 15		!another chance to plot it
	if(idev.ge.5.and.PLOT) goto 1813	!do vtrans
	if(plot) goto 1812		!return with plot=true?
c
c TIDY UP AND EXIT
999	continue
c    IASK=1 or -1 to ask 'are you sure' before leaving VHIST; =2,-2 to not ask
c    IASK=positive to leave graph on screen (1 or 2); neg (-1,-2) to delete
	if(iabs(iask).eq.1) then
	   call BELL(1)
c	   call LOCATE(12,1)
c	   print 9991
c9991	   format(' ARE YOU SURE [Y] ? ')
c	   read 101,ans
c	   if(UC(ans).eq.'N') goto 151
	   ans='Y'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'ARE YOU SURE',defolt,ict,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'N') goto 151
	endif
	if(iask.gt.0) call DCMENU(0,4,Lb,text,0,0)	!delete boxes before exit
 	if(iask.lt.0)call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
c return value of INTERP via ijoin(10) -no longer used
c	if(interp) then	!signal to interpolate
c	   ijoin(10)=ijoin(10)-100	!signal that INTERP=true on exit
c	endif
	if(ntxsav.ne.-1000) titlex=titxsav		!restore axis label
	if(ntysav.ne.-1000) titley=titysav		!restore axis label
	RETURN
	END


	subroutine MINMAX2(xval,yval,ndat,xmin1,xmax1,ymin1,ymax1,
     & logx,logy,ndimd,ncurvd,icurvd,ndv1)
c For Yval want the largest/smallest values in any of the arrays that
c are used -calculated here
c Modif 09/10/94 10:36am for allocatable arrays with ndv1 added to call.
c 07/05/91 09:44pm modified to deal with ndat>2048. Looks only at all points
c (including those skipped when ndelt>1; ndelt value not known here)
c Should be OK if called with Ycal etc (for which ngr=1 always)
c Ndimd=2nd dimension in main prog of xval,yval
	real*4 XVAL(ndv1,ndimd),YVAL(ndv1,ndimd)
	integer ndat(ndimd)	!contain number of points in each array
	integer icurvd(ndimd)	!contain numbers of arrays actually used
	logical logx,logy
c
	ymin1=1.e37
	xmin1=1.e37
	ymax1=-1.e37
	xmax1=-1.e37
	do 50 j1=1,ncurvd		!scan ncurvd data sets
	j=icurvd(j1)		!data set to be scanned
	 do 1 i=1,ndat(j)
	 xv=xval(i,j)
	 yv=yval(i,j)
	 if(logy.and.yv.lt.1.e-37) goto 1
	 if(logx.and.xv.lt.1.e-37) goto 1
	 if(xv.lt.xmin1) xmin1=xv
	 if(xv.gt.xmax1) xmax1=xv
	 if(yv.lt.ymin1) ymin1=yv
	 if(yv.gt.ymax1) ymax1=yv
1	 continue
50	continue
	RETURN
	end


	subroutine COUNTDAT(j,ndat,ndelt,xval,yval,logx,logy,logity,sqrty,
     & y0,yinf,ymin,ymax,x0,xmin,xmax,ndimd,NDISP,ndv1)
	logical logx,logy,logity,sqrty
	real*4 XVAL(ndv1,ndimd),YVAL(ndv1,ndimd)
	integer ndat(ndimd)
c
c Subroutine, needed only if ndat(j)>2048, to calculate NDISP=number of points
c in display range when every NDELTth point used (eg call with ndelt=1 to get
c ndisp from which calculate new ndelt in order to keep ndisp =< 2048)
c Modif 09/10/94 10:36am for allocatable arrays with ndv1 added to call.
c
	ndisp=0
c
	do 215 i=1,ndat(j),ndelt
	 xv=xval(i,j)
	 yv=yval(i,j)
	 if(logity) then
	   yv=(yv-y0)/(yinf-yv)	! Hill scale
	   if(yv.gt.x0) then
		yv=alog10(yv)
	   endif
	 endif
	 if(logx) then
	   if(xv.gt.x0) then
		xv=alog10(xv)
	   else
		xv=2.*xmax		!so not plotted
	   endif
	 endif
	 if(logy) then
	   if(yv.gt.x0) then
		yv=alog10(yv)
	   else
		yv=2.*ymax		!so not plotted
	   endif
	 endif
	 if(sqrty.and.yv.ge.0.) yv=sqrt(yv)
c  xv,yv now defined but don't count if outside min,max range
	 if(xv.lt.xmin.or.xv.gt.xmax) goto 215
	 if(yv.lt.ymin.or.yv.gt.ymax) goto 215
	 NDISP=NDISP+1
215	 continue
c
	RETURN
	end

