       subroutine VPLOT5(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,syms,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,plotonly,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,interp,
     & isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kmax,plotrue,
     & iver)
c
c
c VPLOT5 is version  in which
c (1) weight() is a parameter, no longer in common/sd/
c  This means that weight() must be declared/allocated as weight(kwi,kwj)
c  before calling, but if not needed can be allocated as W(1,1).  Also include
c  icurvw(ndimd) (like icurvd) such that icurvw(i)=1 if curve i (as defined
c  by icurvd) should have error bars, =0 if w(i,j) defined but error bars not
c  to be drawn initially; and =-1 if weights not defined (replaces isdev).
c  (2) And add kmax=declared dimension of theta() also.
c  (3) Problem with colours/line thickness for data/calc curves when there
c are more than 10, because icol(1-10)/icol(11-20) only assigned for these
c -at present just recycle round same 10 values
c  (4) 'draft' option removed -replace with 'plot only'
c  (5) ntrace,ytsep moved from common/TPOS to separate COMMON/mtrace/ntrace,
c 	ytsep,adcfil, where adcfil=path/name of CONSAM file -this will allow
c	multipage plots of CONSAM data to be done within VPLOT5.
c  (6) When itrace=2 on entry, assumes that multi-trace plot wanted
c	but ytsep, ntrace, not yet defined finally (e.g. as when multi-page
c	plots requested in AUTPLOT).  In this case goes straight to
c	MULTRACE to define separation of lines on page.
c
c TO BE DONE
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
c
c---
c Modif 10/27/95 10:02pm: qfile declared and call of VPLQ5 altered
c Modif 10/22/95 07:03am to add SCALE/OFFSET to RESCALE menu
c///
c Modif 03/03/95 07:29pm so default is to plot all points, however many
c there are.
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
c	(ignored if itrace=0 in call) (now in common/mtrace/)
c	ntrace = number of traces into which 1st data set is
c	   split (normally =1)
c	ytsep = vertical distance between the traces in pA for 1st set
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
c VPLOT3 is new version (03/16/94 07:48am) that has following chANGLEs
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
c	be used for any subroutine that need to chANGLE viewport and/or
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
c (1) SD bar thickness now in RLTH(30)
c (2) ISCAL=2,3,4 added
c (3) iline=10 to 18 joins calc points with Hgraph line type 0-8
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
c (7) Management of Xcross,Ycross chANGLEd. ISCAL=-1 added (see below) and
c	now, if xmin,ymin chANGLEd then xcross,ycross reset to new xmin,ymin
c	IF old xmin,ymin=old xcross,ycross. But when this not true then
c	xcross,ycross NOT reset (eg so xmin,ymin can be chANGLEd on an
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
c IFITYPE=32 for amp  histos (AHIST) if SD are constrained to be  equal
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
c ARRAY INDEX FOR POSITIONS (IDRAW,ANGLE,RX,RY,RXBOX,RYBOX)
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
c Line thickness for whole page plots in RLTH(i) as follows ( actual
c thickness =THICK*RLTH(i) were THICK=THBIG=1.0 for whole page).
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
c ANGLE() defines default ANGLE for text strings similarly
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
c place when graph shape chANGLEd (eg via FIX ON VDU option). However symbols
c etc, with position specified in world coord, will not plot outside
c the graphboundary so must be within axes!
c
c IDRAW(i) =1 to draw (without box) at the position that has been already
c			defined in rx,ry,rxbox,rybox (no need to define box again)
c		Elements of  IDRAW() defined as for RLTH() above
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
c   (2) After chANGLE of log/sqrt axes
c AFTER CHANGLE OF LOG AXES NEED TO REDRAW EVERYTHING AT DEFAULT POSN
c When, as for NEWTEXT and ARROWS then could take log/sqrt/antilog etc
c of rx,ry but this involves checking how axis CHANGLES eg might chANGLE
c from logy/logx to logy/x so Y axis is log, but not chANGLEd so no
c need to alter RY(). This not yet fixed, so log axes should be chANGLEd
c before any newtext/arrows added.
c
c   (3) After chANGLE of graph shape (by call to graphboundary)
c After chANGLE of shape by call to graphboundary must redraw ALL text
c at the same world coordinates in rx,ry. ie do not want any of the idraw=-2
c (all should be -1,0,1). But boxes may be in wrong positions
c or distorted,so all boxes redefined now, from the current rx,ry.
c
	real*4 XVAL(ndv1,ndimd),YVAL(ndv1,ndimd)
	real*4 XCAL(ndc1,ndimc),YCAL(ndc1,ndimc)
c for data
	integer*4 ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd)
	integer*4 icurvw(ndimd)
	real*4 syms(ndimd)		!symbol size
	real*4 weight(kwi,kwj)
c for calc curves
	integer*4 ncal(ndimc),icurvc(ndimc),iline(ndimc)
	allocatable::x1calc,x2calc
	real x1calc(:),x2calc(:)
c---For scale factors (see SCALCURV)
	allocatable::xscale,yscale,yoffset
	real*4 xscale(:),yscale(:),yoffset(:)
	logical xdone,ydone,meta,plotrue,wmeta,wbmp
c///
c for interpolation
	real xint(2048),yint(2048),Y2int(2048)	!for interpolation
	logical ivplot,onjump,tload,calbarX,calbarY,calbar,blank
	logical vert,mono,monsav,redrawn
	logical plotonly,mulpage,help,present,plotcols
	real theta(kmax)
	dimension zseq(10)
	character*1 ans,UC,ch1,ch2,getch
	character*33 qfile,metafil,printfil,oldfile
	character*40 titlex,titley,titxsav,titysav
	character*75 xtitle,ytitle	!output from LAXES
	character*75 oldtext
	character*64 TITLE1
	character parval*200		!to hold param values
	character text1*150		!to hold any string (for fixtext call)
	character defolt*30,cans*30		!to hold text & result of QDIALOG
      character kpchar*11, ch*1,cnum1*11,ans1*30		!must have *11 for INTCONV
	LOGICAL PLOT,logx,logy,logity,down,fitted,croset,EQUAL,sqrty
	logical doframe,autplt,landplot,landscap,zoomed,interp
	logical allpnt,bigplot		!for when one or more plots >2048 points
	logical caplock,debug,pon,slock,numlock
	integer*2 int2,videotyp
	real*4 RLTH(100),rlt,rlt4		!for line thickness
c arrays for arrows, extra text etc
c posn etc for 20 bits of new text + 6=param values
c NB start posn for text and arrows (and axis labels in LAXES)must be
c kept in world coord (real) not device coord (integer) or they come
c out in wrong place if graph outline is chANGLEd (as in 'FIX ON VDU')
c so rx(),ry() must all hold world coord
	real RX(100),RY(100),ANGLE(100)
	real rxbox(4,100),rybox(4,100)
	integer IJUS(100)
      real size(30),wpots(30)
	integer idraw(100),icol(100),icolsav(100)
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25)
	integer ifnt(30)

	real xb(10),yb(10),xe(10),ye(10)		!up to 10 arrows
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	integer iltype(10)				!line type for ditto
	character*80 newtext(20)		!extra text
	integer  nreal(20)
c new arrays for horizontal lines
	real*4 yhline(10)		!record y value
	real*4 xhlb(10),xhle(10)     !start/end of HLINE
	integer ilhtype(10)	!line type for horizontal lines
	integer*1 ihlinrel(10)	!=1 if line pos rel to line 1
c new arrays for vertical lines
	integer ilvtype(10)	!line type for vertical lines
	integer*1 ivlinrel(10)	!=1 if line pos rel to line 1
c	real xlb1(10),ylb1(10),xle1(10),yle1(10)	!up to 10 extra lines
c	real*4 yhline(10)		!record y value
c	real*4 xhlb1(10),xhle1(10)     !start/end of HLINE
	real*4 xvline(10)		!record x value
	real*4 yvlb(10),yvle(10)     !start/end of VLINE
c=====      integer fndcur
	integer Lb(30)		!for DCMENU
	character*78 text(30),strings(10),str2(10),titles,titles1	!for DCMENU
	character*11 newname
	logical discprt
	common/dp/discprt
	COMMON/JLOGO/t1c(10),t2c(10),t1v(10),t2v(10),xoff1,y1v,y2v,
     & y1c,y2c
	common/DMENU/ifonb,csize,ifont2,nboxlast,nblast		!for DCMENU
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi
	common/hlp/help		!for QDIALOG
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
c lines 5,6 added to TPOS 09/04/94 08:29pm for hor and vert lines
c  line 7 added to TPOS 09/12/94 07:11am multiple traces
c
c Define functions
	pon()=slock()
	debug()=caplock()
	EQUAL(x,y)=abs(x-y).lt.0.00001

c
3	format(i8)
4	format(g13.6)
c Initialisations:
c (1) Initialisation whether AUTPLT or not
c	if(VIDEOTYP().ne.18) call INIPLT(idev,.false.,1.0)
	scalfac=1.0
	nprimo=0
	just=0
	wmeta=.false.
	wbmp=.false.
	present=.true.
c	call GINO
c	if(VIDEOTYP().ne.18) call VGA
	call VGA
	call gsetcols(0)
	call errswi(-1)
	call brkswi(1)
	call chaswi(1)
	call grfmod (1)
	call harcha
	call mode(18)
	xtsav=xtic
	ytsav=ytic
	call papenq(xp,yp,ipap)
	vxlo=0	! for VIEWPORT
	vxhi=xp
	vylo=0
	vyhi=yp
	if(plotrue) autplt=.true.
	if(.not.autplt) iver=1100
	xlo=xlo1		!named ixlo1 in call so ixlo can be in common/hgv/
	xhi=xhi1		!ditto
	ylo=ylo1 !named iylo1 in call so iylo can be in common/hgv/
	yhi=yhi1		!ditto
	ifont2=0
c	ifont2=ifont	!copy for common/dmenu/
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
c---
c set values in common
c	subroutine DEFDIALOG(ibox,irhi,iclo,nrow,ncol,icb)
c==	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
	nxlo=50	!pixels
	nylo=-1
	nyhi=370    !pixels
c	ictm=0		!text black
c	ibkm=7		!background light grey
c	icfm=2		!frame/title green
c	icupm=12		!upper case red
c Lahey colours:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
	ictm=15		!text white
	ibkm=1		!background daek blue
	icfm=11		!frame/title light blue
	icupm=12		!upper case red
c Allocate xscale etc
	ndims=ndimd
	if(ndimc.gt.ndimd) ndims=ndimc
	ALLOCATE(xscale(ndims),yscale(ndims),yoffset(ndims))
	do i=1,ndims
	   xscale(i)=1.0
	   yscale(i)=1.0
	   yoffset(i)=0.0
	enddo
	ALLOCATE(x1calc(ndimc),x2calc(ndimc))

c///
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
c	call setcls(mono,icol,autplt,plotcols,isetcol)
c Use plotter colours as default initially
	plotcols=.true.
	call setcls(mono,icol,.false.,plotcols,0)
c=====================////////////////////////
	cbig=3.0
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
	zoomed=.false.
	x0=1.e-36		!smallest value for taking logs
	cfacsml=0.6		!character size factor for small plots (0.6*2.5=1.5)
	thfacsml=0.6	!line thickness factor for small plots
	ifsav=ifont		!save input value (if there is one)
	tlen=4.
	bigplot=.false.		!now superfluous
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
	   size0=csize
	   thick=thbig
	   fitted=ncurvc.gt.0
	   allpnt=.true.		!plot all points if ndat>2048
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
	endif
c
c (3) Initialisation only when AUTPLT=false- none defined on entry
	linetype=0		!continuous line

	if(AUTPLT) goto 89
c========================  ???????????????  Ioana
c	narrow=0	!number of arrows added
c	nline=0	!number of free lines added
c========for now use nhline/nvline from TPOS even if AUTPLT=false
c	nhline=0	!number of horizontal lines added
c	nvline=0	!number of vertical lines added
c=================
	ntext=0	!number of extra text strings
c=====keep in point size
	if(cbig.lt.2.) cbig=3.5
	if(ifont.lt.0) ifont=1
	ifont2=0		!copy for common/dmenu/
	allpnt=.true.
	thbig=1.3		!line thickness factor for whole page
	csize=cbig
	size0=csize
	thick=thbig


	thfacsml=0.6		!line thickness factor for small plots
	s=0.8*csize !default symbol size
	if(syms(1).le.0.) then
	   do 9 i=1,ndimd
9	   syms(i)=s
	endif
c
	do 81 i=1,100
	  ANGLE(i)=0
	  idraw(i)=-2		!until defined
c	  rx(i)=0.0		!NB world coord may be neg so cant tell if defined yet
81	continue
	idraw(28)=1		!c-jump logo
	idraw(29)=1		!v-jump logo
	do i=1,10
c	   iltype(i)=0		!continuous line for extra lines
	enddo
c
c	ANGLE(8)=0			!0 set above
	ijus(8)=center		!for x-axis label
	ANGLE(9)=90		!for y-axis label
	ijus(9)=center

	do i=1,30
	  ifnt(i)=ifont
	  size(i)=1.3
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
	rlt=0.2	      !default thickness, unless reset
	do i=1,100
	  RLTH(i)=rlt
	enddo
c	RLTH(1) to RLTH(20) = 0.2 draw
	RLTH(21)=0.3	!axis
 	RLTH(22)=0.3	!frame
c	RLTH(27)=0.15	!symbols
c	RLTH(28)=0.20	!C-jump logo
c	RLTH(29)=0.20	!V-jump logo
c	RLTH(30)=0.12	!SD bars
c	RLTH(31) to ....  = 0.2 newtext
c
 	RLTH(23)=0.5		!labels
 	RLTH(24)=0.5		!numbers
 	RLTH(25)=0.5		!title
 	RLTH(26)=0.25	!param value text
	do i=31,50
		rlth(i)=0.5
	enddo

89	continue	!jump here if autplt
c  Define text string for param values
	if(idraw(6).ne.0.and.ifitype.ne.0) call PARTEXT(parval,ifitype,
     & theta,ncomp,ifnt(6),size(6),kmax)
c END OF INITIALISATIONS
c
	call setfnt(ifont)
c	call SETSIZE(size0)    !sets isize too
c NB cannot yet define position, box coordinates, justification, font etc
c for all strings that are defined on entry: viz Plot Title, axis
c labels,(axis numbers if eventually drawn separately), and
c parameter values. Cannot do here at start (so done once only unless
c text altered), because must be done after CALL SCALE, so must be done
c every time graph is redrawn
c
	idev=0			!screen
180	continue			!return here after plot to repeat
183	continue
	logx=ilog.eq.1.or.ilog.eq.3.or.ilog.eq.4.or.ilog.eq.6
	logy=ilog.eq.2.or.ilog.eq.3
	logity=ilog.eq.4		!for Hill plot
	sqrty=ilog.eq.5.or.ilog.eq.6
c If ncurvd=0 use ycal for scaling
c========================================================
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
	if(ivplot) then
	   xtic=50.		!mV (or tics are too close with short X axis)
	   itx=0		!tics central
	   ity=0
	   xcross=0.
	   ycross=0.
	   landscap=.false.
	   doframe=.false.
	endif
	if(iy1v.ne.-1000) then
	   y1c=0.73*yp         !initial positions for jump logos
	   y2c=0.75*yp         !that do not overlap title
	   y1v=0.72*yp
	   y2v=0.71*yp
	endif
c	xoff1=0.0		!for drawing jump logo- now input


	xminsav=xmin
	yminsav=ymin
	xmaxsav=xmax
	ymaxsav=ymax
	xtsav=xtic
	ytsav=ytic
	xcsav=xcross
	ycsav=ycross


108	continue
c
C?????????????????????
c NB must define default as graph size (ymin) not data min (ymin1) etc
	xmin1z=xmin1
	ymin1z=ymin1
	xmax1z=xmax1
	ymax1z=ymax1

c	call papenq(xp,yp,ipap)
	if(ncurvc.gt.0) then
		do i=1,ncurvc
			j=icurvc(i)
			x1calc(j)=xmin
			x2calc(j)=xmax
	     enddo
	endif
	if(xlo.ge.0.) goto 1082
c     Make iyhi,ixhi lower to leave room for dialog box (also for narrow
c     vertical box ar RHS of screen?)
c     -proportions are close to golden section = 1:1.618
c     NB if following altered then also alter under 'plot now' option
c====================================================================
	is=1
1083	if(landscap) then
	   xlo=0.2*xp		! set
	   if(calbarY) xlo=0.10*xp	!make wider
	   xhi=0.8*xp		! display
	   ylo=0.2*yp! location
	   yhi=0.75*yp		! screen
	else
	   xlo=0.32*xp         !portrait
	   xhi=0.67*xp	!axis length x=3500 y=4200
	   ylo=0.2*yp
	   yhi=0.75*yp
	   if(ycross.gt.ymin) then
	      ylo=0.15*yp 	!make lower as no need for room for numbers/title
	      yhi=0.75*yp
		xlo=0.3*xp	!make bigger to match
		xhi=0.7*xp
	   endif
	endif

1082	continue
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
	if((.not.croset).and.(.not.ivplot)) then
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
104	continue			!RETURN HERE TO DO PLOT
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
	   titley='   Y   '
	   if(logx) titlex='log(X) '
	   if(logy) titley='log(Y) '
c	   if(ilog.eq.4) titley='log[(Y-Y(0)]/(Ymax-Y))'
	   if(ilog.eq.4) titley='log[(Y-Y(0)]/(Ymax-Y))'
c start of modif #1 07/23/96 10:52am
	   if(ilog.eq.7) then
		titlex='1/x'
		titley='1/y'
	   else if(ilog.eq.8) then
		titlex='y'
		titley='y/x'
	   else if(ilog.eq.9) then
		titlex='y/x'
		titley='y'
	   else if(ilog.eq.10) then
		titlex='x/y'
		titley='x'
	   endif
	endif
	call linvis(1)
c NB FRAME call is now in LAXES
c Last line of param is so that position of text strings (for axis
c labels etc) can be defined when they are, at the first call,
c calculated internally. Colours set internally in LAXES.
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
     & thick,RLTH,inumx,inumy,0.0,ilog,doframe,mono,icol,xcalib,ycalib,
     & rx,ry,ANGLE,ijus,idraw,rxbox,rybox,
     & numbx,numby,cnumx,cnumy,cexpx,cexpy)
	call lincols(15,idev)		!bright white
	just=1
c***********************************************************************
136	continue
c 	call pixgra(0,55,xabs1,yabs1)
c	call pixgra(639,389,xabs2,yabs2)
c	if (zoomed) then
 	   xabs1=xmin
         yabs1=ymin
 	   xabs2=xmax
         yabs2=ymax
c	endif
	goto 1234
	if(logx) then
	   xabs1=10**xabs1
	   xabs2=10**xabs2
	endif
	if(logy) then
	   yabs1=10**yabs1
	   yabs2=10**yabs2 !??????? fix this
	endif
	if(sqrty) then
	   yabs1=yabs1**2
	   yabs2=yabs2**2
	endif
1234	continue
c========================================================================
c 					DO TITLE IF ANY
c========================================================================

	call broken(0)
	if(idraw(10).eq.0.or.itit.eq.0) goto 1361		!no title
	call setfnt(ifnt(10))
	call setsize(size(10))
	nl=NBLANK1(title1)
	ymin2=ymin
	ymax2=ymax
	if(sqrty) ymin2=sqrt(ymin)
	if(sqrty) ymax2=sqrt(ymax)

c 	position for title:
c 	Draw param values, if req. NB need to set box position etc only the first
c 	time here (when idraw()=-2 still). If TITLE values are altered (in position,
c 	size etc) the relevant parameters will be adjusted at time of alteration.
	if(idraw(10).eq.-2) then
	  idraw(10)=1		!title position now defined
	  rx(10)=0.5*(xmin+xmax)
	  ry(10)=ymax2+0.025*(ymax2-ymin2)
	  ANGLE(10)=0.
	  ijus(10)=center
c       define rxbox,rybox for posn of TITLE1
	  call graspa(rx(10),ry(10),xsc,ysc)
	  call TEXTBOX(xsc,ysc,title1(1:nl),ANGLE(10),size(10),
     &     ijus(10),rxbox(1,10),rybox(1,10),0)
	endif
	if(rlth(25).le.0.6) then
		iwg=0
	else if(rlth(25).le.0.8) then
		iwg=1
	else if(rlth(25).le.1.0) then
		iwg=2
	else if(rlth(25).le.1.2) then
		iwg=3
	else if(rlth(25).le.1.5) then
		iwg=4
	else if(rlth(25).le.2.0) then
		iwg=5
	else if(rlth(25).ge.2.0) then
		iwg=6
	endif
	if(idev.ge.5) then
	   call LINWID(2*RLTH(25))
c	   call fntwgt(2*iwg)
	else
	   call LINWID(RLTH(25))
c	   call fntwgt(iwg)
	endif
c	if(.not.mono)
      call lincols(icol(25),idev)		!colour for title
	call graspa(rx(10),ry(10),xsc,ysc)
	call JUSTIFYSTRING(xsc,ysc,title1,ANGLE(10),size(10),
     & ijus(10))
c 	draw the box round title if necessary- could call TEXTBOX again, but
c	position already defined so quicker to call FRAME- but safer to use
c	former, in case graph size chANGLEd
	if(idraw(10).eq.-1) call TEXTBOX(xsc,ysc,title1,
     &   ANGLE(10),size(10),ijus(10),rxbox(1,10),rybox(1,10),1)
	   call setfnt(ifont)		!reset



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
	    call TEXTBOX(xsc,ysc,parval(1:nl),ANGLE(6),size(6),
     &     ijus(6),rxbox(1,6),rybox(1,6),0)
	else
	    x=rx(6)	!pos as defined by cursors or FIXTEXT
	    y=ry(6)
	endif
	if(idev.ge.5) then
	   call LINWID(2*RLTH(26))
	else
	   call LINWID(RLTH(26))
	endif
c	if(.not.mono)
      call lincols(icol(26),idev)		!colour for params
	call graspa(rx(6),ry(6),xsc,ysc)
	call JUSTIFYSTRING(xsc,ysc,parval(1:nl),ANGLE(6),size(6),
     & ijus(6))
c 	draw the box round PARVAL if necessary- could call TEXTBOX again, but
c 	position already defined so quicker to call IFRAME1
	call graspa(rx(6),ry(6),xsc,ysc)
	if(idraw(6).eq.-1) call TEXTBOX(xsc,ysc,parval(1:nl),
     &   ANGLE(6),size(6),ijus(6),rxbox(1,6),rybox(1,6),1)
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
	      call LINWID(2*RLTH(i+30))
	   else
	      call LINWID(RLTH(i+30))
	   endif
	   call setfnt(ifnt(j))
c	   call setsize(size(j))
c	   if(.not.mono)
         call lincols(icol(i+30),idev)		!colour for text(i)
	   call graspa(rx(j),ry(j),xsc,ysc)
	   call JUSTIFYSTRING(xsc,ysc,newtext(i),ANGLE(j),
     &    size(j),ijus(j))
	   if (idraw(j).eq.-1) then
		call TEXTBOX(xsc,ysc,newtext(i),
     &      ANGLE(j),size(j),ijus(j),rxbox(1,j),rybox(1,j),1)
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
     &xv,yv,csize,idev,xabs1,yabs1,xabs2,yabs2,RLTH,zoomed,iver)

c ==============================================================================
c                       DRAW LINES IF ANY
c ==============================================================================
	ikey=0
	call broken(0)
	call draw_lines(ikey,nline,xlb,xle,ylb,yle,iltype,
     & nhline,yhline,xhlb,xhle,ilhtype,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel,
     & csize,idev,xabs1,yabs1,xabs2,yabs2,RLTH,
     & xmin,xmax,ymin,ymax,zoomed,iver)
	call broken(0)
3333  continue
c============================================================================
c Draw jump logos if req (need to keep, and queue, the definitions eg
c whether to draw, ylevel for each and height of deflections), and
c for V-jump better read in voltages to make deflections proportional to them
c	if(debug()) print 802,ivplot,ncjump,iy1c,idraw(28),
c     &	nvjump,iy1v,idraw(29)
c     & l4,6i8)
c     28=jump bar (logo) for C-jumps
c     29=jump bar (logo) for V-jumps
	call broken(0)
	if(.not.IVplot) then
	   if(ncjump.gt.0.and.y1c.ge.0.and.idraw(28).eq.1) then
c	      if(.not.mono)
            call lincols(icol(28),idev)		!colour for c-logo
		call JLOGO(xmin,xmax,ncjump,t1c,t2c,
     &           xoff1,idev,RLTH(28),1,y1c,y2c,icol(71))
	   endif
	   if(nvjump.gt.0.and.iy1v.ge.0.and.idraw(29).eq.1) then
c	      if(.not.mono)
            call lincols(icol(29),idev)		!colour for v-logo
		call JLOGO(xmin,xmax,nvjump,t1v,t2v,
     &         xoff1,idev,RLTH(29),1,y1v,y2v,icol(71))
	   endif
	endif
c
c===========================================================================
c                          PLOT THE GRAPH
c===========================================================================
	call broken(0)
	barleng=0.01*(xmax-xmin)	!bar on top/bottom of error bars
c     Control which curves are plotted via Icurvd() now
	xsps=200.
	ysps=120.
	if(ncurvd.le.0) goto 303	!calc curve only
	nbadd=0
c===================================================================

c
	do 50 j1=1,ncurvd		!plot ncurvd data sets
	   jflag=0
	   j=icurvd(j1)		!data set to be plotted (col # where data starts)
	   if(j.le.10) then
	      j0=j
	   else
	      j0=1+mod(j-1,10)	!so j=11-20, 21-30 all -> j0=1-10 etc
	   endif
c	   if(.not.mono)
         call lincols(icol(j0),idev)	 !col for jth data set: symbols+line+SD
	   linetype=ijoin(j)		!line to join data points
	   iud=1				!draw line between points
	   if(linetype.eq.-1) iud=0	!don't
	   nint=0			!points for interpolation
c        Definitions to cope with ndat>2048; define ngr(j)=number of cols of
c        Yval(i,j) occupied by each of the data graphs (all =1 in normal case
c        where ndat(j)=<2048).  If number of points > 2048 then display only
c        every ndelt-th point -must first check how many values lie within the
c        current xmin,xmax,ymin,ymax rANGLE by call to COUNTDAT.
	   if(ndat(j).le.2048.or.allpnt) then
	      ndelt=1
	   else
	      ndelt=1
	      call COUNTDAT(j,ndat,ndelt,xval,yval,logx,logy,logity,sqrty,
     &      y0,yinf,ymin,ymax,x0,xmin,xmax,ndimd,NDISP,ndv1)
	      if(ndisp.gt.2048) then	!calc ndelt>1
		   ndelt=1 + (ndisp-1)/2048
	      endif
	   endif
	   if(ndelt.gt.1) then
	      call INTCONV(ndelt,cnum1)
	      if(ndelt.eq.2) then
		   cnum1(1:3)='2nd'
	      else if(ndelt.eq.3) then
		   cnum1(1:3)='3rd'
	      else
		   cnum1=CHARNB(cnum1)//'th'
	      endif
	      call WDIALOG(2,'Every '//CHARNB(cnum1)//' point',12)
	   endif
c
c        NB 09/06/91 11:50am seem to get some problems with line thickness if have
c        many points, and solvable if line thickness not chANGLEd for every point
c        ChANGLE is only needed if want BOTH symbols (other than point=0) AND
c        data points joined with line.  Mostly both are not needed and if so load
c        line thickness here, before the 'do 215' loop for each point
	   tload=.true.					!thickness to be loaded in loop
	   if((iud.eq.1).and.(isym(j).eq.0)) then
	      if(idev.ge.5) then
	         call LINWID(2*RLTH(j0))
	      else
	         call LINWID(RLTH(j0))
	      endif
	      tload=.false.					!thickness already loaded
	   else if(isym(j).ne.0.and.ijoin(j).eq.-1) then
	      if(idev.ge.5) then
	         call LINWID(2*RLTH(27))
	      else
	         call LINWID(RLTH(27))
	      endif
	      tload=.false.					!thickness already loaded
	   endif
c        Bit done now when itrace=2 on entry, to define ytsep() etc for
c        multi-page plots
c        If called with itrace=2 then check separation of traces etc
c        and do multipage plots here -then return straight away
	   if(itrace.eq.2) then
	      reclen=xval(ndat(1),1)-xval(1,1)	!length of trace
	      mulpage=.true.
	      call MULTRACE(yval,mono,xmin,xmax,ymin,ymax,idev,
     &      ndat(1),ijoin(1),icol(1),RLTH(1),ndv1,ndimd,ntrace,
     & 	ytsep,reclen,idraw(1),icol(71),
     & 	iend,np1,np2,mulpage,adcfil,csize,ifont,calfac,ioff,3)
	      itrace=1		!so this bit not done again
 	      call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	      RETURN
	   endif
c
c        Put do 215 loop + interp drawing into DRAWDAT, to draw curve # j
	   if(j.eq.1.and.ntrace.gt.1) then
	      call MULTRACE(yval,mono,xmin,xmax,ymin,ymax,idev,
     &      ndat(1),ijoin(1),icol(1),RLTH(1),ndv1,ndimd,ntrace,
     & 	ytsep,reclen,idraw(1),icol(71),
     & 	iend,np1,np2,mulpage,adcfil,csize,ifont,calfac,ioff,2)
	   else
		icw=icurvw(j1)	!SD control for curve 1,2,....
	      call DRAWDAT5(j,xval,yval,ndelt,
     &       logity,logx,logy,sqrty,y0,yinf,x0,mono,ilog,
     &       xmin,xmax,ymin,ymax,iud,tload,idev,
     &       ndat(j),ijoin(j),icol(j0),isym(j),syms(j),RLTH(j0),RLTH(j0)
     &       ,RLTH(27),RLTH(27),RLTH(30),RLTH(30),barleng,interp,npint,
     &       Xint,Yint,nint,yp1,ypn,Y2int,ndv1,ndimd,
     &	 weight,kwi,kwj,icw)
	   endif
	   call intconv(j1,cnum1)
	   nj=nblank1(cnum1)
	   if(.not.plot) then
	      call justifystring(xsps,ysps,cnum1(1:nj)//'.',0.,2.,-1)
	      call jSYMBOL(xsps+8.,ysps+1.,isym(j),syms(j),icol(j0),idev)     !symbol if isym>0
	   endif
	   ysps=ysps-6.
50	continue		!end of sets loop
	if(nbadd.gt.0) call BELL(2)	!add option to print bad values?
c     CALC CURVE (before rescale options)
	if(ncurvc.gt.0) goto 303	!do calc curve before rescale option
	if(meta.and.ncurvc.le.0) goto 5555
	if(wmeta.and.ncurvc.le.0) goto 5558
	if(wbmp.and.ncurvc.le.0) goto 5558
	if(idev.le.4.and.ncurvc.le.0.and.PLOT) goto 1812
	if(idev.ge.5.and.ncurvc.le.0.and.PLOT) goto 1813	!do vtrans
15	continue
c     Special returns that follow redrawing after HELP screen
	if(iret.ne.0) then
	   i=iret
	   iret=0
	   goto (15,221,11,13,221,235,236) i		!return after HELP screen
	endif
	if(iabs(iask).eq.3) goto 999		!straight out after drawing
	goto 150
c========================================================================
c     CALC CURVE SECTION.

303	continue
	if(ncurvc.le.0) goto 999
	linetype=0		!always cont line- dashes synthesized via zseq
c
	x0=1.e-36		!smallest value for logs
	do 54 j1=1,ncurvc		!plot ncurvc curves
	j=icurvc(j1)			!curve # to be plotted
	x1=x1calc(j)
	x2=x2calc(j)
	if(iline(j).lt.0) goto 54	!skip this curve (added 06/24/90 05:29pm)
	ij=iabs(iline(j))
	call SETBIGJ(j,j0)
	if(idev.ge.5) then
c	      call LOADVTB('2'//'W'//char(RLTH(j0+10)))
	else
	      call LINWID(RLTH(j0+10))
	endif
c	if(.not.mono)
      call linCOLs(icol(j0+10),idev)		!colour for jth calc curve
	if(ij.gt.0.and.ij.le.9) then
c	   straight=.false.
	   goto 310		!dashed calc curve
	else if(ij.ge.10) then
c	   straight=.true.
	   linetype=ij-10			!join points with straight line type #ij
	endif
c
c NB May get problem with log scales if 0 or neg, log is not taken, but if
c non-log values happen to be within xmin,xmax, ymin, ymax (which are log
c values, then jflag is set and MOVETO is done -in wrong place!) -fixed
c by 'goto 220' if log cannot be taken (01/19/96 11:08am)
	Jflag=0
	do 220 k=1,ncal(j)
	xv=xcal(k,j)
	yv=ycal(k,j)
	if(logity) then
	   if(abs(yinf-yv).lt.1.e-30) goto 220	!for case of yv=yinf
	   if(yv.gt.yinf-x0) goto 220	!skip values above ymax!
	   if(yv.lt.y0+x0) goto 220	!skip values below ymin!
	   yv=(yv-y0)/(yinf-yv)	! Hill scale
	   if(yv.gt.x0) then
		yv=alog10(yv)
	   else
		goto 220
c	      nbadc=nbadc+1
c	      if(nbadc.le.25) xcbad(nbadc)=xv
c	      if(nbadc.le.25) ycbad(nbadc)=yv
	   endif
	endif
	if(logx) then
	   if(xv.gt.x0) then
		xv=alog10(xv)
	   else
		goto 220
c	      nbadc=nbadc+1
c	      if(nbadc.le.25) xcbad(nbadc)=xv
c	      if(nbadc.le.25) ycbad(nbadc)=yv
	   endif
	endif
	if(logy) then
	   if(yv.gt.x0) then
		yv=alog10(yv)
	   else
		goto 220
c	      nbadc=nbadc+1
c	      if(nbadc.le.25) xcbad(nbadc)=xv
c	      if(nbadc.le.25) ycbad(nbadc)=yv
	   endif
	endif
	if(sqrty.and.yv.ge.0.) yv=sqrt(yv)
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
c start of modif #2 07/26/96 10:32pm
c This version moves to point even if it is out of rANGLE, but with no line.
c This ensures that when x and y get back in rANGLE the dot they come
c from is in the right place.
c	if(xv.lt.xmin.or.xv.gt.xmax) goto 220
c	if(yv.lt.ymin.or.yv.gt.ymax) goto 220
c	if(jflag.eq.0) call MOVETO(xv,yv,0,0)	!1st point in rANGLE
c	if(jflag.gt.0) call MOVETO(xv,yv,1,linetype)	!rest of points
	if(jflag.eq.0) then
	   call graMOV(xv,yv)	!1st point in rANGLE
	else
c	   if(xv.le.xmin.or.xv.ge.xmax.or.yv.lt.ymin.or.yv.gt.ymax) then
	   if(xv.le.x1.or.xv.ge.x2.or.yv.lt.ymin.or.yv.gt.ymax) then
		call graMOV(xv,yv)	!1st point in rANGLE
	   else
		call broken(linetype)
		call gralin(xv,yv)	!1st point in rANGLE
	   endif
	endif
c end of modif #2 07/26/96 10:32pm
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
c
	x0=1.e-36		!smallest value for logs
	Jflag=0
	do 313 i=1,ncal(j)
	xv=xcal(i,j)
	yv=ycal(i,j)
	if(logity) then
	   if(abs(yinf-yv).lt.1.e-30) goto 313	!for case of yv=yinf
	   if(yv.gt.yinf-x0) goto 313	!skip values above ymax!
	   if(yv.lt.y0+x0) goto 313	!skip values below ymin!
	   yv=(yv-y0)/(yinf-yv)	! Hill scale
	   if(yv.gt.x0) then
		yv=alog10(yv)
	   else
		goto 313
c	      nbadc=nbadc+1
c	      if(nbadc.le.25) xcbad(nbadc)=xv
c	      if(nbadc.le.25) ycbad(nbadc)=yv
	   endif
	endif
	if(logx) then
	   if(xv.gt.x0) then
		xv=alog10(xv)
	   else
		goto 313
c	      nbadc=nbadc+1
c	      if(nbadc.le.25) xcbad(nbadc)=xv
c	      if(nbadc.le.25) ycbad(nbadc)=yv
	   endif
	endif
	if(logy) then
	   if(yv.gt.x0) then
		yv=alog10(yv)
	   else
		goto 313
c	      nbadc=nbadc+1
c	      if(nbadc.le.25) xcbad(nbadc)=xv
c	      if(nbadc.le.25) ycbad(nbadc)=yv
	   endif
	endif
	if(sqrty.and.yv.ge.0.) yv=sqrt(yv)
c start of modif #1 07/23/96 10:52am
	x0=1.e-36		!smallest value for taking logs
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
c end of modif #1 07/23/96 10:52am
c	if(xv.lt.xmin.or.xv.gt.xmax) goto 313
	if(xv.lt.x1.or.xv.gt.x2) goto 313
	if(yv.lt.ymin.or.yv.gt.ymax) goto 313
	if(jflag.eq.0) then
	   call graMOV(xv,yv)	!move to 1st point in rANGLE
	   jflag=1
	   goto 317
	endif
c     Now part done for all points after 1st in rANGLE
314	dxn=(xv-xvlast)/xr
	dyn=sfac*(yv-yvlast)/yr
c	if(debug().and.abs(dxn).lt.1.e-4) then
c	   print 3141,i,xv,xvlast,dxn
c3141	   format(' i,xv,xvlast,dxn = ',i5,3g13.6)
c	   pause
c	endif
	vert=(dxn.eq.0.).or.abs(dxn).lt.(1.e-20*abs(dyn))    !line is vertical
c=======fix here -> VHIST
	if(vert) then
	   b=1.0
	   if(dyn.lt.0.) b=-1.0
	else
	   b=dyn/dxn	!slope of line
	endif
c     calc zn=dist from last point to current one in units= percent
c     of length of X axis
318	zn=sqrt(dxn*dxn + dyn*dyn)
c=	if(zleft.ge.zn) goto 315
c=	goto 316
c next bit when amount left to draw extends beyond (or exactly
c up to) the current point- go to this point
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
c	   goto 317
	else
c
c316	   continue
c  next bit done when amount of line remaining to be drawn does
c not extend as far as current point- keep drawing segments (with
c slope as between last point and current one) until it does.
	   if(vert) then
		xv1=xvlast
c=======fix here -> VHIST
	      yv1=yvlast + b*zleft*yr/sfac	!b=-1 or +1 here
	   else
		xv1=xvlast + zleft*xr/sqrt(1.+b*b)
c		yv1=yvlast + b*(xv1-xvlast)*yr/xr
		yv1=yvlast + b*(xv1-xvlast)*yr/(sfac*xr)
	   endif
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


c =========================================================================
	if(meta) goto 5555
	if(wmeta.or.wbmp) goto 5558
	if(.not.plot) goto 15		!another chance to plot it
	if(idev.ge.5.and.PLOT) goto 1813	!do vtrans
	if(plot) goto 1812		!return with plot=true?




c************************************************************************
150	continue
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	Lb(2)=0	!remove zoom
	if(nint.gt.2048) Lb(13)=0	!Interp illegal (see DRAWDAT)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	if(.not.bigplot) Lb(14)=0
151	continue
	text(1) =' 1: RESCALE     '
c	text(2) =' 2: ZOOM      '
	text(3) =' 3: GRAPH SHAPE'
	text(4) =' 4: GRAPH AXES '
	text(5) =' 5: POSH OPTIONS'
	text(6) =' 6: GIVE TITLE '
	text(7) =' 7: PLOT NOW   '
	text(8) =' 8: QUEUE PLOT '
	text(9) =' 9: END DISPLAY'
	text(10)='10: REDRAW    '
	text(11)=' +: X AXIS LABEL'
	text(12)=' -: Y AXIS LABEL'
	text(13)=' x: INTERPOLATE '
	if(allpnt) then
	   text(14)=' /: OMIT POINTS '
	else
	   text(14)=' /: ALL POINTS  '
	endif
	text(15)=' .: MORE OPTIONS  '
	call broken(0)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)

152	call CKEY(ch,ikey)
	if(ikey.lt.-2.or.ikey.gt.15) goto 151
	if(ikey.eq.-1) then	!F1 key
	   call VHELP(1)
	   goto 152		!another menu choice
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-2)
	   goto 152		!another menu choice
	endif
	if(ikey.eq.14) then
	   allpnt=.not.allpnt
	   goto 306		!redraw
	endif

c           1   2  3  4   5   6   7   8   9  10  11  12  13  14  15
	goto(11,140,40,13,221,991,155,301,999,306,401,401,403,152,154)ikey


c========================================================================
c                               MORE OPTIONS
c========================================================================
154	continue
	nbox=12	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
C	Lb(11)=0
	Lb(12)=0
	if(mono) then
	   text(1)='1: COLOUR DISPLAY'
	else
	   text(1)='1: MONOCHROME  '
	endif
	text(2)='2: SET COLOURS (SCREEN)'
	text(3)='3: SET COLOURS (PLOT)'
	text(4)='4: SET COLOURS (MANUAL)'
	text(5)='5: USE THICKER LINES '
	text(6)='6: USE THINNER LINES '
	text(7)='7: SET LINE THICKNESS'
	text(8)='8: SET DEFAULT TEXT SIZE'
	text(9)='9: SET DEFAULT FONT  '
	text(10)='0: REDRAW         '
	text(11)='+: SET FOR REDUCED PLOT'
	call DCMENU(nbox,4,Lb,text,icol1,icol2)
	call CKEY(ch,ikey)
c	if(ikey.gt.10) goto 154
	if(ikey.eq.10) goto 150	!main menu
	if(ikey.eq.-16) goto 150	!0 or ESC to main menu also
	if(ikey.eq.-1) then		!F1 key
	   call VHELP(15)
	   goto 154
	else if(ikey.eq.1) then
	   mono=.not.mono
	   if(mono) call lincols(15,idev)		!bright white
	else if(ikey.eq.2) then
c     set default colours even if AUTPLOT for now
	  mono=.false.
	  plotcols=.false.
	  call setcls(mono,icol,.false.,plotcols,0)
	else if(ikey.eq.3) then
	  mono=.false.
	  plotcols=.true.
	  call setcls(mono,icol,.false.,plotcols,0)
	else if(ikey.ge.4.and.ikey.le.7) then
	   if(ikey.eq.4) imode=2		!for graph colours
	   if(ikey.eq.5) imode=11		!increase line thickness
	   if(ikey.eq.6) imode=12		!decrease line thickness
	   if(ikey.eq.7) imode=1	      !set graph line thickness
	   call SETATT(narrow,nline,ntext,isdev,
     &   RLTH,icol,icol0,icol1,icol2,ncurvd,icurvd,ncurvc,icurvc,
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
	else if(ikey.eq.11) then
C====SET FOR REDUCED SIZE PLOT
	else
	   call SETATT(narrow,nline,ntext,isdev,
     &   RLTH,icol,icol0,icol1,icol2,ncurvd,icurvd,ncurvc,icurvc,
     &   ndimd,ndimc,ifitype,13)
	   goto 154		!try again
	endif
	goto 306	!redraw

c========================================================================
c                           READ IN TITLE
c========================================================================
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

	if (title1(nlg:nlg).eq.char(95)) title1(nlg:nlg)=char(32)
	if(nlg.gt.1) nlg=nlg-1
	if(idraw(10).eq.0.and.nlg.gt.1) idraw(10)=1
	itit=1		!for plotq
	call flush(7)
	goto 152

c======================================================================
c				AXIS LABELS
c========================================================================

c Section to read in new axis labels
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
	      call JUSTIFYSTRING(xsc,ysc,oldtext(1:nlx),ANGLE(n),
     &		SIZE(n),ijus(n))
	   endif
	   redrawn=.false.	!until hit REDRAW
	   if(ikey.eq.11) then
	      call graspa(rx(n),ry(n),xsc,ysc)
	      call GSTRING(xsc,ysc,titlex,ifnt(n),ANGLE(n),size(n),
     &	 ijus(n),icol(23),icol(71),40,.true.,nrealx)	!40=size of titlex
	   else if(ikey.eq.12) then
	      call graspa(rx(n),ry(n),xsc,ysc)
	      call GSTRING(xsc,ysc,titley,ifnt(n),ANGLE(n),size(n),
     &	 ijus(n),icol(23),icol(71),40,.true.,nrealy)
	   endif
	   idraw(n)=-2	!so new label drawn at default posn in LAXES
	endif
	ilabel=1		!so internal default labels not used
	goto 152

c========================================================================
c                   INTERPOLATION SECTION. Interpolate first 256 points of display- now 2048
c=========================================================================
403	continue
	if(nint.gt.2048) then	!interp illegal (see DRAWDAT)
	   call BELL(1)
	   goto 152			!back to menu
	endif
	n=5
	yp1=0.
	ypn=0.
	interp=.true.
	ans='Y'
 	call DEFOLTa(ans,defolt)
	call QDIALOG(1,'Use defaults (interp 5 points, end slopes=0)'
     &	,defolt,ict,cans)
	call GETINPa(cans,ans)
	if(UC(ans).eq.'N') then
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
	  call graMOV(xint(1),yint(1))   !move to 1st point in rANGLE
	  x=npint+1
c	  print 700,4
	  do 4033 i=1,nint-1
	  do 4033 k=1,npint
	   xin=xint(i) + (xint(i+1)-xint(i))*k/x	!divide the interval
   	   klo=0		!full bisection for now
	   call SPLINT(Xint,Yint,Y2int,nint,klo,khi,xin,yout)
	   call broken(lintype)
	   if(iud.eq.0) call graMOV(xin,yout)   !join with line from last point
	   if(iud.eq.1) call graLIN(xin,yout)   !join with line from last point
	   call SETBIGJ(j,j0)
	   ic=icol(j0)
	   if(mono) ic=15		!bright white
	   call SYMBOL(xin,yout,isym(j),syms(j),ic)     !symbol if isym>0
4033	  continue
c	  print 700,5
	  goto 152
	endif

c==========================================================================
c                              ZOOM section
c===========================================================================
140	continue
 	interp=.false.	!must redo interpolation after rescaling
	ikey=1		!if not zoomed yet
	if(zoomed) then
	   nbox=2
	   call SETLBOX(nbox,Lb,1)
	   call NUMSET			!set num lock on ready for response
	   call DCMENU(0,4,Lb,text,0,0)		!delete all
c	   if(.not.zoomed) Lb(2)=0
	   text(1)='1. ZOOM IN    '
	   text(2)='2. RESTORE ORIG'
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
	   call jSYMBOL(x1,y1,4,csize,12,idev)   !mark bottom left with red diamond
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
	   call jSYMBOL(x1,y1,4,csize,12,idev)   !mark bottom left with red diamond
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
	if (idraw(1).ne.0) idraw(1)=-2		!calibration bars drawn at default position and length
	do 34 i=31,80
34	idraw(i)=-2		!all numbers at default posn
	goto 306		!redraw


c=====================================================================
c                       RESCALE section:
c=====================================================================
11	continue
	interp=.false.	!must redo interpolation after rescaling
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
113	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
111	continue
	if(IVplot.or.iy1v.eq.-1000) then
	   Lb(8)=0
	   Lb(9)=0
	endif
	text(1)=' 1: Xmin,Xmax  '
	text(2)=' 2: Ymin,Ymax  '
	if(calbarX.and.calbarY) then
	   text(3)=' 3: X-bar, Y-bar'
	   text(4)=' 4; FIX CAL BARS'
	else if(calbarX.and.(.not.calbarY)) then
	   text(3)=' 3: X-bar, Y-tic'
	   text(4)=' 4: FIX CAL BAR '
	else if(calbarY.and.(.not.calbarX)) then
	   text(3)=' 3: X-tic, Y-bar'
	   text(4)=' 4: FIX CAL BAR '
	else
	   text(3)=' 3: X-tic,Y-tic '
	   text(4)=' 4: X,Y crossing'
	endif
	text(5)=' 5: TIC LAYOUT '
	text(6)=' 6: SCALE/OFFSET'
	Lb(7)=0
	Lb(12)=0
	Lb(13)=0
	Lb(14)=0
	Lb(15)=0
	if(ncjump.eq.0.and.nvjump.eq.0) then
	   Lb(8)=0
	   text(9)=' 9: DEFINE t=0 '
	else
	   text(8)=' 8: ON-JUMP t=0 '
	   text(9)=' 9: OFF-JUMP t=0'
	endif
	text(10)='10: REDRAW    '
	text(11)='+ : MULTIPLE TRACE'
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
112	call CKEY(ch,ikey)
	if(ikey.eq.-1) then	!F1 key
	   call VHELP(3)
	   goto 112		!another menu choice
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-2)
	   goto 112		!another menu choice
	endif
	if(ikey.eq.11) then
	   reclen=xval(ndat(1),1)-xval(1,1)	!length of trace
	   call MULTRACE(yval,mono,xmin,xmax,ymin,ymax,idev,
     &      ndat(1),ijoin(1),icol(1),RLTH(1),ndv1,ndimd,ntrace,
     & 	ytsep,reclen,idraw(1),icol(71),
     & 	iend,np1,np2,mulpage,adcfil,csize,ifont,calfac,ioff,0)
	   goto 111
	endif
114	if(ikey.eq.6) then
	   call SCALCURV(xval,yval,ndat,xcal,ycal,ncal,ncurvd,icurvd,
     &   ncurvc,icurvc,xscale,yscale,yoffset,xdone,ydone,
     &   xmin1,xmax1,ymin1,ymax1,ndv1,ndimd,ndc1,ndimc,ndims)
c        Reset xmin, xmax etc
	   if(xdone) then
		xmin2=xmin1
		xmax2=xmax1
	      if(logx) then
		   xmin2=alog10(xmin1)
		   xmax2=alog10(xmax1)
	      endif
		call FIXAX(xmin2,xmax2,xmin,xmax,xtic,ilog)
	   endif
	   if(ydone) then
		ymin2=ymin1
		ymax2=ymax1
		if(logy) then
		   ymin2=alog10(ymin1)
		   ymax2=alog10(ymax1)
		endif
		call FIXAX(ymin2,ymax2,ymin,ymax,ytic,ilog)
	   endif
	   croset=.false.		!so xcross,ycross reset
	endif
c After rescale that affects numbering on axes (xmin,xmax,xtic,xcross) must
c redraw all numbers on the affected axis at their default positions (idraw=-2)
c Also do axis labels at default posn??
c Modif 04/18/95 05:16pm so that if text deleted (idraw=0) it is not redrawn
	if(idraw(6).ne.0) idraw(6)=-2       !Parameter values drawn at default posn
	if(idraw(10).ne.0) idraw(10)=-2	!so title, if present, drawn at default posn
c Calib bars drawn at default position, but current length
c (if this is defined ie idraw(1).ne.-2), otherwise default length
c---ikey=6 added in next line
	if(calbar.and.(ikey.eq.1.or.ikey.eq.2.or.ikey.eq.3.or.
     &	ikey.eq.6)) then
	   if(idraw(1).ne.-2.and.idraw(1).ne.0) idraw(1)=-3
	endif
	if((ikey.ge.1.and.ikey.le.4).or.ikey.eq.6.or.
     &	ikey.eq.8.or.ikey.eq.9) then
	   if(idraw(8).ne.0) idraw(8)=-2       !X axis label drawn at default posn
	   do i=31,55
		if(idraw(i).ne.0) idraw(i)=-2
	   enddo
	   if(idraw(9).ne.0) idraw(9)=-2       !Y axis label drawn at default posn
	   do i=56,80
		if(idraw(i).ne.0) idraw(i)=-2
	   enddo
	endif
	if(ikey.eq.8.and.lb(8).ne.0) goto 25
	if(ikey.eq.8.and.lb(9).ne.0) goto 25
c           1  2  3  4  5   6   7   8   9  10
	goto(16,17,18,19,14,112,112,112,112,115) ikey
	goto 11
c
c======================================================================
c                 FIX TICS
c======================================================================
14	continue	!FIX TICS
	nbox=10	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	text(1)=' 1: X tic ABOVE '
	text(2)=' 2: X tic CENTRE'
	text(3)=' 3: X tic BELOW '
	text(4)=' 4: Y tic RIGHT '
	text(5)=' 5: Y tic CENTRE'
	text(6)=' 6: Y tic LEFT  '
	text(7)=' 7: TIC LENGTH '
	text(8)=' 8: MINOR X TICS'
	text(9)=' 9: MINOR Y TICS'
	text(10)='10: DONE      '
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
	call CKEY(ch,ikey)
	if(ikey.eq.1) itx=1
	if(ikey.eq.2) itx=0
	if(ikey.eq.3) itx=-1
	if(ikey.eq.4) ity=1
	if(ikey.eq.5) ity=0
	if(ikey.eq.6) ity=-1
	if(ikey.ge.7.and.ikey.le.9) then
	   if(ikey.eq.7) then
 		call DEFOLTi(itlen,defolt)
		call QDIALOG(1,'Tic length',defolt,ict,cans)
		call GETINPi(cans,itlen)
	   else
		if(ikey.eq.8) then
 		   call DEFOLTi(ntx,defolt)
		   call QDIALOG(1,'Major tic for every n minor: n',
     &		defolt,ict,cans)
		   call GETINPi(cans,ntx)
		else if(ikey.eq.9) then
 		   call DEFOLTi(nty,defolt)
		   call QDIALOG(1,'Major tic for every n minor: n',
     &		defolt,ict,cans)
		   call GETINPi(cans,nty)
		endif
	   endif
	endif
	if(ikey.eq.10) goto 11
	goto 14
115	continue
	goto 306	!redraw


C==================================================
c                  'Xmin,Xmax  '
c====================================================
16	continue
C move to suitable place for typing xmin,max- use Utility Lib LOCATE
c routine to move to 2nd line down (line 1=lower line of the 2-line
c scrolling region
c If xcross=xmin,ycross=ymin at present then set croset=.false. so xcross
c reset to new xmin to keep them equal
	if(equal(xmin,xcross).and.equal(ymin,ycross)) croset=.false.
3041	continue
	call DEFOLT2r(xmin1z,xmax1z,defolt)
	call QDIALOG(1,'Xmin, Xmax',defolt,ict,cans)
	call GETINP2r(cans,xmin1z,xmax1z)
	XMIN=XMIN1z
	XMAX=XMAX1z
	if(logx) then
c=======what is next line for??!
c====	   if (xmin1z.le.0) xmin1z=0.01*(xmax1z-xmin1z)
	   if(xmin1z.lt.1.e-30.or.xmax1z.lt.1.e-30) goto 3041
	   XMIN=alog10(XMIN1z)
	   XMAX=alog10(XMAX1z)
	endif
	if(xmax.le.xmin) goto 3041
	call DCMENU(-1,4,Lb,text,0,0)	!delete box 1 only
	Lb(1)=-1
	call DCMENU(-1,4,Lb,text,icol0,icol2)	!draw box 1 only (italic)
	goto 112

c=============================================================
c				' Ymin,Ymax  '
c=============================================================
17	continue
	if(equal(xmin,xcross).and.equal(ymin,ycross)) croset=.false.
3051	continue
	call DEFOLT2r(ymin1z,ymax1z,defolt)
	call QDIALOG(1,'Ymin, Ymax',defolt,ict,cans)
	call GETINP2r(cans,ymin1z,ymax1z)
	YMIN=YMIN1z
	YMAX=YMAX1z
	if(logy) then
c=======what is next line for??!
c====	   if (xmin1z.le.0) ymin1z=0.01*(ymax1z-ymin1z)
	   if(ymin1z.lt.1.e-30.or.ymax1z.lt.1.e-30) goto 3051
	   YMIN=alog10(YMIN1z)
	   YMAX=alog10(YMAX1z)
	endif
	if(ymax.le.ymin) goto 3051
	call DCMENU(-2,4,Lb,text,0,0)	!delete box 2 only
	Lb(2)=-1
	call DCMENU(-2,4,Lb,text,icol0,icol2)	!draw box 2 only (italic)
	goto 112

c==================================================================
c			FIX CAL BARS
c==================================================================

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
	   call FIXCALIB(titlex,xcalib)		!chANGLE number in title
	   call FIXCALIB(titley,ycalib)		!chANGLE number in title
	endif
	call DCMENU(-3,4,Lb,text,0,0)	!delete box 3 only
	Lb(3)=-1
	call DCMENU(-3,4,Lb,text,icol0,icol2)	!draw box 3 only (italic)
	goto 112

c=====================================================================
c			FIX CAL BARS
c=====================================================================
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
	   call FIXCBAR(rx,ry,RLTH(21),idev,itx,ity,ntx,nty,itlen,idraw(1))
	   goto 306		!redraw
	endif
c
c Redefine t=0.  If this section is done several times need to keep the
c accumulated X offset that has been applied in XOFF1

c=====================================================================
c			DEFINE/JUMP
c=====================================================================

25	continue
	if(ncjump.eq.0.and.nvjump.eq.0.and.ikey.eq.9) goto 2501	!t=0 at cursor
	onjump=ikey.eq.8
	if(ncjump+nvjump.gt.8) call BELL(2)	!not enough boxes!
	call SETLBOX(10,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	text(9) =' 9:DEFINE t=0 '
	text(10)='10: START AT t=0 '
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
		call CURPOS(xhi,yhi)
            call CURsor(ikey,x1,y1)	!note new usage
		ch=char(ikey)
      	call spagra(x1,y1,t0,s0)
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


c***************************************************************************
c                         GRAPH SHAPE:
c Section to move graphboundary (eg if Y axis on right hand edge)
c***************************************************************************

40	continue

	nbox=10	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	Lb(8)=0
	call DCMENU(0,5,Lb,text,0,0)		!delete all
	text(1)=' 1. USE CURSORS'
	text(2)=' 2. GIVE NUMBERS '
	text(3)=' 3. DEFAULT SHAPE'
	text(4)=' 4. SQUARE SHAPE'
	text(5)=' 5. PORTRAIT SHAPE'
	text(6)=' 6. FULL WIDTH  '
	if(.not.ivplot) then
	  text(7)=' 7. I-V PLOT SHAPE'
c	  is=0
	else
	  text(7)=' 7. NOT I-V PLOT '
c	  is=1
	endif
	text(9)=' 9. PAGE PREVIEW '
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
	if(ikey.lt.1.or.ikey.gt.10) goto 405
	if(ikey.eq.7) goto 408
	if(ikey.eq.10) goto 306		!redraw
	if(ikey.eq.9) then
	   call DCMENU(0,5,Lb,text,0,0)		!delete all
	   call WDIALOG(1,'NB Anything outside red box NOT plotted',12)
c	   call PAGEV(landscap,title1,csize,icol(71))
	   goto 306		!redraw
	endif
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
c        rylo=ylo/70.	!as percent
c	   ryhi=yhi/70.	!as percent
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
	else if(ikey.eq.5) then		!portrait shape as above
	   xlo=0.32*xp            !portrait
	   xhi=0.67*xp	!axis length x=3500 y=4200
	   ylo=0.2*yp
	   yhi=0.75*yp
	   if(ycross.gt.ymin) then
	      ylo=0.15*yp 	!make lower as no need for room for numbers/title
	      yhi=0.75*yp
		xlo=0.30*xp	!make bigger to match
		xhi=0.70*xp
	   endif
	else if(ikey.eq.6) then		!full width
	   xlo=0.05*xp
	   xhi=0.95*xp
	   ylo=0.15*yp 		!make taller too (so graphboundary
	   yhi=0.8*yp		! fills whole plot area on screen)
	endif

	call GRAPHBOUNDARY(xlo,xhi,ylo,yhi)
c After chANGLE of shape by call to graphboundary must redraw ALL text
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
	  if (idraw(i).ne.0) idraw(i)=-2		!all numbers at default posn
	enddo
	do 33 i=1,80	!i=index in ANGLE,...,rybox arrays
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
	if(i.ge.11.and.i.le.30) text1(1:80)=newtext(i-10)
	if(i.eq.6) text1=parval
	if(i.eq.8) text1=xtitle
	if(i.eq.9) text1=ytitle
	if(i.eq.10) text1=title1
	if(i.ge.31.and.i.le.55) text1=cnumx(i-30)
	if(i.ge.56.and.i.le.80) text1=cnumy(i-55)
	x=(rx(i))
	y=(ry(i))
	nt=NBLANK1(text1)
		call graspa(rx(i),ry(i),xsc,ysc)
	call TEXTBOX(xsc,ysc,text1(1:nt),ANGLE(i),size(i),
     & ijus(i),rxbox(1,i),rybox(1,i),0)
33	continue
c Reset current values
	call setfnt(ifont)	!reset current values
	goto 104		!redraw (SCALE already called)
c
c ChANGLE to/from I-V plot shape (ikey=7)
408	continue
	if(ikey.eq.7) then
	   if(.not.ivplot) then
		ivplot=.true.
c		xticsav=xtic
		xtsav=xtic
		xtic=50.	!mV (or tics are too close with short X axis)
		itx=0		!tics central
		ity=0
		xcross=0.
		ycross=0.
		landscap=.false.
		doframe=.false.
	   else
		ivplot=.false.
c		xtic=xticsav
		xtic=xtsav
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


c**********************************************************************
c END OF GRAPH SHAPE OPTIONS
c**********************************************************************

c
C SECTION TO CHANGLE LOG SCALES
C  ILOG  =0 for arithmetic plot,
C	 =1 for plot Y vs log(x)
C	 =2 for plot log(Y) vs x
C	 =3 for plot log(Y) vs log(x)
c	 =4 for Hill plot
c	 =5 for sqrt(y) vs x
c	 =6 for sqrt(y) vs log(x)

c**********************************************************************
c 				GRAPH AXES
c**********************************************************************
13	continue
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	Lb(9)=0	!not yet used
	if(.not.logx) Lb(12)=0
	if(.not.logy) Lb(13)=0
c	Lb(14)=0	!not yet used
	Lb(15)=0	!not yet used
134	call DCMENU(0,5,Lb,text,0,0)		!delete all
	if(ilog.ne.0) then
	   text(1)='1: ARITHMETIC '
	else if(ilog.eq.0) then
	   if(.not.calbar) then
	      text(1)=' 1: DRAW CAL BAR(s)'
	   else
	      text(1)=' 1: DRAW AXES  '
	  endif
	endif
	text(2)=' 2: Y vs log(X) '
	text(3)=' 3: log(Y) vs X '
	text(4)=' 4: log(Y)/log(X)'
	text(5)=' 5: HILL PLOT  '
	text(6)=' 6: sqrt(Y) / X '
	text(7)=' 7: sqrt(Y)/logX'
	text(8)=' 8: QUEUE PLOT '
	text(10)='10: REDRAW   '
	text(11)=' +: MULTIPLE TRACE'
	text(14)='/ : MORE       '      !modif #1
	if(logx) then
	   if(inumx.eq.-1) then
		text(12)=' -: EXPONENT X NUM'
	   else if(inumx.eq.1) then
		text(12)=' -: FIXED X NUMBERS'
	   endif
	else
	   if(inumx.eq.-1) then
		text(12)=' -: ALLOW X SCALING'
	   else if(inumx.eq.1) then
		text(12)=' -: NO X SCALING'
	   endif
	endif
	if(logy) then
	   if(inumy.eq.-1) then
		text(13)=' x: EXPONENT Y NUM'
	   else if(inumy.eq.1) then
		text(13)=' x: FIXED Y NUMBERS'
	   endif
	else
	   if(inumy.eq.-1) then
		text(13)=' x: ALLOW Y SCALING'
	   else if(inumy.eq.1) then
		text(13)=' x: NO Y SCALING'
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
c	endif
c Start of addition for modif #1
	else if(ikey.eq.14) then
	   call DCMENU(0,5,Lb,text,0,0)		!delete all
	   nbox=15	!number of boxes for DCMENU
	   call SETLBOX(nbox,Lb,0)
	   do i=1,4
	   	Lb(i)=1
	   enddo
	   text(1)='1: 1/Y vs 1/X '
	   text(2)='2: Scatchard  '   !y/x vs y
	   text(3)='3: Eadie-Hofstee' !y vs y/x
	   text(4)='4: X/Y vs X   '
	   call DCMENU(nbox,5,Lb,text,icol1,icol2)
1312	   call CKEY(ch,ikey)
	   if(ikey.lt.-16.or.ikey.gt.15) goto 1312
	   if(ikey.eq.-16) goto 306	!ESC=redraw
	   if(ikey.ge.1.and.ikey.le.4) then
		ilogsav=ilog
		ilog=ikey+6		!ilog=7,8,9,10
		goto 133
	   endif
	   goto 306		!redraw if any other key hit
	endif
c End of addition for modif #1
	if(ikey.eq.10) goto 306		!redraw
	if(ikey.eq.11) then
	   reclen=xval(ndat(1),1)-xval(1,1)	!length of trace
	   call MULTRACE(yval,mono,xmin,xmax,ymin,ymax,idev,
     &      ndat(1),ijoin(1),icol(1),RLTH(1),ndv1,ndimd,ntrace,
     & 	ytsep,reclen,idraw(1),icol(71),
     & 	iend,np1,np2,mulpage,adcfil,csize,ifont,calfac,ioff,0)
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
	if(ikey.eq.1.and.ilog.eq.0) then	!cal bars     arithmetic
	   if(.not.zoomed) then
	   yminsav=ymin
	   ymaxsav=ymax
	   ytsav=ytic
	   ycsav=ycross
	   xminsav=xmin
	   xmaxsav=xmax
	   xtsav=xtic
	   xcsav=xcross
	   endif
	   calbar=.not.calbar
	   if(calbar) then
	      idraw(1)=-2		!calibration bars drawn at default position/length
		doframe=.false.
		i=3
 		call DEFOLTi(i,defolt)
44		call QDIALOG(1,
     &      'Calibration bar on (1) X axis (2) Y axis (3) both: n',
     &	defolt,ict,cans)
		call GETINPi(cans,i)
		if(i.lt.1.or.i.gt.3) goto 44
		if(i.eq.1.or.i.eq.3) then
		   ntx=-1000
		   if(ntxsav.ne.-1000) then		!units not yet defined
			call TDIALOG(1,'Units for X axis (eg ms):',
     &		titlex,40,.false.,ict)
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
	      xlo=0.2*xp		! reset
		doframe=.true.
	      if(xtic.le.0.) call FIXAX(xmin1,xmax1,x1,x2,xtic,0)	!get xtic
		if(ytic.le.0.) call FIXAX(ymin1,ymax1,x1,x2,ytic,0)
		ntx=5
		nty=5
		if(ntxsav.ne.-1000) titlex=titxsav		!restore axis label
		if(ntysav.ne.-1000) titley=titysav		!restore axis label
	   endif
	   calbarX=ntx.eq.-1000
	   calbarY=nty.eq.-1000
	   calbar=calbarX.or.calbarY
	   if(calbarY) then
		xlo=0.10*xp	!make wider
	   endif
	   goto 135		!revert to default positions and redraw
	endif
c
c If ikey>1 then now define ILOG
	if(ikey.le.7) then
	   ilogsav=ilog
	   ilog=ikey-1
	endif

c=====================================================================
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
     &	'(1) Give new Xmin; (2) Scale/offset data; (3) Abandon',
     &	defolt,ict,cans)
	   call GETINPi(cans,iopt)
	   if(iopt.eq.2) then
		ikey=6
		goto 114	!for SCALCURV
	   else if(iopt.eq.1) then

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
     &	'(1) Give new Ymin; (2) Scale/offset data; (3) Abandon',
     &	defolt,ict,cans)
	   call GETINPi(cans,iopt)
	   if(iopt.eq.2) then
		ikey=6
		goto 114	!for SCALCURV
	   else if(iopt.eq.1) then
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
c	   ymin1z=ymin1
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
128	   format(' For Hill plot take Y(0), Ymax = ',2g13.6)
	endif

	xmin2=xmin1	!xmin1 etc always non-log values
	xmax2=xmax1
	ymin2=ymin1
	ymax2=ymax1
	if(logx) xmin2=alog10(xmin1)
	if(logx) xmax2=alog10(xmax1)
	if(logy) ymin2=alog10(ymin1)
	if(logy) ymax2=alog10(ymax1)
c	  skip values above ymax, or below Ymin!
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
c start of modif #1
c      =7 for 1/y vs 1/x
c      =8 for y/x vs y 		!Scatchard
c      =9 for y vs y/x		!Eadie Hofstee
c      =10 for x/y vs x		!eponym?
	if(ilog.eq.7) then	!1/y vs 1/x
	   xmin2=1./xmax1
	   xmax2=1./xmin1
	   ymin2=1./ymax1
	   ymax2=1./ymin1
	else if(ilog.eq.8) then
	   xmin2=ymin1
	   xmax2=ymax2
	   ymin2=ymax1/xmax1
	   ymax2=ymin1/xmin1
	else if(ilog.eq.9) then
	   ymin2=ymin1
	   ymax2=ymax2
	   xmin2=ymax1/xmax1
	   xmax2=ymin1/xmin1
	else if(ilog.eq.10) then
	   xmin2=xmin1
	   xmax2=xmax1
	   ymin2=xmin1/ymin2
	   ymax2=xmax2/ymax2
	endif
c end of modif #1
c	do i=1,nline
c		xlb1(i)=xlb(i)
c		xle1(i)=xle(i)
cc		ylb1(i)=ylb(i)
c		yle1(i)=yle(i)
c	enddo
c	do i=1,nvline
c		xvline1(i)=xvline(i)
c		yvlb1(i)=yvlb(i)
c		yvle1(i)=yvle(i)
c	enddo
c	do i=1,nhline
c		xhlb1(i)=(xhlb(i))
c		xhle1(i)=(xhle(i))
c	enddo
c	if(logx) then
c	   do i=1,nline
c		xlb1(i)=alog10(xlb1(i))
c		xle1(i)=alog10(xle1(i))
c	   enddo
c	   do i=1,nvline
c		xvline1(i)=alog10(xvline1(i))
c	   enddo
c	   do i=1,nhline
c		xhlb1(i)=alog10(xhlb1(i))
c		xhle1(i)=alog10(xhle1(i))
c		yhline1(i)=yhline(i)
c	   enddo
c	endif
c	if(logy) then
c	   do i=1,nline
c		ylb1(i)=alog10(ylb1(i))
c		yle1(i)=alog10(yle1(i))
c	   enddo
c	   do i=1,nvline
c		yvlb1(i)=alog10(yvlb1(i))
c		yvle1(i)=alog10(yvle1(i))
c	   enddo
c	   do i=1,nhline
c		yhline1(i)=alog10(yhline1(i))
c	   enddo
c	endif
c	if(sqrty) then
c	   do i=1,nline
c		ylb1(i)=sqrt(ylb1(i))
c		yle1(i)=sqrt(yle1(i))
c	   enddo
c	   do i=1,nvline
c		yvlb1(i)=sqrt(yvlb1(i))
c		yvle1(i)=sqrt(yvle1(i))
c	   enddo
c	   do i=1,nhline
c		yhline1(i)=sqrt(yhline1(i))
c	   enddo
c	endif
	il=1
	if(.not.logx) il=0
c      if logx then following call converts xmin,xmax to log scale
	if(zoomed) then
	   call FIXAX(xmin1z,xmax1z,xminp,xmaxp,xtic,il)
c	   xmin=xmin2
c	   xmax=xmax2
	   xmin=xmin1z
	   xmax=xmax1z
	   if(logx) then
		if (xmin1z.le.0) xmin1z=0.01*(xmax1z-xmin1z)
		xmin=alog10(xmin1z)
	      xmax=alog10(xmax1z)
	   endif
	else
	   call FIXAX(xmin2,xmax2,xmin,xmax,xtic,il)
	   xminsav=xmin
	   xmaxsav=xmax
	   xtsav=xtic
	   xcsav=xcross
	endif
	il=1
	if(.not.logy) il=0
	if(zoomed) then
	   call FIXAX(ymin1z,ymax1z,yminp,ymaxp,ytic,il)
c	   ymin=ymin2
c	   ymax=ymax2
	   ymin=ymin1z
	   ymax=ymax1z
	   if(logy) then
		if (xmin1z.le.0) ymin1z=0.01*(ymax1z-ymin1z)
            ymin=alog10(ymin1z)
	      ymax=alog10(ymax1z)
	   endif
	else
	   call FIXAX(ymin2,ymax2,ymin,ymax,ytic,il)
	   yminsav=ymin
	   ymaxsav=ymax
	   ytsav=ytic
	   ycsav=ycross
	endif
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
c AFTER CHANGLE OF LOG AXES NEED TO REDRAW EVERYTHING AT DEFAULT POSN
c When, as for NEWTEXT and ARROWS then could take log/sqrt/antilog etc
c of rx,ry but this involves checking how axis CHANGLES eg might chANGLE
c from logy/logx to logy/x so Y axis is log, but not chANGLEd so no
c need to alter RY(). This not yet fixed, so log axes should be chANGLEd
c before any newtext/arrows added.
135	continue
	do i=31,80
	  if (idraw(i).ne.0) idraw(i)=-2		!all numbers at default posn
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
	goto 306		!redraw


c**********************************************************************
c 			POSH PLOT SECTION
c**********************************************************************
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
	text(1)=' 1: ADD NEW TEXT'
 	text(2)=' 2: FIX TEXT   '
 	text(3)=' 3: FIX NUMBERS '
	text(4)=' 4. FIX CAL BAR(s)'
	text(5)=' 5: ARROWS & LINES '
	text(6)=' 6: SYMBOL TYPE'
	text(7)=' 7: SYMBOL SIZE'
	text(8)=' 8: FIX DATA LINE'
	text(9)=' 9: FIX CALC LINE'
	text(10)='10. REDRAW   '
	if(doframe) then
	 text(11)=' +: OMIT FRAME'
	else
       text(11)=' +: DRAW FRAME'
	endif
	text(12)=' -: SHOW/HIDE SD'
	text(13)=' x: FIX C-JUMP LOGO'
	text(14)=' /: FIX V-JUMP LOGO'
	text(15)=' .: LINE THICKNESS'
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
219	call CKEY(ch,ikey)
	if(ikey.eq.-1) then	!F1 key
	   CALL VHELP(1)
	   GOTO 219
	endif
c   		1   2   3   4   5  6   7   8   9   10  11 12
	goto(241,232,235,233,234,116,116,116,116,306,12,404,
     &	402,402,236) ikey
	goto 219	!no valid key
c
c Section to chANGLE line type etc
c ikey=6 for symbol type; ikey=7 for symbol size
c ikey=8 for lines joining data points; ikey=9 for calc curve lines

c===========================================================================
c				 SYMBOL SIZE/TYPE ; FIX DATA/CALC LINE
c===========================================================================
116	continue
	call FIXCURV(ikey,ncurvd,icurvd,isym,syms,ijoin,logx,
     & x1calc,x2calc,ncurvc,icurvc,iline,icol,RLTH,ndimd,ndimc)
	call DCMENU(-ikey,4,Lb,text,0,0)	!delete box #ikey only
	Lb(ikey)=-1
	call DCMENU(-ikey,4,Lb,text,icol0,icol2)	!redraw (italic)
	goto 221	!back to menu
c
c     Section to alter bars (logos) that represent jumps

c===========================================================================
c				 FIX JUMPS LOGOS
c===========================================================================
402	continue
	if(ikey.eq.13) call FIXLOGO(text,xmin,xmax,ncjump,t1c,t2c,
     & xoff1,idev,RLTH(28),y1c,y2c,idraw(28),icol(71),icol(28))
	if(ikey.eq.14) call FIXLOGO(text,xmin,xmax,nvjump,t1v,t2v,
     & xoff1,idev,RLTH(29),y1v,y2v,idraw(29),icol(71),icol(29))
	goto 221
c
c Toggle error bars

c===========================================================================
c				SHOW/HIDE SD
c===========================================================================
404	continue
	call DCMENU(-8,4,Lb,text,0,0)		!delete box 8 only
	id=1
	if(ncurvd.gt.1) then
	   call SELDATA(ncurvd,id,icol,0)		!choose which data line
	   if(id.eq.-1) goto 221		!cancel
	endif
	if(id.eq.0) then 		!all sets
	   j1=1
	   j2=ncurvd
	else
	   j1=id
	   j2=id    !set id only
	endif
	do j0=j1,j2
	   j=icurvd(j0)
	   if(icurvw(j).eq.0) then
		icurvw(j)=1
	   else if(icurvw(j).eq.1) then
		icurvw(j)=0
	   else if(icurvw(j).eq.-1.and.j0.eq.id) then
		call BELL(1)
		call WDIALOG(1,'SD not defined for this set',12)
		goto 221
	   endif
	enddo
	goto 221	!more options?


c===========================================================================
c				FRAME
c===========================================================================
12	doframe=.not.doframe	!toggle frame
	goto 221	!more chANGLEs?

c===========================================================================
c				FIX CAL BARS
c===========================================================================
233	continue
	call FIXCBAR(rx,ry,RLTH(21),idev,itx,ity,ntx,nty,itlen,idraw(1))
c Modif 04/18/95 05:16pm so that if text deleted (idraw=0) it is not redrawn
	if(idraw(8).ne.0) idraw(8)=-2       !X axis label drawn at default posn
	if(idraw(9).ne.0) idraw(9)=-2       !Y axis label drawn at default posn
	goto 306		!redraw
c
c USE FIXTEXT OPTION HERE

c===========================================================================
c				FIXTEXT
c===========================================================================
232	continue

	call DCMENU(0,4,Lb,text,0,0)		!delete all
	call WDIALOG(1,'Left button=drag; right button=select text',11)

	imd=0             ! move frame only
      call GTEXT(parval,xtitle,ytitle,title1,cnumx,cnumy,cexpx,
     &  cexpy,newtext,numbx,numby,ntext,inumx,inumy,logx,logy,
     &  rx,ry,ijus,icol,ANGLE,ifnt,idraw,size,icol(71),imd,RLTH,
     &  nrealt,nrealx,nrealy,nreal,idev)

	goto 221
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
     &  logx,logy,rx,ry,ijus,icol(24),ANGLE,ifnt(7),idraw,size(7),
     &  icol(71),imd,RLTH(24),ikey,idev)
c All idraw() for numbers to be drawn should be no longer set to -2
c so numbers should be redrawn at new posn in LAXES
	goto 221


C====================================================================
c			 Add/delete Arrow
C====================================================================
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
	Lb(11)=0
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
	text(12)=' +: ADD GRID LINES'
	text(13)=' -: FIX GRID    '
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
	call CKEY(ch,ikey)
	if(ikey.lt.1.or.ikey.gt.15) goto 221	!no valid key
c start of arrow- need to store start/end points, and number of arrows,
c so they can be reproduced when graph redrawn
	if(ikey.eq.1.or.ikey.eq.6) then
	   call draw_arrow(ikey,narrow,xe,xb,ye,yb,ixfix,iyfix,
     &   xv,yv,csize,idev,xabs1,yabs1,xabs2,yabs2,RLTH,zoomed,iver)
	call broken(0)
	else if (ikey.ge.2.and.ikey.le.4) then
	   call draw_lines(ikey,nline,xlb,xle,ylb,yle,iltype,
     &   nhline,yhline,xhlb,xhle,ilhtype,
     &   nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel,
     &   csize,idev,xabs1,yabs1,xabs2,yabs2,RLTH,
     &   xmin,xmax,ymin,ymax,zoomed,iver)
	call broken(0)

	else if(ikey.eq.13.or.ikey.eq.12) then    !add/fix grid (hor + vert lines)
	   call BELL(2)
	   call WDIALOG(1,'Grid not yet fixed',12)
	else if(ikey.eq.7.or.ikey.eq.8.or.ikey.eq.9) then           !fix lines
	   if(ikey.eq.7.and.nline.eq.0) goto 234	!nothing to fix
	   if(ikey.eq.8.and.nhline.eq.0) goto 234		!nothing to fix
	   if(ikey.eq.9.and.nvline.eq.0) goto 234		!nothing to fix
	   call FIXLINES(ikey,nline,xlb,xle,ylb,yle,iltype,
     &	 nhline,yhline,xhlb,xhle,ilhtype,ihlinrel,
     &	 nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,
     & 	 RLTH,icol,icol(71),idev)
	call broken(0)
	else if(ikey.eq.10) then
	     goto 221
	endif

	goto 234

c========================================================================
c 					Line thickness
c========================================================================
236	continue
	imode=1	!for line thickness
	call SETATT(narrow,nline,ntext,isdev,
     & RLTH,icol,icol0,icol1,icol2,ncurvd,icurvd,ncurvc,icurvc,
     & ndimd,ndimc,ifitype,imode)
	call broken(0)
	goto 306	!redraw



c============================================================================
c 					ADD TEXT
c============================================================================
241	continue
	if(ntext.le.19) goto 2411	!OK- room for another one
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
c		size(j)=size(n)
		size(j)=size(n)
		ANGLE(j)=ANGLE(n)
		ijus(j)=ijus(n)
		RLTH(j1+30)=RLTH(n+30)
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
	call WDIALOG(1,'Define start position for text with mouse',-ict)
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
	call GSTRING(xc,yc,newtext(ntext),ifnt(n),ANGLE(n),size(n),
     &	ijus(n),ic,icol(71),80,.true.,nreal(ntext))
	nl=NBLANK(newtext(ntext))
	if(nl.ge.1) then
	   call graspa(rx(n),ry(n),xsc,ysc)
	   call TEXTBOX(xsc,ysc,newtext(ntext)(1:nl),ANGLE(n),size(n),
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
c	ictm=15		!text white
c	ibkm=8		!background dark grey
c	icfm=1		!frame dark blue
c	icupm=12		!upper case red
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
		call setcls(.true.,icol,autplt,plotcols,isetcol)
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
		  call jSYMBOL(x1,y1,4,csize,12,idev)   !mark bottom left with red diamond
		  call CURpos(xhi,yhi)
		  call cursor(ikey,x2,y2)
		  ch=char(ikey)
		  call jSYMBOL(x1,y1,4,csize,12,idev)   !mark bottom left with red diamond
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
	        wmeta=.false.
	        wbmp=.false.
		  goto 777
	   end select
	   if(mpos.eq.1.or.mpos.eq.2.or.mpos.eq.3.or.mpos.eq.4) then
	      thick=0.5		!line thickness multiplier
	      csize=csize*cfacsml
	      thick=thick*thfacsml
	      xloq=vxloq+0.6*s1*xp	! for GRAPHBOUNDARY- same for all
	      xhiq=vxloq+0.55*s2*xp
	      yloq=vyloq+0.5*s1*yp
	      yhiq=vyloq+0.52*s2*yp
	   endif
	else if(ihelp.eq.4.or.ihelp.eq.5) then
	   meta=.true.
	   idev=2
 	   if(ihelp.eq.4) then
c     	call setcls(.false.,icol,autplt,plotcols,2)
	   else
          	call setcls(.true.,icol,autplt,plotcols,2)
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
c	   call WDIALOG(1,
c     &'Sorry this option is not available for computers with less then
c     &32MB ',ict)
c	   goto 777
	     wmeta=.true.
	     idev=1
      else if(ihelp.eq.7) then
c	   call WDIALOG(1,
c     &'Sorry this option is not available for computers with less then
c     &32MB ',ict)
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
	   if(ihelp.eq.1.or.ihelp.eq.2.or.ihelp.eq.3) then
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
	   wmeta=.false.
	   wbmp=.false.
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
	   call wmf(60,640,480)
	   call rgbdef(0,0.0,0.0,0.0)
	   call errswi(-1)
	   call brkswi(1)
	   call gsetcols(0)
	else if(wbmp) then
	   call WDIALOG(1,
     &   'Assembling the bmp. . .',ict)
	   call devend
	   call bmp
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
	   else if(ihelp.eq.1) then
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
	meta=.false.
	CLOSE(UNIT=ICGMDV)
	CALL DEVEND
	goto 9876
5558  continue
	call WDIALOG(1,'wmf / bmp done. . .',12)
	wmeta=.false.
	wbmp=.false.
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
      call setcls(.false.,icol,.false.,plotcols,0)
	xlo=xlo2		!restore screen GRAPHBOUNDARY
	xhi=xhi2
	ylo=ylo2
	yhi=yhi2
	vxlo=0		! for VIEWPORT
	vxhi=xp
	vylo=0
	vyhi=yp
	goto 1081

c=================================================================================7
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
	   if(ihelp.eq.1) then
	   printfil='efit'//kpchar(1:nc1)//'.out'
	   else
	   printfil='lfit'//kpchar(1:nc1)//'.out'
	   endif
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
		call setcls(mono,icol,autplt,plotcols,isetcol)
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



c===========================================================================
c PLOT QUEUE SECTION
301	continue
c For VPLOT restore weight,isdev; add (for jump logos):
c ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,y1c,y2c,ivplot,
 	call VPLQ5(xval,yval,xcal,ycal,ndimd,ndimc,
     & ncurvd,ndat,icurvd,isym,ijoin,ncurvc,ncal,icurvc,iline,syms,
     & xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ntx,nty,itx,ity,
     & xlo,xhi,ylo,yhi,itit,title1,ifont,ilog,iscal,doframe,
     & titlex,titley,ilabel,inumx,inumy,idiskq,qfile,sval,
     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,
     & theta,ifitype,ncomp,idest,icol,mono,interp,
     & RLTH,RX,RY,rxbox,rybox,IJUS,ANGLE,idraw,ifnt,SIZE,
     & cnumx,cnumy,cexpx,cexpy,numbx,numby,
     & narrow,xb,yb,xe,ye,ntext,nline,xlb,ylb,xle,yle,iltype,
     & newtext,y0,yinf,
     & nhline,yhline,xhlb,xhle,ilhtype,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel,
     & ntrace,ytsep,ndv1,ndc1,
     & weight,kwi,kwj,icurvw,kmax)
	goto 306		!redraw
c
c 				end of plot queue section
c###########################################################################

c=====================================================
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
	if(iask.gt.0) call DCMENU(0,5,Lb,text,0,0)	!delete boxes before exit
 	if(iask.lt.0)call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	if(ntxsav.ne.-1000) titlex=titxsav		!restore axis label
	if(ntysav.ne.-1000) titley=titysav		!restore axis label
	deALLOCATE(xscale,yscale,yoffset)
	deALLOCATE(x1calc,x2calc)
	RETURN
	END

c###############################################

