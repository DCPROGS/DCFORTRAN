c================================================================================
	subroutine LAXES2(xmin,xmax,ymin,ymax,XTIC,YTIC,xcross,ycross,
     & ntx,nty,itx,ity,itlen,titlex,titley,xtitle,ytitle,ilabel,idev,
     & cf7,cf8,cf9,ifont7,ifont8,ifont9,thick,lth,inumx,inumy,
     & xwbase,ilog,doframe,mono,icol,xcalib,ycalib,
     & rx,ry,iangle,ijus,idraw,rxbox,rybox,
     & numbx,numby,cnumx,cnumy,cexpx,cexpy)
c================================================================================
c
c 05/30/89 10:21am Lahey/Hgraph version of LAXES6
c
c Modif 06/18/93 09:01am to definition of inumx,inumy (see below)
c Modif 02/01/93 04:09pm to include mono and icol() as arguments for colour
c   and also xcalib,ycalib (length of cal bars)
c Modif 09/03/91 10:06am to allow calibration bars (rather than normal axes)
c	to be drawn.  Signalled by setting ntx=-1000. Length of the bars (in
c	world units, eg pA,ms) kept thus:
c	rx(1),ry(1)=origin of X-calibration bars (world units),
c	rx(2),ry(2)=end of X calibration bars
c	rx(3),ry(3)=origin of Y-calibration bars (world units),
c	rx(4),ry(4)=end of Y calibration bars
c     (horizontal and vertical so ry(1)=ry(2), and rx(3)=rx(4)
c	idraw(1) controls whether bars are drawn or not
c
c 02/06/90 09:28am LAXES2 draws all axes 'by hand'. Add parameters
c  ITX=1 if x tics above axis (normal)
c  ITX=0 if x tics centered on axis
c  ITX=-1 if x tics below axis
c  ITY=1 if y tics to right of axis (normal)
c  ITY=0 if y tics centered on axis
c  ITY=-1 if y tics to left of axis
c 01/26/90 05:48pm Altered so that not rescaled with SX,SY when scale
c factor used for axes: this no longer needed when numbers drawn
c separately (not by DRAWAXIS) and makes it much harder to keep track
c of text positions after rescaling.
c LAXES1=Version for axes that cross. All axis numbers drawn separately
c so that they can be moved, and their size changed.
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
c   Draw ALL numbers on given axis at internally calc position (in which
c case their boxes are defined by TEXTBOX), or ALL at position recorded
c by textbox call. IDRAW(31)=-2 forces the former for X axis, and IDRAW(56)=-2
c for Y axis; when former required (ie at first call, and after rescale)
c set ALL the idraw()(31-55) and (56-80) that control axis numbers
c to -2 because there may be a different set of axis numbers after rescale
c	 Line thickness for whole page plots in lth(i) as follows ( actual
c thickness =THICK*lth(i) were THICK=THBIG=1.0 for whole page)
c 1-10= data sets (#1-10) (symbol and joining-line if any);
c 11-20=calc curves (#1-10);
c 21=axes; 22=frame; 23=axis labels (same for X,Y)
c 24=axis numbers (same for X,Y); 25=plot title; 26=param value text;
c 27=symbols; 28=SD bars; 29-30=spare;
c 31-50=extra text (#1-20); 51-60=arrows (#1-10); 61-70=lines (#1-10)
c 71-100 =spare for future
C [OLD- 31-35=extra text (#1-5); 36-40=arrows (#1-5)]
c
c
c (1) Arg CHH replaced by CSIZE (=0.5-18.0); cf7,cf8,cf9 as above
c (2) TICSIZ now itlen(eg=250)
c (3) nlenx,nleny not needed (call TRIM and NBLANK does the job)
c (4) call to TRIM (Hgraph) following call to NBLANK (Lahey) often
c	 (always?) seems to remove the last bit of STRING to be
c	concatenated on. Solve by using FIXFORM followed by HFORMAT
c	to get number in right format.
c (5) Hgraph needs Xtic,Ytic for major tics (not minor as DEC)- fixed in
c	SCVD1 (for EKDIST) (and FIXAX)
c (6) Exponential format labelling of log axes (when inum=1, not -1) now done
c (7) Square root scale.
c If major tics are ytic1=1. or 2. or 3. units apart on the sqrt scale then
c major tics numbered 0,1,2,4,9,.. for ytic1=1, ytic=1.;
c major tics numbered 0,4,16,36,64,100 for ytic1=2., ytic=4.;
c major tics numbered 0,9,36,81,144 for ytic1=3, ytic=9.;
c      Minor tics drawn between major ones according to NTY but there is
c	no obvious natural spacing for the minor tics so may be better to
c	have none (nty=1) and have major ticks quite close. For larger
c	ymax use ytic=4.,nty=4. The minor
c	tics are equally-spaced on the real (non-sqrt) scale, so with
c	3 minor tics (nty=4) between the tics labelled 0 and 4 minor tics
c	are at 1,2,3. Between the tics labelled 4 and 16 they would
c	be at 4+3=7, 4+6=10 and 4+9=13 (next=4+12=16)
c (8) FRAME now drawn in LAXES0 if DOFRAME=true- easy to get scale right
c 	if drawn here after call SCALE
C (9) ILABEL NOT YET FIXED
c (10) Line thickness in lth(i) as follows
c Line thickness for whole page plots in lth(i) as follows ( actual
c   NB frame goes whole way round so thick frame also effectively
c	thickens axes too, without thickening numbers on axes.
c (11) IFONT added to arg list as font number for axis labels
c	 on 0-11 scale (see manual p57, and SETFONT1.FOR)
c
c Modified 11/04/89 10:47pm:
c LAXES0 altered by inclusion as a parameter of arrays RX(),RY(), with
c coords for position of x-label, and for y-label. And
c NB start posn for text and arrows (and axis labels in LAXES)must be
c kept in world coord (real) not device coord (integer) or they come
c out in wrong place if graph outline is changed (as in 'FIX ON VDU')
c so rx(),ry() must hold world coord
c
c LAXES6 allows log scales to be labelled with non-log values in Fn.d format
c rather than with exponent notation (10**n) ( controlled by new
c parameters INUMX, INUMY)- note values diff from PDP version
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
c LAXES5 is version that call test prog LABEL1
c***Modif Jan 88: if NTX is negative then numbering is omitted from X axis
c		but major tic still drawn (for every -NTX minor tics)
c		Similarly for Y axis if NTY<0
c		If ntx/y<0 then also omits scale factor and sets FXD=0
c		(giving integers for numbers subsequently put on axes)
c** July 87-  calls LABX1,LABY1 which use LABEL1 rather than LABEL
c**Modif May 87 to do sqrt(y) scale.
c LAXES4 is much altered DC version of earlier LAXESn versions
c Rather long! (-could shorten by use of subroutines that will do
c both X and Y axes, and labels- tried in LAXES5.OLD - the .FOR is shorter
c but the .OBJ is longer!)
c	(1) logx,logy given as param and log scales labelled
c		as such, with non-log values on decades in
c		exponent form. Xtic,Ytic ignored if logx,logy resp.
c	(2) Xmin,...,Ymax now param rather than taken from COMMON.
c		 For log scales they are log10 values.
c		 For sqrt(y) scale they are original values but
c		  must call scale(xmin,xmax,sqrt(ymin),sqrt(ymax))
c		No need for special plotter-scaled values from
c		COMMON in this version.
c	(3) Non-log scales assumed to start with a major tic
c		at xmin at present, but log scales have 1st
c		major tic at an integer (log) value.
c	(4) If axes are at MAX then tics drawn inwards but still
c		needs modif to draw numbers outside axes in this case
c	(5) ILABEL=0 no labels put on either axis
c	    ILABEL=1 labels drawn at left/bottom wherever axes are
c		with writing starting from XMIN/YMIN
c	    ILABEL=2 labels drawn on axes starting to right of xcross
c		and above ycross (so labels should not be too long)
c	    ILABEL=3 labels on axes but starting on left for X axis
c		and at bottom for Y axis
c	(6) TicSiz=3. is  usually OK for tic size.
c	    A major tic is drawn every NTX (NTY) minor tics if
c	    not a log scale.
c
c LAXES2 is same as LAXES1 except for modif in TIC and LABAX for use
c with GRAFDC.OBJ in which ISIZE values are bigger (*3)
c Modification of LAXES.FOR (from GRFLIB) by DC Aug 1986
c   (1) Scales plotted numbers by an appropriate factor of 10
c	Numbers have 3 digits + decimal point so have form
c	9.99, 99.9 or 999. (FXD set to 2,2, 1,1 or 0,0 resp)
c	For numbers outside this range scaled (and labelled)
c	by appropriate factor of 10 for the 9.99 format (FXD(2,2))
c   (2) Labels axes with the factor used (this file contains the
c		modified TIC routine that does this= TIC1)
c   (3) Puts labels on axes if req (this file contains the
c		modified LABAX routine that does this= LABAX2)
c   (4) If xwbase pos (eg=10.) add 'per 10 ms' to Y label for freq dens plots
c	If xwbase neg (eg=-10.) add 'per 10 pA' to Y label
c 	No addition if xwbase=0.
c
	character*40 titlex,titley
	character*75 xtitle,ytitle
	character*10 string		!for components of titles
	character*80 str		!for components of titles
	character*6 frmty
c	character*12 frmt,frmtx
	character*8 x10x,x10y		!for ' x10' on x,y axes
	character*2 fnt,fnt2,sze		!for SIZFNT
	character*5 newfnt
	character*1 up,down,font,ch
	character*10 cnumx(50),cnumy(50),cexpx(50),cexpy(50)
c	character*1 backsp,size,font,nline
c arrays for position of text strings:
	real RX(100),RY(100)
	real rxbox(4,100),rybox(4,100),iangle(100)
	integer IJUS(100),idraw(100)
	integer icol(100),half
	integer left,right,center
	integer*2 lth(100),lth
	logical logx,logy,logity,EQUAL,sqrty,doframe,calbarX,calbarY,mono
	logical debug,caplock
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu

	debug()=caplock()
	EQUAL(xeq,yeq)=abs(xeq-yeq).lt.0.00001
	lth(i)=ifixr(thick*float(lth(i))) !sets line thickness
11	format(' in LAXES2')
	logx=ilog.eq.1.or.ilog.eq.3.or.ilog.eq.4.or.ilog.eq.6
	logy=ilog.eq.2.or.ilog.eq.3
	logity=ilog.eq.4		!for Hill plot
	sqrty=ilog.eq.5.or.ilog.eq.6
	calbarX=ntx.eq.-1000
	calbarY=nty.eq.-1000
	wl=0.01
	if(wymax.lt.50) wl=0.005
	nolabel=0
	if((wxmax-xmax)/(wxmax-wxmin).lt.0.1) nolabel=1
	left=-1	!ditto
	right=1		!ditto
	center=0
	if(logx) xtic=1.0
	if(logy) ytic=1.0
	tlenx=(ymax-ymin)*0.05
	tleny=(xmax-xmin)*0.05
c Define ' x10' so that 'x' is always in duplex font (looks like mult
c sign), but font reset to IFONT for '10'
c Set size,font for axis label text

	x10x=' '//fnt2//'x'//fnt//'10'	!'x10' for x axis

	x10y=' '//fnt2//'x'//fnt//'10'	!'x10' for y axis
c
c

c===============================================
	ymax1=ymax
	ymin1=ymin
	ycros1=ycross
	if(sqrty) then
	   ymax1=sqrt(ymax)	!for comparison with pos
	   ymin1=sqrt(ymin)	!display thinks this is ymin,ymax
	   ycros1=sqrt(ycross)
c	   tlenx=(ymax1-ymin1)*0.05
	endif
	if(calbarX) goto 31
	nxf=0		!used if numbering omitted (ntx<0)
	if(logx.and.inumx.eq.-1) nxf=4	!temp fix to allow down to 0.0001
	nx=0		!ditto, so no scale factor written
	if(ntx.lt.0) goto 31
	if(logx) goto 31
	if(.not.logx.and.inumx.eq.-1) goto 31	!no scale factor allowed
	amax=abs(xmax)
	if(abs(xmin).gt.amax) amax=abs(xmin)
	nxf=2-ifix(alog10(amax))
	if(nxf.ge.0.and.nxf.le.2) then
	   nx=0
	else
	   nx=nxf-2
	   nxf=2
	endif
c============================================
31	continue	!now Y values
c===========================================================

	if(calbarY) goto 33
	nyf=0		!used if numbering omitted (nty<0)
	if(logy.and.inumy.eq.-1) nyf=2	!temp fix to allow 0.01,0.1
	ny=0		!ditto, so no scale factor written
	if(nty.lt.0) goto 33
c Do following bit even if logy to calc idmax which is used to decide
c on left shift before putting numbers on log Y axis (bigger shift
c for bigger exponents). This is still wrong (03/07/91 06:46pm) if, eg,
c ymax=52.234 because it leaves room for the '.234' whereas numbers actually
c drawn at integer multiples of ytic ( eg 45,50,55 etc) so try using
c ytic rather than amax (below)
	amax=abs(ymax)
	if(abs(ymin).gt.amax) amax=abs(ymin)
c     Try calc of max number of digits on Yaxis numbers (at present used
c     for calc of offset of Y-axis label only) as IDMAX, as follows:
	nmax=3	!max number of sig figs after decimal point
c	call FIXDEC1(amax,m0,n,nmax)
c	idmax=m0+n+3
	y=float(ifix(amax/ytic))*ytic	!=largest number drawn
	call FIXDEC1(y,m0,n,nmax)	!try this 03/07/91 06:55pm
	idmax=m0+n+3
	if(n.eq.0) idmax=m0+n+1		!no dec point
	if(logy.and.inumy.eq.1.and.ymin.lt.0.) idmax=idmax+1	!for neg sign in exp
c	if(idmax.gt.7) idmax=7
	nd=1+ifix(alog10(amax)+0.001)
	if(ymin.lt.0.) nd=-nd	!signal to MAKEXP to leave room for minus sign
c     end of calc of idmax (except modif below if numbers scaled)
	if(logy) goto 33
	if(.not.logy.and.inumy.eq.-1) goto 33	!no scale factor allowed
	nyf=2-(iabs(nd)-1)
	if(nyf.ge.0.and.nyf.le.2) then
c	if(nyf.ge.-1.and.nyf.le.2) then
	   ny=0
	else
	   ny=nyf-2
	   nyf=2
	endif
	if(ny.ne.0) idmax=idmax-iabs(ny)+2	!numbers take less space if scaled
c
c Make format into string for use in setxlabel call

c===========================================================
33	continue
c===========================================================

	nmax=3	!max number of sig figs after decimal point
	frmty='7.'//char(nyf+48)	!='7.nyf'
	if(logy) frmty='3.1'		!does this control title offset?-YES
c
c If log scale required 1st major tic is at 1st integer (log) value
c so find if this is at xmin
c For sqrt(y) scale 1st major tick at Ymin as usual.
	nx1=iabs(ntx)	!mod Jan 88
	ny1=iabs(nty)
	if(calbarX) goto 40
	if(.not.logx) goto 40
	nx1=9		!major tic every 10th for log axis
	if(abs(amod(xmin,1.)).lt.0.0001) goto 40	!xmin is integer
	n=ifix(xmin)
	if(xmin.lt.0.) n=n-1	!find nearest integer below xmin
40	continue
	if(calbarY) goto 41
	if(.not.logy) goto 41
	ny1=9		!major tic every 10th for log axis
	if(abs(amod(ymin,1.)).lt.0.0001) goto 41	!ymin is integer
	n=ifix(ymin)
	if(ymin.lt.0.) n=n-1	!find nearest integer below ymin
41	continue
c
c
c Construct axis titles here, in XTITLE,YTITLE
c First XTITLE
c  -initialise X and YTITLE to blanks
	call CHAFNT(ifont8)
	call SETSIZE(cf8)	!this defines ISIZE
	do 19 i=1,75
	xtitle(i:i)=char(32)
19	ytitle(i:i)=char(32)
	nlx=NBLANK1(titlex)
c	if(ilabel.ne.0.and.nlx.gt.0) xtitle=titlex(1:nlx)
c Problem with titles that contain 'equal to or grater than' sign=char(242)
c This prints OK but not recognized by Hgraph; but latter has this sign
c in greek font (as '>') so must replace ('A'=greek)
	if(ilabel.ne.0.and.nlx.gt.0) then
	   k=0
	   do 18 i=1,nlx
	   ch=titlex(i:i)
	   ic=ichar(ch)
	   if(ic.ne.242) then
	      k=k+1
	      xtitle(k:k)=ch
	   else if(ic.eq.242) then
		newfnt=font//'A'//'>'//font//char(ifont8+48)
		do 181 j=1,5
		k=k+1
181		xtitle(k:k)=newfnt(j:j)
	   endif
18	   continue
	endif
	if(nx.eq.0.or.logx.or.calbarX) goto 20
c add 10**nx to xtitle
	nlx=NBLANK1(xtitle)
	string=char(iabs(nx)+48)		!ascii for number NX
	if(nx.lt.0) string='-'//string
	nmax=3	!max number of sig figs after decimal point
	call FIXDEC1(float(nx),m0,n,nmax)
c	call FIXFORM(float(nx),m0,n,frmt)
	n=0
	call DCFORMAT(float(nx),m0+n+3,n,string)
	ns=NBLANK1(string)
c	xtitle=xtitle(1:nlx)//' x10'//up//string(1:ns)//down
	if(nlx.ge.1.and.ns.ge.2) then
	   xtitle=xtitle(1:nlx)//x10x//up//string(2:ns)//down
	endif
20	continue
	if(logx) then
	   nl=NBLANK1(xtitle)
	   if(nl.ge.1) xtitle=xtitle(1:nl)//'(log scale)'
	endif
21	continue
c
c and YTITLE
	call CHAFNT(ifont9)
	call SETSIZE(cf9)	!this defines ISIZE
	if(ilabel.eq.0) goto 22		!add scale factor?
	nly=NBLANK1(titley)
	if(nly.ge.1) ytitle=titley(1:nly)
	if(calbarY) goto 22
c  If xwbase pos (eg=10.) add 'per 10 ms' to Y label for freq dens plots
c  If xwbase neg (eg=-10.) add 'per 10 pA' to Y label
c  No addition if xwbase=0.
	if(xwbase.gt.-1.e-20.and.xwbase.lt.1.e-20) goto 22	!skip
c###need to choose format better (trailing zeroes are not blanks!)
	nmax=3	!max number of sig figs after decimal point
	call FIXDEC1(abs(xwbase),m0,n,nmax)
c	call FIXFORM(abs(xwbase),m0,n,frmt)
	call DCFORMAT(abs(xwbase),m0+n+3,n,string)
c	call TRIM(string)		!removes leading blanks?
	ns=NBLANK1(string)
	if(nly.ge.1.and.ns.ge.1) then
	   ytitle=ytitle(1:nly)//' per'//string(1:ns)
	endif
	nly=NBLANK1(ytitle)
	if(nly.ge.1) then
	   if(xwbase.gt.0.) ytitle=ytitle(1:nly)//' ms'
	   if(xwbase.lt.0.) ytitle=ytitle(1:nly)//' pA'
	endif
c label is O.K. here, but last ' ms' gets lost later
22	continue
c Now scale factor
	if(ny.eq.0.or.logy.or.calbarY) goto 23
c add 10**ny to ytitle
	nly=NBLANK1(ytitle)
c following line will prob not deal with negative NY
c	string=char(ny+48)		!ascii for number NY
c if only one digit (and pos sign) in ny then need only use
	if(ny.gt.0) then
	   string(1:1)=char(ny+48)
	   ns=1
	else if(ny.lt.0) then
	   string(1:1)='-'
	   string(2:2)=char(iabs(ny)+48)
	   ns=2
	endif
	if(nly.ge.1.and.ns.ge.1) then
	   ytitle=ytitle(1:nly)//x10y//up//string(1:ns)//down
	endif
23	continue
	nly=NBLANK1(ytitle)
	if(logy) ytitle=ytitle(1:nly)//'(log scale)'
	if(sqrty) ytitle=ytitle(1:nly)//'(square root scale)'
	if(logity) ytitle=ytitle(1:nly)//'(logit scale)'
c
c Rescale: if nx not zero need to scale the numbers plotted on
c the axes by 10.**nx- can do this here by temporarily changing
c the scaling, then revert to original as soon as axes drawn (see Hgraph p14)
	if(logx.or.calbarX) nx=0	!to be sure- done above?
	if(logy.or.calbarY) ny=0	!to be sure- done above?
	sx=10.**nx
	sy=10.**ny
	if(sqrty) sy=1.0
	if(doframe)then
	 if(idev.ge.5) then
c	   call LOADVTB('2'//'W'//char(lth(22)))
	 else
	   t4=lth(22)
	   call LINWID(0.0)
	 endif
	 if(.not.mono) call lincols(icol(22))
	 call LINWID(0.0)
	 call DFRAME(xmin,xmax,ymin1,ymax1,0)	!linetype=0
	endif

c===============================================
C ----- AXES -----------------------------------
c===============================================

      idr=idraw(1)
	if(calbarX.or.calbarY) then
	   if(idev.ge.5) then
c	     call LOADVTB('2'//'W'//char(lth(21)))
	   else
	     t4=lth(21)
	     call LINWID(0.0)
	   endif
	   if(.not.mono) call lincols(icol(21))
	   if (idr.le.-2) then	!draw at default position if idraw=-2,-3
		if (idr.eq.-3) then		!use current length but default posn
		   xcalib=rx(2)-rx(1)		!xcalib=current length
		   ycalib=ry(4)-ry(3)
		else
		  if(calbarX) then
		    call FIXAX(xmin,xmax,x1,x2,x,0)		!default length
c		    xcalib=2.*x		!2 major 'tics'
		    xcalib=x		!1 major 'tic'
		  endif
		  if(calbarY) then
		    call FIXAX(ymin,ymax,x1,x2,x,0)
c		    ycalib=2.*x		!2 major 'tics'
		    ycalib=x		!1 major 'tic'
		  endif
		endif
		idraw(1)=1			!reset
c	      Get default position
		rx(1)=xmin+0.02*(xmax-xmin)		!x-origin
		ry(1)=ymin+0.02*(ymax-ymin)
		rx(3)=rx(1)			!y origin
		ry(3)=ry(1)			!y origin
		ry(2)=ry(1)			!horizontal
		rx(4)=rx(3)			!vertical
c	   Set bar lengths
		if(calbarX) then
		  rx(2)=rx(1)+xcalib
	        call FIXCALIB(titlex,xcalib)		!change number in title
c	        xtitle(1:40)=titlex
		endif
		if(calbarY) then
		  ry(4)=ry(3)+ycalib
	        call FIXCALIB(titley,ycalib)		!change number in title
c	        ytitle(1:40)=titley
		endif
c     (horizontal and vertical so ry(1)=ry(2), and rx(3)=rx(4)
	      call DCALBAR(rx,ry,lth(21),idev,1,itx,ity,ntx,nty,tlen)
	   else if(idraw(1).eq.1) then
	      call DCALBAR(rx,ry,lth(21),idev,idraw(1),itx,ity,
     &	   ntx,nty,tlen)
	   endif
c	   if(idraw(1).eq.0) then	!no labels if no bars drawn
c		idraw(8)=0		!but if this done can't undelete labels easily
c		idraw(9)=0
c	   endif
c First x-label
	   call CHAFNT(ifont8)
	   call SETSIZE(cf8)	!this defines ISIZE
         xtitle(1:40)=titlex
         ytitle(1:40)=titley
	   nlx=NBLANK1(xtitle)
	   if(nlx.lt.1) nlx=1
	   if(.not.calbarX) goto 50
	   if(idraw(8).eq.-2) then
c No position defined yet for X bar label so calc start pos for label here:
		idraw(8)=1		!reset IDRAW
		ymid=0.5*(ymin + ymax)
		x=(0.5*(rx(1) + rx(2)))
c		ih=charheight(isize)
	  	iangle(8)=0			!set during initialisation
	  	ijus(8)=center
c		if(ry(1).gt.ymid) iyjus(8)=below	!label above x-bar
		call getsize(cf8,w,h)
		y=ry(1)-h/2		!Y posn of X bar
	  	rx(8)=x		!keep start pos for x axis label
	  	ry(8)=y
c     define rxbox,rybox for posn of XTITLE
	  	call TEXTBOX(x,y,xtitle(1:nlx),iangle(8),cf8,
     &       ijus(8),rxbox(1,8),rybox(1,8),0)
c
c		if(debug()) print 700,1,ix,iy,ih,nlx,xtitle(1:nlx)
c700		format(' pos,ix,iy,ih,nl,Xtitle= ',5i8,/,a75)
		if(idev.ge.5) then
c		   call LOADVTB('2'//'W'//char(lth(23)))        !axis labels
		else
	   	   t4=lth(23)
	   	   call LINWID(0.0)
		endif
	      if(.not.mono) call lincols(icol(23))
		if(nlx.gt.1) call JUSTIFYSTRING(x,y,xtitle(1:nlx),
     &	iangle(8),cf8,ijus(8))
	   else if(idraw(8).eq.1) then
		x=(rx(8))
		y=(ry(8))
	      if(.not.mono) call lincols(icol(23))
		if(nlx.gt.1) call JUSTIFYSTRING(x,y,xtitle(1:nlx),
     &	iangle(8),cf8,ijus(8))
	   endif

c??????????????????????????????????????
c Now Y-label
50	   continue
	   if(.not.calbarY) goto 51
	   call CHAFNT(ifont9)
	   call SETSIZE(cf9)	!this defines ISIZE
	   if(sqrty) ytic1=sqrt(ytic1)
	   nly=NBLANK1(ytitle)
	   if(nly.lt.1) nly=1
	   idr9=idraw(9)
	   if(idr9.eq.-2) then
		idraw(9)=1		!reset IDRAW
		xmid=0.5*(xmin + xmax)
		y=(0.5*(ry(3)+ry(4)))
	      call GETSIZE(cf9,W,H)	!this defines ISIZE
c		iw=charwidth(isize)
	  	iangle(9)=90		!set during initialisation
	  	ijus(9)=left
c	  	iyjus(9)=center
		roff=w
		if(rx(3).gt.xmid) Roff=-Roff
		x=(rx(3))		!X posn of Y bar
		x=x - Roff		!move left to start axis label
	  	rx(9)=x
	  	ry(9)=y
c        define rxbox,rybox for posn of YTITLE
	  	call TEXTBOX(x,y,ytitle(1:nly),iangle(9),CF9,
     &       ijus(9),rxbox(1,9),rybox(1,9),0)
		if(idev.ge.5) then
c		   call LOADVTB('2'//'W'//char(lth(23)))        !axis labels
		else
	   	   t4=lth(23)
	   	   call LINWID(0.0)
		endif
	      if(.not.mono) call lincols(icol(23))
		if(nly.gt.1) call JUSTIFYSTRING(x,y,ytitle(1:nly),
     &      iangle(9),cf9,ijus(9))
	   else if(idraw(9).eq.1) then
		x=(rx(9))
		y=(ry(9))
		if(idev.ge.5) then
c		   call LOADVTB('2'//'W'//char(lth(23)))        !axis labels
		else
	   	   t4=lth(23)
	   	   call LINWID(0.0)
		endif
	      if(.not.mono) call lincols(icol(23))
		if(nly.gt.1) call JUSTIFYSTRING(x,y,ytitle(1:nly),
     &      iangle(9),cf9,ijus(9))
	   endif

c===========================================================
51	   continue
c===========================================================
c	   goto 999		!all finished
	endif		!end of calibration bar drawing
c
c Define tic lengths in device coord (itlen=major tic length in
c device coord)
c Line thickness for the axes (lth(21), axis labels (lth(23)) and axis
c numbers (lth(24)) all controlled separately.
c 	21=axes; 22=frame; 23=axis labels (same for X,Y)
c	24=axis numbers (same for X,Y); 25=plot title;
	call CHAFNT(ifont8)
	call SETSIZE(cf8)	!this defines ISIZE
	xtic1=xtic
	nlx=NBLANK1(xtitle)
	if(nlx.lt.1) nlx=1
c MODIF: to allow separate line thickness control of axis titles, always
c draw them separately, as in (3) below. Calc the starting position for
c the axis label here (as long as both coordinates negative; otherwise
c give precedence to input values)
c IDRAW(8)=0 if x-axis label to be omitted; =1 if to be drawn separately
c by JUSTIFYSTRING (not drawn in DRAWAXIS) starting at a default posn calc
c in LAXES, or =2 if posn defined by the coords in RX(),RY() (eg as defined
c by cursors); (=-1 axis label to be drawn by DRAWAXIS not used now)
c IDRAW(9) same but for Y axis
c
	if(idev.ge.5) then
c	   call LOADVTB('2'//'W'//char(lth(21)))        !axes
	else
	   t4=lth(21)
	   call LINWID(0.0)
	endif
	if(calbarX) goto 52

c====================================
c	draw axis and xlabel :

	if(idraw(8).eq.0) then
	   if(.not.mono) call lincols(icol(21))
	   call DXAXIS(xmin,xmax,xtic,ycros1,logx,nx1,tlenx,itx)
	else if(idraw(8).eq.-2) then
	   idraw(8)=1		!reset IDRAW
	   xmid=0.5*(xmin + xmax)
	   x=xmid
	   call getsize(cf8,w,h)
	   iangle(8)=0			!set during initialisation
	   ijus(8)=center
	   w=w-ang*(w-h)/90.
	   h=h+ang*(w-h)/90.
	   if(.not.equal(xcross,xmin)) then
		if(xcross.gt.xmin.and.xcross.le.xmid) then
		   x=xcross+h
		   ijus(8)=left
		endif
		if(xcross.gt.xmin.and.xcross.gt.xmid) then
		   x=xcross-h
		   ijus(8)=right
		endif
	   endif
	   y=ycros1	!Y posn of X axis (see SETXAXIS)
	   y=y - (h+h/3)		!move down to start axis label
	   rx(8)=x		!keep start pos for x axis label
	   ry(8)=y
	   call TEXTBOX(x,y,xtitle(1:nlx),iangle(8),cf8,
     &        ijus(8),rxbox(1,8),rybox(1,8),0)
	   if(.not.mono) call lincols(icol(21))
	   call DXAXIS(xmin,xmax,xtic,ycros1,logx,nx1,tlenx,itx)
	   if(idev.ge.5) then
c		   call LOADVTB('2'//'W'//char(lth(23)))        !axis labels
	   else
	   	   t4=lth(23)
	   	   call LINWID(0.0)
	   endif
	   if(.not.mono) call lincols(icol(23))
	   if(nlx.gt.1.and.nolabel.lt.1) call JUSTIFYSTRING(x,y,
     &   xtitle(1:nlx),
     &   iangle(8),cf8,ijus(8))
	else if(idraw(8).eq.1) then
	   if(.not.mono) call lincols(icol(21))
	   call DXAXIS(xmin,xmax,xtic,ycros1,logx,nx1,tlenx,itx)
	   x=rx(8)
	   y=ry(8)
	   if(idev.ge.5) then
c	      call LOADVTB('2'//'W'//char(lth(23)))        !axis labels
	   else
	      t4=lth(23)
	      call LINWID(0.0)
	   endif
	   if(.not.mono) call lincols(icol(23))
	   if(nlx.gt.1.and.nolabel.lt.1) call JUSTIFYSTRING(rx(8),ry(8),
     &   xtitle(1:nlx),
     &   iangle(8),cf8,ijus(8))
	endif
c ********************* Y AXIS ***************************
52	continue
	if(calbarY) goto 53
	call CHAFNT(ifont9)    ! later
	call SETSIZE(cf9)	!this defines ISIZE
	ytic1=ytic
	if(sqrty) ytic1=sqrt(ytic1)
	nly=NBLANK1(ytitle)
	if(nly.lt.1) nly=1
	if(idev.ge.5) then
c	   call LOADVTB('2'//'W'//char(lth(21)))        !axes
	else
	   t4=lth(21)
	   call LINWID(0.0)
	endif

c==============================================
c	draw yaxis and ylabel
c==============================================
	if(idraw(9).eq.0) then
	   if(.not.mono) call lincols(icol(21))
	   call DYAXIS(ymin,ymax,ytic,xcross,logy,ny1,tleny,ity,sqrty)
	else if(idraw(9).eq.-2) then
	   idraw(9)=1		!reset IDRAW
	   ymid=0.5*(ymin1+ ymax1)
	   y=(ymid)
	   call getsize(cf9,w,h)
	   iangle(9)=90		!set during initialisation
	   ijus(9)=center
	   w=w-ang*(w-h)/90.
	   h=h+ang*(w-h)/90.
	   if(.not.equal(ycros1,ymin1)) then
		if(ycros1.gt.ymin1.and.ycros1.le.ymid) then
		   y=(ycros1)+3*w
		   ijus(9)=left
		endif
		if(ycross.gt.ymin1.and.ycross.gt.ymid) then
		   y=(ycros1)-3*w
		   ijus(9)=right
		endif
	   endif
	   x=(xcross)	!X posn of Y axis (see SETYAXIS)
	   i=ichar(frmty(1:1))-48
	   x=x - (w*idmax+w/2)		!move left to start axis label
c	   if(x.lt.0.) x=0.   !try to prevent excessive shift 07/17/94 09:44pm
	   rx(9)=x
	   ry(9)=y
	   call TEXTBOX(rx(9),ry(9),ytitle(1:nly),iangle(9),cf9,
     &        ijus(9),rxbox(1,9),rybox(1,9),0)
	   if(.not.mono) call lincols(icol(21))
	   call DYAXIS(ymin,ymax,ytic,xcross,logy,ny1,tleny,ity,sqrty)
	   if(idev.ge.5) then
c		   call LOADVTB('2'//'W'//char(lth(23)))        !axis labels
	   else
	   	   t4=lth(23)
	   	   call LINWID(0.0)
	   endif
	   if(.not.mono) call lincols(icol(23))
c====================================================================
	   if(nly.gt.1.and.nolabel.lt.1) call JUSTIFYSTRING(x,y,
     &   ytitle(1:nly),
     &   iangle(9),cf9,ijus(9))
	   gio=4
	else if(idraw(9).eq.1) then
	   if(.not.mono) call lincols(icol(21))
	   call DYAXIS(ymin,ymax,ytic,xcross,logy,ny1,tleny,ity,sqrty)
	   x=(rx(9))
	   y=(ry(9))
	   if(idev.ge.5) then
c	      call LOADVTB('2'//'W'//char(lth(23)))        !axis labels
	   else
	      t4=lth(23)
	      call LINWID(0.0)

	   endif
	   if(.not.mono) call lincols(icol(23))
	   if(nly.gt.1.and.nolabel.lt.1) call JUSTIFYSTRING(rx(9),ry(9),
     &   ytitle(1:nly),
     &   iangle(9),cf9,ijus(9))
	endif
53	continue
c **********************************************************************
c DRAW IN AXIS NUMBERS
	if(nolabel.eq.1) goto 999
	call SETSIZE(cf7)	!size for numbers
	cs=cf7
 	call CHAFNT(ifont7)
	if(idev.ge.5) then
c	   call LOADVTB('2'//'W'//char(lth(24)))        !numbers
	else
	   t4=lth(24)
	   call LINWID(0.0)
	endif
	if(.not.mono) call lincols(icol(24))
	if(idraw(31).ne.-2.or.idraw(56).ne.-2) then
c 	   draw numbers at predefined positions
c	   call getsize(cf7,w,h)
	endif
c
c  Numbers for X axis
c If numbers are draw at positions already defined then cannot necessarily
c use x>xmax as criterion for knowing when all numbers drawn, so record
c the total number of numbers drawn on each axis as NUMBX,NUMBY resp.
c
	if(calbarX) goto 54
c Following draws numbers at initial default pos.
c If logx make sure numbers are on decades
c If not log(x) then make 1st major tick at an integer multiple of XTIC
	if(idraw(31).eq.-2) then
	   inx=1		!counts numbers for textbox etc
	   if(logx) then
		x=AFIXR(xmin)
		if(x.lt.xmin) x=x+1.0
	   else
		z=amod(xmin,xtic1)
		if(xmin.lt.0.) X=xmin-z
		if(xmin.gt.0.) X=xmin-z+xtic1
		if(xmin.eq.0.) X=xmin
	   endif
14	   continue		!return here for next number
	   j=inx+30		!index=31-55 for x axis
	   if(j.gt.55) goto 42
	   idraw(j)=1	!so drawn below next time, now CNUM defined
	   ix=inx
	   z=x*sx		!number actually drawn is scaled
	   x1=x  		!number position in world coord
	   y1=ycros1		!number position in world coord
	   if(logx.and.inumx.eq.1) then
		call MAKEXP(z,x1,y1,cs,.true.,nd,ij,cnumx(ix),
     &	cexpx(ix),itx)
	   else
	      if(logx) z=10.**x
		call MAKNUM(z,x1,y1,cs,.true.,nd,ij,cnumx(ix),itx)
	   endif
c  	   NB must redetermine box positions after rescale
	   rx(j)=x1
	   ry(j)=y1
	   ijus(j)=ij	!also predefine???
  	   call TEXTBOX(x,y,cnumx(i),iangle(j),cf7,
     &   ijus(j),rxbox(1,j),rybox(1,j),0)
	   if(.not.mono) call lincols(icol(24))	!reset colour after TEXTBOX call
	   x=x+xtic1
	   inx=inx+1
	   if(x.le.xmax) goto 14	!next number
	   NUMBX=inx-1
c 		x axis at predefined position
	else if(idraw(31).ne.-2) then
c 		OR draw x axis numbers at predefined positions
	   do 60 inx=1,numbx
	      j=inx+30		!index=31-55 for x axis
	      if(idraw(j).eq.0) goto 60		!skip this one
	      x=rx(j)
	      y=ry(j)
	      ij=ijus(j)
	      ang=iangle(j)
	      nl=NBLANK1(cnumx(inx))
	      str=cnumx(inx)(1:nl)
	      if(logx.and.inumx.eq.1) then	!draw superscript
		   call SETSIZE(cf7)	!this defines ISIZE
		   call CHAEXI(0.8,0.8,0.6,0.3)
	         nl1=NBLANK1(cexpx)
		   nm=nblank1(cnumx)
	         str=cnumx(inx)(1:nl)//char(42)//char(69)
     &         //cexpx(i)(1:nl1)
	      endif
	      call JUSTIFYSTRING(rx(j),ry(j),str(1:nl),iangle(j),cf7,ij)
60	   continue
	endif
c
c============================================/////////

c Numbers for Y axis
c============================================/////////

54	continue
	if(calbarY) goto 999

42	continue
	if(idraw(56).eq.-2) then
	   half=0
	   iny=1
	   if(logy) then
		y=AFIXR(ymin)
		if(y.lt.ymin) y=y+1.0
	   else
		z=amod(ymin1,ytic1)
		if(ymin1.lt.0.) Y=ymin1-z
		if(ymin1.gt.0.) Y=ymin1-z+ytic1
		if(ymin1.eq.0.) Y=ymin1
		if(abs((ymax1-ymin1)/ytic1).gt.25) half=1
	   endif

15	   continue		!return here for next number
	   j=iny+55		!index=56-80 for y axis
	   i=iny
	   if(j.gt.80.and.half.eq.0) goto 43
	   idraw(j)=1	!so drawn below next time, now CNUM defined
	   z=y*sy			!number actually drawn is scaled
	   x1=xcross     		!number position in world coord
	   y1=y			!number position in world coord
	   if(logy.and.inumy.eq.1) then
		call MAKEXP(z,x1,y1,cs,.false.,nd,ij,cnumy(i),
     &	cexpy(i),ity)
	   else
		if(sqrty) z=y**2
	      if(logy) z=10.**y
		if(half.eq.1.and.mod(iny,2).eq.0) goto 9876
		call MAKNUM(z,x1,y1,cs,.false.,nd,ij,cnumy(i),ity)
	   endif
9876	   rx(j)=x1
	   ry(j)=y1
	   x=x1
	   y=y1
	   ijus(j)=ij	!predefine???
	   call TEXTBOX(x,y,cnumy(i),iangle(j),cf7,
     &    ijus(j),rxbox(1,j),rybox(1,j),0)
	   if(.not.mono) call lincols(icol(24))	!reset colour after TEXTBOX call
	   y=y+ytic1
	   iny=iny+1
	   if(y.le.ymax1) goto 15	!next number

	   NUMBY=iny-1
43	   continue
	else if(idraw(56).ne.-2) then
c y axis numbers at predefined positions
	   do 61 i=1,numby
	   j=i+55		!index=56-80 for y axis
	   if(idraw(j).eq.0) goto 61		!skip this one
	   x=(rx(j))
	   y=(ry(j))
	   ij=ijus(j)
	   ang=iangle(j)
	   nl=NBLANK1(cnumy(i))
	   str=cnumy(i)(1:nl)
	   if(logy.and.inumy.eq.1) then	!draw superscript
		call SETSIZE(cf7)	!this defines ISIZE
		call CHAEXI(0.8,0.8,0.6,0.3)
	      nl1=NBLANK1(cexpy)
		nm=nblank1(cnumy)
	      str=cnumy(i)(1:nl)//char(42)//char(69)
     &      //cexpy(i)(1:nl1)
	   endif
	   call JUSTIFYSTRING(x,y,str(1:nl),ang,cf7,ij)
61	   continue
	endif

999	continue
	call SETSIZE(size0)	!reset
	RETURN				! end of LAXES
	END

c=========================================================================
	subroutine DXAXIS(xmin,xmax,xtic,ycros1,logx,nx1,tlen,itx)
	logical logx,equal
	logical debug,caplock
	allocatable:: str
	character*6
c

c	call axipos()
	ntic=0
	if(logx) then 
         ival=0
	   iscale=4
	endif
	call axisca(iscale,ntic,xmin,xmax,1)
	call axidra(-1,ival,1)
	if (logx) call axistr(str,nstr,1,1)

	end
c=========================================================================
	subroutine DYAXIS(ymin,ymax,ytic,xcross,logy,ny1,tlen,ity,sqrty)
	logical logy,equal,sqrty
	logical debug,caplock
c

c	call axipos()
	ntic=0
	if(logy) then 
         ival=0
	   iscale=4
	endif
	call axisca(iscale,ntic,xmin,xmax,2)
	call axidra( 1,ival,2)
	if (logy) call axistr(str,nstr,-1,2)

	end
c=========================================================================
c Define function 'equal'
	EQUAL(xa,ya)=abs(xa-ya).lt.0.00001
	debug()=caplock()
c
c Define tic lengths in device coord (itlen=major tic length in
c device coord)
	tmaj=tlen
	tmin=tlen/2
	xticm=xtic/float(nx1)   !distance between minor tics
c draw the axis
	call MOVTO2(xmin,ycros1)
	call broken(0)
	call LINTO2(xmax,ycros1)
c draw tics    - first major tic is at x1 for non-log scale (=xmin  at
c present but might want to make this adjustable later), but for
c log scale first major tic is at first integer decade
	k=0
	if(logx) then
	   x1=AFIXR(xmin)
	   x=x1	!in case goto 42 next
	   if(EQUAL(x1,xmin)) goto 42
	   x=x1-1.0	!nearest integer BELOW xmin to start minor tics
	else
	   x1=xmin	 !make x1= position of 1st major tic
	   z=amod(xmin,xtic)
	   if(xmin.lt.0.) X1=xmin-z
	   if(xmin.gt.0.) X1=xmin-z+xtic
	   if(xmin.eq.0.) X1=xmin
	   x=x1	!in case goto 42 next
	   if(EQUAL(x1,xmin)) goto 42
	   x=x1-xtic	!!to start minor tics preceding 1st major
	endif
c Now draw minor tics that precede 1st major tic
	roff=(itx-1)*tmin/2	!=0 for itx=1; =-it/2 for itx=0; =-it for itx=-1
	if(nx1.le.1) goto 42
	do 43 i=2,nx1
	   if(logx) then
	   	x2=x+alog10(float(i))
	   else
	      x2=x+float(i-1)*xticm
	   endif
	   if(x2.gt.xmin.and.x2.le.xmax) then
	      call MOVTO2(x2,ycros1)
	      call LINTO2(x2,ycros1+roff+tmin)	!draw tic
	  endif
43	continue
c
c set x for 1st major tic
	x=x1
c draw 1st major tic at x=X1
42	continue
	call MOVTO2(x,ycros1)
c=================================================
	roff=(itx-1)*tmaj/2	!=0 for itx=1; =-it/2 for itx=0; =-it for itx=-1
c	call MOVTO2(x,y+roff)       !start pos to draw tic
	call LINTO2(x,ycros1+roff+tmaj)	!draw tic
c	if(debug()) print 200,1,x,ycros1,ix,iy,ioff
c200	format(' pos#',i2,' x,ycros1,ix,iy,ioff=',2g13.6,3i8)
c first major tic drawn; now draw ntx-1 minor tics (or 8 log-spaced tics
c if log axis)
	roff=(itx-1)*tmin/2	!=0 for itx=1; =-it/2 for itx=0; =-it for itx=-1
	if(nx1.le.1) goto 471
	do 47 i=2,nx1
	   if(logx) then
	      x2=x+alog10(float(i))
	   else
	      x2=x+float(i-1)*xticm
	   endif
	   if(x2.gt.xmax) goto 48	!x axis finished
	   call MOVTO2(x2,ycros1)
	   call LINTO2(x2,ycros1+roff+tmin)	!draw tic
47	continue
471	continue
	k=k+1
c Move up to posn for next major tic; recalc postition from the posn
c of the 1st major tic=x1 each time to reduce rounding errors
	if(logx) then
	   x=x1+float(k)
	else
	   x=x1+float(k)*xtic
	endif
	if(x.le.xmax) goto 42	!draw next cycle
c
48	continue
	RETURN
	end

c=========================================================================

	subroutine DYAXIS(ymin,ymax,ytic,xcross,logy,ny1,tlen,ity,sqrty)
	logical logy,equal,sqrty
	logical debug,caplock
c
c Note that for sqrt(Y) scale the calling prog (VHIST) has done
c call SCALE(xmin,xmax,sqrt(ymin),sqrt(ymax))
c Define function 'equal'
	EQUAL(xa,ya)=abs(xa-ya).lt.0.00001
	debug()=caplock()
c
c Define tic lengths in device coord (itlen=major tic length in
c device coord)
	tmaj=tlen
	tmin=tlen/2
	yticm=ytic/float(ny1)   !distance between minor tics
	ymin1=ymin
	ymax1=ymax
	ytic1=ytic
	if(sqrty) then
	   ymax1=sqrt(ymax)	!for comparison with pos
	   ymin1=sqrt(ymin)	!display thinks this is ymin,ymax
	   ytic1=sqrt(ytic)
	endif
c draw the axis
	call MOVTO2(xcross,ymin1)
	call LINTO2(xcross,ymax1)
c draw tics    - first major tic is at x1 for non-log scale (=xmin  at
c present but might want to make this adjustable later), but for
c log scale first major tic is at first integer decade
c For non-log scale 1st major tic is at integer multiple of Ytic
	k=0
	if(logy) then
	   y1=AFIXR(ymin1)
	   y=y1	!in case goto 42 next
	   if(EQUAL(y1,ymin1)) goto 42
	   y=y1-1.0	!nearest integer BELOW ymin1 to start minor tics
	else
	   y1=ymin1	 !make y1= position of 1st major tic
	   z=amod(ymin1,ytic1)
	   if(ymin1.lt.0.) Y1=ymin1-z
	   if(ymin1.gt.0.) Y1=ymin1-z+ytic1
	   if(ymin1.eq.0.) Y1=ymin1
	   y=y1	!in case goto 42 next
	   if(EQUAL(y1,ymin1)) goto 42
	   y=y1-ytic1	!!to start minor tics preceding 1st major
	endif
c Now draw minor tics that precede 1st major tic
	roff=(ity-1)*tmin/2	!=0 for ity=1; =-it/2 for ity=0; =-it for ity=-1
	if(ny1.le.1) goto 42
	do 43 i=2,ny1
	if(logy) then
	   y2=y+alog10(float(i))
	else
	   y2=y+float(i-1)*yticm
	endif
	if(y2.gt.ymin1.and.y2.le.ymax1) then
	   call MOVTO2(xcross,y2)
	   call LINTO2(xcross+roff+tmin,y2)	!draw tic
	endif
43	continue
c
c set y for 1st major tic
	y=y1
c
c draw 1st major tic at y=Y1
42	continue
	call MOVTO2(xcross,y)
	roff=(ity-1)*tmaj/2	!=0 for ity=1; =-it/2 for ity=0; =-it for ity=-1
	call LINTO2(xcross+roff+tmaj,y)	!draw tic
c	if(debug().and.sqrty) print 200,1,k,ytic1,y1,y*y,y
c first major tic drawn; now draw nty-1 minor tics (or 8 log-spaced tics
c if log axis)
c If major tics are ytic1=1. or 2. or 3. units apart on the sqrt scale then
c major tics numbered 0,1,4,9,.. for ytic1=1, ytic=1.;
c major tics numbered 0,4,16,36,64,100 for ytic1=2., ytic=4.;
c major tics numbered 0,9,36,81,144 for ytic1=3, ytic=9.;
	roff=(ity-1)*tmin/2	!=0 for ity=1; =-it/2 for ity=0; =-it for ity=-1
c draw minor tics (if ny1>1)
	if(ny1.eq.1) goto 471
	if(sqrty) then
	   dy=(y+ytic1)**2 - y**2 !real distance from this tic to next eg 16-4=12
	   dy=dy/float(ny1)	!divide into equal (real unit) increments
	endif
	do 47 i=2,ny1
	if(logy) then
	   y2=y+alog10(float(i))
	else if(sqrty) then
	   y2=y*y + float(i-1)*dy
	   y2=sqrt(y2)
	else
	   y2=y+float(i-1)*yticm
	endif
	if(y2.gt.ymax1) goto 48	!axis finished
	call MOVTO2(xcross,y2)
	call LINTO2(xcross+roff+tmin,y2)	!draw tic
47	continue
471	continue
	k=k+1
c Move up to posn for next major tic; recalc position from the posn
c of the 1st major tic=y1 each time to reduce rounding errors
	if(logy) then
	   y=y1+float(k)
	else
c	if(debug().and.sqrty) print 200,3,k,ytic1,y1,y*y,y
	   y=y1+float(k)*ytic1
c	if(debug().and.sqrty) print 200,4,k,ytic1,y1,y*y,y
	endif
	if(y.le.ymax1) goto 42	!draw next cycle
c
48	continue
	RETURN
	end

