	program SCAN
c=====TO DO
c	Can the displays for fstdisp, lastdisp also be done regular
c	aivpix in the loop (problem is when we want to truncate on left!)?

c======fstdisp,lstdisp should never be used in novlap big enough -check!
c======also should only be used backward/not backward

c====see errors in SCAN1.PRT

c (4) -make defaults-menu as subroutine? (Spindrift Window?)
c (5) Ioana -can save video memory rather than read pixels for help menu?
c (7) Think about new data disc organization?
c (8)
c
c Single channel time course fitting (and threshold crossing too?)
c 03/01/93 03:37pm Lahey V5.x version
c
c Now uses AIVPIX0 so no longer need to offset jfst
c; AIVPIX0.ASM This is version of AIVPIX1 without the jfst argument, which
c;MUST be called with idata(jfst), rather than just 'idata'.  This is more
c;convenient for arrays that start with negative indices, because, for example,
c;if idata is declared as idata(-100,1100) then 'idata' in the call implicitly
c;refers to idata(-100), so must call AIVPIX1 with jfst=101 to start display
c;at idata(1).  It is simpler to call AIVPIX0(idata(1),...) than to
c;call AIVPIX1(idata,jfst=101,...)
c;call aivpix0(ixdisp,iydisp(jfst),ndisp,inc,ipen,first,bk)
c
c Notation:
c READSEC now outputs the following each time a section is read
c  Absolute indices:
c   in1=1 + (isec-1)*nbuf = abs index of 1st point in section (exc overlap)
c   in2=isec*nbuf = abs index of last point in section (exc overlap)
c   jn1=in1 - novlap = abs index of 1st point actually read in (inc overlap)
c   jn2=in2 + novlap = abs index of last point actually read in (inc overlap)
c   indfst= abs index of first point on display
c   indlast= abs index of last point on display
c  Indices in idata (within section)
c   is1=1 = index of 1st point in section (exc overlap)
c   is2=nbuf (or nrlast for isec=nsec)= index of last point (exc overlap)
c   js1=is1 - novlap = index of 1st point in idata (inc overlap)
c   js2=is2 + novlap = index of last point in idata (inc overlap)
c   isdfst= index in idata() of first point on display
c   isdlast= index in idata() of last point on display
c
c SCALING:
C 1401 units are -32768 t0 +32752 in 16 unit increments; dividing
c by 16 gives range as -2048 to +2047 (as for CED502) =-5.0V to +4.9976V
c So 32768/5=6553.6 units/volt, or, for CED502 data 2048/5=409.6
c Now (05/21/93 09:22am) scale in 2 stages to avoid loss of original
c precision by scaling in READSEC to display range integers  (0-480, or
c actually yd1 to yd2= 127 to 457).  Calculate scalfac to scale orig
c values to integers that cover the range up to at least 4096, so as to
c keep original precision, so numbers are larger than required for display
c by a FACTOR, nscale, that is in integer power of 2.  Also do not add
c offset in READSEC.  Then display routines have only to divide by
c NSCALE and add offset, which should be very fast operations.
c RAW UNITS=ADC units read from disc
c INTERMEDIATE UNITS=units output by readsec (ADC*sfac1) (no offset)
C		e.g. ybase,base,itriglev
c PIXEL UNITS=(values in intermed units)/2**nscale+iyoff (with offset)
c		e.g. ybasevga,basevga,itrigvga
c
cIDEAS FOR SCAN
c	(2) Use of inset displays? eg while fitting enlarged trace, show
c unenlarged version in box? How to mark bit to be enlarged for fitting?
c -like marking 'good bit'?
c	(3) Colours e.g. -green=fitted; red=unusable; ?=not yet fitted
c	(4) Might seem faster if fairly small sections used as each section
c	would not take long to read from disc?
c	(5) If array with data in it was deallocated and reallocated at each
c	read, index in it could (?) be kept same as index in all data
c	e.g. for section 1 allocate idata(1:100), for section 2 idata(101:200) etc
c	(6) will need some other fix for nchan=2 because cannot use EQUIV
c	for allocatable arrays: best sort out into 2 sep arrays before
c	starting if nchan=2; do this in consam?
c
c Scaling from SCAN5: iboff=offset initially but this is adjusted (a) in ROLDAT
c when initial baseline set (b) in AFIT1 when baseline adjusted.
c from DATGET:
c	DO 204 I=1,NBUF
c204	IBUF(I)=IFIX(DGAIN*FLOAT(IBUF(I)))+IBOFF
c from ROLDAT:
c	IOFF1=IBASE-IFIXR(YBASE)  !OFFSET;SETS DISPLAY BASELINE AT ibdisp
cc IOFF1=change (vs that used after read) in offset from last time
c	DO 2044 I=1,NBUF	!OFFSET CURRENT SECTION BEFORE SCROLL ETC
c2044	IBUF(I)=IBUF(I)+IOFF1
c	iboff=iboff+ioff1		!new offset for next read
cc need to adjust IOFF etc each time baseline set in AFIT?
c
c
c  Indexing used in SAMPED
c	Can now keep track of individual points by their index=IN in the total
c data set on disc (in=1,2,...,ndat), though this has to be read in in sections.
c The point with index=IS, idata(is), in section number ISEC has index
c	in= is + (isec-1)*nbuf
c Conversely point with index=IN in the whole data set occurs in section
c	isec= 1 + (in-1)/nbuf
c and has index, within this section,
c	is= in - (isec-1)*nbuf
c Display starts at point with index is=isfst in current section so point
c with index=ID on display (1st displayed point=1) has index in section of
c	is= id + isfst - 1
c Similarly incurs,iscurs,idcurs= index in whole array, section, display
c respectively of the data point nearest to the cursor.
c
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c
	integer*2 ktype,videotyp
c	integer*2 lrow,int2
c	integer iact(0:15)
	logical discprt
	logical pon,slock,debug,caplock
	character*11 cdate,ctime,cnum
	character filtfile*20		!for GETFILT
	character*32 filnam	!make 32 bytes (so common optimally aligned)
	character*74 mtitle
	character ndev*2
c For INSCAN:
	real*4 tint(1024),timsav(1024)
	real*4 timamax(19)
	integer*2 iampl(1024)
c==	integer ksect(1024),ishut(1024)
	real*4 filt1(1024)
c	real*4 filt(100)
	character*8 adctime
	logical invert,opendown,newpar,warned,sdone,baseOK,restart
	logical disp,rescale,auto,end,backward,stopped,triggered
	logical fitmode,usepots,fstdisp,lastdisp,mono,ampdone
	logical open,shut
c For Amplicon ADCs
	integer*2 ichan(0:15),iadc(0:15),iadc0(0:15),nadc
c For IV graphics
c			Assembly subroutines used:
C     		=========================
c           AIVPIX1(IX,IDATA,NDISP,JFST,INC,IPEN,iFIRST,IBACKGROUND)
C			IX,IDATA: INTEGER*2 ARRAY
C			ALL THE OTHERS : INTEGER*4
C		DRAW(IX,IDATA,NDISP,JFST,IPEN,IPEN,IBK)
c		FILLWIN(IXIN,IXUP,IYIN,IYUP,ICOLOR)
c           SET_STRING(STRING,ICOLOR)
C		SET_CURSOR(INTX,INTY)
C		DET_STRING(STRING,ICOLOR,CHAR)
C		PUT_STRING(STRING,ICOLOR,ICOLOR,CHAR)
c           MODE(NUMBER)
c		WPIXEL(INTX,INTY,ICOLOR)
c		RPIXEL(INTX,INTY,ICOLOR)
c		CLEARS(INTY1,INTY2,ICOLOR)
c           HLINE(INTX1,INTX2,INTY,ICOLOR)
C		VLINE(INTX,INTY1,INTY2,ICOLOR)
c
c New declarations, using default integer*4 whenever possible
c

	allocatable	:: idata
c==	allocatable	:: transfer
c==	allocatable :: text1,text2
c==	allocatable :: txtwin1,txtwin2,pchar1,pchar2
c============================================================
	integer*2 idata(:)
c==	integer*2 txtwin1(:,:),txtwin2(:,:),transfer(:)
	integer*2 ix(2048)
	integer*2 idata1(2048)	!for data to be fitted (and as temp file)
c============================================================
c	integer*4 iabar1(0:641),iabar2(0:641),iabar3(0:641)
	integer*4 iabar3(0:641)
	character*1	getch,ans,UC
c	character*1	ch,ans,UC,LC,achar
c==	character*1  	pchar1(:),pchar2(:)
c==	character*10	pch(20)
c==	character*18 	text1(:)
c==	character*11 	text2(:)
	logical kbhit
c end of IV declarations
C For DCMENU
	integer Lb(18)
	character*78 text(18)
c
	common/dp/discprt
	common/dpp/filnam,machine,ndisc,jcol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	common/filtblk/filt1,nfilt1,dtf1,npfilt,finter
	common/prog/iprogx0,iprogx1,iprogx2,ipmax,ibdpp,iyframe,icprog,ibk
	common/DMENU/csizb,ifonb,csize,ifont2,nboxlast,nblast
	common/hgv/ivxlo,ivxhi,ivylo,ivyhi,ixlo,ixhi,iylo,iyhi
C
c define function
	pon()=slock()
	debug()=caplock()
c
	filnam='SCAN.PRT'
	OPEN(unit=7,file='PRN',iostat=nerr)
	call WINPRINT	!print file control
	print 1
	if(pon()) write(7,1)
	if(discprt) write(8,1)
1	FORMAT(' SCAN: Time course fitting of single channels',/)
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 2,cdate,ctime(1:8),mtitle
	if(pon()) write(7,2) cdate,ctime(1:8),mtitle
	if(discprt) write(8,2) cdate,ctime(1:8),mtitle
2	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 3
3	format(
     & ' SCROLL LOCK on for printing'/
     & ' CAPS LOCK on for debugging')
c
101	format(a1)
c	lastrow=2			!NB args of SCROLL=integer
c	lrow=int2(lastrow)	!but args of LOCATE are int*2
	call SETATTR(112)		!'inverse' attrib seems nec while in graphics
	nsetup=0	!so asks for step response function in INSCAN
	fitmode=.false.
	sdone=.false.	!not yet calculated baseline and RMS
	baseOK=.false.	!not yet accepted bit of baseline for RMS
	shut=.true.		!before first opening located
	open=.not.shut
	nshutfit=30		!default # of shut points to be fitted after last trans
	mono=jcol.eq.0	!monochrome screen
	idiskq=-1
c
	kt=0			!number of transitions located so far
c
c colours:
	ipen=14           !COLOR FOR DATA
	ibk1=1           	!BACKGROUND COLOR for data area
	ibk=3           	!BACKGROUND COLOR for outside data area
	icw=11		!colour to write in dialog box (light blue)
	icw1=12		!red to write in dialog box for warnings
	icbl=10		!baseline line colour
	ictrig=12         !color of trigger level
	if(mono) then
	   ipen=15
	   ibk=0	!for monochrome
	   ibk1=1	!for monochrome
	   icw=15
	   icw1=15
	   ictrig=15
	endif
	ibk2=8            !background color in expand mode (and FIT mode?)
	ipen=14		!data colour
	if(mono) then
	   ibk2=0
	   ipen=15
	endif
c
c defs for DCMENU
c	nbox=10	!number of boxes for DCMENU
	icol1=14	!yellow text/border for boxes in DCMENU
	icol2=8	!grey background for boxes
	icol3=12	!red text/border
	icol4=15	!white fill
	if(mono) then
	   icol3=15		!white text/border
	   icol4=0		!black fill
	endif
	ifonb=0
	csizb=1.7
	ifont=4
	csize=2.
	ivxlo=0		! for VIEWPORT
	ivxhi=10000
	ivylo=0
	ivyhi=7000
	ixlo=0		! for graphboundary
	ixhi=10000
	iylo=0
	iyhi=7000
c
	call INSCAN(iampl,tint,ntmax,nbuf,novlap,srate,calfac,
     & finter,tsect,ioff,ndat,nsec,nrlast,namp,totamp,iampsv,ibassv,
     & ktrans,nwrit,nwsav,nfits,newpar,igback,restart,opendown,invert,
     & usepots,disp,smult,ntrig,navtest,dgain,iboff,rms,
     & expfac,bdisp,isub,xtrig,ndev,cdate,adctime,
     & nsetup,filtfile,filt1,nfilt1,dtf1,timamax,
     & tf01,tf1,tf50,tf99,tf999,npfilt,
     & warned,sdone,ifop,rescale,auto,iscan,idest)
	if(idest.eq.99) goto 999
c Fix scaling here (or in INSCAN)
c======not yet fixed for INVERT or OPENDOWN!
c Scaling: 1401 units are -32768 t0 +32752 in 16 unit increments; dividing
c by 16 gives range as -2048 to +2047 (as for CED502) =-5.0V to +4.9976V
c So 32768/5=6553.6 ADC units/volt
c Use Y coords (0-479) between 130, 450 for now, to span 65536 ADC units
c Calc initial value of iboff here (not inscan)?
	yd1=127.	!lowest point on screen (Y pixels) for display of channels
	yd2=457.	!highest point on screen (Y pixels) for display of channels
	iyd1=ifixr(yd1)
	iyd2=ifixr(yd2)
	sfac1=(yd2-yd1)/65536.
	sfac1=sfac1*dgain		!true scal fac for ADC to pixel units
	nscale=1 + ifix(alog(4096./(yd2-yd1))/alog(2.))
	sfac2=sfac1*float(2**nscale)	!Converts ADC units to intermed units
	sfac3=1.0/float(2**nscale) 	!converts intermed to pixel units
	calfac1=calfac/sfac1		!converts pixel display units to pA
	calfac2=calfac/sfac2		!converts intermed units to pA
c==	iyoff=ifixr(yd1 + bdisp*(yd2-yd1))	!baseline position (in Y pixels)
	iyoff=ifixr(yd1 + 0.5*(yd2-yd1))	!zero in centre until baseline done
c (NB iyoff is in pixel units with offset)
c
c=============
c IV menus
c=============
c==============================================================
c	THIS PART IS DEALING ONLY WITH THE MENUS
C==============================================================
c	Enter initial coordinates and parameters for menues
c	ixin1=16
c	iyin1=16
c	n1=16
c	ifwin1=3
c	ictxt1=8
c definitions for EXPMODE (prev called SCREEN2)
c	ixin2=120
c	iyin2=16
c	ixup2=624
c	iyup2=96
c	n2=15
c	ifwin2=8
c	ictxt2=8
c	Calculates the colors for background and scroll bar:
c	call set_colors(ifwin1,ibkwin1,isbwin1)
c	call set_colors(ifwin2,ibkwin2,isbwin2)
c	ixstr=68          !X COORDINATE FOR WRITING THE SECTION
c	iystr=23          !Y COORDINATE FOR WRITING THE SECTION
c	icstr=11

	ixmes=2           !X COORDINATE FOR WRITING THE MESAGES
	iymes=21          !Y COORDINATE FOR WRITING THE MESAGES
	ixfill=int(ixmes*8)
	iyfill=int(480-iymes*16)
	ixdat=0           !X COORDINATE FOR WRITING THE MESAGES
	iydat=22          !Y COORDINATE FOR WRITING THE MESAGES
c
c
c Now allocate data array
	ALLOCATE(idata(1-novlap:nbuf+novlap))

c==================================================================
c	OTHER DEFINITIONS  AND  CONSTANTS
C==================================================================

c Enter graphics mode
c	call MODE(18)		!clears screen
	idev=0			!screen
	call INIPLT(idev,.false.,1.0)		!Hgraph mode 18
c	call VIEWPORT(ivxlo,ivxhi,ivylo,ivyhi)
c	call GRAPHBOUNDARY(ixlo,ixhi,iylo,iyhi)
C================================================================
C	ENTER THE DEFAULT SETTINGS
C================================================================
	ndisp=640    	!NDISPLAY
	ndisp1=ndisp	!normally, but ndisp1 smaller for truncated display
c	incabs=160     	!absolute value of INCREMENT for rolling display
	incabs=16		!slow roll until RMS and baseline done
	inc=incabs     !'working' value   for increment (may be pos or neg)
	indfst=1        	!****ABSOLUTE INDEX for 1st point to display
	indlast=indfst+ndisp-1
	indfst0=indfst      !save its value
	isec0=1 + (indfst0-1)/nbuf     	!section # for point #indfst
c SET BASELINE ,AMPLITUDE,THRESHOLD
c	iybar1=int(bdisp*480.)   !baseline
c	iybar1=iyoff
c	icbar1=10                !color of baseline
c	iybar2=128               !amplitude
c	icbar2=11                !color of amplitude line
	if(opendown) then
	   itrigvga=ifixr(yd1 + 0.25*(yd2-yd1))	!init value for trigger level
	else
	   itrigvga=ifixr(yd1 + 0.75*(yd2-yd1))	!init value for trigger level
	endif
c itrigvga is in pixel units for display
	itriglev=(itrigvga-iyoff)/sfac3	!in intermed units (same as idata)
c
c Initialise for ADCs
	do i=0,15
	    ichan(i)=0
	    iadc(i)=0
	enddo
	ichan(0)=0
	nadc=1	!read channel 0 only
c initialise fstdisp,lastdisp etc (used only for part-screen displays at
c section boundaries, when novlap is nit big enough to cope)
	fstdisp=.false.
	ixoff=1
	lastdisp=.false.
c
c Calcs for progress line
C	FOR DRAWING THE THREE CURSORS ON THE SCREEN AND THE FRAME INSIDE
C	WHICH WE MOVE THE PROGRESS LINE:
C		1.MARKER (BLUE CURSOR) : FIX, SHOW THE ABSOLUTE POSITION IN
C			THE FILE AT WHICH YOU START THE ANALYSE
C		2.PROGRESS LINE(WHITE CURSOR):MOVES AUTOMATICALLY
C			WITH THE FAST SCROLLING
C		3.RED CURSOR:MOVES MANUAL TO THE DESIRED POSITION IN THE FILE
C	THIS IS DONE HERE BECAUSE IF YOU WANT,EVENTUALLY TO START
C	AT A DESIRED POSITION TO NOW THE TOTAL NUMBER OF VALUES IN THE
C	RECORD
C==================================================================
	ibdx=ifix(640./float(nsec))     ! pixels per section for prog line
	ibdpp=nbuf/ibdx		! data points per pixel on progress line
	ibproc=ifixr(float(ibdx*nrlast)/float(nbuf))  !pixels for last section
	ixframe=ibdx*(nsec-1)+ibproc	!total pixels inc last section
c	ibabs=ndat/ixframe      	!data points/pixel on prog line (=ibdpp)
	iyframe=472    	      	!top edge of progress line
	icframe=0          		!prog line frame colour
	if(mono) icframe=15	!monochrome screen
	icprog=15         		!PROGRESS LINE COLOR
	ixcr=0  			! X FOR RED CURSOR
	iycr=iyframe-6          ! Y FOR RED CURSOR
	iccr=13                 ! RED CURSOR COLOR
	ifabs0=1 + float(indfst0)/ibdpp
	ixmark=ifabs0-1         ! X MARKER
	iymark=iyframe+8        ! Y MARKER
	icmark=11               ! COLOR MARKER
	iprogx0=1			!starting X-coord (pixel #) for prog line
	ipmax=1			!needed for delete in NEWPROGLINE
c end of progress line calcs
c
C========================================================
C	Having indfst, calc section #, and index (isdfst) within the section
C=====================================================

	call CLEARS(0,480,ibk)	!fill screen with light blue (for outside data area)
	call FILLWIN(0,640,iyd1,iyd2,ibk1)	!fill data area with colour ibk
	call HLINE(0,639,iyd1-1,14)		!line to mark bottom of data area
	call HLINE(0,639,iyd2+1,14)	!line to mark top of data area
	if(.not.sdone) then
c	   call OPENDIALOG(1,23,2,3,56,14,0)
c open a 3 row dialog box top=row 23; cols 2-58
c	subroutine DEFDIALOG(ibox,irhi,iclo,nrow,ncol,icb)
c Draw the box defined by DEFDIALOG
c	subroutine OPENDIALOG(ibox,icf)
	   call DEFDIALOG(1,23,2,3,60,0)
	   call OPENDIALOG(1,14)
	   call WDIALOG(1,
     &	'GET BIT OF EMPTY BASELINE ON SCREEN FOR SD',icw1)
	   call BELL(2)
	endif
c

c RETURN HERE AFTER 'GO TO RED CURSOR' TO (RE)START DISPLAY WITH ARBITRARY indfst
444	CONTINUE	!return here after 'go to red cursor'
c The point with index=IS, idata(is), in section number ISEC has index
c	in = is + (isec-1)*nbuf
c Conversely point with index=IN in the whole data set occurs in section
c	isec = 1 + (in-1)/nbuf
c and has index, within this section,
c	is = in - (isec-1)*nbuf

	isec=1 + (indfst-1)/nbuf     	!section # for point #indfst
	call CALCIND(isec,nbuf,nrlast,novlap,nsec,
     & in1,in2,jn1,jn2,is1,is2,js1,js2,nread,jn1next)  !get indices for section
	isdfst=indfst - (isec-1)*nbuf	!index WITHIN section of point #indfst
	isdlast=isdfst+ndisp-1
c Check that the specified starting point for the display, consam(indfst),
c is not so near to the end of section #isec (just calc) that there are
c too few points to show it (but if no overlap then must show whatever there is!)
C***********************************************************************
c If following condition is true here, display crosses section boundary,
c so goto next section and use its initial overlap part to do display:
c	if(indfst.gt.(in2-ndisp).and.indfst.lt.in2) then
c		isdfst=isdfst-nbuf
c		isdlast=isdfst+ndisp-1
c		isec=isec+1
c	endif         !CONDITION FOR THE LIMIT BETWEEN SECTIONS
c Alter this so overlap in current section is used if long enough. If not
c then use next section (a) if there is one and (b) if it has overlap
c long enough to show the specified first point (indfst).  Otherwise show
c truncated display, starting with the right point in current section but
c with fewer than ndisp points.
	ndisp1=ndisp		!unless truncated
	if(indlast.gt.jn2) then	!current section has not got all ndisp points
	   if(isec.lt.nsec.and.indfst.ge.jn1next) then	!use next section
		isec=isec+1
		call CALCIND(isec,nbuf,nrlast,novlap,nsec,
     & 	in1,in2,jn1,jn2,is1,is2,js1,js2,nread,jn1next)	!recalc indices
	   else 			!show fewer than ndisp points from current section
		ndisp1=jn2-indfst+1
	   endif
c===later:	put up red box to say display truncated?!
	endif
c jfst no longer needed with AIVPIX0 (see above); just call with idata(isdfst)
c where isdfst=index, within current section, of first point to be displayed
c
c jump here for NEXT SECTION
333   continue
c READ IDATA AND SCALE INSIDE IREADSEC
	call READSEC(isec,idata,nrlast,nsec,ioff,nbuf,nread,novlap,
     & sfac2,in1,in2,jn1,jn2,is1,is2,js1,js2)
c Parameters for progress line (move_prog_line in IOLIB) (NB only upper line
c moves backwards -call with flag_pline=0 to move both lines, and flag_pline=1
c to move upper line only):
c (iprogx1,iprogx2 were prev isecf,isec1)
c ipxs not used now
c	ipxs=(isec-1)*ibdx+1	!ibdx=pixels/section on prog line
c					!so ipxs=pixel # for 1st point in section #isec
	iprogx1=1 + indfst/ibdpp	!current X coord (# of pixels) for upper prog line
	iprogx2=iprogx1			!ditto for lower line (does not move back)
c
	backward=.false.
	end=.false.
c
c JUMP HERE WHEN NDISP changed
5	continue
c Jump to 31 to calc baseline and rms noise when baseline OK
c note- ybase, vary are in intermed units, without offset
31	if(baseOK.and.(.not.sdone)) then
	   triggered=.false.
	   call IVAR2(idata1,ndfit,ybase,vary)
	   rms=sqrt(vary)*calfac2	!in pA (calfac2 for intermed to pA)
	   scrit=smult*sqrt(vary)	!in intermed units
c baseline kept in display units (pixels=0-480), but keep as floating point
	   basevga=yd1 + bdisp*(yd2-yd1)	!BASELINE AFTER OFFSET BELOW (pixel units)
c ybase= abs position of baselin in 'intermed units' (no offset, so 0=0pA)
	   x=ybase*calfac2	!in pA (calfac2 for intermed units to pA)
	   call DCFORMAT(rms,8,3,cnum)
	   call WDIALOG(1,'RMS baseline noise (pA)'//cnum,icw)
	   call DCFORMAT(x,8,3,cnum)
	   call WDIALOG(1,'Baseline set to (pA)'//cnum,icw)
c         print 33,rms,x				!in pA
         if(pon()) write(7,33) rms,x		!in pA
         if(discprt) write(8,33) rms,x		!in pA
33	   format(' RMS baseline noise (pA)= ',g11.4,/,
     &	' Baseline set to (pA)= ',G13.6)
	   call BELL(1)
	   call WDIALOG(1,' O.K. [Y] ?',icw1)
c	   read 101,ans
c NB -the 'do while' form will jump straight out, without waiting for
c character, if ans='Y','N' or ' ' before 'do while' on arrival here
c	   do while (ival.ne.78.and.ival.ne.89.and.ival.ne.13)      !N,Y, enter
32	   ans=UC(GETCH(ktype))
	   ival=ICHAR(ans)
c	   enddo
	   if(ival.ne.78.and.ival.ne.89.and.ival.ne.13) goto 32     !N,Y, enter
c	   call CLRDIALOG(1,ibk)	!clear dialog box #1
	   call CLRDIALOG(1,0)		!clear dialog box #1 to black
	   call FILLWIN(0,640,iyd1,iyd2,ibk1)	!clear data area
	   sdone=.true.
	   if(UC(ans).eq.'N') then
		sdone=.false.	!carry on to do again
	      call WDIALOG(1,
     &	 'GET BIT OF EMPTY BASELINE ON SCREEN FOR SD',icw1)
	      call BELL(2)
		goto 78
	   endif
c iyoff, ioff1 must be in pixel units
	   ybasevga=ybase*sfac3 + iyoff		!convert to display (pixel) units
	   ioff1=ifixr(basevga-ybasevga)		!offset to adjust baseline to bdisp
	   iyoff=iyoff+ioff1		!new offset for next display
	   if(opendown) then
	      itrigvga=ifix(basevga-0.12*(yd2-yd1))		!init value for trigger level
	   else
	      itrigvga=ifix(basevga+0.12*(yd2-yd1))		!init value for trigger level
	   endif
	   itriglev=(itrigvga-iyoff)/sfac3	!in intermed units (same as idata)
	   incabs=160			!fast roll now
	   indfst=indfst0			!start again from beginning
	   indlast=indfst+ndisp-1
	   isdfst=indfst0 - (isec0-1)*nbuf	!index WITHIN section of point #indfst
	   isdlast=isdfst+ndisp-1
	   iprogx1=1 + indfst/ibdpp	!current X coord (# of pixels) for upper prog line
	   iprogx2=iprogx1			!ditto for lower line (does not move back)
	   call NEWPROGLINE()	!restore prog line
	   if(.not.ampdone) then
	      call WDIALOG(1,
     &	 'NOW FIT A GOOD FULL OPENING FOR AMPLITUDE',icw1)
	      call BELL(2)
	   endif
	   if(isec.ne.isec0) then
		isec=isec0
		goto 444	!read in 1st section again
c	   else
c still in current section -new iyoff should be used for next display
c	      do i=js1,js2
c		  idata(i)=idata(i) + ioff1
c	      enddo
	   endif
	endif
c
c Draw progress line (+marker, red cursor)
78	continue
	call put_cursors(0,ixframe,iyframe,icframe,iprogx1,icprog,
	&		     ixcr,iycr,iccr,IXMARK,IYMARK,icmark,ibdx,ibk)
c	CALCULATE IX=X array for display (outside loop)
	dx=640.0/float(ndisp)		!use ndisp (not ndisp1) here
	do i=1,ndisp
		ix(i)=ifixr(float(i-1)*dx)	!=0 for 1st point
	end do
	ifirst=1		!no erase in AIVPIX1 first time (which resets it to 0)
c
c Do initial display so there is something on screen when prog stops (in
c scroll_bar) at initial menu. Don't increment indfst so same display repeated
c in loop when roll forward started (so first INC points are searched for trigger)
cc	call AIVPIX1(ix,idata,ndisp,jfst,inc,ipen,ifirst,ibk)
c	call AIVPIX0(ix,idata(isdfst),ndisp1,inc,ipen,ifirst,ibk)
c	call AIVPIX3(ix,idata(isdfst),ndisp1,idata(isprev),
c     &	ipen,ifirst,ibk1)
	call AIVPIX4(ix,idata(isdfst),ndisp1,idata(isprev),
     &	ipen,ifirst,ibk1,nscale,iyoff,iyoffprev)
	isprev=isdfst	!record value in case needed in DELPIX
	ndprev=ndisp1	!ditto
	iyoffprev=iyoff
cc debug! I think that ADCPIX does not work properly!
cc	call SETACT(iact,-1)
cc	iact(ipen)=ibk	!redraw points of colour ipen in background colour
cc	call ADCPIX(ix,idata(isprev),ndisp,iact)

	if(sdone) then
	   do i=0,639
c		call rpixel(i,iybar1,iabar1(i))       !save colours to restore later
c		call rpixel(i,iybar2,iabar2(i))
		call rpixel(i,itrigvga,iabar3(i))
     	   end do
c	   call hline(0,639,iybar1,icbar1)   !SET BASELINE -overwrites all cols
c	   call hline(0,639,iybar2,icbar2)   !SET AMPLITUDE
	   call HLINE(0,639,ifixr(basevga),icbl)	   !show baseline
	   call HLINE(0,639,itrigvga,ictrig)   !SET THRESHOLD LINE
	endif

22	CONTINUE		!return here to REROLL
c==	kx=152
c write section number (?)
c	call set_cursor(ixstr,iystr)
c	call det_string(pch(isec),icstr,ch)
c
c Read initial values of ADC for rolling display

	if(usepots) call READAMP(ichan,nadc,iadc0)
c
25	continue
			!LOOP WHEN YOU STOP FROM BACKWARDS LOOP
			!STOP (MANUAL/AUTOMATICALLY) FROM FORWARDS LOOP
			!get to END of data
c STOP option returns here and waits for new option
c	stopped=.false. !necessary? Don't reset here as it is needed in GETOPT?
c	call SET_MENU(iXIN1,iYIN1,iXUP1,iYUP1,TXTWIN1,iFWIN1,iCTXT1,
c     &jX1,jY1,kX)
c	nout1=1
c===========
150	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	Lb(14)=0		!not used yet
	if(.not.sdone) Lb(4)=0	!draw separately
c	call DCMENU(0,4,Lb,text,0,0)		!delete all
151	continue
	text(1)='1: ROLL ON     '
	text(2)='2: ROLL BACK  '
	text(3)='3:EXPAND MODE'
	if(sdone) then
	   text(4)='4:TRIGGER LEVEL'
	else
	   text(4)='4.ACCEPT BASELINE'
	endif
	text(5)='5:GO TO CURSOR'
	text(6)='6:FIT TRANSITIONS'
	text(7)='7: QUEUE PLOT '
	text(8)='8: STOP ROLL'
	text(9)='9: QUIT'
	text(10)='10:  HELP (F1)'
	text(11)='+:FASTER ROLL'
	text(12)='-:SLOWER ROLL'
	text(13)='*:NEW PARAMETERS'
	text(15)='.:DEFAULT OPTIONS'
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
c	call DCMENU(-4,4,Lb,text,0,0)	!delete box 4 only
c	Lb(4)=-1		!italic
	if(.not.sdone) call DCMENU(-4,4,Lb,text,icol3,icol4)	!draw box 4 only
c===========
c	------------------------------------------
c Return here after setting triggerlevel, inc speed, dec speed, help
30	continue
c152	ikey=nkey()
152	call CKEY(ch,ikey)
	if(ikey.lt.-1.or.ikey.gt.15) goto 152
	if(ikey.eq.-1) ikey=10		!F1=help
c==	if(ikey.eq.-1.or.ikey.eq.10) then	!F1 key
c==	   iret=1
c==	   goto 237		!help
c==	endif
	select case(ikey)
	   case(1)		!roll on
		if(end) goto 30
		inc=incabs		!set increment positive
		backward=.false.
		stopped=.false.
	   case(2)		!roll back
	      if(indfst.lt.incabs) goto 25
		end=.false.
		inc=-incabs
		backward=.true.
	   case(3)
		goto 800		!expand mode
	   case(4)			!set trigger level
		if(sdone) then
		   call set_cursor(ixmes,iymes)
		   call det_string('<Enter>=continue; \/=down; /\=up',
     &	     14,ch)
		   call move_line(itrigvga,ictrig,iabar3,ii)
		   itriglev=(itrigvga-iyoff)/sfac3	!in intermed units (as idata)
		   call fillwin (ixfill,ixfill+300,iyfill-16,iyfill,ibk1)
		   goto 30
		else
		   ndfit=ndisp			!copy data for rms/baseline
		   do i=isprev,isprev+ndisp-1
		     j=i-isprev+1
		     idata1(j)=idata(i)	!copy data to be fitted into idata1()
		   enddo
		   call NUMCLR
		   baseOK=.true.	!baseline accepted
		   goto 31
		endif
	   case(5)		!go to red cursor
		end=.false.
		triggered=.false.		!in case come to here after trigger set
		call set_cursor(ixmes,iymes)
		call det_string(
     &	'<-=left; ->=right; home; end; <Enter>=continue',14,ch)
		call MOVE_RED_CURSOR(ixcr,iycr,iccr,ixframe,ibk)
		ivl=indfst/ibdpp
		indfst=float(ixcr)*ibdpp
		if(indfst.lt.1) indfst=1
		indlast=indfst+ndisp-1
		call FILLWIN(0,640,iyd1,iyd2,ibk1)	!fill data area with colour ibk
c		call fillwin(0,640,90,480,ibk)
		call vline(ivl,iyFRAME,iyFRAME+7,icframe)
		goto 444
	   case(6)		!fit transitions
		ndfit=ndisp			!copy data for fitting into idata1
		do i=isprev,isprev+ndisp-1
		  j=i-isprev+1
		  idata1(j)=idata(i)	!copy data to be fitted into idata1()
		enddo
		fitmode=.true.
		goto 200
	   case(7)		!plot
c           calfac=pA/ADC (ymin,ymax not needed in AUTPLOT?)
		call SCQPLT(idiskq,ndev,indfst,isdfst,ndisp1,idata,
     &      nbuf,novlap,srate,calfac,calfac2,ioff,cdate,adctime,mono)
		goto 152	!get another option
	   case(8)		!stop
		stopped=.true.
		goto 25
	   case(9)		!quit
		goto 99
	   case(10)		!help
		call first_help(ibk)
		goto 30
	   case(11)		!Increase roll rate
		   if(incabs.lt.(ndisp-16)) then
			incabs=incabs+16
			inc=incabs
			if(backward) inc=-incabs
		   endif
		   goto 30
	   case(12)		!decrease roll rate
		if(incabs.ge.16) then
		   incabs=incabs-16
		   if(incabs.le.0) incabs=4
		   inc=incabs
		   if(backward) inc=-incabs
		endif
		goto 30
	   case(13)		!new parameters
c========not yet done
		goto 152	!get another option
	   case(14)		!no key yet
		goto 152	!get another option
	   case(15)		!default options
c========not yet done
		goto 152	!get another option
	end select
c	call SCROLL_BAR(ixin1,iyin1,ixup1,iyup1,jx1,jy1,n1,pchar1,
c     &txtwin1,ifwin1,ictxt1,nout1,achar,ktype,ivar)
cc Waits in SCROLL_BAR for option
c	call GETOPT(ixin1,iyin1,ixup1,iyup1,n1,pchar1,
c     & txtwin1,ictxt1,nout1,achar,ibkwin1,inc,incabs,ndisp,
c     & ixmes,iymes,ch,iybar1,icbar1,iabar1,ii,iybar2,icbar2,
c     & iabar2,itriglev,ictrig,iabar3,ixfill,iyfill,ibk,ibk1,end,
c     & stopped,backward,indfst,isdfst,isec,nbuf,sdone,
c     & ixcr,iycr,iccr,ixframe,iyframe,ivl,ibdpp,ilabel)
c	if(discprt) then	!debug
c	   write(8,704) backward,ilabel
c704	   format(' backward = ',l4,': ilabel = ',i3)
c	endif
c	if(ilabel.ne.0) then
c	  goto(5,8,22,25,30,77,333,444,800,32) ilabel
c	endif
c
c
c After trigger, goes back to AIVPIX to do display according to XTRIG value
c then to 25 to wait for option.  If 'roll on' (forward or back) chosen, then
c arrives here. Move on INC points, so same trigger not found again (NB means
c that any alter trigger crossings in the same INC points as that in which
c original trigger found will be missed (could move on fewer points?)
	if(triggered) then
	   triggered=.false.
	   indfst=indsav + inc			!move on INC points after trigger
	   indlast=indfst+ndisp1-1
	   isdfst=indfst - (isec-1)*nbuf	!index WITHIN section of point #indfst
	   isdlast=isdfst+ndisp1-1
	endif

77	continue
	call NUMSET		!num lock on for kbhit() options
	if (debug()) then
	   call set_cursor(ixdat,iydat-2)
	   print 700,indfst,isdfst,ndisp1,inc,isec,
     &	triggered,backward,stopped
	endif
c	call AIVPIX1(ix,idata,ndisp,jfst,inc,ipen,ifirst,ibk)
c	call AIVPIX0(ix,idata(isdfst),ndisp1,inc,ipen,ifirst,ibk)
c	call AIVPIX3(ix,idata(isdfst),ndisp1,idata(isprev),
c     &	ipen,ifirst,ibk1)
	call AIVPIX4(ix,idata(isdfst),ndisp1,idata(isprev),
     &	ipen,ifirst,ibk1,nscale,iyoff,iyoffprev)
	isprev=isdfst	!record start value ready for erasing next time
	ndprev=ndisp1	!ditto
	iyoffprev=iyoff
c Debug:
	if(discprt) then
	   write(8,701) indfst,indfst+ndisp1-1,isdfst,isdfst+ndisp1-1,isec
701	   format(
     & ' Display points ',i8,' to ',i8,' i.e. ',i8,' to ',i8,
     & ' in section ',i4)
	endif
771	continue	!jump here when lastdisp=true
c CALCULATE PARAMETERS TO MOVE PROGRESS LINE:
c	iprogx1=ipxs+ifixr(float(isdfst)/float(ibdpp))
c I think here i have to put a delay for backwards because first
c draw the progress line and then the points at the section limits!
c	call move_progress_line(iprogx0,iprogx1,iprogx2,iyframe,
c     &   icprog,ibk,flag_pline)
c	call move_prog_line(iprogx0,iprogx1,iprogx2,iyframe,
c     &   icprog,ibk,backward)
c	call MOVE_PROG_LINE(indfst,iprogx0,iprogx1,ibdpp,iyframe,
c     & icprog,ibk,backward)
c Draw prog line taking position from indfst
	call MOVE_PROG_LINE(indfst,backward)	!other params in common/prog/
c	iprogx0=iprogx1	!now in subroutine
c	iprogx2=iprogx1
c
c Deal with trigger after display: go to 25 to wait for option to be
c given (with triggered=true still).  Next display will depend on whether
c 'roll on' (forward or backward) is chosen, or 'expand' is chosen
c
	if(triggered) then
	   call NUMCLR
	   goto 25	!wait for options in main menu (eg roll on/expand/...)
	endif
c
c Update section# display -romoved for now
cc	if (jfst.lt.novlap.and.backward) then
c	if (backward.and.isdfst.lt.1) then	!prev line is 'isdfst.lt.0'; wrong!?
c		call set_cursor(ixstr,iystr)
c		call det_string(pch(isec-1),icstr,ch)
c	else
c		call set_cursor(ixstr,iystr)
c		call det_string(pch(isec),icstr,ch)
c	endif
c====================================================================
c Search for threshold crossing: if found set 'triggered'=true
c Can search up to idata(js2)=last defined value in idata()
	if(.not.sdone) goto 40	!skip trigger search
	istart=isprev
	iend=isdfst+incabs-1	!look at first INC points in section normally
	if(iend.gt.js2) iend=js2
c If lastdisp or fstdisp then search whole display (actually not quite right
c because some of these may get searched again in next/prev section -next
c bit should fix this for lastdisp)
c===not fixed yet for fstdisp; this is done when moving backwards so
c need search only first incabs points here, but some points will not get
c searched when we move to prev section for next display)
	if(lastdisp) then
	   iend=isprev+ndprev-1		!search whole display
	   if(isec.lt.nsec) then
		iend=jn1next - (isec-1)*nbuf	!next display will start at jn1next
	   endif
	endif
c debug:
	if(discprt) then
	   kn1=istart + (isec-1)*nbuf
	   kn2=iend + (isec-1)*nbuf
	   write(8,702) kn1,kn2,isdfst,iend,isec
702	   format(
     & ' Search points ',i8,' to ',i8,' i.e. ',i8,' to ',i8,
     & ' in section ',i4)
	endif
	do k=istart,iend
		if(idata(k).le.itriglev) then
			triggered=.true.
			ktrig=k	!record index where trigger occurred
			ksect=isec	!and section #
			if(discprt) write(8,703) ktrig,ksect
703			format(' Trigger found at point ',i7,' section ',i4)
c			indsav=indfst	!keep orig value
c			isdsav=isdfst	!keep orig value
c			ndsav=ndisp1	!keep orig value
c		  redefine display according to xtrig
			isdfst=k-ifix(xtrig*float(ndisp1))
			isdlast=isdfst+ndisp1-1
			indfst=isdfst + (isec-1)*nbuf
			indlast=indfst+ndisp1-1
			indsav=indfst	!keep orig value
			isdsav=isdfst	!keep orig value
			ndsav=ndisp1	!keep orig value
			goto 50	!skip increment, then display according to xtrig
c			goto 25
		endif
	end do
40	continue
c If end reached then, after display and search for trigger, return to menu (25)
c and wait for new option
	if(end) then
	   end=.false.
	   goto 25
	endif

c Now decide on increment for next section (via pots or keys), or respond
c to other key inputs, before incrementing index for next section
c Control via POTS
	if(usepots) then
	   call READAMP(ichan,nadc,iadc)
	   idt0=int4(iadc(0)-iadc0(0))
c	   inc=inc + idt0/5
	   inc=160+idt0/8			!gives problem with 'rewind' key??
	   backward=inc.lt.0
	   incabs=iabs(inc)		!reset abs value
c	   iadc0(0)=iadc(0)		!reset init value
	   call LOCATE(3,0)
	   print *,inc
	endif
c Menu is same as above, though not all options vailable here: must hit a
c number pad key!
	if(kbhit()) then
c	   achar=getch(ktype)
	   call GETIKEY(ikey)
	   if(ikey.le.0) goto 98	!carry on
	   select case(ikey)
		case(1)		!roll on
		   inc=incabs		!set increment positive
		   backward=.false.
		case(2)		!roll back
		   inc=-incabs
		   backward=.true.
		case(3)
		   call NUMCLR
		   goto 800		!expand mode
		case(4)			!set trigger level -not here
		case(5)		!go to red cursor -not here
		case(6)		!new parameters -not here
		case(7)		!plot	-not here
		case(8)		!stop
		   call NUMCLR
		   stopped=.true.
		   goto 25
		case(9)		!quit
		   call NUMCLR
		   goto 99
		case(10)		!help
		   call NUMCLR
		   call first_help(ibk)
		   goto 30
		case(11)		!Increase roll rate
		   if(incabs.lt.(ndisp-16))then		!ndisp1?
			incabs=incabs+16
			inc=incabs
		      if(backward) inc=-incabs
		   endif
		case(12)		!decrease roll rate
		   if(incabs.ge.16) then
			incabs=incabs-16
			if(incabs.le.0) incabs=4
			inc=incabs
			if(backward) inc=-incabs
		   endif
		case(13)
		case(14)
		case(15)
	   end select
98	   continue
	   ndisp1=ndisp	!unless display truncated =========needed here?
	   if (debug()) then
	      print 700,indfst,isdfst,ndisp1,inc,isec,
     &	  triggered,backward,stopped
700		format(' indfst=',i9,' isdfst,ndisp,inc,isec = ',4i7,/,
     &	  ' trig= ',l4,' back = ',l4,' stop= ',l4)
	   endif
	endif		!end of kbhit section
c
c Now increment for next display
c (NB lastdisp, fstdisp should never be used if novlap big enough -can be
c removed from loop somehow?)
	if(lastdisp.and.isec.lt.nsec) then
	   lastdisp=.false.	!reset
c now delete AFTER new section read in
	   indfst=jn1next       !display from 1st point in next section
	   indlast=indfst+ndisp-1
	else if(fstdisp.and.isec.gt.1) then
	   fstdisp=.false.	!reset
c now delete AFTER new section read in
	   indlast=jn2prev
	   indfst=indlast-ndisp+1
	else
 	   isdfst=isdfst+inc
	   isdlast=isdfst+ndisp-1
	   indfst=indfst+inc
	   indlast=indfst+ndisp-1
	endif
	if(isec.eq.1.and.indfst.lt.1) then		!reached start
	   isdfst=1
	   isdlast=ndisp
	   indfst=1
	   indlast=ndisp
	   call NUMCLR
	   goto 25	!start of data: stop and choose new option
	endif
	if(isec.eq.nsec.and.indlast.gt.ndat) then	!reached end
	   isdlast=js2
	   isdfst=isdlast-ndisp+1
	   if(isdfst.lt.js1) then
		isdfst=js1
		ndisp1=isdlast-isdfst+1
	   endif
	   indfst=isdfst + (isec-1)*nbuf
	   indlast=indfst+ndisp1-1
	   end=.true.
c	   goto 25	!end of data: stop and choose new option -NOT YET DISPLAYED!
	endif
c	call set_cursor(ixdat,iydat-2)
c
c These values define next display -are all necessary points present
c in IDATA()? If not then read another section into IDATA
c Check that the specified starting point for the display, consam(indfst),
c is not so near to the end of section #isec (just calc) that there are
c too few points to show it (but if no overlap then must show whatever there is!)
c   Use overlap in current section if long enough. If not
c then use next/prev section (a) if there is one and (b) if it has overlap
c long enough to show the specified first point (indfst).  Otherwise show
c truncated display, starting with the correct point in current section but
c with fewer than ndisp points.  Only one such display is needed to show
c all the available data in current section (no point in rolling it across
c the screen) so use special call to AIVPIX here.  But if not rolled must
c search ALL the points for a trigger
50	continue
	if(indfst.ge.jn1.and.indlast.le.jn2) goto 77	!whole display in isec
									!so keep rolling
	ndisp1=ndisp		!unless truncated
	if(indlast.gt.jn2) then	!display ends in the next section
	   if(isec.lt.nsec.and.indfst.ge.jn1next) then	!use next section
		isec=isec+1
		call CALCIND(isec,nbuf,nrlast,novlap,nsec,
     & 	 in1,in2,jn1,jn2,is1,is2,js1,js2,nread,jn1next)	!recalc indices
	   else		!show remainder (<ndisp points) from current section
c next AIVPIX is called with ndisp1<ndisp so it would erase only ndisp1 points,
c therefore delete all points drawn last time (ndprev) now: use aivpix4
c with ifirst=-1 to erase only (could also use DELPIX4)
		ifirst=-1		!erase only in AIVPIX4
		call AIVPIX4(ix,idata,ndprev,idata(isprev),
     &	 ipen,ifirst,ibk1,nscale,iyoff,iyoffprev)
c=		call AIVPIX4(ix,idum,ndprev,idata(isprev),
c=     &	 ipen,ifirst,ibk1,nscale,idum,iyoffprev)
c		call DELPIX(ix,idata,ndprev,isprev,ipen,ibk1,
c     &	 nbuf,novlap)
		ndisp1=jn2-indfst+1
c		call AIVPIX0(ix,idata(isdfst),ndisp1,inc,ipen,ifirst,ibk)
c		call AIVPIX3(ix,idata(isdfst),ndisp1,idata(isprev),
c     &	 ipen,ifirst,ibk1)
		ifirst=1	!so draw only (no erase) in next AIVPIX
		call AIVPIX4(ix,idata(isdfst),ndisp1,idum,
     &	 ipen,ifirst,ibk1,nscale,iyoff,idum)
		isprev=isdfst	!record value in case needed in DELPIX
		ndprev=ndisp1	!ditto
		iyoffprev=iyoff
		if(discprt) then		!debug
		   kn2=indfst+ndisp1-1
		   ks2=isdfst+ndisp1-1
		   write(8,701) indfst,kn2,isdfst,ks2,isec
		endif
		lastdisp=.true.	!so right points are searched etc
c   now back to loop with lastdisp=true, but skip aivpix, which is already done
		goto 771	!no new section to be read
	   endif
	else if(indfst.lt.jn1) then	!display starts in the prev section
						!i.e. rolling back
	   jn2prev=(isec-1)*nbuf+novlap		!last point in isec-1
	   if(isec.gt.1.and.indlast.le.jn2prev) then	!use prev section
		isec=isec-1
		call CALCIND(isec,nbuf,nrlast,novlap,nsec,
     & 	 in1,in2,jn1,jn2,is1,is2,js1,js2,nread,jn1next)	!recalc indices
	   else 			!show fewer than ndisp points from current section
c problem when rolling back AND novlap not big enough to allow full length
c (ndisp points) display from either current or previous section -partial display
c as above should use the specified indlast at right edge of screen and work
c leftwards to point jn1=last available point in current section. Can achieve
c this by calling AIVPIX with ix(ixoff) where ixoff normally 1, but reset here.
c Delete all points drawn last time (ndprev) now: use aivpix4
c with ifirst=-1 to erase only (could also use DELPIX4)
		ifirst=-1		!erase only in AIVPIX4
		call AIVPIX4(ix,idum,ndprev,idata(isprev),
     &	 ipen,ifirst,ibk1,nscale,idum,iyoffprev)
c		call DELPIX(ix,idata,ndprev,isprev,ipen,ibk1,
c     &	  nbuf,novlap)
		ndisp1=indlast-jn1+1
		ixoff=ndisp-ndisp1+1
c		call AIVPIX0(ix(ixoff),idata(is1),ndisp1,inc,
c		call AIVPIX3(ix(ixoff),idata(is1),ndisp1,idata(isprev),
c     &	 ipen,ifirst,ibk1)		!display from 1st point of section=is1
		ifirst=1	!so no delete in next AIVPIX
		call AIVPIX4(ix(ixoff),idata(is1),ndisp1,idata(isprev),
     &	 ipen,ifirst,ibk1,nscale,iyoff,iyoffprev)
		isprev=is1		!record value in case needed in DELPIX
		ndprev=ndisp1	!ditto
		iyoffprev=iyoff
		if(discprt) then		!debug
		   ks2=is1+ndisp1-1
		   kn1=is1 + (isec-1)*nbuf
		   kn2=kn1+ndisp1-1
		   write(8,701) kn1,kn2,is1,ks2,isec
		endif
		fstdisp=.true.		!so right points searched etc
c   now back to loop with lastdisp=true, but skip aivpix, which is already done
		goto 771	!no new section to be read
	   endif
	endif
c===later:	put up red box to say display truncated?!
c Now in new section so erase last display separately here (and set ifirst=1
c so next AIVPIX draws new display only) do this before idata() changed!
c -but this leaves the screen blank while new data is being read in -can
c avoid this by copying current data to idata1, and using this to delete
c AFTER call to READSEC
	do i=1,ndprev
	   idata1(i)=idata(i+isprev-1)
	enddo
	ifirst=1
c Read the new section
	call READSEC(isec,idata,nrlast,nsec,ioff,nbuf,nread,novlap,
     & sfac2,in1,in2,jn1,jn2,is1,is2,js1,js2)
c	call set_cursor(ixstr,iystr)
c	call det_string(pch(isec),icstr,ch)

c Now delete previous display AFTER new data read in (note: nbuf, novlap in
c call to DELPIX replaced with 2048, 0 i.e. dimensions of idata1())
c (ixoff normally =1, but needed for deletion after FSTDISP when
c rollong backwards): use aivpix4
c with ifirst=-1 to erase only (could also use DELPIX4)
	ifirst=-1		!erase only in AIVPIX4
	call AIVPIX4(ix(ixoff),idum,ndprev,idata1,
     &	 ipen,ifirst,ibk1,nscale,idum,iyoffprev)
c	call DELPIX(ix(ixoff),idata1,ndprev,1,ipen,ibk1,2048,0)
c Recalc indices for display appropriate for the section just read.
	ixoff=1		!reset
	isdfst=indfst - (isec-1)*nbuf	!index WITHIN section of point #indfst
	isdlast=isdfst+ndisp-1
	goto 77

c**********************************************************************
c EXPAND MODE HERE
800	continue
c if arrive at expmode with trigger set, then reset here and use display
c dictated by XTRIG as starting point for expand/contract as specified
c in indfst, ndisp1 (pre trigger display already stored in indsav, ndsav.
c For expand/contract store the display to be restored (on return from
c expmode) via isprev, ndprev here)
c NO -leave triggered=true, so when EXPMODE is left to reroll, the display
c jumps past trigger, and 'triggered' is reset, after GETOPT
c	if(triggered) then	!expand mode after trigger
c	   triggered=.false.
c	else
	if(.not.triggered) then	!(if triggered, indsav etc already set)
	   isdsav=isprev		!to restore display after expmode
	   indsav=isprev + (isec-1)*nbuf
	   ndsav=ndprev
	endif
	ip1sav=iprogx1	!save progress line position
	ip2sav=iprogx2	!save progress line position
	call EXPMODE(IDATA,isdfst,indfst,ndisp1,finter,
     & nsec,iyd1,iyd2,ibk,ibk1,ibk2,expfac,js1,js2,isec,nbuf,novlap,
     & sdone,usepots,mono,baseOK,fitmode,basevga,nscale,iyoff,sfac3,
     & icol1,icol2,icol3,icol4,icbl,
     & ipen,idata1,ndfit,ix,dx,
     & idiskq,ndev,srate,calfac,calfac2,ioff,cdate,adctime)
c ndfit,ix,dx are returned with fitmode=true when section on screen is
c to be fitted; idata1(1) to idata1(ndfit) contain data to be fitted in FITSUB
c
	if(fitmode) goto 200
c
c REROLL after expmode
210	continue
	call FILLWIN(0,640,iyd1,iyd2,ibk1)		!clear data area
	indfst=indsav		!restore value before expmode
	isdfst=isdsav		!restore value before expmode
	ndisp1=ndsav
	indlast=indfst+ndisp1-1	!restore
	isdlast=isdfst+ndisp1-1	!restore
	call NEWPROGLINE()	!restore prog line
	ifirst=1	!prev display erased by fillwin in EXPMODE
	if(baseOK.and.(.not.sdone)) then
	   call NUMCLR
	   goto 31	!restore prog line after 31
	else
c restore progress line to what it was before EXPMODE
	   iprogx1=ip1sav	!restore progress line position
	   iprogx2=ip2sav	!restore progress line position
	   goto 5	!REROLL (goto 5 to do 1st display there, not above)
	endif
c
c Now transition fitting
200	continue
	if(fitmode) then
c	   if(discprt) then	!debug
c		do i=1,nfilt1,5
c		  write(8,704) (j,filt1(j),j=i,i+4)
c704           format(5(3x,i6,1x,f7.4))
c		enddo
c	   endif
	   call FILLWIN(0,640,iyd1,iyd2,ibk2)
	   call HLINE(0,639,ifixr(basevga),icbl)	   !show baseline
	   call FITSUB(idata1,ix,dx,ndfit,basevga,ampdone,ntrig,
     &    navtest,scrit,ibk2,tf50,opendown,calfac1,calfac2,
     &    nscale,iyoff,sfac3,timamax,mono,open,shut,nshutfit,
     &    yd1,yd2,avtot,navamp,avamp,kt)
	   fitmode=.false.
	   triggered=.false.
	   if(ampdone) then
		goto 210		!reroll from present position
	   else
	      call DCFORMAT(avamp*calfac2,7,2,cnum)
	      call WDIALOG(1,'Amplitude (pA)'//cnum,icw)
		call BELL(1)
		call WDIALOG(1,' O.K. [Y] ?',icw1)
34		ans=UC(GETCH(ktype))
		ival=ICHAR(ans)
		if(ival.ne.78.and.ival.ne.89.and.ival.ne.13) goto 34     !N,Y, enter
c		read 101,ans
		call CLRDIALOG(1,0)		!clear dialog box #1 to black
		call FILLWIN(0,640,iyd1,iyd2,ibk1)	!clear data area
		ampdone=.true.
		if(UC(ans).eq.'N') then
		   ampdone=.false.	!carry on to do again
	         call WDIALOG(1,
     &	 'TRY ANOTHER GOOD FULL OPENING FOR AMPLITUDE',icw1)
	         call BELL(2)
		   goto 78
		else
		   call ENDDIALOG(1,ibk)	!remove box 1
	      endif
		indfst=indfst0			!start again from beginning
		indlast=indfst+ndisp-1
		isdfst=indfst0 - (isec0-1)*nbuf	!index WITHIN section of point #indfst
		isdlast=isdfst+ndisp-1
		iprogx1=1 + indfst/ibdpp	!current X coord (# of pixels) for upper prog line
		iprogx2=iprogx1			!ditto for lower line (does not move back)
		call NEWPROGLINE()	!restore prog line
		if(isec.ne.isec0) then
		   isec=isec0
		   goto 444	!read section isec0
		else
		   goto 5	!re-roll from start
		endif
	   endif	!end of initial amplitude setting
	endif		!end of fitting

c QUIT NOW
99	continue
      call NUMCLR
c	Ask if you want to continue or quit:
c	call set_cursor(ixmes,iymes)
c	call det_string('do you really want to Quit ?',icstr,ch)
c	ans=getch(ktype)
c	if(UC(ans).eq.'n') then
c		call fillwin(300,640,90,110,ibk)
c		ifile=1
c		goto 222		!restart from beginning
c	endif
c
c==	deallocate(txtwin1,txtwin2,text1,text2,pchar1,pchar2,idata)
c
c999	call mode(3)
c	end
999	continue
	deallocate(idata)
	if(VIDEOTYP().ne.3) call VIDEOMOD(3)
c	call ENDPLT		!Hgraph version
	call NUMCLR
	call ENDPRINT
	end

