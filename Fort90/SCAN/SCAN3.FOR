	program SCAN
VERSION FROM 15-3-93 THAT USES IOANA MENUS (LATER VERSION USE MAINLY DCMENU)
c=====TO DO
c======at present the first NINC points are not searched for trigger!
c	  -THIS NOW FIXED by doing all displays in loop, but
c	Can the displays for fstdisp, lastdisp also be done regular
c	aivpix in the loop (problem is when we want to truncate on left!)?

c======fstdisp,lstdisp should never be used in novlap big enough -check!
c======also should only be used backward/not backward

c====see errors in SCAN1.PRT

c (3) then start on transition detection -choose display in EXPMODE
c	and leave with display up; look for transitions in displayed
c	points and fit times/amplitudes
c (4) fix menus (use DCBOX?) -make defaults-menu as subroutine?
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
c Scaling: 1401 units are -32768 t0 +32752 in 16 unit increments; dividing
c by 16 gives range as -2048 to +2047 (as for CED502) =-5.0V to +4.9976V
c So 32768/5=6553.6 units/volt, or, for CED502 data 2048/5=409.6
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
	integer*2 lrow,int2,videotyp,ktype
	integer iact(0:15)
	logical discprt
	logical pon,slock,debug,caplock
	character*11 cdate,ctime
	character*32 filnam	!make 32 bytes (so common optimally aligned)
	character*74 mtitle
c For INSCAN:
	real*4 tint(1024),timsav(1024)
	integer*2 iampl(1024)
c==	integer ksect(1024),ishut(1024)
	real*4 filt(100)
	logical invert,openup,newpar,warned,sdone,restart
	logical disp,rescale,auto,end,backward,stopped,triggered
	logical fitmode,usepots,fstdisp,lastdisp
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
	allocatable	:: transfer
	allocatable :: text1,text2
	allocatable :: txtwin1,txtwin2,pchar1,pchar2
c============================================================
	integer*2 	idata(:)
	integer*2	txtwin1(:,:),txtwin2(:,:),transfer(:)
	integer*2 	ix(2048)
	integer*2 idata1(2048)	!for data to be fitted (and as temp file)
c============================================================
	integer*4	iabar1(0:641),iabar2(0:641),iabar3(0:641)
	character*1	 getch,achar,ch,ans,UC,LC
	character*1  	pchar1(:),pchar2(:)
	character*10	pch(20)
	character*18 	text1(:)
	character*11 	text2(:)
	logical kbhit
c end of IV declarations
	common/dp/discprt
	common/dpp/filnam,machine,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	common/prog/iprogx0,iprogx1,iprogx2,ipmax,ibdpp,iyframe,icprog,ibk
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
	sdone=.false.	!not yet determined baseline and SD
	ilabel=0
c
	call INSCAN(iampl,tint,ntmax,nbuf,novlap,srate,calfac,
     & finter,tsect,ioff,ndat,nsec,nrlast,namp,totamp,iampsv,ibassv,
     & ktrans,nwrit,nwsav,nfits,newpar,igback,restart,openup,invert,
     & usepots,disp,smult,ntrig,dgain,iboff,rms,
     & expfac,bdisp,isub,xtrig,nsetup,filt,nfilt,dtf,tf0,tf50,tf1,
     & warned,sdone,ifop,rescale,auto,iscan,idest)
	if(idest.eq.99) goto 999
c Fix scaling here (or in INSCAN)
c======not yet fixed for INVERT or OPENUP!
c Scaling: 1401 units are -32768 t0 +32752 in 16 unit increments; dividing
c by 16 gives range as -2048 to +2047 (as for CED502) =-5.0V to +4.9976V
c So 32768/5=6553.6 ADC units/volt
c Use Y coords (0-479) between 130, 450 for now, to span 65536 ADC units
c Calc initial value of iboff here (not inscan)?
	yd1=90.	!lowest point on screen (Y pixels) for display of channels
	yd2=390.	!highest point on screen (Y pixels) for display of channels
	scalfac=(yd2-yd1)/65536.
	scalfac=scalfac*dgain
c==	iyoff=ifixr(yd1 + bdisp*(yd2-yd1))	!baseline position (in Y pixels)
	iyoff=ifixr(yd1 + 0.5*(yd2-yd1))	!zero in centre until baseline done
c	iyzero=		!abs zero of current irrelevant as offset for sampling
c
c=============
c IV menus
c=============
c==============================================================
c	THIS PART IS DEALING ONLY WITH THE MENUS
C==============================================================
c	Enter initial coordinates and parameters for menues
	ixin1=16
	iyin1=16
	n1=16
	ifwin1=3
	ictxt1=8

c definitions for EXPMODE (prev called SCREEN2)
	ixin2=120
	iyin2=16
	ixup2=624
	iyup2=96

	n2=15
	ifwin2=8
	ictxt2=8

c	Calculates the colors for background and scroll bar:
	call set_colors(ifwin1,ibkwin1,isbwin1)
	call set_colors(ifwin2,ibkwin2,isbwin2)
c
c
c     Now establishes  the dimensions of text
c
	allocate (text1 (1:n1))
	allocate (text2 (1:n2))
	allocate (pchar1 (1:n1))
	allocate (pchar2 (1:n2))
c
c
c     Here You define the content of strings:
c
C	FIRST WINDOW(MENU) for FAST SCROLLING:
C	--------------------------------------
c 	text1(1)='Forward           '
c	text1(2)='Increase speed    '
c	text1(3)='Baseline          '
c	text1(4)='Next section      '
c	text1(5)='Rewind           '
c	text1(6)='Decrease speed    '
c	text1(7)='Amplitude line    '
c	text1(8)='Stop              '
c	text1(9)='Expand            '
c	text1(10)='Change ndisplay   '
c	text1(11)='Threshold bar     '
c	text1(12)='Quit              '
c	text1(13)='Move red cursor   '
c	text1(14)='Other file        '
c	text1(15)='Window            '
c	text1(16)='Help              '
C	SECOND WINDOW(MENU) for SLOW SCROLLING:
C	---------------------------------------
	pchar1(1)='f'
	pchar1(2)='i'
	pchar1(3)='b'
	pchar1(4)='n'
	pchar1(5)='r'
	pchar1(6)='d'
	pchar1(7)='a'
	pchar1(8)='s'
	pchar1(9)='e'
	pchar1(10)='c'
	pchar1(11)='t'
	pchar1(12)='q'
	pchar1(13)='m'
	pchar1(14)='o'
	pchar1(15)='w'
	pchar1(16)='h'

	text2(11)='1.Reroll   '
	text2(12)='2.Expand   '
	text2(13)='3.Fit 1 trs'
	text2(14)='4.Fit 2 trs'
	text2(15)='5.Fit 3 trs'
	text2(6)='6.Fit n trs'
	text2(7)='7.Fit all  '
	text2(8)='8.Help     '
	text2(9)='9.Contract '
	text2(10)='/\.Dec.Ampl'
	text2(1)='/.Inc/2    '
	text2(2)='*.Inc*2    '
	text2(3)='<-.Backward'
	text2(4)='->.Forwards'
	text2(5)='\/.Inc.Ampl'

	call CALC_PAR_1(iXIN1,iYIN1,N1,iXUP1,iYUP1,NR1)
	allocate (txtwin1 (ixin1:ixup1,iyin1:iyup1))
	allocate (txtwin2 (ixin2:ixup2,iyin2:iyup2))
	allocate (transfer (1:60000))

	open(unit=11,file='ioana7.dat',status='unknown',
     &	access='transparent')
	read(unit=11,rec=1) (transfer(itr),itr=1,40000)
	close(unit=11)
	itr=1
	do imx=ixin1,ixup1
		do jmx=iyin1,iyup1
			txtwin1(imx,jmx)=transfer(itr)
		itr=itr+1
		end do
	end do
c
	open(unit=11,file='ioana6.dat',status='unknown',
     &	access='transparent')
	read(unit=11,rec=1) transfer
	close(unit=11)
	itr=1
	do imx=ixin2,ixup2
		do jmx=iyin2,iyup2
			txtwin2(imx,jmx)=transfer(itr)
		itr=itr+1
		end do
	end do

	deallocate (transfer)
c Now allocate data array
	ALLOCATE(idata(1-novlap:nbuf+novlap))



c==================================================================
c	OTHER DEFINITIONS  AND  CONSTANTS
C==================================================================
	ipen=14           !COLOR FOR DATA
	ibk1=1           	!BACKGROUND COLOR for data area
	ibk=3           	!BACKGROUND COLOR for outside data area
	if(icol.eq.0) then
c	   ibk=15
	   ibk=0	!for monochrome
	   ibk1=0	!for monochrome
	endif

	ixstr=68          !X COORDINATE FOR WRITING THE SECTION
	iystr=23          !Y COORDINATE FOR WRITING THE SECTION

	ixmes=0           !X COORDINATE FOR WRITING THE MESAGES
	iymes=23          !Y COORDINATE FOR WRITING THE MESAGES
	ixfill=int(ixmes*8)
	iyfill=int(480-iymes*16)
	ixdat=0           !X COORDINATE FOR WRITING THE MESAGES
	iydat=22          !Y COORDINATE FOR WRITING THE MESAGES

	pch(1)='Section  1'
	pch(2)='Section  2'
	pch(3)='Section  3'
	pch(4)='Section  4'
	pch(5)='Section  5'
	pch(6)='Section  6'
	pch(7)='Section  7'
	pch(8)='Section  8'
	pch(9)='Section  9'
	pch(10)='Section 10'
	pch(11)='Section 11'
	pch(12)='Section 12'
	pch(13)='Section 13'
	pch(14)='Section 14'
	pch(15)='Section 15'
	pch(16)='Section 16'
	pch(17)='Section 17'
	pch(18)='Section 18'
	pch(19)='Section 19'
	pch(20)='Section 20'

c Enter graphics mode
	call MODE(18)		!clears screen
	icstr=11
C================================================================
C	ENTER THE DEFAULT SETTINGS
C================================================================
	ndisp=640    	!NDISPLAY
	ndisp1=ndisp	!normally, but ndisp1 smaller for truncated display
	incabs=160     	!absolute value of INCREMENT for rolling display
	inc=incabs     !'working' value   for increment (may be pos or neg)
	indfst=1        	!****ABSOLUTE INDEX for 1st point to display
	indlast=indfst+ndisp-1
	indfst0=indfst      !save its value
c SET BASELINE ,AMPLITUDE,THRESHOLD
c	iybar1=int(bdisp*480.)   !baseline
	iybar1=iyoff
	iybar2=128               !amplitude
	iybar3=250               !threshold
	icbar1=10                !color of baseline
	icbar3=12                !color of threshold
	icbar2=11                !color of amplitude line
c========fix XTRIG
c?	ixtrig=int(xtrig*640.)  !not used because I use Treshold
c========next bit from LGSAMP
cc Trigger found so recalc isfst to put trigger at req position on screen
cc -want idata(jt) to be 100*XTRIG% across screen, and redefine Ydisp to
cc achieve this.
c	isold=isfst		!for debug
c	isfst=jt-ifix(xtrig*float(ndisp))
c	islast=isfst+ndisp-1	!index in current section of last point on display
c Problem if isfst or islast not in the current section!!?
c========end of bit from LGSAMP
c
c Rest are defined in INSCAN
c	expfac=4.   	!EXPANSION FACTOR
c	bdisp=0.8   	!USE TO CALCULATE BASELINE POSITION :iYBAR1
c	xtrig=0.2   	!I DONT'T USE THIS NOW
c				!I DETECT THE OPENINGS WITH THE HELP OF
c				!THRESHOLD LINE :iYBAR3
c
c Draw box for default values
	call clears(20,479,3)	!screen blue
	l1=120
	l2=520
	call fillwin(l1,l2,80,440,0)	!black rectangle
	call hline(l1+2,l2-2,438,14)	!yellow frame
	call hline(l1+2,l2-2,82,14)
	call vline(l1+2,82,438,14)
	call vline(l2-2,82,438,14)
	call set_cursor(0,29)		!bottom of screen
	call det_string('< /\ >: move cursor up;',14,ch)
	call set_cursor(23,29)
	call det_string('< \/ >: move cursor down;',14,ch)
	call set_cursor(48,29)
	call det_string('< R >: reset;',14,ch)
	call set_cursor(63,29)
	call det_string('< C >: continue',14,ch)
	intrax=18
	intray=5
c
 	call set_cursor(30,3)		!yellow heading
	call put_string('DEFAULT  SETTING',icstr,14,ch)
c write defaults in the box
 	call set_cursor(intrax,intray)
	call put_string('Number of points on the screen       = 640',
     &			icstr,14,ch)
	intray=intray+2
  	call set_cursor(intrax,intray)
	call put_string('Increment for fast scrolling         = 160',
     &			icstr,14,ch)
	intray=intray+2
  	call set_cursor(intrax,intray)
	call put_string('Initial display expansion factor     = 4',
     &			icstr,14,ch)
	intray=intray+2
  	call set_cursor(intrax,intray)
	call put_string('Display baseline (percent of Y axis) = 0.80',
     &			icstr,14,ch)
	intray=intray+2
  	call set_cursor(intrax,intray)
	call put_string('Trigger position (percent of X axis) = 0.20',
     &			icstr,14,ch)

	intray=intray+2
  	call set_cursor(intrax,intray)
	call put_string('Data to start                        = 1',
     &			icstr,14,ch)

	ixf=450
	iyf=380
c Alter the defaults?
	call drawframe(ixf,ixf+60,iyf,iyf+25,14)	!box round first number
	intray=5
	iyf0=iyf
59	ch=getch(ktype)
	if(ktype.eq.2) ch=LC(ch)	!convert to lower case	-NOT for arrow keys!
	ivar=ichar(ch)
	if(ktype.eq.8.or.ktype.eq.2) then
		select case(ivar)
			case(80)		!cursor down
				call drawframe(ixf,ixf+60,iyf0,iyf0+25,0)
				if(iyf0.gt.220) then
					iyf0=iyf0-32
					intray=intray+2
				endif
				call drawframe(ixf,ixf+60,iyf0,iyf0+25,14)
				goto 59
			case(72)          ! cursor up
				call drawframe(ixf,ixf+60,iyf0,iyf0+25,0)
				if(iyf0.lt.iyf) then
					iyf0=iyf0+32
					intray=intray-2
				endif
				call drawframe(ixf,ixf+60,iyf0,iyf0+25,14)
				goto 59
			case (99)		!'c' = continue
				goto 559
		end select
	endif
	if(ktype.eq.2.and.ivar.eq.114) then
		call set_cursor(57,intray)
		read(*,*,err=59) rnumer
			if (iyf0.eq.iyf) then
				ndisp=int(rnumer)
c				if(ndisp.gt.2048) goto 711
			else if(iyf0.eq.iyf-32) then
				incabs=int(rnumer)
c				if(incabs.gt.ndisp) goto 711
			else if(iyf0.eq.iyf-64) then
				expfac=rnumer
c				if(expfac.gt.8) goto 711
			else if(iyf0.eq.iyf-96) then
				bdisp=rnumer
			else if(iyf0.eq.iyf-128) then
				xtrig=rnumer
			else if(iyf0.eq.iyf-160) then
				indfst=int(rnumer)
c				if(indfst.gt.ndat) goto 711
			endif
	endif
	goto 59
559	continue
c=================================================end of menus
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
c	iyframe=462     	      	!top edge of progress line
	iyframe=472    	      	!top edge of progress line
	icframe=0          		!prog line frame colour
	if(icol.eq.0) icframe=15	!monochrome screen
	icprog=15         		!PROGRESS LINE COLOR
	ixcr=0  			! X FOR RED CURSOR
	iycr=iyframe-6          ! Y FOR RED CURSOR
	iccr=13                 ! RED CURSOR COLOR
	ifabs0=1 + float(indfst0)/ibdpp
	ixmark=ifabs0-1         ! X MARKER
	iymark=iyframe+8        ! Y MARKER
	icmark=11               ! COLOR MARKER
	iprogx0=1			!starting X-coord (pixel #) for prog line
c end of progress line calcs
c
C========================================================
C	Having indfst, calc section #, and index (isdfst) within the section
C=====================================================

	call CLEARS(0,480,ibk)	!fill screen with light blue (for outside data area)
	call FILLWIN(0,640,90,390,ibk1)	!fill data area with colour ibk
	call HLINE(0,639,89,14)		!line to mark bottom of data area
	call HLINE(0,639,391,14)	!line to mark top of data area
	if(.not.sdone) then
c==	   call OPENDIALOG(irhi,iclo,nrow,ncol,icf,icb)
c	   call OPENDIALOG(2,2,3,60,14,0) !3 row dialog box top=row 2; cols 2-62
	   call OPENDIALOG(2,2,1,60,14,0) !1 row dialog box top=row 2; cols 2-62
	   call WDIALOG('GET BIT OF EMPTY BASELINE ON SCREEN FOR SD',12)
	   call BELL(2)
	endif
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
c	jfst=is+novlap    	!not needed now
c	print*,isdfst,jfst
c jfst no longer needed with AIVPIX0 (see above); just call with idata(isdfst)
c where isdfst=index, within current section, of first point to be displayed
c
c jump here for NEXT SECTION
333   continue
c READ IDATA AND SCALE INSIDE IREADSEC
	call READSEC(isec,idata,nrlast,nsec,ioff,nbuf,nread,novlap,
     & scalfac,iyoff,in1,in2,jn1,jn2,is1,is2,js1,js2)
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
c Jump to 32 to calc SD from roll mode (copy data to idata1())
32	if(ilabel.eq.10.and.(.not.sdone)) then
	   j=0
	   do i=isprev,isprev+ndisp1-1
		j=j+1
		idata1(j)=idata(i)
	   enddo
	   ndfit=ndisp1
	endif
c Jump to 31 to calc SD from expand mode
31	if(ilabel.eq.10.and.(.not.sdone)) then
	   call IVAR2(idata1,ndfit,ybase,vary)
	   rms=sqrt(vary)		!in ADC units
	   iscrit=IFIX(smult*rms)		!in ADC units
	   ibase=ibdisp	!BASELINE AFTER OFFSET BELOW
	   if(openup) ibase=-ibdisp
         print 33,rms*calfac,float(ibase)*calfac		!in pA
         if(pon()) write(7,33) rms*calfac,float(ibase)*calfac		!in pA
         if(discprt) write(8,33) rms*calfac,float(ibase)*calfac		!in pA
33	   format(' RMS baseline noise (pA)= ',g11.4,/,
     &	' Baseline set to (pA)= ',G13.6,/,' O.K. [Y] ? ')
	   read 101,ans
	   sdone=.true.
	   if(UC(ans).eq.'N') sdone=.false.	!carry on to do again
	endif
c
c Draw progress line (+marker, red cursor)
	call put_cursors(0,ixframe,iyframe,icframe,iprogx1,icprog,
	&		     ixcr,iycr,iccr,IXMARK,IYMARK,icmark,ibdx,ibk)
c	CALCULATE IX=X array for display (outside loop)
	dx=640.0/float(ndisp)		!use ndisp (not ndisp1) here
	do i=1,ndisp
		r=float(i)*dx
		ix(i)=ifix(r)
	end do
	ifirst=1		!no erase in AIVPIX1 first time (which resets it to 0)
c
c Do initial display so there is something on screen when prog stops (in
c scroll_bar) at initial menu. Don't increment indfst so same display repeated
c in loop when roll forward started (so first INC points are searched for trigger)
cc	call AIVPIX1(ix,idata,ndisp,jfst,inc,ipen,ifirst,ibk)
c	call AIVPIX0(ix,idata(isdfst),ndisp1,inc,ipen,ifirst,ibk)
	call AIVPIX3(ix,idata(isdfst),ndisp1,idata(isprev),
     &	ipen,ifirst,ibk1)
	isprev=isdfst	!record value in case needed in DELPIX
	ndprev=ndisp1	!ditto
cc debug! I think that ADCPIX does not work properly!
cc	call SETACT(iact,-1)
cc	iact(ipen)=ibk	!redraw points of colour ipen in background colour
cc	call ADCPIX(ix,idata(isprev),ndisp,iact)

	do i=0,639
		call rpixel(i,iybar1,iabar1(i))       !save colours to restore later
		call rpixel(i,iybar2,iabar2(i))
		call rpixel(i,iybar3,iabar3(i))
     	end do
	call hline(0,639,iybar1,icbar1)   !SET BASELINE -overwrites all cols
	call hline(0,639,iybar2,icbar2)   !SET AMPLITUDE
	call hline(0,639,iybar3,icbar3)   !SET THRESHOLD LINE

22	CONTINUE		!return here to REROLL
	kx=152
c write section number (?)
	call set_cursor(ixstr,iystr)
	call det_string(pch(isec),icstr,ch)
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
	call SET_MENU(iXIN1,iYIN1,iXUP1,iYUP1,TXTWIN1,iFWIN1,iCTXT1,
     &jX1,jY1,kX)
	nout1=1
C	MOVE BAR OR ENTER CHARACTER FROM KEYBOARD:
C	------------------------------------------
30	continue
			!JUMP FROM INCREASE/DECREASE SPEED,
			!MOVING BASELINE, AMPLITUDE,THRESHOLD
			!DISPLAY DATA, HELP
	call SCROLL_BAR(ixin1,iyin1,ixup1,iyup1,jx1,jy1,n1,pchar1,
     &txtwin1,ifwin1,ictxt1,nout1,achar,ktype,ivar)
c Waits in SCROLL_BAR for option
	call GETOPT(ixin1,iyin1,ixup1,iyup1,n1,pchar1,
     & txtwin1,ictxt1,nout1,achar,ibkwin1,inc,incabs,ndisp,
     & ixmes,iymes,ch,iybar1,icbar1,iabar1,ii,iybar2,icbar2,
     & iabar2,iybar3,icbar3,iabar3,ixfill,iyfill,ibk,ibk1,end,
     & stopped,backward,indfst,isdfst,isec,nbuf,sdone,
     & ixcr,iycr,iccr,ixframe,iyframe,ivl,ibdpp,ilabel)
	if(discprt) then	!debug
	   write(8,704) backward,ilabel
704	   format(' backward = ',l4,': ilabel = ',i3)
	endif
	if(ilabel.ne.0) then
	  goto(5,8,22,25,30,77,333,444,800,32) ilabel
	endif
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
	if (debug()) then
	   call set_cursor(ixdat,iydat-2)
	   print 700,indfst,isdfst,ndisp1,inc,isec,
     &	triggered,backward,stopped
	endif
c	call AIVPIX1(ix,idata,ndisp,jfst,inc,ipen,ifirst,ibk)
c	call AIVPIX0(ix,idata(isdfst),ndisp1,inc,ipen,ifirst,ibk)
	call AIVPIX3(ix,idata(isdfst),ndisp1,idata(isprev),
     &	ipen,ifirst,ibk1)
	isprev=isdfst	!record start value ready for erasing next time
	ndprev=ndisp1	!ditto
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
		if(idata(k).le.iybar3) then
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
c	Now you can stop manual the fast scrolling and to take further
c	action:go back,expand,move slowly,fit transitions,quit,etc
	if(kbhit()) then
		achar=getch(ktype)
	      if(ktype.eq.2) achar=LC(achar)  !convert to lower case-NOT for arrow keys!
		call GETOPT1(ixin1,iyin1,ixup1,iyup1,n1,pchar1,
     & 	txtwin1,ictxt1,nout1,achar,ibkwin1,inc,incabs,ndisp,
     & 	ibk,end,stopped,backward,ilabel)
		ndisp1=ndisp	!unless display truncated
		if (debug()) then
	      print 700,indfst,isdfst,ndisp1,inc,isec,
     &	  triggered,backward,stopped
700		format(' indfst=',i9,' isdfst,ndisp,inc,isec = ',4i7,/,
     &	  ' trig= ',l4,' back = ',l4,' stop= ',l4)
		endif
		if(ilabel.ne.0) then
	  	  goto(8,25,30,800) ilabel		!otherwise straight on
		endif
	endif
c
c Now increment for next display
c (NB lastdisp, fstdisp should never be used if novlap big enough -can be
c removed from loop somehow?)
	if(lastdisp.and.isec.lt.nsec) then
	   lastdisp=.false.	!reset
c now delete AFTER new section read in
c	   call DELPIX(ix,idata,ndprev,isprev,ipen,ibk,
c     &	  nbuf,novlap)
	   indfst=jn1next       !display from 1st point in next section
	   indlast=indfst+ndisp-1
	else if(fstdisp.and.isec.gt.1) then
	   fstdisp=.false.	!reset
c now delete AFTER new section read in
c	   call DELPIX(ix(ixoff),idata,ndprev,isprev,ipen,ibk,
c     &	  nbuf,novlap)
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
c therefore delete all points drawn last time (ndprev) now
		call DELPIX(ix,idata,ndprev,isprev,ipen,ibk1,
     &	 nbuf,novlap)
		ifirst=1	!so no delete in next AIVPIX
		ndisp1=jn2-indfst+1
c		call AIVPIX0(ix,idata(isdfst),ndisp1,inc,ipen,ifirst,ibk)
		call AIVPIX3(ix,idata(isdfst),ndisp1,idata(isprev),
     &	 ipen,ifirst,ibk1)
		isprev=isdfst	!record value in case needed in DELPIX
		ndprev=ndisp1	!ditto
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
c this by calling AIVPIX with ix(ixoff) where ixoff normally 1, but reset here
		call DELPIX(ix,idata,ndprev,isprev,ipen,ibk1,
     &	  nbuf,novlap)
		ifirst=1	!so no delete in next AIVPIX
		ndisp1=indlast-jn1+1
		ixoff=ndisp-ndisp1+1
c		call AIVPIX0(ix(ixoff),idata(is1),ndisp1,inc,
		call AIVPIX3(ix(ixoff),idata(is1),ndisp1,idata(isprev),
     &	 ipen,ifirst,ibk1)		!display from 1st point of section=is1
		isprev=is1		!record value in case needed in DELPIX
		ndprev=ndisp1	!ditto
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
c	call DELPIX(ix,idata,ndprev,isprev,ipen,ibk,nbuf,novlap)
c selective delete with ADCPIX does not seem to work!
c	call SETACT(iact,-1)
c	iact(ipen)=ibk	!redraw points of colour ipen in background colour
c	call ADCPIX(ix,idata(isprev),ndisp,iact)
	ifirst=1
c Read the new section
	call READSEC(isec,idata,nrlast,nsec,ioff,nbuf,nread,novlap,
     & scalfac,iyoff,in1,in2,jn1,jn2,is1,is2,js1,js2)
	call set_cursor(ixstr,iystr)
	call det_string(pch(isec),icstr,ch)

c Now delete previous display AFTER new data read in (note: nbuf, novlap in
c call to DELPIX replaced with 2048, 0 i.e. dimensions of idata1())
c (ixoff normally =1, but needed for deletion after FSTDISP when
c rollong backwards)
	call DELPIX(ix(ixoff),idata1,ndprev,1,ipen,ibk1,2048,0)
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
	ipmax=1		!needed for delete in NEWPROGLINE
	call EXPMODE(ixin2,iyin2,ixup2,iyup2,text2,
     & TXTWIN2,iFWIN2,iCTXT2,n2,IDATA,isdfst,indfst,ndisp1,
     & nsec,iybar1,icbar1,iybar2,icbar2,ibk1,
     & usepots,expfac,js1,js2,isec,pch,nbuf,novlap,
     & fitmode,idata1,ndfit)
c Last 3 params are returned with fitmode=true when section on screen is
c to be fitted; idata1(1) to idata1(ndfit) contain data to be fitted in FITSUB
c	call DRAW(ix,idata,ndisp,jfst,ipen,ipen,ibk)
c	do i=0,639
c		call rpixel(i,iybar1,iabar1(i))
c		call rpixel(i,iybar2,iabar2(i))
c		call rpixel(i,iybar3,iabar3(i))
c     	end do
c	call hline(1,639,iybar1,icbar1)
c	call hline(1,639,iybar2,icbar2)
c	call hline(1,639,iybar3,icbar3)
	if(fitmode) then
	   call FITSUB(idata1,ndfit)
	else
c	   goto 22	!REROLL
	   indfst=indsav		!restore value before expmode
	   isdfst=isdsav		!restore value before expmode
	   ndisp1=ndsav
	   indlast=indfst+ndisp1-1	!restore
	   isdlast=isdfst+ndisp1-1	!restore
c restore progress line to what it was before EXPMODE
	   iprogx1=ip1sav	!restore progress line position
	   iprogx2=ip2sav	!restore progress line position
	   call NEWPROGLINE()	!restore prog line
	   ifirst=1	!prev display erased by fillwin in EXPMODE
	   if(.not.sdone) then
		ilabel=10
		goto 31
	   endif
	   goto 5	!REROLL (goto 5 to do 1st display there, not above)
	endif
c
c QUIT NOW
c======is set_menu needed here?
8	call SET_MENU(iXIN1,iYIN1,iXUP1,iYUP1,TXTWIN1,iFWIN1,iCTXT1,
     &  iX1,iY1,kX)
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
	deallocate(txtwin1,txtwin2,text1,text2,pchar1,pchar2,idata)
c
c999	call mode(3)
c	end
999	continue
	if(VIDEOTYP().ne.3) call VIDEOMOD(3)
	call ENDPRINT
	end

