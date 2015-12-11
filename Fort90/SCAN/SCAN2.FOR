	program SCAN
c Single channel time course fitting (and threshold crossing too?)
c 03/01/93 03:37pm Lahey V5.x version
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
c	in = is + (isec-1)*nbuf
c Conversely point with index=IN in the whole data set occurs in section
c	isec = 1 + (in-1)/nbuf
c and has index, within this section,
c	is = in - (isec-1)*nbuf
c Display starts at point with index is=isfst in current section so point
c with index=ID on display (1st displayed point=1) has index in section of
c	is = id + isfst - 1
c Similarly incurs,iscurs,idcurs= index in whole array, section, display
c respectively of the data point nearest to the cursor.
c
	integer*2 lrow,int2,videotyp
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
	logical disp,rescale,auto,forward,end
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
c============================================================
	integer*4	iabar1(0:641),iabar2(0:641),iabar3(0:641)

c=======old declarations (many integer*2)
c	integer*2	txtwin1 (:,:), txtwin2(:,:)
c	integer*2	transfer(:)
c	integer*2	ctxt1,ctxt2,idrap,iady
c	integer*2 	xin1,yin1,xup1,yup1,fwin1
c	integer*2 	xin2,yin2,xup2,yup2,fwin2
c	integer*2  	n1,n2,nout1
c	integer*2	abar1(0:641),ybar1,cbar1,abar2(0:641),ybar2,cbar2
c	integer*2	abar3(0:641),ybar3,cbar3
c	integer*2	kx,ibk,ret,att,intrax,intray,ktype,flag_pline
c	integer*2   ix1,ix2,icstr
c	integer*2 	idt,first,ndisp,iyoff	!NB ndisp is integer*2 here
c==========check all other integer*2
c	integer*2 	ix(2048),isec,iax(100)
c	integer*2	minim(50),maxim(50),ixmin(50),ixmax(50)
c	integer*4 	ioff,ilen,jfst,ndata,nr,nlast
c====end of old declarations
c
	character*1		getch,achar,ch,ans,UC
	character*1  	pchar1(:),pchar2(:)
	character*10	pch(12)
	character*18 	text1(:)
	character*11 	text2(:)
	logical 		kbhit
c end of IV declarations
	common/dp/discprt
	common/dpp/filnam,machine,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
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
1	FORMAT(' SCAN: Time course fitting of sigle channels',/)
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
	lastrow=2			!NB args of SCROLL=integer
	lrow=int2(lastrow)	!but args of LOCATE are int*2
	xtrig=0.2	!trigger pos 20% of way across screen
	call SETATTR(112)		!'inverse' attrib seems nec while in graphics
	nsetup=0	!so asks for step response function in INSCAN
c
	call INSCAN(iampl,tint,ntmax,nbuf,srate,calfac,finter,tsect,
     & ioff,ndat,nsec,nrlast,namp,totamp,iampsv,ibassv,ktrans,nwrit,
     & nwsav,nfits,newpar,igback,restart,openup,invert,disp,smult,ntrig,
     & dgain,iboff,rms,
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
	yd1=130.	!lowest point on screen (Y pixels) for display of channels
	yd2=450.	!highest point on screen (Y pixels) for display of channels
	scalfac=(yd2-yd1)/65536.
	scalfac=scalfac*dgain
	iyoff=ifixr(yd1 + bdisp*(yd2-yd1))	!baseline position (in Y pixels)
c	iyzero=		!abs zero of current irrelevant as offset for sampling
c
c Allocate the data array
c	novlap=8192		!length of overlap extensions	===in calling prog
	novlap=0		!length of overlap extensions
	ALLOCATE(idata(1-novlap:nbuf+novlap))
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
c	read(unit=11,fmt=101) (transfer(itr),itr=1,40000)
c101	format(i2)
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

c==================================================================
c	OTHER DEFINITIONS  AND  CONSTANTS
C==================================================================
	ipen=14           !COLOR FOR DATA
	ibk=9           	!BACKGROUND COLOR

	ixstr=68          !X COORDINATE FOR WRITING THE SECTION
	iystr=23          !Y COORDINATE FOR WRITING THE SECTION

	ixmes=300          !X COORDINATE FOR WRITING THE MESAGES
	iymes=0           !Y COORDINATE FOR WRITING THE MESAGES

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
c
	call mode(18)
	icstr=11
C================================================================
C	ENTER THE DEFAULT SETTINGS
C================================================================
	call clears(0,480,3)

	ix1=120
	ix2=520
	call fillwin(ix1,ix2,80,440,0)
	call hline(ix1+2,ix2-2,438,14)
	call hline(ix1+2,ix2-2,82,14)
	call vline(ix1+2,82,438,14)
	call vline(ix2-2,82,438,14)

	call set_cursor(0,0)
	print*,'TOTAL NUMBER OF INT*2 VALUES IN DATA',':',ndat

 	call set_cursor(30,3)
	call put_string('DEFAULT  SETTING',icstr,14,ch)

	intrax=18
	intray=5
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
	call set_cursor(50,28)
	call det_string('press Enter to continue',14,ch)
	ch=getch(ktype)
	call clears(0,480,ibk)

c=================================================end of menus
c initialise for ADCs
	do i=0,15
	    ichan(i)=0
	    iadc(i)=0
	enddo
	ichan(0)=0
	nadc=1	!read channel 0 only

	ndisp=640    	!NDISPLAY
	inc=160     	!INCREMENT
c Rest are defined in INSCAN
c	expfac=4.    	!EXPANSION FACTOR
c	bdisp=0.8   	!USE TO CALCULATE BASELINE POSITION :YBAR1
c	xtrig=0.2   	!I DONT'T USE THIS NOW
c===================start with inabs=1 for now
c Define starting point in INSCAN too
C*******************************************************************
	inabs=1        	!index in whole file of first point on screen
c********************************************************************
c SET BASELINE ,AMPLITUDE,THRESHOLD
c======??? iybar1?
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
	inabs0=inabs      !STORE THE RECORD YOU BEGAN WITH
c======isec0 not used??
c	isec0=1           !STORE INITIAL SECTION

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
	ibdx=ifix(640/nsec)     ! pixels per section for prog line
	ibdpp=nbuf/ibdx		! data points per pixel on progress line
	ibproc=ifixr(float(ibdx*nrlast)/float(nbuf))  !pixels for last section
	ixframe=ibdx*(nsec-1)+ibproc	!total pixels inc last section
	iyframe=462     	      	!Y FRAME
	icframe=0          		!FRAME COLOR
	icprog=15         		!PROGRESS LINE COLOR
	ixcr=0  			! X FOR RED CURSOR
	iycr=iyframe-6          ! Y FOR RED CURSOR
	iccr=13                 ! RED CURSOR COLOR
	ifabs0=1 + float(inabs0)/ibdpp
	ixmark=ifabs0-1         ! X MARKER
	iymark=iyframe+8        ! Y MARKER
	icmark=11               ! COLOR MARKER
	iprogx0=1			!starting X-coord (pixel #) for prog line
c end of progress line calcs
c
C========================================================
C	HAVING INABS, CALCULATE SECTION AND index (is)
C=====================================================
444	CONTINUE	!return here after 'go to red cursor'
c The point with index=IS, idata(is), in section number ISEC has index
c	in = is + (isec-1)*nbuf
c Conversely point with index=IN in the whole data set occurs in section
c	isec = 1 + (in-1)/nbuf
c and has index, within this section,
c	is = in - (isec-1)*nbuf
	isec=1 + (inabs-1)/nbuf     	!section # for point #inabs
	ins1=1 + (isec-1)*nbuf		!abs index of first point in section
c	icsec=isec*nbuf			!abs index of last point in section
	ins2=isec*nbuf			!abs index of last point in section
c	rdata=float(inabs)-float(nbuf)*float(isec-1)
c	ndata=int(rdata)        ! ****INDEX FOR DATA IN THE SECTION*****
	is=inabs - (isec-1)*nbuf	!index WITHIN section of point #inabs
c	print*,is
C***********************************************************************
c=======check next bit
c If following condition is true here, display crosses section boundary,
c so goto next section and use its initial overlap part to do display:
c	if(inabs.gt.(icsec-ndisp).and.inabs.lt.icsec) then
	if(inabs.gt.(ins2-ndisp).and.inabs.lt.ins2) then
		is=is-nbuf
		isec=isec+1
	endif         !CONDITION FOR THE LIMIT BETWEEN SECTIONS
	jfst=is+novlap    	!***INDEX OF ASSEMBLY ARRAY***
c========why '+novlap'? -can't use negative index in assembler
c	print*,is,jfst
c=======end of bit to check
c
c Now read section #isec and scale it
c
c Jump to 333 for 'next section'
333   continue
c =================================================================
c	READ IDATA AND SCALE INSIDE IREADSEC
c==================================================================
	call READSEC(isec,idata,nrlast,nsec,ioff,nbuf,nr,novlap,
     & scalfac,iyoff)
c Parameters for progress line (move_prog_line in IOLIB) (NB only upper line
c moves backwards -call with flag_pline=0 to move both lines, and flag_pline=1
c to move upper line only):
c	ibdpp=nbuf/ibdx		! data points per pixel on progress line
c (iprogx1,iprogx2 were prev isecf,isec1)
	iprogx1=1 + inabs/ibdpp	!current X coord (# of pixels) for upper prog line
	iprogx2=iprogx1			!ditto for lower line (does not move back)
c
c
	forward=.true.
	end=.false.
c	att=0 	!THIS IS A VERY IMPORTANT FLAG FOR FAST SCROLLING:
			! 0 IF YOU GO FORWARDS
			! 1 IF YOU GO BACKWARDS
			! IT IS INITIATED TO 0
c	att2=0 	!THIS IS A VERY IMPORTANT FLAG FOR LAST SECTION:
			! STOP YOU TO INCREASE JFIST
			! 0 NORMAL
			! 1 IF YOU REACHED THE END OF FILE
			! IT IS INITIATED TO 0

5	continue    !jump here when NDISP changed
c
c	DRAW MARKER,FRAME,PROGRESS LINE,RED CURSOR
C
	call put_cursors(0,ixframe,iyframe,icframe,iprogx1,icprog,
	&		     ixcr,iycr,iccr,IXMARK,IYMARK,icmark,ibdx,ibk)

c
c	CALCULATE IX=X array for display
c=======is this outside the loop?
	dx=640.0/float(ndisp)
	do i=1,ndisp
		r=float(i)*dx
		ix(i)=ifix(r)
	end do
	ifirst=1		!no erase in AIVPIX1 first time (which resets it to 0)
C=================================================================
C	START SCROLLING:
C ===============================================================
c NB jfst is integer*4, all others are integer*2
	call AIVPIX1(ix,idata,ndisp,jfst,inc,ipen,ifirst,ibk)

	do i=0,639
		call rpixel(i,iybar1,iabar1(i))       !save colours to restore later
		call rpixel(i,iybar2,iabar2(i))
		call rpixel(i,iybar3,iabar3(i))
     	end do
	call hline(0,639,iybar1,icbar1)   !SET BASELINE -overwrites all cols
	call hline(0,639,iybar2,icbar2)   !SET AMPLITUDE
	call hline(0,639,iybar3,icbar3)   !SET THRESHOLD LINE

22    CONTINUE		!return here to REROLL
	kx=152

c****
	call READAMP(ichan,nadc,iadc0)
25	continue
			!LOOP WHEN YOU STOP FROM BACKWARDS LOOP
			!STOP (MANUAL/AUTOMATICALLY) FROM FORWARDS LOOP
			!get to END of data
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
	call MENU_OPTIONS(nout1,achar,pchar1,iret)

	select case(iret)

		case(1) 			!Roll on
			call set_cursor(0,14)
c			if(att2.eq.1.) goto 30
			if(end) goto 30
			call fillwin(16,624,yin1,yup1,bkwin1)
			call fillwin(320,472,yin1,yup1,3)
			call fillwin(16,320,yin1,yin1+16,3)
			call fillwin(16,168,yup1-16,yup1,15)
			call write_text(txtwin1,xin1,yin1,xup1,yup1,ctxt1)
			call set_cursor(0,14)
c	     		if(att.eq.1) goto 7
	     		if(.not.forward) goto 7

		case(2)		!Increase speed
			if(inc.lt.(ndisp-16)) then
				inc=inc+16
			endif
			goto 30

		case(3)		!Baseline
			call set_cursor(200,0)
			call det_string('<Enter>=continue; \/=down; /\=up'
     &		,14,ch)
			call move_line(iybar1,icbar1,iabar1,ii)
			call fillwin(200,640,463,480,ibk)
			goto 30


		case(4)		!Next section
c			att2=0
			end=.false.
c===========surely COULD be at end??
			isec=isec+1
c=======why 'fillwin'??
			call fillwin(0,640,90,480,ibk)
			is=1
c			inabs=is + (isec-1)*nbuf
			inabs=1 + (isec-1)*nbuf
c========'+novlap'??
			jfst=is+novlap
			goto 333

		case(5)		!Go back
			call fillwin(16,624,yin1,yup1,bkwin1)
			call fillwin(320,472,yin1,yup1,3)
			call fillwin(16,320,yin1,yin1+16,3)
			call fillwin(16,168,yin1+32,yin1+48,15)
			call write_text(txtwin1,xin1,yin1,xup1,yup1,ctxt1)
c			att2=0
			end=.false.
			goto 654

		case(6)		!Decrease speed
			if(inc.ge.32) then
				inc=inc-16
			endif
			goto 30

		case(7)	!Amplitude line
			call set_cursor(200,0)
			call det_string('<Enter>=continue; \/=down; /\=up'
     &		,14,ch)
			call move_line(iybar2,icbar2,iabar2,ii)
			call fillwin(200,640,463,480,ibk)
			goto 30

		case(8)		!Stop
			goto 25


		case(9)		!Expand=screen2
			call screen2(ixin2,iyin2,ixup2,iyup2,text2,
     &				TXTWIN2,iFWIN2,iCTXT2,n2,
     &				IDATA,is,ndisp,inabs,nrlast,nsec,
     &				iybar1,icbar1,iybar2,icbar2,ibk,
     &				expfac,isec,pch,0,
     &				nbuf,novlap)

			call draw(ix,idata,ndisp,jfst,ipen,ipen,ibk)
			do i=0,639
				call rpixel(i,iybar1,iabar1(i))
				call rpixel(i,iybar2,iabar2(i))
				call rpixel(i,iybar3,iabar3(i))
     			end do
			call hline(0,639,iybar1,icbar1)
			call hline(0,639,iybar2,icbar2)
			call hline(0,639,iybar3,icbar3)
			goto 22

		case(10)		!Change ndisplay
c			att2=0
			end=.false.
707			continue
			call fillwin (ixmes,ixmes+300,464,480,0)
			call set_cursor(ixmes,iymes)
			call det_string('Ndispl = [100 to 2048] :',
	&		icstr,ch)
			read(*,*,err=707) ndisp
			if(ndisp.lt.100.or.ndisp.gt.2048) then
				goto 707
			endif
			call fillwin(0,640,80,450,ibk)
			call fillwin(ixmes,ixmes+300,464,480,ibk)
			goto 5

		case(11)		!Threshold
			call set_cursor(200,0)
			call det_string('<Enter>=continue; \/=down; /\=up'
     &		,14,ch)
			call move_line(iybar3,icbar3,iabar3,ii)
			call fillwin(200,640,463,480,ibk)
			goto 30

		case(12)		!Quit
			goto 8

		case(13)          !Move red cursor
c			att2=0
			end=.false.
			CALL move_red_cursor(ixcr,iycr,iccr,ixframe,ibk)
c			rivl=float(inabs)*vbabs
c			ivl=ifix(rivl)
			ivl=inabs/ibdpp	!x coord of inabs on prog line (=pixel #)
						! to draw vert line to mark orig position
			inabs=ixcr*ibdpp	!new inabs
			call fillwin(0,640,90,480,ibk)
			call vline(ivl,iyFRAME,iyFRAME+7,0)	!mark old inabs
			goto 444

		case(14)  		!Other  file
c======option to change data file removed
		case(15)       		!Window
			call screen2(ixin2,iyin2,ixup2,iyup2,text2,
     &				TXTWIN2,iFWIN2,iCTXT2,n2,
     &				IDATA,is,ndisp,inabs,nrlast,nsec,
     &				iybar1,icbar1,iybar2,cbar2,ibk,
     &				expfac,isec,pch,ixin2-8,
     &				nbuf,novlap)

			call draw(ix,idata,ndisp,jfst,ipen,ipen,ibk)
			do i=0,639
				call rpixel(i,iybar1,iabar1(i))
				call rpixel(i,iybar2,iabar2(i))
				call rpixel(i,iybar3,iabar3(i))
     			end do
			call hline(0,639,iybar1,icbar1)
			call hline(0,639,iybar2,icbar2)
			call hline(0,639,iybar3,icbar3)
			goto 22


		case(16)    		!Help

			call first_help(ibk)
			goto 30

	end select

	is=is+inc
	inabs=inabs+inc
      jfst=jfst+inc
	jlast=jfst+ndisp-1

c**************************************************************************
c     SCROLL DATA FORWARDS:
c==========================================================================
7	continue
	forward=.true.
	end=.false.
c	att=0
c	att2=0.
777   CONTINUE
c	nlast=98304
	nlast=nbuf+novlap
c	if(isec.eq.nsec) nlast=nrlast+8192+inc  !+inc.....?
	if(isec.eq.nsec) nlast=nrlast+novlap+inc  !+inc.....?
c	ibdx=ifix(640/nsec)     ! pixels per section for prog line
	ipxs=1 + (isec-1)*ibdx	!x-coord (pixel #) for 1st progline pixel in isec
c==	seci=(float(isec)-1)*bdx+1
c==	iseci=int(seci)
77	do while(jlast.le.nlast)
c draw next trace
	 	call aivpix1(ix,idata,ndisp,jfst,inc,ipen,ifirst,ibk)
c recalc progress line
		iprogx1=ipxs + is/ibdpp	!current X coord (pixel #) for upper prog line
		flag_pline=0	!move both lines
		call move_progress_line(iprogx0,iprogx1,iprogx2,
     &		iyframe,icprog,ibk,flag_pline)
		iprogx0=iprogx1
		iprogx2=iprogx1
c write current section # on screen
		call set_cursor(ixstr,iystr)
		call det_string(pch(isec),icstr,ch)
c
c Look for threshold crossing
		do i=is,is+inc
			if(idata(i).le.iybar3) goto 25
		end do
c Control via POTS
		call READAMP(ichan,nadc,iadc)
		idt0=int4(iadc(0)-iadc0(0))
		inc=inc + idt0/10
		if(inc.lt.4) inc=4
		iadc0(0)=iadc(0)
		call LOCATE(3,0)
		print *,inc
c Control via keys
c	Now you can stop manual the fast scrolling and to take further
c	action:go back,expand,move slowly,fit transitions,quit,etc
		if(kbhit()) then
		   achar=getch(ktype)
		   call MENU_OPTIONS(nout1,achar,pchar1,iret)
		   select case(iret)
			case(1)  		!Roll on
			case(2)		!Increase speed
			  if(inc.lt.(ndisp-16)) then
			  	inc=inc+16
			  endif
			case(3)
			case(4)
			case(5)           !Go back
			   call fillwin(16,624,yin1,yup1,bkwin1)
			   call fillwin(320,472,yin1,yup1,3)
			   call fillwin(16,320,yin1,yin1+16,3)
			   call fillwin(16,168,yin1+32,yin1+48,15)
			   call write_text(txtwin1,xin1,yin1,xup1,
     &		   		yup1,ctxt1)
c			   att2=0.
			   end=.false.
			   goto 654
			case(6)        	!Decrease speed
			   if(inc.ge.32) then
			   	inc=inc-16
			   endif
			case(7)
			case(8)           !Stop
c			   call hline(0,639,iybar1,9)
c			   call hline(0,639,iybar2,9)
			   goto 25
			case(9)		!Expand=screen2
			   call screen2(ixin2,iyin2,ixup2,iyup2,text2,
     &		       TXTWIN2,iFWIN2,iCTXT2,n2,
     &			 IDATA,is,ndisp,inabs,nrlast,nsec,
     &			 iybar1,icbar1,iybar2,icbar2,ibk,
     &			 expfac,isec,pch,0,
     &			nbuf,novlap)
			   call draw(ix,idata,ndisp,jfst,ipen,ipen,ibk)
			   call draw(ix,idata,ndisp,jfst,ipen,ipen,ibk)
			   call hline(1,639,iybar1,icbar1)
			   call hline(1,639,iybar2,icbar2)
			   call hline(1,639,iybar3,icbar3)
			   goto 22
			case(10)
			case(11)
			case(12)		!quit
			   goto 8
			case(13)
			case(14)
			case(15)         !Window
			   call screen2(ixin2,iyin2,ixup2,iyup2,text2,
     &		     TXTWIN2,iFWIN2,iCTXT2,n2,
     &		     IDATA,is,ndisp,inabs,nrlast,nsec,
     &		     iybar1,icbar1,iybar2,icbar2,ibk,
     &		     expfac,isec,pch,ixin2-8,nbuf,novlap)
			   call draw(ix,idata,ndisp,jfst,ipen,ipen,ibk)
			   call draw(ix,idata,ndisp,jfst,ipen,ipen,ibk)
			   call hline(1,639,iybar1,icbar1)
			   call hline(1,639,iybar2,icbar2)
			   call hline(1,639,iybar3,icbar3)
			   goto 22
			case(16)          !help
			   call first_help(ibk)
		   end select
		endif		!end of 'if kbhit()'
c
	 	is=is+inc
		inabs=inabs+inc
 	      jfst=jfst+inc
	 	jlast=jfst+ndisp-1
		call set_cursor(ixdat,iydat)
		if(debug()) then
		   print*,ndat,' ',inabs,' ',is,' ',jfst,' ',jlast
		endif
	end do
c End of 'do while' loop (starts at label 77) to roll through one section
c When end of section reached, read in the next one
	if(isec.lt.nsec) then
		isec=isec+1
		call READSEC(isec,idata,nrlast,nsec,ioff,nbuf,nr,novlap,
     & 	 scalfac,iyoff)
c		OPEN(unit=14,file=filename(ifile),status='UNKNOWN',
c     & 	access='TRANSPARENT')
cc		call IREADSEC(isec,idata,nrlast,nsec,ioff,nbuf,nr,iady,ady)
c		call newREAD(isec,idata,nrlast,nsec,ioff,nbuf,
c     &		novlap,iady,ady)
c		CLOSE(unit=14)
c		jfst=jfst-90112
c		is=jfst-8192
		jfst=jfst-nbuf
		is=jfst-novlap
		jlast=jfst+ndisp-1
		goto 777		!roll forwards
	endif
	if(isec.eq.nsec.and.jlast.gt.nlast) then
		is=is-inc
		inabs=inabs-inc
 		jfst=jfst-inc
	 	jlast=jfst+ndisp-1
		end=.true.
c		att2=1.
	endif
	goto  25
c**********************************************************************


c**********************************************************************
c     SCROLL DATA BACKWARDS
C	-==============================================================
654 	continue
c	isecn=iprogx1  !========no refs to isecn!!
	end=.false.
c	att2=0.
6	continue
c===========is next bit OK?
	if(isec.eq.1) then
		iconst=novlap+1
	else
		iconst=1
	endif
c define values for moving lower progress line:
c==	seci=(float(isec)-1)*bdx+1
c==	iseci=int(seci)
c	ibdx=ifix(640/nsec)     ! pixels per section for prog line
	ipxs=1 + (isec-1)*ibdx	!x-coord (pixel #) for 1st progline pixel in isec
c
c Now 'do while' to roll backwards WITHIN current section
	do while(jfst.ge.(iconst+inc))
C=========================================================
c	MOVE LOWER PROGRESS LINE ,THE ONE THAT GO BACKWARDS:
C=========================================================
	   flag_pline=1
	   call move_progress_line(iprogx0,iprogx1,iprogx2,
     &	 iyframe,icprog,ibk,flag_pline)
C=========================================================
	   iprogx0=iprogx1
c========should next line be BEFORE call move_prog_line?
	   iprogx1=ipxs + is/ibdpp	!current X coord (pixel #) for upper prog line
	   if(jfst.lt.8193) then
		call set_cursor(ixstr,iystr)
		call det_string(pch(isec-1),icstr,ch)
	   else
		call set_cursor(ixstr,iystr)
		call det_string(pch(isec),icstr,ch)
	   endif
	   call drawpix(ix,idata,ndisp,is,inc,ipen,ibk)
	   istore=is-inc
	   if(kbhit()) then
	      achar=getch(ktype)
		call MENU_OPTIONS(nout1,achar,pchar1,iret)
		select case(iret)
		   case(1)
		   	call fillwin(16,624,yin1,yup1,bkwin1)
		   	call fillwin(320,624,yin1,yup1,3)
		   	call fillwin(16,268,yin1,yin1+16,3)
		   	call fillwin(16,168,yup1-16,yup1,15)
		   	call write_text(txtwin1,xin1,yin1,xup1,
     &	   		yup1,ctxt1)
			goto 7
		   case(2)
		   	if(inc.lt.(ndisp-16)) then
		   	   inc=inc+16
			endif
			call delpix(ix,idata,ndisp,istore,ipen,ibk)
		   case(3)
		   case(4)
		   case(5)
		   case(6)
			if(inc.ge.32) then
				inc=inc-16
			endif
			call delpix(ix,idata,ndisp,istore,ipen,ibk)
		   case(7)
		   case(8)
c			att=1
			forward=.false.
			goto 25
		   case(9)         !screen2
			call screen2(ixin2,iyin2,ixup2,iyup2,text2,
     &		 TXTWIN2,iFWIN2,iCTXT2,n2,
     &		 IDATA,is,ndisp,inabs,nrlast,nsec,
     &		 iybar1,icbar1,iybar2,icbar2,ibk,
     &		 expfac,isec,pch,0,nbuf,novlap)
			jfst=jfst-inc
			call draw(ix,idata,ndisp,jfst,ipen,ipen,ibk)
			call hline(1,639,iybar1,icbar1)
			call hline(1,639,iybar2,icbar2)
			call hline(1,639,iybar3,icbar3)
			goto 22
		   case(10)
		   case(11)
		   case(12)
			goto 8
		   case(13)
		   case(14)
		   case(15)
			call screen2(ixin2,iyin2,ixup2,iyup2,text2,
     &		TXTWIN2,iFWIN2,iCTXT2,n2,
     &		IDATA,is,ndisp,inabs,nrlast,nsec,
     &		iybar1,icbar1,iybar2,icbar2,ibk,
     &		expfac,isec,pch,ixin2-8,nbuf,novlap)
			jfst=jfst-inc
			call draw(ix,idata,ndisp,jfst,ipen,ipen,ibk)
			call hline(1,639,iybar1,icbar1)
			call hline(1,639,iybar2,icbar2)
			call hline(1,639,iybar3,icbar3)
			goto 22
		   case(16)
		end select
	   endif		!end of 'if(kbhit())' options
c decrement indices to roll backwards
	   is=is-inc
	   inabs=inabs-inc
 	   jfst=jfst-inc
c check for threshold crossing
555	   do i=is+inc,is+inc+inc
		   if(idata(i).le.iybar3) goto 25
	   end do
	   call set_cursor(ixdat,iydat)
	   if(debug()) then
		print*,ndat,' ',inabs,' ',is,' ',jfst,' ',jlast
	   endif
	end do
c end of 'do while' loop to roll backwards within one section
c
c Now read in PREVIOUS section to continue backwards
	if(isec.gt.1) then
	     	jfst=jfst+nbuf
	     	is=jfst-novlap
	     	jlast=jfst+ndisp-1
		isec=isec-1
		call READSEC(isec,idata,nrlast,nsec,ioff,nbuf,nr,novlap,
     & 	 scalfac,iyoff)
c		OPEN(unit=14,file=filename(ifile),status='UNKNOWN',
c     & 		access='TRANSPARENT')
cc		call IREADSEC(isec,idata,nrlast,nsec,ioff,nbuf,nr,iady,ady)
c		call newREAD(isec,idata,nrlast,nsec,ioff,nbuf,
c     &		novlap,iady,ady)
c		CLOSE(unit=14)
		goto 6	!return to roll-backwards
	endif
	goto 25		!if isec=1??
c
c===============================================================
C	LAST IMAGE ON SCREEN:
c===============================================================
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

