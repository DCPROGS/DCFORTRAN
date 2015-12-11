	program AUTPLOT
c
c LaheyF90/Gino program to
c (a) plot graphs automatically from queue (PLOTQ.DAT)
c (b) make fancy individual plots
c idev=0: screen
c idev=1: .wmf
c idev=2: .cgm
c idev=3,4: postscript printer
c idev=5  : laserjet
c idev=6  : color deskjet
c
c Modified 10/25/97 05:20pm for single channels queued from SCAN
c     1000 added to abs(iptype) if SCAN is analysing jump data (from
c	a cjump.dat file, rather then consam.dat). -when this is the
c	case the 1000 is removed at once, and set cjdat=true (NB cjump
c	is false in this case -cjump=true only when plotting all jumps
c	automatically ffrom the cjump.dat file, rather than when reading
c	from a plotq.dat).  When reading a single channel record from
c	plotq.dat, the data may be either from consam.dat (cjdat=false)
c	or from cjump.dat (cjdat=true).
c Modified 07/02/97 09:30am so that vertical position of jump logo (y1c,y2c)
c is given in y units (e.g. pA) not screen units. (Values read from old F77
c queues are converted to pA in RDVPLQ)
c
c Version number specified as iptype, thus
c IPTYPE=1 for VPLOT        -obsolete
c	  =11 for VPLOTR        -obsolete
c	  =12 for VPLOT2, VPLOT3        -obsolete
c	  =14 for VPLOT4 -record read in different order!
c	  =15 for VPLOT5
c	  =2 for VHIST4 and below
c	  =21 for VHIST5
c	  =3 for SC when position in CONSAM is queued
c	  =-3 for SC when data itself queued
c	  =31/-31 for SC when data for fitted curve also queued
c	  =32/-32 for SC when data for fitted curve also queued using extra record
c       =40 3d plot
c For version that allows for filtered data
c IPTYPE=30 for SC when position in CONSAM is queued
c	  =-30 for SC when data itself queued
c	  =301/-301 for SC when data for fitted curve also queued
c	  =302/-302 for SC when data for fitted curve also queued using extra record
c Lahey V5.x version 02/15/93 11:34am
c
c Uses VPLOT4 now, with allocatable data arrays. (prev call AUTPLOT4.FOR)
c
c Auto plotting of single channels added 08/24/94 06:38pm Note that this
c is for data queued from SCAN, which has minimal detail -if this data
c is shown with POSH option, and queued again from VPLOT, it becomes
c a 'graph', not a 'single channel' queue item.
c
c QFILE increased in size to *33, to hold whole path for plotq file name
c Now uses VPLOT3 03/16/94 11:02am
c and VHIST3 (03/24/94 10:19am) NB VHIST3 now has Xcal, Ycal arrays of size
c 2048, like VHIST3, so no need for separate Xcal,Xcal1 now.
c Modified 08/12/92 02:18pm so jstrec(), lstrec() both have dimension=200
c (must be read and written in two sections for compatibility with old files)
c Modified 05/29/92 12:45pm for non-consecutive and/or rearrange plots
c and mpos=5 introduced to denote whole page plots.  Possibility of
c rearrangement means that, for quarter page plots,  mpos=1 no longer
c necessarilly signals first plot on new page, and mpos=4 may not signal
c that page is full. But for quarter page plots still always have 4 plots
c per page, and kplot=total number if plots to be done, in the loop
c do 100 ip=1,kplot. Thus mod(ip,4)=1, i.e. ip=1,5,9,... always indicates
c the first plot on a new page, and ip=4,8,12,... (or ip=kplot) always
c indicates the last plot on a page, i.e. mod(ip,4)=0 or ip=kplot.
c
c Modified 03/10/92 09:07am to plot all graphs with VPLOT2 (in 'single
c plot' mode), whether they were queued from VPLOT1, VPLOTR or VPLOT2
c (iptype=1,11,12 resp).
c
c Modified 07/10/91 03:35pm to do plots from VPLOTR with more than 2048
c  points. Array ifstcol(10) added to queue to specify column of Yval(i,j)
c  where data starts (this is essentially ICURVD but latter is not queued)
c  This needs modifs to RDVPLQ and VPLOTQ which now have ifstcol as param.
c
c Modified 04/13/91 07:25pm: CJUMP plots added  (and renamed AUTPLOT)
c   (array called ISTREC in CJUMP called IRECFST here because ISTREC used
c	extensively here)
c   Can plot only the raw current records from CJUMP (no subtracted records
c   or IV plots, which must be calc in CJFIT)
c
c The array jstrec(200) contains in jstrec(nplot) the start record
c for plot #nplot, and in lstrec(nplot) the last record number for it
c
	integer*2 jstrec(200),lstrec(200)		!for all
	logical plotcirc,novlogo,noclogo,joinline,mono,interp
	logical arrange,consec,colplotter,useconsam,noconsam,present
	logical consamdef,usecons	!see note after vplot3 in single channel sec
	logical scplots,allocated
	logical filtered
      integer jplot(200),jpos(200)    !to hold re-ordered plots
	integer*4 irecfst(1000)		!now use int*4 version
	logical cjump,newform
	character cdate*11,ctime*11,ndev*2,adctime*8
	character filnam*32,prtport*4		!for winprint
	character cdate1*11,adctim1*8		!read from consam
	character adcfil*33,adcf1*30,path*40	!path for consam file
      character*1 ans,UC,getch
	character*33 oldfile,printfil
	character*11 kpchar
	real*4 stepamp(50),dt(49),filt(640)
	real*4 ycalc(5120)
	real*4 amark(10)
	logical cjdat
	ALLOCATABLE:: icurvd,icurvc
	integer icurvd(:),icurvc(:)
c For VHIST
	real XVAL(0:511,10),YVAL(0:511,10)
	dimension ndat(10),ncal(10),ijoin(10),iline(10)	!for VHIST
c Arrays for Vplot
	ALLOCATABLE:: XVAL1,YVAL1,XCAL,YCAL
	real XVAL1(:,:),YVAL1(:,:)		!for VPLOT
	real XCAL(:,:),YCAL(:,:)		!for VPLOT/VHIST
	ALLOCATABLE:: weight,icurvw
	real*4 weight(:,:)
	integer*4 icurvw(:)
c Allocate other arrays for VPLOT
	ALLOCATABLE:: ndat1,isym1,ijoin1,syms1
	ALLOCATABLE:: ncal1,iline1
	integer ndat1(:),isym1(:),ijoin1(:)
	integer ncal1(:),iline1(:)
	real syms1(:)				!for data
	ALLOCATABLE:: theta
	real theta(:)
	character title1*64,qfile*33,titlex*40,titley*40,title*76
	logical debug,caplock,pon,slock,errflag,plotcols
	logical auto,draft,screen,quarter,doframe,autplt,landscap,fitted
c Arrays to hold all details of posh plots
	real*4 rlth(100) 		!for line thickness
	real RX(100),RY(100),angle(100)
	real rxbox(4,100),rybox(4,100)
	integer IJUS(100)
	integer idraw(100),icol(100)
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25)
	character*80 newtext(20)		!extra text
	integer ifnt(30)
	real size(30)
	real xb(10),yb(10),xe(10),ye(10)		!up to 10 arrows
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	integer iltype(10)				!line type for ditto
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
c new arrays for multiple trace plots
c for VPLOT:
c	integer ifstcol(10)	!for more than 2048 points/plot
	logical ivplot
c For cjump
      character cdate2*11,ctime2*11
	character title2*79
	logical vjump,sampv,control
c
c
	logical student,cluster
	logical mouse_on
	common/mousval/mouse_on,nbutton
c Common block to increase raster vector buffer size for Hgraph
c Maxnum (=2nd dimension of vect) set to 8192 below (rather than default
c which is 1024)- reduces disc work making temp files (eg 05158741.n) which
c have to be made for every maxnum vectors to be plotted (do not alter
c curnum or 1st dimension of VECT (=6))
	integer*2 maxnum,curnum,vect(6,8192)
	integer*2 videotyp
	logical logx,logy
	logical discprt
	character*40 mtitle		!machine name
      logical fill,inter,axes,fillbad,cross,posneg
      INTEGER ISTYLE(6),ISUP(6)
	allocatable xval3,yval3,z3,bad,zeta1,zeta2
	real*4 z3(:,:)
	real*4 xval3(:),yval3(:),zeta1(:)
	logical bad(:,:),zeta2(:)
	character*75 xtitle,ytitle,ztitle,title3
	COMMON/CHVBLK/maxnum,curnum,vect
	common/dpp/filnam,prtport,ndisc,jcol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c Define common/TPOS for VPLOT1 to hold data on text position etc from
c queue, for posh plots
	COMMON/TPOS/rx,ry,rxbox,rybox,ijus,angle,idraw,
     & ifnt,size,rlth,thbig,narrow,xb,yb,xe,ye,
     & nline,xlb,xle,ylb,yle,iltype,ntext,newtext,
     & cnumx,cnumy,cexpx,cexpy,numbx,numby,
     & nhline,yhline,xhlb,xhle,ilhtype,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel
	common/plot3d/ifnt3,ifnl,alfa,beta,gama,delta,
     &     ijust,ijustx,ijusty,ijustz,
     &     xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,numx,numy,
     &     xmin3,xmax3,ymin3,ymax3,zmin3,zmax3,xtic3,ytic3,ztic3,
     &     ratio,rat,radius,theta3,phi,ifram,numaxi,nwidth,npow,nplace,
     &     nxstep,nystep,istyle,isup,
     &     fill,inter,axes,fillbad,autplt,cross
	COMMON/cols/icol,mono
	common/dp/discprt
c
	COMMON/mtrace/ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil
	COMMON/JLOGOS/t1c(10),t2c(10),t1v(10),t2v(10),xoff1,y1v,y2v,
     & y1c,y2c                    !for VPLOT
	common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi
	common/user/student,cluster,iwindows
c
	pon()=slock()
	debug()=caplock()
c
c Use F90 routine to prevenet underflow crashes??
	errflag=.true.
	call UNDFL(errflag)
	plotcols=.true.
c
c
	irecl=128
101	format(a1)
      OPEN(unit=10,file='ERROR.GIN')
	call errdev(10)
	cluster=.false.
	student=.false.
	call MYPATH(path)
	if(path(1:8).eq.'O:\CVFIT') cluster=.true.
	call SETMOUSE()		!define values in common\mousval\ (in IVLIB)
	call GINO
	call errdev(10)
	call vga
	call mode(3)
      filnam='AUTPLOT.PRT'
	call WINPRINT	!print file control
      OPEN(unit=7,file=prtport,iostat=nerr)
	print 102
	if(pon()) write(7,102)
	if(discprt) write(8,102)
102	format(' AUTPLOT- Automatic plotting program',/)
	call GETSPEC('autplot.exe')	!print details
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 1,cdate,ctime(1:8),mtitle
	if(pon()) write(7,1) cdate,ctime(1:8),mtitle
	if(discprt) write(8,1) cdate,ctime(1:8),mtitle
1	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 399
399	format(
     & ' SCROLL LOCK on for printing',/,
     & ' CAPS LOCK on for debugging',/)
c
c Get input
	idiskq=-1		!for poshplots; until disk defined
120   continue                !return here for another run
	call INAUT(nplot,jstrec,lstrec,QFILE,auto,id,ndisc,
     & screen,draft,colplotter,quarter,cjump,irecfst,mono,idiskq,
     & adcfil,ioffset,newform,iver)
	if(id.eq.-1) goto 99	!abort after discnum1
	consamdef=.false.
c Define default colours here according to how mono was set in INAUT
	arrange=.false.			! default for quarter p. plot rearrangement
	idelt=1			!plot every point
	iscalfix=0		!don't override xmin,...
      call PLOTOPT(cjump,plotcirc,novlogo,noclogo,joinline,jtype,
     & arrange,consec,quarter,nplot,kplot,jplot,jpos,n1,n2,
     & idelt,iscalfix,Xmin,Xmax,Ymin,Ymax)
	if(n1.eq.0) goto 999
c
	irecl=1024		!for plotq
	if(.not.cjump) then
	   irecl=1024		!for plotq
         OPEN(unit=11,file=QFILE,status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=irecl)
	else
	   if(newform) then
		OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &		 access='TRANSPARENT')
	   else
		irecl=512
      	OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &	    access='DIRECT',form='UNFORMATTED',recl=irecl)
	   endif
	   goto 103		!straight to plot loop
	endif
C NOTE: RECSIZE=512 real =1024 integers= 4 BLOCKS  = 2048 bytes
c The array jstrec(200) contains in jstrec(nplot) the start record
c for plot #nplot, and in lstrec(nplot) the last record number for it
c
c For auto-plotting must first look at all the plots to see whether
c any of them is a single channel plot which needs CONSAM; if so, get the
c path for the CONSAM file (assumed to be same for all queued plots).
c Also need to know if there are any single channel plots, so can
c request addition of baseline/amplitude marker line
	scplots=.false.	!set true if there is at least on SC plot to be done
	do ip=1,kplot	!go through all plots
	   iplot=jplot(ip)		!plot # in queue
	   istrec=(jstrec(iplot))	!1st record no for plot # iplot
	   read(11,rec=istrec) iptype
	   j=iabs(iptype)
c 10/25/97 05:41pm If 1000 was added to iptype to indicate data is ffrom
c cjump.dat, then make sure it is removed before plotting
	   if(iptype.lt.0) then
	 	isign=-1
	    else
	 	isign=1
	   endif
	   if(j.gt.1000.and.j.lt.2000) then
		j=j-1000
		cjdat=.true.
	   else
		cjdat=.false.
	   endif
	   iptype=j*isign		!1000 removed
c end of modif
c Modif 04/04/97 09:59am
	   if(j.eq.3.or.j.eq.31.or.j.eq.32.or.
     &	j.eq.30.or.j.eq.301.or.j.eq.302) then
	      useconsam=iptype.gt.0	!= -3,-31 or -32 if SC data itself is queued
		scplots=.true.
		filtered=j.eq.30.or.j.eq.301.or.j.eq.302
	   endif
c      Now getconsam also gets path for cjump.dat, if cjdat is true
	   if(useconsam) then	!get CONSAM path here
		call READSCQ(krn,istrec,iptype,useconsam,itit,title1,
     &	 ndev,ib,istart,iend,srate,ymin,ymax,calfac,ioff,calfac2,
     &	 cdate,adctime,adcf1,isdfst,ndisp1,nrec1,jdat,base,
     &	 ntrans,y0,t0,nfilt,dtf,tif1,tif2,dt,stepamp,filt,
     &	 ndelt,ffilt,fcz,fczoom,njump)
		if(adcf1(2:2).eq.':') adcfil(1:30)=adcf1	!path for consam in queue
		call GETCONS(adcfil,noconsam,title,cdate1,adctim1,nsam,
     &     srate1,cjdat,irecfst,newform,ioffset,cfac,fflt,idest)
c		if(srate.ne.srate1) then
		if(.not.cjdat.and.(srate.ne.srate1)) then	!srate1 not def for cjdat
		   call BELL(1)
		   print 34,srate,srate1
34		   format(' ERROR: srate, srate1 = ',2g13.6)
		endif
		consamdef=.true.
		if(idest.eq.91) goto 91
		if(idest.eq.99) goto 99
	   endif
	enddo
91	continue
c
c  Override options for single channels
	if(scplots.and.auto) then
	   call SCOPT(iptype,nampmark,amark)
	   if(nampmark.gt.0) then
      	if(pon()) write(7,4)
	      if(discprt) write(8,4)
4		format(' Single channel plots have lines as follows:')
		do i=1,nampmark
	         if(pon()) write(7,5) amark(i)
	         if(discprt) write(8,5) amark(i)
5		   format(' Line at ',f9.3,' pA')
		enddo
	   endif
	endif
c
	iptype=iptsav		!restore orig value
c
c START PLOT LOOP HERE
c
103	continue
c	do 100 nplot=n1,n2
	do 100 ip=1,kplot	!go through all plots
	 iplot=jplot(ip)		!plot # in queue
	 mpos=jpos(ip)		!position for this plot
c
c Things to go in 1st record of each plot
c IPTYPE=1 for VPLOT
c	  =11 for VPLOTR
c	  =12 for VPLOT2, VPLOT3
c	  =14 for VPLOT4 -record read in different order!
c	  =15 for VPLOT5 -record read in very different order!
c	  =2 for VHIST
c	  =21 for VHIST5
c	  =3 for SC when position in CONSAM is queued
c	  =-3 for SC when data itself queued
c	  =31/-31 for SC when data for fitted curve also queued
c	  =32/-32 for SC when data for fitted curve also queued using extra record
c       =40,41,42 3d
c ITIT=0 for no title
c ITIT=1 for title
	if(cjump) goto 31			!data from CJUMP.DAT
c Make sure that QFILE is opened with fixed record length to get iptype
c for the current plot
	CLOSE(unit=11)
      OPEN(unit=11,file=QFILE,status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=irecl)
	istrec=(jstrec(iplot))	!1st record no for plot # iplot
	nrect=(lstrec(iplot))-istrec+1		!total number of records
	nrect=nrect-2				!number of records for Xval etc
	read(11,rec=istrec) iptype
c 10/25/97 05:41pm If 1000 was added to iptype to indicate data is ffrom
c cjump.dat, then make sure it is removed before plotting
c Also remove sign from iptype now
	iptsav=iptype		!to restore orig value
	j=iabs(iptype)
	if(j.gt.1000.and.j.lt.2000) then
	   j=j-1000
	   cjdat=.true.
	else
	   cjdat=.false.
	endif
c end of modif
c Modif 04/04/97 09:59am
	if(j.eq.3.or.j.eq.31.or.j.eq.32.or.
     &  j.eq.30.or.j.eq.301.or.j.eq.302) then
	    useconsam=iptype.gt.0	!= -3,-31 or -32 if SC data itself is queued
	    filtered=j.eq.30.or.j.eq.301.or.j.eq.302
	    iptype=j
	endif
	usecons=useconsam	   !for readscq1 (see notes after vplot3 in SC section
c
	if(iptype.ge.11.and.iptype.le.15) goto 31	!vplot
	if(iptype.eq.21) goto 32	!vhist
	if(iptype.eq.31.or.iptype.eq.32.or.iptype.eq.30.or.
     &   iptype.eq.301.or.iptype.eq.302) goto 33	!single channel
	if(iptype.eq.40.or.iptype.eq.41.or.iptype.eq.42) goto 40 ! 3d plot
	goto(31,32,33) iptype
	print *,' Invalid iptype'
	stop		!if invalid iptype found
c
c
c=============================================================
c 	Graphs- from VPLOT1, VPLOTR, VPLOT2 or VPLOT3
31	continue
c Read for VPLOT
c Logical PLOT replaced by DOFRAME=true if frame to be drawn
c Read data from queue:
c Modif 06/23/97 11:06am to read as far as ncurvd, ncurvc, to cope
c with huge files which may cause problems if ndimd is unnecessarilly big
	if(.not.cjump) then
	   if(iptype.eq.14) then
		read(11,rec=istrec) iptype,ndv1,ndimd,ndc1,ndimc
		kwi=100	!dimensions for weight()
		kwj=10
		kmax=20
		if(ndv1.eq.0) ndv1=1
		if(ndimd.eq.0) ndimd=1
		if(ndc1.eq.0) ndc1=1
		if(ndimc.eq.0) ndimc=1
	   else if(iptype.eq.15) then
		if(iver.lt.1100) then
		   read(11,rec=istrec) iptype,ndv1,ndimd,ndc1,ndimc,kwi,kwj,kmax,
     &	  itit,title1(1:44),xmin,xmax,ymin,ymax,xcross,ycross,
     & 	  xtic,ytic,ixlo,ixhi,iylo,iyhi,csize,ifont,ilog,iscal,doframe,
     &	  titlex,titley,ilabel,ncurvd,ncurvc,inumx
		  xlo=float(ixlo)
		  xhi=float(ixhi)
		  ylo=float(iylo)
		  yhi=float(iyhi)
		else
		  read(11,rec=istrec)iptype,ndv1,ndimd,ndc1,ndimc,kwi,kwj,
     &	  kmax,itit,title1,xmin,xmax,ymin,ymax,xcross,ycross,
     &        xtic,ytic,xlo,xhi,ylo,yhi,ifont,ilog,iscal,doframe,
     &        titlex,titley,ilabel,ncurvd,ncurvc
		endif
		if(ndimd.gt.ncurvd) ndimd=ncurvd
		if(ndimc.gt.ncurvc) ndimc=ncurvc
	   else
	      ndv1=2048	!dimensions as for earlier versions
		ndimd=10
		ndc1=2048
		ndimc=10
		kwi=100	!dimensions for weight()
		kwj=10
		kmax=20
	   endif
c============================================================
	   if(allocated(xval1)) then
		DEALLOCATE(Xval1,Yval1)
	   endif
	   if(allocated(xcal)) then
		DEALLOCATE(Xcal,Ycal)
	   endif
	   if(allocated(weight)) then
		DEALLOCATE(weight,icurvw)
	   endif
	   if(allocated(theta)) then
		DEALLOCATE(theta)
	   endif
	   if(allocated(ndat1)) then
	     DEALLOCATE(ndat1,isym1,ijoin1,syms1)
	   endif
	   if(allocated(ncal1)) then
	     DEALLOCATE(ncal1,iline1)
	   endif
	   if(allocated(icurvd)) then
	     DEALLOCATE(icurvd,icurvc)
	   endif
	   if(ndv1.le.0) ndv1=1		!in case no data curves
	   if(ndimd.le.0) ndimd=1		!in case no data curves
	   if(ndc1.le.0) ndc1=1		!in case no calc curves
	   if(ndimc.le.0) ndimc=1		!in case no calc curves
	   if(kwi.le.0) kwi=1
	   if(kwj.le.0) kwj=1
	   ALLOCATE(Xval1(ndv1,ndimd),Yval1(ndv1,ndimd),
     &	Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
	   ALLOCATE(weight(kwi,kwj),icurvw(ndimd))
	   if(kmax.lt.1) kmax=1		!must allocate theta whether used or not
	   ALLOCATE(theta(kmax))
	   ALLOCATE(ndat1(ndimd),isym1(ndimd),
     &    ijoin1(ndimd),syms1(ndimd))
	   ALLOCATE(ncal1(ndimc),iline1(ndimc))
	   ALLOCATE(icurvd(ndimd),icurvc(ndimc))
c Zero all these arrays -they may contain out of range vakues that
c cause trouble in subroutine calls
	   do j=1,ndimd
		ndat1(j)=0
		isym1(j)=0
		syms1(j)=0.0
		ijoin1(j)=0
		icurvw(j)=0
	      do i=1,ndv1
		   xval1(i,j)=0.0
		   yval1(i,j)=0.0
		enddo
	   enddo
	   do j=1,ndimc
		ncal1(j)=0
		iline1(j)=0
		icurvc(j)=0
	      do i=1,ndc1
		   xcal(i,j)=0.0
		   ycal(i,j)=0.0
		enddo
	   enddo
c
	   krn=istrec
c Up to this point, have read PLOTQ as though it was always in 1024 byte
c records, but for
	   if(iptype.ge.15.and.iptype.le.25) then
		CLOSE(unit=11)
            OPEN(unit=11,file=QFILE,status='UNKNOWN',
     &		access='TRANSPARENT')
c		istrec=1 + int4(jstrec(iplot)-1)*1024  !start rec when transparent
c NB argument of int4() must NOT be an expression
		istrec=1 + (int4(jstrec(iplot))-1)*1024  !start rec when transparent
	   endif
	   call RDVPLQ(istrec,xval1,yval1,xcal,ycal,ndimd,ndimc,ncurvd,
     &   ndat1,isym1,ijoin1,ncurvc,ncal1,iline1,syms1,xmin1,xmax1,ymin1,
     &   ymax1,
     &   xcross,ycross,xtic,ytic,ntx,nty,itx,ity,xlo,xhi,ylo,yhi,
     &   itit,title1,ifont,ilog,iscal,doframe,titlex,titley,ilabel,
     &   inumx,inumy,sval,theta,ifitype,ncomp,isdev,weight,y0,yinf,
     &   iptype,ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,
     &   ivplot,interp,screen,colplotter,
     &   itrace,ntrace,ytsep,ndv1,ndc1,
     &   kwi,kwj,icurvw,kmax,iver)
c
	   if(iscalfix.eq.1) then !do not alter xmin,.. defined in PLOTOPT (but define tic)
		call FIXAX(xmin,xmax,x1,x2,xtic,0) !calc tic based on specified xmin,max
		ymin=ymin1
		ymax=ymax1
		xcross=xmin
	   else if(iscalfix.eq.2) then
		call FIXAX(ymin,ymax,x1,x2,ytic,0)
		xmin=xmin1
		xmax=xmax1
		ycross=ymin
	   else if(iscalfix.eq.3) then
		call FIXAX(xmin,xmax,x1,x2,xtic,0) !calc tic based on specified xmin,max
		call FIXAX(ymin,ymax,x1,x2,ytic,0)
		xcross=xmin
		ycross=ymin
	   else if(iscalfix.eq.0) then
		xmin=xmin1
		xmax=xmax1
		ymin=ymin1
		ymax=ymax1
	   endif
	   if(iptype.eq.1) then
		ncjump=0             !no jumps in VPLOT
		nvjump=0
	   else if(iptype.eq.11) then
		isdev=-1             !no SD in VPLOT
	   endif
	   if((iptype.eq.1.or.iptype.eq.11).and.ncurvc.gt.0) then
	 	do i=1,ncurvc
	 	   if(iline1(i).gt.0) iline1(i)=iline1(i)-1
		enddo
	   endif
	else if(cjump) then
	   iptype=11		!use vplot
	   istrec=irecfst(iplot)
c 	   alloc/dealloc yval1 etc for each plot; read nsamp here to get size
c        to allocate Yval (all cjump versions are same up to nsamp)
	   read(14,rec=istrec) cdate2,ctime2,title2,naver,navc,iav,
     & 	control,vjump,sampv,nsamp
	   ndv1=nsamp
	   ndimd=1
	   ndc1=1
	   ndimc=1
	   if(allocated(xval1)) then
		DEALLOCATE(Xval1,Yval1)
	   endif
	   if(allocated(xcal)) then
		DEALLOCATE(Xcal,Ycal)
	   endif
	   if(allocated(weight)) then
		DEALLOCATE(weight,icurvw)
	   endif
	   if(allocated(theta)) then
		DEALLOCATE(theta)
	   endif
	   if(allocated(ndat1)) then
	     DEALLOCATE(ndat1,isym1,ijoin1,syms1)
	   endif
	   if(allocated(ncal1)) then
	     DEALLOCATE(ncal1,iline1)
	   endif
	   if(allocated(icurvd)) then
	     DEALLOCATE(icurvd,icurvc)
	   endif
	   ALLOCATE(Xval1(ndv1,ndimd),Yval1(ndv1,ndimd),
     &	Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
	   kwi=1
	   kwj=1
	   ALLOCATE(weight(kwi,kwj),icurvw(ndimd))
	   icurvw(1)=-1		!now weights for cjump data
	   if(kmax.lt.1) kmax=1		!must allocate theta whether used or not
	   ALLOCATE(theta(kmax))
	   ALLOCATE(ndat1(ndimd),isym1(ndimd),
     &    ijoin1(ndimd),syms1(ndimd))
	   ALLOCATE(ncal1(ndimc),iline1(ndimc))
	   ALLOCATE(icurvd(ndimd),icurvc(ndimc))
c
	   call RDCJDAT(istrec,xval1,yval1,xcal,ycal,ndimd,ndimc,
     &   xmin,xmax,ymin,ymax,xtic,ytic,xcross,ycross,landscap,
     &   ncurvd,ndat1,isym1,ijoin1,ncurvc,ncal,iline1,syms1,
     &   ntx,nty,itx,ity,xlo,iask,itit,title1,csize,ifont,ilog,iscal,
     &   doframe,titlex,titley,ilabel,ifitype,ncjump,t1c,t2c,nvjump,
     &   t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,iplot,screen,
     &   ndv1,ndc1,iscalfix,ioffset,newform)
	endif
c
1122	continue
c=================================================================
c DO POSH PLOT IF NOT AUTO
c=================================================================
	if(.not.auto) then
c 	   Can now define ICURVD, needed for VPLOT simply as:
	   if(ncurvd.gt.0) then
		do 11 i=1,ncurvd
11		icurvd(i)=i
	   endif
	   if(ncurvc.gt.0) then
		do 12 i=1,ncurvc
12		icurvc(i)=i
	   endif
	   iscal=0		!use input xmin etc	!added 10/27/97 06:17pm
c        For cjump data have no queued details, so scale internally
	   if(cjump) then
		autplt=.false.      ! chaged from false
		iscal=1
		isdev=-1
		itrace=0
	   endif
	   cbig=csize		!from queue
	   thbig=1.0		!in common for line thickness
c
c;;;;;	isetcol=0		!for VPLOT
	   autplt=.true.	!called from AUTPLT
	   logx=ilog.eq.1.or.ilog.eq.3.or.ilog.eq.4.or.ilog.eq.6
	   logy=ilog.eq.2.or.ilog.eq.3
	   if(.not.logx) inumx=-1	!see notes in LAXES2
	   if(.not.logy) inumy=-1	!see notes in LAXES2
	   call VPLOT5(xval1,yval1,ndat1,icurvd,ncurvd,ijoin1,syms1,ndimd,
     &   xcal,ycal,ncal1,icurvc,ncurvc,iline1,ndimc,isym1,ilog,iscal,
     &   xmin,xmax,ymin,ymax,xtic,ytic,xcross,ycross,ntx,nty,itx,ity,
     &   XLO,XHI,YLO,YHI,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     &   titlex,titley,ilabel,doframe,idiskq,autplt,draft,itit,title1,
     &   cbig,ifont,landscap,fitted,-2,theta,ifitype,ncomp,interp,
     &   isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kmax,iver)
	   goto 100		!end of POSH plotting
	endif
c
c=======================================================================
c 		AUTOMATIC PLOTTING
c=======================================================================

c If option selected (in PLOTOPT) to rearrange plots, then do it here

	   call PLOTAUT(draft,screen,quarter,ifont,csize,thbig,idev,
     &   mpos,ip,iptype) 	!sets 'thick'=thbig in common for VPLOTQ
	   if(.not.screen) print 1003,ip,iplot,mpos,title1
	   if(pon()) write(7,1003) ip,iplot,mpos,title1
         if(discprt) write(8,1003) ip,iplot,mpos,title1
1003	   format(/,1x,i5,': Plot no ',i5,' (position ',i2,')',/,
     &	2x,a44)
	   if(plotcirc) then
	      do i=1,ncurvd
	         if(isym1(i).eq.0) then
		      isym1(i)=-7		!filled circle
		      syms1(i)=0.7
	         endif
	      enddo
	   endif
	   if(noclogo) y1c=-10000.
	   if(novlogo) y1v=-10000.
	   if(joinline) then
	      do i=1,ncurvd
	         ijoin1(i)=jtype
	  	enddo
	   endif
c 	   For cjump plots VPLOTQ sets position of jump logos (at present they
c 	   are normally in abs screen units so those for bottom row of 1/4 page
c	   plots come out at top of screen!)
	   logx=ilog.eq.1.or.ilog.eq.3.or.ilog.eq.4.or.ilog.eq.6
	   logy=ilog.eq.2.or.ilog.eq.3
	   if(.not.logx) inumx=1	!see notes in LAXES2
	   if(.not.logy) inumy=1	!see notes in LAXES2
	   autplt=.false.
	   plotcols=.true.
	   if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) then
		mono=.true.
	   else
		mono=.false.
	   endif
	   call setcls(mono,icol,autplt,plotcols,isetcol)
	   call VPLOTQ5(XVAL1,YVAL1,ndat1,ncurvd,ijoin1,syms1,ndv1,ndimd,
     &   xcal,ycal,ncal1,ncurvc,iline1,ndc1,ndimc,isym1,ilog,
     &   XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     &   y0,yinf,titlex,titley,ilabel,
     &   doframe,draft,itit,title1,csize,ifont,
     &   theta,ifitype,ncomp,weight,isdev,quarter,iptype,
     &   ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ivplot,
     &   cjump,idev,interp,iplot,idelt,
     &   kwi,kwj,icurvw,kmax,iver)
c
	   autplt=.true.
	   goto 90		!page full yet?
c
c===============================================================
c 				HISTOGRAMS
c===============================================================

32	continue
c     At present xval, yval are fixed dimension, but must allocate Xcal,Ycal
	if(iptype.eq.2) then
	   ndv1=2048	!dimensions as for earlier versions
	   ndimd=10
	   ndc1=2048
	   ndimc=10
	   kmax=20	!for theta
	else if(iptype.eq.21) then
	   read(11,rec=istrec) iptype,ndimd,ndv1,ndimc,ndc1,kmax
	   ndimd=10		!still fixed=use queud value when alloc
	   ndimc=10		!still fixed=use queud value when alloc
	endif
c
	if(allocated(xval1)) then
	   DEALLOCATE(Xval1,Yval1)
	endif
	if(allocated(theta)) then
	   DEALLOCATE(theta)
	endif
	if(allocated(xcal)) then
	   DEALLOCATE(Xcal,Ycal)
	endif
	if(allocated(icurvd)) then
	   DEALLOCATE(icurvd,icurvc)
	endif
	ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
	ALLOCATE(theta(kmax))
	ALLOCATE(icurvd(ndimd),icurvc(ndimc))
      read(11,rec=1) nplot,jstrec,LSTREC,iver
	if(iver.eq.1100) then
		CLOSE(unit=11)
            OPEN(unit=11,file=QFILE,status='UNKNOWN',
     &		access='TRANSPARENT')
c		istrec=1 + int4(jstrec(iplot)-1)*1024  !start rec when transparent
c NB argument of int4() must NOT be an expression
		istrec=1 + (int4(jstrec(iplot))-1)*1024  !start rec when transparent
	   endif
	krn=istrec
	call RDVHSQ(istrec,xval,yval,xcal,ycal,ndimd,ndimc,ncurvd,
     & ndat,ijoin,ncurvc,ncal,iline,xmin1,xmax1,ymin1,ymax1,
     & xcross,ycross,xtic,ytic,ntx,nty,itx,ity,xlo,xhi,ylo,yhi,
     & itit,title1,ifont,ilog,iscal,doframe,titlex,titley,ilabel,
     & inumx,inumy,theta,ifitype,ncomp,sval,isval,xwbase,lt2,
     & screen,colplotter,iptype,ndv1,ndc1,kmax,iver,idev)
c	ndimd=10		!still fixed=use queued value when alloc
c	ndimc=10		!still fixed=use queued value when alloc
c
	lt2=0		!now lo, hi bins are filled
	   if(iscalfix.eq.1) then !do not alter xmin,.. defined in PLOTOPT (but define tic)
		call FIXAX(xmin,xmax,x1,x2,xtic,0) !calc tic based on specified xmin,max
		ymin=ymin1
		ymax=ymax1
		xcross=xmin
	   else if(iscalfix.eq.2) then
		call FIXAX(ymin,ymax,x1,x2,ytic,0)
		xmin=xmin1
		xmax=xmax1
		ycross=ymin
	   else if(iscalfix.eq.3) then
		call FIXAX(xmin,xmax,x1,x2,xtic,0) !calc tic based on specified xmin,max
		call FIXAX(ymin,ymax,x1,x2,ytic,0)
		xcross=xmin
		ycross=ymin
	   else if(iscalfix.eq.0) then
c	===NB do not touch xmin,etc if iscalfix=0 (done for jumps only, and this
c	=== is done in RDCJDAT)
c		call FIXAX(xmin1,xmax1,xmin,xmax,xtic,0)
c		call FIXAX(ymin1,ymax1,ymin,ymax,ytic,0)
c		xmax=xmax1		!reset in case FIXAX increases it
		xmin=xmin1
		xmax=xmax1
		ymin=ymin1
		ymax=ymax1
	   endif
c=	   xcross=xmin
c=	   ycross=ymin
	if(debug()) then
	   print 321,ifitype
321	   format(' ifitype = ',i4,'  O.K. [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
		print 322
322		format('&ifitype = ')
		read *,ifitype
	   endif
	endif
c========================================================
c 		DO POSH PLOT IF NOT AUTO
c========================================================
	if(.not.auto) then
c 		Can now define ICURVD, needed for VHIST simply as:
	   if(ncurvd.gt.0) then
		do 111 i=1,ncurvd
111		icurvd(i)=i
	   endif
	   if(ncurvc.gt.0) then
		do 112 i=1,ncurvc
112		icurvc(i)=i
	   endif
c
c VIDEOTYP()=18 for graphics mode, =3 for alpha mode
	   if(videotyp().ne.18) call VGA
	   iscal=0		!use input xmin etc
c Input values of ixlo etc may be for 1/4 page plot.  To do posh plots
c need whole page so set xlo=-1 (need to keep input values?)
c	   xlo=-1		!NO- used queued values (so LANDSCAP ignored)
c If data from an old queue the landscape format would be ixlo etc= 1400,9500
c 1600,6500 rather than the smaller format now used, so alter then to
c the present landscape default size

	  call papenq(xp,yp,ipap)
	  if(xlo.eq.0.14*xp) then
		xlo=0.13*xp		! set
		xhi=0.81*xp		! display
		ylo=0.2*yp		! location
		yhi=0.76*yp		! screen
	   endif

	   autplt=.true.	!called from AUTPLT
	   cbig=csize		!from queue
	   thbig=1.0		!in common for line thickness
c
	   logx=ilog.eq.1.or.ilog.eq.3.or.ilog.eq.4.or.ilog.eq.6
	   logy=ilog.eq.2.or.ilog.eq.3
	   if(.not.logx) inumx=1	!see notes in LAXES2
	   if(.not.logy) inumy=1	!see notes in LAXES2
c
	autplt=.true.
	isetcol=0
	call VHIST5(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,xwbase,lt2,inumx,inumy,
     & titlex,titley,ilabel,doframe,idiskq,
     & autplt,draft,itit,title1,cbig,ifont,landscap,fitted,
     & theta,ifitype,ncomp,sval,isval,-2,
     & isetcol,ndv1,ndc1,kmax,iver)
c     & theta,ifitype,ncomp,sval,isval,-2,iver)
	  goto 100		!end of POSH plotting
	endif
c
c AUTOMATIC PLOTTING
	call PLOTAUT(draft,screen,quarter,ifont,csize,thbig,idev,
     & mpos,ip,iptype) 	!sets 'thick'=thbig in common for VHISTQ
	autplt=.false.
	if(.not.screen) print 1003,ip,iplot,mpos,title1
	if(pon()) write(7,1003) ip,iplot,mpos,title1
      if(discprt) write(8,1003) ip,iplot,mpos,title1
	plotcols=.true.
	   if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) then
		mono=.true.
	   else
		mono=.false.
	   endif
	call setcls(mono,icol,autplt,plotcols,isetcol)
	call VHISTQ(XVAL,YVAL,NDAT,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,ncurvc,iline,ndimc,ILOG,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & xwbase,lt2,isval,sval,titlex,titley,ilabel,doframe,
     & draft,itit,title1,csize,ifont,theta,ifitype,ncomp,quarter,idev,
     & ndv1,ndc1,kmax,iver,iplot)
	 autplt=.true.
c
	goto 90		!page full yet?
c

c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c====================================================================
c 		SINGLE CHANNEL RECORDS
c====================================================================
33	continue
	iptype=iptsav		!restore orig value
c	  =3 for SC when position in CONSAM is queued
c	  =-3 for SC when data itself queued
c	  =31/-31 for SC when data for fitted curve also queued
c	  =32/-32 for SC when data for fitted curve also queued using extra record
c 10/25/97 05:41pm If 1000 was added to iptype to indicate data is ffrom
c cjump.dat, then make sure it is removed before plotting
	j=iabs(iptype)
	if(j.gt.1000.and.j.lt.2000) then
	   j=j-1000
	   cjdat=.true.
	else
	   cjdat=.false.
	endif
	iptype=j
c end of modif
	call READSCQ(krn,istrec,iptype,useconsam,itit,title1,
     &  ndev,ib,istart,iend,srate,ymin,ymax,calfac,ioff,calfac2,
     &  cdate,adctime,adcf1,isdfst,ndisp1,nrec1,jdat,base,
     &  ntrans,y0,t0,nfilt,dtf,tif1,tif2,dt,stepamp,filt,
     &  ndelt,ffilt,fcz,fczoom,njump)
c 	Allocate Xval1 etc -PROBLEM -do not know how many points calc curve will
c 	have until CONV1 calculated -ke <=5120, but i1 added to this in READSQ1,
c 	and value not known yet -probably 5120 OK usually -not always!!
	ndv1=jdat
	ndimd=1
	ndc1=20480
	ndimc=1
	if(allocated(xval1)) then
	   DEALLOCATE(Xval1,Yval1)
	endif
	if(allocated(xcal)) then
	   DEALLOCATE(Xcal,Ycal)
	endif
	if(allocated(ndat1)) then
	   DEALLOCATE(ndat1,isym1,ijoin1,syms1)
	endif
	if(allocated(ncal1)) then
	   DEALLOCATE(ncal1,iline1)
	endif
	if(allocated(icurvd)) then
	   DEALLOCATE(icurvd,icurvc)
	endif
	ALLOCATE(Xval1(ndv1,ndimd),Yval1(ndv1,ndimd),Xcal(ndc1,ndimc),
     & Ycal(ndc1,ndimc))
	ALLOCATE(ndat1(ndimd),isym1(ndimd),ijoin1(ndimd),syms1(ndimd))
	ALLOCATE(ncal1(ndimc),iline1(ndimc))
	ALLOCATE(icurvd(ndimd),icurvc(ndimc))
c Actual data is read below
c
216	continue		!return here to replot different points from CONSAM
c 	Now read data and define xval, yval etc in READSCQ, whether auto or not.
	autplt=.true.    !change queue does not have box positions etc
	call READSCQ1(iptype,jdat,intp,krn,nrec1,dx,srate,xmin,xmax,
     & usecons,adcfil,ioff,
     & istart,calfac,calfac2,ndisp1,ncurvd,ncurvc,icurvd,icurvc,
     & xval1,yval1,xcal,ycal,ndat1,isym1,ncal1,ijoin1,syms1,
     & ycalc,y0,ntrans,DT,ke,filt,nfilt,stepamp,dtf,tif1,tif2,t0,
     & ntx,nty,itx,ity,titlex,titley,ilabel,
     & ndv1,ndimd,ndc1,ndimc)
	interp=.false.
	ilog=0
	xlo=-1
	ncjump=0
	nvjump=0
	ivplot=.false.
	cbig=2.5
	ifont=2
	landscap=.true.
	fitted=.false.
	draft=.false.
	doframe=.false.
	itit=1
	ifitype=0
	isdev=-1
	isetcol=1	!new for VPLOT3
	do i=1,100
	   icol(i)=-1		!so default colours used
	enddo
c 	-default colours NOT used for following values which are defined
c 	as valid colours
	icol(1)=9	!blue for data set #1
	icol(11)=12	!red for calc curve #1
	icol(25)=0	!black for plot title
	icol(72)=2	!green for baseline (first horizontal line)
	icol(73)=2	!dashed green for first marker
	icol(74)=1	!dashed blue for 2nd marker
	icol(75)=5	!dashed purple for 3rd marker
	icol(76)=6	!dashed brown for 4th marker
	icol(77)=4	!dashed red for 5th marker
	icol(78)=4	!dashed yellow for 6th marker
c 	Colours not queued for single channels, so must use default colours
c 	for now
c
c 	No SD or theta for single channels, but must allocate arrays before
c 	calling either vplot5 or vplotq5
	if(allocated(weight)) then
	   DEALLOCATE(weight,icurvw)
	endif
	if(allocated(theta)) then
	   DEALLOCATE(theta)
	endif
	kwi=1	!no SD for single channels
	kwj=1
	kmax=1
	ALLOCATE(weight(kwi,kwj),icurvw(ndimd))
	ALLOCATE(theta(kmax))
	do i=1,ndimd
	   icurvw(i)=-1	!no weights!
	enddo

c===============
	if(auto) then
c	   if(.not.quarter) then	!set in PLOTAUT for quarter page
c	      xlo=0.14*xp
c	      xhi=0.95*xp
c	      ylo=0.21*yp
c	      yhi=0.87*yp
c	   endif
	   call PLOTAUT(draft,screen,quarter,ifont,csize,thbig,idev,
     &   mpos,ip,iptype) 	!sets 'thick'=thbig in common for VHISTQ
	if(.not.screen) print 1003,ip,iplot,mpos,title1
      if(discprt) write(8,1003) ip,iplot,mpos,title1
 	call flush(7)
c 	Colours/positions etc not queued so set now
	   autplt=.false.
	   plotcols=.true.
	   if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) then
		mono=.true.
	   else
		mono=.false.
	   endif
	   call setcls(mono,icol,autplt,plotcols,isetcol)
	   if(noclogo) y1c=-10000.
	   if(novlogo) y1v=-10000.
	   if(joinline) then
		do i=1,ncurvd
		   ijoin1(i)=jtype
		enddo
	   endif
c  	xmin,xmax queued, but others not so need to set values here, as done in
c 	vplot3 when iscal=2 before calling vplotq
c	subroutine MINMAX2(xval,yval,ndat,xmin1,xmax1,ymin1,ymax1,
c     & logx,logy,ndimd,ncurvd,icurvd,ndv1)
	   call MINMAX2(xval1,yval1,ndat1,xmin1,xmax1,ymin1,
     &    ymax1,.false.,.false.,ndimd,ncurvd,icurvd,ndv1)	!in VPLOT4
	   call FIXAX(ymin1,ymax1,ymin,ymax,ytic,0)
	   xcross=xmin		!crossing point for axes
	   ycross=ymin
c      Define baseline
	   ybase=base*calfac2	!baseline in pA
	   if(ybase.gt.ymin.and.ybase.le.ymax) then	!base not in old queues!
		nhline=1		!draw horizontal line at baseline
		yhline(1)=ybase
		xhlb(1)=xmin
		xhle(1)=xmax
		ilhtype(1)=0	!continuous line
	   endif
c
c 	Add auto horizontal amp marker lines
c 	If there is no fitted curve then draw markers relative to ybase
c  	but if there is a fitted curve then look for baseline in it -value
c  	for opendown is not queued, but if amplitudes for marker lines are
c  	negative then assume openings are downwards
	   if(nampmark.gt.0) then
c 	First find baseline
		if(abs(iptype).eq.3) then	!no fitted curve
		   base=ybase
		else			!fitted curve
		   call MINMAX2(xcal,ycal,ncal1,xmin2,xmax2,ymin2,
     & 	   ymax2,.false.,.false.,ndimc,ncurvc,icurvc,ndc1)	!in VPLOT4
		   if(amark(1).lt.0.) then	!openings downwards
			base=ymax2
		   else
			base=ymin2
		   endif
		endif
		do i=1,nampmark
		   y=base+amark(i)
		   if(ybase.gt.ymin.and.ybase.le.ymax) then	!base not in old queues!
			nhline=nhline+1		!draw horizontal line
			yhline(nhline)=y
			xhlb(nhline)=xmin
			xhle(nhline)=xmax
			ilhtype(nhline)=2		!dashed line
		   endif
		enddo
	   endif
c
	   do i=1,100		!no positions defined from queue
		angle(i)=0
		idraw(i)=-2		!until defined
		rx(i)=0.0		!NB world coord may be neg so cant tell if defined yet
	   enddo
	   logx=ilog.eq.1.or.ilog.eq.3.or.ilog.eq.4.or.ilog.eq.6
	   logy=ilog.eq.2.or.ilog.eq.3
	   if(.not.logx) inumx=-1	!see notes in LAXES2
	   if(.not.logy) inumy=-1	!see notes in LAXES2
	   autplt=.true.
	   plotcols=.true.
	   if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) then
		mono=.true.
	   else
		mono=.false.
	      if(idev.eq.6) then
			isetcol=1
		else
			isetcol=0
		endif
	   endif
	   call setcls(mono,icol,autplt,plotcols,isetcol)
	   call VPLOTQ5(XVAL1,YVAL1,ndat1,ncurvd,ijoin1,syms1,ndv1,ndimd,
     &    xcal,ycal,ncal1,ncurvc,iline1,ndc1,ndimc,isym1,ilog,
     &    XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     &    y0,yinf,titlex,titley,ilabel,
     &    doframe,draft,itit,title1,csize,ifont,
     &    theta,ifitype,ncomp,weight,isdev,quarter,iptype,
     &   ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ivplot,
     &    cjump,idev,interp,iplot,idelt,
     &    kwi,kwj,icurvw,kmax,iver)
	    autplt=.true.
	   goto 90		!page full yet?
c
	else if(.not.auto) then
	   if(videotyp().ne.18) call VGA
	   autplt=.true.	! change queue does not have box positions etc -use defaults
c  	   ISCAL=2 if input values of xmin,xmax only to be used; others internally set
	   ntx=-1000	!cal bars
	   nty=-1000	!cal bars
	   inumx=-1	!see notes in LAXES2
	   inumy=-1	!see notes in LAXES2
	   iscal=2		!use only xmin, xmax -others internal
	   do i=1,100		!no positions defined from queue
		angle(i)=0
		idraw(i)=-2		!until defined
		rx(i)=0.0		!NB world coord may be neg so cant tell if defined yet
	   enddo
c	   autplt=.true.
	   autplt=.false.		!to ensure all defaults used
	   call VPLOT5(xval1,yval1,ndat1,icurvd,ncurvd,ijoin1,syms1,ndimd,
     & xcal,ycal,ncal1,icurvc,ncurvc,iline1,ndimc,isym1,ilog,iscal,
     & xmin,xmax,ymin,ymax,xtic,ytic,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,draft,itit,title1,
     & cbig,ifont,landscap,fitted,-2,theta,ifitype,ncomp,interp,
     & isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kmax,iver)
c
c Replot different bit if CONSAM is present. NB have enough data in queue
c to use CONSAM even when data itself was queued (iend, jdat defined in
c READSCQ), so can still ask whether to use it.  If data itself was
c queued then name of CONSAM file not yet defined, so must ask for it now.
c
214	   continue
	   ans='N'
	   if(.not.cjdat) then
	      call DCASK(
     &     'Replot with different points taken from CONSAM.DAT','n',ans)
	   else
	      call DCASK(
     &     'Replot with different points taken from CJUMP.DAT','n',ans)
	   endif
	   if(ans.eq.'Y') then
		if(consamdef) then
		   if(.not.useconsam) usecons=.true.	!data itself queued
		else
		   print 217
217		   format(
     &	    ' Data itself was queued: get CONSAM name/path now')
		   if(adcf1(2:2).eq.':') adcfil(1:30)=adcf1 !path for consam in queue
c             Now getconsam finds name of cjump.dat if cjdat=true
		   call GETCONS(adcfil,noconsam,title,cdate1,adctim1,nsam,
     &	    srate1,cjdat,irecfst,newform,ioffset,cfac,fflt,idest)
		   if(.not.cjdat.and.(srate.ne.srate1)) then	!srate1 not def for cjdat
			call BELL(1)
			print 34,srate,srate1
c34			format(' ERROR: srate, srate1 = ',2g13.6)
		   endif
		   if(idest.eq.99) goto 99
		   if(noconsam) goto 100
		   consamdef=.true.	!consam now defined
		   usecons=.true.
c Cannot just set useconsam=.true. now, because must still read
c plotq as for NOCONSAM, but read data from consam! ie useconsam=true
c for readscq1, but not otherwise.  Set consamdef=true here to show
c consam name/path is now defined, and set usecons=true for use in call to
c READSCQ1.  From now on, consamdef remains true, but usecons is set equal
c to useconsam at the start of each plot loop, so next time a plot that
c has data itself queued is found, it functions normally.
		endif
c
		jdat=iend-istart+1
		samlen=dx*float(jdat)	!sample length (ms)
c
		print 210,samlen,jdat,istart,iend
210		format(
     & '(NB now do multiple page plots via MORE OPTIONS in VPLOT)',/,/,
     &	' At present: display length = ',g13.6,' ms',/,
     &	' number of points = ',i8,'(from #',i9,' to #',i9,')')
		print 211
211		format(' Alter the start point by time (ms) = ')
		call INPUTr(ds)
		if(ds.ne.0.0) then
		   ds1=1000.*ds			!in microsec
		   t0=t0 - ds1		!so calc curve in proper position
		   tif1=tif1 - ds1
		   tif2=tif2 - ds1
		endif
		print 212
212		format('&Alter the end point by time (ms) = ')
		call INPUTr(de)
		ids=ifixr(ds/dx)
		ide=ifixr(de/dx)
		istart=istart+ids
		iend=iend+ide
		jdat=iend-istart+1
		samlen=dx*float(jdat)	!new sample length (ms)
		print 213,samlen,istart,iend,jdat
213		format(' New sample length = ',g13.6,' ms',/,
     &	' istart, iend = ',2i9,'  number of points = ',i8,/,
     &	' O.K. [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(UC(ans).eq.'N') goto 214
	      if(pon()) write(7,215) samlen,istart,iend,jdat  !print new values
	      if(discprt) write(8,215) samlen,istart,iend,jdat
215		format(' New sample length = ',g13.6,' ms',/,
     &	' istart, iend = ',2i9,'  number of points = ',i8)
		ndv1=jdat		!new value
		ndimd=1
c See comment above concerning problem of suitable size for ndc1
c		ndc1=jdat
		ndc1=20480
		ndimc=1
		DEALLOCATE(Xval1,Yval1,Xcal,Ycal)
		ALLOCATE(Xval1(ndv1,ndimd),Yval1(ndv1,ndimd),
     &	 Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
		goto 216		!replot it
	   else
		itrace=0
		ntrace=1
	      goto 100		!end of POSH plotting
	   endif
	endif			!end of 'if(.not.auto)'


c====================================================================
c               			3D PLOTS
c====================================================================
40    continue
	CLOSE(unit=11)
      OPEN(unit=11,file=QFILE,status='UNKNOWN',
     &		access='TRANSPARENT')
	istrec=1 + (int4(jstrec(iplot))-1)*1024  !start rec when transparent
        do i=1,100
           icol(i)=-1           !so default colours used
        enddo
	if(iptype.eq.40) then
           read(11,rec=istrec)iptype,title3,nx,ny,ndx,ndy,numx,numy,
     &     xtitle,ytitle,ztitle,idrawt,idrawx,idrawy,idrawz,
     &     xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,
     &     ifnt3,ifnl,alfa,beta,gama,delta,
     &     ijust,ijustx,ijusty,ijustz,
     &     xmin3,xmax3,ymin3,ymax3,zmin3,zmax3,xtic3,ytic3,ztic3,
     &     ratio,rat,radius,theta3,phi,ifram,numaxi,nwidth,npow,nplace,
     &     nxstep,nystep,ncolfu,ncolfd,ncolgu,ncolgd,kcol,
     &     icol25,icol23,icol71,istyle,isup,
     &     fill,inter,axes,fillbad,cross,ioffs
             if(kcol.eq.4) then
                  posneg=.true.
                  kcol=2
             else if(kcol.eq.1) then  ! contour
                  posneg=.false.
                  ncont=14
                  kcol=-ncont
             else if(kcol.eq.2) then  ! one
                  kcol=1
                  posneg=.false.
             else if(kcol.eq.3) then  ! two
                  kcol=2
                  posneg=.false.
             endif
                icol(1)=1          !contour 1
                icol(2)=9          !cont 2
                icol(3)=3          !cont 3
                icol(4)=11   !cont 4
                icol(5)=2          !cont 5
                icol(6)=10   !cont 6
                icol(7)=8          !cont 7
                icol(8)=5          !cont 8
                icol(9)=13   !cont 9
                icol(10)=6         !cont 10
                icol(11)=4         !cont 11
                icol(12)=12   !cont 12
                icol(13)=14   !cont 13
                icol(14)=15   !cont 14
                icol(15)=7         !cont 15
                icol(21)=11     !axes
                icol(22)=0   !bad region
             icol(33)=ncolfu
             icol(34)=ncolfd
             icol(31)=ncolgu
             icol(32)=ncolgd
             icol(25)=icol25
             icol(23)=icol23
             icol(71)=icol71
             icol(22)=0
             icol(35)=12        !positive
             icol(36)=9 !negative
	else if(iptype.eq.41) then
           read(11,rec=istrec)iptype,title3,nx,ny,ndx,ndy,numx,numy,
     &     xtitle,ytitle,ztitle,idrawt,idrawx,idrawy,idrawz,
     &     xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,
     &     ifnt3,ifnl,alfa,beta,gama,delta,
     &     ijust,ijustx,ijusty,ijustz,
     &     xmin3,xmax3,ymin3,ymax3,zmin3,zmax3,xtic3,ytic3,ztic3,
     &     ratio,rat,radius,theta3,phi,ifram,numaxi,nwidth,npow,nplace,
     &     nxstep,nystep,ncolfu,ncolfd,ncolgu,ncolgd,ncolmp,ncolmn,kcol,
     &     icol25,icol23,icol71,istyle,isup,
     &     fill,inter,axes,fillbad,cross,ioffs,icbad
             if(kcol.eq.4) then
                  posneg=.true.
                  kcol=2
             else if(kcol.eq.1) then  ! contour
                  ncont=14
                  kcol=-ncont
                  posneg=.false.
             else if(kcol.eq.2) then  ! one
                  kcol=1
                  posneg=.false.
             else if(kcol.eq.3) then  ! two
                  kcol=2
                  posneg=.false.
             endif
                icol(1)=1          !contour 1
                icol(2)=9          !cont 2
                icol(3)=3          !cont 3
                icol(4)=11   !cont 4
                icol(5)=2          !cont 5
                icol(6)=10   !cont 6
                icol(7)=8          !cont 7
                icol(8)=5          !cont 8
                icol(9)=13   !cont 9
                icol(10)=6         !cont 10
                icol(11)=4         !cont 11
                icol(12)=12   !cont 12
                icol(13)=14   !cont 13
                icol(14)=15   !cont 14
                icol(15)=7         !cont 15
                icol(21)=11     !axes
                icol(22)=0   !bad region
             icol(33)=ncolfu
             icol(34)=ncolfd
             icol(31)=ncolgu
             icol(32)=ncolgd
             icol(25)=icol25
             icol(23)=icol23
             icol(71)=icol71
             icol(22)=icbad
             icol(35)=ncolmp    !positive
             icol(36)=ncolmn    !negative
      else if(iptype.eq.42) then
           read(11,rec=istrec)iptype,title3,nx,ny,ndx,ndy,numx,numy,
     &     xtitle,ytitle,ztitle,idrawt,idrawx,idrawy,idrawz,
     &     xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,
     &     ifnt3,ifnl,alfa,beta,gama,delta,
     &     ijust,ijustx,ijusty,ijustz,
     &     xmin3,xmax3,ymin3,ymax3,zmin3,zmax3,xtic3,ytic3,ztic3,
     &     ratio,rat,radius,theta3,phi,ifram,numaxi,nwidth,npow,nplace,
     &     nxstep,nystep,istyle,isup,kcol,
     &     fill,inter,axes,fillbad,cross,posneg,ioffs
      endif
	if(allocated(xval3)) DEALLOCATE(Xval3)
	if(allocated(yval3)) DEALLOCATE(Yval3)
	if(allocated(z3)) DEALLOCATE(z3)
	if(allocated(bad)) DEALLOCATE(bad)
	allocate(xval3(ndx),yval3(ndy),z3(ndx,ndy),bad(ndx,ndy))
	istr1=istrec+ioffs
	indk=ndx*ndy
	allocate(zeta1(indk+1),zeta2(indk+1))
        if(iptype.eq.42) then
        read(11,rec=istr1) (icol(k),k=1,100),(XVAL3(k),k=1,ndx),
     &  (YVAL3(k),k=1,ndy),(zeta1(k),k=1,indk),(zeta2(k),k=1,indk)
        else
        read(11,rec=istr1) (XVAL3(k),k=1,ndx),
     &  (YVAL3(k),k=1,ndy),(zeta1(k),k=1,indk),(zeta2(k),k=1,indk)
        endif
	k=1
	do i=1,ndx
	   do j=1,ndy
		z3(i,j)=zeta1(k)
	      bad(i,j)=zeta2(k)
		k=k+1
	   enddo
	enddo
	DEALLOCATE(zeta1,zeta2)
      if(auto) then
	   call PLOTAUT(draft,screen,quarter,ifont,csize,thbig,idev,
     &   mpos,ip,iptype) 	!sets 'thick'=thbig in common for VHISTQ
	   if(.not.screen) print 1003,ip,iplot,mpos,title3
         if(discprt) write(8,1003) ip,iplot,mpos,title3
 	   call flush(7)
	   call gauto3d(xval3,yval3,z3,bad,nx,ny,ndx,ndy,
     &    xtitle,ytitle,ztitle,title3,idrawt,idrawx,idrawy,idrawz,
     &    quarter,idev,plot,iplot,kcol,posneg)
	   goto 90
	else
	   isetcol=2
	   autplt=.true.
	   mono=.false.
	   call gplot3d(xval3,yval3,z3,bad,nx,ny,ndx,ndy,
     &	xtitle,ytitle,ztitle,title3,idrawt,idrawx,idrawy,idrawz,
     &      kcol,posneg,isetcol,qfile)
	   goto 100
	endif
c
c===========================================================
c 		AUTOMATIC PLOTTING
c===========================================================


c Plotaut sets xlo etc, and 'thick'=thbig in common, for VPLOTQ.
	call PLOTAUT(draft,screen,quarter,ifont,csize,thbig,idev,
     & mpos,ip,iptype) 	!sets 'thick'=thbig in common for VPLOTQ
c
90	continue	!for auto plotting

c

cc Position control for test on screen. Note that position is controlled
c by ixlo,... etc; values are set in PSPACE (rather than values from queue)
c when MSEQ=true. 'Square' graphs (i.e. not half-page) can be shown on
c screen as whole page or 1/4 page.
c For whole page mpos set initially to 5 (and not changed)
c	mpos=mpos+1
c When page is full must (a) call ENDPLT if draft mode (idev<5), or
c (b)call VTPLOT do do the plots from PLOT99.DAT if ndev=99 (ndev>5 in general)
c (only needed if AUTO; for POSHPLOT this is done within VHIST/VPLOT)
c Page full if mpos=4 or 5(###will need modif for half-page plots), or
c if last plot reached.  But if quarter page plots are rearranged then
c mpos=4 does not necessarily mean that page is full! Now ip=4,8,12,... (or
c ip=kplot) always indicates the last plot on a page, i.e.
c mod(ip,4)=0 or ip=kplot.
	if((mod(ip,4).eq.0).or.(ip.eq.kplot).or.mpos.eq.5) then
	   if(idev.le.6) then
		if(screen) ans=getch(k)
		if(.not.screen) then
		   call devend
	         call timer(iticks1)
		   if(idev.eq.3.or.idev.eq.4) then
	         	oldfile='EPS.OUT'
		   else if(idev.eq.5) then
	         	oldfile='HPLJR.OUT'
		   else if(idev.eq.6) then
	         	oldfile='DJ500C.OUT'
		   endif
	         INQUIRE(file=oldfile,exist=present,flen=len)
	         if(present) then
	            kp=kp+10
	   		call intconv(kp,kpchar)
	   		nc1=nblank1(kpchar)
			if(idev.eq.5) then
	   		   printfil='lfit'//kpchar(1:nc1)//'.out'
			else if(idev.eq.6) then
	   		   printfil='cfit'//kpchar(1:nc1)//'.out'
			else
	   		   printfil='fit'//kpchar(1:nc1)//'.out'
			endif
	      	INQUIRE(file=printfil,exist=present)
			if(present) then
			   call ERASE(printfil)	!erase old printfil, before rename
			endif
	   		call RENAME(oldfile,printfil)
			Print 70,
     &		charnb(oldfile)//' renamed '//charnb(printfil)
70			format(1x,a34)
		      print 71,
     &		' Copying '//charnb(printfil)//' to printer . . .'
71		      format(a40)
			print 72,float(len)/1.e6
72			format('  (length = ',f9.3,' Mb)')
			if(idev.eq.3.or.idev.eq.4) then
			   call COPY (printfil,'lpt1')
		      else if(idev.eq.5)then
			   call COPY (printfil,'lpt1')
c	   		   call system('copy/b hpljr.out lpt1:')
			else if(idev.eq.6) then
			   call COPY (printfil,'lpt1')
c	   		   call system('copy/b dj500c.out lpt1:')
c			   give time 60 seconds!
666			   call timer(iticks2)
			   if((iticks2-iticks1).lt.6000) goto 666
		      endif
		   else
			call BELL(1)
			print 73,oldfile
73			format(' Printer file not found: ',a33)
			pause
		   endif
5432		continue
		endif
 		call FLUSH(7)
	   endif
	endif
c

100	continue	!end of plot loop
c==============================================================
c
99    continue
	mono=.false.
      CLOSE(UNIT=11)
c	if(screen) pause		!to see last page on screen
	if(.not.auto.and.(.not.screen)) then
c         write(7,*) char(27)//'&l0H'		!page eject
 	   call FLUSH(7)
	endif
c	pause
      call videoMOD(3)                !utility lib- this makes graph go for good!
      call DCASK('Another run','n',ans)
      if(ans.eq.'Y') then
	   if(allocated(xval1)) then
		DEALLOCATE(Xval1,Yval1)
	   endif
	   if(allocated(xcal)) then
		DEALLOCATE(Xcal,Ycal)
	   endif
	   nhline=0
	   nvline=0
	   nampmark=0
	   goto 120
	endif
c
999	continue
c Put idiskq, adcfil and cjump in .INI file (rest already written
c at end of INAUT)
      OPEN(unit=12,file='AUTPLOT.INI',status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=128)
	read(12,rec=1) ndev,qfile,iopt,idum,auto,screen,
     &	  quarter
	write(12,rec=1) ndev,qfile,iopt,idiskq,auto,screen,
     &	  quarter,adcfil,cjump
	CLOSE(unit=12)
c
	call devend
	call ginend
	call ENDPRINT()
	close(unit=10)
	call flush(7)
	pause
	call mode(3)
c
	end

