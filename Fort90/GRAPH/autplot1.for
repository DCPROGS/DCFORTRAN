	program AUTPLOT
c
c Lahey/Hgraph program to
c (a) plot graphs automatically from queue (PLOTQ.DAT)
c (b) make fancy individual plots
c
c Version number specified as iptype, thus
c IPTYPE=1 for VPLOT        -obsolete
c	  =11 for VPLOTR        -obsolete
c	  =12 for VPLOT2, VPLOT3        -obsolete
c	  =14 for VPLOT4 -record read in different order!
c	  =15 for VPLOT5
c	  =2 for VHIST
c	  =3 for SC when position in CONSAM is queued
c	  =-3 for SC when data itself queued
c	  =31/-31 for SC when data for fitted curve also queued
c	  =32/-32 for SC when data for fitted curve also queued using extra record
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
c	integer jstrec(100),lstrec(100)		!for all
	integer*2 jstrec(200),lstrec(200)		!for all
	logical plotcirc,novlogo,noclogo,joinline,mono,monsav,interp
	logical arrange,consec,colplotter,mono1,useconsam,noconsam,present
	logical consamdef,usecons	!see note after vplot3 in single channel sec
	logical scplots,allocated
	logical mulpage,filtered
      integer jplot(200),jpos(200)    !to hold re-ordered plots
	integer*2 irecfst(250)		!=500 bytes for CJUMP
	logical cjump
	character cdate*11,ctime*11,ndev*2,adctime*8
	character filnam*32,prtport*4		!for winprint
	character cdate1*11,adctim1*8		!read from consam
	character adcfil*33,adcf1*30	!path for consam file
      character*1 ans,UC,getch
	character*33 oldfile,printfil
	character*11 kpchar
c For single channels
c	integer*2 idata(20480)	!now in READSCQ
c    for calc curve
	real*4 stepamp(50),dt(49),filt(640)
	real*4 ycalc(5120)
	real*4 amark(10)
c	logical pdpdata
c For vhist and vplot
c	integer icurvd(10),icurvc(10)
	ALLOCATABLE:: icurvd,icurvc
	integer icurvd(:),icurvc(:)
c For VHIST
	real XVAL(0:511,10),YVAL(0:511,10)
c==	real XCAL(512,10),YCAL(512,10)	!for VHIST2
c	dimension ndat(10),isym(10),ncal(10),ijoin(10),iline(10)	!for VHIST
	dimension ndat(10),ncal(10),ijoin(10),iline(10)	!for VHIST
c Arrays for Vplot
	ALLOCATABLE:: XVAL1,YVAL1,XCAL,YCAL
	real XVAL1(:,:),YVAL1(:,:)		!for VPLOT
	real XCAL(:,:),YCAL(:,:)		!for VPLOT/VHIST
c=	real XVAL1(2048,10),YVAL1(2048,10)	!for VPLOT
c=	real XCAL(2048,10),YCAL(2048,10)	!for VPLOT/VHIST
	ALLOCATABLE:: weight,icurvw
	real*4 weight(:,:)
	integer*4 icurvw(:)
c -also allocate ndat1() etc!
c Allocate other arrays for VPLOT
	ALLOCATABLE:: ndat1,isym1,ijoin1,syms1
	ALLOCATABLE:: ncal1,iline1
	integer ndat1(:),isym1(:),ijoin1(:)
	integer ncal1(:),iline1(:)
	real syms1(:)				!for data
c	integer ndat1(10),isym1(10),ncal1(10),ijoin1(10),iline1(10)	!for VPLOT
c	real syms1(10)
c	real theta(20)
	ALLOCATABLE:: theta
	real theta(:)
	character title1*64,qfile*33,titlex*40,titley*40,title*76
c	logical plot,labels,mseq,vplot
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
	logical mouse_on,mous_set,horizont
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
c=	real*4 DATCOP(46960)		!put in common to save space
c=	integer*2 INT2COP(1000)
c=	character*10 CHARCOP(100)
c
c=	COMMON/copblk/DATCOP,INT2COP,CHARCOP
c
	COMMON/CHVBLK/maxnum,curnum,vect
	common/dp/discprt
	common/dpp/filnam,prtport,ndisc,jcol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c Define common/TPOS for VPLOT1 to hold data on text position etc from
c queue, for posh plots
	COMMON/TPOS/rx,ry,rxbox,rybox,ijus,angle,idraw,
     & ifnt,size,rlth,thbig,narrow,xb,yb,xe,ye,
     & nline,xlb,xle,ylb,yle,iltype,ntext,newtext,
     & cnumx,cnumy,cexpx,cexpy,numbx,numby,
     & nhline,yhline,xhlb,xhle,ilhtype,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel
	COMMON/mtrace/ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil
	COMMON/cols/icol,mono
c lines 5,6 added to TPOS 09/04/94 08:29pm for hor and vert lines
c  line 7 added to TPOS 09/12/94 07:11am multiple traces
c==	COMMON/SD/weight(100,10)  !set isdev=-1 in call if not needed for VPLOT
	COMMON/JLOGO/t1c(10),t2c(10),t1v(10),t2v(10),xoff1,y1v,y2v,
     & y1c,y2c                    !for VPLOT

	common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi
c
c
	pon()=slock()
	debug()=caplock()
c
	call GINO
	call SETMOUSE()		!define values in common\mousval\ (in IVLIB)
c
c Use F90 routine to prevenet underflow crashes??
	errflag=.true.
	call UNDFL(errflag)
	plotcols=.true.
c
c
101	format(a1)
      filnam='AUTPLOT.PRT'
c      OPEN(unit=7,file='PRN')
c      OPEN(unit=7,file='LPT1')		!OK
c      OPEN(unit=7,file='LPT3')
	call WINPRINT	!print file control
      OPEN(unit=7,file=prtport)
	print 102
	if(pon()) write(7,102)
	if(discprt) write(8,102)
102	format(' AUTPLOT- Automatic plotting program',/)
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
	mulpage=.false.		!multipage plots
	call INAUT(nplot,jstrec,lstrec,QFILE,auto,id,ndisc,
     & screen,draft,colplotter,quarter,cjump,irecfst,mono,idiskq,
     & adcfil)
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
	   irecl=512
         OPEN(unit=14,file=QFILE,status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=irecl)
	   goto 103
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
	   istrec=int4(jstrec(iplot))	!1st record no for plot # iplot
	   read(11,rec=istrec) iptype
	   j=iabs(iptype)
c	   if(j.eq.3.or.j.eq.31.or.j.eq.32) then
c	      useconsam=iptype.gt.0	!= -3,-31 or -32 if SC data itself is queued
c		scplots=.true.
c	   endif
c Modif 04/04/97 09:59am
	   if(j.eq.3.or.j.eq.31.or.j.eq.32.or.
     &	j.eq.30.or.j.eq.301.or.j.eq.302) then
	      useconsam=iptype.gt.0	!= -3,-31 or -32 if SC data itself is queued
		scplots=.true.
		filtered=j.eq.30.or.j.eq.301.or.j.eq.302
	   endif
	   if(useconsam) then	!get CONSAM path here
		call READSCQ(krn,istrec,iptype,useconsam,itit,title1,
     &	 ndev,ib,istart,iend,srate,ymin,ymax,calfac,ioff,calfac2,
     &	 cdate,adctime,adcf1,isdfst,ndisp1,nrec1,jdat,base,
     &	 ntrans,y0,t0,nfilt,dtf,tif1,tif2,dt,stepamp,filt,
     &	 ndelt,ffilt,fcz,fczoom)
		if(adcf1(2:2).eq.':') adcfil(1:30)=adcf1	!path for consam in queue
		call GETCONSAM(adcfil,noconsam,title,cdate1,adctim1,
     &	 nsam,srate1,idest)
		if(srate.ne.srate1) then
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
c	  =3 for SC when position in CONSAM is queued
c	  =-3 for SC when data itself queued
c	  =31/-31 for SC when data for fitted curve also queued
c	  =32/-32 for SC when data for fitted curve also queued using extra record
c ITIT=0 for no title
c ITIT=1 for title
	if(cjump) goto 31			!data from CJUMP.DAT
c Make sure that QFILE is opened with fixed record length to get iptype
c for the current plot
	CLOSE(unit=11)
      OPEN(unit=11,file=QFILE,status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=irecl)
	istrec=int4(jstrec(iplot))	!1st record no for plot # iplot
	nrect=int4(lstrec(iplot))-istrec+1		!total number of records
	nrect=nrect-2				!number of records for Xval etc
	read(11,rec=istrec) iptype
	j=iabs(iptype)
c Modif 04/04/97 09:59am
	if(j.eq.3.or.j.eq.31.or.j.eq.32.or.
     &  j.eq.30.or.j.eq.301.or.j.eq.302) then
	    useconsam=iptype.gt.0	!= -3,-31 or -32 if SC data itself is queued
	    filtered=j.eq.30.or.j.eq.301.or.j.eq.302
	    iptype=j
	endif
	usecons=useconsam	   !for readscq1 (see notes after vplot3 in SC section
c
	if(iptype.ge.11.and.iptype.le.25) goto 31	!vplot
	if(iptype.eq.31.or.iptype.eq.32.or.iptype.eq.30.or.
     &   iptype.eq.301.or.iptype.eq.302) goto 33	!single channel
	goto(31,32,33) iptype
	stop
c
c Graphs- from VPLOT1, VPLOTR, VPLOT2 or VPLOT3
31	continue
c Read for VPLOT
c Logical PLOT replaced by DOFRAME=true if frame to be drawn
c Read data from queue:
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
		read(11,rec=istrec)iptype,ndv1,ndimd,ndc1,ndimc,kwi,kwj,kmax
	   else
	      ndv1=2048	!dimensions as for earlier versions
		ndimd=10
		ndc1=2048
		ndimc=10
		kwi=100	!dimensions for weight()
		kwj=10
		kmax=20
	   endif
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
	   krn=istrec
c Up to this point, have read PLOTQ as though it was always in 1024 byte
c records, but for
	   if(iptype.ge.15.and.iptype.le.25) then
		CLOSE(unit=11)
            OPEN(unit=11,file=QFILE,status='UNKNOWN',
     &		access='TRANSPARENT')
		istrec=1 + int4(jstrec(iplot)-1)*1024  !start rec when transparent
	   endif
	   call RDVPLQ(istrec,xval1,yval1,xcal,ycal,ndimd,ndimc,ncurvd,
     & ndat1,isym1,ijoin1,ncurvc,ncal1,
     & iline1,syms1,xmin1,xmax1,ymin1,ymax1,
     & xcross,ycross,xtic,ytic,ntx,nty,itx,ity,xlo,xhi,ylo,yhi,
     & itit,title1,ifont,ilog,iscal,doframe,titlex,titley,ilabel,
     & inumx,inumy,sval,theta,ifitype,ncomp,isdev,weight,y0,yinf,iptype,
     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ivplot,
     & interp,screen,colplotter,
     & itrace,ntrace,ytsep,ndv1,ndc1,
     & kwi,kwj,icurvw,kmax,iver,idev)
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
c===NB do not touch xmin,etc if iscalfix=0 (done for jumps only, and this
c=== is done in RDCJDAT)
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
	   if(iptype.eq.1) then
		ncjump=0             !no jumps in VPLOT
		nvjump=0
	   else if(iptype.eq.11) then
		isdev=-1             !no SD in VPLOT
	   endif
c iline options altered in VPLOT2 so fix here
	   if((iptype.eq.1.or.iptype.eq.11).and.ncurvc.gt.0) then
	 	do 20 i=1,ncurvc
20	 	   if(iline1(i).gt.0) iline1(i)=iline1(i)-1
	    endif
c last 2 lines are values for vplotr
	   if(.not.screen) print 1003,ip,iplot,mpos,title1
	   if(pon()) write(7,1003) ip,iplot,mpos,title1
         if(discprt) write(8,1003) ip,iplot,mpos,title1
1003	   format(/,1x,i5,': Plot no ',i5,' (position ',i2,')',/,
     &	2x,a44)
c NOW CJUMP
	else if(cjump) then
	   iptype=11		!use vplot
	   istrec=int4(irecfst(iplot))
c alloc/dealloc yval1 etc for each plot; read nsamp here to get size
c to allocate Yval (all cjump versions are same up to nsamp)
	   read(14,rec=istrec) cdate2,ctime2,title2,naver,navc,iav,
     & 	control,vjump,sampv,nsamp
	   ndv1=nsamp
	   ndimd=1
	   ndc1=1
	   ndimc=1
	   if(allocated(xval1)) then
		DEALLOCATE(Xval1,Yval1,icurvw)
	   endif
	   if(allocated(xcal)) then
		DEALLOCATE(Xcal,Ycal)
	   endif
	   ALLOCATE(Xval1(ndv1,ndimd),Yval1(ndv1,ndimd),icurvw(ndimd),
     &	Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
	   call RDCJDAT(istrec,xval1,yval1,xcal,ycal,ndimd,ndimc,
     &   xmin,xmax,ymin,ymax,xtic,ytic,xcross,ycross,landscap,
     &   ncurvd,ndat1,isym1,ijoin1,ncurvc,ncal,iline1,syms1,
     &   ntx,nty,itx,ity,xlo,iask,itit,title1,csize,ifont,ilog,iscal,
     &   doframe,titlex,titley,ilabel,ifitype,ncjump,t1c,t2c,nvjump,
     &   t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,iplot,screen,
     &   ndv1,ndc1,iscalfix)
c	   ifstcol(1)=1		!for VPLOTQ (one curve only plotted)
c	   call SETCLS(mono,icol,.false.,.true.)	!set plotter colours
c	   if(idev.eq.4.and.(.not.mono)) then
c		call REVCOL(icol)	!reverse black/white
c	   endif
	  if(idev.eq.0) then
	     call setcls(.false.,icol,.false.,plotcols,isetcol)
	  else
	     call setcls(.true.,icol,.false.,plotcols,isetcol)
	  endif
	endif
c
c DO POSH PLOT IF NOT AUTO
	if(.not.auto) then
c Can now define ICURVD, needed for VPLOT simply as:
	   if(ncurvd.gt.0) then
		do 11 i=1,ncurvd
c11		icurvd(i)=ifstcol(i)	!=i if no plots > 2048 points
11		icurvd(i)=i
	   endif
	   if(ncurvc.gt.0) then
		do 12 i=1,ncurvc
12		icurvc(i)=i
	   endif
	   autplt=.true.	!called from AUTPLT
	   goto 666
c
c====================??????????????????????????????????????

c 	   For Toshiba VIDEOTYP()=18 for graphics mode, =3 for alpha mode
         call GINO
	   if(videotyp().ne.18) call VGA
         call papenq(xp,yp,ipap)
	   iscal=0		!use input xmin etc
c 	   Input values of ixlo etc may be for 1/4 page plot.  To do posh plots
c        need whole page so set ixlo=-1 (need to keep input values?)
c	   ixlo=-1		!NO- used queued values (so LANDSCAP ignored)
c 	   If data from an old queue the landscape format would be ixlo etc= 1400,9500
c        1600,6500 rather than the smaller format now used, so alter then to
c        the present landscape default size
	   if(xlo.eq.0.14*xp) then
		xlo=0.13*xp		! set
c		if(calbarY) xlo=0.05*xp	!make wider
		if(nty.eq.-1000) xlo=0.05*xp !make wider NB calbarY=nty.eq.-1000
		xhi=0.82*xp		! display
		ylo=0.2*yp		! location
		yhi=0.76*yp		! screen
	   endif
c        For cjump data have no queued details, so scale internally
666	   if(cjump) then
		autplt=.true.      ! chaged from false
		iscal=1
		isdev=-1
		itrace=0
	   endif
	   cbig=csize		!from queue
	   thbig=1.0		!in common for line thickness
	   isetcol=0		!for VPLOT
c
	   logx=ilog.eq.1.or.ilog.eq.3.or.ilog.eq.4.or.ilog.eq.6
	   logy=ilog.eq.2.or.ilog.eq.3
	   if(.not.logx) inumx=1	!see notes in LAXES2
	   if(.not.logy) inumy=1	!see notes in LAXES2
c
	  if(idev.eq.0) then
	     call setcls(.false.,icol,.false.,plotcols,isetcol)
	  else
	     call setcls(.true.,icol,.false.,plotcols,isetcol)
	  endif
	 autplt=.true.
	 call VPLOT5(xval1,yval1,ndat1,icurvd,ncurvd,ijoin1,syms1,ndimd,
     & xcal,ycal,ncal1,icurvc,ncurvc,iline1,ndimc,isym1,ilog,iscal,
     & xmin,xmax,ymin,ymax,xtic,ytic,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,draft,itit,title1,
     & cbig,ifont,landscap,fitted,-2,theta,ifitype,ncomp,interp,
     & isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kmax,iver)
	   goto 100		!end of POSH plotting
	endif
c
c=======================================================================
c AUTOMATIC PLOTTING
c=======================================================================

c If option selected (in PLOTOPT) to rearrange plots, then do it here

	 call PLOTAUT(draft,screen,quarter,ifont,csize,thbig,idev,
     & mpos,ip) 	!sets 'thick'=thbig in common for VPLOTQ
c
c	if(idev.eq.4.and.(.not.mono)) then
c	   call REVCOL(icol)	!reverse black/white
c	endif
c
	if(plotcirc) then
	  do i=1,ncurvd
	   if(isym1(i).eq.0) then
		isym1(i)=-7		!filled circle
		syms1(i)=0.7
	   endif
	  enddo
	endif
	if(noclogo) y1c=-1
	if(novlogo) y1v=-1
	if(joinline) then
	  do i=1,ncurvd
	   ijoin1(i)=jtype
	  enddo
	endif
c For cjump plots VPLOTQ sets position of jump logos (at present they
c are normally in abs screen units so those for bottom row of 1/4 page
c plots come out at top of screen!)
	  if(idev.eq.0) then
	     call setcls(.false.,icol,.false.,plotcols,isetcol)
	  else
	     call setcls(.true.,icol,.false.,plotcols,isetcol)
	  endif
	call VPLOTQ5(XVAL1,YVAL1,ndat1,ncurvd,ijoin1,syms1,ndv1,ndimd,
     & xcal,ycal,ncal1,ncurvc,iline1,ndc1,ndimc,isym1,ilog,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & y0,yinf,titlex,titley,ilabel,
     & doframe,draft,itit,title1,csize,ifont,
     & theta,ifitype,ncomp,weight,isdev,quarter,iptype,
     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ivplot,
     & cjump,idev,interp,iplot,idelt,
     & kwi,kwj,icurvw,kmax,iver)
c
	goto 90		!page full yet?
c
c HISTOGRAMS
c (remove from queue: mpos,mlast,ixlo,...,iyhi,ipdis,isup,flo,fhi,
c lt1,ivel,plot;  add doframe
32	continue
c  At present xval, yval are fixed dimension, but must allocate Xcal,Ycal
	ndv1=2048	!dimensions as for earlier versions
	ndimd=10
	ndc1=2048
	ndimc=10
	kmax=20	!for theta
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
		istrec=1 + int4(jstrec(iplot)-1)*1024  !start rec when transparent
	   endif
	krn=istrec
	call RDVHSQ(istrec,xval,yval,xcal,ycal,ndimd,ndimc,ncurvd,
     & ndat,ijoin,ncurvc,ncal,iline,xmin1,xmax1,ymin1,ymax1,
     & xcross,ycross,xtic,ytic,ntx,nty,itx,ity,xlo,xhi,ylo,yhi,
     & itit,title1,ifont,ilog,iscal,doframe,titlex,titley,ilabel,
     & inumx,inumy,theta,ifitype,ncomp,sval,isval,xwbase,lt2,
     & screen,colplotter,iver,idev)
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
c===NB do not touch xmin,etc if iscalfix=0 (done for jumps only, and this
c=== is done in RDCJDAT)
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
	if(.not.screen) print 1003,ip,iplot,mpos,title1
	if(pon()) write(7,1003) ip,iplot,mpos,title1
      if(discprt) write(8,1003) ip,iplot,mpos,title1
c	if(debug()) print 3661,nrect,krn,(datcop(i),i=1,20)
c	if(debug()) print 205,(xval(i,1),i=1,10),(yval(i,1),i=1,10),
c     &  (xcal(i,1),i=1,10),(ycal(i,1),i=1,10)
c	if(debug()) pause
c DO POSH PLOT IF NOT AUTO
	if(.not.auto) then
c Can now define ICURVD, needed for VHIST simply as:
	   if(ncurvd.gt.0) then
		do 111 i=1,ncurvd
111		icurvd(i)=i
	   endif
	   if(ncurvc.gt.0) then
		do 112 i=1,ncurvc
112		icurvc(i)=i
	   endif
c
c For Toshiba VIDEOTYP()=18 for graphics mode, =3 for alpha mode
	   if(videotyp().ne.18) call VGA
	   iscal=0		!use input xmin etc
c Input values of ixlo etc may be for 1/4 page plot.  To do posh plots
c need whole page so set xlo=-1 (need to keep input values?)
c	   xlo=-1		!NO- used queued values (so LANDSCAP ignored)
c If data from an old queue the landscape format would be ixlo etc= 1400,9500
c 1600,6500 rather than the smaller format now used, so alter then to
c the present landscape default size
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
	call VHIST4(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,xwbase,lt2,inumx,inumy,
     & titlex,titley,ilabel,doframe,idiskq,
     & autplt,draft,itit,title1,cbig,ifont,landscap,fitted,
     & theta,ifitype,ncomp,sval,isval,-2,iver)
	  goto 100		!end of POSH plotting
	endif
c
c AUTOMATIC PLOTTING
	call PLOTAUT(draft,screen,quarter,ifont,csize,thbig,idev,
     & mpos,ip) 	!sets 'thick'=thbig in common for VHISTQ
c
c	   call SETCLS(mono,icol,.false.,.true.)	!set plotter colours
c	   if(idev.eq.4.and.(.not.mono)) then
c		call REVCOL(icol)	!reverse black/white
c	   endif
c
	  if(idev.eq.0) then
	     call setcls(.false.,icol,.false.,plotcols,isetcol)
	  else
	     call setcls(.true.,icol,.false.,plotcols,isetcol)
	  endif
	call VHISTQ(XVAL,YVAL,NDAT,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,ncurvc,iline,ndimc,ILOG,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & xwbase,lt2,isval,sval,titlex,titley,ilabel,doframe,
     & draft,itit,title1,csize,ifont,theta,ifitype,ncomp,quarter,idev,
     & iver,iplot)
c
	goto 90		!page full yet?
c
c
c SINGLE CHANNEL RECORDS
33	continue
c	  =3 for SC when position in CONSAM is queued
c	  =-3 for SC when data itself queued
c	  =31/-31 for SC when data for fitted curve also queued
c	  =32/-32 for SC when data for fitted curve also queued using extra record
c	if(debug()) print 34
c34	format(' reading data')
	call READSCQ(krn,istrec,iptype,useconsam,itit,title1,
     &  ndev,ib,istart,iend,srate,ymin,ymax,calfac,ioff,calfac2,
     &  cdate,adctime,adcf1,isdfst,ndisp1,nrec1,jdat,base,
     &  ntrans,y0,t0,nfilt,dtf,tif1,tif2,dt,stepamp,filt,
     &  ndelt,ffilt,fcz,fczoom)
c Allocate Xval1 etc -PROBLEM -do not know how many points calc curve will
c have until CONV1 calculated -ke <=5120, but i1 added to this in READSQ1,
c and value not known yet -probably 5120 OK usually -not always!!
	ndv1=jdat
	ndimd=1
c	ndc1=jdat
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
	if(.not.screen) print 1003,ip,iplot,mpos,title1
c	if(pon()) write(7,1003) ip,iplot,mpos,title1
      if(discprt) write(8,1003) ip,iplot,mpos,title1
	call flush(7)
c
216	continue		!return here to replot different points from CONSAM
c Now read data and define xval, yval etc in READSCQ, whether auto or not.
	autplt=.true.    !change queue does not have box positions etc
	call READSCQ1(iptype,jdat,intp,krn,nrec1,dx,srate,xmin,xmax,
     & usecons,adcfil,ioff,mulpage,
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
	ifont=4
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
c -default colours NOT used for following values which are defined
c as valid colours
	icol(1)=9	!blue for data set #1
	icol(11)=12	!red for calc curve #1
	icol(25)=0	!black for plot title
c Colours not queued for single channels, so must use default colours
c for now
c
c No SD or theta for single channels, but must allocate arrays before
c calling either vplot5 or vplotq5
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
	if(auto) then
	   if(.not.quarter) then	!set in PLOTAUT for quarter page
	      xlo=0.14*xp
	      xhi=0.95*xp
	      ylo=0.21*yp
	      yhi=0.87*yp
	   endif
	   call PLOTAUT(draft,screen,quarter,ifont,csize,thbig,idev,
     &   mpos,ip) 	!sets 'thick'=thbig in common for VHISTQ
c Colours/positions etc not queued so set now
c	   call SETCLS(mono,icol,.false.,.true.)	!set plotter colours
c	   if(idev.eq.4.and.(.not.mono)) then
c		call REVCOL(icol)	!reverse black/white
c	   endif
	  if(idev.eq.0) then
	     call setcls(.false.,icol,.false.,plotcols,isetcol)
	  else
	     call setcls(.true.,icol,.false.,plotcols,isetcol)
	  endif
	  if(noclogo) y1c=-1
	   if(novlogo) y1v=-1
	   if(joinline) then
		do i=1,ncurvd
		   ijoin1(i)=jtype
		enddo
	   endif
c	   ifstcol(1)=1		!for VPLOTQ (one curve only plotted) (see RDVPLQ)
c  xmin,xmax queued, but others not so need to set values here, as done in
c vplot3 when iscal=2 before calling vplotq
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
c Add auto horizontal amp marker lines
c If there is no fitted curve then draw markers relative to ybase
c  but if there is a fitted curve then look for baseline in it -value
c  for opendown is not queued, but if amplitudes for marker lines are
c  negative then assume openings are downwards
	   if(nampmark.gt.0) then
c First find baseline
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
	  if(idev.eq.0) then
	     call setcls(.false.,icol,.false.,plotcols,isetcol)
	  else
	     call setcls(.true.,icol,.false.,plotcols,isetcol)
	  endif
	  autplt=.true.
	   call VPLOTQ5(XVAL1,YVAL1,ndat1,ncurvd,ijoin1,syms1,ndv1,ndimd,
     &    xcal,ycal,ncal1,ncurvc,iline1,ndc1,ndimc,isym1,ilog,
     &    XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     &    y0,yinf,titlex,titley,ilabel,
     &    doframe,draft,itit,title1,csize,ifont,
     &    theta,ifitype,ncomp,weight,isdev,quarter,iptype,
     &   ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ivplot,
     &    cjump,idev,interp,iplot,idelt,
     &    kwi,kwj,icurvw,kmax,iver)
	   goto 90		!page full yet?
c
	else if(.not.auto) then
	   if(videotyp().ne.18) call VGA
	   autplt=.true.	! change queue does not have box positions etc -use defaults
c  ISCAL=2 if input values of xmin,xmax only to be used; others internally set
	   if(mulpage) then
		itrace=2		!see vplot5
	   endif
	   iscal=2
c	   iscal=0		!use external xmin,xmax etc
c	   iscal=1		!scale internally
	   do i=1,100		!no positions defined from queue
		angle(i)=0
		idraw(i)=-2		!until defined
		rx(i)=0.0		!NB world coord may be neg so cant tell if defined yet
	   enddo
	 autplt=.true.
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
c	   if(.not.useconsam.or.noconsam) goto 100
c	   if(noconsam) goto 100
c
214	   call DCASK(
     &    'Replot with different points taken from CONSAM.DAT','n',ans)
	   if(ans.eq.'Y') then
		if(consamdef) then
		   if(.not.useconsam) usecons=.true.	!data itself queued
		else
		   print 217
217		   format(
     &	    ' Data itself was queued: get CONSAM name/path now')
		   if(adcf1(2:2).eq.':') adcfil(1:30)=adcf1 !path for consam in queue
		   call GETCONSAM(adcfil,noconsam,title,cdate1,adctim1,
     &	    nsam,srate1,idest)
		   if(srate.ne.srate1) then
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
		iopt=1
39		print 35,iopt
35		format(
     & ' (1) Alter start and/or end point of current display',/,
     & ' (2) Plot part or all of CONSAM on multiple pages',/,
     & ' Option number [',i2,'] = ')
		call INPUTi(iopt)
		jdat=iend-istart+1
		samlen=dx*float(jdat)	!sample length (ms)
		if(iopt.eq.2) then
		   tsamlen=dx*float(nsam)	!ms
		   np1=1
		   np2=nsam
361		   print 36,samlen,jdat,istart,iend,nsam,tsamlen*0.001,
     &		np1,np2
36		   format(' At present: display length = ',g13.6,' ms',/,
     &  ' number of points = ',i8,'(from #',i9,' to #',i9,').',/,
     &  ' CONSAM contains ',i10,' points: length = ',g13.6,' seconds',/,
     &  ' Plot from point n1 to n2: n1,n2 [',i9,',',i9,'] = ')
		   call INPUT2i(np1,np2)
		   if(np2.lt.1.or.np2.gt.nsam) goto 361
		   print 37
37		   format(' Number of seconds per page [1.0] = ')
		   secpage=1.0
		   call INPUTr(secpage)
		   secpage=secpage*1000.	!ms/page
		   jdat1=np2-np1+1
		   samlen1=dx*(float(jdat1-1))
		   print 38, 1+ifix(samlen1/secpage)
38		   format(' This corresponds to ',i5,' pages: O.K. [Y] ? ')
		   read 101,ans
		   if(UC(ans).eq.'N') goto 39
		   ntrace=6		!default value
		   ytsep=7.0	!ditto -can adjust it in vplot
		   istart=np1	!to read first page in readsq1
		   jdat=ifixr(secpage/dx)	!points/page
		   iend=istart+jdat-1
		   mulpage=.true.
		   ndv1=jdat		!new value
		   ndat1(1)=jdat
		   ndimd=1
		   ncurvc=0		!no calc lines for multi-page plots
		   ndc1=1
		   ndimc=1
		   DEALLOCATE(Xval1,Yval1,Xcal,Ycal)
		   ALLOCATE(Xval1(ndv1,ndimd),Yval1(ndv1,ndimd),
     &	    Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
c Read first pageful in READSCQ1 but do rest within VPLOT5
		   goto 216		!replot it
		endif
c Now iopt=1 section
		print 210,samlen,jdat,istart,iend
210		format(' At present: display length = ',g13.6,' ms',/,
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
		read 101,ans
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
	      goto 100		!end of POSH plotting
	   endif
	endif			!end of 'if(.not.auto)'
c
c AUTOMATIC PLOTTING
cc If option selected (in PLOTOPT) to rearrange plots, then do it here
c	if(arrange) then
c	   print 903
c903	   format(' Give position for plot (1-4); -1 to skip',/,
c     &  	 ' Position = ')
c	   read 905,mpos
c905	   format (i8)
c	endif
c Plotaut sets xlo etc, and 'thick'=thbig in common, for VPLOTQ.
	call PLOTAUT(draft,screen,quarter,ifont,csize,thbig,idev,
     & mpos,ip) 	!sets 'thick'=thbig in common for VPLOTQ
c
	goto 90		!page full yet?
c

cc Position control for test on screen. Note that position is controlled
c by ixlo,... etc; values are set in PSPACE (rather than values from queue)
c when MSEQ=true. 'Square' graphs (i.e. not half-page) can be shown on
c screen as whole page or 1/4 page.
c For whole page mpos set initially to 5 (and not changed)
c90	if(mpos.eq.5) goto 91
c	mpos=mpos+1
c When page is full must (a) call ENDPLT if draft mode (idev<5), or
c (b)call VTPLOT do do the plots from PLOT99.DAT if ndev=99 (ndev>5 in general)
c (only needed if AUTO; for POSHPLOT this is done within VHIST/VPLOT)
c
90	continue	!for auto plotting
c	pause
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
		   print 71
71		   format(' Entering ENDPLT . . .')
		   if(idev.eq.3.or.idev.eq.4) then
	         	oldfile='EPS.OUT'
		   else if(idev.eq.5) then
	         	oldfile='HPLJR.OUT'
		   else if(idev.eq.6) then
	         	oldfile='HPPJ.OUT'
		   endif
	         INQUIRE(file=oldfile,exist=present,flen=len)
	         if(present) then
c	   		call COPY(oldfile,'lpt1')
	            kp=kp+10
	   		call intconv(kp,kpchar)
	   		nc1=nblank1(kpchar)
	   		printfil='fit'//kpchar(1:nc1)//'.out'
	   		call COPY(oldfile,printfil)
			call COPY (printfil,'lpt1')
			call ERASE(oldfile)
		   endif
		endif
c      	if(.not.screen) write(7,*) char(27)//'&l0H'	!page eject
c      	if(pon()) write(7,*) char(27)//'&l0H'		!page eject
      	if(discprt) write(8,*) char(27)//'&l0H'		!page eject
c7		format('1')		!form feed
		call FLUSH(7)
	   else			!idev=99
c		call ERASCR		!new Hgraph has problem with ERASCR if idev=99?
c device for final plot (ENDPLT called within VTPLOT)
		idev1=5
		if(.not.screen) iview=0	!so not shown on screen first
		call VTPLOT(99,iview,idev1)	!plot PLOT99.DAT
	   endif
	endif
c
100	continue	!end of plot loop
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
	call flush(7)
	pause
	call mode(3)
c
	end

