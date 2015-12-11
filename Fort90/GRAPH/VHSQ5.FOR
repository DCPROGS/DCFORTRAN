	subroutine VHSQ5(xval,yval,xcal,ycal,ndimd,ndimc,
     & ncurvd,ndat,icurvd,ijoin,ncurvc,ncal,icurvc,iline,
     & xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ntx,nty,itx,ity,
     & xlo,xhi,ylo,yhi,itit,title1,ifont,ilog,iscal,doframe,
     & titlex,titley,ilabel,inumx,inumy,idiskq,qfile,sval,isval,
     & theta,ifitype,ncomp,idest,icol,mono,xwbase,lt2,
     & rlth,RX,RY,rxbox,rybox,IJUS,angle,idraw,ifnt,size,
     & cnumx,cnumy,cexpx,cexpy,numbx,numby,narrow,
     & xb,yb,xe,ye,ntext,nline,xlb,ylb,xle,yle,iltype,newtext,
     & ndv1,ndc1,kmax)
c
c Subroutine for VHIST to put data in PLOT QUEUE file (based on VPLQ).
c VHSQ5 is version for VHIST5 with ndv1, ndc1 and kmax added to paameters
c   (NB there is no vhsq4 version!) -now has iptype=21 (not iptype=2) so
c   data queued form this version can be detected in autplot
c VHSQ3 is version for VHIST3/VPLOT3 with Xcal, Ycal increased in size to
c 2048.
c
c Modified 01/24/03 07:13am to increase size of ameant() areat() from 10 to 100
c
c Modified 02/09/93 03:51pm to add ICOL(), mono (and jcol=1002 if colours
c  have been queued)
c Modified 08/12/92 02:18pm so jstrec(), lstrec() both have dimension=200
c Last 3 lines are extra args to hold all details of posh plots.
c 02/06/90 05:57pm ntx,..,ity added
c  (1) Arguments removed ipen,mpos,mlast,ivel,
c	ipdis,isup,nlenx,nleny
	real*4 DATCOP(:)
	integer*2 INT2COP(:)
	character*10 CHARCOP(:)
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character cnum1*11,cnum2*11
      ALLOCATABLE :: DATCOP,INT2COP,CHARCOP
c
	real XVAL(0:ndv1,ndimd),YVAL(0:ndv1,ndimd)
	real XCAL(ndc1,ndimc),YCAL(ndc1,ndimc)
c for histo data
	integer ndat(ndimd),icurvd(ndimd),ijoin(ndimd)
c for calc curves
	integer ncal(ndimc),icurvc(ndimc),iline(ndimc)
	real theta(kmax)
	dimension ndat1(10),ncal1(10),ijoin1(10),iline1(10)  !for queue
	integer*2 jstrec(200),lstrec(200)		!for queue
c	integer*4 jstrec1(100),lstrec1(100)		!for old queue
	character*1 ans,UC
	character*40 qfile
	character ndev*2
c	character filnam*13,infil*15
	character*40 titlex,titley
	character*64 title1,title2	!plot title (local)
	LOGICAL doframe,present,newfil,mono
	logical discprt
	logical caplock,debug
c extra args to hold all details of posh plots
	integer*1 idum(1)		!to fill file to length that is multiple of 1024

	real RX(100),RY(100)
	real RLTH(100) 		!for line thickness
	real rxbox(4,100),rybox(4,100),angle(100),size(30)
	integer IJUS(100)
	integer idraw(100),icol(100)
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25)
	integer ifnt(30)

	real xb(10),yb(10),xe(10),ye(10)		!up to 10 arrows
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	integer iltype(10)				!line type for ditto
	character*80 newtext(20)		!extra text
c integer*2 copies to save disc space
	integer*2 IJUS1(100)
	integer*2 idraw1(100),icol1(100)
	common/ptext/ameant(100),areat(100)	   !in partext, scvdu, vhsq5, rdvhsq
c
	common/dp/discprt
c
	debug()=caplock()
c
3	format(i8)
	idest=0
	iver=1100
	ict=13	!text colour for DIALOG box
	if(mono) ict=15
	idum(1)=0		!to fill file to length that is multiple of 1024
      ALLOCATE(datcop(100000),int2cop(1000),charcop(100))

c
c Make integer*2 copies to save disc space
	do 50 j=1,100
	 IJUS1(j)=IJUS(j)

	 idraw1(j)=idraw(j)
	 icol1(j)=icol(j)
50	continue
c
c	lastrow=2			!NB args of SCROLL=integer
c	lrow=int2(lastrow)	!but args of LOCATE are int*2
c
	if(ncurvd.eq.0) goto 362
	do 32 i=1,ncurvd
	j=icurvd(i)
	ndat1(i)=ndat(j)
	ijoin1(i)=ijoin(j)
32	continue
362	continue
	if(ncurvc.eq.0) goto 363
	do 33 i=1,ncurvc
	j=icurvc(i)
	ncal1(i)=ncal(j)
	iline1(i)=iline(j)
33	continue
363	continue
c
c Make compressed data records
	k=0		!index for datcop
	if(ncurvd.le.0) goto 365
	do 37 j1=1,ncurvd
	j=icurvd(j1)
	n=ndat(j)+2
	   do 371 i=0,n
	   k=k+1
371	   datcop(k)=Xval(i,j)
	   do 372 i=0,n
	   k=k+1
372	   datcop(k)=Yval(i,j)
37	continue
365	continue
	if(ncurvc.le.0) goto 366
	do 39 j1=1,ncurvc
	j=icurvc(j1)
	n=ncal(j)
	   do 391 i=1,n
	   k=k+1
391	   datcop(k)=xcal(i,j)
	   do 392 i=1,n
	   k=k+1
392	   datcop(k)=ycal(i,j)
39	continue
366	continue
c
c Now add onto DATCOP the real*4 data for posh plots- keep incrementing k
c
	do 682 i=1,10
	   k=k+1
	   DATCOP(k)=size(i)
         k=k+1
         DATCOP(k)=angle(i)
c	if(debug()) print 800,i,csfac(i),k,datcop(k)
c800	format(i8,g13.6,5x,i8,g13.6)
	   k=k+1
	   DATCOP(k)=rx(i)
	   k=k+1
	   DATCOP(k)=ry(i)
	   do 680 j=1,4
	     k=k+1
	     DATCOP(k)=rxbox(j,i)
	     k=k+1
	     DATCOP(k)=rybox(j,i)
680	   continue
682	continue		!keep 1-10 always
c
	if(ntext.gt.0) then
	do 68 i1=1,ntext
	   i=i1+10
	   k=k+1
	   DATCOP(k)=size(i)
           k=k+1
         DATCOP(k)=angle(i)
	   k=k+1
	   DATCOP(k)=rx(i)
	   k=k+1
	   DATCOP(k)=ry(i)
	   do 681 j=1,4
	     k=k+1
	     DATCOP(k)=rxbox(j,i)
	     k=k+1
	     DATCOP(k)=rybox(j,i)
681	   continue
68	continue
	endif
	do 69 i1=1,numbx
	   i=i1+30
           k=k+1
         DATCOP(k)=angle(i)
	   k=k+1
	   DATCOP(k)=rx(i)
	   k=k+1
	   DATCOP(k)=ry(i)
	   do 691 j=1,4
	     k=k+1
	     DATCOP(k)=rxbox(j,i)
	     k=k+1
	     DATCOP(k)=rybox(j,i)
691	   continue
69	continue
	do 70 i1=1,numby
	   i=i1+55
           k=k+1
         DATCOP(k)=angle(i)
	   k=k+1
	   DATCOP(k)=rx(i)
	   k=k+1
	   DATCOP(k)=ry(i)
	   do 701 j=1,4
	     k=k+1
	     DATCOP(k)=rxbox(j,i)
	     k=k+1
	     DATCOP(k)=rybox(j,i)
701	   continue
70	continue
	if(narrow.gt.0) then
	   do 71 i=1,narrow
	   k=k+1
	   DATCOP(k)=xb(i)
	   k=k+1
	   DATCOP(k)=xe(i)
	   k=k+1
	   DATCOP(k)=yb(i)
	   k=k+1
	   DATCOP(k)=ye(i)
71	   continue
	endif
	if(nline.gt.0) then
	   do 72 i=1,nline
	   k=k+1
	   DATCOP(k)=xlb(i)
	   k=k+1
	   DATCOP(k)=xle(i)
	   k=k+1
	   DATCOP(k)=ylb(i)
	   k=k+1
	   DATCOP(k)=yle(i)
72	   continue
	endif
        do 60 i=1,30	!keep 1st 30 RLTH always
	   k=k+1
	   DATCOP(k)=RLTH(i)
60	continue
	if(ntext.gt.0) then
	   do 61 i=1,ntext
	   k=k+1
	   DATCOP(k)=RLTH(i+30)
61	   continue
	endif
	if(narrow.gt.0) then
	   do 62 i=1,narrow
	   k=k+1
	   DATCOP(k)=RLTH(i+50)
62	   continue
	endif
	if(nline.gt.0) then
	   do 63 i=1,nline
	   k=k+1
	   DATCOP(k)=RLTH(i+60)
63	   continue
	endif
c end of DATCOP
	k1=k
	nbyte1=4*k
c
c Make compressed array for new text
	k2=ntext
	nbyte2=k2*80

c
c and for CNUMX etc
	k=0
	do 73 i=1,numbx
	k=k+1
	CHARCOP(k)=cnumx(i)
	k=k+1
	CHARCOP(k)=cexpx(i)
73	continue
c
	do 74 i=1,numby
	k=k+1
	CHARCOP(k)=cnumy(i)
	k=k+1
	CHARCOP(k)=cexpy(i)
74	continue
	k3=k
	nbyte3=10*k
c
c make compressed array for int*2 values
	k=0		!index for int2cop
	do 64 i=1,10
	   k=k+1
	   INT2COP(k)=ijus1(i)
	   k=k+1
	   INT2COP(k)=idraw1(i)
64	continue
	if(ntext.gt.0) then
	   do 65 i=1,ntext
	   k=k+1
	   INT2COP(k)=ijus1(i+10)
	   k=k+1
	   INT2COP(k)=idraw1(i+10)
65	   continue
	endif
	do 66 i=1,numbx
	   k=k+1
	   INT2COP(k)=ijus1(i+30)
	   k=k+1
	   INT2COP(k)=idraw1(i+30)
66	continue
	do 67 i=1,numby
	   k=k+1
	   INT2COP(k)=ijus1(i+55)
	   k=k+1
	   INT2COP(k)=idraw1(i+55)
67	continue
c
c Now store ICOL() array (at end if INT2COP, so read only for versions
c that have colour)
	do i=1,30	!keep 1st 30 ICOL always
	   k=k+1
	   INT2COP(k)=icol1(i)
	enddo
	if(ntext.gt.0) then
	    do i=1,ntext
	      k=k+1
	      INT2COP(k)=icol1(i+30)
	    enddo
	endif
	if(narrow.gt.0) then
	    do i=1,narrow
	      k=k+1
	      INT2COP(k)=icol1(i+50)
	    enddo
	endif
	if(nline.gt.0) then
	    do i=1,nline
	      k=k+1
	      INT2COP(k)=icol1(i+60)
	    enddo
	endif
c record background colour
	k=k+1
	INT2COP(k)=icol1(71)
c
	k4=k
	nbyte4=k*2	!no of bytes in INT2COP
c End of int2cop
c
c== Total number of records
c Total number of bytes
c First bit actually occupies 552 bytes at present, but leave gap after
c this before writing int2cop etc, in case it is necessary to add other
c things that have to be read first.
c	nbytet=552        !'first record'
c now have 3 extra integers =12 + lrger theta=4+30=120 i,e 552+12+120=684
c	ioffset=768
	ioffset=1024
	nbytet=ioffset    !'first record'
	nbytet=nbytet+nbyte1+nbyte2+nbyte3+nbyte4
c Calculate the number of 1024 byte records that this would occupy if
c fixed record length had been use.
c
	nrect=1 + (nbytet-1)/1024	!for everything
c
c OPEN DISC FILE
441	continue
	newfil=.false.
	call GBLANK(qfile,40,n1,n2)
	if(n1.eq.0.and.n2.eq.0) idiskq=-1	!qfile not defined
	if(idiskq.ge.1) then	  !qfile already defined
	   ans='Y'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'Queue in '//charnb(qfile)//' O.K.',
     &	   defolt,ict,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'Y') then
		ndev=qfile(1:1)
c=	      ndev=char(idiskq+64)//':'	!see def of idiskq below
	      if(ndev.eq.'A:'.or.ndev.eq.'B:') then
	         ans='Y'
	         call DEFOLTa(ans,defolt)
	         call QDIALOG(1,'Is disc '//ndev//' mounted',
     &	      defolt,ict,cans)
	         call GETINPa(cans,ans)
		   if(ans.eq.'N') goto 25          !specify disc
		endif
		goto 48
	   endif
	endif
c Next bit when qfile not yet defined (or A: not mounted)
	qfile='c:\PLOTQ.plq'	!default
25	continue
c=	call DISCDIALOG(1,'Specify disc for plot queue file',
c=     &	idiskq,ndev,-1,1,ict)
c=	if(idiskq.eq.-1) goto 99	!if ESC hit then do not queue anything
	call TDIALOG(1,'Give path and name for queue file',
     &  qfile,40,.false.,ict)
	idiskq=ichar(qfile(1:1)) - 64
	if(idiskq.lt.1) idiskq=1  !value not needed now -just signals when qfile set
48	continue
c=	qfile=ndev//filnam
	INQUIRE(file=qfile,exist=present,flen=nlen)
	if(.not.present.or.nlen.eq.0) then
	   ans='Y'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     & 'Queue file '//CHARNB(qfile)//' does not exist: create it',
     &    defolt,ict,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'N') goto 4081
c
	   nplot=0
	   do 482 i=1,200
	   jstrec(i)=0
482	   lstrec(i)=0
         OPEN(unit=12,file=qfile,status='UNKNOWN',access='TRANSPARENT',
     &   err=302)
c         OPEN(unit=12,file=qfile,status='UNKNOWN',
c     &    access='DIRECT',form='UNFORMATTED',recl=1024,err=302)
	   write(12,rec=1) nplot,jstrec,lstrec,iver
	   newfil=.false.		!file now exists
	else
c         OPEN(unit=12,file=qfile,status='UNKNOWN',
c     &    access='DIRECT',form='UNFORMATTED',recl=1024,err=302)
         OPEN(unit=12,file=qfile,status='UNKNOWN',access='TRANSPARENT',
     &   err=302)
	endif
c Now use first record for nplot etc
c The array jstrec(200) contains in jstrec(nplot) the start record
c for plot #nplot, and in lstrec(nplot) the last record number for it
c ###For floppy at least, add name,jfirst etc as for SCDIR so each person
c can have floppies with own posh plots
	read(12,rec=1) nplot,jstrec,lstrec,iver1
	if(iver1.ne.1100) then
	   call WDIALOG(1,
     &	'Plot queue has old format: TRY AGAIN',12)
c	   call CONVQREC(jstrec,jstrec1,lstrec,lstrec1,12,1)
	   close(unit=12)
         goto 25
	endif
	if(.not.newfil) then	!if(newfil) nplot already specified
	 if(nplot.eq.0) then	!initialise to 0 in PLTQINIT or above
	   iplot=1
	   jstrec(1)=2
c	   istrec=2
	   istrec=1025		!for transparent write
	 else
c	   istrec=int4(lstrec(nplot))+1	!start record for current plot
	   istrec=int4(lstrec(nplot))*1024+1  !start record for transparent write
	   iplot=nplot+1		!next number
	 endif
	else		!if newfil
c	   istrec=int4(lstrec(nplot)+1)	!start record for current plot
	   istrec=int4(lstrec(nplot))*1024+1  !start record for transparent write
	   iplot=nplot+1		!next number
	endif
4041	continue
	call INTCONV(iplot,cnum1)
	ans='Y'
	call DEFOLTa(ans,defolt)
	if(.not.newfil) then
	   call QDIALOG(1,
     & 'Next queue number = '//CHARNB(cnum1)//' in '//CHARNB(qfile)//
     &   ': O.K.',defolt,ict,cans)
	else if(newfil) then
	   call QDIALOG(1,
     & 'Queue number = '//CHARNB(cnum1)//' in '//CHARNB(qfile)//
     &   ': O.K.',defolt,ict,cans)
	endif
	call GETINPa(cans,ans)
c101	format(a1)
	if(ans.ne.'N') then		!O.K.
	   if(iplot.le.nplot) goto 405	!overwriting existing record
	   nplot1=iplot		!value to be written to 1st record
	   goto 406
	endif
c
c Bit done if want to change (1)plot number,(2)disc or (3)plot queue file name
c
	newfil=.false.
4081	continue
	iopt=1
408	call DEFOLTi(iopt,defolt)
	call WDIALOG(1,
     & 'Change (1) the queue number (2) name of queue file',ict)
	call QDIALOG(1,
     & '(3) abandon queueing; (4) O.K. queue now. Option #',
     &  ' ',ict,cans)
	call GETINPi(cans,iopt)
	if(iopt.eq.1) then
	   call INTCONV(iplot,cnum1)
	   call QDIALOG(1,
     &    'Queue number (up to '//CHARNB(cnum1)//')',' ',ict,cans)
	   call GETINPi(cans,i)
	   if(i.ge.1) iplot=i
	else if(iopt.eq.2) then
	   call TDIALOG(1,'Give path and name for queue file',
     &     qfile,40,.false.,ict)
	   newfil=.true.
	   CLOSE(unit=12)
	   goto 48		!Open new file and check if OK now?
	else if(iopt.eq.3) then
	   goto 45		!abandon
	else if(iopt.ne.4) then
	   iopt=4		!new defolt
	   goto 408		!ask again
	endif
c Queue file etc now defined: do next bit if old file is being overwritten
405	continue
	if(iplot.eq.1) jstrec(1)=2	!new start record (rec 1=jstrec etc now)
	istrec=1 + (jstrec(iplot)-1)*1024	!start record transparent
	nplot1=iplot		!value to be written to 1st record
c Allow earlier plot to be overwritten in POSHPLOT.DAT only after query
	if(iplot.lt.nplot) then		!overwriting earlier plot
	   nr1=(lstrec(iplot)-jstrec(iplot))+1
c nr1=no of 1024 byte records for plot to be overwritten
c	   read(12,rec=istrec) iptype,itit,title2
	   read(12,rec=istrec) iptype,ndimd2,ndv12,ndimc2,ndc12,kmax2,
     &	itit2,title2
	   if(nrect.le.nr1) then
		ans='Y'
	      call DEFOLTa(ans,defolt)
	      call QDIALOG(1,'Will overwrite '//CHARNB(title2)//'  O.K.',
     &	   defolt,ict,cans)
	      call GETINPa(cans,ans)
		if(ans.eq.'N') then
		   idiskq=-1
		   goto 441
		endif
	   else
		call BELL(2)
	      call QDIALOG(1,'Will overwrite next plot too: O.K.',
     &	   ' ',ict,cans)
	      call GETINPa(cans,ans)
		if(UC(ans).eq.'N') then
		   idiskq=-1
		   goto 441
		endif
	   endif
	   call INTCONV(nplot,cnum1)
	   call INTCONV(iplot,cnum2)
	   ans='N'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'Reduce last plot # from '//CHARNB(cnum1)//
     &	' to '//CHARNB(cnum2)//' ',defolt,ict,cans)
	   call GETINPa(cans,ans)
 	   if(ans.eq.'Y') then
		nplot1=iplot  !to write to 1st record
	   else
		nplot1=nplot
	   endif
	endif
406	continue
c Check that data will fit in: make max file size=1423 records which is
c most that will fit on 3.5 inch floppy (1457664 bytes usable, and
c 1457664/1024=1423.5   (1457664/2048=711.75)
c	lastr=istrec+nrect-1		!last record for current plot
c	if(lastr.gt.1423) then
	lastr=istrec+nbytet-1		!last record (byte #) for current plot
c To retain compatability with queues written in 1024 byte records, must
c round this up to the nearest integer multiple for 1024 (see nfill, below)
	lastr1=lastr		!rounded up
	n=mod(lastr,1024)
	if(n.ne.0) then
	   nfill=1024-n
	   lastr1=lastr+nfill
	endif
c====
c	if(lastr1.gt.1457664) then
c	   call BELL(2)
c	   call WDIALOG(1,
c     &    'Not enough room in queue file: start on another disc:',12)
c	   idiskq=-1
c	   goto 441
c	endif
c=====
	if(lastr1.gt.1457664.and.ndev.eq.'A:') then
	   call BELL(2)
	   ans='Y'
	   call DEFOLTa(ans,defolt)
     	   call QDIALOG(1,
     & 	'Plot queue will be too big for floppy disc'//' O.K.',
     &	defolt,ict,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'N') then
		idiskq=-1
		goto 441
	   endif
	endif
c	jstrec(iplot)=(istrec)	!set value for current plot
c	lstrec(iplot)=(lastr)	!set value for current plot
c Set jstrec(), lstrec() for current plot, defined in terms of 1024 byte
c records, from istrec,lastr which are defined in bytes for
c transparent read/write
	jstrec(iplot)=1 + ((istrec-1)/1024)
	lstrec(iplot)=jstrec(iplot)+(nrect) - 1
	lstrec(iplot)=1 + ((lastr-1)/1024)	!should be same!
c Update rec #1 -now done AFTER data written so nplot does not get
c altered if there is some problem in writing the data (FIXPLQ1.FOR can
c alter nplot etc)
c	write(12,rec=1) nplot1,jstrec,lstrec
c
c Things to go in 1st record of each plot (Nplot copied into rec 1 also
c so it can be retrieved)
c IPTYPE=1 for VPLOT1
c	=2 for VHIST
c	=3 for PLOTSC
c	iptype=2	!for VHIST
c	iptype=21	!for VHIST5
c latest iptype=22 version (with ameant, areat arrays for simultaneous
c fits with constrained tau)
	iptype=22	!for VHIST5
c
	krn=istrec
c
c (remove from write: mpos,mlast,ixlo,...,iyhi,ipdis,isup,flo,fhi,
c lt1,ivel,plot;  add doframe
c	if(debug()) print 121,iptype,itit,title1,xmin,xmax,ymin,ymax,
c     & xcross,ycross,xtic,ytic,csize,ifont,ilog,iscal,
c     & doframe,titlex,titley,ilabel,ndimd,ndimc,krn
c121	format(' iptype,itit,title1,xmin,xmax,ymin,ymax,',/,
c     & ' xcross,ycross,xtic,ytic,csize,ifont,ilog,iscal,',/,
c     & ' doframe,titlex,titley,ilabel,ndimd,ndimc,krn',2i8,/,
c     & 1x,a44,/,
c     & 8g10.3,/,g10.3,3i8,l4,/,1x,a40,/,1x,a40,/,4i8)
c
	jcol=1002		!colour queued
c old (iptype=2) version
c	write(12,rec=krn) iptype,itit,title1,xmin,xmax,ymin,ymax,
c     & xcross,ycross,xtic,ytic,xlo,xhi,ylo,yhi,ifont,ilog,
c     & iscal,doframe,titlex,titley,ilabel,ndimd,ndimc,ncurvd,ndat1,
c     & ncurvc,ncal1,ijoin1,iline1,inumx,inumy,sval,isval,
c     & theta,ifitype,ncomp,numbx,numby,ifnt,ntext,narrow,
c     & nline,iltype,xwbase,lt2,ntx,nty,itx,ity,k1,k2,k3,k4,
c     & jcol,mono,ioffset
c newer (iptype=21) version
	write(12,rec=krn) iptype,ndimd,ndv1,ndimc,ndc1,kmax,
     & itit,title1,xmin,xmax,ymin,ymax,
     & xcross,ycross,xtic,ytic,xlo,xhi,ylo,yhi,ifont,ilog,
     & iscal,doframe,titlex,titley,ilabel,ncurvd,ndat1,
     & ncurvc,ncal1,ijoin1,iline1,inumx,inumy,sval,isval,
     & theta,ifitype,ncomp,numbx,numby,ifnt,ntext,narrow,
     & nline,iltype,xwbase,lt2,ntx,nty,itx,ity,k1,k2,k3,k4,
     & jcol,mono,ioffset
c latest iptype=22 version (with ameant, areat arrays for simultaneous
c fits with constrained tau)
	write(12,rec=krn) iptype,ndimd,ndv1,ndimc,ndc1,kmax,
     & itit,title1,xmin,xmax,ymin,ymax,
     & xcross,ycross,xtic,ytic,xlo,xhi,ylo,yhi,ifont,ilog,
     & iscal,doframe,titlex,titley,ilabel,ncurvd,ndat1,
     & ncurvc,ncal1,ijoin1,iline1,inumx,inumy,sval,isval,
     & theta,ifitype,ncomp,numbx,numby,ifnt,ntext,narrow,
     & nline,iltype,xwbase,lt2,ntx,nty,itx,ity,k1,k2,k3,k4,
     & jcol,mono,ioffset,
     & (ameant(i),i=1,ncomp),(areat(i),i=1,ncomp)
c
	if(jcol.ne.1002) then
	   mono=.true.		!not queued
	endif
c
c Now write NEWTEXT,CHARCOP,INT2COP and DATCOP to disk
	istr1=istrec+ioffset
	if(ntext.eq.0) then
	   write(12,rec=istr1) (INT2COP(i),i=1,k4),
     &  (DATCOP(i),i=1,k1),(CHARCOP(i),i=1,k3)
	else
	   write(12,rec=istr1) (INT2COP(i),i=1,k4),
     &  (DATCOP(i),i=1,k1),(CHARCOP(i),i=1,k3),(NEWTEXT(i),i=1,ntext)
	endif
c NOTE -if this file gets read as fixed length, 1024 byte, records, its
c length must be an integer multiple of 1024 bytes, or attempt to read the
c last record will give an error (see TDAT.FOR) -so now write dummy data
c to make sure that this is so.
	INQUIRE(file=qFILe,flen=nlen)
	n=mod(nlen,1024)
	if(n.ne.0) then
	   nfill=1024-n
	   write(12,rec=nlen+1) (idum(1),i=1,nfill)
	endif
c

c Update rec #1 (after data written safely!)
	write(12,rec=1) nplot1,jstrec,lstrec,iver
c
	call INTCONV(iplot,cnum1)
	call INTCONV(istrec,cnum2)
	call WDIALOG(1,
     & 'Plot queue # '//CHARNB(cnum1)//': start byte # '//
     &	CHARNB(cnum2)//' on disc '//ndev,ict)
	call WDIALOG(1,CHARNB(title1),ict)
c
      if(discprt) write(8,46)iplot,istrec,lastr,qfile,
     & jstrec(iplot),lstrec(iplot),title1
46	format(/,
     & ' Plot queue no ',i3,' in bytes ',i8,' to ',i8,' in ',a30,/,
     & ' (i.e. 1024 byte records numbers ',i5,' to ',i5,')',/,
     & /,1x,a44,'**************************')
c	if(debug()) pause
	call flush(7)
45	CLOSE(UNIT=12)
99	continue
	DEALLOCATE(datcop,int2cop,charcop)
	RETURN		!return to screen coordinates?
c
C ERROR SECTION
302	continue
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row lrow,col 0
c	print 403
c403	format('&Error in opening PLOTQ file: try again ? ')
c	read 101,ans
	call BELL(1)
      call QDIALOG(1,'Error in opening PLOTQ file: try again',
     &   ' ',ict,cans)
      call GETINPa(cans,ans)
	newfil=.false.
	if(ans.eq.'Y') goto 4081
	idest=1551
	RETURN
c
	end


