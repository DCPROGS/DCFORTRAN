	subroutine VHSQ1(xval,yval,xcal,ycal,ndimd,ndimc,
     & ncurvd,ndat,icurvd,ijoin,ncurvc,ncal,icurvc,iline,
     & xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ntx,nty,itx,ity,
     & ixlo,ixhi,iylo,iyhi,itit,title1,csize,ifont,ilog,iscal,doframe,
     & titlex,titley,ilabel,inumx,inumy,idiskq,sval,isval,
     & theta,ifitype,ncomp,idest,icol,mono,xwbase,lt2,
     & ilth,RX,RY,rxbox,rybox,IXJUS,IYJUS,iangle,idraw,ifnt,
     & csfac,cnumx,cnumy,cexpx,cexpy,numbx,numby,narrow,
     & xb,yb,xe,ye,ntext,nline,xlb,ylb,xle,yle,iltype,newtext)
c
c Subroutine for VHIST to put data in PLOT QUEUE file (based on VPLQ).
c Modified 02/09/93 03:51pm to add ICOL(), mono (and jcol=1002 if colours
c  have been queued)
c Modified 08/12/92 02:18pm so jstrec(), lstrec() both have dimension=200
c Last 3 lines are extra args to hold all details of posh plots.
c 02/06/90 05:57pm ntx,..,ity added
c  (1) Arguments removed ipen,mpos,mlast,ivel,
c	ipdis,isup,nlenx,nleny
	real*4 DATCOP(26960)
	integer*2 INT2COP(1000)
	character*10 CHARCOP(100)
c
	real XVAL(0:511,ndimd),YVAL(0:511,ndimd)
	real XCAL(512,ndimc),YCAL(512,ndimc)
c for histo data
	integer ndat(ndimd),icurvd(ndimd),ijoin(ndimd)
c for calc curves
	integer ncal(ndimc),icurvc(ndimc),iline(ndimc)
	real theta(20)
	dimension ndat1(10),ncal1(10),ijoin1(10),iline1(10)  !for queue
	integer*2 jstrec(200),lstrec(200)		!for queue
	integer*4 jstrec1(100),lstrec1(100)		!for old queue
	integer*2 lrow
	character*1 ans,UC
	character ndev*2,filnam*13,infil*15
	character*40 titlex,titley
	character*44 title1	!plot title (local)
	LOGICAL doframe,slock,pon,present,newfil,mono
	logical discprt
	logical caplock,debug
c extra args to hold all details of posh plots
	integer*2 ilth(100) 		!for line thickness
	real RX(100),RY(100)
	real rxbox(4,100),rybox(4,100)
	integer IXJUS(100),IYJUS(100)
	integer iangle(100),idraw(100),icol(100)
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25)
	integer ifnt(30)
	real csfac(30)
	real xb(10),yb(10),xe(10),ye(10)		!up to 10 arrows
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	integer iltype(10)				!line type for ditto
	character*80 newtext(20)		!extra text
c integer*2 copies to save disc space
	integer*2 IXJUS1(100),IYJUS1(100)
	integer*2 iangle1(100),idraw1(100),icol1(100)
c
	common/dp/discprt
c
	pon()=slock()
	debug()=caplock()
c
3	format(i8)
	idest=0
	iver=1001
c
c Make integer*2 copies to save disc space
	do 50 j=1,100
	 IXJUS1(j)=IXJUS(j)
	 IYJUS1(j)=IYJUS(j)
	 iangle1(j)=iangle(j)
	 idraw1(j)=idraw(j)
	 icol1(j)=icol(j)
50	continue
c
	lastrow=2			!NB args of SCROLL=integer
	lrow=int2(lastrow)	!but args of LOCATE are int*2
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
	   DATCOP(k)=csfac(i)
	if(debug()) print 800,i,csfac(i),k,datcop(k)
800	format(i8,g13.6,5x,i8,g13.6)
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
	   DATCOP(k)=csfac(i)
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
	do 60 i=1,30	!keep 1st 30 ILTH always
	   k=k+1
	   INT2COP(k)=ilth(i)
60	continue
	if(ntext.gt.0) then
	   do 61 i=1,ntext
	   k=k+1
	   INT2COP(k)=ilth(i+30)
61	   continue
	endif
	if(narrow.gt.0) then
	   do 62 i=1,narrow
	   k=k+1
	   INT2COP(k)=ilth(i+50)
62	   continue
	endif
	if(nline.gt.0) then
	   do 63 i=1,nline
	   k=k+1
	   INT2COP(k)=ilth(i+60)
63	   continue
	endif
c
	do 64 i=1,10
	   k=k+1
	   INT2COP(k)=ixjus1(i)
	   k=k+1
	   INT2COP(k)=iyjus1(i)
	   k=k+1
	   INT2COP(k)=iangle1(i)
	   k=k+1
	   INT2COP(k)=idraw1(i)
64	continue
	if(ntext.gt.0) then
	   do 65 i=1,ntext
	   k=k+1
	   INT2COP(k)=ixjus1(i+10)
	   k=k+1
	   INT2COP(k)=iyjus1(i+10)
	   k=k+1
	   INT2COP(k)=iangle1(i+10)
	   k=k+1
	   INT2COP(k)=idraw1(i+10)
65	   continue
	endif
	do 66 i=1,numbx
	   k=k+1
	   INT2COP(k)=ixjus1(i+30)
	   k=k+1
	   INT2COP(k)=iyjus1(i+30)
	   k=k+1
	   INT2COP(k)=iangle1(i+30)
	   k=k+1
	   INT2COP(k)=idraw1(i+30)
66	continue
	do 67 i=1,numby
	   k=k+1
	   INT2COP(k)=ixjus1(i+55)
	   k=k+1
	   INT2COP(k)=iyjus1(i+55)
	   k=k+1
	   INT2COP(k)=iangle1(i+55)
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
c Total number of records
	nrec1=1 + (nbyte1-1)/1024	!for DATCOP
	nrect=1+nrec1			!+1 for 1st record always written
	if(ntext.eq.0) then
	   if(nbyte3+nbyte4.le.1024) then
		nrect=nrect+1
	   else
		nrect=nrect+2
	   endif
	else if(ntext.gt.0) then
	   if(nbyte2+nbyte3+nbyte4.le.1024) then
		nrect=nrect+1
	   else if(nbyte2+nbyte3.le.1024) then
		nrect=nrect+2
	   else if(nbyte2.le.1024.and.(nbyte3+nbyte4.le.1024))then
		nrect=nrect+2
	   else if(nbyte2.le.1024)then
		nrect=nrect+3
	   else
		nrect=nrect+4
	   endif
	endif
c
c OPEN DISC FILE
441	continue
	newfil=.false.
	filnam='\PLOTQ.DAT'
	if(idiskq.ge.1) then	  !already defined
	   ndev=char(idiskq+64)//':'	!see def of idiskq below
	   if(ndev.eq.'A:'.or.ndev.eq.'B:') then
		call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
		call LOCATE(lrow,0)		!row lrow,col 0
		print 412,ndev
412		format('&Queue on disc ',a2,'  O.K. [Y] ? ')
		read 101,ans
		if(UC(ans).eq.'N') goto 25          !specify disc
		call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
		call LOCATE(lrow,0)		!row lrow,col 0
		print 413,ndev
413		format('&Is disc ',a2,'  mounted [Y] ? ')
		read 101,ans
		if(UC(ans).eq.'N') goto 25          !specify disc
	   endif
	   goto 48
	endif
25	continue
	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
	call LOCATE(lrow,0)		!row lrow,col 0
	print 410
410	format('&Specify disc for plot queue file')
	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
	call LOCATE(lrow,0)		!row lrow,col 0
	call DISCNUM1(idiskq,ndev,-4,1)   !iask=-4 suppresses new line
	if(idiskq.eq.-1) goto 99	!if ESC hit then do not queue anything
48	continue
	infil=ndev//filnam
	INQUIRE(file=INFIL,exist=present,flen=nlen)
	if(.not.present.or.nlen.eq.0) then
	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
	   call LOCATE(lrow,0)		!row lrow,col 0
	   print 481,infil
481	   format('&Queue file ',a15,' does not exist: create it [Y] ? ')
	   read 101,ans
c	   if(UC(ans).eq.'N') goto 25
	   if(UC(ans).eq.'N') goto 4081
	   nplot=0
	   do 482 i=1,200
	   jstrec(i)=0
482	   lstrec(i)=0
         OPEN(unit=12,file=INFIL,status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=1024,err=302)
	   write(12,rec=1) nplot,jstrec,lstrec,iver
	else
         OPEN(unit=12,file=INFIL,status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=1024,err=302)
	endif
c Now use first record for nplot etc
c The array jstrec(200) contains in jstrec(nplot) the start record
c for plot #nplot, and in lstrec(nplot) the last record number for it
c ###For floppy at least, add name,jfirst etc as for SCDIR so each person
c can have floppies with own posh plots
	read(12,rec=1) nplot,jstrec,lstrec,iver1
	if(iver1.ne.1001) then
	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
	   call LOCATE(lrow,0)		!row lrow,col 0
	   print 217
217	   format('&Plot queue has old format: converted to new')
	   call CONVQREC(jstrec,jstrec1,lstrec,lstrec1,12,1)
	endif
	if(.not.newfil) then	!if(newfil) nplot already specified
	 if(nplot.eq.0) then	!initialise to 0 in PLTQINIT or above
	   iplot=1
	   jstrec(1)=2
	   istrec=2
	 else
	   istrec=int4(lstrec(nplot)+1)	!start record for current plot
	   iplot=nplot+1		!next number
	 endif
	else		!if newfil
	   istrec=int4(lstrec(nplot)+1)	!start record for current plot
	   iplot=nplot+1		!next number
	endif
4041	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
	call LOCATE(lrow,0)		!row lrow,col 0
	if(.not.newfil) then
	  print 404,iplot,infil,istrec
404	  format(
     & '&Next queue number= ',i3,' in ',a15,' (rec #= ',i5,
     & ' ): O.K. [Y] ? ')
	else if(newfil) then
	  print 400,iplot,infil,istrec
400	  format(
     & '&Queue number= ',i3,' in ',a15,' (rec #= ',i5,
     & ' ): O.K. [Y] ? ')
	endif
	read 101,ans
101	format(a1)
	if(UC(ans).ne.'N') then		!O.K.
	   if(iplot.le.nplot) goto 405	!overwriting existing record
	   nplot1=iplot		!value to be written to 1st record
	   goto 406
	endif
c
c Bit done if want to change (1)plot number,(2)disc or (3)plot queue file name
c
	newfil=.false.
4081	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
	call LOCATE(lrow,0)		!row lrow,col 0
	print 408
408	format(
     &'&Change (1)queue # (2)disc (3)queue file name, or (4)abandon:')
	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
	call LOCATE(lrow,0)		!row lrow,col 0
	print 409
409	format(
     &'& Option number (1,2,3,4) [hit <enter> if O.K. now]: = ')
	read 3,iopt
	if(iopt.lt.0.or.iopt.gt.4) goto 4081
	if(iopt.eq.0) goto 411		!all changes done
	if(iopt.eq.4) goto 45
	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
	call LOCATE(lrow,0)		!row lrow,col 0
	if(iopt.eq.1) then
	   print 51,nplot
51	   format('&Queue number (up to',i3,' ) = ')
	   read 3,i
	   if(i.ge.1) iplot=i
	else if(iopt.eq.2) then
	   call DISCNUM1(idiskq,ndev,-4,1)   !iask=-4 suppresses new line
	   if(idiskq.eq.-1) goto 99	!if ESC hit then do not queue anything
	   newfil=.true.
	else if(iopt.eq.3) then
521	   print 52
52	   format(
     &'&(1)PLOTQ.DAT (2)POSHPLOT.DAT (3) other name: Option # [1] = ')
	   read 3,i
	   if(i.lt.1.or.i.gt.3) goto 521
	   if(i.eq.1) then
		filnam='\PLOTQ.DAT'
	   else if(i.eq.2) then
		filnam='\POSHPLOT.DAT'
	   else if(i.eq.3) then
		call TITENT('Queue file name:',filnam(2:13),12)
		filnam='\'//filnam
	   endif
	   newfil=.true.
	endif
	goto 4081	!any more changes
c Come here if any changes made
411	continue
	if(newfil) then
	   CLOSE(unit=12)
	   goto 48		!Open new file and chack if OK now?
	else			!if queue number only changed
	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
	   call LOCATE(lrow,0)		!row lrow,col 0
	   print 400,iplot,infil,istrec
	   read 101,ans
	   if(UC(ans).ne.'N') then		!O.K.
		if(iplot.le.nplot) goto 405	!overwriting existing record
		nplot1=iplot		!value to be written to 1st record
		goto 406
	   endif
	endif
c
c Queue file etc now defined: do next bit if old file is being overwritten
405	continue
	if(iplot.eq.1) jstrec(1)=2	!new start record (rec 1=jstrec etc now)
	istrec=int4(jstrec(iplot))	!start record for plot to be overwritten
	nplot1=iplot		!value to be written to 1st record
c Allow earlier plot to be overwritten in POSHPLOT.DAT only after query
	if(iplot.lt.nplot) then		!overwriting earlier plot
	   lr1=int4(lstrec(iplot))		!last rec of plot to be overwritten
	   nr1=lr1-istrec+1		!no of records for ditto
	   read(12,rec=istrec) iptype,itit,title1
	   if(nrect.le.nr1) then
	      call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
	      call LOCATE(lrow,0)		!row lrow,col 0
		print 29,title1
29		format('&Will overwrite ',a44,'  O.K. ? ')
		read 101,ans
		if(UC(ans).eq.'N') then
		   idiskq=-1
		   goto 441
		endif
	   else
		call BELL(2)
	      call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
	      call LOCATE(lrow,0)		!row lrow,col 0
		print 30
30		format('&Will overwrite next plot too: O.K. ? ')
		read 101,ans
		if(UC(ans).eq.'N') then
		   idiskq=-1
		   goto 441
		endif
	   endif
	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
	   call LOCATE(lrow,0)		!row lrow,col 0
	   print 34,nplot,iplot
34	   format('&Reduce last plot # from',i4,' to',i4,' [N] ? ')
	   read 101,ans
	   if(UC(ans).eq.'Y') then
		nplot1=iplot  !to write to 1st record
	   else
		nplot1=nplot
	   endif
	endif
c
406	continue
c Check that data will fit in: make max file size=1423 records which is
c most that will fit on 3.5 inch floppy (1457664 bytes usable, and
c 1457664/1024=1423.5   (1457664/2048=711.75)
	lastr=istrec+nrect-1		!last record for current plot
	if(lastr.gt.1423) then
	   call BELL(2)
	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
	   call LOCATE(lrow,0)		!row lrow,col 0
	   print 43
43	   format(
     & '&Not enough room in queue file: start on another disc:')
	   idiskq=-1
	   goto 441
	endif
	jstrec(iplot)=int2(istrec)	!set value for current plot
	lstrec(iplot)=int2(lastr)	!set value for current plot
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
	iptype=2	!for VHIST
c
	krn=istrec
c
c (remove from write: mpos,mlast,ixlo,...,iyhi,ipdis,isup,flo,fhi,
c lt1,ivel,plot;  add doframe
	if(debug()) print 121,iptype,itit,title1,xmin,xmax,ymin,ymax,
     & xcross,ycross,xtic,ytic,csize,ifont,ilog,iscal,
     & doframe,titlex,titley,ilabel,ndimd,ndimc,krn
121	format(' iptype,itit,title1,xmin,xmax,ymin,ymax,',/,
     & ' xcross,ycross,xtic,ytic,csize,ifont,ilog,iscal,',/,
     & ' doframe,titlex,titley,ilabel,ndimd,ndimc,krn',2i8,/,
     & 1x,a44,/,
     & 8g10.3,/,g10.3,3i8,l4,/,1x,a40,/,1x,a40,/,4i8)
	jcol=1002		!colour queued
	write(12,rec=krn) iptype,itit,title1,xmin,xmax,ymin,ymax,
     & xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,csize,ifont,ilog,
     & iscal,doframe,titlex,titley,ilabel,ndimd,ndimc,ncurvd,ndat1,
     & ncurvc,ncal1,ijoin1,iline1,inumx,inumy,sval,isval,
     & theta,ifitype,ncomp,numbx,numby,ifnt,ntext,narrow,
     & nline,iltype,xwbase,lt2,ntx,nty,itx,ity,k1,k2,k3,k4,
     & jcol,mono
c
	if(jcol.ne.1002) then
	   mono=.true.		!not queued
	endif
c
c Now write NEWTEXT,CHARCOP,INT2COP,and DATCOP to disk
c First DATCOP
	n=1
	do 380 j=1,nrec1
	  m=n+255
	  krn=krn+1
	  write(12,rec=krn) (DATCOP(i),i=n,m)
	  n=n+256
380	continue
c
	if(ntext.eq.0) then
	   if(nbyte3+nbyte4.le.1024) then
		krn=krn+1
		write(12,rec=krn) (CHARCOP(i),i=1,k3),(INT2COP(i),i=1,k4)
	   else
		krn=krn+1
		write(12,rec=krn) (CHARCOP(i),i=1,k3)
		krn=krn+1
		write(12,rec=krn) (INT2COP(i),i=1,k4)
	   endif
	else if(ntext.gt.0) then
	   if(nbyte2+nbyte3+nbyte4.le.1024) then
		krn=krn+1
		write(12,rec=krn) (NEWTEXT(i),i=1,ntext),
     &		(CHARCOP(i),i=1,k3),(INT2COP(i),i=1,k4)
	   else if(nbyte2+nbyte3.le.1024) then
		krn=krn+1
		write(12,rec=krn)(NEWTEXT(i),i=1,ntext),(CHARCOP(i),i=1,k3)
		krn=krn+1
		write(12,rec=krn) (INT2COP(i),i=1,k4)
	   else if(nbyte2.le.1024.and.(nbyte3+nbyte4.le.1024))then
		krn=krn+1
		write(12,rec=krn)(NEWTEXT(i),i=1,ntext)
		krn=krn+1
		write(12,rec=krn) (CHARCOP(i),i=1,k3),(INT2COP(i),i=1,k4)
	   else if(nbyte2.le.1024)then
		krn=krn+1
		write(12,rec=krn) (NEWTEXT(i),i=1,ntext)
		krn=krn+1
		write(12,rec=krn) (CHARCOP(i),i=1,k3)
		krn=krn+1
		write(12,rec=krn) (INT2COP(i),i=1,k4)
	   else
		krn=krn+1
		write(12,rec=krn) (NEWTEXT(i),i=1,12)
		krn=krn+1
		write(12,rec=krn) (NEWTEXT(i),i=13,20)
		krn=krn+1
		write(12,rec=krn) (CHARCOP(i),i=1,k3)
		krn=krn+1
		write(12,rec=krn) (INT2COP(i),i=1,k4)
	   endif	!end of ntext>0
	endif
c
c Update rec #1 (after data written safely!)
	write(12,rec=1) nplot1,jstrec,lstrec,iver
	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
	call LOCATE(lrow,0)		!row lrow,col 0
	print 46,iplot,istrec,krn,ndev,title1
	if(slock()) write(7,46)iplot,istrec,krn,ndev,title1
      if(discprt) write(8,46)iplot,istrec,krn,ndev,title1
46	format(/,
     & ' Plot queue no ',i3,' in records ',i5,' to ',i5,' on disc ',a2,
     & /,1x,a44,'**************************')
	call flush(7)
45	CLOSE(UNIT=12)
99	continue
	RETURN		!return to screen coordinates?
c
C ERROR SECTION
302	continue
	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
	call LOCATE(lrow,0)		!row lrow,col 0
	print 403
403	format('&Error in opening PLOTQ file: try again ? ')
	call BELL(1)
	read 101,ans
	newfil=.false.
	if(UC(ans).eq.'Y') goto 4081
	idest=1551
	RETURN
c
	end


