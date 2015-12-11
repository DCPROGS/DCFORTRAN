	subroutine VPLQ2(xval,yval,xcal,ycal,ndimd,ndimc,
     & ncurvd,ndat,icurvd,isym,ijoin,ncurvc,ncal,icurvc,iline,syms,
     & xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ntx,nty,itx,ity,
     & ixlo,ixhi,iylo,iyhi,itit,title1,csize,ifont,ilog,iscal,doframe,
     & titlex,titley,ilabel,inumx,inumy,idiskq,sval,
     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,
     & theta,ifitype,ncomp,idest,autplt,icol,mono,interp,
     & lth,RX,RY,rxbox,rybox,IXJUS,IYJUS,iangle,idraw,ifnt,
     & csfac,cnumx,cnumy,cexpx,cexpy,numbx,numby,
     & narrow,xb,yb,xe,ye,ntext,nline,xlb,ylb,xle,yle,iltype,
     & newtext,isdev,weight,y0,yinf)
c
c To queue plots in VPLOT2
c Modified 02/09/93 03:51pm to add ICOL(), mono and interp
c Modified 02/09/93 03:51pm to add ICOL(), mono and interp (and jcol=1002 if
c colours and interp have been queued)
c Modified 08/12/92 02:18pm so jstrec(), lstrec() both have dimension=200
c (must be read and written in two sections for compatibility with old files)
c Subroutine for VPLOTR to put data in PLOT QUEUE file (based on VPLQ).
c Signal whether jump logos to be drawn by setting iy1 negative to
c omit logo
c Last 3 lines are extra args to hold all details of posh plots.
c  (1) Arguments removed ipen,mpos,mlast,ivel,xlo,...,yhi,
c	ipdis,isup,nlenx,nleny
	real*4 DATCOP(46960)
	integer*2 INT2COP(1000)
	character*10 CHARCOP(100)
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character cnum1*11,cnum2*11		!for dialog
c
	dimension XVAL(2048,ndimd),YVAL(2048,ndimd)
	dimension XCAL(2048,ndimc),YCAL(2048,ndimc)
	dimension ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd)
	dimension ncal(ndimc),icurvc(ndimc),iline(ndimc)
	real*4 syms(ndimd)
	real theta(20),syms1(10)
	dimension ndat1(10),isym1(10),ncal1(10),ijoin1(10),iline1(10)  !for queue
	integer*2 jstrec(200),lstrec(200)		!for queue
	integer*4 jstrec1(100),lstrec1(100)		!for old queue
c	integer*2 lrow
	character*1 ans,UC
	character ndev*2,filnam*13,infil*15
	character*40 titlex,titley
	character*44 title1,title2	!plot title
	LOGICAL doframe,slock,pon,autplt,ivplot,present,newfil
	logical mono,interp
	logical discprt
	logical caplock,debug
c extra args to hold all details of posh plots
	real*4 t1v(10),t2v(10),t1c(10),t2c(10)	!for jump logos and t=0 def.
	integer*2 lth(100) 		!for line thickness
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
	real weight(100,ndimd)
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
	ict=13	!text colour for DIALOG box
	if(mono) ict=15
c
c Make integer*2 copies to save disc space
	do 50 j=1,100
	 IXJUS1(j)=IXJUS(j)
	 IYJUS1(j)=IYJUS(j)
	 iangle1(j)=iangle(j)
	 idraw1(j)=idraw(j)
	 icol1(j)=icol(j)
50	continue
c Signal whether jump logos to be drawn by setting iy1 negative to
c omit logo
	if(idraw(28).eq.0) iy1c=-1	!omit c-jump logo
	if(idraw(29).eq.0) iy1v=-1	!omit v-jump logo
c
	if(ncurvd.eq.0) goto 362
	do 32 i=1,ncurvd
	j=icurvd(i)
c	if(debug()) then
c	   print 801,ncurvd,i,j,ndat(j)
c801	   format(' ncurvd,i,j=icurvd(i),ndat(j) = ',4i8)
c	   pause
c	endif
	ndat1(i)=ndat(j)
	isym1(i)=isym(j)
	syms1(i)=syms(j)
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
c Establish top 3 lines of screen as scrolling region using utility library
c routines (windows lib works similarly but will not draw borders while
c in graphics mode, as far as I can tell, so no advantage really). See
c \fortran\TSCROL.FOR for tests.
c	lastrow=2			!NB args of SCROLL=integer
c	lrow=int2(lastrow)	!but args of LOCATE are int*2
c NB potential problem in writing arrays ndat,icurvd,...,ijoin as they
c have got variable dimensions!
c Not necessary (or desirable) to write the arrays icurvd,Icurvc because if
c arrays 2,4,5 say to be plotted better to write them, and read them back,
c as arrays 1,2,3 so need keep only the total number of data/curve arrays
c (ncurvd,ncurvc respectively)- so ndat(),isym(),ijoin() need to be
c renumbered 1,...,ncurvd; and ncal(),iline() renumbered 1,..,ncurvc. Put them
c in new arrays (suffix 1) of fixed dimension. At the same time
c get number of records for each (1) data (2) curve array and work out
c total number of records need. Have 512 reals per record.
c
c Make compressed data array
c If ISDEV=0 or 1 then write WEIGHT(i,j) for each set also, for SD bars;
c TO SAVE space use following method
c (a) make sure records are full so write Xval,Yval,Wt with no gaps
c (b) no need to write Xcal,Ycal in most cases: can just specify theta
c	and model #, so calc curve(s) can be recalc in AUTPLT. Only
c	problem with this is that VPLOT is used in many programs so
c	may have problems in specifying in AUTPLT how calc curve is to
c	be calculated- better leave Ycal in for now
	k=0		!index for datcop
	if(ncurvd.le.0) goto 365
	do 37 j1=1,ncurvd
	j=icurvd(j1)
	n=ndat(j)
	   do 371 i=1,n
	   k=k+1
371	   datcop(k)=Xval(i,j)
	   do 372 i=1,n
	   k=k+1
372	   datcop(k)=Yval(i,j)
	   if(isdev.ge.0) then
		do 373 i=1,n
		k=k+1
373		datcop(k)=weight(i,j)
	   endif
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
	do 60 i=1,30	!keep 1st 30 lth always
	   k=k+1
	   INT2COP(k)=lth(i)
60	continue
	if(ntext.gt.0) then
	   do 61 i=1,ntext
	   k=k+1
	   INT2COP(k)=lth(i+30)
61	   continue
	endif
	if(narrow.gt.0) then
	   do 62 i=1,narrow
	   k=k+1
	   INT2COP(k)=lth(i+50)
62	   continue
	endif
	if(nline.gt.0) then
	   do 63 i=1,nline
	   k=k+1
	   INT2COP(k)=lth(i+60)
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
c	if(autplt) filnam='\POSHPLOT.DAT'
	if(idiskq.ge.1) then	  !already defined
	   ndev=char(idiskq+64)//':'	!see def of idiskq below
	   if(ndev.eq.'A:'.or.ndev.eq.'B:') then
c		call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c		call LOCATE(lrow,0)		!row lrow,col 0
c		print 412,ndev
c412		format('&Queue on disc ',a2,'  O.K. [Y] ? ')
c		read 101,ans
c		if(UC(ans).eq.'N') goto 25          !specify disc
	      ans='Y'
	      call DEFOLTa(ans,defolt)
	      call QDIALOG(1,'Queue on disc '//ndev//' O.K.',
     &	   defolt,ict,cans)
	      call GETINPa(cans,ans)
		if(ans.eq.'N') goto 25          !specify disc
c		call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c		call LOCATE(lrow,0)		!row lrow,col 0
c		print 413,ndev
c413		format('&Is disc ',a2,'  mounted [Y] ? ')
c		read 101,ans
c		if(UC(ans).eq.'N') goto 25          !specify disc
	      ans='Y'
	      call DEFOLTa(ans,defolt)
	      call QDIALOG(1,'Is disc '//ndev//' mounted',
     &	   defolt,ict,cans)
	      call GETINPa(cans,ans)
		if(ans.eq.'N') goto 25          !specify disc
	   endif
	   goto 48
	endif
25	continue
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row lrow,col 0
c	print 410
c410	format('&Specify disc for plot queue file')
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row lrow,col 0
c	call DISCNUM1(idiskq,ndev,-4,1)   !iask=-4 suppresses new line
	call DISCDIALOG(1,'Specify disc for plot queue file',
     &	idiskq,ndev,-1,1,ict)
	if(idiskq.eq.-1) goto 99	!if ESC hit then do not queue anything
48	continue
	infil=ndev//filnam
	INQUIRE(file=INFIL,exist=present,flen=nlen)
	if(.not.present.or.nlen.eq.0) then
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row lrow,col 0
c	   print 481,infil
c481	   format('&Queue file ',a15,' does not exist: create it [Y] ? ')
c	   read 101,ans
	   ans='Y'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     & 'Queue file '//CHARNB(infil)//' does not exist: create it',
     &    defolt,ict,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'N') goto 4081
c
	   nplot=0
	   do 482 i=1,200
	   jstrec(i)=0
482	   lstrec(i)=0
         OPEN(unit=12,file=INFIL,status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=1024,err=302)
	   write(12,rec=1) nplot,jstrec,lstrec,iver
	   newfil=.false.		!file now exists
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
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row lrow,col 0
c	   print 217
c217	   format('&Plot queue has old format: converted to new')
	   call WDIALOG(1,
     &	'Plot queue has old format: converted to new',12)
	   call CONVQREC(jstrec,jstrec1,lstrec,lstrec1,12,1)
	endif
	if(.not.newfil) then	!if(newfil) nplot already specified
	 if(nplot.eq.0) then	!initialise to 0 in PLTQINIT or above
	   iplot=1
	   jstrec(1)=2
	   istrec=2
	 else
	   istrec=int4(lstrec(nplot))+1	!start record for current plot
	   iplot=nplot+1		!next number
	 endif
	else		!if newfil
	   istrec=int4(lstrec(nplot)+1)	!start record for current plot
	   iplot=nplot+1		!next number
	endif
4041	continue
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row lrow,col 0
	call INTCONV(iplot,cnum1)
	ans='Y'
	call DEFOLTa(ans,defolt)
	if(.not.newfil) then
c	  print 404,iplot,infil,istrec
c404	  format(
c     & '&Next queue number= ',i3,' in ',a15,' (rec #= ',i5,
c     & ' ): O.K. [Y] ? ')
	   call QDIALOG(1,
     & 'Next queue number = '//CHARNB(cnum1)//' in '//CHARNB(infil)//
     &   ': O.K.',defolt,ict,cans)
	else if(newfil) then
c	  print 400,iplot,infil,istrec
c400	  format(
c     & '&Queue number= ',i3,' in ',a15,' (rec #= ',i5,
c     & ' ): O.K. [Y] ? ')
	   call QDIALOG(1,
     & 'Queue number = '//CHARNB(cnum1)//' in '//CHARNB(infil)//
     &   ': O.K.',defolt,ict,cans)
	endif
	call GETINPa(cans,ans)
c	read 101,ans
c101	format(a1)
	if(UC(ans).ne.'N') then		!O.K.
	   if(iplot.le.nplot) goto 405	!overwriting existing record
	   nplot1=iplot		!value to be written to 1st record
	   goto 406
	endif
c
c Bit done if want to change (1)plot number,(2)disc or (3)plot queue file name
c
	newfil=.false.
4081	continue
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row lrow,col 0
c	print 408
c408	format(
c     &'&Change (1)queue # (2)disc (3)queue file name, or (4)abandon:')
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row lrow,col 0
c	print 409
c409	format(
c     &'& Option number (1,2,3,4) [hit <enter> if O.K. now]: = ')
c	read 3,iopt
c	if(iopt.lt.0.or.iopt.gt.4) goto 4081
	iopt=0	!default
	call WDIALOG(1,
     & 'Change (1) the queue number (2) disc on which queued',ict)
	call WDIALOG(1,
     & ' (3) name of queue file, or (4) abandon queueing',ict)
408	call QDIALOG(1,
     & 'Type option # (1,2,3,4) [or hit <enter> if O.K. now]:',
     &  ' ',ict,cans)
	call GETINPi(cans,iopt)
	if(iopt.lt.0.or.iopt.gt.4) goto 408
	if(iopt.eq.0) goto 411		!all changes done
	if(iopt.eq.4) goto 45
c
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row lrow,col 0
	if(iopt.eq.1) then
c	   print 51,nplot
c51	   format('&Queue number (up to',i3,' ) = ')
c	   read 3,i
	   call INTCONV(iplot,cnum1)
	   call QDIALOG(1,
     &    'Queue number (up to'//CHARNB(cnum1)//')',' ',ict,cans)
	   call GETINPi(cans,i)
	   if(i.ge.1) iplot=i
	else if(iopt.eq.2) then
c	   call DISCNUM1(idiskq,ndev,-4,1)   !iask=-4 suppresses new line
	   call DISCDIALOG(1,'Specify disc for plot queue file',
     &	idiskq,ndev,-1,1,ict)
	   if(idiskq.eq.-1) goto 99	!if ESC hit then do not queue anything
	   newfil=.true.
	else if(iopt.eq.3) then
c521	   print 52
c52	   format(
c     &'&(1)PLOTQ.DAT (2)POSHPLOT.DAT (3) other name: Option # [1] = ')
c	   read 3,i
521	   i=1		!default
	   call QDIALOG(1,
     &   '(1)PLOTQ.DAT (2)POSHPLOT.DAT (3) other name: Option # [1]',
     &   '1',ict,cans)
	   call GETINPi(cans,i)
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
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row lrow,col 0
c	   print 400,iplot,infil,istrec
c	   read 101,ans
c	   if(UC(ans).ne.'N') then		!O.K.
	   call INTCONV(iplot,cnum1)
	   ans='Y'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     & 'Queue number = '//CHARNB(cnum1)//' in '//CHARNB(infil)//
     &   ': O.K.',defolt,ict,cans)
	   call GETINPa(cans,ans)
	   if(ans.ne.'N') then		!O.K.
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
c	if(autplt.and.nplot.lt.npsav) then		!overwriting earlier plot
	if(iplot.lt.nplot) then		!overwriting earlier plot
	   lr1=int4(lstrec(iplot))		!last rec of plot to be overwritten
	   nr1=lr1-istrec+1		!no of records for ditto
	   read(12,rec=istrec) iptype,itit,title2
	   if(nrect.le.nr1) then
c	      call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	      call LOCATE(lrow,0)		!row lrow,col 0
c		print 29,title1
c29		format('&Will overwrite ',a44,'  O.K. ? ')
c		read 101,ans
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
c	      call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	      call LOCATE(lrow,0)		!row lrow,col 0
c		print 30
c30		format('&Will overwrite next plot too: O.K. ? ')
c		read 101,ans
	      call QDIALOG(1,'Will overwrite next plot too: O.K.',
     &	   ' ',ict,cans)
	      call GETINPa(cans,ans)
		if(UC(ans).eq.'N') then
		   idiskq=-1
		   goto 441
		endif
	   endif
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row lrow,col 0
c	   print 34,nplot,iplot
c34	   format('&Reduce last plot # from',i4,' to',i4,' [N] ? ')
c	   read 101,ans
c	   if(UC(ans).eq.'Y') then
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
c
406	continue
c Check that data will fit in: make max file size=1423 records which is
c most that will fit on 3.5 inch floppy (1457664 bytes usable, and
c 1457664/1024=1423.5   (1457664/2048=711.75)
	lastr=istrec+nrect-1		!last record for current plot
	if(lastr.gt.1423) then
	   call BELL(2)
c	   call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	   call LOCATE(lrow,0)		!row lrow,col 0
c	   print 43
c43	   format(
c     & '&Not enough room in queue file: start on another disc:')
	   call WDIALOG(1,
     &    'Not enough room in queue file: start on another disc:',12)
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
c IPTYPE=11 for VPLOTR
c IPTYPE=12 for VPLOT2
c	=2 for VHIST
c	=3 for PLOTSC
	iptype=12	!for VPLOT2
c
	krn=istrec
c
c (remove from write: mpos,mlast,ipdis,isup,flo,fhi,
c lt1,ivel,plot;  add doframe
c	if(debug()) then
c	if(pon()) print 121,iptype,itit,title1,xmin,xmax,ymin,ymax,
c     & xcross,ycross,xtic,ytic,csize,ifont,ilog,iscal,
c     & doframe,titlex,titley,ilabel,ndimd,ndimc,krn,
c     & narrow,nline,iltype(1),isdev,y0,yinf,ntx,nty,itx,ity,
c     & k1,k2,k3,k4,
c     & ncjump,t1c(1),t2c(1),nvjump,t1v(1),t2v(1),xoff1,
c     & iy1v,iy2v,iy1c,iy2c,ivplot
c	if(discprt)write(8,121)iptype,itit,title1,xmin,xmax,ymin,ymax,
c     & xcross,ycross,xtic,ytic,csize,ifont,ilog,iscal,
c     & doframe,titlex,titley,ilabel,ndimd,ndimc,krn,
c     & narrow,nline,iltype(1),isdev,y0,yinf,ntx,nty,itx,ity,
c     & k1,k2,k3,k4,
c     & ncjump,t1c(1),t2c(1),nvjump,t1v(1),t2v(1),xoff1,
c     & iy1v,iy2v,iy1c,iy2c,ivplot
c121	format(' iptype,itit,title1,xmin,xmax,ymin,ymax,',/,
c     & ' xcross,ycross,xtic,ytic,csize,ifont,ilog,iscal,',/,
c     & ' doframe,titlex,titley,ilabel,ndimd,ndimc,krn',2i8,/,
c     & 1x,a44,/,
c     & 8g10.3,/,g10.3,3i8,l4,/,1x,a40,/,1x,a40,/,4i8,/,
c     & ' narrow,nline,iltype(1),isdev,y0,yinf=',/,
c     & 4i8,2g13.6,/,
c     & ' ntx,nty,itx,ity,k1,k2,k3,k4=',/,8i5,/,
c     & ' ncjump,t1c(1),t2c(1),nvjump,t1v(1),t2v(1),xoff1= ',/,
c     & i4,2g13.6,i6,3g13.6,/,
c     & ' iy1v,iy2v,iy1c,iy2c,ivplot =',/,
c     & 4i5,l4)
c	endif
c
	jcol=1002		!signal that colour,interp queued
	write(12,rec=krn) iptype,itit,title1,xmin,xmax,ymin,ymax,
     & xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,csize,ifont,ilog,
     & iscal,doframe,titlex,titley,ilabel,ndimd,ndimc,ncurvd,ndat1,
     & isym1,ncurvc,ncal1,ijoin1,iline1,syms1,inumx,inumy,sval,
     & theta,ifitype,ncomp,numbx,numby,ifnt,ntext,
     & narrow,nline,iltype,isdev,y0,yinf,ntx,nty,itx,ity,k1,k2,k3,k4,
     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,
     & jcol,mono,interp
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
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row lrow,col 0
c	print 46,iplot,istrec,krn,ndev,title1
	call INTCONV(iplot,cnum1)
	call INTCONV(istrec,cnum2)
	call WDIALOG(1,
     & 'Plot queue # '//CHARNB(cnum1)//': start record # '//
     &	CHARNB(cnum2)//' on disc '//ndev,ict)
	call WDIALOG(1,CHARNB(title1),ict)
	if(slock()) write(7,46)iplot,istrec,krn,ndev,title1
      if(discprt) write(8,46)iplot,istrec,krn,ndev,title1
46	format(/,
     & ' Plot queue no ',i3,' in records ',i5,' to ',i5,' on disc ',a2,
     & /,1x,a44,'**************************')
c	if(debug()) pause
	call flush(7)
45	CLOSE(UNIT=12)
99	continue
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

