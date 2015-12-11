	subroutine INAUT(nplot,jstrec,lstrec,QFILE,auto,id,ndisc,
     & screen,draft,colplotter,quarter,cjump,irecfst,mono,idiskq,
     & adcfil,ioffset,newform,first,
     & plotcirc,noclogo,novlogo,joinline,jtype,idelt,iver)
c Input for AUTPLT  F90/Gino version
c Modified 08/12/92 02:18pm so jstrec(), lstrec() both have dimension=200
c (must be read and written in two sections for compatibility with old files)
c CJUMP plots added 04/13/91 07:25pm (and renamed INAUT)
c   (array called ISTREC in CJUMP called IRECFST here because ISTREC used
c	extensively here)
c Lahey/Hgraph version 10/26/89 06:44pm
	integer*2 jstrec(200),lstrec(200)
	integer*4 jstrec1(100),lstrec1(100)		!for old queue
	integer*2 irecfst2(250)		!=500 bytes for CJUMP
	integer*4 irecfst(1000)		!now use int*4 version
	logical newform,first
	logical plotcirc,noclogo,novlogo,joinline
	logical plotcir1,noclogo1,novlogo1,joinlin1
	character*1 ans,UC
c	character ndev*2,qfile*33
	character qfile1*33		!to read old .ini
	character ndev*2,qfile*40	!changed 08/30/02 12:45pm
	character adcfil*33	!path for consam file (for .INI)
	character title1*64
	logical discprt,pon,slock,auto,quarter,draft,screen
	logical cjump,present,mono,colplotter,retry
c
	common/pdelay/delay	!from multrace
	common/dp/discprt
c
	pon()=slock()
c
101	format(a1)
c
c
	INQUIRE(file='AUTPLOT.INI',exist=present,flen=len)
	if(.not.present.or.len.eq.0) then
	   ndev='C:'
	   call DISCNUM1(id,ndev,-1,1)	!ask for winchester or floppy
	   if(id.eq.-1) RETURN
	   qfile=ndev//'\PLOTQ.DAT'
	   auto=.true.		!default
	   quarter=.true.		!default
c	   screen=.false.		!default
	   delay=60.	!delay per megabyte for plots in multrace
	else
         OPEN(unit=12,file='AUTPLOT.INI',status='UNKNOWN',
     &     access='DIRECT',form='UNFORMATTED',recl=128)
	   read(12,rec=1) iver,ndev,qfile,auto,screen,
     &     quarter,adcfil,cjump,delay,plotcir1,noclogo1,novlogo1,
     &     joinlin1,jtype1,idelt1
c	   if(iver.ne.2.or.idelt1.lt.0.or.idelt1.gt.100) then	!old ini format
	   if(iver.ne.2) then			!old ini format
	      read(12,rec=1) ndev,qfile1,iopt,idiskq,auto,screen,
     &     quarter,adcfil,cjump,delay,plotcir1,noclogo1,novlogo1,
     &     joinlin1,jtype1,idelt1
		qfile(1:33)=qfile1
		qfile(34:40)='      '
	   endif

	   if(cjump.and.first) then	!use jtype etc from .ini only for cjump
		plotcirc=plotcir1
		noclogo=noclogo1
		novlogo=novlogo1
		joinline=joinlin1
		jtype=jtype1
		idelt=idelt1
	      if(jtype.lt.-1.or.jtype.gt.8) jtype=0	!in case not defined yet
		if(idelt.lt.1.or.idelt.gt.100) idelt=1
	    endif
	    CLOSE(unit=12)
	endif
141	continue
	i=1
	if(cjump) i=2
	print 14,i
14	format(
     & ' (1) Plot graphs from a plot queue file',/,
     & ' (2) Plot raw current records from CJUMP (cjump.cjd, or .dat)',/
     & ,' Option number [',i2,'] = ')
	call INPUTi(i)
	if(i.lt.0.or.i.gt.2) goto 141
	cjump=i.eq.2
c
c Define disc to be used
	retry=.false.
94	continue
c	   if(ndisc.gt.1) print 34
c34	   format(' Specify disk for CJUMP.DAT file:')
c	   call DISCNUM1(id,ndev,-1,0)	!ask for winchester or floppy
c	   if(id.eq.-1) RETURN
c	else
	if(.not.cjump) then
	   if(retry) then
		ans='N'
		goto 36
	   endif
	   print 356,qfile
356	   format(' Data from',1x,a40,': O.K. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
36	   if(UC(ans).eq.'N') then
c	      if(ndisc.gt.1) print 35
c35		format(' Specify disk partition for PLOTQ file:')
c	      ndev='C:'
c		call DISCNUM1(id,ndev,-1,1)	!ask for winchester or floppy
c		if(id.eq.-1) RETURN
c		n=NBLANK(qfile)
c		if(qfile(2:2).eq.':') then
c		   qfile(1:2)=ndev
c		else
c		   qfile(1:n+2)=ndev//qfile(1:n)
c		endif
		call TITENT0(
     &	'Name and path of plot queue file:',qfile,40,.false.)
c		iopt=2				!get file name
c		goto 97
	   endif
c	   iopt=0
	   INQUIRE(file=qfile,exist=present,flen=len)
	   if(.not.present.or.len.eq.0) then
		iopt=1
		call BELL(3)
		print 241,qfile
241		format(' ',1x,a33,' does not exist:',/,
     &	' (1) Try another filename/path',/,
     &	' (2) Leave program',/,
     &	' Option number [1] = ')
		call INPUTi(iopt)
c		read 4,iopt
		if(iopt.eq.2) then
		   id=-1
		   RETURN
		endif
		ans='N'
		goto 36	!get new file name
c	   else
c		goto 370		!file does exist
	   endif
c
c97	   continue
c	   if(iopt.eq.0) then
c		goto 370
c	   else if(iopt.eq.1) then
c	      call DISCNUM1(id,ndev,-1,0)	!ask for winchester or floppy
c	      if(id.eq.-1) RETURN
c	      goto 97
c	   else if(iopt.eq.2) then
c		qfile='               '
c	      print 352
c352	      format(' File name (e.g. [plotq]E[.dat]) = ')
c	      read 253,qfile
cc253	      format(a15)
c	      n=NBLANK(qfile)
cc If only one character then make the rest. If n=0 (hit ENTER) then use
cc plotq.dat
c		if(n.eq.0) then
c		   qfile='plotq.dat'
c		else if(n.eq.1) then
c		   qfile='plotq'//qfile(1:1)//'.dat'
c		else
cc Look for extension on file name. If '.' present assume its given, otherwise
cc add extension .DAT
c	         do 354 i=1,n
c354	          if(qfile(i:i).eq.'.') goto 355	!extension given
c	         qfile=qfile(1:n)//'.dat'			!add extension
c355	         n=NBLANK(qfile)
c		   if(n.eq.5) then 	!assume 'E.dat' given so add 'plotq'
c		      qfile='plotq'//qfile
c		   endif
c		endif
c	      n=NBLANK(qfile)
c	      qfile=ndev//'\'//qfile(1:n)
c	   endif
c
c OPEN QUEUE FILE
370	   continue
         OPEN(unit=11,file=QFILE,status='OLD',
     &   access='DIRECT',form='UNFORMATTED',recl=1024,err=302)
c The array jstrec(200) contains in jstrec(nplot) the start record
c for plot #nplot, and in lstrec(nplot) the last record number for it
         read(11,rec=1) nplot,jstrec,lstrec,iver
c*************DEBUG for DKM's lost index*******************
	   if(nplot.eq.0) then
		print 306
306		format(
     &	' Specify nplot, jstrec(nplot), lstrec (nplot) [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(UC(ans).ne.'N') then
		   print 307
307		   format(' nplot = ')
		   call INPUTi(nplot)
		   print 308
308		   format(' jstrec, lstrec = ')
		   call INPUT2i(j1,l1)
		   jstrec(nplot)=int2(j1)
		   lstrec(nplot)=int2(l1)
		endif
	   endif
	   if(pon()) write(7,3) qfile,nplot
	   if(discprt) write(8,3) qfile,nplot
3	   format(' Plots from QUEUE file: ',a40,' (',i4,' plots)')
	   if(iver.ne.1001.and.iver.ne.1100) then
		call BELL(2)
		print 217
217		format(
     &	'&EITHER ',/,
     &	'  (1) this is not a Plot Queue file',/,
     &	' OR ',/,
     &	'  (2)it is plot queue with old format',/,
     &	' Which option is correct [1] = ')
		iopt=1
		call INPUTi(iopt)
		if(iopt.ne.2) then
 		   goto 141
		else
		   print 2171
2171		   format(' Convert old queue on disk to new format [N] ? ')
		   ans='N'
		   call INPUTa(ans)
	 	   if(UC(ans).ne.'Y') then
		      call CONVQREC(jstrec,jstrec1,lstrec,lstrec1,11,0)
		   else
	  	      call CONVQREC(jstrec,jstrec1,lstrec,lstrec1,11,1)
		   endif
	      endif
	      goto 98
	   endif
	endif		!end of file opening when not cjump
c
c Now options to read in CJUMP file
	if(cjump) then
	   print 3561,adcfil
3561	   format(' CJUMP data from',1x,a33,': O.K. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
361	   if(UC(ans).eq.'N') then
c	      adcfil=ndev//'\CJUMP.cjd'
	      adcfil='\CJUMP.cjd'
251	      continue
	      call TITENT0('Jump data file name:',adcfil,33,.false.)
	      INQUIRE(file=adcfil,exist=present,flen=len)
	      if(.not.present.or.len.eq.0) then
		   call BELL(3)
		   print 24,adcfil
24		   format('&',1x,a33,' does not exist')
		   goto 251
		else
               OPEN(unit=14,file=adcfil,status='OLD',
     & 	   access='DIRECT',form='UNFORMATTED',recl=512)
c fix this for new cjump5 format files
		   read(14,rec=1) njd
		   newform=.false.	!old format file (512 byte records)
		   if(njd.lt.1) then
			newform=.true.	!new format 'transparent' file
			CLOSE(unit=14)
c          Re-open in correct format
		      OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &		 access='TRANSPARENT')
			read(14,rec=1) jver,njd,nxtrec,ioffset,irecfst
		   else
		      read(14,rec=1) njd,nxtrec,irecfst2
		   endif
		   nplot=njd	!name used in this prog
		endif
	   else
	      INQUIRE(file=adcfil,exist=present,flen=len)
	      if(.not.present.or.len.eq.0) then
		   call BELL(3)
		   print 24,adcfil
c24		   format('&',1x,a33,' does not exist')
		   ans='N'
		   goto 361
	     endif
           OPEN(unit=14,file=adcfil,status='OLD',
     & 	access='DIRECT',form='UNFORMATTED',recl=512)
	     read(14,rec=1) njd
	     newform=.false.	!old format file (512 byte records)
	     if(njd.lt.1) then
		   newform=.true.	!new format 'transparent' file
		   CLOSE(unit=14)
c          Re-open in correct format
		   OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &		 access='TRANSPARENT')
		   read(14,rec=1) jver,njd,nxtrec,ioffset,irecfst
		else
		   read(14,rec=1) njd,nxtrec,irecfst2
		endif
		nplot=njd	!name used in this prog
	   endif
c Convert the integer*2 istrec2() into integer *4 for all subsequent use
	   if(.not.newform) then
		do i=1,njd
		   irec=int4(irecfst2(i))
		   if(irec.le.-1.and.irec.ge.-32768) then
			irec=irec+65536
		   else if(irec.lt.-32768) then
			print*,' IREC IS TOO BIG!!'
			STOP
		   endif
		   irecfst(i)=irec
		enddo
	   endif
	endif		!adcfil now defined
c
98	continue
	print 304,nplot
304	format(' Number of plots on file= ',i6)
	if(.not.cjump) then
	   print 10
10	   format(' List all the plot titles [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	   if(UC(ans).ne.'Y') goto 21
	else
	   goto 21		!if cjump
	endif
c
c SECTION TO LIST PLOT TITLES
c	if(pon()) write(7,121) qfile
c      if(discprt) write(8,121) qfile
c121	format(' Data from file ',a40)	!printed above
	do 100 iplot=1,nplot
	if(mod(iplot,20).eq.0) then
	   print 95
95	   format(' Press any key for more ...')
	   call ANYKEY
	endif
	istrec=int4(jstrec(iplot))	!1st record no for plot # iPLOT
c	read(11,rec=istrec) iptype,itit,title1
	read(11,rec=istrec) iptype
c NB iptype=15 is writen as transparent file, but jstrec() kept as for
c 1024-byte record file so should be OK to read as far as title1 when
c opened with fixed record length
	if(iptype.eq.15) then
	   read(11,rec=istrec) iptype,ndv1,ndimd,ndc1,ndimc,kwi,kwj,kmax,
     & 	itit,title1
c	else if(iptype.ge.15.and.iptype.le.25) then
	else if(iptype.ge.21.and.iptype.le.25) then
	   read(11,rec=istrec) iptype,ndv1,ndimd,ndc1,ndimc,kmax,
     & 	itit,title1
	else if(iptype.eq.33.or.(iptype.ge.40.and.iptype.le.43)) then
	   read(11,rec=istrec) iptype,title1
	else
	   read(11,rec=istrec) iptype,itit,title1
	endif
c	nl=nblank(title1)
	iptype=iabs(iptype)
c	if(iptype.eq.1) goto 31
c	if(iptype.eq.2) goto 32
c	if(iptype.eq.3) goto 32
	if(iptype.ge.11.and.iptype.le.15) goto 31	!vplot
	if(iptype.eq.21) goto 32	!vhist
	if(iptype.eq.31.or.iptype.eq.32) goto 33	!single channel
	if(iptype.eq.40.or.iptype.eq.41.or.iptype.eq.42) goto 34	!3D
	if(iptype.eq.33) goto 735
	goto(31,32,33) iptype
c
31	continue
c Logical PLOT replaced by DOFRAM=true if frame to be drawn for VPLOT8 only
c	read(11,rec=krn) idum,mpos,mlast,nrect,xmin,xmax,ymin,ymax,ixlo,ixhi,
c     & iylo,iyhi,xcross,ycross,xtic,ytic,ivel,ipen1,ipen2,ipen3,
c     & itit,title1
	print 12,iplot,title1(1:52)
	if(pon()) write(7,12)iplot,title1(1:52)
      if(discprt) write(8,12)iplot,title1(1:52)
12	format(' Plot ',i3,' (graph):',1x,a52)
c12	format(' Plot ',i3,' (graph):',1x,a64)
	goto 100
c Histograms or Single channel records
32	continue
	print 13,iplot,title1(1:52)
	if(pon()) write(7,15)iplot,title1(1:52)
      if(discprt) write(8,15)iplot,title1(1:52)
13	format(' Plot ',i3,' (histo):',1x,a52)
	goto 100
c Single channel records
33	continue
c title1 is still 44 characters in scqplt
	title1(45:64)='                   '
	print 15,iplot,title1(1:50)
	if(pon()) write(7,15)iplot,title1(1:50)
      if(discprt) write(8,15)iplot,title1(1:50)
15	format(' Plot ',i3,' (single chan)',2x,a50)
	goto 100
c 3D plots
34	print 151,iplot,title1(1:52)
	if(pon()) write(7,151)iplot,title1(1:52)
      if(discprt) write(8,151)iplot,title1(1:52)
151	format(' Plot ',i3,' (3D plot)',1x,a52)
	goto 100
c     VSAMP plots
735	print 152,iplot,title1(1:52)
	if(pon()) write(7,152)iplot,title1(1:52)
      if(discprt) write(8,152)iplot,title1(1:52)
152	format(' Plot ',i3,' (VSamp)',1x,a52)
	goto 100
c
100	continue
	print 96
96	format(' Use this file [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).eq.'N') then
	   retry=.true.
	   goto 94
	endif
21	continue
c
161	continue
	i=1			!default
	if(.not.auto) i=2	!default
	print 16,i
16	format(
     & ' (1) AUTO: Plot series of graphs automatically from queue',/,
     & ' (2) POSH: Do single plot with options to change labels etc',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(i)
4	format(i8)
	if(i.lt.0.or.i.gt.2) goto 161
	if(i.eq.0) i=1
	auto=i.eq.1
	if(.not.auto) screen=.true.
c	screen=i.eq.2
	if(.not.auto) goto 50		!ie if posh
c
c Define values for auto plotting
171	continue
	i=1			!default
	if(screen) i=2	!default
	print 17,i
17	format(
     & ' (1) PLOT: Plot automatically from queue on Laserjet/Deskjet',/,
     & ' (2) SCREEN: Plot automatically from queue on screen',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(i)
	if(i.lt.0.or.i.gt.2) goto 171
	if(i.eq.0) i=1
	screen=i.eq.2
c
	if(screen) then
	   mono=.false.	!colour on screen
	  idev=0
	else
	   idev=4
	   if(colplotter)then
		call DCASK('Plot in colour','y',ans)
		mono=ans.eq.'N'
	   else
		mono=.true.
	   endif
	endif
c NB if colours were queued then this value of mono will be overwritten
c later by that read from queue
c
191	continue
	i=1			!default
	if(.not.quarter) i=2	!default
	print 19,i
19	format(
     & ' (1) Quarter (and/or half) page plots (up to 4 per page)',/,
     & ' (2) Whole page plots (one per page)',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(i)
	if(i.lt.0.or.i.gt.2) goto 191
	if(i.eq.0) i=1
	quarter=i.eq.1
c
	draft=.false.
c	if(.not.screen) then
c	  i1=2
c	  print 18,i1
c18	  format(
c     & ' (1) Draft plots (fast, but thin lines)',/,
c     & ' (2) Better plots (using default line thickness)',/,
c     & ' Option number [',i2,'] = ')
c	  read 4,i
c	  if(i.ge.1.and.i.le.2) i1=i
c	  draft=i1.eq.1
c	else
c	  draft=.false.
c	endif
c
c
c Define file(s) to be plotted
50	CONTINUE
	idev=0
	if(.not.cjump) then
	   CLOSE(unit=11)
	else
	   CLOSE(unit=14)
	endif
c Write defaults for next time to AUTPLOT.INI (except adcfil, new value
c of which not yet defined -written at end of AUTPLOT)
c	if(.not.cjump) then	!keep as default for next time
         OPEN(unit=12,file='AUTPLOT.INI',status='UNKNOWN',
     &   access='DIRECT',form='UNFORMATTED',recl=128)
	   iver=2
	   write(12,rec=1) iver,ndev,qfile,auto,screen,
     &     quarter,adcfil,cjump,delay
c	   write(12,rec=1) ndev,qfile,iopt,idiskq,auto,screen,
c     &	  quarter,adcfil,cjump,delay
	   CLOSE(unit=12)
c	endif
	RETURN
c
C ERROR SECTION
302	continue
	call BELL(1)
	print 303
303	format(' Error in opening PLOTQ file ',/,
     & ' Is data in A:\PLOTQ.DAT ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).eq.'Y') then
	   QFILE='A:\PLOTQ.DAT'
	else
	   retry=.true.
	   goto 94
	endif
c
	END


