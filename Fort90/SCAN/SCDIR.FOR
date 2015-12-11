 	PROGRAM SCDIR
C
c Version for MICRO
c Notes for new SC DATA disk format: 03/23/93 01:46pm
c (1) Use a single file, SCANDAT.DAT, with record 1 recording where
c	each expt starts and ends, disc # etc, but with present directory
c	data immediately preceding each set of tint, iampl. Snag is that
c	latter still fits in 128 bytes, so 1024 byte record length not
c	economical?  Could use transparent file? (with fixed offset to
c	start of the binary data?)
c (2) ISCANVER=100 for first version (ISCANVER=99 for a few expts
c	analysed before point amp histo included on data disk)
c (3) Most alterations not yet written in for header (none for data yet)
c	At present can change only title and iscanver.
c
c OLD DATA DISKS:
c Modified 07/08/92 10:20pm so another correction can be done, viz
c to change all the amplitudes in a specified SC data file ('expt')
c and change the calibration factor accordingly.  This is needed if
c we want to pool experiments with different calibration factors, to
c make calfac same for all (because EKDIST uses only a single CALFAC
c value).  Can happen with SCAN5 (PDP version when display gain factor
c is different for analysing different expts!!)
C LIST DATA AFTER IMPOSING RESOLUTION ON IT IF REQ.
C
C  Meaning of ISCANver
C  ISCAN=-1001 IS A FLAG TO SHOW DIRECTORY AND DIST-FITTING PROGS THAT
C THE DATA POINTED TO BY THIS DIRECTORY RECORD IS IN THE FORMAT PRODUCED
C BY SCAN (RATHER THAN BY OLD SCFIT8).
c  Iscan=-1002 means that ISCAN is followed by valid values of JFILT
c and RMS (used in new EKDIST to calc false event rate)
c  Iscan=-1003 is as -1002 but indicates that analysis done with
c version of progs in which 'dubious' replaces 'assumed' amplitudes
c  Iscan=-2001 means data from SCANF
c  Iscan=-3001 means data from SCSIM
c iscan=100 means data from new PC SCAN program
c iscan=101 means data from new PC SCAN program with cjump defined
c
C NOTATION:
c---------------------------------------------------
C  old 	        new         meaning
c---------------------------------------------------
c directory record 1:
c  NSC		NEXPT	Total no of expts (up to the
c			last on on the disc being read)
C  KNREC	NXTREC	Next vacant record in SCDAT.DAT
C  JD1		IDISC	Floppy disc # (for particular owner)
c  JINC		JFIRST	No of the first expt on this floppy (=old JINC+1)
C  ---   	NAME	Name of owner
C  KD1	      not used  Index no of owner
C-------------------------------------------------------------------
c directory record for expt #iexp=directory record # iexp-jfirst+2
C  KSN		ISTREC	No of 1st record of the data (in SCDAT.DAT)
C  NREC		NREC	No of records occupied by the data
C
c  NINT1=no of intervals in file
c  NINT=total no of intervals AFTER resolution imposed (see RESINT)
c
c  WHOLE=new logical variable added to directory (record 1)- when
c	true it indicates that this is a complete record of all expts
c	(all floppies should be). Set to false when copies of specified
c	expts only (iexp1 to iexp2) are copied onto DL1 for use in
c	EKDIST in which case record one contains WHOLE,IEXP1,IEXP so
c	can check that desired expts are present, and to prevent copy
c	of incomplete copies of SCDIR.DAT, SCDAT.DAT onto floppy.
c
	dimension TINT(20480)	!for compat with RESINT
c  for disc reads must specify integer*2
	INTEGER*2 IAMPL(20480)
	integer*2 Nexpt,nxtrec,idisc,jfirst,iexp1,iexp2
      integer*2 ISTREC,NREC,nint,NINT1,IAVAMP,ISCAN,jfilt
	character*4 adate(3)
	character TITLE1*56,name*12
	character*1 ans,UC
	character ndev*2,infil*14
	character*33 infil1
	LOGICAL DFOPEN,F18open,RESIMP,sublev,SCNDAT,pon,whole,slock
	logical debug,caplock,wdat,wdir,present,open
	character*11 cdate,ctime
c Declarations for new style data disk
	integer*4 jstrec(200),jstrec1(200),idiscn,nfiles,iscanver
	character name1*12,titlen*70
	logical same,newscan
	character expdate*11,tapeID*24,expnum*9
	character*14 ptype(5)
c For cjump (NB ipatch is integer*4 for SCAN, SCDIR, EKDIST (but integer*2
c in CJUMP -see DSKWRIT).
	logical cjump
c
c	allocatable::tint1
c	allocatable::iampl1
c	allocatable::iprops
c	real*4 tint1(:)
c	integer*2 iampl1(:)
c	integer*1 iprops(:)
	integer*1 iprops(20480)
c
	logical discprt
	character*40 mtitle*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	common/dp/discprt
c	common/dpp/filnam,machine,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	COMMON/RBLCK/TRESO,TRESG,IACRIT,mavamp
c
C
	pon()=slock()
	debug()=caplock()
c
6	FORMAT(I8)
101	FORMAT(A1)
	DFOPEN=.FALSE.		!DATA FILE OPEN FLAG
	F18open=.false.		!unit 18 (SCANDAT.DAT) open for new directory
	ptype(1)='outside-out'	!definitions for NEWSCAN
	ptype(2)='inside-out'
	ptype(3)='cell-attached'
	ptype(4)='whole-cell'
	ptype(5)='simulated data'
c
      filnam='SCDIR.PRT'
	call WINPRINT	!print-file control
      OPEN(unit=7,file=prtport,iostat=nerr)		!open printer
	print 1011
	if(pon()) write(7,1011)
	if(discprt) write(8,1011)
1011	format(' SINGLE CHANNEL DIRECTORY',/,
     & ' (Scroll-lock ON to print)')
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 1,cdate,ctime(1:8),mtitle
	if(pon()) write(7,1) cdate,ctime(1:8),mtitle
	if(discprt) write(8,1) cdate,ctime(1:8),mtitle
1	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     &	'   Machine = ',a40)
c
c	print 20
c20	format(
c     &' List names of single channel data files on hard discs [N] ? ')
c	read 101,ans
c	if(UC(ans).ne.'Y') goto 21
cc ndisc now from WINPRT.INI file via common/dpp/
cc	ndisc=10			!1=C,2=D,3=E,4=F,5=G, 6=H, 7=I, 8=J, 9=K, 10=L
cc	if(machine.eq.1) ndisc=4	!1=C,2=D,3=E,4=F for Tosh
c      print 52
c      if(pon()) write(7,52)
c      if(discprt) write(8,52)
c52	format(' OLD-STYLE DATA FILES')
c	do i=1,ndisc
c	  ndev=char(i+66)//':'
c	  INFIL=ndev//'\SCDIRm.DAT'
c	  INQUIRE(file=INFIL,exist=present,flen=nlen)
c	  if(present.and.nlen.gt.0) then
c            OPEN(unit=18,file=INFIL,status='old',
c     &      ACCESS='DIRECT',FORM='UNFORMATTED',RECL=128,err=61)
c	      read(18,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole
c		if(whole) then
c	       print 44,ndev,name,idisc,jfirst,nexpt
c	       if(pon()) write(7,44) ndev,name,idisc,jfirst,nexpt
c		 if(discprt) write(8,44) ndev,name,idisc,jfirst,nexpt
c44		 format(1x,a2,1x,a12,' disc # ',i2,
c     &	 '  -contains expts ',i4,' to ',i4,' Complete disc')
c		else
c	       print 45,ndev,name,idisc,jfirst,nexpt
c	       if(pon()) write(7,45) ndev,name,idisc,jfirst,nexpt
c		 if(discprt) write(8,45) ndev,name,idisc,jfirst,nexpt
c45		 format(1x,a2,1x,a12,' disc # ',i2,
c     &	 '  -contains expts ',i4,' to ',i4,' INCOMPLETE disc')
c		endif
c	      CLOSE(unit=18)
c	   endif
c	enddo
c      print 521
c      if(pon()) write(7,521)
c      if(discprt) write(8,521)
c521	format(' DATA FILES FOR NEW SCAN')
c	do i=1,ndisc
c	  ndev=char(i+66)//':'
c	  INFIL=ndev//'\SCANDAT.DAT'
c	  INQUIRE(file=INFIL,exist=present,flen=nlen)
c	  if(present.and.nlen.gt.0) then
c		irdup=1456664	!byte # to start writing duplicate record
c	      OPEN(unit=18,file=INFIL,status='OLD',access='TRANSPARENT')
c		F18open=.true.
cc         NB idisc is integer*2 for old type files!
c	      read(18,rec=1) name,idiscn,nfirst,nfiles,jstrec,nextrec
c	      read(18,rec=irdup) name1,idiscn1,nfirst1,nfiles1,jstrec1,
c     &	  nextrec1
c	      call CHECKDUP(name,idiscn,nfirst,nfiles,jstrec,nextrec,
c     &	  name1,idiscn1,nfirst1,nfiles1,jstrec1,nextrec1,same)
c		nspace=irdup-nextrec+1
c	      print 53,ndev,name,idisc,nfirst,nfiles,nspace
c	      if(pon()) write(7,53) ndev,name,idisc,nfirst,nfiles,nspace
c	      if(discprt) write(8,53) ndev,name,idisc,nfirst,nfiles,nspace
c53	      format(1x,a2,1x,a12,' disc # ',i2,
c     &    '  -contains expts ',i4,' to ',i4,' ; free bytes left = ',i8)
c	      CLOSE(unit=18)
c	      F18open=.false.
c		if(.not.same) then
c		   call BELL(3)
c		   print 8,ndev
c8		   format(
c     &	' DUPLICATE FILES ON DATA DISK DO NOT AGREE:',/,
c     &	'  -CHECK DISK ',a2,' AND TRY AGAIN')
c	      endif
c	   endif
c	enddo
c
21	continue
	ndev='A:'		!default
61	iask=-1
	idef=1
	print 32
32	format(/,
     &' Specify disk partition for single channel data:')
	call DISCNUM1(id,ndev,iask,idef)	!which disc?
	if(id.eq.-1) goto 999
c Check for data files in root segment
	newscan=.false.
	print 611
611	format(
     &    ' (1) Directory for new SCAN',/,
     &    ' (2) Old-style directory',/,
     &    ' Option number [1] = ')
	iopt=1
	call INPUTi(iopt)
	newscan=.true.
	if(iopt.eq.2) newscan=.false.
	infil1=ndev//'\SCANDAT.DAT'
	call TITENT0('Data file name:',infil1,33,.false.)
	INQUIRE(file=infil1,exist=present,flen=nlen,err=700)
	if((.not.present).or.nlen.eq.0) then
	   call BELL(2)
	   print 612,infil1
612	   format(' Can not find ',a33)
	   goto 61
	endif
      print 62,infil1
      if(pon()) write(7,62) infil1
      if(discprt) write(8,62) infil1
62	format(' Data file: ',a33)
c
	if(newscan) then
c	  INFIL=ndev//'\SCANDAT.DAT'
	  irdup=1456664	!byte # to start writing duplicate record
	  OPEN(unit=18,file=INFIL1,status='OLD',access='TRANSPARENT')
	  F18open=.true.
	  read(18,rec=1) name,idiscn,nfirst,nfiles,jstrec,nextrec
	  irec=jstrec(nfiles-nfirst+1)
	  nspace=irdup-nextrec+1
	  print 420,name,idiscn,nfirst,nfiles,nextrec,nspace,infil1
	  if(pon()) write(7,420)name,idiscn,nfirst,nfiles,nextrec,nspace,
     &	infil1
	  if(discprt)write(8,420)name,idiscn,nfirst,nfiles,nextrec,nspace,
     &	infil1
420	   format(1x,a12,' Data disc number ',i2,/,
     &   '  -contains experiments ',i4,' to ',i4,/,
     &   ' Next vacant byte # = ',i8,' ; free bytes left = ',i8,/,
     &   ' (from: ',a33,')')
	else
c	   infil=ndev//'\SCDIRm.DAT'
         OPEN(unit=18,file=INFIL1,status='old',
     &    ACCESS='DIRECT',FORM='UNFORMATTED',RECL=128,err=61)
c
	   read(18,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,iexp1,iexp2
	   print 42,name,idisc,jfirst,nexpt,nxtrec
	   if(pon()) write(7,42) name,idisc,jfirst,nexpt,nxtrec
	   if(discprt) write(8,42) name,idisc,jfirst,nexpt,nxtrec
42	   format(1x,a12,' Floppy disc number ',i2,/,
     &	'  -contains experiments ',i4,' to ',i4,/,
     &	' Next vacant record in SCDAT.DAT= ',i6)
	   if(whole) print 421
421	   format(' Complete data disc')
	   if(.not.whole) call bell(2)
	   if(.not.whole) print 422,iexp1,iexp2
422	   format(
     &	' Incomplete data disc- experiments ',i5,' to ',i5,' only')
	endif
c
	print 423
423	format(' Alter these values [N] ? ')
	ans='N'
	call INPUTa(ans)
C	read 101,ans
	if(UC(ans).ne.'Y') goto 43
c
	if(.not.newscan) then
	   CALL TITENT('Enter owners name: ',NAME,12)
	   print 242
242	   FORMAT(' floppy disc number= ')
	   read 6,idisc
	   print 1242
1242	   format(' Number of first expt on this floppy= ')
	   read 6,jfirst
	   print 205
205	   FORMAT('  Total no of experiments up to last on this disc = ')
	   read 6,Nexpt
	   print 207
207	   FORMAT('  No of next vacant record in SCDAT.DAT= ')
	   read 6,nxtrec
	   print 424
424	   format('  Is this a complete data disc [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	   whole=UC(ans).eq.'Y'
	   write(18,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,iexp1,iexp2
         print 40
         if(pon()) write(7,40)
         if(discprt) write(8,40)
40	   format(' Record 1 of directory changed as follows:')
	   print 42,name,idisc,jfirst,nexpt,nxtrec
	   if(pon()) write(7,42) name,idisc,jfirst,nexpt,nxtrec
	   if(discprt) write(8,42) name,idisc,jfirst,nexpt,nxtrec
	   if(whole) print 421
	   if(.not.whole) print 422,iexp1,iexp2
	else		!NEW SCAN
	   print 305
305	   format(' Total no of experiments up to last on this disc = ')
	   read 6,nfiles
	   write(18,rec=1) name,idiscn,nfirst,nfiles,jstrec,nextrec
	endif
c======insert alterations for newscan case?
43	continue
	if(newscan) then
	   if(pon()) write(7,420) name,idiscn,nfirst,nfiles,nextrec,
     &	nspace,infil1
	   if(discprt) write(8,420)name,idiscn,nfirst,nfiles,nextrec,
     &	nspace,infil1
	else
	   if(pon().and.whole) write(7,421)
	   if(discprt.and.whole) write(8,421)
	   if(pon()) write(7,42) name,idisc,jfirst,nexpt,nxtrec
	   if(discprt) write(8,42) name,idisc,jfirst,nexpt,nxtrec
	   if(.not.whole.and.pon()) write(7,422) iexp1,iexp2
	   if(.not.whole.and.discprt) write(8,422) iexp1,iexp2
	endif
c
255	print 46	!READ RECORDS?
46	FORMAT(' Read directory records [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	IF(UC(ans).eq.'N') GOTO 260
	isca=nfirst		!default
	iscb=nfiles
	print 210,isca,iscb
210	FORMAT(
     &'& Between experiments: N1 (,N2) [',i4,' - ',i4,'] = ')
	call INPUT2i(isca,iscb)
	if(iscb.lt.isca) iscb=isca
	if(newscan) then
	   if(isca.lt.nfirst) isca=nfirst
	   if(iscb.gt.nfiles) iscb=nfiles
	   print 312		!COLUMN HEADINGS
	   if(pon()) write(7,312)
	   if(discprt) write(8,312)
312	   format(/,
     &    ' File #:   Title',/)
c     &   ' Date of expt ',T22,'Title',/,
c     &   ' # of Trans  1st Rec  # Rec  Cal fac   Mean amp(pA)',
c     &   ' RMS noise(pA) Filter(kHz)',/)
c Loop starts here
	   do ifile=isca,iscb
		irec=jstrec(ifile-nfirst+1)	!where to start reading header
		read(18,rec=irec)expnum,nfile,kt,iscanver		!read version
		if(iscanver.ne.-3001) then
		   read(18,rec=irec)expnum,nfile,kt,iscanver,expdate,titlen,
     & 	   tapeID,ipatch,Emem,temp,ffilt,avamp,rms,calfac2,ioffset,
     & 	   imin,imax,cjump,nsweep,tzero,tsamp
		else		!-3001=simulated data
		   read(18,rec=irec)expnum,nfile,kt,iscanver,expdate,titlen,
     & 	   tapeID,ipatch,Emem,temp,ffilt,avamp,rms,calfac2,ioffset,
     & 	   treso,tresg
		endif
		if(iscanver.lt.101) cjump=.false.	!not defined on disk
		amp=avamp*calfac2
		filt=1000.*ffilt	 !in Hz
		if(iscanver.eq.-1001) then
		   filt=-1.
		   rms=0.
		endif
		if(iscanver.ge.99) then
	         print 313,nfile,titlen,expdate,tapeID,
     &	    ptype(ipatch),kt,Emem,temp,filt,amp,rms
c     &	    ptype(ipatch),kt,Emem,temp,filt,amp,rms,calfac2
      	   if(pon()) write(7,313)nfile,titlen,expdate,tapeID,
     &	   ptype(ipatch),kt,Emem,temp,filt,amp,rms
	         if(discprt) write(8,313)nfile,titlen,expdate,tapeID,
     &	    ptype(ipatch),kt,Emem,temp,filt,amp,rms
313		   format(' Ex',i5,': ',a70,/,
     &	'   Expt date ',a11,': Tape ',a24,': Patch ',a14,/,
     &	1x,i6,' transitions: Membrane pot ',f8.2,'mV: Temperature ',
     &	   f7.1,/,
     &	   '   Filter (-3dB)',f9.1,'Hz : Full amp (app) ',f8.2,'pA',
     &	   ': RMS noise ',f9.3,'pA.')
c     &	   ' Calibration factor = ',/)
		   if(cjump) then
		      print 48,nsweep,tsamp*1.e-3,tzero*1.e-3
		      if(pon()) write(7,48) nsweep,tsamp*1.e-3,tzero*1.e-3
		      if(discprt) write(8,48) nsweep,tsamp*1.e-3,tzero*1.e-3
48			format('   CJUMP data:',i5,' jumps, sample length ',
     &		 g13.6,' ms, tzero = ',f11.2,' ms.')
		   endif
	         print 108
      	   if(pon()) write(7,108)
      	   if(discprt) write(8,108)
		else if(iscanver.eq.-3001) then	!converted simulations disk
		   to=1000.*treso
		   tg=1000.*tresg
	         print 320,nfile,titlen,expdate,kt,to,tg,filt,amp,rms
      	   if(pon()) write(7,320) nfile,titlen,expdate,kt,to,tg,
     &		filt,amp,rms
	         if(discprt) write(8,320)nfile,titlen,expdate,kt,to,tg,
     &		filt,amp,rms
320		   format(' Ex',i5,': ',a70,/,' Analysis date ',a11,
     &	   ':  ',i6,' transitions. Simulated data; res = ',
     &	   f7.1,',',f7.1,' microsec (open, shut)',/,
     &	   ' Filter (-3dB)',f9.1,' Hz : Full amp (app) ',f8.2,'pA',
     &	   ': RMS noise ',f9.3,'pA',/)
		else if(iscanver.lt.0) then	!converted old disk
	         print 319,nfile,titlen,expdate,kt,filt,amp,rms
      	   if(pon()) write(7,319) nfile,titlen,expdate,kt,filt,
     &		amp,rms
	         if(discprt) write(8,319)nfile,titlen,expdate,kt,filt,
     &		amp,rms
319		   format(' Ex',i5,': ',a70,/,' Analysis date ',a11,
     &	   ':  ',i6,' transitions (data from PDP SCAN)',/,
     &	   ' Filter (-3dB)',f9.1,' Hz : Full amp (app) ',f8.2,'pA',
     &	   ': RMS noise ',f9.3,'pA',/)
		endif
		if(mod(ifile-isca+1,5).eq.0) then	!5 expts fit on screen
		    print 414
414		    format('&Hit any key for more . . .')
		    call ANYKEY
		endif
	   enddo		!end of loop for each file
	else
c Now old type
	   if(isca.lt.jfirst) isca=jfirst
	   if(iscb.gt.nexpt) iscb=nexpt
c new layout:
	   print 212		!COLUMN HEADINGS
	   if(pon()) write(7,212)
	   if(discprt) write(8,212)
212	   format(/,
     &   ' Exp #   Date ',T22,'Title',/,
     &   ' # of Trans  1st Rec  # Rec  Cal fac   Mean amp(pA)',
     &   ' RMS noise(pA) Filter(kHz)',/)
c
	   ISCA1=ISCA-jfirst+2
	   ISCB1=ISCB-jfirst+2
c
C  ISCAN=-1001 IS A FLAG TO SHOW DIRECTORY AND DIST-FITTING PROGS THAT
C THE DATA POINTED TO BY THIS DIRECTORY RECORD IS IN THE FORMAT PRODUCED
C BY SCAN1 (RATHER THAN BY OLD SCFIT8).
c  Iscan=-1002 means that ISCAN is followed by valid values of JFILT
c and RMS (used in new EKDIST to calc false event rate)
c  Iscan=-1003 is as -1002 but indicates that analysis done with
c version of progs in which 'dubious' replaces 'assumed' amplitudes
c  Iscan=-2001 means data from SCANF
c  Iscan=-3001 means data from SCSIM
	   DO 213 I1=ISCA1,ISCB1
		I=I1
		READ(18,rec=I)ADATE,TITLE1,ISTREC,NREC,NINT1,ACAL,
     &	IAVAMP,ISCAN,jfilt,rms
		scndat=(ISCAN.ge.-1003.and.iscan.le.-1001).or.iscan.eq.
     & 	-2001.or.iscan.eq.-3001
		if(iscan.eq.-1001) jfilt=-1
		if(iscan.eq.-1001) rms=0.
c reprint titles every 10
		nm=mod(i1-isca1,10)
		if(nm.eq.0.and.i1.ne.isca1) then
		   print 212		!COLUMN HEADINGS
		   if(pon()) write(7,212)
		   if(discprt) write(8,212)
		endif
		print 214,i-2+jfirst,adate,title1
		if(pon()) write(7,214) i-2+jfirst,adate,title1
		if(discprt) write(8,214) i-2+jfirst,adate,title1
214		format(1x,'(',i4,')',1x,3a4,1x,a56)
		if(.not.scndat) then
      	   print 60
	         if(pon()) write(7,60)
	         if(discprt) write(8,60)
60		   format(' DATA IN OLD (SCFIT) FORMAT')
		endif
		if(iscan.eq.-2001) print 218
		if(pon().and.iscan.eq.-2001) write(7,218)
		if(discprt.and.iscan.eq.-2001) write(8,218)
218		format(' DATA FROM THRESHOLD-CROSSING FIT')
		if(iscan.eq.-3001) print 2181
		if(pon().and.iscan.eq.-3001) write(7,2181)
		if(discprt.and.iscan.eq.-3001) write(8,2181)
2181		format(' SIMULATED DATA')
		print 215,nint1,istrec,nrec,acal,
     &	  acal*float(iavamp),acal*rms,jfilt
		if(pon()) write(7,215)nint1,istrec,nrec,acal,
     &	  acal*float(iavamp),acal*rms,jfilt
		if(discprt) write(8,215)nint1,istrec,nrec,acal,
     &	  acal*float(iavamp),acal*rms,jfilt
215	 	format(5x,i5,3x,i5,3x,i4,2x,3g13.6,i6)
		if(nm.eq.9.and.i1.ne.iscb1) then
		   print 216
216		   format('  -type any key for more')
		   call ANYKEY
		endif
c
213	   CONTINUE	!end of loop for each expt
	endif 	!end of old style
c
	call flush(7)
	GOTO 255
C
260	print 261
261	format(' List any of the data [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).ne.'Y') goto 80
99	continue
	if(newscan.and.(.not.F18open)) then
c	   INFIL=ndev//'\SCANDAT.DAT'
	   OPEN(unit=18,file=INFIL1,status='OLD',access='TRANSPARENT')
	   F18open=.true.
	endif
	if(.not.newscan.and.(.not.dfopen)) then
	   infil=ndev//'\SCDATm.DAT'
         OPEN(unit=12,file=INFIL,status='old',
     &    ACCESS='DIRECT',FORM='UNFORMATTED',RECL=1024)
	   dfopen=.true.
	endif
C NOTE: RECSIZE=256 real=512 INTEGERS=1024 bytes
5	continue
C
	if(newscan) then
411	   print 57,nfirst,nfiles
57	   FORMAT('&List experiment no (between ',i4,' and ',i4,') = ')
	   call INPUTi(ifile)
	   if(ifile.lt.nfirst.or.ifile.gt.nfiles) then
		call BELL(2)
		print 31,ifile
c31		format(' Expt #',i4,' not on this disc')
		goto 411
	   endif
	   read(18,rec=1) name,idiscn,nfirst,nfiles,jstrec,nextrec
	   irec=jstrec(ifile-nfirst+1)	!where to start reading header
	   read(18,rec=irec)expnum,nfile,kt,iscanver,expdate,titlen,
     &    tapeID,ipatch,Emem,temp,ffilt,avamp,rms,calfac2,ioffset,
     & 	imin,imax
	   irec1=irec+ioffset      !where to start writing data -as before
	   amp=avamp*calfac2
	   filt=1000.*ffilt	 !in Hz
	   nint1=int2(kt)
	   nexpt=int2(nfiles)
	   jfirst=int2(nfirst)
	   if(iscanver.ge.99) then
	      print 313,nfile,titlen,expdate,tapeID,
     &       ptype(ipatch),kt,Emem,temp,filt,amp,rms
	   else if(iscanver.lt.0) then
	      print 319,nfile,titlen,expdate,kt,filt,amp,rms
	   endif
	   print 314
314      format(' - O.K. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(UC(ans).eq.'N') goto 260
	   if(iscanver.ge.99) then
            if(pon()) write(7,313)nfile,titlen,expdate,tapeID,
     &       ptype(ipatch),kt,Emem,temp,filt,amp,rms
	      if(discprt) write(8,313)nfile,titlen,expdate,tapeID,
     &       ptype(ipatch),kt,Emem,temp,filt,amp,rms
	   else if(iscanver.lt.0) then	!converted old disk
	      print 319,nfile,titlen,expdate,kt,filt,amp,rms
      	if(pon()) write(7,319) nfile,titlen,expdate,kt,filt,
     &	  amp,rms
	      if(discprt) write(8,319)nfile,titlen,expdate,kt,filt,
     &	  amp,rms
	   endif
	   kt1=kt
	   if(kt.gt.20480) then
		call BELL(2)
		print 315
315		format(' CAN READ ONLY FIRST 20480 VALUES AT PRESENT')
c =====make tint() etc allocatable in next version -and pass size to RESINT
		kt1=20480
	   endif
	   read(18,rec=irec1)
     &     (tint(i),i=1,kt1),(iampl(i),i=1,kt1),(iprops(i),i=1,kt1)
	   nint1=int2(kt1)
	   goto 316
	endif
c
c
c Now version for old disks
311	print 47,jfirst,nexpt
47	FORMAT('&List experiment no (between ',i4,' and ',i4,') = ')
	read 3,NSC
3	FORMAT(I8)
	if(nsc.lt.jfirst.or.nsc.gt.nexpt) then
	   call BELL(2)
	   print 31,nsc
31	   format(' Expt #',i4,' not on this disc')
	   goto 311
	endif
	NSC1=NSC-jfirst+2
	READ(18,rec=NSC1)ADATE,TITLE1,ISTREC,NREC,NINT1,ACAL,
     & IAVAMP,ISCAN,jfilt,rms
	scndat=(ISCAN.ge.-1003.and.iscan.le.-1001).or.iscan.eq.-2001.
     & or.iscan.eq.-3001
	if(iscan.eq.-2001) print 218
	if(pon().and.iscan.eq.-2001) write(7,218)
	if(discprt.and.iscan.eq.-2001) write(8,218)
	if(iscan.eq.-3001) print 2181
	if(pon().and.iscan.eq.-3001) write(7,2181)
	if(discprt.and.iscan.eq.-3001) write(8,2181)
	if(scndat.and.(iscan.eq.-1001.or.iscan.eq.-1002)) print 1031
	if(pon().and.scndat.and.(iscan.eq.-1001.or.iscan.
     & eq.-1002)) write(7,1031)
	if(discprt.and.scndat.and.(iscan.eq.-1001.or.iscan.
     & eq.-1002)) write(8,1031)
1031	format(' All ''last'' and ''average'' amplitudes set ''assumed''')
	if(iscan.eq.-1003) print 1032
	if(pon().and.iscan.eq.-1003) write(7,1032)
	if(discprt.and.iscan.eq.-1003) write(8,1032)
1032	format(' Only ''dubious'' amplitudes set ''assumed''')
c
	print 2161,ADATE
	if(pon()) write(7,2161) ADATE
	if(discprt) write(8,2161) ADATE
2161	FORMAT(1X,3A4)
	print 2172,TITLE1
2172	FORMAT(1X,a56,/'  O.K. [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).ne.'N') GOTO 41
	GOTO 5
41	CONTINUE	!READ DATA
	if(pon()) write(7,70) nsc,adate,title1
	if(discprt) write(8,70) nsc,adate,title1
70	format(' Experiment',i5,2x,3a4,/,1x,a56)
C
c Read the data
	k=ISTREC		!START RECORD FROM DIRECTORY
	read(12,rec=k) (TINT(J),J=1,256)
	if(nint1.gt.256) then
		k=k+1
	     	read(12,rec=k) (TINT(J),J=257,512)
	endif
	if(nint1.gt.512) then
		k=k+1
		read(12,rec=k) (TINT(J),J=513,768)
	endif
	if(nint1.gt.768) then
		k=k+1
		read(12,rec=k) (TINT(J),J=769,1024)
	endif
	k=k+1
	read(12,rec=k) (IAMPL(J),J=1,512)
	if(nint1.gt.512) then
		k=k+1
		read(12,rec=k) (IAMPL(J),J=513,1024)
	endif
c
c From here, similar for old and new directories
316	continue
	tmax1=-1.e37
	tmax2=-1.e37
	tmin1=1.e37
	tmin2=1.e37
	do 50 i=1,nint1
	   if(newscan) then
		open=iampl(i).ne.0
	   else
		open=iacal(i,iampl).ne.0
	   endif
	   if(open) then
      	if(tint(i).gt.tmax1) tmax1=tint(i)
		if(tint(i).lt.tmin1) tmin1=tint(i)
	   else
      	if(tint(i).gt.tmax2) tmax2=tint(i)
		if(tint(i).lt.tmin2) tmin2=tint(i)
	   endif
50	continue
      print 51,tmin1,tmax1,tmin2,tmax2
      if(pon()) write(7,51) tmin1,tmax1,tmin2,tmax2
      if(discprt) write(8,51) tmin1,tmax1,tmin2,tmax2
51	format(' Open times (ms): min, max = ',2g13.6,/,
     & ' Shut times (ms): min, max = ',2g13.6)
C
	nint=nint1	!unless changed by RESINT
c
	RESIMP=.FALSE.
c	goto 234	!UNTIL RESINT FIXED
	print 233
233	FORMAT( ' Impose resolution [N] ? ')
	ans='N'
	call INPUTa(ans)
	RESIMP=.FALSE.
	if(UC(ans).ne.'Y') GOTO 234
	RESIMP=.TRUE.
	SUBLEV=.FALSE.
	IF(iscan.eq.-2001) GOTO 504	!READ FIRST ONE
c For data from simulation prog (iscan=-3001) may, or may not,have more
c than 1 open level (no info in directory about this)- if have only one
c then do not want to ask if sublevels to be analysed!- now check this
	if(iscan.ne.-3001) goto 5021
c for sim data only, calc mean of non-zero amps
	s=0.
	j=0
	do 5031 i=1,nint1
	I1=I
	if(iacal(I1,iampl).eq.0) goto 5031
	s=s+float(iacal(i1,iampl))
	j=j+1
5031	continue
	abar=s/float(j)
	print 5041,acal*abar
5041	format(' Mean amp= ',g13.6)
	do 5051 i=1,nint1
	I1=I
	if(iacal(i1,iampl).eq.0) goto 5051
c if all amps not same, goto 5021
	if(abs(float(iacal(i1,iampl))-abar).gt.1.0) goto 5021
5051	continue
	goto 5061	!with sublev=.false.
5021	print 502
502	FORMAT( ' Sublevels to be analysed [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).EQ.'Y') SUBLEV=.TRUE.
	if(iscan.eq.-1001.or.iscan.eq.-2001) jfilt=-1	!no filter in dir
c NB xsec set to -1001 so only that part of RESINT needed here is done
c NB care with integer*2 needed in resint call
	imode=2	!for call of RESINT from SCDIR
5061	continue
	iavamp4=int4(iavamp)
	jfilt4=int4(jfilt)
	call RESINT(TINT,nint1,nint,acal,iavamp4,imode,iampl,
     & sublev,jfilt4,fc,rms,trise,ihjc)
504	CONTINUE
	print 108
234	print 236
236	FORMAT(' Type some values [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).EQ.'N') GOTO 235
	print 2321,NINT
2321	FORMAT('&No of intervals= ',I5,' List elements N1,N2= ')
	call INPUT2i(n1,n2)
c
	if(newscan) then
	   if(iscanver.ge.99) then
            print 317
            if(pon()) write(7,317)
            if(discprt) write(8,317)
317	      format(
     &'     i        duration (ms)   amplitude (pA)   properties')
	   else if(iscanver.lt.0) then	!converted old disk
            print 321
            if(pon()) write(7,321)
            if(discprt) write(8,321)
321	      format(
     &'     i        duration (ms)   amplitude (pA)')
	   endif
c
	   do i=n1,n2
	      print 318,i,tint(i),calfac2*float(iampl(i)),iprops(i)
      	if(pon()) write(7,318)i,tint(i),calfac2*float(iampl(i)),
     &	   iprops(i)
	      if(discprt) write(8,318) i,tint(i),calfac2*float(iampl(i)),
     &	   iprops(i)
318		format(i8,3x,g13.6,3x,g13.6,3x,i6)
	   enddo
	else
	   do i1=n1,n2
		I=I1
		IF(IABS(int4(IAMPL(I))).LT.9999.) print 345,
     &	 I,TINT(I),ACAL*FLOAT(IACAL(I,iampl))
		IF(IABS(int4(IAMPL(I))).GT.9999.) print 3451,
     &	 I,TINT(I),ACAL*FLOAT(IACAL(I,iampl))
		if(debug()) then
		   print 3452,iampl(i)
		   if(pon()) write(7,3452) iampl(i)
		   if(discprt) write(8,3452) iampl(i)
3452		   format(20x,' iampl(i)= ',i8)
		endif
		IF(pon().and.IABS(int4(IAMPL(I))).LT.9999.) write(7,345)
     &	 I,TINT(I),ACAL*FLOAT(IACAL(I,iampl))
		IF(discprt.and.IABS(int4(IAMPL(I))).LT.9999.) write(8,345)
     &	 I,TINT(I),ACAL*FLOAT(IACAL(I,iampl))
		IF(pon().and.IABS(int4(IAMPL(I))).GT.9999.) write(7,3451)
     &	 I,TINT(I),ACAL*FLOAT(IACAL(I,iampl))
		IF(discprt.and.IABS(int4(IAMPL(I))).GT.9999.) write(8,3451)
     &	 I,TINT(I),ACAL*FLOAT(IACAL(I,iampl))
231		FORMAT(I8,3(2X,G13.6))
345		FORMAT(I8,3X,G13.6,1X,G13.6)
3451		FORMAT(I8,3X,G13.6,1X,G13.6,'*')	!ASTERISK=ASSUMED AMP
	   enddo
	endif
	call flush(7)
	print 108
	if(pon()) write(7,108)
	if(discprt) write(8,108)
108	FORMAT(/)
	GOTO 234
235	print 30
30	FORMAT('&List extreme intervals [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).ne.'Y') GOTO 262
	print 302
302	FORMAT('& Long (L) or short (S) ? ')
	read 101,ans
	IL=0
	if(UC(ans).EQ.'L') IL=1
	print 38
38	FORMAT( ' openings (O) or gaps (G) ? ')
	read 101,ans
	IG=0
	if(UC(ans).EQ.'G') IG=1
	print 36
36	FORMAT( ' T crit (real ms)= ')
	read 37,TCRIT
37	FORMAT(G13.6)
	J=0
	DO 107 I1=1,NINT
	I=I1
c===========fixed for newscan up to here
	IF((IACAL(I,iampl).EQ.0.AND.IG.EQ.0).OR.(IACAL(I,iampl).NE.0
     &	.and.IG.EQ.1)) GOTO 107	!TRY NEXT
	IF((IL.EQ.1.AND.TINT(I).LT.TCRIT).OR.(IL.EQ.0.AND.TINT(I).GT.
     &	TCRIT)) GOTO 107	!TRY NEXT
	J=J+1
	print 35,J,I,TINT(I)
	if(pon()) write(7,35) J,I,TINT(I)
	if(discprt) write(8,35) J,I,TINT(I)
35	FORMAT(2I8,2X,G13.6)
107	CONTINUE
	call flush(7)
C
C SECTION TO ALTER DATA OR DIRECTORY
262	IF(RESIMP) GOTO 255   !MUST NOT WRITE ALTERED DATA BACK TO DISC!!
	wdat=.false.
	wdir=.false.
80	print 81
81	format(' Alter any of the values [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).ne.'Y') goto 98
c
	if(newscan) goto 11
c
441	print 1440
1440	FORMAT( ' ALTER:'
     &	,/,' (0) No more alterations'
     &	,/,' (1) Time interval values'
     &	,/,' (2) Amplitude values'
     &	,/,' (3) Amplitude calibration factor'
C    &	,/,' (4) ORDER OF ALL ELEMENTS'
     &	,/,' (5) Title of experiment'
     &	,/,' (6) Nint, iavamp,jfilt,rms.'
     &	,/,' (7) Record number for directory data'
C    &	,/,' (8) REMOVE SOME LINES COMPLETELY'
C    &	,/,' (9) LIST UNUSABLE INTERVAL NUMBERS'
     &	,/,' (10) Scale all amplitudes (ADC values), and CALFAC'
     &	,/,' N= ')
	read 6,N
	IF(N.EQ.0) GOTO 277	!WRITE ALTERED DATA TO DISC
	GOTO (401,402,403,404,405,406,407,408,409,410),N
401	print 268
268	FORMAT('&Number to be altered= ')
	read 6,NBAD
	DO 269 I=1,NBAD
	print 301
301	FORMAT('&index= ')
	read 6,IN
	print 1270,IN
1270	FORMAT('&TINT(',I2,') (ms)= ')
269	read 37,TINT(IN)
	wdat=.true.
	GOTO 441
C
402	print 268
	read 6,NBAD
	DO 273 I=1,NBAD
	print 301
	read 6,IN
	print 1274,IN
1274	FORMAT('&IAMPL(',I2,') (ADC units)= ')
273	read 6,IAMPL(IN)
	wdat=.true.
	GOTO 441
C
403	continue
	print 4021,acal
4021	format(' Amplitude calibration= ',g13.6,': new value = ')
	read 37,acal
	wdir=.true.
	GOTO 441
C
404	continue
c	print 279,NOPEN
c279	FORMAT( ' NOPEN= ',I8,/,' WORK ON FIRST N ELEMENTS. N= ')
c	read 6,NMAX
c	print 278
c278	FORMAT( ' MOVE LAST N1 OF THESE TO BEGINNING. N1= ')
c	read 6,N1
c	CALL VROT(TOPEN,NOPEN,NMAX,N1)
c	CALL VROT(TGAP,NOPEN,NMAX,N1)
c	CALL VROT(AMPL,NOPEN,NMAX,N1)
	GOTO 441
C
405	call TITENT0('Enter title for experiment: ',title1,56,.false.)
	wdir=.true.
	GOTO 441
C
406	print 1450,nint,jfilt,acal*float(Iavamp),acal*rms
1450	FORMAT(
     & ' Nint, filter (integer kHz), full amp(pA), rms noise(pA)= '
     & ,/,2i8,2g13.6,/,
     & '   new values= ')
	read 415,NINT,jfilt,AVAMP,rms
415	format(2i8,2g13.6)
	iavamp=ifixr(avamp/acal)
	rms=rms/acal
	wdir=.true.
	GOTO 441
C
407	print 453
453	FORMAT( ' RECORD # IN SCDIR.DAT FOR DIRECTORY DATA=')
	read 6,NSC1
	wdir=.true.
	GOTO 441
C
408	continue
	GOTO 441
c
C LIST UNUSABLE GAPS
409	continue
	GOTO 441
C Scale IAMPL and CALFAC
c To change all the amplitudes in a specified SC data file ('expt')
c and change the calibration factor accordingly.  This is needed if
c we want to pool experiments with different calibration factors, to
c make calfac same for all (because EKDIST uses only a single CALFAC
c value).  Can happen with SCAN5 (PDP version when display gain factor
c is different for analysing different expts!!)
410	continue
	print 4101
4101	format(
     &' The amplitudes (IAMPL) will all be divided by the specified ',/,
     &' factor, amd CALFAC will me multiplied by the same factor:',/,
     &' Factor = ')
	call INPUTr(fac)
	acal=acal*fac
c  NB must remove any added/subtracted 10000 before dividing by FAC
	do 4102 i=1,nint
	   iamp=IACAL(i,iampl)				!remove 10000 if present
	   iamp1=ifixr(float(iamp)/fac)		!divide by fac
	   if(iampl(i).gt.9999) then			!restore 10000 if necessary
		iamp1=iamp1+10000
	   else if(iampl(i).lt.-9999) then
		iamp1=iamp1-10000
	   endif
	   iampl(i)=iamp1		!new value back into IAMPL()
4102	continue
	wdir=.true.
	wdat=.true.
	GOTO 441
C
C WRITE ALTERED DATA BACK
277	CONTINUE
c
	if(wdir) WRITE(18,rec=NSC1)ADATE,TITLE1,ISTREC,NREC,NINT,ACAL,
     & IAVAMP,ISCAN,jfilt,rms
C
C
	if(.not.wdat) goto 98
	k=ISTREC		!START RECORD FROM DIRECTORY
	write(12,rec=k) (TINT(J),J=1,256)
	if(nint.gt.256) then
		k=k+1
	     	write(12,rec=k) (TINT(J),J=257,512)
	endif
	if(nint.gt.512) then
		k=k+1
		write(12,rec=k) (TINT(J),J=513,768)
	endif
	if(nint.gt.768) then
		k=k+1
		write(12,rec=k) (TINT(J),J=769,1024)
	endif
	k=k+1
	write(12,rec=k) (IAMPL(J),J=1,512)
	if(nint.gt.512) then
		k=k+1
		write(12,rec=k) (IAMPL(J),J=513,1024)
	endif
c
	goto 260	!list again?
c
c END OF ALTERATIONS FOR OLD DIRECTORIES
c
c ALTERATIONS FOR NEW DIRECTORIES
11	continue
	print 12
12	format( ' ALTER:'
     &	,/,' [0] No more alterations'
     &	,/,' (1) Title of experiment'
     &	,/,' (2) Version number (iscanver)'
     &	,/,' (3) Filter cut-off frequency'
     &	,/,' (4) Calibration (calfac2)'
     &	,/,' (5) Tape details'
     &	,/,' N = ')
	n=0
	call INPUTi(n)
	if(n.eq.0) goto 137	!write altered data to disc
	goto (601,602,603,604,605), n
c
601	continue	!new title
	call TITENT0('Enter title: ',title,70,.false.)
	wdir=.true.		!header altered
	goto 11
c
602	continue	!change version number
	print 6021,iscanver
6021	format(' Version number [',i5,'] = ')
	call INPUTi(iscanver)
	wdir=.true.		!header altered
	goto 11
c
603	continue	!change version number
	print 6031,filt
6031	format(' Filter cut-off frequency (HERTZ) (now ',f9.1,') = ')
	call INPUTr(filt)
	ffilt=filt*0.001	!ffilt is in kHz
	wdir=.true.		!header altered
	goto 11
c
604	continue	!calfac2
	print 6041,calfac2
6041	format(' calfac2 (intermed units-> pA) (now ',f9.1,') = ')
	call INPUTr(calfac2)
	wdir=.true.		!header altered
	goto 11
c
605	continue
	call TITENT0('Enter tape details: ',tapeid,24,.false.)
	wdir=.true.		!header altered
	goto 11
c
c	Write altered header back to disk
137	continue
	if(wdir) then
	   write (18,rec=irec)expnum,nfile,kt,iscanver,expdate,titlen,
     &    tapeID,ipatch,Emem,temp,ffilt,avamp,rms,calfac2,ioffset,
     &    imin,imax,cjump,nsweep,tzero,tsamp
	endif
c END OF ALTERATIONS FOR NEW DIRECTORIES
c
	goto 260	!list again?
c
98	print 97
97	format(' List another expt [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).eq.'Y') goto 99
999	IF(DFOPEN) CLOSE(UNIT=12)
	CLOSE(UNIT=18)
	call ENDPRINT
	goto 9999
c Error section
700	continue
	print 71
71	format(' Error in finding disk')
	if(ndev.eq.'A:') then
	   print 72
72	   format(
     &   '  -if floppy is not mounted, insert it now then hit enter.')
	endif
	call ANYKEY
	goto 21
9999	continue
	END

	SUBROUTINE VROT(BUF,NDIM,NMAX,NROT)
	REAL BUF(NDIM)
C MODIF SO MAX DIMENSION OF ARRAY IS NDIM (NOT > DIM IN
C CALLING PROG) BUT ROTATION APPLIED ONLY TO FIRST NMAX POINTS.
C ROTATES ELEMENTS OF REAL ARRAY BUF (VERSION FOR INTEGER
C ARRAY= VECROT.FOR). USED IN SCDIST.FOR.
	M=0
	K=0	!COUNTS NO OF ELEMENTS REASSIGNED
3	TEMP=BUF(NMAX-M)
	K1=NROT-M
	IF(K1.LT.1) K1=K1+NMAX		!NEEDED IF NROT NEGATIVE
	J1=NMAX-M
1	J=J1
	J1=J1-NROT
	IF(J1.LT.1) J1=J1+NMAX		!WRAP AROUND
	IF(J1.GT.NMAX) J1=J1-NMAX	!DITTO FOR NEG NROT
	BUF(J)=BUF(J1)
	K=K+1
	IF(J1.EQ.K1) GOTO 2
	GOTO 1
2	BUF(J1)=TEMP
	K=K+1
	IF(K.EQ.NMAX) RETURN
	M=M+1
	GOTO 3
	END


	FUNCTION IACAL(KT,iampl)
C TO GET AMPLITUDE IN ADC UNITS FROM IAMPL(KT). REMOVES 10000 IF
C THIS HAS BEEN ADDED  (FOR POS AMP) OR SUBRACTED (FOR NEG AMP)
C TO INDICATE 'ASSUMED' VALUE .
	integer*2 IAMPL(5120)
c	COMMON/ABLCK/IAMPL(1024)
C
	IX=0
	IF(KT.EQ.0) GOTO 1
	IX=IAMPL(KT)
	IF(IX.GT.9999) IX=IX-10000
	IF(IX.LT.-9999) IX=IX+10000
1	IACAL=IX
	RETURN
	END


