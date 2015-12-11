=======to be converted
=======make special version for CJFIT/CJUMP?
=======keep separate dir/dat files or convert like the others?
=======put data as parameter not in common

	subroutine RELWRT(iprog,cdate,calfac)
C Lahey version 09/12/90 05:21pm
c Modif 07/16/91 09:30am RELWRT1 has dimensions of AVCUR,AVOLT as in CJVDU1
c Modif 01/10/91 09:40pm so title1 is 66 char (not 56) and cdate*11 used
c for dates
c To write experiments to a permanent file in TRANAV, CJUMP, CJFIT (can also
c keep as POSHPLOT but takes more space)
c
c Permanent files always on floppy disc- backup copy done in this
c subroutine also.
C
C NEW DIRECTORY NOTATION:
c---------------------------------------------------
C  old 	        new         meaning
c---------------------------------------------------
c directory record 1:
c  Nrel  	Total no of expts=relaxations (up to the
c			last on on the disc being read)
C  NXTREC	Next vacant record in RELDAT.DAT
C  IDISC	Floppy disc # (for particular owner)
c  JFIRST	No of the first experiment on this floppy (=old JINC+1)
C  NAME		Name of owner
C-------------------------------------------------------------------
c directory record for expt #iexp=directory record # iexp-jfirst+2
C  ISTREC	No of 1st record of the data (in RELDAT.DAT)
C  NREC		No of records occupied by the data in RELDAT.DAT
C
C
c Declarations for data from CJFIT (iprog=1)
	real*4 avcur(20480),avolt(10240)
	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	integer*2 ilen2c(10),igap2c(10)	!integer*2 versions for disc
	integer*2 ilen2v(10),igap2v(10)
	integer*2 ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
	real*4 t1v(10),t2v(10),t1c(10),t2c(10)
	logical vjump,sampv,control
c Declaration for values from TRANAV (iprog=2)
	real*4 AVTRAN(8192)	!(called avcur in main prog)
c
	character cdate*11,cdate1*11
	character title1*66,name*12,ndev*2
	character datfil*15,dirfil*15
	character*1 ans,UC
	LOGICAL OVWRIT,LASTEX,DEBUG,backup,whole,keepall
	logical e1,e2
	logical discprt,pon,slock
	common/dp/discprt
c COMMON for params from CJFIT
=======avcur, avolt removed from common
	COMMON/cjpar/nsweep,
     & naver,navc,control,vjump,sampv,nsamp,itsamp,nAv,dnAv,
     & nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,calvolt,
     & irate,iTPREc,nAc,dnAc,ncjump,ilenc,igapc,
     & t1v,t2v,t1c,t2c,nDv,iDd,keepall,ivhold
c
c COMMON for params from TRANAV (NB AVCUR, NAVER in main prog renamed as
c AVTRAN,NMEAN in common to avoid clash with common/cjpar/
	COMMON/tranpar/avtran,nmean,npav,srate
c
	pon()=slock()
C
101	format(a1)
3	FORMAT(I8)
c
c	print 1		!moved to main prog (better for CJFIT)
c1	format(' Keep data on disc [Y] ? ')
c	input 101,ans
c	if(UC(ans).eq.'N') RETURN
11	continue
	ndev='A:'		!default = floppy
	call DISCNUM(id,ndev,-1,1)	!which disc?
	if(id.eq.-1) RETURN		!if escape hit in DISCNUM
	DIRFIL=ndev//'\RELDIR.DAT'
	DATFIL=ndev//'\RELDAT.DAT'
	if(ndev.eq.'A:') pause ' Now mount data floppy:'
c Find amount of free space on floppy so can check if there is room to write
c the current expt (NB disc can be completely filled because in Lahey Fortran
c there is no need to specify in advance the number of records in files)
111	continue
	nbd=0
	nbp=0
	INQUIRE(file=DIRFIL,exist=e1,flen=n1)
	INQUIRE(file=DATFIL,exist=e2,flen=n2)
	if(.not.e1.or.(.not.e2)) then
	   print 498
498	   format(' Data files not found on this disc:',/,
     &  ' (1) Try a different disc',/,
     &  ' (2) Initialise new data files on disc',/,
     &  '  Option number [1] = ')
	   input 3,iopt
	   if(iopt.eq.0) iopt=1
	   if(iopt.eq.1) goto 11
	   goto 502
	endif
	if(e1) nbd=n1
	if(e2) nbp=n2
	nbtot=nbd+nbp
	nbfree= 1457664-nbtot	!number of free bytes left on 1.44Mb floppy
	pused=100.*float(nbtot)/1457664.
c	if(debug()) print 44,nbd,nbp,nbtot,nbfree
c	print 44,nbd,nbp,nbtot,nbfree
c	if(pon()) write(7,44) nbd,nbp,nbtot,nbfree
c44	format(' Bytes for RELDIR,RELDAT = ',2i9,/,
c     & ' Total bytes used = ',i8,'; total free(1.44Mb floppy) = ',i8)
c
	BACKUP=.FALSE.
251	if(backup) goto 456
c
      OPEN(unit=19,file=DIRFIL,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=128)
c NB recl is in bytes (4*value for PDP)
C NOTE: RECSIZE=32 real =64 INTEGERS =128 bytes
C
	read(19,rec=1)Nrel,nxtrec,Jdisc,jfirst,name,whole
C nrel=HIGHEST experiment NUMBER IN RELDAT.DAT.
C NXTREC POINTS TO NEXT VACANT RECORD IN RELDAT.DAT.
C JDISC=NUMBER OF rel DISC BEING READ
C JFIRST=NO OF 1ST experiment ON THIS DISC
	print 42,name,jdisc,jfirst,nrel
42	format(1x,a12,' Relaxation data disc number ',i2,/,
     & '  -contains events ',i4,' to ',i4)
	print 44,nbd,nbp,nbtot,pused
44	format(' Bytes for RELDIR,RELDAT = ',2i9,/,
     & ' Total used = ',i8,'; percent used (1.44Mb disc) = ',f7.1,/)
	if(whole) goto 460
	call bell(2)
	print 461
461	format(' CAUTION: THIS IA AN INCOMPLETE DATA DISC',/)
460	CONTINUE
C RECORDS 2 ONWARDS OF DIRECTORY CONTAIN DATA FOR experiment #ISC
c IN RECORD # ISC-jfirst+2
C
c Have 128 real values per record so no of data records needed=
	if(iprog.eq.1) then
	   nrec0=1 + (nsamp-1)/128            ! For CJFIT data -AVCUR
	   nrec=nrec0+1			!+one record for ilenc,ivolt etc arrays
	   if(sampv) nrec=nrec+nrec0		  ! another nrec0 for AVOLT
	else if(iprog.eq.2) then
	   nrec=1 + (npav-1)/128		   !for TRANAV data
	endif
c
	nbreq=512*nrec + 128	!bytes req for data + directory record
c
14	continue
	read(19,rec=1) nexpold,nxtrec,Jdisc,jfirst,name
	CLOSE(UNIT=19)
	nexpnew=nexpold+1	!current relaxation no=new total no of experiments if not
c			!overwritten
	nxtrnew=nxtrec+NREC	!New 'next vacant record' if ditto
	irecdir=nexpnew-jfirst+2	!record no for directory data in
c				!RELDIR.DAT for current experiment if ditto
	KRS=nxtrec		!Start record no for data in RELDAT.DAT
c				!if ditto
c
	Nex=nexpold+1		!no of next relaxation
c
	if(nbreq.le.nbfree) then
	   print 8,nex,nxtrec
8	   FORMAT(' Next experiment #= ',I6,',   next vacant record= '
     &   ,i6,/,'  O.K. [Y] ? ')
	   input 101,ans
	   OVWRIT=.FALSE.
	   LASTEX=.FALSE.
	   if(UC(ans).eq.'N') then
83		print 81
81		format(
     &	' (1) Overwrite an existing relaxation',/,
     &	' (2) Abandon',/,
     &	' Option number [2] = ')
		input 3,iopt
		if(iopt.le.0) iopt=2
		if(iopt.eq.1) goto 82
		if(iopt.eq.2) RETURN
		goto 83		!no valid option
	   else
		goto 9                !if ans=Yes
	   endif
	else if(nbreq.gt.nbfree) then
831	   print 811
811	   format(
     &   ' Not enough room to hold this relaxation on floppy:',/,
     &   ' (1) Overwrite an existing relaxation',/,
     &   ' (2) Make a new data disc now (need 2 formatted floppies',/,
     &   ' (3) Abandon (rerun this program when new disc made)',/,
     &   ' Option number [2] = ')
	   input 3,iopt
	   if(iopt.le.0) iopt=2
	   if(iopt.eq.1) goto 82
	   if(iopt.eq.2) goto 502
	   if(iopt.eq.3) goto 402
	   goto 831		!no valid option
	endif
c TOTAL NO OF relaxation UNCHANGED IF OVERWRITE,UNLESS
c LAST relaxation IS OVERWRITTEN/ (LASTEX=TRUE) IN WHICH CASE HAVE
c SPACE
82	continue
c Next section done when next relaxation NOT O.K.
	nexpnew=nexpold	!No of relaxation unchanged if ovwrit
	nxtrnew=nxtrec	!New 'next vacant record' if ovwrit
	OVWRIT=.TRUE.
	print 10
10	FORMAT(' Overwrite experiment number [0 to abandon] = ')
	input 3,iexpold
	if(iexpold.le.0) RETURN
	IF(iexpold.EQ.nexpold) LASTEX=.TRUE.
c I.E. relaxation TO BE OVERWRITTEN IS LAST ONE IN RELDAT.DAT
c NEXT READ DIRECTORY FOR OLD DATA
	irdold=iexpold-jfirst+2
      OPEN(unit=19,file=DIRFIL,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=128)
	read(19,rec=irdold) iprog1,cdate1,TITLE1,ISTRC1,nrecold
	CLOSE(UNIT=19)
c*
	Nex=iexpold		!relaxation NO IF OVERWRITE
	irecdir=irdold	!REC FOR DIRECTORY DATA IF OVERWRITE
c
c Now define new 'next vacant record' if ovwrit (unchanged
c unless it is last relaxation on the disc that is altered)
	if(LASTEX.and.ovwrit) then
	   nxtrnew=istrc1+nrec
c	   If new relaxation to overwrite the last one is no longer than that
c	   overwritten then there is room for it, but if new one is longer
c	   there may not be room- check now. Old record has directory record
c	   already, so extra space req=number of extra data records to be
c	   written: orig data is in nrecold records, current relaxation is in nrec
c	   records so need 512*(nrec-nrecold) more bytes than the nbtot
c	   that are already occupied
	   if(nrec.gt.nrecold) then
		nbreq1=512*(nrec-nrecold)
		if(nbreq1+nbtot.gt.1457664) then
		   print 86
86		   format(' Not enough room on disc')
		   goto 402
		endif
	   endif
	endif
c
c IF LAST relaxation IS OVERWRITTEN HAVE GOT SPACE FOR ANY AMOUNT
c OF DATA ON DISK
c IF WANT TO WRITE (EXCEPT IN LAST relaxation) MUST NEXT SEE IF THERE
c IS ROOM ON DISK FOR NEW DATA i.e. new must not occupy more records
c than old
	if((.not.lastex).and.ovwrit.and.(nrec.gt.nrecold)) goto 405
	print 2161,cdate1
2161	FORMAT(1X,a11)
	print 2171,TITLE1
2171	FORMAT(1X,a66,/,' will be altered:   O.K. [Y] ? ')
	input 101,ans
	if(UC(ans).EQ.'N') GOTO 14
c
c NOW WRITE DATA
303	KRS=ISTRC1	!START REC FOR RELDAT.DAT IF OVERWRITE
c
9	continue		!jump here for default 'next expt='
456	continue		!return here to write to backup disc
      OPEN(unit=20,file=DATFIL,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=512)
	KRSV=KRS
	if(iprog.eq.1) then
c	  First write misc values to a RELDAT record, then data array(s)
	  call ILCONV(ilenc,igapc,ilenv,igapv,
     &	ilen2c,igap2c,ilen2v,igap2v,iDd,0)	!convert ilenc to ilen2c etc
	   write(20,rec=krs) naver,navc,nsamp,itsamp,calfac,calvolt,
     &   vjump,sampv,nAv,dnAv,iTPREv,ivhold,irate,iTPREc,nAc,dnAc,
     &   ilen2c,igap2c,ilen2v,igap2v,ivolt1,ivolt2,
     &   t1v,t2v,t1c,t2c,nDv,iDd
	   krs=krs+1
c	  Now data array(s)
	   n=1
	   do 501 j=1,nrec0
	   m=n+127			!128 REALS=256 WORD=1 RECORD
	   write(20,rec=krs) (AVCUR(i),i=n,m)
	   krs=krs+1
	   n=n+128
501	   continue
	   if(sampv) then
		n=1
		do 521 j=1,nrec0
		m=n+127			!128 REALS=256 WORD=1 RECORD
		write(20,rec=krs) (AVOLT(i),i=n,m)
		krs=krs+1
		n=n+128
521		continue
	   endif
	else if(iprog.eq.2) then
	   n=1
	   do 522 j=1,nrec
	   m=n+127			!128 REALS=256 WORD=1 RECORD
	   write(20,rec=krs) (AVTRAN(i),i=n,m)
	   krs=krs+1
	   n=n+128
522	   continue
	endif
	CLOSE(unit=20)
c
	if(.not.backup) then
	   print 16,NREC,KRSV,KRS-1
	   if(pon()) write(7,16)NREC,KRSV,KRS-1
         if(discprt) write(8,16)NREC,KRSV,KRS-1
16	   FORMAT(' Relaxation stored in',I3,' records',/,
     &   '  from ',I4,' to ',I4)
	else
         print 457
         if(pon()) write(7,457)
         if(discprt) write(8,457)
457	   format(' Backup copy made')
	   goto 458
	endif
c   UPDATE DIRECTORY
	call TITENT(' Enter experiment title:',title1,66)
	if(pon()) write(7,17)Nex,title1
      if(discprt) write(8,17)Nex,title1
17	format(' Relaxation no: ',i8,/,' Title: ',a66)
458	continue
      OPEN(unit=19,file=DIRFIL,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=128)
c
	WRITE(19,rec=1) nexpnew,nxtrnew,Jdisc,jfirst,name,WHOLE
c nxtrnew=NEXT VACANT RECORD # IN RELDAT.DAT
	KRS=KRSV	!START RECD FOR THIS DATA IN RELDAT.DAT
c			!also resets KRS ready for backup-write of PSNET.DAT
c NB date occupies 12 characters (3a4) in PDP versions but only char*8 for
c Lahey date subroutine. DC subroutine DATE1 outputs data in char*11 form
c so, for compatability with PDP data discs, use DATE1 but add an extra
c blank character at end.
	call DATE1(cdate)
	istrec=krs
	if(iprog.eq.1) then
c	   WRITE(19,rec=irecdir,ERR=21) iprog,cdate,TITLE1,istrec,nrec,
	   WRITE(19,rec=irecdir) iprog,cdate,TITLE1,istrec,nrec,
     &   control,ncjump,nvjump,nsweep	!116 bytes (recl=128)
	else if(iprog.eq.2) then
	   WRITE(19,rec=irecdir,ERR=21) iprog,cdate,TITLE1,istrec,nrec,
     &   nmean,npav,srate
	endif
	CLOSE(unit=19)
	if(backup.or.ndev.ne.'A:') RETURN
	pause ' Now mount backup floppy: type <RETURN> when ready'
	backup=.true.
	goto 456
c
c Error messages:
21	print 211
211	FORMAT( ' Write error for file: ')
	goto 24
c20	print 212
c212	format(' Error in opening file ')
c	goto 24
24	call bell(4)
	print 26
26	format(' Change disc and try again? ')
	input 101,ans
	if(UC(ans).eq.'N') RETURN
	goto 251
c
c Section to deal with too much data/disc full
402	call bell(5)
c now PRINT instructions for making new disc
	print 452,name,jdisc+1,nex
	if(pon()) write(7,452)name,jdisc+1,nex
      if(discprt) write(8,452)name,jdisc+1,nex
452	format(
     & ' Make a new disc: procede as follows after leaving',/,
     & '  this program.',/,
     & ' (1) Make a new disc with the NEW DATA DISC option: specify:',/,
     & '     Owner:',a12,'  Disc number= ',i3,/,
     & '     ''No of the first relaxation to go on this disc''= ',i6,/,
     & ' (2) Run this program again and ask to go',/,
     & '     ''straight to write to permanent file'' to record',/,
     & '     the present experiment.',/)
 	pause ' Type <RETURN> to continue'
	RETURN
C
c Section to make new data floppies here
502	continue
	print 503
503	format(' Initialise a new relaxation data floppy disc.',/,
     & ' Insert a formatted floppy in A:',/,
     & ' Type <ENTER> when ready')
	input 3,i	!=pause
	print 504,name
504	format(' Owner''s name = ',a12,'  O.K. [Y] ? ')
	input 101,ans
	if(UC(ans).eq.'N') then
	   call TITENT('Enter owner''s name:',name,12)
	endif
	jdisc1=jdisc+1
	print 505,jdisc1
505	format('&Disc number = ',i4,'  O.K. [Y] ? ')
	input 101,ans
	if(UC(ans).eq.'N') then
	   print 506
506	   format('&Disc number = ')
	   input 3,jdisc1
	endif
	jfirst=nex
	print 507,jfirst
507	format(
     & '&Number of the first relaxation to go on this disc = ',i4,
     & '  O.K. [Y] ? ')
	input 101,ans
	if(UC(ans).eq.'N') then
	   print 508
508	   format(
     &  '&Number of the first relaxation to go on this disc = ')
	   input 3,jfirst
	endif
	nrel=jfirst-1
	nxtrec=1
	whole=.true.
      OPEN(unit=19,file=DIRFIL,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=128)
	write(19,rec=1)Nrel,nxtrec,Jdisc1,jfirst,name,whole
	close(unit=19)
      OPEN(unit=20,file=DATFIL,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=512)
	close(unit=20)
	print 509
509	format(
     & ' Now mount a second formatted floppy to initialise as backup',
     & ' disc',/,' Type <ENTER> when ready')
	input 3,i	!=pause
      OPEN(unit=19,file=DIRFIL,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=128)
	write(19,rec=1)Nrel,nxtrec,Jdisc1,jfirst,name,whole
	close(unit=19)
      OPEN(unit=20,file=DATFIL,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=512)
	close(unit=20)
	ndev='A:'
	goto 111
c
c
405	call bell(3)
	print 455
455	format(
     & ' The present data occupy more blocks than the experiment',/,
     & '  that they replace.')
	goto 14
c
C END OF WRITE TO RELDAT.DAT
	END



