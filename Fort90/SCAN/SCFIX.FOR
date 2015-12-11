	PROGRAM SCFIX
C To rearrange HK's single channel data discs (modif from SCDIR)
c Version for MICRO
c Modified 07/08/92 10:20pm so another correction can be done, viz
c to change all the amplitudes in a specified SC data file ('expt')
c and change the calibration factor accordingly.  This is needed if
c we want to pool experiments with different calibration factors, to
c make calfac same for all (because EKDIST uses only a single CALFAC
c value).  Can happen with SCAN5 (PDP version when display gain factor
c is different for analysing different expts!!)
C LIST DATA AFTER IMPOSING RESOLUTION ON IT IF REQ.
C
C
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
	dimension TINT(1024)
c  for disc reads must specify integer*2
	INTEGER*2 IAMPL(1024)
	integer*2 Nexpt,nxtrec,idisc,jfirst,jfirst1,iexp1,iexp2
	integer*2 jfsav,nexsav
      integer*2 ISTREC,istrec1,NREC,nint,NINT1,IAVAMP,ISCAN,jfilt
	integer*2 jstrec(500)
	character*4 adate(3)
	character TITLE1*56,name*12
	character*1 ans,UC
	character ndev*2,ndev1*2,infil*14
	logical SCNDAT,pon,whole,slock
	logical debug,caplock,present,first,final,newfile
      character*32 filnam
	character*11 cdate,ctime
c
	character*40 mtitle		!machine names
	logical discprt
	common/dp/discprt
	common/dpp/filnam,machine,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c
C
	pon()=slock()
	debug()=caplock()
c
6	FORMAT(I8)
101	FORMAT(A1)
c
      filnam='SCFIX.PRT'
      OPEN(unit=7,file='PRN',iostat=nerr)		!open printer
	call WINPRINT	!print-file control
	print 1011
	if(pon()) write(7,1011)
	if(discprt) write(8,1011)
1011	format(' SINGLE CHANNEL DISC FIXER',/,
     & ' (Scroll-lock ON to print)')
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 1,cdate,ctime(1:8),mtitle
	if(pon()) write(7,1) cdate,ctime(1:8),mtitle
	if(discprt) write(8,1) cdate,ctime(1:8),mtitle
1	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     &	'   Machine = ',a40)
c
	print 20
20	format(' List names of single channel data files on discs [N] ? ')
	read 101,ans
	if(UC(ans).ne.'Y') goto 21
c ndisc now from WINPRT.INI file via common/dpp/
c	ndisc=10			!1=C,2=D,3=E,4=F,5=G, 6=H, 7=I, 8=J, 9=K, 10=L
c	if(machine.eq.1) ndisc=4	!1=C,2=D,3=E,4=F for Tosh
	do 9 i=1,ndisc
	  ndev=char(i+66)//':'
	  INFIL=ndev//'\SCDIRm.DAT'
	  INQUIRE(file=INFIL,exist=present,flen=nlen)
	  if(present.and.nlen.gt.0) then
            OPEN(unit=17,file=INFIL,status='old',
     &      ACCESS='DIRECT',FORM='UNFORMATTED',RECL=128,err=61)
	      read(17,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole
		if(whole) then
	       print 44,ndev,name,idisc,jfirst,nexpt
	       if(pon()) write(7,44) ndev,name,idisc,jfirst,nexpt
		 if(discprt) write(8,44) ndev,name,idisc,jfirst,nexpt
44		 format(1x,a2,1x,a12,' disc # ',i2,
     &	 '  -contains expts ',i4,' to ',i4,' Complete disc')
		else
	       print 45,ndev,name,idisc,jfirst,nexpt
	       if(pon()) write(7,45) ndev,name,idisc,jfirst,nexpt
		 if(discprt) write(8,45) ndev,name,idisc,jfirst,nexpt
45		 format(1x,a2,1x,a12,' disc # ',i2,
     &	 '  -contains expts ',i4,' to ',i4,' INCOMPLETE disc')
		endif
	      CLOSE(unit=17)
	   endif
9	  continue
c
21	continue
61	iask=-1           !Hard AND floppy
	idef=0		!no default
c	idef=1		!default in ndev
	print 110
110	format(' Specify disc where experiment(s) to be copied FROM')
	call DISCNUM1(id,ndev,iask,idef)	!which disc?
	if(id.eq.-1) goto 999
	ndev1='A:'		!default winchester
	idef=1		!default in ndev
	print 111
111	format(' Specify disc where experiment(s) to be copied TO')
	call DISCNUM1(id,ndev1,iask,idef)	!which disc?
	if(id.eq.-1) goto 999
c Open INPUT directory file (=17)
	infil=ndev//'\SCDIRm.DAT'
      OPEN(unit=17,file=INFIL,status='old',
     &ACCESS='DIRECT',FORM='UNFORMATTED',RECL=128,err=61)
c Open OUTPUT directory file (=18)
	infil=ndev1//'\SCDIRm.DAT'
	newfile=.false.
	INQUIRE(file=INFIL,exist=present,flen=nlen)
	if(.not.present.or.nlen.eq.0) then
	  newfile=.true.
        OPEN(unit=18,file=INFIL,status='new',
     &  ACCESS='DIRECT',FORM='UNFORMATTED',RECL=128)
        OPEN(unit=13,file=ndev1//'scdatm.dat',status='new',
     &   ACCESS='DIRECT',FORM='UNFORMATTED',RECL=1024)
	   CLOSE(unit=13)
	else
        OPEN(unit=18,file=INFIL,status='old',
     &  ACCESS='DIRECT',FORM='UNFORMATTED',RECL=128,err=61)
	endif
c
	first=.true.	!read rec #1 of input disc
	final=.false.	!true for final check
c
118	continue		!return to read output rec 1
c read input rec #1
	if(first) then
        print 321,ndev
        if(pon()) write(7,321) ndev
        if(discprt) write(8,321) ndev
321	  format(' INPUT DISC ',a2)
	  read(17,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,iexp1,iexp2
	  jfsav=jfirst
	  nexsav=nexpt
	else
        print 322,ndev1
        if(pon()) write(7,322) ndev1
        if(discprt) write(8,322) ndev1
322	  format(' OUTPUT DISC ',a2)
	  if(newfile) goto 121		!give details before reading
	  read(18,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,iexp1,iexp2
c	  jfsav1=jfirst
	endif
	print 42,name,idisc,jfirst,nexpt,nxtrec
	if(pon()) write(7,42) name,idisc,jfirst,nexpt,nxtrec
	if(discprt) write(8,42) name,idisc,jfirst,nexpt,nxtrec
42	format(1x,a12,' Floppy disc number ',i2,/,
     &	'  -contains experiments ',i4,' to ',i4,/,
     &	' Next vacant record in SCDAT.DAT= ',i6)
	if(whole) print 421
421	format(' Complete data disc')
	if(.not.whole) call bell(2)
	if(.not.whole) print 422,iexp1,iexp2
422	format(
     &	' Incomplete data disc- experiments ',i5,' to ',i5,' only')
	print 423
423	format(' Alter these values [N] ? ')
	read 101,ans
	if(UC(ans).ne.'Y') goto 43
c
121	continue
	call TITENT('Enter owners name: ',NAME,12)
	print 242
242	FORMAT( ' floppy disc number= ')
	read 6,idisc
	print 1242
1242	format (' Number of first expt on this floppy= ')
	read 6,jfirst
	print 205
205	FORMAT('  Total no of experiments up to last on this disc = ')
	read 6,Nexpt
	print 207
207	FORMAT('  No of next vacant record in SCDAT.DAT= ')
	read 6,nxtrec
	print 424
424	format('  Is this a complete data disc ? ')
	read 101,ans
	whole=UC(ans).eq.'Y'
	if(first) then
	  jfsav=jfirst
	  write(17,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,iexp1,iexp2
	else
c	  jfsav1=jfirst
	  write(18,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,iexp1,iexp2
	endif
      print 40
      if(pon()) write(7,40)
      if(discprt) write(8,40)
40	format(' Record 1 of directory changed as follows:')
	print 42,name,idisc,jfirst,nexpt,nxtrec
	if(pon()) write(7,42) name,idisc,jfirst,nexpt,nxtrec
	if(discprt) write(8,42) name,idisc,jfirst,nexpt,nxtrec
	if(whole) print 421
	if(.not.whole) print 422,iexp1,iexp2
c
43	continue
	if(pon().and.whole) write(7,421)
	if(discprt.and.whole) write(8,421)
	if(pon()) write(7,42) name,idisc,jfirst,nexpt,nxtrec
	if(discprt) write(8,42) name,idisc,jfirst,nexpt,nxtrec
	if(.not.whole.and.pon()) write(7,422) iexp1,iexp2
	if(.not.whole.and.discprt) write(8,422) iexp1,iexp2
c
	if(final) goto 119	!all done
	if(first) then
	   first=.false.	!read rec #1 of output disc
	   goto 118
	endif
c
311	continue
	jfirst=jfsav
	nexpt=nexsav
c	jfirst1=jfsav1
	print 112,ndev
112	format(' Transfer expts n1 to n2 FROM ',a2,': n1,n2 = ')
	call INPUT2i(n1,n2)
	if(n2.lt.n1) n2=n1
	if(n1.lt.jfirst.or.n2.gt.nexpt) then
	   call BELL(2)
	   print 31,n1,n2
31	   format(' Expts #',i4,' to ',i4,' not all on this disc')
	   goto 311
	endif
	ntrans=n2-n1+1	!number to be transferred
c
	print 113,ndev1
113	format(' New number for first transferred expt on ',a2,' nn1 = ')
	call INPUTi(nn1)
	nn2=nn1+ntrans-1
	print 114,ndev1,nn1
114	format(
     &' Record number in SCDATm.DAT on ',a2,' where data for ',/,
     &' expt ',i4,' is to start: istrec1 = ')
	call INPUTi(i)
	istrec1=int2(i)
	print 115,ndev1
115	format(
     &' Number of the FIRST expt on output disc ',a2,' jfirst1 = ')
	call INPUTi(i)
	jfirst1=int2(i)
	nn=nn1
      print 116,n1,n2,ndev,nn1,nn2,ndev1,jfirst1,nn1,ndev1,istrec1
      if(pon()) write(7,116) n1,n2,ndev,nn1,nn2,ndev1,jfirst1,nn1,
     & ndev1,istrec1
      if(discprt) write(8,116)n1,n2,ndev,nn1,nn2,ndev1,jfirst1,nn1,
     & ndev1,istrec1
116	format(
     &' Expts ',i4,' to ',i4,' on ',a2,' to be transferred to:',/,
     &' expts ',i4,' to ',i4,' on ',a2,' (on which first expt number = '
     &,i4,')',/,
     &' Data for expt ',i4,' on ',a2,' to start at record #',i4,
     &' in SCDATm.DAT')
c new layout:
	print 212		!COLUMN HEADINGS
	if(pon()) write(7,212)
	if(discprt) write(8,212)
212	format(/,
     &' Exp #   Date ',T22,'Title',/,
     & ' # of Trans  1st Rec  # Rec  Cal fac   Mean amp(pA)',
     & ' RMS noise(pA) Filter(kHz)',/)
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
c
	nn=nn1	!for expt number for output
	in=1		!index =1,2,...,ntrans
c
	do 213 n=n1,n2
	irec=n-jfirst+2	!input directory record no
	read(17,rec=irec)ADATE,TITLE1,ISTREC,NREC,NINT1,ACAL,IAVAMP,ISCAN,
     &jfilt,rms
	scndat=(ISCAN.ge.-1003.and.iscan.le.-1001).or.iscan.eq.-2001.
     & or.iscan.eq.-3001
	if(iscan.eq.-1001) jfilt=-1
	if(iscan.eq.-1001) rms=0.
	if(.not.scndat) then
		call bell(2)
		print 2,iscan
2		format('&Cannot read record (ISCAN = ',i5,')')
		goto 213	!try next
	endif
c reprint titles every 10
	nm=mod(n-n1,10)
	if(nm.eq.0.and.n.ne.n1) then
	   print 212		!COLUMN HEADINGS
	   if(pon()) write(7,212)
	   if(discprt) write(8,212)
	endif
	print 214,n,adate,title1
	if(pon()) write(7,214) n,adate,title1
	if(discprt) write(8,214) n,adate,title1
214	format(1x,'INPUT EXPT #(',i4,')',1x,3a4,1x,a56)
	if(iscan.eq.-2001) print 218
	if(pon().and.iscan.eq.-2001) write(7,218)
	if(discprt.and.iscan.eq.-2001) write(8,218)
218	format(' DATA FROM THRESHOLD-CROSSING FIT')
	if(iscan.eq.-3001) print 2181
	if(pon().and.iscan.eq.-3001) write(7,2181)
	if(discprt.and.iscan.eq.-3001) write(8,2181)
2181	format(' SIMULATED DATA')
	print 215,nint1,istrec,nrec,acal,acal*float(iavamp),acal*rms,jfilt
	if(pon()) write(7,215)nint1,istrec,nrec,acal,acal*float(iavamp),
     & acal*rms,jfilt
	if(discprt) write(8,215)nint1,istrec,nrec,acal,acal*float(iavamp),
     & acal*rms,jfilt
215 	format(5x,i5,3x,i5,3x,i4,2x,3g13.6,i6)
c Now OUTPUT
	jstrec(in)=istrec1		!record new start record for each expt
	irec1=nn-jfirst1+2
	write(18,rec=irec1)ADATE,TITLE1,istrec1,NREC,NINT1,ACAL,IAVAMP,
     &iscan,jfilt,rms
      print 117,nn,ndev1,istrec1
      if(pon()) write(7,117) nn,ndev1,istrec1
      if(discprt) write(8,117) nn,ndev1,istrec1
117	format(' OUTPUT as expt #',i4,' on ',a2,' with data to start',/,
     &'  at record # ',i5,' in SCDATm.DAT')
c	if(nm.eq.9.and.i1.ne.iscb1) then
c	   print 216
c216	   format('  -type any key for more')
c	   call ANYKEY
c	endif
c
c Prepare for next expt
	in=in+1
	nn=nn+1			!next output expt number
	istrec1=istrec1+nrec		!new start record for next expt
213	CONTINUE	!end of loop for directory records
c
	call flush(7)
c
c Fix record #1 of output directory now, before closing files? NO -
c must fix this separately using SCDIR because nexpt etc not known
c===========
c	write(18,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,iexp1,iexp2
c
c	CLOSE(unit=17)	!close input directory -NO
c	CLOSE(unit=18)	!close output directory
c
c Now transfer the data!
c  Open data files
	infil=ndev//'\SCDATm.DAT'
      OPEN(unit=12,file=INFIL,status='old',
     & ACCESS='DIRECT',FORM='UNFORMATTED',RECL=1024)
	infil=ndev1//'\SCDATm.DAT'
      OPEN(unit=13,file=INFIL,status='old',
     & ACCESS='DIRECT',FORM='UNFORMATTED',RECL=1024)
c
	nn=nn1	!for expt number for output
	in=1		!index =1,2,...,ntrans
c
	do 120 n=n1,n2
	irec=n-jfirst+2	!input directory record no
c read input directory
	read(17,rec=irec)ADATE,TITLE1,ISTREC,NREC,NINT1,ACAL,IAVAMP,ISCAN,
     &jfilt,rms
c read input data
	k=int4(ISTREC)		!START RECORD FROM DIRECTORY
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
c Now write the data to output disc
	istrec1=jstrec(in)	!new start record output data
	k=int4(ISTREC1)		!START RECORD FROM DIRECTORY
	write(13,rec=k) (TINT(J),J=1,256)
	if(nint1.gt.256) then
		k=k+1
	     	write(13,rec=k) (TINT(J),J=257,512)
	endif
	if(nint1.gt.512) then
		k=k+1
		write(13,rec=k) (TINT(J),J=513,768)
	endif
	if(nint1.gt.768) then
		k=k+1
		write(13,rec=k) (TINT(J),J=769,1024)
	endif
	k=k+1
	write(13,rec=k) (IAMPL(J),J=1,512)
	if(nint1.gt.512) then
		k=k+1
		write(13,rec=k) (IAMPL(J),J=513,1024)
	endif
c
      print 122,n,ndev,istrec,ndev1,istrec1
      if(pon()) write(7,122) n,ndev,istrec,ndev1,istrec1
      if(discprt) write(8,122) n,ndev,istrec,ndev1,istrec1
122	format(
     &' DATA for expt ',i4,' on ',a2,' read starting at record ',i5,/,
     &' and written to ',a2,' starting at record ',i5)
c Prepare for next expt
	in=in+1
	nn=nn+1			!next output expt number
120	continue		!end of data transfer loop
c
c Now return to check record #1 of output directory file
	first=.false.
	newfile=.false.
	final=.true.	!final check
	goto 118		!do check
119	continue		!and return to here
c
	CLOSE(unit=17)	!close input directory
	CLOSE(unit=18)	!close output directory
	CLOSE(unit=12)	!close input data file
	CLOSE(unit=13)	!close output data file
c
999	continue
	call ENDPRINT
	END

