	SUBROUTINE DISKIN(TINT,IAMPL,NINTt,ACAL,IAVAMP,idisk,
     & SUBLEV,jfilt,rms,idest,nfile,kfile,trise,fc,iscan,
     & TINT1,nlon,nloff,tjlen,ttoff,toffset,nintt1,nintt2,treso,
     & tresg,ndev,ndev1,first,readini,calcor,acalmin)
c NB set-up number=jsetup (prev=jfilt1) also kept in directory from Jan 1989
c  so should be made an arg here for use in EKDIST!
c 05/26/89 08:38pm Lahey version of DSKIN1 for EKDIST (DEBUG not a param)
c Modif 02/18/93 10:34pm to cope with cases when pooled expts have not
c  all got the same amplitude calibration factor (acal).
c Modified 12/02/91 03:09pm to read first latency values from CJFIT1: values
c	kept on standard single channel floppies (SCDIR, SCDAT) but have only
c	TINT(i) which are all shut times = latency to first opening. Signalled
c	by setting iscan=-4001.  Extra variables to be kept are
c		nlon=number of 1st latency values for ON jump
c		 -store these in TINT(1) to TINT(nlon)
c		nloff=number of 1st latency values for OFF jump
c		 -store these in TINT1(10240,2) (equivalened to TINT(20480))
c		(stored in disk in CJFIT as TINT1(512), TINT2(512) for on
c			and off latencies in each expt -i.e up to 512 latencies
c			per expt)
c		tjlen=length of jump (real milliseconds) (=truncation point
c		  for ON jump latencies)
c		ttoff=truncation point for OFF latencies (=time from end of
c		  OFF jump (corrected for offset) to end of the sample)
c		toffset= time from trigger to when jump actually starts at
c		  the patch
c		nlon,nloff,tjlen,ttoff,toffset=20 bytes (if all *4)
c		nintt1,nintt2= total number of ON/OFF latencies when several
c			expts are pooled (before resolution imposed)
c
c This version uses DISCNUM and DOPEN to choose and open files (Oct 88)
c For call to DOPEN(idata,ndev)
c	IDATA=0 to open directory on disc NDEV
c	IDATA=1 to open data file on disc NDEV
c Open directory on hard disc(unit=14)
c	call DOPEN2(0,ndev)
c Open directory on floppy (unit=16)
c	call DOPEN2(0,ndev1)
c Open data on hard disc (unit=12)
c	call DOPEN2(1,ndev)
c Open data on floppy (unit=13)
c	call DOPEN2(1,ndev1)
c
c** Modif APR-87 so only 2 units open at once. With 'OPEN' in
c subroutine 'assoc variable' does not seem to work even when
c in a global COMMON so update of record # done by prog now
c
C SUBROUTINE FOR OVERLAY IN EKDIST.FOR TO READ FROM SCDAT.DAT
C**N.B. COMPILE 'FORT/NOLINE/UNITS:7 EKDIST'-otherwise will get
c 'too many logical units open'
c Reads upto 5 expts all at once into expanded TINT,IAMPL arrays
c  NINT1=no of intervals in each file read (local to this subroutine)
c  NINTT=total no of intervals in all files before resolution imposed
c  NINT=total no of intervals AFTER resolution imposed (see RESINT)
c
c DISC CONVENTION. Data files need to be on hard disc for speed as need
c to be read in many times: to save copying whole of floppy onto DL1
c every time now have option to read in specified expts only
c  WHOLE=new logical variable added to directory (record 1)- when
c	true it indicates that this is a complete record of all expts
c	(all floppies should be). Set to false when copies of specified
c	expts only (iexp1 to iexp2) are copied onto DL1 for use in
c	EKDIST in which case record one contains WHOLE,IEXP1,IEXP so
c	can check that desired expts are present, and to prevent copy
c	of incomplete copies of SCDIR.DAT, SCDAT.DAT onto floppy.
c
c After 1st run, if resolution changed then original data (TINT,IAMPL)
c must be re-read, even if expt number not changed, as RESINT alters TINT
c Even if res not changed come back to DISKIN to write expt title
C
c  for disc reads must specify integer*2
c	read(14,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,ix1,ix2
c	read(16,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,ix1,ix2
c	READ(16,rec=NSC1)ADATE,TITLE1,ISTREC,NREC,NINT,ACAL,
c     & IAVAMP2,ISCAN2,jfilt2,rms,nlon,nloff,tjlen,ttoff,toffset
c	read(14,rec=1)Nexpt,nxtrec,jdisc,jfirst,name,whole,iexp1,iexp2
c	READ(14,rec=NSC1)ADATE1,TITLE1,ISTREC,NREC,NINT1,ACAL,
c     & IAVAMP2,ISCAN2,jfilt2,rms,nlon,nloff,tjlen,ttoff,toffset
c	write(14,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,iexp1,iexp2
c	write(14,rec=NSC1)ADATE,TITLE1,ISTREC,NREC,NINT,ACAL,
c     & IAVAMP2,ISCAN2,jfilt2,rms,nlon,nloff,tjlen,ttoff,toffset
c Of the integers iavamp,jfilt,iscan are parameters so read them as
c int*2 (iavamp2,jfilt2,iscan2) but use int*4 as parameters
	INTEGER*2 IAMPL(20480)
	integer*2 Nexpt,nxtrec,idisc,jdisc,jfirst,ix1,ix2,iexp1,iexp2
      integer*2 ISTREC,NREC,nint,NINT1,IAVAMP2,ISCAN2,jfilt2,jsetup2
	integer*2 nlon2,nloff2
	real TINT(20480)		!up to 20 *1024
	real TINT1(10240,2)		!EQUIV with TINT (used for latencies)
	real*4 acalib(20)			!to correct unequal ACAL in pooled expts
	integer ijfst(20),ijlast(20)	!ditto
	integer kfile(20)
	character title1*56,name*12
	character ans,UC,ndev*2,ndev1*2
	character infil*14
	character*4 adate(3),adate1(3)
	character filnam*30
	character*80 string
	character state
	character*15 fname /' '/
	LOGICAL DEBUG,SUBLEV,SCNDAT,pon,slock,caplock,whole,present
	logical sbin,shist,sres,sexp
	logical discprt,first,readini,calcor,iasc
	common/dp/discprt
	common/sblk/sbin,shist,sres,sexp
	common/dpp/filnam,machine,ndisc,icol	!for WINPRINT,ENDPRINT,DISCNUM
c	data ipassw/2102/
C
	pon()=slock()
	debug()=caplock()
c	if(.not.first) goto 307		! not first call to DISKIN
	iasc = .true.                 !
C
101	format(A1)
105	format(2I8)
108	format(/)
c
c
	if(.not.iasc)goto 281
	if(sexp)goto 257
	print 29
29	format(' Input data from ACSII file [Y] ? ')
	input 101,ans
	if(UC(ans).ne.'N')goto 28
	iasc=.false.
	goto 281
c
28 	   call DISCNUM1(id,ndev,-1,0)	!ask for winchester or floppy
     		 if(id.eq.-1) RETURN
	   print 251
251	   format(' Use default file name [patch.asc] [N] ? ')
	   input 101,ans
	   if(UC(ans).eq.'Y') then
		fname=ndev//'\patch.asc'
	   else
	      print 252
252	      format('&Input file name = ')
	      input 253,fname
253	      format(a15)
	      n=NBLANK(fname)
c Look for extension on file name. If '.' present assume its given, otherwise
c add extension .asc
	      do 254 i=1,n
254	       if(fname(i:i).eq.'.') goto 255	!extension given
	      fname=fname(1:n)//'.asc'			!add extension
	      n=n+4
255	      fname=ndev//'\'//fname(1:n)
	   endif
257	   INQUIRE(file=fname,exist=present,flen=len)
	   if(.not.present.or.len.eq.0) then
		call BELL(3)
		print 24,fname
24		format('&',1x,a15,' does not exist')
	      goto 307
	   else
	  if(present.and.len.gt.0) then
            OPEN(unit=18,file=fname,status='old',
     &      ACCESS='sequential',FORM='FORMATTED',RECL=80)
	   endif
	endif		!File now defined
c
      read(18,297)string
297	format(A80)
	print 279,string
279	format('&',1X,A80)
      read(18,298)string
298	FORMAT(A26)
	print 278,string
278	format('&',1X,A26)
	nintt = (len-106)/22
294	FORMAT(I8)
	Do 295 I=1,nintt
		read(18,296)ievt,tint(i),state
296		format(I5,1X,1G12.4,1X,A1)
		if(state.eq.'C')Iampl(i)=0
		if(state.eq.'S')Iampl(i)=1000
		if(state.eq.'O')Iampl(i)=2000
295	continue
	close(unit=18)
	do 259 j=1,20
		print *,j,tint(j),iampl(j)
259	continue
		i=i-1
		print *,i,tint(i),iampl(i)
	iscan = -2001
	jfilt = -1
	if(sexp)goto 4021
	print 245
245	format(' Input baseline rms value here [Y] ? ')
	input 101,ans
	if(UC(ans).ne.'N')then
		print 247
247		format(' Baseline rms (pA) = ')
		call Inputr(rms)
	else
		rms = 0.0
	endif
	print 235
235	format(' Input mean channel amplitude here [Y] ? ')
	input 101,ans
	if(UC(ans).ne.'N')then
		print 237
237		format(' Mean amplitude (pA) = ')
		call Inputr(rmamp)
		acal = 0.001
		iavamp = ifixr(rmamp/acal)
		iavamp2=int2(iavamp)	!integer*2 version for parameter
	else
 		iavamp2 = 1
		acal = 0.0
	endif
	rms=rms/acal
	jlast=nintt
c
	goto 4021	!Do resolution now
c
281	if(.not.first)goto 301
c	check which disk to use
c
c Section that is done first time only
c Check which disc to use
307	continue
	calcor=.false.	!calibration factor not adjusted
	print 30
30	format(' List names of single channel data files on discs [N] ? ')
	read 101,ans
	if(UC(ans).ne.'Y') goto 31
c ndisc now from WINPRT.INI file via common/dpp/
c	ndisc=10			!1=C,2=D,3=E,4=F,5=G, 6=H, 7=I, 8=J, 9=K, 10=L
c	if(machine.eq.1) ndisc=4	!1=C,2=D,3=E,4=F for Tosh
	do 9 i=1,ndisc
	  ndev1=char(i+66)//':'
	  INFIL=ndev1//'\SCDIRm.DAT'
	  INQUIRE(file=INFIL,exist=present,flen=nlen)
	  if(present.and.nlen.gt.0) then
            OPEN(unit=18,file=INFIL,status='old',
     &      ACCESS='DIRECT',FORM='UNFORMATTED',RECL=128,err=61)
	      read(18,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole
		if(whole) then
	       print 32,ndev1,name,idisc,jfirst,nexpt
	       if(pon()) write(7,32) ndev1,name,idisc,jfirst,nexpt
		 if(discprt) write(8,32) ndev1,name,idisc,jfirst,nexpt
32		 format(1x,a2,1x,a12,' disc # ',i2,
     &	 '  -contains expts ',i4,' to ',i4,' Complete disc')
		else
	       print 45,ndev1,name,idisc,jfirst,nexpt
	       if(pon()) write(7,45) ndev1,name,idisc,jfirst,nexpt
		 if(discprt) write(8,45) ndev1,name,idisc,jfirst,nexpt
45		 format(1x,a2,1x,a12,' disc # ',i2,
     &	 '  -contains expts ',i4,' to ',i4,' INCOMPLETE disc')
		endif
	      CLOSE(unit=18)
	   endif
9	  continue
c
31	continue
c61	iask=-1
61	iask=0		!ask for hard disc only
	idef=1		!default in ndev
	call DISCNUM1(id,ndev,iask,idef)
	if(id.eq.-1) goto 99
c
c
c Check which expts already on hard disc
c open directory on hard disc
306	call DOPEN(0,ndev)	!unit=14
	read(14,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,ix1,ix2
	print 426
426	format('  On hard disc:')
	if(.not.whole) goto 427
	print 42,name,idisc,jfirst,nexpt
42	format(1x,a12,' Data disc number ',i2,/,
     &	'  -contains experiments ',i4,' to ',i4)
	goto 4271
427	print 425,name,idisc,ix1,ix2
425	format(1x,a12,' Data disc number ',i2,/,
     &	' Incomplete data disc- experiments ',i5,' to ',i5,' only')
c
4271	print 3041
3041	format(
     &	' (1) Use this data disc ',/,
     &	' (2) Copy expts from floppy to hard disc ',/,
     &	' (3) Try a different hard disc ',/,
     &	' Option number [1] =  ')
	read 3,i
	if(i.eq.2) goto 311	!leave 14 open
	CLOSE(unit=14)
	if(i.eq.3) goto 307	!another hard disc
c For i=1 or default -go on
      if(pon()) write(7,63) name,idisc
      if(discprt) write(8,63) name,idisc
63	format(1x,a12,' Data disc number ',i2)
	goto 301		!if i=1 (or i=0, default)
c
c Now copy expts from floppy
311	continue
	ndev1='A:'
c Open directory on floppy (unit=16)
	call DOPEN(0,ndev1)
c
c Read directory record 1 to check what is on floppy
	read(16,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,ix1,ix2
	print 424,name,idisc,jfirst,nexpt,nxtrec
424	format(1x,a12,' Floppy disc number ',i2,/,
     &	'  -contains experiments ',i4,' to ',i4,/,
     &	' Next vacant record in SCDAT.DAT= ',i6)
	if(whole) print 421
421	format(' Complete data disc')
	if(.not.whole) call bell(2)
	if(.not.whole) print 422,ix1,ix2
422	format(
     &	' Incomplete data disc- experiments ',i5,' to ',i5,' only')
	print 423
423	format(' Correct floppy disc mounted [Y] ? ')
	read 101,ans
	if(UC(ans).ne.'N') goto 310
	close(unit=16)
	goto 311	!try another floppy
310	print 304
304	format(
     &' Copy expts N1 to N2 from floppy to hard disc (0=none):',/,
     &' N1,N2= ')
	read 105,iexp1,iexp2
	if(iexp1.le.0) goto 301
c
	whole=.false.
	write(14,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,iexp1,iexp2
	close(unit=14)
	close(unit=16)		!close before loop starts
c	unit=12 data on hard disc
c	unit=13 data on floppy
c	unit=14 directory on hard disc
c	unit=16 directory on floppy
c
	do 51 iexp=iexp1,iexp2
c Copy directory recd for expt # iexp
c Open directory on hard disc (unit=14)
	call DOPEN(0,ndev)
c Open directory on floppy (unit=16)
	call DOPEN(0,ndev1)
	NSC1=iexp-jfirst+2		!directory record no
	READ(16,rec=NSC1)ADATE,TITLE1,ISTREC,NREC,NINT,ACAL,IAVAMP2,
     & iscan2,jfilt2,rms,jsetup2,nlon2,nloff2,tjlen,ttoff,toffset,
     & treso,tresg
c NLON to end=values for =20 bytes (if all *4) -still 20 bytes spare
	iavamp=int4(iavamp2)	!integer*4 version for parameter
	iscan=int4(iscan2)	!integer*4 version for parameter
	jfilt=int4(jfilt2)	!integer*4 version for parameter
	nlon=int4(nlon2)
	nloff=int4(nloff2)
C  ISCAN=-1001 IS A FLAG TO SHOW DIRECTORY AND DIST-FITTING PROGS THAT
C THE DATA POINTED TO BY THIS DIRECTORY RECORD IS IN THE FORMAT PRODUCED
C BY SCAN1 (RATHER THAN BY OLD SCFIT8, for which ISCAN was undefined).
c  Iscan=-1002 means that ISCAN is followed by valid values of JFILT
c and RMS (used in new EKDIST to calc false event rate)
c  Iscan=-1003 is as -1002 but indicates that data analysed by version
c in which meaning of 'assumed' amplitude changed to 'dubious'
c  Iscan=-2001 means data from SCANF
c  Iscan=-3001 means data from SCSIM
c  Iscan=-4001 means data = first latencies from CJFIT1
	scndat=(ISCAN.ge.-1003.and.iscan.le.-1001).or.iscan.eq.-2001.
     & or.iscan.eq.-3001.or.iscan.eq.-4001
	if(iscan.eq.-1001) jfilt=-1
	if(iscan.eq.-1001) rms=0.
	if(.not.scndat) then
		call bell(4)
		print 2
2		format(' This program cannot read old (SCFIT) data')
		stop
	endif
	write(14,rec=NSC1)ADATE,TITLE1,ISTREC,NREC,NINT,ACAL,iavamp2,
     & iscan2,jfilt2,rms,jsetup2,nlon2,nloff2,tjlen,ttoff,toffset,
     & treso,tresg
c NLON2 to end=values for =24 bytes  -still 14 bytes spare
C Copy data records
	close(unit=14)
	close(unit=16)
c Open data on hard disc (unit=12)
	call DOPEN(1,ndev)
c Open data on floppy (unit=13)
	call DOPEN(1,ndev1)
c
	KRS=istrec		!START RECORD FROM DIRECTORY
	if(iscan.ne.-4001) then
	  READ(13,rec=KRS) (TINT(J),J=1,256)
	  if(nint.le.256) goto 320
	  krs=krs+1
	  READ(13,rec=KRS) (TINT(J),J=257,512)
	  if(nint.le.512) goto 320
	  krs=krs+1
	  READ(13,rec=KRS) (TINT(J),J=513,768)
	  if(nint.le.768) goto 320
	  krs=krs+1
	  READ(13,rec=KRS) (TINT(J),J=769,1024)
320	  krs=krs+1
	  READ(13,rec=KRS) (IAMPL(J),J=1,512)
	  if(nint.le.512) goto 321
	  krs=krs+1
	  READ(13,rec=KRS) (IAMPL(J),J=513,1024)
	else 		!i.e. first latency data (iscan=-4001)
	  READ(13,rec=KRS) (TINT1(j,1),j=1,256)		!on jump latencies
	  if(nlon.gt.256) then
	     krs=krs+1
	     READ(13,rec=KRS) (TINT1(j,1),j=257,512)       !on jump latencies
	  endif
	  krs=krs+1
	  READ(13,rec=KRS) (TINT1(j,2),j=1,256)          !off jump latencies
	  if(nloff.gt.256) then
	     krs=krs+1
	     READ(13,rec=KRS) (TINT1(j,2),j=257,512)
	  endif
	endif
c
321	continue
	KRS1=ISTREC		!START RECORD FROM DIRECTORY
	if(iscan.ne.-4001) then
	  write(12,rec=KRS1) (TINT(J),J=1,256)
	  if(nint.le.256) goto 322
	  krs1=krs1+1
	  write(12,rec=KRS1) (TINT(J),J=257,512)
	  if(nint.le.512) goto 322
	  krs1=krs1+1
	  write(12,rec=KRS1) (TINT(J),J=513,768)
	  if(nint.le.768) goto 322
	  krs1=krs1+1
	  write(12,rec=KRS1) (TINT(J),J=769,1024)
322	  krs1=krs1+1
	  write(12,rec=KRS1) (IAMPL(J),J=1,512)
	  if(nint.le.512) goto 323
	  krs1=krs1+1
	  write(12,rec=KRS1) (IAMPL(J),J=513,1024)
	else 		!i.e. first latency data (iscan=-4001)
	  write(12,rec=KRS1) (TINT1(j,1),j=1,256)
	  if(nlon.gt.256) then
	     krs1=krs1+1
	     write(12,rec=KRS1) (TINT1(j,1),j=257,512)       !on jump latencies
	  endif
	  krs1=krs1+1
	  write(12,rec=krs1) (TINT1(j,2),j=1,256)          !off jump latencies
	  if(nloff.gt.256) then
	     krs1=krs1+1
	     write(12,rec=krs1) (TINT1(j,2),j=257,512)
	  endif
	endif

323	  continue
c
1051	CONTINUE
	CLOSE(unit=12)
	CLOSE(unit=13)		!close data files
	if(pon()) write(7,10) iexp
	if(discprt) write(8,10) iexp
	print 10,iexp
10	format(' Experiment ',i6,' transferred')
51	continue		!end of iexp loop
c
c Copy onto hard disc completed
c
	idisk=1				!C: by default
c
c Jump to 301 after 1st call to DISKIN
c READ the DATA
c Open directory on hard disc (unit=14)
301	call DOPEN(0,ndev)
c
	read(14,rec=1)Nexpt,nxtrec,jdisc,jfirst,name,whole,iexp1,iexp2
	CLOSE(unit=14)
	if(whole) iexp1=jfirst
	if(whole) iexp2=nexpt
C NEXPT=HIGHEST EXPT NUMBER IN SCDAT.DAT.
C NXTRC POINTS TO NEXT VACANT RECORD IN SCDAT.DAT.
C JDISC=NUMBER OF  SC DISC BEING READ
C JFIRST=NO OF 1ST EXPT ON THIS DISC
C RECORDS 2 ONWARDS OF DIRECTORY CONTAIN DATA FOR SC EXPT #ISC
c IN RECORD # ISC-jfirst+2
c
c If amplitudes are read in here, which they will be unless 'sexp.and.sres'
c is true, then set calcor=false to indicate that ACAL has not been reset.
	if(.not.(sexp.and.sres)) calcor=.false.
	if(sexp) goto 302	!same expt as last time
c
	if(readini) then
	   print 55,nfile,kfile(1),(kfile(i),i=2,nfile)
55	   format(' Last run used ',i3,' experiment files, viz. ',
     &    i3,20(',',i3))
	   call DCASK('Use same experiments again','y',ans)
	   if(ans.eq.'Y') then
		goto 302	!read the expts
	   endif
	endif
207	continue
	print 400
400	FORMAT(' Pool experiments n1 to n2 [or specify each]: n1(,n2)= ')
	read 53,nsc1,nsc2
53	format(2i8)
	if(nsc1.gt.0) then
	   if(nsc2.eq.0) nsc2=nsc1
	   nfile=nsc2-nsc1+1
	   do 54 i=1,nfile
		j=nsc1+i-1	!expt number
		kfile(i)=j
54	   continue
	else if(nsc1.eq.0) then
	   print 52
52	   FORMAT(' Number of experiments to be pooled (<=20) = ')
	   read 3,NFILE
	   DO 249 I=1,NFILE
	   print 6
6	   FORMAT('&Read experiment no= ')
	   read 3,nsc
3	   format(i8)
	   kfile(i)=nsc
c Jump to 249 if expts already specified
249	   continue
	   goto 302		!read the expts
	endif
c
c	goto 302
c
302	index=1		!index in enlarged TINT etc
	index1=1		!index for on latencies
	index2=1		!index for off latencies
c
c Now read and pool the expts
	DO 401 I=1,NFILE
	nsc=kfile(i)
	if(nsc.lt.iexp1.or.nsc.gt.iexp2) then
	   call BELL(2)
	   print 309,nsc
309	   format(
     & ' Experiment ',i5,' not on current data disc: specify new disc')
	   idef=0		!no default
431	   call DISCNUM1(id,ndev,iask,idef)
	   call DOPEN(0,ndev)	!unit=14
	   read(14,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,ix1,ix2
	   print 42,name,idisc,jfirst,nexpt
	   call DCASK(' Is this disc O.K.','y',ans)
	   if(ans.eq.'N') then
		CLOSE(unit=14)
		goto 431
	   endif
	   CLOSE(unit=14)
	endif
	NSC1=NSC-jfirst+2
c Open directory on Winchester (unit=14)
c NB after first call to DISKIN an adjusted value of acal could be overwritten
c by following read.  The adjusted value is reset below if calcor=.true.
	call DOPEN(0,ndev)
	READ(14,rec=NSC1)ADATE1,TITLE1,istrec,nrec,nint1,acal,iavamp2,
     & iscan2,jfilt2,rms,jsetup2,nlon2,nloff2,tjlen,ttoff,toffset,
     & t1,t2
	iscan=int4(iscan2)
	acalib(i)=acal		!keep calibration for each expt
	if(iscan.eq.-4001) then	!resolution is on disc only for latencies
	   treso=t1
	   tresg=t2
	endif
	iavamp=int4(iavamp2)	!integer*4 version for parameter
	jfilt=int4(jfilt2)
	nlon=int4(nlon2)
	nloff=int4(nloff2)
	CLOSE(unit=14)
	scndat=(ISCAN.ge.-1003.and.iscan.le.-1001).or.iscan.eq.-2001.
     & or.iscan.eq.-3001.or.iscan.eq.-4001
	if(.not.scndat.or.iscan.eq.-1001) jfilt=-1	!signal to RESINT
c scndat=DATA FROM SCAN1 NOT SCFIT8
	if(.not.sexp) goto 113
	if(.not.first) goto 111		!skip after 1st time
113	if(iscan.eq.-2001) then
	   call BELL(2)
	   print 112
112	   format(' DATA FROM 50% THRESHOLD CROSSING')
c     &	' Please print password to procede. Password= ')
C	   call paswrd(ipass)	!so invisible
c	   read 3,ipass
c	   if(ipass.ne.ipassw) goto 99
	else if(iscan.eq.-4001) then
	   call BELL(2)
	   print 1121
1121     format(' DATA ARE FIRST LATENCIES FROM CJFIT')
	endif
111	continue
	if(sexp) goto 72
	if(iscan.eq.-1001.or.iscan.eq.-1002) print 1031
	if(pon().and.scndat.and.iscan.ne.-1003) write(7,1031)
	if(discprt.and.scndat.and.iscan.ne.-1003) write(8,1031)
1031	format(' All ''last'' and ''average'' amplitudes set ''assumed''')
	if(iscan.eq.-1003) print 1032
	if(pon().and.iscan.eq.-1003) write(7,1032)
	if(discprt.and.iscan.eq.-1003) write(8,1032)
1032	format(' Only ''dubious'' amplitudes set ''assumed''')
72	continue
	fc0=0.5*float(jfilt)	!kHz (-3dB)
	print 201,2.*fc0
201	format(' filter setting (kHz on box)= ',g13.6)
c Filter function for unfiltered signal has JFILT set to 0- use 10kHz
	if(jfilt.eq.0) goto 20
	goto 21
20	fc0=5.0
	print 22
	if(pon()) write(7,22)
	if(discprt) write(8,22)
22	format(
     &' Unfiltered signal- fc set to 10kHz (5kHz,-3dB) for risetime.')
c Used only to calc 2.5*rise time for excluding short events and below
c for res of threshold-crossing analysis
21	continue
c
	if(sexp) goto 303
	print 216,ADATE1
216	FORMAT(1X,3A4)
	if(iscan.ne.-4001) then
	  print 217,TITLE1,NINT1
217	  FORMAT(1X,a56,/,1X,I5,' transitions.  O.K [Y] ? ')
	else
	  print 2171,TITLE1,nlon,nloff
2171	  FORMAT(1x,a56,/,
     &  1x,i4,' ON latencies,',i4,' OFF latencies:  O.K [Y] ? ')
	endif
	read 101,ans
	if(UC(ans).EQ.'N') GOTO 207
303	if(sexp) then
	   if(pon()) write(7,70) nsc
	   if(discprt) write(8,70) nsc
	   if(sexp) print 70,nsc
70	   format(' Experiment',i5)
	else
	   if(pon()) write(7,71) nsc,adate1,title1
	   if(discprt) write(8,71) nsc,adate1,title1
71	   format(' Experiment',i5,2x,3a4,/,1x,a56)
	endif
C
	if(sexp.and.sres) goto 401	!end of loop
C
c	IF(.NOT.SCNDAT) GOTO 406
c INDEX set =1 outside the 'nfile' loop
	KRS=ISTREC		!START RECORD FROM DIRECTORY
c Open data on Winchester (unit=12)
	   call DOPEN(1,ndev)
	if(iscan.ne.-4001) then
	   jlast=index+nint1-1  !index in TINT of last obs in current expt
	   j1=index
	   nreci=1 + (nint1-1)/256		!no of records for TINT
	   print 141,nsc,nint1
141	   format( ' Experiment ',i5,',  ',i6,' intervals:')
	   do 40 n=1,nreci
	    j2=j1+255	!1-256
	    if(j2.gt.jlast) j2=jlast
	    READ(12,rec=KRS) (TINT(J),J=j1,j2)
	    krs=krs+1
	    print 140,j1,j2
140	    format(' Intervals',i5,' to ',i5,' read in')
	    j1=j2+1			!ready for next
40	   continue
c and now amplitudes
	   j1=index			!reset
	   nreca=1 + (nint1-1)/512		!no of records for IAMPL
	   do 41 n=1,nreca
	    j2=j1+511
	    if(j2.gt.jlast) j2=jlast
	    READ(12,rec=KRS) (IAMPL(J),J=j1,j2)
	    krs=krs+1
	    print 142,j1,j2
142	    format(' Amplitudes',i5,' to ',i5,' read in')
	    ijfst(i)=j1 		!record index in iampl of values from ith expt
	    ijlast(i)=j2
	    j1=j2+1			!read for next
41	   continue
c  update index
	   index=jlast+1		!ready for next expt
	else 		!i.e. first latency data (iscan=-4001)
	   print 1401,nsc,nlon,nloff
1401	   format( ' Experiment ',i5,
     &  1x,i4,': ON latencies,',i4,' OFF latencies')
c	  first read ON jump latencies
	   nintt1=nintt1+nlon
	   nreci=1 + (nlon-1)/256		!no of records for TINT1
	   jlast=index1+nlon-1  !index in TINT1(j,1) of last obs in current expt
	   j1=index1
	   do 43 n=1,nreci
	    j2=j1+255	!1-256 first time
	    if(j2.gt.jlast) j2=jlast
	    READ(12,rec=krs) (TINT1(j,1),j=j1,j2)		!on jump latencies
	    krs=krs+1
	    print 143,j1,j2
143	    format(' ON latencies',i5,' to ',i5,' read in')
	    j1=j2+1			!ready for next
43	   continue
	   index1=jlast+1		!ready for next expt
c	  Now read OFF jump latencies
	   nintt2=nintt2+nloff
	   nreci=1 + (nloff-1)/256		!no of records for TINT1
	   jlast=index2+nloff-1  !index in TINT1(j,2) of last obs in current expt
	   j1=index2
	   do 44 n=1,nreci
	    j2=j1+255	!1-256 first time
	    if(j2.gt.jlast) j2=jlast
	    READ(12,rec=krs) (TINT1(j,2),j=j1,j2)		!off jump latencies
	    krs=krs+1
	    print 144,j1,j2
144	    format(' OFF latencies',i5,' to ',i5,' read in')
	    j1=j2+1			!ready for next
44	   continue
	   index2=jlast+1		!ready for next expt
	endif		!end of tint/tint1 read-in
c
	CLOSE(unit=12)
c
c
c	GOTO 401
C NOW SECTION TO READ FILES THAT WERE PRODUCED BY SCFIT8 RATHER THAN
C SCAN1 (BEFORE EX#72 IN DC DISC 1) AND CONVERT DATA TO SCAN1 FORMAT
c Note that nrec=2 for >256 topens, =1 otherwise
c Nint1 already doubled so= nopen+ngap now
c406	KRS=ISTREC		!START RECORD FROM DIRECTORY
cc	N=1
c	jlast=index+nint1-1  !index in TINT of last obs in current expt
c	j1=index
c	print 141,nsc,nint1
cc Open data on Winchester (unit=12)
c	call DOPEN2(1,ndev)
c	DO 123 n=1,NREC		!read 512 at at time
c	j2=j1+511
c	if(j2.gt.jlast) j2=jlast
c	READ(12,rec=KRS) (TINT(J),J=j1,j2-1,2)	!TOPEN INTO TINT(1,3,5..)
c	krs=krs+1
c	READ(12,rec=KRS) (TINT(J),J=j1+1,j2,2)	!TGAP INTO TINT(2,4,6,..)
c	krs=krs+1
c	print 140,j1,j2
c	READ(12,rec=KRS) (AMPL(J),J=j1/2+1,j2/2)	!AMPL(1,2,...)
c	krs=krs+1
c	print 142,j1/2+1,j2/2
c	j1=j2+1
c123	CONTINUE
c	close(unit=12)
c	index=jlast+1		!ready for next expt
c
401	CONTINUE	!LOOP to define and read all expts
c
c
	if(sexp.and.sres) goto 411	!amplitudes have not been re-read
c
c Check whether ACAL was same for each expt pooled
c Find the smallest acal, and use this throughout rest of program; for any
c expt file that has a larger acal, multiply the iampl() values for this
c expt accordingly
c NB if ratio=4 then iampl could be +/-4*2048= +/-8192, still safely within
c the +/-9999 limits used to detect dubious openings
	acalmin=1.e37		!for smallest acal
	if(nfile.gt.1) then
	   do i=1,nfile
		if(acalib(i).lt.acalmin) acalmin=acalib(i)
	   enddo
	   do i=1,nfile
	      if(acalib(i).gt.acalmin) then
		   ratio=acalib(i)/acalmin		!multiply all iampl by ratio
		   if(ratio.gt.4.001) then
			call BELL(3)
			print 73
73			format(
     &		' Calibration factors differ by > 4: cannot correct')
			STOP
		   endif
		   do j=ijfst(i),ijlast(i)
			ia=int4(iampl(j))
			ioff=0
			if(ix.gt.9999) ioff=-10000
			if(ix.lt.-9999) ioff=10000
			ia=ia+ioff		!amplitude without offset
			a=float(ia)*ratio
			iampl(j)=int2(ifixr(a)-ioff)	!replace offset and fix
		   enddo
		   calcor=.true.		!calibration factor HAS been adjusted
	 	   nsc=kfile(i)
      	   print 74,nsc,acalib(i),acalmin
		   if(pon()) write(7,74) nsc,acalib(i),acalmin
		   if(discprt) write(8,74) nsc,acalib(i),acalmin
74		   format(' Expt ',i4,': calibration = ',g13.6,' pA/ADC;',/,
     &	    '   amplitudes adjusted for = ',g13.6,' pA/ADC')
		endif
	   enddo
	   acal=acalmin	!used as acal from now on
	endif
c end of calibration correction
c
411	continue
c If cal factor has been adjusted on previous call to DISKIN then reset
c it here (will have been overwritten by disc read above)
	if(calcor) acal=acalmin
	if(sexp) goto 410
	fc=fc0
	if(iscan.eq.-3001) goto 230	!simulations
	if(iscan.eq.-4001) goto 230	!first latency dist
4021	print 402
402	format(/,
     &' Risetime setting:',/,
     &' (1) Use nominal filter setting only',/,
     &' (2) Include effect of prefilter and tape on rise time too ',/,
     &' (3) Specify numerical value here',/,
     &' Option number [1] =  ')
	read 3,i1
	if(i1.le.1) goto 403	!default
	if(i1.eq.2) goto 4031
	if(i1.gt.3) goto 4021
	print 4032
4032	format('& Rise time (microseconds)= ')
	call INPUTr(trise)
	fc=332.1/trise	!corresponding fc (-3dB)
	goto 410
4031	print 405
405	format('& -3dB frequency for tape recorder (kHz)= ')
	call INPUTr(fc1)
	print 407
407	format(' -3dB frequency for patch amplifier (kHz)= ')
	call INPUTr(fc2)
	fc=1.0/sqrt(1./fc0**2 + 1./fc1**2 + 1./fc2**2)
403	if(iasc)then
		print 274
274		format(' -3dB frequency of filter (kHz)= ')
		call Inputr(fc)
	endif
	trise=332.1/fc		!rise time (microsec)
c	trise=332.1/(0.5*float(jfilt))		!rise time (microsec)
410	print 4011,fc,trise
	if(pon()) write(7,4011) fc,trise
	if(discprt) write(8,4011) fc,trise
4011	format(
     &' Filter: fc(kHz,-3 dB)= ',f8.2,':  risetime (microsec)= ',g13.6)
	if(iscan.eq.-1001.or.iscan.eq.-3001) goto 230
	if(iscan.eq.-2001) print 227,trise*1.5
227	format(' Best resolution for 50% threshold =',
     &' 1.5 rise-times = ',1pg13.6,'microseconds')
230	continue
	if(sexp.and.sres.and.scndat) goto 104	!return
c	if(sexp.and.sres.and.(.not.scndat)) goto 106
C
	if(iscan.ne.-4001) then
	  NINTt=JLAST	!new total no of intervals- all expts
	  print 146,nintt
	  if(pon()) write(7,146) nintt
	  if(discprt) write(8,146) nintt
146	  format(' Total number of intervals= ',i8)
	else
	  print 147,nintt1,nintt2
	  if(pon()) write(7,147) nintt1,nintt2
	  if(discprt) write(8,147) nintt1,nintt2
147	  format(
     & ' Total number of ON latencies = ',i7,': OFF latencies = ',i7)
	endif
c
	if(sexp) goto 404
	SUBLEV=.FALSE.
c no sublevels from SCANF (iscan=-2001) or first latencies (iscan=-4001)
	IF(.NOT.SCNDAT.or.iscan.eq.-2001.or.iscan.eq.-4001) GOTO 404
	print 102
102	FORMAT(' Sublevels to be analysed [Y] ? ')
	read 101,ans
	if(UC(ans).ne.'N') SUBLEV=.TRUE.
404	CONTINUE
c
c For old format data put amplitudes into SCAN1 format -NOT IN NOW
145	if(debug()) call DEBTYP(nintt,0,0,tint,iampl,acal,
     & 0,-1,-1,yval,pon())
c	subroutine DEBTYP(nval,n1,n2,tint,iampl,acal,jval,j1,j2,yval,prnt)
c To print values of intervals and amps for debugging in EKDIST
c Nval,jval used only to indicate no of values when i1,i2 asked for
c  here (n1=0 or j1=0), otherwise ignored.
c (1)If n1>0 in call then TINT,AMP values from n1 to n2 are printed
c without asking for values here (and without title)
c (2) If n1=0 in call then asks which elements of TINT, AMP to type
c (3) If n1<0 in call then do only Jval part
c (4) print of Yval() similarly controlled by Jval,j1,j2
c Section (done only if debug on) to use only part of the data
	if(.not.debug().or.iscan.eq.-4001) goto 233
107	print 232
232	format(' Use all of the intervals and amplitudes? ')
	read 101,ans
	if(UC(ans).eq.'Y') goto 233
	print 2181
2181	format(' Specify:',/,' (1) numbers to be omitted',/,
     & ' (2) numbers to be used',/,' Option number = ')
	read 3,iopt
	if(iopt.eq.1) goto 2182
	print 218
218	format(' Use only numbers n1 to n2: n1,n2= ')
	read 219,n1,n2
219	format(2i8)
	NINTt=n2-n1+1
	if(pon()) write(7,109) nintt,n1,n2
	if(discprt) write(8,109) nintt,n1,n2
109	format(' Used only',i5,' transitions ( nos ',i5,' to ',i5,')')
c shift arrays left by n1-1 places
	CALL VAROT(TINT,20480,-(n1-1))
	call VIROT2(IAMPL,20480,-(n1-1))	!integer*2 version
	goto 222
c now omission
2182	continue
	print 2183
2183	format(' Omit numbers n1 to n2: n1,n2= ')
	read 219,n1,n2
c arrays unchanged up to n1-1, then go straight on to n2+1 to nintt=end
c Set tint(n1-1)=-1000. to show break
	nintt=nintt-(n2-n1+1)		!new value
	do 2184 i=n1,nintt
	j=i-n1+n2+1		!n2+1,n2+2,...,new nintt
	tint(i)=tint(j)
	iampl(i)=iampl(j)
2184	continue
	tint(n1-1)=-1000.
	if(pon()) write(7,110) nintt,n1,n2
	if(discprt) write(8,110) nintt,n1,n2
110	format(' Used only',i5,' transitions (omitted ',i5,' to ',i5,')')
c
222	continue
	tint(nintt)=-1000.
	print 220,nintt
c220	format(' Now ',i6,' values: print some? ')
220	format(' Now ',i6,' values:')
	if(debug()) call DEBTYP(nintt,0,0,tint,iampl,acal,
     & 0,-1,-1,yval,pon())
c
233	continue
c	print 108
104	CONTINUE
c	first=.false.	!now set in main prog
	RETURN
c
99	continue
	idest=99
	RETURN
	END

