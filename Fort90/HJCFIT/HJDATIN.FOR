SUPERSEDED BY HJCDAT1, HJCDAT2 (THIS VERSION DATED 27/07/93)
======================================================================
	subroutine HJDATIN(TINT,NINTt,ACAL,IAVAMP,idisk,
     & SUBLEV,jfilt,rms,idest,nfile,kfile,trise,fc,iscan,idiskq,
     & treso,tresg,ndevs,ndev1,first,nset,concA,concB,readin)
c Version of DATIN for HJCFIT version that accepts several conc at once
c Modified 11/05/92 06:40pm to keep disc partition, as well as expt #, for
c every experiment -kept in array ndevs*2(20,10) (like kfile=expt #)
c Modified 06/16/92 12:34pm
c     (1) to read expts not all on same SC data disc
c 	(2) to read expts done at different agonist concentrations
c	  into tint(i,j), iampl(i,j) j=1,..10 =diff sets (e.g. diff conc)
c	  nintt(j)=array now, with number of data values for each set
c	And define concA(j) and concB(j) to contain concentrations
c	(xA,xB) for j=1,...,nset. [NOW done later, after NLIG read]. These
c	 arrays are used in HJCLIK to identify the conc at which each
c	 transition was done.
c	(3) Also need arrays to specify number of values at each conc=ninnt(j)
c Data input for HJCFIT
c NB set-up number=jsetup (prev=jfilt1) also kept in directory from Jan 1989
c     so should be made an arg here for use in main prog
c NB in future version make JFILT in Hz not kHz!
c
c VERSION NUMBERS (iscan)
c NB iscan=-1001,-1002,-1003,-2001 all indicate PDP-generated files that
c	have been kermitted to PC
c    iscan=-3001 (simulated) and -4001 (first latencies) will usually be
c	data generated on PC
C  ISCAN=-1001 shows data from SCAN (not from old PDP SCFIT prog which
c	cannot be used now)
c  Iscan=-1002 means that ISCAN is followed by valid values of JFILT
c    and RMS (used in new EKDIST to calc false event rate)
c  Iscan=-1003 is as -1002 but indicates that data analysed by version
c    in which meaning of 'assumed' amplitude changed to 'dubious'
c  Iscan=-2001 means data from SCANF
c  Iscan=-3001 means data from SCSIM
c  Iscan=-4001 means data = first latencies from CJFIT1
c
c
c 05/26/89 08:38pm Lahey version of DSKIN1 for EKDIST (DEBUG not a param)
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
c Reads upto 20 expts all at once into expanded TINT,IAMPL arrays
c  NINT1=no of intervals in each file read (local to this subroutine)
c  NINTT(j)=total no of intervals in all files before resolution imposed
c  NINT(j)=total no of intervals AFTER resolution imposed (see RESINT)
c
	integer*2 Nexpt,nxtrec,idisc,jdisc,jfirst,ix1,ix2,iexp1,iexp2
      integer*2 ISTREC,NREC,nint,NINT1,IAVAMP2,ISCAN2,jfilt2,jsetup2
	integer*2 IAMPL(20480,10)
	real TINT(20480,10)		!up to 20 *1024 for each conc
	integer kfile(20,10),nfile(10),nintt(10)
	real*4 concA(10),concB(10)		!to hold concentrations for each expt
	character title1*56,name*12
	character ans,UC
	character*2 ndev,ndev1,ndevs(20,10)
	character*4 adate(3),adate1(3)
c	character*11 cdate
	LOGICAL DEBUG,SUBLEV,SCNDAT,pon,slock,caplock,whole
	logical readin,samexp
c at present sexp always false for HJCFIT but set asmexp=true if the
c experiments (in kfile) and discs (in ndevs) are as read from .INI
	logical sbin,shist,sres,sexp
	logical discprt,first
	common/dp/discprt
	common/sblk/sbin,shist,sres,sexp
	COMMON/AMPBLK/iampl
c	data ipassw/2102/
C
	pon()=slock()
	debug()=caplock()
c
	if(.not.first) goto 301		!not first call to DATIN
c	idiskq=-1			!defined if needed in VHIST -now set in main
C
101	FORMAT(A1)
c105	FORMAT(2I8)
108	FORMAT(/)
c
c DEBUG replaced by calls to DEBTYP controlled by CAPLOCK
C
c Check which disc to use
307	continue
c	ndev='D:'		!default hard disc (now read from disc)
	ndev=ndevs(1,1)	!OK if all expts from same disk
	if(readin) goto 301	!no need to ask for disk now (in ndevs)
	print 3071
3071	format(' Specify disc for single-channel data file:')
	call DISCNUM1(id,ndev,0,1)	!ask for hard disc
	if(id.eq.-1) goto 99
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
     &	' (1) Use these experiments ',/,
     &	' (2) Copy expts from floppy to hard disc ',/,
     &	' (3) Try a different hard disc ',/,
     &	' Option number [1] =  ')
	i=1
	call INPUTi(i)
	if(i.eq.2) goto 311	!leave 14 open
	CLOSE(unit=14)
	if(i.eq.3) goto 307	!another hard disc
	goto 301		!if i=1 (or i=0, default)
c
c Now copy expts from floppy
311	continue
	ndev1='A:'
c	if(iterm().eq.1) call DISCN1(id,ndev1,-2,1)	!ask for floppy
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
c	read 105,iexp1,iexp2
	call INPUT2i(iexp1,iexp2)
	if(iexp1.le.0) goto 301
c
	whole=.false.
	write(14,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,iexp1,iexp2
	CLOSE(unit=14)
	CLOSE(unit=16)		!close before loop starts
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
	  READ(13,rec=KRS) (TINT(i,1),i=1,256)
	  if(nint.le.256) goto 320
	  krs=krs+1
	  READ(13,rec=KRS) (TINT(i,1),i=257,512)
	  if(nint.le.512) goto 320
	  krs=krs+1
	  READ(13,rec=KRS) (TINT(i,1),i=513,768)
	  if(nint.le.768) goto 320
	  krs=krs+1
	  READ(13,rec=KRS) (TINT(i,1),i=769,1024)
320	  krs=krs+1
	  READ(13,rec=KRS) (IAMPL(i,1),i=1,512)
	  if(nint.le.512) goto 321
	  krs=krs+1
	  READ(13,rec=KRS) (IAMPL(i,1),i=513,1024)
	else 		!i.e. first latency data (iscan=-4001)
	  READ(13,rec=KRS) (TINT(i,1),i=1,256)		!on jump latencies
	  if(nlon.gt.256) then
	     krs=krs+1
	     READ(13,rec=KRS) (TINT(i,1),i=257,512)       !on jump latencies
	  endif
	  krs=krs+1
	  READ(13,rec=KRS) (TINT(i,2),i=1,256)          !off jump latencies
	  if(nloff.gt.256) then
	     krs=krs+1
	     READ(13,rec=KRS) (TINT(i,2),i=257,512)
	  endif
	endif
c
321	continue
	KRS1=ISTREC		!START RECORD FROM DIRECTORY
	if(iscan.ne.-4001) then
	  write(12,rec=KRS1) (TINT(i,1),i=1,256)
	  if(nint.le.256) goto 322
	  krs1=krs1+1
	  write(12,rec=KRS1) (TINT(i,1),i=257,512)
	  if(nint.le.512) goto 322
	  krs1=krs1+1
	  write(12,rec=KRS1) (TINT(i,1),i=513,768)
	  if(nint.le.768) goto 322
	  krs1=krs1+1
	  write(12,rec=KRS1) (TINT(i,1),i=769,1024)
322	  krs1=krs1+1
	  write(12,rec=KRS1) (IAMPL(i,1),i=1,512)
	  if(nint.le.512) goto 323
	  krs1=krs1+1
	  write(12,rec=KRS1) (IAMPL(i,1),i=513,1024)
	else 		!i.e. first latency data (iscan=-4001)
	  write(12,rec=KRS1) (TINT(i,1),i=1,256)
	  if(nlon.gt.256) then
	     krs1=krs1+1
	     write(12,rec=KRS1) (TINT(i,1),i=257,512)       !on jump latencies
	  endif
	  krs1=krs1+1
	  write(12,rec=krs1) (TINT(i,2),i=1,256)          !off jump latencies
	  if(nloff.gt.256) then
	     krs1=krs1+1
	     write(12,rec=krs1) (TINT(i,2),i=257,512)
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
	if(sexp) goto 302	!same expt as last time
C RECORDS 2 ONWARDS OF DIRECTORY CONTAIN DATA FOR SC EXPT #ISC
c IN RECORD # ISC-jfirst+2
C
207	continue
	if(readin) then
	   print 55,nset
55	   format(' Last run used ',i3,' data sets')
	   do 551 j=1,nset
	   print 552,j,concA(j)*1.e6
552	   format(1x,i4,': Concentration (uM) = ',g13.6,
     &   ':  files used were',/)
	   do 553 i=1,nfile(j)
		print 554,ndevs(i,j),kfile(i,j)
554		format('&',a2,i3)
		if(i.lt.nfile(j)) print 555
555		format('&, ')
		if(mod(i,8).eq.7) print 108
553	   continue
	   print 108
551	   continue
	   samexp=.false.
	   call DCASK(
     &	'Use same experiments again','y',ans)
c     &	'Use same experiments AND concentrations again','y',ans)
	   if(ans.eq.'Y') then
		samexp=.true.
		goto 302	!read the expts
	   endif
c	   call DCASK('Change concentration(s) only','y',ans)
c	   if(ans.eq.'Y') then
c	      do 4131 j=1,nset
c	      print 416,j
cc416	      format(' Concentration (micromolar) for set ',i4,
cc     &   ': xA (,xB) = ')
c	      call INPUT2r(x1,x2)
c	      concA(j)=x1*1.e-6		!molar
c	      concB(j)=x2*1.e-6		!molar
c4131	      continue
c	      goto 302	!read the expts
c	   endif
	endif
c
c Specify the experiments to be read:
	nset=1
	print 412,nset
412	format(
     &' Number of different data sets (eg concentrations) [',i2,'] = ')
c	read 3,n1
c	if(n1.ge.1) nset=n1
	call INPUTi(nset)
	print 3071
c3071	format(' Specify disc for single-channel data file:')
	call DISCNUM1(id,ndev,0,1)	!ask for hard disc
	if(id.eq.-1) goto 99
c
	do 413 j=1,nset
c	   print 416,j			!read conc after nlig known
c416	   format(' Concentration (micromolar) for set ',i4,
c     &   ': xA (,xB) = ')
c	   call INPUT2r(x1,x2)
c	   concA(j)=x1*1.e-6		!molar
c	   concB(j)=x2*1.e-6		!molar
	   print 411,j
411	   format(' Set ',i3,
     &': Pool experiments n1 to n2 [ENTER to specify each]: n1(,n2) = ')
	   read 53,nsc1,nsc2
53	   format(2i8)
	   if(nsc1.gt.0) then
		if(j.gt.1) then	!get disc for set 2,3,...
	 	  ndev=ndevs(1,j)	!OK if all expts from same disk
		  print 3071
		  call DISCNUM1(id,ndev,0,1)	!ask for hard disc
		  if(id.eq.-1) goto 99
		endif
		if(nsc2.eq.0) nsc2=nsc1
		nfile(j)=nsc2-nsc1+1	!number of expts in jth set
		do 54 i=1,nfile(j)
		  jex=nsc1+i-1	!expt number
		  kfile(i,j)=jex
		  ndevs(i,j)=ndev		!set all same for now
54	      continue
	   else if(nsc1.eq.0) then
		print 52,nfile(j)
52		format(
     &	' Number of experiments to be pooled (<=20) [',i3,'] = ')
c		read 3,i
c		if(i.ge.1) nfile(j)=i
		call INPUTi(nfile(j))
		do 249 i=1,nfile(j)
		  nsc=kfile(i,j)
		  print 6,kfile(i,j)
6		  FORMAT('&Read experiment no [',i4,'] = ')
c		  read 3,jex
c		  if(jex.ge.1) nsc=jex
c3		  format(i8)
		  call INPUTi(nsc)
	        kfile(i,j)=nsc
		  print 60,ndev
60		  format('&  from disc ',a2,' O.K. [Y] ? ')
		  read 101,ans
		  if(UC(ans).eq.'N') then
			call DISCNUM1(id,ndev,-1,0)
			print 108
			if(id.eq.-1) goto 99
		  endif
		  ndevs(i,j)=ndev
		  samexp=.true.	!expts and discs both specified now
249	      continue
	   endif
413	continue	!end of loop for each set (conc)
c
c
302	continue
c
c Now read and pool the expts
	do 400 j=1,nset		!outer loop for each set
	ndev=ndevs(1,j)
      print 71,j
      if(pon()) write(7,71) j
      if(discprt) write(8,71) j
71	format(' Set ',i3)
c Check that new set is on same disc as previous -if not read jfirst etc
	if(samexp.and.j.gt.1) then	!check if disc same as last read
	   if(ndevs(1,j).ne.ndevs(nfile(j-1),j-1)) then
		ndev=ndevs(1,j)		!new disc
	      call DOPEN(0,ndev)	!unit=14
	      read(14,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,
     &		iexp1,iexp2
		if(whole) iexp1=jfirst
		if(whole) iexp2=nexpt
		CLOSE(unit=14)
	   endif
	endif
	index=1		!index in enlarged TINT etc
	index1=1		!index for on latencies
	index2=1		!index for off latencies
	do 401 k=1,nfile(j)     !loop for each 'expt' in one set
	 nsc=kfile(k,j)
	 if(samexp.and.k.gt.1) then	!check if disc same as last
	   if(ndevs(k,j).ne.ndevs(k-1,j)) then
		ndev=ndevs(k,j)		!new diss
	      call DOPEN(0,ndev)	!unit=14
	      read(14,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,
     &		iexp1,iexp2
		if(whole) iexp1=jfirst
		if(whole) iexp2=nexpt
		CLOSE(unit=14)
	   endif
	 endif
	 if(.not.samexp.and.nsc.lt.iexp1.or.nsc.gt.iexp2) then
	   call BELL(2)
	   print 309,nsc,ndev
309	   format(
     & ' Experiment ',i5,' not on data disc ',a2,' specify new disc'/)
	   idef=0		!no default
431	   call DISCNUM1(id,ndev,iask,idef)
	   if(id.eq.-1) goto 99
	   ndevs(k,j)=ndev	!keep new disc along with expt #
	   call DOPEN(0,ndev)	!unit=14
	   read(14,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,ix1,ix2
	   print 42,name,idisc,jfirst,nexpt
	   call DCASK(' Is this disc O.K.','y',ans)
	   if(ans.eq.'N') then
		CLOSE(unit=14)
		goto 431
	   endif
	   iexp1=ix1
	   iexp2=ix2
	   if(whole) iexp1=jfirst
	   if(whole) iexp2=nexpt
	   CLOSE(unit=14)
	endif
	NSC1=NSC-jfirst+2
c Open directory on hard disk (unit=14)
	call DOPEN(0,ndev)
	READ(14,rec=NSC1)ADATE1,TITLE1,istrec,nrec,nint1,acal,iavamp2,
     & iscan2,jfilt2,rms,jsetup2,nlon2,nloff2,tjlen,ttoff,toffset,
     & t1,t2
	iscan=int4(iscan2)
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
C	   call PASWRD(ipass)	!so invisible
c	   read 3,ipass
c	   if(ipass.ne.ipassw) goto 99
	else if(iscan.eq.-4001) then
	   call BELL(2)
	   print 1121
1121     format(' DATA ARE FIRST LATENCIES FROM CJFIT -NOT YET FIXED')
	   STOP
	endif
111	continue
	if(i.eq.1.and.iscan.ne.-3001) then
	  if(iscan.eq.-1001.or.iscan.eq.-1002) print 1031
	  if(pon().and.scndat.and.iscan.ne.-1003) write(7,1031)
	  if(discprt.and.scndat.and.iscan.ne.-1003) write(8,1031)
1031	format(' All ''last'' and ''average'' amplitudes set ''assumed''')
	  if(iscan.eq.-1003) print 1032
	  if(pon().and.iscan.eq.-1003) write(7,1032)
	  if(discprt.and.iscan.eq.-1003) write(8,1032)
1032	  format(' Only ''dubious'' amplitudes set ''assumed''')
	  fc0=0.5*float(jfilt)	!kHz (-3dB)
	  print 201,2.*fc0
201	  format(' filter setting (kHz on box)= ',g13.6)
c Filter function for unfiltered signal has JFILT set to 0- use 10kHz
	  if(jfilt.eq.0) goto 20
	  goto 21
20	  fc0=5.0
	  print 22
	  if(pon()) write(7,22)
	  if(discprt) write(8,22)
22	  format(
     &' Unfiltered signal- fc set to 10kHz (5kHz,-3dB) for risetime.')
c Used only to calc 2.5*rise time for excluding short events and below
c for res of threshold-crossing analysis
21	  continue
	endif
c
	if(sexp) goto 303
c	print 216,ADATE1
c216	FORMAT(1X,3A4)
	if(iscan.ne.-4001) then
	  print 217,nsc,title1,ndev,adate1,nint1
217	  format(' Expt',i4,2x,a56,/,' disc ',a2,1x,3a4,': ',
     &   i5,' transitions.  O.K. [Y] ? ')
	else
	  print 2171,nsc,TITLE1,nlon,nloff
2171	  FORMAT(' Expt ',i4,2x,a56,/,
     &  1x,i4,' ON latencies,',i4,' OFF latencies:  O.K [Y] ? ')
	endif
	read 101,ans
	if(UC(ans).EQ.'N') GOTO 207
303	continue
c	print 70,ndevs(k,j),nsc,adate1,title1
	if(sexp) print 70,ndevs(k,j),nsc,adate1,title1
	if(pon()) write(7,70) ndevs(k,j),nsc,adate1,title1
	if(discprt) write(8,70) ndevs(k,j),nsc,adate1,title1
70	format(' Experiment ',a2,i5,2x,3a4,/,1x,a56)
C
	if(sexp.and.sres) goto 401	!end of loop
C
c	IF(.NOT.SCNDAT) GOTO 406
c INDEX set =1 outside the 'nfile' loop
	KRS=ISTREC		!START RECORD FROM DIRECTORY
c Open data on Winchester (unit=12)
	   call DOPEN(1,ndev)
	if(iscan.ne.-4001) then
	   ilast=index+nint1-1  !index in TINT of last obs in current expt
	   i1=index
	   nreci=1 + (nint1-1)/256		!no of records for TINT
c	   print 141,nsc,nint1
c141	   format('&Experiment ',i5,',  ',i6,' intervals:')
	   do 40 n=1,nreci
	    i2=i1+255	!1-256
	    if(i2.gt.ilast) i2=ilast
	    READ(12,rec=KRS) (TINT(i,j),i=i1,i2)
	    krs=krs+1
c	    print 140,i1,i2
c140	    format(' Intervals',i5,' to ',i5,' read in')
	    i1=i2+1			!ready for next
40	   continue
c and now amplitudes
	   i1=index			!reset
	   nreca=1 + (nint1-1)/512		!no of records for IAMPL
	   do 41 n=1,nreca
	    i2=i1+511
	    if(i2.gt.ilast) i2=ilast
	    READ(12,rec=KRS) (IAMPL(i,j),i=i1,i2)
	    krs=krs+1
c	    print 142,i1,i2
c142	    format(' Amplitudes',i5,' to ',i5,' read in')
	    i1=i2+1			!read for next
41	   continue
c  update index
	   index=ilast+1		!ready for next expt
	else if(iscan.eq.-4001) then		!i.e. first latency data
	   print 1401,nsc,nlon,nloff
1401	   format( ' Experiment ',i5,
     &  1x,i4,': ON latencies,',i4,' OFF latencies')
c	  first read ON jump latencies
	   nintt1=nintt1+nlon
	   ilast=index1+nlon-1  !index in TINT(i,1) of last obs in current expt
	   i1=index1
	   do 43 n=1,nreci
	    i2=i1+255	!1-256 first time
	    if(i2.gt.ilast) i2=ilast
	    READ(12,rec=krs) (TINT(i,1),i=i1,i2)		!on jump latencies
	    krs=krs+1
	    print 143,i1,i2
143	    format(' ON latencies',i5,' to ',i5,' read in')
	    i1=i2+1			!ready for next
43	   continue
	   index1=ilast+1		!ready for next expt
c	  Now read OFF jump latencies
	   nintt2=nintt2+nloff
	   nreci=1 + (nloff-1)/256		!no of records for TINT1
	   ilast=index2+nloff-1  !index in TINT1(i,2) of last obs in current expt
	   i1=index2
	   do 44 n=1,nreci
	    i2=i1+255	!1-256 first time
	    if(i2.gt.ilast) i2=ilast
	    READ(12,rec=krs) (TINT(i,2),i=i1,i2)		!off jump latencies
	    krs=krs+1
	    print 144,i1,i2
144	    format(' OFF latencies',i5,' to ',i5,' read in')
	    i1=i2+1			!ready for next
44	   continue
	   index2=ilast+1		!ready for next expt
	endif		!end of tint/tint1 read-in
c
	CLOSE(unit=12)
c
401	CONTINUE	!LOOP to define and read all expts
	nintt(j)=ilast	!new total no of intervals- all expts in set j
400	continue	!end of loop for each set (eg conc)
c
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
c	read 3,i1
	i1=1
	call INPUTi(i1)
	if(i1.le.1) goto 403	!default
	if(i1.eq.2) goto 4031
	if(i1.gt.3) goto 4021
	print 4032
4032	format('& Rise time (microseconds)= ')
	call INPUTr(trise)
	fc=332.1/trise	!reset fc
	goto 410
4031	print 405
405	format('& -3dB frequency for tape recorder (kHz)= ')
	call INPUTr(fc1)
409	format(g13.6)
	print 407
407	format(' -3dB frequency for patch amplifier (kHz)= ')
	call INPUTr(fc2)
	fc=1.0/sqrt(1./fc0**2 + 1./fc1**2 + 1./fc2**2)
403	trise=332.1/fc		!rise time (microsec)
c	trise=332.1/(0.5*float(jfilt))		!rise time (microsec)
410	print 4011,fc,trise
	if(pon()) write(7,4011) fc,trise
	if(discprt) write(8,4011) fc,trise
4011	format(
     &' Filter: fc(kHz,-3 dB)= ',f8.2,':  risetime (microsec)= ',g13.6)
	if(iscan.eq.-1001.or.iscan.eq.-3001) goto 230
	if(iscan.eq.-2001) print 227
227	format(' Best resolution for 50% threhold = 1.5 rise-times')
230	continue
	if(sexp.and.sres.and.scndat) goto 104	!return
c	if(sexp.and.sres.and.(.not.scndat)) goto 106
C
	if(iscan.ne.-4001) then
	 do 1461 j=1,nset
	  print 146,j,nintt(j)
	  if(pon()) write(7,146) j,nintt(j)
	  if(discprt) write(8,146) j,nintt(j)
146	  format(' Set ',i3,': total number of intervals= ',i8)
	  if(debug()) then
		print 23
23		format(' Print intervals and amplitudes # n1,n2 [skip] = ')
c		read 105,n1,n2
		n1=0
		call INPUT2i(n1,n2)
		if(n1.gt.0) then
		  if(n2.lt.n1) n2=n1
		  do 25 i=n1,n2
		  print 26,i,tint(i,j),iampl(i,j)
		  if(discprt) write(8,26) i,tint(i,j),iampl(i,j)
26		  format(i6,g13.6,i6)
25		  continue
		endif
	  endif
1461	 continue
	else
	  print 147,nintt1,nintt2
	  if(pon()) write(7,147) nintt1,nintt2
	  if(discprt) write(8,147) nintt1,nintt2
147	  format(
     & ' Total number of ON latencies = ',i7,': OFF latencies = ',i7)
	endif
c
	if(sexp) goto 404
	SUBLEV=.false.
c HJCFIT cannot do sublevels (yet!)
cc no sublevels from SCANF (iscan=-2001) or first latencies (iscan=-4001)
c	IF(.NOT.SCNDAT.or.iscan.eq.-2001.or.iscan.eq.-4001) GOTO 404
c	print 102
c102	FORMAT(' Sublevels to be analysed [Y] ? ')
c	read 101,ans
c	if(UC(ans).ne.'N') SUBLEV=.TRUE.
404	continue
c
145	continue
c	if(debug()) call DEBTYP(nintt,0,0,tint,iampl,acal,
c     & 0,-1,-1,yval,pon())
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
c
	if(.not.debug().or.iscan.eq.-4001) goto 233
	do 107 j=1,nset
	print 232,j
232	format(' Set ',i3,': use all of the intervals and amplitudes? ')
	read 101,ans
	if(UC(ans).eq.'Y') goto 233
	print 2181
2181	format(' Specify:',/,' (1) numbers to be omitted',/,
     & ' (2) numbers to be used',/,' Option number = ')
c	read 3,iopt
	call INPUTi(iopt)
	if(iopt.eq.1) goto 2182
	print 218
218	format(' Use only numbers n1 to n2: n1,n2= ')
c	read 219,n1,n2
	call INPUT2i(n1,n2)
c219	format(2i8)
	nintt(j)=n2-n1+1
	if(pon()) write(7,109) nintt(j),n1,n2
	if(discprt) write(8,109) nintt(j),n1,n2
109	format(' Used only',i5,' transitions ( nos ',i5,' to ',i5,')')
c shift arrays left by n1-1 places (NB usual VAROT works OK when TINT is
c 2-dimensional in call -see TROT.FOR)
	call VAROT(TINT(1,j),20480,-(n1-1))
	call VIROT2(IAMPL(1,j),20480,-(n1-1))	!integer*2 version
	goto 222
c now omission
2182	continue
	print 2183
2183	format(' Omit numbers n1 to n2: n1,n2= ')
c	read 219,n1,n2
	call INPUT2i(n1,n2)
c arrays unchanged up to n1-1, then go straight on to n2+1 to nintt=end
c Set tint(n1-1)=-1000. to show break
	nintt(j)=nintt(j)-(n2-n1+1)		!new value
	do 2184 i=n1,nintt(j)
	i1=i-n1+n2+1		!n2+1,n2+2,...,new nintt
	tint(i,j)=tint(i1,j)
	iampl(i,j)=iampl(i1,j)
2184	continue
	tint(n1-1,j)=-1000.
	if(pon()) write(7,110) j,nintt(j),n1,n2
	if(discprt) write(8,110) j,nintt(j),n1,n2
110	format(' Set ',i3,
     &' Used only',i5,' transitions (omitted ',i5,' to ',i5,')')
c
222	continue
	tint(nintt(j),j)=-1000.
	print 220,nintt(j)
c220	format(' Now ',i6,' values: print some? ')
220	format(' Now ',i6,' values:')
c	if(debug()) call DEBTYP(nintt,0,0,tint,iampl,acal,
c     & 0,-1,-1,yval,pon())
107	continue	!loop for sets
c
233	continue
c	print 108
104	CONTINUE
	first=.false.
	RETURN
c
99	continue
	idest=99
	RETURN
	END

