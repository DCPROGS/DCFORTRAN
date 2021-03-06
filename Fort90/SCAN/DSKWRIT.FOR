	subroutine DSKWRIT(tint,iampl,iprops,nint,ntmax,ffilt,rms,avamp,
     & calfac2,iscanver,
     & ifreq1,ifreq2,ifreq3,imin,imax,ifmin,ifmax,
     & cjump,nsweep,njdim,jumps,kjumps,tzerod,tsample,
     & adcfil,expdate,defname,tapeID,ipatch,npatch,Emem,temper)
c To write permanent data files in PC version of SCAN.
c NB nint=number of defined intervals=kt-1 (last duration not defined)
c
c Modif 11/11/96 07:07pm to correct error in calculation of nextrec
c that caused last 16 bytes of every expt to be overwritten by the
c header of the next experiment! -need extra 24 bytes for ifmin(1:3),ifmax(1:3)
c and this was omitted previously!!  Also now start the writing of each
c new expt (apart from first) not at the value of nextrec read from disk
c but 24 bytes later -this leaves now 24+8=32 byte gap between expts (and
c doing it this way means that first expt to be stored with this modif program,
c which will read a bad value of nextrec from disk, will not overwite the
c end of the previous expt)
c
c Modif 03/03/94 09:37am to write point amplitude frequencies (non-zero
c  elements only are kept). Space taken =4*(n1+n2+n3) bytes, where
c  n1=ifmax(1)-ifmin(1)+1 etc.
c Now have entirely new data structure
c  Use only one file, SCANDAT.DAT, (rather than separate data and directory
c files).  At start of disc keep array
c	(1) name=owner's name
c	(2) idisc=disc # for this owner
c	(3) nfirst=number of 1st expt on this disk
c	(4) nfiles=number of files ('expts') already on ALL disks
c	(5) jstrec(i)=index of byte # where data file ('expt') #i starts
c	(6) nextrec=next vacant record (byte) # for data (16 now added
c		to this before using it -see modif notes)
c    (need integer*4 to cope with # of bytes on floppy)
c
c Duplicate of all this kept at end of the floppy
c Formatted floppy has 1457664 bytes. Records above add up to 824 bytes,
c but leave say 1000 bytes free by starting to write duplicate records
c at byte # irdup=1456664.  Thus area on floppy available for data
c is from say byte # 1000 to 1456663
c
c
c 	jstrec(i) points to start of data, which consists of
c	(a) header with directory data -fixed length= ### bytes.
c	(b) data arrays tint,iampl,iprops (option to also keep timsav etc??)
c Header:
c expnum,ifile,kt-1 (=nint), iscanver,expdate,title,tapeID,ipatch,Emem,
c    9     4      4          4        11     70     24      4     4
c temp,ffilt,avamp,rms,calfac2,iscanver,ioffset,imin,imax
c   4    4     4    4    4         4        4     4    4
c =170 bytes
c -ADD, FOR CJUMPS,
c cjump,nsweep,tzero,tsamp,toffset
c    4    4      4     4     4
c =190 bytes
c and add (jumps(i),i=1,nsweep),(jumps(i),i=1,nsweep) after the
c other data =4*nsweep bytes
c
c  Header always starts with '***File nnnn' so if jstrec is lost can
c  easily search for start of each file.  Header also contains info on
c  length of following data arrays.
c
c  Options for writing:
c Now simpler -abandon concatenation.  Only option is to overwrite the last
c expt (since all are now different lengths it will not usually be possible
c to overwrite any but the last).  Thus best to write data to permanent disk
c only after analysis completed.  If restarts done after writing it, then simply
c overwrite last expt.
c Data=
c real*4 tint(1...nint) 	   4nint bytes
c integer*2 iampl(1..nint)   2nint bytes
c integer*1 iprops(1..nint)  nint  bytes
c Total storage needed=7*nint bytes
c +header=ioffset=256 bytes
c
	integer*4 jstrec(200),jstrec1(200),nfiles,ifile
	real*4 tint(ntmax)
	integer*2 iampl(0:ntmax)
	integer*1 iprops(ntmax)	!properties of ith interval (see above)
	integer*4 DSKSPC			!utility library
      character*32 filnam,datfile
	character*40 mtitle,prtport*4		!machine names
	character ndev*2,name*12,name1*12,ans*1,UC*1,expdate*11,title*70
	character tapeID*24,expnum*9,ch*1
	character*14 ptype(5)
	character expdate1*11,title1*70,tape1*24
	logical pon,slock,debug,caplock,same,overwrit,present
	logical discprt
c for point amp histos
	integer*4 ifreq1(imin:imax),ifreq2(imin:imax),ifreq3(imin:imax)
	integer*4 ifmin(4),ifmax(4)
c For cjumps
	real*8 tzerod,tsample
	integer*2 jumps(njdim)
	integer*4 kjumps(njdim)
	logical cjump,setpatch
	integer*4 ipatch
c
c	common/miscdat/ipatch,Emem	!for DSKWRIT/READJUMP/AUTWRIT/READSCAN
	common/dp/discprt
	common/dpp/filnam,prtport,ndisc,jcol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c
c Find out where data file is (default=A:\scandat.dat) (options for other
c disc/other path/other name). (NB if disk contains SCDIRm.DAT this shows
c that it is an old-style single channel disk)
c
	pon()=slock()
	debug()=caplock()
c
c NB CJUMP/CJFIT/READ14/GETPATCH/READJUMP all use IPATCH=INTEGER*2
C BUT SCAN/EKDIST USE IPATCH=INTEGER*4 -CONVERTED IN READJUMP
c=	ipatch=int4(jpatch)
c
	irfst=1000		!earliest byte # for data
	irlast=1456660	!last byte # for data
	irdup=1456664	!byte # to start writing duplicate record
	ioffset=256		!from start of header to start of data
c
	ptype(1)='outside-out'
	ptype(2)='inside-out'
	ptype(3)='cell-attached'
	ptype(4)='whole-cell'
	ptype(5)='simulated data'
c Calc number of bytes needed for point amplitude data
	nbfreq=0
	do i=1,3
	   nbfreq=nbfreq+(ifmax(i)-ifmin(i)+1)
	enddo
	nbfreq=4*nbfreq	!for bytes
	if(cjump) then
	   tzero=sngl(tzerod)
	   tsamp=sngl(tsample)
	endif
c
c Find largest and smallest non-zero entries in ifreq()
	call GETIFMAX(imin,imax,ifreq1,ifreq2,ifreq3,ifmin,ifmax)
c
	ndev='A:'		!default
61	iask=-1
	idef=1
c Print help each time!
	print 1
1	format(
     & ' Writing data to permanent data disk (SCANDAT.DAT):',/,
     & '  (1) It is simplest to write data once, when analysis is',/,
     & '    completed: then write it to ''next file #''.',/,
     & '  (2) If data is written to a file, and subsequently SCAN',/,
     & '    is restarted and more transitions measured, then',/,
     & '    use the ''overwrite'' option to re-write the new data,',/,
     & '    overwriting the earlier, shorter, version.',/,
     & '  (3) You can write permanent data file at any time while ',/,
     & '    temporary (restart) file (SCANTEMP.DAT) is still on disk',/,
     & '    by restarting SCAN, and choosing ''Go straight to write',/,
     & '    permanent data file'' option',/)
c
	call DCASK(' Write results to SCANDAT.DAT now','y',ans)
	if(ans.eq.'N') goto 999
c
	call DISCNUM1(id,ndev,iask,idef)	!which disc?
	if(id.eq.-1) goto 999
	datfile=ndev//'\SCANDAT.DAT'
	INQUIRE(file=datfile,exist=present,flen=len)
	if(present.and.len.ne.1457491) then
	   call BELL(3)
	   print 33,len
33	   format(
     &   ' SCANDAT file present but length = ',i10,' bytes',/,
     &   '  It should be 1457491 bytes. CHECK IT (no data written)',/)
		RETURN	!without writing data
	endif
	if(.not.present.or.len.eq.0) then
	   call BELL(2)
	   n=NBLANK(datfile)
	   if(n.le.14) then
	      print 211,datfile
211	      format(
     &	' The file ',a14,' not present: create new file [Y] ? ')
	   else
	      print 2111,datfile
2111	      format(
     &	' The file ',a32,' not present: create new file [Y] ? ')
	   endif
	   read 101,ans
101	   format(a1)
	   if(UC(ans).eq.'N') then
		print 19
c19		format('NO DATA WRITTEN!')
		RETURN	!without writing data
	   else	!create new file
		ch=datfile(1:1)		!disk letter
		if(UC(ch).eq.'A') then
		   print 31
31		   format(
     &     		' NEED A BLANK FLOPPY DISK WITH NO BAD BLOCKS',
     &     		' (BEST TO USE NORTON SAFE FORMAT)')
		   print 32
32		   format(
     &	    ' Mount the disk and hit any key AFTER it is ready',/)
		   call ANYKEY
		endif
		ispace=DSKSPC(ch)		!utility library
		if(ispace.lt.1457491) then
		   call BELL(3)
		   print 30,ch,ispace
30		   format(/,' NOT ENOUGH SPACE ON ',A1,': FOR SCANDAT',/,
     &	    '(',i10,' bytes: require 1457491 bytes)',/,/)
		   goto 61		!try again
		endif
		call TITENT0('Enter owners name: ',NAME,12,.true.)
		idisc=1
		print 2
2		format(' data disc number [1] = ')
		call INPUTi(idisc)
		nfirst=1
		if(idisc.ne.1) then
5		   print 3
3		   format(
     &	 ' Total number of experiment files before this one [0] = ')
		   nfiles=0			!total number of files
		   call INPUTi(nfiles)
		   nfirst=nfiles+1
		   ifile=nfiles		!number of file for current data
		   print 4,ifile+1,name,idisc
4		   format(
     &	  ' Data will be written to experiment file number = ',i6,/,
     &	   ' which will be the first file on ',a12,'''s disk ',i3,/,
     &	    ' O.K. [Y] ? ')
		   read 101,ans
		   if(UC(ans).eq.'N') goto 5
		endif
		do i=1,200
		   jstrec(i)=0
		enddo
		nextrec=irfst     !start at byte # 1000
		OPEN(unit=15,file=datfile,status='NEW',access='TRANSPARENT')
		write(15,rec=1) name,idisc,nfirst,nfiles,jstrec,nextrec
c	 and write duplicate
		write(15,rec=irdup) name,idisc,nfirst,nfiles,jstrec,nextrec
	   endif
	else
	   OPEN(unit=15,file=datfile,status='OLD',access='TRANSPARENT')
	   read(15,rec=1) name,idisc,nfirst,nfiles,jstrec,nextrec
	   read(15,rec=irdup)name1,idisc1,nfirst1,nfiles1,jstrec1,nextrec1
c bytes=12+4+4+4+800+4=828
	   nfilesav=nfiles		!keep value from disk
	   call CHECKDUP(name,idisc,nfirst,nfiles,jstrec,nextrec,
     &	name1,idisc1,nfirst1,nfiles1,jstrec1,nextrec1,same)
	   print 6,name,idisc,nfirst,nfiles,nextrec
	   if(pon()) write(7,6) name,idisc,nfirst,nfiles,nextrec
	   if(discprt) write(8,6) name,idisc,nfirst,nfiles,nextrec
6	   format(1x,a12,' Floppy disc number ',i2,/,
     &    '  -contains experiments ',i4,' to ',i4,/,
     &    '  (next vacant record for data = ',i8,')',/,
     &    ' O.K. [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') goto 61
	   if(.not.same) then
		call BELL(3)
		print 8
8		format(
     &	'DUPLICATE FILES ON DATA DISK DO NOT AGREE -NO DATA',/,
     &	'WRITTEN NOW -CHECK DISK AND THEN WRITE IT USING THE',/,
     &	'''WRITE PERMANENT DATA ONLY'' OPTION')
		RETURN	!without writing data
	   endif
	endif
c
	nextfile=nfiles+1
	overwrit=.false.
	if(nfiles.eq.0) then		!only for first expt on disk #1
	   overwrit=.false.
	   print 10,nextfile
10	   format(
     &   ' Write data to next experiment file, #',i4,': O.K. [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
		print 19
c19		format('NO DATA WRITTEN!')
		CLOSE(unit=15)
		RETURN	!without writing data
	   else
		ifile=nextfile		!new expt #
		nfiles=ifile
		irec=nextrec		!where to start writing header
		jstrec(ifile-nfirst+1)=irec	!=jstrec(1) for nfile=nfirst
		irec1=irec+ioffset      !where to start writing data
c      NB +24 bytes for ifmin(1:3), ifmax(1:3)
		nextrec=irec1+7*nint+24+nbfreq+8	!8 extra bytes, just in case!
		if(cjump) nextrec=nextrec + 6*nsweep	!bytes for jump(),kjump()
	   endif
	else		!nfiles>0 -anytime except start of disk 1
12	   print 11,nextfile,nfiles
11	   format(
     &   ' (1) Write data to next experiment file, #',i4,/,
     &   ' (2) Overwrite the last experiment file, #',i4,/,
     &   ' (3) Cancel the disk write',/,
     &   ' Option number [1] = ')
	   overwrit=.false.
	   iopt=1
	   call INPUTi(iopt)
	   if(iopt.lt.1.or.iopt.gt.3) goto 12
	   if(iopt.eq.1) then
		ifile=nextfile		!new expt #
		nfiles=nextfile		!new total number
c		irec=nextrec		!where to start writing header
		irec=nextrec+24		! + 24 bytes extra -see modif notes at top
		jstrec(ifile-nfirst+1)=irec	!=jstrec(1) for ifile=nfirst
		irec1=irec+ioffset      !where to start writing data
c      NB +24 bytes for ifmin(1:3), ifmax(1:3)
		nextrec=irec1+7*nint+24+nbfreq+8	!8 extra bytes, just in case!
		if(cjump) nextrec=nextrec + 6*nsweep	!bytes for jump(),kjump()
	   else if(iopt.eq.2) then	!don't change nfiles
		ifile=nfiles
	      overwrit=.true.
		irec=jstrec(ifile-nfirst+1)	!where to start reading header
	   read(15,rec=irec)expnum,nfiles1,nintold,iscan1,expdate1,title1,
     & 	tape1,ipatch1,Emem1,temp1,ffilt1,avamp1,rms1,calfac21,ioff1
		print 16,nfiles1,nintold,expdate1,title1
16		format(
     &	' This will overwrite data in file number ',i4,' with ',i6,
     &	' transitions:',/,'   Date of expt = ',a11,':  title:',/,
     &	6x,a70,/,
     &	' O.K. [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(UC(ans).eq.'N') goto 12
c          OK to overwrite: ifile and irec already defined; NB nextrec may
c		be changed if number of intervals written is not same as before
		irec1=irec+ioffset      !where to start writing data -as before
c      NB +24 bytes for ifmin(1:3), ifmax(1:3)
		nextrec=irec1+7*nint+24+nbfreq+8		!8 extra bytes, just in case!
		if(cjump) nextrec=nextrec + 6*nsweep	!bytes for jump(),kjump()
	   else		!iopt=3
		print 19
c19		format(' NO DATA WRITTEN!')
		CLOSE(unit=15)
		RETURN	!without writing data
	   endif
	endif
c
c Check that data will fit on disk!
c NB if this is overwrite, and nint bigger than before, it may be
c that file 94 (or whatever) fitted on current disc before, but no
c longer fits, so ex 94 must now be deleted from current disc (and
c nfiles on this disk reduced to 93), before writing 94 to a new disk!
	if(nextrec.gt.irlast.or.nfiles-nfirst+1.gt.200) then
	   call BELL(3)
	   print 18
18	   format(
     &   ' NOT ENOUGH ROOM ON DISK FOR DATA!',/,
     &   ' Create a new data disk (do NOT put it in yet!) [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
		print 19
19		format(' NO DATA WRITTEN!')
		CLOSE(unit=15)
		RETURN	!without writing data
	   else	!make new disk
		if(overwrit) then
		   nfiles=nfilesav-1	!reduce number of expts on this disk
		   nextrec=irec		!as defined on 'overwrite' case above
	         print 20,idisc,nfilesav,nfilesav-1,nfilesav
      	   if(pon()) write(7,20) idisc,nfilesav,nfilesav-1,nfilesav
	         if(discprt) write(8,20)idisc,nfilesav,nfilesav-1,nfilesav
20		   format(
     & ' No longer room on disc ',i3,' for enlarged file # ',i5,/,
     & ' Number of files on this disc reduced to ',i5,', and file #',i5,
     & /,' will now be written to a new disk (do not put it in yet!).')
c		Fix old disk
		   write(15,rec=1) name,idisc,nfirst,nfiles,jstrec,nextrec
c       	 and write duplicate
		  write(15,rec=irdup)name,idisc,nfirst,nfiles,jstrec,nextrec
c        Now define values for new disk in overwrite case
		   nfiles=nfilesav
		else		!not overwrite
		   nfiles=nfilesav+1	!non-overwrite case
		endif
	      CLOSE(unit=15)
		nfirst=nfiles
		ifile=nfiles		!number of file for current data
		idisc=idisc+1
		do i=1,200
		   jstrec(i)=0
		enddo
		nextrec=irfst     !start at byte # 1000
		irec=nextrec		!where to start writing header
		jstrec(ifile-nfirst+1)=irec	!=jstrec(1) for ifile=nfirst
		irec1=irec+ioffset      !where to start writing data
c      NB +24 bytes for ifmin(1:3), ifmax(1:3)
		nextrec=irec1+7*nint+24+nbfreq+8    !8 extra bytes, just in case!
		if(cjump) nextrec=nextrec + 6*nsweep	!bytes for jump(),kjump()
c		print 21
c21		format(
c     &	' Now mount a blank formatted floppy disk',/,
c     &	' -hit any key AFTER the new disk is ready')
c		call ANYKEY()
		print 22,name,idisc,nfirst
22		format(
     &	' This will be ',a12,'''s disk #',i3,/,
     &	': first file on this disk will be #',i5,/,
     &	    ' O.K. [Y] ? ')
		read 101,ans
		if(UC(ans).eq.'N') then
		   print 19
c19		   format('NO DATA WRITTEN!')
		   RETURN	!without writing data
		else		!OK -make new file
		   ch=datfile(1:1)		!disk letter
		   if(UC(ch).eq.'A') then
			print 31		!need blank floppy
			print 32		!hit any key when ready
			call ANYKEY
		   endif
		   ispace=DSKSPC(ch)		!utility library
		   if(ispace.lt.1457491) then
			call BELL(3)
			print 30,ch,ispace	!not enough space
			if(UC(ch).eq.'A') print 31
			goto 61		!try again
		   endif
		   OPEN(unit=15,file=datfile,status='NEW',
     &		access='TRANSPARENT')
c            Put data on the new disk below, as for old disk
		endif
	   endif
	endif		!end of new disk section
c
c
c Get expt date, tape#, Em, patch type etc + title
c Header:
c expnum,ifile,kt-1 (=nint), iscanver,expdate,title,tapeID,ipatch,Emem,
c    9     4      4          4        11     70     24      4     4
c temp,ffilt,avamp,rms,calfac2,iscanver,ioffset,imin,imax
c   4    4     4    4    4         4        4     4    4
c =170 bytes
c Say
c ioffset=256 bytes (from start of header to start of data -leaves plenty
c of room for inserting other things in header retrospectively!)
	expnum='***File #'
	if(overwrit) then
	   print 23,nint,nintold
23	   format(
     & ' All details, including title, as for overwritten file (except',
     & /,i8,' transitions rather than ',i8,'): [N] ? ')
	   read 101,ans
	   if(UC(ans).eq.'Y') then
		expdate=expdate1
		title=title1
		tapeID=tape1
		ipatch=ipatch1
		Emem=Emem1
		temp=temp1
		ffilt=ffilt1
		avamp=avamp1
		rms=rms1
		calfac2=calfac21
c=		iscanver=iscan1	!new one written with current iscanver
		ioffset=ioff1
		goto 25
	   endif
	endif
c
	call TITENT0('Enter date of experiment: ',expdate,11,.false.)
	call TITENT0('Enter tape details: ',tapeID,24,.false.)
111	continue
	setpatch=.false.
	if(cjump) then
	   print 112,ptype(ipatch)
112	   format(' Patch type = ',a14,': O.K. [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') setpatch=.true.
	endif
	if(.not.cjump.or.setpatch) then
	   print 311,ipatch
311	   format(
     & ' Enter patch type:',/,
     & ' (1) outside-out',/,
     & ' (2) inside-out',/,
     & ' (3) cell-attached',/,
     & ' (4) whole-cell',/,
     & ' (5) simulated data',/,
     & '   Option number [',i2,'] = ')
	   call INPUTi(ipatch)
	   if(ipatch.lt.1.or.ipatch.gt.5) goto 111
	endif
c
	setpatch=.false.
	if(cjump) then
	   print 113,Emem
113	   format(' Holding potential = ',g13.6,': O.K. [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') setpatch=.true.
	endif
	if(.not.cjump.or.setpatch) then
	   print 13,Emem
13	   format(' Membrane potential (mV) [',f9.1,'] = ')
	   call INPUTr(Emem)
	endif
c
	print 121,temper
121	format(' Temperature [',f9.1,'] = ')
	call INPUTr(temp)
	call TITENT0('Enter title: ',title,70,.true.)
c
	if(cjump) then
	   toffset=0.
	   print 14
14	   format(/,
     &' Time offset (from piezo command to conc change) (ms) = ')
	   call INPUTr(toffset)
	endif

c
25	continue	!jump here if title etc same as for overwritten file
c Write the header
	CLOSE(unit=15)
	OPEN(unit=15,file=datfile,status='UNKNOWN',access='TRANSPARENT')
	write(15,rec=irec) expnum,ifile,nint,iscanver,expdate,title,
     & tapeID,ipatch,Emem,temp,ffilt,avamp,rms,calfac2,ioffset,
     & imin,imax,cjump,nsweep,tzero,tsamp,toffset
c Write the data
	if(.not.cjump) then
	  write(15,rec=irec1)
     &  (tint(i),i=1,nint),(iampl(i),i=1,nint),(iprops(i),i=1,nint),
     &  (ifmin(i),i=1,3),(ifmax(i),i=1,3),
     &  (ifreq1(i),i=ifmin(1),ifmax(1)),
     &  (ifreq2(i),i=ifmin(2),ifmax(2)),
     &  (ifreq3(i),i=ifmin(3),ifmax(3))
	else
	  write(15,rec=irec1)
     &  (tint(i),i=1,nint),(iampl(i),i=1,nint),(iprops(i),i=1,nint),
     &  (ifmin(i),i=1,3),(ifmax(i),i=1,3),
     &  (ifreq1(i),i=ifmin(1),ifmax(1)),
     &  (ifreq2(i),i=ifmin(2),ifmax(2)),
     &  (ifreq3(i),i=ifmin(3),ifmax(3)),
     &  (jumps(i),i=1,nsweep),(kjumps(i),i=1,nsweep)
	endif
c
c Finally update the jstrec etc records
	write(15,rec=1) name,idisc,nfirst,nfiles,jstrec,nextrec
c and write duplicate
	write(15,rec=irdup) name,idisc,nfirst,nfiles,jstrec,nextrec
	CLOSE(unit=15)
c
	print 27,name,idisc
27	format(
     &' Now mount a backup disk (',a12,' # ',i3,')',/,
     &	' -hit any key when ready')
	call ANYKEY()
c
	OPEN(unit=15,file=datfile,status='UNKNOWN',access='TRANSPARENT')
c Write the header
	write(15,rec=irec) expnum,ifile,nint,iscanver,expdate,title,
     & tapeID,ipatch,Emem,temp,ffilt,avamp,rms,calfac2,ioffset,
     & imin,imax,cjump,nsweep,tzero,tsamp,toffset
c Write the data
	if(.not.cjump) then
	  write(15,rec=irec1)
     &  (tint(i),i=1,nint),(iampl(i),i=1,nint),(iprops(i),i=1,nint),
     &  (ifmin(i),i=1,3),(ifmax(i),i=1,3),
     &  (ifreq1(i),i=ifmin(1),ifmax(1)),
     &  (ifreq2(i),i=ifmin(2),ifmax(2)),
     &  (ifreq3(i),i=ifmin(3),ifmax(3))
	else
	  write(15,rec=irec1)
     &  (tint(i),i=1,nint),(iampl(i),i=1,nint),(iprops(i),i=1,nint),
     &  (ifmin(i),i=1,3),(ifmax(i),i=1,3),
     &  (ifreq1(i),i=ifmin(1),ifmax(1)),
     &  (ifreq2(i),i=ifmin(2),ifmax(2)),
     &  (ifreq3(i),i=ifmin(3),ifmax(3)),
     &  (jumps(i),i=1,nsweep),(kjumps(i),i=1,nsweep)
	endif
c
c Finally update the jstrec etc records
	write(15,rec=1) name,idisc,nfirst,nfiles,jstrec,nextrec
c and write duplicate
	write(15,rec=irdup) name,idisc,nfirst,nfiles,jstrec,nextrec
	CLOSE(unit=15)
c
c Print out what has been done
	amp=avamp*calfac2
c
      print 26,name,idisc,ifile,jstrec(ifile-nfirst+1),nextrec-8,
     & nfirst,nfiles,nextrec,nint,expdate,title,tapeID,ptype(ipatch),
     & Emem,temp,ffilt,amp,rms,calfac2,ioffset
      if(pon()) write(7,26) name,idisc,ifile,jstrec(ifile-nfirst+1),
     & nextrec-8,
     & nfirst,nfiles,nextrec,nint,expdate,title,tapeID,ptype(ipatch),
     & Emem,temp,ffilt,amp,rms,calfac2,ioffset
      if(discprt) write(8,26) name,idisc,ifile,jstrec(ifile-nfirst+1),
     & nextrec-8,
     & nfirst,nfiles,nextrec,nint,expdate,title,tapeID,ptype(ipatch),
     & Emem,temp,ffilt,amp,rms,calfac2,ioffset

26	format(
     & ' Data written to ',a12,' data disk #',i3,' as file #',i5,/,
     & ' (data starts at byte #',i8,' and ends at byte #',i8,')',/,
     & ' This disk now contains files ',i5,' to ',i5,/,
     &  ' and byte # for the next write is ',i8,/,
     & ' Number of transitions = ',i8,'; experiment date ',a11,/,
     & ' Title: ',a70,/,
     & ' Tape details: ',a24,/,
     & ' Patch type: ',a14,/,
     & ' Membrane potential (mV) = ',f9.2,/,
     & ' Temperature = ',f9.1,/,
     & ' Filter (kHz, -3 dB) = ',f12.5,/,
     & ' Average full amplitude (pA) (approx) = ',f9.2,/,
     & ' RMS noise (pA) = ',f10.4,/,
     & ' Calibration factor (amplitude to pA)',g13.6,/,
     & ' Offset (bytes) from start of header to start of data = ',i8)
c
999	continue
	RETURN
	end

