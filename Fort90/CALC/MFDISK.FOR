	subroutine MFDISK(nset,ilabel,titlef,titled,titlex,titley,
     & ifile,nfile,prt,readini,iesc,readat,ndisc,infil,ifile1,
     & idtype,xA0,xA,xA1,tpulse,nstau,Yinf,ncomp,Vmem,tau,area,
     & npt,fopen,iver)
c To do disk read/write for MECHFIT (MFDAT.DAT) (-based on CVDISK as used
c for CVFIT)
c  (1) Data file read/written in transparent mode (as in iver=1003 for CVFIT)
c  At start of disc keep iver,nfile,jstrec(100),lstrec(100), ioff=812 bytes.
c   Start data at, say, byte 1025 (ioff=1024) (as when 1024 byte records used)
c
c Input:
c IFILE=>1 to read values for file #ifile (which is asked for here so set
c	any value eg ifile=1 in call (and also lists all titles if req)
c IFILE=0 to write values to file number decided here (and returned in IFILE)
c IFILE<0 to write values to file number -IFILE (sign restored before return)
c FOPEN=true if data file already open
c
	integer jstrec(100),lstrec(100)
	character*60 titlef	!file name
	character*60 titled(20)
	integer*4 idtype(20)
	integer npt(20)
	real*4 xA0(20),xA(20),xA1(20),tpulse(20),Yinf(20)
	integer ncomp(20),nstau(20)
	real*4 Vmem(20)   !for (V-Vrev)
	real*4 tau(10,20),area(10,20)		!nc=component #; np=idtype
	character*40 titlex,titley
 	character*1 ans,UC
	character ndev*2,filnam*12,infil*33
	logical slock,pon,debug,caplock,filspec,readat
	logical discprt,present,prt,readini,fopen
	common/dp/discprt
c
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
c Check which disc to use and open file
	iesc=0
	filspec=ifile.lt.0
	if(IFILE.le.0) goto 2	!write the data
	if(FOPEN) goto 5
c
c	if(ndisc.eq.1) goto 70
c	call DCASK(
c     & 'List the data files (MFDAT.DAT) on each disk partition','n',ans)
c	if(ans.eq.'Y') then
c	   print 6
c6	   format(' Disk partition for MFDAT data file directory:')
c	   call DISCNUM1(id,ndev,-1,0)	!ask for winchester or floppy
c	   if(id.eq.-1) then
c		iesc=99
c		RETURN			!abort if ESC hit
c	   endif
c	   INFIL=ndev//'\MFDAT\*.*'
c72	   call TITENT0(
c     &   'List all files in:',infil,33,.false.)
c	   call FINDFIL(infil,33,ierr)
c	   if(ierr.gt.0) then
c		readat=.true.
c		goto 71	!infil defined in FINDFIL
c	   else	!if ierr=0 (no files) or ierr=-1 (path not found)
c		call DCASK(
c     &     'Try another directory','n',ans)
c		if(ans.eq.'Y') goto 72
c	   endif
c	endif
c
70	continue
	call DCASK('Read data from disk','Y',ans)
	readat=ans.eq.'Y'
c	if(.not.readat) RETURN		!type in the data (read .INI first)
c
c Read initialisation file, cvfit.ini
c ===now moved to main prog, so can keep default array size!
	if(.not.readat) RETURN		!type in the data
	if(.not.readini) then    !.INI not present and so not read in main prog
	   if(ndisc.gt.1) then
		filnam='\MFDAT.DAT'
	      call DISCNUM1(id,ndev,-1,0)	!ask for winchester or floppy
	      if(id.eq.-1) then
		   iesc=99
		   RETURN			!abort if ESC hit
	      endif
	      INFIL=ndev//'\mfdat\'//FILNAM
	   else
		infil='\mfdat\MFDAT.DAT'		!default for ndisc=1
	   endif
	endif
c
31	continue
	call TITENT0(
     &'Name and path of data file:',infil,33,.false.)
c
71	continue	!jump here if INFIL already chosen in FINDFIL
	INQUIRE(file=infil,exist=present,flen=len)
	if(.not.present.or.len.eq.0) then
	   call BELL(3)
	   print 24,infil
24	   format(1x,a14,' does not exist')
	   present=.false.		!so asks for ndev
	   goto 31
	else
         OPEN(unit=12,file=INFIL,status='UNKNOWN',access='TRANSPARENT',
     &	err=30)
	   fopen=.true.
	endif
5	continue	!jump here if already open
c
c Section to read data  for file #ifile
1	continue
	read(12,rec=1) nfile,jstrec,lstrec,iver
c
	print 335,nfile
335	format(1x,i5,' files: list file titles [N] ? ')
	read 101,ans
	if(UC(ans).ne.'Y') goto 338
	do if=1,nfile
	   irec=jstrec(if)
	   read(12,rec=irec) kver,nset,titlef,titled
         print 5201,if,titlef,nset
         if(prt) write(7,5201) if,titlef,nset
         if(discprt) write(8,5201) if,titlef,nset
5201	   format(' File no ',i3,': ',a60,/,' contains ',i4,' data sets')
	   do j=1,nset
	      print 10,j,titled(j)(1:60)
      	if(prt) write(7,10) j,titled(j)(1:60)
	      if(discprt) write(8,10) j,titled(j)(1:60)
10		format('  data set #',i3,2x,a60)
	   enddo
	enddo
	print 138
138	format(
     & ' (1) Use this disk file',/,
     & ' (2) Try another disk',/,
     & ' Option number [1] = ')
	i=1
	call INPUTi(i)
	if(i.eq.2) then
	   close(unit=12)
	   fopen=.false.
	   present=.false.		!so asks for ndev
	   goto 31
	endif
338	continue
c Print file name/path
      if(pon()) write(7,20) infil
      if(discprt) write(8,20) infil
20	format(' Data from: ',a33)
c
	if(nfile.eq.1) then
	   ifile=1
	else if(readini) then
	   print 141,nfile,ifile1
141	   FORMAT(' Read file number (1 -',i3,') [',i3,'] = ')
	   ifile=ifile1
	   call INPUTi(ifile)
	else
	   print 140,nfile
140	   FORMAT(' Read file number (1 -',i3,') = ')
	   call INPUTi(ifile)
	endif
	ifile1=ifile
	if(ifile.lt.1.or.ifile.gt.nfile) goto 338
	if(prt) write(7,1401)ifile
      if(discprt) write(8,1401)ifile
1401	format(' Data from disk file ',i5)
c	nrect=lstrec(ifile)-jstrec(ifile)+1		!total number of records=bytes
	irec=jstrec(ifile)
	read(12,rec=irec) kver,nset,titlef,titled,
     & titlex,titley,ilabel,idtype,xA0,xA,xA1,tpulse,nstau,
     & Yinf,ncomp,Vmem,tau,area,npt
	RETURN
c
c
c Section to WRITE DATA into file number to be specified here (or as
c input if ifile<0
2	continue
c get disc to write to
	if(fopen) then
	   print 22,infil
22	   format(' Write data to ',a33,': O.K. [Y] = ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
		CLOSE(unit=12)
		fopen=.false.
		goto 23	!get new file name
	   endif
	   goto 501		!already open
	endif
c
	if(ndisc.gt.1) print 35
35	format(' Specify disk for MFDAT data file (MFDAT.DAT):')
	call DISCNUM1(id,ndev,-1,1)	!ask for hard or floppy disk
	if(id.eq.-1) then
	   iesc=99
	   RETURN			!abort if ESC hit
	endif
	infil=ndev//'\MFDAT.DAT'
23	continue
	call TITENT0(
     &'Name and path for output MFDAT data file:',infil,33,.false.)
	ifile=iabs(ifile)		!restore sign if nec
c
	INQUIRE(file=infil,exist=present,flen=len)
	if(.not.present.or.len.eq.0) then
	   call BELL(1)
	   print 241,infil
241	   format(1x,a14,' does not exist: create it [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
		present=.false.		!so asks for ndev
		goto 2    !get new disc
	   endif
c    Create new data file if not present
	   do i=1,100
		jstrec(i)=0
		lstrec(i)=0
	   enddo
	   nfile=0
	   len=0
	   readat=.false.
         OPEN(unit=12,file=INFIL,status='UNKNOWN',access='TRANSPARENT',
     &	err=30)
	   write(12,rec=1) nfile,jstrec,lstrec,iver
	   fopen=.true.
	else		!write data to an existing file
         OPEN(unit=12,file=INFIL,status='UNKNOWN',access='TRANSPARENT',
     &	err=30)
	   read(12,rec=1) nfile,jstrec,lstrec,iver1
	   if(iver1.ne.1) then
		call BELL(3)
		print 11,iver1,iver,iver
11		format(
     &' This is version ',i2,' data file, and this program is version'
     &  ,i2,/,
     &' EITHER write to V',i2,' file or later, OR start new file')
		CLOSE(unit=12)
		goto 2    !get new disc
	   endif
	   fopen=.true.
	endif
501	continue
c
c Write the data
c Have 10 arrays, 80 bytes each = 800 bytes
c (idtype,xA0,xA,xA1,tpulse,nstau,Yinf,ncomp,Vmem,,npt)
c + 2 arrays x 800 bytes (tau,area)=1600
c plus kver, nset, ilabel=12 bytes
c plus titlef=60, titled=1200, titlex=40, titley=40 =1340
c -total=800+1600+12+1340=3752 bytes
c NB can also use LEN from inquire statement to check where new data
c should be written!
	iver=1		!version number for disk (record 1)
	kver=1		!version number for each file
	nvalt=3752			!data length in bytes
c
	read(12,rec=1) nfile,jstrec,lstrec
	nfsav=nfile
	if(nfile.eq.0) then
	   jstrec(1)=1025		!now transparent -byte addresses
	   istrec=jstrec(1)
	endif
c
	if(filspec) goto 405	!ifile specified on input
c
44	continue
	if(.not.readat) then
	   ifile=nfile+1		!default file # to write=next unused one
	   print 404,ifile,ifile
404	   format(
     & ' Next unused file number is # ',i3,/,
     & ' File number to write data in (-1 to skip write)  [',i3,'] = ')
	else
	   ifile=ifile1    !default file # to write=one that data was read from
	   print 4041,ifile1,nfile+1,ifile1
4041	   format(
     & ' Data are from file #',i3,'; next unused file is # ',i3,/,
     & ' File number to write data in (0 to skip write)  [',i3,'] = ')
	endif
	call INPUTi(ifile)
	i=ifile
	if(i.le.0) goto 45		!skip write
	if(i.ge.1.and.i.le.nfile+1) ifile=i		!if i=0, ifile=default value
	if(ifile.ge.1.and.ifile.le.nfile) then  !overwrite existing file (nfile unchanged)
	   if(ifile.eq.1) jstrec(1)=1025	!new start record
	   istrec=jstrec(ifile)		!start record for plot to be overwritten
	else if(ifile.eq.nfile+1) then			!add a new file
	   if(nfile.eq.0) then		!when new file created (ifile=1 here)
		jstrec(1)=1025
		istrec=jstrec(1)
	   else
		istrec=lstrec(nfile)+128	!leave 128 byte gap
	   endif
	   nfile=nfile+1			!update nfile
	else
	   goto 45
	endif
405	continue
	if(istrec.lt.1025) then
	   call BELL(2)
	   print 4051,istrec
4051	   format(' ERROR: first record for data = ',i5)
	   goto 44
	endif
c Check that data will fit on floppy: make max file size=711 records which is
c most that will fit on 3.5 inch floppy (1457664 bytes usable, and
c 1457664/2048=711.75)
	lastr=istrec+nvalt-1		!last byte # for current data
	if(lastr.gt.145760) then
	   call BELL(2)
	   print 43,istrec,lastr
43	    format(
     &   ' Not enough room in data file (1st and last record #= ',2i9)
	    nfile=nfsav
	   goto 44
	endif
c If there are files following this one check that they will not be
c overwritten (option to move files up not in now)
c As check, also check istrec against LEN (from inquire statement) -no
c -omit this for now (prevents alteration of LAST expt on disk)
c	if(ifile.lt.nfsav.or.istrec.lt.len) then
	if(ifile.lt.nfsav) then
	   call BELL(2)
	   print 50,ifile+1
50	   format(
     &	' This file may overwrite part of the next(#',i3,')',/,
     &	' Write to file # ',i3,' anyway [N] ? ')
	   read 101,ans
	   if(ans.ne.'Y') goto 44
	endif
c
c Update jstrec,lstrec
	jstrec(ifile)=istrec	!set value for current plot
	lstrec(ifile)=lastr	!set value for current plot
c
c Now write to disk: update rec #1
	write(12,rec=1) nfile,jstrec,lstrec,iver
c
c Things to go in 1st record of each plot (nfile copied into rec 1 also
c so it can be retrieved). Write all data set titles etc-(=1004 bytes)
c everything except Xobs,Yobs,SETX
c NB Can write only iver=1003!
	irec=istrec
	iver=1		!version number for disk (record 1)
	kver=1		!version number for each file
	write(12,rec=irec) kver,nset,titlef,titled,
     & titlex,titley,ilabel,idtype,xA0,xA,xA1,tpulse,nstau,
     & Yinf,ncomp,Vmem,tau,area,npt
c
	CLOSE(unit=12)
	print 46,ifile,istrec,lastr,infil
	if(slock()) write(7,46) ifile,istrec,lastr,infil
      if(discprt) write(8,46) ifile,istrec,lastr,infil
46	format
     & (' Data file #',i3,' stored in records ',i9,' to ',i9,' of ',a33)
	call flush(7)
45	continue
	RETURN
c
30	continue	!file open error
	call BELL(2)
	print 32
32	format(' Error in opening file.',/)
	present=.false.		!so asks for ndev
	goto 31
	END

