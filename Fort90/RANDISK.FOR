	subroutine RANDISK(Xobs,Yobs,nset,njx,njy,setvar,setx,
     & titled,titlex,titley,IFILE,FOPEN)
	real YOBS(100,10),XOBS(100,10),setx(10),DATCOP(2010)
	integer jstrec(100),lstrec(100),njx(10),njy(10)
	character*60 TITLED(10),titled1			!data set names
	character*14 titlex(10),titley(10)		!col headings
 	character*1 ans,UC
	character ndev*2,filnam*12,infil*33
	logical slock,pon,debug,caplock,setvar,fopen,present
	logical discprt
	common/dp/discprt
c
c To do disc write for RANTEST (based on method in VPLQ1 for minimum
c disk space usage)
c This subroutine does not close unit=12- must set FOPEN and close file
c outside it
c Input:
c IFILE=>1 to read values for file #ifile (which is asked for here so set
c	any value eg ifile=1 in call (and also lists all titles if req)
c IFILE=<0 to write values to file number decided here (and returned in IFILE)
c FOPEN=true if data file already open
c
c
	pon()=slock()
c
101	format(a1)
c Check which disc to use and open file
	if(FOPEN) goto 5
c
3	continue
	ndev='A:'		!default
	print 35
35	format(' Specify disk for RANTEST data file (RANDAT.DAT):')
	call DISCNUM1(id,ndev,-1,1)	!ask for hard or floppy disk
	if(id.eq.-1) goto 999
	infil=ndev//'\RANDAT.DAT'
	call TITENT0(
     &'Name and path for output RANTEST data file:',infil,33,.false.)
	ifile=iabs(ifile)		!restore sign if nec
c
	INQUIRE(file=infil,exist=present,flen=len)
	if(.not.present.or.len.eq.0) then
	   call BELL(1)
	   print 241,infil
241	   format(1x,a33,' does not exist: create it [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
		present=.false.		!so asks for ndev
		goto 3    !get new path/filename
	   endif
c	open new data file (code as in CDSKINIT which should not be needed now)
	   do i=1,100
		jstrec(i)=0
		lstrec(i)=0
	   enddo
	   nfile=0
c=         OPEN(unit=12,file=INFIL,status='UNKNOWN',access='TRANSPARENT')
 	   OPEN(unit=12,file=INFIL,status='NEW',
     &	 access='DIRECT',form='UNFORMATTED',recl=1024)
	   write(12,rec=1) nfile,jstrec,lstrec
	   fopen=.true.
	else
c=       OPEN(unit=12,file=INFIL,status='UNKNOWN',access='TRANSPARENT')
         OPEN(unit=12,file=INFIL,status='UNKNOWN',
     &	 access='DIRECT',form='UNFORMATTED',recl=1024)
	   fopen=.true.
	endif
c	ndev='A:'		!default winchester
c	call DISCNUM1(id,ndev,-1,1)	!ask for winchester or floppy
c	if(id.eq.-1) goto 999
c	FILNAM='\RANDAT.DAT'
c	INFIL=ndev//FILNAM
c      OPEN(unit=12,file=INFIL,status='UNKNOWN',
c     & access='DIRECT',form='UNFORMATTED',recl=1024)
c NB recl is in bytes (4*value for PDP)
c	FOPEN=.true.
c
5	continue	!jump here if already open
c
	if(IFILE.le.0) goto 2	!write the data
c
c Section to read data  for file #ifile
1	continue
	read(12,rec=1) nfile,jstrec,lstrec
c
c
	print 335
335	format(' List file titles [N] ? ')
	read 101,ans
	if(UC(ans).ne.'Y') goto 338
c In Lahey fortran file is not of pre-fixed size but has as many records
c as have been written
	do 518 ifile=1,nfile
	irec=jstrec(ifile)
	read(12,rec=irec) titled,nset
	print 520,ifile
520	format(' File number: ',i3)
	do 519 j=1,nset
519	print 10,j,titled(j)
10	format('  Data set # ',i3,3x,a60)
518	continue	!end of loop to print titles
c
338	print 140,nfile
140	FORMAT(' Read file number (1 -',i3,')= ')
	call INPUTi(ifile)
	if(ifile.lt.1.or.ifile.gt.nfile) goto 338
	if(pon()) write(7,1401)ifile
      if(discprt) write(8,1401)ifile
1401	format(' Data from disk file ',i5)
	nrect=lstrec(ifile)-jstrec(ifile)+1		!total number of records
	nrecd=nrect-1				!number of records for Xval etc
	irec=jstrec(ifile)
	read(12,rec=irec) titled,nset,njx,njy,setvar,setx,
     & titlex,titley
c Now read 'compressed' data in DATCOP from disk
	n=1
	do 380 j=1,nrecd
	  m=n+255
	  irec=irec+1
	  read(12,rec=irec) (DATCOP(i),i=n,m)
	  n=n+256
380	continue
c now decode DATCOP into data arrays
	k=0		!index for datcop
	do 47 j=1,nset
	   do 471 i=1,njx(j)
	   k=k+1
471	   Xobs(i,j)=datcop(k)
	   do 472 i=1,njy(j)
	   k=k+1
472	   Yobs(i,j)=datcop(k)
  	   k=k+1
	   setx(j)=datcop(k)
47	continue
	RETURN
c
c
c Section to write data into file number to be specified here
2	continue
c Compress all real*4 data into DATCOP(): Xobs,Yobs,setx (max=2010 reals)
	k=0		!index for datcop
	do 37 j=1,nset
	   do 371 i=1,njx(j)
	   k=k+1
371	   datcop(k)=Xobs(i,j)
	   do 372 i=1,njy(j)
	   k=k+1
372	   datcop(k)=Yobs(i,j)
	   k=k+1
	   datcop(k)=setx(j)
37	continue
	nvalt=k
	nrecd=1 + (nvalt-1)/256 !no of records for data
	nrect=nrecd+1		!+1 records for misc numbers
c
	read(12,rec=1) nfile,jstrec,lstrec
	if(nfile.eq.0) then	!initialise to 0 in RDSKINIT
	   nfile=1
	   ifile=1
	   jstrec(1)=2
	   istrec=2
	else
	   istrec=lstrec(nfile)+1	!start record for current plot
	   ifile=nfile+1		!next number (nfile not changed yet)
	endif
	print 404,ifile,istrec
404	format(
     & '&Next file number= ',i3,' (start rec #= ',i5,' ): O.K. [Y] ? ')
	read 101,ans
	if(UC(ans).ne.'N') goto 405
c Section done if data not written to next file:
44	continue
	print 408,nfile+1
408	format(' File number (up to',i3,' ) (<ENTER> to abandon)= ')
	ifile=0
	call INPUTi(ifile)
	if(ifile.le.0) goto 45
	if(ifile.eq.1) istrec=2		!new start record (rec 1=jstrec etc now)
	if(ifile.gt.nfile) goto 44	!do not write beyond end of current files
	istrec=jstrec(ifile)	!start record for plot to be overwritten
	nrect1=lstrec(ifile)-jstrec(ifile)+1    !# of records in overwritten file
	if(nrect1.gt.nrect) then
	   call BELL(2)
	   print 431
431	   format(
     &   ' Data needs more records than overwritten file: try another')
         goto 44
	endif
	irec=jstrec(ifile)
	read(12,rec=irec) titled1
	print 521,ifile,titled1
521	format(' Will overwrite file number: ',i3,/,
     & ' (data set #1 =',1x,a60,/,' O.K. [Y] ')
	read 101,ans
	if(UC(ans).eq.'N') goto 44
c
c Check that data will fit in: make max file size=711 records which is
c most that will fit on 3.5 inch floppy (1457664 bytes usable, and
c 1457664/2048=711.75)
405	continue
	lastr=lstrec(ifile)		!last record for current plot
	if(lastr.le.711) goto 42	!OK
	call bell(2)
	call LOCATE(lrow,0)		!row lrow,col 0
	print 43,istrec,lastr
43	format(
     & ' Not enough room in data file (1st and last record #= ',2i5)
	goto 44
c Reach here with ifile=file to be written to, starting at istrec
42	continue
	if(ifile.gt.nfile) nfile=nfile+1	!update if no overwrite
	jstrec(ifile)=istrec	!set value for current plot
	lstrec(ifile)=istrec+nrect-1	!set value for current plot
c Update rec #1
	write(12,rec=1) nfile,jstrec,lstrec
c
c Things to go in 1st record of each plot (nfile copied into rec 1 also
c so it can be retrieved). Write all data set titles etc-(=1004 bytes)
c everything except Xobs,Yobs,SETX
c
	irec=istrec
	write(12,rec=irec) titled,nset,njx,njy,setvar,setx,
     & titlex,titley
c
c Now write DATCOP to disk
	n=1
	do 480 j=1,nrecd
	  m=n+255
	  irec=irec+1
	  write(12,rec=irec) (DATCOP(i),i=n,m)
	  n=n+256
480	continue
c
	print 46,ifile,istrec,irec
	if(slock()) write(7,46) ifile,istrec,irec
      if(discprt) write(8,46) ifile,istrec,irec
46	format
     & (' Data file #',i3,' in records ',i5,' to ',i5)
	call flush(7)
45	continue
	RETURN
999   continue
	nset=-1		!signal to abort
	RETURN
	end



