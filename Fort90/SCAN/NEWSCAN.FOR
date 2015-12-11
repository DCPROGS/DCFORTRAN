	program NEWSCAN
c
c To convert old single channel data disks (SCDIRm.DAT, SCDATm.DAT) to
c format produced by SCAN (SCANDAT.DAT)
c
	real*4 TINT(1024)
c  for disc reads must specify integer*2
	integer*2 IAMPL(1024)
	integer*2 Nexpt,nxtrec,idisc,jfirst,iexp1,iexp2
      integer*2 ISTREC,NREC,NINT1,IAVAMP,ISCAN,jfilt
	character*4 adate(3)
	character TITLE1*56,name*12
	character*1 ans,UC
	character ndev*2
	LOGICAL pon,whole,slock
	logical debug,caplock,present
	character*11 cdate,ctime
c Declarations for new style data disk
	integer*1 iprops(1024)
	integer*4 jstrec(200),idiscn,nfile,iscanver
	character titlen*70
	character expdate*11,tapeID*24,expnum*9
c	character*14 ptype(5)
c
c Other declarations
	character*33 infil1,infil2,outfil,path
	logical discprt
	common/dp/discprt
	character*40 mtitle*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c
c
c
c define function
	pon()=slock()
	debug()=caplock()
c
	filnam='NEWSCAN.PRT'
	call WINPRINT	!print file control
      OPEN(unit=7,file=prtport,iostat=nerr)		!open printer
	print 1
	if(pon()) write(7,1)
	if(discprt) write(8,1)
1	FORMAT(
     & ' NEWSCAN: to convert single channel data disk to SCAN format',/)
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 2,cdate,ctime(1:8),mtitle
	if(pon()) write(7,2) cdate,ctime(1:8),mtitle
	if(discprt) write(8,2) cdate,ctime(1:8),mtitle
2	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 3
3	format(
     & ' SCROLL LOCK on for printing'/
     & ' CAPS LOCK on for debugging')
c
c Definitions for SCANDAT
	irfst=1000		!earliest byte # for data
c=	irlast=1456660	!last byte # for data
	irdup=1456664	!byte # to start writing duplicate record
	ioffset=256		!from start of header to start of data
	expnum='***File #'
c
c Specify where old format data are located (default=A:)
c get path for directory
29	print 43
43	format(/,
     &' Specify disk partition for input (SCDIRm,DAT etc):')
	call DISCNUM1(id,ndev,iask,idef)
	if(id.eq.-1) goto 99
	infil1=ndev//'\SCDIRm.DAT'
	call TITENT0('Directory file name:',infil1,33,.false.)
	INQUIRE(file=infil1,exist=present,flen=nlen)
	if(.not.present.or.nlen.eq.0) then
	   call BELL(2)
	   print 31,infil1
31	   format(' Can not find ',a33)
	   goto 29
	endif
c Open, and read, input files
      OPEN(unit=18,file=INFIL1,status='old',
     &    ACCESS='DIRECT',FORM='UNFORMATTED',RECL=128)
c
	read(18,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,iexp1,iexp2
	print 42,name,idisc,jfirst,nexpt
	if(pon()) write(7,42) name,idisc,jfirst,nexpt
	if(discprt) write(8,42) name,idisc,jfirst,nexpt
42	format(1x,a12,' Floppy disc number ',i2,/,
     &	'  -contains experiments ',i4,' to ',i4)
	call DCASK('Convert this disk','y',ans)
	if(ans.eq.'N') then
	   CLOSE(unit=18)
	   goto 29
	endif
	if(.not.whole) then
	   call BELL(2)
	   print 422
422	   format(
     &	' Incomplete data disc- expt files ',i5,' to ',i5,' only')
	   call DCASK('Convert it anyway','n',ans)
	   if(ans.eq.'N') then
		CLOSE(unit=18)
		goto 29
	   endif
	endif
c
c Get path for input
	n=nblank(infil1)
	do i=n,1,-1
	   if(infil1(i:i).eq.'\') then
		n=i
		do j=1,i
		   path(j:j)=infil1(j:j)	!=path, including '\'
		enddo
		goto 30
	   endif
	enddo
30	continue
	infil2=path(1:n)//'SCDATm.DAT'
	call TITENT0('Data file name:',infil2,33,.false.)
	INQUIRE(file=infil2,exist=present,flen=nlen)
	if(.not.present.or.nlen.eq.0) then
	   call BELL(2)
	   print 31,infil2
c31	   format(' Can not find ',a33)
	   goto 29
	endif
c
c Specify where new SCANDAT.DAT should go
c
34	print 11
11	format(/,
     &' Specify disk partition where output (SCANDAT.DAT) is to go:')
	call DISCNUM1(id,ndev,-1,0)	!ask for winchester or floppy
	if(id.eq.-1) goto 99
	outfil=ndev//'\SCANDAT.DAT'
	call TITENT0('Data file name:',outfil,33,.false.)
	INQUIRE(file=outfil,exist=present,flen=nlen)
	if(present.and.nlen.ne.0) then
	   call BELL(2)
	   call DCASK('File already exists: overwrite it','n',ans)
	   if(ans.eq.'N') goto 34
	endif
c
c
c Input file OK so now convert all NEXPT files
c Old directory (unit=18) already open;
c   now open old data file as unit=19
      OPEN(unit=19,file=INFIL2,status='old',
     &    ACCESS='DIRECT',FORM='UNFORMATTED',RECL=1024)
c   and open SCANDAT.DAT as unit=20
	OPEN(unit=20,file=OUTFIL,status='UNKNOWN',access='TRANSPARENT')
c
c First convert header (do not know jstrec() or nextrec for SCANDAT yet,
c so write rec=1 later when they have been worked out)
	nfiles=int4(nexpt)
	idiscn=int4(idisc)
	nfirst=int4(jfirst)
c	nextfile=nfirst         !the first expt file # to be written
	nextrec=irfst     !start at byte # 1000 for expt files
c
c Now go through all the files
	do ifile=nfirst,nfiles
c        read the old directory
	   irec=ifile-nfirst+2
	   read(18,rec=irec)adate,title1,istrec,nrec,nint1,acal,
     &	iavamp,iscan,jfilt,rms1
c      convert header values for SCANDAT
	   nfile=ifile	!'file #'
	   kt=int4(nint1)
	   iscanver=int4(iscan)
	   expdate=adate(1)//adate(2)//adate(3)(1:2)	!actually analysis data
	   titlen(1:56)=title1
	   tapeID='                       '		!not known
	   ipatch=0					!not known
	   Emem=0.					!not known
	   Temp=0.					!not known
	   ffilt=0.5*float(jfilt)		!0.5 to get -3dB
	   avamp=float(iavamp)			!in integer units
	   rms=acal*rms1				!in pA
	   calfac2=acal		!see below
c        and read old data
	   k=istrec		!START RECORD FROM DIRECTORY
	   read(19,rec=k) (TINT(j),j=1,256)
	   if(nint1.gt.256) then
		k=k+1
	     	read(19,rec=k) (TINT(j),j=257,512)
	   endif
	   if(nint1.gt.512) then
		k=k+1
		read(19,rec=k) (TINT(j),j=513,768)
	   endif
	   if(nint1.gt.768) then
		k=k+1
		read(19,rec=k) (TINT(j),j=769,1024)
	   endif
	   k=k+1
	   read(19,rec=k) (IAMPL(j),j=1,512)
	   if(nint1.gt.512) then
		k=k+1
		read(19,rec=k) (IAMPL(j),j=513,1024)
	   endif
c   now convert amplitudes, and set iprops for unusable intervals
c or dubious amplitudes
c AMPLITUDE CONVERSION: need amplitudes as integer*2 in SCANDAT, such
c that multiplying by calfac2 gives pA, so use as they are with calfac2=acal
	   do i=1,nint1
		iprops(i)=0
		if(tint(i).lt.0.) then
		   iprops(i)=IBSET(iprops(i),3)     !set bit 3='8' for unusable
		endif
		if(iabs(iampl(i)).gt.9999.) then
		   iprops(i)=IBSET(iprops(i),0)     !set bit 0='1' for dubious
		   iampl(i)=IACAL(i,iampl)		!remove the +/- 10000
		endif
	   enddo
c
c Now write to SCANDAT
	   irec=nextrec		!where to start writing header
	   irec1=irec+ioffset      !where to start writing data
c Write the header
	   write(20,rec=irec) expnum,nfile,kt,iscanver,expdate,titlen,
     &    tapeID,ipatch,Emem,temp,ffilt,avamp,rms,calfac2,ioffset
c Write the data
	   write(20,rec=irec1)
     &     (tint(i),i=1,kt),(iampl(i),i=1,kt),(iprops(i),i=1,kt)
	   print 33,ifile,irec,irec1
         if(pon()) write(7,33) ifile,irec,irec1
         if(discprt) write(8,33) ifile,irec,irec1
33	   format(
     &   ' Expt file # ',i5,' converted: header starts ',i8,
     &   ': data starts at ',i8)
	   jstrec(ifile-nfirst+1)=irec	!=jstrec(1) for ifile=nfirst
	   nextrec=irec1 + 7*kt +8		!8 extra bytes, just in case!
	enddo       !end of nexpt loop
c
c
c Now update rec=1 and irdup in SCANDAT
	write(20,rec=1) name,idiscn,nfirst,nfiles,jstrec,nextrec
c and write duplicate
	write(20,rec=irdup) name,idiscn,nfirst,nfiles,jstrec,nextrec
c
	nspace=irdup-nextrec+1
      print 35,nextrec,nspace
      if(pon()) write(7,35) nextrec,nspace
      if(discprt) write(8,35) nextrec,nspace
35	format(/,
     & ' Next vacant byte # = ',i8,' ; free bytes left = ',i8)
c
c Close all files
	CLOSE(unit=18)
	CLOSE(unit=19)
	CLOSE(unit=20)
c
99	continue
	call ENDPRINT
	end

	integer*2 function IACAL(k,iampl)
C TO GET AMPLITUDE IN ADC UNITS FROM IAMPL(KT). REMOVES 10000 IF
C THIS HAS BEEN ADDED  (FOR POS AMP) OR SUBRACTED (FOR NEG AMP)
C TO INDICATE 'ASSUMED' VALUE .
	integer*2 IAMPL(1024),ix
C
	IX=0
	IF(k.EQ.0) GOTO 1
	IX=IAMPL(k)
	IF(IX.GT.9999) IX=IX-10000
	IF(IX.LT.-9999) IX=IX+10000
1	IACAL=IX
	RETURN
	END


