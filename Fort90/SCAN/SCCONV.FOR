 	program SCCONV
c To convert the floating point values in single channel data files
c (SCDIR.DAT and SCDAT.DAT) to Lahey format.
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
c
c  WHOLE=new logical variable added to directory (record 1)- when
c	true it indicates that this is a complete record of all expts
c	(all floppies should be). Set to false when copies of specified
c	expts only (iexp1 to iexp2) are copied onto DL1 for use in
c	EKDIST in which case record one contains WHOLE,IEXP1,IEXP so
c	can check that desired expts are present, and to prevent copy
c	of incomplete copies of SCDIR.DAT, SCDAT.DAT onto floppy.
c
	REAL TINT(1024)
c  for disc reads must specify integer*2
	INTEGER*2 IAMPL(1024)
	integer*2 Nexpt,nxtrec,idisc,jfirst,iexp1,iexp2
      integer*2 ISTREC,NREC,NINT1,IAVAMP,ISCAN,jfilt
	integer*2 nopen,ngapt,namp,nrub	!for very old SCFIT data
c	DIMENSION ADATE(3)
	character*4 adate(3)
	character TITLE1*56,name*12
	character*1 ans,UC
	character ndev*2
	character*4 acalc,rmsc,TINTc(1024)
	character*4 amplc(512)		!for very old SCFIT data
	real*4 ampl(512)			!for very old SCFIT data
	LOGICAL SCNDAT,whole
	logical discprt
	logical pon,slock,debug,caplock
	character*11 cdate,ctime
	character*40 mtitle*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	common/dp/discprt
c
c define function
	pon()=slock()
	debug()=caplock()
c
	filnam='SCCONV.PRT'
	call WINPRINT	!print file control
      OPEN(unit=7,file=prtport,iostat=nerr)		!open printer
	print 1
	if(pon()) write(7,1)
	if(discprt) write(8,1)
1	FORMAT(' SCCONV: to convert PDP single channel data disc',/)
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
c Open all files here
c Assume that SC data disc in DEC format is in d:\kermit\
	print 11
11	format(' Specify disc where output (converted files) are to go')
c	ndev='D:'		!default winchester
	call DISCNUM1(id,ndev,-1,0)	!ask for winchester or floppy
c Input:
	OPEN(UNIT=10,file='d:\kermit\SCDIR.DAT',status='old',
     &ACCESS='DIRECT',FORM='UNFORMATTED',RECL=128)
c   -for data rec length=256 real=1024 bytes
	OPEN(UNIT=11,file='d:\kermit\SCDAT.DAT',status='old',
     &ACCESS='DIRECT',FORM='UNFORMATTED',RECL=1024)
c Output- keep files on disc 'ndev'
	OPEN(UNIT=12,file=ndev//'\SCDIRm.DAT',status='unknown',
     &ACCESS='DIRECT',FORM='UNFORMATTED',RECL=128)
	OPEN(UNIT=13,file=ndev//'\SCDATm.DAT',status='unknown',
     &ACCESS='DIRECT',FORM='UNFORMATTED',RECL=1024)
c
	read(10,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,iexp1,iexp2
c NB these should all read correctly- no reals
	print 42,name,idisc,jfirst,nexpt,nxtrec
      if(pon()) write(7,42) name,idisc,jfirst,nexpt,nxtrec
      if(discprt) write(8,42) name,idisc,jfirst,nexpt,nxtrec
42	format(' From PDP: ',1x,a12,' Floppy disc number ',i2,/,
     &	'  -contains experiments ',i4,' to ',i4,/,
     &	' Next vacant record in SCDAT.DAT= ',i6)
	if(whole) then
         print 421
         if(pon()) write(7,421)
         if(discprt) write(8,421)
421	   format(' Complete data disc')
	   iexp1=jfirst
	   iexp2=nexpt
	else
         print 422,iexp1,iexp2
         if(pon()) write(7,422) iexp1,iexp2
         if(discprt) write(8,422) iexp1,iexp2
	   call bell(2)
422	   format(
     &	' Incomplete data disc- experiments ',i5,' to ',i5,' only')
c
	endif
c Expts iexp1 to iexp2 to be converted
	print 24,iexp1,iexp2
24	format(
     & ' Experiments',i4,' to',i4,' to be converted: O.K. [Y] ? ')
      read 101,ans
101   format(a1)
c Next line converts 'ans' to upper case
c      if((ans.ge.'a').and.(ans.le.'z')) ans = char(ichar(ans)-32)
      if(UC(ans).eq.'N') then
		print 241
241		format(' Convert experiments N1 to N2: N1,N2= ')
		read 242,iexp1,iexp2
242		format(2i8)
		if(iexp2.lt.iexp1) iexp2=iexp1
	endif
c
      print 26,iexp1,iexp2
      if(pon()) write(7,26) iexp1,iexp2
      if(discprt) write(8,26) iexp1,iexp2
26	format(' Experiments ',i5,' to ',i5,' to be converted:')
c
c First transfer record 1
	write(12,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,iexp1,iexp2
c
	DO 213 i1=iexp1,iexp2
	irec=i1-jfirst+2	!directory record number for current expt
c
	read(10,rec=irec)ADATE,TITLE1,ISTREC,NREC,NINT1,ACALc,
     & IAVAMP,ISCAN,jfilt,rmsc
	scndat=(ISCAN.ge.-1003.and.iscan.le.-1001).or.iscan.eq.-2001.
     &	or.iscan.eq.-3001
	if(iscan.eq.-1001) jfilt=-1
	if(iscan.eq.-1001) rms=0.
	if(.not.scndat) then
	   read(10,rec=irec)ADATE,TITLE1,ISTREC,NREC,
     &	nopen,ngapt,namp,nrub
	   nint1=2*nopen
	endif
c
	if(nint1.gt.1024) then
	   call bell(1)
	   print 801,nint1
801	  format(' Number of intervals = ',i8,': reset it to 1024 [Y] ? ')
	   read 101,ans
	   if(UC(ans).ne.'N') then
	      print 27,i1,nint1
	      if(pon()) write(7,27) i1,nint1
	      if(discprt) write(8,27) i1,nint1
27		format(' Experiment ',i4,' had ', i8,
     &	  ' intervals: changed to 1024')
		nint1=1024
	   endif
	endif
c
	if(.not.scndat) then
c	   call bell(4)
c	   print 12,i1
c12	   format(' This program cannot convert experiment #',i4)
c	   goto 213		!try next
	   k=ISTREC		!START RECORD FROM DIRECTORY
	   j1=1
	   do n=1,nrec
		j2=j1+511
		if(j2.gt.nint1) j2=nint1
		read(11,rec=k) (TINTc(j),j=j1,j2-1,2)	!topen into tint(1,3,5,..)
		k=k+1
		read(11,rec=k) (TINTc(j),j=j1+1,j2,2)	!tgap into tint(2,4,6,..)
		k=k+1
		read(11,rec=k) (AMPLc(j),j=j1/2+1,j2/2)	!ampl(1,2,...)
		k=k+1
		j1=j2+1
	   enddo
c Now convert TINTc values
	   call CONVDEC4(TINTc,TINT,nint1)
c Now convert AMPLc values
	   call CONVDEC4(AMPLc,AMPL,nopen)
c Convert amplitudes to the SCAN format
	   s=0.
	   j=0
	   amax=0.
	   do i=1,nopen
		if(ampl(i).le.9998.) then
		   s=s+ampl(i)
		   j=j+1
		   if(abs(ampl(i)).gt.amax) amax=abs(ampl(i))
		endif
	   enddo
	   acal=amax/2048.
	   s=s/float(j)
	   iavamp=ifixr(s/acal)
	   j=10000
	   if(iavamp.lt.0) j=-10000
	   do i=1,nopen
		iampl(2*i)=0
		if(ampl(i).le.9998.) then
		   iampl(2*i-1)=ifixr(ampl(i)/acal)
		else
		   iampl(2*i-1)=iavamp+j
		endif
	   enddo
	   do i=1,nint1
		if(tint(i).lt.0.) tint(i)=-1000.
	   enddo
c
	   rms=0.		!not defined
	   jfilt=-1		!not defined
         print 217,i1,TITLE1
         if(pon()) write(7,217) i1,TITLE1
         if(discprt) write(8,217) i1,TITLE1
217	   FORMAT(' Convert OLD file #',i4,2x,a56)
	   goto 100
	endif		!end of old data section
c
c	print 800,istrec
c800	format(' istrec= ',i6)
c convert ACAL,RMS which are floating point
	call CONVDEC4(acalc,acal,1)
	call CONVDEC4(rmsc,rms,1)
c
      print 2172,i1,TITLE1
      if(pon()) write(7,2172) i1,TITLE1
      if(discprt) write(8,2172) i1,TITLE1
2172	FORMAT(' Convert file #',i4,2x,a56)
C
c Read the data, with TINT read in character*4 form into TINTc
	k=ISTREC		!START RECORD FROM DIRECTORY
	read(11,rec=k) (TINTc(J),J=1,256)
	if(NINT1.gt.256) then
		k=k+1
	     	read(11,rec=k) (TINTc(J),J=257,512)
	endif
	if(NINT1.gt.512) then
		k=k+1
		read(11,rec=k) (TINTc(J),J=513,768)
	endif
	if(NINT1.gt.768) then
		k=k+1
		read(11,rec=k) (TINTc(J),J=769,1024)
	endif
	k=k+1
	read(11,rec=k) (IAMPL(J),J=1,512)
	if(NINT1.gt.512) then
		k=k+1
		read(11,rec=k) (IAMPL(J),J=513,1024)
	endif
c
c Now convert TINTc values
	call CONVDEC4(TINTc,TINT,nint1)
c
100	continue
c Finally write converted values back to new file- unit=12
c First directory- first record unchanged- take out of loop!
c	write(12,rec=1)Nexpt,nxtrec,idisc,jfirst,name,whole,iexp1,iexp2
c  and entry for current expt (with converted acal,rms values)
	write(12,rec=irec)ADATE,TITLE1,ISTREC,NREC,NINT1,ACAL,
     & IAVAMP,ISCAN,jfilt,rms
c And write back data
	k=ISTREC		!START RECORD FROM DIRECTORY
	write(13,rec=k) (TINT(J),J=1,256)
	if(NINT1.gt.256) then
		k=k+1
	     	write(13,rec=k) (TINT(J),J=257,512)
	endif
	if(NINT1.gt.512) then
		k=k+1
		write(13,rec=k) (TINT(J),J=513,768)
	endif
	if(NINT1.gt.768) then
		k=k+1
		write(13,rec=k) (TINT(J),J=769,1024)
	endif
	k=k+1
	write(13,rec=k) (IAMPL(J),J=1,512)
	if(NINT1.gt.512) then
		k=k+1
		write(13,rec=k) (IAMPL(J),J=513,1024)
	endif
c
213	CONTINUE	!end of expt loop
c
	close(unit=10)
	close(unit=11)
	close(unit=12)
	close(unit=13)
      print 25
      if(pon()) write(7,25)
      if(discprt) write(8,25)
25	format(' Conversion completed')
	call flush(7)
	call ENDPRINT
c
	end

