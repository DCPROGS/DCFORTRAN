	subroutine DISKIN1(idatyp,nfile,kfile,pfile,nval,irecs,calfacs2,
     & nintt,avamp,rms,ffilt,trise,fc,iscan,readini,ndev,idest,
     & imin,imax,nopoint,name,idiscn,cjump,nswps,tzero,tsamp,toffset,
     & qfile2,adcfil,npatch,defname,nfits,dfinter,sfac2,npfilt,
     & tresolo,tresolg,nmax,ntot,nset,concA,concB,samexp,
     & pfileb,nfileb,kfileb)
c (NB call with qfile, not qfile2)
c no longer need to pass treso,tresg back to main prog
c nfileb() and pfileb() removed from args of new version -needed?
c  -restored 02/26/04 04:45pm
c
c VERSION OF DISKIN FOR NEW SCAN FILES
c
c Modif 08/31/00 04:21pm for new pfile(i,j), and other arrays converted
c to 2 dimensions as in HJCFIT to allow simultaneous fits of multiple sets
c with different conc e.g. with some or all tau() common to all sets9
c NB some arrays may need to be deallocated and re-allocated in main prog
c in nset specified here is not same as from .ini file, bit no arrays used
c in this subroutine have variable dimension
c
c Modif 04/03/99 03:35pm: nfits added to params so timsav etc can be read
c in diskin2
c Modif 01/03/99 04:20pm to read latest scan.scn data files (iscanver=103 o
c  bigger).  These are same as the new scantemp.dat (and have same data
c  as old scantemp.dat, though written in a different order).  Quite
c  a lot of the values are needed only for restarting scan, but they
c  amount to less that 200 bytes so might as well keep them, so scan.dat
c  can also be used to restart scan)
c
c  TO DO:
c  (1) Fix to read first latencies from CJFIT, as in old DISKIN
c
c To read permanent data files (from PC version of SCAN) in EKDIST
c Changes from old diskin:
c (1) last gap in every expt is not defined so set iprops=8 for it
c	If channel was open at end (length undefined) then remove this
c	opening and set length of the gap preceding it as unusable -see DISKIN2
c (2) SUBLEV no longer needed,nor idisk
c (3) jfilt now replaced by ffilt
c (4) acal no longer needed as all amplitudes kept in pA (see DISKIN2)
c     (and rms, avamp returned in pA)
c (5) ISCANVER=100 for first version (ISCANVER=99 for a few expts
c	analysed before point amp histo included on data disk). If data
c	is from an old disk, converted by NEWSCAN then ISCAN is as for
c	old disk (-1001 etc).
c	ISCANVER=101 onwards for versions that deals with CJUMP data
c
c Now have entirely new data structure
c  Use only one file, SCANDAT.DAT, (rather than separate data and directory
c files).  At start of disc keep array
c	(1) name=owner's name
c	(2) idisc=disc # for this owner
c	(3) nfirst=number of 1st expt on this disk
c	(4) nfiles=number of files ('expts') already on ALL disks
c	(5) jstrec(i)=index of byte # where data file ('expt') #i starts
c	(6) nextrec=next vacant record (byte) # for data
c    (need integer*4 to cope with # of bytes on floppy)
c
c Duplicate of all this kept at end of the floppy
c Formatted floppy has 1457664 bytes. Records above add up to 824 bytes,
c but leave say 1000 bytes free by starting to write duplicate records
c at byte # irdup=1456664.  Thus area on floppy available for data
c is from say byte # 1000 to 1456663
c 	jstrec(i) points to start of data, which consists of
c	(a) header with directory data -fixed length= ### bytes.
c	(b) data arrays tint,iampl,iprops (option to also keep timsav etc??)
c  Header always starts with '***File nnnn' so if jstrec is lost can
c  easily search for start of each file.  Header also contains info on
c  length of following data arrays.
c
c KEEPING DATA FILES FOR .INI
c 	Rather as well as keeping expt numbers (eg 4,7,8,9) in kfile, now
c keep entire path, in pfile(). Eg i'th expt is file number kfile(i) in
c in pfile(i); eg pfile(i)='H:\dcdata\scandat.da1'
c
c Data=
c real*4 tint(1...nint) 	   4nint bytes
c integer*2 iampl(1..nint)   2nint bytes
c integer*1 iprops(1..nint)  nint  bytes
c Total storage needed=7*nint bytes
c +header=ioffset=256 bytes
c Declarations as in DSKWRIT (SCAN)
	integer*4 jstrec(200)
c	integer*4 jstrec1(200)
c	character name1*12
c	logical same
	character*40 mtitle		!machine names
	character ndev*2,name*12,ans*1,UC*1,expdate*11,title*70
	character tapeID*24,expnum*9,prtport*4
	character cnum1*11,cnum2*11
c Other declarations:
	logical contig,onedisk,samexp
c==	integer kfile(50)
	real*4 concA(10),concB(10)		!to hold concentrations for each expt
	integer kfile(20,10),kfileb(20,10),nfile(10),nintt(10),nfits(10)
	real*4 tresolo(10),tresolg(10)
	character*40 pfile(20,10)	!path names for SCANDAT files
	character*40 pfileb(20,10)	!from .ini
	integer nfileb(10)
	real*4 calfacs2(20,10)
	integer nval(20,10),irecs(20,10)
	integer nswps(20,10)
	character*14 ptype(5)
	logical cjump
c	real*4 calfacs2(50)
c	integer nval(50),irecs(50),nswps(50)
c	character filnam*32
	LOGICAL debug,caplock,present,nopoint
	logical sbin,shist,sres,sexp
	logical discprt,readini
c Extra declarations for new scan.dat files
	real*8 dfinter,tlast
	real*8 tzerod,tsample
	logical invert,opendown,newpar,sdone
	logical disp,usepots,simulat
	logical disptran,dispderiv,dispguess
	logical shut,shutprev,shutsav,goback,backward
	character ADCFIL*30,cdate*11,cdate1*11,filtfile*20
	character*35 qfile1	!as in scan.dat
	character*40 qfile	!as in vplot and vhist
	character*40 qfile2	!use input -ignore value from disk!
	character adctime*8
	character defname*6
c
c  For parsname etc
	character path*30,path1*30,pname*8,suffix*3	!for parsname
	character exten*4		!extension .scn etc
	logical nopath
c
	common/dp/discprt
	common/sblk/sbin,shist,sres,sexp
c	common/dpp/filnam,machine,ndisc,icol	!for WINPRINT,ENDPRINT,DISCNUM
	common/dpp/filnam,prtport,ndisc,jcol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c	data ipassw/2102/
C
	debug()=caplock()
c
	ptype(1)='outside-out'
	ptype(2)='inside-out'
	ptype(3)='cell-attached'
	ptype(4)='whole-cell'
	ptype(5)='simulated data'
c
c
c
cc Section that is done first time only
cc Check which disc to use
c307	continue
c Down to 'all expt' is now same as hjcdat1, to allow for >1 data set
c
307	continue
c
	if(idatyp.eq.1) goto 100	!separate section for new format data
	if(idatyp.eq.4.or.idatyp.eq.5) then	!.EDE or .EVL files
	   cjump=.false.
	   goto 100
	endif
c
c Ask which disk data file is on
61	continue
	samexp=.false.
	if(readini) then
	   if(nset.gt.1) then
		print 55,nset
55		format(' Last run used ',i3,' data sets')
	   endif
	   do j=1,nset
	     print 552,j,concA(j)*1.e6
552	     format(' Set ',i4,': Concentration (uM) = ',g13.6,
     &     ':  files used were',/,
     &     '     #    file #   file name and path',/)
c	     do i=1,nfile(j)
	     do i=1,nfileb(j)
		print 554,i,kfileb(i,j),pfileb(i,j)
c=		print 554,i,kfile(i,j),pfile(i,j)
554		format(' ',i3,2x,i6,6x,a40)
	     enddo
	     print 108
108	     format(/)
	   enddo	!end of j=1,nset
	   call DCASK(
     &	'Use same experiments again','y',ans)
c     &	'Use same experiments AND concentrations again','y',ans)
	   if(ans.eq.'Y') then
		samexp=.true.
		do j=1,nset
		   nfile(j)=nfileb(j)
		   do i=1,nfileb(j)
			pfile(i,j)=pfileb(i,j)
			kfile(i,j)=kfileb(i,j)
		   enddo
		enddo
		goto 302	!read the expts
	   endif
	endif
c
c
c Now the bit done when NOT using same expts as last time
33	continue
	nset=1
	print 412,nset
412	format(/,
     &' Number of different data sets (eg concentrations) [',i2,'] = ')
	call INPUTi(nset)
	do j=1,nset
	   nsc1=1
	   nsc2=1
	   print 411,j
411	   format(' Set ',i3,
     &' Pool files n1 to n2 [enter 0 if not contiguous]: n1(,n2) = ')
	   call INPUT2i(nsc1,nsc2)
	   if(nsc1.gt.0) then
		contig=.true.
		if(nsc2.eq.0) nsc2=nsc1
		nfile(j)=nsc2-nsc1+1
		do i=1,nfile(j)
		   jexp=nsc1+i-1	!expt number
		   kfile(i,j)=jexp
		enddo
	   else if(nsc1.eq.0) then
		contig=.false.
		print 52,j
52		format(
     & ' Number of experiment-files to be pooled for set #',i3,' = ')
		call INPUTi(nfile(j))
	   endif
	   iask=-1		!hard disk OR floppy
c	   iask=0		!ask for hard disc only
	   idef=1		!default in ndev
	   if(nfile(j).gt.1) then
		call DCASK('Are all these files on one data disk','y',ans)
		onedisk=ans.eq.'Y'
	   else
		onedisk=.true.
	   endif
	   if(.not.readini) ndev='C:'
29	   if(onedisk) then
		print 43
43		format(
     &	' Specify disk partition where data disk is located:')
	      call DISCNUM1(id,ndev,iask,idef)
	      if(id.eq.-1) goto 99
	      pfile(1,j)=pfileb(1,j)
	      call TITENT0('Data file name:',pfile(1,j),33,.false.)
	      INQUIRE(file=pfile(1,j),exist=present,flen=nlen,err=701)
		if(present.and.nlen.gt.0) then
		   OPEN(unit=18,file=pfile(1,j),status='OLD',
     &		access='TRANSPARENT')
c         NB idisc is integer*2 for old type files!
	   	   read(18,rec=1) name,idiscn,nfirst,nfiles,jstrec,nextrec
	         CLOSE(unit=18)
      	   print 53,ndev,name,idiscn,nfirst,nfiles
53	         format(1x,a2,1x,a12,' disc # ',i2,
     &	 '  -contains expts ',i4,' to ',i4)
	         call DCASK(' Is this disc O.K.','y',ans)
		   if(ans.eq.'N') goto 29
      	   if(discprt) write(8,53) ndev,name,idiscn,nfirst,nfiles
		   do i=1,nfile(j)
			pfile(i,j)=pfile(1,j)		!set all paths
		   enddo
		   if(.not.contig) then
			do i=1,nfile(j)
			   print 6
6	      	   format(' Read experiment file no= ')
			   call INPUTi(nsc)
	      	   kfile(i,j)=nsc
			enddo
		   endif
c         Check that all files are on this disk
		   do i=1,nfile(j)
			if(kfile(i,j).lt.nfirst.or.kfile(i,j).gt.nfiles) then
			   print 36,kfile(i,j)
36			   format(
     &		   '&Data file ',i4,' not on this disk: try again.')
			   goto 33
			endif
		   enddo
		else
		   print 32
32		   format(' Data disk not present: try again')
		   goto 33
		endif
	   else		!not one disk
		if(.not.readini) ndev='C:'
		do i=1,nfile(j)
35	   	continue
		   if(.not.contig) then
	            print 6
c6	      	format(' Read experiment file no= ')
			call INPUTi(nsc)
	      	kfile(i,j)=nsc
		   endif
		   print 34,kfile(i,j)
34	         format(
     &	   ' Specify disk partition for data disk with file',i3)
	         call DISCNUM1(id,ndev,iask,idef)
	         if(id.eq.-1) goto 99
		   if(i.eq.1) then
	      	pfile(1,j)=ndev//'\SCANDAT.DAT'
		   else
	      	pfile(i,j)=pfile(i-1,j)
		   endif
	         call TITENT0('Data file name:',pfile(i,j),33,.false.)
	         INQUIRE(file=pfile(i,j),exist=present,flen=nlen,err=701)
	   	   if(present.and.nlen.gt.0) then
			OPEN(unit=18,file=pfile(i,j),status='OLD',
     &		  access='TRANSPARENT')
	      	read(18,rec=1)name,idiscn,nfirst,nfiles,jstrec,nextrec
			CLOSE(unit=18)
      		print 53,ndev,name,idiscn,nfirst,nfiles
	      	call DCASK(' Is this disc O.K.','y',ans)
			if(ans.eq.'N') goto 29
      		if(discprt) write(8,53) ndev,name,idiscn,nfirst,nfiles
		      if(kfile(i,j).lt.nfirst.or.kfile(i,j).gt.nfiles) then
			   print 36,kfile(i,j)
			   goto 35
			endif
		   else
	   		print 32
			goto 35
	         endif
		enddo          !end of nfile loop
	   endif       !end of onedisk/not one disk
	   if(nset.gt.1) then
		print 64,j,concA(j)*1.e6
c64		format(' Set ',i4,': specify concentration (uM) [',g10.3,'] = ')
		call INPUTr(x)
		concA(j)=x*1.e-6	!molar
	   endif
	enddo		!end of j=1,nset loop
	goto 302	!end of old format (scandat.dat)
c
c SEPARATE SECTION FOR NEW SCAN.SCN FILES (idatyp=1)
c The array kfile() is not needed
c Also jump to here to get file names if idatyp=4 or 5
100	continue
c
109	continue
	samexp=.false.
	if(readini) then
	   if(nset.gt.1) then
		print 55,nset
c55		format(' Last run used ',i3,' data sets')
	   endif
	   do j=1,nset
	     if(nset.gt.1) print 552,j,concA(j)*1.e6
c552	     format(' Set ',i4,': Concentration (uM) = ',g13.6,
c     &     ':  files used were',/,
c     &     '     #    file #   file name and path',/)
	     do i=1,nfileb(j)
		print 554,i,kfileb(i,j),pfileb(i,j)
c554		format(' ',i3,2x,i6,6x,a40)
	     enddo
	     print 108
c108	     format(/)
	   enddo	!end of j=1,nset
c
c
	   call PARSNAME(pfileb(1,1),path,ndev,pname,suffix,nopath,33)
	   iopt=1
	   if(idatyp.eq.1) then
		if(suffix.ne.'scn'.and.suffix.ne.'SCN') then
		   call BELL(1)
		   print 73
73		   format(' Input of a *.SCN file expected')
		   iopt=3
		   pause
		endif
	   else if(idatyp.eq.4) then
		if(suffix.ne.'ede'.and.suffix.ne.'EDE') then
		   call BELL(1)
		   print 74
74		   format(' Input of a *.EDE file expected')
		   iopt=3
	         pause
		endif
	   else if(idatyp.eq.5) then
		if(suffix.ne.'evl'.and.suffix.ne.'EVL') then
		   call BELL(1)
		   print 75
75		   format(' Input of a *.EVL file expected')
		   iopt=3
		   pause
		endif
	   endif
	   print 1,iopt
1	   format(
     &    ' (1) Use same experiments again',/,
     &    ' (2) Change the path only',/,
     &    ' (3) Use different experiments',/,
     &    ' Option number [',i2,'] = ')
	   call INPUTi(iopt)
	   if(iopt.eq.1) then
		samexp=.true.
		do j=1,nset
		   nfile(j)=nfileb(j)
		   do i=1,nfileb(j)
			pfile(i,j)=pfileb(i,j)
			kfile(i,j)=kfileb(i,j)
		   enddo
		enddo
		goto 302	!read the expts
	   else if(iopt.eq.2) then
		call PARSNAME(pfileb(1,1),path,ndev,pname,suffix,nopath,33)
	      call TITENT0('specify new path',path,30,.false.)
		n=nblank(path)
		if(n+12.gt.33) then
		   print 2
2		   format(' Path too long')
		   goto 109
		endif
		do j=1,nset
		   do i=1,nfileb(j)
			call PARSNAME(pfileb(i,j),path1,ndev,
     &		   pname,suffix,nopath,33)
			n1=NBLANK(pname)
			pfile(i,j)=path(1:n)//pname(1:n1)//'.'//suffix
			pfileb(i,j)=pfile(i,j)	!must change pfileb() too, for check
		   enddo
		enddo
		goto 109	!check names the expts
	   endif
	endif
c
c Now bit for new scan.dat files when same expts NOT used
	nset=1
	print 412,nset
c412	format(
c     &' Number of different data sets (eg concentrations) [',i2,'] = ')
	call INPUTi(nset)
	do j=1,nset
	   if(nfile(j).le.0) nfile(j)=1
	   print 62,j,nfile(j)
62	   format(
     & ' Set ',i2,': number of SCAN.SCN data files to be pooled [',
     &   i3,'] = ')
	   call INPUTi(nfile(j))
	   call INTCONV(j,cnum1)
	   do i=1,nfile(j)
		if(readini) then
		   pfile(i,j)=pfileb(i,j)
		else
		   pfile(i,j)=pfileb(1,1)
		endif
    		call INTCONV(i,cnum2)
621		call TITENT0('Set '//charnb(cnum1)//', data file '//
     &	charnb(cnum2)//': name =',pfile(i,j),33,.false.)
	      INQUIRE(file=pfile(i,j),exist=present,flen=nlen,err=701)
		if(.not.present.or.nlen.eq.0) then
		   call BELL(1)
		   print 622,pfile(i,j)
622		   format(' Try again: can''t find ',a40)
		   goto 621
		endif
		kfile(i,j)=i	!not really needed for current data format
	   enddo	!i=1,nfile(j)
	   if(nset.gt.1) then
		print 64,j,concA(j)*1.e6
64		format(
     &      ' Set ',i4,': specify concentration (uM) [',g10.3,'] = ')
		call INPUTr(x)
		concA(j)=x*1.e-6	!molar
	   endif
	enddo		!j=1,nset
c
c
c All expt numbers (kfile) and paths (pfile) now in
c Now get total number of transitions, nintt, and return to main prog to
c allocate arrays
302	continue
	nintt=0
	atemp=0.0		!for mean temp
	aavamp=0.0		!for mean avamp
	arms=0.0		!for mean rms
	aEmem=0.0		!for mean Emem
	n3=0
	ntot=0
	nmax=0		!largest of the nintt(j)
c
	OPEN(unit=18,file=pfile(1,1),status='UNKNOWN',
     &	access='TRANSPARENT')
c First read value of ISCAN from first file -should normally be same for
c all files that are pooled
	if(idatyp.eq.1) then	!read iscan
	   read(18,rec=1) iscan	!this will actually be part of 'name' for old scandat
	else if(idatyp.eq.4) then	!read header for .EDE file
	   call READ_EDE(n,kfile,pfile,nset,nfile,nval,irecs,
     &	calfacs2,ffilt,nintt,ntot,nmax)
	   iscan=201	!signals .ede file (but not in file of course)
c	   goto 8
	else if(idatyp.eq.5) then	!read header for .EVL file
	   call READ_EVL(n,kfile,pfile,nset,nfile,nval,irecs,
     &	srate,calfacs2,ffilt,nintt,ntot,nmax)
	   iscan=301	!signals .evl file (but not in file of course)
c	   goto 8
	endif
	if(idatyp.eq.4.or.idatyp.eq.5) then	!read extras
	   a=3.
	   print 283,a
283	   format(
     & ' In order to calculate false event rate, it is necessary to ',/,
     & ' specify the (main) amplitude of openings, and the rms     ',/,
     & ' (std dev) of noise, which are not kept in .ede files.',/,
     & ' Amplitude (pA, with sign) = [',f8.2,'] = ')
	   call INPUTr(a)
	   r=0.3
	   print 284,r
	   call INPUTr(r)
284	   format(
     & ' rms noise (pA) = [',f8.2,'] = ')
	   do j=1,nset
		avamp=a
		rms=r
	   enddo
702	   print 70,srate
c70	   format(' Sample rate for original data (kHz) [',f9.2,'] = ')
	   call INPUTr(srate)
	   if(srate.lt.1.) goto 702
	   srate=srate*1000.	!Hz
	   finter=1.e6/srate	!microsec
	   dfinter=1.d6/dble(srate)	!microsec
	   goto 8
	endif
c
	if(iscan.eq.103.or.iscan.eq.104.or.iscan.eq.-103) then
	   if(idatyp.eq.2) then
		print 203
203		format(
     & ' Data from SCAN seem to be new format (SCAN.SCN): OK [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(ans.eq.'Y') idatyp=1
	   endif
	else
	   if(idatyp.eq.1) then
		print 21
21		format(
     & ' Data from SCAN seem to be old format (SCANDAT.DAT): OK [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(ans.eq.'Y') idatyp=2
	   endif
	endif
c
	if(idatyp.eq.1) then
	   read(unit=18,rec=1) iscan,ioffset
	else if(idatyp.eq.2) then
         read(18,rec=1) name,idiscn,nfirst,nfiles,jstrec,nextrec
	   ifile=kfile(1,1)
	   irec=jstrec(ifile-nfirst+1)	!where to start reading header
	   read(18,rec=irec)expnum,ifile1,kt,iscan	!get iscan
	endif
	iscan1=iscan
	simulat=iscan.eq.-3001.or.iscan.eq.-103	!from SCSIM
c
c Now read all files
	do j=1,nset
	 nintt(j)=0		!# of obs for set j
	 do i=1,nfile(j)
c=	   if(pfile(i,j).ne.pfile(1,j)) then
		CLOSE(unit=18)
		OPEN(unit=18,file=pfile(i,j),status='UNKNOWN',
     &		access='TRANSPARENT')
c=	   endif
	   if(idatyp.eq.2) then
		read(18,rec=1) name,idiscn,nfirst,nfiles,jstrec,nextrec
		ifile=kfile(i,j)
		irec=jstrec(ifile-nfirst+1)	!where to start reading header
c		read(18,rec=irec)expnum,ifile1,kt,iscan,expdate,title,
c     &	    tapeID,ipatch,Emem,temp,ffilt,avamp,rms,calfac2,ioffset,
c     &	    imin,imax,cjump,nsweep,tzero,tsamp,toffset
		simulat=iscan.eq.-3001.or.iscan.eq.-103	!from SCSIM
		if(.not.simulat) then
	  	   read(18,rec=irec)expnum,ifile1,kt,iscan,expdate,title,
     &          tapeID,ipatch,Emem,temp,ffilt,avamp,rms,calfac2,ioffset,
     &	    imin,imax,cjump,nsweep,tzero,tsamp,toffset
		else	!read also imposed resolution for simulated data
		   read(18,rec=irec)expnum,ifile1,kt,iscan,expdate,title,
     &          tapeID,ipatch,Emem,temp,ffilt,avamp,rms,calfac2,ioffset,
     &	    treso,tresg
		   tresolo(j)=treso
		   tresolg(j)=tresg
		endif
		irecs(i,j)=irec + ioffset
	   else if(idatyp.eq.1) then
		read(unit=18,rec=1) iscan
		simulat=iscan.eq.-3001.or.iscan.eq.-103	!from SCSIM
c		NB idatyp from disk is superfluous (and may be wrong)
		if(.not.simulat) then
		    read(unit=18,rec=1) iscan,ioffset,nint,title,expdate,
     &	     defname,tapeID,ipatch,npatch,Emem,temper,adcfil,
     &	     qfile1,cjump,
     &	     nfits0,ntmax,nfmax,nbuf,novlap,srate,finter,
     &	     tsect,ioff,ndat,nsec,nrlast,avtot,navamp,avamp,rms,
     &	     nwrit,nwsav,newpar,opendown,invert,
     &	     usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,
     &	     iboff,
     &	     expfac,bdisp,ibflag,iautosub,xtrig,ndev,cdate,adctime,
     &	     nsetup,filtfile,ffilt,npfilt,
     &	     sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,
     &	     ioff1,disptran,dispderiv,dispguess,ampfac,tmin,
     &	     tsfac,tlfac,sdone,dfinter,tlast,
     &	     shut,shutprev,backward,prevlevel,t0sav,y0sav,vard,
     &	     nshutfit,
     &	     infit,infirst,ixfprev,idiskq,ifirst,base,basevga,
     &	     ibasevga,
     &	     itrig,itrigvga,itriglev,inc,incabs,
     &	     indfst,isdfst,isec,ndisp,ndisp1,
     &	     idatyp1,cdate1,nchan,
     &	     tcfac,facjump,shutsav,goback,
     &	     imin,imax,errfac,derivfac,confac,
     &	     nsweep,njdim,tzerod,intzero,tsample,ktjump,njfit,
     &	     njump,nnull,ktlast,
     &	     izoom,fcz,fczoom,ampz,avampsav,
     &	     itsimp,minmeth,nbasemin,iscrit,scritvar,smultmin
c
		   nfits(j)=nfits0
c
		else if(simulat) then
c    read simulated data in new scan.scn format -dep on whether cjump or not
		 read(18,rec=1) iscan,ioffset,nint,title,expdate,
     &	  tapeID,ipatch,Emem,cjump,avamp,rms,ffilt,calfac2,
     &	  treso,tresg
		 if(cjump) then
		    read(18,rec=1) iscan,ioffset,nint,title,expdate,
     &	    tapeID,ipatch,Emem,cjump,avamp,rms,ffilt,calfac2,
     &	    treso,tresg,nsweep,tzero,tsamp
		 endif
		 tresolo(j)=treso
		 tresolg(j)=tresg
		endif
		irecs(i,j)=ioffset
		kt=nint
		temp=temper
		qfile(1:35)=qfile1
		if(iscan.lt.-103) then
		   call BELL(2)
		   print 63,iscan
63		   format(' ERROR IN SCAN VERSION NUMBER = ',i5)
		   pause
		endif
	   endif
	   tzero=sngl(tzerod)
	   tsamp=sngl(tsample)
	   tzero=tzero/1000.    !ms
	   tsamp=tsamp/1000.    !ms
c Insert precaution for earliest analyses that did not have point amp histos:
c Insert changes for reading of simulated data (iscan=-3001 or -103)
	   nopoint=.false.	!no point amplitude data
	   if(iscan.le.99) then		!including simulated
		imin=1
		imax=1
		nopoint=.true.
	   endif
	   if(iscan.lt.101.and.iscan.ge.99) then	!if simulated, don't change cjump
		cjump=.false.	!cjump not defined for these
	   endif
c
	   nintt(j)=nintt(j)+kt			!accumulate total number
	   nval(i,j)=kt
	   irecs(i,j)=irec + ioffset
	   calfacs2(i,j)=calfac2
	   nswps(i,j)=nsweep
	   avamp=avamp*calfac2		!convert to pA
	   if(ifile.ne.ifile1) then
		print 37,ifile,ifile1
37		format(' ERROR; ifile,ifile = ',2i8)
		stop
	   endif
c Check that filter setting and patch type same for all files
	   if(i.eq.1) then
		ffilt1=ffilt
		ipatch1=ipatch
		Emem1=Emem
	   else
		if(ffilt.ne.ffilt1) then
		   call BELL(1)
	         print 40,i,ffilt,ffilt1
	         if(discprt) write(8,40) i,ffilt,ffilt1
40		   format(' WARNING: filter setting for file #',i3,
     &		' not same as that for first file (',2g13.6,')')
		endif
		if(ipatch.ne.ipatch1) then
		   call BELL(1)
	         print 41,i,ipatch,ipatch1
	         if(discprt) write(8,41) i,ipatch,ipatch1
41		   format(' WARNING: patch type for file #',i3,
     &		' not same as that for first file (',2i6,')')
		endif
		if(Emem.ne.Emem1) then
		   call BELL(1)
	         print 42,i,Emem,Emem1
	         if(discprt) write(8,42) i,Emem,Emem1
42		   format(' WARNING: potential for file #',i3,
     &		' not same as that for first file (',2g13.6,')')
		endif
		if(iscan.ne.iscan1) then
		   call BELL(1)
	         print 44,i,iscan,iscan1
	         if(discprt) write(8,44) i,iscan,iscan1
44		   format(' WARNING: ISCAN for file #',i3,
     &		' not same as that for first file (',2i6,')')
		endif
	   endif
c	 calc means
	   atemp=atemp+temp
	   aavamp=aavamp+avamp
	   arms=arms+rms
	   aEmem=aEmem+Emem
	   n3=n3+1
c
c      print details for each expt file
	   if(.not.cjump) then
	      print 9,pfile(i,j)
            if(discprt) write(8,9) pfile(i,j)
9	      format(' STEADY STATE DATA from file = ',a40)
	   else
	      print 91,pfile(i,j)
            if(discprt) write(8,91) pfile(i,j)
91	      format(' CJUMP DATA from file = ',a40)
	   endif
	   if(iscan.ge.99) then		!new data disk from PC SCAN
	     n=NBLANK(pfile(i,j))
	     if(idatyp.eq.2) then
      	print 38,ifile,kt,pfile(i,j)(1:n),expdate,title,
     &	 tapeID,ptype(ipatch),Emem,temp,ffilt,avamp,rms,calfac2
	     if(discprt)write(8,38)ifile,kt,pfile(i,j)(1:n),expdate,title,
     &	 tapeID,ptype(ipatch),Emem,temp,ffilt,avamp,rms,calfac2
38		format(
     &     ' Experiment file ',i4,': ',i6,' transitions from ',a33,
     &    '; ',a11,/,
     &    ' Title: ',a70,/,
     &    ' Tape details: ',a24,4x,
     &    ' Patch type: ',a14,4x,/,
     &    ' Membrane potential (mV) = ',f9.2,4x,
     &    ' Temperature = ',f9.1,/,
     &    ' Filter (kHz, -3 dB) = ',f12.5,4x,/,
     &    ' Full amplitude (pA) (approx) = ',f9.2,/,
     &    ' RMS noise (pA) = ',f10.4,4x,
     &    ' Calibration (amplitude to pA) = ',g13.6)
	     else if(idatyp.eq.1) then
		print 28,title,pfile(i,j)(1:n),expdate,
     &	 npatch,nint,adcfil,tapeID,ptype(ipatch),Emem,
     &	 temper,ffilt,avamp,rms,calfac2,qfile2
      	if(discprt) write(8,28) title,pfile(i,j)(1:n),expdate,
     &	 npatch,nint,adcfil,tapeID,ptype(ipatch),Emem,
     &	 temper,ffilt,avamp,rms,calfac2,qfile
28		format(/,
     &       ' Title: ',a70,/,
     &	 ' Results file: ',a33,/,
     &	 ' Date of experiment: ',a11,' (patch number ',i3,')'/,
     &	 ' Number of transitions = ',i8,/,
     &	 ' Raw data file: ',a30,/,
     &	 ' Tape details: ',a24,/,
     &	 ' Patch type: ',a14,/,
     &	 ' Membrane potential (mV) = ',f9.2,/,
     &	 ' Temperature = ',f9.1,/,
     &       ' Filter (kHz, -3 dB) = ',f12.5,4x,/,
     &       ' Full amplitude (pA) (approx) = ',f9.2,/,
     &       ' RMS noise (pA) = ',f10.4,4x,/,
     &       ' Calibration (amplitude to pA) = ',g13.6,/,
     &	 ' Plot queue file: ',a40)
c
	     endif
	     if(cjump) then
		   print 201,toffset
201		   format(' Time offset (ms from command to c-jump) = ',
     &	   f10.3,' ms: O.K. [Y] ? ')
		   ans='Y'
		   call INPUTa(ans)
		   if(UC(ans).eq.'N') then
			print 202
202			format(' Time offset (ms from command to c-jump) = ')
			call INPUTr(toffset)
		   endif
	         print 20,nsweep,tsamp,tzero,toffset
	         if(discprt) write(8,20) nsweep,tsamp,tzero,toffset
20		   format(/,' CJUMP data:',/,
     &	   1x,i6,' jumps of ',f10.1,' ms (t=0 at ',f7.2,' ms)',/,
     &	  ' Time offset (ms from command to c-jump) = ',f9.2,' ms')
	     endif
	   else if(.not.simulat.and.iscan.lt.0) then	!converted old disk
		print 319,ifile,title,expdate,kt,ffilt,avamp,rms,calfac2
	      if(discprt) write(8,319)ifile,title,expdate,kt,ffilt,
     &	 avamp,rms,calfac2
319		format(/,' Ex',i5,': ',a70,/,' Analysis date ',a11,
     &	 ':  ',i6,' transitions (data from PDP SCAN)',/,
     &	 ' Filter (-3dB)',f9.1,' Hz : Full amp (app) ',f8.2,' pA',
     &	 ': RMS noise ',f9.3,' pA',/,
     &       ' Calibration (amplitude to pA) = ',g13.6)
	   else if(simulat) then
		n=NBLANK(pfile(i,j))
      	print 381,ifile,kt,pfile(i,j)(1:n),expdate,title,
     &	 ffilt,avamp,rms,calfac2
	      if(discprt)write(8,381)ifile,kt,pfile(i,j)(1:n),expdate,
     &	 title,ffilt,avamp,rms,calfac2
381		format(/,' SIMULATED DATA',/,
     &     ' Experiment file ',i4,': ',i6,' transitions from ',a33,
     &    '; ',a11,/,
     &    ' Title: ',a70,/,
     &    ' Filter (kHz, -3 dB) = ',f12.5,4x,
     &    ' Full amplitude (pA) = ',f9.2,/,
     &    ' RMS noise (pA) = ',f10.4,4x,
     &    ' Calibration (amplitude to pA) = ',g13.6)
		if(cjump) then
		   toffset=0.0
		   print 201,toffset
c201		   format(' Time offset (ms from command to c-jump) = ',
c     &	   f10.3,' ms: O.K. [Y] ? ')
		   ans='Y'
		   call INPUTa(ans)
		   if(UC(ans).eq.'N') then
			print 202
c202			format(' Time offset (ms from command to c-jump) = ')
			call INPUTr(toffset)
		   endif
	         print 20,nsweep,tsamp,tzero,toffset
	         if(discprt) write(8,20) nsweep,tsamp,tzero,toffset
c20		   format(/,' CJUMP data:',/,
c     &	   1x,i6,' jumps of ',f10.1,' ms (t=0 at ',f7.2,' ms)',/,
c     &	  ' Time offset (ms from command to c-jump) = ',f9.2,' ms')
		endif
	   endif
c
	 enddo		!end if i=1,nfile loop
	 if(nintt(j).gt.nmax) nmax=nintt(j)
	 ntot=ntot + nintt(j)
	enddo		!end if j=1,nset loop
c
	if(idatyp.eq.2) then	!finter not stored in old scandat.dat
	   print 70,srate
70	   format(' Sample rate for original data (kHz) [',f9.2,'] = ')
	   srate=srate*1000.	!Hz
	   finter=1.e6/srate	!microsec
	   dfinter=1.d6/dble(srate)	!microsec
	endif
	CLOSE(unit=18)
c
c    calc mean temp, avamp, rms: and check ffilt, ipatch same for all
	if(iscan.ge.99) then		!new data disk from PC SCAN
	   en=float(n3)
	   temp=atemp/en
	   avamp=aavamp/en
	   rms=arms/en
	   Emem=aEmem/en
c
         print 39, ntot,nset,temp,avamp,rms,Emem
         if(discprt) write(8,39) ntot,nset,temp,avamp,rms,Emem
39	   format(/,
     &     ' Total number of transitions = ',i8,', in ',i4,' sets.',/,
     &     ' Temperature (mean) = ',f9.1,/,
     &     ' Approx full amplitude (mean) = ',f9.2,/,
     &     ' RMS baseline noise (mean) = ',f10.4,/,
     &     ' Potential (mV) (mean) = ',f9.2,/)
	else
         print 46, ntot,nset
         if(discprt) write(8,46) ntot,nset
46	   format(/,
     &     ' Total number of transitions = ',i8,', in ',i4,' sets.',/)
	endif
c
8     continue		!jump here after reading EDE and EVL files
c Calc trise etc
	fc=ffilt		!kHz (-3dB)
	trise=332.1/fc		!rise time (microsec)
	if(ffilt.le.0.) then
         print 45
         if(discprt) write(8,45)
45	   format(' Filtering and rise-time not defined')
	   idest=0
	   RETURN
	endif
c
4021	continue
	pause
	print 402
402	format(/,
     &' Risetime setting:',/,
     &' (1) Use nominal filter setting only',/,
     &' (2) Include effect of prefilter and tape on rise time too ',/,
     &' (3) Specify numerical value here',/,
     &' Option number [1] =  ')
	i1=1
	call INPUTi(i1)
	if(i1.le.1) then
	   trise=332.1/fc		!rise time (microsec)
	else if(i1.eq.2) then
	   print 405
405	   format('& -3dB frequency for tape recorder (kHz)= ')
	   call INPUTr(fc1)
	   print 407
407	   format(' -3dB frequency for patch amplifier (kHz)= ')
	   call INPUTr(fc2)
	   fc=1.0/sqrt(1./fc**2 + 1./fc1**2 + 1./fc2**2)
403	   trise=332.1/fc		!rise time (microsec)

	else if(i1.eq.3) then
	   print 4032
4032	   format('& Rise time (microseconds)= ')
	   call INPUTr(trise)
	   fc=332.1/trise	!corresponding fc (-3dB)
	else
	   goto 4021
	endif
c
	print 4011,fc,trise
	if(discprt) write(8,4011) fc,trise
4011	format(
     &' Filter: fc(kHz,-3 dB)= ',f8.2,':  risetime (microsec)= ',
     & g13.6,/)
c
c iscan=-2001 means data from old SCANF (threshold crossing prog)
	if(iscan.eq.-2001) print 227
227	format(' Best resolution for 50% threhold = 1.5 rise-times')
c
	idest=0
	RETURN
c
c Error section
c700	continue
c	call DISKERR(ndev)
c	goto 307
701	continue
	call DISKERR(ndev)
	goto 33
c
99	continue
	idest=99
	RETURN
	end


	subroutine DISKERR(ndev)
	character*2 ndev
c
	call BELL(2)
	print 71
71	format(' Error in finding disk')
	if(ndev.eq.'A:') then
	   print 72
72	   format(
     &   '  -if floppy is not mounted, insert it now then hit enter.')
	endif
	call ANYKEY
	RETURN
	end

