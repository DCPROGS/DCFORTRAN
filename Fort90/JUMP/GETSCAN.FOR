	subroutine GETSCAN(tintlat,jumps,nsweep,nfile,pfile,kfile,
     &	tzero,tsamp,nomit,jumpomit,readini,endalign,tintend,
     &	tact,njd,ndim,ndim1,idatyp)

c/=========Also check treatment of jumps in resint5 -what happens if
c/=========the only opening in a jump is removed and to jumps(), kjumps()
c
c Modified 05/10/00 11:33am to read new SCAN.DAT (SCAN.SCN) files
c
c To read SCANDAT.DAT in order to get 1st latencies when needed in CJFIT
c (based on DISKIN1/DISKIN2 as used in EKDIST)
c
c  Do not need to return whole tint() etc to main prog -just return
c array of 1st latencies (call RESINT within GETSCAN).  Thus can
c keep tint() etc local within GETSCAN, and no need to split it (as in DISKIN,
c on which GETSCAN is based) in order to return to main prog to allocate tint()
c NB potential confusion between nsweep from CJUMP.DAT file which is
c number of sweeps set for a multi-sweep run in CJUMP, and nsweep
c from SCANDAT.DAT, which is the number of jumps that were analysed
c in SCAN (the first jump # of these, numbered as in CJUMP.DAT, is recorded
c only in jumps(1)).  Thus, rename nsweep from SCANDAT as NJUMPt, and first
c jump, numbered as in CJUMP.DAT, as NJUMP1 (done in CJFIT -retain nsweep here)
c
c Modified 12/16/96 12:32pm so kjumps/kjumps0 are now integer*4 as in ekdist
c (and resint5)
c Now have local integer*2 array to read from scandat (later scandat version
c will have integer*4 on disk)
c
c ===DISKIN notes:
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
c	is from an old disk, converted by NEWSCAN then ISCANVER is as for
c	old disk (-1001 etc).
c	ISCANVER=101 for version that deals with CJUMP data
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
	real*4 tintlat(ndim)		!1st latencies
	real*4 tintend(ndim1)		!'last latencies'
	real*4 tact(ndim1)		!activation length
	integer*2 jumps(ndim)
	integer*2 jumpomit(0:ndim)
c Allocate tint etc locally
	allocatable::tint,tint0
	allocatable::iampl0,ampl0,ampl
	allocatable::iprops,iprops0
	allocatable::index
	allocatable::kjumps0,kjumps,kjumps2
	integer*4 kjumps0(:),kjumps(:)
	integer*2 kjumps2(:)
	real*4 tint(:),tint0(:),ampl0(:),ampl(:)
	integer*2 iampl0(:)
	integer*1 iprops(:),iprops0(:)
	integer*4 index(:)
	integer*4 ifmin(4),ifmax(4)

c Declarations as in DSKWRIT (SCAN)
	integer*4 jstrec(200),jstrec1(200)
	character*40 mtitle		!machine names
	character ndev*2,name*12,name1*12,ans*1,UC*1,expdate*11,title*70
	character tapeID*24,expnum*9
c Other declarations:
	logical contig,onedisk,same,pathset
	integer kfile(20)
	character*33 pfile(20)	!path names for SCAN/SCANDAT files
	character*14 ptype(5)
	logical cjump
	real*4 calfacs2(50)
	integer nval(50),irecs(50),nswps(50)
c	integer*2 Nexpt,nxtrec,idisc,jdisc,jfirst,ix1,ix2,iexp1,iexp2
c	integer*2 ISTREC,NREC,nint,NINT1,IAVAMP2,ISCAN2,jfilt2,jsetup2
c	integer*2 nlon2,nloff2
	character infil*14
	character filnam*32
	LOGICAL debug,pon,slock,caplock,present,pon1,dsav,endalign
	logical sbin,shist,sres,sexp
c For scan.dat
c	character*1 qfile1*35	!as in scan.dat
c	character*1 qfile*40	!as in vplot and vhist
c	character adctime*8,adcfil*30
c	character defname*6
c Extra declarations for scan.dat files
	real*8 dfinter,tlast
	real*8 tzerod,tsample
	logical invert,opendown,newpar,sdone
	logical disp,usepots
	logical disptran,dispderiv,dispguess
	logical shut,shutprev,shutsav,goback,backward
	character ADCFIL*30,cdate*11,cdate1*11,filtfile*20
	character*1 qfile1*35	!as in scan.dat
	character adctime*8
	character defname*6
	character cnum*11
c
c	logical discprt,first,readini
	logical discprt,readini
	common/dp/discprt
	common/sblk/sbin,shist,sres,sexp
c	common/dpp/filnam,machine,ndisc,icol	!for WINPRINT,ENDPRINT,DISCNUM
	common/dpp/filnam,machine,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	common/rblck/treso,tresg,acrit,avamp	!for RESINT
c	data ipassw/2102/
c===Declarations from DISKIN2
c
	logical btest,dubious,badint,zero,null,bad,omit,undef,zero1
C
	pon()=slock()
	debug()=caplock()
	dubious(i)=BTEST(iprops(i),0)	!ampl(i) was dubious (bit 0 set ='1')
	badint(i)=BTEST(iprops(i),3)	!tint(i) was unusable (bit 3='8' set)
c
	ptype(1)='outside-out'
	ptype(2)='inside-out'
	ptype(3)='cell-attached'
	ptype(4)='whole-cell'
	ptype(5)='simulated data'
c
c	if(.not.first) goto 301		!not first call to DISKIN
c
c
c Section that is done first time only
c Check which disc to use
307	continue
c  idatyp=1 for new format, idatyp=2 for scandat disc

      print 60,idatyp
60	format(/,
     & ' (1) Read data from SCAN.DAT file',/,
     & ' (2) Read data from old SCANDAT.DAT disc',/,
     & ' Option number [',i2,']  = ')
	call INPUTi(idatyp)
	print 30
30	format(/,
     &' Now specify name of SCAN output file which contains latencies')

	if(idatyp.eq.2) goto 62	!old format!
c
c
c FIND SCAN.DAT FILE
c
	if(readini) then
	   print 651,nfile
651	   format(' Last run used ',i3,' experiment files, viz.: ',/,
     &	    '     #    file name and path')
	   do i=1,nfile
	      print 65,i,pfile(i)
65	      format(i6,6x,a33)
	   enddo
	   call DCASK('Use same experiments again','y',ans)
	   if(ans.eq.'Y') then
		goto 302	!read the expts
	   endif
	endif
	print 621,nfile
621	format(' Number of SCAN.SCN data files to be pooled [',i3,'] = ')
	call INPUTi(nfile)
	do i=1,nfile
    	   call INTCONV(i,cnum)
	   if(i.gt.1) then
		i1=i-1
	   else
		i1=1
	   endif
	   pfile(i)=pfile(i1)	!default for pfile(i)
64	   call TITENT0('Data file '//charnb(cnum)//': name =',
     &	pfile(i),33,.false.)
	   INQUIRE(file=pfile(i),exist=present,flen=nlen)
	   if(present.and.nlen.gt.0) then
		OPEN(unit=18,file=pfile(i),status='UNKNOWN',
     &		access='TRANSPARENT')
c          NB first 5 values are same whether cjump/simulated or not
		read(unit=18,rec=1) iscanver,ioffset,nint,title1,expdate,
     &	 defname,tapeID,ipatch,npatch,Emem,temper,adcfil,
     &	 qfile1,cjump
		CLOSE(unit=18)
		print 66,i,title1,expdate,nint
66		format(' Experiment file (scan.scn) #',i3,/,
     &	' title: ',a70,/,
     &	' date: ',a11,/,
     &	' number of intervals = ',i9,/,
     &      ' O.K. [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(ans.eq.'Y') then
		   if(discprt) write(8,67)
     &	    expdate,npatch,title1,nint,tapeID
67		    format(/,
     &	  ' Date of experiment: ',a11,' (patch number ',i3,')'/,
     &        ' Title: ',a70,/,
     &	  ' Number of transitions = ',i8,/,
     &	  ' Tape details: ',a24)
		else
		   goto 64
		endif
	   else
		call BELL(1)
		print 63,pfile(i)
63		format(' Can''t find ',a33,/,' Try again')
	      goto 64
	   endif
	enddo
	goto 302	!read data
c End of scan.dat file  definition
c
c
62	continue
c SECTION FOR OLD SCANDAT.DAT
	irdup=1456664	!byte # to start writing duplicate record
c No longer useful to list all names -skip it
	ans='N'
c=	call DCASK(
c=     & 'List names of all single channel data files on discs','n',ans)
	if(ans.eq.'N') goto 31
c ndisc now from WINPRINT.INI file via common/dpp/
	do i=1,ndisc
	  ndev=char(i+66)//':'
	  INFIL=ndev//'\SCANDAT.DAT'
	  INQUIRE(file=INFIL,exist=present,flen=nlen,err=700)
	  if(present.and.nlen.gt.0) then
	      OPEN(unit=18,file=INFIL,status='OLD',access='TRANSPARENT')
c         NB idisc is integer*2 for old type files!
	      read(18,rec=1) name,idiscn,nfirst,nfiles,jstrec,nextrec
	      read(18,rec=irdup) name1,idiscn1,nfirst1,nfiles1,jstrec1,
     &	  nextrec1
	      call CHECKDUP(name,idiscn,nfirst,nfiles,jstrec,nextrec,
     &	  name1,idiscn1,nfirst1,nfiles1,jstrec1,nextrec1,same)
	      print 53,ndev,name,idiscn,nfirst,nfiles
	      if(pon()) write(7,53) ndev,name,idiscn,nfirst,nfiles
	      if(discprt) write(8,53) ndev,name,idiscn,nfirst,nfiles
53	      format(1x,a2,1x,a12,' disc # ',i2,
     &	 '  -contains expts ',i4,' to ',i4)
	      CLOSE(unit=18)
		if(.not.same) then
		   call BELL(3)
		   print 8,ndev
8		   format(
     &	' DUPLICATE FILES ON DATA DISK DO NOT AGREE:',/,
     &	'  -CHECK DISK ',a2,' AND TRY AGAIN')
	      endif
	   endif
	enddo
c
31	continue
c
c Ask which disk data file is on
61	continue
c Jump expts will all be in a single file in SCANDAT.DAT, so need only
c pfile(1)=scanfil, and kfile(1)=kfile1 here
	if(readini) then
	   print 551,nfile
551	   format(' Last run used ',i3,' experiment files, viz.: ',/,
     &	    '     #    file #   file name and path')
	   do i=1,nfile
	      print 55,i,kfile(i),pfile(i)
55	      format(i6,2x,i6,6x,a33)
	   enddo
	   call DCASK('Use same experiments again','y',ans)
	   if(ans.eq.'Y') then
		goto 302	!read the expts
	   endif
	endif
c
c Now the bit done when not using same expts as last time
33	continue
	ans='Y'
	call DCASK('Data from '//charnb(pfile(1)),ans,ans)
	pathset=ans.eq.'Y'
	nsc1=0
	nsc2=0
	print 400
400	format(
     &' Pool files n1 to n2 [<enter> if not contiguous]: n1(,n2)= ')
	call INPUT2i(nsc1,nsc2)
	if(nsc1.gt.0) then
	   contig=.true.
	   if(nsc2.eq.0) nsc2=nsc1
	   nfile=nsc2-nsc1+1
	   do i=1,nfile
		j=nsc1+i-1	!expt number
		kfile(i)=j
	   enddo
	else if(nsc1.eq.0) then
	   contig=.false.
	   print 52
52	   format(' Number of experiment-files to be pooled = ')
	   call INPUTi(nfile)
	endif
	iask=-1		!hard disk OR floppy
c	iask=0		!ask for hard disc only
	idef=1		!default in ndev
	if(nfile.gt.1) then
	   call DCASK('Are all files on one data disk','y',ans)
	   onedisk=ans.eq.'Y'
	else
	   onedisk=.true.
	endif
29	continue
	if(onedisk) then
	   if(.not.pathset) then	!read data file path/name here
		if(.not.readini) ndev='C:'
c		print 43
c43	     format(' Specify disk partition where data disk is located:')
c		call DISCNUM1(id,ndev,iask,idef)
		if(id.eq.-1) goto 99
c=		pfile(1)=ndev//'\SCANDAT.DAT'
		call TITENT0('Data file name:',pfile(1),33,.false.)
	   endif
	   INQUIRE(file=pfile(1),exist=present,flen=nlen,err=701)
	   if(present.and.nlen.gt.0) then
	     OPEN(unit=18,file=pfile(1),status='OLD',access='TRANSPARENT')
c         NB idisc is integer*2 for old type files!
	      read(18,rec=1) name,idiscn,nfirst,nfiles,jstrec,nextrec
	      CLOSE(unit=18)
      	print 53,ndev,name,idiscn,nfirst,nfiles
	      call DCASK(' Is this disc O.K.','y',ans)
		if(ans.eq.'N') goto 29
	      if(pon()) write(7,53) ndev,name,idiscn,nfirst,nfiles
      	if(discprt) write(8,53) ndev,name,idiscn,nfirst,nfiles
		do i=1,nfile
		   pfile(i)=pfile(1)		!set all paths
		enddo
		if(.not.contig) then
		   do i=1,nfile
	      	print 6
6	      	format(' Read experiment file no= ')
			call INPUTi(nsc)
	      	kfile(i)=nsc
		   enddo
		endif
c         Check that all files are on this disk
		do i=1,nfile
		   if(kfile(i).lt.nfirst.or.kfile(i).gt.nfiles) then
			print 36,kfile(i)
36			format(
     &		   '&Data file ',i4,' not on this disk: try again.')
			goto 33
		   endif
		enddo
	   else
	      print 32
32		format(' Data disk not present: try again')
		goto 33
	   endif
	else		!not one disk
	   if(.not.readini) ndev='C:'
	   do i=1,nfile
35	      continue
		if(.not.contig) then
	         print 6
c6	         format(' Read experiment file no= ')
		   call INPUTi(nsc)
	         kfile(i)=nsc
		endif
		print 34,kfile(i)
34	      format(' Specify disk partition for data disk with file',i3)
	      call DISCNUM1(id,ndev,iask,idef)
	      if(id.eq.-1) goto 99
		if(i.eq.1) then
	         pfile(1)=ndev//'\SCANDAT.DAT'
		else
	         pfile(i)=pfile(i-1)
		endif
	      call TITENT0('Data file name:',pfile(i),33,.false.)
	      INQUIRE(file=pfile(i),exist=present,flen=nlen,err=701)
	      if(present.and.nlen.gt.0) then
		   OPEN(unit=18,file=pfile(i),status='OLD',
     &		access='TRANSPARENT')
	         read(18,rec=1) name,idiscn,nfirst,nfiles,jstrec,nextrec
		   CLOSE(unit=18)
      	   print 53,ndev,name,idiscn,nfirst,nfiles
	         call DCASK(' Is this disc O.K.','y',ans)
		   if(ans.eq.'N') goto 29
	         if(pon()) write(7,53) ndev,name,idiscn,nfirst,nfiles
      	   if(discprt) write(8,53) ndev,name,idiscn,nfirst,nfiles
		   if(kfile(i).lt.nfirst.or.kfile(i).gt.nfiles) then
			print 36,kfile(i)
c36			format(
c     &		   '&Data file ',i4,' not on this disk: try again.')
			goto 35
		   endif
		else
	   	   print 32
c32		   format(' Data file not present: try again')
		   goto 35
	      endif
	   enddo          !end of nfile loop
	endif       !end of onedisk/not one disk
c
c All expt numbers (kfile) and paths (pfile) now in
c Now get total number of transitions, nintt, and return to main prog to
c allocate arrays
302	continue
	nintt=0
	nswpt=0		!total number of jumps (sweeps)
	atemp=0.0		!for mean temp
	aavamp=0.0		!for mean avamp
	arms=0.0		!for mean rms
	aEmem=0.0		!for man Emem
c
	OPEN(unit=18,file=pfile(1),status='UNKNOWN',access='TRANSPARENT')
	do i=1,nfile
	   if(i.gt.1.and.pfile(i).ne.pfile(1)) then
		CLOSE(unit=18)
		OPEN(unit=18,file=pfile(i),status='UNKNOWN',
     &		access='TRANSPARENT')
	   endif
	   if(idatyp.eq.1) then
		read(unit=18,rec=1) iscanver
		if(iscanver.ne.-103) then		!need to read as far as tzero, tsamp
c		   read(unit=18,rec=1) iscan,ioffset,nint,title,expdate,
		   read(unit=18,rec=1) iscanver,ioffset,kt,title,expdate,
     &	     defname,tapeID,ipatch,npatch,Emem,temper,adcfil,
     &	     qfile1,cjump,nfits,ntmax,nfmax,nbuf,novlap,srate,finter,
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
     &	     idatyp,cdate1,nchan,
     &	     tcfac,facjump,shutsav,goback,
     &	     imin,imax,errfac,derivfac,confac,
     &	     nsweep,njdim,tzerod,intzero,tsample,ktjump,njfit,
     &	     njump,nnull
c
		   kt=nint
		   tzero=sngl(tzerod)/1000.    !ms
		   tsamp=sngl(tsample)/1000.    !ms
		else if(iscanver.eq.-103) then
c    read simulated data in new scan.dat format -dep on whether cjump or not
		   read(18,rec=1) iscanver,ioffset,nint,title,expdate,
     &	      tapeID,ipatch,Emem,cjump,avamp,rms,ffilt,calfac2,
     &	      treso,tresg
		   if(cjump) then
		      read(18,rec=1) iscanver,ioffset,nint,title,expdate,
     &	      tapeID,ipatch,Emem,cjump,avamp,rms,ffilt,calfac2,
     &	      treso,tresg,nsweep,tzero,tsamp
		   endif
		   kt=nint
		endif
	   else if(idatyp.eq.2) then
	      read(18,rec=1) name,idiscn,nfirst,nfiles,jstrec,nextrec
	      ifile=kfile(i)
	      irec=jstrec(ifile-nfirst+1)	!where to start reading header
	      read(18,rec=irec)expnum,ifile1,kt,iscanver,expdate,title,
     &       tapeID,ipatch,Emem,temp,ffilt,avamp,rms,calfac2,ioffset,
     &       imin,imax,cjump,nsweep,tzero,tsamp
	      tzero=tzero/1000.    !ms
	      tsamp=tsamp/1000.    !ms
	   endif
c Insert precaution for earliest analyses that did not have point amp histos:
	   if(iscanver.lt.101) cjump=.false.	!cjump not defined for these
	   if(.not.cjump) then
		call BELL(3)
		print 21
21		format(' SCANDAT DATA ARE NOT FROM A JUMP EXPERIMENT!')
		CLOSE(unit=18)
		goto 61
	   endif
c
	   nintt=nintt+kt			!accumulate total number
	   nval(i)=kt
c	   irec1=irec+ioffset      !where to start reading data -record here
	   irecs(i)=irec + ioffset
	   calfacs2(i)=calfac2
	   nswps(i)=nsweep
	   nswpt=nswpt+nsweep
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
		   if(pon()) write(7,40) i,ffilt,ffilt1
	         if(discprt) write(8,40) i,ffilt,ffilt1
40		   format(' WARNING: filter setting for file #',i3,
     &		' not same as that for first file (',2g13.6,')')
		endif
		if(ipatch.ne.ipatch1) then
		   call BELL(1)
	         print 41,ipatch,i,ipatch1
		   if(pon()) write(7,41) ipatch,i,ipatch1
	         if(discprt) write(8,41) ipatch,i,ipatch1
41		   format(' WARNING: patch type for file #',i3,
     &		' not same as that for first file')
		endif
		if(Emem.ne.Emem1) then
		   call BELL(1)
	         print 42,Emem,i,Emem1
		   if(pon()) write(7,42) Emem,i,Emem1
	         if(discprt) write(8,42) Emem,i,Emem1
42		   format(' WARNING: potential for file #',i3,
     &		' not same as that for first file')
		endif
	   endif
c	 calc means
	   atemp=atemp+temp
	   aavamp=aavamp+avamp
	   arms=arms+rms
	   aEmem=aEmem+Emem
c
c      print details for each expt file
	   if(iscanver.ge.99) then		!new data disk from PC SCAN
		n=NBLANK(pfile(i))
      	print 38,ifile,kt,pfile(i)(1:n),expdate,title,
     &	 tapeID,ptype(ipatch),Emem,temp,ffilt,avamp,rms,calfac2
      	if(pon()) write(7,38) ifile,kt,pfile(i)(1:n),expdate,title,
     &	 tapeID,ptype(ipatch),Emem,temp,ffilt,avamp,rms,calfac2
	      if(discprt) write(8,38)ifile,kt,pfile(i)(1:n),expdate,title,
     &	 tapeID,ptype(ipatch),Emem,temp,ffilt,avamp,rms,calfac2
38		format(/,
     &    ' SCANDAT.DAT file (from which latencies are read):',/,
     &     ' Experiment file ',i4,': ',i6,' transitions from ',a33,
     &    '; ',a11,/,
     &    ' Title: ',a70,/,
     &    ' Tape details: ',a24,4x,
     &    ' Patch type: ',a14,4x,/,
     &    ' Membrane potential (mV) = ',f9.2,4x,
     &    ' Temperature = ',f9.1,/,
     &    ' Filter (kHz, -3 dB) = ',f12.5,4x,
     &    ' Full amplitude (pA) (approx) = ',f9.2,/,
     &    ' RMS noise (pA) = ',f10.4,4x,
     &    ' Calibration (amplitude to pA) = ',g13.6)
		if(cjump) then
	         print 20,nsweep,tsamp,tzero
      	   if(pon()) write(7,20) nsweep,tsamp,tzero
	         if(discprt) write(8,20) nsweep,tsamp,tzero
20		   format(' CJUMP data:',/,
     &	   1x,i6,' jumps of ',f10.1,' ms (t=0 at ',f7.2,' ms)')
		endif
	   else if(iscanver.lt.0) then	!converted old disk
		print 319,ifile,title,expdate,kt,ffilt,avamp,rms,calfac2
      	if(pon()) write(7,319) ifile,title,expdate,kt,ffilt,
     &	 avamp,rms,calfac2
	      if(discprt) write(8,319)ifile,title,expdate,kt,ffilt,
     &	 avamp,rms,calfac2
319		format(' Ex',i5,': ',a70,/,' Analysis date ',a11,
     &	 ':  ',i6,' transitions (data from PDP SCAN)',/,
     &	 ' Filter (-3dB)',f9.1,' Hz : Full amp (app) ',f8.2,' pA',
     &	 ': RMS noise ',f9.3,' pA',/,
     &       ' Calibration (amplitude to pA) = ',g13.6)
	   endif
c
	enddo		!end if i=1,nfile loop
c
	CLOSE(unit=18)
c
c    calc mean temp, avamp, rms: and check ffilt, ipatch same for all
	if(iscanver.ge.99) then		!data disk from PC SCAN
	   temp=atemp/float(nfile)
	   avamp=aavamp/float(nfile)
	   rms=arms/float(nfile)
	   Emem=aEmem/float(nfile)
c
         print 39, nintt,temp,avamp,rms,Emem
         if(pon()) write(7,39) nintt,temp,avamp,rms,Emem
         if(discprt) write(8,39) nintt,temp,avamp,rms,Emem
39	   format(/,' Total number of transitions = ',i8,/,
     &     ' Temperature (mean) = ',f9.1,/,
     &     ' Approx full amplitude (mean) = ',f9.2,/,
     &     ' RMS baseline noise (mean) = ',f10.4,/,
     &     ' Potential (mV) (mean) = ',f9.2,/)
	endif
c Calc trise etc
	fc=ffilt		!kHz (-3dB)
4021	continue
	print 402
402	format(/,
     &' Risetime setting:',/,
     &' (1) Use nominal filter setting only',/,
     &' (2) Include effect of prefilter and tape on rise time too ',/,
     &' (3) Specify numerical value here',/,
     &' Option number [1] =  ')
	i1=1
	call INPUTi(i1)
	if(i1.le.1) then !default
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
	if(pon()) write(7,4011) fc,trise
	if(discprt) write(8,4011) fc,trise
4011	format(
     &' Filter: fc(kHz,-3 dB)= ',f8.2,':  risetime (microsec)= ',
     & g13.6,/)
	if(iscanver.eq.-2001) print 227
227	format(' Best resolution for 50% threhold = 1.5 rise-times')
c
c Redfine nsweep as total number of sweeps (jumps)
	nsweep=nswpt
c
c Can now allocate arrays
	ALLOCATE(tint0(nintt),iampl0(nintt),iprops0(nintt),ampl0(nintt))
	ALLOCATE(tint(nintt),ampl(nintt),iprops(nintt))
	ALLOCATE(index(nintt))
	ALLOCATE(kjumps0(nsweep),kjumps(nsweep),kjumps2(nsweep))

c Rest of code is based on DISKIN2 (all point amp stuff removed -not needed here
c
	Print 622
622	format(/,' Reading data . . .')
c Now read and pool the expts
	OPEN(unit=18,file=pfile(1),status='UNKNOWN',access='TRANSPARENT')
	ind=1	!i=1,...,nintt -index for concatenated tint()
	indswp=1	!i=1,...,nsweep =index in concatenated jumps,kjumps
	do i=1,nfile
	   irec1=irecs(i)
	   if(i.gt.1.and.pfile(i).ne.pfile(1)) then
		CLOSE(unit=18)
		OPEN(unit=18,file=pfile(i),status='UNKNOWN',
     &		access='TRANSPARENT')
	   endif
	   j1=ind
	   j2=ind+nval(i)-1
	   n1=indswp		!for jumps
	   n2=indswp+nswps(i)-1
c Do not read point amp data, even when present
	   if(idatyp.eq.1) then
		read(18,rec=1) iscanver
		if(iscanver.ne.-103) then	!real data from SCAN
		   read(18,rec=1)
     &          (tint0(j),j=j1,j2),(iampl0(j),j=j1,j2),
     &	    (iprops0(j),j=j1,j2),(ifmin(j),j=1,3),(ifmax(j),j=1,3)
		   irec2=irec1 + (j2-j1+1)*7 + 24 + (ifmax(1)-ifmin(1)+1)*4+
     &	     (ifmax(2)-ifmin(2)+1)*4 + (ifmax(3)-ifmin(3)+1)*4	!as below
	         read(18,rec=irec2)
     &  	     (jumps(j),j=n1,n2),(kjumps0(j),j=n1,n2)
c Correct kjumps by adding to index the number of intervals that were
c read up to end of previous file = j1-1
		   if(i.gt.1) then
			do j=n1,n2
			   kjumps0(j)=kjumps0(j)+(j1-1)
			enddo
		   endif
		else if(iscanver.eq.-103) then	!simulated data from SCSIM
		  read(18,rec=1)
     &          (tint0(j),j=j1,j2),(iampl0(j),j=j1,j2),
     &	    (iprops0(j),j=j1,j2)
c             now define synthetic jumps(), kjumps() for simulated data
		  do n=n1,n2
		   jumps(n)=n
		  enddo
		  nj1=0
		  do n=1,nval(i)				!nint here =kt in scsim
		   if(BTEST(iprops0(n),5)) then	!first in jump
			nj1=nj1+1
			kjumps0(n1+nj1-1)=n
		   endif
		  enddo
		  if(nj1.ne.nswps(i)) then
		    call BELL(3)
		    print 76,nj1,nswps(i)
76		    format(' ERROR: nj1,njump = ',2i8)
		  endif
		endif
c
	   else if(idatyp.eq.2) then
		read(18,rec=irec1)
     &      (tint0(j),j=j1,j2),(iampl0(j),j=j1,j2),(iprops0(j),j=j1,j2),
     &	 (ifmin(j),j=1,3),(ifmax(j),j=1,3)
c Can avoid declaring jfreq etc by calculating the start record, in bytes,
c for the start of the jumps() array.  The tint0 array starts at rec=irec1
c tint0 = (j2-j1+1)*4 bytes
c iampl0 = (j2-j1+1)*2 bytes
c iprops0 = (j2-j1+1)*1 bytes
c ifmin=3*4=12 bytes
c ifmax=3*4=12 bytes
c jfreq1=(ifmax(1)-ifmin(1)+1)*4 bytes
c jfreq2=(ifmax(2)-ifmin(2)+1)*4 bytes
c jfreq1=(ifmax(3)-ifmin(3)+1)*4 bytes
		irec2=irec1 + (j2-j1+1)*7 + 24 + (ifmax(1)-ifmin(1)+1)*4 +
     &	(ifmax(2)-ifmin(2)+1)*4 + (ifmax(3)-ifmin(3)+1)*4
		if(iscanver.le.101) then
	         read(18,rec=irec2)
     &  	   (jumps(j),j=n1,n2),(kjumps2(j),j=n1,n2)
c
c Convert kjumps2 to integer*4
c		   do j=1,nsweep
c		      kjumps0(j)=int4(kjumps2(j))
c		   enddo
		   do j=n1,n2
			kj=int4(kjumps2(j))
	      	if(kj.le.-1.and.kj.ge.-32768) then
			   kj=kj+65536
			else if(kj.lt.-32768) then
	      	   print*,' KJUMPS() IS TOO BIG!!'
	      	   STOP
	   		endif
			kjumps0(j)=kj
		   enddo
		else if(iscanver.ge.102) then	!kjumps is integer*4 already
	         read(18,rec=irec2)
     &  	   (jumps(j),j=n1,n2),(kjumps0(j),j=n1,n2)
		endif
c====need iscanver=-3001 here (old scsim output)?
c
c Correct kjumps by adding to index the number of intervals that were
c read up to end of previous file = j1-1
		if(i.gt.1) then
		   do j=n1,n2
			kjumps0(j)=kjumps0(j)+(j1-1)
		   enddo
		endif
	   endif	!end of idatyp=1/2
c Now convert amplitudes to picoamps with calfac for this particular file
	   do j=j1,j2
		ampl0(j)=calfacs2(i)*float(iampl0(j))
	   enddo
c
c NOW UPDATE index and indswp
	   ind=j2+1		!index in tint for 1st value in next expt file
	   indswp=n2+1	!index in kjump for 1st value in next expt file
	enddo		!end of 'do i=1,nfile'
c
	DEALLOCATE(kjumps2)
c
	CLOSE(unit=18)
c
c For cjump case, check kjump() vs iprops(), for consistency, in case of
c errors in SCAN!
	if(debug()) then
	 do j=1,nsweep
	   if(discprt) write(8,26)j,kjumps0(j)
26	   format(2i8)
	 enddo
	endif
c
	ij=0		!count jumps
	nnull=0	!count null jumps
c	if(cjump) then
	   ns1=1		!initialise for GETJUMP
	   do k=1,nintt
		if(BTEST(iprops0(k),5)) then        !bit 5='32' set
		   ij=ij+1			!count jumps
		   if(kjumps0(ij).ne.k) then
			call BELL(2)
			print 23,ij,kjumps0(ij),k
	      	if(pon()) write(7,23) ij,kjumps0(ij),k
	 	      if(discprt) write(8,23) ij,kjumps0(ij),k
23			format(' kjumps(',i4,') = ',i6,
     &			' is not equal to k =',i6)
		   endif
		   if(k.eq.nintt.or.BTEST(iprops0(k+1),5)) then
			nnull=nnull+1
		   endif
		endif
		if(debug()) then
	       call GETJUMP(k,ijump,njump,ns1,jumps,kjumps0,nsweep) !test for debug
	     	 if(discprt) then
		   if(discprt) write(8,27) k,tint0(k),iampl0(k),iprops0(k),
     &		ijump,njump
27		   format(1x,i6,g13.6,4i8)
		 endif
		endif
c
	   enddo
c
	   if(ij.ne.nsweep) then
		call BELL(1)
		print 24,ij,nsweep
      	if(pon()) write(7,24) ij,nsweep
	      if(discprt) write(8,24) ij,nsweep
24		format(' nsweep =',i4,' not equal to ij = ',i4)
	   else
	      print 25,nsweep,nnull
      	if(pon()) write(7,25) nsweep,nnull
	      if(discprt) write(8,25) nsweep,nnull
25		format(' Read ',i4,' jumps, of which ',i4,' are null')
	   endif
c
c Check whether jumps() suddenly restarts for near 1, as when data is
c from more than one cjump.dat file, each one starting at (or near) jump #1
c	   ALLOCATE(isame(nsweep),jsame(nsweep))
	   print 108
	   if(pon()) write(7,108)
	   if(discprt) write(8,108)
108	   format(/)
	   print 141
c	   if(pon()) write(7,141)
c	   if(discprt) write(8,141)
141	   format(' Sweep #     Jump #    (orig jump #)')
	   nsame=0
	   jadd=0
	   do i=1,nsweep
		pon1=pon()
		if(jadd.eq.0) then
		   print 139,i,jumps(i)
		   if(pon1) write(7,139) i,jumps(i)
		   if(discprt) write(8,139) i,jumps(i)
139		   format(i7,3x,i7)
		else
		   print 140,i,jumps(i),jumps(i)-jadd
		   if(pon1) write(7,140) i,jumps(i),jumps(i)-jadd
		   if(discprt) write(8,140) i,jumps(i),jumps(i)-jadd
140		   format(i7,3x,i7,'  (',i7,')')
		endif
		if(i.lt.nsweep.and.jumps(i+1).lt.jumps(i)) then
		   print 381,i,jumps(i),i+1,jumps(i+1),jumps(i),i+1
381			   format(
     &' Sweep ',i5,' is jump ',i5,' but',/,' sweep ',i5,' is jump ',i5,/
     &,' Add ',i5,' to all jump numbers from sweep ',i5,' O.K. [Y] ? ')
		   ans='Y'
		   call INPUTa(ans)
		   if(UC(ans).ne.'N') then
			jadd=jumps(i)
			do k=i+1,nsweep
			   jumps(k)=jumps(k)+jadd
			enddo
		   endif
		endif
	   enddo
c
c  For cjumps, now check that no jumps are duplicated
	   do i=1,nsweep-1
		do j=i+1,nsweep
		   if(i.ne.j) then
			if(jumps(i).eq.jumps(j)) then
c               Calculate number of transitions in each of the duplicated jumps
c			(NB get ni=1 for null sweep so # of transitions is ni-1
			   if(i.eq.nsweep) then
				ni=nintt-kjumps0(i)+1
			   else
				ni=(kjumps0(i+1)-1) - kjumps0(i)+1
			   endif
			   ni=ni-1	!# of transitions
			   if(j.eq.nsweep) then
				nj=nintt-kjumps0(j)+1
			   else
				nj=(kjumps0(j+1)-1) - kjumps0(j)+1
			   endif
			   nj=nj-1	!# of transitions
			   print 321,i,ni,j,nj,jumps(i)
			   if(pon1) write(7,321) i,ni,j,nj,jumps(i)
			   if(discprt) write(8,321) i,ni,j,nj,jumps(i)
321			   format(/,' Sweep ',i4,' (',i5,
     &			' transitions) and ',/,' sweep ',
     &			i4,' (',i5,' transitions) are both jump # ',i4)
			   nsame=nsame+1
c			   isame(nsame)=i
c			   jsame(nsame)=j
			endif
		   endif
		enddo
	   enddo
	   if(nsame.ne.0) call BELL(2)
c	   DEALLOCATE(isame,jsame)
c
c Omit some jumps?
	   print 108
c	   if(pon1) write(7,108)
c	   if(discprt) write(8,108)

	   ans='N'
	   if(nsame.gt.0) ans='Y'
	   call DCASK('Omit any of these jumps',ans,ans)
	   if(ans.eq.'Y') then
		print 131,jumps(1),jumps(nsweep),nomit
131 		format(' Jumps ',i5,' to ',i5,' read in from SCANDAT.DAT.',/,
     &	'  Last time ',i3,' of these jumps were omitted: ')
		if(nomit.gt.0) then
		   dsav=discprt
		   discprt=.false.
		   call PRINTni2(nomit,jumpomit(1))	!int*2 version
		   discprt=dsav
		   print 137
137		   format(' number   sweep #   jump #')
		   do i=1,nomit
			j=jumpomit(i)
			if(j.ge.1.and.j.le.nsweep) then
			   print 136,i,j,jumps(j)
		   	   if(pon1) write(7,136) i,j,jumps(j)
			   if(discprt) write(8,136) i,j,jumps(j)
136			   format(i3,6x,i5,4x,i5)
			endif
		   enddo
	      endif
	      call DCASK('Omit same jumps again','n',ans)
		if(ans.eq.'N') then
		   do i=1,nsweep
			jumpomit(i)=0
		   enddo
		   print 133
133		   format(' Specify numbers of sweeps to be omitted')
		   call INPUTni2(nomit,jumpomit(1),nsweep)	!int*2 version
		endif
	   else
		nomit=0
		do i=1,nsweep
		   jumpomit(i)=0
		enddo
	   endif
c       Print omissions
	   if(nomit.eq.0) then
	      if(pon1) write(7,134)
      	if(discprt) write(8,134)
134		format(' No jumps omitted',/)
	   else
		print 135,nomit
	      if(pon1) write(7,135) nomit
      	if(discprt) write(8,135) nomit
135		format(1x,i4,' jumps omitted: ',/,
     &	' number   sweep #   jump #')
c		call PRINTni2(nomit,jumpomit(1))
		nn=nnull 	!for values after omissions
		ns=nsweep 	!for values after omissions
		do i=1,nomit
		   j=jumpomit(i)
		   print 136,i,j,jumps(j)
		   if(pon1) write(7,136) i,j,jumps(j)
		   if(discprt) write(8,136) i,j,jumps(j)
c136		   format(i3,6x,i5,4x,i5)
c NB if last interval has bit 32 set (=start of sweep) with nothing
c after it then last sweep must be null
		   ns=ns-1
		   k=kjumps0(j)
		   if(BTEST(iprops0(k),5)) then        !bit 5='32' set
			if(k.eq.nintt.or.BTEST(iprops0(k+1),5)) then
			   nn=nn-1
			endif
		   endif
		enddo
	      print 251,ns,nn
      	if(pon1) write(7,251) ns,nn
	      if(discprt) write(8,251) ns,nn
251		format(/,
     &  ' After omissions have ',i4,' jumps, of which ',i4,' are null')
	   endif
c Set resolution: these data come straight from SCAN so may want to
c set resolution to same value as used in EKDIST
	   ans='N'
	   call DCASK('Specify resolution for latencies',ans,ans)
	   if(ans.eq.'Y') then
		imode=0		!call RESINT from EKDIST
		call RESINT5(tint0,tint,ampl0,ampl,iprops0,iprops,nint,
     &     nintt,imode,ffilt,fc,rms,trise,index,
     &     cjump,nsweep,kjumps0,kjumps)
	   else
		print 16
		if(discprt) write(8,16)
16		format(/,' No resolution imposed',/)
		nint=nintt
		do i=1,nint
		   tint(i)=tint0(i)
		   ampl(i)=ampl0(i)
		   iprops(i)=iprops0(i)
		enddo
		do i=1,nsweep
		   kjumps(i)=kjumps0(i)
		enddo
	   endif
C     endif
c -check number of nulls and of altered first latencies following the
c imposition of the reolution and ask if OK
c
c Define array of first latencies
c The latencies are now put into the element of tintlat() that corresponds
c to the numbering of jumps in cjump.dat.  Initialise tintlat to -4., as signal
c that any undefined elements were not fitted in scan
c
c If endalign required then make also an array of length of shut time from
c from end of last opening to end of sample (or just record index of point
c to start plotting?)
	do i=1,njd
	   tintlat(i)=-4.
	enddo
	do i=1,ndim1
	   tintend(i)=-4.
	enddo
c
	do i=1,nsweep
	   ij=jumps(i)		!index for tintlat
	   omit=.false.
	   if(nomit.gt.0) then
		do k=1,nomit
		   if(i.eq.jumpomit(k)) then
			tintlat(ij)=-3.	!set as 'omit'
			if(endalign) tintend(ij)=-3.
		      omit=.true.
			goto 13		!jump to next sweep
		   endif
		enddo
	   endif
	   ifirst=kjumps(i)	!index in tint() of 1st interval in ith jump
	   njump=jumps(i)		!jump numbering as in CJUMP.DAT
	   if(.not.BTEST(iprops(ifirst),5)) then
		call BELL(1)
		print 22,njump,kjumps(i),iprops(i)
22		format(' ERROR: for jump #',i4,' kjumps = ',i4,
     &	' but iprops = ',i3)
	   endif
	   zero=BTEST(iprops(ifirst),5).and.ampl(ifirst).ne.0
	   if(i.lt.nsweep) then
		iend=kjumps(i+1)-1	!index of last transition in ith jump
	   else
		iend=nint
	   endif
c
	   if(ifirst.eq.iend) then
		null=BTEST(iprops(ifirst),5)
	   else
		null=BTEST(iprops(ifirst),5).and.BTEST(iprops(ifirst+1),5)
	   endif
c
	   bad=.false.
	   if(dubious(ifirst)) then
		bad=.true.
		print 10,njump
10		format(' Jump #',i4,' has latency set as dubious amplitude')
	   endif
	   if(badint(ifirst)) then
		bad=.true.
		print 9,njump
9		format(' Jump #',i4,' has latency set as unusable interval')
	   endif
c
	   if(zero) then
		print 7,njump
7		format(' Jump #',i4,' was open at t=0: latency=0.0')
	   endif
	   if(null) then
		print 11,njump
11		format(' Jump #',i4,' was null')
	   endif
c
c For alignment on last opening need index of last transition in each sweep
	   zero1=.false.
	   if(endalign.and.(.not.null)) then
		if(i.lt.nsweep) then
		   ilast=kjumps(i+1)-1	!index in tint() of last interval in ith jump
		else
		   ilast=nint	!last interval in last sweep
		endif
	      zero1=ampl(ilast).ne.0
		if(zero1) then
		   print 71,njump
71		   format(' Jump #',i4,' was open at end')
		endif
c  Had problem with cases where the sum of all intervals in the jump
c was noticably different from tsamp! (problem in SCAN?) so insert check
c here.  This error is positive for every jump but 1 in DW expt 16-10-96
c and usually below 10 ms but up to 20 ms or more (tsamp=5000 ms)
c For now only way is to finde the length of the activation by adding up
c all the intervals in it here, and keeping it as a separate array
		if(debug()) then
		   s=0.0
		   n=0
		   print 15
		   if(discprt) write(8,15)
15		   format(' jump   # intervals   total length   error')
		   do k=ifirst,ilast
			n=n+1
			s=s + tint(k)
		   enddo
		   s=s + tzero
		   print 14,njump,n,s,tsamp-s
		   if(discprt) write(8,14) njump,n,s,tsamp-s
14		   format(2i8,3x,g13.6,2x,g13.6)
	      endif
c  Add up the activation length here (unless null).
c If open at t=0 then start from ifirst, otherwise start after
c    latency at ifirst+1
c If open at end, include the last interval, but otherwise count
c    up to ilast-1
		if(bad) then
	         tact(ij)=-2.
		else if(null) then
	         tact(ij)=-1.
		else
		   k1=ifirst+1
		   k2=ilast-1
		   if(zero) then
			k1=ifirst
		   else if(zero1) then
			k2=ilast
		   endif
		   s=0.0
		   n=0
		   do k=k1,k2
			n=n+1
			s=s + tint(k)
		   enddo
		   tact(ij)=s
		endif
	   endif
c
	   if(bad) then
	      tintlat(ij)=-2.
	   else if(null) then
	      tintlat(ij)=-1.
	   else if(zero) then
	      tintlat(ij)=0.0
	   else
	      tintlat(ij)=tint(ifirst)
	   endif
	   if(endalign) then
		if(bad) then
	         tintend(ij)=-2.
		else if(null) then
	         tintend(ij)=-1.
		else if(zero1) then
	         tintend(ij)=0.0
		else
	         tintend(ij)=tint(ilast)
		endif
	   endif
c
13	   continue
	enddo		!end of 'do i=1,nsweep'
c
      print 12,nsweep,jumps(1),jumps(nsweep),nomit
      if(pon()) write(7,12) nsweep,jumps(1),jumps(nsweep),nomit
      if(discprt) write(8,12) nsweep,jumps(1),jumps(nsweep),nomit
12	format(/,' First latencies for ',i4,' jumps read from SCANDAT',/,
     & '  (jumps # ',i4,' to ',i4,' in CJUMP.DAT), of which ',i3,
     & ' omitted')
c
c Now print everything
	print 241
	if(pon()) write(7,241)
	if(discprt) write(8,241)
241	format(/,' Jump #      latency    last interval   activation')
	i1=0
	do i=1,njd
	   tin=tintlat(i)
	   ten=tintend(i)
	   tac=tact(i)
	   null=-1.5.le.tin.and.tin.le.-0.5
	   bad=-2.5.le.tin.and.tin.le.-1.5
	   omit=-3.5.le.tin.and.tin.le.-2.5
	   undef=-4.5.le.tin.and.tin.le.-3.5
	   zero=tin.eq.0.0
	   zero1=ten.eq.0.0
	   if(undef) then
		print 246,i
		if(pon1) write(7,246) i
		if(discprt) write(8,246) i
246		format(i7,16x,' undefined')
		goto 250
	   endif
	   i1=i1+1
	   if(zero) then
		print 242,i,tin,ten,tac
		if(pon1) write(7,242) i,tin,ten,tac
		if(discprt) write(8,242) i,tin,ten,tac
242		format(i7,3x,g13.6,3x,g13.6,2x,g13.6,' open at t=0')
	   else if(null) then
		print 243,i
		if(pon1) write(7,243) i
		if(discprt) write(8,243) i
243		format(i7,16x,' null sweep')
	   else if(bad) then
		print 244,i,tin,ten,tac
		if(pon1) write(7,244) i,tin,ten,tac
		if(discprt) write(8,244) i,tin,ten,tac
244		format(i7,3x,g13.6,3x,g13.6,2x,g13.6,' bad sweep')
	   else if(omit) then
		print 245,i
		if(pon1) write(7,245) i
		if(discprt) write(8,245) i
245		format(i7,16x,' sweep omitted')
	   else
		print 240,i,tin,ten,tac
		if(pon1) write(7,240) i,tin,ten,tac
		if(discprt) write(8,240) i,tin,ten,tac
240		format(i7,3x,g13.6,2x,g13.6,2x,g13.6)
	   endif
250	   continue
	enddo
c
c Deallocate arrays
	DEALLOCATE(tint0,iampl0,iprops0,ampl0)
	DEALLOCATE(tint,ampl,iprops)
	DEALLOCATE(index)
	DEALLOCATE(kjumps0,kjumps)
99	continue
	RETURN
c
c Error section
700	continue
	call DISKERR(ndev)
	goto 307
701	continue
	call DISKERR(ndev)
	goto 33
c
c99	continue
c	idest=99
c	RETURN
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

