	subroutine HJCDAT1(idatyp,nfile,kfile,pfile,nval,irecs,calfacs2,
     & nintt,avamp,rms,ffilt,trise,fc,iscan,readini,ndev,idest,
     & name,idiscn,treso,tresg,nset,conc,nlig,ligname,nmax,ntot,
     & qfile2,adcfil,nfileb,pfileb,npatch,defname,samexp,
     & cjump,nsweep,tzero,tsamp)
c
c HJCDAT1 is entirely new version of HJDATIN to read in experimental
c data for HJCFIT for data files from SCANDAT.SCN
c
c NB use qfile in call
c
c To be done: Modified to read AXON dwell time data (.*** file)
c
c Modified 02/07/04 07:26am to read Dempster dwell time data (.EDE file) and
c AXON (.EVL file).
c
c Modif for 100 states 03/29/01 02:17pm
c
c Modif 11/01/01 07:41am to make avamp,rms,ffilt,fc,trise all arrays rms(10) etc
c so false vent rate calculated correctly for each set when nset>1
c
c Modif 05/04/00 04:40pm  Last line of parameters added (for time when jump
c data allowed in hjcfit)
c
c Modif 04/06/00 09:22am for default qfile name.  If same experiments used
c again then default qfile name taken from .ini, otherwise taken
c for the first data file name, pfile(1,1) (usually don't pool more
c than one data file with present versions) -this is done in main prog,
c depending on value od samexp, which is now a parameter
c
c Modif 01/03/99 04:20pm to read latest scan.dat data files (iscanver=103 or
c  bigger).  These are same as the new scantemp.dat (and have same data
c  as old scantemp.dat, though written in a different order).  Quite
c  a lot of the values are needed only for restarting scan, but they
c  amount to less that 200 bytes so might as well keep them, so scan.dat
c  can also be used to restart scan)
c
c It is now based on DISKIN1, DISKIN2 for EKDIST, but have kfile(20,10) for
c file numbers and pfile(20,10) for data file paths (rather than kfile(50),
c pfile(50) in EKDIST), to specify ith file in jth data set.
c (also calfacs2, irec, nval all made 20,10 for use in HJCDAT2)
c  NINTT(j)=total no of intervals in all files in set j before resolution imposed
c  NTOT=total # of intervals in all sets
c  NMAX=maximimum nintt(j) (=allocated dimension of tint0() etc).
c  NINT(j)=total no of intervals AFTER resolution imposed (see RESINT)
c
c NB resolution, treso, tresg, is on disk only for latencies, but reading
c of latencies not included here yet (see old HJDATIN) as it will probably
c be changed.
c
c===original
c	subroutine HJDATIN(TINT,NINTt,ACAL,IAVAMP,idisk,
c     & SUBLEV,jfilt,rms,idest,nfile,kfile,trise,fc,iscan,idiskq,
c     & treso,tresg,ndevs,ndev1,first,nset,concA,concB,readin)
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
c DATA TYPE
c	idatyp=1	data from normal .SCN file
c	idatyp=2	data from old scandat.dat file
c	idatyp=3	internal simulated data (autosim=T) -does not use hjcdat1.for
c	idatyp=4	data from Dempster *.EDE file
c	idatyp=5	data from Axon *.EVL file
c
c VERSION NUMBERS (iscan=iscanver)
c     ISCAN=100 for first version
C	ISCAN=99 for a few expts
c	analysed before point amp histo included on data disk). If data
c	is from an old disk, converted by NEWSCAN then ISCAN is as for
c	old disk (-1001 etc).
c	ISCAN=101-199 for versions that deals with CJUMP data
c	ISCAN=201-299 for data from Dempster .EDE file
c	ISCAN=301-399 for data from Axon .EVL file
c
c (NB iscan=-1001,-1002,-1003,-2001 all indicate PDP-generated files that
c	have been kermitted to PC)
c    iscan=-3001 = simulated on SCSIM and written in scandat.dat disc format
c    iscan=-103 = simulated on SCSIM and written in scan.dat (scan.scn) format
c     iscan= -4001 =first latencies (used anywhere now?)
c
C  ISCAN=-1001 shows data from SCAN (not from old PDP SCFIT prog which
c	cannot be used now)
c  Iscan=-2001 means data from old SCANF (threshold crossing prog)
c  Iscan=-1002 means that ISCAN is followed by valid values of JFILT
c    and RMS (used in new EKDIST to calc false event rate)
c  Iscan=-1003 is as -1002 but indicates that data analysed by version
c    in which meaning of 'assumed' amplitude changed to 'dubious'
c  Iscan=-2001 means data from SCANF
c  Iscan=-3001 means data from SCSIM
c  Iscan=-4001 means data = first latencies from CJFIT1
c
c
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
c 	As well as keeping expt numbers (eg 4,7,8,9) in kfile, now
c keep entire path, in pfile(). Eg i'th expt is file number kfile(i) in
c in pfile(i); eg pfile(i)='H:\dcdata\scandat.da1'
c
c Data=
c real*4 tint(1...nint) 	   4nint bytes
c integer*2 iampl(1..nint)   2*nint bytes
c integer*1 iprops(1..nint)  nint  bytes
c Total storage needed=7*nint bytes
c +header=ioffset=256 bytes
	integer*4 jstrec(200)
	character*40 mtitle*40,filnam*32,prtport*4	!for WINPRINT
	character ndev*2,name*12,ans*1,expdate*11,title*70
	character tapeID*24,expnum*9
c Other declarations:
	real*4 conc(10,10)
c===	real*4 concA(10),concB(10)		!to hold concentrations for each expt
	character*20 ligname(10)
	logical contig,onedisk
	integer kfile(20,10),nfile(10),nfileb(10),nintt(10)
	character*33 pfile(20,10),pfileb(20,10)	!path names for .SCN files
	real*4 calfacs2(20,10)
	integer nval(20,10),irecs(20,10)
	character*14 ptype(5)
	LOGICAL debug,caplock,present
	logical sbin,shist,sres,sexp,samexp
	logical discprt,readini,cjump
c The following are now arrays
	real*4 avamp(10),rms(10),fc(10),ffilt(10),trise(10)
c Extra declarations for new scan.dat files
	real*8 dfinter,tlast
	real*8 tzerod,tsample
	logical invert,opendown,newpar,sdone
	logical disp,usepots,simulat
	logical disptran,dispderiv,dispguess
	logical shut,shutprev,shutsav,goback,backward
	character ADCFIL*30,cdate*11,cdate1*11,filtfile*20
	character*1 qfile1*35	!as in scan.dat
	character*1 qfile*40	!as in vplot and vhist
	character*1 qfile2*40	!use input -ignore value from disk!
	character adctime*8
	character defname*6
	character cnum1*11,cnum2*11
c  For parsname etc
	character path*30,path1*30,pname*8,suffix*3	!for parsname
	character exten*4		!extension .scn etc
	logical nopath

	common/dp/discprt
	common/sblk/sbin,shist,sres,sexp
c	common/dpp/filnam,machine,ndisc,icol	!for WINPRINT,ENDPRINT,DISCNUM
	common/dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
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
307	continue
	if(idatyp.eq.1) then
	   exten='.SCN'
	else if(idatyp.eq.2) then
	   exten='.DAT'
	else if(idatyp.eq.4) then
	   exten='.EDE'
	   iscan=201	!signals .ede file (but not in file of course)
	else if(idatyp.eq.5) then
	   iscan=301	!signals .evl file (but not in file of course)
	   exten='.EVL'
	endif
c
	if(idatyp.ne.2) goto 100	!separate section for new format data
c
c Ask which disk data file is on
61	continue
	samexp=.false.
	if(readini) then
109	   continue
	   print 55,nset
55	   format(' Last run used ',i3,' data sets')
	   do j=1,nset
		if(nlig.eq.0) then
		   print 10,j
10		   format(' Set ',i4,':  files used were',/,
     &     '     #    file #   file name and path',/)
		else
		   do i=1,nlig
			print 11,j,conc(i,j)*1.e6,ligname(i)
11			format(
     &		' Set ',i4,': Concentration (uM) = ',g13.6,1x,a20)
		   enddo
		   print 12
12		   format(
     &     	    ':  files used were',/,
     & 	    '     #    file #   file name and path',/)
		endif
	     do i=1,nfileb(j)
		  print 554,i,kfile(i,j),pfileb(i,j)
554		  format(' ',i3,2x,i6,6x,a33)
	     enddo
	     print 108
108	     format(/)
	   enddo
	   call PARSNAME(pfileb(1,1),path,ndev,pname,suffix,nopath,33)
	   iopt=1
	   if(idatyp.eq.1) then
		if(suffix.ne.'scn'.and.suffix.ne.'SCN') then
		   call BELL(1)
		   print 73
c73		   format(' Input of a *.SCN file expected')
		   iopt=3
		   pause
		endif
	   else if(idatyp.eq.4) then
		if(suffix.ne.'ede'.and.suffix.ne.'EDE') then
		   call BELL(1)
		   print 74
c74		   format(' Input of a *.EDE file expected')
		   iopt=3
	         pause
		endif
	   else if(idatyp.eq.5) then
		if(suffix.ne.'evl'.and.suffix.ne.'EVL') then
		   call BELL(1)
		   print 75
c75		   format(' Input of a *.EVL file expected')
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
			pfileb(i,j)=path(1:n)//pname(1:n1)//'.'//suffix
		   enddo
		enddo
		goto 109	!check names the expts
	   endif
	endif
c
c Now the bit done when NOT using same expts as last time
33	continue
	nset=1
	print 412,nset
412	format(/,
     &' Number of different data sets (eg concentrations) [',i2,'] = ')
c	read 3,n1
c	if(n1.ge.1) nset=n1
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
6	      	   format(' Read experiment file no = ')
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
222	         call TITENT0('Data file name:',pfile(i,j),33,.false.)
		   call DOSFILE(pfile(i,j),33,pfile(i,j),nerr)
		   if(nerr.ne.0) then
			call BELL(1)
			print 221
221			format(' ERROR: not DOS file format')
		   	goto 222
		   endif
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
	enddo		!end of j=1,nset loop
	goto 302	!end of old format (scandat.dat)
c
c SEPARATE SECTION FOR NEW SCAN.DAT FILES (idatyp=1)
c Also jump to here to get file names if idatyp=4 or 5
c
c The array kfile() is not needed
100	continue
	samexp=.false.
	if(readini) then
110	   print 55,nset
c55	   format(' Last run used ',i3,' data sets')
	   do j=1,nset
		if(nlig.eq.0) then
		   print 13,j
13		   format(' Set ',i4,':  files used were',/,
     &     '     #     file name and path',/)
		else
		   do i=1,nlig
			print 11,j,conc(i,j)*1.e6,ligname(i)
c11			format(
c     &		' Set ',i4,': Concentration (uM) = ',g13.6,1x,a20)
		   enddo
		   print 12
c12		   format(
c     &     	    ':  files used were',/,
c     & 	    '     #    file name and path',/)
		endif
		do i=1,nfileb(j)
		   print 14,i,pfileb(i,j)
14		   format(' ',i3,6x,a33)
	     enddo
	     print 108
c108	     format(/)
	   enddo
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
c1	   format(
c     &    ' (1) Use same experiments again',/,
c     &    ' (2) Change the path only',/,
c     &    ' (3) Use different experiments',/,
c     &    ' Option number [',i2,'] = ')
	   call INPUTi(iopt)
	   if(iopt.eq.1) then
		samexp=.true.
		do j=1,nset
		   nfile(j)=nfileb(j)
		   do i=1,nfileb(j)
			pfile(i,j)=pfileb(i,j)
		   enddo
		enddo
		goto 302	!read the expts
	   else if(iopt.eq.2) then
		call PARSNAME(pfileb(1,1),path,ndev,pname,suffix,nopath,33)
	      call TITENT0('specify new path',path,30,.false.)
		n=nblank(path)
		if(n+12.gt.33) then
		   print 2
c2		   format(' Path too long')
		   goto 110
		endif
		do j=1,nset
		   do i=1,nfileb(j)
			call PARSNAME(pfileb(i,j),path1,ndev,
     &		   pname,suffix,nopath,33)
			n1=NBLANK(pname)
			pfileb(i,j)=path(1:n)//pname(1:n1)//'.'//suffix
		   enddo
		enddo
		goto 110	!check names the expts
	   endif
	endif
c Now bit for new scan.dat files when same expts NOT used
	nset=1
	print 412,nset
c412	format(
c     &' Number of different data sets (eg concentrations) [',i2,'] = ')
	call INPUTi(nset)
	do j=1,nset
	   if(nfile(j).le.0) nfile(j)=1
	   print 62,j,exten,nfile(j)
62	   format(
     & ' Set ',i2,': number of ',a4,' data files to be pooled [',
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
		call DOSFILE(pfile(i,j),33,pfile(i,j),nerr)
		if(nerr.ne.0) then
		   call BELL(1)
		   print 221
c221		   format(' ERROR: not DOS file format')
		   goto 621
		endif
	      INQUIRE(file=pfile(i,j),exist=present,flen=nlen,err=701)
		if(.not.present.or.nlen.eq.0) then
		   call BELL(1)
		   print 622,pfile(i,j)
622		   format(' Try again: can''t find ',a33)
		   goto 621
		endif
	   enddo	!i=1,nfile(j)
	enddo		!j=1,nset
c
c All expt numbers (kfile) and paths (pfile) now in
c Now get total number of transitions, nintt, and return to main prog to
c allocate arrays
302	continue
	atemp=0.0		!for mean temp
	aavamp=0.0		!for mean avamp
	arms=0.0		!for mean rms
	aEmem=0.0		!for mean Emem
	n3=0
	ntot=0
	nmax=0		!largest of the nintt(j)
c
	INQUIRE(file=pfile(1,1),exist=present,flen=nlen,err=701)
	if(.not.present.or.nlen.eq.0) then
	   call BELL(1)
	   print 6221,1,pfile(i,j)
6221	   format(' Try again: can''t find file #',i2,': ',a33)
	   goto 100
	endif
	OPEN(unit=18,file=pfile(1,1),status='UNKNOWN',
     &	access='TRANSPARENT')
c First read value of ISCAN from first file -should normally be same for
c all files that are pooled
c
	if(idatyp.eq.1) then	!read iscan
	   read(18,rec=1) iscan	!this will actaully be part of 'name' for old scandat
	else if(idatyp.eq.4) then	!read header for .EDE file
	   call READ_EDE(n,kfile,pfile,nset,nfile,nval,irecs,
     &	calfacs2,ffilt,nintt,ntot,nmax)
c	   goto 9
	else if(idatyp.eq.5) then	!read header for .EVL file
	   call READ_EVL(n,kfile,pfile,nset,nfile,nval,irecs,
     &	srate,calfacs2,ffilt,nintt,ntot,nmax)
c	   goto 9
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
		avamp(j)=a
		rms(j)=r
	   enddo
	   goto 9
	endif
c
c
	if(iscan.eq.103.or.iscan.eq.104.or.iscan.eq.-103) then
	   if(idatyp.eq.2) then		!iscan suggests should NOT have idatyp=2
		print 20,iscan
20		format(
     & ' Data from SCAN seem to be new format (SCAN.SCN): OK [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(ans.eq.'Y') idatyp=1
	   endif
	else
	   if(idatyp.eq.1) then
		print 21,iscan
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
		if(.not.present.or.nlen.eq.0) then
		   call BELL(1)
		   print 6221,j,pfile(i,j)
c6221		   format(' Try again: can''t find file #',i2,': ',a33)
		   goto 100
		endif
		OPEN(unit=18,file=pfile(i,j),status='UNKNOWN',
     &		access='TRANSPARENT')
c=	   endif
	   if(idatyp.eq.2) then
		read(18,rec=1) name,idiscn,nfirst,nfiles,jstrec,nextrec
		ifile=kfile(i,j)
		irec=jstrec(ifile-nfirst+1)	!where to start reading header
		if(iscan.ne.-3001) then
	  	   read(18,rec=irec)expnum,ifile1,kt,iscan,expdate,title,
     &  tapeID,ipatch,Emem,temp,ffilt(j),avamp(j),rms(j),calfac2,ioffset
		else	!read also imposed resolution for simulated data
		   read(18,rec=irec)expnum,ifile1,kt,iscan,expdate,title,
     & tapeID,ipatch,Emem,temp,ffilt(j),avamp(j),rms(j),calfac2,ioffset,
     &	    treso,tresg
		endif
c		irec1=irec+ioffset      !where to start reading data -record here
		irecs(i,j)=irec + ioffset
	   else if(idatyp.eq.1) then
		if(iscan.ne.-103.and.iscan.ne.104) then
		 read(unit=18,rec=1) iscan,ioffset,nint,title,expdate,
     &     defname,tapeID,ipatch,npatch,Emem,temper,adcfil,qfile1,cjump,
     &	 nfits,ntmax,nfmax,nbuf,novlap,srate,finter,
     &	 tsect,ioff,ndat,nsec,nrlast,avtot,navamp,avamp(j),rms(j),
     &	 nwrit,nwsav,newpar,opendown,invert,
     &	 usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,iboff,
     &	 expfac,bdisp,ibflag,iautosub,xtrig,ndev,cdate,adctime,
     &	 nsetup,filtfile,ffilt(j),npfilt,
     &	 sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,
     &	 ioff1,disptran,dispderiv,dispguess,ampfac,tmin,
     &	 tsfac,tlfac,sdone,dfinter,tlast,
     &	 shut,shutprev,backward,prevlevel,t0sav,y0sav,vard,nshutfit,
     &	 infit,infirst,ixfprev,idiskq,ifirst,base,basevga,ibasevga,
     &	 itrig,itrigvga,itriglev,inc,incabs,
     &	 indfst,isdfst,isec,ndisp,ndisp1,
     &	 idatyp1,cdate1,nchan,
     &	 tcfac,facjump,shutsav,goback,
     &	 imin,imax,errfac,derivfac,confac,
     &	 nsweep,njdim,tzerod,intzero,tsample,ktjump,njfit,
     &	 njump,nnull,ktlast,
     &	 izoom,fcz,fczoom,ampz,avampsav,
     &	 itsimp,minmeth,nbasemin,iscrit,scritvar,smultmin
		else if(iscan.eq.104) then
		 read(unit=18,rec=1) iscan,ioffset,nint,title,expdate,
     &     defname,tapeID,ipatch,npatch,Emem,temper,adcfil,qfile1,cjump,
     &	 nfits,ntmax,nfmax,nbuf,novlap,srate,finter,
     &	 tsect,ioff,ndat,nsec,nrlast,avtot,navamp,avamp(j),rms(j),
     &	 nwrit,nwsav,newpar,opendown,invert,
     &	 usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,iboff,
     &	 expfac,bdisp,ibflag,iautosub,xtrig,ndev,cdate,adctime,
     &	 nsetup,filtfile,ffilt(j),npfilt,
     &	 sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,
     &	 ioff1,disptran,dispderiv,dispguess,ampfac,tmin,
     &	 tsfac,tlfac,sdone,dfinter,tlast,
     &	 shut,shutprev,backward,prevlevel,t0sav,y0sav,vard,nshutfit,
     &	 infit,infirst,ixfprev,idiskq,ifirst,base,basevga,ibasevga,
     &	 itrig,itrigvga,itriglev,inc,incabs,
     &	 indfst,isdfst,isec,ndisp,ndisp1,
     &	 idatyp1,cdate1,nchan,
     &	 tcfac,facjump,shutsav,goback,
     &	 imin,imax,errfac,derivfac,confac,
     &	 nsweep,njdim,tzerod,intzero,tsample,ktjump,njfit,
     &	 njump,nnull,ktlast,
     &	 izoom,fcz,fczoom,ampz,avampsav,
     &	 itsimp,minmeth,nbasemin,iscrit,scritvar,smultmin,
     &       stpfac,nlig,conc

		else if(iscan.eq.-103) then		!simulated (see dskwrit2)
		 read(18,rec=1) iscanver,ioffset,nint,title,expdate,
     &	  tapeID,ipatch,Emem,cjump,avamp(j),rms(j),ffilt(j),calfac2,
     &	  treso,tresg
		 if(cjump) then
		    read(18,rec=1) iscanver,ioffset,nint,title,expdate,
     &        tapeID,ipatch,Emem,cjump,avamp(j),rms(j),ffilt(j),calfac2,
     &	    treso,tresg,nsweep,tzero,tsamp
		 endif
		endif
		irecs(i,j)=ioffset
		kt=nint
		temp=temper
		qfile(1:35)=qfile1
c		if(iabs(iscan).lt.103) then
c		   call BELL(2)
c		   print 63,iscan
c63		   format(' ERROR IN SCAN VERSION NUMBER = ',i5)
c		   pause
c		endif
	   endif
	   tzero=sngl(tzerod)
	   tsamp=sngl(tsample)
	   tzero=tzero/1000.    !ms
	   tsamp=tsamp/1000.    !ms
c
	   ntot=ntot+kt
	   nintt(j)=nintt(j) + kt 	!accumulate total number for set j
	   nval(i,j)=kt
	   calfacs2(i,j)=calfac2
	   avamp(j)=avamp(j)*calfac2		!convert to pA
	   if(ifile.ne.ifile1) then
		print 37,ifile,ifile1
37		format(' ERROR; ifile,ifile = ',2i8)
		stop
	   endif
c Check that filter setting and patch type same for all files
	   if(i.eq.1) then
		ffilt1=ffilt(j)
		ipatch1=ipatch
		Emem1=Emem
	   else
c=		if(ffilt(j).ne.ffilt1) then
		if(abs(ffilt(j)-ffilt1).gt.1.e-20) then
		   call BELL(1)
	         print 40,i,ffilt(j),ffilt1
	         if(discprt) write(8,40) i,ffilt(j),ffilt1
40		   format(' WARNING: filter setting for file #',i3,
     &		' not same as that for first file (',2g13.6,')')
		endif
		if(ipatch.ne.ipatch1) then
		   call BELL(1)
	         print 41,ipatch,i,ipatch1
	         if(discprt) write(8,41) ipatch,i,ipatch1
41		   format(' WARNING: patch type for file #',i3,
     &		' not same as that for first file (',2i4,')')
		endif
c=		if(Emem.ne.Emem1) then
		if(abs(Emem-Emem1).gt.1.e-20) then
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
	   aavamp=aavamp+avamp(j)
	   arms=arms+rms(j)
	   aEmem=aEmem+Emem
	   n3=n3+1
c
c      print details for each expt file
	   if(iscan.ge.99) then		!new data disk from PC SCAN
	    n=NBLANK(pfile(i,j))
	    if(idatyp.eq.2) then
      	print 38,j,ifile,kt,pfile(i,j)(1:n),expdate,title,
     &   tapeID,ptype(ipatch),Emem,temp,ffilt(j),avamp(j),rms(j),calfac2
	      if(discprt) write(8,38) j,ifile,kt,pfile(i,j)(1:n),expdate,
     & 	title,tapeID,ptype(ipatch),Emem,temp,
     &      ffilt(j),avamp(j),rms(j),calfac2
38		format(/,' SET # ',i3,/,
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
	    else if(idatyp.eq.1) then
		print 28,j,title,pfile(i,j)(1:n),expdate,
     &	 npatch,nint,adcfil,tapeID,ptype(ipatch),Emem,
     &	 temper,ffilt(j),avamp(j),rms(j),calfac2,qfile2
      	if(discprt) write(8,28) j,title,pfile(i,j)(1:n),expdate,
     &	 npatch,nint,adcfil,tapeID,ptype(ipatch),Emem,
     &	 temper,ffilt(j),avamp(j),rms(j),calfac2,qfile2

28		format(/,' SET # ',i3,/,
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
	    endif
c	   else if(iscan.eq.-3001) then	!converted simulations disk
	   else if(simulat) then	!simulation
		to=1000.*treso
		tg=1000.*tresg
		print 320,ifile,title,expdate,kt,to,tg,
     &	ffilt(j),avamp(j),rms(j),calfac2
	      if(discprt) write(8,320)ifile,title,expdate,kt,to,tg,
     &	ffilt(j),avamp(j),rms(j),calfac2
320		format(' Ex',i5,': ',a70,/,' Analysis date ',a11,
     &	   ':  ',i6,' transitions. Simulated data; res = ',
     &	   f7.1,',',f7.1,' microsec (open, shut)',/,
     &	   ' Filter (-3dB)',f9.1,' Hz : Full amp (app) ',f8.2,'pA',
     &	   ': RMS noise ',f9.3,'pA',/,
     &       ' Calibration (amplitude to pA) = ',g13.6)
	   else if(iscan.lt.0) then	!converted old disk
		print 319,ifile,title,expdate,kt,
     &	ffilt(j),avamp(j),rms(j),calfac2
	      if(discprt) write(8,319)ifile,title,expdate,kt,
     &	ffilt(j),avamp(j),rms(j),calfac2
319		format(' Ex',i5,': ',a70,/,' Analysis date ',a11,
     &	 ':  ',i6,' transitions (data from PDP SCAN)',/,
     &	 ' Filter (-3dB)',f9.1,' Hz : Full amp (app) ',f8.2,' pA',
     &	 ': RMS noise ',f9.3,' pA',/,
     &       ' Calibration (amplitude to pA) = ',g13.6)
	   endif
	   pause
c
	 enddo		!end if i=1,nfile loop
	 if(nintt(j).gt.nmax) nmax=nintt(j)
	enddo		!end if j=1,nset loop
c
	CLOSE(unit=18)
c
c    calc mean temp, avamp, rms: and check ffilt, ipatch same for all
9	continue		!jump here after reading EDE and EVL files
	if(iscan.ge.99.and.iscan.le.199) then    !new data disk from PC SCAN
	   en=float(n3)
	   temp=atemp/en
	   aavamp=aavamp/en
	   arms=arms/en
	   Emem=aEmem/en
c
         print 39, ntot,nset,temp,aavamp,arms,Emem
         if(discprt) write(8,39) ntot,nset,temp,aavamp,arms,Emem
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
c Calc trise etc (for false event rate in RESINT)
	do j=1,nset
	   fc(j)=ffilt(j)		!kHz (-3dB)
	   trise(j)=332.1/fc(j)		!rise time (microsec)
	   if(ffilt(j).le.0.) then
      	print 45,j
      	if(discprt) write(8,45) j
45		format(' Filtering and rise-time not defined for set ',i3)
		idest=0
		RETURN
	   endif
c
4021	   continue
	   print 402,j
402	   format(/,' SET # ',i3,/,
     &' Risetime setting:',/,
     &' (1) Use nominal filter setting only',/,
     &' (2) Include effect of prefilter and tape on rise time too ',/,
     &' (3) Specify numerical value here',/,
     &' Option number [1] =  ')
	   i1=1
	   call INPUTi(i1)
	   if(i1.le.1) then
		trise(j)=332.1/fc(j)		!rise time (microsec)
	   else if(i1.eq.2) then
		print 405
405		format('& -3dB frequency for tape recorder (kHz) = ')
	      call INPUTr(fc1)
	      print 407
407	      format(' -3dB frequency for patch amplifier (kHz) = ')
	      call INPUTr(fc2)
	      fc(j)=1.0/sqrt(1./fc(j)**2 + 1./fc1**2 + 1./fc2**2)
403	      trise(j)=332.1/fc(j)		!rise time (microsec)
	   else if(i1.eq.3) then
	      print 4032
4032	      format('& Rise time (microseconds)= ')
	      call INPUTr(trise(j))
	      fc(j)=332.1/trise(j)	!corresponding fc (-3dB)
	   else
	      goto 4021
	   endif
c
	   print 4011,j,fc(j),trise(j)
	   if(discprt) write(8,4011) j,fc(j),trise(j)
4011	   format(' SET ',i3,
     &': Filter: fc(kHz,-3 dB)= ',f8.2,';  risetime (microsec)= ',
     & g13.6,/)
	   if(iscan.eq.-2001) print 227
227	   format(' Best resolution for 50% threhold = 1.5 rise-times')
	enddo		!end of j=1,nset
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

