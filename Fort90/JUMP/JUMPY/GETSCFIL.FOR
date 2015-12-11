	subroutine GETSCFIL(consfil,scantfil,nfile,kfile,scdatfil,
     & nlen,srate,readini,icfil,ncfil,idest)
c To locate all the files for a single channel analysis (c.f. GETCONSAM
c which asks only for the CONSAM file name)
c Call with readini=true if default names supplied in call
c (1) CONSAM.DAT
c (2) SCANDAT.DAT(s) on which experiment(s) summarized
c (3) SCANTEMP.DAT(s) corresponding to each expt
c (NB scdatfil() is like pfile() in diskin1, but diff size)
c
c Defaults are on the basis of recommended backup naming e.g
c Q:\scdat\dc\consmdc2.153
c Q:\scdat\dc\scantdc2.153
c Q:\scdat\dc\scantdat.dc2
c Default paths are input to this subroutine (e.g. kept in the .INI file
c for the program that calls GETSCFIL).  Once the CONSAM file has been
c correctly identified the .153 and the .dc2 can be used for defaults
c for the other files.
c
	character cdate*11,adctim*8,title*76		!read from consam
	character cnum*11
c	integer iexp1,iexp2			  !Number of 1st/last file on scandat.dat
	character ans*1,UC*1
c for scandat
	integer*4 jstrec(200),idiscn,nfiles
	character name*12
	integer kfile(20)
	character consfil*40			!path for consam.dat file
	character*40 scdatfil(20)	!path names for SCANDAT files (char*40 here)
	character*40 scantfil(20)  !path names for SCANTEMP files
	logical contig,onedisk,pathset
c for parsname
	character path*30,pname*8,suffix*3,ndev*2
	logical nopath

	logical readini
	logical present,pon,slock,discprt
c for read of consam header
	integer*2 idt,inchan,id1,id2,iver
	character cs*3
c
	SAVE srate1
c
	common/dp/discprt
c
	pon()=slock()
c
101	format(a1)
	idest=0
c
	if(icfil.gt.1) goto 1			!get next consam file
c
	if(readini) then
	   if(nfile.lt.0.or.nfile.gt.10) nfile=1	!in case .ini not set up yet
	   print 551,nfile
551	   format(' Last run used ',i3,' experiment files, viz.: ',/,
     &  ' #    file #        SCANDAT file                SCANTEMP file')
	   do i=1,nfile
	      print 55,i,kfile(i),scdatfil(i)(1:34),scantfil(i)(1:34)
55	      format(i2,2x,i4,2x,a34,2x,a34)
	   enddo
	   print 552,consfil
552	   format(/,' Raw data (CONSAM) file: ',a40,/)
	   call DCASK('Use same experiments again','y',ans)
	   if(ans.eq.'Y') then
		if(pon()) write(7,56)
		if(discprt) write(8,56)
56		format(' Data from ')
		if(pon()) write(7,552) consfil
		if(discprt) write(8,552) consfil
	      if(discprt) write(8,561)
561	      format(/,
     &  ' #    file #        SCANDAT file                SCANTEMP file')
		do i=1,nfile
	         if(pon()) write(7,55) i,kfile(i),scdatfil(i)(1:34),
     &		scantfil(i)(1:34)
	         if(discprt) write(8,55) i,kfile(i),scdatfil(i)(1:34),
     &		scantfil(i)(1:34)
		enddo
		RETURN
	   endif
	else if(.not.readini) then	!no default supplied in call
	   consfil='Q:\scdat\dc\consmdc2.153'
	endif
c
c Now the bit done when not using same expts as last time -get SCANDAT
c Get CONSAM first
	call DCASK('All raw data from one CONSAM file','y',ans)
	ncfil=1
	icfil=1
	if(ans.eq.'N') then
	   print 11
11	   format(' Number of CONSAM files to be used [1] = ')
	   call INPUTi(ncfil)
	endif
1	continue
	if(ncfil.eq.1) then
	   call TITENT0('CONSAM file name:',consfil,40,.false.)
	else
	   call INTCONV(icfil,cnum)
	   call TITENT0('CONSAM file name #'//charnb(cnum)//':',
     &	consfil,40,.false.)
	endif
	INQUIRE(file=consfil,exist=present,flen=len)
	if(.not.present.or.len.le.0) then
	   call BELL(1)
	   print 4,consfil
4	   format(1x,a40,' NOT FOUND: try again [Y] ? ')
	   read 101,ans
	   if(UC(ans).ne.'N') goto 1
	   goto 99
	else
	   OPEN(unit=14,file=consfil,status='UNKNOWN',
     &      access='TRANSPARENT')
c don't overwrite CDATE!
	   read(14,rec=1) title,cdate,adctim,idt,ioff,ilen,inchan,
     &   id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,iver
	   nlen=ilen/2		!number of data points in CONSAM
	   if(icfil.eq.1) then
		srate1=srate
	   else
		if(srate.ne.srate) then
		   call BELL(3)
		   print 13,srate,srate1
13		   format(' ALL CONSAM FILES MUST HAVE SAME SAMPLE RATE',/,
     &	  ' For this file rate = ',g13.6,' Hz; for 1st file rate = '
     &	   ,g13.6,' Hz')
		   goto 99
		endif
	   endif
	   print 6, consfil,title,cdate,adctim
6	   format(' CONSAM data from: ',a40,/,1x,a76,/,
     &	' Date ',a11,' time ',a8,':   O.K. [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
		call BELL(1)
		print 41,consfil
41		format(1x,a40,' NOT RIGHT: try again [Y] ? ')
		read 101,ans
		if(UC(ans).ne.'N') goto 1
		goto 99
	   endif
	   if(pon()) write(7,7) consfil,title,cdate,adctim
	   if(discprt) write(8,7) consfil,title,cdate,adctim
7	   format(' CONSAM data from: ',a40,/,1x,a76,/,
     &		' date ',a11,' time ',a8)
	endif
c
	call PARSNAME(consfil,path,ndev,pname,suffix,nopath,40)
	scdatfil(1)=charnb(path)//'scandat'//'.'//pname(6:8)
c
33	continue
	ans='Y'
	call DCASK('Data from (SCANDAT file)'//charnb(scdatfil(1)),
     &	ans,ans)
	pathset=ans.eq.'Y'
c	pathset=.true.
	nsc1=0
	nsc2=0
	cnum='           '
	cnum(1:3)=suffix
	call CHTOINT(cnum,nsc1)		!default expt number
	iopt=1
44	print 400,iopt
400	format(
     &' (1) Fitting results in a single experiment file from SCANDAT',/,
     &' (2) Pool several consecutive files from SCANDAT',/,
     &' (3) Specify non-consecutive experiment files from SCANDAT',/,
     &' Option number [',i2,'] = ')
	call INPUTi(iopt)
	if(iopt.eq.1) then
	   contig=.true.
	   print 45,nsc1
45	   format(' Use experiment files n1 [',i3,'] =  ')
	   call INPUTi(nsc1)
	   nsc2=nsc1
	else if(iopt.eq.2) then
	   contig=.true.
	   nsc2=nsc1+1
	   print 42,nsc1,nsc2
42	   format(' Pool contiguous files n1 to n2 [',i3,',',i3,'] = ')
	   call INPUT2i(nsc1,nsc2)
	else if(iopt.eq.3) then
	   contig=.false.
	   nsc1=0
	   print 52
52	   format(' Number of experiment-files to be pooled = ')
	   call INPUTi(nfile)
	else
	   goto 44
	endif
	if(nsc1.gt.0) then
	   contig=.true.
	   if(nsc2.eq.0) nsc2=nsc1
	   nfile=nsc2-nsc1+1
	   do i=1,nfile
		j=nsc1+i-1	!expt number
		kfile(i)=j
	   enddo
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
	if(onedisk) then
29	   if(.not.pathset) then	!read data file path/name here
		if(.not.readini) ndev='C:'
		print 43
43	     format(' Specify disk partition where data disk is located:')
		call DISCNUM1(id,ndev,iask,idef)
		if(id.eq.-1) goto 99
		scdatfil(1)=ndev//'\SCDAT\SCANDAT.DAT'
		call TITENT0('Data file (SCANDAT) name:',scdatfil(1),40,
     &	.false.)
	   endif
	   INQUIRE(file=scdatfil(1),exist=present,flen=nlen)
	   if(present.and.nlen.gt.0) then
	     OPEN(unit=15,file=scdatfil(1),status='OLD',
     &	access='TRANSPARENT')
c         NB idisc is integer*2 for old type files!
	      read(15,rec=1) name,idiscn,nfirst,nfiles,jstrec,nextrec
	      CLOSE(unit=15)
      	print 53,ndev,name,idiscn,nfirst,nfiles
53	      format(1x,a2,1x,a12,' disc # ',i2,
     &	 '  -contains expts ',i4,' to ',i4)
	      call DCASK(' Is this disc O.K.','y',ans)
		if(ans.eq.'N') goto 29
	      if(pon()) write(7,53) ndev,name,idiscn,nfirst,nfiles
      	if(discprt) write(8,53) ndev,name,idiscn,nfirst,nfiles
		do i=1,nfile
		   scdatfil(i)=scdatfil(1)		!set all paths
		enddo
		if(.not.contig) then
		   do i=1,nfile
	      	print 61
61	      	format(' Read experiment file no= ')
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
c        Get the scantemp for current scandat expt
		   call GETSCANT(i,kfile,consfil,scantfil,srate,idest)
		   if(idest.eq.99) goto 99
		enddo
	   else
	      print 32
32		format(' SCANDAT data disk not present: try again')
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
	         scdatfil(1)=ndev//'\SCDAT\SCANDAT.DAT'
		else
	         scdatfil(i)=scdatfil(i-1)
		endif
	      call TITENT0('Data file (SCANDAT) name:',scdatfil(i),40,
     &	.false.)
	      INQUIRE(file=scdatfil(i),exist=present,flen=nlen)
	      if(present.and.nlen.gt.0) then
		   OPEN(unit=15,file=scdatfil(i),status='OLD',
     &		access='TRANSPARENT')
	         read(15,rec=1) name,idiscn,nfirst,nfiles,jstrec,nextrec
		   CLOSE(unit=15)
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
c Check title etc of scandat file
		   iex=kfile(i)
		   call EXPTIT(iex,nfirst,jstrec)
		   print 12
12		   format(/,' Is this O.K. [Y] ? ')
		   ans='Y'
	   	   call INPUTa(ans)
		else
		   print 32
c32		   format(' Data file not present: try again')
		   goto 35
    		endif
c        Get the scantemp for current scandat expt
		call GETSCANT(i,kfile,consfil,scantfil,srate,idest)
		if(idest.eq.99) goto 99
	   enddo          !end of nfile loop
	endif       !end of onedisk/not one disk
c
c All expt numbers (kfile) and paths (scdatfil) now in
	CLOSE(UNIT=14)
	CLOSE(UNIT=15)
	CLOSE(UNIT=16)
c
	RETURN
99	idest=99
	CLOSE(UNIT=14)
	CLOSE(UNIT=15)
	CLOSE(UNIT=16)
	RETURN
	end

	subroutine EXPTIT(iex,nfirst,jstrec)
	integer*4 jstrec(200)
	character titlen*70,expdate*11,tapeID*24,expnum*9
	character*14 ptype(5)
	logical cjump
c
	logical pon,slock,discprt
c
	common/dp/discprt
c
	pon()=slock()
c
	ptype(1)='outside-out'	!definitions for NEWSCAN
	ptype(2)='inside-out'
	ptype(3)='cell-attached'
	ptype(4)='whole-cell'
	ptype(5)='simulated data'
c
	irec=jstrec(iex-nfirst+1)	!where to start reading header
c
	read(15,rec=irec) expnum,nfile,kt,iscanver		!read version
	if(iscanver.ne.-3001) then
	   read(15,rec=irec)expnum,nfile,kt,iscanver,expdate,
     &     titlen,tapeID,ipatch,Emem,temp,ffilt,avamp,rms,
     & 	      calfac2,ioffset,imin,imax,cjump,nsweep,tzero,tsamp
	else		!-3001=simulated data
	   read(15,rec=irec)expnum,nfile,kt,iscanver,expdate,
     &        titlen,tapeID,ipatch,Emem,temp,ffilt,avamp,rms,
     &        calfac2,ioffset,treso,tresg
	endif
	if(iscanver.lt.101) cjump=.false.	!not defined on disk
	amp=avamp*calfac2
	filt=1000.*ffilt	 !in Hz
	if(iscanver.eq.-1001) then
	   filt=-1.
	   rms=0.
	endif
	if(iscanver.ge.99) then
	   print 313,nfile,titlen,expdate,tapeID,
     &     ptype(ipatch),kt,Emem,temp,filt,amp,rms
c     &    ptype(ipatch),kt,Emem,temp,filt,amp,rms,calfac2
         if(pon()) write(7,313)nfile,titlen,expdate,tapeID,
     &    ptype(ipatch),kt,Emem,temp,filt,amp,rms
	   if(discprt) write(8,313)nfile,titlen,expdate,tapeID,
     &     ptype(ipatch),kt,Emem,temp,filt,amp,rms
313	   format(' Ex',i5,': ',a70,/,
     &     '   Expt date ',a11,': Tape ',a24,': Patch ',a14,/,
     &     1x,i6,' transitions: Membrane pot ',f8.2,
     &     'mV: Temperature ',f7.1,/,
     &     '   Filter (-3dB)',f9.1,'Hz : Full amp (app) ',f8.2,'pA',
     &     ': RMS noise ',f9.3,'pA.')
c     &    ' Calibration factor = ',/)
	   if(cjump) then
	      print 48,nsweep,tsamp*1.e-3,tzero*1.e-3
		if(pon()) write(7,48) nsweep,tsamp*1.e-3,
     &	  tzero*1.e-3
		if(discprt) write(8,48) nsweep,tsamp*1.e-3,
     &	  tzero*1.e-3
48		format('   CJUMP data:',i5,' jumps, sample length ',
     &	     i6,' ms, tzero = ',f9.2,' ms.')
	   endif
	   print 108
         if(pon()) write(7,108)
         if(discprt) write(8,108)
108	   format(/)
	else if(iscanver.eq.-3001) then	!converted simulations disk
	   to=1000.*treso
	   tg=1000.*tresg
	   print 320,nfile,titlen,expdate,kt,to,tg,filt,amp,rms
         if(pon()) write(7,320) nfile,titlen,expdate,kt,to,tg,
     &    filt,amp,rms
	   if(discprt) write(8,320)nfile,titlen,expdate,kt,to,tg,
     &    filt,amp,rms
320	    format(' Ex',i5,': ',a70,/,' Analysis date ',a11,
     &        ':  ',i6,' transitions. Simulated data; res = ',
     &         f7.1,',',f7.1,' microsec (open, shut)',/,
     &       ' Filter (-3dB)',f9.1,' Hz : Full amp (app) ',f8.2,'pA',
     &        ': RMS noise ',f9.3,'pA',/)
	else if(iscanver.lt.0) then	!converted old disk
	   print 319,nfile,titlen,expdate,kt,filt,amp,rms
         if(pon()) write(7,319) nfile,titlen,expdate,kt,filt,
     &    amp,rms
	   if(discprt) write(8,319)nfile,titlen,expdate,kt,filt,
     &      amp,rms
319	   format(' Ex',i5,': ',a70,/,' Analysis date ',a11,
     &          ':  ',i6,' transitions (data from PDP SCAN)',/,
     &        ' Filter (-3dB)',f9.1,' Hz : Full amp (app) ',f8.2,'pA',
     &          ': RMS noise ',f9.3,'pA',/)
	endif
c
	RETURN
	end


	subroutine GETSCANT(i,kfile,consfil,scantfil,srate,idest)
	integer kfile(20)
	character*40 consfil	!path name for CONSAM file
	character*40 scantfil(20)  !path names for SCANTEMP files
	character path*30,pname*8,suffix*3,ndev*2,UC*1,ans*1,cnum*11
	logical nopath
	logical present,pon,slock,discprt
c
	common/dp/discprt
c
	pon()=slock()
c
101	format(a1)
	idest=0
c
	call PARSNAME(consfil,path,ndev,pname,suffix,nopath,40)
	call INTCONV(kfile(i),cnum)
c Allow for whether leading zeroes are included in suffix
	if(suffix(1:2).eq.'00'.and.kfile(i).lt.9) then
	   suffix='00'//cnum(1:1)	!overide suffix with current expt no
	else if(suffix(1:1).eq.'0'.and.kfile(i).lt.99) then
	   suffix='0'//cnum(1:2)	!overide suffix with current expt no
	else
	   suffix=cnum(1:3)		!overide suffix with current expt no
	endif
	scantfil(i)=charnb(path)//'scant'//pname(6:8)//'.'//suffix
2	print 20,kfile(i)
20	format(' Expt. ',i3,':')
	call TITENT0('SCANTEMP file name:',scantfil(i),40,.false.)
	INQUIRE(file=scantfil(i),exist=present,flen=len)
	if(.not.present.or.len.le.0) then
	   call BELL(1)
	   print 4,scantfil(i)
4	   format(1x,a40,' NOT FOUND: try again [Y] ? ')
	   read 101,ans
	   if(UC(ans).ne.'N') goto 2
	   goto 99
	else
	   OPEN(unit=16,file=scantfil(i),status='UNKNOWN',
     &      access='TRANSPARENT')
c don't overwrite CDATE!
	   read(16,rec=1)
 	   read(unit=16,rec=1) iscanver,kt,nfits,ntmax,nfmax,ioffset,
     &    nbuf,novlap,srate1,finter
c   check these OK
	   if(srate1.ne.srate) then
		call BELL(1)
		print 43, scantfil(i),srate1,srate
43		format(
     &	 1x,a40,'sample rate = ',g10.2,' not same as CONSAM ',
     &	   ' ( = ',g10.2,'): try again [Y] ? ')
	      read 101,ans
		if(UC(ans).ne.'N') goto 2
		goto 99
	   endif
	   if(pon()) write(7,8) scantfil(i)
	   if(discprt) write(8,8) scantfil(i)
8    	   format(' SCANTEMP data from: ',a40)
	endif
	RETURN
99	idest=99
	RETURN
	end

