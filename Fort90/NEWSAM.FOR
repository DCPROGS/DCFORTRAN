	program NEWSAM
c Program which takes CONSAM.DAT as input, and creates output file which
c is modified in any/all of the following ways.
c (1) Particular start and end points specified in CONSAM
c (2) Filtered by GAUSSIAN filter
c (3) Every nth point omitted
c
c In CONSAM, the first data point, idata(1) starts at byte (record #) ioff+1,
c idata(2) at ioff+3, idata(3) at ioff+5 -so point #in (in whole data) starts
c at byte # ioff+(2*in-1)
c	irec=ioff + (2*jn1-1)    !record # for 1st value to be read
c
c path in pfile(i); eg pfile(i)='H:\dcdata\scandat.da1'
c	character*33 pfile(50)	!path names for SCANDAT files
	character*33 pfile1,pfile2	!path names for input/output files
	character ndev*2,ans*1,UC*1
	character filnam*32
	character*11 cdate,ctime
	character*40 mtitle	!machine name
	logical pon,slock,debug,caplock,present
	logical discprt
c for CONSAM header
	integer*2 idt,inchan,id1,id2,iver
	character cs*3,adctime*8
	character*76 title
c data files (overlap=100 points at each end)
	integer*2 idatin(-99:100100),idatout(-99:100100)
c===test
c	integer*2 idatin(-19:120),idatout(-19:120)	!===test
	integer*2 itest(1000)				!===test
c
	common/dp/discprt
	common/dpp/filnam,machine,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
	nbuf=100000		!size of data arrays
	novlap=100		!size of overlap
c	nbuf=100		!size of data arrays ===test
c	novlap=20		!size of overlap     ===test
c
	filnam='NEWSAM.PRT'
	OPEN(unit=7,file='PRN',iostat=nerr)
	call WINPRINT	!print file control
	print 1
	if(pon()) write(7,1)
	if(discprt) write(8,1)
1	FORMAT(' NEWSAM: Filter and/or modify CONSAM.DAT file',/)
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
     & ' CAPS LOCK on for debugging',//)
c
	if(ndisc.gt.1) then
	   print 4
4	   format(' Specify disk partition for input CONSAM:')
	   iask=-1
	   idef=0
	   call DISCNUM1(id,ndev,iask,idef)
	   if(id.eq.-1) STOP
	else
	   ndev='C:'
	endif
	pfile1=ndev//'\CONSAM.DAT'
	if(debug()) pfile1=ndev//'\test.dat'
	call TITENT0('Input CONSAM file name:',pfile1,33,.false.)
31	continue
c	INQUIRE(file=pfile1,exist=present,flen=nlen,err=701)
	INQUIRE(file=pfile1,exist=present,flen=nlen)
	if(present.and.nlen.gt.0) then
        OPEN(unit=14,file=PFILE1,status='UNKNOWN',access='TRANSPARENT')
	  read(14,rec=1) title,cdate,adctime,idt,ioff,ilen,inchan,
     &   id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,iver
c	  ffilt=ffilt/1000.		!convert to kHz -keep in Hz!
c	  ffilt1=ffilt1/1000.		!convert to kHz
c     & id1,id2,cs(1:3),calfac,srate,filt,calfac1,iver
c	CLOSE(unit=14)			!leave open throughout
c	  pAV=calfac*6553.6			!pA/volt (display now scaled in volts)
	  nint=ilen/2	!ilen=bytes; nint=number of integers
        print 35, ndev,cdate,adctime,ffilt,title,srate,nint
        if(pon()) write(7,35) ndev,cdate,adctime,ffilt,title,srate,nint
        if(discprt)write(8,35) ndev,cdate,adctime,ffilt,title,srate,nint
35	  format(' Disk ',a2,' Date= ',a11,' time= ',a8,
     & ' filter (Hz) = ',f10.2,/,1x,a76,/,
     & ' sample rate (Hz) = ',f10.1,'  length (integers) = ',i10,/)
	else
	   print 30,pfile1
30	   format(' CAN NOT FIND ',a33)
	   call TITENT0('Input CONSAM file name:',pfile1,33,.false.)
	   goto 31
	endif
c
c Get name and path for output file
	if(ndisc.gt.1) then
	   print 44
44	   format(' Specify disk partition for OUTPUT file:')
	   iask=-1
	   idef=1
	   call DISCNUM1(id,ndev,iask,idef)
	   if(id.eq.-1) STOP
	else
	   ndev='C:'
	endif
	pfile2=ndev//'\CONSAM1.DAT'
	if(debug()) pfile2=ndev//'\test1.dat'
	call TITENT0('OUTPUT file name:',pfile2,33,.false.)
      OPEN(unit=15,file=PFILE2,status='UNKNOWN',access='TRANSPARENT')
c
	if(debug()) then
c NB test.dat has 1000 values starting at rec=ioff+1=513 (see TDISK),
c so read all of them in here into itest()
	  print 72
	  if(discprt)write(8,72)
72	  format(' INPUT DISK FILE')
        OPEN(unit=16,file=PFILE1,status='UNKNOWN',access='TRANSPARENT')
	   irec=ioff+1
	   N=1000
	   read(16,rec=irec) (itest(i),i=1,N)
	   CLOSE(unit=16)
	   print 70
70	   format(' List values n1 to n2 from INPUT file: n1,n2 = ')
	   call INPUT2i(n1,n2)
	   if(n1.gt.0) then
	    do i=n1,n2
		print 71,i,itest(i)
	      if(discprt)write(8,71) i,itest(i)
71		format(i6,3x,i8)
	    enddo
	   endif
	endif
c
	i1=1		!first and last data value (default)
	i2=nint
c
	print 42,1,nint
42	format(
     & ' First and last data points to be used; i1, i2 [',
     & i2,',',i8,'] = ')
	call INPUT2i(i1,i2)
      if(pon()) write(7,46) i1,i2
      if(discprt)write(8,46) i1,i2
46	format(' Points ',i9,' to ',i9,' selected for output')
c	 Calculate number of sections needed for data
	ndatin=i2-i1+1
	nsec=1 + (ndatin-1)/nbuf
	nlast=ndatin-(nsec-1)*nbuf	!points in last section
	if(nlast.eq.0) nsec=nsec-1
c
	iopt=2
	print 40,iopt
40	format(
     & ' (1) Write subsection of CONSAM file to disk',/,
     & ' (2) Filter CONSAM with Gaussian filter, and write to disk',/,
     & ' (3) Omit points from output file, and write to disk',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(iopt)
	select case(iopt)
	   case(1)
		noutt=0		!total # of points written to output
c         Now read consam(i1) to consam(i2), and write back with
c		points omitted, to output file, consam1(1) to consam1(noutt)
c   Go through sections
 		irec=ioff + (2*i1-1)  !initialise record # for 1st value to be read
 		irec1=ioff + 1  !initialise record # for 1st value to be written
		do isec=1,nsec
c            Read input data, consam(i1) to consam(i2).  Section #isec
c		in CONSAM starts (excluding overlaps) at consam(j) where
c		j=(isec-1)*nbuf+i1 (=i1 for isec=1), and ends at j=j1+nbuf-1
c		NB read data into idatin(1) to idatin(nbuf) for every section
c		The start value, i1, affects only the record # where the
c		reading starts
		   nvals=nbuf
		   if(isec.eq.nsec) nvals=nlast
		   read(14,rec=irec) (idatin(i),i=1,nvals)
		   irec=irec+2*nvals	!1st record to read in next loop
c Now write output: same, but start at 'consam(1)', i.e. start writing
c at irec=ioff+1
		   write(15,rec=irec1) (idatin(j),j=1,nvals)
		   irec1=irec1+2*nvals	!1st record to read in next loop
		   noutt=noutt+nvals
		enddo		!end of isec loop
		CLOSE(unit=14)
c		CLOSE(unit=15)	!leave open to write header at end
c Print what was done
		ilen=2*noutt	!changed in header (bytes)
		write(15,rec=1) title,cdate,adctime,idt,ioff,ilen,inchan,
     &	   id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,iver
	     	print 51, pfile2,noutt,srate1
	     	if(pon()) write(7,51) pfile2,noutt,srate1
     		if(discprt)write(8,51) pfile2,noutt,srate1
c51		format(' OUTPUT file name = ',a33,/,
c     & 1x,i9,' points written to output: sample rate = ',f10.3,' Hz')
		CLOSE(unit=15)
c
	   case(2)
		idelt=1		!default
		noutt=0		!total # of points written to output
		irec1=ioff+1	!used	when idelt>0
c
		print 43
43		format(' -3 dB frequency for filter (Hz) = ')
		call INPUTr(fc)
		fc0=fc
		fc1=1./sqrt((1.0/(fc*fc)) - (1.0/(ffilt*ffilt)))
		fc2=1./sqrt((1.0/(fc*fc)) + (1.0/(ffilt*ffilt)))
		i=1
		print 45,ffilt,fc1,fc,fc,fc2
45		format(
     &	' Data already filtered at ',g13.6,' Hz.',/,
     &  '  (1) Filter at fc = ',g13.6,' Hz to give final fc = ',g13.6,/,
     &  '  (2) Filter at fc = ',g13.6,' (overall fc = ',g13.6,')',/,
     &  ' Option number [1] = ')
		call INPUTi(i)
		if(i.eq.1) then
		   fc=fc1		!used for GFILTER
		   fc3=fc0		!final fc
		else
		   fc=fc0		!used for GFILTER
		   fc3=fc2		!final fc
		endif
c Reduce 'sample rate' for output? If srate>10fc then recommend largest
c reduction that keeps srate at least 10fc. First reduction possible
c (without interpolation) is by factor of 2, so try only if srate>20fc
		r=srate/fc3
		srate1=srate		!value for output
		if(r.ge.20.) then
		   do i=2,100
			srate1=srate/float(i)
			if(srate1/fc3.lt.10.) then
			   idelt=i-1
			   goto 50
			endif
		   enddo
50		   continue
		   srate1=srate/float(idelt)
		   r1=srate1/fc3
		   print 49,r,idelt,srate1,r1,idelt
49		   format(' Sample rate is ',f7.2,' times final fc:',/,
     &' Recommend reduction the sample rate for output file by using',/,
     &' only every ',i3,'th point to give sample rate of ',f9.2,' Hz',/,
     &' which is ',f9.2,' times fc.',/,
     &'  Keep every idelt''th point: idelt [',i2,'] = ')
		   call INPUTi(idelt)
		   srate1=srate/float(idelt)
		endif
c
c	 Calculate number of sections needed for data
		ndatin=i2-i1+1
		nsec=1 + (ndatin-1)/nbuf
		nlast=ndatin-(nsec-1)*nbuf	!points in last section
		if(nlast.eq.0) nsec=nsec-1
c Go through sections
c Initialise record # to start reading/writing the data on the first time
c round the loop (the value for irec here does not include overlap which is
c incorporated later).  After each read/write the values of irec, irec1 are
c updated ready for next loop.
c NB may be able to use prelim overlap in the first section if i1>1 chosen, so
c adjust irec so we read CONSAM from input at a point that includes points
c before consam(i1), if any.
c Calculate values here for first section:
 		irec1=ioff + 1  !initialise record # for 1st value to be written
		i11=1		!init value for element of idatout() to be
c				!written to disk
c
c=		irec=ioff + (2*i1-1)  !initialise record # for 1st value to be read
		iov1=i1-1		!number of points before #i1
		if(iov1.gt.novlap) iov1=novlap  !number of points for prelim overlap
		j1=i1-iov1
		irec=ioff + (2*j1-1)  !initialise record # for 1st value to be read
c=		j2=j2+novlap
c=		nread=j2-j1+1
		nread=iov1+nbuf+novlap	!check does not go past end?!
		i0=(1-iov1)			!index of 1st point read into idatin()
		in=i0+nread-1		!index of last point read into idatin()
c Also calc values here, needed later, for last section (may have final
c overlap section if i2<nint).
		iovn=nint-i2		!number of points after i2
		if(iovn.gt.novlap) iovn=novlap  !number of points for final overlap
c
c  Need different values if nsec=1 so first section is also last, so calc here
		if(nsec.eq.1) then
		   nread=iov1+(i2-i1+1)+iovn
		   in=i0+nread-1
		endif
c
		do isec=1,nsec
c
		   print 60,isec
60		   format(' Reading section ',i3,' . . .')
		   read(14,rec=irec) (idatin(i),i=i0,in)
c
c  If there is not a full overlap in first and last sections,
c   then fill in the overlap using first/last data points.
		   if(isec.eq.1.and.iov1.lt.novlap) then
			do m=1-novlap,i0-1
			   idatin(m)=idatin(i0)
			enddo
		   endif
		   if(isec.eq.nsec.and.iovn.lt.novlap) then
			do m=in+1,nlast+novlap
			   idatin(m)=idatin(in)
			enddo
		   endif
c
c  Now filter this section: number of points in idatin() is now npnt:
		   if(isec.lt.nsec) then
			npnt=nbuf+2*novlap
		   else
			npnt=nlast+2*novlap
		   endif
		   print 61,isec
61		   format(' Filtering section ',i3,' . . .')
		   call GFILTER(idatin,idatout,npnt,srate,fc)
c
c Now set values for read on next cycle now (used only if nsec>1)
c -every subsequent read starts 2*novlap points before the last one
c ended, or nbuf-novlap points after the last one started
c More simply, every read after the first starts novlap points
c before boundary at end of current section, i.e. at consam(i), i=isec*nbuf+i1
c
		   if(isec.lt.nsec) then    !if isec=nsec there is NO next section
			irec=irec + 2*(nread-2*novlap)
c===following gives same value in tests (keep as check for now!)
			j=isec*nbuf+i1-novlap
			irec0=ioff + (2*j-1)
			if(irec0.ne.irec) then
			   call BELL(2)
			   print 75,isec,irec0,irec1
75			   format(' ERROR: isec, irec0, irec = ',i5,2i9)
			endif
c==NO			irec=irec+2*(nbuf-novlap)		!next record # to be read
			i0=1-novlap
			if(isec.eq.nsec-1) then	!next section is last
			   nread=novlap + nlast + iovn
			else 			!next section not the last
			   nread=novlap + nbuf + novlap
			endif
			in=i0+nread-1
		   endif
c Finally write the relevant part of idatout() to the output file,
c -  exclude overlap parts (and omit points if idelt>1)
c
		   print 62,isec
62		   format(' Writing section ',i3,' . . .')
		   if(isec.lt.nsec) then
		      nvals=nbuf
		   else
		      nvals=nlast
		   endif
		   write(15,rec=irec1) (idatout(i),i=i11,nvals,idelt)
c
c number of values written =
		   nout=ifix(1. + float(nvals-i11)/float(idelt))
		   irec1=irec1+2*nout	!1st record to read in next loop
c index of last value
		   ilast=i11+(nout-1)*idelt
		   noutt=noutt+nout	!total points written
		   i11=ilast+idelt-nbuf	!index of 1st point to write in next section
		enddo		!end of isec loop
		CLOSE(unit=14)
c		CLOSE(unit=15)	!leav open to write header at end
c Print what was done
	     	print 48, fc,fc3
	     	if(pon()) write(7,48)  fc,fc3
	     	if(discprt)write(8,48) fc,fc3
48		format(
     &	 ' Filtered at ',f9.2,' Hz to give final fc = ',f9.2,' Hz')
c    Write the header for output file; same as input except for filter freq,
c    ilen and sample rate (last 2 reduced if idelt>1)
		srate=srate1	!put new sample rate in header
		ffilt=fc3		!put final fc into header
		ilen=noutt*2	!and new length (bytes)
		write(15,rec=1) title,cdate,adctime,idt,ioff,ilen,inchan,
     &	   id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,iver
	     	print 51, pfile2,noutt,srate1
	     	if(pon()) write(7,51) pfile2,noutt,srate1
     		if(discprt)write(8,51) pfile2,noutt,srate1
51		format(' OUTPUT file name = ',a33,/,
     & 1x,i9,' points written to output: sample rate = ',f10.3,' Hz')
		CLOSE(unit=15)
c          End of Gaussian filtering

	   case(3)        !omit points only
54		idelt=1
		noutt=0		!total # of points written to output
		srate1=srate
		r=srate/ffilt
		print 52,r
52		format(
     &	' Sample rate is now ',f7.2,' times final fc.',/,
     &	' Omit every idelt''th point: idelt [1] = ')
		call INPUTi(idelt)
		srate1=srate/float(idelt)
		r1=srate1/ffilt
		print 53,idelt,srate1,r1
53		format(
     &	' Omit every ',i3,'th point',/,
     &	'  sample rate = ',f10.3,' Hz = ',f9.3,' times fc',/,
     &	'  O.K [Y] ? ')
		read 101,ans
		if(UC(ans).eq.'N') goto 54
c         Now read consam(i1) to consam(i2), and write back with
c		points omitted, to output file, consam1(1) to consam1(noutt)
c	 Calculate number of sections needed for data
		ndatin=i2-i1+1
		nsec=1 + (ndatin-1)/nbuf
		nlast=ndatin-(nsec-1)*nbuf	!points in last section
		if(nlast.eq.0) nsec=nsec-1
c
c             Now calculate record number in CONSAM to read consam(i1)...
c		NB i1=absolute index in CONSAM of first value
 		irec=ioff + (2*i1-1)  !initialise record # for 1st value to be read
 		irec1=ioff + 1  !initialise record # for 1st value to be written
		i11=1		!initialise
c   Go through sections
		do isec=1,nsec
c            Read input data, consam(i1) to consam(i2).  Section #isec
c		in CONSAM starts (excluding overlaps) at consam(j) where
c		j=(isec-1)*nbuf+i1 (=i1 for isec=1), and ends at j=j1+nbuf-1
c		NB read data into idatin(1) to idatin(nbuf) for every section
c		The start value, i1, affects only the record # where the
c		reading starts
		   if(isec.lt.nsec) then
		      nvals=nbuf
		   else
		      nvals=nlast
		   endif
		   read(14,rec=irec) (idatin(i),i=1,nvals)
		   irec=irec+2*nvals	!1st record to read in next loop
c Now write output
		   write(15,rec=irec1) (idatin(i),i=i11,nvals,idelt)
c number of values written =
		   nout=ifix(1. + float(nvals-i11)/float(idelt))
		   irec1=irec1+2*nout	!1st record to read in next loop
c index of last value
		   ilast=i11+(nout-1)*idelt
		   noutt=noutt+nout	!total points written
		   i11=ilast+idelt-nbuf	!index of 1st point to write in next section
		enddo		!end of isec loop
		CLOSE(unit=14)
c		CLOSE(unit=15)	!leave open to write header at end
c Print what was done
c    Write the header for output file; same as input except for filter freq,
c    ilen and sample rate (last 2 reduced if idelt>1)
		srate=srate1
		ilen=noutt*2	!bytes
		write(15,rec=1) title,cdate,adctime,idt,ioff,ilen,inchan,
     &	   id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,iver
	     	print 55, pfile2,idelt,noutt,srate
	     	if(pon()) write(7,55) pfile2,idelt,noutt,srate
     		if(discprt)write(8,55) pfile2,idelt,noutt,srate
55		format(' OUTPUT file name = ',a33,/,
     &  '  Every ',i3,'''th point omitted from output',/,
     &  1x,i9,' points written to output: sample rate = ',f10.3,' Hz')
     		CLOSE(unit=15)
c          End of decimation
c
	end select
	if(debug()) then
c NB test1.dat has up to 1000 values starting at rec=ioff+1=513 (see TDISK),
c so read all of them in here into itest()
	   do i=1,1000
		itest(i)=0
	   enddo
	   print 73
	   if(discprt)write(8,73)
73	   format(' OUTPUT DISK FILE')
         OPEN(unit=16,file=PFILE2,status='UNKNOWN',access='TRANSPARENT')
	   irec=ioff+1
	   read(16,rec=irec) (itest(i),i=1,noutt)
	   CLOSE(unit=16)
	   print 74
74	   format(' List values n1 to n2 from OUTPUT file: n1,n2 = ')
	   call INPUT2i(n1,n2)
	   do i=n1,n2
		print 71,i,itest(i)
	      if(discprt)write(8,71) i,itest(i)
c71		format(i6,3x,i8)
	   enddo
	endif
	call ENDPRINT
c
	end

