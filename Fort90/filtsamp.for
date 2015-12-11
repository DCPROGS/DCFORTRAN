	program FILTSAMP
c Program which takes CONSAM.DAT as input, and creates output file which
c is modified in any/all of the following ways.
c (1) Particular start and end points specified in CONSAM
c (2) Filtered by GAUSSIAN filter
c (3) Every nth point omitted
c
c Modified 01/13/03 08:39am Make all file name/path arrays *40, except adcfil*30
c which is needed as argument for READAXON) and ASCFIL*33 needed for call to
c ACSREAD3/4.
c Code for plain integer*2 binary data file altered so that in this case
c the file is converted to consam format before filtering -this avoids
c problems when file is used in SCAN
c
c Modified 04/30/98 09:09pm to have option to read an ascii file (for Eisenberg
c data).  At present it works only if the number of points in the ASCII file
c is small enough to be read in all at once
c
c In CONSAM, the first data point, idata(1) starts at byte (record #) ioff+1,
c idata(2) at ioff+3, idata(3) at ioff+5 -so point #in (in whole data) starts
c at byte # ioff+(2*in-1)
c	irec=ioff + (2*jn1-1)    !record # for 1st value to be read
c
c path in pfile(i); eg pfile(i)='H:\dcdata\scandat.da1'
c	character*40 pfile(50)	!path names for SCANDAT files
	character*40 pfile1,pfile2,pfile3	!path names for input/output files
	character ans*1,UC*1
c	character ndev*2
	character*11 cdate,ctime
	logical debug,caplock,present,setgain,ascii
	logical consm,axon,invert,errflag,integ
	logical discprt
c for CONSAM header
	integer*2 idt,inchan,id1,id2,iver
	character cs*3,adctime*8
c	character*76 title
	character tapeID*24
	character*14 ptype(5)
	character expdate*11,defname*6,title1*76,title*70
c For axon files
	character adcfil*30
	logical good
c For ascii read
	character*20 colhead(1)		!for ASCREAD3
	integer nrow(1)
	allocatable::Xnum			!for ASCREAD4
	real*4 Xnum(:,:)
	character*40 ascfil
	character*70 text       !for ascread3
	logical dcfile
c data files (overlap=100 points at each end)
	ALLOCATABLE::idatin,idatout
	integer*2 idatin(:),idatout(:)
c	integer*2 idatin(-99:100100),idatout(-99:100100)
	integer*2 imean,imax,imin,idat
c===test
c	integer*2 idatin(-19:120),idatout(-19:120)	!===test
c===	integer*2 itest(1000)				!===test
c
	common/dp/discprt
	character*40 mtitle*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c
	debug()=caplock()
c
101	format(a1)
	errflag=.true.
	call UNDFL(errflag)
c
	filnam='FILTSAMP.PRT'
	call WINPRINT	!print file control
      OPEN(unit=7,file=prtport,iostat=nerr)		!open printer
	print 1
	if(discprt) write(8,1)
1	FORMAT(' FILTSAMP: Filter and/or modify CONSAM.SSD file',/)
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 2,cdate,ctime(1:8),mtitle
	if(discprt) write(8,2) cdate,ctime(1:8),mtitle
2	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 3
3	format(
     & ' SCROLL LOCK on for printing'/
     & ' CAPS LOCK on for debugging',//)
c
511	continue
	ptype(1)='outside-out'
	ptype(2)='inside-out'
	ptype(3)='cell-attached'
	ptype(4)='whole-cell'
	ptype(5)='simulated data'

	print 76
76	format(
     & ' (1) Read standard CONSAM file',/,
     & ' (2) Read AXON file (.ABF)',/,
     & ' (3) Read data from ASCII file but output as CONSAM',/,
     & ' (4) Data from any 16 bit integer binary file',/,
     & ' Option number [1] = ')
	i=1
	call INPUTi(i)
	consm=i.eq.1
	axon=i.eq.2
	ascii=i.eq.3
	integ=i.eq.4
c
	nbuf=1000000		!size of data arrays
	novlap=1000		!size of overlap
c	nbuf=100		!size of data arrays ===test
c	novlap=20		!size of overlap     ===test
	print 63,nbuf
63	format(' Number of points in buffer [',i8,'] = ')
	call INPUTi(nbuf)
	print 64,novlap
64	format(' Number of points in overlap [',i8,'] = ')
	call INPUTi(novlap)
c
c Initial default input file names
	if(ascii) then
	   pfile1='ascii.dat'
	else if(consm) then
	   pfile1='CONSAM.SSD'
	endif
c
	if(ascii) then
	   print 151
151	   format(/,
     &' ASCII file must have ONE column, and in this version must be',/,
     &' small enough to be read in all at once.')
	else if(.not.ascii) then
	   ALLOCATE(idatin(-(novlap-1):nbuf+novlap))
	   ALLOCATE(idatout(-(novlap-1):nbuf+novlap))
	endif
c
71	if(ascii) then
	   print 41
41	   format(' Specify disk partition for input ASCII file:')
c	   iask=-1
c	   idef=0
c	   call DISCNUM1(id,ndev,iask,idef)
c	   if(id.eq.-1) STOP
c file name is also requested in ascread3
	   call TITENT0(
     &	'Name & path for INPUT (ascii file):',ascfil,33,.false.)
	   pfile1=ascfil(1:33)
	   pfile1(34:40)='       '
	else if(axon) then
c	   print 411
c411	   format(' Specify disk partition for input ABF file:')
c	   iask=-1
c	   idef=0
c	   call DISCNUM1(id,ndev,iask,idef)
c	   if(id.eq.-1) STOP
	   adcfil='AXON.ABF'
	   call TITENT0(
     &   'Name & path for INPUT (Axon format) file:',adcfil,30,.false.)
	   pfile1(1:30)=adcfil
	   pfile1(31:40)='          '
	else if(consm) then
c	   print 4
c4	   format(' Specify disk partition for input CONSAM:')
c	   iask=-1
c	   idef=0
c	   call DISCNUM1(id,ndev,iask,idef)
c	   if(id.eq.-1) STOP
	   call TITENT0(
     &  'Name & path for INPUT (consam format) file:',pfile1,40,.false.)
	else if(integ) then
c The first data point, idata(1) starts at byte (record #) ioff+1, idata(2) at
c ioff+3, idata(3) at ioff+5 -so point #in (in whole data) starts at
c byte # ioff+(2*in-1)
	   print 70
70	   format(
     &'  The binary integer file will be converted to CONSAM format.',/,
     &'   Next give the information for the header to be added to  ',/,
     &'   the integer data.                                        ')
	   pause
c70	   call TITENT0(
c     & 'Name & path for INPUT (binary integer) file:',pfile1,40,.false.)
	   call WRITCSAM(pfile1,pfile3,cdate,ctime)
	   print 701,pfile1,pfile3
	   if(discprt) write(8,701) pfile1,pfile3
701	   format(/,
     &    ' The 16 bit integer file: ',a40,/,
     &    ' has been converted to a consam format file: ',/,
     &    4x,a40,/,
     &    ' which will now be used as input for filtering.')
	   pause
	   pfile1=pfile3	 !output consam file name becomes new default input
	   integ=.false.
	   consm=.true.
	   goto 71
c	   i1=1
c	   print 160,i1
c160	   format(
c     &	' Byte # for start of data (=header length+1) [',i4,'] = ')
c	   call INPUTi(i1)
c	   ioff=i1-1
c	   INQUIRE(file=pfile1,exist=present,flen=ilen)
c	   if(.not.present.or.ilen.eq.0) then
c		print 30,pfile1
cc30		format(' CAN NOT FIND ',a40 )
c		goto 70
c	   endif
c	   nint=ilen/2
c	   print 162
c162	   format(
c     &   ' Calibration factor (pA per ADC unit) = ')
c	   call INPUTr(calfac)
c	   print 531
c531	   format('&Sample rate (Hz) = ')
c	   call INPUTr(srate)
c	   print 3
cc3	   format(' Low pass filter (Hz, -3dB) = ')
c	   call INPUTr(ffilt)
c	   print 633,ilen,pfile1,calfac,srate
c	   if(discprt) write(8,633) ilen,pfile1,calfac,srate
c633	   format(
c     &   ' Generic data file has ',i8,' bytes: name = ',a40,/,
c     &   ' gain (pA per ADC unit) = ',g13.6,
c     &	':   sample rate (Hz) = ',g13.6)
cc	   nchan=1
c	   ffilt=ffilt/1000.		!convert to kHz
c         OPEN(unit=14,file=pfile1,status='UNKNOWN',access='TRANSPARENT')
	endif
c
31	continue
c	INQUIRE(file=pfile1,exist=present,flen=nlen,err=701)
	INQUIRE(file=pfile1,exist=present,flen=nlen)
c
	if(ascii) then
	 if(present.and.nlen.gt.0) then
	   ioff2=-1
	   ilinhead=-1
c       Get filename, number of rows and cols
	   ncols=1		!=declared size of colhead and nrow
	   call ASCREAD3(ioff2,ilinhead,ncols,nrow,nrowmax,len,
     &    ascfil,colhead,text,lentext,dcfile)	!call with nrow=array
	   pfile1=ascfil(1:33)
	   pfile1(34:40)='       '
c	   nrows=nrowmax	!for below
	   nd1=nrowmax
	   nd2=ncols
	   ALLOCATE(xnum(nd1,nd2))
	   print 77
77	   format(' Converting ascii to integers . . .')
	   call ASCREAD4(ioff2,ncols,nrow,len,
     & 	ascfil,xnum,nd1,nd2)		!call with nrow=array
	   if(nrow(1).gt.nbuf) then
		nbuf=nrow(1)
	   endif
	   ndat=nrow(1)
	   nint=ndat
	   nsec=1		!all read in at once in this version
	   s=0.
	   xmin=1.e37
	   xmax=-1.e37
	   do i=1,ndat
		x=xnum(i,1)
		s=s+x
		if(x.lt.xmin) xmin=x
		if(x.gt.xmax) xmax=x
	   enddo
	   xmean=s/float(ndat)
	   print 65,xmean,xmin,xmax
65	   format(' Mean = ',g13.6,': range = ',g13.6,' to ',g13.6)
	   print 67
c Scaling: 1401 units are -32768 t0 +32752 in 16 unit increments
c range=65536, quarter range 16384, three-quarter range=49152
67	   format(' Calibration factor (pA per ADC unit) = ')
	   call INPUTr(calfac)
	   ifac=49152	!for eisenberg data!
	   print 66,ifac
66	   format(
     &' Factor to multiply by before conversion to integer [',i6,'] = ')
	   call INPUTi(ifac)
	   fac=float(ifac)
	   calfac=calfac/fac
	   ALLOCATE(idatin(-(novlap-1):nbuf+novlap))
c Add offset of 16384
	   do i=1,ndat
		idatin(i)=-int2(ifixr((xnum(i,1)-xmean)*fac))+16385
	   enddo
	   DEALLOCATE(xnum)
	   ALLOCATE(idatout(-(novlap-1):nbuf+novlap))
c	   if(ncols.gt.1) then
c		ny=nrow(2)
c	   endif
	   if(dcfile) then
		if(lentext.gt.0) then
		   print 213,text
213		   format(' Title: ',a70)
		endif
	   endif
c Print file name
         print 27,ascfil
         if(discprt) write(8,27) ascfil
27	   format(' Data from ASCII file: ',a33)
	   print 68
68	   format(' Sample rate for input file (Hz) = ')
	   call INPUTr(srate)
	   print 69
69	   format(' Filter frequency for input file (-3 dB, Hz) = ')
	   call INPUTr(ffilt)
	   print 81
81	   format(' Specify title for OUTPUT (consam format) file')
	   call TITENT0('Title:',title,70,.false.)
	   inchan=1
	   ioff=512	!as in consam
	   adctime=ctime(1:8)
	   idt=0
	   id1=0
	   id2=0
	   cs(1:3)='H  '
	   ffilt1=0.0
	   calfac1=-1.0
	   ilen=ndat*2	!bytes
	   call GETEXPT(expdate,defname,tapeID,ipatch,
     &	  npatch,Emem,temper)
c and print details of patch
	   print 28,adcfil,expdate,npatch,tapeID,ptype(ipatch),Emem,temper
         if(discprt) write(8,28)
     &     adcfil,expdate,npatch,tapeID,ptype(ipatch),Emem,temper
	   goto 80
	 else		!file not found
	   print 30,pfile1
c30	   format(' CAN NOT FIND ',a33)
	   call TITENT0('Input ASCII file name:',ascfil,33,.true.)
	   pfile1=ascfil(1:33)
	   pfile1(34:40)='       '
	   goto 31
	 endif
	endif		!end of 'if axon'
c Now CONSAM
	if(present.and.nlen.gt.0) then
	 if(consm) then
        OPEN(unit=14,file=PFILE1,status='UNKNOWN',access='TRANSPARENT')
	  read(14,rec=1) iver
	  if(iver.eq.1002) then	!already have expdate etc
	     read(14,rec=1)iver,title,cdate,adctime,idt,ioff,ilen,
     &	 inchan,id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,
     &	 expdate,defname,tapeID,ipatch,npatch,Emem,temper
	  else
	     read(14,rec=1) title1,cdate,adctime,idt,ioff,ilen,inchan,
     &        id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,iver
	     title=title1(1:70)
	     if(iver.ne.1001) then
		  call BELL(2)
		  print 311
311		  format(' ERROR IN CONSAM VERSION NUMBER')
	     endif
	     call GETEXPT(expdate,defname,tapeID,ipatch,
     &	  npatch,Emem,temper)
	     iver=1002		!write header back to CONSAM with new format
	     write(14,rec=1)iver,title,cdate,adctime,idt,ioff,ilen,
     &	 inchan,id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,
     &	 expdate,defname,tapeID,ipatch,npatch,Emem,temper
	  endif
	  nint=ilen/2	!ilen=bytes; nint=number of integers
     	  print 58, pfile1
	  if(discprt)write(8,58) pfile1
58	  format(' INPUT file name = ',a40)
        print 35, cdate,adctime,ffilt,title,srate,nint
        if(discprt)write(8,35) cdate,adctime,ffilt,title,srate,nint
35	  format(' Date= ',a11,' time= ',a8,
     & ' filter (Hz) = ',f10.2,/,1x,a76,/,
     & ' sample rate (Hz) = ',f10.1,'  length (integers) = ',i10,/)
c and print details of patch
        print 271
        if(discprt) write(8,271)
271	  format(' Input data from CONSAM format file: ')
	  print 28,adcfil,expdate,npatch,tapeID,ptype(ipatch),Emem,temper
        if(discprt) write(8,28)
     & adcfil,expdate,npatch,tapeID,ptype(ipatch),Emem,temper
28	  format(/,
     & ' INPUT: continuous data file name: ',a30,/,
     & '  Date of experiment: ',a11,' (patch number ',i3,')'/,
     & '  Tape details: ',a24,/,
     & '  Patch type: ',a14,/,
     & '  Membrane potential (mV) = ',f9.2,/,
     & '  Temperature = ',f9.1)
c
	 else if(axon) then
	   call READAXON(adcfil,title1,cdate,adctime,ilen,inchan,
     &            calfac,pAV,srate,ffilt,iver,ioff,good)
	   title=title1(1:70)
	   faccal=pAV/calfac	!to convert calfac if pAV corrected
	   nint=ilen/2	!ilen=bytes; nint=number of integers
c  Define values needed for output as CONSAM format
	   if(.not.good) goto 511
	   idt=0
	   id1=0
	   id2=0
	   cs(1:3)='H  '
c	   iver=1001
	   iver=1002	!new format header
	   ffilt1=0.0
	   calfac1=-1.0
	   call TITENT0('Title for CONSAM output:',title,70,.false.)
c      Get other details
	   call GETEXPT(expdate,defname,tapeID,ipatch,
     &	  npatch,Emem,temper)
c and print details of patch
        print 272
        if(discprt) write(8,272)
272	  format(' Input data from AXON (.abf) format file: ')
	   print 28,adcfil,expdate,npatch,tapeID,ptype(ipatch),Emem,temper
         if(discprt) write(8,28)
     &     adcfil,expdate,npatch,tapeID,ptype(ipatch),Emem,temper
c     Axon files may not have correct filter frequency in header
	   print 21,ffilt
21	   format(' Low pass filter (-3dB) = ',f8.1,' Hz: O.K. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(UC(ans).ne.'Y') then
		print 310
310		format(' Low pass filter (Hz, -3dB) = ')
		call INPUTr(ffilt)
	   endif
	 endif	!end of if (axon)
	else
	   print 30,pfile1
30	   format(' CAN NOT FIND ',a40)
	   if(consm) then
		call TITENT0('Input CONSAM file name:',pfile1,40,.false.)
	   else if(axon) then
		call TITENT0('Input AXON file name:',adcfil,30,.false.)
		pfile1(1:30)=adcfil
		pfile1(31:40)='          '
	   endif
	   goto 31
	endif
	if(axon) then
	   print 351,pAV
351	   format(' Gain (pA/V) = ',g13.6,' O.K. [Y] ')
	   ans='Y'
	   call INPUTa(ans)
	   if(UC(ans).eq.'N') then
		print 352
352		format('&Gain (pA/V) = ')
		call INPUTr(pAV)
		calfac=pAV/faccal
	   endif
	else if(consm) then
	   pAV=calfac*6553.6
	endif
c
	print 281,pAV,calfac,ffilt
	if(discprt) write(8,281) pAV,calfac,ffilt
281	format(
     & '  Gain (pA/V) = ',g13.6,/,
     & '  Calibration (pA/ADC unit) = ',g13.6,/,
     & '  Low pass filter (Hz, -3dB) = ',g13.6,/)
c
c Get name and path for output file
80	continue
	call TITENT0('OUTPUT file name & path:',pfile2,40,.false.)
      OPEN(unit=15,file=PFILE2,status='UNKNOWN',access='TRANSPARENT')
c
c	if(debug()) then
cc NB test.dat has 1000 values starting at rec=ioff+1=513 (see TDISK),
cc so read all of them in here into itest()
c	  print 72
c	  if(discprt)write(8,72)
c72	  format(' INPUT DISK FILE')
c        OPEN(unit=16,file=PFILE1,status='UNKNOWN',access='TRANSPARENT')
c	   irec=ioff+1
c	   N=1000
c	   read(16,rec=irec) (itest(i),i=1,N)
c	   CLOSE(unit=16)
c	   print 70
c70	   format(' List values n1 to n2 from INPUT file: n1,n2 = ')
c	   call INPUT2i(n1,n2)
c	   if(n1.gt.0) then
c	    do i=n1,n2
c		print 71,i,itest(i)
c	      if(discprt)write(8,71) i,itest(i)
c71		format(i6,3x,i8)
c	    enddo
c	   endif
c	endif
c
	i1=1		!first and last data value (default)
	i2=nint
c
	print 42,1,nint
42	format(
     & ' First and last data points to be used; i1, i2 [',
     & i2,',',i8,'] = ')
	call INPUT2i(i1,i2)
      if(discprt)write(8,46) i1,i2
46	format(' Points ',i9,' to ',i9,' selected for output')
c	 Calculate number of sections needed for data
	ndatin=i2-i1+1
	nsec=1 + (ndatin-1)/nbuf
	nlast=ndatin-(nsec-1)*nbuf	!points in last section
	if(nlast.eq.0) nsec=nsec-1
c
c Change gain (calfac*ADC=pA)
	gainfac=1.0
	print 56,gainfac
56	format(' Amplify data by factor: gain [',f7.2,'] = ')
	call INPUTr(gainfac)
	ans='N'
	call DCASK('Invert the data',ans,ans)
	invert=ans.eq.'Y'
	setgain=.false.
	if(invert.or.gainfac.ne.1.0) then
	   calfac=calfac/gainfac
	   setgain=.true.
c Get rough baseline from 1st 20 points
	   irec=ioff+1
	   if(.not.ascii) then
		read(14,rec=irec) (idatin(i),i=1,20)
	   endif
	   s=0.0
	   do i=1,20
		s=s + float(idatin(i))
	   enddo
	   imean=int2(ifixr(s/20.))
c find largest value that can be multiplied by gainfac without
c exceeding 32767
	   imax=int2(32767./gainfac)
	   imin=-imax
	endif
	if(setgain) then
     	   print 57, gainfac,calfac,imean
	   if(discprt)write(8,57) gainfac,calfac,imean
57	   format(
     & ' Gain increased by factor of ',f9.3,' (new CALFAC = ',g13.6,')',
     & /,' Mean of 1st 20 points = ',i6,
     & ' subtracted before gain applied.',/,
     & ' Any values that would exceed 32767 are reset to 32767',/,
     & ' Any values that would be below -32767 are reset to -32767')
c     & ' Any values that would exceed 32767 are reset to 0',/,
c     & ' Any values that would be below -32767 are reset to 0')
	   if(invert) then
     	   print 571
	   if(discprt)write(8,571)
571	   format(' Data inverted')
	   endif
	endif
c
c
	iopt=1
	print 40,iopt
40	format(/,
     & ' (1) Filter with Gaussian filter, amd omit points if nec.',/,
     & ' (2) Write subsection of CONSAM file to disk',/,
     & ' (3) Omit points from output file, and write to disk',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(iopt)
	select case(iopt)
c
	   case(1)
		idelt=1		!default
		noutt=0		!total # of points written to output
		irec1=ioff+1	!used	when idelt>0
		if(axon) then
		   ioff=512
		   irec1=513	!output as for CONSAM
		endif
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
		if(r.ge.14.) then
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
		if(.not.ascii) then
		   nsec=1 + (ndatin-1)/nbuf
		   nlast=ndatin-(nsec-1)*nbuf	!points in last section
		   if(nlast.eq.0) nsec=nsec-1
		endif
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
		if(axon) then
		   ioff=512
		   irec1=513	!output as for CONSAM
		endif
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
		nmin=0
		nmax=0
		do isec=1,nsec
c
		   if(debug()) then
			n=in-i0+1
			n0in=(irec-ioff+1)/2	!first point read in input consam
			n1in=n0in+n-1	!last point read in input consam
			nin=n1in-n0in+1	!number read from input consam

		      print 601,isec,nin,n0in,n1in,n,i0,in
			if(discprt) write(8,601) isec,nin,n0in,n1in,n,i0,in
601		      format(' Reading section ',i3,/,
     & 1x,i10,' points read from input file (#',i10,' to ',i10,')',/,
     & 1x,i10,' points in input buffer (#',i10,' to ',i10,')',/)
		   else
		      print 60,isec
60		      format(' Reading section ',i3,' . . .')
		   endif
		   if(.not.ascii) then
			read(14,rec=irec) (idatin(i),i=i0,in)
		   endif
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
		   if(setgain) then
		     n=nbuf
		     if(isec.eq.nsec) n=nlast
		     do i=1-novlap,n+novlap
			 idat=idatin(i)-imean
			 if(idat.gt.imax) then
			   idatin(i)=32767
c			   idatin(i)=0
			   nmax=nmax+1
			 else if(idat.lt.imin) then
			   idatin(i)=-32767
c			   idatin(i)=0
			   nmin=nmin+1
			 else
			   idatin(i)=int2(ifixr(float(idat)*gainfac))
			 endif
		     enddo
		     if(invert) then
			  n=nbuf
			  if(isec.eq.nsec) n=nlast
		        do i=1-novlap,n+novlap
			    idatin(i)=-idatin(i)
			  enddo
		     endif
		   endif
c
c  Now filter this section: number of points in idatin() is now npnt:
		   if(isec.lt.nsec) then
			npnt=nbuf+2*novlap
		   else
			npnt=nlast+2*novlap
		   endif
		   if(debug()) then
			print 611,isec,npnt
			if(discprt) write(8,611) isec,npnt
611			format(
     &		' Filtering section ',i3,': number of points = ',i10)
		   else
			print 61,isec
61			format(' Filtering section ',i3,' . . .')
		   endif
		   call GFILTER(idatin,idatout,npnt,srate,fc,nerr)
		   if(nerr.ne.0) then
			call BELL(1)
			print 700, nerr
700			format(' error # ',i3,' in GFILTER')
		   endif
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
c number of values written =
		   if(isec.lt.nsec) then
		      nvals=nbuf
		   else
		      nvals=nlast
		   endif
		   nout=ifix(1. + float(nvals-i11)/float(idelt))
		   if(debug()) then
			n0out=(irec1-ioff+1)/2	!first point read in input consam
			n1out=n0out+nout-1	!last point read in input consam
			print 621,isec,nout,idelt,i11,nvals,n0out,n1out
			if(discprt) write(8,621) isec,nout,idelt,i11,nvals,
     &		   n0out,n1out
621			format(
     &		' Writing section ',i3,': ',i10,' points written',/,
     &  '(every ',i2,'th point from output buffer #',i10,' to ',i10,')',
     &  /,' written to ouput file points #',i10,' to ',i10,/)
		   else
			print 62,isec
62			format(' Writing section ',i3,' . . .')
		   endif
		   write(15,rec=irec1) (idatout(i),i=i11,nvals,idelt)
c
		   irec1=irec1+2*nout	!1st record to read in next loop
c index of last value
		   ilast=i11+(nout-1)*idelt
		   noutt=noutt+nout	!total points written
		   i11=ilast+idelt-nbuf	!index of 1st point to write in next section
		enddo		!end of isec loop
c
		print 282,nmin,nmax
		if(discprt) write(8,282) nmin,nmax
c282		format(' After gain applied,',/,
c     &	'  number of values in input below -32767 = ',i10,/,
c     &	'  number of values in input above 32767 = ',i10,/)
		if(.not.ascii) CLOSE(unit=14)
c		CLOSE(unit=15)	!leav open to write header at end
c Print what was done
	     	print 48, fc,fc3
	     	if(discprt)write(8,48) fc,fc3
48		format(
     &	 ' Filtered at ',f9.2,' Hz to give final fc = ',f9.2,' Hz')
c    Write the header for output file; same as input except for filter freq,
c    ilen and sample rate (last 2 reduced if idelt>1)
		srate=srate1	!put new sample rate in header
		ffilt=fc3		!put final fc into header
		ilen=noutt*2	!and new length (bytes)
		ioffout=ioff
		if(axon) ioffout=512
c		write(15,rec=1) title,cdate,adctime,idt,ioffout,ilen,inchan,
c     &	   id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,iver
	      write(15,rec=1)iver,title,cdate,adctime,idt,ioffout,ilen,
     &	 inchan,id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,
     &	 expdate,defname,tapeID,ipatch,npatch,Emem,temper
	     	print 51, pfile2,noutt,srate1
     		if(discprt)write(8,51) pfile2,noutt,srate1
51		format(/,' OUTPUT file name (CONSAM format) = ',a40,/,
     & 1x,i9,' points written to output: sample rate = ',f10.3,' Hz')
		CLOSE(unit=15)
c          End of Gaussian filtering

	   case(2)
		srate1=srate	!all points
		noutt=0		!total # of points written to output
c         Now read consam(i1) to consam(i2), and write back with
c		points omitted, to output file, consam1(1) to consam1(noutt)
c   Go through sections
 		irec=ioff + (2*i1-1)  !initialise record # for 1st value to be read
 		irec1=ioff + 1  !initialise record # for 1st value to be written
		if(axon) irec1=513	!output as for CONSAM\
c
		nmin=0
		nmax=0
		do isec=1,nsec
c            Read input data, consam(i1) to consam(i2).  Section #isec
c		in CONSAM starts (excluding overlaps) at consam(j) where
c		j=(isec-1)*nbuf+i1 (=i1 for isec=1), and ends at j=j1+nbuf-1
c		NB read data into idatin(1) to idatin(nbuf) for every section
c		The start value, i1, affects only the record # where the
c		reading starts
		   nvals=nbuf
		   if(isec.eq.nsec) nvals=nlast
		   if(.not.ascii) then
			read(14,rec=irec) (idatin(i),i=1,nvals)
		   endif
		   irec=irec+2*nvals	!1st record to read in next loop
c Now write output: same, but start at 'consam(1)', i.e. start writing
c at irec=ioff+1
		   if(setgain) then
		     do i=1,nvals
			 idat=idatin(i)-imean
			 if(idat.gt.imax) then
			   idatin(i)=32767
c			   idatin(i)=0
			   nmax=nmax+1
			 else if(idat.lt.imin) then
			   idatin(i)=-32767
c			   idatin(i)=0
			   nmin=nmin+1
			 else
			   idatin(i)=int2(ifixr(float(idat)*gainfac))
			 endif
		     enddo
		     if(invert) then
		        do i=1,nvals
			    idatin(i)=-idatin(i)
			  enddo
		     endif
		   endif
		   write(15,rec=irec1) (idatin(j),j=1,nvals)
		   irec1=irec1+2*nvals	!1st record to write in next loop
		   noutt=noutt+nvals
		enddo		!end of isec loop
		if(.not.ascii) CLOSE(unit=14)
c		CLOSE(unit=15)	!leave open to write header at end
c Print what was done
		ilen=2*noutt	!changed in header (bytes)
		ioffout=ioff
		if(axon) ioffout=512
c		write(15,rec=1) title,cdate,adctime,idt,ioffout,ilen,inchan,
c     &	   id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,iver
	      write(15,rec=1)iver,title,cdate,adctime,idt,ioffout,ilen,
     &	 inchan,id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,
     &	 expdate,defname,tapeID,ipatch,npatch,Emem,temper
	     	print 51, pfile2,noutt,srate1
     		if(discprt)write(8,51) pfile2,noutt,srate1
c51		format(' OUTPUT file name = ',a40,/,
c     & 1x,i9,' points written to output: sample rate = ',f10.3,' Hz')
		print 282,nmin,nmax
		if(discprt) write(8,282) nmin,nmax
282		format(' After gain applied,',/,
     &	'  number of values in input below -32767 = ',i10,/,
     &	'  number of values in input above 32767 = ',i10,/)
		CLOSE(unit=15)

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
		ans='Y'
		call INPUTa(ans)
		if(UC(ans).eq.'N') goto 54
c         Now read consam(i1) to consam(i2), and write back with
c		points omitted, to output file, consam1(1) to consam1(noutt)
c	 Calculate number of sections needed for data
		ndatin=i2-i1+1
		if(.not.ascii) then
		   nsec=1 + (ndatin-1)/nbuf
		   nlast=ndatin-(nsec-1)*nbuf	!points in last section
		   if(nlast.eq.0) nsec=nsec-1
		endif
c
c             Now calculate record number in CONSAM to read consam(i1)...
c		NB i1=absolute index in CONSAM of first value
 		irec=ioff + (2*i1-1)  !initialise record # for 1st value to be read
 		irec1=ioff + 1  !initialise record # for 1st value to be written
		if(axon) irec1=513	!output as for CONSAM\
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
		   if(.not.ascii) then
		      read(14,rec=irec) (idatin(i),i=1,nvals)
		   endif
		   irec=irec+2*nvals	!1st record to read in next loop
		   if(setgain) then
		     do i=1,nvals
			 idat=idatin(i)-imean
			 if(idat.gt.imax) then
			   idatin(i)=32767
			 else if(idat.lt.imin) then
			   idatin(i)=-32767
			 else
			   idatin(i)=int2(ifixr(float(idat)*gainfac))
			 endif
		     enddo
		     if(invert) then
		        do i=1,nvals
			    idatin(i)=-idatin(i)
			  enddo
		     endif
		   endif
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
		if(.not.ascii) then
		   CLOSE(unit=14)
		endif
c		CLOSE(unit=15)	!leave open to write header at end
c Print what was done
c    Write the header for output file; same as input except for filter freq,
c    ilen and sample rate (last 2 reduced if idelt>1)
		srate=srate1
		ilen=noutt*2	!bytes
		ioffout=ioff
		if(axon) ioffout=512
c		write(15,rec=1) title,cdate,adctime,idt,ioffout,ilen,inchan,
c     &	   id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,iver
	      write(15,rec=1)iver,title,cdate,adctime,idt,ioffout,ilen,
     &	 inchan,id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,
     &	 expdate,defname,tapeID,ipatch,npatch,Emem,temper
	     	print 55, pfile2,idelt,noutt,srate
     		if(discprt)write(8,55) pfile2,idelt,noutt,srate
55		format(' OUTPUT file name = ',a40,/,
     &  '  Every ',i3,'''th point omitted from output',/,
     &  1x,i9,' points written to output: sample rate = ',f10.3,' Hz')
     		CLOSE(unit=15)
c          End of decimation
c
	end select
c	if(debug()) then
cc NB test1.dat has up to 1000 values starting at rec=ioff+1=513 (see TDISK),
cc so read all of them in here into itest()
c	   do i=1,1000
c		itest(i)=0
c	   enddo
c	   print 73
c	   if(discprt)write(8,73)
c73	   format(' OUTPUT DISK FILE')
c         OPEN(unit=16,file=PFILE2,status='UNKNOWN',access='TRANSPARENT')
c	   irec=ioff+1
c	   read(16,rec=irec) (itest(i),i=1,noutt)
c	   CLOSE(unit=16)
c	   print 74
c74	   format(' List values n1 to n2 from OUTPUT file: n1,n2 = ')
c	   call INPUT2i(n1,n2)
c	   do i=n1,n2
c		print 71,i,itest(i)
c	      if(discprt)write(8,71) i,itest(i)
cc71		format(i6,3x,i8)
c	   enddo
c	endif
c
	call ENDPRINT
c
	end

