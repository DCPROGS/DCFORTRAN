	subroutine WRITCSAM(datfil,consfil,cdate,ctime)
c To write integer*2  data in consam format
c
c In CONSAM, the first data point, idata(1) starts at byte (record #) ioff+1,
c idata(2) at ioff+3, idata(3) at ioff+5 -so point #in (in whole data) starts
c at byte # ioff+(2*in-1)
c	irec=ioff + (2*in-1)    !record # for 1st value to be read

	ALLOCATABLE::ibuf
	integer*2 ibuf(:)
	character*40 datfil,consfil
	logical present,blank
	character ans*1
c for CONSAM header
	integer*2 idt,inchan,id1,id2,iver
	integer*4 ioff,ilen
	character cs*3,adctime*8
	character tapeID*24
	character*14 ptype(5)
	character expdate*11,defname*6,title*70
	character*11 cdate,ctime
c
	logical discprt,debug,caplock
	common/dp/discprt
c
	debug()=caplock()
c
	ptype(1)='outside-out'
	ptype(2)='inside-out'
	ptype(3)='cell-attached'
	ptype(4)='whole-cell'
	ptype(5)='simulated data'
c The first data point, idata(1) starts at byte (record #) ioff+1, idata(2) at
c ioff+3, idata(3) at ioff+5 -so point #in (in whole data) starts at
c byte # ioff+(2*in-1)
	blank=.true.
11	call TITENT0(
     &	'Name and path for input 16 bit integer file',
     &	datfil,40,blank)
	call TITENT0(
     &	'Name and path for OUTPUT (CONSAM format) file',
     &	consfil,40,blank)
	i1=1
	print 60,i1
60	format(
     &	' Byte # for start of data (=header length+1) [',i4,'] = ')
	call INPUTi(i1)
	ioff=i1-1
	INQUIRE(file=datfil,exist=present,flen=ilen)
	if(.not.present.or.ilen.eq.0) then
	   print 10,datfil
10	   format(' Can''t find ',a40)
	   ans='Y'
	   call DCASK('Try again',ans,ans)
	   if(ans.eq.'N') STOP
	   blank=.false.
	   goto 11
	endif
c
	nbuf=1000000		!size of data arrays
	print 63,nbuf
63	format(' Number of points in buffer [',i8,'] = ')
	call INPUTi(nbuf)
	ALLOCATE(ibuf(nbuf))
c
	print 62
62	format(
     &   ' Calibration factor (pA per ADC unit) = ')
	call INPUTr(calfac)
	print 68
68	format(' Sample rate for input file (Hz) = ')
	call INPUTr(srate)
	print 69
69	format(' Filter frequency for input file (-3 dB, Hz) = ')
	call INPUTr(ffilt)
	nbyte=ilen		!NB ilen is integer*4
	nint=nbyte/2	!total number of int*2 values in record
	ffilt1=0.0
	inchan=1
c	ioffout=512	!as in consam
	adctime=ctime(1:8)
	idt=0
	id1=0
	id2=0
	cs(1:3)='H  '
	call TITENT0('Title for CONSAM output:',title,70,.false.)
c      Get other details
	call GETEXPT(expdate,defname,tapeID,ipatch,
     &	  npatch,Emem,temper)
c
	i1=1		!first and last data value (default)
	i2=nint
	print 42,1,nint
42	format(
     & ' 1st and last points of input file to use: i1, i2 [',
     & i2,',',i8,'] = ')
	call INPUT2i(i1,i2)
c and print details of patch
      print 46, datfil,i1,i2,consfil,title,calfac,
     & expdate,npatch,tapeID,ptype(ipatch),Emem,temper
      if(discprt)write(8,46) datfil,i1,i2,consfil,title,calfac,
     & expdate,npatch,tapeID,ptype(ipatch),Emem,temper
46	format(/,
     & ' Conversion of 16 bit integer file to consam format',/,
     & ' INPUT file: ',a40,/,
     & ' Points ',i9,' to ',i9,' written in consam format to ',/,
     & ' OUTPUT file: ',a40,/,
     & ' title: ',a70,/,
     & '  Calibration factor (pA per ADC unit) = ',g13.6,/,
     & '  Date of experiment: ',a11,' (patch number ',i3,')'/,
     & '  Tape details: ',a24,/,
     & '  Patch type: ',a14,/,
     & '  Membrane potential (mV) = ',f9.2,/,
     & '  Temperature = ',f9.1,/)

c	irec=ioff + (2*in-1)    !record # for 1st value to be read
	ioffin=2*i1-1	!offset to start reading input at ibuf(i1)
c	 Calculate number of sections needed for data
	ndat=i2-i1+1
	nsec=1 + (ndat-1)/nbuf
	nlast=ndat-(nsec-1)*nbuf	!points in last section
	if(nlast.eq.0) nsec=nsec-1
c
c Open input file
      OPEN(unit=14,file=datfil,status='UNKNOWN',access='TRANSPARENT')
c Open output file
      OPEN(unit=15,file=consfil,status='UNKNOWN',access='TRANSPARENT')
	iver=1002
c Write consam header
	write(15,rec=1)iver,title,cdate,adctime,idt,ioff,ilen,
     &   inchan,id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac,
     &    expdate,defname,tapeID,ipatch,npatch,Emem,temper
c Now write the data, nbuf points at a time (nlast for last section)
c All that differs is start record for read/write
c
c	irec=ioff + (2*in-1)    !record # for 1st value to be read
c
	irecin=ioffin
	irecout=513		!ioff=512 for consam
	do n=1,nsec
	   nread=nbuf
	   if(n.eq.nsec) nread=nlast
	   read(14,rec=irecin) (ibuf(i),i=1,nread)
	   write(15,rec=irecout) (ibuf(i),i=1,nread)
	   irecin=irecin + 2*nread	!bytes
	   irecout=irecout + 2*nread
	   if(debug()) then
	      print 59,n,nsec
59	      format(' Reading/writing data section ',i3,' out of ',i3)
		ir=i2-i1+1
		print 61,ir,irecin,ir,irecout
61		format(
     &	' ',i8,' data points read starting at byte # ',i10,/,
     &	' ',i8,' data points written starting at byte # ',i10,/)
	   else
	      print 64,n,nsec
64	      format(' Reading/writing data section ',i3,' out of ',i3)
	   endif
	enddo
c
	CLOSE(unit=14)
	CLOSE(unit=15)
	DEALLOCATE(ibuf)
c
	RETURN
	end

