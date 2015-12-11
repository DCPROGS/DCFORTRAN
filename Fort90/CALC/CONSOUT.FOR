	subroutine CONSOUT(tint,iampl,ndim,nint,ffilt,rms,avamp,
     & calfac2,cdate,ctime,iscan,icur,icurmax)
c
c To output simulate CONSAM file, with noise added if req, from SCSIM
c
c Modif 11/15/01 09:05am (a bit late!) to write in current CONSAM format
c Keep the same version # as consam -iver=1002 -the fact that this is simulated
c is indicated by ipatch=5

	real*4 tint(ndim)
	integer*2 iampl(ndim)
	allocatable::filt,idata,stepamp,dt
	real*4 filt(:),stepamp(:),dt(:)
	integer*2 idata(:),icurmax
	integer*2 icur(100,100),iy0
c	integer*2 ioffset
	logical addbit
c declarations for CONSAM header
	character ADCFIL*30
c	character ndev*2
	integer*2 idt,inchan,id1,id2,iver
	character cs*3,adctime*8,cdate*11,ctime*11
c==	character*76 title
c Extras for version 1002 as in scan
	character*70 title
	character expdate*11,defname*6,cdate1*8
	character tapeID*24
c
	logical debug,caplock,debon
	logical discprt
	common/dp/discprt
	common/rand/ix,iy,iz
c
	debug()=caplock()
c
c First convert tint() into digitised trace at some specified sample rate
c As it is initially unfiltered, the sample rate needs to be infinite
c so better specify a filter frequency first (possibly higher than
c that eventually required?).  This can then be convolved exactly as in SCAN
c though not all at once!
c
c Decide sample rate and filter freq.
	ffilt=10000.
	print 1
1	format(' Filter cutoff frequency (-3 dB, Hz) = ')
	call INPUTr(ffilt)
	srate=10.*ffilt
	print 2,srate
2	format(' Sample rate (Hz) [',f10.1,'] = ')
	call INPUTr(srate)
	adcfil='C:\consam.dat'
	call TITENT0('Input CONSAM file name:',ADCFIL,30,.false.)
	call TITENT0('Title for Consam file',title,70,.true.)
c
c Write the header
	adctime=ctime(1:8)
	calfac=calfac2
	inchan=1
c=	iver=2001		!current normal consam has iver=1001 (not now!)
c Keep the same version # as consam -iver=1002 -the fact that this is simulated
c is indicated by ipatch=5
	iver=1002		!current normal consam has iver=1002
	ioff=512
	id1=0
	id2=0
	idt=0
	ffilt1=0.
	calfac1=0.
	cs='   '
c===NB ILEN NOT KNOWN YET!! OR CALFAC (BELOW)
c      OPEN(unit=14,file=ADCFIL,status='UNKNOWN',access='TRANSPARENT')
c	write(14,rec=1) title,cdate,adctime,idt,ioff,ilen,inchan,
c     &      id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,iver
c
c Get the step response function as in SCAN (see getfilt, normfilt, newfilt)
	filtf=ffilt/1000.		!kHz
c Print which filter used
	print 61,1000.*filtf
      if(discprt) write(8,61) 1000.*filtf
61	format(' Gaussian filter: cutoff (Hz,-3dB) = ',g13.6)
c 'normalise' the step response -code based on NORMFILT but points at srate
c	dtf1=1.0 		!1 microsec regardless of fc
	dtf=1.e6/srate	!microsec per point
	dtm=1000./srate	!millisec/point
	tmin=-409.4718/filtf	!filtf=fc in kHz, so tmin gives filt1=0.001
	tmax=-tmin		!so tmax gives filt1()=0.999
90	nfilt=1 + ifix((tmax-tmin)/dtf)
	ALLOCATE(filt(nfilt))
	do i=1,nfilt
	   t=(tmin + float(i)*dtf)
	   filt(i)=0.5*(1.0 + erf(5.336446e-3*filtf*t))
	enddo
c Define tf50 etc (see GAUFILT.MCD)
c	tf01=0.0
c	tf1=101.2187/filtf
c	tf50=409.4718/filtf
c	tf99=717.7249/filtf
c	tf999=818.9436/filtf
c Go through intervals-use sections of more-or-less constant duration (# of
c sample points (rather than constant number of intervals)
c
c The first data point, idata(1) starts at byte (record #) ioff+1, idata(2) at
c ioff+3, idata(3) at ioff+5 -so point #in (in whole data) starts at
c byte # ioff+(2*in-1)
c
	ttot=0.0
	do i=1,nint
	   ttot=ttot+tint(i)
	enddo
c
	nmax=1000000	!max number of points to be written at one go
	print 8,nmax
8	format(' Maximum buffer size (depends on RAM) [',i7,'] = ')
	call INPUTi(nmax)
c Decide how many intervals to be written
c NB Convolution calculates from start of first transition to end of last.
c Make sure that each chunk calculated in a call to convsim ends on
c a flat bit ie last tint() is 'long', say 5*trise?  After calculating
c on chunk in convsim. then need to add points to idata() for duration
c of the next interval -that before the first transition in the next convsim
c At present this has to be an integer number of sample points so get
c some error (max=0.5 sample intervals) -so longer the better
	trise=332.1/filtf		!rise time (microsec)
	tcrit=5*trise/1000.	!millisec
	tmax=nmax*dtm	!max time in ms for nmax points
c NB Amplitudes must be scaled so they look like ADC units in CONSAM
C 1401 units are -32768 t0 +32752 in 16 unit increments
c so scale to make largest amp occupy 3/4 of screen, and offset baseline
c by 16k too
c NB minus sign when stepamp is calculated to invert amplitudes
c NB don't reach here if i1=i2 so ntrans=0
	scalfac=32768./float(int4(icurmax))		!ADC per icurmax pA
	offset=16384.	!so zero is 3/4 way up screen
c	ioffset=16384	!so zero is 3/4 way up screen
	calfac=1.0/scalfac			!pA/ADC
c Noise amplitude?
	rms=float(int4(icurmax))/20.		!SNR=20
	print 5,rms
5	format(' RMS noise (pA) to be added to trace [',f7.3,'] = ')
	call INPUTr(rms)
c	print 6,rms
	if(discprt) write(8,6) rms
6	format(/,
     &  ' White Gaussian noise added. Amplitude = ',f7.3,' pA RMS')
	rms1=scalfac*rms
c
	irec=ioff+1		!record (byte #) to start writing current section
c
c Before the loop, write the first interval (bit before the first transition)
c into CONSAM (and set initial level =iy0)
	y0=scalfac*float(int4(iampl(1))) + offset	!initial level first time
	iy0=int2(y0)
	nadd=ifixr(tint(1)/dtm) - nfilt
	if(nadd.ge.1) then
	   if(allocated(idata)) then
		DEALLOCATE(idata)
	   endif
	   ALLOCATE(idata(nadd))
	   do i=1,nadd
		idata(i)=iy0	!set to final level from convsim
	   enddo
	   do i=1,nadd
		idata(i)=idata(i) + int2(rms1*ranorm())
	   enddo
         OPEN(unit=14,file=ADCFIL,status='UNKNOWN',access='TRANSPARENT')
	   write(14,rec=irec) (idata(i),i=1,nadd)
	   CLOSE(unit=14)
	   irec0=irec
	   irec=irec+2*nadd	!for next write (2* for bytes)
	   nvals=nvals+nadd	!number of integers written to consam
	   print 53,nvals-nadd+1,nvals,irec0,irec
	   if(discprt.and.debon) write(8,53) nvals-nadd+1,nvals,irec0,irec
53	   format(' Write bit before 1st transition: points ',
     &    i8,' to ',i8,/,' (bytes ',i10,' to ',i10,')')
	endif
c
	i1=1			!index for tint()
	i2=i1		!first time (duration of tint(1) does not matter_
	i2last=1
c
c START LOOP: return here for next chunk (after resetting i1)
3	continue
	debon=debug()
	t=0.0
	do while (t.le.tmax.and.i2.le.nint-1)
	   i2=i2+1		!i2 index of last interval to be included
	   t=t + tint(i2)
	enddo
c
c Last interval must be long enough to reach full amp -if not then reduce
c i2 until long one is found
	if(tint(i2).lt.tcrit) then
	   do while(i2.gt.i1)
		i2=i2-1
		if(tint(i2).ge.tcrit) goto 4
	   enddo
c if we reach here with no jump to label 4, then the whole of the current chunk
c has no intervals longer than tcrit.  This is most likely to happen when we
c are close to the end of tint(), so the current chunk is short, but if events
c were all very short it could happen at other times.  For now neglect the
c latter and assume this is near end of data, so just stop here when this happens
9	   print 52,i2
	   if(discprt) write(8,52) i2
52	   format(' Writing stopped at event ',i8,
     &    ' because no more long events')
	   goto 99
	endif
c
c if we reach here with no jump to label 4, then the whole of the current chunk
c has no intervals longer than tcrit.  This is most likely to happen when we
c are close to the end of tint(), so the current chunk is short, but if events
c were all very short it could happen at other times.  For now neglect the
c latter and assume this is near end of data, so just stop here when this happens
c
c Define array sizes
c
4	continue
	if(i2.le.i2last) then	!prevent loop on same event near end
	   print 52,i2
	   if(discprt) write(8,52) i2
c52	   format(' Writing stopped at event ',i8,
c     &    ' because no more long events')
	   goto 99
	endif
	i2last=i2
	ntrans=i2-i1
c
c If no transition found, ie tint(i1) is longer than tmax, just write a flat
c bit of duration tint(i1) at current level in idata and put it in consam
c NB the bit between the end of the last transition in one chunk (section in
c idata() that is calculated in each call to convsim and written to disk) has
c to be filled in without using convsim at all, with points at the level
c at the end of the last call to convsim. The number of points=interval between
c last transition in one convsim and first transition of next, so number of
c the duration of this interval extends from the first point in the last filter
c of the 1st convsim call, to the first point in the first filter of the
c 2nd convsim call, but not all flat so must subtract from this the number
c of points in the former to get the number of points to be added. So for
c interval of length t1, need t1/dtm points, from which must subtract nfilt.
c The call to convsim generates trace up to the level at start of tint(i2)
c so the last complete interval in it is tint(i2-1) followed by transition
c to iampl(i2).  Thus points must be added to represent tint(i2) before
c carrying on with next chunk.
c
c If no transition found (i1=i2), ie tint(i2) is longer than tmax, just write a flat
c bit of duration tint(i2) at current level in idata and put it in consam
	if(ntrans.eq.0) then
	   nadd=ifixr(tint(i2)/dtm) - nfilt
	   addbit=.true.		!extra points to be added still
	   goto 7
	endif
c
	if(allocated(idata)) DEALLOCATE(idata)
	if(allocated(stepamp)) DEALLOCATE(stepamp)
	if(allocated(dt)) DEALLOCATE(dt)
	ALLOCATE(idata(nmax),stepamp(ntrans+1),dt(ntrans-1))
c Define dt() and stepamp for convolution, as in SCAN
c dt(i), i=1,ntrans-1:   dt(1)=time from 1st to 2nd transition
c				 dt(2)=time from 1st to 3rd transition
c				 dt(3)=time from 1st to 4th transition etc
	do nt=1,ntrans-1
	   dt(nt)=0.
	   j=i1
	   do j=i1+1,i1+nt
		dt(nt)=dt(nt)+1000.*tint(j)	!in microsec
	   enddo
	enddo
c
	j=i1-1
	do i=1,ntrans
	   j=j+1		!starts at i1
	   stepamp(i)=-scalfac*float(iampl(j+1)-iampl(j))
	enddo
c
c Do the convolution
	call CONVSIM(idata,nmax,y0,ntrans,DT,ke,
     & filt,nfilt,stepamp,dtf)
	iy0=idata(ke)			!initial level for next time
	y0=float(int4(iy0))	!initial level for next time
c If there is room in idata() then add points here for tint(i2), otherwise
c write them to consam separately
	nadd=ifixr(tint(i2)/dtm) - nfilt
	if(ke+nadd.le.nmax) then
	   do i=ke+1,ke+nadd
		idata(i)=iy0	!set to final level from convsim
	   enddo
	   ke=ke+nadd			!new no of points to be written to consam
	   addbit=.false.		!extra points already added
	else
	   addbit=.true.		!extra points to be added still
	endif
c
c Offset idata -already done via y0
c	do i=1,ke
c	   idata(i)=idata(i) + ioffset
c	enddo
c Add noise
	do i=1,ke
	   idata(i)=idata(i) + int2(rms1*ranorm())
	enddo
c
c Write this chunk to disc (consam.dat) and do next
      OPEN(unit=14,file=ADCFIL,status='UNKNOWN',access='TRANSPARENT')
	write(14,rec=irec) (idata(i),i=1,ke)
	CLOSE(unit=14)
	irec0=irec
	irec=irec+2*ke	!for next write (2* for bytes)
	nvals=nvals+ke	!number of integers written to consam
	print 51,i1,i2,nvals-ke+1,nvals,irec0,irec
	if(discprt.and.debon) write(8,51) i1,i2,nvals-ke+1,
     &	nvals,irec0,irec
51	format(' Intervals ',i6,' to ',i6,' written to CONSAM: points ',
     & i8,' to ',i8,/,' (bytes ',i10,' to ',i10,')')

c
c If the flat bit for tint(i2) has not already been added, do it now
7	if(addbit) then
	   if(allocated(idata)) DEALLOCATE(idata)
	   ALLOCATE(idata(nadd))
	   do i=1,nadd
		idata(i)=iy0	!set to final level from convsim
	   enddo
	   do i=1,nadd
		idata(i)=idata(i) + int2(rms1*ranorm())
	   enddo
         OPEN(unit=14,file=ADCFIL,status='UNKNOWN',access='TRANSPARENT')
	   write(14,rec=irec) (idata(i),i=1,nadd)
	   CLOSE(unit=14)
	   irec0=irec
	   irec=irec+2*nadd	!for next write (2* for bytes)
	   nvals=nvals+nadd	!number of integers written to consam
	   print 50,nvals-nadd+1,nvals,irec0,irec
	   if(discprt.and.debon) write(8,50) nvals-nadd+1,nvals,irec0,irec
50	   format('  add points ',i8,' to ',i8,
     &	' (bytes ',i10,' to ',i10,')')
	endif
c
	i1=i2
	if(i1.lt.nint) goto 3
c
c  Write the consam header
99	ilen=nvals*2	!number of bytes written to consam
      OPEN(unit=14,file=ADCFIL,status='UNKNOWN',access='TRANSPARENT')
c=	write(14,rec=1) title,cdate,adctime,idt,ioff,ilen,inchan,
c=     &      id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,iver
	iver=1002	!new format
c Define extra things needed for iver=1002
c CDATE="15-Nov-2001" etc
c=	if(cdate1(1:2).eq.'01') cdate(4:6)='Jan'
	call DATE(cdate1)
	defname(1:2)=cdate1(7:8)		!year
	defname(3:4)=cdate1(1:2)		!month
	defname(5:6)=cdate1(4:5)		!day
	expdate=cdate
	tapeid='Simulated data (SCSIM)  '
	ipatch=5	!simulated
	npatch=1
	Emem=-100.
	temp=20.
	write(14,rec=1) iver,title,cdate,adctime,idt,ioff,ilen,inchan,
     & id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,
     & expdate,defname,tapeID,ipatch,npatch,Emem,temp
	CLOSE(unit=14)
c
      print 35, title,cdate,adctime,ffilt,title,srate,ilen,calfac
      if(discprt)write(8,35) title,cdate,adctime,ffilt,title,srate,
     &	ilen,calfac
35	format(' Title of simulated consam.dat:',/,1x,a70,/,
     &    ' Date= ',a11,' time= ',a8,
     &    ' filter (Hz) = ',f9.1,/,1x,a70,/,
     &    ' sample rate (Hz) = ',f10.1,'  length (bytes) = ',i10,/,
     &    ' SIMULATED: calibration factor = ',g13.6)
c
	if(allocated(filt)) DEALLOCATE(filt)
	if(allocated(idata)) DEALLOCATE(idata)
	if(allocated(stepamp)) DEALLOCATE(stepamp)
	if(allocated(dt)) DEALLOCATE(dt)
	RETURN	!from consout
	end
