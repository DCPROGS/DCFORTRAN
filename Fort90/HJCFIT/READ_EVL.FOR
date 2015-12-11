	subroutine READ_EVL(n,kfile,pfile,nset,nfile,nval,irecs,
     &	srate,calfacs2,ffilt,nintt,ntot,nmax)
c Subroutine to read Axon event list file (*.EVL)
c NB amplitudes should already be in pA, so use calfacs2 for ms/sample point
c (which could be different for different sets), rather than for
c  calibration (amplitude to pA)
c
c NB cannot tell how many intervals there are in a file without reading the
c whole file!
c
c Meaning of EventProps: the least significant bits are
c  Bit 0, event too short ('too short' means
c	that, after points that are affected by filtering are removed, there
c	are no points left to average for amplitude);
c Bit 1, amplitude manually adjusted by user;
c Bit 2, event rejected.
c Compare IPROPS
c	 0=all OK;
c	+1=amplitude dubious = bit 0;
c	+2=amplitude fixed = bit 1;
c	+4 amplitude of opening constrained (see fixamp) = bit 2;
c	+8=duration unusable = bit 3;
c
c Header 256 bytes
c  character*8	FileID	   must be 'EVNTLIST'
c  integer*2	FFversion	   file format version (present=2.0)
c  character*12	DataName	   name of original data file eg 93801222.DAT
c  character*8	ADCUnit	   user units eg 'pA'
c  character*128	comment
c  real*4		TimePerPoint   interval between points (microsec?)
c  integer*2	Interpolation  not used, set to 1
c  real*4  		Filter	   -3dB freq (Hz) filter used by Fetchan
c  						(does not include acquisition filter or
c  							 signal conditioner filter)
c  integer*2	Episode	   0 for gap-free or event driven Fetchex files
c  						or axotape files (used only by Clampex files)
c  integer*2	Trigstart	   0 for events analysis (non zero for latency analysis)
c  real*4		Holding	   Holding pot (mV)
c  real*4		AcqFilter	   -3dB freq (Hz) acquisition filter (inc signal cond filter)
c  real*4		Gaps		   (microsec?) levels less than or eq to this are ignored
c  integer*2	TrigEnd	      time in samples fron start of episode for latency analysis
c  integer*2	NumLevels	   number of detection levels, inc baseline (level 0)
c
c====================
c Events list (offset = 256 bytes)
c 	24 bytes/record for Fetchex or Axotape (probably this is only important case)
c 	26 bytes/record for Clampex (extra integer*2 at start)
c
c   [integer*2	eventepi	   clampex only]
c   integer*4	LevelStart	   elapsed time in samples to beginning of the event
c					     beginning of this event.  For
c					     Fetchex gap-free and event-driven
c					     files this is the time from the
c					     start of the data file, including
c					     gaps.  (Note that this value
c					     cannot be used to directly compute
c					     a data point index into the
c					     original data file for event-
c					     driven data).
c
c   real*4		Amplitude	     Mean amplitude of the level
c					     relative to the running average of
c					     the baseline.  (Note that each
c					     instance of a level 0 (baseline)
c					     amplitude is also relative to the
c					     running average of the baseline.)
c
c   integer*4	LevelLength	     Duration of this level in samples.
c					     This duration INCLUDES the filter-
c					     attenuated end points AND any
c					     ignored events
c   real*4		EventSD	     SD of data within level (user units)
c   integer*2	EventLevel	     Level number (0=baseline)
c   integer*2 	EventProps	     16-bit bitmap, each bit of which
c					     can be used to represent a BOOLEAN
c					     value (TRUE/FALSE).  Only 3 bits
c					     are actually used, counting from
c					     the least significant bit:  Bit 0,
c					     event too short;  Bit 1, amplitude
c					     manually adjusted by user;  Bit 2,
c					     event rejected.
c   real*4		Baseline	     running baseline average (for level 0) -not used

c
	character*33 pfile(20,10)	!path names for .EDE files
	integer kfile(20,10),nfile(10),nintt(10)
	integer nval(20,10),irecs(20,10)
	real*4 calfacs2(20,10),ffilt(10)
c
c Declarations for header
	character*8	FileID
	integer*2 FFversion
	character*12 DataName
	character*8	ADCUnit
	character*128 comment
	real*4 TimePerPoint
	integer*2 Interpolation
	real*4 Filter
	integer*2 Episode
	integer*2 Trigstart
	real*4 Holding
	real*4 AcqFilter
	real*4 Gaps
	integer*2 TrigEnd
	integer*2 NumLevels
c
c Declarations for data record
	integer*2	eventepi
	integer*4	LevelStart
	real*4	Amplitude
      integer*4	LevelLength
	real*4	EventSD
	integer*2	EventLevel
	integer*2 	EventProps
c
	integer*1 ip0,ip1,ip2
	logical discprt,btest,present,clampex
	logical debug
	common/dp/discprt
	common/evl/clampex	!for read_evl and hjcdat2
c
c
c Now read all files
	do j=1,nset
	 nintt(j)=0		!# of obs for set j
	   do i=1,nfile(j)
		CLOSE(unit=18)
		INQUIRE(file=pfile(i,j),exist=present,flen=nlen)
		OPEN(unit=18,file=pfile(i,j),status='UNKNOWN',
     &		access='TRANSPARENT')
c Read the header
		irec=1
                read(unit=18,rec=irec) FileID,FFversion
                if(ffversion.eq.1) then
		read(unit=18,rec=irec) FileID,FFversion,DataName,ADCUnit,
     &	    comment,TimePerPoint,Interpolation,Filter,Episode,
     &      Trigstart,Holding,AcqFilter,Gaps

                else if(ffversion.eq.2) then
		read(unit=18,rec=irec) FileID,FFversion,DataName,ADCUnit,
     &	    comment,TimePerPoint,Interpolation,Filter,Episode,
     &      Trigstart,Holding,AcqFilter,Gaps,TrigEnd
                else
		read(unit=18,rec=irec) FileID,FFversion,DataName,ADCUnit,
     &	    comment,TimePerPoint,Interpolation,Filter,Episode,
     &	    Trigstart,Holding,AcqFilter,Gaps,TrigEnd,NumLevels

                endif
c
c check these values for a valid file
		if(FileID.ne.'EVNTLIST') then
		   print 70,FileID
70		   format(' File ID is ',a8,' (should be ''EVNTLIST'')')
		endif
		clampex=.false.
		if(Episode.ne.0) then
		   print 71
71		  format(' This is a Clampex file (not Fetchan or Axotape)')
		   clampex=.true.
		else
		   print 710
710		   format(' This is a Fetchan or Axotapefile (not Clampex)')
		   clampex=.false.
		endif
		if(TrigStart.ne.0) then
		   print 72
72	         format(' This is not an event analysis file')
		endif
c
		irecs(i,j)=256		!offset to start of data records
     		if((Filter.ge.20.).and.(Filter.le.1.e5).and.
     &	 (AcqFilter.ge.20.).and.(AcqFilter.le.1.e5)) then
		   ffilt(j)=1.0/sqrt(1./Filter**2 + 1./AcqFilter**2)
		else if((Filter.ge.20.).and.(Filter.le.1.e5)) then
		   ffilt(j)=Filter
		else if((AcqFilter.ge.20.).and.(AcqFilter.le.1.e5)) then
		   ffilt(j)=ACqFilter
		else
 		   print 281,ffilt(j)
281		   format(
     &  ' filter frequency (-3 dB) for this file (Hz) [',f10.3,'] = ')
		   call INPUTr(ffilt(j))
		endif
c
		if(i.eq.1) then
		   ffilt1=ffilt(j)
  		else
		   if(abs(ffilt(j)-ffilt1).gt.1.e-20) then
			call BELL(1)
	      	print 40,i,ffilt(j),ffilt1
	      	if(discprt) write(8,40) i,ffilt(j),ffilt1
40			 format(' WARNING: filter setting for file #',i3,
     &		' not same as that for first file (',2g13.6,')')
		   endif
		endif
		ffilt(j)=0.001*ffilt(j)	!in KHz
c		calfacs2(i,j)=1.0		!amplitude already in pA
		calfacs2(i,j)=0.001*TimePerPoint		!ms/sample point
c		avamp(j)=avamp(j)*calfac2		!convert to pA
c Test read (temporary)
c NB cannot tell how many intervals there are in a file without reading the
c whole file! So better read it here for define nint, nval(i,j), nintt(j)
		irec1=256 + 1	!byte # for first dwell time
c Increment bytes for next read by 22 if open, 26 if shut (or if
c integer*2 eventepi is not read, increment by 20 / 24?)
c  End if the next read would go past the end of the file (nlen bytes)
c  The next read would start at irec1, but must calculate where this read would
c  end.  Or, if calcs of record lengths are OK. it should be good enough to
c check that next read starts at byte # < nlen, in do while loop
		nin=0		!count the number of intervals that are read
		nev=0		!count 'problems'
		n0=0		!count 'problems'
		n1=0		!count 'problems'
		n2=0		!count 'problems'
c		do n=1,10
		do while(irec1.lt.nlen)
		   nin=nin+1
		   if(clampex) then
	            read(unit=18,rec=irec1) eventepi,LevelStart,Amplitude,
     &		LevelLength,EventSD,EventLevel,EventProps
			if(EventLevel.eq.0) then
			   irec1=irec1 + 26
c			   irec2=irec1 + 22	!start byte for read after next
			else
			   irec1=irec1 + 22
c			   irec2=irec1 + 26	!start byte for read after next
			endif
		   else
	            read(unit=18,rec=irec1) LevelStart,Amplitude,
     &	 	LevelLength,EventSD,EventLevel,EventProps
			if(EventLevel.eq.0) then
			   irec1=irec1 + 24
c			   irec2=irec1 + 20	!start byte for read after next
			else
			   irec1=irec1 + 20
c			   irec2=irec1 + 24	!start byte for read after next
			endif
		   endif
c
		   ip0=0
		   ip1=0
		   ip2=0
		   if(btest(EventProps,0)) ip0=1
		   if(btest(EventProps,1)) ip1=1
		   if(btest(EventProps,2)) ip2=1
c Count 'problems'
		   if(EventLevel.gt.1) nev=nev+1
		   if(ip0.eq.1) n0=n0+1
		   if(ip1.eq.1) n1=n1+1
		   if(ip2.eq.1) n2=n2+1
c	   the least significant bit:  Bit 0,
c	   event too short;  Bit 1, amplitude
c	   manually adjusted by user;  Bit 2,
c	   event rejected.
c		   debug=.true.
		   debug=.false.
		   if(debug) then
			print 1,nin,LevelStart,Amplitude,ADCunit,LevelLength,
     &		float(LevelLength)*calfacs2(i,j),EventSD,
     &		EventLevel,EventProps,ip0,ip1,ip2
1			format(
     &	' ',i5,' start = ',i8,' amp = ',g11.4,a8,
     &       ' length = ',i10,/,' t (ms) = ',f8.3,' SD = ',f8.3,
     &	 ' level = ',i3,' props = ',i3,' bits0,1,2 = '3i3)
		   endif
		enddo
c end of test read
c
		nint=nin	!number actually read
		ntot=ntot+nint
		nintt(j)=nintt(j) + nint 	!accumulate total number for set j
		nval(i,j)=nint
		pause
		print 25,Filter,AcqFilter,1000.*ffilt(j)
25		format(
     &       ' Filter in Fetchan (Hz, -3 dB) = ',f12.5,4x,/,
     &    ' Filter (acquisition + signal cond) (Hz, -3 dB) = ',f12.5,/,
     &       ' Check: filter(overall) (Hz, -3 dB) = [',f12.5,'] = ')
		x=1000.*ffilt(j)
		call INPUTr(x)
		ffilt(j)=x*1.e-3	!kHz
c      print details for each expt file
		print 28,j,comment(1:60),comment(61:128),pfile(i,j)(1:n),
     &	 nint,DataName,Holding,Filter,AcqFilter,1000.*ffilt(j),
     &	 ADCUnit,TimePerPoint,1.e6/TimePerPoint,FFversion
		if(discprt) write(8,28) j,comment(1:60),comment(61:128),
     &	 pfile(i,j)(1:n),
     &	 nint,DataName,Holding,Filter,AcqFilter,1000.*ffilt(j),
     &	 ADCUnit,TimePerPoint,1.e6/TimePerPoint,FFversion
c
28		format(/,' SET # ',i3,/,
     &       ' Comment: ',a60,/,2x,a68,/,
     &	 ' Results file: ',a33,/,
     &	 ' Number of transitions = ',i8,/,
     &	 ' Raw data file: ',a12,/,
     &	 ' Membrane potential (mV) = ',f9.2,/,
     &       ' Filter in Fetchan (Hz, -3 dB) = ',f12.5,4x,/,
     &    ' Filter (acquisition + signal cond) (Hz, -3 dB) = ',f12.5,/,
     &       ' Filter(overall) (Hz, -3 dB) = ',f12.5,4x,/,
     &       ' User units = ',a8,/,
     &       ' Calibration (microsec/sample point) = ',g13.6,/,
     &       ' Sampling frequency (Hz) = ',g13.6,/,
     &       ' EVL file version number = ',i3,/)
c     &       ' Calibration (amplitude to pA) = ',g13.6,/,
c     &	 ' Plot queue file: ',a40)
c
		srate=1.e3/TimePerPoint		!sample rate, kHz
c
		if(clampex) then
		   print 26
		   if(discprt) write(8,26)
26		   format(' Clampex file type')
		else
		   print 27
		   if(discprt) write(8,27)
27		   format(' Fetchex or Axotape file type')
		endif
		print 29,nev,n0,n1,n2
		if(discprt) write(8,29) nev,n0,n1,n2
29		format(
     & ' Number of levels above first = ',i6,/,
     & ' Number of events too short to measure amplitude = ',i6,/,
     & ' Number of events with amplitude adjusted manually = ',i6,/,
     & ' Number of events that were rejected = ',i6)
		if(nev.gt.0) then
		   print 30
		   if(discprt) write(8,30)
30		   format(' WARNING: record may contain multiple openings')
		   pause
		endif
c	   pause
	   enddo		!end if i=1,nfile loop
	   if(nintt(j).gt.nmax) nmax=nintt(j)
	enddo		!end if j=1,nset loop
	print 90,TimePerPoint
	if(discprt) write(8,90) TimePerPoint
90	format(/,
     &' WARNING: pClamp records dwell times as an integer number of',/,
     &' sample points, i.e. they are integer multiples of ',f10.2,/,
     &' microseconds: resolution should be set to take account of',/,
     &' the poor resolution of short intervals.',/)
	pause

	RETURN
	end


