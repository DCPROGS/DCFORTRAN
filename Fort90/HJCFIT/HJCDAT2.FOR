	subroutine HJCDAT2(tint0,iampl0,ampl0,iprops0,iscan,
     &  nintt,nfile,kfile,pfile,calfacs2,nval,irecs,nmax,nset)
c Second part of data-read for HJCFIT -tint0() etc arrays now allocated, so
c data can be read into them here.
c Modif 12/29/98 08:21pm to read new format scan.dat files (idatyp=1,
c   iscan=>103, or iscan=-103 for SCSIM).
c   The extra arrays, timsav(), ifits(), ibaselin() are
c   written last in scanout.for, and are not read here because not needed.
c   (actually no changes needed, at least if jumps not read)
c
c (1) last gap in every expt is not defined so set iprops=8 for it
c	If channel was open at end (length undefined) then remove this
c	opening and set length of the gap preceding it as unusable
c (2) all amplitudes now kept in pA: ampl().
c (3) no need to read point amp frequencies for HJCFIT
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
	real*4 tint0(nmax,nset),ampl0(nmax,nset)
	integer*2 iampl0(nmax,nset)
	integer*1 iprops0(nmax,nset)
	integer kfile(20,10),nfile(10),nintt(10)
	character*33 pfile(20,10)	!path names for SCANDAT files
	real*4 calfacs2(20,10)
	integer nval(20,10),irecs(20,10)
	logical discprt,btest,simulat
	character ans*1
	common/dp/discprt
c
c  Declarations for read of .EDE files
	integer*4 ChannelState,ZeroLevel,StartAt,Endat
	real*8    ExactStart,ExactEnd,Duration
	real*4    Average,Variance
	integer*4 Next,Previous
	logical*1 ignore,available
c
c Declarations for.EVL data record
	integer*2	eventepi
	integer*4	LevelStart
	real*4	Amplitude
      integer*4	LevelLength
	real*4	EventSD
	integer*2	EventLevel
	integer*2 	EventProps
	integer*1 ip0,ip1,ip2
	logical present,clampex
	common/evl/clampex	!for read_evl and hjcdat2
c
	simulat=iscan.eq.-3001.or.iscan.eq.-103	!from SCSIM
	if(iscan.ge.201.and.iscan.le.299) goto 201	!read .ede file
	if(iscan.ge.301.and.iscan.le.399) goto 301	!read .evl file
c
	do j=1,nset		!read for each set
         print 71,j
         if(discprt) write(8,71) j
71	   format(' Set ',i3)
	   OPEN(unit=18,file=pfile(1,j),status='UNKNOWN',
     & 	access='TRANSPARENT')
	   index=1		!index in whole tint array = 1,...,nintt(j)
	   do i=1,nfile(j)     !loop for each 'expt' in one set
		if(i.gt.1.and.pfile(i,j).ne.pfile(i-1,j)) then
		   CLOSE(unit=18)
		   OPEN(unit=18,file=pfile(i,j),status='UNKNOWN',
     &		access='TRANSPARENT')
		endif
c		print 554,kfile(i,j),pfile(i,j)
c554		format(' Reading file: ',i6,6x,a33)
		irec1=irecs(i,j)
		n1=index
		n2=index+nval(i,j)-1
	      read(18,rec=irec1) (tint0(n,j),n=n1,n2),
     &	 (iampl0(n,j),n=n1,n2),(iprops0(n,j),n=n1,n2)
c Now convert amplitudes to picoamps with calfac for this particular file
		do n=n1,n2
		   ampl0(n,j)=calfacs2(i,j)*float(iampl0(n,j))
		enddo
		ilast=n2		!last value read
c Set last gap for each individual file unusable automatically (except for
c simulated data, iscan=-3001 or -103)
	      ntsav=nintt(j)
c
		if(simulat) goto 21    !skip 'set unusable' for simulations
c
		if(iampl0(n2,j).eq.0) then
		   iprops0(n2,j)=IBSET(iprops0(n2,j),3) !bit 3='8' set last gap unusable
c=	         index=n2+1	!index in tint for 1st value in next expt file
		   ilast=n2
		else
		   do n=n2-1,1,-1			!look back for 1st shut time
			nintt(j)=nintt(j)-1			!decrement total number
			if(iampl0(n,j).eq.0) then	!shut time found
			   iprops0(n,j)=IBSET(iprops0(n,j),3) !bit 3='8' set it unusable
c=			   index=n+1	!index in tint for 1st value in next expt file
			   ilast=n
			   goto 30	!jump out of loop
			endif
		   enddo
		endif
30		continue
		if(nintt(j).lt.ntsav) then
	         print 20,i,nintt(j)
	     	   if(discprt) write(8,20) i,nintt(j)
20		   format(
     &	  ' Channel open at end of file #',i3,
     &	  ': number of intervals reduced to ',i8)
		endif
21	      continue
		index=ilast+1	!ready for next file in current set
	   enddo	!end of i=1,nfile(j) loop
c
	   if((.not.BTEST(iprops0(nintt(j),j),3))) then !bit 3='8' set for unusable
		call bell(1)
		print 1051,tint0(nintt(j),j)
1051		format(
     &	' Last interval is ',g13.6,' :reset as unusable [Y] ? ')
		ans='Y'	!default
		call INPUTa(ans)
		if(ans.eq.'Y') then
		   iprops0(nintt(j),j)=IBSET(iprops0(nintt(j),j),3)	!amplitude fixed; set bit 3 ='8'
		   print 105
     		   if(discprt) write (8,105)
105		   format(' Last interval set unusable.',/)
		endif
	   endif
	   CLOSE(unit=18)		!ready for next set
	enddo		!end of j=1,nset loop
	goto 999	!end of SCAN.SCN
c
c Read a Dempster *.ede file
201	continue
	do j=1,nset		!read for each set
         print 71,j
         if(discprt) write(8,71) j
c71	   format(' Set ',i3)
	   OPEN(unit=18,file=pfile(1,j),status='UNKNOWN',
     & 	access='TRANSPARENT')
	   index=1		!index in whole tint array = 1,...,nintt(j)
	   do i=1,nfile(j)     !loop for each 'expt' in one set
		if(i.gt.1.and.pfile(i,j).ne.pfile(i-1,j)) then
		   CLOSE(unit=18)
		   OPEN(unit=18,file=pfile(i,j),status='UNKNOWN',
     &		access='TRANSPARENT')
		endif
		irec1=5	!first 60-byte data record
		n1=index
		n2=index+nval(i,j)-1
c read nval records, and set tint0,ampl0,iprops0
		do n=1,nval(i,j)
		   read(unit=18,rec=irec1) ChannelState,ZeroLevel,
     &	 	StartAt,Endat,ExactStart,ExactEnd,Duration,
     &		Average,Variance,
     &		Next,Previous,
     &		ignore,available
c
		   tint0(n1,j)=sngl(Duration*1.d3)	!ms: Duration is in seconds
c
		   if(ChannelState.eq.0) then
			ampl0(n1,j)=0
			iampl0(n1,j)=0
		   else
			ampl0(n1,j)=Average
			iampl0(n1,j)=1
		   endif
c
		   if(Ignore) then
		      iprops0(n1,j)=IBSET(iprops0(n1,j),3) !bit 3='8' set to set as unusable
		   else
			iprops0(n1,j)=0
		   endif
		   n1=n1+1			!next element of tint0 etc02/23/04 08:11am
	         irec1=irec1+60		!next record
		enddo
		ilast=n2		!last value read
c Set last gap for each individual file unusable automatically (except for
c simulated data, iscan=-3001 or -103)
	      ntsav=nintt(j)
c
		if(iampl0(n2,j).eq.0) then
		   iprops0(n2,j)=IBSET(iprops0(n2,j),3) !bit 3='8' set last gap unusable
		   ilast=n2
		else
		   do n=n2-1,1,-1			!look back for 1st shut time
			nintt(j)=nintt(j)-1			!decrement total number
			if(iampl0(n,j).eq.0) then	!shut time found
			   iprops0(n,j)=IBSET(iprops0(n,j),3) !bit 3='8' set it unusable
			   ilast=n
			   goto 202	!jump out of loop
			endif
		   enddo
		endif
202		continue
		if(nintt(j).lt.ntsav) then
	         print 20,i,nintt(j)
	     	   if(discprt) write(8,20) i,nintt(j)
c20		   format(
c     &	  ' Channel open at end of file #',i3,
c     &	  ': number of intervals reduced to ',i8)
		endif
		index=ilast+1	!ready for next file in current set
	   enddo	!end of i=1,nfile(j) loop
c
	   if((.not.BTEST(iprops0(nintt(j),j),3))) then !bit 3='8' set for unusable
		call bell(1)
		print 1051,tint0(nintt(j),j)
c1051		format(
c     &	' Last interval is ',g13.6,' :reset as unusable [Y] ? ')
		ans='Y'	!default
		call INPUTa(ans)
		if(ans.eq.'Y') then
		   iprops0(nintt(j),j)=IBSET(iprops0(nintt(j),j),3)	!amplitude fixed; set bit 3 ='8'
		   print 105
     		   if(discprt) write (8,105)
c105		   format(' Last interval set unusable.',/)
		endif
	   endif
	   CLOSE(unit=18)		!ready for next set
	enddo		!end of j=1,nset loop
	goto 999	!end of *.ede
c
c Read an Axon *.evl file
301	continue
	do j=1,nset		!read for each set
         print 71,j
         if(discprt) write(8,71) j
c71	   format(' Set ',i3)
	   OPEN(unit=18,file=pfile(1,j),status='UNKNOWN',
     & 	access='TRANSPARENT')
	   index=1		!index in whole tint array = 1,...,nintt(j)
	   do i=1,nfile(j)     !loop for each 'expt' in one set
		if(i.gt.1.and.pfile(i,j).ne.pfile(i-1,j)) then
		   CLOSE(unit=18)
		   OPEN(unit=18,file=pfile(i,j),status='UNKNOWN',
     &		access='TRANSPARENT')
		endif
		irec1=256 + 1		!first data record
		n1=index
		n2=index+nval(i,j)-1
c read nval records, and set tint0,ampl0,iprops0
		do n=1,nval(i,j)
c
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
		   tint0(n1,j)=float(LevelLength)*calfacs2(i,j)
c
		   if(EventLevel.eq.0) then
			ampl0(n1,j)=0
			iampl0(n1,j)=0
		   else
			ampl0(n1,j)=Amplitude
			iampl0(n1,j)=1
		   endif
c Meaning of EventProps
c the least significant bit:  Bit 0, event too short ('too short' means
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
		   ip0=0
		   ip1=0
		   ip2=0
		   if(btest(EventProps,0)) ip0=1
		   if(btest(EventProps,1)) ip1=1
		   if(btest(EventProps,2)) ip2=1
c         set 'amplitude dubious'(bit 0) if ip0=1 ('too short')
		   if(ip0.ne.0) then
		      iprops0(n1,j)=IBSET(iprops0(n1,j),0) !set  bit 0='1' amp dubious
		   else
		      iprops0(n1,j)=IBCLR(iprops0(n1,j),0) !clear bit 0='1' amp dubious
		   endif
c         set 'amplitude fixed'(bit 1) if ip1=1 ('amp manually adjusted')
		   if(ip1.ne.0) then
		      iprops0(n1,j)=IBSET(iprops0(n1,j),1) !set bit 1='2' amp fixed
		   else
		      iprops0(n1,j)=IBCLR(iprops0(n1,j),1) !clear bit 0='1' amp dubious
		   endif
c         set 'duration unusable' (bit 3) if ip2=1 ('rejected')
		   if(ip2.ne.0) then
		      iprops0(n1,j)=IBSET(iprops0(n1,j),3) !bit 3='8' set to set as unusable
		   else
		      iprops0(n1,j)=IBCLR(iprops0(n1,j),3) !clear 3='8' set to set as unusable
		   endif
c
		   n1=n1+1			!next element of tint0 etc
		enddo
		ilast=n2		!last value read
c Set last gap for each individual file unusable automatically (except for
c simulated data, iscan=-3001 or -103)
	      ntsav=nintt(j)
c
		if(iampl0(n2,j).eq.0) then
		   iprops0(n2,j)=IBSET(iprops0(n2,j),3) !bit 3='8' set last gap unusable
		   ilast=n2
		else
		   do n=n2-1,1,-1			!look back for 1st shut time
			nintt(j)=nintt(j)-1			!decrement total number
			if(iampl0(n,j).eq.0) then	!shut time found
			   iprops0(n,j)=IBSET(iprops0(n,j),3) !bit 3='8' set it unusable
			   ilast=n
			   goto 302	!jump out of loop
			endif
		   enddo
		endif
302		continue
		if(nintt(j).lt.ntsav) then
	         print 20,i,nintt(j)
	     	   if(discprt) write(8,20) i,nintt(j)
c20		   format(
c     &	  ' Channel open at end of file #',i3,
c     &	  ': number of intervals reduced to ',i8)
		endif
		index=ilast+1	!ready for next file in current set
	   enddo	!end of i=1,nfile(j) loop
c
	   if((.not.BTEST(iprops0(nintt(j),j),3))) then !bit 3='8' set for unusable
		call bell(1)
		print 1051,tint0(nintt(j),j)
c1051		format(
c     &	' Last interval is ',g13.6,' :reset as unusable [Y] ? ')
		ans='Y'	!default
		call INPUTa(ans)
		if(ans.eq.'Y') then
		   iprops0(nintt(j),j)=IBSET(iprops0(nintt(j),j),3)	!amplitude fixed; set bit 3 ='8'
		   print 105
     		   if(discprt) write (8,105)
c105		   format(' Last interval set unusable.',/)
		endif
	   endif
	   CLOSE(unit=18)		!ready for next set
	enddo		!end of j=1,nset loop
c	goto 999	!end of *.evl
c
999	continue
	RETURN
	END


