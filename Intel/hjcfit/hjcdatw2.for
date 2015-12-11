	subroutine HJCDATw2(tint0,iampl0,ampl0,iprops0,iscan,
     &  nintt,nfile,kfile,pfile,calfacs2,nval,irecs,nmax,nset,clampex)
c
	use menu_f90
c (1) last gap in every expt is not defined so set iprops=8 for it
c	If channel was open at end (length undefined) then remove this
c	opening and set length of the gap preceding it as unusable
c (2) all amplitudes now kept in pA: ampl().
c (3) no need to read point amp frequencies for HJCFIT
c
	real*4 tint0(nmax,nset),ampl0(nmax,nset)
	integer*2 iampl0(nmax,nset)
	integer*1 iprops0(nmax,nset)
	integer kfile(20,10),nfile(10),nintt(10)
	character*60 pfile(20,10)	!path names for SCANDAT files
	real*4 calfacs2(20,10)
	integer nval(20,10),irecs(20,10)
	logical discprt,btest,simulat,clampex
	character cstring*11
	
c	Declarations for read of .EDE files
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
	logical present
	
	common/dp/discprt

	simulat=iscan.eq.-3001.or.iscan.eq.-103	!from SCSIM
	if(iscan.ge.201.and.iscan.le.299) goto 201	!read .ede file
	if(iscan.ge.301.and.iscan.le.399) goto 301	!read .evl file
      i=1
      j=1
	do j=1,nset		!read for each set
        
         if(discprt) write(7,71) j
71	   format(' Set ',i3)
	   
	   index=1		!index in whole tint array = 1,...,nintt(j)
	   do i=1,nfile(j)     !loop for each 'expt' in one set
	      OPEN(unit=18,file=pfile(i,j),status='UNKNOWN',
     & 	    access='DIRECT', form='BINARY', RECL=1)
!		    if(i.gt.1.and.(pfile(i,j).ne.pfile(i-1,j))) then
!		        CLOSE(unit=18)
!		        OPEN(unit=18,file=pfile(i,j),status='UNKNOWN',
 !    &		    access='DIRECT', form='BINARY', RECL=1)
!		    endif
		    irec1=irecs(i,j)
		    n1=index
		    n2=index+nval(i,j)-1
c	 read(18,rec=irec1) (tint0(n,j),n=n1,n2)
	      read(18,rec=irec1) (tint0(n,j),n=n1,n2),
     &	    (iampl0(n,j),n=n1,n2),(iprops0(n,j),n=n1,n2)
c Now convert amplitudes to picoamps with calfac for this particular file
		    do n=n1,n2
		    ampl0(n,j)=calfacs2(i,j)*float(iampl0(n,j))
		    enddo
		    ilast=n2		!last value read
c Set last gap for each individual file unusable automatically (except for
c simulated data, iscan=-3001 or -103)
	      ntsav=nintt(j)
c
	!!! true or not	if(simulat) goto 21    !skip 'set unusable' for simulations
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
c		call intconv(nintt(j),cstring)
c		IANS=gmdisplaymessageBOX('','Set:'//char(48+j)//
c     &	'Channel open at end of file; number of intervals reduced to:'
c     &    //cstring,Ginformation,gok)
	       if(discprt) write(7,20) i,nintt(j)
20		   format(
     &	  ' set :Channel open at end of file #',i3,
     &	  ': number of intervals reduced to ',i8)
		endif
21	      continue
		index=ilast+1	!ready for next file in current set
	   CLOSE(unit=18)
	   enddo	!end of i=1,nfile(j) loop
c
	!!! true or false	if(simulat) goto 211	
	   if((.not.BTEST(iprops0(nintt(j),j),3))) then !bit 3='8' set for unusable
		
		call realtoch(tint0(nintt(j),j),cstring,11)
		
		  IANS=gmdisplaymessageBOX('','Set:'//char(48+j)//
     &	  ' Last interval is '//cstring//
     &      ' :reset as unusable ? ',Gquestion,gyesno)
		
		if(ians.eq.gyesbutton) then
		   iprops0(nintt(j),j)=IBSET(iprops0(nintt(j),j),3)	!amplitude fixed; set bit 3 ='8'
		   
     		   if(discprt) write (7,105)
105		   format(' Last interval set unusable.',/)
		endif
	   endif

211	   CLOSE(unit=18)		!ready for next set
	enddo		!end of j=1,nset loop
c
      goto 999
      	
201   continue !ede
      do j=1,nset		!read for each set
c        print 71,j
         if(discprt) write(7,71) j

	   index=1		!index in whole tint array = 1,...,nintt(j)
	   do i=1,nfile(j)     !loop for each 'expt' in one set
c		if(i.gt.1.and.pfile(i,j).ne.pfile(i-1,j)) then
c		   CLOSE(unit=18)
c		   OPEN(unit=18,file=pfile(i,j),status='UNKNOWN',
c     &	   access='DIRECT', form='BINARY', RECL=1)
c		endif
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
c	         print 20,i,nintt(j)
	     	   if(discprt) write(7,20) i,nintt(j)
c20		   format(
c     &	  ' Channel open at end of file #',i3,
c     &	  ': number of intervals reduced to ',i8)
		endif
		index=ilast+1	!ready for next file in current set
	   enddo	!end of i=1,nfile(j) loop
c
	   if((.not.BTEST(iprops0(nintt(j),j),3))) then !bit 3='8' set for unusable
		 call realtoch(tint0(nintt(j),j),cstring,11)
		
		    IANS=gmdisplaymessageBOX('','Set:'//char(48+j)//
     &	    ' Last interval is '//cstring//
     &      ' :reset as unusable ? ',Gquestion,gyesno)
		
	
		if(ans.eq.'Y') then
		   iprops0(nintt(j),j)=IBSET(iprops0(nintt(j),j),3)	!amplitude fixed; set bit 3 ='8'
		 
     		   if(discprt) write(7,105)
c105		   format(' Last interval set unusable.',/)
		endif
	   endif
	   CLOSE(unit=18)		!ready for next set
	enddo		!end of j=1,nset loop
	goto 999	!end of *.ede 


c Read an Axon *.evl file
301	continue
	do j=1,nset		!read for each set
c         print 71,j
         if(discprt) write(7,71) j
c71	   format(' Set ',i3)
 !       OPEN(unit=18,file=pfile(1,j),status='UNKNOWN',
 !   & 	    access='DIRECT', form='BINARY', RECL=1)

	   index=1		!index in whole tint array = 1,...,nintt(j)
	   do i=1,nfile(j)     !loop for each 'expt' in one set
		    if(i.gt.1) then
		        if(pfile(i,j).ne.pfile(i-1,j)) then
		            CLOSE(unit=18)
		            OPEN(unit=18,file=pfile(i,j),status='UNKNOWN',
     & 	            access='DIRECT', form='BINARY', RECL=1)
                endif
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

	     	   if(discprt) write(7,20) i,nintt(j)

		endif
		index=ilast+1	!ready for next file in current set
	   enddo	!end of i=1,nfile(j) loop
c
	   if((.not.BTEST(iprops0(nintt(j),j),3))) then !bit 3='8' set for unusable

		    iprops0(nintt(j),j)=IBSET(iprops0(nintt(j),j),3)	!amplitude fixed; set bit 3 ='8'

     		    if(discprt) write (7,105)

            call realtoch(tint0(nintt(j),j),cstring,11)
		
		    IANS=gmdisplaymessageBOX('','Set:'//char(48+j)//
     &	    ' Last interval is '//cstring//
     &      ' :reset as unusable ? ',Gquestion,gyesno)
		
		if(ians.eq.gyesbutton) then
		   iprops0(nintt(j),j)=IBSET(iprops0(nintt(j),j),3)	!amplitude fixed; set bit 3 ='8'
		   
     		   if(discprt) write (7,105)

		endif
	   endif
	   CLOSE(unit=18)		!ready for next set
	enddo		
999	continue
	
	RETURN
	END


