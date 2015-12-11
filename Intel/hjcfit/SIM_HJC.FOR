	subroutine SIM_HJC(main,tint,iampl,pisim,nsim1,tsim,
     & picum,icur,amean,peq,topen,nopen,nt,nmax,nt1,kt,ktmax,
     & ilast,ttot,nchan,nsims,k,kA,kB,kC,kD,km,imainpanel,itinfos,
     & ifirstsim)
c
c Modification of SIMSC (does simulations in SCSIM) to make simulated
c steady state data in HJCFIT.
c (1) All stuff about resolution removed because that is done later in hjcfit.
c (2) Burst analysis removed (if needed, do this in SCSIM)
c (3) One channel only????
c
c SIMSC notes
c Modified 08/29/98 05:36pm to analyse bursts.  When in B there is no way
c to tell whether this is within burst, or whether it is the silent sojourn
c in B at the end of a burst -only wheen C reached can we know that burst
c has ended.  Thus must record separately the transitions that occur during
c the first, 2nd . . . opening in a burst and the 1st, 2nd. . . gap -the last
c one being discarded if the next set reached is C rather than A
c Use nsimo(is,j) tsimo(is,j) to record jth opening in a burst is=1,..,kA
c and nsimg(i,j), tsimg(i,j) to record jth gap in burst, i=1,..,kB (actual
c state number is=i+kA)
c
c Modif 12/23/89 08:57pm. TINT, iAMPL enlarged, and tint1,tint2 at
c present only same size as tint (rather
c than 5 times bigger to allow for elim of short events when resolution
c imposed). See def of nmax below. And TINSIM,ISTATE removed (used
c before to display indiv state transitions at end).
c*Modif Sept 87 to
c	(a) add N independent channels (N=nchan)
c	(b) impose specified resolution to each channel while
c		going along so get Ktmax USABLE intervals (otherwise
c		models that generate many very short intervals will
c		lose most of data when resolution imposed in EKDIST)
!	use DFLIB
!	use dfwin
!	USE DFPORT
	use menu_f90
	
	character*200 string
c
	real*8 u2,drandom
	real*4 tint(nmax),random
	integer*2 iampl(nmax)		!allocated in main
	allocatable::tint1
	allocatable::iampl1
	real*4 tint1(:)
	integer*2 iampl1(:)
c  Arrays allocated to size=km in hjcsim2
	real*4 pisim(km,km),tsim(km),picum(km,km),Peq(km)
	real*4 amean(km)
	integer*2 icur(km,km),ilast
	integer nsim1(km)
c Local arrays
	real*4 pcum(101)		!for INSTAT
	integer ist(100)		!for INSTAT
c
	logical KBHIT		!Utility lib
	character*1 ch,getch
	LOGICAL debon,deb
	logical caplock,fixed
	logical discprt,ABORTW1
	common/abt/ABORTW1
	common/dp/discprt
	common/rand/ixr1,iyr1,izr1
	integer*2 iacrit
	common/RBLCK/treso,tresg,avamp,iacrit
	common/tty/ittypanel,itty
c
	SAVE k1		!for nsims>1
C
c	debug()=caplock()
c
	ALLOCATE(tint1(nmax),iampl1(nmax))
c
      do i=1,nmax
        tint1(i)=0.0
        iampl1(i)=0	
        tint(i)=0.0
        iampl(i)=0
      enddo
cc	nchan=ifix(enchan + 0.01)
c Start loop to generate transitions
	do i=1,k
	   do j=1,k
		pisim(i,j)=0.	!checks simulated frequencies
	   enddo
	enddo
	topen=0.
	nopen=0
	ABORTW1=.false.
c Note: KT counts no of open and shut times i.e. no of periods spent
c in states of equal conductance, rather than no of transitions between
c states (counted by NT) many of which will have equal conductance
	nt=1		!no of transitions done for tinsim
	nt1=1		!no of transitions done
	ichan=1		!channel no
	ktmaxs=ktmax	!save orig ktmax
	if(nsims.eq.1) then
	   k1=-1		!so INSTAT asks how to choose init state
	   fixed=.false.	!unless set true by INSTAT when start state fixed
	endif
c==	SAVE k1		!for nsims>1

c
200	continue	!return here for 2nd... channels if req
	kt=1		!no of intervals done on current channel
	if(ichan.gt.1.and.fixed) then
	   is=i1		!fixed value, already defined
	else
		if(k1.le.0) then
		
			call INSTAT1(main,peq,k,kA,kB,kC,kD,i1,pcum,ist,k1,fixed)
			is=i1
	   else
	   
			u=random()		!uniform(0,1)
!			u=rand(iflag)
!			call random(u)
			do i=1,k1
				i1=ist(i)
				if(ist(i).eq.-1) then
				i1=i
				goto 114
				endif
				if(u.gt.pcum(i).and.u.le.pcum(i+1)) goto 114	!jump out with i1
			enddo
114			continue
		endif
	   is=i1			!present state
	endif
      if(discprt) write(7,341)is
341	format(' Initial state = #',i3)
	ilast=icur(is,ichan)	!initialise to initial level
	
	if(nsims.eq.1) then
	 write(string,fmt='(a15,i2)') ' Simulating channel ',ichan
	 CALL GMSETTEXTSETTING(ITTY,string)

	endif
	call gmflushcallbackqueue()
	
c
c START OF LOOP
14	continue		!return here for next
	
	if(mod(nt1,1000).eq.0) then	!check if debug on every 100th transition
	write(string,fmt='(i8,a15,i8,a15)') nt1,'transitions;',kt,
     &   ' intervals.'
c	CALL GMSETTEXTSETTING(ITTY,string)
	CALL GMSETTEXTSETTING(Itinfos,string(1:60))
	call gmSetGuiCursor(-1,GHOURGLASS,GDEFAULT)
	call gmDefineTimerCallback(1,100)
	call gmDefineKeySelectCallback(27,101) ! Sets up escape key as escape key!
	call gmDefineKeySelectCallback(113,101)
	call gmDefineKeySelectCallback(81,101)
	call gmDefineKeySelectCallback(112,102)
	call gmDefineKeySelectCallback(80,102)
	icall=gmAction(icall)

	if(icall.eq.101.or.icall.eq.103) then
		ABORTW1=.true.
	else if (icall.eq.102) then
		imes=gmdisplaymessagebox('','Press OK to continue',
     &	ginformation,gok)
	endif

	
	endif
c
c  Get lifetime for current state
	if(ABORTW1) goto 15
	u2=drandom()			!uniform(0,1)
!	u=random()
!	u2=drand(iflag)
	t=-amean(is)*sngl(dlog(u2))	!in ms
c  Check if level has changed
c   -if not, accumulate times for equal currents but do not increment kt
c (for first transition always accumulates -ilast set to initial state)
	if(icur(is,ichan).eq.ilast) then
	   tint1(kt)=tint1(kt)+t
	   iampl1(kt)=icur(is,ichan)
	else
c        Interval just completed -type it and record it
	  
	   if(kt.ge.ktmax) goto 15		!END RUN
	   kt=kt+1
	   tint1(kt)=t
	   iampl1(kt)=icur(is,ichan)
         ilast=icur(is,ichan)
c
	endif
c
c type the individual sojourn just defined
c Note that ACAL now set to 1.0 and 'amplitude' stored in ICUR(i,j)
c (see SCSIM)
	
	nsim1(is)=nsim1(is)+1
	tsim(is)=tsim(is)+t
c  - which state next?
	u=sngl(drandom())		!uniform(0,1)
	do j=1,k
	   if(j.ne.is) then	!otherwise skip
		n=j
		bot=0.
		if(j.gt.1) bot=picum(is,j-1)
		if(u.gt.bot.and.u.le.picum(is,j)) goto 13	  !out of loop
	   endif
	enddo
13	j=n		!next state
c test freqs
	pisim(is,j)=pisim(is,j)+1.
	is=j
	nt1=nt1+1
c	if(nt.le.102400) nt=nt+1
	call gmDefineKeySelectCallback(27,0)
	if(mod(nt1,1000).eq.0) then
	call gmDefineKeySelectCallback(113,0)
	call gmDefineKeySelectCallback(81,0)
	call gmDefineKeySelectCallback(112,0)
	call gmDefineKeySelectCallback(80,0)
	call gmDefineTimerCallback(1,0)
	call gmSetGuiCursor(-1,GDEFAULT,GDEFAULT)
	call gmflushcallbackqueue()
	endif
	goto 14
c END OF LOOP
c
15	continue
	if(ABORTW1) then
	CALL GMSETTEXTSETTING(ITTY,'Run ABORTWed')
	endif
	nint1=kt
c
c Check we have finally got requested number of intervals
c For ICHAN=1 copy them into TINT(i),iAMPL(i)
c For ICHAN>1 add results in TINT1 into ditto.
	ktmax=nint1
	if(ktmax.gt.ktmaxs) ktmax=ktmaxs	!orig no of int requested
	if(ktmax.gt.nmax) ktmax=nmax
      write(string,fmt='(a15,i8,a15,i8,a30)') ' Final run gave ',
     &   nt1,'transitions;',kt,' intervals.'
	 CALL GMSETTEXTSETTING(ITTY,string)
    
      if(discprt) write(7,39) nt1,kt
39	format(' Final run gave ',i8,' transitions, which gave',/,
     & '  ',i8,' intervals.',/)
c
	if(ichan.eq.1) then
	   do i=1,ktmax
		tint(i)=tint1(i)
		iampl(i)=iampl1(i)
	   enddo
	   ntot=ktmax	!ready to input to SUMSC when next channel added
	else if(ichan.gt.1) then
c Add the run just simulated to the sum of all earlier ones
	   nomit=0	!don't omit zero length intervals in output
	   call SUMSC(tint,iampl,ntot,tint1,iampl1,ktmax,nomit,
     &    nmax,nmax,.true.)
	endif
28	continue

	ichan=ichan+1
	if(ichan.le.nchan) goto 200		!do next channel #
	kt=ntot				!return # if int in final record
c
c calc simulated Popen
	to=0.
	do i=1,kA
	   to=to + tsim(i)
	enddo
	ts=0.
	do i=kA+1,k
	   ts=ts + tsim(i)
	enddo
	Ttot=to+ts
      if(discprt) write(7,167)to,ts,to+ts,to/Ttot
	write(string,fmt='(a50,3g13.6,a10,g13.6)') 
     &' Simulated: total time, open, shut, length= ',to,ts,to+ts,
     &  ' Popen= ',to/ttot
	 CALL GMSETTEXTSETTING(ITTY,string)

167	format(' Simulated: total time, open, shut, length= ',
     & 3g13.6,/,' Popen= ',g13.6)
c
c Calc PI matrix from simulated transition freqs
	do i=1,k
	   s=0.		!for row sum
	   do j=1,k
		if(i.ne.j) s=s+pisim(i,j)
	   enddo
	   do j=1,k
		if(s.gt.0.) pisim(i,j)=pisim(i,j)/s
	   enddo
	enddo

	CALL GMSETTEXTSETTING(ITTY,
     & ' State  # of sojourns_____Mean life________Occupancy')
	 
	CALL GMSETTEXTSETTING(ITTY, 
     &'________________simulated___true____simulated____true')
	if(discprt) write(7,164)
164	format(/,
     & ' State  # of sojourns       Mean life',14x,' Occupancy',/,
     & 21x,'    simulated    true        simulated     true')
	do 163 i=1,k
	if(nsim1(i).eq.0) goto 1631
	write(string,fmt='(i6,2x,i10,4x,g13.5,2x,g13.5,2x,g13.5,2x,g13.5)'
     &) i,nsim1(i),
     & tsim(i)/float(nsim1(i)),amean(i),tsim(i)/Ttot,peq(i) 
		CALL GMSETTEXTSETTING(ITTY,string)
      if(discprt) write(7,162)i,nsim1(i),tsim(i)/float(nsim1(i))
     & ,amean(i),tsim(i)/Ttot,peq(i)
162	format(i6,2x,i10,4x,4g13.6)
	goto 163
1631	continue
	write(string,fmt='(i6,2x,i10,4x,a10,3g13.6)') i,nsim1(i),
     & '       ?     ',amean(i),tsim(i)/Ttot,peq(i) 
		CALL GMSETTEXTSETTING(ITTY,string)
    
      if(discprt) write(7,1621)i,nsim1(i),
     & amean(i),tsim(i)/Ttot,peq(i)
1621	format(i6,2x,i10,4x,'       ?     ',3g13.6)
163	continue
      if(discprt) write(7,1632) nt1
	write(string,fmt='(a60,i12)') 
     & ' Total number of sojourns in individual states= ',nt1
		CALL GMSETTEXTSETTING(ITTY,string)
1632	format(
     & ' Total number of sojourns in individual states= ',i12)
c
c Print also  time spent in each state as % of time spent in the set to which
c that state belongs
	if(nsims.gt.1) goto 99	!skip rest of print
c
c	
	if(discprt) write(7,108)
108	format(/)
	tset=0.0
	do i=1,kA
	   tset=tset+tsim(i)
	enddo
	do i=1,kA
	   if(nsim1(i).gt.0) then
		if(tset.gt.1.e-10) then
		write(string,fmt='(a15,i3,a30,f9.4)') 'Time spent in state ',
     &    i,' as % of time in set A = ',100.*tsim(i)/tset
		CALL GMSETTEXTSETTING(ITTY,string)
      	   if(discprt) write(7,173)i,100.*tsim(i)/tset
173	         format(
     &	' Time spent in state ',i3,' as % of time in set A = ',f9.4)
		else
		write(string,fmt='(a15,i3)') 'Time spent in state ',i
		CALL GMSETTEXTSETTING(ITTY,string)
      	   if(discprt) write(7,1731)i
1731	         format(' Time spent in state ',i3,' = 0')
		endif
	   endif
	enddo
	
	if(discprt) write(7,108)
	tset=0.0
	do i=kA+1,kA+kB
	   tset=tset+tsim(i)
	enddo
	do i=kA+1,kA+kB
	   if(nsim1(i).gt.0) then
		if(tset.gt.1.e-10) then
	write(string,fmt='(a15,i3,a30,f9.4)') 'Time spent in state ',
     &    i,' as % of time in set B = ',100.*tsim(i)/tset
		CALL GMSETTEXTSETTING(ITTY,string)
		  
      	   if(discprt) write(7,174)i,100.*tsim(i)/tset
174	         format(
     &	' Time spent in state ',i3,' as % of time in set B = ',f9.4)
		else
		   
		write(string,fmt='(a15,i3)') 'Time spent in state ',i
		CALL GMSETTEXTSETTING(ITTY,string)
      	   if(discprt) write(7,1731)i
c1731	         format(' Time spent in state ',i3,' = 0')
		endif
	   endif
	enddo
	tset=0.0
	do i=kA+kB+1,k
	   tset=tset+tsim(i)
	enddo

	if(discprt) write(7,108)
	do i=kA+kB+1,k
	   if(nsim1(i).gt.0) then
		if(tset.gt.1.e-10) then
	write(string,fmt='(a15,i3,a30,f9.4)') 'Time spent in state ',
     &    i,' as % of time in set C = ',100.*tsim(i)/tset
		CALL GMSETTEXTSETTING(ITTY,string)
		   
      	   if(discprt) write(7,175)i,100.*tsim(i)/tset
175	         format(
     &	' Time spent in state ',i3,' as % of time in set C = ',f9.4)
		else
		   	write(string,fmt='(a15,i3)') 'Time spent in state ',i
		CALL GMSETTEXTSETTING(ITTY,string)
      	   if(discprt) write(7,1731)i
c1731	         format(' Time spent in state ',i3,' = 0')
		endif
	   endif
	enddo
	
	if(discprt) write(7,108)
	nset=0
	do i=1,kA
	   nset=nset+nsim1(i)
	enddo
	if(nset.gt.0) then
	   do i=1,kA
		
	write(string,fmt='(a15,i3,a30,f9.4)')'No. of sojourns in state ',
     &    i,' as % of no in set A = ',100.*float(nsim1(i))/float(nset)
		CALL GMSETTEXTSETTING(ITTY,string)
	      if(discprt) write(7,183)i,100.*float(nsim1(i))/float(nset)
183		format(
     &  ' No. of sojourns in state ',i3,' as % of no. in set A = ',f9.4)
	   enddo
	else
	   	
		CALL GMSETTEXTSETTING(ITTY,' No sojourns in set A')
         if(discprt) write(7,1831)
1831	   format(' No sojourns in set A')
	endif
	
	if(discprt) write(7,108)
	nset=0
	do i=kA+1,kA+kB
	   nset=nset+nsim1(i)
	enddo
	if(nset.gt.0) then
	   do i=kA+1,kA+kB
		
	write(string,fmt='(a15,i3,a30,f9.4)')'No. of sojourns in state ',
     &    i,' as % of no in set B = ',100.*float(nsim1(i))/float(nset)
	CALL GMSETTEXTSETTING(ITTY,string)
     		if(discprt) write(7,184)i,100.*float(nsim1(i))/float(nset)
184		format(
     &  ' No. of sojourns in state ',i3,' as % of no. in set B = ',f9.4)
	   enddo
	else
	   CALL GMSETTEXTSETTING(ITTY,' No sojourns in set B')
         if(discprt) write(7,1841)
1841	   format(' No sojourns in set B')
	endif
	nset=0
	do i=kA+kB+1,k
	   nset=nset+nsim1(i)
	enddo
	
	if(discprt) write(7,108)
	if(nset.gt.0) then
	   do i=kA+kB+1,k
		
	write(string,fmt='(a15,i3,a30,f9.4)')'No. of sojourns in state ',
     &    i,' as % of no in set B = ',100.*float(nsim1(i))/float(nset)
	CALL GMSETTEXTSETTING(ITTY,string)
	      if(discprt) write(7,185)i,100.*float(nsim1(i))/float(nset)
185		format(
     &  ' No. of sojourns in state ',i3,' as % of no. in set C = ',f9.4)
	enddo
	else
	   CALL GMSETTEXTSETTING(ITTY,' No sojourns in set C')
         if(discprt) write(7,1851)
1851	   format(' No sojourns in set C')
	endif
	
	if(discprt) write(7,108)
c
99	continue
	DEALLOCATE(tint1,iampl1)
c
	call gmDefineKeySelectCallback(113,0)
	call gmDefineKeySelectCallback(81,0)
	call gmDefineKeySelectCallback(112,0)
	call gmDefineKeySelectCallback(80,0)
	call gmDefineTimerCallback(1,0)
	call gmSetGuiCursor(-1,GDEFAULT,GDEFAULT)
	
	call gmflushcallbackqueue()
	icall=0
	RETURN
	end

