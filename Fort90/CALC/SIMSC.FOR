	subroutine SIMSC(tint,iampl,pisim,nsim,tsim,
     & picum,icur,amean,peq,topen,nopen,nt,ndim,nt1,kt,ktmax,
     & ilast,ttot,nchan,setres,kA,kB,kC,kD,nbmax)
c
c To do simulations in SCSIM
c
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
c imposed). See def of ndim,ndim1 below. And TINSIM,ISTATE removed (used
c before to display indiv state transitions at end).
c*Modif Sept 87 to
c	(a) add N independent channels (N=nchan)
c	(b) impose specified resolution to each channel while
c		going along so get Ktmax USABLE intervals (otherwise
c		models that generate many very short intervals will
c		lose most of data when resolution imposed in EKDIST)
c
	real*8 u2,drandom
	real*4 tint(ndim)
	integer*2 iampl(ndim)		!allocated in main
	allocatable::tint1,tint2
	allocatable::iampl1,iampl2
c	allocatable::iprops
	real*4 tint1(:),tint2(:)
	integer*2 iampl1(:),iampl2(:)
	real*4 pisim(100,100),tsim(100),picum(100,100),Peq(100)
	real*4 amean(100)
	real*4 pcum(101)		!for INSTAT
	integer ist(100)		!for INSTAT
	integer*2 icur(100,100),ilast
	integer nsim(100)
c for analysis of bursts
	real*4 tsimo(kA,nbmax),tsimg(kB,nbmax)
	integer nsimo(kA,nbmax),nsimg(kB,nbmax)
	real*4 tsimo1(kA,nbmax),tsimg1(kB,nbmax)
	integer nsimo1(kA,nbmax),nsimg1(kB,nbmax)
	integer nlastgap(kB,nbmax)		!so last gap can be removed at end
	real*4 tlastgap(kB,nbmax)		!ditto
	real*4 tolast(kA,nbmax),tslast(kB,nbmax)
	logical within,started,seprint
	allocatable::nopbst
	integer nopbst(:)
c
	character*1 ans,UC
	logical KBHIT		!Utility lib
	character*1 ch,getch
	LOGICAL first,second,debug,setres,debon,deb
	logical flaggap
	logical caplock,fixed
	logical discprt
	common/dp/discprt
	COMMON/dimblk/K,KMAX,ir,nmod
	common/rand/ix,iy,iz
	integer*2 iacrit
	common/RBLCK/treso,tresg,avamp,iacrit
C
	debug()=caplock()
c
	ALLOCATE(tint1(ndim),tint2(ndim),
     &	iampl1(ndim),iampl2(ndim))
	ALLOCATE(nopbst(ndim))
c
c tsimo(i,j,m) record sojourns in state i, for jth opening in mth burst
	do j=1,nbmax
	   do i=1,kA
		tsimo(i,j)=0.
		nsimo(i,j)=0
		tsimo1(i,j)=0.
		nsimo1(i,j)=0
	   enddo
	   do i=1,kB
		tsimg(i,j)=0.
		nsimg(i,j)=0
		tsimg1(i,j)=0.
		nsimg1(i,j)=0
		nlastgap(i,j)=0
		tlastgap(i,j)=0.
	   enddo
	enddo
c
	print 33
33	format(' F1 key to END RUN')
	call CLRKB		!Remove any waiting keyboard char: Utility lib
cc	nchan=ifix(enchan + 0.01)
c Start loop to generate transitions
	do i=1,k
	   do j=1,k
		pisim(i,j)=0.	!checks simulated frequencies
	   enddo
	enddo
	topen=0.
	nopen=0
c Note: KT counts no of open and shut times i.e. no of periods spent
c in states of equal conductance, rather than no of transitions between
c states (counted by NT) many of which will have equal conductance
	nt=1		!no of transitions done for tinsim
	nt1=1		!no of transitions done
	ichan=1		!channel no
	ktmaxs=ktmax	!save orig ktmax
	k1=-1		!so INSTAT asks how to choose init state
	fixed=.false.	!unless set true by INSTAT when start state fixed
c	if((.not.setres).or.second) goto 41		!setres initialised to true
	print 40
40	format(' Impose resolution on results [N] ? ')
	ans='N'
	call INPUTa(ans)
	setres=UC(ans).eq.'Y'
c
	ans='Y'
	flaggap=.false.
	call DCASK('Flag gaps in specified length range',ans,ans)
	if(ans.eq.'Y') then
	   flaggap=.true.
	   print 41,tslo,tshi
41	   format(' Length range (ms) for gaps to be flagged [',
     &	g11.4,',',g11.4,'] = ')
	   call INPUT2r(tslo,tshi)
	endif
c
200	continue	!return here for 2nd... channels if req
	kt=1		!no of intervals done on current channel
	second=.false.	!first run
      if(discprt) write(8,201)ichan
	print 201,ichan
201	format(/,' Channel #',i3)
	if(ichan.gt.1.and.fixed) then
	   is=i1		!fixed value, already defined
	else
	   call INSTAT(peq,k,kA,kB,kC,kD,i1,pcum,ist,k1,fixed)
	   is=i1			!present state
	endif
      if(discprt) write(8,341)is
c	print 341,is
341	format(' Initial state = #',i3)
	ilast=icur(is,ichan)	!initialise to initial level
	islast=is
	call CLS
	call LOCATE(0,0)
	print 34,ichan
34	format(' Simulating channel ',i2,' ....',/
     & ' Abort run with F2; Caplock on to see values.',/,
     & ' Hit any key to start',/)
	call ANYKEY
	debon=debug()
c Add analysis of bursts 08/29/98 05:13pm
c   Initial state open?
	started=is.le.kA	!can't start on bursts until opening found
	nbst=0
	kE=kA+kB
c
c START OF LOOP
14	continue		!return here for next
	if(mod(nt1,100).eq.0) then	!check if debug on every 100th transition
c NB key F2 has ktype=0, scan code=60 (see \fortran\tkey.for)
	   if(KBHIT()) then
		ch=GETCH(ktype)
		if(ktype.eq.0.and.ichar(ch).eq.60) goto 15	!end run
	   endif
	   deb=debug()
	   if(debon.and.(.not.deb)) call CLS	!clear when debug switched off
	   debon=deb
	   if(mod(nt1,nbmax).eq.0) then
		call LOCATE(3,0)
		print 35,nt1,kt
35		format('&',i8,' transitions;',i8,' intervals.')
	   endif
	endif
c Set within (=within a burst) true if channel opens, and leave it true until
c a sojourn in C is encountered, then set it false.  Can't tell whether a given
c shut period is within or between ubtil it is completed, so record every shut
c period in nsimg, tsimg, but overwrite values if it turns out to be a 'between
c burst' shut period.  Once an opening is encountered, start stays true and
c bursts are recorded.  An opening ends with a A->B or A->C transition, so
c use this to count openings within a burst
c Must not include ANYTHING that occurs after the last valid end-of-burts
c So after burst ends, zero nsimg1 etc, keep all transitions in nsimg1 etc,
c and when next burst-end found, add nsimg1 etc into nsimg -if end not
c found they are never added

	if(.not.started) started=is.le.kA	!can't start on bursts until opening found
	if(within.and.is.gt.kE) then		!C reached -burst ended
	   within=.false.		!set false -stays false until next opening
	   nbst=nbst+1		!number of bursts
	   nopbst(nbst)=jo	!number of ops/bst

c Remove last gap in the burst and zero tlastgap ready for the next
	   do i=1,kB
		nsimg1(i,jo)=nsimg1(i,jo) - nlastgap(i,jo)
		tsimg1(i,jo)=tsimg1(i,jo) - tlastgap(i,jo)
c		if(debon) then		!temp debug
c		   print 91,i,jo,nsimg1(i,jo),nlastgap(i,jo)
c		   if(discprt) write(8,91)i,jo,nsimg1(i,jo),nlastgap(i,jo)
c91		   format(' nsimg1(',i2,',',i4,') = ',i5,' reduced by ',i3)
c		endif
		nlastgap(i,jo)=0
		tlastgap(i,jo)=0.
	   enddo
c        add nsimg1 etc into nsimg, now burst ended properly
	   do j=1,jo
		do i=1,kA
		   nsimo(i,j)=nsimo(i,j) + nsimo1(i,j)
		   tsimo(i,j)=tsimo(i,j) + tsimo1(i,j)
		   nsimo1(i,j)=0
		   tsimo1(i,j)=0.
		enddo
	   enddo
	   do j=1,jo-1
		do i=1,kB
		   nsimg(i,j)=nsimg(i,j) + nsimg1(i,j)
		   tsimg(i,j)=tsimg(i,j) + tsimg1(i,j)
		   nsimg1(i,j)=0
		   tsimg1(i,j)=0.
		   nlastgap(i,j)=0
		   tlastgap(i,j)=0.
		enddo
	   enddo
c
	   if(debon) then
		print 70,nbst,nopbst(nbst)
		if(discprt) write(8,70) nbst,nopbst(nbst)
70		format(25x,
     &	' Burst no. ',i6,' with ',i5,' openings completed')
c		 list length of ops/gaps
		do j=1,jo
		   to=0.
		   do i=1,kA
			to=to+tsimo(i,j) - tolast(i,j)
		   enddo
		   print 73,j,to
		   if(discprt) write(8,73) j,to
73		   format(30x,' opening ',i4,': length = ',g11.4)
		   if(j.lt.jo) then
			ts=0.
			do i=1,kB
			   ts=ts+tsimg(i,j) - tslast(i,j)
			enddo
			print 74,j,ts
			if(discprt) write(8,74) j,ts
74			format(30x,' gap ',i4,': length = ',g11.4)
		   endif
		enddo
	   endif
	   jo=0			!reset index for opening within burst
	else if(is.le.kA) then
	   within=.true.
	endif
	if(islast.le.kA.and.is.gt.kA) then	!opening just ended
	   jo=jo+1		!increment number of openings in current burst
	   if(debon) then	!zero tslast for debug print of next shut time
		do i=1,kB
		   tslast(i,jo)=tsimg(i,jo)	!for debug print
		enddo
	   endif
	endif
	if(islast.gt.kA.and.is.le.kA) then	!opening just started
	   if(debon) then	!zero tolast for debug print of next open time (jo+1)
		do i=1,kA
		   tolast(i,jo+1)=tsimo(i,jo+1)	!for debug print
		enddo
	   endif
	endif
c
c
c  Get lifetime for current state
	u2=drandom()			!uniform(0,1)
	t=-amean(is)*sngl(dlog(u2))	!in ms
c  Check if level has changed
c   -if not, accumulate times for equal currents but do not increment kt
c (for first transition always accumulates -ilast set to initial state)
	if(icur(is,ichan).eq.ilast) then
	   tint1(kt)=tint1(kt)+t
	else
c        Interval just completed -type it and record it
	   if(debon.and.kt.gt.0) then
		print 171,kt,tint1(kt),iampl1(kt)
      	if(discprt) write(8,171)kt,tint1(kt),iampl1(kt)
171		format(' Interval #',i8,8x,g13.6,i8)
		print 31
		if(discprt) write(8,31)
31		format(/)
	   endif
	   if(flaggap) then
		ts=tint1(kt)
		if(ts.ge.tslo.and.ts.lt.tshi.and.iampl1(kt).eq.0) then
		   print 741,tslo,tshi
		   if(discprt) write(8,741) tslo,tshi
741		   format(' **** gap between ',g11.4,' and ',g11.4)
		endif
	   endif
	   if(kt.ge.ktmax) goto 15		!END RUN
	   kt=kt+1
	   tint1(kt)=t
	   iampl1(kt)=icur(is,ichan)
         ilast=icur(is,ichan)
	   islast=is
c
	endif
c
c Record bursts -if 'within' then must be in A or B
c Index for opening within burst is not incremented until opening ends
c so use jo+1 here
	if(started.and.within) then
	   if(ilast.eq.0) then	!in gap within burst
		i=is-kA		!i=1,...,kB
		nsimg1(i,jo)=nsimg1(i,jo) + 1
		tsimg1(i,jo)=tsimg1(i,jo) + t
c		if(debon) then		!temp debug
c		   print 92,i,jo,nsimg(i,jo)
c		   if(discprt) write(8,92) i,jo,nsimg1(i,jo)
c92		   format(' nsimg1(',i2,',',i4,') = ',i5,' incremented')
c		endif
		nlastgap(i,jo)=nlastgap(i,jo)+1	!so last gap can be removed at end of bst
		tlastgap(i,jo)=tlastgap(i,jo)+t	!ditto
c		if(debon) then
c		   print 71,jo,nbst+1,is,t
c		   if(discprt) write(8,71) jo,nbst+1,is,t
c71		   format(1x,'   shut time no. ',i5,' within burst no. ',
c     &         i8,': state ',i3,' length = ',g11.4)
c		endif
	   else		!open
		nsimo1(is,jo+1)=nsimo1(is,jo+1) + 1
		tsimo1(is,jo+1)=tsimo1(is,jo+1) + t
c		if(debon) then
c		   print 72,jo+1,nbst+1,is,t
c		   if(discprt) write(8,72) jo+1,nbst+1,is,t
c72		   format(1x,'   open time no. ',i5,' within burst no. ',
c     &         i8,': state ',i3,' length = ',g11.4)
c		endif
	   endif
	endif
c
c type the individual sojourn just defined
	if(debon) then
c Note that ACAL now set to 1.0 and 'amplitude' stored in ICUR(i,j)
c (see SCSIM)
	   print 17,nt1,is,t,icur(is,ichan)
         if(discprt) write(8,17)nt1,is,t,icur(is,ichan)
17	   format(i12,2x,i6,2x,g13.6,i5)
	endif
	nsim(is)=nsim(is)+1
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
	goto 14
c END OF LOOP
c
15	continue
c Impose resolution on channels just simulated- but want to preserve
c original Tint1 so more values can be added to it, so copy it to tint2
c before doing resolution
	do i=1,kt
c	   if(debon) then
c		print 21,i,tint1(i),iampl1(i)
c      	if(discprt) write(8,21)i,tint1(i),iampl1(i)
c21		format(i6,g13.6,i8)
c	   endif
	   tint2(i)=tint1(i)
	   iampl2(i)=iampl1(i)
	enddo
	nint2=kt
	first=.not.second
	if(setres) call RESINSIM(tint2,tint2,iampl2,iampl2,nint2,
     & kt,first)
c
	if(second) goto 16
c Result is NINT intervals- estimate how many more need to be simulated
c to get up to Ktmax intervals AFTER resolution applied?
      print 37,nt1,kt,nint2
      if(discprt) write(8,37) nt1,kt,nint2
37	format(' First run gave ',i8,' transitions, which gave',/,
     & '  ',i8,' intervals before resolution imposed, and ',i8,
     & ' resolved intervals.')
	t=float(ktmax)*float(kt)/float(nint2)
	ktmax=ifix(t*1.1)	 !add 10% extra to make sure requested number reached
	if(ktmax.gt.ndim) ktmax=ndim
c This is est of total no  of intervals needed before resolution in
c order to get ktmax after resolution applied. Increase Ktmax
c appropriately and get more (KT already simulated, in TINT1,iAMPL1)
	print 161,ktmax-kt
	if(discprt) write(8,161) ktmax-kt
161	format(' Estimate ',i8,' more intervals needed')
	if((ktmax-kt).lt.10) goto 16	!too few to be worthwhile
	print 38
38	format(' Simulating more ....',/
     & ' Abort run with F2; Caplock on to see values.',/,
     & ' Hit any key to start',/)
	call ANYKEY
	call CLS
	debon=debug()
	second=.true.
	goto 14
c
16	continue
c Check we have finally got requested number of intervals (not more
c than 1024) and , for ICHAN=1 copy them into TINT(1024),iAMPL(1024)
c For ICHAN>1 add results in TINT2 into ditto.
	ktmax=nint2
	if(ktmax.gt.ktmaxs) ktmax=ktmaxs	!orig no of int requested
	if(ktmax.gt.ndim) ktmax=ndim
      print 39,nt1,kt,nint2
      if(discprt) write(8,39) nt1,kt,nint2
39	format(' Final run gave ',i8,' transitions, which gave',/,
     & '  ',i8,' intervals before resolution imposed, and ',i8,
     & ' resolved intervals.',/)
c
	if(ichan.gt.1) goto 27
c
	do i=1,ktmax
	   tint(i)=tint2(i)
	   iampl(i)=iampl2(i)
	enddo
	ntot=ktmax	!ready to input to SUMSC when next channel added
	goto 28
27	continue
	nomit=0	!don't omit zero length intervals in output
	call SUMSC(tint,iampl,ntot,tint2,iampl2,ktmax,nomit,
     & ndim,ndim,.true.)
28	print 32,ichan,ntot,nomit
      if(discprt) write(8,32)ichan,ntot,nomit
32	format(' Total no of intervals in final record (',i2,
     & ' channels)= ',i8,/,
     & ' Summation produced ',i4,' zero length intervals')
	ichan=ichan+1
	if(ichan.le.nchan) goto 200		!do next channel #
	kt=ntot				!return # if int in final record
c Add the run just simulated to the sum of all earlier ones
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
      if(discprt) write(8,167)to,ts,to+ts,to/Ttot
	print 167,to,ts,to+ts,to/Ttot
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
	call ATYPE(pisim,' PI(sim)',k,k,Kmax,kmax)
	print 164
	if(discprt) write(8,164)
164	format(/,
     & ' State  # of sojourns       Mean life',14x,' Occupancy',/,
     & 21x,'    simulated    true        simulated     true')
	do 163 i=1,k
	if(nsim(i).eq.0) goto 1631
	print 162,i,nsim(i),tsim(i)/float(nsim(i))
     & ,amean(i),tsim(i)/Ttot,peq(i)
      if(discprt) write(8,162)i,nsim(i),tsim(i)/float(nsim(i))
     & ,amean(i),tsim(i)/Ttot,peq(i)
162	format(i6,2x,i10,4x,4g13.6)
	goto 163
1631	print 1621,i,nsim(i),
     & amean(i),tsim(i)/Ttot,peq(i)		!if no sojourns
      if(discprt) write(8,1621)i,nsim(i),
     & amean(i),tsim(i)/Ttot,peq(i)
1621	format(i6,2x,i10,4x,'       ?     ',3g13.6)
163	continue
      if(discprt) write(8,1632) nt1
	print 1632, nt1
1632	format(
     & ' Total number of sojourns in individual states= ',i12)
c Print also  time spent in each state as % of time spent in the set to which
c that state belongs
	print 108
	if(discprt) write(8,108)
108	format(/)
	tset=0.0
	do i=1,kA
	   tset=tset+tsim(i)
	enddo
	do i=1,kA
	   if(nsim(i).gt.0) then
		if(tset.gt.1.e-10) then
		   print 173,i,100.*tsim(i)/tset
      	   if(discprt) write(8,173)i,100.*tsim(i)/tset
173	         format(
     &	' Time spent in state ',i3,' as % of time in set A = ',f9.4)
		else
		   print 1731,i
      	   if(discprt) write(8,1731)i
1731	         format(' Time spent in state ',i3,' = 0')
		endif
	   endif
	enddo
	print 108
	if(discprt) write(8,108)
	tset=0.0
	do i=kA+1,kA+kB
	   tset=tset+tsim(i)
	enddo
	do i=kA+1,kA+kB
	   if(nsim(i).gt.0) then
		if(tset.gt.1.e-10) then
		   print 174,i,100.*tsim(i)/tset
      	   if(discprt) write(8,174)i,100.*tsim(i)/tset
174	         format(
     &	' Time spent in state ',i3,' as % of time in set B = ',f9.4)
		else
		   print 1731,i
      	   if(discprt) write(8,1731)i
c1731	         format(' Time spent in state ',i3,' = 0')
		endif
	   endif
	enddo
	tset=0.0
	do i=kA+kB+1,k
	   tset=tset+tsim(i)
	enddo
	print 108
	if(discprt) write(8,108)
	do i=kA+kB+1,k
	   if(nsim(i).gt.0) then
		if(tset.gt.1.e-10) then
		   print 175,i,100.*tsim(i)/tset
      	   if(discprt) write(8,175)i,100.*tsim(i)/tset
175	         format(
     &	' Time spent in state ',i3,' as % of time in set C = ',f9.4)
		else
		   print 1731,i
      	   if(discprt) write(8,1731)i
c1731	         format(' Time spent in state ',i3,' = 0')
		endif
	   endif
	enddo
	print 108
	if(discprt) write(8,108)
	nset=0
	do i=1,kA
	   nset=nset+nsim(i)
	enddo
	if(nset.gt.0) then
	   do i=1,kA
		print 183,i,100.*float(nsim(i))/float(nset)
	      if(discprt) write(8,183)i,100.*float(nsim(i))/float(nset)
183		format(
     &  ' No. of sojourns in state ',i3,' as % of no. in set A = ',f9.4)
	   enddo
	else
	   print 1831
         if(discprt) write(8,1831)
1831	   format(' No sojourns in set A')
	endif
	print 108
	if(discprt) write(8,108)
	nset=0
	do i=kA+1,kA+kB
	   nset=nset+nsim(i)
	enddo
	if(nset.gt.0) then
	   do i=kA+1,kA+kB
		print 184,i,100.*float(nsim(i))/float(nset)
     		if(discprt) write(8,184)i,100.*float(nsim(i))/float(nset)
184		format(
     &  ' No. of sojourns in state ',i3,' as % of no. in set B = ',f9.4)
	   enddo
	else
	   print 1841
         if(discprt) write(8,1841)
1841	   format(' No sojourns in set B')
	endif
	nset=0
	do i=kA+kB+1,k
	   nset=nset+nsim(i)
	enddo
	print 108
	if(discprt) write(8,108)
	if(nset.gt.0) then
	   do i=kA+kB+1,k
		print 185,i,100.*float(nsim(i))/float(nset)
	      if(discprt) write(8,185)i,100.*float(nsim(i))/float(nset)
185		format(
     &  ' No. of sojourns in state ',i3,' as % of no. in set C = ',f9.4)
	enddo
	else
	   print 1851
         if(discprt) write(8,1851)
1851	   format(' No sojourns in set C')
	endif
	print 108
	if(discprt) write(8,108)
c
c Now analysis of bursts
	nobmax=0
	n1=0
	if(nbst.eq.0) then
	   print 1901
	   if(discprt) write(8,1901)
1901	   format(' No bursts completed')
	   goto 99
	endif
c
	do ib=1,nbst
	   n=n+nopbst(ib)
	   if(nopbst(ib).gt.nobmax) nobmax=nopbst(ib)
	   if(nopbst(ib).eq.1) n1=n1+1
	enddo
	amob=float(n)/float(nbst)
	print 190,nbst,kB
	if(discprt) write(8,190) nbst,kB
190	format(
     &' ANALYSIS OF ',i8,' THEORETICAL BURSTS:',/,
     & '  (bursts defined by kB = ',i3,' short shut states)')
	call DCASK(
     & ' Print separately for 1st, 2nd, . . . opening in burst','n',ans)
	seprint=ans.eq.'Y'
	print 189,amob,n1,nobmax
	if(discprt) write(8,189) amob,n1,nobmax
189	format(' mean no of openings/burst = ',g13.6,/,
     & '   number of bursts with one opening = ',i5,/,
     & '   maximum number of openings/burst = ',i6,/,
     & /,/,' Opening #   State   # of sojourns   Mean life')
	do i=1,kA
	   no=0
	   to=0.
	   do j=1,nobmax
		no=no+nsimo(i,j)
		to=to+tsimo(i,j)
		if(seprint) then
		   if(nsimo(i,j).gt.0) then
			print 75,j,i,nsimo(i,j),tsimo(i,j)/float(nsimo(i,j))
			if(discprt) write(8,75) j,i,nsimo(i,j),
     &						tsimo(i,j)/float(nsimo(i,j))
		   else
			print 76,j,i,nsimo(i,j)
			if(discprt) write(8,76) j,i,nsimo(i,j)
		   endif
		endif
	   enddo
	   if(no.gt.0) then
		print 191,i,no,to/float(no)
		if(discprt) write(8,191) i,no,to/float(no)
	   else
		print 193,i,no
		if(discprt) write(8,193) i,no
	   endif
191	   format(' All',4x,i6,7x,i6,11x,g13.6)
193	   format(' All',4x,i6,7x,i6)
75	   format(1x,i4,3x,i6,7x,i6,11x,g13.6)
76	   format(1x,i4,3x,i6,7x,i6)
	enddo
	print 192
	if(discprt) write(8,192)
     &
192	format(/,' Shut states (set B) within bursts',/,
     & ' Gap #   Shut state   # of sojourns   mean life')
	do i=1,kB
	   ns=0
	   ts=0.
	   do j=1,nobmax
		ns=ns+nsimg(i,j)
		ts=ts+tsimg(i,j)
		if(seprint) then
		   if(nsimg(i,j).gt.0) then
			print 75,j,i+kA,nsimg(i,j),tsimg(i,j)/float(nsimg(i,j))
			if(discprt) write(8,75) j,i+kA,nsimg(i,j),
     &						tsimg(i,j)/float(nsimg(i,j))
		   else
			print 76,j,i+kA,nsimg(i,j)
			if(discprt) write(8,76) j,i+kA,nsimg(i,j)
		   endif
		endif
	   enddo
	   if(ns.gt.0) then
		print 191,i+kA,ns,ts/float(ns)
		if(discprt) write(8,191) i+kA,ns,ts/float(ns)
	   else
		print 193,i+kA,ns
		if(discprt) write(8,193) i+kA,ns
	   endif
	enddo
c
99	continue
	DEALLOCATE(tint1,tint2,iampl1,iampl2)
	DEALLOCATE(nopbst)
c
	RETURN
	end

