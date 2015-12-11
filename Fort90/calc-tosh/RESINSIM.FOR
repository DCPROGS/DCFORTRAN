	subroutine RESINSIM(tint0,tint,iampl0,iampl,nint,nintt,
     & first)
c
c RESINSIM= version of RESINT1 for SCSIM
c  (1) No need for iprops() at all -all values are good. But last interval
c	 must still be treated as unusable if it is short (below tres) because
c	 we do not know how long the next interval (with which it should be
c	 concatenated) was: (see new go_on line).
c  (2) No need for ihjc (always HJC method); no need for filter stuff
c  (3) Uses integer amplitudes, like original
c  (4) Output arrays (tint,iampl) may be same as input (tint0,iampl0)
c	(initialization of output arrays removed)
c  (5) 'first' made a parameter so asks for resolution & amplitude when true
c	   (sres,sexp not needed).
c  (6) nintt=number of intervals BEFORE resolution imposed
c      nint=number of intervals AFTER resolution imposed
c	 ndim=declared dimension of tint, iampl in calling prog (not needed?)
c
c VERSION FOR NEW EKDIST (for new SCAN files) 02/13/94 05:49pm
c TO DO:
c (1) iprops=2,4 not transferred to output at present
c (2) imode not needed
c
c New notes:
c (1) no longer uses sublev, jfilt
c (2) now has separate input arrays (tint0 etc) and output (tint etc):
c	former used to hold orig data (no resolution imposed) so need not re-read
c (3) avamp now only in common (not a parameter)
c
c
c OLD NOTES:
c Modif 03/11/92 01:57pm to allow HJC method of imposition of resolution
c OR original method (in which contigous intervals below resolution get
c set bad). If HJC method used ihjc=1 on exit, otherwise ihjc=2.
c 02/18/92 01:53pm RESINT is updated version of RESINT3 with some old
c bits removed
c	IMODE=0 if called from EKDIST
c	IMODE=1 if called from HJCFIT
c	IMODE=2 if called from SCDIR
c 05/27/89 08:38pm Lahey version of RESIN3 (debug no longer a param)
c
c  NINT1=no of intervals in each file read (local to this subroutine)
c  NINTT=total no of intervals in all files before resolution imposed
c  NINT=total no of intervals AFTER resolution imposed
c
c Dec 1988: as RESIN2 but also has Trise (fixed in DSKIN1) in call so
c resolution can be expressed as a multiple of Trise, and the corresponding
c value of A/Amax printed
c Dec 87: as RESINT but fc as well as jfilt in call (for EKDIS3 only
c at present; fc may be modif for filtering by tape etc)). Calls version
c of FALSEV that also uses fc.
C
	real*4 tint0(nintt)
	real*4 tint(nintt)
	integer*2 iampl0(nintt),iampl(nintt),iacrit,iamp
	logical DEBUG,pon,slock,caplock,debon,go_on
	logical RESOLV,DIFAMP		!FUNCTIONS
	logical first
c	integer*2 ktype
c	logical KBHIT		!Utility lib
c	character ch,getch
	character*1 ans,UC
	logical discprt
	common/dp/discprt
	common/RBLCK/treso,tresg,avamp,iacrit
c
c Input= tint,iampl values found in scan1, i.e. with max pos
c    resolution (and treso,tresg after first call)
c Output= tint,iampl with shortest resolvable time taken as
c    treso,tresg (milliseconds) for measurements of open and gap times
c       respectively. Obviously get correct answer only
c    if tres is equal to or greater than (i.e.resolution is
c    less than) the resolution of the input.
c  Openings sep by gaps (amp=0) less than tresg are concatenated (together
c with the intermediate gap) into a single opening if the amplitude
c of the two openings is the same within x percent of full amplitude.
c If the openings have different amps they are not concat but half the
c short gap length is added to each.
c   Unusable intervals are supposed to be always resolvable and they
c are kept as unusable (=-1000.) but cause a concatenated group to end.
c   If 2 or more unresolvable intervals occur in succession then
c they, together with the preceding and following resolvable interval
c (the end and start times,respectively, of which will be unsure) are
c counted as a single unusable interval with amplitude set to that
c of the first resolvable interval in the group (which is a legitimate
c amplitude estimate for amp distribution). If such a group is followed
c by another unresolvable interval this must also be concat into
c the unusable group. Thus after finding 2 (or more) consecutive
c unresolvable intervals these (=preceding resolvable interval) plus
c all succesive intervals are all concat into a single unusable group
c until two succesive resolvable intervals are found. Former of
c these is part of the unusable group, latter of them has properly
c defined starting point and is start of a new (possibly concat) interval.
c   Thus resolution of output is consistent whereas that of input is
c not, because shortness of interval that it is decided to fit in
c scfit will not be entirely consistent.
c    Each new concat opening is assigned an amplitude that
c is (1)the mean of the non-assumed amps of each of the openings
c that make it up, if there are any, otherwise (2) the mean of
c the assumed amps.
c
C
c Define functions
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
c Initialise output arrays -remove so can have output arrays same as input
c	if(.not.sres) then
c	   do i=1,nintt
c		tint(i)=0.0
c		ampl(i)=0.0
c	   enddo
c	endif
c
203	continue
	if(first) then
	     x=treso*1000.
	     print 305,x		!default from .INI file
305        format(' Min resolvable open time (microsec) [',f8.2,'] = ')
	     call INPUTr(x)
	     treso=0.001*x		!in ms (default is in ms already)
	     x=tresg*1000.
	     print 3051,x		!default from .INI file
3051	     format('&Min resolvable shut time (microsec) [',f8.2,'] = ')
	     call INPUTr(x)
	     tresg=0.001*x		!in ms (default is in ms already)
	endif
	if(pon()) write(7,30) 1000.*treso,1000.*tresg
	if(discprt) write(8,30) 1000.*treso,1000.*tresg
	if(.not.first) print 30,1000.*treso,1000.*tresg
30	format(' Resolution (microsec):',/,
     & '   for openings = ',f8.2,/,
     & '   for shuttings= ',f8.2)
c
	if(treso.lt.0.00001.and.tresg.lt.0.00001) then
	   nint=nintt	!no resolution imposed
	   do i=1,nint
		tint(i)=tint0(i)	!copy input straight to output
		iampl(i)=iampl0(i)
	   enddo
	   goto 145
	endif
c
	if(first) then
	   print 102,avamp
102	   FORMAT(' Full amplitude (pA) = ',G13.6,'  O.K. [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
		print 110
110		format(' Full amp (pA, with sign) = ')
		call INPUTr(avamp)
	   endif
	   acrit=abs(0.1*avamp)
	   print 111,acrit
111	   FORMAT('& pA for ''real'' amp difference [',f6.2,'] = ')
	   call INPUTr(acrit)
	   iacrit=int2(ifixr(acrit))
	   if(pon()) write(7,31) avamp,acrit
	   if(discprt) write(8,31) avamp,acrit
31	   format(' For sublevels take:',/,3x,
     & ' full amplitude (pA)= ',f8.2,'; pA for real difference= ',f6.3)
	endif
C
307	continue
C CHECK FOR NEG INTERVALS, AND SET THEM
C UNUSABLE SKIPPING THOSE ALREADY SET UNUSABLE.
	do i=1,nintt-1
	   if(tint0(i).lt.0.) then
	      call BELL(1)
	      print 107,i,tint0(i)
	      if(discprt) write(8,107) i,tint0(i)
107	      format('  ****tint(',i4,')= ',g13.6,'  is negative -STOP!')
		STOP
	   endif
	enddo
c
	if(debug()) then
	   print 503
	   if(discprt) write(8,503)
503	   format(' Input',35x,' Output')
	endif
c
c=	ihjc=1	!always HJC method
	print 602
      if(pon()) write(7,602)
      if(discprt) write(8,602)
602	format(
     & ' HJC definition of e-openings and e-gaps used.')
      print 60
60	format(
     & ' Put Caplock on to see values. Working . . . ')
c
c NOW IMPOSE RESOLUTION
	ki=0	!counts concatenated intervals
	i=0
	go_on=.true.	!to cope with irresolvable last interval
	debon=debug()
C
C IF THE FIRST INTERVAL IS BOTH USABLE AND RESOLVABLE
C THIS IS STARTING POINT. IF NOT LOOK FOR FIRST INTERVAL THAT
C IS BOTH, AND IS PRECEDED BY AN RESOLVABLE INTERVAL TOO (OTHERWISE
C ITS START WILL BE DEFINED ONLY BY THE POSITION OF THE PRECEDING
C UNRESOLVABLE INTERVAL AND SO WILL BE UNRELIABLE)
	i=i+1
	if(RESOLV(i,tint0,iampl0,nintt)) goto 2 		!found it
1	i=i+1
	if(RESOLV(i,tint0,iampl0,nintt).and.
     &  RESOLV(i-1,tint0,iampl0,nintt)) goto 2
	goto 1
c
C NOW START TO LOOK FOR UNRESOLVABLE INTERVALS
2	ki=ki+1
c Check debug() every 100 intervals as in simsc (also in RESINT1/2)
	if(mod(i,100).eq.0) then	!check if debug on every 100th transition
	   debon=debug()
	endif
	if(debon) then
	   print 501,i,tint0(i),iampl0(i)
         if(discprt) write(8,501)i,tint0(i),iampl0(i)
	endif
	call SUMAMP(-1,j,S,j1,s1,iamp)	!initialise sums
	tint(ki)=tint0(i)			!start new concat group
	call SUMAMP(1,j,S,j1,s1,iampl0(i))	!accumulate amps
c is next interval resolvable?
7	continue
	if(i.eq.nintt) goto 4		!END
	if(debon) then
	   print 501,i+1,tint0(i+1),iampl0(i+1)
         if(discprt) write(8,501) i+1,tint0(i+1),iampl0(i+1)
501	   format(i8,g13.6,2x,i8)
	endif
	if(RESOLV(i+1,tint0,iampl0,nintt)) goto 4  !yes: assign amp & go on
C Do next bit if NOT resolvable. Has the interval after it got the 'same' amp
C as that before it?
c Note that for simulated data it is quite possible for data to end with
c an unresolvable (<tres) open or shut time so it is NOT safe to refer to
c i+2 here -must end at i=nintt-1 in this case
	if(i.eq.nintt-1) then		!not resolvable so end
	   go_on=.false.
	   goto 4
	endif
	if(debon) then
	   print 501,i+2,tint0(i+2),iampl0(i+2)
         if(discprt) write(8,501) i+2,tint0(i+2),iampl0(i+2)
	endif
c
71	tin=tint(ki)
	if(DIFAMP(iampl0(i),iampl0(i+2))) goto 5
c
c Now bit to be done if tint0(i+1) unresolvable and intervals
c on each side have the 'same' amp. Concatenate.
9	tint(ki)=tin + tint0(i+1) + tint0(i+2)
c if any of these intervals unusable set whole group unusable
	call SUMAMP(1,j,S,j1,s1,iampl0(i+2))	!accumulate amps
	i=i+2
	goto 7	!continue concatenating until resolvable interval found
c
c NEXT bit done if tint(i+1) unresolvable but intervals on each
c side have 'different' amps.
5	continue
	tin1=tint0(i+1)
	tint(ki)=tin + 0.5*tin1
	tint(ki+1)=0.5*tin1 + tint0(i+2)
c
c   Concat of tint(ki) now ended so assign amp to it
	if(j.gt.0) then
	   iampl(ki)=int2(ifixr(S/float(j)))
	else if(j1.gt.0) then
	   iampl(ki)=int2(ifixr(S1/float(j1)))
	endif
c   tint(ki+1) (approx=tint0(i+2)) may need more things concat with it
c   so initialise sums (and setdub) and start new ones.
	call SUMAMP(-1,j,S,j1,s1,iamp)		!initialise sums
	call SUMAMP(1,j,S,j1,s1,iampl0(i+2))	!accumulate amps
	ki=ki+1
	i=i+2
	goto 7		!CARRY ON CONCAT
C
C Assign amp if concatenation finished
4	continue
	if(j.gt.0) then
	   iampl(ki)=int2(ifixr(S/float(j)))
	else if(j1.gt.0) then
	   iampl(ki)=int2(ifixr(S1/float(j1)))
	endif
	i=i+1
	if(debon) then
	   print 502,ki,tint(ki),iampl(ki)
	   if(discprt) write(8,502)ki,tint(ki),iampl(ki)
502	   format(30x,i8,g13.6,2x,i8)
	endif
	if(i.le.nintt.and.go_on) goto 2
c
	nint=ki
145	continue
	print 289,NINT
	if(pon()) write(7,289) NINT
	if(discprt) write(8,289) NINT
289	FORMAT( ' Number of resolved intervals= ',I8,/)
	  if(debug()) then
		print 23
23		format(' Print intervals and amplitudes # n1,n2 [skip] = ')
		n1=0
		call INPUT2i(n1,n2)
		if(n1.gt.0) then
		  if(n2.lt.n1) n2=n1
		  do 25 i=n1,n2
		  print 26,i,tint(i),iampl(i)
		  if(discprt) write(8,26) i,tint(i),iampl(i)
26		  format(i6,g13.6,2x,i8)
25		  continue
		endif
	  endif
c
	RETURN
	END


	subroutine SUMAMP(imode,j,S,j1,s1,iamp)
C TO SUM AMPLITUDES TO CALC MEAN AMP OF A CONCATENATED GROUP
C (MEASURED AMPS IN S, ASSUMED IN S1). IF imode=-1 IT INITIALISES.
c
	integer*2 iamp
c
	if(imode.lt.0) then   !initialise only
	   j=0
	   S=0.0
	   j1=0
	   S1=0.0
	   RETURN
	endif
c
	j=j+1
	S=S+float(iamp)
c
	RETURN
	end


	logical function RESOLV(i,tint0,iampl0,nintt)
	real*4 tint0(nintt)
	integer*2 iampl0(nintt),iacrit
	common/RBLCK/treso,tresg,avamp,iacrit
C
C Set TRUE if tint(i) is resolvable (or unusable- assumed long)
	RESOLV=.TRUE.
c
	if(iampl0(i).eq.0) then
	   if(tint0(i).lt.tresg) resolv=.false.
	else
	   if(tint0(i).lt.treso) resolv=.false.
	endif
	RETURN
	END


	logical function DIFAMP(iamp1,iamp2)
	integer*2 iamp1,iamp2,iacrit
	common/RBLCK/treso,tresg,avamp,iacrit
C
C FALSE (i.e. amplitudes are 'same') if (1) both amps are zero
C OR (2) both are non-zero but differ by less than ACRIT (pA)
C
	DIFAMP=.true.
	if(iamp1.eq.0.and.iamp2.eq.0) DIFAMP=.false.
	if(iamp1.ne.0.and.iamp2.ne.0.and.
     & iabs(int4(iamp1-iamp2)).lt.iacrit) DIFAMP=.false.
	RETURN
	END


