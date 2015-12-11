	subroutine RESINT2(tint0,tint,ampl0,ampl,iprops0,iprops,nint,
     & nintt,imode,ffilt,fc,rms,trise,index,ihjc)
c	subroutine RESINT(TINT,NINTt,nint,acal,iavamp,imode,iampl,
c     & sublev,jfilt,fc,rms,trise,ihjc)
c
c RESINT2 is version of RESINT1 that has a new array, index(), defined
c so that index(i)=k where i=index of output arrays (tint etc, i=1,..,nint)
c and k=index of input arrays (tint0 etc, k=1,...,ninnt), so index in
c original data for interval #i in data (resolution imposed) used for fitting
c is index(i).
c
c Modified 12/01/94 02:48pm so looks at caplock key only every 100th interval
c  (so runs faster).
c
c VERSION FOR NEW EKDIST (for new SCAN files) 02/13/94 05:49pm
c TO DO:
c (1) iprops=2,4 not transferred to output at present
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
	real*4 tint0(nintt),ampl0(nintt)
	real*4 tint(nintt),ampl(nintt)
	integer*1 iprops0(nintt),iprops(nintt)
	integer*4 index(nintt)
c	dimension TINT(20480)		!up to 20 *1024
c	integer*2 iampl(20480)
	logical DEBUG,pon,slock,caplock,debon
c==	logical sublev
	logical btest,dubious,badgap,setdub,setbad
	logical RESOLV,DIFAMP		!FUNCTIONS
	logical sbin,shist,sres,sexp
c	integer*2 ktype
c	logical KBHIT		!Utility lib
c	character ch,getch
	common/sblk/sbin,shist,sres,sexp
	character*1 ans,UC
	logical discprt
	common/dp/discprt
c==	common/rblck/treso,tresg,iacrit,mavamp
	common/rblck/treso,tresg,acrit,avamp
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
	dubious(i)=BTEST(iprops0(i),0)	!ampl(i) was dubious (bit 0 set ='1')
	badgap(i)=BTEST(iprops0(i),3)	!tint(i) was unusable (bit 3='8' set)
c
101	format(a1)
c Initialise output arrays
	if(.not.sres) then
	   do i=1,nintt
		tint(i)=0.0
		ampl(i)=0.0
		iprops(i)=0
		index(i)=0
	   enddo
	endif
c
	if(imode.eq.2) then
	   sbin=.false.			!for SCDIR
	   shist=.false.
	   sexp=.false.
	   sres=.false.
c	   fc=0.5*float(jfilt)	!kHz (-3dB)
	   fc=ffilt			!kHz (-3dB)
	   trise=332.1/fc		!rise time (microsec)
	endif
c If call with SRES=true want another histogram with same resolution
c so TINT() already defined- just print values ,then go to 145
203	continue
	if(.not.sres) then
c Extra bit for glutamate etc
	call DCASK('Calculate resolution by Howe method','n',ans)
	 if(ans.eq.'Y') then
	   call AAMAXSUB(trise,treso,aamaxo)
	   treso=0.001*treso		!in ms
	   tresg=treso			!in ms
	   aamaxg=aamaxo
	   zo=1000.*treso/trise		!tres in ms, trise in mus
	   zg=zo
	   call FALSEV1(tres,fc,rms,avamp,frate)
c	   call FALSEV(treso,fc,rms,iavamp,acal,frato)
	   fratg=frato
	   goto 311	!back to normal prog to print results
	 endif
	endif
	if(imode.eq.0.or.imode.eq.2) then
	   if(.not.sres) then
	     x=treso*1000.
	     print 305,x		!default from .INI file
305        format(' Min resolvable open time (microsec) [',f8.2,'] = ')
	     call INPUTr(x)
	     treso=0.001*x		!in ms (default is in ms already)
	   endif
	   if(ffilt.lt.0.) goto 202
c Filter function for unfiltered signal has ffilt set to 0- use 5 kHz
	   f=ffilt
	   if(f.eq.0.) then
		f=5.
		ffilt=f
		print 22
		if(pon()) write(7,22)
		if(discprt) write(8,22)
22		format(
     & ' Unfiltered signal- fc set to 5kHz (-3dB) for risetime.')
	   endif
c Used only to calc 2.5*rise time for excluding short events
	   if(debug()) print 211,float(j),fc,rms,avamp
211	   format(
     & ' Filter(kHz),fc, RMS noise, mean amp= ',4g13.6)
	   call FALSEV1(treso,fc,rms,avamp,frato)
c	   call FALSEV1(treso,fc,rms,iavamp,acal,frato)
	   zo=1000.*treso/trise		!tres in ms, trise in mus
	   aamaxo=erfs(0.88604*zo)
202	   if(.not.sres) then
	     x=tresg*1000.
	     print 3051,x		!default from .INI file
3051	     format('&Min resolvable shut time (microsec) [',f8.2,'] = ')
	     call INPUTr(x)
	     tresg=0.001*x		!in ms (default is in ms already)
	   endif
	   if(ffilt.gt.0.) then
	    call FALSEV1(tresg,fc,rms,avamp,fratg)
	    zg=1000.*tresg/trise		!tres in ms, trise in mus
	    aamaxg=erfs(0.88604*zg)
	   endif
	else if(imode.eq.1) then	!HJCFIT
	   print 20,treso
20       format(
     & '&Resolution for open and shut times (microsec) [',f8.2,'] = ')
	   call INPUTr(treso)
	   treso=0.001*treso		!in ms (default is in ms already)
	   tresg=treso			!for HJCFIT
	   if(ffilt.gt.0.) then
	    call FALSEV1(treso,fc,rms,avamp,frato)
	    zo=1000.*treso/trise		!tres in ms, trise in mus
	    aamaxo=erfs(0.88604*zo)
	    call FALSEV1(tresg,fc,rms,avamp,fratg)
	    zg=1000.*tresg/trise		!tres in ms, trise in mus
	    aamaxg=erfs(0.88604*zg)
	   endif
	endif
311	continue
	if(.not.sres) then
	   print 201,frato,zo,aamaxo,fratg,zg,aamaxg
201	   format(
     & ' false event rate (per sec):',/,
     & '    for openings = ',g12.5,' (= ',g11.4,' risetimes, A/Amax = ',
     & g12.5,' )',
     & '   for shuttings= ',g12.5,' (= ',g11.4,' risetimes, A/Amax = ',
     & g12.5,' )',
     & ': O.K. [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') goto 203
	endif
	if(pon()) write(7,30) 1000.*treso,zo,aamaxo,1000.*tresg,zg,aamaxg
	if(discprt) write(8,30) 1000.*treso,zo,aamaxo,1000.*tresg,
     & zg,aamaxg
	if(sres) print 30,1000.*treso,zo,aamaxo,1000.*tresg,zg,aamaxg
30	format(' Resolution (microsec):',/,
     & '   for openings = ',f8.2,' (= ',g11.4,' risetimes, A/Amax = ',
     & g12.5,' )',/,
     & '   for shuttings= ',f8.2,' (= ',g11.4,' risetimes, A/Amax = ',
     & g12.5,' )',/)
	if(pon().and.ffilt.gt.0) write(7,32) frato,fratg
	if(discprt.and.ffilt.gt.0) write(8,32) frato,fratg
	if(sexp.and.ffilt.gt.0) print 32,frato,fratg
32	format(
     & '  false event rate (per sec) for openings  = ',g13.6,/,
     & '			      for shuttings = ',g13.6)
	call flush(7)
c
	if(treso.lt.0.00001.and.tresg.lt.0.00001) then
	   nint=nintt	!no resolution imposed
	   do i=1,nint
		tint(i)=tint0(i)	!copy input straight to output
		ampl(i)=ampl0(i)
		iprops(i)=iprops0(i)
	   enddo
	   goto 145
	endif
	if(sexp.and.sres) goto 145
	if(sexp) goto 307
c
c==	IF(.NOT.SUBLEV) GOTO 307
c==	MAVAMP=IAVAMP	!UNLESS RESET
	print 102,avamp
102	FORMAT(' Full amplitude (pA) = ',G13.6,'  O.K. [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') then
	   print 110
110	   format(' Full amp (pA, with sign) = ')
	   call INPUTr(avamp)
c==	   MAVAMP=IFIXR(X1/ACAL)	!FULL AMP READ IN, IN ADC UNITS
	endif
	acrit=abs(0.1*avamp)
	print 111,acrit
111	FORMAT('& pA for ''real'' amp difference [',f6.2,'] = ')
	call INPUTr(acrit)
c==	IACRIT=IFIX(0.5+ABS(X)/ACAL)	!IN ADC UNITS
	if(pon()) write(7,31) avamp,acrit
	if(discprt) write(8,31) avamp,acrit
31	format(' For sublevels take:',/,3x,
     & ' full amplitude (pA)= ',f8.2,'; pA for real difference= ',f6.3)
C
307	continue
C CHECK FOR NEG INTERVALS, AND SET THEM
C UNUSABLE SKIPPING THOSE ALREADY SET UNUSABLE.
	do i=1,nintt-1
	   if((.not.badgap(i)).and.tint0(i).lt.0.) then
c	   if((.not.BTEST(iprops0(i),3)).and.tint0(i).lt.0.) then
c	   IF((TINT(I).GT.-1000.1).AND.(TINT(I).LT.-999.9)) GOTO 106
c	   IF(TINT(I).GT.0.0) GOTO 106
	      call BELL(1)
	      print 107,i,tint0(i)
	      if(discprt) write(8,107) i,tint0(i)
107	      format('  ****tint(',i4,')= ',g13.6,'  reset as unusable')
		iprops0(i)=IBSET(iprops0(i),3)	!gap unusable; set bit 3 ='8'
c==	      TINT(I)=-1000.
	   endif
	enddo
c
c Last interval now set unusable in DISKIN2
c
	if(debug()) then
	   print 503
	   if(discprt) write(8,503)
503	   format(' Input',35x,' Output')
	endif
	if(sres) goto 610
	print 600
600	format('&Options for resolution:')
	print 602
602	format(
     & ' (1) Use HJC definition of e-openings and e-gaps')
	print 603
603	format(
     & ' (2) Consecutive short intervals set unusable (old def)')
	print 605,ihjc		!ihjc from input
605	format(' Option [',i2,'] = ')
	call INPUTi(ihjc)
610	if(ihjc.eq.1) then
         if(pon()) write(7,602)
         if(discprt) write(8,602)
	else
         if(pon()) write(7,603)
         if(discprt) write(8,603)
	endif
c
      print 60
60	format(
     & ' Put Caplock on to see values. Working . . . ')
c
c NOW IMPOSE RESOLUTION
	ki=0	!counts concatenated intervals
	i=0
	debon=debug()
C
C IF THE FIRST INTERVAL IS BOTH USABLE AND RESOLVABLE
C THIS IS STARTING POINT. IF NOT LOOK FOR FIRST INTERVAL THAT
C IS BOTH, AND IS PRECEDED BY AN RESOLVABLE INTERVAL TOO (OTHERWISE
C ITS START WILL BE DEFINED ONLY BY THE POSITION OF THE PRECEDING
C UNRESOLVABLE INTERVAL AND SO WILL BE UNRELIABLE)
	i=i+1
	if(RESOLV(i,tint0,ampl0,iprops0,nintt).and.
     &	(.not.badgap(i))) goto 2				!found it
c==	IF(RESOLV(I,TINT,iampl).AND.(TINT(I).GT.0.0)) GOTO 2  !FOUND IT
1	i=i+1
	if(RESOLV(i,tint0,ampl0,iprops0,nintt).and.
     &  RESOLV(i-1,tint0,ampl0,iprops0,nintt).and.
     &  (.not.badgap(i))) goto 2
c	IF(RESOLV(I,TINT,iampl).AND.RESOLV(I-1,TINT,iampl).AND.
c     & (TINT(I).GT.0.0)) GOTO 2
	goto 1
c
C NOW START TO LOOK FOR UNRESOLVABLE INTERVALS
2	ki=ki+1
c	if(KBHIT()) then
c		ch=GETCH(ktype)
c		deb=.not.deb
c		if(ktype.eq.0.and.ichar(ch).eq.59) deb=.not.deb		!F1 hit
c	endif
	if(mod(i,100).eq.0) then	!check if debug on every 100th transition
	   debon=debug()
	endif
c
	if(debon) then
	   print 501,i,tint0(i),ampl0(i)
         if(discprt) write(8,501)i,tint0(i),ampl0(i)
	endif
	call SUMAMP(-1,j,S,j1,s1,amp,.false.,setdub)	!initialise sums
	tint(ki)=tint0(i)			!start new concat group
	index(ki)=i			!define index
	if(badgap(i)) then	!transfer 'unusable'
	    iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
	endif
c===	dubious=BTEST(iprops0(i),0)		!bit 0 set (='1')
	call SUMAMP(1,j,S,j1,s1,ampl0(i),dubious(i),setdub)	!accumulate amps
c	call SUMAMP(I,J,S,J1,S1,iampl)		!ACCUMULATE AMPS
c is next interval resolvable?
7	continue
	if(i.eq.nintt) goto 4		!END
	if(debon) then
	   print 501,i+1,tint0(i+1),ampl0(i+1)
         if(discprt) write(8,501) i+1,tint0(i+1),ampl0(i+1)
501	   format(i8,g13.6,2x,g13.6)
	endif
	if(RESOLV(i+1,tint0,ampl0,iprops0,nintt)) goto 4  !yes: assign amp & go on
C Do next bit if NOT resolvable. Has the interval after it got the 'same' amp
C as that before it? Note that it is safe to refer to i+2 here
C because last interval is set unusable and so counted as resolvable always,
C so if i=nintt-1 we skip this section via last line above.
C  First check for 2 succesive irresolvable intervals (old method: not if
c ihjc=1)                                             
	if(debon) then
	   print 501,i+2,tint0(i+2),ampl0(i+2)
         if(discprt) write(8,501) i+2,tint0(i+2),ampl0(i+2)
	endif
c
	if(ihjc.eq.1) goto 71				!skip this bit
c Section for old resolution method (ihjc=2)
	if(RESOLV(i+2,tint0,ampl0,iprops0,nintt)) goto 71     !no -carry on
C   2 found so set unusable and look for next pair of resolvable intervals
C  before proceeding (old method, ihjc=2)
	iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
c	TINT(KI)=-1000.
	if(debon) then
	   print 501,i+1,tint0(i+1),ampl0(i+1)
         if(discprt) write(8,501)i+1,tint0(i+1),ampl0(i+1)
	endif
	i=i+3
61	continue
	if(debon) then
	   print 501,i,tint0(i),ampl0(i)
	   if(discprt) write(8,501)i,tint0(i),ampl0(i)
	endif
	if(RESOLV(i,tint0,ampl0,iprops0,nintt).and.
     & RESOLV(i+1,tint0,ampl0,iprops0,nintt).or.(i+1.ge.nintt)) goto 4
	i=i+1
	goto 61
c end of ihjc=2
C
c Back to regular method
71	tin=tint(ki)
	setbad=badgap(ki)			!tint(ki) was unusable
c==	setbad=BTEST(iprops(ki),3)	!tint(ki) was unusable
c==	IF(.NOT.SUBLEV) GOTO 9		!ALWAYS SAME AMP
	if(DIFAMP(ampl0(i),ampl0(i+2))) goto 5
c
c Now bit to be done if tint0(i+1) unresolvable and intervals
c on each side have the 'same' amp. Concatenate.
9	tint(ki)=tin + tint0(i+1) + tint0(i+2)
c if any of these intervals unusable set whole group unusable
c==	setbad=setbad.or.BTEST(iprops0(i+1),3).or.BTEST(iprops0(i+2),3)
	setbad=setbad.or.badgap(i+1).or.badgap(i+2)
	if(setbad) then
	   iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
	endif
c==	IF((TIN.LT.-900.).OR.(TINT(I+1).LT.-900.).OR.
c==     & (TINT(I+2).LT.-900.)) TINT(KI)=-1000.
	call SUMAMP(1,j,S,j1,s1,ampl0(i+2),dubious(i+2),setdub)	!accumulate amps
	i=i+2
	goto 7	!continue concatenating until resolvable interval found
c
c NEXT bit done if tint(i+1) unresolvable but intervals on each
c side have 'different' amps.
5	continue
	tin1=tint0(i+1)
	tint(ki)=tin + 0.5*tin1
c==	setbad=setbad.or.BTEST(iprops0(i+1),3)	!tin or tin1 was unusable
	setbad=setbad.or.badgap(i+1)		!tin or tin1 was unusable
	if(setbad) then
	   iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
	endif
c==	IF((TIN.LT.-900.).OR.(TIN1.LT.-900.)) TINT(KI)=-1000.
	tint(ki+1)=0.5*tin1 + tint0(i+2)
	index(ki+1)=i+2			!define index
	setbad=badgap(i+1).or.badgap(i+2)	!tin1 or tint0(i+2) was unusable
c==	IF((TINT(I+2).LT.-900.).OR.(TIN1.LT.-900.)) TINT(KI+1)=-1000.
	if(setbad) then
	   iprops(ki+1)=IBSET(iprops(ki+1),3)	!gap unusable; set bit 3 ='8'
	endif
c
c   Concat of tint(ki) now ended so assign amp to it
	if(j.gt.0) then
	   ampl(ki)=S/float(j)
	   if(setdub) iprops(ki)=IBSET(iprops(ki),0)	!bit 0 set (='1')
	else if(j1.gt.0) then
	   ampl(ki)=S1/float(j1)
	   if(setdub) iprops(ki)=IBSET(iprops(ki),0)	!bit 0 set (='1')
	endif
c   tint(ki+1) (approx=tint0(i+2)) may need more things concat with it
c   so initialise sums (and setdub) and start new ones.
	call SUMAMP(-1,j,S,j1,s1,amp,.false.,setdub)	!initialise sums
	call SUMAMP(1,j,S,j1,s1,ampl0(i+2),dubious(i+2),setdub)	!accumulate amps
	ki=ki+1
	i=i+2
	goto 7		!CARRY ON CONCAT
C
C Assign amp if concatenation finished
4	continue
c==	IF(J1.GT.0) IAMPL(KI)=IFIXR(S1/FLOAT(J1))		!ASSUMED MEAN
c==	IF(J.GT.0) IAMPL(KI)=IFIXR(S/FLOAT(J))		!MEASURED MEAN
	if(j.gt.0) then
	   ampl(ki)=S/float(j)
	   if(setdub) iprops(ki)=IBSET(iprops(ki),0)	!bit 0 set (='1')
	else if(j1.gt.0) then
	   ampl(ki)=S1/float(j1)
	   if(setdub) iprops(ki)=IBSET(iprops(ki),0)	!bit 0 set (='1')
	endif
	i=i+1
	if(debon) then
	   print 502,ki,tint(ki),ampl(ki)
	   if(discprt) write(8,502)ki,tint(ki),ampl(ki)
502	   format(30x,i8,g13.6,2x,g13.6)
	endif
	if(i.le.nintt) goto 2
c
c
	nint=ki
c	if(imode.eq.2) RETURN		!for SCDIR
145	continue
	print 289,NINT
	if(pon()) write(7,289) NINT
	if(discprt) write(8,289) NINT
289	FORMAT( ' Number of resolved intervals= ',I8,/)
	  if(debug()) then
25		print 23
23		format(' Print intervals and amplitudes # n1,n2 [skip] = ')
		n1=0
		call INPUT2i(n1,n2)
		if(n1.gt.0) then
		  if(n2.lt.n1) n2=n1
		  do i=n1,n2
		     print 26,i,tint(i),ampl(i),iprops(i),index(i)
		     if(discprt) write(8,26) i,tint(i),ampl(i),iprops(i),
     &		index(i)
26		     format(i6,g13.6,2x,g13.6,2x,i6,2x,i8)
		  enddo
		  goto 25
		endif
	  endif
c
	RETURN
	END


	subroutine SUMAMP(imode,j,S,j1,s1,amp,dubious,setdub)
C TO SUM AMPLITUDES TO CALC MEAN AMP OF A CONCATENATED GROUP
C (MEASURED AMPS IN S, ASSUMED IN S1). IF imode=-1 IT INITIALISES.
c Initially setdub=false, but if any dubious amplitudes encountered
c before next initialisation, setdub=true so can be used to set
c averaged amplitude as dubious
	logical dubious,setdub
c
	if(imode.lt.0) then   !initialise only
	   j=0
	   S=0.0
	   j1=0
	   S1=0.0
	   setdub=.false.
	   RETURN
	endif
c
	if(dubious) then
	   j1=j1+1
	   S1=S1+amp
	   setdub=.true.
	else
	   j=j+1
	   S=S+amp
	endif
c
	RETURN
	end


	logical function RESOLV(i,tint0,ampl0,iprops0,nintt)
c	logical function RESOLV(I,TINT,iampl)
	real*4 tint0(nintt),ampl0(nintt)
	integer*1 iprops0(nintt)
	logical btest
	common/rblck/treso,tresg,acrit,avamp
C
C Set TRUE if tint(i) is resolvable (or unusable- assumed long)
	RESOLV=.TRUE.
	if(BTEST(iprops0(i),3)) RETURN 	!unusable
c
	if(ampl0(i).eq.0) then
	   if(tint0(i).lt.tresg) resolv=.false.
	else
	   if(tint0(i).lt.treso) resolv=.false.
	endif
	RETURN
	END


	logical function DIFAMP(amp1,amp2)
	common/rblck/treso,tresg,acrit,avamp
C
C FALSE (i.e. amplitudes are 'same') if (1) both amps are zero
C OR (2) both are non-zero but differ by less than ACRIT (pA)
C
	DIFAMP=.true.
	if(amp1.eq.0.and.amp2.eq.0) DIFAMP=.false.
	if(amp1.ne.0.and.amp2.ne.0.and.
     & abs(amp1-amp2).lt.acrit) DIFAMP=.false.
	RETURN
	END


