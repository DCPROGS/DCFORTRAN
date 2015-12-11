	subroutine RESINT3(tint0,tint,ampl0,ampl,iprops0,iprops,nint,
     & nintt,imode,ffilt,fc,rms,trise,index,ihjc,
     & cjump,nsweep,kjumps0,kjumps)
c
c RESINT3 is version of resint2 that can deal with c-jumps.
c===================REMOVE tres=0 FOR NOW, TO TEST
c
c  For CJUMP need special treatment for first, and for last (if not null),
c  intervals.  Number of jumps (and hence jumps() array) not changed by
c  imposition of resolution, though if a jump contains any unusable (bit 3='8')
c  intervals may choose to treat whole jump as bad?).  Also possible that
c  a jump may become null (e.g. if all openings in it are < tres).  However
c  the index of the first opening of each sweep=kjumps() for output will
c  generally differ from that of input=kjumps0().
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
c Note:
c (1) iprops=2,4 (amp fixed, amp constrained) not transferred to output
c (in fact not used at all ay present)
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
	logical DEBUG,pon,slock,caplock,debon
	logical btest,dubious,badgap,setdub,setbad
	logical RESOLV,DIFAMP		!FUNCTIONS
	logical sbin,shist,sres,sexp
c For cjumps
	logical cjump,null,fbad,difamp2,bad2,flag
	integer*2 kjumps0(nsweep),kjumps(nsweep)
c
	common/sblk/sbin,shist,sres,sexp
	character*1 ans,UC
	logical discprt
	common/dp/discprt
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
	   print 24,treso
24       format(
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
30	format(/,' Resolution (microsec):',/,
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
c===================REMOVE FOR NOW, TO TEST
c/	if(treso.lt.0.00001.and.tresg.lt.0.00001) then
c/	   nint=nintt	!no resolution imposed
c/	   do i=1,nint
c/		tint(i)=tint0(i)	!copy input straight to output
c/		ampl(i)=ampl0(i)
c/		iprops(i)=iprops0(i)
c/	   enddo
c/	   goto 145
c/	endif
	if(sexp.and.sres) goto 145
	if(sexp) goto 307
c
	print 102,avamp
102	FORMAT(' Full amplitude (pA) = ',G13.6,'  O.K. [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') then
	   print 110
110	   format(' Full amp (pA, with sign) = ')
	   call INPUTr(avamp)
	endif
	acrit=abs(0.1*avamp)
	print 111,acrit
111	FORMAT('& pA for ''real'' amp difference [',f6.2,'] = ')
	call INPUTr(acrit)
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
	      call BELL(1)
	      print 107,i,tint0(i)
	      if(discprt) write(8,107) i,tint0(i)
107	      format('  ****tint(',i4,')= ',g13.6,'  reset as unusable')
		iprops0(i)=IBSET(iprops0(i),3)	!gap unusable; set bit 3 ='8'
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
c	print 600
c600	format('&Options for resolution:')
c	print 602
602	format(
     & ' (1) Use HJC definition of e-openings and e-gaps')
c	print 603
603	format(
     & ' (2) Consecutive short intervals set unusable (old def)')
c	print 605,ihjc		!ihjc from input
c605	format(' Option [',i2,'] = ')
c	call INPUTi(ihjc)
	ihjc=1		!HJC always
610	if(ihjc.eq.1) then
         if(pon()) write(7,602)
         if(discprt) write(8,602)
	else
         if(pon()) write(7,603)
         if(discprt) write(8,603)
	endif
c
	ndirect=0	!count # of direct trans introduced by elim of short gaps
	nconcat=0	!count number of adjacent amps<acrit are concatenated
c
      print 60
60	format(
     & ' Put CAPLOCK on to see values. Working . . . ')
c
c NOW IMPOSE RESOLUTION
	if(.not.cjump) goto 1001
c First code for cjump case
c  NB this is like code for regular case but de-spaghettified (and modif
c for jumps)
	ki=0	!index for concatenated intervals
c==	i=0	!index for original intervals ==set via kjumps() for each sweep
c
	kjumps(1)=1		!index in output tint() of 1st interval of 1st jump
c
	debon=debug()
c
	do 1000 ij=1,nsweep
	if(ij.gt.1) then
	   kjumps(ij)=ki+1  !index in output tint() of 1st interval of ijth jump
	endif
	i=kjumps0(ij)	 		!index in tint0() of 1st interval in jth jump
	ifirst=i
	if(ij.lt.nsweep) then
	   ilast=kjumps0(ij+1)-1	!index of last transition in ijth jump
	else
	   ilast=nintt
	endif
c First, skip null sweeps -put them straight into output
	if(i.eq.ilast) then
	   null=BTEST(iprops0(i),5)
	else
	   null=BTEST(iprops0(i),5).and.BTEST(iprops0(i+1),5)
	endif
	if(null) then
	   ki=ki+1
	   tint(ki)=tint0(i)
	   iprops(ki)=iprops0(i)	!=bit 5 = '32'
	   ampl(ki)=ampl0(i)
	   index(ki)=i			!define index
	   nnull=nnull+1
	   if(debon) then
		print 501,i,tint0(i),ampl0(i),iprops0(i)
       	if(discprt) write(8,501) i,tint0(i),ampl0(i),iprops0(i)
	      print 505,ki,tint(ki),ampl(ki),iprops(ki),index(ki)
	      if(discprt) write(8,505)ki,tint(ki),ampl(ki),iprops(ki),
     &	index(ki)
505	      format(30x,' out = ',i6,g12.5,g12.5,i3,i6, '(null)')
	   endif
	   goto 1000			!this sweep done
	endif
c Now start on intervals when sweep is not null.  First check that latency is
c not shorter than tres (i=kjumps(ij)=index of latency for jump #ij, set above).
c If latency is shut, less than tresg, and followed by resolvable opening then
c latency becomes opening (and vice versa) (cannot just skip over first
c interval if it is bad or unresolvable, as is done normally).  If more
c than one iresolvable interval follows t=0 then concat them as usual
c to get 1st latency.  Otherwise, when 1st latency is resolvable, just
c concatenate first latency with subsequent intervals as usual, and set
c bit 5 ('32') in the result. Problem is that in regular case (and in HJC
c definition) a concat interval always starts with a resolvable event
	if(.not.RESOLV(ifirst,tint0,ampl0,iprops0,nintt).and.
     &	RESOLV(ifirst+1,tint0,ampl0,iprops0,nintt)) then
c		Start a (possibly) concat group at 2nd (i+1) interval (but
c		add in duration of the irresolvable orig latency)
	   fbad=.true.	!1st latency below tres, but next interval resolvable
	else
	   fbad=.false.
	endif
c Now start concat for cjump case
20	ki=ki+1
	if(mod(i,100).eq.0) then	!check if debug on every 100th transition
	   debon=debug()
	endif
	if(debon) then
	   print 501,i,tint0(i),ampl0(i),iprops0(i)
         if(discprt) write(8,501)i,tint0(i),ampl0(i),iprops0(i)
	   if(i.eq.ilast) then	!i+2 not printed in this case
		if(debon) then
		   print 504,ij
      	   if(discprt) write(8,504) ij
504		   format('    (last interval in jump # ',i5,')')
		endif
	   endif
	endif
c
	call SUMAMP(-1,j,S,j1,s1,amp,.false.,setdub)	!initialise sums
	tint(ki)=tint0(i)			!start new concat group
	index(ki)=i			!define index
	if(i.eq.ifirst) then
	   iprops(ki)=IBSET(iprops(ki),5)	!1st latency; set bit 5 ='32'
	   if(fbad) then
		i=i+1					!move to 2nd (resolvable) interval
		tint(ki)=tint(ki)+tint0(i)	!concat with 2nd interval
		fbad=.false.
		if(debon) then
		   print 501,i,tint0(i),ampl0(i),iprops0(i)
      	   if(discprt) write(8,501)i,tint0(i),ampl0(i),iprops0(i)
		endif
	   endif
	endif
	if(badgap(i)) then	!transfer 'unusable'
	    iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
	endif
	call SUMAMP(1,j,S,j1,s1,ampl0(i),dubious(i),setdub)	!accumulate amps
c is next interval resolvable?
70	continue
	if(i.eq.ilast) then
	   if(BTEST(iprops(i),5)) then	!already set as first, so null sweep
		goto 1000				!straight on to next sweep
	   else
		iprops(ki)=IBSET(iprops(ki),6)	!set as last; set bit 6 ='64'
	      goto 40		!END
	   endif
	endif
c for i=ilast-1 next line prints ilast values
	if(debon) then
	   print 501,i+1,tint0(i+1),ampl0(i+1),iprops0(i+1)
         if(discprt) write(8,501) i+1,tint0(i+1),ampl0(i+1),iprops0(i+1)
	endif
c==	if(i.eq.nintt) goto 40		!END
c=	if(RESOLV(i+1,tint0,ampl0,iprops0,nintt)) then   !yes: assign amp & go on
c Need sep bit here to deal with i=ilast-1 (so next interval is last in
c the jump) -because last interval is NOT nec resolvable for jump. If it is
c not resolvable then last interval becomes the preceding one, which must
c therefore have bit 6='64' set (unless it is also the first interval,
c which could happen only if all events in the sweep were below res so
c this sweep becomes null after resolution imposed!)
c
	if(.not.RESOLV(i+1,tint0,ampl0,iprops0,nintt)) then   !no (if yes goto 4)
C Do next bit if NOT resolvable. Has the interval after it got the 'same' amp
C as that before it?
c At this point i=anything up to i=ilast-1. For jumps it makes no sense
c to refer to i+2=ilast+1 which will be first interval of next jump
	   if(i.lt.ilast-1) then
		tin2=tint0(i+2)
		bad2=badgap(i+2)
		DIFAMP2=DIFAMP(ampl0(i),ampl0(i+2))
		if(debon) then
		   print 501,i+2,tint0(i+2),ampl0(i+2),iprops0(i+2)
      	   if(discprt) write(8,501) i+2,tint0(i+2),ampl0(i+2),
     &	   iprops0(i+2)
		endif
	   else	!i=ilast-1, so i+1=ilast, i+2 irrelevant
		tin2=0.
		bad2=.false.
		DIFAMP2=.false.
	   endif
c
	   tin=tint(ki)
	   setbad=badgap(ki)			!tint(ki) was unusable
c==	   if(DIFAMP(ampl0(i),ampl0(i+2))) goto 5
c==	   if(DIFAMP(ampl0(i),ampl0(i+2))) then
	   if(DIFAMP2) then
c NEXT bit done if tint(i+1) unresolvable but intervals on each
c side have 'different' amps (not done for i=ilast-1 because
c difamp2 set false above, so OK to refer to i+2 here)
c=5		continue
		tin1=tint0(i+1)
     		tint(ki)=tin + 0.5*tin1
		setbad=setbad.or.badgap(i+1)		!tin or tin1 was unusable
		if(setbad) then
		   iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
		endif
		if(j.gt.0) then
		   ampl(ki)=S/float(j)
		   if(setdub) iprops(ki)=IBSET(iprops(ki),0)	!bit 0 set (='1')
		else if(j1.gt.0) then
		   ampl(ki)=S1/float(j1)
		   if(setdub) iprops(ki)=IBSET(iprops(ki),0)	!bit 0 set (='1')
		endif
c         Now finished tint(ki)=value before the newly-formed direct open-open
c		transition, so print output here and start the (possibly concat)
c		level after it =tint(ki+1)
		if(ampl(ki).ne.0.and.ampl0(i+2).ne.0) then
		   ndirect=ndirect+1	!count # of direct trans introduced
		endif
		if(debon) then
		   print 507,ki,tint(ki),ampl(ki),iprops(ki),index(ki)
		   if(discprt) write(8,507)ki,tint(ki),ampl(ki),iprops(ki),
     &		index(ki)
507	  	   format(30x,' out = ',i6,g12.5,g12.5,i3,i6,' (diff amp)')
		endif
		tint(ki+1)=0.5*tin1 + tint0(i+2)
		index(ki+1)=i+2			!define index
		setbad=badgap(i+1).or.badgap(i+2)	!tin1 or tint0(i+2) was unusable
		if(setbad) then
		   iprops(ki+1)=IBSET(iprops(ki+1),3)	!gap unusable; set bit 3 ='8'
		endif
c   tint(ki+1) (approx=tint0(i+2)) may need more things concat with it
c   so initialise sums (and setdub) and start new ones.
		call SUMAMP(-1,j,S,j1,s1,amp,.false.,setdub)	!initialise sums
		call SUMAMP(1,j,S,j1,s1,ampl0(i+2),dubious(i+2),setdub)	!accumulate amps
		ki=ki+1
		i=i+2
	      goto 70  !continue concatenating until resolvable interval found
	   else		!difamp=false
c Now bit to be done if tint0(i+1) unresolvable and intervals
c on each side have the 'same' amp. Concatenate.  May be done for i=ilast-1
c so cannot refer to i+2 (use tin2, bad2 defined above)
c=		tint(ki)=tin + tint0(i+1) + tint0(i+2)
		tint(ki)=tin + tint0(i+1) + tin2
		nconcat=nconcat+1
c if any of these intervals unusable set whole group unusable
c=		setbad=setbad.or.badgap(i+1).or.badgap(i+2)
		setbad=setbad.or.badgap(i+1).or.bad2
		if(setbad) then
		   iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
		endif
		if(i.lt.ilast-1) then
		   call SUMAMP(1,j,S,j1,s1,ampl0(i+2),dubious(i+2),setdub)	!accumulate amps
		   i=i+2
		   goto 70	!continue concatenating until resolvable interval found
		endif		!otherwise carry on to 40
	   endif
	endif
C Assign amp if concatenation finished
40	continue
	if(j.gt.0) then
	   ampl(ki)=S/float(j)
	   if(setdub) iprops(ki)=IBSET(iprops(ki),0)	!bit 0 set (='1')
	else if(j1.gt.0) then
	   ampl(ki)=S1/float(j1)
	   if(setdub) iprops(ki)=IBSET(iprops(ki),0)	!bit 0 set (='1')
	endif
	if(debon) then
	   print 502,ki,tint(ki),ampl(ki),iprops(ki),index(ki)
	   if(discprt) write(8,502)ki,tint(ki),ampl(ki),iprops(ki),
     &	index(ki)
	endif
	i=i+1
	if(i.le.ilast) goto 20
c	if(i.le.nintt) goto 20
1000	continue		!end of ij=1,nsweep loop
	if(debon) then
	   do i=1,nsweep
	      print 506,i,kjumps(i)
      	if(discprt) write(8,506) i,kjumps(i)
506		format('  kjumps(',i6,') = ',i6)
	   enddo
	endif
c Count null sweeps etc
	nint=ki
	nnull=0	!count null sweeps
	nop1=0	!count number of sweeps that start with channel open
	nopn=0	!count number of sweeps that end with channel open
	nord=0	!counts those for which none of above are true
	do i=1,nsweep
	   flag=.false.	!set if any of above is true
	   ifirst=kjumps(i)	!index in tint() of 1st interval in ith jump
	   if(i.lt.nsweep) then
		ilast=kjumps(i+1)-1	!index of last transition in ith jump
	   else
		ilast=nint
	   endif
	   if(ifirst.eq.ilast) then
		nnull=nnull+1
		flag=.true.
	   endif
	   if(ampl(ifirst).ne.0.) then
		nop1=nop1+1
		flag=.true.
	   endif
	   if(ampl(ilast).ne.0.) then
		nopn=nopn+1
		flag=.true.
	   endif
	   if(.not.flag) then
		nord=nord+1
	   endif
	enddo
      print 50,nsweep,nnull,nop1,nopn,nord
      if(pon()) write(7,50) nsweep,nnull,nop1,nopn,nord
      if(discprt) write(8,50) nsweep,nnull,nop1,nopn,nord
50	format(
     & ' Number of sweeps = ',i6,/,/,
     & ' Number of null sweeps = ',i6,/,
     & ' Number of sweeps with channel open at t=0 = ',i6,/,
     & ' Number of sweeps with channel open at end = ',i6,/,
     & ' Number of for which none of these above are true = ',i6,/)
c
	goto 1002		!skip regular case
c
c Concatenate intervals (REGULAR CASE)
1001	continue		!jump here for regular case
	ki=0	!counts concatenated intervals
	i=0
	ndirect=0	!count # of direct trans introduced by elim of short gaps
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
1	i=i+1
	if(RESOLV(i,tint0,ampl0,iprops0,nintt).and.
     &  RESOLV(i-1,tint0,ampl0,iprops0,nintt).and.
     &  (.not.badgap(i))) goto 2
	goto 1
c
C NOW START TO LOOK FOR UNRESOLVABLE INTERVALS
c
2	ki=ki+1
c
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
	   print 501,i,tint0(i),ampl0(i),iprops0(i)
         if(discprt) write(8,501)i,tint0(i),ampl0(i),iprops0(i)
	endif
	call SUMAMP(-1,j,S,j1,s1,amp,.false.,setdub)	!initialise sums
	tint(ki)=tint0(i)			!start new concat group
	index(ki)=i			!define index
	if(badgap(i)) then	!transfer 'unusable'
	    iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
	endif
	call SUMAMP(1,j,S,j1,s1,ampl0(i),dubious(i),setdub)	!accumulate amps
c is next interval resolvable?
7	continue
	if(i.eq.nintt) goto 4		!END
	if(debon) then
	   print 501,i+1,tint0(i+1),ampl0(i+1),iprops0(i+1)
         if(discprt) write(8,501) i+1,tint0(i+1),ampl0(i+1),iprops0(i+1)
501	   format(i6,g12.5,g12.5,i5)
	endif
	if(RESOLV(i+1,tint0,ampl0,iprops0,nintt)) goto 4  !yes: assign amp & go on
C Do next bit if NOT resolvable. Has the interval after it got the 'same' amp
C as that before it? Note that it is safe to refer to i+2 here
C because last interval is set unusable and so counted as resolvable always,
C so if i=nintt-1 we skip this section via last line above.
C  First check for 2 succesive irresolvable intervals (old method: not if
c ihjc=1)
	if(debon) then
	   print 501,i+2,tint0(i+2),ampl0(i+2),iprops0(i+2)
         if(discprt) write(8,501) i+2,tint0(i+2),ampl0(i+2),iprops0(i+2)
	endif
c (old method removed from here)
	tin=tint(ki)
	setbad=badgap(ki)			!tint(ki) was unusable
	if(DIFAMP(ampl0(i),ampl0(i+2))) goto 5
c
c Now bit to be done if tint0(i+1) unresolvable and intervals
c on each side have the 'same' amp. Concatenate.
	tint(ki)=tin + tint0(i+1) + tint0(i+2)
	nconcat=nconcat+1
c if any of these intervals unusable set whole group unusable
	setbad=setbad.or.badgap(i+1).or.badgap(i+2)
	if(setbad) then
	   iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
	endif
	call SUMAMP(1,j,S,j1,s1,ampl0(i+2),dubious(i+2),setdub)	!accumulate amps
	i=i+2
	goto 7	!continue concatenating until resolvable interval found
c
c NEXT bit done if tint(i+1) unresolvable but intervals on each
c side have 'different' amps.
5	continue
	tin1=tint0(i+1)
	tint(ki)=tin + 0.5*tin1
	setbad=setbad.or.badgap(i+1)		!tin or tin1 was unusable
	if(setbad) then
	   iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
	endif
c         Now finished tint(ki)=value before the newly-formed direct open-open
c		transition, so print output here and start the (possibly concat)
c		level after it =tint(ki+1)
	if(ampl(ki).ne.0.and.ampl0(i+2).ne.0) then
	   ndirect=ndirect+1	!count # of direct trans introduced
	endif
	if(debon) then
	   print 507,ki,tint(ki),ampl(ki),iprops(ki),index(ki)
	   if(discprt) write(8,507)ki,tint(ki),ampl(ki),iprops(ki),
     &	index(ki)
c507	   format(30x,' out = ',i6,g12.5,g12.5,i3,i6,' (diff amp)')
	endif
	tint(ki+1)=0.5*tin1 + tint0(i+2)
	index(ki+1)=i+2			!define index
	setbad=badgap(i+1).or.badgap(i+2)	!tin1 or tint0(i+2) was unusable
	if(setbad) then
	   iprops(ki+1)=IBSET(iprops(ki+1),3)	!gap unusable; set bit 3 ='8'
	endif
c
c   Concat of tint(ki) now ended so assign amp to it
10	if(j.gt.0) then
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
	if(j.gt.0) then
	   ampl(ki)=S/float(j)
	   if(setdub) iprops(ki)=IBSET(iprops(ki),0)	!bit 0 set (='1')
	else if(j1.gt.0) then
	   ampl(ki)=S1/float(j1)
	   if(setdub) iprops(ki)=IBSET(iprops(ki),0)	!bit 0 set (='1')
	endif
	i=i+1
	if(debon) then
	   print 502,ki,tint(ki),ampl(ki),iprops(ki),index(ki)
	   if(discprt) write(8,502)ki,tint(ki),ampl(ki),iprops(ki),
     &	index(ki)
502	   format(30x,' out = ',i6,g12.5,g12.5,i3,i6)
	endif
	if(i.le.nintt) goto 2
c
c
1002	continue		!finished concatenation
	nint=ki
c	if(imode.eq.2) RETURN		!for SCDIR
145	continue
c Check number unusable or dubious
	nb1=0
	nb2=0
	do i=1,nint-1
	   if(dubious(i)) nb1=nb1+1
	   if(badgap(i)) nb2=nb2+1
	enddo
c
	print 289,NINT,ndirect,nconcat,nb1,nb2
	if(pon()) write(7,289) NINT,ndirect,nconcat,nb1,nb2
	if(discprt) write(8,289) NINT,ndirect,nconcat,nb1,nb2
289	FORMAT( ' Number of resolved intervals= ',i8,/,' ',i4,
     & ' direct transitions introduced be removal of short gaps',/,
     & 1x,i4,' concatenations because amplitude ''equal'' on each side'
     & ' of irresolvable gap',/,
     & ' ',i4,' intervals with dubious amplitudes',/,
     & ' ',i4,' intervals with undefined durations',/)
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


