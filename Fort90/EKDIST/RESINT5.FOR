	subroutine RESINT5(tint0,tint,ampl0,ampl,iprops0,iprops,nint,
     & nintt,imode,ffilt,fc,rms,trise,index,
     & cjump,nsweep,kjumps0,kjumps)
c
c Resint5 is version in which kjumps0 and kjumps are integer*4
c NB any changes must be made (a) in both jump and steady state sections
c AND (b) in RESINT51 (used in HJCFIT)
c
c RESINT4  is completely revised version
c	(1) Adjacent openings, when 2nd opening resolvable, are now concatenated
c	   (as for open periods) when amplitudes are 'same' (within Acrit), and
c	   number of such concat (=nconc3) is printed.
c	(2) Default Acrit changed to ***
c	(3) All amplitudes of concatenated intervals are mean amplitudes
c	   weighted with their duration.
c	(4) Property #1 (amp dubious), and #8 (duration unusable) are
c	   transferred to the concatenated group if one ore more of
c	   the intervals in the group has these properties.
c	(5) Property #2 (amp fixed) and #3 (amp constrained) are transferred
c	   only if there has been no concatenation and interval is same as
c	   in the original data
c	(6) Uses only HJC method (ihjc removed)
c
c  Modif 11/19/97 06:51am to correct small error in handling of consecutive
c   unresolvable events. In order to comply with HJC definition, consider
c   an opening (say) followed by an unresolved shut time and then an
c   open time of 'different' amplitude from the first.  If the last open time
c   is resolvable it should end the concatenation, but if not resolvable
c   the concatenation should continue -last bit was absent but now inserted
c   (only effect was to allow a very small number of events shorter than
c   the resolution to appear in output).
c
c  Modif 04/08/97 05:45pm by adding common/censor/excamp,alo,ahi, so if
c  called with sres=true, the results are censored as for first call
c
c  DIFAMP fixed 10/11/96 10:53am to use .le.acrit, which is necessary if acrit=0
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
c is index(i) -this is indexin orig data of the first interval of a concatenated
c group that forms interval #i after resolution imposed.
c
c Modified 12/01/94 02:48pm so looks at caplock key only every 100th interval
c  (so runs faster).
c
c VERSION FOR NEW EKDIST (for new SCAN files) 02/13/94 05:49pm
c Note:
c (1) iprops=2,4 (amp fixed, amp constrained) not transferred to output
c (in fact not used at all at present)
c New notes:
c (1) no longer uses sublev, jfilt
c (2) now has separate input arrays (tint0 etc) and output (tint etc):
c	former used to hold orig data (no resolution imposed) so need not re-read
c (3) avamp now only in common (not a parameter)
c  NB
c integer*1 iprops(i) holds 'properties' of i'th duration and amp
c (integer*1 has range -128 to +127 (bit 7 set gives -128; can use bits 0-6)
c	 0=all OK;
c	+1=amplitude dubious = bit 0;
c	+2=amplitude fixed = bit 1;
c	+4 amplitude of opening constrained (see fixamp) = bit 2;
c	+8=duration unusable = bit 3;
c	etc
c and keep sum of values of more than one property is true
c ie set bits using VAXIBM3 library functions (see TBIT.FOR)
c	IBSET(iprops,i)	-sets bit #i=0,...,7   (integer*4 function)
c	IBCLR(iprops,i)	-clears bit #i=0,...,7  (integer*4 function)
c	BTEST(iprops,i)   -=true if bit #i is set (logical function)
c
c OLD NOTES:
c Modif 03/11/92 01:57pm to allow HJC method of imposition of resolution
c OR original method (in which contigous intervals below resolution get
c set bad).
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
	integer*1 iprops0(nintt),iprops(nintt),ip1
	integer*4 index(nintt)
	logical DEBUG,pon,slock,caplock,debon
	logical btest,dubious,badgap0,badgap,setdub,setbad,setend
	logical RESOLV,DIFAMP		!FUNCTIONS
	logical diffamp
	logical ifall,shutint
	logical sbin,shist,sres,sexp
c For cjumps
	logical cjump,null,fbad,flag
	integer*4 kjumps0(nsweep),kjumps(nsweep)
	logical excamp,shut,shut1,open2,exc2,setexc
c
	common/sblk/sbin,shist,sres,sexp
	character*1 ans,UC
	logical discprt
	common/dp/discprt
	common/rblck/treso,tresg,acrit,avamp
	common/censor/excamp,alo,ahi
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
	dubious(idum)=BTEST(iprops0(idum),0)	!ampl0(i) was dubious (bit 0 set ='1')
	badgap0(idum)=BTEST(iprops0(idum),3)	!tint0(i) was unusable (bit 3='8' set)
	badgap(idum)=BTEST(iprops(idum),3)	!tint(i) was unusable (bit 3='8' set)
c
101	format(a1)
	setend=.false.	!used only if last interval in consam not resolvable
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
	excamp=.false.
	ifall=.true.
	if(.not.sres) then
c Extra bit for glutamate etc
36	 iopt=1
	 if(cjump) then
	    print 34,iopt
34	    format(/,
     & ' (1) Carry on',/,
     & ' (2) Exclude all openings in a specified amplitude window',/,
     & ' Option number (0=help) [',i2,'] = ')
	 else
	    print 341,iopt
341	    format(/,
     & ' (1) Carry on',/,
     & ' (2) Exclude all openings in a specified amplitude window',/,
     & ' (3) Calculate resolution to exclude sublevels (Howe method)',/,
     & ' Option number (0=help) [',i2,'] = ')
	 endif
	 call INPUTi(iopt)
	 if(cjump.and.iopt.eq.3) goto 36	!not yet fixed
	 if(iopt.eq.0) then
		print 5
5		format(
     &' Shut times separated by one or more openings that are all',/,
     &' within the specified amplitude range are concatenated',/,
     &' with each other, and the openings, into one long shut time')
		goto 36
	 endif
	 if(iopt.eq.2) then
	    excamp=.true.
	    ifall=.true.
	    alo=-50.
	    ahi=-50.
	    print 35
35	    format(
     &' Treat all openings with amplitudes between a1 and a2',/,
     &'  as irresolvable: a1, a2 (with sign) = ')
	    call INPUT2r(alo,ahi)
	    if((abs(alo).gt.20.).or.(abs(ahi).gt.20.)) then
		call BELL(3)
		goto 36
	    endif
	    if((alo.eq.0.).or.(ahi.eq.0.).or.(alo*ahi).lt.-0.0001) then
		call BELL(3)
		print 770
770		format(' Range must NOT include zero')
		goto 36
	    endif
	    if(alo.gt.ahi) then
		a=alo
		alo=ahi
		ahi=a
	    endif
	    i1=2
	    if(ifall) i1=1
	    print 511,i1
511	       format(/,
     &' (1) Concatenate if ALL consec openings are in range',/,
     &' (2) Concatenate if ANY consec openings are in range',/,
     &'  Option number [',i2,'] = ')
	    call INPUTi(i1)
	    if(i1.eq.1) then
		 ifall=.true.
	       print 51,alo,ahi
	       if(discprt) write(8,51) alo,ahi
51	       format(/,
     &' Shut times that are separated by one or more openings with',/,
     &' ALL amplitudes between ', g13.6,'pA and ',g13.6,'pA are ',/,
     &' concatenated into one long shut time',/)
	    else if(i1.eq.2) then
		 ifall=.false.
	       print 512,alo,ahi
	       if(discprt) write(8,512) alo,ahi
512	       format(/,
     &' Shut times that are separated by one or more openings with',/,
     &' ANY amplitudes between ', g13.6,'pA and ',g13.6,'pA are ',/,
     &' concatenated into one long shut time',/)
	    endif
	    goto 36
	 endif
c=	call DCASK(
c=     &'Calculate resolution to exclude sublevels (Howe method)','n',ans)
c	 if(ans.eq.'Y') then
	 if(iopt.eq.3) then
	   call AAMAXSUB(trise,treso,aamaxo)
	   treso=0.001*treso		!in ms
	   tresg=treso			!in ms
	   aamaxg=aamaxo
	   zo=1000.*treso/trise		!tres in ms, trise in mus
	   zg=zo
	   call FALSEV1(tres,fc,rms,avamp,frate)
	   fratg=frato
c==	   goto 311	!back to normal prog to print results
	   goto 36
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
c=	acrit=abs(0.1*avamp)
	acrit=0.0
	print 111,acrit
111	FORMAT('& pA for ''real'' amplitude difference [',f6.2,'] = ')
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
	   if((.not.badgap0(i)).and.tint0(i).lt.0.) then
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
602	format(
     & ' Uses HJC definition of e-openings and e-gaps')
610	continue
      if(pon()) write(7,602)
      if(discprt) write(8,602)
c
	nc=1		!# of intervals concat in each group (if nc=1 transfer
c			!properties directly from input to output)
c
      print 60
60	format(
     & ' Now impose the resolution ',/,
     & ' To see what is being done, put CAP LOCK on now',/,
     & ' (press any key to continue)')
	call ANYKEY()
c
c NOW IMPOSE RESOLUTION
	if(.not.cjump) goto 1001
c
c Make the check optional for now, until code tested
c	nocheck=.false.
c  	print 33
c33	format(
c     &' (Temporary question, until new checking code fully tested)',/,
c     &' Omit test of output for consecutive equal amplitudes [N] ? ')
c	read 101,ans
c	if(UC(ans).eq.'Y') nocheck=.true.
c First code for CJUMP CASE
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
	      if(ampl(ki).eq.0.and.tint(ki).lt.tresg.or.
     &	  ampl(ki).ne.0.and.tint(ki).lt.treso) then
		   print 508,ki,i,tint(ki),ampl(ki),iprops(ki)
		   if(discprt) write(8,508)ki,i,tint(ki),ampl(ki),iprops(ki)
508		   format(' RESINT ERR #1: ki,i = ',2i8,' t = ',
     &		g13.6,' a = ',g13.6,' ip = ',i3)
		   pause
		endif
	   endif
	   if(debon) then
		print 501,i,tint0(i),ampl0(i),iprops0(i)
       	if(discprt) write(8,501) i,tint0(i),ampl0(i),iprops0(i)
	      print 505,ki,tint(ki),ampl(ki),iprops(ki),index(ki)
	      if(discprt) write(8,505)ki,tint(ki),ampl(ki),iprops(ki),
     &	index(ki)
505	      format(27x,' out = ',i6,g12.5,g12.5,i3,i6, '(null)')
	   endif
	   goto 1000			!this sweep done
	endif
	kifirst=ki
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
	call SUMWAMP(-1,jamp,S,amp,tin,ampav,ttot)		!initialise sums
	setdub=.false.				!initialise setdub
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
	if(badgap0(i)) then	!transfer 'unusable'
	    iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
	endif
	shutint=ampl0(i).eq.0	!Thus is start of concatenated shut interval
	if(shutint) then
	   ampav=0		!current amplitude for concat group
	else
	   call SUMWAMP(1,jamp,S,ampl0(i),tint0(i),ampav,ttot)
	endif
	if(dubious(n)) setdub=.true.
	if(dubious(i)) setdub=.true.
	nc=1
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
	   if(ampl(ki).eq.0.and.tint(ki).lt.tresg.or.
     &	  ampl(ki).ne.0.and.tint(ki).lt.treso) then
		print 509,ki,i,tint(ki),ampl(ki),iprops(ki)
		if(discprt) write(8,509)ki,i,tint(ki),ampl(ki),iprops(ki)
509		format(' RESINT ERR #2: ki,i = ',2i8,' t = ',
     &		g13.6,' a = ',g13.6,' ip = ',i3)
		pause
	   endif
	endif
	if(debon) then
	   print 501,i+1,tint0(i+1),ampl0(i+1),iprops0(i+1)
         if(discprt) write(8,501) i+1,tint0(i+1),ampl0(i+1),iprops0(i+1)
	endif
c Need sep bit here to deal with i=ilast-1 (so next interval is last in
c the jump) -because last interval is NOT nec resolvable for jump. If it is
c not resolvable then last interval becomes the preceding one, which must
c therefore have bit 6='64' set (unless it is also the first interval,
c which could happen only if all events in the sweep were below res so
c this sweep becomes null after resolution imposed!)
c
	if(shutint) then		!look for opening that ends shut group
	   a2=ampl0(i+1)
	   if(a2.ne.0.and.RESOLV(i+1,tint0,ampl0,iprops0,nintt)) then
c          =shut group ended by open time i+1
		goto 40
	   else	!keep concatenating the current shut group
c		if(i.eq.ilast-1) then	!check for end of data
c	         iprops(ki)=IBSET(iprops(ki),3) !interval #ki is last and set it bad
c		   setbad=.true.
c		   setend=.true.	!so last (irresolvable) interval omitted
c		   tint(ki)=-1.
c		   goto 40	!end group
c		endif
		nc=nc+1
		tin=tint(ki)
		setbad=badgap(ki)			!tint(ki) already set unusable
		tint(ki)=tin + tint0(i+1)
c if any of these intervals unusable set whole group unusable
		setbad=setbad.or.badgap0(i+1)
		if(setbad) then
		   iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
		endif
		if(i.lt.ilast-1) then
		   if(dubious(i+1)) setdub=.true.
		   i=i+1
		   goto 70	!continue concatenating until resolvable opening found
		endif		!otherwise carry on to 40
	   endif
c
	else if(.not.shutint) then	!look for diff amp (open or shut) that ends open group
	   a2=ampl0(i+1)
	   diffamp=DIFAMP(ampav,a2).or.a2.eq.0
	   if(diffamp.and.RESOLV(i+1,tint0,ampl0,iprops0,nintt)) then
c          =open group ended by resolvable shut time i+1, OR by resolvable
c		opening that has an amplitude different from that of the amplitude
c		of the current amplitude of the concatenated open group
		goto 40
	   else	!keep concatenating the current open group
c		if(i.eq.ilast-1) then	!check for end of data
c	         iprops(ki)=IBSET(iprops(ki),3) !interval #ki is last and set it bad
c		   setbad=.true.
c		   setend=.true.	!so last (irresolvable) interval omitted
c		   tint(ki)=-1.
c		   goto 40	!end group
c		endif
		nc=nc+1
		tin=tint(ki)
		setbad=badgap(ki)			!tint(ki) already set unusable
		tint(ki)=tin + tint0(i+1)
c if any of these intervals unusable set whole group unusable
		setbad=setbad.or.badgap0(i+1)
		if(setbad) then
		   iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
		endif
		call SUMWAMP(1,jamp,S,ampl0(i+1),tint0(i+1),ampav,ttot)
		if(dubious(i+1)) setdub=.true.
		if(i.lt.ilast-1) then
		   if(dubious(i+1)) setdub=.true.
		   i=i+1
		   goto 70	!continue concatenating until resolvable opening found
		endif		!otherwise carry on to 40
	   endif
	endif
c
c Assign amp if concatenation finished
40	continue
	if(nc.eq.1) then			!no concatenation
	   iprops(ki)=iprops0(i)	!so transfer props directly
	   if(setbad) then !e.g. when last interval not resolvable it is set bad
	       iprops(ki)=IBSET(iprops(ki),3)
	   endif
	endif
	if(shutint) then
	   ampl(ki)=0
	else
	   if(jamp.gt.0) then
	      ampl(ki)=ampav		!last mean output from sumwamp
	   else
		print*,'RESINT ERROR #2'
		STOP
	   endif
	endif
	if(setdub) iprops(ki)=IBSET(iprops(ki),0)	!bit 0 set (='1')
	if(debon) then
	   print 502,ki,tint(ki),ampl(ki),iprops(ki),index(ki)
	   if(discprt) write(8,502)ki,tint(ki),ampl(ki),iprops(ki),
     &	index(ki)
	endif
	if(debon) then
	   if(ampl(ki).eq.0.and.tint(ki).lt.tresg.or.
     &	ampl(ki).ne.0.and.tint(ki).lt.treso) then
		print 510,ki,i,tint(ki),ampl(ki),iprops(ki)
		if(discprt) write(8,509)ki,i,tint(ki),ampl(ki),iprops(ki)
510		format(' RESINT ERR #3: ki,i = ',2i8,' t = ',
     &		g13.6,' a = ',g13.6,' ip = ',i3)
		pause
	   endif
	endif
	i=i+1
	if(i.le.ilast) goto 20		!next interval in same jump
c
c Insert here a retrospective scan of the output for last sweep to check for
c adjacent intervals with identical (zero or otherwise) amplitude -if
c any found, concatenate them
c=	if(nocheck) goto 1000	!temporary -until code checked
3	nintj=ki
	i1=kifirst
	if(nintj.le.kifirst+2) goto 1000	!not enough to test in this jump
	n0=0
	do while (i1.le.nintj-2)
	   i1=i1+1
	   a1=ampl(i1)
	   a2=ampl(i1+1)
	   if(a1.eq.a2) then
		n0=n0+1
		tint(i1)=tint(i1)+tint(i1+1)
		iprops(i1)=iprops(i1)+iprops(i1+1)
		do j1=i1+1,nintj-1
		   tint(j1)=tint(j1+1)
		   ampl(j1)=ampl(j1+1)
		   iprops(j1)=iprops(j1+1)
		   index(j1)=index(j1+1)
		enddo
		nintj=nintj-1
		ki=nintj
		print 28,ij,i,i+1,a1,nintj+1,nintj
		if(pon()) write(7,28) ij,i,i+1,a1,nintj+1,nintj
		if(discprt) write(8,28) ij,i,i+1,a1,nintj+1,nintj
28		format(/,' Jump # ',i5,
     &' Intervals ',i5,' and ',i5,' had same amplitude = ',g11.5,/,
     &' so they have been concatenated and total number of intervals',/,
     &' reduced from ',i5,' to ',i5,/)
	   endif
	enddo
	if(n0.gt.0) goto 3	!recheck in case there were 3 consec zeros!
c
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
	nint=ki
	goto 1002		!skip regular case
c
c Concatenate intervals (STEADY STATE CASE)
c NB There are two reasons to concatenate adjacent intervals -(a) because 2nd
c interval is irresolvable, and (b) because 2nd interval, though resolvable
c has same amplitude (open or shut) as first.
c NB symbols i, ki have been schanged to n, ni from here on (to cope with
c a bug in SOLD90)
1001	continue		!jump here for regular case
c	nstop=504	!for debugger
	ni=0	!counts concatenated intervals
	n=0
	nc=1		!# of intervals concat in each group (if nc=1 transfer
c			!properties directly from input to output)
	debon=debug()
C
C IF THE FIRST INTERVAL IS BOTH USABLE AND RESOLVABLE
C THIS IS STARTING POINT. IF NOT LOOK FOR FIRST INTERVAL THAT
C IS BOTH, AND IS PRECEDED BY AN RESOLVABLE INTERVAL TOO (OTHERWISE
C ITS START WILL BE DEFINED ONLY BY THE POSITION OF THE PRECEDING
C UNRESOLVABLE INTERVAL AND SO WILL BE UNRELIABLE)
	n=n+1
	if(RESOLV(n,tint0,ampl0,iprops0,nintt).and.
     &	(.not.badgap0(n))) goto 2				!found it
1	n=n+1
	if(RESOLV(n,tint0,ampl0,iprops0,nintt).and.
     &  RESOLV(n-1,tint0,ampl0,iprops0,nintt).and.
     &  (.not.badgap0(n))) goto 2
	goto 1
c
C NOW START TO LOOK FOR UNRESOLVABLE INTERVALS
c (return to 2 for start of next concat group)
c Simplified version 02/25/00 07:01am
c (1) A concantenated shut period starts with a good, resolvable shutting
c 	and ends when first good resolvable opening found.
c	Length of concat shut period=sum of all durations before the resol opening
c	Amplitude of concat shut period=0
c (2) A concantenated open period starts with a good, resolvable opening
c 	and ends when first good resolvable interval is found that
c	has a different amplitude (either shut, or open but diff amplitude).
c	Length of concat open period=sum of all concatenated durations
c	Amplitude of concat open period weighted mean amp of all concat intervals
c Simpler to have separate code for shut groups and for open groups. If first
c interval of group is shut then set shutint=true.
c
c First interval of any concat group must be good and resolvable so
c insert warning to check this
2	ni=ni+1
c
	if(mod(n,100).eq.0) then	!check if debug on every 100th transition
	   debon=debug()
	endif
c
c First interval in each concatenated group must be resolvable, but may
c be bad (in which case next group will be bad)
	if(.not.(RESOLV(n,tint0,ampl0,iprops0,nintt))) then
	   call BELL(1)
	   print 73,n,tint0(n)
         if(discprt) write(8,73) n,tint0(n)
73	   format(' ERROR in RESINT -TELL DC: n = ',i6,g13.6,' ms')
	endif
	if(debon) then
	   print 501,n,tint0(n),ampl0(n),iprops0(n)
         if(discprt) write(8,501)n,tint0(n),ampl0(n),iprops0(n)
	endif
	call SUMWAMP(-1,jamp,S,amp,tin,ampav,ttot)		!initialise sums
	setdub=.false.				!initialise setdub
	tint(ni)=tint0(n)			!start new concat group
	nc=1
	index(ni)=n			!define index
	if(badgap0(n)) then	!transfer 'unusable'
	    iprops(ni)=IBSET(iprops(ni),3)	!gap unusable; set bit 3 ='8'
	endif
	shutint=ampl0(n).eq.0	!Thus is start of concatenated shut interval
	if(shutint) then
	   ampav=0		!current amplitude for concat group
	else
	   call SUMWAMP(1,jamp,S,ampl0(n),tint0(n),ampav,ttot)
	endif
	if(dubious(n)) setdub=.true.
c
c is next interval resolvable? (Return to 7 for next concat interval in a group)
7	continue
	if(n.eq.nintt) goto 4		!END
c	if(n.ge.nstop) then
c	   pause		!dummy line to set debugger to 'break on line'
c	endif
	if(debon) then
	   print 501,n+1,tint0(n+1),ampl0(n+1),iprops0(n+1)
         if(discprt) write(8,501) n+1,tint0(n+1),ampl0(n+1),iprops0(n+1)
501	   format(i6,g12.5,g12.5,i5)
	endif
c
	if(shutint) then		!look for opening that ends shut group
	   a2=ampl0(n+1)
	   if(a2.ne.0.and.RESOLV(n+1,tint0,ampl0,iprops0,nintt)) then
c          =shut group ended by open time n+1
		goto 4
	   else	!keep concatenating the current shut group
		if(n.eq.nintt-1) then	!check for end of data
	         iprops(ni)=IBSET(iprops(ni),3) !interval #ki is last and set it bad
		   setbad=.true.
		   setend=.true.	!so last (irresolvable) interval omitted
		   tint(ni)=-1.
		   goto 4	!end group
		endif
c		if(debon) then
c		   print 501,n+2,tint0(n+2),ampl0(n+2),iprops0(n+2)
c      	   if(discprt) write(8,501) n+2,tint0(n+2),ampl0(n+2),
c     &	    iprops0(n+2)
c		endif
		nc=nc+1
		tin=tint(ni)
		setbad=badgap(ni)			!tint(ki) already set unusable
		tint(ni)=tin + tint0(n+1)
c if any of these intervals unusable set whole group unusable
		setbad=setbad.or.badgap0(n+1)
		if(setbad) then
		   iprops(ni)=IBSET(iprops(ni),3)	!gap unusable; set bit 3 ='8'
		endif
		if(dubious(n+1)) setdub=.true.
		n=n+1
		goto 7	!continue concatenating until resolvable opening found
	   endif
c
	else if(.not.shutint) then	!look for diff amp (open or shut) that ends open group
	   a2=ampl0(n+1)
	   diffamp=DIFAMP(ampav,a2).or.a2.eq.0
	   if(diffamp.and.RESOLV(n+1,tint0,ampl0,iprops0,nintt)) then
c          =open group ended by resolvable shut time n+1, OR by resolvable
c		opening that has an amplitude different from that of the amplitude
c		of the current amplitude of the concatenated open group
		goto 4
	   else	!keep concatenating the current open group
		if(n.eq.nintt-1) then	!check for end of data
	         iprops(ni)=IBSET(iprops(ni),3) !interval #ki is last and set it bad
		   setbad=.true.
		   setend=.true.	!so last (irresolvable) interval omitted
		   tint(ni)=-1.
		   goto 4	!end group
		endif
c		if(debon) then
c		   print 501,n+2,tint0(n+2),ampl0(n+2),iprops0(n+2)
c      	   if(discprt) write(8,501) n+2,tint0(n+2),ampl0(n+2),
c     &	    iprops0(n+2)
c		endif
		nc=nc+1
		tin=tint(ni)
		setbad=badgap(ni)			!tint(ki) already set unusable
		tint(ni)=tin + tint0(n+1)
c if any of these intervals unusable set whole group unusable
		setbad=setbad.or.badgap0(n+1)
		if(setbad) then
		   iprops(ni)=IBSET(iprops(ni),3)	!gap unusable; set bit 3 ='8'
		endif
		call SUMWAMP(1,jamp,S,ampl0(n+1),tint0(n+1),ampav,ttot)
		if(dubious(n+1)) setdub=.true.
		n=n+1
		goto 7	!continue concatenating until resolvable opening found
	   endif
	endif
c
C Assign amp if concatenation finished
4	continue
	if(nc.eq.1) then			!no concatenation
	   iprops(ni)=iprops0(n)	!so transfer props directly
	   if(setbad) then !e.g. when last interval not resolvable it is set bad
	       iprops(ni)=IBSET(iprops(ni),3)
	   endif
	endif
	if(shutint) then
	   ampl(ni)=0
	else
	   if(jamp.gt.0) then
	      ampl(ni)=ampav		!last mean output from sumwamp
	   else
		print*,'RESINT ERROR #1'
		STOP
	   endif
	endif
	if(setdub) iprops(ni)=IBSET(iprops(ni),0)	!bit 0 set (='1')
	n=n+1
	if(debon) then
	   print 502,ni,tint(ni),ampl(ni),iprops(ni),index(ni)
	   if(discprt) write(8,502)ni,tint(ni),ampl(ni),iprops(ni),
     &	index(ni)
502	   format(27x,' out = ',i6,g12.5,g12.5,i3,i6)
	endif
c
	if(setend) then
	   setend=.false.	!used only if last interval in consam not resolvable
	   goto 8
	endif
	IF(n.le.nintt) GOTO 2   !LOOK FOR NEXT CONCAT GROUP
c
c All intervals now finished
8	continue
	nint=ni
c
c Insert here a retrospective scan of the output to check for
c adjacent intervals with identical (zero or otherwise) amplitude -if
c any found, concatenate them
301	n0=0
	i=0
	do while (i.le.nint-2)
	   i=i+1
	   a1=ampl(i)
	   a2=ampl(i+1)
	   if(a1.eq.a2) then
		n0=n0+1
		tint(i)=tint(i)+tint(i+1)
		do k=0,6
		   if(BTEST(iprops(i+1),k)) then
			iprops(i)=IBSET(iprops(i),k)	!set bits as in concat vals
		   endif
		enddo
		do j=i+1,nint-1
		   tint(j)=tint(j+1)
		   ampl(j)=ampl(j+1)
		   iprops(j)=iprops(j+1)
		   index(j)=index(j+1)
		enddo
		nint=nint-1
		print 27,i,i+1,a1,nint+1,nint
		if(discprt) write(8,27) i,i+1,a1,nint+1,nint
27		format(/,
     &' Intervals ',i5,' and ',i5,' had same amplitude = ',g11.5,/,
     &' so they have been concatenated and total number of intervals',/,
     &' reduced from ',i5,' to ',i5,/)
	   endif
	enddo
	if(n0.gt.0) goto 301	!recheck in case there were 3 consec zeros!
c
c If excamp=true the insert here a retrospective scan that concatenates
c shut and open times to make on long shut time if there is a group
c of one or more contiguous openings that are all within
c the exclusion window
c Transfer of iprops -only relevant one is case where either first
c or final shut time is unusable, in which case the concatenated
c shut time is set unusable
	if(excamp) then
	   print 71
71	   format(' Now remove specified amplitudes',/,
     &    ' To see what is being done, put CAP LOCK on now',/,
     &    ' (press any key to continue)')
	   call ANYKEY()
c
	   debon=debug()
	   if(.not.ifall) goto 72
c
	   if(debon) then
	       print 29
	       if(discprt) write(8,29)
29	       format(/,
     &' The following pairs of shut intervals are separated by',/,
     &'   openings that ALL have amplitudes in the',/,
     &'  in exclusion window, so they have been concatenated into',/,
     &'  one shut time, and total number of intervals reduced')
	   endif
	   nconc4=0
	   i=0
	   ip1=0
	   do while (i.le.nint-2)
		i=i+1
		a1=ampl(i)
		a2=ampl(i+1)
		if((a1.eq.0.).and.(a2.ge.alo.and.a2.le.ahi)) then
c   look forward until next shut time found -if all intervening openings
c   are within the specified window than concatenate all of them with
c   shutting on each side into one long shut time
		   t=tint(i)+tint(i+1)
		   do j=i+2,nint
			a=ampl(j)
			shut=a.eq.0.
			if(a.ge.alo.and.a.le.ahi) then		!still in window
			   t=t + tint(j)	!group continues -add duration of opening
			else if(shut) then
			   t=t + tint(j)	!group finished -last shut time added
			   ifirst=i
			   ilast=j
			   if(badgap(ifirst).or.badgap(ilast))ip1=IBSET(ip1,3)
			   i=ifirst-1		!so i=ifirst after 1 added above
			   ncon=ilast-ifirst+1	!number concatenated
			   n=nint
			   nint=nint - (ncon-1)
			   nconc4=nconc4+1
			   if(debon) then
			      print 291
			      if(discprt) write(8,291)
291			      format(
     &	' Following intervals concatenated into one shut time:')
				do k=ifirst,ilast
				   print 292,k,tint(k),ampl(k),iprops(k)
				   if(discprt) write(8,292) k,tint(k),
     &				ampl(k),iprops(k)
292				   format(i5,2g13.6,i5)
				enddo
			   endif
c
			   i1=ifirst	!for brevity
			   tint(i1)=t
			   iprops(i1)=ip1
			   if(debon) then
			      print 293,i1,tint(i1),ampl(i1),iprops(i1),n,nint
			      if(discprt) write(8,293) i1,tint(i1),
     &				ampl(i1),iprops(i1),n,nint
293				format('  replaced by',/,i5,2g13.6,i5,/,
     &			'  and n reduced from ',i5,' to ',i5,/)
			   endif
c			Move all the others down in array
			   do k=ifirst+1,nint-ncon+1
				k1=k+ncon-1
				tint(k)=tint(k1)
			 	ampl(k)=ampl(k1)
				iprops(k)=iprops(k1)
				index(k)=index(k1)
			   enddo
			   goto 21
			else		!amp outside window -abandon the group
			   goto 21
			endif
		   enddo
		endif
21		continue
		ip1=0
	   enddo
	   goto 74	!end of 'ifall' calcs
c
c Now case where ANY amp outside range counts (.not.ifall)
72	   continue
c	   if(debon) then
c		print 295
c	      if(discprt) write(8,295)
c295	      format(/,
c     &' The following pairs of shut intervals are separated by',/,
c     &'   openings that ANY of which have amplitudes in the',/,
c     &'  in exclusion window, so they have been concatenated into',/,
c     &'  one shut time, and total number of intervals reduced')
c	   endif
c The output tint() should start with an opening, so must first find a valid
c start = sequence of one or more consec openings all outside exclusion range
	   i=0
	   do while (i.le.nint-1)
		i=i+1
		a1=ampl(i)
		a2=ampl(i+1)
		shut1=a1.eq.0.
		open2=a2.ne.0.
		exc2=(a2.ge.alo.and.a2.le.ahi)	!a2 in exclusion range
		if(shut1.and.open2.and.(.not.exc2)) then	!putative start
		   setexc=.false.	!set true if amp in exclusion range found
		   do j=i+1,nint-1
			a=ampl(j)
			shut=a.eq.0.
			if(a.ge.alo.and.a.le.ahi) then	!amp in exclusion window
			   setexc=.true.
			endif
			if(shut) goto 800
		   enddo
		   goto 801		!get here if reach end of data ====?
800		   continue		!ampl(j) is shut
c          next shutting found, but if setexc true still have not got good start
		   if(setexc) then
			i=j-1		!so starts at shut interval j after i=i+1
			goto 801
		   else
			ifirst=i+1	!first opening of seq of non-excluded openings
			iend=j-1
			goto 802
		   endif
		endif
801		continue
	   enddo	!end of do while to look for starting point
802	   continue
c Now ready to start at tint(istart) = first opening of a seq of 1 or more
c good (non-excluded) openings
c Define:
c ifirst=index of first (=shut) interval in group to be concat
c ilast=index of last (=shut) interval in group to be concat
c  so ilast+1=index of first non-exc opening in seq of one or more non-exc openings
c iend=index of last opening in seq of one or more non-exc openings
c
c At this point we know that k=istart to k=iend is a sequence of one or
c more good (non-excluded) openings, so they can be copied directly tp output
	   ilast=ifirst-1
	   m=0
	   do k=ilast+1,iend
		m=m+1
		tint(m)=tint(k)
		ampl(m)=ampl(k)
		iprops(m)=iprops(k)
		if(debon) then
		   print 296,m,tint(m),ampl(m),iprops(m),k
		   if(discprt) write(8,296) m,tint(m),
     &	   	ampl(m),iprops(m),k
296		   format(i5,2g13.6,i5,
     &		' (original interval # ',i5,')')
		endif
	   enddo
c Now ready to start on the rest of the data.  tint(iend+1) must be shut (and
c may be start if a concat group_, and it must be
c followed by an opening which may or may not be excluded
	   ifirst=iend+1
	   nconc4=0
	   ip1=0
	   i=ifirst-1	!=ifirst when i=i+1 below
c
	   do while (i.le.nint-1)
		i=i+1
		a1=ampl(i)
		a2=ampl(i+1)
		shut1=a1.eq.0.
		open2=a2.ne.0.
		exc2=(a2.ge.alo.and.a2.le.ahi)	!a2 in exclusion range
		if(shut1.and.open2.and.(.not.exc2)) then	!putative start
		   setexc=.false.	!set true if amp in exclusion range found
		   do j=i+1,nint-1
			a=ampl(j)
			shut=a.eq.0.
			if(a.ge.alo.and.a.le.ahi) then	!amp in exclusion window
			   setexc=.true.
			endif
			if(shut) goto 900
		   enddo
		   goto 901		!get here if reach end of data ====?
900		   continue		!ampl(j) is shut
c          next shutting found, but if setexc true still have not got good start
		   if(setexc) then
			i=j-1		!so starts at shut interval j after i=i+1
			goto 901
		   else	!not setexc -good sequence found
			ilast=i	!i+1=first opening of seq of non-excluded openings
			iend=j-1
c                 Now ifirst,ilast,iend defined so can assign tint()
			ncon=ilast-ifirst+1	!number concatenated
			if(ncon.gt.1) then
			   nconc4=nconc4+1
			   t=0.0
			   do k=ifirst,ilast
				t=t+tint(k)
			   enddo
			   if(badgap(ifirst).or.badgap(ilast))ip1=IBSET(ip1,3)
			   m=m+1
			   tint(m)=t		!the concatenated shut time
			   ampl(m)=0.
			   iprops(m)=ip1
			   if(debon) then
			      print 291
			      if(discprt) write(8,291)
c291			      format(
c     &	' Following intervals concatenated into one shut time:')
				do k=ifirst,ilast
				   print 292,k,tint(k),ampl(k),iprops(k)
				   if(discprt) write(8,292) k,tint(k),
     &				ampl(k),iprops(k)
c292				   format(i5,2g13.6,i5)
				enddo
				print 297,m,tint(m),ampl(m),iprops(m),
     &			   ifirst,ilast
				if(discprt) write(8,297) m,tint(m),
     &			   ampl(m),iprops(m),ifirst,ilast
297				format(' concatenation gives:',/,i5,2g13.6,i5,
     &		' (original intervals # ',i5,' to ',i5,')')
				print
			   endif
			else		!ncon=1, so assign k=ifirst=ilast
			   k=ifirst	!=ilast
			   m=m+1
			   tint(m)=tint(k)
			   ampl(m)=ampl(k)
			   iprops(m)=iprops(k)
			   if(debon) then
				print 296,m,tint(m),ampl(m),iprops(m),k
				if(discprt) write(8,296) m,tint(m),
     &			  ampl(m),iprops(m),k
c296				format(i5,2g13.6,i5,
c     &			' (original interval # ',i5,')')
			   endif
			endif		!end of concat when ncon>1 or ncon=1
c Now assign the good sequence, k=ilast+1, iend
			do k=ilast+1,iend
			   m=m+1
			   tint(m)=tint(k)
			   ampl(m)=ampl(k)
			   iprops(m)=iprops(k)
			   if(debon) then
				print 296,m,tint(m),ampl(m),iprops(m),k
				if(discprt) write(8,296) m,tint(m),
     &	 		ampl(m),iprops(m),k
c296				format(i5,2g13.6,i5,
c     &			' (original interval # ',i5,')')
			   endif
			enddo
c      Now look for next sequences
			ifirst=iend+1
c			goto 901
		   endif	!end of 'if setexc'
		endif
901		continue
		ip1=0
	   enddo	!end of 'do while'
	   nint=m	!set new nint after end of 'do while'
c
74	   continue
c Check result for consecutive equal amplitudes
300	   i1=0
	   n0=0
	   do while (i1.le.nint-2)
		i1=i1+1
		a1=ampl(i1)
		a2=ampl(i1+1)
		if(a1.eq.a2) then
		   n0=n0+1
		   tint(i1)=tint(i1)+tint(i1+1)
		   iprops(i1)=iprops(i1)+iprops(i1+1)
		   do j1=i1+1,nintj-1
			tint(j1)=tint(j1+1)
			ampl(j1)=ampl(j1+1)
			iprops(j1)=iprops(j1+1)
			index(j1)=index(j1+1)
		   enddo
		   nint=nint-1
		   ki=nint
		   print 281,i1,i1+1,a1,nint+1,nint
		   if(discprt) write(8,281) i1,i1+1,a1,nint+1,nint
281		   format(/,
     &' Intervals ',i5,' and ',i5,' had same amplitude = ',g11.5,/,
     &' so they have been concatenated and total number of intervals',/,
     &' reduced from ',i5,' to ',i5,/)
		endif
	   enddo
	   if(n0.gt.0) goto 300	!recheck in case there were 3 consec zeros!
c
	endif		!end of excamp
c
c Jump to here after setting resolution for cjump
1002	continue		!finished concatenation
c	if(imode.eq.2) RETURN		!for SCDIR
145	continue
c Check number unusable or dubious
	nb1=0
	nb2=0
	nb3=0
	nb4=0
	nb5=0
	do i=1,nint-1
c	   if(dubious(i)) nb1=nb1+1	!dubious refers to iprops0(), not iprops()
c	   if(badgap0(i)) nb2=nb2+1
	   if(BTEST(iprops(i),0)) nb1=nb1+1 !ampl(i) was dubious (bit 0 set ='1')
	   if(BTEST(iprops(i),1)) then 	!ampl(i) was fixed (bit 0 set ='2')
		if(ampl(i).eq.0.) then
		   nb2=nb2+1
		else
		   nb3=nb3+1
		endif
	   endif
	   if(BTEST(iprops(i),2)) nb4=nb4+1 !ampl(i) was constrained (bit 0 set ='4')
	   if(BTEST(iprops(i),3)) nb5=nb5+1 !tint(i) was unusable(bit 3 set ='8')
	enddo
c
c=====TEMP========
c	OPEN(unit=20,file='tint2.dat',status='UNKNOWN',
c     &	access='TRANSPARENT')
c	write(20,rec=1) nint,(tint(i),i=1,nint),(ampl(i),i=1,nint),
c     &	(iprops(i),i=1,nint)
c	CLOSE(unit=20)
c=====TEMP ends========
c
	print 289,NINT,nb1,nb2,nb3,nb4,nb5
	if(pon()) write(7,289) NINT,nb1,nb2,nb3,nb4,nb5
	if(discprt) write(8,289) NINT,nb1,nb2,nb3,nb4,nb5
289	FORMAT( ' Number of resolved intervals= ',i8,/,
     & ' ',i5,' intervals with dubious amplitudes in output',/,
     & ' ',i5,' shut intervals with fixed amplitudes in output',/,
     & ' ',i5,' open intervals with fixed amplitudes in output',/,
     & ' ',i5,' intervals with constrained amplitudes in output',/,
     & ' ',i5,' intervals with undefined durations in output',/)
	if(excamp) then
	   print 294,nconc4
	   if(discprt) write(8,294) nconc4
294	   format(/,1x,i4,
     &  ' groups of intervals with amplitudes that were all within',/,
     &  ' within exclusion window were concatenated',/)
	endif
	if(debug()) then
25		print 23,nint
23		format(
     & ' Print intervals and amplitudes # n1,n2 (0=skip) [1,',i6,'] = ')
		n1=1
		n2=nint
		call INPUT2i(n1,n2)
		if(n1.gt.0) then
		  if(n2.lt.n1) n2=n1
		  print 231
	        if(discprt) write(8,231)
231		  format(' No',6x,'length (ms)',6x,'amp (pA)',3x,
     &	   'properties',2x,' orig index')
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


	subroutine SUMWAMP(imode,j,S,amp,tint,ampav,ttot)
c To calculate sum of amplitudes, weighted by their duration (=tint)
c Returns ampav=current value of average after each call, and ttot=current
c value of denominator=total duration of the intervals used
C IF imode=-1 IT INITIALISES.
c
	if(imode.lt.0) then   !initialise only
	   j=0
	   S=0.0
	   ttot=0.0
	   RETURN
	endif
c
c Insert precaution in case of 'zero' duration intervals
c   Note that, although ampav is calculated afresh at each call, it is
c not actually used (at present) until concatenation is finished, so
c a value calculated when j=1 (as below) will not be used unless there
c is no concatenation and this is the only amplitude
c   In this version of j>1 and ttot (total length) is still near zero
c then ampav is undefined (this would occur only when all the durations
c in a concatenation are near-zero!)
	j=j+1
	S=S+amp*tint
	ttot=ttot+tint
	if(ttot.gt.1.e-30*abs(s)) then
	   ampav=s/ttot
	else if(j.eq.1) then
	   ampav=amp
	endif
c
	RETURN
	end


	logical function RESOLV(i,tint0,ampl0,iprops0,nintt)
	real*4 tint0(nintt),ampl0(nintt)
	integer*1 iprops0(nintt)
	common/rblck/treso,tresg,acrit,avamp
	logical btest
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
c  Fixed 10/11/96 10:53am to use .le.acrit, which is necessary if acrit=0
	DIFAMP=.true.
	if(amp1.eq.0.and.amp2.eq.0) DIFAMP=.false.
	if(amp1.ne.0.and.amp2.ne.0.and.
     & abs(amp1-amp2).le.acrit) DIFAMP=.false.
c     & abs(amp1-amp2).lt.acrit) DIFAMP=.false.
	RETURN
	END


