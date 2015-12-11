	subroutine REShjc1(ffilt,fc,rms,trise,sres,sexp,cjump,
     & excamp,alo,ahi,treso,tresg,acrit,avamp,autosim)
c
c REShjc1 is the first part of RESINT61, to set the required resolution
c in hjcfit. The actual setting of the reolution is done in REShjc2 (which
c contains the bulk of resint61).  Also add to arguments the values previously
c in common/rblck/treso,tresg,acrit,avamp; common/censor/excamp,alo,ahi; and
c common/sblk/sbin,shist,sres,sexp. NB excamp,alo,ahi are outputs
c
c OLD RESINT61 NOTES
c RESINT61 is version of resint6 for HJCFIT, in which dimensions of tint() etc
c are defined as in resint3.
c RESINT51 is version of resint5 for HJCFIT, in which dimensions of tint() etc
c are defined as in resint3.
c
c RESINT6 is version in which definition of resolution is exactly
c as specified by HJC (unresolved events are not split between first
c and second interval of output, but are part of first) 02/28/00 10:51am
c Resint5 is version in which kjumps0 and kjumps are integer*4
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
	logical DEBUG,caplock
	logical sres,sexp
c For cjumps
	logical cjump,autosim
	logical excamp
c
c	common/sblk/sbin,shist,sres,sexp
	character*1 ans,UC
	logical discprt
	common/dp/discprt
c	common/rblck/treso,tresg,acrit,avamp
c	common/censor/excamp,alo,ahi
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
	debug()=caplock()
c
c If call with SRES=true want another histogram with same resolution
c so TINT() already defined- just print values ,then go to 145
203	continue
	excamp=.false.
	if(.not.sres) then
	   if(autosim) goto 80
c Extra bit for glutamate etc
36	   iopt=1
	   if(cjump) then
		print 34,iopt
34		format(/,
     & ' (1) Carry on',/,
     & ' (2) Exclude all openings in a specified amplitude window',/,
     & ' Option number (0=help) [',i2,'] = ')
	   else
		print 341,iopt
341		format(/,
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
		alo=-50.
		ahi=-50.
		print 35
35		format(
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
770            format(' Range must NOT include zero')
               goto 36
		endif
		if(alo.gt.ahi) then
		   a=alo
		   alo=ahi
		   ahi=a
		endif
		print 51,alo,ahi
		if(discprt) write(8,51) alo,ahi
51		format(/,
     &' Shut times that are separated by one or more openings with',/,
     &' amplitudes that are ALL between ', g13.6,'pA and ',g13.6,'pA',/,
     &' are concatenated into one long shut time')
		goto 36
	   endif
c=       call DCASK(
c=     &'Calculate resolution to exclude sublevels (Howe method)','n',ans)
c	   if(ans.eq.'Y') then
	   if(iopt.eq.3) then
		call AAMAXSUB(trise,treso,aamaxo)
		treso=0.001*treso		!in ms
		tresg=treso			!in ms
		aamaxg=aamaxo
		zo=1000.*treso/trise		!tres in ms, trise in mus
		zg=zo
		call FALSEV1(tres,fc,rms,avamp,frate)
		fratg=frato
		goto 36
	   endif
	endif		!end of if(.not.sres) then
c	else if(imode.eq.1) then	!HJCFIT

80	continue		!jump here for autosim
	print 24,treso
24    format(
     & ' Resolution for open and shut times (microsec) [',f10.3,'] = ')
	call INPUTr(treso)
241	if(treso.lt.0.0001) then
	   call BELL(1)
	   print 242
242	   format(
     &	' Recommend non-zero resolution, say 0.001 microsec')
	   treso=0.1
	   print 240,treso
240      format(
     & ' Resolution for open and shut times (microsec) [',f10.3,'] = ')
	   call INPUTr(treso)
	   goto 241
	endif
c
	print 102,avamp
102	FORMAT(' Full amplitude (pA) = ',G13.6,'  O.K. [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).eq.'N') then
	   print 110
110	   format(' Full amp (pA, with sign) = ')
	   call INPUTr(avamp)
	endif
c
	treso=0.001*treso		!in ms (default is in ms already)
	tresg=treso			!for HJCFIT
c
	if(autosim) then
	   if(discprt) write(8,82) 1000.*treso
82	   format(/,' Resolution (microsec) = ',f8.2,/)
	   goto 81
	endif
c
	if(ffilt.gt.0.) then
	    call FALSEV1(treso,fc,rms,avamp,frato)
	    zo=1000.*treso/trise		!tres in ms, trise in mus
	    aamaxo=erfs(0.88604*zo)
	    call FALSEV1(tresg,fc,rms,avamp,fratg)
	    zg=1000.*tresg/trise		!tres in ms, trise in mus
	    aamaxg=erfs(0.88604*zg)
	endif
c	endif 	!end  of imode=1
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
	   ans='Y'
	   call INPUTa(ans)
	   if(UC(ans).eq.'N') then
		treso=treso*1.e3	!back to ms for default
		tresg=tresg*1.e3	!back to ms for default
		goto 203
	   endif
	endif
	if(discprt) write(8,30) 1000.*treso,zo,aamaxo,1000.*tresg,
     & zg,aamaxg
	if(sres) print 30,1000.*treso,zo,aamaxo,1000.*tresg,zg,aamaxg
30	format(/,' Resolution (microsec):',/,
     & '   for openings = ',f8.2,' (= ',g11.4,' risetimes, A/Amax = ',
     & g12.5,' )',/,
     & '   for shuttings= ',f8.2,' (= ',g11.4,' risetimes, A/Amax = ',
     & g12.5,' )',/)
	if(discprt.and.ffilt.gt.0) write(8,32) frato,fratg
	if(sexp.and.ffilt.gt.0) print 32,frato,fratg
32	format(
     & '  false event rate (per sec) for openings  = ',g13.6,/,
     & '			      for shuttings = ',g13.6)
c SET ACRIT
81	continue
c=	acrit=abs(0.1*avamp)
	acrit=0.0
	print 111,acrit
111	FORMAT('& pA for ''real'' amplitude difference [',f6.2,'] = ')
	call INPUTr(acrit)
	if(discprt) write(8,31) avamp,acrit
31	format(' For sublevels take:',/,3x,
     & ' full amplitude (pA)= ',f8.2,'; pA for real difference= ',f6.3)
c
	if(debug()) then
	   print 503
	   if(discprt) write(8,503)
503	   format(' Input',35x,' Output')
	endif
	if(sres) goto 610
602	format(
     & ' Uses exact HJC definition of e-openings and e-gaps')
610	continue
      if(discprt) write(8,602)
c
	RETURN
	end
