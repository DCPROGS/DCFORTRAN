	subroutine GETBURST(tint,ampl,iprops,nintt,nint,nfile,
     & iexstrt,iexend,onetcrit,tcrit,tcvals,ibamp,findgap,
     & dubamp,exass,ibindex,nbst,bampl,ndim,idebug,imode)
c
c To locate burst in tint() with normal method only at present (ibtype=1 in cdist2)
c imode=0: gets the settings (tcrit etc) and returns nbst to allow arrays
c 	to be allocated in calling prog
c imode=1: repeats burst location and defines
c	ibindex(1,i), ibindex(1,i) = index of first and last opening in ith burst
c	bampl(i) = mean amp of each burst
c Simplify debug printout using idebug
c
c Modif 01/14/05 12:10pm so ops(), gaps() now allocatable, not fixed at 5000
c
	real*4 TINT(nintt),ampl(nintt)
	integer*1 iprops(nintt)
	integer ibindex(2,ndim)
	real*4 bampl(ndim)
	real*4 tcvals(50)
	integer iexstrt(50),iexend(50)
	logical onetcrit,pflag,newfile,findgap,badend,conam1
	logical pbst,ppopen,listall,bprint,bad,open,shut,badgap
	logical dubamp,dubious,exass,debug,deb
c	dimension ops(5001),gaps(5000)	!temp store for 1 burst
	allocatable::ops, gaps
	real*4 ops(:),gaps(:)
cc	dimension isubs(5000)
c
	character*1 ans,UC
	logical discprt,caplock
	common/dp/discprt
c
	debug()=caplock()
	dubious(i)=BTEST(iprops(i),0)	!ampl(i) was dubious (bit 0 set ='1')
	badgap(i)=BTEST(iprops(i),3)	!tint(i) was unusable (bit 3='8' set)
c
108	format(/)
c
	if(imode.eq.1) then
	   idebug=0		!switch off all debugging for 2nd pass
	   n1=-1
	   n2=-1
	   TC=0.
	   ppopen=.false.
	   listall=.false.
	   call CAPCLR 	!switch caplock off
	   goto 121
	endif
c
	if(nfile.eq.1) then
	   onetcrit=.true.
	   if(tcrit.lt.0.) then		!first time
		print 321
321		format(/,
     &	' Critical gap length, tcrit, to define bursts (ms) = ')
		call INPUTr(tcrit)
	   else
		print 320,tcrit
320		format(/,
     &  ' Critical gap length, tcrit, to define bursts [',f7.2,'ms] = ')
		call INPUTr(tcrit)
		tcvals(1)=tcrit
	   endif
	else
	   ans='N'
	   if(onetcrit) ans='Y'
	   call DCASK('Critical gap length same for all files',ans,ans)
	   onetcrit=ans.eq.'Y'
	   if(tcrit.lt.0.) then		!first time
		if(onetcrit) then
		   print 122
122		   format(' Critical gap length for all files (ms) = ')
		   call INPUTr(tcrit)
		   do i=1,nfile
			tcvals(i)=tcrit
		   enddo
		else
		   do i=1,nfile
			print 31,i
31			format(' File #',i3,': critical gap length (ms) = ')
			call INPUTr(tcvals(i))
		   enddo
		endif
	   else
		if(onetcrit) then
		   print 320,tcrit
c320		   format(' Critical gap length [',f7.2,'ms] = ')
		   call INPUTr(tcrit)
		   do i=1,nfile
			tcvals(i)=tcrit
		   enddo
		else
		   do i=1,nfile
			print 123,i,tcvals(i)
123			format(
     &        ' File #',i3,': critical gap length (ms) [',f7.2,'ms] = ')
			call INPUTr(tcvals(i))
		   enddo
		endif
	   endif
	endif		!end of tcrit setting
c
	print 133
133	format(/,
     & ' DEFAULTS FOR DEFINITION OF BURSTS',/,
     & ' (1) ''Burst amplitude'' defined as:',/,
     & '       mean current (excluding shut periods) during burst',/,
     & ' (2) Bursts with any ''assumed'' amps are INCLUDED',/,
     & ' (3) Require a gap > tcrit before the 1st burst in each file',/,
     & ' (4) Unusable shut time NOT a valid end of burst',/,
     & ' (5) No listing of the individual bursts',/,
     & ' Use ALL of the defaults [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(ans.eq.'Y') then
	   ibamp=1
	   exass=.false.
	   findgap=.true.
	   badend=.false.
	   n1=-1		!defaults (no debug printing)
	   n2=-1
	   TC=0.
	   kplast=0
	   ppopen=.false.
	   goto 134
	endif
c
	ibamp=1	!default
	print 124
124	format(' (1) ''Burst amplitude'' defined as:',/,
     & '  mean current (excluding shut periods) during burst')
	print 522
522	format(' (2) ''Burst amplitude'' defined as:',/,
     & '  mean of the amps fitted to each opening of burst')
	print 30
30	format(' Option number [1] = ')
	call INPUTi(ibamp)
c
	print 2261
2261	format('&Exclude bursts with any ''assumed'' amps [N] ? ')
	ans='N'
	call INPUTa(ans)
	exass=UC(ans).eq.'Y'
c?	print 703
c?703	format(
c?     & ' Exclude time spent in openings with amplitudes outside',
c?     & ' specified range [N] ? ')
c?	ans='N'
c?	call INPUTa(ans)
c?	conam1=ans.eq.'Y'
c?	amplo=-10000.0
c?	amphi=10000.	!include openings of any amp
c?	if(conam1) then
c?	   print 227
c?227	   FORMAT(/,' Define amplitude range:',/,
c?     & ' Low amp, high amp (real pA with sign)= ')
c?	   call INPUT2r(amplo,amphi)
c?	   if(amphi.lt.amplo) then
c?		a1=amphi	!swap
c?		amphi=amplo
c?		amplo=a1
c?	   endif
c?         if(discprt) write(8,705)amplo,amphi
c?705	   format(' Amplitude range from ',g13.6,' to ',g13.6)
c?	endif
c
	ans='Y'
	call DCASK(
     &'Find a gap > tcrit before the first burst in each file',ans,ans)
	findgap=ans.eq.'Y'
	ans='N'
	call DCASK(
     &'Treat an unusable shut time as a valid end of burst',ans,ans)
	badend=ans.eq.'Y'
c
	n1=-1		!defaults (no debug printing)
	n2=-1
	TC=0.
	kplast=0
	print 3211
3211	format(' List intervals in some/all bursts [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).ne.'Y') GOTO 3212
3215	print 112
112	format('  (1) List for all bursts')
	print 113
113	format('  (2) List from burst i1 to i2')
	print 114
114	format(
     & '  (3) List bursts with a gap longer than a specified value')
	print 115
115	format(
     & '  (4) List bursts with duration longer than a specified value')
	print 116
116	format(/,' Option number = ')
	call INPUTi(j)
	if(j.lt.1.or.j.gt.4) GOTO 3215
	if(discprt) goto(1121,1131,1141,1151) J
	goto 3216
1121	write(8,112)
	goto 3216
1131	write(8,113)
	goto 3216
1141	write(8,114)
	goto 3216
1151	write(8,115)
3216	listall=j.eq.1
	if(j.ne.2) goto 3217
	print 3218
3218	format('&  i1,i2 = ')
	call INPUT2i(n1,n2)
c111	format('&  i1,i2 = ',2i8)
	goto 3212
3217	if(j.ne.3.and.j.ne.4) goto 3212
	print 3219
3219	FORMAT(' Specified value (ms)= ')
	call INPUTr(tc)
      if(discprt) write(8,60)tc
60	format(' Specified value (ms)= ',g13.6)
	if(j.eq.4) tc=-tc
	idebug=j
c
3212	continue
	ppopen=.false.
	print 405
405	format('&Type also P(open) etc each burst [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).eq.'Y') then
	   ppopen=.true.
	   idebug=5
	endif
c Print the assumptions
134	continue	!skip here if all defaults used
	if(discprt.and.ibamp.eq.1) write(8,124)
	if(discprt.and.ibamp.eq.2) write(8,522)
	if(findgap) then
	   if(discprt) write(8,35)
35	   format(
     &	' First burst starts only after gap > tcrit in each file')
	else
	   if(discprt) write(8,125)
125	   format(
     &	' First burst starts with first good opening in each file')
	endif
	if(badend) then
	   if(discprt) write(8,126)
126	   format(' Unusable shut time treated as a valid end of burst')
	else
	   if(discprt) write(8,127)
127	   format(' Unusable shut time aborts a burst')
	endif
c
c Now define the bursts:
121	continue
	Ifst=1
c	ilast=1		!for debugging
	Nbst=0		!NO OF BURSTS
	Nsgap=0		!NO OF GAPS-WITHIN-BST
	Nopen=0		!NO IF INDIV OPENINGS
	NAset=0		!NO OF OPEN (SUBSET A) PERIODS
	NBset=0		!NO OF GAP (SUBSET B) PERIODS
	nbad1=0		!number of bursts aborted by bad open time
	nbad2=0		!number of bursts aborted by unusable shut time
	J=0		!DITTO FOR ID=9,10 IF JTH NEGATIVE
c
c Allocate arrays ops(), gaps(), and if nasmax not big enough,
c deallocate, make bigger and start again
	nasmax=5000		!initially
700	ALLOCATE(ops(nasmax),gaps(nasmax))
c
	if(imode.eq.0) then
         print 80
80	   format(/,
     & ' Now define the bursts',/,
     & ' To see the values, put CAP LOCK on now',/,
     & ' (press any key to continue)')
	   call ANYKEY()
	endif
c
C BURST DEFINED by gaps (zero amp) < Tcrit
C NOP includes any openings inc sublevels
C NSG counts gaps<Tcrit within bursts (so number of sojourns in C&H subset A
C per burst = NSG+1 =< NOP.
C NOTE unusable gap counted as resolvable so it ends burst, but
C an unusable opening causes whole burst to be ignored
C  FIRST LOOK FOR FIRST OPENING (must be usable) in burst.
c OR, IF FINDGAP=T, LOOK FOR USABLE GAP > TCRIT BEFORE STARTING
c Also, if onetcrit=false, need to see which file we are in currently
c and set tcrit accordingly. Also need to ensure gap >tcrit before
c first burst in each file if findgap=true
c Set newfile=true while looking for first burst in current file
c and set it false after a burst has been succesfully found
	ifile=1
	newfile=.true.
	if(nfile.gt.1) then
	   tcrit=tcvals(1)
	   iflast=0
	endif
c Define pflag so 'new file' does not print again when still looking
c for first opening of burst
	pflag=.true.
360	bprint=.false.		!reset for each bst
	deb=debug()			!ditto
c check which file the current ifst is in, and set tcrit accordingly
c (also need to know when new file starts when findgap=true)
	if(nfile.gt.1) then
	   do i=1,nfile
		if(i.eq.nfile) then
		   iend=nint
		else
		   iend=iexstrt(i+1)-1
		endif
		if(ifst.ge.iexstrt(i).and.ifst.le.iend) then	!ifile found
		   ifile=i
		   tcrit=tcvals(ifile)
		   if(ifile.ne.iflast) then
			newfile=.true.
			pflag=.true.
			iflast=ifile
		   endif
		   goto 128
		endif
	   enddo
	endif
	if(newfile.and.pflag.and.imode.eq.0) then
	   print 70,ifile,tcrit
	   if(discprt.and.deb) write(8,70) ifile,tcrit
70	   format(' New file: #',i3,'  tcrit = ',g13.6,' ms')
	endif
128	continue
c Burst extends from TINT(ifst) to TINT(K-1)
c Locate start and end if burst for ibtype=1.
	amp=ampl(ifst)
	OPEN=amp.ne.0
	shut=.not.open
	if(newfile.and.findgap) then	!find gap>tcrit before starting
	   if(shut.and.(tint(ifst).ge.tcrit).and.
     &		(.not.badgap(ifst))) then
		ifst=ifst+1
		if(ifst.gt.nint) goto 99
		amp=ampl(ifst)
		open=amp.ne.0
		if(open.and.(.not.badgap(ifst))) goto 322
	   endif
	else
	   if(open.and.(.not.badgap(ifst))) goto 322
	endif
c    First opening of burst, tint(Ifst), found so go to 322
	Ifst=Ifst+1
	pflag=.false.		!so 'new file' does not print again
	if(Ifst.le.nint) goto 360	!KEEP LOOKING FOR FIRST OPENING
	goto 99
C  Now have 1st opening of bst =tint(Ifst). Do not change Ifst until
c burst ends
322	NOP=1		!Number of openings in current burst
	NSG=0		!No of gaps<Tcrit in current burst
	NAS=0		!No of open (subset A) periods in current bst
	NBS=0		!No of gaps (subset B) in current bst
c			!=NAS-1 (same as NSG for ibtype=1,3)
	topen=0.0	!length of open period
	tgap=0.0	!length of gap-within-bst period
	Nbst=Nbst+1	!Update # of bursts- reversed below if bst is bad
	BAD=.false.	!current burst is usable
44	topen=tint(ifst)	!first indiv opening
48	igflag=0	!currently not in a gap period
	ioflag=1	!currently in an open period
	k=Ifst+1		!LOOK AT NEXT INTERVAL	DOES IT END BURST?
C  k loop starts at label 323
323	continue
	amp=ampl(k)
	open=amp.ne.0
	shut=.not.open
	IF(shut.and.(tc.gt.0.001).and.tint(k).ge.tc) bprint=.true.
c End of burst? If shut can update TGAP here because if this shutting
c ends the burst then the current tgap not used for anything, and
c if it does not end the burst then TGAP to be updated anyway
	if(shut) tgap=tgap+tint(k)
	if(shut) then
	   if(badend) then
		if(tgap.gt.tcrit.or.(badgap(k).and.k.lt.nint)) goto 324
	   else
		if(badgap(k).and.k.lt.nint) then
		   nbad2=nbad2+1
		   bad=.true.	!abort burst
		   if(imode.eq.0) print 71,k
		   if(deb.and.discprt.and.imode.eq.0) then
			write(8,71) k,tint(k),iprops(k)
71			format(
     &	 ' Bad gap #',i5,' aborts burst:',g12.5, ' ms, props = ',i2)
		   endif
		else
		   if(tgap.gt.tcrit) goto 324	!valid end of burst
		endif
	   endif
	endif
C  Go to 324 when end of bst found, with k=index of gap that ends it.
C NOTE that the last interval is always set unusable, but it is not
C a valid end-of-burst
	topen=topen+tint(k)	!instead of next 4 lines
c===
c	if(full) topen=topen+tint(k)	!note-each opening is sub OR full
c	if(sub.and.excsub) goto 40			!omit sub
c	if(sub.and.(id.eq.4.or.id.eq.6)) goto 40	!omit sub
c	if(sub) topen=topen+tint(k)			!add in sub
c40	continue
	if((open.and.badgap(k)).or.(k.eq.nint)) then
	   nbad1=nbad1+1
	   bad=.true.
	endif
C    Burst not counted if it contains an unusable opening (not pos in current
c	version of SCAN ,or if last data point reached before valid end-of-burst
c 	found, so set bad, but carry on to end anyway to make sure start of
c	next burst found correctly
c Now assign lengths for open and gap periods.
	if(open) goto 130	!'open'
	goto 129			!'gap'
c end of ibtype=1
c
c Allocate 'open' and 'gap' lengths- same for all IBTYPE
c Gap ends as soon as open period found (except for 1st open period, NAS=1)
c so assign length of gap that precedes it (only done when 1st opening of
c an open period found- controlled by IOFLAG)
c Next bit done if tint(k) is 'open'
c
130	continue
	nop=nop+1	!no of indiv openings
	ioflag=1	!currently in an open period
	if(igflag.eq.0) goto 131
	nbs=nbs+1
	gaps(nbs)=tgap
c#	if(deb) print 614,nbs,gaps(nbs)
614	format(' GAPS(',i3,')= ',G13.6)
	tgap=0.0		!zero for next gap period
	igflag=0		!clear flag
	goto 131
c Next bit done if tint(k) is 'gap'
c Open period ends as soon as gap found so assign length of open period
c that precedes it (only done when 1st gap of an gap period found-
c controlled by IGFLAG). (For ibtype=1,3 'gap period'=single shut
c period anyway; but for ibtype=2 may have several intervals in one 'gap')
129	nsg=nsg+1	!no of indiv gap periods
	igflag=1	!currently in a gap period
	if(ioflag.eq.0) goto 131
	nas=nas+1
c	if(nas.gt.5000) print 623
c623	format(' ***ERROR: MORE THAN 5000 OPENINGS/BURST') !enlarge OPS()
	if(nas.gt.nasmax) then
	   print 623,nasmax
623	   format(
     &   ' ***More than ',i6,' openings/burst: array size increased') !enlarge OPS()
	   nasmax=nasmax*2
	   nbst=nbst-1	!reverse increment
	   Nbst0=Nbst0-1	!ditto for total
	   if(allocated(ops)) DEALLOCATE(ops,gaps)
	   goto 700
	endif
	ops(nas)=topen
	topen=0.0		!zero for next open period
	ioflag=0		!clear flag
131	continue
c	IF(deb) print 405,Ifst,K,NOP,NSG,nas,nbs,ioflag,igflag
c405	format(' Ifst,K,NOP,NSG,nas,nbs,ioflag,igflag= ',/,8I8)
	k=k+1
c Continue in same burst
C	if(k.le.nint) goto (323,3231,3232) ibtype
	if(k.le.nint) goto 323
	Nbst=Nbst-1	!REVERSE Nbst UPDATE IF NO PROPER ENDING FOUND
	goto 99		!DO NOT COUNT AS BURST IF NO PROPER ENDING FOUND
C
C END OF BURST FOUND. RECORD IT (IF NOT SET BAD). IF SET BAD YVAL(Nbst)
C VALUES WILL BE OVERWRITTEN BY THOSE FOR NEXT BURST
324	continue
	nas=nas+1
	ops(nas)=topen		!assign last opening of the burst
c#	if(deb) print 615,nas,ops(nas)
c if debug, print all intervals up to end of this bst
3241	continue
	newfile=.false.	!set false after a valid burst located
	pflag=.false.
c keep the first and last opening in each burst (allocate arrays once nbst known)
c NO -define later in case bad, so this burst overwritten!
	ilast=k-1
c	if(imode.eq.1) then
c	   ibindex(1,nbst)=ifst
c	   ibindex(2,nbst)=k-1
c	endif
	if(deb) then
c       Type intervals preceding current burst if any (type BURST etc later, in
c       in case this burst is bad, and gets overwritten later
c	   print 132,nbst,Ifst,k-1
c	   if(discprt) write(8,132) nbst,ifst,k-1
c132	   format(/,/,' BURST ',i7,' = intervals ',i5,' to ',i5)
c      print current bst
	   call DEBTYP(0,ifst,k-1,tint,ampl,iprops,
     & 0,-1,-1,yval,.false.,nintt,ndimy)
c     print 2 intervals following current bst
	   call DEBTYP(0,k,k+1,tint,ampl,iprops,
     & 0,-1,-1,yval,.false.,nintt,ndimy)
	endif
c
c	ilast=k		! 1st interval to print for next call
c
c=	if(bad1.and.(ID.eq.4.or.ID.eq.6)) bad=.true.
c
c Calculate 'amplitude' for the burst just defined, if needed (needed
c for amp dist (id=12,13), or if conam1 or exass=true). Burst already
c excluded if it contains an unusable opening
c	if((.not.bad).and.(id.eq.12.or.id.eq.13.or.conam1.or.
c     & exass)) goto 523	!calc amp for burst
c	goto 524		!skip amp calc
c523	continue
	dubamp=.false.
	s=0.
	den=0.
	do 525 m=Ifst,k-1	!go through all intervals in bst
	   m1=m
	   amp=ampl(m1)
	   if(amp.eq.0) goto 525	!skip shut
	   if(dubious(m1)) dubamp=.true.	!true if ANY dubious amp found
	   if(ibamp.eq.1) then
		s=s + tint(m)*amp
		den=den + tint(m)
	   else
		s=s + amp		!accum amp
		den=den+1.0
	   endif
525	continue
c
	if(den.lt.0.01) then
	   call bell(4)
	   print 527
527	   format(' Burst with no openings found!')
	   stop
	else
	   amp=s/den		!define amp
c How to label burst amplitude as dubious now?!
c NO -do later, in case this burst bad and so overwritten
c	   if(imode.eq.1) bampl(nbst)=amp		!amp (set dubious if nec)
c	   if(deb) then
c		print 617,nbst,amp
c		if(discprt) write(8,617) nbst,amp
c617		format(' Mean amp for burst #',i4,' (pA) = ',G13.6,/)
c	   endif
	endif
c
c Exclude bursts as specified
	if(exass.and.dubamp) bad=.true.
c?	if(conam1.and.((amp.lt.amplo).or.(amp.gt.amphi))) bad=.true.
c=524	continue
c If set bad then overwrite the burst that has just been completed
c If findgap=true then, after bad gap at end of file causes bad=true
c here, the gap that ends the overwriten burst is a valid start for
c the next one, so reduce ifst by one here to prevent it being skipped
	if(bad) then
	   Nbst=Nbst-1	!reverse update of Nbst
	   if(findgap) k=k-1	!so ifst is reduced by 1 at label 325
	   goto 325 	!Skip assignment of Yval; look for next bst
	endif
c 	IF(deb) print 4051,Ifst,K,Nbst,NOP,nsg,nas,nbs
c 4051	format(' End of burst: Ifst,K,Nbst,NOP,nsg,nas,nbs= ',/,7I8)
c This burst is OK so now print and define ibindex() and bampl()
	if(imode.eq.1) then
	   ibindex(1,nbst)=ifst
c	   ibindex(2,nbst)=k-1
	   ibindex(2,nbst)=ilast
	   bampl(nbst)=amp		!amp (set dubious if nec)
	endif
	if(deb) then
	   print 132,nbst,Ifst,ilast
	   if(discprt) write(8,132) nbst,ifst,ilast
132	   format(/,/,' BURST ',i7,' = intervals ',i5,' to ',i5)
	   print 617,nbst,amp
	   if(discprt) write(8,617) nbst,amp
617	   format(' Mean amp for burst #',i4,' (pA) = ',G13.6,/)
	endif
c
	if((nas.ne.nbs+1).or.(nbs.ne.nsg).or.(nas.ne.nsg+1)) then
	   call bell(2)
	   print 598,nas,nbs,nop,nsg
598	   format(' ***ERROR: nas,nbs,nop,nsg= ',4i8)
	   pause
	endif
c
c Define Yval
c338	continue
C End of current burst: UPDATE Nsgap ETC BY ADDING VALUES FOR LAST BURST
c print this burst?
c
c Bit added 06/22/89 01:29pm to print total open, total shut and
c Popen for each burst if ppopen=true.
	totop=0.
	totgap=0.
	do i=1,nas
	   totop=totop+ops(i)
	enddo
	if(nas.gt.1) then
	   totop1=0.0
	   do i=1,nas-1
		totop1=totop1+ops(i)
	   enddo
	endif
	do i=1,nbs
	   totgap=totgap+gaps(i)
	enddo
	blength=totop+totgap
	popen=totop/blength
	bleng1=totop1+totgap
	if(nas.gt.1) popen1=totop1/bleng1
c
	pbst=(tc.lt.-0.001).and.bleng1.gt.abs(tc)		!print BURST
	pbst=pbst.or.listall.or.(Nbst.ge.N1.and.Nbst.le.N2).or.
     & bprint
	if(.not.pbst) then
	   if(ppopen) then	!print bst number since skipped if pbst=false
	      print 57, nbst
	      if(discprt) write(8,57) nbst
	   endif
	   goto 354		!skip print of intervals in burst
	endif
357	continue
	if((kplast.gt.0).and.(ifst-1.ge.kplast+1)) then
         print 54
         if(discprt) write(8,54)
54	   format(/,' Intervals between bursts:')
	   do m=kplast+1,ifst-1
	      m1=m
            print 56,m,tint(m),ampl(m1)
            if(discprt) write(8,56) m,tint(m),ampl(m1)
c56	      format(6x,i6,2x,2g13.6)
56	      format(i6,2x,2g13.6)
	   enddo
	   print 108
	   if(discprt) write(8,108)
	endif
      print 57, nbst
      if(discprt) write(8,57) nbst
57	format(' Burst #',i4)
	do m=ifst,k-1
	   m1=m
         if(discprt) write(8,356) m,tint(m),ampl(m1)
355	   print 356,m,tint(m),ampl(m1)
356	   format(i6,2x,2g13.6)
	enddo
	kplast=k-1		!index of last tint() printed
c	print 108
c	if(discprt) write(8,108)
354	continue
c
	if(ppopen) then
	   print 403,ifst,totop,nas,totgap,nbs,
     &	blength,popen,popen1
	   if(discprt)write(8,403) ifst,totop,nas,totgap,nbs,
     &	blength,popen,popen1
c403	   format(' Burst #',i4,/,
403	   format(' Burst starts at interval # ',i9,/,
     &  ' Total open = ',g13.6,' (n = ',i4,');',
     &  ' Total shut = ',g13.6,'(n = ',i4,')',/,
     &  ' Burst length = ',g13.6,'   P(open) = ',f8.5,
     &  ' (exc last op = ',f8.5,')')
c   Also print yval if a valid value is defined -need to define index for
c yval to do this (as in def of nyval below)
	endif
c
	Nsgap=Nsgap+NSG	!Nsgap=TOTAL NO OF SHORT GAPS IN CURRENT EXPT
	Nopen=Nopen+NOP
	NAset=NAset+nAs
	NBset=NBset+nBs
325	Ifst=k+1	!index of interval after that which ended last bst
	if(Ifst.le.nint) goto 360
c END of Ifst loop
C RECORDING OF VALUES FOR LAST BURST COMPLETED- GO TO 360 TO LOOK
C EXPLICITLY FOR FIRST OPENING OF NEXT BURST
C
C ALL BURSTS IDENTIFIED.
	if(allocated(ops)) DEALLOCATE(ops,gaps)
c
c After second pass get here with all bursts defined
c====
99	continue
c
	if(imode.eq.1) goto 4091
	print 581, nbad1,nbad2
	if(discprt) write(8,581) nbad1,nbad2
581	format(/,
     & ' Number of bursts aborted by bad open time = ',i4,/,
     & ' Number of bursts aborted by bad shut time = ',i4,/)
	print 3281
3281	FORMAT(' Print burst parameters [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).EQ.'N') GOTO 4091
c
	if(onetcrit) then
	   if(discprt) write(8,58) tcrit
58	   format(' Critical gap length, tcrit (ms)= ',g13.6)
	else
	   do i=1,nfile
		if(discprt) write(8,59) i,tcvals(i)
59		format(' File # ',i3,': critical gap length (ms)= ',g13.6)
	   enddo
	endif
      if(discprt) write(8,328) NOPEN,nsgap,NASET,nBset,NBST,
     & FLOAT(NOPEN)/FLOAT(NBST),FLOAT(NASET)/FLOAT(NBST),
     & FLOAT(nBset)/FLOAT(NBST)
	print 328,NOPEN,nsgap,NASET,nBset,NBST,
     & FLOAT(NOPEN)/FLOAT(NBST),FLOAT(NASET)/FLOAT(NBST),
     & FLOAT(nBset)/FLOAT(NBST)
328	FORMAT(/,
     & ' No of indiv openings= ',I8,/
     & ' No of indiv gaps within bursts= ',I8,/
     & ,' No of open periods= ',I8,/
     & ,' No of gap periods within bursts= ',I8,/
     & ,' No of bursts= ',I8,/
     & ,' Mean no of indiv openings/burst= ',G13.6,/
     & ,' Mean no of open periods/burst= ',G13.6,/
     & ,' Mean no of gaps/burst= ',G13.6/)
c NB only if ibtype=2 can no of indiv gaps differ from no of gap periods
c
4091	continue
c
	RETURN
	end
