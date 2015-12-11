	subroutine SEQLST(tint,iampl,nint,mavamp,iacrit,acal,trise,iscan,
     & ylo,yhi,nval,kval,treso,tresg,tc)
c 06/06/89 07:02pm Lahey version
c To list sublevel sequences in EKDIST

c Modified 06/09/92 08:38am to allow different tcrit values for each
c  conductance level (treso,tresg added to param list)
c Nov 87: alter criteria for valid burst (Trise constraint etc)
c Dec 87: add 'for sequences that start in level i, x% go to level j'
	dimension kcombs(1000,10),ifreq(1000),kseq(10),kfreq(10,10)
	dimension tint(20480),itot(10)
	integer*2 iampl(20480)
	dimension ylo(10),yhi(10),tc(10)
	dimension jval(50),tval(50),aval(50) !to store a sequnce for printing
	dimension numtot(4),nsame(10)
	logical pon,full,sub,debug,excdub,slock,caplock,reset,samwin,slev
	character*1 ans,UC
	logical discprt
	common/dp/discprt
c
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
107	continue
	print 2323
2323	FORMAT( ' (1)List freq of direct transitions between amp levels.')
	print 2322
2322	FORMAT( ' (2)List all groups of openings with sublevels')
	print 106
106	format(' Option number [1] = ')
	read 21,id
	if(id.le.0) id=1
21	format(i8)
	if(id.ne.1.and.id.ne.2) goto 107
	if(id.eq.2.and.pon()) write(7,2322)
	if(id.eq.2.and.discprt) write(8,2322)
	if(id.eq.1.and.pon()) write(7,2323)
	if(id.eq.1.and.discprt) write(8,2323)
	if(id.eq.1) goto 430
C
C (1) id=2  -SECTION TO LIST ALL GROUPS OF OPENINGS THAT CONTAIN
C   A SUBLEVEL I.E.EITHER ISOLATED SUBLEVELS OR GROUPS OF CONTIGUOUS
C   OPENINGS (WHICH CAN OCCUR ONLY IF THERE ARE ONE OR MORE SUBLEVELS
C   IN THE GROUP. CANNOT CHOOSE THIS OPTION IF SUBLEV IS FALSE.
	n3=0	!no of groups with contigous open levels
	print 23
23	format(' Exclude isolated sublevels? ')
	read 101,ans
	if(UC(ans).eq.'Y'.and.pon()) write(7,24)
	if(UC(ans).eq.'Y'.and.discprt) write(8,24)
24	format( ' Isolated sublevels excluded')
2361	I=1
	J=0	!COUNTS PERIODS IN A FOR THIS EXPT
C NEXT LOOK FOR START OF A GROUP
240	n=0	!number in the group
	FULL=IABS(IACAL(I,iampl)-MAVAMP).LE.IACRIT
	SUB=(IACAL(I,iampl).NE.0).AND.(.NOT.FULL)
C   IF THERE ARE TWO ADJACENT 'FULL' AMPLITUDES THIS SHOULD NOT COUNT
C   AS A SUBLEVEL GROUP- SHOULD HAPPEN ONLY IF ONE OF THEM IS SET
C   UNUSABLE.
C   DOES NOT RETURN HERE IF I=NINT SO NEXT LINES OK
	IF(FULL.AND.(IABS(IACAL(I+1,iampl)-MAVAMP).LE.IACRIT)) GOTO 260
	GOTO 261
260	IF(TINT(I).GT.0.0.AND.TINT(I+1).GT.0.0) goto 27
	goto 263
27	call BELL(2)
	print 262,I,I+1
262	FORMAT( ' ***ERROR. TWO ADJACENT USABLE FULL OPENINGS= ',2I5)
	GOTO 263
261	IF(SUB.OR.(FULL.AND.IACAL(I+1,iampl).NE.0)) GOTO 241
c goto 241 when start of group found
263	I=I+1
	IF(I.LE.NINT) GOTO 240	!START NOT YET FOUND-TRY NEXT INTERVAL
	GOTO 991
241	K=I+1
242	continue
	n=n+1
	jval(n)=k-1	!store for print at end of group
	tval(n)=tint(k-1)
	aval(n)=acal*float(iacal(k-1,iampl))
c242	print 245,K-1,TINT(K-1),ACAL*FLOAT(IACAL(K-1,iampl))
c	if(pon()) write(7,245)K-1,TINT(K-1),ACAL*FLOAT(IACAL(K-1,iampl))
c      if(discprt) write(8,245)K-1,TINT(K-1),ACAL*FLOAT(IACAL(K-1,iampl))
245	FORMAT(I5,2X,2G13.6)
	IF(IACAL(K,iampl).EQ.0) GOTO 243
c goto 243 when NEXT SHUT PERIOD=END OF GROUP FOUND
	K=K+1
	IF(K.LE.NINT) GOTO 242	!KEEP LOOKING FOR END
	GOTO 991	!DATA FINISHED BEFORE END FOUND, SO DO NOT COUNT
c Another group completed
243	if(UC(ans).eq.'Y'.and.n.eq.1) goto 25
	n3=n3+1
	do 26 i1=1,n
	print 245,jval(i1),tval(i1),aval(i1)
	if(pon()) write(7,245)jval(i1),tval(i1),aval(i1)
26    if(discprt) write(8,245)jval(i1),tval(i1),aval(i1)
	print 108	!NEW LINE BETWEEN GROUPS
	if(pon()) print 108
108	format(/)
25	J=J+1	!ANOTHER GROUP COMPLETED
	I=K+1
	IF(I.LT.NINT) GOTO 240	!LOOK FOR START OF NEXT GROUP
C N.B. DOES NOT RETURN IF I=NINT AS IF TINT(NINT) IS START OF A
C GROUP THERE IS NO WAY TO TELL IF THIS GROUP HAS BEEN COMPLETED-
C THE NEXT INTERVAL MIGHT HAVE BEEN OPEN TOO
991	NYVAL=J
c count total no of sublevels in data separately
	n1=0
	n2=0
	do 264 i=1,nint
	i1=i
	j=iacal(i1,iampl)
	if(j.eq.0) goto 264	!shut
	FULL=IABS(j-MAVAMP).LE.IACRIT
	n1=n1+1			!open
	if(.not.full) n2=n2+1	!i.e. if SUB
264	continue
	print 265,n1,n2,nyval,n3
	if(pon()) write(7,265)n1,n2,nyval,n3
      if(discprt) write(8,265)n1,n2,nyval,n3
265	format(
     & ' Total no of openings (inc sublevels)    = ',i8,/,
     & ' Total no of sublevels		          = ',i8,/,
     & ' Total no of open periods with a sublevel= ',i8,/,
     & ' Total no of open periods with contiguous openings= ',i8,/)
	call flush(7)
	pause		!before display
	RETURN	!LISTS ONLY; NO DISTRIBUTION
c
c Now section to list all sublevel sequences (id=1)
430	print 421
421	format(' List sequences of k consecutive levels: k [2] = ')
	read 102,kval
	if(kval.le.0) kval=2
	print 72
72	format(
     & '&Exclude sequences with any dubious amplitudes [Y] ? ')
	read 101,ans
	excdub=UC(ans).ne.'N'
c Setting of tcrit now moved below, done after windows set
c	print 40
c40	format(
c     & '&Exclude sequences that contain a ''short'' event [Y] ? ')
c	read 101,ans
c	tc=-1.		!do not exclude if neg
c	if(UC(ans).eq.'N') goto 41
41	continue
	samwin=.false.
	if(ylo(1).gt.-999.) then		!already defined
	   print 612
612	   format(' Use previously specified amplitude windows [Y] ? ')
	   read 101,ans
	   if(UC(ans).ne.'N') samwin=.true.
	endif
614	continue
	if(.not.samwin) then
	   print 420
420	   format(
     & ' Number of conductance levels to be identified (0=restart)=  ')
	   read 102,nval
	   if(nval.eq.0) goto 41
	endif
	print 108
	if(pon()) write(7,108)
	if(discprt) write(8,108)
	print 731,kval
	if(pon()) write(7,731)kval
      if(discprt) write(8,731)kval
731	format(' Sequences of ',i2,' contiguous amplitude levels:',/,
     & ' Sequence is abandoned if',/,
     & '  (1) an event has its duration set unusable',/,
     & '  (2) an event is found with amplitude not in any of the',/,
     & '      specified ranges')
	if(excdub) print 73
	if(excdub.and.pon()) write(7,73)
	if(excdub.and.discprt) write(8,73)
73	format('  (3) opening with dubious amplitude found')
      print 43
      if(pon()) write(7,43)
      if(discprt) write(8,43)
43	format(
     & '  (4) an event shorter than specified mimimum length is found')
102	format(i8)
	print 732
	if(pon()) write(7,732)
	if(discprt) write(8,732)
732	format(
     & '  (5) two consecutive events found in same amplitude window')
c work out the possible sequences
	call NCOMB(nval,kval,kcombs,k1,ksym)
	if(k1.lt.0) goto 430
	do 434 i=1,k1
434	ifreq(i)=0
c
	if(samwin) goto 615
	print 611
611	format(' Are openings downwards [Y] ? ')
	read 101,ans
	sign=-1.
	if(UC(ans).eq.'N') sign=1.
	print 61
61	format('&Calculate critical amplitudes for each window? ')
	read 101,ans
	if(UC(ans).eq.'N') goto 62
c
c Calculation of current windows
	print 613
613	format(/,
     & ' Now specify the distribution for each amplitude component.',/,
     & ' Give mean amplitude (pA) with positive sign.',/,
     & ' Assumes whole amplitude range is covered, starting with the',/,
     & ' smallest (nearest to zero) amplitude.')
c     & ' smallest (nearest to zero) amplitude. If the first level is',/,
c     & ' for zero amplitude then for 1st distribution set lower',/,
c     & ' limit=-1. (or so), mean=0 and sd,area as for the 2nd',/,
c     & '  (smallest non-zero amp) distribution')
	n0=2
	print 641
641	format(' Is the smallest amplitude the shut level [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') goto 642
	ylo(1)=-0.1
	yhi(1)=0.1
	ylo(2)=0.1
c Omit sign for screen print- amplitudes always positive
	print 453,1,ylo(1),yhi(1)
	if(pon()) write(7,453)1,sign*ylo(1),sign*yhi(1)
      if(discprt) write(8,453)1,sign*ylo(1),sign*yhi(1)
	print 1,1
	read 2,x1,sd1,a1
	n0=3
	goto 643
c
642	print 64
64	format(/,' Lower limit for smallest (nearest zero) current= ')
	call INPUTr(ylo(1))
	print 1,1
	read 2,x1,sd1,a1
c
643	continue
	do 63 i=n0,nval
	print 1,i-1
1	format(' For distribution ',i3,':',/,' Mean,sd,area= ')
	read 2,x2,sd2,a2
2	format(3g13.6)
c
	call ACGAU(Acrit,x1,sd1,a1,x2,sd2,a2)
c
	yhi(i-1)=acrit
	reset=.false.
	print 9,acrit
9	format(' Calculated critical amp = ',g13.6)
93	continue
c Omit sign for screen print- amplitudes always positive
	print 92,i-1,ylo(i-1),yhi(i-1)
92	format(
     & '  window #',i3,' from',1x,g13.6,' to ',g13.6,
     & ' pA:  O.K. [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') then
	   print 5,i-1
5	   format('&   window #',i3,' between a1,a2= ')
	   call INPUT2r(ylo(i-1),yhi(i-1))
	   reset=.true.
	   goto 93		!check values
	endif
c	if(UC(ans).eq.'Y') goto 65
c	print 5
c5	format('&Acrit= ')
c	read 6,acrit
c65	yhi(i-1)=acrit
65	continue
	if(pon()) write(7,453)i-1,sign*ylo(i-1),sign*yhi(i-1)
      if(discprt) write(8,453)i-1,sign*ylo(i-1),sign*yhi(i-1)
	if(reset) goto 71		!can't calc % misclassified if window reset
	u1=(acrit-x1)/sd1
	u2=(acrit-x2)/sd2
	y1=erfs(u1/sqrt(2.))
	y2=erfs(u2/sqrt(2.))
	en=100.*0.5*(a1*(1.-y1)+a2*(1.+y2))
	if(pon()) write(7,7)en
      if(discprt) write(8,7)en
	print 7,en
7	format(20x,' Percent misclassified= ',g13.6)
71	continue
	ylo(i)=yhi(i-1)		!set ready for next loop
	x1=x2
	sd1=sd2
	a1=a2
63	continue	!end of nval loop
c
	print 68
68	format(' Upper limit for largest current= ')
	call INPUTr(yhi(nval))
	if(pon()) write(7,453)nval,sign*ylo(nval),sign*yhi(nval)
      if(discprt) write(8,453)nval,sign*ylo(nval),sign*yhi(nval)
	goto 67
c
c Manual reading of current windows
62	continue
	do 423 i=1,nval
	print 422,i,i
422	format('& (',i2,
     & ') Amp range (abs pA) for level',i2,': low amp, high amp= ')
	call INPUT2r(ylo(i),yhi(i))
c	read 105,ylo(i),yhi(i)
c105	format(2g13.6)
c452	if(pon()) write(7,453)i,sign*ylo(i),sign*yhi(i)
c      if(discprt) write(8,453)i,sign*ylo(i),sign*yhi(i)
c453	format(i4,3x,g13.6,' to ',g13.6,' pA')
423	continue
c
c Both methods:
67	continue
c restore correct sign for windows
	do 74 i=1,nval
	ylo(i)=sign*ylo(i)
	yhi(i)=sign*yhi(i)
	if(ylo(i).lt.yhi(i)) goto 74
	x=ylo(i)
	ylo(i)=yhi(i)
	yhi(i)=x
74	continue
615	continue	!jump here if prev ylo,yhi used
c Amplitude windows now set
c
c Next set tcrit values
	if(samwin) then		!already defined
	   call DCASK(
     &  'Use previously specified resolutions','y',ans)
	   if(ans.eq.'Y') goto 618          !print values
	endif
c  Check if windows include shut level
	slev=.false.
	ishut=0
	do 44 i=1,nval
	 if(ylo(i)*yhi(i).lt.0.0) then
	   slev=.true.
	   ishut=i
	   goto 441
	 endif
44	continue
c
441	print 40,tresg*1.e3
40	format(
     &' Set the minimum length of an interval that is acceptable as ',/,
     &' part of a valid sequence.  Note: this must be long enough',/,
     &' to ensure that the amplitude is as specified, so for open   ',/,
     &' levels at least 2 risetimes will normally be used.  For the',/,
     &' shut level (if included) it MAY be sufficient to use a value',/,
     &' as low as the shut time resolution (',f8.1,' microsec) if   ',/,
     &' this is long enough to ensure that a ''closure'' is really a',/,
     &' complete shutting, rather than a sojourn in a lower         ',/,
     &' subconductance level.')
	call DCASK(
     & 'Minimum length same for open and shut times','n',ans)
	if(ans.eq.'Y') then
	  call RISTIM(trise,-1.,pon())
c	  if(iscan.eq.-1003.or.iscan.eq.-1002) print 443,trise*0.001*2.5
c443    format(' Filter rise time*2.5 (ms)= ',g13.6)
232     print 42
42	  format(
     & ' -exclude events shorter than f risetimes [calc another',
     & ' value]: f= ')
	  call INPUTr(fval)
	  tc1=fval*trise*1.e-3		!msec
	  tres=tresg
	  if(treso.lt.tresg) tres=treso
	  if(tc1.lt.tres) then
		call BELL(2)
c		print 48,tres*1.e3
c48		format(
c     &	' This is shorter than resolution so reset to ',f8.2,' mus')
		fval=tres/(trise*1.e-3)
	    endif
c	  read 6,fval
c6	  format(g13.6)
	  if(fval.lt.0.0001) then
	    print 231
231	    format(' length/trise= ')
	    call INPUTr(fval)
	    call RISTIM(trise,fval,pon())
	    goto 232
	  endif
	  do 45 i=1,nval
45	  tc(i)=fval*trise*1.e-3	!ms
	else		!set different tcrit values for diff levels
	  call RISTIM(trise,-1.,pon())	!print table
	  if(slev) then
	    print 46,tresg*1.e3
46	    format(' Exclude shut times shorter than tc microsec: '
     &	'[',f8.2,'] tc = ')
	    call INPUTr(x)
	    if(x.gt.0.000001) then
		tc(ishut)=x*1.e-3		!msec
	    else
		tc(ishut)=tresg		!msec
	    endif
	    if(tc(ishut).lt.tresg) then
		call BELL(2)
		print 48,tresg*1.e3
c48		format(
c     &	' This is shorter than resolution so reset to ',f8.2,' mus')
		tc(ishut)=tresg
	    endif
	  endif
	  call DCASK(
     &  'Use same minimum length for all open times','y',ans)
	  if(ans.eq.'Y') then
	    print 50
50	    format(
     & '& exclude open times shorter than f risetimes: f= ')
	    call INPUTr(fval)
	    if(fval*trise*1.e-3.lt.treso) then
		call BELL(2)
		print 48,treso*1.e3
48		format(
     &	' This is shorter than resolution so reset to ',f8.2,' mus')
		fval=treso/(trise*1.e-3)
	    endif
	    do 53 i=1,nval
53	    if(i.ne.ishut) tc(i)=fval*trise*1.e-3	!msec
        else
	    do 52 i=1,nval
	    if(i.eq.ishut) goto 52
	    print 47,i,ylo(i),yhi(i)
47	    format(1x,i3,'  for range from ',f8.3,' to ',f8.2,/,
     & '  exclude open times shorter than f risetimes: f= ')
	    call INPUTr(fval)
	    tc(i)=fval*trise*1.e-3	!msec
	    if(tc(i).lt.treso) then
		call BELL(2)
		print 48,treso*1.e3
c48		format(
c     &	' This is shorter than resolution so reset to ',f8.2,' mus')
		tc(i)=treso
	    endif
52	    continue
	  endif
	endif
c
c Now print values
618	continue
	print 454,nval
	if(pon()) write(7,454) nval
      if(discprt) write(8,454) nval
454	format(1x,i4,' current windows:')
	do 617 i=1,nval
	print 453,i,ylo(i),yhi(i),tc(i)*1.e3
	if(pon()) write(7,453)i,ylo(i),yhi(i),tc(i)*1.e3
      if(discprt) write(8,453)i,ylo(i),yhi(i),tc(i)*1.e3
617	continue
453	format(i4,3x,g13.6,' to ',g13.6,' pA: resolution (mus) = ',g13.6)
c Look for first interval in a sequence- ie the first (usable) interval
c that falls into ANY of the specified categories.
c INT0 is index for the first opening in a sequence; INT is index
c within a sequence
	nseq=0
	int0=0		!start at tint(1)
	do 16 i=1,4
16	numtot(i)=0	!initialise totals
	do 161 i=1,10
161	nsame(i)=0	!initialise totals for 'in same window'
	do 162 i=1,nval
	do 162 j=1,nval
162	kfreq(i,j)=0	!initialise kfreq
c
429	int0=int0+1
	j=0		!number in sequence
425	call SEQNUM(int0,num,ylo,yhi,nval,tint,iampl,acal,excdub,tc)
	if(debug())print 438,int0,acal*float(iacal(int0,iampl)),
     & tint(int0),num
438	format(
     & ' 1st i= ',i6,' amp= ',g13.6,'  t= ',g13.6,' Level #= ',i4)
	if(num.gt.0) goto 424	!1st in sequence found
427	int0=int0+1				!for num<0
	if(int0.ge.nint) goto 428		!all obs done
	goto 425		!start not yet found
424	j=j+1
	kseq(j)=num		!1st in sequence
c now look for rest of sequence
	int=int0+1			!next interval
436	if(int.ge.nint) goto 428
	call SEQNUM(int,num,ylo,yhi,nval,tint,iampl,acal,excdub,tc)
	if(debug())print 4381,int,acal*float(iacal(int,iampl)),
     & tint(int),num
4381	format(
     & ' next i= ',i6,' amp= ',g13.6,'  t= ',g13.6,' Level #= ',i4)
c4381	format('  Int= ',i6,': amp= ',g13.6,'. Num= ',i4)
c  If an unspecified amplitude is found (num<0), or if 2 adjacent
c  values in same window found, then abandon this sequence and look
c  for the next
	if(num.gt.0) goto 15		!valid interval
	if(debug()) print 4382
4382	format(' sequence abandoned')
	n1=iabs(num)	!=1,2,3,4
	numtot(n1)=numtot(n1)+1
	goto 429   !abandon this sequence & start new one
15	continue
	j=j+1
	kseq(j)=num		!next in sequence
	if(kseq(j).ne.kseq(j-1)) goto 435	!OK; look for next
	nsame(num)=nsame(num)+1	!record window # in which repeat occurred
	print 437,int-1,int,acal*float(iacal(int-1,iampl)),
     & acal*float(iacal(int,iampl))
437	format( ' Intervals: ',2i6,' are in same window: amps= ',
     & 2g13.6)
	goto 429		!look for new sequence
435	if(j.eq.kval) goto 426		!sequence complete
	int=int+1
	goto 436		!look for next level in seq
c   -a sequence now found; find which of the combos it matches
c   and add 1 to ifreq()
426	nseq=nseq+1			!count no of sequences found
	if(debug())print 439,nseq,(kseq(i),i=1,kval)
439	format(' Seq #',i6,6x,10i4)
	do 431 i=1,k1		!look through all combos
	do 432 j=1,kval
432	if(kseq(j).ne.kcombs(i,j)) goto 431	!no match yet
	ifreq(i)=ifreq(i)+1		!obs combo matches combo #i
	goto 433			!jump out and look for next
431	continue		!end of i loop
c When kval=2 we can now record 'for sequences that start in
c level i, x% go to level j'.  Must do this while we still have
c the current sequence in kseq(). For initial value=level i=kseq(1) for
c the current sequence, keep the number of cases in which next level is
c level j =kseq(2) in kfreq(j), j=1,..,nval (=no of levels) so need array
c kfreq(nval,nval) in size. If level 1=shut level the also do separately
c excluding the shut level.
433	if(kval.gt.2) goto 429		!look for next sequence now
	i=kseq(1)
	j=kseq(2)
	kfreq(i,j)=kfreq(i,j) + 1
	goto 429		!look for next sequence now
c
c All obs used so all seq found: list them, with their frequencies
428	n1=0
	do 4281 i=1,nval
4281	n1=n1+nsame(i)		!total no in same window
	if(pon()) write(7,445)nseq,(numtot(i),i=1,4),n1
      if(discprt) write(8,445)nseq,(numtot(i),i=1,4),n1
	print 445,nseq,(numtot(i),i=1,4),n1
445	format(' Number of valid sequences found= ',i8,/,
     & ' No of sequences started but abandoned because:',/,
     & '  (1) event set as  unusable =              ',i8,/,
     & '  (2) event not in any specified range=     ',i8,/,
     & '  (3) opening with dubious amplitude=       ',i8,/,
     & '  (4) event shorter than Tcrit =            ',i8,/,
     & '  (5) consecutive events in same amp range= ',i8)
	do 4452 i=1,nval
	if(pon()) write(7,4451)i,nsame(i)
      if(discprt) write(8,4451)i,nsame(i)
4452	print 4451,i,nsame(i)
4451	format(
     & '                  number in range #',i3,' = ',i8)
	print 442
	if(pon()) write(7,442)
	if(discprt) write(8,442)
442	format(
     & ' (NB not mutually exclusive: given in order of precedence)',/,/,
     & ' Seq #   Frequency      Sequence')
	if(ksym.eq.0) goto 446
	print 447
	if(pon()) write(7,447)
	if(discprt) write(8,447)
447	format(' symmetric:')
	do 440 i=1,ksym
	if(pon()) write(7,444)i,ifreq(i),(kcombs(i,j),j=1,kval)
      if(discprt) write(8,444)i,ifreq(i),(kcombs(i,j),j=1,kval)
440	print 444,i,ifreq(i),(kcombs(i,j),j=1,kval)
444	format(i6,2x,i6,6x,10i4)
446	if(ksym.eq.k1) goto 451
	print 450
	if(pon()) write(7,450)
	if(discprt) write(8,450)
450	format(' unsymmetric:')
	j1=1
	do 448 i=ksym+1,k1
	if(pon()) write(7,444)i,ifreq(i),(kcombs(i,j),j=1,kval)
      if(discprt) write(8,444)i,ifreq(i),(kcombs(i,j),j=1,kval)
	print 444,i,ifreq(i),(kcombs(i,j),j=1,kval)
	j1=j1+1
	if(mod(j1,2).eq.0) goto 448
	print 108
	if(pon()) write(7,108)
	if(discprt) write(8,108)
448	continue
451	call flush(7)
c Now, when kval=2 print kfreq() values
	if(kval.gt.2) goto 999
c  Get totals and calc percentages. Total number that start in level i =
c sum of row i of kfreq()
	lev1=1			!do not exclude shut level
170	continue		!return here to repeat excluding shut level
	itott=0			!total no of valid trans
	do 163 i=lev1,nval
	itot(i)=0
	do 164 j=lev1,nval
164	if(i.ne.j) itot(i)=itot(i) + kfreq(i,j)
	itott=itott + itot(i)
163	continue
	print 1632,itott
	if(pon()) write(7,1632)itott
      if(discprt) write(8,1632)itott
1632	format(2x,i8,' valid sequences')
	if(itott.eq.0) goto 172
c
	do 1631 i=lev1,nval
	x1=100.*float(itot(i))/float(itott)
	print 165,itot(i),x1,i
	if(pon()) write(7,165)itot(i),x1,i
      if(discprt) write(8,165)itot(i),x1,i
165	format(2x,i6,
     & ' sequences (',f7.2,' percent) start in level',i4)
	do 166 j=lev1,nval
	if(itot(i).eq.0) goto 166		!skip
	if(i.eq.j) goto 166		!skip
	x=100.*float(kfreq(i,j))/float(itot(i))
	print 167,kfreq(i,j),x,j
	if(pon()) write(7,167)kfreq(i,j),x,j
      if(discprt) write(8,167)kfreq(i,j),x,j
167	format(10x,i6,' (',f7.2,' percent) go to level ',i4)
166	continue		!end of j loop
1631	continue		!end of i loop
c Now repeat this excluding level 1 if level 1=shut level
172	if(ylo(1)*yhi(1).lt.0.) goto 168	!opp signs so include zero
	goto 999
168	if(lev1.eq.2) goto 999		!already done
	print 169
	if(pon()) write(7,169)
	if(discprt) write(8,169)
169	format(/,' Repeat for open-open transitions only')
	lev1=2
	goto 170
c
999	call flush(7)
	print 171
	if(pon()) write(7,171)
	if(discprt) write(8,171)
171	format(/,/)
	pause		!before display
	RETURN	!LISTS ONLY; NO DISTRIBUTION
C
	END



	subroutine SEQNUM(int,num,ylo,yhi,nval,tint,iampl,acal,excdub,tc)
c Returns NUM=sublevel number for INTth interval=1,2,...,nval
cc Returns NUM=0 if:
c Returns NUM<0 as follows (for  debug easier if reason for abandoning
c sequence is known)
c    (1) if interval is unusable,NUM=-1
c or (2) interval is in none of the categories,NUM=-2
c or (3) ,if exdub=true, amplitude is set to be dubious, NUM=-3
c or (4) if interval is less than crit length TC (unless TC is
c	 negative), NUM=-4
	dimension TINT(20480)
	integer*2 iampl(20480)
	dimension ylo(10),yhi(10),tc(10)
	logical excdub
c
c Dec 87. Rearranged so that always tested to see if amp is in
c  in one of the specified ranges as this is a common reason for
c  sequence to be abandoned (if ranges do not cover all obs), so
c  do not want printout to specify that sequence abandoned because
c  event was too short (as it may be) if it was not in range anyway.
c If not in any range num=-2 (unless unusable in which case num=-1
c whatever other criteria are met) so ranking is
c	If unusable then num=-1 whatever other criteria met
c	If not in range then num=-2 (unless 1 is true)
c	If not dubious (and excdub) then num=-3 (unless 1 or 2 are true)
c	If short then num=-4 (unless 1,2 or 3 are true)
c
	num=0
	time=tint(int)
	if(time.le.0.) goto 98	!cannot test length
c	if((tc.gt.0.).and.time.lt.tc) num=-4
	if(excdub.and.iabs(iampl(int)).gt.9998) num=-3
c if dubious amp AND short num=-3 (=dubious)
	amp=ACAL*FLOAT(IACAL(int,iampl))
	do 1 i=1,nval
c	print 2,i,amp,ylo(i),yhi(i)
c2	format(' IN SEQNUM: i,amp,ylo,yhi= ',i4,3g13.6)
	i1=i
c	if(ylo(i).le.amp.and.amp.lt.yhi(i)) then
	if(amp.gt.ylo(i).and.amp.lt.yhi(i)) then
	   if(time.gt.tc(i)) goto 99	!valid value
	   num=-4		!in a range, but too short
	endif
1	continue
	if(num.ne.-4) num=-2		!not in any range
	RETURN
98	num=-1
	RETURN
99	if(num.eq.0) num=i1	!if already set negative then leave it
	RETURN
	end


	subroutine NCOMB(nval,kval,kcombs,k1,ksym)
C
	dimension	k(10)
	dimension kcomb(1000,10),kcombs(1000,10)
	character*1 ans,UC
C To enumerate of sublevel order possibilities (see TCOMB.FOR)
c N=no of states
c k=length of sequence
c KCOMBS() contains K1 sequences, the first KSYM rows being the
c symmetrical ones (if any, ie if kval is odd) and the rest being
c unsymmetrical, each one being followed by its mirror image.
c
	print 105,nval*(nval-1)**(kval-1)
105	format(' No of combos expected= ',i8, ' O.K. [Y] ? ')
	read 112,ans
112	format(a1)
	k1=-1
	if(UC(ans).eq.'N') RETURN
	k1=0		!to count combos
c
	do 1 i1=1,nval
	k(1)=i1
c
	do 2 i2=1,nval
	if(i2.eq.i1) goto 2
	k(2)=i2
	if(kval.gt.2) goto 31
	k1=k1+1
c	print 100,(k(i),i=1,kval)
100	format(10i4)
	call ASCOMB(k,kcomb,k1,kval)
	goto 2
31	continue
c
	do 3 i3=1,nval
	if(i3.eq.i2) goto 3
	k(3)=i3
	if(kval.gt.3) goto 41
	k1=k1+1
c	print 100,(k(i),i=1,kval)
	call ASCOMB(k,kcomb,k1,kval)
	goto 3
41	continue
	do 4 i4=1,nval
	if(i4.eq.i3) goto 4
	k(4)=i4
	if(kval.gt.4) goto 51
	k1=k1+1
c	print 100,(k(i),i=1,kval)
	call ASCOMB(k,kcomb,k1,kval)
	goto 4
51	continue
	do 5 i5=1,nval
	if(i5.eq.i4) goto 5
	k(5)=i5
	if(kval.gt.5) goto 61
	k1=k1+1
c	print 100,(k(i),i=1,kval)
	call ASCOMB(k,kcomb,k1,kval)
	goto 5
61	continue
	do 6 i6=1,nval
	if(i6.eq.i5) goto 6
	k(6)=i6
	if(kval.gt.6) goto 71
	k1=k1+1
c	print 100,(k(i),i=1,kval)
	call ASCOMB(k,kcomb,k1,kval)
	goto 6
71	continue
	do 7 i7=1,nval
	if(i7.eq.i6) goto 7
	k(7)=i7
	if(kval.gt.7) goto 81
	k1=k1+1
c	print 100,(k(i),i=1,kval)
	call ASCOMB(k,kcomb,k1,kval)
	goto 7
81	continue
	do 8 i8=1,nval
	if(i8.eq.i7) goto 8
	k(8)=i8
	if(kval.gt.8) goto 91
	k1=k1+1
c	print 100,(k(i),i=1,kval)
	call ASCOMB(k,kcomb,k1,kval)
	goto 8
91	continue
	do 9 i9=1,nval
	if(i9.eq.i8) goto 9
	k(9)=i9
	if(kval.gt.9) goto 101
	k1=k1+1
c	print 100,(k(i),i=1,kval)
	call ASCOMB(k,kcomb,k1,kval)
	goto 9
101	continue
	do 10 i10=1,nval
	if(i10.eq.i9) goto 10
	k(10)=i10
	if(kval.gt.10) goto 97	!error
	k1=k1+1
c	print 100,(k(i),i=1,kval)
	call ASCOMB(k,kcomb,k1,kval)
c	goto 3
10	continue
9	continue
8	continue
7	continue
6	continue
5	continue
4	continue
3	continue
2	continue
1	continue
c	print 104,k1,nval*(nval-1)**(kval-1)
c104	format(' No of combos found, expected= ',2i8)
c
c Now sort combos to get symmetric ones first (if any, ie if k odd)
c and put results in kcombs(). Then list assymetrc ones, each followed
c by its mirror image.
	ks1=0		!index for kcombs
	ksym=0		!no of sym combos
	ir=1 + kval/2	!index of central value
c	print 122,ir
c122	format(' ir= ',i3)
	if(mod(kval,2).eq.0) goto 114	!kval even, so none symmetric
c
	do 115 i=1,k1
	ival=i
115	call SYM(ir,ival,kcomb,kcombs,ks1,kval)
cd	do 120 i=1,ks1
cd120	print 100,(kcombs(i,j),j=1,kval)
cd	print 121,ks1
cd121	format(' Number if symmetric combs= ',i6)
	ksym=ks1
c
114	continue
c now look for asymmetric ones, and their mirror images
c First unused one encountered must be asymmetric so copy it, so mark,
c then copy its mirror image and must search again for this combo
c to mark it so not used again
	do 116 i=1,k1
	ival=i
	if(kcomb(ival,1).lt.0) goto 116		!already copied (symm)
	ks1=ks1+1		!increment index
	do 117 j=1,kval
	kcombs(ks1,j)=kcomb(i,j)	! copy it
117	k(kval-j+1)=kcomb(i,j)		!mirror image in k()
	ks1=ks1+1		!increment index
	do 1171 j=1,kval
1171	kcombs(ks1,j)=k(j)	! copy mirror image
	kcomb(i,1)=-1		!mark the ones that have been copied
c look for mirror image and mark it
	do 118 i1=i,k1
	if(kcomb(i1,1).lt.0) goto 118		!already copied
	do 119 j1=1,kval
119	if(k(j1).ne.kcomb(i1,j1)) goto 118	!not mirror
	kcomb(i1,1)=-1			!mirror- mark it
	goto 116		!and leave loop
118	continue
116	continue
c
c	print 123,ksym
c123	format(' Number if symmetric combs= ',i6)
c	if(ksym.eq.0) goto 126
c	do 106 i=1,ksym
c	print 100,(kcombs(i,j),j=1,kval)
c106	continue
c126	kunsym=k1-ksym
c	print 124,kunsym
c124	format(' Number if unsymmetric combs= ',i6)
c	do 125 i=ksym+1,k1
c	print 100,(kcombs(i,j),j=1,kval)
c125	continue
	goto 99
c
97	print 110
110	format(' k>10 not allowed')
99	continue
	RETURN
	END

	subroutine ASCOMB(k,kcomb,k1,kval)
c Assign combs to rows of Kcomb()
	dimension	k(10)
	dimension kcomb(1000,10)
	do 1 i=1,kval
1	kcomb(k1,i)=k(i)
	return
	end

	subroutine SYM(ir,ival,kcomb,kcombs,ks1,kval)
	dimension kcomb(1000,10),kcombs(1000,10)
	dimension k(10)
c to test symmetry
c copy current row
	do 1 i=1,kval
	k(i)=kcomb(ival,i)
1	continue
cd	print 11,ival,(k(i),i=1,kval)
cd11	format(i8,2x,10i4)
c
	do 2 i=1,ir
2	if(k(i).ne.k(kval-i+1)) goto 9		!not sym
c sym, so copy to kcombs(ks1,j)
	ks1=ks1+1		!increment index
	do 3 j=1,kval
3	kcombs(ks1,j)=k(j)
	kcomb(ival,1)=-1	!mark the ones that have been copied
9	RETURN
	end



