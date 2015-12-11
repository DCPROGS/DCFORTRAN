	subroutine SEQLST(tint,ampl,iprops,nint,nintt,avamp,acrit,
     & trise,iscan,ylo,yhi,ymid,kval,treso,tresg,tc,index,
     & kfile,nfile,name,idiscn,nvalsav)
c	subroutine SEQLST(tint,iampl,nint,mavamp,iacrit,acal,trise,iscan,
c     & ylo,yhi,nval,kval,treso,tresg,tc)
c
c Version for new EKDIST/SCAN 02/16/94 04:27pm
c
c 06/06/89 07:02pm Lahey version
c To list sublevel sequences in EKDIST
c
c Modified 07/14/94 08:49am to record 1st interval # in original data
c (as in SCANDAT, before resolution imposed -using index()) of each
c sequence.  Kept in intvals(i,j) for jth value (up to 5000) for the ith
c combo.  Written to EKDIST.TMP so sequences can be viewed in SCAN.
c
c Modified 06/09/92 08:38am to allow different tcrit values for each
c  conductance level (treso,tresg added to param list)
c Nov 87: alter criteria for valid burst (Trise constraint etc)
c Dec 87: add 'for sequences that start in level i, x% go to level j'
	ALLOCATABLE XVAL,YVAL,XCAL,YCAL
	real XVAL(:,:),YVAL(:,:)		!for VPLOT
	real XCAL(:,:),YCAL(:,:)		!for VPLOT/VHIST
	ALLOCATABLE::ndat,icurvd,isym,ijoin,syms
	integer ndat(:),icurvd(:),isym(:),ijoin(:)
	real*4 syms(:)
	ALLOCATABLE::ncal,icurvc,iline
	integer ncal(:),icurvc(:),iline(:)
c	integer ndat(1),icurvd(1),isym(1),ijoin(1)	!for data
c	real syms(1)				!for data
c	integer ncal(1),icurvc(1),iline(1)	!for calc curve
	ALLOCATABLE:: weight,icurvw
	real*4 weight(:,:)
	integer*4 icurvw(:)
	LOGICAL fitted,doframe,draft,autplt,interp,landscap,ivplot
	logical allocated
	character*40 titlex,titley
	character*64 title1
c
	integer*4 index(nintt)
	integer kfile(50)
	real*4 TINT(nintt),ampl(nintt)
	integer*1 iprops(nintt)
	allocatable::kcombs,ifreq,intvals
	integer*4 kcombs(:,:),ifreq(:),intvals(:,:)
	integer*4 kseq(10),kfreq(10,10)
	integer*4 itot(10),numtot(4),nsame(10)
	real*4 ylo(10),yhi(10),ymid(10),tc(10),yline(20)
	dimension jval(50),tval(50),aval(50) !to store a sequnce for printing
	logical pon,full,sub,debug,excdub,slock,caplock,reset,samwin,slev
	logical dubious,badint,btest,repeat,conam,good,skip,incmult
	character*1 ans,UC
	character name*12
	logical discprt
c For 3D display of dependency plot
	allocatable::freq,xx,yy,ampx,ampy
	real*4 freq(:,:),xx(:,:),yy(:,:),ampx(:),ampy(:)
	logical excsam,exc0,excwin
c
	common/dp/discprt
	common/rand/ix,iy,iz
	common/ampsav/ncompa,ameana(10),areaa(10),sda(10)
c
c Define functions
	pon()=slock()
	debug()=caplock()
	dubious(i)=BTEST(iprops(i),0)	!ampl(i) was dubious (bit 0 set ='1')
	badint(i)=BTEST(iprops(i),3)	!tint(i) was unusable (bit 3='8' set)
c
101	format(a1)
	nval=0		!until defined
107	continue
	print 2323
2323	FORMAT(' (1) List freq of direct transitions between amp levels.')
	print 2322
2322	FORMAT(' (2) List all groups of openings with sublevels')
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
	full=abs(ampl(i)-avamp).le.acrit
c==	FULL=IABS(IACAL(I,iampl)-MAVAMP).LE.IACRIT
	sub=(ampl(i).ne.0).and.(.not.full)
C   IF THERE ARE TWO ADJACENT 'FULL' AMPLITUDES THIS SHOULD NOT COUNT
C   AS A SUBLEVEL GROUP- SHOULD HAPPEN ONLY IF ONE OF THEM IS SET
C   UNUSABLE.
C   DOES NOT RETURN HERE IF I=NINT SO NEXT LINES OK
	if(full.and.(abs(ampl(i+1)-avamp).le.acrit)) goto 260
	goto 261
c=260	if(TINT(I).GT.0.0.AND.TINT(I+1).GT.0.0) goto 27
260	if((.not.badint(i)).and.(.not.badint(i+1))) goto 27
	goto 263
27	call BELL(2)
	print 262,I,I+1
262	format(' ***ERROR. TWO ADJACENT USABLE FULL OPENINGS= ',2I5)
	goto 263
261	if(sub.or.(full.and.ampl(i+1).ne.0)) goto 241
c goto 241 when start of group found
263	i=i+1
	if(i.le.nint) goto 240	!START NOT YET FOUND-TRY NEXT INTERVAL
	goto 991
241	k=i+1
242	continue
	n=n+1
	jval(n)=k-1	!store for print at end of group
	tval(n)=tint(k-1)
c==	aval(n)=acal*float(iacal(k-1,iampl))
	aval(n)=ampl(k-1)
c242	print 245,K-1,TINT(K-1),aval(n)
c	if(pon()) write(7,245)K-1,TINT(K-1),aval(n)
c      if(discprt) write(8,245)K-1,TINT(K-1),aval(n)
245	format(I5,2X,2G13.6)
c==	IF(IACAL(K,iampl).EQ.0) GOTO 243
	if(ampl(k).eq.0) goto 243
c goto 243 when NEXT SHUT PERIOD=END OF GROUP FOUND
	k=k+1
	if(k.le.nint) goto 242	!KEEP LOOKING FOR END
	goto 991	!DATA FINISHED BEFORE END FOUND, SO DO NOT COUNT
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
25	j=j+1	!ANOTHER GROUP COMPLETED
	i=k+1
	if(i.lt.nint) goto 240	!LOOK FOR START OF NEXT GROUP
C N.B. DOES NOT RETURN IF I=NINT AS IF TINT(NINT) IS START OF A
C GROUP THERE IS NO WAY TO TELL IF THIS GROUP HAS BEEN COMPLETED-
C THE NEXT INTERVAL MIGHT HAVE BEEN OPEN TOO
991	nyval=j
c count total no of sublevels in data separately
	n1=0
	n2=0
	do 264 i=1,nint
	i1=i
c=	j=iacal(i1,iampl)
c=	if(j.eq.0) goto 264	!shut
c=	FULL=IABS(j-MAVAMP).LE.IACRIT
	amp=ampl(i1)
	if(amp.eq.0) goto 264	!shut
	full=abs(amp-avamp).le.acrit
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
430	continue
c
c List frequencies
	call DCASK('List transition frequencies','y',ans)
	if(ans.eq.'N') goto 97
c Allocate arrays
	if(.not.allocated(kcombs)) then
	   ALLOCATE(kcombs(100,10),ifreq(100),intvals(10,5000))
	endif
	print 421
421	format(' List sequences of k consecutive levels: k [2] = ')
	kval=2
	call INPUTi(kval)
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
	   if(UC(ans).ne.'N') then
		samwin=.true.
		nval=nvalsav
	   endif
	endif
614	continue
	if(.not.samwin) then
	   nval=nvalsav
	   print 420,nvalsav
420	   format(
     & ' Number of conductance levels to be identified (0=restart) [',
     &	i2,'] =  ')
	   call INPUTi(nval)
	   nvalsav=nval
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
c Section to define windows
	print 611
611	format(' Are openings downwards [Y] ? ')
	read 101,ans
	sign=-1.
	if(UC(ans).eq.'N') sign=1.
	print 61
61	format('&Calculate critical amplitudes for each window [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') goto 62
c
c Calculation of calculation of amplitude windows
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
	reset=.false.
	if(ncompa.le.0) then
	   ncomp=0		!amplitude pdf not yet defined  (see common/ampsav/)
	else
	   ncomp=ncompa
	endif
c
4	print 641
641	format(' Is the smallest amplitude the shut level [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') then
	   n0=2
	   print 64
64	   format(/,' Lower limit for smallest (nearest zero) current = ')
	   call INPUTr(ylo(1))
c	   if(ncomp.le.0) then
c	      print 1,1
c 		call INPUT2r(x1,sd1)
c		print 11
c		call INPUTr(a1)
c	   else
	   if(ncomp.gt.0) then	!if ncomp=0 components requested in ACRITS
		x1=ameana(1)
		sd1=sda(1)
		a1=areaa(1)
		print 3,1,x1,sd1,a1
		read 101,ans
		if(UC(ans).eq.'N') then
		   ncomp=0
		   goto 4
		endif
	   endif
	   ymid(1)=x1
	else
	   n0=3
	   ylo(1)=-0.01
	   yhi(1)=0.01
	   ymid(1)=0.
	   ylo(2)=0.01
94	   print 92,1,ylo(1),yhi(1)
	   read 101,ans
	   if(UC(ans).eq.'N') then
		print 5,1
		call INPUT2r(ylo(1),yhi(1))
		reset=.true.
		ylo(2)=yhi(1)
		goto 94		!check values
	   endif
c Omit sign for screen print- amplitudes always positive
c	   print 453,1,ylo(1),yhi(1)
c	   if(pon()) write(7,453)1,sign*ylo(1),sign*yhi(1)
c         if(discprt) write(8,453)1,sign*ylo(1),sign*yhi(1)
	   if(ncompa.gt.0) then		!if ncomp=0 components requested in ACRITS
		x1=ameana(1)
		sd1=sda(1)
		a1=areaa(1)
		print 3,1,x1,sd1,a1
		read 101,ans
		if(UC(ans).eq.'N') then
		   ncomp=0
		   goto 4
		endif
c	   else
c		print 1,1
c 		call INPUT2r(x1,sd1)
c		print 11
c		call INPUTr(a1)
	   endif
	   ymid(2)=x1
	endif
c
	kth=0
	do 63 i=n0,nval
	   i1=i-1
	   if(ncompa.gt.0) then
		x2=ameana(i1)
		sd2=sda(i1)
		a2=areaa(i1)
		print 3,i-1,x2,sd2,a2
3		format(' Amplitude distribution component #',i3,/,
     &	' mean, SD, area = ',3(g13.6,2x),'O.K. [Y] ? ')
		read 101,ans
		if(UC(ans).eq.'N') then
		   ncomp=0
		   goto 4
		endif
c	   else		!components now requested in ACRITS if ncomp=0
c 		print 1,i-1
c1		format(' For amplitude distribution, component #',i3,':',/,
c     & 	' Specify: mean, SD = ')
c		call INPUT2r(x2,sd2)
c		print 11
c11		format(' and area = ')
c		call INPUTr(a2)
cc		read 2,x2,sd2,a2
cc2   		format(3g13.6)
	   endif
	   ymid(i)=x2
c
	   kth=kth+1
	   call ACRITS(ameana,areaa,sda,ncompa,kth,.false.)
	   ncomp=ncompa
c=	   call ACGAU(Acrit,x1,sd1,a1,x2,sd2,a2)
c
c=	   yhi(i-1)=acrit
c=	   print 9,acrit
c=9	   format(' Calculated critical amp = ',g13.6)
91	   acrit=-1.
	   print 9
9	   format(' Critical amplitude = ')
	   call INPUTr(acrit)
	   if(acrit.le.0.) goto 91
	   yhi(i-1)=acrit
93	   continue
c Omit sign for screen print- amplitudes always positive
	   print 92,i-1,ylo(i-1),yhi(i-1)
92	   format(
     & '  window #',i3,' from',1x,g13.6,' to ',g13.6,
     & ' pA:  O.K. [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
      	print 5,i-1
5    		format('&   window #',i3,' between a1, a2= ')
		call INPUT2r(ylo(i-1),yhi(i-1))
		reset=.true.
		goto 93		!check values
	   endif
65	continue
c	if(pon()) write(7,453)i-1,sign*ylo(i-1),sign*yhi(i-1)
c      if(discprt) write(8,453)i-1,sign*ylo(i-1),sign*yhi(i-1)
c	if(reset) goto 71		!can't calc % misclassified if window reset
c71	continue
	ylo(i)=yhi(i-1)		!set ready for next loop
	x1=x2
	sd1=sd2
	a1=a2
63	continue	!end of nval loop
c
	print 68
68	format(' Upper limit for largest current= ')
	call INPUTr(yhi(nval))
c	if(pon()) write(7,453)nval,sign*ylo(nval),sign*yhi(nval)
c      if(discprt) write(8,453)nval,sign*ylo(nval),sign*yhi(nval)
	goto 67
c
c Manual reading of current windows
62	continue
	print 641
c641	format(' Is the smallest amplitude the shut level [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') then
	   n0=1
	else
	   ylo(1)=-0.01
	   yhi(1)=0.01
	   ymid(1)=0.
	   print 4221,1,1,ylo(1),yhi(1)
4221	   format('& (',i2,
     & ') Amp range (abs pA) for level',i2,': low amp, high amp = ',
     &    2f8.3,/)
	   n0=2
	endif
	do i=n0,nval
	   print 422,i,i
422	   format('& (',i2,
     &    ') Amp range (abs pA) for level',i2,': low amp, high amp = ')
	   call INPUT2r(ylo(i),yhi(i))
	   ymid(i)=0.5*(ylo(i)+yhi(i))
c452	   if(pon()) write(7,453)i,sign*ylo(i),sign*yhi(i)
c         if(discprt) write(8,453)i,sign*ylo(i),sign*yhi(i)
c453	   format(i4,3x,g13.6,' to ',g13.6,' pA')
	enddo
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
	tres=tresg
	if(treso.gt.tresg) tres=treso
	call DCASK(
     & 'Minimum length same for open and shut times','n',ans)
	if(ans.eq.'Y') then
	  call RISTIM(trise,-1.,pon())
c	  if(iscan.eq.-1003.or.iscan.eq.-1002) print 443,trise*0.001*2.5
c443    format(' Filter rise time*2.5 (ms)= ',g13.6)
232     tcrit=tres	!default
	  print 42,tres
42	  format(
     & ' -exclude events shorter than tcrit [',f8.2,' ms]:',
     & ' tcrit = ')
	  call INPUTr(tcrit)
	  fval=tcrit/(trise*1.e-3)
	  tc1=fval*trise*1.e-3		!msec
	  tres=tresg
	  if(treso.lt.tresg) tres=treso
	  if(tc1.lt.tres-0.001) then
		call BELL(2)
		call DCASK('Do you really want no minimum length','Y',ans)
		if(ans.eq.'N') goto 441
		fval=0.0
		tc1=0.0
	  endif
c	  if(fval.lt.0.0001) then
c	    print 231
c231	    format(' length/trise= ')
c	    call INPUTr(fval)
c	    call RISTIM(trise,fval,pon())
c	    goto 232
c	  endif
	  do i=1,nval
	     tc(i)=fval*trise*1.e-3	!ms
	  enddo
	else		!set different tcrit values for diff levels
	  call RISTIM(trise,-1.,pon())	!print table
	  if(slev) then
45	    continue
	    x=tresg*1.e3		!default
	    print 46,x
46	    format(' Exclude shut times shorter than tc microsec: ',
     &	'[',f8.2,'] tc = ')
	    call INPUTr(x)
	    if(x.lt.0.00001) then
		call DCASK('Do you really want no minimum length','Y',ans)
		if(ans.eq.'N') goto 45
	    endif
	    tc(ishut)=x*1.e-3		!msec
	  endif
53	  call DCASK(
     &  'Use same minimum length for all open times','y',ans)
	  if(ans.eq.'Y') then
	    fval=2.
	    print 50
50	    format(
     & '  -exclude open times shorter than f risetimes: f [2] = ')
	    call INPUTr(fval)
	    if(fval*trise*1.e-3.lt.treso) then
		call BELL(2)
		call DCASK('Do you really want no minimum length','Y',ans)
		if(ans.eq.'N') goto 53
		fval=0.0
		tc1=0.0
	    endif
	    do i=1,nval
	       if(i.ne.ishut) tc(i)=fval*trise*1.e-3	!msec
	    enddo
        else
	    do i=1,nval
	      if(i.ne.ishut) then
52		  print 47,i,ylo(i),yhi(i)
47		  format(1x,i3,'  for range from ',f8.3,' to ',f8.2,/,
     & '  -exclude open times shorter than f risetimes: f = ')
		  call INPUTr(fval)
		  tc(i)=fval*trise*1.e-3	!msec
		  if(tc(i).lt.treso) then
			call BELL(2)
			call DCASK('Do you really want no minimum length',
     &		 'Y',ans)
			if(ans.eq.'N') goto 52
			fval=0.0
			tc1=0.0
			tc(i)=tc1
		  endif
		endif
	    enddo
	  endif
	endif
c
c Now print values
618	continue
	print 454,nval
	if(pon()) write(7,454) nval
      if(discprt) write(8,454) nval
454	format(/,1x,i4,' current windows, defined thus:')
	do 617 i=1,nval
	print 453,i,ylo(i),yhi(i),tc(i)*1.e3
	if(pon()) write(7,453)i,ylo(i),yhi(i),tc(i)*1.e3
      if(discprt) write(8,453)i,ylo(i),yhi(i),tc(i)*1.e3
617	continue
453	format(i4,3x,g13.6,' to ',g13.6,' pA: resolution (mus) = ',g13.6)
c
c Look for first interval in a sequence- ie the first (usable) interval
c that falls into ANY of the specified categories.
c INT0 is index for the first opening in a sequence; INT is index
c within a sequence
c If incmult=true then multiple transitions within a window are counted
c as a single sojourn in that window for purposes of defining a sequence
c e.g. 1-2-3 sequence would be counted if there were several transitions
c between open states that were (a) all within window #2, and (b) had TOTAL
c length > tc (and usable etc)
	incmult=.false.
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
c
	if(incmult) then
325	   continue
	   ttot=0.0		!total length of time in present level
	   call SEQNUM2(num,ylo,yhi,nval,ampl(int0),
     &    excdub,dubious(int0),badint(int0))
	   if(num.le.0) then		!1st in sequence not yet found
		int0=int0+1				!for num<0
		if(int0.ge.nint) goto 428		!all obs done
		goto 325		!start not yet found
	   endif
c	Is next interval in the same window
=============
==========problem -if incmult case we require only that total time in
==========current window is >tc, but at present any indiv sojourn<tc
==========causes rejection via num=-4 (ex SEQNUM)
=========better put duration test in sep (not seqnum) subroutine so when
=========incmult=true this is called only after all soujourns in the
=========current window are ended, and their total duration found
=============
	else		!single sojourn allowed in each window (as original)
425	   continue
	   call SEQNUM(num,ylo,yhi,nval,tint(int0),ampl(int0),
     &    excdub,tc,dubious(int0),badint(int0))
	   if(debon)print 438,int0,ampl(int0),tint(int0),num
438	   format(
     & ' 1st i= ',i6,' amp= ',g13.6,'  t= ',g13.6,' Level #= ',i4)
c=	   if(num.gt.0) goto 424	!1st in sequence found
	   if(num.le.0) then		!1st in sequence not yet found
		int0=int0+1				!for num<0
		if(int0.ge.nint) goto 428		!all obs done
		goto 425		!start not yet found
	   endif
c
	   j=j+1
	   kseq(j)=num		!1st in sequence
c Now look for rest of sequence
	   int=int0+1			!next interval
436	   continue			!return to look for next level in sequence
	   if(int.ge.nint) goto 428
	   call SEQNUM(num,ylo,yhi,nval,tint(int),ampl(int),
     &	 excdub,tc,dubious(int),badint(int))
	   if(debon)print 4381,int,ampl(int),tint(int),num
4381	   format(
     & ' next i= ',i6,' amp= ',g13.6,'  t= ',g13.6,' Level #= ',i4)
c  If an unspecified amplitude is found (num<0), or if 2 adjacent
c  values in same window found, then abandon this sequence and look
c  for the next
c=	   if(num.gt.0) goto 15		!valid interval
	   if(num.le.0) then		!not valid interval
		if(debon) print 4382,num
4382		format(' sequence abandoned: num = ',i3)
		n1=iabs(num)	!=1,2,3,4
		numtot(n1)=numtot(n1)+1
		goto 429   !abandon this sequence & start new one
	   endif
c=15	   continue
	   j=j+1
	   kseq(j)=num		!next in sequence

c	   if(kseq(j).ne.kseq(j-1)) goto 435	!OK; look for next
	   if(kseq(j).eq.kseq(j-1)) then		!else OK: look for next
		nsame(num)=nsame(num)+1	!record window # in which repeat occurred
c		print 437,int-1,int,ampl(int-1),ampl(int)
c437		format( ' Intervals: ',2i6,' are in same window: amps= ',
c     &      2g13.6)
		goto 429		!look for new sequence
	   endif
c435	   if(j.eq.kval) goto 426		!sequence complete
	   if(j.lt.kval) then		!else sequence complete when j=kval
		int=int+1
		goto 436		!look for next level in seq
	   endif
c	endif		!end of 'if incmult'
c
c   -A sequence now found; find which of the combos it matches
c   and add 1 to ifreq()
c   (Also record values of int0 for each sequence, for use in SCAN to view them)
426	nseq=nseq+1			!count no of sequences found
	if(debug())print 439,nseq,(kseq(i),i=1,kval)
439	format(' Seq #',i6,6x,10i4)
	do i=1,k1		!look through all combos
	   do j=1,kval
		if(kseq(j).ne.kcombs(i,j)) goto 431		!no match yet
	   enddo
	   ifreq(i)=ifreq(i)+1		!obs combo matches combo #i
	   if(ifreq(i).le.5000) then
	      intvals(i,ifreq(i))=index(int0)		!record int0 for SCAN
	   endif
	   goto 433			!jump out and look for next
431	   continue
	enddo		!end of i=1,k1 loop
433	continue
c
c
c When kval=2 we can now record 'for sequences that start in
c level i, x% go to level j'.  Must do this while we still have
c the current sequence in kseq(). For initial value=level i=kseq(1) for
c the current sequence, keep the number of cases in which next level is
c level j =kseq(2) in kfreq(j), j=1,..,nval (=no of levels) so need array
c kfreq(nval,nval) in size. If level 1=shut level the also do separately
c excluding the shut level.
	if(kval.gt.2) goto 429		!look for next sequence now
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
445	format(/,' Number of valid sequences found= ',i8,/,
     & ' No of sequences started but abandoned because:',/,
     & '  (1) event set as  unusable =              ',i8,/,
     & '  (2) event not in any specified range=     ',i8,/,
     & '  (3) opening with dubious amplitude=       ',i8,/,
     & '  (4) event shorter than Tcrit =            ',i8,/,
     & '  (5) consecutive events in same amp range= ',i8)
c
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
     & ' Seq #   Frequency     Sequence')
c
	if(ksym.eq.0) goto 446
	print 447
	if(pon()) write(7,447)
	if(discprt) write(8,447)
447	format(' symmetric:')
	do i=1,ksym
c=	 print 444,i,ifreq(i),(kcombs(i,j),j=1,kval)
	 print 444,i,ifreq(i),
     &  (kcombs(i,j),ymid(kcombs(i,j)),j=1,kval)
	 if(pon()) write(7,444)i,ifreq(i),
     &  (kcombs(i,j),ymid(kcombs(i,j)),j=1,kval)
       if(discprt) write(8,444)i,ifreq(i),
     &  (kcombs(i,j),ymid(kcombs(i,j)),j=1,kval)
444	 format(i6,2x,i6,6x,10(i4,'(',f4.1,'pA)',2x))
	enddo
c
446	if(ksym.eq.k1) goto 451
	print 450
	if(pon()) write(7,450)
	if(discprt) write(8,450)
450	format(' unsymmetric:')
	j1=1
	do i=ksym+1,k1
	 print 444,i,ifreq(i),
     &  (kcombs(i,j),ymid(kcombs(i,j)),j=1,kval)
	 if(pon()) write(7,444)i,ifreq(i),
     &  (kcombs(i,j),ymid(kcombs(i,j)),j=1,kval)
       if(discprt) write(8,444)i,ifreq(i),
     &  (kcombs(i,j),ymid(kcombs(i,j)),j=1,kval)
	 j1=j1+1
	 if(mod(j1,2).ne.0) then
	   print 108
	   if(pon()) write(7,108)
	   if(discprt) write(8,108)
	 endif
	enddo
451	call flush(7)
c Now, when kval=2 print kfreq() values
	if(kval.gt.2) goto 99
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
99	continue
c Now bit done for triplets only
	if(kval.eq.3) then
         print 28
         if(pon()) write(7,28)
         if(discprt) write(8,28)
28	   format(/,
     &  ' Number of valid sequences that have each level in middle',/,
     &  ' Level    number of sequences')
	   do n=1,nval	!go through conductance levels
		nt=0
		do i=1,k1   !go through combos
		   if(kcombs(i,2).eq.n) nt=nt+ifreq(i)
		enddo
	      print 29,n,nt
      	if(pon()) write(7,29) n,nt
	      if(discprt) write(8,29) n,nt
29		format(1x,i5,6x,i8)
	   enddo
	endif
c
999	call flush(7)
	print 171
	if(pon()) write(7,171)
	if(discprt) write(8,171)
171	format(/,/)
c
c Write intvals() etc to EKDIST.TMP for use in SCAN
c Need: ksym,k1,kval ('sequences of kval consec levels'
c KCOMBS() contains K1 sequences, the first KSYM rows being the
c symmetrical ones (if any, ie if kval is odd) and the rest being
c unsymmetrical, each one being followed by its mirror image.
c Test of readback: works here, where ifreq() already defined, but won't read
c back in SCAN.  Need to read back intvals() separately
c k1,ksym,kval,nfile,name,idiscn,ioffset=24+12=36 bytes
c (kfile(i),i=1,nfile) = nfile*4 bytes
c ((kcombs(i,j),ymid(kcombs(i,j)),j=1,kval),i=1,k1)= 3*4*kval*k1 bytes
c (ifreq(i),i=1,k1) = 4*k1 bytes
	ioffset=36 + 4*nfile + 3*4*kval*k1 + 4*k1
	OPEN(unit=19,file='\fortran\EKDIST.TMP',status='UNKNOWN',
     &    access='TRANSPARENT')
	 write(unit=19,rec=1) k1,ksym,kval,nfile,name,idiscn,ioffset,
     & (kfile(i),i=1,nfile),
     & ((kcombs(i,j),ymid(kcombs(i,j)),j=1,kval),i=1,k1),
     & (ifreq(i),i=1,k1)

c	 write(unit=19,rec=ioffset+1) ((intvals(i,j),i=1,k1),
c     &	  j=1,ifreq(i))
	irec=ioffset+1
	do i=1,k1
	   do j=1,ifreq(i)
		write(unit=19,rec=irec) intvals(i,j)
		irec=irec+4
	   enddo
	enddo
	CLOSE(unit=19)
c
c	OPEN(unit=19,file='EKDIST.TMP',status='UNKNOWN',
c     &    access='TRANSPARENT')
c	 read(unit=19,rec=1) k1,ksym,kval,ioffset,
c     & ((kcombs(i,j),ymid(kcombs(i,j)),j=1,kval),i=1,k1),
c     & (ifreq(i),i=1,k1)
c	read(unit=19,rec=ioffset+1) ((intvals(i,j),i=1,k1),j=1,ifreq(i))
c	CLOSE(unit=19)
c
c=======
c Now display amp(i) vs amp(i+1).  If amplitude windows define above, then
c transitions between open levels can be distinguished from others.
97	continue
	call DCASK('Display transitions','y',ans)
	if(ans.eq.'Y') then
	   ans='Y'
	   call DCASK('Exclude transitions within same window',ans,ans)
	   excsam=ans.eq.'Y'
	   ans='Y'
	   call DCASK('Exclude transitions to/from shut state',ans,ans)
	   exc0=ans.eq.'Y'
	   if(.not.exc0) then
	      print 32
32	      format(' SD of noise to add to zero amplitudes [0.05pA] = ')
	      sd=0.05
	      call INPUTR(sd)
c	      call RANDSK(ix,iy,iz,0,repeat)
	   endif
c
	   call RISTIM(trise,-1.,pon())
521	   ans='N'
	   if(nval.gt.0) then
	      conam=.true.
		call DCASK(
     &'Use previously defined limits for open and shut times','Y',ans)
	   endif
	   tres=tresg
	   if(treso.gt.tresg) tres=treso
	   if(nval.eq.0.or.ans.eq.'N') then
	      print 33
33	      format(
     &' Use only amplitudes of events longer than f risetimes:'
     &' f [2] = ')
	      fval=2.
	      call INPUTr(fval)
	      conam=.true.
	      tc1=fval*trise*1.e-3		!msec
	      if(tc1.lt.treso) then
		   call BELL(2)
		   call DCASK('Do you really want no minimum length',
     &		 'Y',ans)
		   if(ans.eq.'N') goto 521
		   fval=0.0
		   tc1=0.0
	         conam=.false.
	      endif
	      do i=1,nval
		   tc(i)=tc1
	      enddo
	   endif
c       First calc number of calc lines so ycal() etc can be allocated
c If ranges defined then draw dotted lines at them (except shut range)
	   nline=0
	   if(nval.gt.0) then
		nl=0
		do i=1,nval
		  y1=abs(ylo(i))
		  y2=abs(yhi(i))
		  if(y1.gt.0.02) then
			nl=nl+1
			yline(nl)=y1
		  endif
		  if(y2.gt.0.02) then
			nl=nl+1
			yline(nl)=y2
		  endif
		enddo
c            now remove duplicate entries
		m=0
		do i=1,nl-1
		   do k=i+1,nl
			if(yline(i).eq.yline(k)) yline(i)=10000.
		   enddo
		enddo
		do i=1,nl
		   if(yline(i).ne.10000.) then
			m=m+1
			yline(m)=yline(i)
		   endif
		enddo
		nline=m
	   endif
	   ndv1=nintt
	   ndimd=1
	   if(nval.gt.0) ndimd=2
	   ncalc=100
	   ndc1=ncalc
	   ndimc=1
	   if(nval.gt.0) ndimc=1 + 2*nline
	   ALLOCATE(xval(ndv1,ndimd),yval(ndv1,ndimd))
	   ALLOCATE(xcal(ndc1,ndimc),ycal(ndc1,ndimc))
	   ALLOCATE(ndat(ndimd),icurvd(ndimd),isym(ndimd),
     &	ijoin(ndimd),syms(ndimd))
	   ALLOCATE(ncal(ndimc),icurvc(ndimc),iline(ndimc))
	   if(allocated(icurvw)) then
		DEALLOCATE(icurvw,weight)
	   endif
	   kwi=1
	   kwj=1
	   ALLOCATE(icurvw(ndimd),weight(kwi,kwj))
	   do i=1,ndimd
		icurvw(i)=-1	!no SD
	   enddo
	   i1=0
	   xmin=1000.		!amplitudes in pA
	   xmax=-1000.
	   nbad1=0        !counts rejections because dubious
	   nbad2=0        !counts rejections because undefined length
	   nbad3=0        !counts rejections because too short
	   nbad4=0        !counts rejections because not in any range
	   if(nval.eq.0) then	!amp windows not defined
c	     do i=1,nint-1
	     i=1
	     do while(i.le.nint-1)
		good=.true.
		skip=.false.
		if(dubious(i).or.dubious(i+1)) then
		   nbad1=nbad1+1
		   good=.false.
		endif
		if(dubious(i+1)) skip=.true.		!skip next time
		if(conam) then
		   if(badint(i).or.badint(i+1)) then
			nbad2=nbad2+1
		      good=.false.
		   endif
		   if(badint(i+1)) skip=.true.		!skip next time
		   t1=tint(i)
		   t2=tint(i+1)
		   if(t1.lt.tc(1).or.t2.lt.tc(1)) then
			nbad3=nbad3+1
		      good=.false.
		   endif
		   if(t2.lt.tc(1)) skip=.true.		!skip next time
		endif
		if(good) then
		   i1=i1+1
		   if(ampl(i).eq.0.) then
			xval(i1,1)=RANORM()*sd
		   else
		      xval(i1,1)=abs(ampl(i))
		   endif
		   if(ampl(i+1).eq.0.) then
			yval(i1,1)=RANORM()*sd
		   else
			yval(i1,1)=abs(ampl(i+1))
		   endif
		   if(xval(i1,1).gt.xmax) xmax=xval(i1,1)
		   if(xval(i1,1).lt.xmin) xmin=xval(i1,1)
		endif
		if(skip) then
		   i=i+2
		else
		   i=i+1
		endif
	     enddo
	     ndat(1)=i1
	     ncurvd=1
	     icurvd(1)=1
	     isym(1)=0		!points
c	     syms(1)=3.0
	     ijoin(1)=-1		!points not joined
	   else		!when amp windows are defined
	     i1=0
	     i2=0
c=	     do i=1,nint-1
	     i=1
	     do while(i.le.nint-1)
		good=.true.
		skip=.false.
		if(dubious(i).or.dubious(i+1)) then
		   nbad1=nbad1+1
		   good=.false.
		endif
		if(dubious(i+1)) skip=.true.		!skip next time
		if(conam) then
		   if(badint(i).or.badint(i+1)) then
			nbad2=nbad2+1
		      good=.false.
		   endif
		   if(badint(i+1)) skip=.true.		!skip next time
		   t1=tint(i)
		   t2=tint(i+1)
		   a1=ampl(i)
		   a2=ampl(i+1)
		   call SEQNUM1(a1,nval,ylo,yhi,ilev1)
		   call SEQNUM1(a2,nval,ylo,yhi,ilev2)

		   if(ilev1.eq.0.or.ilev2.eq.0) then
			nbad4=nbad4+1	!not in any defined range
		      good=.false.
		   else
			if(t1.lt.tc(ilev1).or.t2.lt.tc(ilev2)) then
			   nbad3=nbad3+1	!too short
		         good=.false.
			endif
		   endif
		   if(excsam) then
			if(ilev1.eq.ilev2) good=.false.
		   endif
		   if(exc0.and.(a1.eq.0.or.a2.eq.0)) good=.false.
		   if(ilev2.eq.0) then
			skip=.true.		!skip next time
		   else if(t2.lt.tc(ilev2)) then
			skip=.true.		!skip next time
		   endif
c		   if(excsam) then
c			if(ilev1.eq.ilev2) good=.false.
c		   endif
		endif
		if(good) then
		   if(ampl(i).eq.0.or.ampl(i+1).eq.0) then
			j=1
			i1=i1+1
			i0=i1
		   else
			j=2
			i2=i2+1
			i0=i2
		   endif
		   if(ampl(i).eq.0.) then
			xval(i0,j)=RANORM()*sd
		   else
		      xval(i0,j)=-ampl(i)
		   endif
		   if(ampl(i+1).eq.0.) then
			yval(i0,j)=RANORM()*sd
		   else
			yval(i0,j)=-ampl(i+1)
		   endif
		   if(xval(i0,j).gt.xmax) xmax=xval(i0,j)
		   if(xval(i0,j).lt.xmin) xmin=xval(i0,j)
		endif
		if(skip) then
		   i=i+2
		else
		   i=i+1
		endif
	     enddo
c
           print 49,nbad1,nbad2,nbad3,nbad4
	     if(pon()) write(7,49) nbad1,nbad2,nbad3,nbad4
           if(discprt) write(8,49) nbad1,nbad2,nbad3,nbad4
49   	     format(
     &  ' Number of amplitude pairs discarded because:',/,
     &  '   one or both had dubious amplitude = ',i5,/,
     &  '   one or both were of undefined length = ',i5,/,
     &  '   one or both were shorter than specified resolution = ',i5,/,
     &  '   one or both were not in any defined amp range = ',i5)
	     if(nval.eq.0) then
	        print 54,i1
      	  if(pon()) write(7,54) i1
	        if(discprt) write(8,54) i1
54		  format(' Number of valid pairs = ',i6)
	     else
	        print 55,i1,i2
      	  if(pon()) write(7,55) i1,i2
	        if(discprt) write(8,55) i1,i2
55		  format(
     &	  ' Number of valid pairs to/from shut state = ',i6,/,
     &	  ' Number of valid pairs between open states = ',i6)
	     endif
	     pause
	     ndat(1)=i1
	     ndat(2)=i2
	     ncurvd=2
	     icurvd(1)=1
	     icurvd(2)=2
	     isym(1)=0		!points
c	     syms(1)=3.0
	     isym(2)=-7
	     syms(2)=0.6
	     ijoin(1)=-1		!points not joined
	     ijoin(2)=-1		!points not joined
	   endif
	   call RANDSK(ix,iy,iz,1,repeat)
	   xmin1=xmin
	   xmin=ROUND(xmin1,0)
	   if(xmin.gt.xmin1) xmin=xmin-0.5
	   ymin=xmin
	   xmax1=xmax
	   xmax=ROUND(xmax1,0)
	   if(xmax.lt.xmax1) xmax=xmax+1.0
	   ymax=xmax
	   xtic=1.0
	   ytic=1.0
	   xcross=xmin
	   ycross=ymin
c	   iscal=1		!scale internally
	   iscal=4		!so xmin,...,ymax (only) set here
	   iscal=0		!so xmin,...,ycross (only) set here
c
	   ncurvc=1
	   icurvc(1)=1
	   iline(1)=0	!diagonal line continuous
	   ncal(1)=ncalc
	   dx=(xmax-xmin)/float(ncalc-1)
	   do i=1,ncalc
		xcal(i,1)=float(i-1)*dx
		ycal(i,1)=xcal(i,1)
	   enddo
	   if(nval.gt.0) then		!define lines
		ncurvc=ncurvc + 2*nline
		do j=1,nline		!horizontal lines
		   j1=j+1
		   do i=1,ncalc
			xcal(i,j1)=xcal(i,1)
			ycal(i,j1)=yline(j)	!assumes amps positive on plot
		   enddo
		   icurvc(j1)=j1
		   ncal(j1)=ncalc
		   iline(j1)=11	!dotted
		enddo
		do j=1,nline	!vertical lines
		   j1=j+nline+1
		   do i=1,ncalc
			xcal(i,j1)=yline(j)
			ycal(i,j1)=ycal(i,1)
		   enddo
		   icurvc(j1)=j1
		   ncal(j1)=ncalc
		   iline(j1)=11	!dotted
		enddo
	   endif
c
	   cbig=2.5
	   ifont=4
	   ifitype=0		!no display of fitted parameters
	   idiskq=-1
	   ilog=0 		!arithmetic
	   ilabel=1		!default pos for axis labels
	   titlex='amplitude(i)'
	   titley='amplitude(i+1)'
c	   n=NBLANK2(titlex,40)
c	   n=NBLANK2(titley,40)
c	   itit=1		!so prev title used
c	   title1='TITLE'
c	   n=NBLANK2(title1,44)
	   iask=-2
	   ncjump=0
	   nvjump=0
	   autplt=.false.
	   fitted=.false.
	   draft=.false.
	   interp=.false.
	   ivplot=.false.
	   doframe=.true.
	   landscap=.true.
	   xlo1=-1		!whole screen
	   ntx=5
	   nty=5
	   itx=1		!ticks orientated normally
	   ity=1
	   isetcol=0		!for VPLOT
	   itrace=0
	   call VPLOT5(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,syms,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,draft,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,interp,
     & isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kmax,.false.,iver)
c     & isetcol,itrace,ndv1,ndc1)
	   DEALLOCATE(xval,yval,xcal,ycal)
	   DEALLOCATE(ndat,icurvd,isym,ijoin,syms)
	   DEALLOCATE(ncal,icurvc,iline)
	   if(allocated(icurvw)) then
		DEALLOCATE(icurvw,weight)
	   endif
	endif
c=======
c Try 3D plot of amp(i), amp(i+1) with frequency on vertical axis
c (need smoothing?)
c Use xmin, xmax as set for vplot initially. Need to make amp bins
c With xv=-3,yv=-25, origin is front left, with x axis to right of it
c (near plane of screen with xv=-3.), and y axis going back. F(1,50) is
c at front right, i.e. large x, small y i.e. it is F(y(1),x(50))
c
c NO -set xmin,ymin=0 for now
	ans='Y'
	call DCASK('Show as 3D plot',ans,ans)
	if(ans.eq.'Y') then
         print 82
         if(pon()) write(7,82)
         if(discprt) write(8,82)
82	   format(/,
     &' 3D plot: Default view has x=amp(i) at front, to right of origin'
     &,/,' (= vertical line), and y=amp(i+1) going back into screen.')
	   xmin=0.0		!changed to -0.2 if not exc0 to allow for sd
	   ymin=0.0
	   xv=-3.		!viewpoint
	   yv=-25.
c	   zv=10.		!set below
	   zv=-1000.	!signal that not yet set
	   iax=1
	   izv=1
	   da=0.1		!default bin width
81	   continue		!return here to redraw
	   ans='Y'
	   call DCASK('Exclude transitions within same window',ans,ans)
	   excsam=ans.eq.'Y'
	   ans='Y'
	   call DCASK('Exclude transitions to/from shut state',ans,ans)
	   exc0=ans.eq.'Y'
	   if(.not.exc0) then
	      print 32
c32	      format(' SD of noise to add to zero amplitudes [0.05pA] = ')
	      sd=0.05
	      call INPUTR(sd)
c	      call RANDSK(ix,iy,iz,0,repeat)
	   endif
	   ans='Y'
	   call DCASK(
     &    'Exclude transitions to/from states not in a defined window',
     &     ans,ans)
	   excwin=ans.eq.'Y'
	   if(exc0) then
	      xmin=0.0		!changed to -0.2 if not exc0 to allow for sd
	      ymin=0.0
	   else
	      xmin=-0.2
	      ymin=-0.2
	   endif
	   print 75,xmin,xmax
75	   format(
     &	' Min and max for x=amp(i) (pA) [',g11.4,',',g11.4,'] = ')
	   call INPUT2r(xmin,xmax)
	   print 76,ymin,ymax
76	   format('&Min and max for y=amp(i+1) (pA) [',
     &	g11.4,',',g11.4,'] = ')
	   call INPUT2r(ymin,ymax)
	   xmin1=xmin		!default for axes
	   xmax1=xmax		!default for axes
	   ymin1=ymin		!default for axes
	   ymax1=ymax		!default for axes
c Want x=amp(i) axis at front, and y=amp(i+1) axis going back into screen
	   print 751,xmin1,xmax1
751	   format(
     &   ' Min and max for x=amp(i) axis [',g11.4,',',g11.4,'] = ')
	   call INPUT2r(xmin1,xmax1)
	   print 761,ymin1,ymax1
761	   format(
     &	'&Min and max for y=amp(i+1) axis [',g11.4,',',g11.4,'] = ')
	   call INPUT2r(ymin1,ymax1)
c
	   print 83
83	   format(' Tick length for x,y grid (neg to omit) [1 pA] = ')
	   deltx=1.0		!1 pA
	   call INPUTr(deltx)
	   delty=deltx
c
	   print 34,da
34	   format(' Bin width for amplitude [',f5.2,'pA] = ')
	   call INPUTr(da)
c Make x and y arrays
	   nx=1 + ifixr((xmax-xmin)/da)
	   ny=1 + ifixr((ymax-ymin)/da)
c	   ny=nx
	   nbinx=nx-1		!identical bins on x and y axes normally
	   nbiny=ny-1		!identical bins on x and y axes normally
	   ALLOCATE(freq(ny,nx),xx(ny,nx),yy(ny,nx),ampx(nx),ampy(ny))
	   do i=1,nx
		ampx(i)=xmin + float(i-1)*da
	   enddo
	   do i=1,ny
		ampy(i)=ymin + float(i-1)*da
	   enddo
c Get bin frequencies in freq(i,j)
c First initialise freq
	   do ir=1,ny
		do jr=1,nx
		   freq(ir,jr)=0.0
		enddo
	   enddo
c
	   nbad1=0        !counts rejections because dubious
	   nbad2=0        !counts rejections because undefined length
	   nbad3=0        !counts rejections because too short
	   nbad4=0        !counts rejections because not in any range
	   zmax=0.0		!for max freq
	   do while(i.le.nint-1)
		good=.true.		!when amp(i), amp(i+1) are a valid pair
		skip=.false.
		if(dubious(i).or.dubious(i+1)) then
		   nbad1=nbad1+1
		   good=.false.
		endif
		if(dubious(i+1)) skip=.true.		!skip next time
		if(conam) then
		   if(badint(i).or.badint(i+1)) then
			nbad2=nbad2+1
		      good=.false.
		   endif
		   if(badint(i+1)) skip=.true.		!skip next time
		   t1=tint(i)
		   t2=tint(i+1)
		   a1=ampl(i)
		   a2=ampl(i+1)
		   call SEQNUM1(a1,nval,ylo,yhi,ilev1)
		   call SEQNUM1(a2,nval,ylo,yhi,ilev2)
		   if(excsam) then
			if(ilev1.eq.ilev2) good=.false.
		   endif
		   if(exc0.and.(a1.eq.0.or.a2.eq.0)) good=.false.
		   if(excwin) then
			if(ilev1.eq.0.or.ilev2.eq.0) good=.false.
		   endif
		   if(t1.lt.tc(1).or.t2.lt.tc(1)) then
			nbad3=nbad3+1
		      good=.false.
		   endif
		   if(t2.lt.tc(1)) skip=.true.		!skip next time
		endif
		if(good) then	!find which bin to increment
c		   a1=abs(ampl(i))
c		   a2=abs(ampl(i+1))
		   if(ampl(i).eq.0.) then
			a1=RANORM()*sd
		   else
		      a1=abs(ampl(i))
		   endif
		   if(ampl(i+1).eq.0.) then
			a2=RANORM()*sd
		   else
		      a2=abs(ampl(i+1))
		   endif
c With xv=-3,yv=-25, origin is front left, with x axis =amp(i) to right of it
c (near plane of screen with xv=-3.), and y=amp(i=1) axis going back.
c F(1,50) is at front right, i.e. large x, small y i.e. it is F(y(1),x(50))
		   do ir=1,nbiny
			if(a2.ge.ampy(ir).and.a2.lt.ampy(ir+1)) then	!i bin found
			   do jr=1,nbinx
				if(a1.ge.ampx(jr).and.a1.lt.ampx(jr+1)) then	!j bin found
				   freq(ir,jr)=freq(ir,jr) + 1.0
				   if(freq(ir,jr).gt.zmax) zmax=freq(ir,jr)
				endif
			   enddo
			endif
		   enddo
		endif
		if(skip) then
		   i=i+2
		else
		   i=i+1
		endif
	   enddo
c
	   zmin=0.0
	   z=ifixr(zmax)
	   if(z.lt.zmax) then
		zmax=z + 1.0
	   else
		zmax=z
	   endif
	   xz=xmin		!x,y position for vertical axis
	   yz=ymin
	   zval=zmin      !vertical pos for x,y grid
	   print 87,zmin,zmax,izv
87	   format(' Vertical position of x, y grid:',/,
     &   ' (1) zmin (',f6.1,')',/,
     &   ' (2) z = 0.0',/,
     &   ' (3) zmax (',f6.1,')',/,
     &   ' (4) specify value',/,
     &   ' Option number [',i2,'] = ')
	   call INPUTi(izv)
	   if(izv.eq.1) then
		zval=zmin
	   else if(izv.eq.2) then
		zval=0.0
	   else if(izv.eq.3) then
		zval=zmax
	   else if(izv.eq.4) then
		print 871
871		format(' Vertical position of x, y grid is at z = ')
		call INPUTr(zval)
	   endif
	   if(zval.lt.zmin) zmin=zval
	   if(zval.gt.zmax) zmax=zval
	   if(zv.lt.-999.) zv=zmax	!otherwise as reset below
c Plot the 3D graph
	   idev=0		!screen
c=	   idev=4		!plotter
	   if(videotyp().ne.18) call INIPLT(idev,.false.,1.0)
89	   CALL SETORIGIN(0.0, 0.0, 0.0)
         CALL SETAPERTURE(0.0, 0.0, 0.0)
         CALL SETVIEWPT(xv, yv, zv)
         CALL SCALE3D(xmin1, xmax1, ymin1, ymax1, zmin, zmax)
         CALL PROJECT(freq, xx, yy, ampx,ampy, nx, ny)
	   ihid=3
	   call LINWID(20)
	   call COLTYP1(11)
         CALL PLOT3D(xx, yy, nx, ny, ihid)
c Draw axes on graph
c	   deltx=xmax-xmin
c	   delty=ymax-ymin
	   call COLTYP1(12)
	   call LINWID(15)
	   call AXES3D(xmin1,xmax1,deltx,ymin1,ymax1,delty,zval,
     &    zmin,zmax,xz,yz)
c Rescale/rotate this graph?
c	   pause
	   if(idev.eq.0) then
		print 80
80		format(' Rescale/rotate this graph [Y] ? ')
		read 101,ans
		call ERASCR()
	 	call VIDEOMOD(3)
c==	      call ENDPLT()
	   else if(idev.eq.4) then
	      call ENDPLT()
c To eject page without picking up another, send 'Esc E' to laserjet (OK for
c deskjet too?).  If this is NOT done then another plot can be put on same page.
		write(7,*) char(27)//'E'
      	call FLUSH(7)
	      DEALLOCATE(freq,xx,yy,ampx,ampy)
		idev=0
		goto 994
	   endif
	   if(UC(ans).eq.'N') then
		print 88
88		format(' Plot the graph now [N] ? ')
		read 101,ans
		if(UC(ans).eq.'Y') then
		   idev=4
	   	   call INIPLT(idev,.false.,1.0)
		   goto 89
		else
		   goto 994			!where next?
		endif
	   endif
	   DEALLOCATE(freq,xx,yy,ampx,ampy)
	   print 78,xv,yv
78	   format(' Viewpoint (xv, yv) [',f7.1,',',f7.1,'] = ')
	   call INPUT2r(xv,yv)
	   print 79,zv
79	   format(' Height of viewpoint (zv) [',f7.1,'] = ')
	   call INPUTr(zv)
	   goto 81
	endif
c
c Deallocate arrays before leaving
994	continue
	if(allocated(kcombs)) then
	   DEALLOCATE(kcombs,ifreq,intvals)	!no longer needed
	endif
	pause		!before display
	RETURN	!LISTS ONLY; NO DISTRIBUTION
C
	END



	subroutine SEQNUM(num,ylo,yhi,nval,time,amp,excdub,tc,
     & dubamp,unusable)

c	subroutine SEQNUM(int,num,ylo,yhi,nval,tint,iampl,acal,excdub,tc)
c Returns NUM=sublevel number for amplitude =AMP
c (called with amp = ampl(int), int=1,2,...,nval)
c Returns NUM<0 as follows (for  debug easier if reason for abandoning
c sequence is known)
c    (1) if interval is unusable,NUM=-1
c or (2) interval is in none of the categories,NUM=-2
c or (3) if exdub=true, amplitude is set to be dubious, NUM=-3
c or (4) if interval is less than crit length TC (unless TC is
c	 negative), NUM=-4
	dimension ylo(10),yhi(10),tc(10)
	logical excdub,dubamp,unusable
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
	if(unusable) goto 98	!cannot test length
	if(excdub.and.dubamp) num=-3
c if dubious amp AND short num=-3 (=dubious)
	do 1 i=1,nval
c	   print 2,i,amp,ylo(i),yhi(i)
c2	   format(' IN SEQNUM: i,amp,ylo,yhi= ',i4,3g13.6)
	   i1=i
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

	subroutine SEQNUM2(num,ylo,yhi,nval,amp,excdub,
     & dubamp,unusable)

c SEQNUM2 is version of SEQNUM in which duration is not tested
c (so time,tc removed) (used for incmult=true case)
c only if incmult=false.
c Returns NUM=sublevel number for amplitude =AMP
c (called with amp = ampl(int), int=1,2,...,nval)
c Returns NUM<0 as follows (for  debug easier if reason for abandoning
c sequence is known)
c    (1) if interval is unusable,NUM=-1
c or (2) interval is in none of the categories,NUM=-2
c or (3) if exdub=true, amplitude is set to be dubious, NUM=-3
	dimension ylo(10),yhi(10)
	logical excdub,dubamp,unusable
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
c
	num=0
	if(unusable) goto 98	!cannot test length
	if(excdub.and.dubamp) num=-3
c if dubious amp AND short num=-3 (=dubious)
	do i=1,nval
c	   print 2,i,amp,ylo(i),yhi(i)
c2	   format(' IN SEQNUM: i,amp,ylo,yhi= ',i4,3g13.6)
	   i1=i
	   if(amp.gt.ylo(i).and.amp.lt.yhi(i)) goto 99	!valid value
	enddo
	if(num.ne.-4) num=-2		!not in any range
	RETURN
98	num=-1
	RETURN
99	if(num.eq.0) num=i1	!if already set negative then leave it
	RETURN
	end



	subroutine SEQNUM1(amp,nval,ylo,yhi,ilev)
c Simplified version of SEQNUM that just returns range (#ival) in which amp
c occurs, or ival=0 if amp is not in any of the defined ranges.
	dimension ylo(10),yhi(10)
c
	ilev=0
	do i=1,nval
	   i1=i
	   if(amp.gt.ylo(i).and.amp.lt.yhi(i)) goto 99	!in a valid range
	enddo
	RETURN	!with ilev=0 if not in any range
99	ilev=i1	!if already set negative then leave it
	RETURN
	end

	subroutine NCOMB(nval,kval,kcombs,k1,ksym)
C
	dimension k(10)
	dimension kcomb(100,10),kcombs(100,10)
	character*1 ans,UC
C To enumerate of sublevel order possibilities (see TCOMB.FOR)
c N=no of states
c k=length of sequence
c KCOMBS() contains K1 sequences, the first KSYM rows being the
c symmetrical ones (if any, ie if kval is odd) and the rest being
c unsymmetrical, each one being followed by its mirror image.
c
	print 105,nval*(nval-1)**(kval-1)
105	format(/,' No of combinations expected= ',i8, ' O.K. [Y] ? ')
	read 112,ans
112	format(a1)
	if(UC(ans).eq.'N') then
	   k1=-1
	   RETURN
	endif
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
	dimension kcomb(100,10)
	do 1 i=1,kval
1	kcomb(k1,i)=k(i)
	return
	end

	subroutine SYM(ir,ival,kcomb,kcombs,ks1,kval)
	dimension kcomb(100,10),kcombs(100,10)
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



