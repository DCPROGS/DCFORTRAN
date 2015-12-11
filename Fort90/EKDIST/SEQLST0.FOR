	subroutine SEQLST(tint,ampl,iprops,nint,nintt,avamp,acrit,
     & trise,iscan,ylo,yhi,ymid,kval,treso,tresg,tc,index,
     & kfile,nfile,name,idiscn,nvalsav,idiskq,
     & iexstrt,iexend,onetcrit,tcvals,ibamp)
c
c Version for new EKDIST/SCAN 02/16/94 04:27pm
c
c 06/06/89 07:02pm Lahey version
c To list sublevel sequences in EKDIST
c
c Modif 03/22/99 06:12am Remove '(2) List all groups of openings with sublevels'
c   Sublevels were defined as llevels that differ  from avamp by more than
c   acrit, but avamp is unreliable and acrit=0. by default (options for
c   sublevels defined in this way also removed from cdist2 now).
c
c   Add option to (a) define levels via amp ranges and (b) define bursts,
c   and list/plot which levels occur in which bursts (code for identifying
c   bursts is simplified form cdist2, but here also keep index of 1st and last
c   opening in the burst in ibstart(), ibend()
c
c Modif Aug 98 to correct interchange of x, y axes in 3D plot of
c  amp(i)/amp(i+1) (see lines markes c/)
c Modif 05/15/96 05:45pm -definition of intvals() changed to allow all
c transitions in a sequence to be marked after return to SCAN
c With sequences of 4, and 3 levels, get k1=24 combos, and with up to
c 1000 sequences in each combo need j=24000 to locate a single
c transition -need 4 (3?) times this to keep position of all transitions!
c=====alternatively keep totlen =total length of sequence for each sequence
c (as integer microsec in intvals?) and mark first and last transitions in SCAN
c=========
c Modif 05/14/96 04:03pm to add ekpath, ipath to parameters -asks where
c    to put EKDIST.TMP
c
c Modified 07/14/94 08:49am to record 1st interval # in original data
c (as in SCANDAT, before resolution imposed -using index()) of each
c sequence.  Kept in intvals(i,j,1) for jth value for the ith
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
	allocatable::badval
	logical badval(:,:),posneg
	LOGICAL fitted,doframe,draft,autplt,interp,landscap,ivplot
	logical allocated
	character*40 titlex,titley
	character*64 title1
	character*33 ekpath	!path for ekdist.tmp
c
	integer*4 index(nintt)
c=	logical plotrue
	integer kfile(50)
	real*4 TINT(nintt),ampl(nintt)
	integer*1 iprops(nintt)
	allocatable::kcombs,ifreq,intvals
	integer*4 kcombs(:,:),ifreq(:),intvals(:,:,:)
	integer*4 kseq(10),kfreq(10,10)
	integer*4 itot(10),numtot(4),nsame(10)
	real*4 ylo(10),yhi(10),ymid(10),tc(10),yline(20)
	logical pon,debug,debon,slock,caplock
	logical excdub,samwin,slev
c	logical full,sub
	logical dubious,badint,btest,repeat,conam,good,skip,incmult
	character*1 ans,UC
	character name*12
	logical discprt
c For 3D display of dependency plot
	allocatable::freq,znew,ampx,ampy
	real*4 freq(:,:),znew(:,:),ampx(:),ampy(:)
	logical excsam,exc0,excwin
	character*75 xtitle,ytitle,ztitle	!output from LAXES
	integer icol(100)		!for DCMENU
	character*75 title2
	character*40 qfile
	common/queue/qfile	!for vplot and vhist
c For division into bursts
	real*4 tcvals(50)
	integer iexstrt(50),iexend(50)
	logical onetcrit,pflag,newfile,findgap,badend,first,conam1
	logical pbst,ppopen,bprint,bad,deb,open,shut,badgap
	logical dubamp,exass
	allocatable::ibstart,ibend,bampl 	!record 1st and last op, & amp for each bst
	integer ibstart(:),ibend(:)
	real*4 bampl(:)
	dimension ops(5001),gaps(5000)	!temp store for 1 burst
c	dimension isubs(5000)
c
	logical mono
	COMMON/cols/icol,mono
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
	badgap(i)=BTEST(iprops(i),3)	!tint(i) was unusable (bit 3='8' set)
c
	nval=0		!until defined
107	continue
	print 2323
2323	FORMAT(
     &' (1) List frequencies of direct transitions between amp levels.')
c	print 2322
c2322	FORMAT(' (2) List all groups of openings with sublevels')
	print 2322
2322	format(
     &' (2) List amplitude levels that occur in each burst')
	id=1
	print 106
106	format(' Option number [1] = ')
	call INPUTi(id)
c	read 21,id
c	if(id.le.0) id=1
c21	format(i8)
	if(id.ne.1.and.id.ne.2) goto 107
	if(id.eq.2.and.pon()) write(7,2322)
	if(id.eq.2.and.discprt) write(8,2322)
	if(id.eq.1.and.pon()) write(7,2323)
	if(id.eq.1.and.discprt) write(8,2323)
	print 1080
	if(discprt) write(8,1080)
1080	format(
     & ' -------------------------------------------------------------')
c Need to define amplitude ranges for both id=1 and 2
c==	if(id.eq.1) goto 430
C
c==replace this, or modify so it uses ranges?

cC (1) id=2  -SECTION TO LIST ALL GROUPS OF OPENINGS THAT CONTAIN
cC   A SUBLEVEL I.E.EITHER ISOLATED SUBLEVELS OR GROUPS OF CONTIGUOUS
cC   OPENINGS (WHICH CAN OCCUR ONLY IF THERE ARE ONE OR MORE SUBLEVELS
cC   IN THE GROUP. CANNOT CHOOSE THIS OPTION IF SUBLEV IS FALSE.
c	n3=0	!no of groups with contigous open levels
c	print 23
c23	format(' Exclude isolated sublevels [N] ? ')
c	ans='N'
c	call INPUTa(ans)
c	if(UC(ans).eq.'Y'.and.pon()) write(7,24)
c	if(UC(ans).eq.'Y'.and.discprt) write(8,24)
c24	format( ' Isolated sublevels excluded')
c2361	I=1
c	J=0	!COUNTS PERIODS IN A FOR THIS EXPT
cC NEXT LOOK FOR START OF A GROUP
c240	n=0	!number in the group
c	full=abs(ampl(i)-avamp).le.acrit
c	sub=(ampl(i).ne.0).and.(.not.full)
cC   IF THERE ARE TWO ADJACENT 'FULL' AMPLITUDES THIS SHOULD NOT COUNT
cC   AS A SUBLEVEL GROUP- SHOULD HAPPEN ONLY IF ONE OF THEM IS SET
cC   UNUSABLE.
cC   DOES NOT RETURN HERE IF I=NINT SO NEXT LINES OK
c	if(full.and.(abs(ampl(i+1)-avamp).le.acrit)) goto 260
c	goto 261
cc=260	if(TINT(I).GT.0.0.AND.TINT(I+1).GT.0.0) goto 27
c260	if((.not.badint(i)).and.(.not.badint(i+1))) goto 27
c	goto 263
c27	call BELL(2)
c	print 262,I,I+1
c262	format(' ***ERROR. TWO ADJACENT USABLE FULL OPENINGS= ',2I5)
c	goto 263
c261	if(sub.or.(full.and.ampl(i+1).ne.0)) goto 241
cc goto 241 when start of group found
c263	i=i+1
c	if(i.le.nint) goto 240	!START NOT YET FOUND-TRY NEXT INTERVAL
c	goto 991
c241	k=i+1
c242	continue
c	n=n+1
c	jval(n)=k-1	!store for print at end of group
c	tval(n)=tint(k-1)
cc==	aval(n)=acal*float(iacal(k-1,iampl))
c	aval(n)=ampl(k-1)
cc242	print 245,K-1,TINT(K-1),aval(n)
cc	if(pon()) write(7,245)K-1,TINT(K-1),aval(n)
cc      if(discprt) write(8,245)K-1,TINT(K-1),aval(n)
c245	format(I5,2X,2G13.6)
c	if(ampl(k).eq.0) goto 243
cc goto 243 when NEXT SHUT PERIOD=END OF GROUP FOUND
c	k=k+1
c	if(k.le.nint) goto 242	!KEEP LOOKING FOR END
c	goto 991	!DATA FINISHED BEFORE END FOUND, SO DO NOT COUNT
cc Another group completed
c243	if(UC(ans).eq.'Y'.and.n.eq.1) goto 25
c	n3=n3+1
c	do 26 i1=1,n
c	print 245,jval(i1),tval(i1),aval(i1)
c	if(pon()) write(7,245)jval(i1),tval(i1),aval(i1)
c26    if(discprt) write(8,245)jval(i1),tval(i1),aval(i1)
c	print 108	!NEW LINE BETWEEN GROUPS
c	if(pon()) print 108
108	format(/)
c25	j=j+1	!ANOTHER GROUP COMPLETED
c	i=k+1
c	if(i.lt.nint) goto 240	!LOOK FOR START OF NEXT GROUP
cC N.B. DOES NOT RETURN IF I=NINT AS IF TINT(NINT) IS START OF A
cC GROUP THERE IS NO WAY TO TELL IF THIS GROUP HAS BEEN COMPLETED-
cC THE NEXT INTERVAL MIGHT HAVE BEEN OPEN TOO
c991	nyval=j
cc count total no of sublevels in data separately
c	n1=0
c	n2=0
c	do 264 i=1,nint
c	i1=i
c	amp=ampl(i1)
c	if(amp.eq.0) goto 264	!shut
c	full=abs(amp-avamp).le.acrit
c	n1=n1+1			!open
c	if(.not.full) n2=n2+1	!i.e. if SUB
c264	continue
c	print 265,n1,n2,nyval,n3
c	if(pon()) write(7,265)n1,n2,nyval,n3
c      if(discprt) write(8,265)n1,n2,nyval,n3
c265	format(
c     & ' Total no of openings (inc sublevels)    = ',i8,/,
c     & ' Total no of sublevels		          = ',i8,/,
c     & ' Total no of open periods with a sublevel= ',i8,/,
c     & ' Total no of open periods with contiguous openings= ',i8,/)
c	call flush(7)
c	pause		!before display
c	RETURN	!LISTS ONLY; NO DISTRIBUTION
c
c Now section to list all sublevel sequences (id=1)
430	continue
c
c List frequencies
c	call DCASK('List transition frequencies','y',ans)
c	if(ans.eq.'N') goto 97
	print 421
421	format(' List sequences of k consecutive levels: k [2] = ')
	kval=2
	call INPUTi(kval)
	print 72
72	format(
     & '&Exclude sequences with any dubious amplitudes [Y] ? ')
	ans='Y'
	call INPUTa(ans)
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
	   ans='Y'
	   call INPUTa(ans)
	   if(UC(ans).ne.'N') then
		samwin=.true.
		nval=nvalsav
	   endif
	endif
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
c Calculate # of combos, and allocate intvals()
	k1=nval*(nval-1)**(kval-1)
c Allocate arrays
	k2=10000	!2nd dimension of intvals (now kept in ekdist.tmp)
c=======temp debug
	kdim=k1
	if(.not.allocated(kcombs)) then
c	   ALLOCATE(kcombs(100,10),ifreq(100),intvals(k1,k2,2))
c===	   ALLOCATE(kcombs(k1,10),ifreq(k1),intvals(k1,k2,2))
	   ALLOCATE(kcombs(kdim,10),ifreq(kdim),intvals(kdim,k2,2))
	endif
c
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
	call NCOMB(nval,kval,kcombs,k1,ksym,kdim)
	if(k1.lt.0) goto 430
c
	if(samwin) goto 615
c Section to define windows
	print 611
611	format(' Are openings downwards [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	sign=-1.
	if(UC(ans).eq.'N') sign=1.
	print 61
61	format('&Calculate critical amplitudes for each window [Y] ? ')
	ans='Y'
	call INPUTa(ans)
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
	if(ncompa.le.0) then
	   ncomp=0		!amplitude pdf not yet defined  (see common/ampsav/)
	else
	   ncomp=ncompa
	endif
c
4	print 641
641	format(' Is the smallest amplitude the shut level [Y] ? ')
	ans='Y'
	call INPUTa(ans)
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
		ans='Y'
		call INPUTa(ans)
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
	   ans='Y'
	   call INPUTa(ans)
	   if(UC(ans).eq.'N') then
		print 5,1
		call INPUT2r(ylo(1),yhi(1))
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
		ans='Y'
		call INPUTa(ans)
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
		ans='Y'
		call INPUTa(ans)
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
	   ans='Y'
	   call INPUTa(ans)
	   if(UC(ans).eq.'N') then
      	print 5,i-1
5    		format('&   window #',i3,' between a1, a2= ')
		call INPUT2r(ylo(i-1),yhi(i-1))
		goto 93		!check values
	   endif
65	continue
c	if(pon()) write(7,453)i-1,sign*ylo(i-1),sign*yhi(i-1)
c      if(discprt) write(8,453)i-1,sign*ylo(i-1),sign*yhi(i-1)
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
	ans='Y'
	call INPUTa(ans)
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
	do i=1,nval
	 if(ylo(i)*yhi(i).lt.0.0) then
	   slev=.true.
	   ishut=i
	   goto 441
	 endif
	enddo
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
	    print 50,fval
50	    format(
     & '  -exclude open times shorter than f risetimes: f [',
     &	f5.2,'] = ')
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
	do i=1,nval
	   print 453,i,ylo(i),yhi(i),tc(i)*1.e3
	   if(pon()) write(7,453)i,ylo(i),yhi(i),tc(i)*1.e3
         if(discprt) write(8,453)i,ylo(i),yhi(i),tc(i)*1.e3
	enddo
453	format(i4,3x,g13.6,' to ',g13.6,' pA: resolution (mus) = ',g13.6)
c
c AMP RANGES AND RESOLUTION NOW SET
c
	if(id.eq.2) goto 120
c
C NOW id=1 -LIST DIRECT TRANSITIONS
c Look for first interval in a sequence- ie the first (usable) interval
c that falls into ANY of the specified categories.
c INT0 is index for the first opening in a sequence; INT is index
c within a sequence
c If incmult=true then multiple transitions within a window are counted
c as a single sojourn in that window for purposes of defining a sequence
c e.g. 1-2-3 sequence would be counted if there were several transitions
c between open states that were (a) all within window #2, and (b) had TOTAL
c length > tc (and usable etc)
c NB should make no difference for kval=2 (or for 1st and last of
c sequence in general) EXCEPT that total length may be > tc if there
c are several transitions with 1st/last window, when length would
c be <tc if only adjacent intervals used
	iopt=2
300	continue	!to repeat with different option
	print 58,iopt
58	format(/,
     & ' (1) Allow single sojourn in each window only',/,
     & ' (2) Allow multiple transitions within each window',/,
     & ' (3) Help',/,
     & ' (4) No more sequence listing',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(iopt)
	if(iopt.eq.3) then
	   print 39
39	   format(
     &'  Option 1 requires that, for example, a valid 1-2-3 sequence',/,
     &' consists of one sojourn in window 1, followed by one sojourn',/,
     &' in window 2, and one in window 3 (each being longer then the',/,
     &' specified resolution).',/,
     &'  Option 2 counts as a valid 1-2-3 sequence a series of',/,
     &' events where, for example, the time spent in window 2',/,
     &' may consist of any number of contiguous sojourns at',/,
     &' current levels all of which are within window 2, and the',/,
     &' total length of which is greater than the specified',/,
     &' resolution.',/)
	   goto 300
	else if(iopt.eq.4) then
	   goto 999
	else if(iopt.eq.1) then
	   incmult=.false.
	   print 56
	   if(pon()) write(7,56)
	   if(discprt) write(8,56)
56	   format(/,
     & ' Sequences with single sojourn in each window only',/,
     & '====================================================')
	else if(iopt.eq.2) then
	   incmult=.true.
	   print 57
	   if(pon()) write(7,57)
	   if(discprt) write(8,57)
57	   format(/,
     & ' Sequences with multiple transitions within each window',/,
     & '====================================================')
	else
	   goto 300
	endif
	nseq=0
	int0=0		!start at tint(1)
	do i=1,4
	   numtot(i)=0	!initialise totals
	enddo
	do i=1,10
	   nsame(i)=0	!initialise totals for 'in same window'
	enddo
	do i=1,nval
	   do j=1,nval
		kfreq(i,j)=0	!initialise kfreq
	   enddo
	enddo
	do i=1,k1
	   ifreq(i)=0
	enddo
c
c When looking for next sequence, can just increment int0 when NOT incmult
c so 2nd interval of last seq is first interval of next.  But in incmult
c case this would result in same sequence being recorded again if there two
c intervals in the initial window, so in this case int0 is  set to last
c interval in first window of sequence in the loop below, then incremented
c by 1 below so points to first interval in 2nd window (level) of prev seq.
429	int0=int0+1
	j=0		!number in sequence
	debon=debug()
c
	if(incmult) then
325	   continue
	   ttot=0.0		!total length of time in present level
	   call SEQNUM2(num,ylo,yhi,nval,ampl(int0),
     &    excdub,dubious(int0),badint(int0))
	   if(num.le.0) then		!1st in sequence not yet found
		int0=int0+1				!for num<0
		if(int0.ge.nint) goto 428		!all obs done
		if(debon) then
		   print 437,int0,num
		   if(discprt) write(8,437) int0,num
437		   format(' start value abandoned: int0, num = ',i6,i4)
		endif
		goto 325		!start not yet found
	   endif
c	Is next interval in the same window?
	   ttot=ttot+tint(int0)
	   num0=num
	   int=int0
326	   int=int+1
	   if(int.ge.nint) goto 428
	   call SEQNUM2(num,ylo,yhi,nval,ampl(int),
     &    excdub,dubious(int),badint(int))
c     Since kval>1 there must always be another level to follow the first
c 	one, so dubious/bad/undefined range must all abort the sequence
	   if(num.le.0) then
		int0=int0+1				!for num<0
		if(int0.ge.nint) goto 428		!all obs done
		if(debon) then
		   print 401,int,num
		   if(discprt) write(8,401) int,num
401		   format(' start value abandoned: int, num = ',i6,i4)
		endif
		n1=iabs(num)	!=1,2,3,4
		numtot(n1)=numtot(n1)+1
		goto 325		!start not yet found
	   else if(num.eq.num0) then		!still same window
		ttot=ttot+tint(int)
		goto 326				!look for more
	   else			!different (valid) window
		if(ttot.lt.tc(num0)) then	!too short -start not yet found
		   int0=int0+1				!as for num<0
		   if(debon) then
			print 37,ttot,int0
		      if(discprt) write(8,37) ttot,int0
37		   	format(
     &	' sequence abandoned: ttot = ',g13.6,' new int0 = ',i6)
		   endif
		   n1=4	!too short
		   numtot(n1)=numtot(n1)+1
		   goto 325
		else
		   int=int-1		!last interval in initial window
		endif
	   endif
c
	   if(debon) then
		do i=int0,int
		   print 36,i,ampl(i),tint(i),num0
		   if(discprt) write(8,36) i,ampl(i),tint(i),num0
36	   	   format(
     & ' 1st in seq: i= ',i6,' amp= ',g13.6,'  t= ',g13.6,' Lev #= ',i4)
		enddo
	   endif
c
	   int0=int		!save for place to start next sequence
	   j=j+1
	   kseq(j)=num0		!define 1st in sequence (j=1)
c
c Now look for rest of sequence
	   int=int+1			!next interval
	   totlen=0.0
336	   continue			!return to look for next level in sequence
	   if(int.ge.nint) goto 428
	   ttot=0.0		!total length of time in present level
	   call SEQNUM2(num,ylo,yhi,nval,ampl(int),
     &    excdub,dubious(int),badint(int))
	   if(debon) then
		print 4381,int,ampl(int),tint(int),num
		if(discprt) write(8,4381) int,ampl(int),tint(int),num
4381	      format(
     & ' next i= ',i6,' amp= ',g13.6,'  t= ',g13.6,' Level #= ',i4)
	   endif
c  If an unspecified amplitude is found (num<0), then abandon this sequence
c  and look for the next
	   if(num.le.0) then		!not valid interval
		if(debon) then
		   print 4382,num
		   if(discprt) write(8,4382) num
4382		   format(' sequence abandoned: num = ',i3)
		endif
		n1=iabs(num)	!=1,2,3,4
		numtot(n1)=numtot(n1)+1
		goto 429   !abandon this sequence & start new one
	   endif
c	Is next interval in the same window?
	   ttot=ttot+tint(int)
	   num0=num
327	   int=int+1
	   if(int.ge.nint) goto 428
	   call SEQNUM2(num,ylo,yhi,nval,ampl(int),
     &    excdub,dubious(int),badint(int))
c     If this is the last level of the sequence (so j=kval-1 at this point)
c	then the only point in looking for more intervals is in case they
c	make ttot>tcrit; a bad/dubious/undefined interval just ends the sequence
c	but does not abort the whole sequence, so abort only if j<kval-1
c	in the next statements.  If kval=j-1 then the 'else' bit is done.
	   if(num.le.0.and.j.lt.kval-1) then
		int=int+1				!for num<0
		if(int.ge.nint) goto 428		!all obs done
		if(debon) then
		   print 4382,num
		   if(discprt) write(8,4382) num
c4382		   format(' sequence abandoned: num = ',i3)
		endif
		n1=iabs(num)	!=1,2,3,4
		numtot(n1)=numtot(n1)+1
		goto 429   !abandon this sequence & start new one
	   else if(num.eq.num0) then		!still same window
		ttot=ttot+tint(int)
		goto 327				!look for more in same window
	   else			!different (valid) window
		if(ttot.lt.tc(num0)) then	!too short -abandon
		   if(debon) then
			print 38,ttot,int
		      if(discprt) write(8,38) ttot,int
38		      format(
     &	' sequence abandoned: ttot = ',g13.6,' int = ',i6)
		   endif
		   n1=4	!too short
		   numtot(n1)=numtot(n1)+1
		   goto 429   !abandon this sequence & start new one
		else
		   int=int-1		!last interval in window
		   totlen=totlen+ttot	!total length of sequence
		endif
	   endif
c
	   if(j.eq.1) then
		int1=int
	   endif
c
	   j=j+1
	   kseq(j)=num0		!next in sequence
c
	   if(j.eq.kval-1) then
		itotlen=ifixr(1.e3*totlen)	!in integer microsec
	   endif
c
	   if(j.lt.kval) then		!else sequence complete when j=kval
		int=int+1
		goto 336		!look for next level in seq
	   endif
c
	else	!not incmult: single sojourn allowed in each window (as original)
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
	   totlen=0.0
436	   continue			!return to look for next level in sequence
	   if(int.ge.nint) goto 428
	   call SEQNUM(num,ylo,yhi,nval,tint(int),ampl(int),
     &	 excdub,tc,dubious(int),badint(int))
	   if(debon)print 4381,int,ampl(int),tint(int),num
c4381	   format(
c     & ' next i= ',i6,' amp= ',g13.6,'  t= ',g13.6,' Level #= ',i4)
c  If an unspecified amplitude is found (num<0), or if 2 adjacent
c  values in same window found, then abandon this sequence and look
c  for the next
	   if(num.le.0) then		!not valid interval
		if(debon) print 4382,num
c4382		format(' sequence abandoned: num = ',i3)
		n1=iabs(num)	!=1,2,3,4
		numtot(n1)=numtot(n1)+1
		goto 429   !abandon this sequence & start new one
	   endif
	   totlen=totlen+tint(int)	!total length of sequence
	   if(j.eq.1) then
		int1=int
	   endif
c
	   j=j+1
	   kseq(j)=num		!next in sequence

	   if(kseq(j).eq.kseq(j-1)) then		!else OK: look for next
		nsame(num)=nsame(num)+1	!record window # in which repeat occurred
c		print 437,int-1,int,ampl(int-1),ampl(int)
c437		format( ' Intervals: ',2i6,' are in same window: amps= ',
c     &      2g13.6)
		goto 429		!look for new sequence
	   endif
c
	   if(j.eq.kval-1) then
		itotlen=ifixr(1.e3*totlen)	!in integer microsec
	   endif
c
	   if(j.lt.kval) then		!else sequence complete when j=kval
		int=int+1
		goto 436		!look for next level in seq
	   endif
	endif		!end of 'if incmult'
c
c   -A sequence now found; find which of the combos it matches
c   and add 1 to ifreq()
c   (Also record values of int0 for each sequence, for use in SCAN to view them)
426	nseq=nseq+1			!count no of sequences found
	if(debon)then
	   print 439,nseq,(kseq(i),i=1,kval)
	   if(discprt) write(8,439) nseq,(kseq(i),i=1,kval)
439	   format(' Seq #',i6,6x,10i4)
	endif
	do i=1,k1		!look through all combos
	   do j=1,kval
		if(kseq(j).ne.kcombs(i,j)) goto 431		!no match yet
	   enddo
	   ifreq(i)=ifreq(i)+1		!obs combo matches combo #i
c	   i00=i				!for debug
	   if(ifreq(i).le.k2) then
	      intvals(i,ifreq(i),1)=index(int1)	!record int for SCAN
	      intvals(i,ifreq(i),2)=itotlen		!record totlen for SCAN
	   else
		call BELL(2)
		print 701,k2
701		format(' ifreq > ',i5)
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
	goto 998
168	if(lev1.eq.2) goto 998		!already done
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
998	continue

	if(iopt.eq.1) then
	   iopt=2
	else if(iopt.eq.2) then
	   iopt=4
	endif
	goto 300	!try another method

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
c No real need for next bit -just write EKDIST.TMP to root segment
c of C:.  This should be present on any machine so there should be
c no problem in finding it in SCAN.
c	if(ipath.lt.1.or.ipath.gt.3) ipath=1	!in .ini after 1st time
c	print 95,ipath
c95	format(
c     & ' Where to write last sequence list for inspection in SCAN:',/,
c     & ' (1) EKDIST.TMP (i.e. in same directory as program)',/,
c     & ' (2) \FORTRAN\SCAN\EKDIST.TMP',/,
c     & ' (3) A different path',/,
c     & ' Option number [',i2,'] = ')
c	call INPUTi(ipath)
c	if(ipath.eq.1) then
c	   ekpath='EKDIST.TMP'
c	else if(ipath.eq.2) then
c	   ekpath='\FORTRAN\SCAN\EKDIST.TMP'
c	else if(ipath.eq.3) then
c	   call TITENT0('Enter entire path',ekpath,33,.false.)
c	endif
c
	ekpath='c:\ekdist.tmp'
	ioffset=36 + 4*nfile + 3*4*kval*k1 + 4*k1
	OPEN(unit=19,file=EKPATH,status='UNKNOWN',
     &    access='TRANSPARENT')
	 write(unit=19,rec=1) k1,k2,ksym,kval,nfile,name,idiscn,ioffset,
     & (kfile(i),i=1,nfile),
     & ((kcombs(i,j),ymid(kcombs(i,j)),j=1,kval),i=1,k1),
     & (ifreq(i),i=1,k1)
	irec=ioffset+1
	do i=1,k1
	   do j=1,ifreq(i)
		do k=1,2
		   write(unit=19,rec=irec) intvals(i,j,k)
		   irec=irec+4
		enddo
	   enddo
	enddo
	CLOSE(unit=19)
c
	print 95
      if(discprt) write(8,95)
95	format(
     & ' Last sequence stored in c:\ekdist.tmp so transitions',/,
     & ' can be inspected in SCAN',/)
c
c
c Now display amp(i) vs amp(i+1).  If amplitude windows define above, then
c transitions between open levels can be distinguished from others.
97	continue
	call DCASK('Display amplitude(i) vs amplitude(i+1)','y',ans)
	if(ans.eq.'Y') then
	   do i=1,100
		icol(i)=-1
	   enddo
	   isetcol=1		!so colour of calc lines can be set
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
	      call RANDSK(ix,iy,iz,-1,repeat)	!iwrite=-1 so no guestion
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
	   ncalc=200
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
c	   dx=(xmax-xmin)/float(ncalc-1)
c calculate from xmin=0 to enable plot to be rescaled from amp=0 if req
	   dx=xmax/float(ncalc-1)
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
		   iline(j1)=2	!dashed
		   icol(10+j1)=2 	!dark green
		enddo
		do j=1,nline	!vertical lines
		   j1=j+nline+1
		   do i=1,ncalc
			xcal(i,j1)=yline(j)
			ycal(i,j1)=ycal(i,1)
		   enddo
		   icurvc(j1)=j1
		   ncal(j1)=ncalc
		   iline(j1)=2	!dashed
		   icol(10+j1)=2 	!dark green
		enddo
	   endif
c
	   cbig=2.5
	   ifont=4
	   ifitype=0		!no display of fitted parameters
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
	   itrace=0
	   call VPLOT5(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,syms,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,draft,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,interp,
     & isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kmax,iver)
	   DEALLOCATE(xval,yval,xcal,ycal)
	   DEALLOCATE(ndat,icurvd,isym,ijoin,syms)
	   DEALLOCATE(ncal,icurvc,iline)
	   if(allocated(icurvw)) then
		DEALLOCATE(icurvw,weight)
	   endif
c=	endif
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
	   xmin=0.0		!changed to -0.2 if not exc0 to allow for sd
	   ymin=0.0
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
	      call RANDSK(ix,iy,iz,-1,repeat)
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
c=	   delty=deltx
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
c/	   ALLOCATE(freq(ny,nx),znew(ny,nx),ampx(nx),ampy(ny))
	   ALLOCATE(freq(nx,ny),znew(nx,ny),ampx(nx),ampy(ny))
	   ALLOCATE(badval(nx,ny))
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
c/		   freq(ir,jr)=0.0
c/		   badval(ir,jr)=.false.
		   freq(jr,ir)=0.0
		   badval(jr,ir)=.false.
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
c/				   freq(ir,jr)=freq(ir,jr) + 1.0
c/				   if(freq(ir,jr).gt.zmax) zmax=freq(ir,jr)
				   freq(jr,ir)=freq(jr,ir) + 1.0
				   if(freq(jr,ir).gt.zmax) zmax=freq(jr,ir)
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
c=	   zmin=0.0
	   z=ifixr(zmax)
	   if(z.lt.zmax) then
		zmax=z + 1.0
	   else
		zmax=z
	   endif

c=============================================================
c Plot the 3D graph
	   ans='Y'
	   call DCASK('Use 3D smoothing on plot',ans,ans)
	   if(ans.eq.'Y') then
		power=1.0
     		call SMOOTH3D(freq,znew,nx,ny,power,nx,ny)
	   else
		do i=1,nx
		   do j=1,ny
			znew(i,j)=freq(i,j)
		   enddo
		enddo
	   endif
c/	   do i=1,nx
c/		   do j=1,ny
c/			a=freq(j,i)
c/			znew(i,j)=a
c/		   enddo
c/	   enddo


	   call GINO
89	   continue
	   idrawt=1
	   idrawx=1
	   idrawy=1
	   idrawz=1
	   icol(23)=0		      !in common for gplot3d
	   icol(25)=9
	   icol(71)=7
	   xtitle='amplitude(i)'
	   ytitle='amplitude(i+1)'
	   ztitle='Number'
	   title2='Plot of consecutive amplitudes'
c##################### Ioana
c  22/02/99
c	you can change them !!!!
	   kcol=2
	   posneg=.true.
	   isetcol=2
	   call GPLOT3D(ampx,ampy,znew,badval,nx,ny,nx,ny,
     & xtitle,ytitle,ztitle,title2,idrawt,idrawx,idrawy,idrawz,
     &       kcol,posneg,isetcol,qfile)
	   DEALLOCATE(freq,znew,ampx,ampy,badval)
	  endif
	endif
	goto 994	!exit
c
c NOW id=2 FOR ANALYSIS BY BURSTS
c Amp ranges set -now locate bursts (including mean burst amp)
c Set tcrit as in CDIST2
120	continue
============start of getburst
	if(nfile.eq.1) then
	   onetcrit=.true.
	   if(tcrit.lt.0.) then		!first time
		print 321
321		format(' Critical gap length (ms) = ')
		call INPUTr(tcrit)
	   else
		print 320,tcrit
320		format(' Critical gap length [',f7.2,'ms] = ')
		call INPUTr(tcrit)
c		call INPUTr(x)
c		if(x.gt.0.0001) tcrit=x	!else prev tcrit used
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
	print 703
703	format(
     & ' Exclude time spent in openings with amplitudes outside',
     & ' specified range [N] ? ')
	ans='N'
	call INPUTa(ans)
	conam1=ans.eq.'Y'
	if(conam1) then
	   print 227
227	   FORMAT(/,' Define amplitude range:',/,
     & ' Low amp, high amp (real pA with sign)= ')
	   call INPUT2r(amplo,amphi)
	   if(amphi.lt.amplo) then
		a1=amphi	!swap
		amphi=amplo
		amplo=a1
	   endif
         if(discprt) write(8,705)amplo,amphi
705	   format(' Amplitude range from ',g13.6,' to ',g13.6)
	endif
	ans='Y'
	call DCASK(
     &'Find a gap > tcrit before the first burst in each file',ans,ans)
	findgap=ans.eq.'Y'
	ans='N'
	call DCASK(
     &'Treat an unusable shut time as a valid end of burst',ans,ans)
	badend=ans.eq.'Y'
c
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
	print 3211
3211	format(' List intervals in all bursts [N] ? ')
	ans='N'
	call INPUTa(ans)
	pbst=ans.eq.'Y'
	ppopen=pbst		!simpler than in cdist2!
c
c Now define the bursts:
	first=.true.	!first pass to determine nbst and allocate arrays
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
      print 80
80	format(/,
     & ' Now define the bursts',/,
     & ' To see the values,',
     & ' put CAP LOCK on now',/,
     & ' (press any key to continue)')
	call ANYKEY()
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
	if(newfile.and.pflag) then
	   print 70,ifile,tcrit
	   if(discprt.and.deb) write(8,70) ifile,tcrit
70	   format(' New file: #',i3,'  tcrit = ',g13.6,' ms')
	endif
128	continue
c Burst extends from TINT(ifst) to TINT(K-1)
c Locate start and end if burst for ibtype=1.
c Note that burst may start with a sublevel opening even if EXCSUB
c is true as may have later full openings which need to be counted
c in for ID=4,6 for example.
c MODIF 07/17/90 04:12pm so bursts with sublevels rejected only
c if excsub=true for id=9,10.
c For  id=10,11 may also have EXCSUB true. For id=10 best
c omit entirely any burst for which the Jth (or any later if Jth neg)
c open period contains any sublevel. For id=11 omit the pair of values
c if EITHER the Ith OR the Jth open period contains any sublevel.
c For each open period ISUBS()=0 if it contains no sublevels; =1 if
c it contains any sublevels.
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
	isub=0
	tgap=0.0	!length of gap-within-bst period
	Nbst=Nbst+1	!Update # of bursts- reversed below if bst is bad
	BAD=.false.	!current burst is usable
C    For ID=4,6 must have at least one full opening,or get rubbish -BAD1
C     checks this.
c=	if((id.eq.4.or.id.eq.6).and.iexc.eq.2) FULL=(amp.ge.amplo)
c=     & .and.(amp.le.amphi)
c===full/sub are not set unless amplo, amphi defined
c	if(FULL) bad1=.false.	!at least one full opening in burst
c	SUB=open.and.(.not.full)
c	if(.not.sub) goto 44
c	isub=1		!this open period contains a sublevel
44	topen=tint(ifst)	!first indiv opening
48	igflag=0	!currently not in a gap period
	ioflag=1	!currently in an open period
	k=Ifst+1		!LOOK AT NEXT INTERVAL	DOES IT END BURST?
C  k loop starts at label 323
323	continue
	amp=ampl(k)
	open=amp.ne.0
	shut=.not.open
c=	full=abs(amp-avamp).le.acrit
c	if(.not.sublev) full=open
c=	if((id.eq.4.or.id.eq.6).and.iexc.eq.2) FULL=(amp.ge.amplo)
c=     & .and.(amp.le.amphi)
c===full/sub are not set unless amplo, amphi defined
c	if(full) bad1=.false.	!at least one full opening in burst
c	sub=open.and.(.not.full)
c	if(sub) isub=1		!this open period contains a sublevel
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
		   print 71,k
		   if(deb.and.discprt) write(8,71) k,tint(k),iprops(k)
71		   format(
     &	 ' Bad gap #',i5,' aborts burst:',g12.5, ' ms, props = ',i2)
		else
		   if(tgap.gt.tcrit) goto 324	!valid end of burst
		endif
	   endif
	endif
C  Go to 324 when end of bst found, with k=index of gap that ends it.
C NOTE that the last interval is always set unusable, but it is not
C a valid end-of-burst
c===full/sub are not set unless amplo, amphi defined
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
130	nop=nop+1	!no of indiv openings
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
	if(nas.gt.5000) print 623
623	format(' ***ERROR: MORE THAN 5000 OPENINGS/BURST') !enlarge OPS()
	ops(nas)=topen
c==	isubs(nas)=isub	   !=1 if this open period contains a sublevel
c#	if(deb) print 615,nas,ops(nas)
c615	format(' OPS(',i3,')= ',G13.6)
	topen=0.0		!zero for next open period
c==	isub=0
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
	if(.not.first) then
	   ibstart(nbst)=ifst
	   ibend(nbst)=k-1
	endif
	if(deb) then
c     type intervals preceding current burst if any
c	  if(ifst-1.gt.ilast) call DEBTYP(0,ilast,ifst-1,tint,ampl,
c     &   iprops,0,-1,-1,yval,.false.,nintt,ndimy)
	   print 132,Ifst,k-1
	   if(discprt) write(8,132) ifst,k-1
132	   format(/,/,' BURST = intervals ',i5,' to ',i5)
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
	   if(.not.first) bampl(nbst)=amp		!amp (set dubious if nec)
	   if(deb) then
		print 617,nbst,amp
		if(discprt) write(8,617) nbst,amp
617		format(' Mean amp for burst #',i4,' (pA) = ',G13.6,/)
	   endif
	endif
c
c Exclude bursts as specified
	if(exass.and.dubamp) bad=.true.
c=	if(id.eq.12.or.id.eq.13) goto 524
	amp=bampl(nbst)
	if(conam1.and.((amp.lt.amplo).or.(amp.gt.amphi))) bad=.true.
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
c
	if((nas.ne.nbs+1).or.(nbs.ne.nsg).or.(nas.ne.nsg+1)) then
	   call bell(2)
	   print 598,nas,nbs,nop,nsg
598	   format(' ***ERROR: nas,nbs,nop,nsg= ',4i8)
	   pause
	endif
c	if(nas.ne.nbs+1) goto 597
c	if(nbs.ne.nsg) goto 597
c	if(nas.ne.nsg+1) goto 597
c	goto 599
c597	call bell(2)
c	print 598,nas,nbs,nop,nsg
c598	format(' ***ERROR: nas,nbs,nop,nsg= ',4i8)
c	pause
c599	continue
c
c Define Yval
c338	continue
C End of current burst: UPDATE Nsgap ETC BY ADDING VALUES FOR LAST BURST
c print this burst?
c
c Bit added 06/22/89 01:29pm to print total open, total shut and
c Popen for each burst when id=3, if ppopen=true.
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
c	if(id.eq.14) then
c	   if(bmin.gt.0.000001) then
c	     if(blength.gt.bmin) then
c		nb14=nb14+1
c		Yval(nb14)=popen
c	     endif
c	   else if(ibmin.gt.1) then
c	     if(nas.ge.ibmin) then
c		nb14=nb14+1
c		Yval(nb14)=popen
c	     endif
c	   else if(ipop.eq.1) then
c	     if(nas.gt.1) then
c	      nb14=nb14+1
c	      Yval(nb14)=popen1
c	     endif
c	   else
c	      nb14=nb14+1
c	      Yval(nb14)=popen
c	   endif
c	   if(deb) call DEBTYP(0,-1,-1,tint,ampl,iprops,
c     &  0,nb14,nb14,yval,.false.,nintt,ndimy)		!print YVAL(nbst)
c	endif
c
c	pbst=(tc.lt.-0.001).and.ID.ge.3.and.ID.le.6.and.(Yval(Nbst).gt.
c     &    abs(tc))		!print BURST
c	pbst=pbst.or.listall.or.(Nbst.ge.N1.and.Nbst.le.N2).or.
c     & bprint
	if(.not.pbst) then
	   if(ppopen) then	!print bst number since skipped if pbst=false
	      print 57, nbst
      	if(pon()) write(7,57) nbst
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
            if(pon()) write(7,56) m,tint(m),ampl(m1)
            if(discprt) write(8,56) m,tint(m),ampl(m1)
c56	      format(6x,i6,2x,2g13.6)
56	      format(i6,2x,2g13.6)
	   enddo
	   print 108
	   if(discprt) write(8,108)
	endif
      print 57, nbst
      if(pon()) write(7,57) nbst
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
     &	blength,popen
	   if(discprt)write(8,403) ifst,totop,nas,totgap,nbs,
     &	blength,popen
c403	   format(' Burst #',i4,/,
403	   format(' Burst starts at interval # ',i9,/,
     &  ' Total open = ',g13.6,' (n = ',i4,');',
     &  ' Total shut = ',g13.6,'(n = ',i4,')',/,
     &  ' Burst length = ',g13.6,'   P(open) = ',g13.6)
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
c Now allocate ibstart, ibend and repeat whole thing to get index of 1st and last
c opening in each burst
	if(first) then
	   pause 'repeat to define ibstart etc)
	   first=.false.
	   if(allocated(ibstart)) then
		DEALLOCATE(ibstart,ibend,bampl)
	   endif
	   ALLOCATE(ibstart(nbst),ibend(nbst),bampl(nbst))
	   goto 121
	endif
c
c After second pass get here with all bursts defined
c====
99	continue
c
	print 581, nbad1,nbad2
	if(discprt) write(8,581)
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
58	   format(' Critical gap length (ms)= ',g13.6)
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
==========end of getburst
c
C
C BURSTS HAVE NOW BEEN DEFINED. -NEXT LIST SEQUENCES BASED ON BURSTS AND/OR
C VIEW SELECTED BURSTS
c===========up to here
c Deallocate arrays before leaving
994	continue
	if(allocated(kcombs)) then
	   DEALLOCATE(kcombs,ifreq,intvals)	!no longer needed
	endif
	if(allocated(ibstart)) then
	   DEALLOCATE(ibstart,ibend,bampl)
	endif
	pause	             	!before display
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
		if(time.ge.tc(i)) goto 99	!valid value
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

	subroutine NCOMB(nval,kval,kcombs,k1,ksym,kdim)
C
	dimension k(10)
	dimension kcomb(kdim,10),kcombs(kdim,10)
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
	call ASCOMB(k,kcomb,k1,kval,kdim)
	goto 2
31	continue
c
	do 3 i3=1,nval
	if(i3.eq.i2) goto 3
	k(3)=i3
	if(kval.gt.3) goto 41
	k1=k1+1
c	print 100,(k(i),i=1,kval)
	call ASCOMB(k,kcomb,k1,kval,kdim)
	goto 3
41	continue
	do 4 i4=1,nval
	if(i4.eq.i3) goto 4
	k(4)=i4
	if(kval.gt.4) goto 51
	k1=k1+1
c	print 100,(k(i),i=1,kval)
	call ASCOMB(k,kcomb,k1,kval,kdim)
	goto 4
51	continue
	do 5 i5=1,nval
	if(i5.eq.i4) goto 5
	k(5)=i5
	if(kval.gt.5) goto 61
	k1=k1+1
c	print 100,(k(i),i=1,kval)
	call ASCOMB(k,kcomb,k1,kval,kdim)
	goto 5
61	continue
	do 6 i6=1,nval
	if(i6.eq.i5) goto 6
	k(6)=i6
	if(kval.gt.6) goto 71
	k1=k1+1
c	print 100,(k(i),i=1,kval)
	call ASCOMB(k,kcomb,k1,kval,kdim)
	goto 6
71	continue
	do 7 i7=1,nval
	if(i7.eq.i6) goto 7
	k(7)=i7
	if(kval.gt.7) goto 81
	k1=k1+1
c	print 100,(k(i),i=1,kval)
	call ASCOMB(k,kcomb,k1,kval,kdim)
	goto 7
81	continue
	do 8 i8=1,nval
	if(i8.eq.i7) goto 8
	k(8)=i8
	if(kval.gt.8) goto 91
	k1=k1+1
c	print 100,(k(i),i=1,kval)
	call ASCOMB(k,kcomb,k1,kval,kdim)
	goto 8
91	continue
	do 9 i9=1,nval
	if(i9.eq.i8) goto 9
	k(9)=i9
	if(kval.gt.9) goto 101
	k1=k1+1
c	print 100,(k(i),i=1,kval)
	call ASCOMB(k,kcomb,k1,kval,kdim)
	goto 9
101	continue
	do 10 i10=1,nval
	if(i10.eq.i9) goto 10
	k(10)=i10
	if(kval.gt.10) goto 97	!error
	k1=k1+1
c	print 100,(k(i),i=1,kval)
	call ASCOMB(k,kcomb,k1,kval,kdim)
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
115	call SYM(ir,ival,kcomb,kcombs,ks1,kval,k1,kdim)
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
	   do j=1,kval
		kcombs(ks1,j)=kcomb(i,j)	! copy it
		k(kval-j+1)=kcomb(i,j)		!mirror image in k()
	   enddo
	   ks1=ks1+1		!increment index
	   if(ks1.gt.k1) then
		print 117, ks1,k1
117		format(' ks1 = ',i5,' exceeds k1 = ',i5)
		pause
	   endif
	   do j=1,kval
		kcombs(ks1,j)=k(j)	! copy mirror image
	   enddo
	   kcomb(i,1)=-1		!mark the ones that have been copied
c look for mirror image and mark it
	   do 118 i1=i,k1
		if(kcomb(i1,1).lt.0) goto 118		!already copied
		   do j1=1,kval
			if(k(j1).ne.kcomb(i1,j1)) goto 118	!not mirror
		   enddo
		   kcomb(i1,1)=-1			!mirror- mark it
		   goto 116		!and leave loop
118		continue
116	   continue
c
	goto 99
c
97	print 110
110	format(' k>10 not allowed')
99	continue
	RETURN
	END

	subroutine ASCOMB(k,kcomb,k1,kval,kdim)
c Assign combs to rows of Kcomb()
	dimension	k(10)
	dimension kcomb(kdim,10)
	do 1 i=1,kval
1	kcomb(k1,i)=k(i)
	return
	end

	subroutine SYM(ir,ival,kcomb,kcombs,ks1,kval,k1,kdim)
	dimension kcomb(kdim,10),kcombs(kdim,10)
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



