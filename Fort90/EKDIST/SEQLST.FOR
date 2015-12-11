	subroutine SEQLST(tint,ampl,iprops,nint,nintt,avamp,acrit,
     & trise,iscan,ylo,yhi,ymid,kval,treso,tresg,tc,index,
     & kfile,nfile,name,idiscn,nvalsav,idiskq,
     & iexstrt,iexend,onetcrit,tcvals,tcrit,ibamp,
     & tint0,consamdef,adcfil,nfits,ifits,timsav,ibaselin,dfinter,
     & sfac2,npfilt,opendown,oneshut,
     & nsam,srate,irecfst,calfac,ioffset)
c
c Version for new EKDIST/SCAN 02/16/94 04:27pm
c
c 06/06/89 07:02pm Lahey version
c To list sublevel sequences in EKDIST
c
c Modif 10/22/99 07:14am to make the display correct.  One problem is
c that value of npfilt in scan.dat is actually the number of points
c to jump after a fit, so npfilt is recalculated in EKDIST (as in INSCAN)
c Other problem was in calculation of totlen and length of bit to be coloured.
c
c Modif 04/04/99 06:21am to display bursts with VSAMP (last line pf params)
c Modif 03/22/99 06:12am Remove '(2) List all groups of openings with sublevels'
c   Sublevels were defined as llevels that differ  from avamp by more than
c   acrit, but avamp is unreliable and acrit=0. by default (options for
c   sublevels defined in this way also removed from cdist2 now).
c
c   Add option to (a) define levels via amp ranges and (b) define bursts,
c   and list/plot which levels occur in which bursts (code for identifying
c   bursts is simplified form cdist2, but here also keep index of 1st and last
c   opening in the burst in ibindex(1,i) and ibindex(2,i) resp.
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
	real*4 ylo1(10),yhi1(10),ymid1(10),tc1(10)
	integer iexstrt(50),iexend(50)
	logical onetcrit,findgap,badgap,opendown
	logical dubamp,exass,oneshut,excone,excshut,match
	allocatable::ibindex,bampl 	!record 1st and last op, & amp for each bst
	integer ibindex(:,:)
	real*4 bampl(:)
	allocatable::blevel
	logical blevel(:,:)
	integer ibcomb(10,5)
	character cnum*11,string*15
c For display in VSAMP
	real*4 TINT0(nintt)
	real*8 timsav(nfits),dfinter
	integer*4 ifits(nfits)
	integer*2 ibaselin(nfits)
	character*33 adcfil
	character line1*70,line2*1,line3*1
	logical consamdef,dispbst,dispseq,colseq,reject,stop,dispall
c For getcons
	character cdate1*11,adctim1*8,title*76		!read from consam
	logical noconsam,newform,cjdat
c
	SAVE id
c
	logical mono
	COMMON/cols/icol,mono
c
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo2,xhi2,ylo2,yhi2
c     & xlo,xhi,ylo,yhi
	common/dp/discprt
	common/rand/ix,iy,iz
	common/ampsav/ncompa,ameana(10),areaa(10),sda(10)
	character*40 strings(50)	!for popmenu
	character*55 helps(7)	!help strings
	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
c
c Define functions
	pon()=slock()
	debug()=caplock()
	dubious(ni)=BTEST(iprops(ni),0)	!ampl(i) was dubious (bit 0 set ='1')
	badint(ni)=BTEST(iprops(ni),3)	!tint(i) was unusable (bit 3='8' set)
	badgap(ni)=BTEST(iprops(ni),3)	!tint(i) was unusable (bit 3='8' set)
c
	nval=0		!until defined
107	continue
c
	print 2323
2323	FORMAT(' AMPLITUDE SEQUENCE LISTING OPTIONS',/,
     &' (1) List frequencies of direct transitions between amp levels.')
c	print 2322
c2322	FORMAT(' (2) List all groups of openings with sublevels')
	print 2322
2322	format(
     &' (2) List amplitude levels that occur in each burst')
	if(id.le.0) id=1
	print 106,id
106	format(' Option number [',i2,'] = ')
	call INPUTi(id)
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
	excdub=.true.
	if(id.eq.1) then
	   print 421
421	   format(' List sequences of k consecutive levels: k [2] = ')
	   kval=2
	   call INPUTi(kval)
	   print 72
72	   format(
     & '&Exclude sequences with any dubious amplitudes [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   excdub=UC(ans).ne.'N'
	endif
41	continue
	samwin=.false.
	if((ylo(1).gt.-999.).and.(nvalsav.ge.1.and.nvalsav.le.10)) then
c print the previous values from .ini file
	   nval=nvalsav
	   print 454,nval
	   if(pon()) write(7,454) nval
         if(discprt) write(8,454) nval
c454	   format(/,1x,i4,' current windows, defined thus:')
	   do i=1,nval
		print 453,i,ylo(i),yhi(i),tc(i)*1.e3
		if(pon()) write(7,453)i,ylo(i),yhi(i),tc(i)*1.e3
      	if(discprt) write(8,453)i,ylo(i),yhi(i),tc(i)*1.e3
c453		format(i4,3x,g13.6,' to ',g13.6,
c     &	' pA: resolution (mus) = ',g13.6)
	   enddo

	   print 612
612	   format(' Use previously specified amplitude windows [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(ans.eq.'Y') then
		samwin=.true.
		nval=nvalsav
		oneshut=(ylo(1).lt.0..and.yhi(1).gt.0.).or.
     &		    (ylo(1).gt.0..and.yhi(1).gt.0.)
		opendown=yhi(nval).lt.0.
	   endif
	endif
	if(.not.samwin) then
	   if(nvalsav.le.0.or.nvalsav.gt.10) nvalsav=3
	   nval=nvalsav
	   print 420,nvalsav
420	   format(
     & ' Number of conductance levels to be identified (0=restart) [',
     &	i2,'] =  ')
	   call INPUTi(nval)
	   nvalsav=nval
	   if(nval.eq.0) goto 41
	endif
c
	if(id.eq.2) goto 131
c
c Calculate # of combos, and allocate intvals()
	k1=nval*(nval-1)**(kval-1)
c Allocate arrays
	k2=10000	!2nd dimension of intvals (now kept in ekdist.tmp)
	kdim=k1
	if(allocated(kcombs)) then
	   DEALLOCATE(kcombs,ifreq,intvals)
	endif
	ALLOCATE(kcombs(kdim,10),ifreq(kdim),intvals(kdim,k2,2))
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
131	continue	!skip to here for id=2
	if(samwin) goto 615
c Section to define windows
	print 611
611	format(' Are openings downwards [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(ans.eq.'Y') then
	   opendown=.true.	!for .ini
	   sign=-1.
	else
	   opendown=.false.	!for .ini
	   sign=1.
	endif
	print 61
61	format('&Calculate critical amplitudes for each window [N] ? ')
	ans='N'
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
	   oneshut=.false.
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
	   oneshut=.true.
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
	   oneshut=.false.
	else
	   oneshut=.true.
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
	  tc0=fval*trise*1.e-3		!msec
	  tres=tresg
	  if(treso.lt.tresg) tres=treso
	  if(tc0.lt.tres-0.001) then
		call BELL(2)
		call DCASK('Do you really want no minimum length','Y',ans)
		if(ans.eq.'N') goto 441
		fval=0.0
		tc0=0.0
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
		tc0=0.0
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
			tc0=0.0
			tc(i)=tc0
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
c Can now calc number and level of amplitude lines which may be needed
c later for i vs i+1 plot, or for VSAMP
	nline=0
	if(nval.gt.0) then
	   nl=0
	   do i=1,nval
		y1=abs(ylo(i))
		y2=abs(yhi(i))
		if(y1.gt.0.02) then
		   nl=nl+1
		   yline(nl)=ylo(i)
		endif
		if(y2.gt.0.02) then
		   nl=nl+1
		   yline(nl)=yhi(i)
		endif
	   enddo
c    now remove duplicate entries
	   m=0
	   do i=1,nl-1
		do k=i+1,nl
		   if(abs(yline(i)-yline(k)).lt.0.01) yline(i)=10000.
		enddo
	   enddo
	   do i=1,nl
		if(yline(i).lt.9999.) then
		   m=m+1
		   yline(m)=yline(i)
		endif
	   enddo
	   nline=m
	endif
c=====
c       First calc number of calc lines so ycal() etc can be allocated
c If ranges defined then draw dotted lines at them (except shut range)
c	   nline=0
c	   if(nval.gt.0) then
c		nl=0
c		do i=1,nval
c		  y1=abs(ylo(i))
c		  y2=abs(yhi(i))
c		  if(y1.gt.0.02) then
c			nl=nl+1
c			yline(nl)=y1
c		  endif
c		  if(y2.gt.0.02) then
c			nl=nl+1
c			yline(nl)=y2
c		  endif
c		enddo
cc            now remove duplicate entries
c		m=0
c		do i=1,nl-1
c		   do k=i+1,nl
c			if(yline(i).eq.yline(k)) yline(i)=10000.
c		   enddo
c		enddo
c		do i=1,nl
c		   if(yline(i).ne.10000.) then
c			m=m+1
c			yline(m)=yline(i)
c		   endif
c		enddo
c		nline=m
c	   endif
c=====
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
	if(nfile.eq.1) then
	   ans='Y'
	   call DCASK('Display sequences',ans,ans)
	   dispseq=ans.eq.'Y'
	else
	   dispseq=.false.
	endif
c If showing sequences, get consam, and set up graphics here so can stay
c in graphics mode throughout (unless debug on in which case must go into
c graphics only for display
	if(dispseq.and.(.not.consamdef)) then
	   cjdat=.false.
c Get consam/axon file name
	   call GETCONS(adcfil,noconsam,title,cdate1,adctim1,nsam,
     &    srate,cjdat,irecfst,newform,ioffset,calfac,ffilt,idest)
	   consamdef=.true.
	   call gsetcols(0)
	   call errswi(-1)
	   call brkswi(1)
	   call chaswi(1)
	   call grfmod (1)
	   call harcha
	   call papenq(xp,yp,ipap)
c Want to set graphboundary to correspond to data window, bottom of which
c is at iyd1=127 pixels, so need graph units which correspond to this
c	   ix=0
c	   call PIXGRA(ix,iyd1,xg,yg)
	   vxlo=0	! for VIEWPORT
	   vxhi=xp
	   vylo=0
c	   vylo=yg
	   vyhi=yp
c	   xlo=0		! for graphboundary
c	   xhi=xp
c	   ylo=0
c	   yhi=yp
	   xlo2=0		! for graphboundary
	   xhi2=xp		!call xlo2 etc for common (xlo exists already)
	   ylo2=0
	   yhi2=yp
c	   xlo=0.05*xp			! set display location on screen
c	   xhi=0.95*xp
c	   ylo=0.05*yp
c	   yhi=0.8*yp	!lower to leav room for dialog box
c	   call mode(18)
c	   icf=7		!frame colour for dialog box 1
c	   ict=11	!text colour for dialog box 1
	   if(.not.debug()) call MODE(18)
	else if(dispseq) then	!consamdef=true
	   if(.not.debug()) call MODE(18)
	endif
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
	   int00=int0	!record index of first interval at first level of sequence
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
		if(debon) then
		   print 401,int,num
		   if(discprt) write(8,401) int,num
401		   format(' start value abandoned: int, num = ',i6,i4)
		endif
		int0=int0+1				!for num<0
		if(int0.ge.nint) goto 428		!all obs done
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
c===	   totlen=0.0
	   totlen=ttot
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
	   int00=int0	!record index of first interval at first level of sequence
	   j=j+1
	   kseq(j)=num		!1st in sequence
c Now look for rest of sequence
	   int=int0+1			!next interval
c	   totlen=0.0
	   totlen=tint(int0)
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
	   if(ifreq(i).gt.k2) then
		call BELL(2)
		print 701,k2
701		format(' ERROR:  ifreq > ',i5)
		good=.false.
	   else
	      intvals(i,ifreq(i),1)=index(int1)	!record int for SCAN
	      intvals(i,ifreq(i),2)=itotlen		!record totlen for SCAN
		good=.true.
	   endif
c Display sequences
	   if(dispseq.and.good) then
		call INTCONV(nseq,cnum)
		line1='Seq '//charnb(cnum)//': levels'
		line2=''		!no second line for message
		line3=''		!no 3rd line for message
		do i1=1,kval
		   call INTCONV(kseq(i1),cnum)
		   line1=charnb(line1)//' '//charnb(cnum)
		enddo
		idelt=10.*sngl(1.d3/dfinter)
		ppms=sngl(1.d3/dfinter)		!points per ms
		i1=int0-1	!one trans before the sequence
		i2=int+2	!one transitions after sequence
		if(i1.lt.1) i1=1
		if(i2.gt.nint) i2=nint
c===		ib1=int0
		ib1=int00
		ib2=int

c         Must now convert i1,i2,ibstart, ibend, to index
c	    in consam=j1, j2, jb1,jb2
		iflast=1	!first time -the do indices in ascending order
c for the transition that starts the sequence, use indexgb version to get
c baseline
		i1=index(i1)	!index in raw data, before resolution imposed
		j1=INDEXGB(i1,ibase,
     &	 iflast,nfits,ifits,timsav,ibaselin,tint0,dfinter,nintt)
		ib1=index(ib1)	!index in raw data, before resolution imposed
		jb1=INDEXG(ib1,
     &	 iflast,nfits,ifits,timsav,tint0,dfinter,nintt)
		ib2=index(ib2)	!index in raw data, before resolution imposed
		jb2=INDEXG(ib2,
     &	 iflast,nfits,ifits,timsav,tint0,dfinter,nintt)
		i2=index(i2)	!index in raw data, before resolution imposed
		j2=INDEXG(i2,
     &	 iflast,nfits,ifits,timsav,tint0,dfinter,nintt)
c
		ppms=sngl(1.d3/dfinter)		!points per ms
c Add 50 ms each side on top trace
		idelt=ifixr(50.*ppms)
		j1=j1-idelt
		j2=j2+idelt
c Define region of sequence itself, to be coloured on lower trace
c jb1=point in consam for transition number=int0=ib1 in data after
c resolution imposed
c====?      totlen=totlen+tint(int)	!total length of sequence
		j11=jb1+npfilt + 4
		j22=j11 + ifixr(totlen*ppms) -npfilt -4
c Show 10 ms before and after each sequence on lower trace=idelt points
		idelt=ifixr(10.*ppms)
		jb1=jb1-idelt
		if(jb1.lt.j1) j1=jb1
		jb2=jb2+idelt
		if(jb2.gt.j2) j2=jb2
		if(j11.lt.jb1) j11=jb1
		if(j22.gt.jb2) j22=jb2
c
c		imode=1	!estimate baseline internally in VSAMP
		imode=0
		ybase=float(ibase)/sfac2	!sfac2 converts intermed units to pA
c		amp=-1.		!scale internally
		amp=abs(avamp)
c		colseq=.false.	!do not colour the sequence on the lower trace
		colseq=.true.	!colour the sequence on the lower trace
		reject=.false. 	!hide the reject option (for now)
		stop=.true.		!show the stop display option
		if(debon) call MODE(18) 	!into graphics for display only
		call VSAMP(j1,j2,jb1,jb2,consamdef,adcfil,nline,yline,
     &	   ybase,amp,imode,colseq,j11,j22,line1,line2,line3,reject,
     &         stop,qfile,nsam,srate,irecfst,calfac,ioffset)
		if(debon) call VIDEOMOD(3)
	   endif
	   if(dispseq) then
		if(stop) then
		   dispseq=.false.
		   call VIDEOMOD(3)
		endif
	   endif
c	   if(dispseq) then
c		ans='N'
c		call DCASK('Stop displaying sequences',ans,ans)
c		dispseq=ans.eq.'N'
c	   endif
c
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
	      tc0=fval*trise*1.e-3		!msec
	      if(tc0.lt.treso) then
		   call BELL(2)
		   call DCASK('Do you really want no minimum length',
     &		 'Y',ans)
		   if(ans.eq.'N') goto 521
		   fval=0.0
		   tc0=0.0
	         conam=.false.
	      endif
	      do i=1,nval
		   tc(i)=tc0
	      enddo
	   endif
c Number of lines to be plotted now defined above
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
	ndim=1
	if(allocated(ibindex)) then
	   DEALLOCATE(ibindex,bampl)
	endif
	ALLOCATE(ibindex(2,ndim),bampl(ndim))
	imode=0	!get nbst
	print 136
136	format(/,' NOW DEFINE THE BURSTS')
	call GETBURST(tint,ampl,iprops,nintt,nint,nfile,
     & iexstrt,iexend,onetcrit,tcrit,tcvals,ibamp,findgap,
     & dubamp,exass,ibindex,nbst,bampl,ndim,idebug,imode)
c	pause 'repeat to define ibindex etc'
	ndim=nbst
	if(allocated(ibindex)) then
	   DEALLOCATE(ibindex,bampl)
	endif
	ALLOCATE(ibindex(2,ndim),bampl(ndim))
	imode=1	!define ibindex() etc
	call GETBURST(tint,ampl,iprops,nintt,nint,nfile,
     & iexstrt,iexend,onetcrit,tcrit,tcvals,ibamp,findgap,
     & dubamp,exass,ibindex,nbst,bampl,ndim,idebug,imode)
c
c Count the number of bursts that have
c (1) at least one sojourn (of valid length) in
c 	at amplitude 1, 2,..., nval ONLY (whole burst is spent within the
c	specified  amplitude window
c (2) sojourns at level 1 and 2, 1 and 3, 2 and 3, etc ONLY, e.g. level 1 and 2
c	only means that there are valid sojourns at these two levels, but at
c	no other levels.  Of these also list how many of these bursts contain
c	direct transitions between the two levels
c (3) sojourns at level 1,2 and 3 etc...
c For counting number of levels visited, any individual opening at that level
c counts so no need to worry about multiple transitions?  But they do matter if
c concatenation of multiple transitions within one window makes a level
c resolvable when no individual opening is resolvable (not likely!)
c
c For each burst, find ALL the levels visited in that burst, ignoring direct
c transitions e.g. blevel(i,j)=true if level i occurs in burst j
c Repeat to print all four possibilities without asking
1521	irep=1
	excshut=.false.	!for first time
	excone=.false.
1201	continue		!return here for next repeat
c
	nval1=nval
	do i=1,nval
	   ylo1(i)=ylo(i)
	   yhi1(i)=yhi(i)
	   ymid1(i)=ymid(i)
	   tc1(i)=tc(i)
	enddo
c=	ans='Y'
c=	call DCASK('Exclude bursts with only one opening',ans,ans)
c=	excone=ans.eq.'Y'
	if(oneshut) then
c=	   ans='Y'
c=	   call DCASK('Exclude the shut level',ans,ans)
c=	   excshut=ans.eq.'Y'
	   if(excshut) then
		nval1=nval-1
		do i=1,nval1
		   ylo1(i)=ylo(i+1)
		   yhi1(i)=yhi(i+1)
		   ymid1(i)=ymid(i+1)
		   tc1(i)=tc(i+1)
		enddo
c	      print 138
c	      if(discprt) write(8,138)
c138		format(
c     &	' Shut level excluded: windows are now redefined thus')
c		do i=1,nval1
c		   print 453,i+1,ylo1(i),yhi1(i),tc1(i)*1.e3
c      	   if(discprt) write(8,453)i+1,ylo1(i),yhi1(i),tc1(i)*1.e3
cc453		   format(i4,3x,g13.6,' to ',g13.6,' pA: resolution (mus) = ',g13.6)
c		enddo
	   endif
	endif
c
	if(irep.gt.1) then
	   dispbst=.false.
	   goto 154
	endif
152	continue
	if(nfile.eq.1) then
	   ans='Y'
	   call DCASK('Display bursts',ans,ans)
	   dispbst=ans.eq.'Y'
	else
	   dispbst=.false.
	endif
c  insert popmenu to select which combos to be displayed
	if(dispbst) then
	   k=0
	   do ir=1,nval1		!have binom(ir,n) sequences for each cycle
		call BCOMB(ibcomb,nval1,ir)
		nc=ifixr(bcoeff(ir,nval1))
		do n=1,nc
		   k=k+1
		   strings(k)='Levels'
		   do i=1,ir
			i1=ibcomb(n,i)
			if(excshut) i1=i1+1
			call INTCONV(i1,cnum)
			i0=1+3*(i-1)
			i0=i0+7		!space for 'levels'
			strings(k)(i0:i0+2)=cnum(1:3)
		   enddo
		   strings(k)=strings(k)//' only'
		enddo
	   enddo
	   strings(k+1)='display All bursts'
	   nvals=k+1
	   iline1=nvals		!default
	   if(nvals.gt.25) then
		nrow=25
	   else
		nrow=nvals
	   endif
c====TEMP BIT TO KEEP POPMENU ON SCREEN!
c	   if(nvals.gt.25) then
c		i1=nvals-25+1
c		nvals=25
c	   else
c		i1=1
c	   endif
c====END OF TEMP BIT TO KEEP POPMENU ON SCREEN!
c
	   helps(1)=
     &   ' Which bursts are to be displayed? '
	   helps(2)=
     &   ' Choose the combination of amplitude'
	   helps(3)=
     &   '  ranges that you want to see'
	   nhelp=3
	   call BELL(2)
	   call MODE(18)
c	   call POPMENU(nxlo,nylo,nyhi,strings,nvals,ictm,icfm,icupm,
c	   call POPMENU(nxlo,nylo,nyhi,strings(i1),nvals,ictm,icfm,icupm,
c     &   ibkm,'Which bursts?',helps,nhelp,iline1,ans,ival)
c	subroutine POPAGE(ixlo,iylo,iyhi,strings,n,ic,icf,icup,ibk,
c     & title,helps,nhelp,iline,charout,ival,nrow)
	   call POPAGE(nxlo,nylo,nyhi,strings,nvals,ictm,icfm,icupm,
     &   ibkm,'Which bursts?',helps,nhelp,iline1,ans,ival,nrow)
	   if(iline1.eq.0) goto 152			!iline=0 for ESC=cancel
	   dispall=iline1.eq.nvals
c
c====TEMP BIT TO KEEP POPMENU ON SCREEN!
c===	   if(i1.gt.1) iline1=iline1+i1-1
c
	   call VIDEOMOD(3)
c repeat loop to identify sequence to be displayed (also defines ibcomb for
c use in checking each burst
	   k=0
	   do ir=1,nval1		!have binom(ir,n) sequences for each cycle
		call BCOMB(ibcomb,nval1,ir)
		nc=ifixr(bcoeff(ir,nval1))
		do n=1,nc
		   k=k+1
		   if(k.eq.iline1) then		!the chosen sequence
			ndsp=n
			irdsp=ir
			goto 153
		   endif
		enddo
	   enddo
153	   continue
	endif		!end of 'if dispbst'
c
	if(irep.eq.1) then
	   pause 'Now allocate levels in each burst (Caplock on to debug)'
	endif
154	continue	!skip burst display after first repeat
	if(allocated(blevel)) then
	   DEALLOCATE(blevel)
	endif
	ALLOCATE(blevel(nval,nbst))
	debon=caplock()
	nb1=0
	nb2=0
	do i=1,4
	   numtot(i)=0	!initialise totals
	enddo
c Allocate lines to be displayed
	if(dispbst) then
	      nline=0
	      if(nval.gt.0) then
		   nl=0
		   do i=1,nval
		     y1=abs(ylo(i))
		     y2=abs(yhi(i))
		     if(y1.gt.0.02) then
		       nl=nl+1
			 yline(nl)=ylo(i)
		     endif
		     if(y2.gt.0.02) then
		       nl=nl+1
			 yline(nl)=yhi(i)
		     endif
		   enddo
c            now remove duplicate entries
		   m=0
		   do i=1,nl-1
		      do k=i+1,nl
			   if(abs(yline(i)-yline(k)).lt.0.01) yline(i)=10000.
		      enddo
		   enddo
		   do i=1,nl
		      if(yline(i).lt.9999.) then
			   m=m+1
			   yline(m)=yline(i)
		      endif
		   enddo
		   nline=m
	      endif
	endif
c
	if(dispbst.and.(.not.consamdef)) then
	   cjdat=.false.
c Get consam/axon file name
	   call GETCONS(adcfil,noconsam,title,cdate1,adctim1,nsam,
     &    srate,cjdat,irecfst,newform,ioffset,calfac,ffilt,idest)
	   consamdef=.true.
	   call gsetcols(0)
	   call errswi(-1)
	   call brkswi(1)
	   call chaswi(1)
	   call grfmod (1)
	   call harcha
	   call papenq(xp,yp,ipap)
c Want to set graphboundary to correspond to data window, bottom of which
c is at iyd1=127 pixels, so need graph units which correspond to this
c	   ix=0
c	   call PIXGRA(ix,iyd1,xg,yg)
	   vxlo=0	! for VIEWPORT
	   vxhi=xp
	   vylo=0
c	   vylo=yg
	   vyhi=yp
c	   xlo=0		! for graphboundary
c	   xhi=xp
c	   ylo=0
c	   yhi=yp
	   xlo2=0		! for graphboundary
	   xhi2=xp		!call xlo2 etc for common (xlo exists already)
	   ylo2=0
	   yhi2=yp
c	   xlo=0.05*xp			! set display location on screen
c	   xhi=0.95*xp
c	   ylo=0.05*yp
c	   yhi=0.8*yp	!lower to leav room for dialog box
c	   call mode(18)
c	   icf=7		!frame colour for dialog box 1
c	   ict=11	!text colour for dialog box 1
	   if(.not.debug()) call MODE(18)
	else if(dispbst) then	!consamdef=true
	   if(.not.debug()) call MODE(18)
	endif		!end of 'if dispbst'
c
c Now loop for bursts
	do j=1,nbst
	   do i=1,nval
		blevel(i,j)=.false.
	   enddo
	   ibstart=ibindex(1,j)
	   ibend=ibindex(2,j)
	   if(debon) then
		print 122,j,ibstart,ibend
		if(discprt) write(8,122) j,ibstart,ibend
122		format(/,' Burst ',i5,': events ',i7,' to ',i7)
	   endif
	   good=.false.
	   do i=ibstart,ibend	!go through all intervals in the burst
		call SEQNUM(num,ylo1,yhi1,nval1,tint(i),ampl(i),
     &	    excdub,tc1,dubious(i),badint(i))
		if(num.gt.0) then
		   blevel(num,j)=.true.
		   good=.true.	!at least on valid amp in this burst
		else
		   n1=iabs(num)	!=1,2,3,4
		   numtot(n1)=numtot(n1)+1
		endif
		if(debon) then
		   print 121,tint(i),ampl(i),iprops(i),dubious(i),
     &		badint(i),num
		   if(discprt) then
			write(8,121)tint(i),ampl(i),iprops(i),
     &			dubious(i),badint(i),num
121		   format(2g13.6,i4,2x,L4,2X,L4,' num = ',i4)
		   endif
		endif
	   enddo
c Remove bursts that have ONLY a good shut period (eg all openings are
c below resolution but one or more shut times are above resolution
	   if(.not.excshut.and.oneshut.and.good) then
		do i=2,nval1
		   if(blevel(i,j)) goto 140	!jump out if one good open level found
		enddo
		blevel(1,j)=.false.	!set the shutl level false too
		good=.false.	!no good open levels
		nb2=nb2+1
	   endif
140	   continue
	   if(good) nb1=nb1+1
	   if(debon) then
		if(discprt) write(8,123) j,(blevel(i1,j),i1=1,nval1)
123		format(/,' Burst ',i5,': levels: ',10L4)
	   endif
c show 10 ms before and after each burst=idelt points
	   if(dispbst) then
		idelt=10.*sngl(1.d3/dfinter)
		i1=ibstart-2	!two transitions before start of burst
		i2=ibend+3		!two transitions after end of burst
		ib1=ibstart
		ib2=ibend+1
		if(i1.lt.1) i1=1
		if(i2.gt.nint) i2=nint
c         Must now convert i1,i2,ibstart, ibend, to index
c	    in consam=j1, j2, jb1,jb2
		iflast=1	!first time -the do indices in ascending order
		i1=index(i1)	!index in raw data, before resolution imposed
		j1=INDEXG(i1,
     &	 iflast,nfits,ifits,timsav,tint0,dfinter,nintt)
c for the transition that starts the burst, use indexgb version to get
c baseline
		ib1=index(ib1)	!index in raw data, before resolution imposed
		jb1=INDEXGB(ib1,ibase,
     &	 iflast,nfits,ifits,timsav,ibaselin,tint0,dfinter,nintt)
		ib2=index(ib2)	!index in raw data, before resolution imposed
		jb2=INDEXG(ib2,
     &	 iflast,nfits,ifits,timsav,tint0,dfinter,nintt)
		i2=index(i2)	!index in raw data, before resolution imposed
		j2=INDEXG(i2,
     &	 iflast,nfits,ifits,timsav,tint0,dfinter,nintt)
c
c show 10 ms before and after each burst=idelt points
		idelt=10.*sngl(1.d3/dfinter)
		jb1=jb1-idelt
		if(jb1.lt.j1) j1=jb1
		jb2=jb2+idelt
		if(jb2.gt.j2) j2=jb2
c
c		imode=1	!estimate baseline internally in VSAMP
		imode=0
		ybase=float(ibase)/sfac2	!sfac2 converts intermed units to pA
c		amp=-1.		!scale internally
		amp=abs(avamp)
		colseq=.false.
		reject=.false. 	!hide the reject option (for now)
c
		call INTCONV(j,cnum)
		line1='Burst '//charnb(cnum)//': levels'
		n=0
		do k1=1,nval1
		   if(blevel(k1,j)) then
			n=n+1
			call INTCONV(k1,cnum)
			line1=charnb(line1)//' '//charnb(cnum)
		   endif
		enddo
		if(n.eq.0) then
		   call INTCONV(j,cnum)
		   line1='Burst '//charnb(cnum)//
     &		': no openings to defined ranges'
		else if(n.lt.nval1) then
		   line1=charnb(line1)//' only'
		endif

		if(dispall)  goto 15	!display
		if(n.ne.irdsp) goto 16	!wrong number of levels -don't display
c    Now this burst has the right number of levels -are they the specified ones?
		nc=ifixr(bcoeff(irdsp,nval1))
		do i=1,irdsp
		   i1=ibcomb(ndsp,i)	!=the irdsp specified levels
		   if(.not.blevel(i1,j)) goto 16	!skip display if any level absent
		enddo
c
15		continue
		colseq=.false.	!do not colour the sequence on the lower trace
		reject=.false. 	!hide the reject option (for now)
		stop=.true.		!show the stop display option
		if(debon) call MODE(18) 	!into graphics for display only
		line2=''		!no second line for message
		line3=''		!no 3rd line for message
		call VSAMP(j1,j2,jb1,jb2,consamdef,adcfil,nline,yline,
     &	   ybase,amp,imode,colseq,j11,j22,line1,line2,line3,reject,
     &         stop,qfile,nsam,srate,irecfst,calfac,ioffset)
c     &	   ybase,amp,imode,colseq,j11,j22,'',reject)
		if(debon) call VIDEOMOD(3)
	   endif
	   if(dispbst) then
		if(stop) then
		   dispbst=.false.
		   call VIDEOMOD(3)
		endif
	   endif
16	   continue
	enddo		!end of j=1,nbst
	print 129,nb1,(numtot(i),i=1,4)
      if(discprt) write(8,129) nb1,(numtot(i),i=1,4)
129	format(
     & /,' Number of bursts with at least one valid amplitude = ',i8,/,
     & ' Number of amplitudes that are not used because:',/,
     & '  (1) event set as  unusable =              ',i8,/,
     & '  (2) event not in any specified range=     ',i8,/,
     & '  (3) opening with dubious amplitude=       ',i8,/,
     & '  (4) event shorter than Tcrit =            ',i8)
c	pause
c=	Only one option at present
c=	if(iopt.le.0) iopt=1
c=	print 124,iopt
c=124	format(/
c=     & ' (1) List bursts that contain ONLY certain levels',/,
c=     & ' (2) List bursts that contain specified direct transitions',/,
c=     & ' Option number [',i2,'] = ')
c=	call INPUTi(iopt)
c	if(iopt.eq.2) goto ***
c
c Look for those that have only one level (for each level)
c then for those that contain 2 diff levels (for all combos of 2)
c  For up to n=5 amplitude levels, define array ibcomb(n,ndim) which contains
c  n rows and ndim columns where ndim=max value of binom_coeff(ir,n)
c The first ir columns only are defined
c The first binom_coeff(ir,n) rows ONLY are defined
c See COMBS.MCD for examples
c For n up to 5, the max value of binom_coeff(ir,n) is 10 so
c ibcomb(10,5) is always big enough
c	do ir=1,nval1		!have binom(ir,n) sequences for each cycle
c	   call BCOMB(ibcomb,nval1,ir)
c	   nc=ifixr(bcoeff(ir,nval1))
c	   print 126,ir
c	   if(discprt) write(8,126) ir
c126	   format(' Combinations of ',i2,' levels')
c	   do n=1,nc
c		print 125,n,(ibcomb(n,i),i=1,ir)
c		if(discprt) write(8,125) n,(ibcomb(n,i),i=1,ir)
c125		format(' Ncomb =',i3,':  ',10i4)
c	   enddo
c	enddo
c Now analyse bursts for the combos just defined
c Use kfreq() to keep frequencies with which each pattern is observed
c kfreq(ir,jc) refers to pattern with ir levels, and jc=combination
c (=row of ibcomb) from those with ir levels
	nbad1=0	!no of bursts with no valid levels
	nbad2=0	!no of bursts >nval valid levels (impossible!)
	nbad3=0	!no of bursts with valid levels but no match found
	nb11=0		!number of bursts with only one open level found
	do i=1,10
	   do j=1,10
		kfreq(i,j)=0	!initialise kfreq
	   enddo
	enddo
	do j=1,nbst
	   ibstart=ibindex(1,j)
	   ibend=ibindex(2,j)
	   nlev=0
	   do i=1,nval1
		if(blevel(i,j)) nlev=nlev+1		!no of diff levels in jth burst
	   enddo
c nlev=number of valid levels in current burst -this restricts the possible
c combos that the burst matches to those with ir=nlev levels, so now find
c which of the nc combos of ir levels is matched.
c If amp ranges are not exhaustive (cover all observed amps) then, if we
c say  a burst contains levels 1,4 but not 2 then it MIGHT also contain other
c levels too, if these levels not covered by any of the defined ranges
	   if(nlev.lt.1) then
		nbad1=nbad1+1
		goto 130
	   else if(nlev.gt.nval1) then
		nbad2=nbad2+1
		goto 130
	   else
		if(ibstart.eq.ibend) then
		   nb11=nb11+1
		   if(excone) goto 130	!next burst
	 	endif
c
		ir=nlev
		call BCOMB(ibcomb,nval1,ir)
		nc=ifixr(bcoeff(ir,nval1))
		do n=1,nc	!go through each row of ibcomb
		   match=.true.
		   do i=1,ir	!check match for all levels in row i
			level=ibcomb(n,i)	!goes through each level that must be present
c==			if(excshut) level=level+1
			if(.not.blevel(level,j)) match=.false.
		   enddo
		   if(match) then
			kfreq(ir,n)=kfreq(ir,n) + 1
			goto 130	!jump out as soon as match found -> next burst
		   endif
		enddo
	   endif
	   nbad3=nbad3+1
130	   continue
	enddo
c  Print results
c====print warning as below
c If amp ranges are not exhaustive (cover all observed amps) then, if we
c say  a burst contains levels 1,4 but not 2 then it MIGHT also contain other
c levels too, if these levels not covered by any of the defined ranges
c Use kfreq() to keep frequencies with which each pattern is observed
c kfreq(ir,jc) refers to pattern with ir levels, and jc=combination
c (=row of ibcomb) from those with ir levels
c	nbad=no of bursts with no valid levels
c	nbad2=no of bursts >nval valid levels (impossible!)
c	nbad3=no of bursts with valid levels but no match found
	if(nbad2.ne.0) then
	   call BELL(1)
	   print 1341,nbad2
	   if(discprt) write(8,1341) nbad2
1341	   format(' ERROR: nbad2 = ',i4)
	   pause
	endif
	print 134,nb1,nb11,nb2,nbad1,nbad3
	if(discprt) write(8,134) nb1,nb11,nb2,nbad1,nbad3
134	format(/,
     &' Number of bursts with at least one valid amplitude = ',i6,/,
     &' Number of these that have only one opening = ',i6,/,
     &' Number of bursts with good shut level but no good openings = ',
     & i4,/,
     &' Number of bursts with zero valid levels = ',i6,/,
     &' Number of bursts with valid levels but no match = ',i4)
	den=float(nb1)	!for calc of percent
	if(excone) den=float(nb1-nb11)
c
	if(excshut) then
	   print 143
	   if(discprt) write(8,143)
143	   format(/,' Shut level is excluded')
	else
	   print 144
	   if(discprt) write(8,144)
144	   format(/,' Shut level is included')
	endif
	if(excone) then
	   print 141
	   if(discprt) write(8,141)
141	   format(' Bursts with one opening are excluded')
	else
	   print 142
	   if(discprt) write(8,142)
142	   format(' Bursts with one opening are included')
	endif
	print 132
	if(discprt) write(8,132)
132	format(/,
     &' Number of bursts that contain opening(s) to specified levels',
     &' ONLY',/,
     & ' (NB : could also contain levels that are not in ANY ',
     & 'specified amp range)',/,/,
     & ' Levels             number    percent')
	ksum=0
	psum=0.
	do ir=1,nval1		!have binom(ir,n) sequences for each cycle
	   call BCOMB(ibcomb,nval1,ir)
	   nc=ifixr(bcoeff(ir,nval1))
	   if(ir.gt.1) then
	      print 126,ir
	      if(discprt) write(8,126) ir
126	      format(' Combinations of ',i2,' levels')
	   endif
c To keep the field constant width for print of combos, need to convert them
c to ascii in string
	   do n=1,nc
c		print 133,(ibcomb(n,i),i=1,ir)
c		if(discprt) write(8,133) (ibcomb(n,i),i=1,ir)
c133		format(5i3)
		string='              '
		do i=1,ir
		   i1=ibcomb(n,i)
		   if(excshut) i1=i1+1
		   call INTCONV(i1,cnum)
		   i0=1+3*(i-1)
		   string(i0:i0+2)=cnum(1:3)
		enddo
		print 133,string
		if(discprt) write(8,133) string
133		format(1x,a15)
c
c Use kfreq() to keep frequencies with which each pattern is observed
c kfreq(ir,jc) refers to pattern with ir levels, and jc=combination
c (=row of ibcomb) from those with ir levels
		kf=kfreq(ir,n)
		p=100.*float(kf)/den
		ksum=ksum+kf
		psum=psum+p
		print 135,kf,p
		if(discprt) write(8,135) kf,p
135		format('& ',i5,3x,f9.2)
	   enddo
	enddo
	print 137,ksum,psum
	if(discprt) write(8,137) ksum,psum
137	format(/,' TOTALS',10x,i5,3x,f9.2)
	pause	             	!to see results
c Repeat to print all four possibilities without asking
	irep=irep+1
	if(irep.eq.2) then
	  excshut=.false.	!for first time
	  excone=.true.
	  goto 1201
	else if(irep.eq.3.and.oneshut) then
	  excshut=.true.	!for first time
	  excone=.false.
	  goto 1201
	else if(irep.eq.4.and.oneshut) then
	  excshut=.true.
	  excone=.true.
	  goto 1201
	endif
	ans='N'
	call DCASK('Repeat to view different bursts',ans,ans)
	if(ans.eq.'Y') then
	  excshut=.false.
	  excone=.false.
	  dispbst=.true.
	  goto 1521
	endif
c
	DEALLOCATE(blevel)
C
c Deallocate arrays before leaving
994	continue
	if(allocated(kcombs)) then
	   DEALLOCATE(kcombs,ifreq,intvals)	!no longer needed
	endif
	if(allocated(ibindex)) then
	   DEALLOCATE(ibindex,bampl)
	endif
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
	ans='Y'
	call INPUTa(ans)
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


	function BCOEFF(ir,n)
c To calculate binomial distribution (for small n) -result is real*4
c
	if(ir.eq.0.or.ir.eq.n) then
	   bcoeff=1.0
	else
	   m=n
	   do i=n-1,n-ir+1,-1
		m=m*i
	   enddo
	   m1=1
	   do i=2,ir
		m1=m1*i
	   enddo
	   bcoeff=float(m)/float(m1)
	endif
	RETURN
	end


	subroutine BCOMB(ibcomb,n,ir)
c  For up to n=5 amplitude levels, define array ibcomb(n,ndim) which contains
c  n rows and ndim columns where ndim=max value of binom_coeff(ir,n)
c The first ir columns only are defined
c The first binom_coeff(ir,n) rows ONLY are defined
c For n up to 5, the max value of binom_coeff(ir,n) is 10 so
c ibcomb(10,5) is always big enough
c
c ncr=binom(r,n)
	integer ibcomb(10,5)
c
	do i=1,10
	   do j=1,5
		ibcomb(i,j)=0
	   enddo
	enddo
c
	if(ir.eq.1.or.ir.eq.n) then
	 if(ir.eq.1) then
	   do i=1,n
		ibcomb(i,1)=i		!first col only defined
	   enddo

	 else if(ir.eq.n) then
	   do i=1,n
		ibcomb(1,i)=i		!first row only defined
	   enddo
	 endif
	 RETURN
	endif
c Now the more complicated cases (n=1, 2 included in above)
	if(n.eq.3) then
	   if(ir.eq.2) then
		ibcomb(1,1)=1
		ibcomb(1,2)=2
		ibcomb(2,1)=1
		ibcomb(2,2)=3
		ibcomb(3,1)=2
		ibcomb(3,2)=3
	   endif
	else if(n.eq.4) then
	   if(ir.eq.2) then
		ibcomb(1,1)=1
		ibcomb(1,2)=2
		ibcomb(2,1)=1
		ibcomb(2,2)=3
		ibcomb(3,1)=1
		ibcomb(3,2)=4
		ibcomb(4,1)=2
		ibcomb(4,2)=3
		ibcomb(5,1)=2
		ibcomb(5,2)=4
		ibcomb(6,1)=3
		ibcomb(6,2)=4
	   else if(ir.eq.3) then
		ibcomb(1,1)=1
		ibcomb(1,2)=2
		ibcomb(1,3)=3
		ibcomb(2,1)=1
		ibcomb(2,2)=2
		ibcomb(2,3)=4
		ibcomb(3,1)=1
		ibcomb(3,2)=3
		ibcomb(3,3)=4
		ibcomb(4,1)=2
		ibcomb(4,2)=3
		ibcomb(4,3)=4
	   endif
	else if(n.eq.5) then
	   if(ir.eq.2) then
		ibcomb(1,1)=1
		ibcomb(1,2)=2
		ibcomb(2,1)=1
		ibcomb(2,2)=3
		ibcomb(3,1)=1
		ibcomb(3,2)=4
		ibcomb(4,1)=1
		ibcomb(4,2)=5
		ibcomb(5,1)=2
		ibcomb(5,2)=3
		ibcomb(6,1)=2
		ibcomb(6,2)=4
		ibcomb(7,1)=2
		ibcomb(7,2)=5
		ibcomb(8,1)=3
		ibcomb(8,2)=4
		ibcomb(9,1)=3
		ibcomb(9,2)=5
		ibcomb(10,1)=4
		ibcomb(10,2)=5
	   else if(ir.eq.3) then
		ibcomb(1,1)=1
		ibcomb(1,2)=2
		ibcomb(1,3)=3
		ibcomb(2,1)=1
		ibcomb(2,2)=2
		ibcomb(2,3)=4
		ibcomb(3,1)=1
		ibcomb(3,2)=2
		ibcomb(3,3)=5
		ibcomb(4,1)=1
		ibcomb(4,2)=3
		ibcomb(4,3)=4
		ibcomb(5,1)=1
		ibcomb(5,2)=3
		ibcomb(5,3)=5
		ibcomb(6,1)=1
		ibcomb(6,2)=4
		ibcomb(6,3)=5
		ibcomb(7,1)=2
		ibcomb(7,2)=3
		ibcomb(7,3)=4
		ibcomb(8,1)=2
		ibcomb(8,2)=3
		ibcomb(8,3)=5
		ibcomb(9,1)=2
		ibcomb(9,2)=4
		ibcomb(9,3)=5
		ibcomb(10,1)=3
		ibcomb(10,2)=4
		ibcomb(10,3)=5
	   else if(ir.eq.4) then
		ibcomb(1,1)=1
		ibcomb(1,2)=2
		ibcomb(1,3)=3
		ibcomb(1,4)=4
		ibcomb(2,1)=1
		ibcomb(2,2)=2
		ibcomb(2,3)=3
		ibcomb(2,4)=5
		ibcomb(3,1)=1
		ibcomb(3,2)=2
		ibcomb(3,3)=4
		ibcomb(3,4)=5
		ibcomb(4,1)=1
		ibcomb(4,2)=3
		ibcomb(4,3)=4
		ibcomb(4,4)=5
		ibcomb(5,1)=2
		ibcomb(5,2)=3
		ibcomb(5,3)=4
		ibcomb(5,4)=5
	   else
		print 1,n
1		format('n = ',i4,' is INVALID')
	   endif
	endif
c
	RETURN
	end


