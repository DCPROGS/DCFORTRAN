	subroutine HJCDISP(iplot,tint,nint,burst,rootA,rootF,theta,
     & tres,tcrit,QT,nlig,ylod,yhid,nvald,readin,kfit,k,km)
c To define arrays for display of final fit in HJCFIT
c (at present, exclusion of 'dubious amplitudes', and specification
c of amplitudes for open periods, are not included. Also no Yval defined for
c iplot=4 since correlation calc not done here)
c Converted for Lahey V5.n 07/09/93 02:57pm (version known as HJCDISP1 for
c old compiler).  Also VHIST2 now used.
c
c iplot=
c (1) Plot histogram of apparent OPEN periods, with fit
c (2) Plot histogram of apparent SHUT times, with fit
c (3) Open time pdf conditional on adjacent gap, with fit
c (4) Mean open vs adjacent gap plot, with fit
c
c   Modified 10/10/92 09:00pm for multiple data sets.  To do plots for a
c specified data set (#iset, say), call HJCLIK to do the calculations
c for this set only by setting ONESET=true (previously relied on last call
c to HJCLIK to provide calcs). Note that some of the calculations needed
c for plots are done in HJCLIK only when idebug=8, as used for print of
c the final pdfs.  Modified now so that these calcs are done also if
c ONESET=true (but pdfs not printed again)
c   Modified 02/27/92 03:58pm so that 'mean open for openings adjacent to
c specified gaps' are now determined in same run through data as for the
c open and shut times.  The method differs from before in that (1) open
c periods rather than indiv openings are used (as assumed in HJC),
c (2) when burst=true any burst with bad opening or shutting is rejected
c entirely so this does not contribute to 'mean open adj to gap' either
c (3) now keep 3 different versions (a) openings vs preceding gap (sy0,ny0)
c (b) openings vs following gap (sy1,ny1) and (c) openings that are adj
c to a gap in spec range whether it is before OR after the opening (ie
c opening counted twice, once according to the range in which prev
c gap falls, and once according to range in which following gap falls)
c
c
	integer*2 IAMPL(20480,10),iaval
	real TINT(20480,10)		!up to 20 *1024 for each conc
	integer nint(10)
	real*4 concA(10),concB(10)		!to hold concentrations for each expt
	integer*2 nopen(20480)	!number of openings per group
	real*4 tval(20480)	!like Yval in EKDIST; for open,shut etc
	real*8 QT(10,10),QD(10,10)
	real*4 tcrit(10)
	logical burst(10)
	real*4 ylo(20),yhi(20)
	real*4 ylod(20),yhid(20)	!for defaults
	real*4 den1(20)
	real*4 sy0(20),syy0(20),sy1(20),syy1(20),sy2(20),syy2(20)
	real*4 sx0(20),sxx0(20),sx1(20),sxx1(20),sx2(20),sxx2(20)
	integer ny0(20),ny1(20),ny2(20),nx0(20),nx1(20),nx2(20)
	real*4 FREQ(510),XAXIS(510),xsav(5,4)
	integer IX(10),JX(10),IL(10)		!for common/cpar/
	character*1 ans,ans1,UC
	logical pon,slock,debug,caplock,logt,deb,prt,readin,oneset
c	logical exass
	logical fitted,open,good,bad,bad1,nores,scale,setmax
c	logical first
	logical sbin,shist,sres,sexp
	logical corprev,cornext		!true only for iplot=3
	logical discprt
	character title*74
c for PHIo1,pdfopen etc (zero resolution calcs)
	real*8 Peq1(10),phio(1,10)
	real*4 tau1(10),area(10),am,sd
c==from scvd1
	character*40 titlex,titley
c==from SCVDU
c	real XVAL(0:511,ndimd),YVAL(0:511,ndimd)
c	real XCAL(512,ndimc),YCAL(512,ndimc)
cc for histo data
c	integer ndat(ndimd),icurvd(ndimd),ijoin(ndimd)
cc for calc curves
c	integer ncal(ndimc),icurvc(ndimc),iline(ndimc)
	real XVAL(0:511,1),YVAL(0:511,1)
	real*4 XCAL(512,10),YCAL(512,10)
c for histo data
	integer ndat(1),icurvd(1),ijoin(1)
c for calc curves
	integer ncal(10),icurvc(10),iline(10)
c
	real theta(20)
c	integer*2 videotyp
	character*44 title1
	logical landscap,autplt,draft,doframe,interp
C==FROM HJCVDU
	real*8 g00A(10),g10A(10),g11A(10),g00F(10),g10F(10),g11F(10)
	real*8 g00(10),g10(10),g11(10)
	real*8 rootA(10),rootF(10),ampA(10),ampF(10)
	real*8 eigen(10)
	real*8 F0HJC,F1HJC		!functions
	real*8 time,tres,FTCT,tcrit2,ptc,ucol(10,1)
	real*4 amp(10),tau(10)
c for display when iplot=4 use new VPLOT2
	dimension xcal1(2048,3),ycal1(2048,3)
	dimension XVAL1(2048,3),YVAL1(2048,3)
c for data
	integer ndat1(3),icurvd1(3),ijoin1(3),isym(3)
	real*4 syms(3)
c for calc curves
	integer ncal1(3),icurvc1(3),iline1(3)
c
	common/KBLK/kA,kB,kC,kD
	COMMON/AMPBLK/iampl
	common/setblk/oneset,iset	!for HJCLIK,DISP to specify one set
	common/CBLK/nset,concA,concB,jsetlast		!for HJCLIK
	COMMON/CPAR/NCDEP,IX,JX,IL,X
	common/dp/discprt
	COMMON/RBLCK/treso,tresg,iacrit,mavamp	!for resint
	common/sblk/sbin,shist,sres,sexp
	common/deb/idebug
	COMMON/SD/weight(100,10)  !set isdev=-1 in call if not needed: for VPLOT2
c==	common/nblk/ngp(10),an(10),nscal(10),first    !ngp(j) etc for set j
	common/fitblk/eigen,g00A,g10A,g11A,g00F,g10F,g11F		!from HJCLIK
	common/fitblk1/ampA,ampF		!from HJCASYMP
c
c
	pon()=slock()
	debug()=caplock()
c
	kF=kB+kC+kD
	if(k.ne.kA+kF) then
	   call BELL(2)
	   print 43,k,kA,kF
43	   format(' ERROR: k,kA,kF = ',3i4,' in HJCDISP')
	   STOP
	endif

	do 9 i=1,10
9	ucol(i,1)=1.d0
	tres1=sngl(1.d3*tres)	!tres in msec
	ans1='Y'
	interp=.false.
c
	ONESET=.true.	!so HJCLIK calcs pdf for set #iset ONLY
	iset=1
226	continue		!return for next plot
	corprev=.false.	!correl'n dep on prev gap (used for iplot=3)
	cornext=.false.	!correl'n dep on next gap (used for iplot=3)
	print 227,iplot
227	format(/,
     & ' (1) Plot histogram of apparent OPEN periods, with fit',/,
     & ' (2) Plot histogram of apparent SHUT times, with fit',/,
     & ' (3) Open time pdf conditional on adjacent gap',/,
     & ' (4) Mean open vs adjacent gap plot, with fit',/,
     & ' (5) No plots -exit now',/,
     & ' Option number [',i2,'] = ')
	read 102,i
	if(i.ge.1.and.i.le.5) iplot=i
	if(iplot.eq.5)  goto 999
c Plots to be done, so specify which data set (if nset>1) and recalc values
	if(nset.gt.1) then
	   print 63
63	   format(' set     concentration')
	   do 64 j=1,nset
64	   print 65,j,concA(j)*1.e6
65	   format(i4,3x,g13.6)
	   print 62,nset,iset
62	   format(' Data set to be plotted (1 - ',i3,') [',i3,'] = ')
	   read 102,j
	   if(j.ge.1.and.j.le.nset) iset=j
         print 67,iset,concA(iset)*1.e6
         if(pon()) write(7,67) iset,concA(iset)*1.e6
         if(discprt) write(8,67) iset,concA(iset)*1.e6
67       format(/,
     & ' Plots for set number ',i3,': concentration = ',g13.6,/,
     & ' ------------------------------------------------------------')
	endif
	xA0=concA(iset)
	xB0=concB(iset)
	print 671,iset
671	format(' Calculating values for data set #',i3,' . . . . ')
c Make QD for the specified concentration(s)
	call QNEWC(QT,1.0,1.0,QD,xA0,xB0,ncdep,nlig,IL,IX,JX,k,km)
c Call HJCLIK again (with oneset=true) to calc, for current set (#iset) only,
c the values of eigen,g00A,g10A,g11A,g00F,g10F,g11F, and (in HJCASYMP) values
c of ampA and ampF.
	sm=HJCLIK(kfit,THETA)
c
	if(iplot.eq.3) then
	   nval=1
	   itype=1		!default
	   print 36
36	   format(
     &   ' (1) Show open times conditional on PRECEDING shut time',/,
     &   ' (2) Show open times conditional on FOLLOWING shut time',/,
     &   ' (3) Show open times conditional on EITHER shut time',/,
     &   ' Option number [1] = ')
	   read 102,i
	   if(i.ge.1.and.i.le.3) itype=i
	   if(itype.eq.1.or.itype.eq.3) corprev=.true. !correl'n dep on prev gap
	   if(itype.eq.2.or.itype.eq.3) cornext=.true. !correl'n dep on next gap
	   if(itype.eq.1) then
	     print 331
	     if(pon()) write(7,331)
	     if(discprt) write(8,331)
331	     format(/,
     & ' Display pdf of durations of apparent openings that are',/,
     & '  PRECEDED by gaps with durations in following range (ms)',/)
	   else if(itype.eq.2) then
	     print 332
	     if(pon()) write(7,332)
	     if(discprt) write(8,332)
332	     format(/,
     & ' Display pdf of durations of apparent openings that are',/,
     & '  FOLLOWED by gaps with durations in following range (ms)',/)
	   else if(itype.eq.3) then
	     print 333
	     if(pon()) write(7,333)
	     if(discprt) write(8,333)
333	     format(/,
     & ' Display pdf of durations of apparent openings that are',/,
     & '  ADJACENT to gaps with durations in following range (ms)',/)
	   endif
	endif
c
	if(iplot.eq.4) then
	   if(readin) then
		nval=nvald
		print 53,nval
53		format(' Default = ',i3,' gap ranges:')
		do 54 i=1,nval
		ylo(i)=ylod(i)
		yhi(i)=yhid(i)
54		print 55,i,ylo(i),yhi(i)
55	      format(i4,1x,f10.3,' to ',f10.3)
	      print 56
56	      format(
     &' Number of gap length ranges to be used (<= 20) [default] = ')
	      read 102,nval1
		if(nval1.ge.1) nval=nval1
	   else
		nval1=1		!so doesn't think default wanted
57	      print 420
420	      format(
     &' Number of gap length ranges to be used (<= 20) = ')
	      read 102,nval
		if(nval.lt.1) goto 57
	   endif
	   print 2571
	   if(pon()) write(7,2571)
	   if(discprt) write(8,2571)
2571	   FORMAT(/,
     & ' Mean durations of individual apparent openings that are',/,
     & '  adjacent to gaps with durations in following ranges (ms)',/)
	endif
	if(iplot.eq.3.or.iplot.eq.4) then
	   do 423 i=1,nval
	   sy0(i)=0.		!initialise
	   syy0(i)=0.
	   ny0(i)=0
	   sy1(i)=0.
	   syy1(i)=0.
	   ny1(i)=0
	   sy2(i)=0.
	   syy2(i)=0.
	   ny2(i)=0
	   sx0(i)=0.		!initialise
	   sxx0(i)=0.
	   nx0(i)=0
	   sx1(i)=0.
	   sxx1(i)=0.
	   nx1(i)=0
	   sx2(i)=0.
	   sxx2(i)=0.
	   nx2(i)=0
	   if(iplot.eq.4.and.nval1.eq.0) goto 741	!use default
	   if(iplot.eq.4.and.burst(iset).and.i.eq.nval) then
	      print 419,i,tcrit(iset)
		if(discprt) write(8,419) i,tcrit(iset)
419	      format(
     &   '&For range #',i3,' gaps of tcrit =',f10.2,' and greater used')
		ylo(i)=tcrit(iset)
		yhi(i)=3.1536e10		!msec=1 year! -see PRANGE
	   else
35	      print 421,i,i
421	      format('&(',i2,
     &   ') Gap times (ms) for range',i2,': low time, high time = ')
	      call INPUT2r(ylo(i),yhi(i))
	      if(ylo(i).gt.yhi(i)) then
	        a1=ylo(i)
	        ylo(i)=yhi(i)
	        yhi(i)=a1
	      endif
		if(ylo(i).lt.tres1) then
		   call BELL(1)
		   print 34,tres1
34		   format(
     & '&Lower limit must not be less than resolution = ',g13.6,' ms',/)
		   goto 35
		endif
		if(iplot.eq.3.and.burst(iset).and.ylo(1).gt.tcrit(iset))then
		   call BELL(1)
		   print 341,tcrit(iset)
341		   format(
     & '&Longest allowable range is tcrit =',f10.2,
     & ' and greater: use this [Y] ? ')
		   read 101,ans
		   if(UC(ans).eq.'N') then
		      goto 35
		   else
			ylo(1)=tcrit(iset)
			yhi(1)=3.1536e10		!msec=1 year! -see PRANGE
		   endif
		endif
	   endif
741	   continue
c	   if(pon()) write(7,453) i,ylo(i),yhi(i)
c	   if(discprt) write(8,453) i,ylo(i),yhi(i)
c453	   format(i4,3x,g13.6,' to ',g13.6)
423	   continue	!end of nval loop
	  deb=debug()
	  prt=pon()
	endif		!gap range(s) set for iplot=3,4
c Keep default ranges for iplot=4
	if(iplot.eq.4) then
	   do 58 i=1,20
		ylod(i)=ylo(i)
		yhid(i)=yhi(i)
		nvald=nval
58	   continue
	endif
c
c For open and shut times, go through the observations exactly as in
c HJCLIK (when 'burst' used, some openings, and all shuttings>tcrit, will
c be excluded
	in=1		!counter for intervals
	ng=0			!to count number of groups
	nopen(1)=0		!number of openings in group #ng
	j=0			!index in tval()
	islast=0		!index in tint() of last shut period
c Start at first (good) opening
	call FINDOPEN(in,iset,iop,ibad)
	if(ibad.eq.1) goto 991		!no good openings!
	in=iop			!make the opening the current obs
c
90	continue		!return here for next opening
c	deb=debug()
c	deb=.true.
c	deb=.false.
c GET LENGTH OF OPEN PERIOD-look forward to see if there are more openings
c IAVAL is in main prog file (NB returns int*2 value, unlike version in EKDIST)
	top=0.	!to accum length of open period
	i1=in
	iofst=-1	!records index in tint() of 1st opening in the open period
	do 51 jn=i1,nint(iset)
	   open=IAVAL(jn,iset).ne.0
	   good=tint(jn,iset).gt.0.0
	   if(open) then
		if(good) then
		   top=top+tint(jn,iset)
		   if(iofst.eq.-1) iofst=jn !index in tint of 1st opening in open period
		   iolast=jn		!index in tint of last opening in open period
		else		!bad opening, so end group
		   in=jn
		   if(burst(iset)) then
			goto 93	!abandon whole burst & look for next
		   else
			goto 91	!end group at end of prev shutting
		   endif
		endif
	   else
		inext=jn	!index of shutting that ends open period
		goto 52	!shut, so jump out
	   endif
51	continue
c Get here if last opening reached (and it is good), but in this case
c a gap is not found and inext not updated. Therefore set inext=nint+1 to
c signal (below) that end reached
	inext=nint(iset)+1
c
52	continue	!shutting found (index=inext), so jumped out
	topen=top
c Now have an open period, length=topen
c At this point the length of the last good shutting will still be in tshut
c (except for 1st open period, for which islast=0 still) so can be used for
c iplot=4 as long as it was adjacent to the opening just found (eg haven't
c skipped a bad burst in between), which will be so if iofst=islast+1
	if((corprev.or.iplot.eq.4).and.
     &    islast.gt.0.and.iofst.eq.islast+1) then
	   call GETRANGE(tshut,topen,ylo,yhi,nval,sy0,syy0,ny0,
     &    sy2,syy2,ny2,sx0,sxx0,nx0,sx2,sxx2,nx2,deb,prt)
	   if(tshut.ge.ylo(1).and.tshut.lt.yhi(1)) then		!in range for pdf
		j=j+1
		tval(j)=topen		!for iplot=3 (prev gap, or both)
	   endif
	endif
c Record values for pdf of all openings (iplot=1)
	if(iplot.eq.1) then
	   j=j+1
	   tval(j)=topen		!in msec
	endif
c
c   Count number of openings in current group (ng not updated until group ends)
	nopen(ng+1)=nopen(ng+1) + 1
c
c NOW GET THE NEXT SHUT TIME (or end the group)
c At present in=index of 1st opening in open period
c If last interval reached, and it is open, then no more gaps, and inext was
c set to nint+1, above, so end the last group at this opening
	if(inext.gt.nint(iset)) goto 92
	in=inext		!should be shut -check, for debug anyway!
	open=iaval(in,iset).ne.0
	if(open) then
	   call BELL(2)
	   print 61,in
61	   format(' INTERVAL # ',i5,' should be shut')
	endif
c Check for 2 adjacent gaps, or bad gap
	bad=tint(in,iset).lt.0.0		!gap marked bad
	if(in.lt.nint(iset)) then
	   bad1=iaval(in+1,iset).eq.0    !also bad if next interval is shut too
	endif
	if(bad.or.bad1) then
	   if(burst(iset)) then
		goto 93	!abandon whole burst & look for next
	   else
		goto 92		!end present group with prev opening
	   endif
	endif
	tshut=tint(in,iset)
	if(tshut.gt.tcrit(iset)) then
c      End the group; first collect openings followed by gaps>tcrit
c	 for correlation,in burst case
	   if((cornext.or.iplot.eq.4).and.burst(iset)
     &   .and.in.eq.iolast+1) then
		call GETRANGE(tshut,topen,ylo,yhi,nval,sy1,syy1,ny1,
     &       sy2,syy2,ny2,sx1,sxx1,nx1,sx2,sxx2,nx2,deb,prt)
	      if(tshut.ge.ylo(1).and.tshut.lt.yhi(1)) then		!in range for pdf
		   j=j+1
		   tval(j)=topen		!for iplot=3 (following gap, or both)
	      endif
	   endif
	   islast=in	!index in tint() of last shut period
	   goto 92	!end present group with prev opening
	endif
c
c Now have a good shut time, in tshut say
c At this point the length of the last good open period will still be in topen
c so it can be used for iplot=3,4 as long as the last opening in this open
c period was adjacent to the gap just found (eg haven't skipped a bad burst in
c between), which will be so if islast=iolast+1. In this case can accum sy1 etc
c which have data for 'following gap'
	tshut=tint(in,iset)
	islast=in	!index in tint() of last shut period
	if((cornext.or.iplot.eq.4).and.
     &	islast.eq.iolast+1) then
	   call GETRANGE(tshut,topen,ylo,yhi,nval,sy1,syy1,ny1,
     &    sy2,syy2,ny2,sx1,sxx1,nx1,sx2,sxx2,nx2,deb,prt)
	   if(tshut.ge.ylo(1).and.tshut.lt.yhi(1)) then		!in range for pdf
		j=j+1
		tval(j)=topen		!for iplot=3 (following gap, or both)
	   endif
	endif
	if(iplot.eq.2) then
	   j=j+1
	   tval(j)=tshut		!in msec
	endif
c	if(deb) then
c	if(ng.ge.63.and.ng.le.66) then
c	   print 8051,ts
c8051	   format(' tshut (ms) = ',g13.6,' new value = ')
c	   call INPUTr(t)
c	   if(t.gt.0.05) ts=t
c	endif
c
c End of shutting.  Next interval should be an opening
	in=in+1
	if(in.gt.nint(iset)) goto 91		!end of data -group ends with shutting
	open=iaval(in,iset).ne.0
	if(.not.open) then
	   call BELL(2)
	   print 611,in
611	   format(' INTERVAL # ',i5,' should be open')
	endif
	goto 90		!get next open period -continue with group
c
c END OF GROUP (#ng) when group ends with a shutting (either because a bad
c opening is found, or because last interval in the data is a (good) shutting
c (neither of these should happen with real data!)
91	continue
	if(burst(iset)) then
	   call BELL(3)
         print 911,ng+1,nopen(ng+1)		!ng not updated yet
         if(discprt) write(8,911) ng+1,nopen(ng+1)
911	   format(
     & ' ERROR: burst cannot end with shut time: Group ',i5,
     &	' nopen = ',i6)
	endif
	if(nopen(ng+1).eq.0) goto 73	!skip this group -look for next
	ng=ng+1		!update number of groups now #ng has ended
c	if(deb) then
	if(idebug.eq.4.or.idebug.eq.5.or.idebug.eq.6) then
	   print 799,ng,nopen(ng),inext,iaval(inext,iset),tint(inext,iset)
	   if(discprt) write(8,799) ng,nopen(ng),
     &	inext,iaval(inext,iset),tint(inext,iset)
799	   format(/,
     & ' GROUP # ',i6,': ',i3,' openings',/,
     & '  index, amp, length = ',2i8,g13.6,' Last interval=shut',/)
	endif
c Now find next good opening and start new group (unless end of data reached)
	if(in.ge.nint(iset)) goto 991
73	nopen(ng+1)=0	!initialise number of openings in next group
c must first find a gap that precedes next good opening
	call FINDGAP(in,iset,is,ibad)
	if(ibad.ne.0) goto 991
	in=is		!index of the gap
	call FINDOPEN(in,iset,iop,ibad)	!now look for good opening
	if(ibad.ne.0) goto 991
	in=iop			!index of the opening
	goto 90			!start new group with the next good opening
c
c
c END OF GROUP (#ng) when group ends with an OPENING (normal way)
92	continue
	if(nopen(ng+1).eq.0) goto 74	!skip this group -look for next
	ng=ng+1		!update number of groups now #ng has ended
c	if(deb) then
	if(idebug.eq.4.or.idebug.eq.5.or.idebug.eq.6) then
	   print 800,ng,nopen(ng),inext,iaval(inext,iset),tint(inext,iset)
	   if(discprt) write(8,800) ng,nopen(ng),
     &	inext,iaval(inext,iset),tint(inext,iset)
800	   format(/,
     & ' GROUP # ',i6,': ',i3,' openings',/,
     & '  i, amp(i), length of gap that ends group= ',2i8,g13.6,/)
	endif
c Now find next good opening and start new group (unless end of data reached)
	if(in.ge.nint(iset)) goto 991
74	nopen(ng+1)=0	!initialise number of openings in next group
c Interval #in should be shut at this point, so now find next good opening
c to start a new group
	call FINDOPEN(in,iset,iop,ibad)	!now look for good opening
	if(ibad.ne.0) goto 991
	in=iop			!index of the opening
522	continue
	goto 90			!start new group with the next good opening
c
c ABANDON WHOLE BURST and look for next (without incrementing ng) if bad
c interval is found within a burst
93	continue
	if(.not.burst(iset)) then		!should be!!
	   call BELL(3)
	   print 932
932	   format(' ERROR: should get here only for ''bursts''')
	endif
c Look for next burst: find a good gap>tcrit
933	call FINDGAP(in,iset,igap,ibad)
	if(ibad.ne.0) goto 991
	if(tint(igap,iset).lt.tcrit(iset)) then   !look again
	   in=igap+1
	   goto 933
	endif
c Gap #igap is > tcrit- get next good opening after it
	in=igap		!index of the gap
	call FINDOPEN(in,iset,iop,ibad)	!now look for good opening
	if(ibad.ne.0) goto 991
	in=iop			!index of the opening
c reset initial vector
	nopen(ng+1)=0	!reinitialise number of openings in next group
c	if(first) then
c       print 934,ng+1,in
c       if(discprt) write(8,934) ng+1,in
c934	 format(' Burst #',i5,' abandoned: restart at opening #',i6)
c	endif
c
	goto 90			!start new group with the next good opening
c
991	continue		!end if definition of open and shut times
	nyval=j
	if(corprev.or.iplot.eq.4) then
c	  Print means and define xval1,yval1 for plotting (weight is in common)
	   title='Mean open time conditional on PRECEDING shut time'
	   call PRANGE(title,ylo,yhi,nval,sy0,syy0,ny0,sx0,sxx0,nx0,
     &	prt,yval1,xval1,1)
	endif
	if(cornext.or.iplot.eq.4) then
	   title='Mean open time conditional on FOLLOWING shut time'
	   call PRANGE(title,ylo,yhi,nval,sy1,syy1,ny1,sx1,sxx1,nx1,
     &	prt,yval1,xval1,2)
	endif
	if((corprev.and.cornext).or.iplot.eq.4) then
	   title='Mean open time conditional on EITHER shut time'
	   call PRANGE(title,ylo,yhi,nval,sy2,syy2,ny2,sx2,sxx2,nx2,
     &	prt,yval1,xval1,3)
	endif
	if(iplot.eq.4) goto 4062	!no bins etc -just plot with vplot2
c
c
c END OF DATA REACHED -tval defined
c
99	continue
	ibad=0
	if(ng.eq.0) then
	   call BELL(5)
	   print 992
992	   format(' NO GROUPS FOUND')
	endif
c
c
c Now set bin widths (based on SETBIN.FOR)
	if(.not.sbin) NBIN=-1		!TO SIGNAL THAT NOT YET SET
	if(nyval.eq.0) then
	  print 4132
4132	  format(' NO VALUES FOR DISTRIBUTION')
	  call BELL(4)
	  STOP
	endif
c
c For pdfs, variable is in X axis so calc as xmin,xmax
	call VARV1(tval,Nyval,xbar,varx,xmin,xmax)
	print 413,NYVAL,xbar,sqrt(varx),xmin,xmax
	if(pon()) write(7,413) NYVAL,xbar,sqrt(varx),xmin,xmax
	if(discprt) write(8,413) NYVAL,xbar,sqrt(varx),xmin,xmax
413	FORMAT(/,' Mean and SD of ',i5,' values= ',G13.6,' +/-',g13.6,
     & /,' Range from ',g13.6,' to ',g13.6)
	xmin=0.0		!reset for histogram
	if(debug()) then
	  print 700,1,xmin,xmax,ncalc
700	  format(' Pos#',i2,': xmin,xmax,ncalc=',2g13.6,i8)
	  pause
	endif
c
c NOW GET X AXIS
	setmax=.false.	!xmax set automatically, not manually
	if(.not.sbin) logt=.false.
	if(sbin) goto 303	!same bins so XAXIS,XWBASE as before
c	iopt=2
c	if(iplot.eq.2) iopt=1	!default for shut times
	iopt=1
	print 2092,iopt
2092	format(
     & ' (1) Distribution of log durations',/,
     & ' (2) Distribution of durations- set bins manually',/,
     & ' (3) Distribution of durations- 20 bins',/,
     & ' (4) Distribution of durations- 40 bins',/,
     & ' (5) Distribution of durations- 60 bins',/,
     & ' (6) Skip histogram',/,
     & ' Option number [',i2,'] = ')
c     & '',/,
	read 102,i
	if(i.ge.1.and.i.le.6) iopt=i
	if(iopt.eq.6) then
	   nbin=0
	   RETURN		!skip histo
	endif
	logt=iopt.eq.1
501	nbw=1
	if(logt.or.iopt.ge.3) goto 2093
503	print 208
208	format(
     & ' Number of different bin widths (-1 to skip histo) [1]= ')
	read 102,nbw
	if(nbw.eq.0) nbw=1
	if(nbw.le.-1) then
	   nbin=0
	   RETURN		!skip histo
	endif
2093	continue
	xaxis(1)=sngl(tres*1.d3)	!in msec
	if(iopt.ge.3) goto 291	!auto bins
	print 2091,xaxis(1)
2091	format('&Histogram to start at ',f8.5,' ms.  O.K. [Y] ? ')
	read 101,ans
	if(UC(ans).ne.'N') goto 291
28	print 209
209	format('&Start histogram at x (ms, pA etc) [0.0] = ')
	call INPUTr(xaxis(1))
c	read 103,xaxis(1)
c103	format(g13.6)
c
291	continue
c
C   GET BIN WIDTHS
c Calculate bin boundaries in XAXIS
	m=1		!counts X axis entries
303	continue
c Sep bit for dist of log(t)- have NBW bins starting at xaxis(1), up to xend
c	if(.not.logt) goto 308
	if(logt) then
	   if(sbin) then
	     dx=exp(alog(10.)/float(nbdec))
	     goto 731
	   endif
	   if(xaxis(1).le.0.) goto 2093
	   if(nyval.le.500) nbdec=8				!default
	   if(nyval.gt.500.and.nyval.le.1000) nbdec=10	!default
	   if(nyval.gt.1000) nbdec=12				!default
1048	   continue
	   print 1041,nbdec
1041	   format(' Number of bins/decade [',i3,'] = ')
	   read 102,i
	   if(i.gt.0) nbdec=i
	   dx=exp(alog(10.)/float(nbdec))
	   xend=1. + xmax - amod(xmax,1.)
	   print 1046,xend
1046	   format('&Last x value (ms)= ',g13.6,'  O.K. [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
	      print 72
72	      format('&Last x value (ms)= ')
	      call INPUTr(xend)
		setmax=.true.	!xmax set manually
	   endif
	   nbin=1+ifix(alog(xend/xaxis(1))/alog(dx))
	   xend=(dx**nbin)*xaxis(1)
	   print 1047,nbin,xend
1047	   format(
     &   '&',1x,i5,' bins; last x reset to ',g13.6,': O.K. [Y] ? ')
	   read 101,ans
	   if(setmax) xmax=xend
	   if(UC(ans).eq.'N') goto 1048
	   do 1043 i=1,nbin
1043	    xaxis(i+1)=xaxis(1)*(dx**i)
	    mlast=nbin+1
731	    xwbase=alog10(dx)	  !in log10 units to scale fit
	    print 1045,nbin,dx
	    if(pon()) write(7,1045) nbin,dx
	    if(discprt) write(8,1045) nbin,dx
1045	    format(
     & ' Distribution of log(t) displayed- ',i6,' bins, factor= ',g11.4)
	else if(.not.logt) then
c Section done when not logt
	   do 210 i=1,nbw
	   if(sbin) goto 306
	   if(iopt.ge.3) then
c auto-binwidth to give 20,40,60 bins (nbw=1, not logt)
		nbin=20
		if(iopt.eq.4) nbin=40
		if(iopt.eq.5) nbin=60
		dx=(xmax-xmin)/float(nbin)
		call SETTIC(dx)
		mlast=1
	   else
		setmax=.true.		!xmax set manually
	   	mLAST=m
212	   	print 104,I
104	   	format(' (',I2,') Bin width, Last x value (ms, pA etc)= ')
	   	call INPUT2r(dx,xend)
32	   	xn=(xend-xaxis(mlast))/dx
C ASK AGAIN IF NOT AN INTEGRAL NO OF BINS BET X START AND XEND
c- also check that not more than 101 bins
	   	if(abs(xn-float(IFIX(0.5+xn))).lt.dx*.01) goto 30	!OK
	   	xend=xaxis(mlast) + float(ifixr(xn))*dx
	   	call BELL(1)
	   	print 31,xend
31	   	format('&Last x reset to ',g13.6,' O.K. [Y] ? ')
		if(setmax) xmax=xend
	   	read 101,ans
	   	if(UC(ans).eq.'N') goto 212
		goto 32
30		nbin=IFIX(0.5+xn)
	   endif
311	   if(nbin.eq.0) goto 303
	   do 211 j=1,nbin
	   m=m+1
	   if(m.gt.501) then
	     call bell(2)
	     print *, ' TOO MANY BINS'
	     goto 501
	   endif
	   XAXIS(m)=XAXIS(mLAST)+FLOAT(J)*DX
211	   continue
c   save values for printing if repeated with same bins
1044	   xsav(i,1)=float(nbin)
	   xsav(i,2)=dx
	   xsav(i,3)=xaxis(mlast)
	   xsav(i,4)=xaxis(m)
306	   if(pon()) write(7,2111) (xsav(i,j),j=1,4)
	   if(discprt) write(8,2111) (xsav(i,j),j=1,4)
	   if(sbin.or.iopt.ge.3) print 2111, (xsav(i,j),j=1,4)
2111	   format(1x,f5.0,
     & ' bins: width= ',g12.5,' from ',g12.5,' to ',g12.5)
210	   continue		!end if i loop
C
	   if(sbin) goto 307
	   nbin=m-1		!NOW NBIN=TOTAL NO OF BINS
	   xwbase=dx		!IF ONLY ONE BIN WIDTH
	   if(nbw.eq.1) goto 307
	   print 107
107	   FORMAT( ' Base width for frequency density= ')
	   call INPUTr(xwbase)
307	   if(pon()) write(7,1071) xwbase
	   if(discprt) write(8,1071) xwbase
	   if(sbin) print 1071,xwbase
1071	   FORMAT(' Base width for frequency density= ',g13.6)
	endif
c
C NOW SORT Y VALUES AND CALC FREQ (DENSITY) FOR EACH FILE
c No need to repeat this if SHIST (same histo, same bins)
	if(shist.and.sbin.and.sexp) goto 4071
	print 82
82	format(' Sorting data into bins...')
	flo=0.
	fhi=0.
	do 83 i=1,510
83	freq(i)=0.0
c
	do 223 i=1,nyval
	yv=tval(i)
	if(yv.lt.xaxis(1)) then
	   flo=flo+1.0
	   goto 223
	endif
	if(yv.gt.xaxis(nbin+1)) then
	   fhi=fhi+1.0
	   goto 223
	endif
	do 221 j=1,nbin
	if(yv.ge.xaxis(j).and.yv.lt.xaxis(j+1)) freq(j)=freq(j)+1.0
221	continue	!j loop (bins)
c For top bin include those EQUAL to boundary (eg so Popen=1.0 not excluded
c from top bin)
	if(yv.eq.xaxis(nbin+1)) freq(nbin)=freq(nbin)+1.0
223	continue	!i loop (obs)
	goto 85
c Sorting done
85	continue
c
	if(flo.lt.0.1) flo=-2.	!do not plot if flo=0
	if(fhi.lt.0.1) fhi=-2.	!ditto
	fmax=0.
C CONVERT FREQ TO FREQ DENSITY NEXT
c N.B. FLO and FHI are abs numbers, not densities- cannot express FHI
c as a density if upper lim is infinite! FLO can be expressed as density
c once bin width has been fixed (see VHIST)
	do 218 j=1,nbin
	if(.not.logt) freq(j)=freq(j)*xwbase/(xaxis(j+1)-xaxis(j))
	if(freq(j).gt.fmax) fmax=freq(j)
218	continue
	if(flo.gt.fmax) fmax=flo
	if(fhi.gt.fmax) fmax=fhi
	i=ifix(0.1+flo)
	if(flo.lt.0.) i=0	!for print
	j=ifix(0.1+fhi)
	if(fhi.lt.0.) j=0
	print 222,i,j
	if(pon()) write(7,222) i,j
	if(discprt) write(8,222) i,j
222	FORMAT(' No of values below Xlow= ',i8,
     & ' No of values above Xhigh= ',i8)
4071	print 407
407	FORMAT(' Type frequency densities [N] ? ')
	read 101,ans
	if(UC(ans).NE.'Y') GOTO 406
	if(.not.logt) then
		print 1061
     		if(pon()) write(7,1061)
		if(discprt) write(8,1061)
1061 		format(
     & ' Bin no.         X values                Frequency')
		DO 220 I=1,NBIN
		print 106,I,xaxis(i),xaxis(i+1),FREQ(I)
		if(pon()) write(7,106) I,xaxis(i),xaxis(i+1),FREQ(I)
220		if(discprt) write(8,106) I,xaxis(i),xaxis(i+1),FREQ(I)
106		FORMAT(I5,3X,g13.6,' - ',g13.6,3x,G13.6)
	endif
c
	if(logt) then
		print 1062
     		if(pon()) write(7,1062)
		if(discprt) write(8,1062)
1062 		format(
     & ' Bin no.         X values                Frequency',/,
     & '              log(X) values              sqrt(freq)')
		DO 1063 I=1,NBIN
		print 1064,I,xaxis(i),xaxis(i+1),FREQ(I)
		if(pon()) write(7,1064) I,xaxis(i),xaxis(i+1),FREQ(I)
		if(discprt) write(8,1064) I,xaxis(i),xaxis(i+1),FREQ(I)
		if((xaxis(i).le.0.).or.(freq(i).lt.0.)) goto 1063
		print 1065,alog10(xaxis(i)),
     &	alog10(xaxis(i+1)),sqrt(freq(i))
		if(pon()) write(7,1065) alog10(xaxis(i)),alog10(xaxis(i+1)),
     &	sqrt(freq(i))
		if(discprt) write(8,1065) alog10(xaxis(i)),
     &	alog10(xaxis(i+1)),sqrt(freq(i))
1063		continue
1064		FORMAT(I5,3X,g13.6,' - ',g13.6,3x,G13.6)
1065		FORMAT(8x,   g13.6,' - ',g13.6,3x,G13.6)
	endif
c
		print 108
		if(pon()) write(7,108)
		if(discprt) write(8,108)
	pause ' Display follows'
406	CONTINUE
C
4061	CONTINUE
	call flush(7)
	if(flo.lt.0.) flo=0.
	if(fhi.lt.0.) fhi=0.
c end code from SETBIN
c
4062	continue
	ilabel=1
	titley='Frequency '
	xw=xwbase  !so Y axis labelled 'per 10 ms' (reset below as nec)
	if(logt) xw=0.	!not so labelled for log-bins
c
	if(iplot.eq.1) then
	   titlex='Apparent open time (ms) '
	else if(iplot.eq.2) then
	   if(.not.burst(iset)) then
	      titlex='Apparent shut time (ms) '
	   else if(burst(iset)) then
	      titlex='App. shut time | 5t4 < 5t4crit (ms)'
	   endif
	else if(iplot.eq.3) then
	   if(itype.eq.1) titlex='App open time (preceded by spec gap)'
	   if(itype.eq.2) titlex='App open time (followed by spec gap)'
	   if(itype.eq.3) titlex='App open time (adj to spec gap)'
	else if(iplot.eq.4) then
	   titlex='Adjacent shut time (mean)'
	   titley='Mean open time (adj to spec gap range) '
	   xw=0.
	endif
c
c    IJOIN(j) gives line type for main histo bars: for Hgraph types 0-8
c
c		0=solid line
c		1=dotted line
c		2-6=dashed line with increasing dash length
c		7=dash-dot
c		8=dash-dot-dot
c
c	ILINE(j) defines line type for the jth calculated curve, via ZSEQ
c         (prev numbered 1-5, but now make 0-4 so 0=continuous as in Hgraph)
c		0=continuous line
c		1=long dash
c		2=short dash
c		3=long,short dash
c		4=long,short,short dash
c
c
c For Toshiba VIDEOTYP()=18 for graphics mode, =3 for alpha mode
c	if(videotyp().ne.18) call INIPLT(0,.false.,1.0)
	autplt=.false.
	draft=.false.
	doframe=.true.
	landscap=.true.
	fitted=.true.
	cbig=2.5
	ifont=4
	isval=0	!no arrow
	xcross=0.0
	ycross=0.0
	inumx=-1	!X axis in fixed format if log scale
	ntx=5		!if not logt set initial input values
	nty=5
	itx=1
	ity=1
	iask=-2	!do not ask before leaving display; delete graph
	ixlo=-1
	if(iplot.eq.4) then	!use VPLOT for mean open vs adj gap
	   ilog=1		!y vs log(x)
	   ndimd1=3
	   ndimc1=3
	   isdev=1			!plot SDs
	   ncurvd=3
	   do 8 j=1,3
	    ndat1(j)=nval
	    icurvd1(j)=j
	    ijoin1(j)=-1
	    ncal1(j)=nval
	    icurvc1(j)=j
8	   continue
	   isym(1)=-7
	   ijoin1(1)=1	!dotted line joins data pnts for 'preceding gap'
	   isym(2)=3
	   isym(3)=4
	   syms(1)=0.	!so symbol size set internally
	   ncurvc=1		!until fixed!
	   iline1(1)=10	!so calc points joined by straight contin lines
	   iscal=1		!scale internally
c Define Ycal1(i,1), xcal1(i,1), i=1,nval
c (ie calc Mean OPen ADJacent to spec gap range)
	   call MOPADJ(tres,ylo,yhi,nval,QD,k,kA,kF,km,ycal1,xcal1,den1)
	   title=' CALCULATED values for mean open given preceding gap'
	   call PCRANGE(title,ylo,yhi,nval,ny0,den1,prt,ycal1,xcal1,1)
	   title=' CALCULATED values for mean open given following gap'
	   call PCRANGE(title,ylo,yhi,nval,ny1,den1,prt,ycal1,xcal1,2)
	   ncurvc=1		!until fixed!
	   iline1(1)=10	!so calc points joined by straight contin lines
	   iscal=1		!scale internally
	   pause ' Display follows'
	  call VPLOT2(XVAL1,YVAL1,ndat1,icurvd1,ncurvd,ijoin1,syms,ndimd1,
     &   XCAL1,YCAL1,ncal1,icurvc1,ncurvc,iline1,ndimc1,isym,ilog,iscal,
     &   XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     &   iXLO,iXHI,iYLO,iYHI,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     &   titlex,titley,ilabel,doframe,idiskq,autplt,draft,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,isdev,interp)
	   goto 994			!where next?
	endif		!end of diversion for iplot=4
c
c Back to iplot=1,2,3 (pdfs)
	ndimd=1
	ndimc=10
c IFITYPE=0 for no display of param
c IFITYPE=1 for time histos
c===c IFITYPE=-1 for HJC time histos (display tres and asymptotic fit)
	ifitype=0
c===	if(iplot.eq.1.or.iplot.eq.2) ifitype=-1
c===first not needed??
c	first=.false.
c
101	format(a1)
102	format(i8)
108	FORMAT(/)
	lt2=2       !short dash for lo,hi bins
	ilog=0
c	if(logt) ilog=1		!plot vs log(t)
	if(logt) ilog=6		!plot sqrt(y) vs log(t)
	iscal=0			!use input xmin,xmax etc
c histos:
	ncurvd=1
	icurvd(1)=1
	ndat(1)=nbin
	ijoin(1)=0
	do 1 i=1,nbin+1
1	 xval(i,1)=xaxis(i)
	do 121 i=1,nbin
	 yval(i,1)=freq(i)
121	continue
c Now fix lo,hi bins etc and define xmin,fmin etc (all here rather than
c in SCVD1 now). Want to display with fit straight away -no prelim display
c without calc curve for scaling so better scale externally -do as in HJCVDU
c CODE FROM HJCVDU:
c
c NB logt already set!
21	continue		!return here to change from open to shut
	icomp=1
	if(iplot.eq.1.or.iplot.eq.2) then
	   print 13,icomp
13	   format(
     & ' (1) Show ''exact'' pdf only (2*exact + asymptotic)',/,
     & ' (2) Show asymptotic distribution also ',/,
     & ' (3) Show asymptotic distribution and its components also ',/,
     & ' Option number [',i2,'] = ')
	   read 102,i
	   if(i.ge.1.and.i.le.3) icomp=i
	   if(icomp.eq.1) then
	    call DCASK('Show also the pdf without missed events',ans1,ans)
	    ans1=ans		!default next time
	    nores=ans.eq.'Y'
	    if(nores) then
	       call DCASK(
     &	'Rescale this pdf to unit area above t=tres','y',ans)
	       scale=ans.eq.'Y'
	    endif
	   endif
	endif
c
	ncalc=512
	if(debug()) then
	  print 700,2,xmin,xmax,ncalc
	  pause
	endif
c Now use xmax as for histo (based on data), calc by VARV1
	xmin=0.
c	if(iplot.eq.1) then
c	   xmax=4.*sngl(-1.d3/rootA(kA))
c	else if(iplot.eq.2) then
c	   xmax=4.*sngl(-1.d3/rootF(kF))
c	endif
	if(logt) then
	   xmin=0.010001	!10 microseconds should be below any tres!
	   xmax=2.*xmax	!make bigger for log display
	   xmin=alog10(xmin)
	   xmax=alog10(xmax)
	endif
	iscal=1		!scale internally
	if(debug()) then
	  print 700,3,xmin,xmax,ncalc
	  pause
	endif
c
c CALCs for iplot=1,2
	if(iplot.eq.1.or.iplot.eq.3) then	!open
	   ncomp=kA
	   do 11 m=1,kA
		tau(m)=sngl(-1.d3/rootA(m))	!define tau for asymptotic pdf
		amp(m)=sngl(ampA(m))		!define amplitude for ditto
11	   continue
	   do 111 m=1,k
		g00(m)=g00A(m)
		g10(m)=g10A(m)
		g11(m)=g11A(m)
111	   continue
	else if(iplot.eq.2) then
	   ncomp=kF
	   do 12 m=1,kF
		tau(m)=sngl(-1.d3/rootF(m))	!define tau for asymptotic pdf
		amp(m)=sngl(ampF(m))		!define amplitude for ditto
12	   continue
	   do 112 m=1,k
		g00(m)=g00F(m)
		g10(m)=g10F(m)
		g11(m)=g11F(m)
112	   continue
	endif
c
19	continue	!return here to recalculate points
	if(debug()) then
	  print 700,4,xmin,xmax,ncalc
	  pause
	endif
	tres1=sngl(1.d3*tres)	!tres in msec
	tres2=2.*tres1
	tres3=3.*tres1
c  Calc dx (=dx1 say) for xcal(i,1) (points from tres1 to xmax)
	if(logt) then		!xmax already in log units
	  dx1=(xmax-alog10(tres1))/float(ncalc)
	else
	  dx1=(xmax-tres1)/float(ncalc)
	endif
	if(logt) then
	   tres1=alog10(tres1)
	   tres2=alog10(tres2)
	   tres3=alog10(tres3)
	endif
	ires2=1+ifix(1.0+(tres2-tres1)/dx1)	!index of point just below tres2
	ires3=1+ifix(1.0+(tres3-tres1)/dx1)	!index of point just below tres3
c
c Define xcal separately, so can arrange to have 2 points at t=tres, one with
c y=0.0, one with y=F0HJC(tres), so vertical line drawn nicely -these might
c as well be the first two points! So set 1st point=tres1,y=0.0, 2nd point
c =tres1 also
	xcal(1,1)=tres1
	do 151 i=2,ncalc
151	 xcal(i,1)=tres1+float(i-2)*dx1      !so xcal(2,1)=tres1 also
c
c
c Xcal(i,1) now defined. But VPLOT, and F0HJC etc expect non-log values so
c must convert back
	if(logt) then
	   tres1=10.**tres1
	   tres2=10.**tres2
	   tres3=10.**tres3
	   do 153 i=1,ncalc
153	   xcal(i,1)=10.**xcal(i,1)
	endif
c	if(debug()) print 1531,ires2,ires3,tres1,tres2,tres3
c1531	format(' ires2,ires3,tres1,tres2,tres3=',/,2i5,3g13.6)
c
c DEFINE Ycal() (including multiply by antrue*xwbase, to get superimposition
c on the histogram.  In this case the true number is just the observed number
c so antrue replaced by float(nyval).  Note that xwbase is in msec, but pdf
c is in 1/sec, so to convert latter to 1/msec must divide by 1000
c For bursts must divide by P[t<tcrit] in case of the shut time pdf to allow
c for fact that shuttings longer than tcrit are excluded (not, of course, for
c open times in iplot=1,3)
	fac=float(nyval)*xwbase/1000.
	if(burst(iset).and.iplot.eq.2) then
	   tcrit2=dble(tcrit(iset))*1.d-3	!in sec
	   Ptc=FTCt(tcrit2,tres,k,kA,kF,km,ucol)
	   fac=fac/sngl(ptc)
	endif
c
c  For iplot=3 calc the conditional pdfs in POPADJ, in ycal(i,2) and ycal(i,3),
c and then also do next bit to put unconditional open pdf in ycal(i,1) (the
c appropriate g10 etc for open times were defined above in iplot=3 case)
	if(iplot.eq.3) then
	   call POPADJ(tres,ylo,yhi,ncalc,k,kA,kF,km,ycal,xcal,den1)
	endif
	ycal(1,1)=0.0	!to draw vertical line
	deb=debug()
	do 15 i=2,ncalc
	 t=xcal(i,1)
	 time=dble(t)
c	 if(deb) then
c	   print 701,i,t
c701	   format(' xcal(i,1) = ',i7,g13.6)
c	 endif
	 if(i.le.ires2) then
	   ycal(i,1)=sngl(F0HJC(time,g00,eigen,tres,k,km))
	 else if(i.gt.ires2.and.i.le.ires3) then
	   ycal(i,1)=sngl(F1HJC(time,g00,g10,g11,eigen,tres,k,km))
	 else         		!asymptotic pdf
	   f=0.0
	   do 16 m=1,ncomp
		fc=amp(m)*exp(-(t-tres1)/tau(m))	!NB needs excess time here
c==		ycal(i,m+2)=fc	!components
		f=f + fc
16	   continue
	   ycal(i,1)=f
	 endif
15	continue
c
	jval=1
	if(iplot.eq.3) jval=3   !uncond + conditional (pre=gap + post-gap)
	do 1541 j=1,jval
	 if(logt) then
	   do 154 i=1,ncalc
154		ycal(i,j)=fac*ycal(i,j)*xcal(i,j)*2.30259	!f(log10(t))=2.3*t*f(t)
	 else
	   do 155 i=1,ncalc
155		ycal(i,j)=fac*ycal(i,j)
	 endif
1541	continue
c
c In order to see the asymptotic distribution, and its components,  right back
c to t=0, calculate it, and its components, separately, with their own xcal().
c NB components skipped for iplot=3 (for which icomp=1 always)
c  BUT the asymptotic distn is defined only for 'excess time', so probably
c should not use below tres?
c   Keep asymptotic dist in ycal(i,2), and its components in ycal(i,3) to
c ycal(ncomp+2).
c  If NORES then use ycal(i,2),xcal(i,2) for pdf with zero res
	if(iplot.le.2.and.icomp.eq.1.and.NORES) then	!calc pdfs by CH82 methods
	  icurvc(2)=2
	  call EQOCCd(QD,k,k-1,km,Peq1)	!calc equilib occs
	  call PHIo1(QD,Peq1,phio,km)		!calc init vector
	  if(iplot.eq.1) then
	    call PDFopen(QD,phio,area,tau1,kA,km)
	    print 108
	    if(discprt) write(8,108)
	    call PDFOUTs('Open time pdf with zero resolution',
     &	-1,-1,area,tau1,kA,am,sd,km,.false.,.false.,discprt)
	    kX=kA
	  else if(iplot.eq.2) then
	    call PDFshut(QD,phio,area,tau1,kF,km)
	    print 108
	    if(discprt) write(8,108)
	    call PDFOUTs('Shut time pdf with zero resolution',
     &	-1,-1,area,tau1,kF,am,sd,km,.false.,.false.,discprt)
	    kX=kF
	  endif
	  fac1=fac
	  if(scale) then
		f=0.0
	      do 47 m=1,kX
47		f=f + area(m)*exp(-tres1/tau1(m))	!area above tres
		fac1=fac1/f					!new scale factor
	  endif
	  xmin1=0.001	  !calc from 1 microsec for zero res
	  if(logt) xmin1=alog10(xmin1)	!xmax already log in this case
	  dx2=float(xmax-xmin1)/float(ncalc)
	  do 44 i=1,ncalc
	   t=xmin1+float(i-1)*dx2       !start at 1 microsec
	   if(logt) t=10.**t
	   xcal(i,2)=t
	   f=0.0
	   do 45 m=1,kX
45	    f=f + (area(m)/tau1(m))*exp(-t/tau1(m))
	   if(logt) f=f*t*2.30259		!f(log10(t))=2.3*t*f(t)
	   ycal(i,2)=f*fac1*1000.
44	  continue
	endif
	if(icomp.eq.2.or.icomp.eq.3) then !define separate xcal for components
	 dx2=float(xmax-xmin)/float(ncalc)
	 if(debug()) then
	  print 700,5,xmin,xmax,ncalc
	  pause
	 endif
	 deb=debug()
	 do 152 i=1,ncalc
	   t=xmin+float(i-1)*dx2       !start at xmin
	   if(logt) t=10.**t
	   xcal(i,2)=t
c	 if(deb) then
c	   print 702,i,t
c702	   format(' xcal(i,2) = ',i7,g13.6)
c	 endif
	 do 152 m=3,ncomp+2
	   xcal(i,m)=xcal(i,2)
152	 continue
c and calculate the asymptotic pdf from t=0 to tmax
	 do 23 i=1,ncalc
	   f=0.0
	   t=xcal(i,2)
	   t1=t-tres1		!needs EXCESS time here
	   do 161 m=1,ncomp
		fc=amp(m)*exp(-t1/tau(m))	!NB needs excess time here
		f=f + fc
		if(logt) fc=fc*t*2.30259		!f(log10(t))=2.3*t*f(t)
		ycal(i,m+2)=fc*fac	!components in ycal(i,3),ycal(i,4),...
161	   continue
	   if(logt) f=f*t*2.30259		!f(log10(t))=2.3*t*f(t)
	   ycal(i,2)=f*fac                !total asymptotic in ycal(i,2)
23	 continue
	endif
c
c Inputs for VHIST:
	if(debug()) then
	  if(nores) jval=2
1125	  print 1121,jval
1121	  format(
     &' Print Xcal(i,j), Ycal(i,j) for j=1,',i2,': i = i1,i2 [skip] = ')
	   read 4,i1,i2
4	   format(2i8)
	   if(i1.eq.0) goto 1122
	   if(i2.lt.i1) i2=i1
	   do 1123 i=i1,i2
	   do 1123 j=1,jval
		print 1124,i,j,xcal(i,j),ycal(i,j)
		if(discprt) write(8,1124) i,j,xcal(i,j),ycal(i,j)
1124		format('i,j = ',2i8,'  x,y = ',2g13.6)
1123	   continue
	   goto 1125
1122	   continue
	endif
	if(iplot.eq.1.or.iplot.eq.2) then
	   icurvc(1)=1		!total
	   iline(1)=0		!total curve continuous
	   ncal(1)=ncalc
	   do 22 j=2,ncomp+2		!ncomp+2 to allow for total asymptotic
	    ncal(j)=ncalc
	    icurvc(j)=j
	    iline(j)=2		!short dash for components
22	   continue
	   iline(2)=3	!long dash for total asymptotic
c==	   ncal(2)=ires31
	   ncurvc=1
	   if(icomp.eq.1) then
	     ncurvc=1
	     if(nores) then
		 ncurvc=2		!show also pdf with zero res
		 icurvc(2)=2
	     endif
	   else if(icomp.eq.2) then
	     ncurvc=2
	   else if(icomp.eq.3) then
	     ncurvc=ncomp+2
	   endif
	else if(iplot.eq.3) then
c	   (1)=uncond; (2)=prec gap; (3)=following gap (see POPADJ)
	   do 39 i=1,3
39	    ncal(i)=ncalc
	   iline(1)=2           !short dash for unconditional
c	default display
	   ncurvc=2
	   if(itype.eq.1.or.itype.eq.3) then
	     icurvc(1)=1				!unconditional
	     icurvc(2)=2				!prec gap
	     iline(1)=2
	     iline(2)=0           !continuous for preceding gap
	     print 40
40	     format(' Calculated curves shown:',/,
     &' (1) Unconditional pdf of apparent openings (short dash)',/,
     &' (2) pdf for openings preceded by spec gaps (contin line)',/,
     & ' O.K. [Y] ? ')
	   else if(itype.eq.2) then
	   	icurvc(1)=1		!unconditional
		icurvc(2)=3		!following gap
	      iline(1)=2
		iline(3)=0           !contin for following gap
	     print 41
41	     format(' Calculated curves shown:',/,
     &' (1) Unconditional pdf of apparent openings (short dash)',/,
     &' (2) pdf for openings followed by spec gaps (contin line)',/,
     & ' O.K. [Y] ? ')
	   endif
	   read 101,ans
	   if(UC(ans).ne.'N') goto 38
c
	   iline(1)=2           !short dash for unconditional
	   iline(2)=0           !continuous for preceding gap
	   iline(3)=3           !long dash for following gap
	   print 37
37	   format(
     &  ' Options for calculated curves:',/,
     &' (1) Unconditional pdf of apparent openings (short dashes)',/,
     &' (2) pdf for openings preceded by spec gaps (contin line)',/,
     &' (3) pdf for openings followed by spec gaps (long dashes)',/,
     &' Number of calculated curves to be displayed = ')
	   read 102,ncurvc
	   do 42 i=1,ncurvc
		print 46,i
46		format('&(',i2,') Curve number = ')
		read 102,icurvc(i)
42	   continue
c
38	   continue
	endif
c
	fmin=0.
c*	xmin=0.
c*	if(logt) xmin=xaxis(1)		!if no lo bin
c*	xmax=xaxis(nbin+1)	!=xval(nbin+1,1), if no hi bin -set above
	fmax=fmax*1.2		     !value from SETBIN
	fmax=float(1+ifix(fmax))	!ROUND UP
	ftic=0.5*10.0**ifix(alog10((fmax-fmin)*0.5))
2102	if((fmax-fmin)/ftic.gt.10.) goto 2101
	ftic=ftic/2.
	goto 2102	!ensure at least 5 tics so get numerical label
2101	xtic=2.*xwbase	!number every 10th bin
c# For Lahey/Hgraph xtic refers to major, not minor tics so make
c bigger ( say 2-fold rather than 5-fold for now?- 5-fold faster!)
	xtic=xtic*5.
	if(.not.logt) ftic=ftic*5.	!smaller tics for sqrt scale for now
c
	yval(0,1)=flo		!set whether 0 or not- it is checked in VHIST
	xval(0,1)=xval(1,1)	!if no flo- it is checked in VHIST (in MINMAX)
	xval(nbin+2,1)=xval(nbin+1,1)		!ditto if no fhi bin
	if(flo.gt.0.0001) then
	   xval(0,1)=xval(1,1)-xwbase
	   if(logt) xval(0,1)=xval(1,1)*(xval(1,1)/xval(2,1))
c*	   xmin=xval(0,1)
	endif
	yval(nbin+2,1)=fhi	!NB Yval(nbin+1,1) not used- set (as flo, above)
	if(fhi.gt.0.0001) then
	   xval(nbin+2,1)=xval(nbin+1,1)+xwbase
	   if(logt) xval(nbin+2,1)=xval(nbin+1,1)*
     &	(xval(nbin+1,1)/xval(nbin,1))
c	  keep ratio for hi bin as for other bins, so same width on log scale
	   xval(nbin+2,1)=xval(nbin+1,1)*(xval(nbin+1,1)/xval(nbin,1))
c*	   xmax=xval(nbin+2,1)
	 endif
c
c Curves: plot calc distribution (and also asymptotic only as dashed line)
c  Calculation based on HJCVDU.FOR (in \CALC), but need to consider
c also where xmin,xmax,antrue defined (the pdfs are defined only above t=tres
c so can prob simply use float(nfit)=anfit say, rather then antrue)
c	if(debug()) then
c	  print 772,xmin,xmax,fmin,fmax,xtic,ftic,flo,fhi,xw
c772	  format('Pos#2: xmin,xmax,fmin,fmax,xtic,ftic,flo,fhi,xw='
c     &	,/,9g13.6)
c	  pause
c	endif
	if(debug()) then
	  print 700,6,xmin,xmax,ncalc
	  pause
	endif
	call VHIST2(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ILOG,ISCAL,
     & XMIN,XMAX,fMIN,fMAX,XTIC,fTIC,xcross,ycross,ntx,nty,itx,ity,
     & iXLO,iXHI,iYLO,iYHI,xw,lt2,inumx,inumy,
     & titlex,titley,ilabel,doframe,idiskq,
     & autplt,draft,itit,title1,cbig,ifont,landscap,fitted,
     & theta,ifitype,ncomp,sval,isval,iask)
c
c where next?
994	iopt=3
	print 18,iopt
18	format(
     & ' (1) Display again, but recalc points for new Xmin,Xmax',/,
     & ' (2) Display again (original display)',/,
     & ' (3) Do a different plot',/,
     & ' Option number [',i2,'] = ')
	read 102,i
	if(i.ge.1.and.i.le.3) iopt=i
	if(iopt.eq.3) goto 993
	if(iopt.eq.1) then
	   iscal=0		!use current xmin,xmax etc
	   goto 19		!recalc 512 points between xmin,xmax
	else if(iopt.eq.2) then
	   iscal=1
	   xmin=0.
	   xmax=4.0*tau(ncomp)		!in msec=4*slowest asympotic component
c	   goto 20           !original display
	   goto 19           !original display
	endif
c
993	iplot=iplot+1
	if(iplot.gt.5) iplot=5
c	if(iset.le.nset) iset=iset+1
	goto 226
c
999	continue
	RETURN
	end

	subroutine GETRANGE(tgap,topen,ylo,yhi,nval,sy,syy,ny,
     & sy2,syy2,ny2,sx,sxx,nx,sx2,sxx2,nx2,deb,prt)
c To accumulate, for calc of mean and sd, the open times (topen) according
c to the range in which the specified shut time, tgap, falls
c The values are kept in sy,syy (=sy0 or sy1 in call according to whether
c it is a preceding or following gap), and they are alse kept in sy2 etc
c whether gap precedes OR follows).  The mean and SD of the shut times is
c accumulated in sx,sxx,nx
	real*4 sy(20),syy(20),sy2(20),syy2(20)
	real*4 sx(20),sxx(20),sx2(20),sxx2(20)
	real*4 ylo(20),yhi(20)
	integer ny(20),ny2(20)
	integer nx(20),nx2(20)
	logical deb,prt
c
	do 1 m=1,nval
	if(tgap.ge.ylo(m).and.tgap.lt.yhi(m)) then		!OK- in range
	  sy(m)=sy(m)+topen		!add open times for mean
	  sy2(m)=sy2(m)+topen		!add open times for mean
	  syy(m)=syy(m)+topen*topen	! for SD
	  syy2(m)=syy2(m)+topen*topen	! for SD
	  ny(m)=ny(m)+1			!increment number
	  ny2(m)=ny2(m)+1			!increment number
	  sx(m)=sx(m)+tgap 		!add shut times for mean
	  sx2(m)=sx2(m)+tgap		!add shut times for mean
	  sxx(m)=sxx(m)+tgap*tgap	! for SD
	  sxx2(m)=sxx2(m)+tgap*tgap	! for SD
	  nx(m)=nx(m)+1			!increment number
	  nx2(m)=nx2(m)+1			!increment number
c	  if(deb) then
c	    print 229,topen,tgap,m,ny(m)
c	    if(prt) write(7,229) topen,tgap,m,ny(m)
cc*	    if(discprt) write(8,229) topen,tgap,m,ny(m)
c	  endif
c229	  format(' open, preceding shut, range #, n = ',2g13.6,2i5)
	endif
1	continue
	RETURN
	end


	subroutine PRANGE(title,ylo,yhi,nval,sy,syy,ny,sx,sxx,nx,
     & prt,yval1,xval1,j)
c To print obs conditional means, and also define xval1,yval1 for plotting
c them
	real*4 sy(20),syy(20)
	real*4 sx(20),sxx(20)
	real*4 ylo(20),yhi(20)
	dimension XVAL1(2048,3),YVAL1(2048,3)
	integer ny(20),nx(20)
	character title*74
	logical prt,discprt
	common/dp/discprt
	COMMON/SD/weight(100,10)
c
	print 1,title
	if(prt) write(7,1) title
	if(discprt) write(8,1) title
1	format(1x,a74,/,7x,
     & ' Gap length: range',7x,'mean gap  # of ops  mean open time',
     & '   Std dev')
c
	do 428 i=1,nval
c	xval1(i,j)=(ylo(i)+yhi(i))/2.0 !midpoint of shut time range -NOW use mean!
	n=ny(i)
	if(ny(i).ne.nx(i)) then
	   call BELL(2)
	   print 2,i,nx(i),ny(i)
2	   format(' ERROR in PRANGE: range #',i2,' nx,ny = ',2i8)
	   pause
	endif
	if(n.gt.1) then
	  ym=sy(i)/float(n)
	  xm=sx(i)/float(n)
	  sd=sqrt((syy(i)-(sy(i)*sy(i)/float(n)))/float(n-1))
c	  sdx=sqrt((sxx(i)-(sx(i)*sx(i)/float(n)))/float(n-1))
	  xval1(i,j)=xm
	  yval1(i,j)=ym
	  weight(i,j)=float(n)/(sd*sd)	!display standard errors
	  if(yhi(i).lt.3.e10) then
	   print 426,i,ylo(i),yhi(i),xm,n,ym,sd
	   if(prt) write(7,426) i,ylo(i),yhi(i),xm,n,ym,sd
	   if(discprt) write(8,426) i,ylo(i),yhi(i),xm,n,ym,sd
426	   format(i3,1x,f10.3,' to ',f10.3,2x,f11.4,3x,i4,2(3x,g13.6))
	  else	!assume yhi(i)=3.1536e10msec=1 year as set above
	   print 42,i,ylo(i),xm,n,ym,sd
	   if(prt) write(7,42) i,ylo(i),xm,n,ym,sd
	   if(discprt) write(8,42) i,ylo(i),xm,n,ym,sd
42	   format(i3,1x,f10.3,' to   1 year  ',2x,f11.4,3x,i4,2(3x,g13.6))
	  endif
	else
	  weight(i,j)=0.
	  if(yhi(i).lt.3.e10) then
         print 427,i,ylo(i),yhi(i),n
         if(prt) write(7,427) i,ylo(i),yhi(i),n
         if(discprt) write(8,427) i,ylo(i),yhi(i),n
427	   format(i3,1x,f10.3,' to ',f10.3,16x,i4)
	  else
         print 43,i,ylo(i),n
         if(prt) write(7,43) i,ylo(i),n
         if(discprt) write(8,43) i,ylo(i),n
43	   format(i3,1x,f10.3,' to   1 year  ',16x,i4)
	  endif
	endif
428	continue
	RETURN
	end


	subroutine PCRANGE(title,ylo,yhi,nval,ny,den1,prt,ycal,xcal,j)
c To print calc conditional means
	character title*74
	real*4 xcal(2048,3),ycal(2048,3)
	real*4 ylo(20),yhi(20)
	real*4 den1(20)
	integer ny(20)
	logical prt,discprt
	common/dp/discprt
c
	ntot=0
	do 27 i=1,nval
27	 ntot=ntot+ny(i)
	print 1,title
	if(prt) write(7,1) title
	if(discprt) write(8,1) title
1	format(1x,a74,/,7x,' Gap length: range',7x,
     & 'Mean gap    P[tlo<t<thi]  N*P   Mean open time')
c
	do 428 i=1,nval
	  enp=float(ntot)*den1(i)
	  if(yhi(i).lt.3.e10) then
c	   print 426,i,ylo(i),yhi(i),xm,n,ym,sd
	   print 426,i,ylo(i),yhi(i),xcal(i,j),den1(i),enp,ycal(i,j)
	   if(prt) write(7,426) i,ylo(i),yhi(i),xcal(i,j),den1(i),
     &	enp,ycal(i,j)
	   if(discprt) write(8,426) i,ylo(i),yhi(i),xcal(i,j),den1(i),
     &	enp,ycal(i,j)
426	   format(i4,1x,f10.3,' to ',f10.3,2x,f11.4,1x,f8.5,2x,
     &	f10.1,3x,g13.6)
	else	!assume yhi(i)=3.1536e10msec=1 year as set above
	   print 42,i,ylo(i),xcal(i,j),den1(i),
     &	enp,ycal(i,j)
	   if(prt) write(7,42) i,ylo(i),xcal(i,j),den1(i),
     &	enp,ycal(i,j)
	   if(discprt) write(8,42) i,ylo(i),xcal(i,j),den1(i),
     &	enp,ycal(i,j)
42	   format(i3,1x,f10.3,' to   1 year  ',2x,f11.4,1x,f8.5,2x,
     &	f10.1,3x,g13.6)
	endif
428	continue
	RETURN
	end

