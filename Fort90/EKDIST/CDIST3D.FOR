	subroutine CDIST3D(tint,ampl,iprops,nint,nintt,ndimy,
     & iscan,treso,tresg,tres,idsav,idiskq)
c
c For doing bivariate distributions and dependency (modif from cdist1)
c Set the bins, and do the display, all within this subroutine?
      allocatable::yval,ygap
      allocatable::xmid,ymid,x,y,z,znew,zf
	real*4 xaxis(501),yaxis(501)
	real*4 xmid(:),ymid(:),yval(:),ygap(:),x(:),y(:),z(:,:),znew(:,:)
	real*4 zf(:,:)
      allocatable::px,py
	real*4 px(:),py(:)
      allocatable::dep,depdif
	real*4 dep(:,:),depdif(:,:)
	allocatable::badval
	logical badval(:,:)
	logical mono,help
	character*40 qfile
	character*75 xtitle,ytitle,ztitle	!output from LAXES
	integer icol(100)		!for DCMENU
	character*75 title1
c
	character*50 text(5)
	character*11 text1,text2
	logical first
c
	real*4 TINT(nintt),ampl(nintt)
	integer*1 iprops(nintt)
	logical pon,exass,debug,slock,caplock
	logical btest,setbad,badint,dubious
	logical open1,open2,shut1,setbad1,setbad2,bad1,bad2,bad
	logical allocated,posneg
	character*1 ans,UC
c	character*11 cnum
	logical discprt,deb
	common/dp/discprt
	common/hlp/help		!for QDIALOG
	COMMON/cols/icol,mono
	common/maxblk/xmin,xmax,ymin,ymax	!for hjcdisp
	common/queue/qfile	!for vplot and vhist

c====================CDIST1 notes
c
c Modif 01/12/96 08:23pm -open period calc changed again!  If amplitude
c   window is specified, need to specify whether open period (a) starts,
c   (b) ends when there is a transition to/from shut level, or whether
c   a transition to ANY level outside the specified window is OK.
c   Probably OK to have two options only
c   (1) Isolated open periods
c	Open period in specified amp window must (a) start by transition
c	from shut level to a level in the window, (b) then any number of
c	open/open transitions as long as all open levels are in the window,
c	(c) open period ends with transition to shut state (transition to
c	any other open level outside window causes rejection of value)
c   (2) All open periods in window
c	(a) starts with a transition from ANY level to a level within
c	the window, (b) then any number of open/open transitions as long
c	as all open levels are in the window, (c) ends with transition to
c	ANY level outside the window.
c
c Modif 10/19/95 10:50am to revise calculation of open periods, to add
c  (1) mean open PERIOD given adjacent shut time range
c  (2) mean open PERIOD given adjacent open period duration range
c  These are now both under idtype=2 and calcs modified substantially
c  Old version, for 'following interval only', actually gave 'preceding
c   interval only'! -now corrected.
c
c TO DO:
c  NEED special version of corcal that uses ygap() for this =CORCALS
c	(1) PDF of open PERIOD given adjacent shut time
c	(2) PDF of open PERIOD given adjacent open period length
c	(3) mean indiv open time given adjacent indiv open time
c		(also need to add to CDISTJ, mean open period given
c		adjacent shut time)
c 	(4) Need to check that above things work when restricted
c		to specified amplitude range
c
c NEW NOTES:
c (1) Altered for point amp histo (03/04/94 03:15pm); NB Yval not defined
c	here, unlike other histos. Data for point amp fit is in nbin, freq(), etc
c	which are assigned in SCVDU after relevant frequencies and number of
c	bins is decided.
c
c Use DEBTYP for debugging
c	subroutine DEBTYP(nval,n1,n2,tint,ampl,iprops,jval,j1,j2,yval,
c     & prnt,nintt,ndimy)
c (old call: DEBTYP(nval,n1,n2,tint,iampl,acal,jval,j1,j2,yval,prnt)
c To print values of intervals and amps for debugging in EKDIST
c Nval,jval used only to indicate no of values when i1,i2 asked for
c  here (n1=0 or j1=0), otherwise ignored.
c (1)If n1>0 in call then TINT,AMP values from n1 to n2 are printed
c without asking for values here (and without title)
c (2) If n1=0 in call then asks which elements of TINT, AMP to type
c (3) If n1<0 in call then do only Jval part
c (4) Print of Yval() similarly controlled by Jval,j1,j2

c 05/28/89 01:16pm Lahey version: debug no longer an arg
c note: ID=IDTYPE
C SUBROUTINE FOR EKDIST TO GET Y VALUES AND COMPUTE HISTOGRAM
C FROM THEM
C
C
c Define functions
	pon()=slock()
	debug()=caplock()
	dubious(idum)=BTEST(iprops(idum),0)	!ampl(i) was dubious (bit 0 set ='1')
	badint(idum)=BTEST(iprops(idum),3)	!tint(i) was unusable (bit 3='8' set)
c
	deb=debug()
c
c22	print 231
c231	format(' (1) Distribution of open period and following shut time')
c	print 232
c232	format(' (2) Distribution of open period and preceding shut time')
c	print 233
c233	format(' (3) Distribution of shut time and following shut time')
c	print 234
c234	format(' (4) Distribution of shut time and preceding shut time')
c	print 235
c235	format(
c     &' (5) Distribution of open period and following open period')
c	print 236
c236	format(
c     &' (6) Distribution of open period and preceding open period')
c==========
	id1=1
22	print 231
231	format(' (1) Distributions of open period-shut times')
	print 232
232	format(' (2) Distributions of shut time-shut time')
	print 233
233	format(' (3) Distributions of open period-open period')
	print 2331
2331	format(' (4) No more bivariate distributions')
	print 230,id1
230	format(' Type of distribution [',i2,'] = ')
	call INPUTi(id1)
	if(id1.eq.4) goto 999
	if(id1.lt.1.and.id1.gt.3) goto 22
	if(id1.eq.1) then
	   text1='open period'
	   text2='shut time  '
	else if(id1.eq.2) then
	   text1='shut time  '
	   text2='shut time  '
	else if(id1.eq.2) then
	   text1='open period'
	   text2='open period'
	endif
	id2=3
	text(1)=
     &' (1) '//charnb(text1)//' and following '//charnb(text2)
	text(2)=
     &' (2) '//charnb(text1)//' and preceding '//charnb(text2)
	text(3)=
     &' (3) '//charnb(text1)//' and both following and preceding '
     &	//charnb(text2)
	text(4)=
     &' (4) '//charnb(text1)//' and following '//charnb(text2)//' MINUS'
	text(5)=
     &  '      '//charnb(text1)//' and preceding '//charnb(text2)
238	print 234,text(1)
234	format(1x,a50)
	print 235,text(2)
235	format(1x,a50)
	print 236,text(3)
236	format(1x,a50)
	print 237,text(4),text(5)
237	format(1x,a50,/,a50)
	print 230,id2
	call INPUTi(id2)
c
	print 1080
	if(pon()) write(7,1080)
	if(discprt) write(8,1080)
1080	format(' ------------------------------------------------',/,
     &       '     BIVARIATE DISTRIBUTIONS')
	if(id2.eq.1) then
	   if(pon()) write(7,234) text(1)
	   if(discprt) write(8,234) text(1)
	else if(id2.eq.2) then
	   if(pon()) write(7,235) text(2)
	   if(discprt) write(8,235) text(2)
	else if(id2.eq.3) then
	   if(pon()) write(7,236) text(3)
	   if(discprt) write(8,236) text(3)
	else if(id2.eq.4) then
	   if(pon()) write(7,237) text(4),text(5)
	   if(discprt) write(8,237) text(4),text(5)
	else
	   call BELL(1)
	   goto 238
	endif
c
c Now define xval, yval arrays that contain the two variables to be used
c For id1=1, id1=1, x(i)=open period, y(i)=shut time that follows it
c  If either interval is set unusable, then look for next good open period
c  and start from there
c For id1=1, id2=2, xval(i)=open period, y(i)=shut time that precedes it
c
c
	exass=.false.
	if(iscan.eq.-1003.or.iscan.ge.100) then
	   print 2263	!iscan=100 for new version
2263	   format(' Exclude openings with ''dubious'' amps [Y] ? ')
	else
	   print 2261
2261	   format(' Exclude openings with ''assumed'' amps [Y] ? ')
	endif
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).ne.'N') exass=.true.
2251	continue
c
	call PDUB(exass,.false.,iscan,pon())
	if(exass.and.id1.ne.2) print 2273
	if(exass.and.pon().and.id1.ne.2) write(7,2273)
	if(exass.and.discprt.and.id1.ne.2) write(8,2273)
2273	format(' (i.e. ANY dubious amp excludes whole open period)')
c	print 1080
c	if(pon()) write(7,1080)
c	if(discprt) write(8,1080)
c1080	format(' ------------------------------------------------')
c
C Subsection of OHIST to calculate lengths of contiguous open periods
c
c Now locate all open periods
	ifst=1
	j=0   !counts periods in A (=subset of open states)
c  Look for start of a group of openings i.e. any opening that has
c  defined duration (i.e. usable).  A single unusable opening in a group
c  makes its length undefined so it is excluded.
c NEW VERSION -ENSURES EACH OPEN PERIOD STARTS WITH SHUT-OPEN TRANSITION
c Find start of a group (open period) -valid start must have a good shut
c time followed by a good opening -if a bad opening is found as first (or
c any later) opening then the open period is abandoned altogether, and the
c next good shut time sought as start for next open period, but for the
c purposes of identifying the nth open period, rejected ones must be counted
c as an open period even though their length is undefined.
c
c Modif 10/29/95 02:02pm to do open period/gap means too; allocate array
c ygap(i) to store length of gap that follows open period #i
c
	if(allocated(ygap)) DEALLOCATE(ygap)
	ALLOCATE(yval(nint),ygap(nint))
	ndimy=nint 	!for debtyp
	do i=1,nint
	   yval(i)=0.0
	enddo
C
	deb=debug()
	ifst=1
	j=0   !counts periods in A (=subset of open states)
247	continue
	do while (ifst.le.nint-1)
	   if(mod(ifst-1,50).eq.0) deb=debug()
	   if(deb) then	!print first in group
		print 502,ifst,tint(ifst),ampl(ifst),iprops(ifst)
      	if(discprt) write(8,502)ifst,tint(ifst),ampl(ifst),
     &		iprops(ifst)
502	      format(i6,g12.5,g12.5,i5)
	   endif
	   a1=ampl(ifst)
	   a2=ampl(ifst+1)
c	   if(.not.conam1) then
	      open1=a1.ne.0
	      open2=a2.ne.0
	      shut1=.not.open1
c	   else
c		if(isol) then
c		   shut1=a1.eq.0.	!must start from shut
c	         open2=(A2.ge.amplo).and.(A2.le.amphi)
c		else
c	         open1=(A1.ge.amplo).and.(A1.le.amphi)
c	         shut1=.not.open1
c	         open2=(A2.ge.amplo).and.(A2.le.amphi)
c		endif
c	   endif
	   setbad1=badint(ifst)		!tint(i) was unusable
	   setbad2=badint(ifst+1)		!tint(i) was unusable
	   bad1=setbad1.or.(dubious(ifst).and.exass)
	   bad2=setbad2.or.(dubious(ifst+1).and.exass)
	   bad=.false.
	   if(bad1.or.bad2) then
		ifst=ifst+1
		n=n+1      		!count this as nth open period, although not usable
		bad=.true.
		goto 247  		!look for next good start
	   else if(shut1.and.open2) then
		n=n+1 		!count this as nth open period
		if(deb) then
		   print 503,ifst+1
		   if(discprt) write(8,503) ifst+1
503		   format(
     &	' Interval #',i6,' is first in putative open period')
	      endif
		goto 248		!valid 'start of open period' now found
	   endif
	   ifst=ifst+1
	enddo
c if this point reached, all intervals inspected
	j=j-1
	goto 992	!all intervals inspected
c Valid 'start of open period' found now -get the rest
248	continue
	j=j+1		!count number of open periods found (reversed if bad)
	ifst=ifst+1		!index of first opening in open period just started
	Yval(j)=tint(ifst)	!length of this opening
	k=ifst+1	!index of interval that follows the first opening
c====replacement for 'find rest'
	bad=.false.
c Now find the rest of the openings in this open period
	do while (k.le.nint)
	   a1=ampl(k)
c==	   if(conam1) open1=(A1.ge.amplo).and.(A1.le.amphi)	!use amp constraint
c Go to 249 when end of group found.
c (1) If no amplitude constraint group ends when ANY shut period found
c    (whether usable or unusable)
c (2) If amplitude constrained then
c	(a) ALL: (isolate=2) group ends when any amplitude outside
c	    the specified range is found.
c	(b) ISOLATED: (isolate=1) group ends validly by transition from
c		within window to shut level, but transition from within
c		window to open level outside window rejects open period
c Whole group is set bad, and so rejected, if (1) contains any unusable
c opening (2) when, if exass=true, it contains any opening with dubious
c amplitude OR if the interval that ends the group has 'dubious'amp.
c=	   if(.not.conam1) then
		open1=a1.ne.0
		if(exass.and.dubious(k)) bad=.true.
		if(.not.open1) goto 249		!valid end of group found
c=	   else		!conam1
cc          valid end of open period depends on whether isolated only req
cc          If isolated required then either (a) next interval is shut ->valid
cc		isolated opening, or (b) it is open and in same window so open
cc		period continues, or (c) it is open but outside window so not
cc		an isolated opening -set the period bad before finishing it off
c		if(isol) then
c		   shut1=(a1.eq.0)				!valid end
c		   open1=.not.shut1
c		   if(open1.and.(a1.lt.amplo.or.a1.gt.amphi)) then
c			bad=.true.	 !not isolated
c		   endif
c		else
c		   shut1=(a1.lt.amplo.or.a1.gt.amphi)	!anywhere out of window
c		endif
c	      if(exass.and.dubious(k)) bad=.true.c
cc		if(a1.eq.0.) goto 249	!valid end of burst found
c		if(shut1) goto 249		!valid end of group found
c=	   endif
	   YVAL(j)=YVAL(j)+tint(k)	!add any subsequent openings to same YVAL
	   setbad=badint(k)		!tint(k) was unusable
	   if(setbad) bad=.true.	!this group bad, but finish anyway
	   k=k+1
	enddo
	j=j-1
	goto 992	!data finished before end found, so do not use
c
c Group completed
249	continue
	if(bad) nbad1=nbad1+1	!count # of above case
c====end new version
	if(.not.deb) goto 50
c print the group for debug
	call DEBTYP(0,Ifst,k-1,tint,ampl,iprops,
     & 0,-1,-1,yval,pon(),nintt,ndimy)
c print interval that follows group
	call DEBTYP(0,k,k,tint,ampl,iprops,
     & 0,-1,-1,yval,pon(),nintt,ndimy)
c if good, print yval
	if(.not.bad) call DEBTYP(0,-1,-1,tint,ampl,iprops,
     & 0,j,j,yval,pon(),nintt,ndimy)	!print YVAL(j)
50	continue
c==	ifst=k+1	!another group completes (j updated above)
	ifst=k	!so do not skip past shut time needed to start new group
c	if(conam1) then	!can't define ygap() when amp constrained
cc=	   if(bad) j=j-1	!if bad do not use. YVAL(j) gets overwritten
c	   if(bad) then
c		j=j-1	!if bad do not use. YVAL(j) gets overwritten
c		if(deb) then
c		   print 504
c		   if(discprt) write(8,504)
c504		   format('&Open period abandoned',/)
c		endif
c	   endif
c	   if(ifst.lt.nint) goto 247	!look for start of new group
c	else
	   ygap(j)=TINT(k)   !record length of shut time following open period #j
	   if(ampl(k).ne.0) then
		print 700,k,tint(k),ampl(k),iprops(k)
700		format(' YGAP ERROR:',i5,2g13.6,i3)
	   endif
	   if(deb) then
		print 701,j,ygap(j)
		if(discprt) write(8,701) j,ygap(j)
701		format(16x,'  Ygap(',i4,') = ',g13.6,/)
	   endif
	   if(bad) then
		j=j-1	!if bad do not use. YVAL(j) gets overwritten
		if(deb) then
		   print 504
		   if(discprt) write(8,504)
504		   format(' Open period abandoned')
		endif
	   endif
	   if(ifst.lt.nint) goto 247	!look for start of new group
c If ifst=nint then end here, but first check that the last gap, tint(k),
c is OK -it will often be set bad, in which case, for the purpose of
c correlations in which 'following gap' is needed, we must exclude
c the open period that preceded it too.
	   if(badint(k)) j=j-2
c	endif
C N.B. does not return if ifst=nint, as if tint(nint) is start of a
C group there is no way to tell if this group has been completed-
C (the next interval might have been open too)
C  NOTE that NYVAL=naset calc here may not be identical with that
C found in calc of burst distributions because latter includes
C only those open periods that are part of valid bursts (no bad
C openings and proper end)
992	nyval=j
c Now have all open periods in yval(), and shut times between them in ygap()
c Note: specified gap is ygap(i); the open period preceding this is
c yval(i) [yval(i-lag+1) in general], and the open period following
c the specified gap is in yval(i+1) [yval(i+lag) in general].
c NB ygap not defined when amplitude constrained so omit bits that need ygap
c	Find max and min
c
	omin=1.e37
	omax=-1.e37
	smin=1.e37
	smax=-1.e37
c Only max needed for logbin, xmin, ymin needed only for fixax (NB to get min
c must skip unusable ones that are set to -1.)
	do i=1,nyval
	   if(yval(i).lt.omin) omin=yval(i)
	   if(yval(i).gt.omax) omax=yval(i)
	   if(ygap(i).lt.smin) smin=ygap(i)
	   if(ygap(i).gt.smax) smax=ygap(i)
c check for errors (exclude -1. for 'set unusable')
	   if(yval(i).lt.treso.and.yval(i).gt.-0.9) print 60,i,yval(i)
60	   format(' ERROR: yval(',i6,') = ',g13.6)
	   if(ygap(i).lt.tresg.and.ygap(i).gt.-0.9) print 61,i,ygap(i)
61	   format(' ERROR: ygap(',i6,') = ',g13.6)
	enddo
	if(id1.eq.1) then
	   xmin=smin
	   xmax=smax
	   ymin=omin
	   ymax=omax
	else if(id1.eq.2) then
	   xmin=smin
	   xmax=smax
	   ymin=smin
	   ymax=smax
	else if(id1.eq.3) then
	   xmin=omin
	   xmax=omax
	   ymin=omin
	   ymax=omax
	endif
c Fix axis limits
	xmin1=xmin	!save original values
	xmax1=xmax
	ymin1=ymin
	ymax1=ymax
c Get new xmax etc
	call FIXAX(xmin1,xmax1,xmin,xmax,xtic,0)	!always non-log
	call FIXAX(ymin1,ymax1,ymin,ymax,ytic,0)
c
c Decide bin boundaries
	tres=max(treso,tresg)	!for now
	nbmax=500
	print 2091,tresg
2091	format(' X axis to start at X [',f8.5,' ms] = ')
	xaxis(1)=tresg
	call INPUTr(xaxis(1))
c
	if(xaxis(1).le.0.) then
	   call BELL(3)
	   print 3031,xaxis(1)
3031	   format(' Can not start log scale at t = ',g11.4,/,
     &   ' New starting value (e.g. 0.01) = ')
	   call INPUTr(xaxis(1))
	endif
c	if(nyval.le.1000) nbdec=4				!default
c	if(nyval.gt.1000.and.nyval.le.4000) nbdec=5		!default
c	if(nyval.gt.4000.and.nyval.le.10000) nbdec=6	!default
c	if(nyval.gt.10000) nbdec=8   				!default
	nbdec=4		!default
1048	continue
	print 1041,nbdec
1041	format(' Number of bins/decade [',i3,'] = ')
	call INPUTi(nbdec)
c
	call LOGBIN(xaxis,nbmax,tres,nbinx,nbdec,xwbasex,xmax)
c Allocate xmid, and define it
c NB all values except xybase are non-log at this stage -this is what is needed
c for sorting into bins, but GPLOT3D needs log values for xmid,ymid
	ALLOCATE(xmid(nbinx))
	do i=1,nbinx
	   xmid(i)=0.5*(alog10(xaxis(i))+alog10(xaxis(i+1)))   !midpoint of bin
	enddo
c
	print 2092,treso
2092	format(' Y axis to start at Y [',f8.5,' ms] = ')
	yaxis(1)=treso
	call INPUTr(yaxis(1))
	call LOGBIN(yaxis,nbmax,tres,nbiny,nbdec,xwbasey,ymax)
c
	lag=1
	print 65,lag
65	format(' Lag [',i3,'] = ')
	call INPUTi(lag)
	print 69
69	format(' Calculating distribution . . .')
c Allocate ymid, and define it
	ALLOCATE(ymid(nbiny))
	do i=1,nbiny
	   ymid(i)=0.5*(alog10(yaxis(i))+alog10(yaxis(i+1)))   !midpoint of bin
	enddo
c Allocate x,y arrays and copy the relevant open/shut times to them, for the
c distribution that is required
c For id=1, x(i)=open period, y(i)=shut time that follows it (with lag=l)
c Note: for in ygap(i);
c	the open period preceding this is yval(i), or yval(i-lag+1) in general
c	and the open period following it is in yval(i+1), or yval(i+lag) in general
c OR for open period in yval(i)
c	the gap that follows it is in ygap(i)   -or ygap(i+lag-1) in general
c	the gap that precedes it is in ygap(i-1) -or ygap(i-lag) in general
c
	if(id2.eq.3) then		!both preceding and following
	   ALLOCATE(x(2*nyval),y(2*nyval))
	else
	   ALLOCATE(x(nyval),y(nyval))
	endif
c
c Now get x,y values
c In case of id2=4 (difference between two pdfs) we need to subtract
c the z(i,j) values, or smoothed znew(i,j) values, for the two pdfs
c so go through once (with first=.true.) to get 'following', and store
c the z(i,j), or znew(i,j), values in zf(i,j) ('f' for following).  Then
c go through again (with first=.false.) and get z(i,j) for 'preceding'
c and calculate final z(i,j)=zf(i,j)-z(i,j)
c Check lag is within limits and count actual number of pairs of values
	id22=id2
	first=.true.
100	continue	!return here for 2nd pdf if id2=4
	if(id2.eq.4) then
	   if(first) then
		id22=1	!following
	   else
		id22=2	!preceding
	   endif
	endif
	if(id1.eq.1.and.id22.eq.1) then
	   xtitle='shut time (following)'
	   ytitle='open period'
	   title1='f(open, following shut)'
	   k=0
	   do i=1,nyval
		ix=i+lag-1
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=ygap(iy)
		   y(k)=yval(ix)
c check for errors
c		   if(x(k).lt.treso.and.x(k).gt.-0.9) print 621,k,x(k)
c62		   format(' ERROR: x(',i6,') = ',g13.6)
c		   if(y(k).lt.tresg.and.y(k).gt.-0.9) print 631,k,y(k)
c63		   format(' ERROR: y(',i6,') = ',g13.6)
		endif
	   enddo
	   nval=k
	else if(id1.eq.1.and.id22.eq.2) then
c	the gap that precedes it is in ygap(i-1) -or ygap(i-lag) in general
	   xtitle='shut time (preceding)'
	   ytitle='open period'
	   title1='f(open, preceding shut)'
	   k=0
	   do i=1,nyval
		ix=i-lag
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=ygap(iy)
		   y(k)=yval(ix)
cc check for errors
c		   if(x(k).lt.treso.and.ygap(i).gt.-0.9) print 62,k,x(k)
cc62		   format(' x(',i6,') = ',g13.6)
c		   if(y(k).lt.tresg) print 63,k,y(k)
cc63		   format(' y(',i6,') = ',g13.6)
		endif
	   enddo
	   nval=k
	else if(id1.eq.1.and.id22.eq.3) then		!both
	   xtitle='shut time (adjacent)'
	   ytitle='open period'
	   title1='f(open, adjacent shut)'
	   k=0
	   do i=1,nyval 		!following
		ix=i+lag-1
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=ygap(iy)
		   y(k)=yval(ix)
cc check for errors
c		   if(x(k).lt.treso.and.x(k).gt.-0.9) print 621,k,x(k)
c621		   format(' x(',i6,') = ',g13.6)
c		   if(y(k).lt.tresg.and.y(k).gt.-0.9) print 631,k,y(k)
c631		   format(' y(',i6,') = ',g13.6)
		endif
	   enddo
	   do i=1,nyval		!go through again to add preceding
		ix=i-lag
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=ygap(iy)
		   y(k)=yval(ix)
c check for errors
c		   if(x(k).lt.treso.and.x(k).gt.-0.9) print 621,k,x(k)
c62		   format(' x(',i6,') = ',g13.6)
c		   if(y(k).lt.tresg.and.y(k).gt.-0.9) print 631,k,y(k)
c63		   format(' y(',i6,') = ',g13.6)
		endif
	   enddo
	   nval=k
	else if(id1.eq.2.and.id22.eq.1) then
	   xtitle='shut time (following)'
	   ytitle='shut time'
	   title1='f(shut, following shut)'
	   k=0
	   do i=1,nyval
		ix=i+lag
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=ygap(iy)
		   y(k)=ygap(ix)
c check for errors
c		   if(x(k).lt.treso.and.x(k).gt.-0.9) print 621,k,x(k)
c62		   format(' x(',i6,') = ',g13.6)
c		   if(y(k).lt.tresg.and.y(k).gt.-0.9) print 631,k,y(k)
c63		   format(' y(',i6,') = ',g13.6)
		endif
	   enddo
	   nval=k
	else if(id1.eq.2.and.id22.eq.2) then
	   xtitle='shut time (preceding)'
	   ytitle='shut time'
	   title1='f(shut, preceding shut)'
	   k=0
	   do i=1,nyval
		ix=i-lag
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=ygap(iy)
		   y(k)=ygap(ix)
c check for errors
c		   if(x(k).lt.treso.and.x(k).gt.-0.9) print 621,k,x(k)
c62		   format(' x(',i6,') = ',g13.6)
c		   if(y(k).lt.tresg.and.y(k).gt.-0.9) print 631,k,y(k)
c63		   format(' y(',i6,') = ',g13.6)
		endif
	   enddo
	   nval=k
	else if(id1.eq.2.and.id22.eq.3) then		!both
	   xtitle='shut time (adjacent)'
	   ytitle='shut time'
	   title1='f(shut, adjacent shut)'
	   k=0
	   do i=1,nyval	!first following
		ix=i+lag
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=ygap(iy)
		   y(k)=ygap(ix)
c check for errors
c		   if(x(k).lt.treso.and.x(k).gt.-0.9) print 621,k,x(k)
c62		   format(' x(',i6,') = ',g13.6)
c		   if(y(k).lt.tresg.and.y(k).gt.-0.9) print 631,k,y(k)
c63		   format(' y(',i6,') = ',g13.6)
		endif
	   enddo
	   do i=1,nyval		!add preceding
		ix=i-lag
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=ygap(iy)
		   y(k)=ygap(ix)
c check for errors
c		   if(x(k).lt.treso.and.x(k).gt.-0.9) print 621,k,x(k)
c62		   format(' x(',i6,') = ',g13.6)
c		   if(y(k).lt.tresg.and.y(k).gt.-0.9) print 631,k,y(k)
c63		   format(' y(',i6,') = ',g13.6)
		endif
	   enddo
	   nval=k
	else if(id1.eq.3.and.id22.eq.1) then
	   xtitle='open period (following)'
	   ytitle='open period'
	   title1='f(open, following open)'
	   k=0
	   do i=1,nyval
		ix=i+lag
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=yval(iy)
		   y(k)=yval(ix)
c check for errors
c		   if(x(k).lt.treso.and.x(k).gt.-0.9) print 621,k,x(k)
c62		   format(' x(',i6,') = ',g13.6)
c		   if(y(k).lt.tresg.and.y(k).gt.-0.9) print 631,k,y(k)
c63		   format(' y(',i6,') = ',g13.6)
		endif
	   enddo
	   nval=k
	else if(id1.eq.3.and.id22.eq.2) then
	   xtitle='open period (preceding)'
	   ytitle='open period'
	   title1='f(open, preceding open)'
	   k=0
	   do i=1,nyval
		ix=i-lag
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=yval(iy)
		   y(k)=yval(ix)
c check for errors
c		   if(x(k).lt.treso.and.x(k).gt.-0.9) print 621,k,x(k)
c62		   format(' x(',i6,') = ',g13.6)
c		   if(y(k).lt.tresg.and.y(k).gt.-0.9) print 631,k,y(k)
c63		   format(' y(',i6,') = ',g13.6)
		endif
	   enddo
	   nval=k
	else if(id1.eq.3.and.id22.eq.3) then		!both
	   xtitle='open period (adjacent)'
	   ytitle='open period'
	   title1='f(open, adjacent open)'
	   k=0
	   do i=1,nyval		!first following
		ix=i+lag
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=yval(iy)
		   y(k)=yval(ix)
c check for errors
c		   if(x(k).lt.treso.and.x(k).gt.-0.9) print 621,k,x(k)
c62		   format(' x(',i6,') = ',g13.6)
c		   if(y(k).lt.tresg.and.y(k).gt.-0.9) print 631,k,y(k)
c63		   format(' y(',i6,') = ',g13.6)
		endif
	   enddo
	   do i=1,nyval		!add in preceding
		ix=i-lag
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=yval(iy)
		   y(k)=yval(ix)
c check for errors
c		   if(x(k).lt.treso.and.x(k).gt.-0.9) print 621,k,x(k)
c62		   format(' x(',i6,') = ',g13.6)
c		   if(y(k).lt.tresg.and.y(k).gt.-0.9) print 631,k,y(k)
c63		   format(' y(',i6,') = ',g13.6)
		endif
	   enddo
	   nval=k
	endif
c
	if(id2.eq.4) then
	   if(id1.eq.1) then
		xtitle='shut time'
		ytitle='open period'
		title1='f(open,following shut) - f(open, preceding shut'
	   else if(id1.eq.2) then
		xtitle='shut time'
		ytitle='shut time'
		title1='f(shut,following shut) - f(shut, preceding shut'
	   else if(id1.eq.3) then
		xtitle='open period'
		ytitle='open period'
		title1='f(open,following open) - f(open, preceding open'
	   endif
	endif
c
c Now sort values into bins
c In this case we have n pairs of x,y values -x=length of open period,
c and y=length of following shut period (not nx, ny independent
c observation).  Need to locate which of the nbinx*nbiny bins each
c pair falls into
c Initialise frequencies
	ALLOCATE(z(nbinx,nbiny))
	if(allocated(badval)) DEALLOCATE(badval)
	ALLOCATE(badval(nbinx,nbiny))
	ndx=nbinx	!dimension of z,bad
	ndy=nbiny	!dimension of z,bad
	n=nval
	do i=1,nbinx
	   do j=1,nbiny
		z(i,j)=0.0
		badval(i,j)=.false.
	   enddo
	enddo
	nxlo=0
	nxhi=0
	nylo=0
	nyhi=0
c Sort into bins
	do k=1,n
	   xv=x(k)
	   yv=y(k)
c Check for values outside range of bins
	   if(xv.lt.xaxis(1)) nxlo=nxlo+1
	   if(xv.ge.xaxis(nbinx+1)) nxhi=nxhi+1
	   if(yv.lt.yaxis(1)) nylo=nylo+1
	   if(yv.ge.yaxis(nbiny+1)) nyhi=nyhi+1
c
	   do i=1,nbinx
		do j=1,nbiny
		   if(xv.ge.xaxis(i).and.xv.lt.xaxis(i+1).and.
     &		yv.ge.yaxis(j).and.yv.lt.yaxis(j+1)) then
			z(i,j)=z(i,j)+1.0
		   endif
		enddo
	   enddo
	enddo
c
c For difference between pdfs, keep the first one now, and go back for the second
	if(id2.eq.4) then
	   if(first) then
		ALLOCATE(zf(nbinx,nbiny))
		do i=1,nbinx
		   do j=1,nbiny
			zf(i,j)=z(i,j)		!save following
		   enddo
		enddo
		DEALLOCATE(z)
		first=.false.
		goto 100		!get preceding
	   else
		do i=1,nbinx
		   do j=1,nbiny
			z(i,j)=zf(i,j)-z(i,j)	 !get difference between pdfs
		   enddo
		enddo
		DEALLOCATE(zf)
	   endif
	endif
c Do inverse distance smoothing? or put this into GPLOT3D -better so
c effect can be seen more easily
	ALLOCATE(znew(nbinx,nbiny))
	power=1.0
	ans='Y'
	call DCASK('Use 3D smoothing before display',ans,ans)
	if(ans.eq.'Y') then
	   call SMOOTH3D(z,znew,nbinx,nbiny,power,nbinx,nbiny)
	else
	   do i=1,nbinx
		do j=1,nbiny
		   znew(i,j)=z(i,j)
		enddo
	   enddo
	endif
c
c Take square root
	if(id2.ne.4) then
	   do i=1,nbinx
		do j=1,nbiny
		   znew(i,j)=sqrt(znew(i,j))
		enddo
	   enddo
	endif
c
	call GINO
	idrawt=1
	idrawx=1
	idrawy=1
	idrawz=1
	icol(23)=0		      !in common for gplot3d
	icol(25)=9
	icol(71)=7
	do i=1,nbinx
	   do j=1,nbiny
	     badval(i,j)=.false.
	   enddo
	enddo
c	xtitle='this is x'
c	ytitle='this is y'
	ans='N'
	call DCASK('Show bivariate distribution',ans,ans)
	if(ans.eq.'Y') then
	   if(id2.ne.4) then
		ztitle='Square root(number)'
	   else
		ztitle='Number'
	   endif
c##################### Ioana
c  22/02/99
c	you can change them !!!!
	   kcol=2
	   posneg=.true.
	   isetcol=2
	   call GPLOT3D(xmid,ymid,znew,badval,nbinx,nbiny,ndx,ndy,
     &    xtitle,ytitle,ztitle,title1,idrawt,idrawx,idrawy,idrawz,
     &       kcol,posneg,isetcol,qfile)
	endif
c
c
c If difference between two pdf plotted then z,znew do not contain the
c right things for dependency plots
	if(id2.eq.4) goto 99
c
	ans='Y'
	call DCASK('Show dependency plot',ans,ans)
	if(ans.eq.'N') goto 98
c
c Put x and y into bins for the univariate distributions
c Now do this using row and column totals, as in Magleby & Song
c (use smoothed values for this?)
c Smooth BEFORE calculation of dependency -smoothed values already
c available in znew -copy to z
	do i=1,nbinx
	   do j=1,nbiny
		z(i,j)=znew(i,j)
	   enddo
	enddo
c
	ALLOCATE(px(nbinx),py(nbiny))
	do i=1,nbinx	!for each row
	   s=0.0
	   do j=1,nbiny	!sum over columns
		s=s+z(i,j)
	   enddo
	   px(i)=s
	enddo
c
	do j=1,nbiny	!for each column
	   s=0.0
	   do i=1,nbinx	!sum over rows
		s=s+z(i,j)
	   enddo
	   py(j)=s
	enddo
c
	sx=0.0
	do i=1,nbinx
	   sx=sx+px(i)
	enddo
	sy=0.0
	do j=1,nbiny
	   sy=sy+py(j)
	enddo
c	print 64,sx,sy
c64	format(' sx, sy = ',2g13.6)	!should be equal!
c	pause
	entot=sx		!=sy too -total number
c Divide by total to get probabilities
	do i=1,nbinx
	   px(i)=px(i)/entot
	enddo
	do j=1,nbiny
	   py(j)=py(j)/entot
	enddo
c
	ALLOCATE(dep(nbinx,nbiny),depdif(nbinx,nbiny))
c NB must avoid divide by zero -should mark the bad values (note i,j, and
c fill in black in gplot3d?)
c	p0min=2.	!minimum value for denominator, pij0
c	p0min=0.1	!minimum value for denominator, pij0
	p0min=1.	!minimum value for denominator, pij0
	print 70,p0min
70	format(' Minimum value for expected number in bin [',f5.2,'] = ')
	call INPUTr(p0min)
	nbad=0
	do i=1,nbinx
	   do j=1,nbiny
		pij0=px(i)*py(j)*entot	!expected number if indep
		pij=z(i,j)			!number actually observed
		if(pij0.gt.p0min) then
		   dep(i,j)=(pij/pij0)-1.0	!dependency
		   badval(i,j)=.false.
		else
		   dep(i,j)=0.0
		   nbad=nbad+1
		   badval(i,j)=.true.
		endif
c
		depdif(i,j)=sqrt(pij)-sqrt(pij0)	!dependency difference
	   enddo
	enddo
	print 71,nbad,p0min
	if(discprt) write(8,71) nbad,p0min
71	format(1x,i4,
     & ' values of dep(i,j) set to 0 because denominator < ',f5.2)
c
c Now plot
	power=1.0
	ans='Y'
	call DCASK('Use 3D smoothing before display',ans,ans)
	if(ans.eq.'Y') then
	   call SMOOTH3D(dep,znew,nbinx,nbiny,power,nbinx,nbiny)
	else
	   do i=1,nbinx
		do j=1,nbiny
		   znew(i,j)=dep(i,j)
		enddo
	   enddo
	endif
	ztitle='dependency'
	title1='Dependency plot'
10	continue
c##################### Ioana
c  22/02/99
c	you can change them !!!!
	   kcol=2
	   posneg=.true.
	   isetcol=2
	call GPLOT3D(xmid,ymid,znew,badval,nbinx,nbiny,ndx,ndy,
     &    xtitle,ytitle,ztitle,title1,idrawt,idrawx,idrawy,idrawz,
     &       kcol,posneg,isetcol,qfile)
	ans='Y'
c	call DCASK('Re-display dependency plot',ans,ans)	!for debug
c	if(ans.eq.'Y') goto 10
c
98	continue
	ans='Y'
	call DCASK('Show dependency difference plot',ans,ans)
	if(ans.eq.'N') goto 99
	do i=1,nbinx
	   do j=1,nbiny
	     badval(i,j)=.false.
	   enddo
	enddo
	power=1.0
	ans='Y'
	call DCASK('Use 3D smoothing before display',ans,ans)
	if(ans.eq.'Y') then
	   call SMOOTH3D(depdif,znew,nbinx,nbiny,power,nbinx,nbiny)
	else
	   do i=1,nbinx
		do j=1,nbiny
		   znew(i,j)=depdif(i,j)
		enddo
	   enddo
	endif
	ztitle='dependency difference'
	title1='Dependency difference plot'
	   kcol=2
	   posneg=.true.
	   isetcol=2
	call GPLOT3D(xmid,ymid,znew,badval,nbinx,nbiny,ndx,ndy,
     & xtitle,ytitle,ztitle,title1,idrawt,idrawx,idrawy,idrawz,
     &       kcol,posneg,isetcol,qfile)

c end of dependency plot
c
99	continue
	if(allocated(px)) then
	   DEALLOCATE(px,py)
	endif
	if(allocated(dep)) then
	   DEALLOCATE(dep,depdif)
	endif
	if(allocated(yval)) then
	   DEALLOCATE(yval,ygap,xmid,ymid,x,y,z,znew)
	endif
	id1=4
	goto 22	!another?
c
999	continue
	if(allocated(px)) then
	   DEALLOCATE(px,py)
	endif
	if(allocated(dep)) then
	   DEALLOCATE(dep,depdif)
	endif
	if(allocated(yval)) then
	   DEALLOCATE(yval,ygap,xmid,ymid,x,y,z,znew)
	endif
	if(allocated(badval)) then
	   DEALLOCATE(badval)
	endif
c
	RETURN
	END


