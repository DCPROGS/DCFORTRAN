	subroutine CDIST1(tint,ampl,iprops,nint,nintt,ndimy,
     & yval,nyval,ohist,bhist,ID,ghist,ahist,pahist,obhist,
     & trise,iscan,treso,tresg,tres,idest,idsav,tcfac,nopoint,
     & revamp,wfreq,ylogap,yhigap,nvalgap,yloop,yhiop,nvalop,
     & nocor,isolate,idiskq,rms,freqs,noutlo,nouthi,
     & consamdef,adcfil,nfits,ifits,timsav,ibaselin,dfinter,
     & sfac2,npfilt,qfile,nsam,srate,irecfst,calfac,ioffset,
     & nfile,nset,jset,nmax,yhio,yhig)
c
c CDIST1=new version of CDIST0 for EKDIST for new SCAN files
c
c Modif 04/20/04 07:08am -yhio, yhig (upper limits for open/shut times
c for correlation) added to parameters, for .ini
c
c Major modif 09/22/00 03:24pm to use 2D arrays for tint() etc, for
c case where nset>1.  Parameters added nset, nmax
c
c Modif 02/04/98 08:17am to add distribution of length of open PERIODS
c that are adjacent to gaps in specified length range (id=4). Code
c for id=2 already identifies all open periods and gaps between them
c and don't want to change this, so allocate temporary array, yadj(ja), to
c hold values for this distribution, and copy it to Yval at the end
c
c Modif 10/03/97 10:31am to correct error that occurred with new calc of
c  open periods when amplitude range is specified
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
	real*4 TINT(nintt),ampl(nintt),yval(ndimy)
	real*4 wfreq(51200)		!for idealized point amp
	integer*1 iprops(nintt)
	dimension ylo(10),yhi(10)
	real*4 yloop(10),yhiop(10),ylogap(10),yhigap(10)	!for .ini
	ALLOCATABLE::ns0,ym0,sd0,ns1,ym1,sd1,pm0,pm1,sx0,sx1,sx2,sdp
	ALLOCATABLE::ns2,ym2,sd2,pm2
	real*4 ym0(:,:),sd0(:,:),ym1(:,:),sd1(:,:),pm0(:,:),pm1(:,:)
	real*4 sx0(:,:),sx1(:,:),sx2(:,:),sdp(:,:)
	real*4 ym2(:,:),sd2(:,:),pm2(:,:)
	integer ns0(:,:),ns1(:,:),ns2(:,:)
	real*4 sy0(10),syy0(10),sy1(10),syy1(10),sy2(10),syy2(10)
	integer ny0(10),ny1(10),ny2(10)
	ALLOCATABLE::ygap		!for new mean open period|adj gap calcs
	real*4 ygap(:)
	ALLOCATABLE::yadj		!for id=4 open periods adj to spec gap
	real*4 yadj(:)
	ALLOCATABLE::yval1	!for SORTR
	real*4 yval1(:)
	ALLOCATABLE::yb		!for debug check
	real*4 yb(:)
	LOGICAL OHIST,BHIST,GHIST,AHIST,pahist,OBHIST,nopoint
	LOGICAL CONAM1,OPEN,BAD,EXCSUB,gap,both,noprint,nocor
	logical exass,exass1,debug,caplock,revamp
	logical sbin,shist,sres,sexp
	logical btest,setbad,badint,dubious,good0,good1,good2
	logical open1,open2,shut1,setbad1,setbad2,bad1,bad2,isol
	logical allocated
	character*1 ans,UC
	character*11 cnum
c For VPLOT
	ALLOCATABLE XVAL2,YVAL2,XCAL,YCAL
	real XVAL2(:,:),YVAL2(:,:)		!for VPLOT
	real XCAL(:,:),YCAL(:,:)		!for VPLOT/VHIST
	ALLOCATABLE::ndat,icurvd,isym,ijoin,syms
	integer ndat(:),icurvd(:),isym(:),ijoin(:)
	real*4 syms(:)
	ALLOCATABLE::ncal,icurvc,iline
	integer ncal(:),icurvc(:),iline(:)
	ALLOCATABLE:: weight,icurvw
	real*4 weight(:,:)
	integer*4 icurvw(:)
	logical autplt,fitted,plotonly,interp,ivplot,doframe,landscap
	character*40 titlex,titley
	character*64 title1
c For consam and Patlak display
c	real*4 TINT0(nintt)
	real*8 timsav(nfits),dfinter
	integer*4 ifits(nfits)
	integer*2 ibaselin(nfits)
	logical consamdef,cjdat,newform,noconsam,exc0,include
	character cdate1*11,adctim1*8		!read from consam
	character adcfil*33			!path for consam file
	character title*76
	ALLOCATABLE::idata
	integer*2 idata(:)
c	real*4 Yfreq(-2047:2048)		!OK for plus/minus 20.4 pA
	real*4 freqs(-5000:5000)	!for Patlak OK for plus/minus 50 pA
c For ASCWRITE
	ALLOCATABLE::data
	real*4 data(:,:)
	character*20 colhead(30)
	character*70 text	!for ASCWRITE
	character*40 qfile
c
	common/adj/iadj		!for scvdu/cdist1
c
	logical discprt,deb,opcal
	common/dp/discprt
	common/sblk/sbin,shist,sres,sexp
	COMMON/OVLBLK/OBMAX,Jth,CONAM1,exass,exass1,AMPHI,AMPLO,
     & EXCSUB,Ith
c Need topen=overall mean open period in TCRITS/BLFUNC (calc in cdist1 at present
c from observed values rather than from fit)
	common/top/topen
C
c Define functions
	debug()=caplock()
	dubious(ix)=BTEST(iprops(ix),0)	!ampl(i) was dubious (bit 0 set ='1')
	badint(ix)=BTEST(iprops(ix),3)	!tint(i) was unusable (bit 3='8' set)
c
	deb=debug()
	if(shist) id=idsav
C NOW GET PARAMETERS FOR HISTOGRAMS
c	do i=1,nintt
	do i=1,ndimy
	   yval(i)=0.0	!needs to be initiallised for some burst distns
	enddo
C
C
	OHIST=.FALSE.
	BHIST=.FALSE.
	GHIST=.FALSE.
	AHIST=.FALSE.
	pahist=.false.		!point amp histo
	OBHIST=.FALSE.		!DIST=OPENINGS/BURST
	if(shist) goto 304	!Use prev CONAM?
	if(jset.gt.1) goto 304
	conam1=.false.
	exass=.false.
c yhio,yhig now from .ini
c	yhio=-1.		!longest open time for correlations
c	yhig=-1.		!longest shut time for correlations
304	continue
c
c Now fix for all cases other than first latencies
	IF(IDEST.EQ.1) GOTO 202		!AMPLITUDES
	IF(IDEST.EQ.2) GOTO 201		!GAPS
	IF(IDEST.EQ.3) GOTO 200		!OPEN TIMES
C
C NOW DEFINE ARRAY YVAL CONTAINING VARIABLES FOR WHICH
C HISTOGRAM IS TO BE PLOTTED
C
C SECTION FOR DURATIONS OF OPENINGS
200	tres=treso	!in ms
	if(shist) goto 235
	if(jset.gt.1) goto 235
	id=2		!default
	goto 22
233	print 230,id
230	format(' Type of distribution (1,2,3,4,5; 0=HELP) [',i2,'] = ')
	call INPUTi(ID)
	if(id.eq.1) then
	   print 2
2	   format(
     &   ' There may be many small amplitude transitions during one',/,
     &   ' ''opening'', each of which will count as an individual ',/,
     &   ' opening, so generally better to look at ''open periods''')
	   ans='N'
	   call DCASK(
     &   'Are you sure you want individual open times',ans,ans)
	   if(ans.eq.'N') goto 200
	endif
	if(id.gt.0.and.id.le.5) goto 235
22	continue
	print 3
3	format(/,' Options for distribution type:')
	print 231
231	format(' (1) Duration of individual apparent openings')
	print 232
232	format(' (2) Duration of contiguous open times (open PERIODS)',/,
     &'   and mean length of open periods that are adjacent to shut',/,
     &'   times or open periods with duration in specified ranges')
	print 242
242	format(
     &' (3) Distribution of lengths of individual app. openings that',
     & /,'    are adjacent to a gap with duration in a specified range')
	print 243
243	format(
     &' (4) Distribution of lengths of apparent open PERIODS that',
     & /,'    are adjacent to a gap with duration in a specified range')
	print 244
244	format(
     &' (5) Distribution of lengths of apparent open PERIODS that',
     & /,'    bounded on BOTH sides by shut times in a specified range')
	goto 233
235	OHIST=.true.
	idsav=id
	if(shist) goto 2251	!^^?omit if conam etc to be asked for
	if(jset.gt.1) goto 2251
	amplo=-10000.0	!default is to include openings of any amplitude
	amphi=10000.
	exass=.false.
	ans='N'
	call DCASK('Specify amplitude range for openings',ans,ans)
	conam1=ans.eq.'Y'
	if(conam1.and.id.eq.3.or.id.eq.4.or.id.eq.5) then
	   call BELL(1)
	   print 245
245	   format(
     & ' CANNOT DEFINE SHUT TIMES WHEN AMPLITUDES CONSTRAINED',/,
     & ' SO NO AMPLITUDE RANGE SPECIFIED')
	   conam1=.false.
	endif
	if(conam1) then
	   print 2271
2271	   format(' Define amplitude range:')
	   print 227
227	   format('  Low amp, high amp (pA with sign) = ')
	   call INPUT2r(amplo,amphi)
	   if(amphi.lt.amplo) then
		a1=amphi	!swap
		amphi=amplo
		amplo=a1
	   endif
c if id=2 ask whether want isolated open periods only
	   if(id.eq.2) then
273		print 2272,isolate
2272		format(
     &   ' (1) Use isolated open periods only',/,
     &   ' (2) Use all open periods in the window',/,
     &   ' (3) Help (more details)',/,
     &   '   Option number [',i2,'] = ')
		call INPUTi(isolate)
		if(isolate.eq.3) then
		   print 274
274		   format(
     &' (1) Isolated open periods',/,
     &'  Open period in specified amp window must (a) start by',/,
     &'  transition from shut level to a level in the window, (b) then',
     &/,'  any number of open/open transitions as long as all open',/,
     &'  levels are in the window, (c) open period ends with transition'
     &/,'  to shut state (transition to any other open level outside',/,
     &'  the window causes rejection of value)')
		   print 275
275		   format(
     &' ',/,
     &' (2) All open periods in window',/,
     &'  (a) starts with a transition from ANY level outside the',/,
     &'  window to a level within the window,',/,
     &'  (b) then any number of open/open transitions as long as all',/,
     &'   open levels are in the window,',/,
     &'  (c) ends with transition to ANY level outside the window.',/)
		   goto 273
		endif
	   endif
	endif
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
	call PDUB(exass,shist,iscan,.false.)
	if(exass.and.(id.eq.2.or.id.eq.4)) print 2273
	if(exass.and.discprt.and.id.eq.2) write(8,2273)
2273	format(' (i.e. ANY dubious amp excludes whole open period)')
	if(conam1.and.discprt) write(8,25) amplo,amphi
	if(conam1.and.shist) print 25,amplo,amphi
25	format(/,' Openings with amplitudes beween ',f7.3,' and ',
     & f7.3,' pA only',/)
	print 1080
	if(discprt) write(8,1080)
1080	format(' -------------------------------------------------------')
	if(id.eq.1) goto 234	!indiv openings
	if(id.eq.2) goto 236	!open periods
	if(id.eq.3) goto 251	!openings next to specified gap
	if(id.eq.4) goto 236	!open periods next to specified gap
	if(id.eq.5) goto 236	!open periods bordered on BOTH sides by specified gap
c
c
c Section for id=1: Indiv openings
234	print 231		!title
	if(discprt) write(8,231)
      print 60
60	format(/,
     & ' Now get values (''Yval'') for the distribution',/,
     & ' To see what is being done, put CAP LOCK on now',/,
     & ' (press any key to continue)')
	call ANYKEY()
c
	j=0			!counts YVAL
c
	do 204 i=1,nint
	i1=i
	if(debug())call DEBTYP(0,i1,i1,tint,ampl,iprops,
     & 0,-1,-1,yval,.false.,nintt,ndimy)
	setbad=badint(i)		!tint(i) was unusable
c Exclude shut or unusable durations
	if(ampl(i1).eq.0.or.setbad) goto 204
	a1=ampl(i1)		!in pA, with sign
C
C Next line excludes openings with dubious amp when EXASS is true
	if(exass.and.dubious(i)) goto 204
c If current opening has not already been excluded by prev line then
c next test if amp is between specified limits -exclude if not
	if(conam1.and.(A1.lt.amplo.or.A1.gt.amphi)) goto 204
	j=j+1
	yval(j)=tint(i)
	if(debug()) call DEBTYP(0,-1,-1,tint,ampl,iprops,
     & 0,j,j,yval,.false.,nintt,ndimy)	!print YVAL(j)
204	continue	!end of i loop
c
	nyval=j
	goto 207
c
c
C id=2: subsection of OHIST to calculate lengths of contiguous open periods
c id=4 added 02/03/98 09:08pm for open periods next to gap with length in
c	 specified range
c  Also incorporate mean open period adjacent to open period in specified
c range here.  And also mean
c Also now do in the same bit, mean open period that is lag=n from
c specified adjacent gap range (indiv openings vs adj gap are still
c done below as option 4, for lag=1). Calculation is
c done both (a) counting every opening twice according to the gap
c that precedes it and that which follows it as Magleby- kept in sy0(),ny0()
c and (b) counting only the gap that FOLLOWS the opening.
c Open/shut correlation now done here, in CORCALS, rather than defining
c alternation open/shut times in yval (as prev used in CORCAL)
236	continue
c	print 2321		!title	-print after correlations
c	if(discprt) write(8,2321)
c2321	format(' (2) DURATION OF CONTIGUOUS OPEN TIMES (OPEN PERIODS)')
	if(conam1) then
	   if(isolate.eq.1) then
		print 274
		if(discprt) write(8,274)
	   else if(isolate.eq.2) then
		print 275
		if(discprt) write(8,275)
	   endif
	endif
c
	if(id.eq.4) then
	   print 238
238	   format(
     &	' Lower, upper limits for length of adjacent gap (ms) = ')
	   call INPUT2r(gaplo,gaphi)
	   if(gaphi.lt.gaplo) then
		a1=gaphi		!swap
		gaphi=gaplo
		gaplo=a1
	   endif
	   if(iadj.eq.0) iadj=3
	   print 126,iadj
126	   format(
     & ' (1) Include open period only when PRECEDED by specified gap',/,
     & ' (2) Include open period only when FOLLOWED by specified gap',/,
     & ' (3) Include open period if adjacent to specified gap',/,
     &   ' Option number [',i2,'] = ')
	   call INPUTi(iadj)
	   print 127,gaplo,gaphi
	   if(discprt) write(8,127) gaplo,gaphi
127	   format(
     & ' (4) Duration of apparent open periods that are adjacent',
     & /,'     to a gap with duration between ',g13.6,' and ',g13.6,
     & ' ms.')
	   if(discprt.and.iadj.eq.1) write(8,128)
128	   format(
     & ' Open period included only if specified gap PRECEDES it')
	   if(discprt.and.iadj.eq.2) write(8,129)
129	   format(
     &  ' Open period included only if specified gap FOLLOWS it')
	   if(discprt.and.iadj.eq.3) write(8,130)
130	   format(' Open period included if specified gap EITHER side')
c       allocate temporary array for values for the distribution
	   if(allocated(yadj)) DEALLOCATE(yadj)
	   ALLOCATE(yadj(nintt))
	   ja=0	!initialise index for yadj(ja)
	else if(id.eq.5) then
	   print 238
c238	   format(
c     &	' Lower, upper limits for length of adjacent gap (ms) = ')
	   call INPUT2r(gaplo,gaphi)
	   if(gaphi.lt.gaplo) then
		a1=gaphi		!swap
		gaphi=gaplo
		gaplo=a1
	   endif
	   print 260
260	   format(
     & ' (1) INCLUDE only open periods with this gap on BOTH sides',/,
     & ' (2) EXCLUDE open periods with this gap on BOTH sides',/,
     & ' Option number [1] = ')
	   iadj=1
	   call INPUTi(iadj)
	   if(iadj.eq.1) then
	      if(discprt) write(8,262) gaplo, gaphi
262	      format(
     &      ' Open period INCLUDED only if specified gap BOTH sides',/,
     &	' specified shut time between ',g13.6,' and ',g13.6,' ms')
	   else if(iadj.eq.2) then
	      if(discprt) write(8,131) gaplo, gaphi
131	      format(
     &      ' Open period EXCLUDED if specified gap BOTH sides',/,
     &	' specified shut time between ',g13.6,' and ',g13.6,' ms')
	   endif
	   if(allocated(yadj)) DEALLOCATE(yadj)
	   ALLOCATE(yadj(nintt))
	   ja=0	!initialise index for yadj(ja)
	endif
c
      print 60
c60	format(/,
c     & ' Now get values (''Yval'') for the distribution',/,
c     & ' To see what is being done, put CAP LOCK on now',/,
c     & ' (press any key to continue)')
	call ANYKEY()
c Now locate all open periods
	ifst=1
	j=0   !counts periods in A (=subset of open states)
c  Look for start of a group of openings i.e. any opening that has
c  defined duration (i.e. usable).  A single unusable opening in a group
c  makes its length undefined so it is excluded.
c=247	continue
c=	A1=ampl(ifst)
c=	open=a1.ne.0
c=	if(conam1) open=(A1.ge.amplo).and.(A1.le.amphi)
c=C Next line excludes openings with dubious amps when exass=true
c=	if(exass.and.dubious(ifst)) goto 2471
c=	setbad=badint(ifst)	!tint(ifst) was unusable
c=	if(open.and.(.not.setbad)) goto 248	!start of group
c=2471	ifst=ifst+1
c=	if(ifst.le.nint) goto 247	!start not yet found-try next interval
c=	goto 992			!finished all data
c=248	bad=.false.
c=	j=j+1		!count groups found so far- but may be bad
c=	YVAL(j)=tint(ifst)		!first in group (always usable)
c=	k=ifst+1    !index in tint() of interval following first of group
c=c k loop starts at 250
c=250	continue
c=	a1=ampl(k)
c=c=	open=a1.ne.0
c=	if(conam1) open=(A1.ge.amplo).and.(A1.le.amphi)
c=c Go to 249 when end of group found.
c=c (1) If no amplitude constraint group ends when ANY shut period found
c=c    (whether usable or unusable)
c=c (2) If amplitude constrained group ends when any amplitude outside
c=c=c    the specified range is found.
c=c Whole group is set bad, and so rejected, if (1) contains any unusable
c=c opening (2) when, if exass=true, it contains any opening with dubious
c=c amplitude OR if the interval that ends the group has 'dubious'amp.
c=	if(exass.and.dubious(k)) bad=.true.
c=	if(.not.open) goto 249		!end of group found
c=c=	YVAL(j)=YVAL(j)+tint(k)	!add any subsequent openings to same YVAL
c=	setbad=badint(k)		!tint(k) was unusable
c=	if(setbad) bad=.true.	!this group bad, but finish anyway
c=	k=k+1
c=	if(k.le.nint) goto 250	!keep looking for end
c=	j=j-1
c=	goto 992	!data finished before end found, so do not use
c=c Group completed
c=249	continue
c=================
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
c Modif 06/01/02 08:50pm for id=5 -open period must have spec gap BOTH sides
c
	if(allocated(ygap)) DEALLOCATE(ygap)
	if(.not.conam1) then
	   ALLOCATE(ygap(nintt))
	endif
	deb=debug()
c Find valid start of open period
	isol=conam1.and.isolate.eq.1		!isolated open periods only
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
	   if(.not.conam1) then
	      open1=a1.ne.0
	      open2=a2.ne.0
	      shut1=.not.open1
	   else
		if(isol) then
		   shut1=a1.eq.0.	!must start from shut
	   	   open1=.not.shut1
	         open2=(A2.ge.amplo).and.(A2.le.amphi)
		else
	         open1=(A1.ge.amplo).and.(A1.le.amphi)
	         shut1=.not.open1
	         open2=(A2.ge.amplo).and.(A2.le.amphi)
		endif
	   endif
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
c
c Valid 'start of open period' found now -get the rest
248	continue
	j=j+1		!count number of open periods found (reversed if bad)
	ifst=ifst+1		!index of first opening in open period just started
	Yval(j)=tint(ifst)	!length of this opening
	k=ifst+1	!index of interval that follows the first opening
	bad=.false.
c Now find the rest of the openings in this open period
	do while (k.le.nint)
	   a1=ampl(k)
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
	   if(.not.conam1) then
		shut1=a1.eq.0
		if(exass.and.dubious(k)) bad=.true.
		if(shut1) goto 249		!valid end of group found
	   else		!conam=true
c          valid end of open period depends on whether isolated only req
c          If isolated required then either (a) next interval is shut ->valid
c		isolated opening, or (b) it is open and in same window so open
c		period continues, or (c) it is open but outside window so not
c		an isolated opening -set the period bad before finishing it off
		if(isol) then
		   shut1=(a1.eq.0)				!valid end
		   open1=.not.shut1
		   if(open1.and.(a1.lt.amplo.or.a1.gt.amphi)) then
			bad=.true.	 !not isolated
		   endif
		else
		   shut1=(a1.lt.amplo.or.a1.gt.amphi)	!anywhere out of window
		endif
c		open1=(A1.ge.amplo).and.(A1.le.amphi)	!a1 in window
c		open2=(a1.ne.0.).and.(a1.lt.amplo.or.a1.gt.amphi)
c		if(deb.and.open2) then
c		   print 505
c		   if(discprt) write(8,505)
c505		   format(' Open period rejected: end is invalid')
c		endif
c		bad=bad.or.open2
	      if(exass.and.dubious(k)) bad=.true.
c		if(a1.eq.0.) goto 249	!valid end of burst found
		if(shut1) goto 249		!valid end of group found
	   endif
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
c
	if(deb) then
c print the group for debug
	   call DEBTYP(0,Ifst,k-1,tint,ampl,iprops,
     & 	0,-1,-1,yval,.false.,nintt,ndimy)
c print interval that follows group
	   call DEBTYP(0,k,k,tint,ampl,iprops,
     & 	0,-1,-1,yval,.false.,nintt,ndimy)
	   if(.not.bad.and.id.ne.4) then
		call DEBTYP(0,-1,-1,tint,ampl,iprops,
     &	 0,j,j,yval,.false.,nintt,ndimy)	!print YVAL(j)
	   endif
	endif
c Now define Yval in case of id=4 -open periods adj to specified gaps
c Note: specified gap is ygap(i); the open period preceding this is
c yval(i) [yval(i-lag+1) in general ], and the open period following
c the specified gap is on yval(i+1) [yval(i+lag) in general ].
c NB ygap not defined when amplitude constrained so omit bits that need ygap
c All values have already been checked for 'bad' and 'dubious'
c     & ' (1) Include open period only when PRECEDED by specified gap',/,
c     & ' (2) Include open period only when FOLLOWED by specified gap',/,
c     & ' (3) Include open period if adjacent to specified gap',/,
c if good, print yval -this is done below, after ygap() defined
c
c  carry on with id=2
c50	continue
c==	ifst=k+1	!another group completes (j updated above)
	ifst=k	!so do not skip past shut time needed to start new group
	if(conam1) then	!can't define ygap() when amp constrained
c=	   if(bad) j=j-1	!if bad do not use. YVAL(j) gets overwritten
	   if(bad) then
		j=j-1	!if bad do not use. YVAL(j) gets overwritten
		if(deb) then
		   print 504
		   if(discprt) write(8,504)
504		   format('&Open period abandoned',/)
		endif
	   endif
	   if(ifst.lt.nint) goto 247	!look for start of new group
	else
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
c	   if(bad) j=j-1	!if bad do not use. YVAL(j) gets overwritten
	   if(bad) then
		j=j-1	!if bad do not use. YVAL(j) gets overwritten
		if(deb) then
		   print 504
		   if(discprt) write(8,504)
c504		   format(' Open period abandoned')
		endif
	   else
c of not bad then define id=4 values and id=5
		if(id.eq.4) then
		   if(j.gt.1.and.(iadj.eq.1.or.iadj.eq.3)) then	!check gap preceding yval(j)
			tin=ygap(j-1)
			if(tin.ge.gaplo.and.tin.lt.gaphi) then	!OK -in range
			   ja=ja+1
			   yadj(ja)=yval(j)
			   if(deb) then
				print 239,ja,yadj(ja)
				if(discprt) write(8,239) ja,yadj(ja)
239				format(16x,'  Yadj(',i4,') = ',g13.6,/)
			   endif
			endif
		   endif
		   if(iadj.eq.2.or.iadj.eq.3) then	!check gap following yval(j)
			tin=ygap(j)
			if(tin.ge.gaplo.and.tin.lt.gaphi) then	!OK -in range
			   ja=ja+1
			   yadj(ja)=yval(j)
			   if(deb) then
				print 239,ja,yadj(ja)
				if(discprt) write(8,239) ja,yadj(ja)
c239				format(16x,'  Yadj(',i4,') = ',g13.6,/)
			   endif
			endif
		   endif
		else if(id.eq.5) then
		   if(j.gt.1) then
			tin=ygap(j-1)
			tin1=ygap(j)
			both=tin.ge.gaplo.and.tin.lt.gaphi.AND.
     &		     tin1.ge.gaplo.and.tin1.lt.gaphi	!both gaps in range
			if(iadj.eq.1) then
			   if(both) then
				ja=ja+1
				yadj(ja)=yval(j)
				if(deb) then
				   print 239,ja,yadj(ja)
				   if(discprt) write(8,239) ja,yadj(ja)
c239				   format(16x,'  Yadj(',i4,') = ',g13.6,/)
				endif
			   endif
			else	   !iadj=2 -include all except this with both gaps in range
			   if(.not.both) then
				ja=ja+1
				yadj(ja)=yval(j)
				if(deb) then
				   print 239,ja,yadj(ja)
				   if(discprt) write(8,239) ja,yadj(ja)
c239				   format(16x,'  Yadj(',i4,') = ',g13.6,/)
				endif
			   endif
			endif
		   endif
		endif		!end of if(id.eq.4) else id.eq.5
	   endif	!end if(bad). . .else
c now the next open period
	   if(ifst.lt.nint) goto 247	!look for start of new group
	endif		!end of if(conam1) . . . else
c If ifst=nint then end here, but first check that the last gap, tint(k),
c is OK -it will often be set bad, in which case, for the purpose of
c correlations in which 'following gap' is needed, we must exclude
c the open period that preceded it too: setting j=j-1 in this case
c excludes only bad gap so could get unequal number of yval() and ygap()
	if(badint(k)) then
	   j=j-1
	   if(id.eq.4.or.id.eq.5) ja=ja-1
	endif
C N.B. does not return if ifst=nint, as if tint(nint) is start of a
C group there is no way to tell if this group has been completed-
C (the next interval might have been open too)
C  NOTE that NYVAL=naset calc here may not be identical with that
C found in calc of burst distributions because latter includes
C only those open periods that are part of valid bursts (no bad
C openings and proper end)
992	continue
c Now have all open periods in yval(), and shut times between them in ygap()
	if(id.eq.4.or.id.eq.5) then	!copy Yadj to Yval
	   nyval=ja
	   do i=1,ja
		yval(i)=yadj(i)
	   enddo
	   DEALLOCATE(Yadj)
	   nocor=.true.
	   goto 710		!skip correlations!!
	else
	   nyval=j
	endif
c
c -now calc correlations and conditional means?
c
	if(nset.eq.1) then
	   ans='N'
	   if(nocor) ans='Y'
	   call BELL(1)
	   print 108
	   call DCASK(' Skip all correlation calculations',ans,ans)
	   nocor=ans.eq.'Y'
	   if(nocor) goto 710
	else
	   nocor=.true.
	   goto 710
	endif
c
c NB ygap not defined when amplitude constrained so omit bits that need ygap
	if(conam1) goto 207
	if(.not.conam1) then	!if amp constrained, ygap not defined
	   ans='Y'
	   call DCASK(
     &'Calculate correlation coeffs for shut time with next open period'
     &   ,ans,ans)
	   if(ans.eq.'Y') then
		call CORCALS(yval,ygap,nyval,ndimy,nintt,yhio,yhig,
     &	  ymaxo,ymaxg,idiskq,1)
		print 108
		if(discprt) write(8,108)
	   endif
	endif
c
c For open|shut:
c Note: specified gap is ygap(i); the open period preceding this is
c yval(i) [yval(i-lag+1) in general], and the open period following
c the specified gap is on yval(i+1) [yval(i+lag) in general].
c NB ygap not defined when amplitude constrained so omit bits that need ygap
c Check which shut time range each gap is in,
c (1) Record length of the open period that PRECEDES this gap (if max specified, then
c both must be < max for a valid pair).  Average these open periods to give
c mean open period conditional on following gap (the open period precedes gap,
c so gap follows open period)
c (2) Record length of the open period that FOLLOWS this gap (if max specified, then
c both must be < max for a valid pair).  Average these open periods to give
c mean open period conditional on preceding gap (the open period follows gap,
c so gap precedes open period)
c
c=	if(conam1) goto 207	!now above
	print 108		!new line
	ans='Y'
	call DCASK(
     &'Calculate mean open period adjacent to spec SHUT time range',
     & ans,ans)
	opcal=ans.eq.'Y'
	deb=debug()
	if(opcal) then
	  print 27
	  if(discprt) write(8,27)
27	  format(/,
     & ' Mean durations of apparent open periods that are adjacent ',/,
     & '  to a shut periods with durations in following ranges (ms)')
	  if(yhig.gt.0.) then	!already set in CORCALS
	     print 26,yhio,yhig
26	     format(
     & ' Exclude open times > ',g11.4,' ms and shut times > ',
     &   g11.4,' ms  [Y] ? ')
		ans='Y'
		call INPUTa(ans)
	     if(UC(ans).eq.'N') yhig=-1.
	  endif
	  if(yhig.lt.0.) then	!already set in CORCALS
		ymaxo=-1.e37
		ymaxg=-1.e37
		do i=1,nyval
		   if(yval(i).gt.ymaxo) ymaxo=yval(i)
		   if(ygap(i).gt.ymaxg) ymaxg=ygap(i)
		enddo
		if(yhio.gt.1.e36) then
		   print 65,nyval,ymaxo
65		   format(' Open periods: ',i6,' values, maximum = ',g13.6,/,
     & 	' Exclude any outlying values (check distribution!) [N] ? ')
		   ans='N'
		   call INPUTa(ans)
		else		!value set from .ini
		   ans='Y'
		endif
		if(UC(ans).eq.'Y') then
		   print 371,yhio
371	  	   format(
     & ' Exclude all open periods longer than Ymax (ms) [',f10.1,'] = ')
		   call INPUTr(yhio)
	         if(discprt) write(8,500) yhio
500		   format(
     &	   ' Open periods longer than ',g13.6,' ms excluded.')
		else
		   yhio=1.e37
       	   if(discprt) write(8,501)
501		   format(' No values excluded')
		endif
c
		if(yhig.gt.1.e36) then
		   print 66,nyval,ymaxg
66		   format(' Shut times: ',i6,' values, maximum = ',g13.6,/,
     & 	' Exclude any outlying values (check distribution!) [N] ? ')
		   ans='N'
		   call INPUTa(ans)
		else		!value set from .ini
		   ans='Y'
		endif
		if(UC(ans).eq.'Y') then
		   print 67,yhig
67		   format(
     & ' Exclude all shut times longer than Ymax (ms) [',f10.1,'] = ')
		   call INPUTr(yhig)
c		   print 68, nyval,yhig
      	   if(discprt) write(8,68)yhig
68		   format(
     &	   ' Shut times longer than ',g13.6,' excluded.')
		else
   		   yhig=1.e37
       	   if(discprt) write(8,501)
c501		   format(' No values excluded')
		endif
	  endif
c
	  nlag=5
	  print 5,nlag
5	  format(
     & '  Calculate for lags (lag=1 is adjacent) up to n [',i2,'] = ')
	  call INPUTi(nlag)
	  call DCASK('Print the values','N',ans)
	  noprint=ans.eq.'N'
	  yhi(10)=-1.	!to indicate not set yet
	  nval=0	!for default
	  if(nval.eq.0) then
		nval=7
		ylo(1)=tresg
		yhi(1)=0.3
		ylo(2)=0.3
		yhi(2)=1.0
		ylo(3)=1.0
		yhi(3)=3.
		ylo(4)=3.
		yhi(4)=30.
		ylo(5)=30.
		yhi(5)=300.
		ylo(6)=300.
		yhi(6)=3000.
		ylo(7)=3000.
		yhi(7)=1000000.
		yhi(10)=3000.	!so default used
	  endif
	  print 30,(ylo(i),yhi(i),i=1,7)
30	  format(' The 7 default ranges (ms) are: ',/,
     &  4(f8.4,'-',f8.4,', '),/,3(f8.4,'-',f8.4,', '))
	  print 3032,(ylogap(i),yhigap(i),i=1,nvalgap)
c3032	  format(' The ranges used last time (ms) were: ',/,
c     &  3(f8.4,'-',f8.4,', '),/,3(f8.4,'-',f8.4,', '),/,
c     &  3(f8.4,'-',f8.4,', '))
	  iropt=2
	  print 3033,iropt
c3033	  format(
c     & ' (1) Use default ranges ',/,
c     & ' (2) Use same ranges as before',/,
c     & ' (3) Define new ranges',/,
c     & '  Option number [',i2,'] = ')
	  call INPUTi(iropt)
	  if(iropt.eq.1) then
	     nval=7
	  else if(iropt.eq.2) then
		nval=nvalgap
		do i=1,nval
		   ylo(i)=ylogap(i)
		   yhi(i)=yhigap(i)
		enddo
	  endif
	  if(iropt.eq.3) then
	     print 3035
3035	     format(
     & ' Number of shut duration ranges to be specified (up to 10) = ')
	     call INPUTi(nval)
	  endif
c
	  print 108
c
c For open|shut:
c Note: specified gap is ygap(i); the open period preceding this is
c yval(i) [yval(i-lag+1) in general], and the open period following
c the specified gap is on yval(i+1) [yval(i+lag) in general].
c NB ygap not defined when amplitude constrained so omit bits that need ygap
c Check which shut time range each gap is in,
c (1) Record length of the open period that PRECEDES this gap (if max specified, then
c both must be < max for a valid pair).  Average these open periods to give
c mean open period conditional on following gap (the open period precedes gap,
c so gap follows open period)
c (2) Record length of the open period that FOLLOWS this gap (if max specified, then
c both must be < max for a valid pair).  Average these open periods to give
c mean open period conditional on preceding gap (the open period follows gap,
c so gap precedes open period)
c
c MODIFIED 04/18/04 09:09pm so that
c (1) preceding open time recorded in sy0,syy0,ny0
c (2) following open time recorded in sy1,syy1,ny1
c (3) both open times recorded in sy2,syy2,ny2
c
c Set the nval shut time ranges, amd initialise arrays for each range

	  do i=1,nval
		 sy0(i)=0.		!initialise
		 syy0(i)=0.		!initialise
		 ny0(i)=0		!initialise
		 sy1(i)=0.		!initialise
		 syy1(i)=0.		!initialise
		 ny1(i)=0		!initialise
		 sy2(i)=0.		!initialise
		 syy2(i)=0.		!initialise
		 ny2(i)=0		!initialise
		 if(iropt.eq.3) then
		   print 38,i
38		   format(
     & '&Shut times (ms) for range',i2,': low, high time = ')
		   call INPUT2r(ylo(i),yhi(i))
		   if(ylo(i).gt.yhi(i)) then
		      a1=ylo(i)
		      ylo(i)=yhi(i)
		      yhi(i)=a1
		   endif
		endif
	  enddo
c end of setting of shut time ranges
c
c
c Keep the ranges used for conditional means (because if we want to
c average over experiments, best if same ranges used in all experiments
c that are averaged)
	  nvalgap=nval
	  do i=1,nval
		ylogap(i)=ylo(i)
		yhigap(i)=yhi(i)
c	 	print 72,i,ylo(i),yhi(i)
	 	if(discprt) write (8,72) i,ylo(i),yhi(i)
72		format(' Shut time range ',i3,': from ',g13.6,' to ',
     &		g13.6,' ms')
	  enddo
c

c Allocate arrays
c reverse order for declaration of ns0 etc (better for acsii write)
	  n1=nlag   	!for brevity
	  n2=nval
	  if(allocated(ns0)) then
		DEALLOCATE(ns0,ym0,sd0,ns1,ym1,sd1,pm0,pm1,
     &	sx0,sx1,sx2,sdp,ns2,ym2,sd2,pm2)
	  endif
	  ALLOCATE(ns0(n1,n2),ym0(n1,n2),sd0(n1,n2),pm0(n1,n2),
     &	sx0(n1,n2),sx1(n1,n2),sx2(n1,n2),sdp(n1,n2))
	  ALLOCATE(ns1(n1,n2),ym1(n1,n2),sd1(n1,n2),pm1(n1,n2))
	  ALLOCATE(ns2(n1,n2),ym2(n1,n2),sd2(n1,n2),pm2(n1,n2))
c For each gap, (a) find which shut time range it is in, (b) identify
c preceding and following open time (or open time which is n=lag away)
c and average these open times in the average appropriate to the shut
c time range found)

	  sum0=0.0
	  ss0=0.0
	  na0=0
	  sum1=0.0
	  ss1=0.0
	  na1=0
	  sum2=0.0
	  ss2=0.0
	  na2=0
c
	  lag=1
c Start loop over lag=1,2,...,nlag
	  do while(lag.le.nlag)
	   do k=1,nval
		sx0(lag,k)=0.0
	   enddo
c	   do i=1+lag,nyval-lag
	   i=1+lag
c Start loop over gap #=1+lag to nyval-lag
	   do while(i.le.nyval-lag)
		if(mod(i,50).eq.0) deb=debug()
		tin=ygap(i)
		yv0=yval(i-lag+1)		!preceding open period
		yv1=yval(i+lag)	!following open period
		good0=tin.lt.yhig.and.yv0.lt.yhio	!gap and prec op both below max
		good1=tin.lt.yhig.and.yv1.lt.yhio	!gap and following op both below max
c If lag>1 then should check that all intervening intervals are good (below max,
c if a max is specified). for lag=lag, there will be lag-1 intervening open and
c sut times to be checked
		if(lag.gt.1) then
		   do j=1,lag-1
			if(ygap(i-j).gt.yhig) good0=.false.
			if(yval(i-j).gt.yhio) good0=.false.
			if(ygap(i+j).gt.yhig) good1=.false.
			if(yval(i+j).gt.yhio) good1=.false.
		   enddo
		endif
		good2=good0.and.good1			!both below max
c==		good1=tin.lt.yhig.and.yv1.lt.yhio	!gap and following op both below max
c==		good0=good1.and.yv0.lt.yhio		!preceding also OK
		if(good0) then
		   sum0=sum0+yv0		!sum for openings before ygap(i)
		   ss0=ss0+(yv0*yv0)
		   na0=na0+1
		endif
		if(good1) then
		   sum1=sum1+yv0		!sum for openings after ygap(i)
		   ss1=ss1+(yv0*yv0)
		   na1=na1+1
		endif
		if(good2) then
		   sum2=sum2+yv0+yv1		!sum for mean of all values used
		   ss2=ss2+(yv0*yv0)+(yv1*yv1)
		   na2=na2+2
		endif
c
c Now loop over the shut time ranges, k=1,...,nval, and for each gap, tin(i),
c check which range it is in, and as long as good (below max, if specified) then
c add in preceding and following open times, for calc of average
c
		do k=1,nval
		   if(tin.ge.ylo(k).and.tin.lt.yhi(k)) then	!OK- in range #k
			if(good0) then
			  sy0(k)=sy0(k) + yv0 	!add PREC open period lengths
			  syy0(k)=syy0(k) + yv0*yv0  ! for SD
			  ny0(k)=ny0(k)+1			!increment number
			  sx0(lag,k)=sx0(lag,k) + tin	!mean of adj gaps used
			endif
			if(good1) then
			  sy1(k)=sy1(k) + yv1 	!add AFTER open period lengths
			  syy1(k)=syy1(k) + yv1*yv1  ! for SD
			  ny1(k)=ny1(k)+1			!increment number
			  sx1(lag,k)=sx1(lag,k) + tin	!mean of adj gaps used
			endif
			if(good2) then
			  sy2(k)=sy2(k) + yv0 + yv1 	!add BOTH open period lengths
			  syy2(k)=syy2(k) + yv0*yv0 + yv1*yv1  ! for SD
			  ny2(k)=ny2(k)+2			!increment number
			  sx2(lag,k)=sx2(lag,k) + tin	!mean of adj gaps used (ny2/2 values)
			endif
			if(deb) then
			   print 120,lag,i,ygap(i),k,i-lag+1,yv0,
     &			i+lag,yv1,ny0(k),ny1(k)
			   if(discprt) write(8,120) lag,i,ygap(i),k,i-lag+1,
     &			yv0,i+lag,yv1,ny0(k),ny1(k)
120			   format(
     &			' LAG = ',i4,/,
     &			' Shut time #',i5,' = ',g13.6,
     &		   ' ms is in range ',i2,/,
     &		   '    Preceding open period, # ',i5,' = ',
     &	         g13.6,' averaged for this range',/,
     &		   ':   Following open period, # ',i5,' = ',
     &	         g13.6,' averaged for this range',/,
     &		   1x,i5,1x,i5,' values in averages 0 and 1')
			endif
		   endif
		enddo
		i=i+1
	   enddo	!end of i=1,nyval loop
c     Print means
	   if(na0.gt.2) then
		ss0=(ss0-(sum0*sum0/float(na0)))/float(na0-1)	!variance
		ss0=sqrt(ss0/float(na0))		!=SD(mean) for overall mean
		sum0=sum0/float(na0)		!overall mean
	   endif
	   if(na1.gt.2) then
		ss1=(ss1-(sum1*sum1/float(na1)))/float(na1-1)	!variance
		ss1=sqrt(ss1/float(na1))		!=SD(mean) for overall mean
		sum1=sum1/float(na1)		!overall mean
	   endif
	   if(na2.gt.2) then
		ss2=(ss2-(sum2*sum2/float(na2)))/float(na2-1)	!variance
		ss2=sqrt(ss2/float(na2))		!=SD(mean) for overall mean
		sum2=sum2/float(na2)		!overall mean
	   endif
c
	   if(noprint.and.lag.gt.1) goto 712
	   print 39,lag
	   if(discprt) write(8,39) lag
39	   format(/,
     & ' MEAN OPEN PERIODS FOR LAG = ',i3,' FROM SPECIFIED SHUT PERIOD')
	   print 391,sum0,na0,ss0
	   if(discprt) write(8,391) sum0,na0,ss0
391	   format(/,
     &	' Mean of preceding open periods = ',g13.6,
     &	' ms;',/,'    n = ',i5,' SD(mean) = ',g13.6)
	   print 392,sum1,na1,ss1
	   if(discprt) write(8,392) sum1,na1,ss1
392	   format(/,
     &	' Mean of following open periods = ',g13.6,
     &	' ms;',/,'    n = ',i5,' SD(mean) = ',g13.6)
	   print 393,sum2,na2,ss2
	   if(discprt) write(8,393) sum2,na2,ss2
393	   format(/,
     &	' Mean of all (preceding+following) open periods = ',g13.6,
     &	' ms;',/,'    n = ',i5,' SD(mean) = ',g13.6)
712	   if(noprint) goto 709
	   print 394		!col headings
	   if(discprt) write(8,394)
394	   format(/,
     &   3x,' Shut period range (ms) ',3x,'  n  mean open',
     & '   SD(mean)    mean as   mean shut',/,
     &   3x,'                        ',3x,'      period  ',
     & '               % of all')
709	   continue
	   do j=1,3		!for preceding, following and both open periods
	    do ir=1,nval
	      if(j.eq.1) then
		   if(ir.eq.1.and.(.not.noprint)) then
			print 40
			if(discprt) write(8,40)
40 			format(/,' For preceding open period only:')
		   endif
		   n=ny0(ir)
		   ns0(lag,ir)=n		!record for below
		   if(n.gt.1) then	!only if n=2 or more
			en=float(n)
	      	ym=sy0(ir)/en
			pm=100.*ym/sum0	!as % of all open periods
	      	sd=sqrt((syy0(ir)-(sy0(ir)*sy0(ir)/en))/(en-1.))
			sd=sd/sqrt(en)	!NOW = SD OF MEAN
			ym0(lag,ir)=ym	!record for print as func of lag
			pm0(lag,ir)=pm	!record for print as func of lag
			sd0(lag,ir)=sd
			sx0(lag,ir)=sx0(lag,ir)/float(n)	!mean x value
			emx=sx0(lag,ir)
c Approx SD of m=ym/sum0 if g<<1 and indep is
c (1/sum0)*sqrt[sd0**2 + (m*ss0)**2], where ss0=SD of sum0 and sd0 is sd of ym.
			em=ym/sum0
	         endif
		else if(j.eq.2) then
		   if(ir.eq.1.and.(.not.noprint)) then
			print 401
			if(discprt) write(8,401)
401 			format(/,' For following open period only:')
		   endif
	         n=ny1(ir)
		   ns1(lag,ir)=n		!record for below
		   if(n.gt.1) then
			en=float(n)
	      	ym=sy1(ir)/en
			pm=100.*ym/sum1	!as % of all open periods
	      	sd=sqrt((syy1(ir)-(sy1(ir)*sy1(ir)/en))/(en-1.))
			sd=sd/sqrt(en)	!NOW = SD OF MEAN
			ym1(lag,ir)=ym	!record for print as func of lag
			pm1(lag,ir)=pm	!record for print as func of lag
			sd1(lag,ir)=sd
			sx1(lag,ir)=sx1(lag,ir)/float(n)	!mean x value
			emx=sx1(lag,ir)
		   endif
		else if(j.eq.3) then
		   if(ir.eq.1.and.(.not.noprint)) then
			print 402
			if(discprt) write(8,402)
402 			format(/,' For adjacent open period:')
		   endif
	         n=ny2(ir)
		   ns2(lag,ir)=n		!record for below
		   if(n.gt.1) then
			en=float(n)
	      	ym=sy2(ir)/en
			pm=100.*ym/sum2	!as % of all open periods
	      	sd=sqrt((syy2(ir)-(sy2(ir)*sy2(ir)/en))/(en-1.))
			sd=sd/sqrt(en)	!NOW = SD OF MEAN
			ym2(lag,ir)=ym	!record for print as func of lag
			pm2(lag,ir)=pm	!record for print as func of lag
			sd2(lag,ir)=sd
			sx2(lag,ir)=sx2(lag,ir)/float(n/2)	!mean x value (n/2 values)
			sdp(lag,ir)=100.*sqrt((sd*sd) + (em*em*ss2*ss2))/sum2
			emx=sx2(lag,ir)
		   endif
		endif
		if(noprint) goto 706
	      if(n.le.1) then
		   print 4261,ir,ylo(ir),yhi(ir),n
		   if(discprt) write(8,4261) ir,ylo(ir),yhi(ir),n
4261		   format(1x,i3,1x,g11.4,' to ',g11.4,i4)
		else
		   print 426,ir,ylo(ir),yhi(ir),n,ym,sd,pm,emx
		   if(discprt) write(8,426)ir,ylo(ir),yhi(ir),n,ym,sd,pm,emx
426		   format(1x,i3,1x,g11.4,' to ',g11.4,i4,4g11.4)
		endif
706		continue
	    enddo
	   enddo
c
c Print mean of adjacent gaps separately (now above)
c	  if(.not.noprint) then
c	   do k=1,nval
c	      print 29,k,sx0(lag,k)
c	      if(discprt) write(8,29) k,sx0(lag,k)
c29		format(' For range #',i3,' mean shut time = ',g13.6)
c	   enddo
c	  endif
c
c Increment the lag
	   lag=lag+1
c	   pause 'lag incremented'
	   deb=debug()
	   sum0=0.0
	   ss0=0.0
	   na0=0
	   sum1=0.0
	   ss1=0.0
	   na1=0
	   sum2=0.0
	   ss2=0.0
	   na2=0
	   do i=1,nval
		sy0(i)=0.		!initialise
		syy0(i)=0.		!initialise
		ny0(i)=0		!initialise
		sy1(i)=0.		!initialise
		syy1(i)=0.		!initialise
		ny1(i)=0		!initialise
		sy2(i)=0.		!initialise
		syy2(i)=0.		!initialise
		ny2(i)=0		!initialise
	   enddo
	  enddo	!end of 'do while(lag.le.nlag)'
c
c end of 'do while(lag.le.nlag)'
c
	  if(.not.noprint) then
	     print 6
	     if(discprt) write(8,6)
6	     format(/,/,' SAME VALUES TABULATED AS FUNCTION OF LAG')
	  endif
	  do i=1,nval
		if(noprint) goto 707
		print 40
		if(discprt) write(8,40)
c40 		format(/,' For preceding open period only:')
	 	print 71,i,ylo(i),yhi(i)
	 	if(discprt) write (8,71) i,ylo(i),yhi(i)
71		format(/,' Shut period range ',i3,': from ',g13.6,' to ',
     &		g13.6,' ms',/,
     &	'   lag',8x,'  n     mean open period    SD(mean)',
     &	'     mean as % of all ')
		do lag=1,nlag
		   n=ns0(lag,i)
		   pm=pm0(lag,i)		!as % of all open periods
	         if(n.le.1) then
			print 8,lag,ns0(lag,i)
			if(discprt) write(8,8) lag,ns0(lag,i)
8			format(i5,8x,i5)
		   else
			print 9,lag,ns0(lag,i),ym0(lag,i),sd0(lag,i),pm
			if(discprt) write(8,9) lag,ns0(lag,i),
     &			ym0(lag,i),sd0(lag,i),pm
9			format(i5,8x,i5,5x,3(3x,g13.6))
		   endif
		enddo
c
		print 401
		if(discprt) write(8,401)
c401 		format(/,' For following open period only:')
	 	print 71,i,ylo(i),yhi(i)
	 	if(discprt) write (8,71) i,ylo(i),yhi(i)
c71		format(/,' Shut period range ',i3,': from ',g13.6,' to ',
c     &		g13.6,' ms',/,
c     &	'   lag',8x,'  n     mean open period    SD(mean)',
c     &	'     mean as % of all ')
		do lag=1,nlag
		   n=ns1(lag,i)
		   pm=pm1(lag,i)		!as % of all open periods
	         if(n.le.1) then
			print 8,lag,ns1(lag,i)
			if(discprt) write(8,8) lag,ns1(lag,i)
c8			format(i5,8x,i5)
		   else
			print 9,lag,ns1(lag,i),ym1(lag,i),sd1(lag,i),pm
			if(discprt) write(8,9) lag,ns1(lag,i),
     &			ym1(lag,i),sd1(lag,i),pm
c9			format(i5,8x,i5,5x,3(3x,g13.6))
		   endif
		enddo
c
		print 402
		if(discprt) write(8,402)
c402 		format(/,' For adjacent open period:')
	 	print 71,i,ylo(i),yhi(i)
	 	if(discprt) write (8,71) i,ylo(i),yhi(i)
c71		format(/,' Shut period range ',i3,': from ',g13.6,' to ',
c     &		g13.6,' ms',/,
c     &	'   lag',8x,'  n     mean open period    SD(mean)',
c     &	'     mean as % of all ')
		do lag=1,nlag
		   n=ns2(lag,i)
		   pm=pm2(lag,i)		!as % of all open periods
	         if(n.le.1) then
			print 8,lag,ns2(lag,i)
			if(discprt) write(8,8) lag,ns2(lag,i)
c8			format(i5,8x,i5)
		   else
			print 9,lag,ns2(lag,i),ym2(lag,i),sd2(lag,i),pm
			if(discprt) write(8,9) lag,ns2(lag,i),
     &			ym2(lag,i),sd2(lag,i),pm
c9			format(i5,8x,i5,5x,3(3x,g13.6))
		   endif
		enddo
707		continue
	  enddo
c
	  print 108
	  if(discprt) write(8,108)
108	  format(/)
	endif 	!end of calc of mean open adjacent . . .
c
c Now plot mean vs lag (for var ranges) OR plot mean vs spec range
c (for var lags)
	iopt=1
	iopt1=1
33	continue
	if(opcal) then
	  print 28,iopt
28	  format(
     &' (1) Plot mean vs adjacent shut time range (for var lags)',/,
     &' (2) Plot mean against lag (for var shut time ranges)',/,
     &' (3) No more plots',/,
     & ' Option number [',i2,'] = ')
	   call INPUTi(iopt)
	   if(iopt.eq.3) then
		DEALLOCATE(ns0,ym0,sd0,ns1,ym1,sd1,pm0,pm1,
     &	sx0,sx1,sx2,sdp,ns2,ym2,sd2,pm2)
		goto 702
	   endif
c Allocations
	   if(iopt.eq.1) then
		ndv1=nval		!number of ranges
		print 290,iopt1
290		format(
     &' (1) Plot mean vs adjacent shut time range (for several lags)',/,
     &' (2) Plot preceding, following and adjacent mean for one lag',/,
     &' (3) No more plots',/,
     & ' Option number [',i2,'] = ')
		call INPUTi(iopt1)
		if(iopt1.eq.3) then
		   DEALLOCATE(ns0,ym0,sd0,ns1,ym1,sd1,pm0,pm1,
     &	    sx0,sx1,sx2,sdp,ns2,ym2,sd2,pm2)
		   goto 702
		endif
		if(iopt1.eq.1) then
300		   print 31,nlag
31		   format(
     &	' Number of curves (lags) to be plotted (up to ',i2,') = ')
		   ncurvd=nlag	!default
		   call INPUTi(ncurvd)
		   if(ncurvd.gt.nlag.or.ncurvd.lt.1) goto 300
c no -define arrays for all lags, not just those plotted
		   ndimd=nlag		!number of lags
		else if(iopt1.eq.2) then
		   ncurvd=3
292		   print 293,nlag
293		   format(
     &	' Plot for lag (up to ',i2,') [1] = ')
		   ilag=1		!default
		   call INPUTi(ilag)
		   if(ilag.gt.nlag) goto 292
c no -define arrays for all lags, not just those plotted
		   ndimd=3
		endif
	      ALLOCATE(xval2(ndv1,ndimd),yval2(ndv1,ndimd))
		ALLOCATE(ndat(ndimd),icurvd(ndimd),isym(ndimd),
     &	ijoin(ndimd),syms(ndimd))
	 	kwi=ndv1
		kwj=ndimd
		ALLOCATE(icurvw(ndimd),weight(kwi,kwj))
c
		if(iopt1.eq.1) then
		   do j=1,ncurvd
			print 32,j,nlag
			lag=j	!default
			print 32,j,nlag,j
32			format(
     &		'&curve #',i3,' for lag (up to ',i4,') [',i3,'] = ')
			call INPUTi(lag)
			icurvd(j)=lag
			ndat(lag)=nval
			isym(lag)=-7		!circles
			syms(lag)=1.0
			ijoin(lag)=0		!points joined
		   enddo
		   xmax=-1.e37
		   xmin=1.e37
c
		   do i=1,nval	!all ranges
			do j=1,nlag	!all lags
			   if(ns2(j,i).ge.2) then
				xval2(i,j)=sx2(j,i)
c				yval2(i,j)=pm2(j,i)		!mean as percent
				yval2(i,j)=ym2(j,i)		!mean in ms
c				weight(i,j)=1.0/(sdp(j,i)*sdp(j,i))	!sd for percent
				weight(i,j)=1.0/(sd2(j,i)*sd2(j,i))	!sd for mean (ms)
				if(xval2(i,j).lt.xmin) xmin=xval2(i,j)
				if(xval2(i,j).gt.xmax) xmax=xval2(i,j)
			   else
				ndat(j)=ndat(j)-1	!omit data points with <2 values
			   endif
			enddo
		   enddo
		   if(xmin.lt.0.01) xmin=0.01	!log scale, so min=0.01 ms
	         titlex='adjacent shut time (mean)'
c	         titley='mean open period (%)'
	         titley='mean open period (ms)'
		else if(iopt1.eq.2) then
		   do j=1,ncurvd
			icurvd(j)=j
			ndat(j)=nval
			isym(j)=-7		!circles
			syms(j)=1.0
			ijoin(j)=0		!points joined
		   enddo
		   xmax=-1.e37
		   xmin=1.e37
		   do i=1,nval
			j=ilag
			if(ns0(j,i).ge.2) then
			   xval2(i,1)=sx0(j,i)
			   yval2(i,1)=ym0(j,i)		!mean in ms
		         weight(i,1)=1.0/(sd0(j,i)*sd0(j,i))	!sd for mean (ms)
			else
			   ndat(1)=ndat(1)-1	!omit data points with <2 values
			endif
			if(ns1(j,i).ge.2) then
			   xval2(i,2)=sx1(j,i)
			   yval2(i,2)=ym1(j,i)		!mean in ms
		         weight(i,2)=1.0/(sd1(j,i)*sd1(j,i))	!sd for mean (ms)
			else
			   ndat(2)=ndat(2)-1	!omit data points with <2 values
			endif
			if(ns2(j,i).ge.2) then
			   xval2(i,3)=sx2(j,i)
			   yval2(i,3)=ym2(j,i)		!mean in ms
		         weight(i,3)=1.0/(sd2(j,i)*sd2(j,i))	!sd for mean (ms)
			else
			   ndat(3)=ndat(3)-1	!omit data points with <2 values
			endif
			if(xval2(i,j).lt.xmin) xmin=xval2(i,j)
			if(xval2(i,j).gt.xmax) xmax=xval2(i,j)
		   enddo
		   if(xmin.lt.0.01) xmin=0.01	!log scale, so min=0.01 ms
	         titlex='post, pre, adjacent shut time (mean)'
	         titley='mean open period (ms)'
		endif
c
c Now plot vs lag
	   else if(iopt.eq.2) then
c Data
340		ndv1=nlag
c=		ncurvd=nval
		print 34,nval
34		format(
     &     ' Number of curves (ranges) to be plotted (up to ',i2,') = ')
		ncurvd=nval		!default
		call INPUTi(ncurvd)
		if(ncurvd.gt.nval.or.ncurvd.lt.1) goto 340
c=		ndimd=ncurvd
c no -define arrays for all ranges, not just those plotted
		ndimd=nval		!number of ranges
	      ALLOCATE(xval2(ndv1,ndimd),yval2(ndv1,ndimd))
		ALLOCATE(ndat(ndimd),icurvd(ndimd),isym(ndimd),
     &	ijoin(ndimd),syms(ndimd))
		if(allocated(icurvw)) then
		   DEALLOCATE(icurvw,weight)
		endif
c    Declare weight as for ym0() etc
	 	kwi=nlag   	!for brevity
		kwj=nval
c		kwi=1
c		kwj=1
		ALLOCATE(icurvw(ndimd),weight(kwi,kwj))
		do j1=1,ncurvd
		   print 35,j1,nval,j1
35		   format(
     &	   '&curve #',i3,' for range # (up to ',i4,') [',i3,'] = ')
		   j=j1	!default
		   call INPUTi(j)
		   icurvd(j1)=j
		   ndat(j)=nlag
		   isym(j)=-7		!circles
		   syms(j)=0.7
		   ijoin(j)=0		!points joined
		enddo
		do i=1,nlag
		   do j=1,nval
			if(ns2(i,j).ge.2) then
			   xval2(i,j)=float(i)
c			   yval2(i,j)=pm2(i,j)	!adjacent (mean, %)
			   yval2(i,j)=ym2(i,j)	!adjacent (mean, ms)
c			   weight(i,j)=1.0/(sdp(i,j)*sdp(i,j))		!for %
			   weight(i,j)=1.0/(sd2(i,j)*sd2(i,j))		!for mean (ms)
			else
			   ndat(j)=ndat(j)-1	!omit data points with <2 values
			endif
		   enddo
		enddo
		xmin=0.
		xmax=float(nlag)
	      titlex='lag'
c	      titley='mean open period (%)'
	      titley='mean open period (ms)'
	   endif
c Calc curve
	   ncalc=100
	   ndc1=ncalc
	   ndimc=1
	   ALLOCATE(xcal(ndc1,ndimc),ycal(ndc1,ndimc))
	   ALLOCATE(ncal(ndimc),icurvc(ndimc),iline(ndimc))
	   ncurvc=1
	   icurvc(1)=1
	   ncal(1)=ncalc
	   dx=xmax/float(ncalc-1)
	   do i=1,ncalc
		xcal(i,1)=xmin+float(i-1)*dx
		ycal(i,1)=100.           !100% line
	   enddo
	   iline(1)=2		!dashed
c
c
c Misc inputs for vplot
	   iscal=1		!scale internally
c	   iscal=4		!so xmin,...,ymax (only) set here
c	   iscal=0		!so xmin,...,ycross (only) set here
	   do i=1,ndimd
		icurvw(i)=1		!SD specified
	   enddo
	   cbig=2.5
	   ifont=4
	   ifitype=0		!no display of fitted parameters
	   kmax=1
	   idiskq=-1
	   ilog=0 				!arithmetic
	   if(iopt.eq.1) then
		ilog=1		!y vs log x
	   endif
	   ilabel=1		!default pos for axis labels
	   iask=-2
	   ncjump=0
	   nvjump=0
	   autplt=.false.
	   fitted=.false.
	   plotonly=.false.
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
	   call VPLOT5(XVAL2,YVAL2,NDAT,icurvd,ncurvd,ijoin,syms,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,plotonly,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,interp,
     & isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kmax,iver)
c     & isetcol,itrace,ndv1,ndc1,)
	   ans='N'
	   call DCASK('Save values as ASCII file',ans,ans)
	   if(ans.eq.'Y') then
c Want alternating columns of pm0,sx0 both of which are declared (nlag,nval)
c ie have nval columns
		ALLOCATE(data(nlag,3*nval))
		ndata=nlag	!number of lines of data
		ncol=3*nval	!nval entries in each line
		icolwid=13		!the minimum col width
		do i=1,ncol,3
		   call INTCONV(1+i/3,cnum)
		   colhead(i)='X (range '//charnb(cnum)//')'
		   colhead(i+1)='Y (range '//charnb(cnum)//')'
		   colhead(i+2)='SD(mean)('//charnb(cnum)//')'
		enddo
		ndim1=nlag
		ndim2=3*nval
		text=
     &	'Mean open period (% overall), adj. to shut time range'
		lentext=nblank(text)
		j1=0
		do j=1,nval
		   j1=j1+3
		   do i=1,nlag
			data(i,j1-2)=sx0(i,j)
			data(i,j1-1)=pm0(i,j)
			data(i,j1)=sdp(i,j)
		   enddo
		enddo
		call ASCWRITE(data,ndata,ncol,text,ndim1,ndim2,lentext,
     &		icolwid,colhead,1)
		DEALLOCATE(data)
	   endif
	   DEALLOCATE(xval2,yval2,xcal,ycal)
	   DEALLOCATE(ndat,icurvd,isym,ijoin,syms)
	   DEALLOCATE(ncal,icurvc,iline)
	   if(allocated(icurvw)) then
		DEALLOCATE(icurvw,weight)
	   endif
	   if(iopt1.eq.1) then
		iopt1=2
	   else
		iopt=iopt+1
	   endif
	   goto 33		!another plot?
	endif
c
C	NOW OPEN-OPEN DONE SAME WAY AS FOR OPEN/SHUT (MODIF 10/29/95 10:00pm)
c OPEN-OPEN NOT YET FIXED AS 04/20/04 11:46am FOR OPEN SHUT, WITH
C		sy2(i)=0.		!initialise
C		syy2(i)=0.		!initialise
C		ny2(i)=0		!initialise

c	print 702
c	if(discprt) write(8,702)
c702	format(' NEW VERSION OF OPEN/OPEN CALC (MODIF 10/29/95 10:00pm)')
c For each open period, (a) find which open time range it is in, (b) identify
c preceding and following open time (or open time which is n=lag away)
c and average these open times in the average appropriate to the open
c time range found)
702	continue
	print 108		!new line
	ans='Y'
	call DCASK(
     &'Calculate mean open period adjacent to spec OPEN period range',
     & ans,ans)
	opcal=ans.eq.'Y'
	deb=debug()
	if(opcal) then
	  if(yhio.gt.0.) then	!already set in CORCALS, or above
	     print 261,yhio
261	     format(
     & ' Exclude open times > ',g11.4,' ms [Y] ? ')
	     ans='Y'
	     call INPUTa(ans)
	     if(UC(ans).eq.'N') yhio=-1.
	  endif
	  if(yhio.lt.0.) then
		ymaxo=-1.e37
		do i=1,nyval
		   if(yval(i).gt.ymaxo) ymaxo=yval(i)
		enddo
		print 65,nyval,ymaxo
c65		format(' Open periods: ',i6,' values, maximum = ',g13.6,/,
c     & 	' Exclude any outlying values (check distribution!) [N] ? ')
	      ans='N'
	      call INPUTa(ans)
		if(UC(ans).eq.'Y') then
		   print 371,yhio
c371	  	   format(
c     & ' Exclude all open periods longer than Ymax (ms) [',f10.1,'] = ')
		   call INPUTr(yhio)
	         if(discprt) write(8,500) yhio
c500		   format(
c     &	   ' Open periods longer than ',g13.6,' excluded.')
		else
		   yhio=ymaxo+0.1
       	   if(discprt) write(8,501)
c501		   format(' No values excluded')
		endif
	  endif
c
	  nlag=5
	  print 5,nlag
c5	  format(
c     & '  Calculate for lags (lag=1 is adjacent) up to n [',i2,'] = ')
	  call INPUTi(nlag)
	  call DCASK('Print the values','N',ans)
	  noprint=ans.eq.'N'
	  yhi(10)=-1.	!to indicate not set yet
	  nval=0	!for default
	  if(nval.eq.0) then
c		nval=6
c		ylo(1)=treso
c		yhi(1)=0.3
c		ylo(2)=0.3
c		yhi(2)=1.0
c		ylo(3)=1.0
c		yhi(3)=3.
c		ylo(4)=3.
c		yhi(4)=10.
c		ylo(5)=10.
c		yhi(5)=30.
c		ylo(6)=30.
c		yhi(6)=10000.
c		yhi(10)=10000.	!so default used
c Modif 11/24/95 10:14am so default ranges calculated on basis of max
c open period (ymaxo, above) so do not get empty range at top
c=		dmin=alog10(tres)
c=		dlog=(alog10(ymaxo)-dmin)/6.0	!inc on log scale
c=		do i=1,6
c=		   d=dmin + float(i-1)*dlog
c=		   ylo(i)=10.**(d)
c=		   yhi(i)=10.**(d+dlog)
c=		enddo
c=		yhi(10)=10000.	!so default used
c=	  endif
c
c A third version -divide the openings more or less equally between
c ranges (ignore exclusions for this calc!)
c NB Must keep original order in YVAL, so copy to Yval1 to sort
		npr=ifixr(float(nyval)/6.)	!number per range
		ALLOCATE(yval1(ndimy))
		do i=1,nyval
		   yval1(i)=yval(i)
		enddo
		call SORTr(yval1,nyval,ndimy,.true.)
		ylo(1)=tres
		yhi(1)=yval1(npr)
		ylo(2)=yhi(1)
		yhi(2)=yval1(2*npr)
		ylo(3)=yhi(2)
		yhi(3)=yval1(3*npr)
		ylo(4)=yhi(3)
		yhi(4)=yval1(4*npr)
		ylo(5)=yhi(4)
		yhi(5)=yval1(5*npr)
		ylo(6)=yhi(5)
		yhi(6)=ymaxo
		yhi(10)=10000.	!so default used
		DEALLOCATE(yval1)
	  endif
	  print 303,(ylo(i),yhi(i),i=1,6)
303	  format(' The 6 default ranges (ms) are: ',/,
     &  3(f8.4,'-',f8.4,', '),/,3(f8.4,'-',f8.4,', '))
	  print 3032,(yloop(i),yhiop(i),i=1,nvalop)
3032	  format(' The ranges used last time (ms) were: ',/,
     &  3(f8.4,'-',f8.4,', '),/,3(f8.4,'-',f8.4,', '),/,
     &  3(f8.4,'-',f8.4,', '))
	  iropt=2
	  print 3033,iropt
3033	  format(
     & ' (1) Use default ranges ',/,
     & ' (2) Use same ranges as before',/,
     & ' (3) Define new ranges',/,
     & '  Option number [',i2,'] = ')
	  call INPUTi(iropt)
	  if(iropt.eq.1) then
	     nval=6
	  else if(iropt.eq.2) then
		nval=nvalop
		do i=1,nval
		   ylo(i)=yloop(i)
		   yhi(i)=yhiop(i)
		enddo
	  endif
	  if(iropt.eq.3) then
	     print 3031
3031	     format(
     & ' Number of open duration ranges to be specified (up to 10) = ')
	     call INPUTi(nval)
	  endif
	  print 37
	  if(discprt) write(8,37)
37	  format(/,/,
     & ' Mean durations of apparent open periods that are adjacent ',/,
     & '  to a open periods with durations in following ranges (ms)')
c
	  print 108
	  do i=1,nval
		sy0(i)=0.		!initialise
		syy0(i)=0.		!initialise
		ny0(i)=0		!initialise
		sy1(i)=0.		!initialise
		syy1(i)=0.		!initialise
		ny1(i)=0		!initialise
		sy2(i)=0.		!initialise
		syy2(i)=0.		!initialise
		ny2(i)=0		!initialise
		if(iropt.eq.3) then
		   print 381,i
381		   format(
     & '&Open times (ms) for range',i2,': low, high time = ')
		   call INPUT2r(ylo(i),yhi(i))
		   if(ylo(i).gt.yhi(i)) then
		      a1=ylo(i)
		      ylo(i)=yhi(i)
		      yhi(i)=a1
		   endif
		endif
	  enddo
c end of setting of open time ranges
c Keep the ranges used for conditional means (because if we want to
c average over experiments, best if same ranges used in all experiments
c that are averaged)
	  nvalop=nval
	  do i=1,nval
		yloop(i)=ylo(i)
		yhiop(i)=yhi(i)
c	 	print 721,i,ylo(i),yhi(i)
	 	if(discprt) write (8,721) i,ylo(i),yhi(i)
721		format(' Open period range ',i3,': from ',g13.6,' to ',
     &		g13.6,' ms')
	  enddo
c Allocate arrays
c	  n1=nval   	!for brevity
c	  n2=nlag
c reverse order for declaration of ns0 etc (better for acsii write)
	  n1=nlag   	!for brevity
	  n2=nval
	  if(allocated(ns0)) then
		DEALLOCATE(ns0,ym0,sd0,ns1,ym1,sd1,pm0,pm1,
     &	sx0,sx1,sx2,sdp,ns2,ym2,sd2,pm2)
	  endif
	  ALLOCATE(ns0(n1,n2),ym0(n1,n2),sd0(n1,n2),pm0(n1,n2),
     &	sx0(n1,n2),sx1(n1,n2),sx2(n1,n2),sdp(n1,n2))
	  ALLOCATE(ns1(n1,n2),ym1(n1,n2),sd1(n1,n2),pm1(n1,n2))
	  ALLOCATE(ns2(n1,n2),ym2(n1,n2),sd2(n1,n2),pm2(n1,n2))
	  sum0=0.0
	  ss0=0.0
	  na0=0
	  sum1=0.0
	  na1=0
	  lag=1
	  do while(lag.le.nlag)
	   do k=1,nval
		sx0(lag,k)=0.0
	   enddo
	   do i=1+lag,nyval-lag
		if(mod(i,20).eq.0) deb=debug()
		tin=yval(i)
		yv0=yval(i-lag)		!preceding open period
C MODIF 10/29/95 10:04pm
		yv1=yval(i+lag)	!following open period
c
		good1=tin.lt.yhio.and.yv1.lt.yhio
		good0=good1.and.yv0.lt.yhio		!preceding also OK
		if(good0) then
		   sum0=sum0+yv0+yv1		!for mean of all values used
		   ss0=ss0+(yv0*yv0)+(yv1*yv1)
		   na0=na0+2
		endif
		if(good1) then
		   sum1=sum1+yv1		!for mean of all values used
		   na1=na1+1
		endif
		do k=1,nval
		   if(tin.ge.ylo(k).and.tin.lt.yhi(k)) then	!OK- in range
			if(good0) then
			  sy0(k)=sy0(k) + yv0 	!add PREC open period lengths
			  syy0(k)=syy0(k) + yv0*yv0  ! for SD
			  ny0(k)=ny0(k)+1			!increment number
			  sy0(k)=sy0(k) + yv1 	!add FOLLOWING open period lengths
			  syy0(k)=syy0(k) + yv1*yv1  ! for SD
			  ny0(k)=ny0(k)+1			!increment number
			  sx0(lag,k)=sx0(lag,k) + tin	!mean of adj open used
			endif
			if(good1) then
			  sy1(k)=sy1(k)+yv1			!ditto for 'following' only
			  syy1(k)=syy1(k)+yv1*yv1		! for SD
c===			  sy1(k)=sy1(k)+yv0			!ditto for 'preceding' only
c===			  syy1(k)=syy1(k)+yv0*yv0		! for SD
			  ny1(k)=ny1(k)+1		!increment number
			endif
			if(deb) then
			   print 123,lag,i,yval(i),k,i-lag,yv0,
     &			i+lag,yv1,ny0(k),ny1(k)
			   if(discprt) write(8,123) lag,i,yval(i),k,i-lag,yv0,
     &			i+lag,yv1,ny0(k),ny1(k)
123			   format(
     &			' LAG = ',i4,/,
     &			' Open period #',i5,' = ',g13.6,
     &		   ' ms is in range ',i2,/,
     &		   '    Preceding open period, # ',i5,' = ',
     &	         g13.6,' averaged for this range',/,
     &		   ':   Following open period, # ',i5,' = ',
     &	         g13.6,' averaged for this range',/,
     &		   1x,i5,1x,i5,' values in averages 0 and 1')
			endif
		   endif
		enddo
	   enddo	!end of i=1,nyval loop
c     Print means
	   ss0=(ss0-(sum0*sum0/float(na0)))/float(na0-1)	!variance
	   ss0=sqrt(ss0/float(na0))		!=SD(mean) for overall mean
	   sum0=sum0/float(na0)
	   sum1=sum1/float(na1)
	   if(noprint.and.lag.gt.1) goto 711
	   print 125,lag
	   if(discprt) write(8,125) lag
125	   format(/,
     & ' MEAN OPEN PERIODS FOR LAG = ',i3,' FROM SPECIFIED OPEN PERIOD')
	   print 391,sum0,na0,ss0,sum1,na1
	   if(discprt) write(8,391) sum0,na0,ss0,sum1,na1
	   print 683		!col headings
	   if(discprt) write(8,683)
683	   format(
     &   8x,' Open period range (ms) ',5x,'  n   mean open',
     & '    SD(mean )     mean as',/,
     &   8x,'                        ',5x,'       period  ',
     & '                  % of all')
711	   continue
	   do j=1,2		!for 'both' and 'following gap'
	    do i=1,nval
	      if(j.eq.1) then
		   n=ny0(i)
		   ns0(lag,i)=n		!record for below
		   if(n.gt.1) then
			en=float(n)
	      	ym=sy0(i)/en
			pm=100.*ym/sum0	!as % of all open periods
	      	sd=sqrt((syy0(i)-(sy0(i)*sy0(i)/en))/(en-1.))
			sd=sd/sqrt(en)	!NOW = SD OF MEAN
			ym0(lag,i)=ym	!record for print as func of lag
			pm0(lag,i)=pm	!record for print as func of lag
			sd0(lag,i)=sd
			sx0(lag,i)=sx0(lag,i)/float(n/2)	!mean x value
c Approx SD of m=ym/sum0 if g<<1 and indep is
c (1/sum0)*sqrt[sd0**2 + (m*ss0)**2], where ss0=SD of sum0 and sd0 is sd of ym.
c*			sdp(lag,i)=100.*sd/sum0    !approx SD of pm0 (if denom exact)
			em=ym/sum0
			sdp(lag,i)=100.*sqrt((sd*sd) + (em*em*ss0*ss0))/sum0
	         endif
		else
		   if(i.eq.1.and.(.not.noprint)) then
			print 40
			if(discprt) write(8,40)
c40 			format(/,' For following open period only:')
		   endif
	         n=ny1(i)
		   ns1(lag,i)=n		!record for below
		   if(n.gt.1) then
			en=float(n)
	      	ym=sy1(i)/en
			pm=100.*ym/sum1	!as % of all open periods
	      	sd=sqrt((syy1(i)-(sy1(i)*sy1(i)/en))/(en-1.))
			sd=sd/sqrt(en)	!NOW = SD OF MEAN
			ym1(lag,i)=ym	!record for print as func of lag
			pm1(lag,i)=pm	!record for print as func of lag
			sd1(lag,i)=sd
		   endif
		endif
	      if(noprint) goto 703
	      if(n.le.1) then
		   print 4261,i,ylo(i),yhi(i),n
		   if(discprt) write(8,4261) i,ylo(i),yhi(i),n
c4261		   format(i4,3x,g13.6,' to ',g13.6,5x,i4)
		else
		   print 426,i,ylo(i),yhi(i),n,ym,sd,pm
		   if(discprt) write(8,426) i,ylo(i),yhi(i),n,ym,sd,pm
		endif
703		continue
	    enddo
	   enddo
c Print mean of adjacent ops separately
	   if(.not.noprint) then
	    do k=1,nval
	      print 291,k,sx0(lag,k)
	      if(discprt) write(8,291) k,sx0(lag,k)
291		format(' For range #',i3,' mean open period = ',g13.6)
	    enddo
	   endif
	   lag=lag+1
c	   pause 'lag incremented'
	   deb=debug()
	   sum0=0.0
	   ss0=0.0
	   na0=0
	   sum1=0.0
	   na1=0
	   do i=1,nval
		sy0(i)=0.		!initialise
		syy0(i)=0.		!initialise
		ny0(i)=0		!initialise
		sy1(i)=0.		!initialise
		syy1(i)=0.		!initialise
		ny1(i)=0		!initialise
	   enddo
	  enddo	!end of 'do while(lag.le.nlag)'
	  if(.not.noprint) then
	     print 6
	     if(discprt) write(8,6)
c6	     format(/,/,' SAME VALUES TABULATED AS FUNCTION OF LAG')
	  endif
	  do i=1,nval
		if(noprint) goto 705
	 	print 7,i,ylo(i),yhi(i)
	 	if(discprt) write (8,7) i,ylo(i),yhi(i)
7		format(/,' Open period range ',i3,': from ',g13.6,' to ',
     &		g13.6,' ms',/,
     &	'   lag',8x,'  n     mean open period    SD(mean)',
     &	'     mean as % of all ')
		do lag=1,nlag
		   n=ns0(lag,i)
		   pm=pm0(lag,i)		!as % of all open periods
	         if(n.le.1) then
			print 8,lag,ns0(lag,i)
			if(discprt) write(8,8) lag,ns0(lag,i)
c8			format(i5,8x,i5)
		   else
			print 9,lag,ns0(lag,i),ym0(lag,i),sd0(lag,i),pm
			if(discprt) write(8,9) lag,ns0(lag,i),
     &			ym0(lag,i),sd0(lag,i),pm
c9			format(i5,8x,i5,5x,3(3x,g13.6))
		   endif
		enddo
		print 40
		if(discprt) write(8,40)
c40 		format(/,' For following open period only:')
		do lag=1,nlag
		   n=ns1(lag,i)
		   pm=pm1(lag,i)		!as % of all open periods
	         if(n.le.1) then
			print 8,lag,ns1(lag,i)
			if(discprt) write(8,8) lag,ns1(lag,i)
		   else
			print 9,lag,ns1(lag,i),ym1(lag,i),sd1(lag,i),pm
			if(discprt) write(8,9) lag,ns1(lag,i),
     &			ym1(lag,i),sd1(lag,i),pm
		   endif
		enddo
705		continue
	  enddo
	  print 108
	  if(discprt) write(8,108)
c108	  format(/)
c
c Write to ascii file (no VPLOT here yet -view in CVFIT)
	  ans='N'
	  call DCASK('Save values as ASCII file',ans,ans)
	  if(ans.eq.'Y') then
c Want alternating columns of pm0,sx0 both of which are declared (nlag,nval)
c ie have nval columns
		ALLOCATE(data(nlag,3*nval))
		ndata=nlag	!number of lines of data
		ncol=3*nval	!nval entries in each line
		icolwid=13		!the minimum col width
		do i=1,ncol,3
		   call INTCONV(1+i/3,cnum)
		   colhead(i)='X (range '//charnb(cnum)//')'
		   colhead(i+1)='Y (range '//charnb(cnum)//')'
		   colhead(i+2)='SD(mean)('//charnb(cnum)//')'
		enddo
		ndim1=nlag
		ndim2=3*nval
		text=
     &	'Mean open period (% overall), adj. to open period range'
		lentext=nblank(text)
		j1=0
		do j=1,nval
		   j1=j1+3
		   do i=1,nlag
			data(i,j1-2)=sx0(i,j)
			data(i,j1-1)=pm0(i,j)
			data(i,j1)=sdp(i,j)
		   enddo
		enddo
		call ASCWRITE(data,ndata,ncol,text,ndim1,ndim2,lentext,
     &		icolwid,colhead,1)
		DEALLOCATE(data)
	   endif
	   DEALLOCATE(ns0,ym0,sd0,ns1,ym1,sd1,pm0,pm1,
     &    sx0,sx1,sx2,sdp,ns2,ym2,sd2,pm2)
c
	endif 	!end of calc of mean open adjacent . . .
c
710	continue	!jump here to skip all correlations
c
	if(allocated(ygap)) then
	   DEALLOCATE(ygap)
	endif
C
	if(id.eq.2) then
	   print 2321		!title
	   if(discprt) write(8,2321)
2321	   format(
     &	' (2) DURATION OF CONTIGUOUS OPEN TIMES (OPEN PERIODS)')
	else if(id.eq.4) then
	   print 237		!title
	   if(discprt) write(8,237)
237	   format(
     &   ' (4) DURATION OF OPEN PERIODS ADJACENT TO SPECIFIED GAP')
	else if(id.eq.5) then
	   print 240		!title
	   if(discprt) write(8,240)
240	   format(
     & ' (5) DURATION OF OPEN PERIODS WITH SPECIFIED GAP ON BOTH SIDES')
	endif
c Calculate overall mean of observed open periods here for use in TCRITS
c (via common/top/topen)
	if(id.eq.2.and.isolate.ne.1) then
	   topen=0.0
	   do i=1,nyval
		topen=topen + yval(i)
	   enddo
	   topen=topen/float(nyval)
	endif

c=======add temporary bit here to check stabplot -calc mean of
c====== non-overlapping groups of n values of yval(), and then calc SD
c======of these means
	if(id.eq.2) then
	   ans='N'
	   call DCASK('Check means of groups of n open periods',ans,ans)
	   if(ans.eq.'Y') then
		print 800
800		format(' n = ')
		call INPUTi(n)
		ngrp=nyval/n
		ALLOCATE(yb(ngrp))
		i1=1
		sxt=0.0
		do ng=1,ngrp
		   i2=i1+n-1
		   sx=0.0
		   sxx=0.0
		   do i=i1,i2
			sx=sx+yval(i)
			sxt=sxt+yval(i)
			sxx=sxx+yval(i)*yval(i)
		   enddo
		   en=float(n)
		   yb(ng)=sx/en
		   sb=sqrt((sxx - sx*sx/en)/(en-1.0))
		   print 801,ng,yb(ng),sb
		   if(discprt) write(8,801) ng,yb(ng),sb
801		   format(2x,i4,2(3x,g13.6))
		   i1=i2+1
		enddo
		sx=0.0
		sxx=0.0
		do i=1,ngrp
		   sx=sx+yb(i)
		   sxx=sxx+yb(i)*yb(i)
		enddo
		ybt=sxt/float(n*ngrp)
		en=float(ngrp)
		sb=sqrt((sxx - sx*sx/en)/(en-1.0))
		print 802,n*ngrp,ybt,ngrp,n,sb
		if(discprt) write(8,802) n*ngrp,ybt,ngrp,n,sb
802		format(' Overall mean of ',i6,' values = ',g13.6,/,
     &	  ' SD of ',i4,' groups of ',i4,' means = ',g13.6)
		DEALLOCATE(yb)
	   endif
	endif
c=======end of temporary check
c
c  Now carry on with pdf of all open periods
	goto 207
c
C  (id=3) NEXT SUBSECTION OF OHIST to calculate lengths of app openings
c adjacent to shut period in specified range (id=3)
251	continue
	print 2521
2521	format(' Lower, upper limits for length of adjacent gap (ms) = ')
	call INPUT2r(gaplo,gaphi)
	if(gaphi.lt.gaplo) then
	   a1=gaphi		!swap
	   gaphi=gaplo
	   gaplo=a1
	endif
	call DCASK('Use both preceding AND following gaps','y',ans)
	both=.true.
	if(UC(ans).eq.'N') both=.false.
	print 257,gaplo,gaphi
	if(discprt) write(8,257) gaplo,gaphi
257	format(
     & ' (3)Duration of individual apparent openings that are adjacent',
     & /,'     to a gap with duration between ',g13.6,' and ',g13.6,
     & ' ms.')
	if(.not.both) print 2572
	if(discprt.and.(.not.both)) write(8,2572)
2572	format(' ONLY the gap following the opening used')
c
      print 60
c60	format(/,
c     & ' Now get values (''Yval'') for the distribution',/,
c     & ' To see what is being done, put CAP LOCK on now',/,
c     & ' (press any key to continue)')
	call ANYKEY()
c
	j=0	!counts YVAL
c
	do 254 i=1,nint
	i1=i
	if(debug())call DEBTYP(0,i1,i1,tint,ampl,iprops,
     & 0,-1,-1,yval,.false.,nintt,ndimy)
c==	iamp=IAVAL(i1)
	setbad=badint(i1)		!tint(i) was unusable
	IF(ampl(i1).eq.0.or.setbad) goto 254      !exc shut or unusable durations
c==	A1=ACAL*FLOAT(IAVAL(i1))
	a1=ampl(i1)
C
C Next line excludes openings of dubious amp if exass is true
	IF(exass.and.dubious(i)) goto 254
c If current opening has not already been excluded by prev line then
c next test if amp is between specified limits -exclude if not
	IF(conam1.and.(a1.lt.amplo.or.a1.gt.amphi)) goto 254
c If still OK test whether the interval that precedes OR follows this
c opening is (a) shut and (b) within specified duration range
c   (1) preceding interval
	if(i1.eq.1) goto 255	!try following int
	if(.not.both) goto 255	!try following int
c==	iamp=IAVAL(i1-1)
	amp=ampl(i1-1)
	tin=tint(i1-1)
	if(amp.ne.0.or.badint(i1-1)) goto 255	!prev int is open or unusable
	if(exass.and.dubious(i1-1)) goto 255 !prev int is dubious
	if(tin.ge.gaplo.and.tin.lt.gaphi) goto 259	!OK- in range
	goto 255		!not in range- try following int
259	continue
	j=j+1
	YVAL(j)=tint(i)
	if(debug()) call DEBTYP(0,-1,-1,tint,ampl,iprops,
     & 0,j,j,yval,.false.,nintt,ndimy)	!print YVAL(j)
cc   (2) following interval (not tested if prev int was OK)
c   (2) following interval (test whether or not prev int was OK)
255	continue
	if(i1.eq.nint) goto 254		!finished
c==	iamp=IAVAL(i1+1)
	amp=ampl(i1+1)
	tin=tint(i1+1)
	if(amp.ne.0.or.badint(i1+1)) goto 254	!next int is open or unusable
	IF(exass.and.dubious(i1+1)) goto 254 !next int is dubious
	if(tin.ge.gaplo.and.tin.lt.gaphi) goto 256	!OK -in range
	goto 254		!not in range- look for next good opening
c goes to 256 when valid value found
256	continue
	j=j+1
	YVAL(j)=tint(i)
	if(debug()) call DEBTYP(0,-1,-1,tint,ampl,iprops,
     & 0,j,j,yval,.false.,nintt,ndimy)	!print YVAL(j)
254	continue	!end of i loop
	NYVAL=j
c
	goto 207
c
c NEXT SECTION FOR DURATIONS OF SHUT PERIODS
C
201	continue
	GHIST=.TRUE.
	tres=tresg
	if(shist) goto 309
	if(jset.gt.1) goto 309
c default
	id=1
	goto 310
314	print 315,id
315	format(
     & ' Type of distribution (1,2,3; 0=HELP)  [',i2,'] = ')
	call INPUTi(ID)
	IF(ID.GT.0) GOTO 309
310	print 311
311	format(
     & ' ===================================',/,/,
     & ' (1) Duration of all shut times')
	print 312
312	format(
     & ' (2) Duration of all shut times that are bordered on each side',
     & /,'    by openings IN a specified amplitude range')
	print 313
313	format(
     & ' (3) Duration of all sojourns at amplitudes that are NOT in',/,
     & '    in a specified amplitude range')
	GOTO 314
309	idsav=id
c	print 108
c	if(discprt) write(8,108)
	if(id.eq.1) then
	   print 301
301	   format(
     &' Exclude gaps bordered by ''dubious'' amplitude openings [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   exass=UC(ans).ne.'N'
	   if(exass.and.discprt) write(8,302)
302	   format(
     &' Gaps that are bordered by openings with ''dubious'' amplitudes',
     &' are excluded')
	   goto 321
	endif
c
	print 2271
c2271	format(' Define amplitude range:')
	print 227
c227	format('  Low amp, high amp (real pA with sign)= ')
	call INPUT2r(amplo,amphi)
	if(amphi.lt.amplo) then
	   a1=amphi		!swap
         amphi=amplo
	   amplo=a1
	endif
	if(discprt) write(8,4531) amplo,amphi
4531	format(' Amplitude range (pA)= ',g13.6,' to ',g13.6)
	tc=0.
	if(id.eq.3) tc=-1.	!see setdub
	call SETDUB(iscan,exass,shist,.false.,tc,trise,tcfac)
	if(exass.and.id.eq.3) print 2273
	if(exass.and.discprt.and.id.eq.3) write(8,2274)
2274	format(' (i.e. ANY dubious amp excludes whole ''gap'' period)')
c
	print 1080
	if(discprt) write(8,1080)
	goto (321,322,323) ID
c All gaps:
321	print 311		!title
	if(discprt) write(8,311)
      print 60
c60	format(/,
c     & ' Now get values (''Yval'') for the distribution',/,
c     & ' To see what is being done, put CAP LOCK on now',/,
c     & ' (press any key to continue)')
	call ANYKEY()
c
	j=0	!to count good gaps
	do 205 i=1,nint
	i1=i
C  -exclude if not shut, or if unusable
	if(ampl(i1).ne.0.or.badint(i)) goto 205
	if(exass) then
	   i0=i1-1
	   i2=i1+1
	   if(i0.gt.0.and.ampl(i0).ne.0) then
		if(dubious(i0)) goto 205
	   endif
	   if(i2.le.nint.and.ampl(i2).ne.0) then
		if(dubious(i2)) goto 205
	   endif
	endif
	j=j+1
	YVAL(j)=tint(i)
205	continue	!end of i loop
	NYVAL=j     !no of usable gaps
	goto 207	!Yval() now defined
c
c Option 2 (specified amp both sides of gap)
322	print 312		!title
	if(discprt) write(8,312)
	j=0
	do 317 i=2,nint-1		!so i-1,i+1 always defined
	i1=i
	if(debug()) call DEBTYP(0,i1-1,i1+1,tint,ampl,iprops,
     & 0,-1,-1,yval,.false.,nintt,ndimy)
c
	if(ampl(i).ne.0.) goto 317	!rest is done only when tint(i) is shut
	i1=i-1			!preceding opening
	i2=i+1			!following opening
	a1=ampl(i1)
	a2=ampl(i2)
	if(ampl(i1).eq.0.) then
	   call BELL(1)
	   print 41,i1,i
41	   format(' ERROR -intervals ',i6,' and ',i6,' both shut')
	   goto 317
	endif
	if(ampl(i2).eq.0.) then
	   call BELL(1)
	   print 41,i,i2
c41	   format(' ERROR -intervals ',i6,' and ',i6,' both shut')
	   goto 317
	endif
	if((A1.lt.amplo).or.(A1.gt.amphi)) goto 317	!exc if not in range
	if((A2.lt.amplo).or.(A2.gt.amphi)) goto 317	!exc if not also in range
C  -exclude if not shut, or if gap unusable
c now TINT(i) is a usable gap
c  -also exclude if either preceding or following opening is
c not in specified amp range (or is neg/unusable)
	if(badint(i)) goto 317	!exclude neg/unusable ops always
	IF(exass.and.dubious(i1)) goto 317 !exc dubious amps
	if((tc.gt.0.).and.tint(i1).lt.tc) goto 317  !exclude short ops
	if(badint(i2)) goto 317	!exclude neg/unusable ops always
	IF(exass.and.dubious(i2)) goto 317 !exc dubious amps
	if((tc.gt.0.).and.tint(i2).lt.tc) goto 317  !exclude short ops
c	i2=i-1		!preceding opening
c319	if(i2.lt.1.or.i2.gt.nint) goto 317
c	if(badint(i2)) goto 317	!exclude neg/unusable ops always
c	IF(exass.and.dubious(i2)) goto 317 !exc dubious amps
c	if((tc.gt.0.).and.tint(i2).lt.tc) goto 317  !exclude short ops
c	A1=ampl(i2)		!amp (pA), with sign
c	if((A1.lt.amplo).or.(A1.gt.amphi)) goto 317	!exc if not in range
c repeat above for opening AFTER the gap
c	if(i2.eq.i1+1) goto 318
c	i2=i1+1		!following opening
c	goto 319
c
318	j=j+1
	YVAL(j)=tint(i)	!valid gap found
	if(debug()) call DEBTYP(0,-1,-1,tint,ampl,iprops,
     & 0,j,j,yval,.false.,nintt,ndimy)	!print YVAL(j)
317	CONTINUE	!END OF I LOOP
	NYVAL=j		!NO OF USABLE GAPS
	goto 207	!Yval() now defined for id=2
c
c Option 3 (all intervals NOT in range)
323	print 313		!title
	if(discprt) write(8,313)
	Ifst=1
	j=0   !counts periods NOT in specified range
C Next look for start of a group of gaps i.e. any usable interval that is
c not in range. A single unusable interval in a group makes its length
c undefined so it is excluded.
347	continue
c==	iamp=IAVAL(Ifst)
c==	A1=ACAL*float(Iamp)
	amp=ampl(ifst)
	gap=(amp.lt.amplo).or.(amp.gt.amphi)
C Next line excludes intervals with dubious amp when exass=true
	if(exass.and.dubious(ifst)) goto 3471	!invalid start
	if(gap.and.(.not.badint(ifst))) goto 348	!start of group
3471	ifst=ifst+1
	if(ifst.le.nint) goto 347	!START NOT YET FOUND-TRY NEXT INTERVAL
	goto 993		!finished all data
348	bad=.false.
	j=j+1		!Count groups found so far (but may be bad)
	YVAL(j)=tint(ifst)		!first in grp (always usable)
	k=ifst+1    !index in tint() of interval following first of group
c k loop starts at 350
350	continue
c==	iamp=IAVAL(k)
c==	A1=ACAL*float(Iamp)
	amp=ampl(k)
	gap=(amp.lt.amplo).or.(amp.gt.amphi)
c Go to 349 when end of group found -ie when ANY period with amplitude
c IN specified range found
c Whole group is set bad, and so rejected, if (1) contains any unusable
c interval (2) when, if exass=true, it contains any interval with dubious
c amplitude OR if the interval that ends the group has 'dubious' amp.
	if(exass.and.dubious(k)) bad=.true.
	if(.not.gap) goto 349		!end of group found
	YVAL(j)=YVAL(j)+tint(K)	!add any subsequent openings to same yval
	if(badint(k)) bad=.true.	!this group bad, but finish anyway
	k=k+1
	if(k.le.nint) goto 350	!keep looking for end
	j=j-1		!last bst bad
	goto 993	!data finished before end found, so do not count
c Group completed
349	if(.not.debug()) goto 53
c print the group for debug
	call DEBTYP(0,ifst,k-1,tint,ampl,iprops,
     & 0,-1,-1,yval,.false.,nintt,ndimy)
c print interval that follows group
	call DEBTYP(0,k,k,tint,ampl,iprops,
     & 0,-1,-1,yval,.false.,nintt,ndimy)
c if good, print yval
	if(.not.bad) call DEBTYP(0,-1,-1,tint,ampl,iprops,
     & 0,j,j,yval,.false.,nintt,ndimy)	!print YVAL(j)
53	ifst=k+1	!another group completed (j updated above)
	if(bad) j=j-1	!if bad do not count. YVAL(j) gets overwritten
	if(ifst.lt.nint) goto 347	!look for start of next group
993	NYVAL=j
	goto 207
c
C
C NEXT SECTION FOR DISTRIBUTION OF AMPLITUDES
C (also option to list all groups of openings that contain sublevels (no
C distribution fitted)
202	AHIST=.true.
	id=idsav
	if(shist) GOTO 2021
	if(jset.gt.1) goto 2021
c
2301	continue
	id=1		!default here
	print 211
211	format(/,' (1) Distribution of fitted amplitudes')
	if(nopoint) goto 70	!no point amp options!
	print 212
212	format(' (2) All point amplitude distribution')
	print 213
213	format(' (3) Shut point amplitude distribution')
	print 214
214	format(' (4) Open point amplitude distribution')
	print 215
215	format(' (5) Open and shut point amplitude distribution')
70	continue
	print 216
216	format(' (6) Largest fitted amplitude in each open period')
	print 217
217	format(' (7) Fitted amplitudes -weight proportional',/,
     &  '     to duration (as in open point amplitude)' )
	print 218
218	format(' (8) Patlak mean-low variance histogram')
	print 2023,id
2023	format(/,' Type of distribution (1 to 8) [',i2,'] = ')
	call INPUTi(id)
	if(id.lt.1.or.id.gt.8) goto 2301
	if(nfile.gt.1.and.id.eq.8) then
	   call BELL(1)
	   print 652
652	   format(
     & ' Cannot do Patlak plots when several experiments pooled')
	   goto 2301
	endif
	if(nopoint.and.(id.ge.2.and.id.le.5)) goto 2301
c
	idsav=id
	if(id.ne.8) then
	   call DCASK('Reverse the sign of the amplitudes','y',ans)
	   revamp=ans.eq.'Y'
	else
	   revamp=.true.
	endif
2021  continue	!jump here if SHIST true
	print 1080
	if(discprt) write(8,1080)
	if(id.eq.1) then
         print 211
         if(discprt) write(8,211)
	else if(id.eq.2) then
         print 212
         if(discprt) write(8,212)
	else if(id.eq.3) then
         print 213
         if(discprt) write(8,213)
	else if(id.eq.4) then
         print 214
         if(discprt) write(8,214)
	else if(id.eq.5) then
         print 215
         if(discprt) write(8,215)
	else if(id.eq.6) then
         print 216
         if(discprt) write(8,216)
	else if(id.eq.7) then
         print 217
         if(discprt) write(8,217)
	else if(id.eq.8) then
         print 218
         if(discprt) write(8,218)
	endif
c
	if(id.ge.2.and.id.le.5) then
	   nyval=1		!prevent error in SETBIN1
	   PAHIST=.true.	!point amp histo
	   RETURN
	endif
c
	tc=0.		!see setdub
	if(id.ne.8) then
	   call SETDUB(iscan,exass,shist,.false.,tc,trise,tcfac)
C
         print 60
c60	   format(/,
c        & ' Now get values (''Yval'') for the distribution',/,
c        & ' To see what is being done, put CAP LOCK on now',/,
c        & ' (press any key to continue)')
	   call ANYKEY()
	endif
c
	if(id.eq.1.or.id.eq.7) then	!fitted amplitudes
	   j=0	!index for id=1
c=?	   j1=0	!index for id=7
	   do 206 i=1,nint
		i1=i
		if(badint(i1)) goto 206	!exclude neg/unusable always
		if(exass.and.dubious(I)) goto 206 !exc dubious amps
		if(ampl(i1).eq.0) goto 206	!exclude zero amps
		if((tc.gt.0.).and.tint(i1).lt.tc) goto 206  !exclude short ones
		amp=ampl(i1)
		if(revamp) amp=-amp
		j=j+1
		YVAL(j)=amp
		if(id.eq.7) then
		   wfreq(j)=tint(i1)*1000.	!length in microsec
		endif
C NOTE- INCLUDES SIGN
206	   continue
	   NYVAL=j
	   goto 207
	else if(id.eq.6) then	!largest amp in each open period
	   ifst=1
	   j=0   !counts periods in A (=subset of open states)
c  Look for start of a group of openings as above
51	   continue
	   a1=ampl(ifst)
	   open=a1.ne.0
C Next line excludes openings with dubious amps when exass=true
	   if(exass.and.dubious(ifst)) goto 54
	   setbad=badint(ifst)	!tint(ifst) was unusable
	   if(open.and.(.not.setbad)) goto 52	!start of group
54	   ifst=ifst+1
	   if(ifst.le.nint) goto 51	!start not yet found-try next interval
	   goto 59			!finished all data
52	   bad=.false.
	   j=j+1		!count groups found so far- but may be bad
c===	   YVAL(j)=tint(ifst)		!first in group (always usable)
	   YVAL(j)=ampl(ifst)		!first in group (always usable)
	   amax=abs(ampl(ifst))
	   k=ifst+1    !index in tint() of interval following first of group
c k loop starts at 55
55	   continue
	   a1=ampl(k)
	   open=a1.ne.0
c Go to 56 when end of group found.
c -open period ends when ANY shut period found (whether usable or unusable)
c Whole group is set bad, and so rejected, if (1) contains any unusable
c opening (2) when, if exass=true, it contains any opening with dubious
c amplitude OR if the interval that ends the group has 'dubious'amp.
	   if(exass.and.dubious(k)) bad=.true.
	   if(.not.open) goto 56		!end of group found
c===	   YVAL(j)=YVAL(j)+tint(k)	!add any subsequent openings to same YVAL
	   if(abs(ampl(k)).gt.amax) then
		amax=abs(ampl(k))
		Yval(j)=ampl(k)
	   endif
	   setbad=badint(k)		!tint(k) was unusable
	   if(setbad) bad=.true.	!this group bad, but finish anyway
	   k=k+1
	   if(k.le.nint) goto 55	!keep looking for end
	   j=j-1
	   goto 59	!data finished before end found, so do not use
c Open period completed
56	   continue
	   if(revamp) YVAL(j)=-YVAL(j)
	   if(.not.debug()) goto 57
c print the group for debug
	  call DEBTYP(0,Ifst,k-1,tint,ampl,iprops,
     &    0,-1,-1,yval,.false.,nintt,ndimy)
c print interval that follows group
	  call DEBTYP(0,k,k,tint,ampl,iprops,
     &    0,-1,-1,yval,.false.,nintt,ndimy)
c if good, print yval
	   if(.not.bad) call DEBTYP(0,-1,-1,tint,ampl,iprops,
     &     0,j,j,yval,.false.,nintt,ndimy)	!print YVAL(j)
57	   continue
	   ifst=k+1	!another group completed (j updated above)
	   if(bad) j=j-1	!if bad do not use. YVAL(j) gets overwritten
	   if(ifst.lt.nint) goto 51	!look for start of new group
C N.B. does not return if ifst=nint, as if tint(nint) is start of a
C group there is no way to tell if this group has been completed-
C (the next interval might have been open too)
59	   nyval=j
	   goto 207
	else if(id.eq.8) then	!Patlak
	   npat=10
	   tpat=float(npat)*sngl(dfinter)*0.001	!ms
	   print 81,npat,tpat
81	   format(' Use averages of n point segments, n [',i3,' (',
     &	f6.3,' ms)] = ')
	   call INPUTi(npat)
	   tpat=float(npat)*sngl(dfinter)*0.001	!ms
	   print 82
82	   format(' Increment for start point of segment [1] = ')
	   npatinc=1
	   call INPUTi(npatinc)
	   print 83,rms
83	   format(' Baseline noise (RMS) = ',g12.5,' pA: O.K. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(UC(ans).eq.'N') then
		print 84,rms
84		format(' Baseline noise (RMS, pA) = ')
	   endif
	   print 85
85	   format(
     &' Multiple of RMS noise below which an amplitude is accepted '
     &    '[1.0] = ')
	   sdfac=1.0
	   call INPUTr(sdfac)
	   sdpat=sdfac*rms
	   exc0=.false.
	   ans='Y'
	   call DCASK('Exclude zero currents',ans,ans)
	   if(ans.eq.'Y') then
	      print 851
851	      format(
     &	' Exclude all currents below Imin pA: Imin [0.5] = ')
		patmin=0.5
		call INPUTr(patmin)
		exc0=.true.
	   endif
c  Now open consam and calculate values
	   if(.not.consamdef) then
		cjdat=.false.
c Get consam/axon file name
		call GETCONS(adcfil,noconsam,title,cdate1,adctim1,nsam,
     &    srate,cjdat,irecfst,newform,ioffset,calfac,ffilt,idest)
		consamdef=.true.
	   endif
	   OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &       access='TRANSPARENT')
c      Read consam in segments of size nbuf.  Find nearest ibaselin() value
c      to the current set of npat points, and subtract it
c
	   nbuf=500000
	   noutlo=0
	   nouthi=0
	   do i=-5000,5000
		freqs(i)=0.0
	   enddo
	   ALLOCATE(idata(nbuf))
	   calfac2=calfac/sfac2		!converts intermed units to pA
	   j1=1
	   j2=nbuf
	   if(nsam.lt.nbuf) j2=nsam
	   nsec=1 + (nsam-1)/nbuf
	   nlast=nsam - (nsec-1)*nbuf
	   enpat=float(npat)
	   endf=float(npat-1)
	   cfac2=calfac*calfac
	   varpat=sdpat*sdpat
	   nyval=0
	   jflast=1
	   do j=1,nsec
		deb=debug()
		nread=nbuf
		if(j.eq.nsec) nread=nlast
		j1=1 + (j-1)*nbuf
		j2=j1+ nread - 1
		if(.not.deb) then
		   print 871,j,nsec
871		   format(' Calculating section ',i4,' out of ',i4)
		else
		   print 87,j,j1,j2
		   if(discprt) write(8,87) j,j1,j2
87		   format(' Section ',i7,' j1, j2 = ',2i11)
		   pause
		endif
		irec=ioffset + (2*j1-1)    !record # for 1st value to be read
		read(14,rec=irec) (idata(i),i=1,nread)
c	     Section now in so look at segments of length npat
c          Locate ibaselin(k) that is closest to present segment (index in
c		consam=j1 to j2)
		i1=1
		i2=npat
		do while (i2.le.nread)
		   s=0.0
		   ss=0.0
		   do i=i1,i2
			yv=float(idata(i))
			s=s + yv
			ss=ss + yv*yv
		   enddo
		   em=s/enpat
c		   sd=sqrt((ss - s*s/enpat)/endf)
c		   sd=sd*calfac		!in pA
		   var=(ss - s*s/enpat)/endf
		   var=var*cfac2		!in pA^2
		   em=em*calfac		!in pA
		   j3=j1+i1-1		!index in consam of start of current segment
		   call GETFIT(j3,kfit,kfit1,jflast,timsav,dfinter,nfits)
		   jflast=kfit	!reset for next call
		   base=float(ibaselin(kfit1))*calfac2	!in pA
		   em=em-base
		   if(revamp) em=-em
c		   include=sd.le.sdpat
		   include=var.le.varpat
		   if(exc0) then
			if(em.lt.patmin) include=.false.	!openings all positive
		   endif
		   if(include) then
			k1=1+ifixr(em*100.)
			if(k1.ge.-5000.and.k1.le.5000) then
			   freqs(k1)=freqs(k1) + 1.0
			else if(k1.lt.-5000) then
			   noutlo=noutlo+1
			else if(k1.gt.5000) then
			   nouthi=nouthi+1
		      endif
			if(deb) then
			   print 88,i1,i2,j3,kfit,kfit1,em,sd,k1,noutlo,nouthi
			   if(discprt) write(8,88)i1,i2,j3,kfit,kfit1,
     &			em,sd,k1,noutlo,nouthi
88		   	   format(' i1,i2,j3,kfit,kfit1 = ',3i11,2i6,/,
     &		    '	em, sd, k1, noutlo, nouthi = ',2g13.6,3x,3i5)
			endif
		   endif
		   i1=i1+npatinc
		   i2=i1+npat-1
c             If the whole of consam has not been fitted, better to stop
c		    when end of fitted part is reached i.e. when time>timsav(nfits)
		   if(kfit.eq.nfits) then
			goto 861		!jump out
		   endif
		enddo
	   enddo
861	   CLOSE(unit=14)
	   DEALLOCATE(idata)
c Print what was done
	   print 89,npat,tpat,npatinc,rms,sdfac
	   if(discprt) write(8,89) npat,tpat,npatinc,rms,sdfac
89	   format(/,/,
     &   ' AMPLITUDES: MEAN-LOW VARIANCE PLOT (Patlak)',/,
     &   ' Segments of n points averaged, n = ',i3,' (',f6.3,' ms)',/,
     &   ' Increment for start point of segment = ',i4,/,
     &   ' Baseline noise (RMS) = ',g12.4,' pA',/,
     &' Multiple of RMS noise below which an amplitude is accepted = '
     &    f7.2)
	endif
c
207	continue
	RETURN		!Rest done in SETBIN
	END


