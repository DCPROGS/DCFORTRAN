	subroutine STAB_HJC(tint,ampl,iprops,nint,nintt,nbad,nbad1,
     & isbad,iebad,namp,ampval,acal,trise,tcfac,iscan,idiskq,
     & index,iopt,mono,stabcut,jset)

c ACAL not defined in hjcfit!
c   ===exclude for now: ,nfile,iexstrt,iexend)
c
c To produce various types of stability plot
c STAB-HJc is version for HJCFIT of stabplot.for
c
c Modif 01/20/99 03:35pm by adding nfile,iexstrt,iexend, to params so
c boundaries between pooled files can be marked
c Modif 07/03/98 08:24pm to also display 'mean of nth shut time' etc (because
c needs vplot5 which is already in here)
c Modif 11/14/97 02:09pm for VHIST5 (can allocate xval etc to any
c  size now, but still fixed at old values for now)
c================================================================
c	Modified 22.10.97(my brother's b'day!!!
c	uses CROSSH ( cross hair cursor) in IVLIB
c	subroutine crossh(ix,iy,ixlo,iylo,ixhi,iyhi,ibk,icf,itype,
c     &	line,idraw,islope,ax,ay)
c     ix,iy =current cursor position in pixels
c     ixlo,iylo,ixhi,iyhi : min,max crosshair pixels
c     ibk = color background
c     itype=0 crosshair itype>0 not yet done
c     line=0 no line beetwen points
c	line=1 draw line ; line> 1 can be extendet to draw what ever you fancy
c	idraw=0 no symbol ; idraw=1 draw 1 symbol at click
c	idraw = n points drawn
c	islope= 0 horizontal line drawn
c	islope= 1 vertical line drawn  beetwen points
c	islope= 2 line drawn  beetwen points
c     iax,iay array to return the coordinates clicked
c================================================================
c Modif 06/15/95 10:30am by including index() as argument so that
c can also print transition numbers marked before resolution imposed
c  (then can go to this transition # in SCAN)
c Modif 03/24/94 10:37am for VHIST3 (Xcal, Ycal now 2048)
c Modified 06/23/92 02:08pm so does plots for amplitudes (iopt=8) as
c  well as for open, shut times (iopt=7)
c Initially version tried that restarts group of 50 (=nval) open periods
c if any bad gap found. This reduces the number of sequences rather
c seriously in some cases, but worse, it results in gaps in the histogram
c because the bins are not contiguous. Present version simply skips any
c bad gap (AND, for the calc of Popen, the open period that precedes it).
c
c iampl needed for debtyp (not needed for function IAVAL) but needed
c for reordering of data if bad bits removed
	real*4 TINT(nintt),ampl(nintt)
	integer*1 iprops(nintt)
	real*4 popen(10240),opbar(10240),gbar(10240)
	real*4 topen(20480)
	integer ifirst(20480),nopen(20480)
      real ax(10),ay(10)
	integer*4 index(nintt)
	LOGICAL OPEN,caplock,debug,bad,restart,mbad,deb
	logical discprt,interp,mono,single,exczero,pseudo,allocated
	logical stabcut
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character*1 ans,UC
	character cnum*11,cnum1*11		!for dialog
c for unusable bits:
	integer isbad(20),iebad(20)
	integer isgood(20),iegood(20)
c===	integer iexstrt(50),iexend(50)
c      character*1 ch,chb
c Arrays for Vplot
	real*4 ampval(10)	!to store amplitudes marked on stability plot
	ALLOCATABLE:: weight,icurvw
	real*4 weight(:,:)
	integer*4 icurvw(:)
cc for data
c	dimension ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd)
cc for calc curves
c	dimension ncal(ndimc),icurvc(ndimc),iline(ndimc)
	ALLOCATABLE XVAL1,YVAL1,XCAL1,YCAL1
	real XVAL1(:,:),YVAL1(:,:)		!for VPLOT
	real XCAL1(:,:),YCAL1(:,:)		!for VPLOT
c	real*4 XVAL1(2048,5),YVAL1(2048,5)	!for VPLOT2
	integer ndat1(5),icurvd1(5),isym1(5),ijoin1(5)	!for VPLOT2 data
	real*4 symsiz(5)
	real*4 theta(50)	!for call of vplot etc (not used)
c	real*4 XCAL1(2048,10),YCAL1(2048,10)	!for VPLOT2
	integer ncal1(10),icurvc1(10),iline1(10)		!for calc curve
c declarations for VHIST
	ALLOCATABLE XVAL,YVAL,XCAL,YCAL
	real XVAL(:,:),YVAL(:,:)		!for Vhist
	real XCAL(:,:),YCAL(:,:)		!for Vhist
c for histo data
	integer ndat(3),icurvd(3),ijoin(3),icol(100)
c for calc curves
	integer ncal(10),icurvc(10),iline(10)	!ndimc=10 for Vhist
	LOGICAL fitted,doframe,draft,plotonly,autplt,landscap,ivplot
	logical btest,badgap,dubious,mono2
	character*40 titlex,titley
	character*64 title1
	logical sbin,shist,sres,sexp
	logical exass
	character*40 qfile
	common/queue/qfile	!for vplot and vhist
c
	COMMON/cols/icol,mono2
	common/sblk/sbin,shist,sres,sexp
	common/RBLCK/treso,tresg,iacrit,mavamp
	common/dp/discprt
c	data star/'1H*,'/
c	data space/'1x,'/
c	data fmt(1)/'(1x,'/
c	data fmt(3)/'g13.6,'/
c	data fmt(5)/'g13.6,'/
c	data fmt(7)/'g13.6,'/
c	data fmt(9)/'g13.6,'/
c	data fmt(11)/'g13.6,'/
c	data fmt(12)/'i8)'/
c	data fmt(2),fmt(4),fmt(6),fmt(8),fmt(10)/5*'1x,'/
c
c Define functions
	debug()=caplock()
	dubious(n)=BTEST(iprops(n),0)	!ampl(i) was dubious (bit 0 set ='1')
	badgap(n)=BTEST(iprops(n),3)	!tint(i) was unusable (bit 3='8' set)
c
c Temp fix for problem that sometimes occurs when defining bad bits
	if(nbad.gt.0) then
	   nbad0=nbad
	   do i=1,nbad0
		if(isbad(i).eq.0.or.iebad(i).eq.0) then
		   if(i.le.nbad0) then
			isbad(i)=isbad(i+1)
			iebad(i)=iebad(i+1)
		   endif
		   nbad=nbad-1
		endif
	   enddo
	   if(nbad.lt.nbad0) then
		call BELL(4)
		print 321
321		format(
     &	'Problem 1 with marking bad bits: tell DC that isbad(1)=0')
	   endif
	endif
c
	do i=1,100
	   icol(i)=-1
	enddo
c Modif 11/14/97 02:09pm for VHIST5 (can allocate xval etc to any
	kmax=50	!dimension for theta, jfix
3 	format(i8)
101	format(a1)
c=	plotrue=.false.
	interp=.false.
	draft=.false.	!for VHIST
	plotonly=.false.	!for VPLOT
c	print 51,fmt
c51	format(1x,11a8)
c colours for dialog box
	icb=0		!background colour for DIALOG box
	ict=11	!text colour for DIALOG box
	if(mono) ict=15
c
778	if(iopt.eq.7) then
	   print 49
	   if(discprt) write(8,49)
49	   format(' Stability of open periods,shut times and P(open)')
	   single=.false.
	else
	   print 48
	   if(discprt) write(8,48)
48	   format(' Stability of amplitudes')
	   nval=1
	   print 521,nval
521	   format(' Number of amplitudes to be averaged [',i3,'] = ')
	   call INPUTi(nval)
533	   nstep=1
	   print 53,nstep
c53	   format('&Increment for running average [',i3,'] = ')
	   call INPUTi(nstep)
	   if(nstep.gt.nval) then
		print 531
c531		format(' Must not be greater than number averaged')
		call BELL(1)
		goto 533
	   endif
	   single=nval.eq.1.and.nstep.eq.1	!use VPLOT
	   ndv1=nintt
	   ndimd=5
	   if(allocated(icurvw)) then
		DEALLOCATE(icurvw,weight)
	   endif
	   kwi=1
	   kwj=1
	   ALLOCATE(icurvw(ndimd),weight(kwi,kwj))
	   do i=1,ndimd
		icurvw(i)=-1	!no SD
	   enddo
         ndc1=2048
	   ndimc=10
	   if(single) then
	 	if(allocated(xval1)) DEALLOCATE(xval1,yval1,xcal1,ycal1)
		ALLOCATE(xval1(ndv1,ndimd),yval1(ndv1,ndimd))
		ALLOCATE(xcal1(ndc1,ndimc),ycal1(ndc1,ndimc))
	   else
		ndv1=5120	!dimension for xval,yval
		nbinmax=ndv1
		ndc1=1024	!dimension for xcal, ycal
		ndimd=3
		ndimc=10
	      if(allocated(xval)) DEALLOCATE(xval,yval,xcal,ycal)
	      ALLOCATE(xval(0:ndv1,ndimd),yval(0:ndv1,ndimd))
	      ALLOCATE(xcal(ndc1,ndimc),ycal(ndc1,ndimc))
	   endif
	   tc=0.0
	   call SETDUB(iscan,exass,shist,.false.,tc,trise,tcfac)
	   deb=debug()
	   goto 500
	endif
c
c OPEN, SHUT, Popen
	ndv1=511	!dimension for xval,yval
	ndc1=1024	!dimension for xcal, ycal
	ndimd=3
	ndimc=10
	if(allocated(xval)) DEALLOCATE(xval,yval,xcal,ycal)
	ALLOCATE(xval(0:ndv1,ndimd),yval(0:ndv1,ndimd))
	ALLOCATE(xcal(ndc1,ndimc),ycal(ndc1,ndimc))
c=	real XVAL(0:511,3),YVAL(0:511,3)
c=	real XCAL(2048,6),YCAL(2048,6)		!ndimc=6 locally
c
	nval=50
47	continue	!return here for another run
	print 52,nval
52	format(' Number of open periods to be averaged [',i3,'] = ')
	call INPUTi(nval)
532	nstep=nval/2
	print 53,nstep
53	format('&Increment for running average [',i3,'] = ')
	call INPUTi(nstep)
	if(nstep.gt.nval) then
	   print 531
531	   format(' Must not be greater than number averaged')
	   call BELL(1)
	   goto 532
	endif
	tcrit=-1.	!neg=no exclusions
	print 54
54	format('&Exclude long intervals [N] ? ')
	read 101,ans
	if(UC(ans).eq.'Y') then
	  print 55
55	  format(
     & '&Exclude an open and shut time pair if either is longer',
     & ' than Tcrit (ms) = ')
	  call INPUTr(tcrit)
c	  read 4,tcrit
	endif
	print 56,nval
56	format(
     & '&Restart group of',i4,' open periods if bad gap found [N] ? ')
	read 101,ans
	restart=UC(ans).eq.'Y'
c
c	if(discprt) write(8,13) nval,nstep
13	format(
     & ' Sections of',i5,' open periods averaged. Start increment= ',i5)
	if(tcrit.gt.0.) then
	   if(discprt) write(8,14) tcrit
14	   format(' All interval longer than',f7.2,' ms omitted')
	endif
	if(restart) then
	   print 57,nval
c	   if(discprt) write(8,57) nval
57	format(
     & '&Group of',i4,' open periods restarted if bad gap found')
	else
	   print 58
c	   if(discprt) write(8,58)
58	format(' Bad gaps omitted without restarting group')
	endif
c
c
	nbo=0		!count number of abandoned open periods
c First identify all open periods, based on method in CDIST0, but
c no options to specify amp range, or to exclude dubious amplitudes
	ifst=1
	j=0   !COUNTS PERIODS IN A (=SUBSET OF OPEN STATES) FOR THIS EXPT.
C NEXT LOOK FOR START OF A GROUP OF OPENINGS I.E.ANY OPENING THAT HAS
C DEFINED DURATION (I.E.USABLE). A SINGLE UNUSABLE OPENING IN A GROUP
C MAKES ITS LENGTH UNDEFINE SO IT IS EXCLUDED
247	continue
	amp=ampl(ifst)
c=	iamp=IAVAL(ifst)
c	if(debug()) print 120,ifst,amp,tint(ifst)
120	format(' i,amp,tint= ',i5,2g13.6)
	open=amp.ne.0
	if(open.and.(.not.badgap(ifst))) goto 248	!START OF GROUP
2471	Ifst=Ifst+1
	if(Ifst.le.nint) goto 247	!START NOT YET FOUND-TRY NEXT INTERVAL
	goto 992			!finished all data
c start of group
248	bad=.false.
	j=j+1			!COUNT GROUPS FOUND SO FAR- BUT MAY BE BAD
	topen(j)=TINT(Ifst)		!FIRST IN GRP (ALWAYS USABLE)
	nopen(j)=1		!number of openings in this group
	ifirst(j)=ifst
	k=ifst+1    !INDEX IN TINT() OF INTERVAL FOLLOWING FIRST OF GROUP
c K loop starts at 250
250	amp=ampl(k)
c=	iamp=IAVAL(k)
c	if(debug()) print 120,k,iamp,tint(k)
	open=amp.ne.0
c Go to 249 when end of group found.
c With no amplitude constraint group ends when ANY shut period found
c    (whether usable or unusable)
c Whole group is set bad, and so rejected, if it contains any unusable
c opening
	IF(.not.open) GOTO 249		!end of group found
	topen(j)=topen(j)+tint(k)	!ADD ANY SUBSEQUENT OPENINGS TO SAME topen
	nopen(j)=nopen(j)+1		!increment number of openings
	if(badgap(k)) bad=.true.	!THIS GROUP BAD,BUT FINISH ANYWAY
	k=k+1
	if(k.le.nint) goto 250	!KEEP LOOKING FOR END
	j=j-1		!last open period was bad
	goto 992	!DATA FINISHED BEFORE END FOUND, SO DO NOT COUNT
c Group completed
249	continue
	Ifst=k+1	!ANOTHER GROUP COMPLETED (J UPDATED ABOVE)
	if(bad) then
	   j=j-1	!IF BAD DO NOT COUNT. topen(j),nopen(j) get overwritten
	   nbo=nbo+1
	endif
c	if(debug()) print 124,j,bad,ifirst(j),nopen(j),topen(j)
124	format(' open period #',i5,': bad,ifst,nopen,topen=',l4,2i5,g13.6)
	IF(Ifst.lt.nint) goto 247	!LOOK FOR START OF NEXT GROUP
C N.B. DOES NOT RETURN IF Ifst=NINT AS IF TINT(NINT) IS START OF A
C GROUP THERE IS NO WAY TO TELL IF THIS GROUP HAS BEEN COMPLETED-
C THE NEXT INTERVAL MIGHT HAVE BEEN OPEN TOO
C  NOTE THAT nops=NASET CALC HERE MAY NOT BE IDENTICAL WITH THAT
C FOUND IN CALC OF BURST DISTRIBUTIONS BELOW AS LATTER INCLUDES
C ONLY THOSE OPEN PERIODS THAT ARE PART OF VALID BURSTS (NO BAD
C OPENINGS AND PROPER END)
992	nops=j	!total number of open periods in data
c	if(debug()) pause 'open periods identified'
c
c METHOD
c Now have the number of open periods,nops, and their durations
c in topen(1),...,topen(nops), the number of openings in the open
c period in nopen(1),...,nopen(nops), and the index of the (open) time
c that is the first in each open period in ifirst(1),..,(nops), where
c ith open period starts with tint(k),k=ifirst(i). The last opening in
c this open period is tint(k+nopen(k)-1). In order to keep
c equal numbers of open and shut periods, the very last open period must
c be rejected.
c A shut (but possibly unusable) period must lie between every open
c period (there could actually be several shut times between each if
c the interval between good open periods contained some bad open
c periods (ones with one or more unusable openings) which were
c skipped when open periods were identified above). Thus each
c good open period is followed by a shut period, viz the ith
c open period is followed by the shut time TINT(m) where
c m=k+nopen(k). The next opening is preceded by the shut period tint(m1),
c m1=ifirst(i+1)-1, for i=1,...,nops-1.  Note that m=m1 as long as there
c is only a single shut period between the ith and (i+1)th open periods,
c ie no bad open periods lie between them.
c
c Open periods are in tint(m), m=1,nops-1 (last one not used- see above).
c
c Now average open periods # 1,..,nval; nstep+1,...,nval+nstep; ... eg if
c nval=50, nstep=20, average 1-50, 21-70, 41-90, 61-110 etc
c
C  print COL TITLES
	print 2015
	if(discprt) write(8,2015)
2015	FORMAT(
     &' Total time(s)',T16,'Mean open(ms)',T30,'Mean shut(ms)',T46,
     &' P(open)',T53,' Ops/sec',T62,' Nopen Nop1 Ngap',/)
c
	do 121 n=1,3
121	xval(1,n)=float(ifirst(1))	!trans # for 1st opening of 1st open period
	j=0		!counts number of groups of nval done
	i1=1		!index for open period #
	nbg=0
	toptot=0.0	!total open time
	toptot1=0.0	!total open time without excluded openings
	noptot=0
	noptot1=0
	tgtot=0.0	!total shut time used in sequences without excluded
	ngtot=0
	nli=0
	nlo=0
	nlg=0
c Main loop starts here
202	continue
	i2=i1+nval-1
	if(i2.gt.nops-1) goto 299	!all done
	opent=0.0		!open time in this group
	opent1=0.0		!ditto for open times not followed by bad gaps
	shutt=0.0
	j=j+1
	nvalo=0	!ops/group=nval unless tcrit excludes intervals, or bad gap
	nvalg=0	!gaps/group=nval unless tcrit excludes intervals, or bad gap
c Loop over NVAL open periods/shuttings starts here
	do 201 i=i1,i2		!loop for ith open period
	if(i.gt.nops-1)	then
	   j=j-1		!last group not completed
	   goto 291		!abandon it and finish
	endif
	opent=opent+topen(i)	!accumulate open periods
	opent1=opent1+topen(i)	!open periods that are NOT followed by bad gaps
	nvalo=nvalo+1
c now check shut time(s) following this open period
	k=ifirst(i)		!ith open period starts with tint(k)
	m=k+nopen(i)	!index of shut time after ith open period
	m1=ifirst(i+1)-1	!index of shut time preceding (i+1)th open period
	if(m.eq.m1.and.tint(m).gt.0.) then
	   shutt=shutt+tint(m)	!add following shut period
	   nvalg=nvalg+1
	else if(restart) then
	   j=j-1                !reset j
	   istart=i+1		!try again starting at (i+1)th open period
	   nbg=nbg+1		!count how many
	   print 81,i1,istart
81	   format(
     & ' Abandon group starting at open period #',i5,'; restart at #',
     & i5)
	   if(debug()) then
	      print 80,i,k,m,m1,topen(i)
80	      format(' i,k,m,m1,topen(i)=',4i5,g13.6)
	      do 83 n=m-1,m1+1
		print 82,n,ampl(n),tint(n)
		if(n.eq.m-1.or.n.eq.m1) print 108
108		format(/)
83		continue
82		format(' n,amp(n),tint(n)=',i5,2g13.6)
		if(debug().and.m1.gt.m) pause '>1 gaps bet open periods'
	   endif
	   goto 290			!abandon this group: try another
	else		!if bad, but not restart
	   opent1=opent1-topen(i)	!remove open period before bad gap
	   nvalo=nvalo-1
	   nbg=nbg+1			!count bad gaps
	endif
c test tcrit
	if(tcrit.gt.0.and.(topen(i).gt.tcrit.or.tint(m).gt.tcrit)) then
	   opent1=opent1-topen(i)	!remove both open
	   shutt=shutt-tint(m)	!and shut times
	   nvalo=nvalo-1		!one fewer values in this group
	   nvalg=nvalg-1		!one fewer values in this group
	   nli=nli+1
	   if(topen(i).gt.tcrit) nlo=nlo+1
	   if(tint(m).gt.tcrit) nlg=nlg+1
	endif
201	continue
c If reach here then a group has been successfully completed
c If RESTART then must have nval open periods,and nval shut times in
c each successful group
c If NOT RESTART then have nval open periods in opent, but nvalo=nval-nbg-nli
c open periods in opent1, from which openings succeeded by bad gaps ( or
c those succeeded by long gaps if tc>0) are excluded.
c  Shutt, from which bad (or long) gaps necessarilly excluded will have
c nvalg shut times=nvalo=nval-nbg-nli. NOTE that bad gaps may be either
c (1) gaps set to <0.0, or (2) gaps that contain more than one shut
c period (separated by bad open period, or (3) gaps > tc if tc>0
c
	toptot=toptot+opent
	toptot1=toptot1+opent1
	noptot=noptot+nval	!includes all ops, inc excluded ones
	noptot1=noptot1+nvalo	!without excluded ones
	tgtot=tgtot+shutt
	ngtot=ngtot+nvalg
	opbar(j)=opent/float(nval)	!use all openings
	gbar(j)=shutt/float(nvalg)	!use # of gaps actually added in (exc bad)
c for Popen use equal number of openings and gaps
	tt=opent1+shutt		!use only non-excluded openings
	popen(j)=opent1/tt
cc xmid= halfway between transition # (in original record) for the
cc (open) interval that starts the first open period in the group
cc and that for the shutting following the last open period in the group
cc	xmid(j)=float(ifirst(i1)+ifirst(i2)+nopen(i2))/2.0
c xaxis=upper edge for bin representing present Popen etc (would need
c xmid() if results plotted with VPLOT rather than VHIST)
	do n=1,3
	   xval(j+1,n)=float(ifirst(i2)+nopen(i2))
	enddo
c Print results for this group
	if(debug()) print 303,j,xval(j,1),opent,opent1,shutt
303	format(' j,xval(j,1),opent,opent1,shutt=',i4,4g13.6)
	if(debug()) then
	 print 20,ifirst(i1),ifirst(i2)
c=	 if(discprt) write(8,20) ifirst(i1),ifirst(i2)
20	 format(10x,' Index of first and last transitions= ',2i6)
	endif
	tt=tt*1.e-3	!in sec for printing
	freq=float(nvalo)/tt
c	fmt(2)=space
c	fmt(4)=space
c	if(nbo.gt.0) fmt(2)=star	!asterisk before TO
c	if(nbg.gt.0) fmt(4)=star	!asterisk before TG
c=	print 60,tt,opbar(j),gbar(j),popen(j),freq,nval,nvalo,nvalg
c=	if(discprt) write(8,60)tt,opbar(j),gbar(j),popen(j),freq,nval,
c=     & nvalo,nvalg
60	format(f10.3,2x,f10.4,6x,f10.4,4x,f8.5,f10.4,3i5)
c Prepare for next group
	i1=i1+nstep		!start next group at open period # i1
	if(i1.gt.nops-1) goto 299	!all done
	goto 202		!do next group
c
c Special bit when a group is abandoned because a bad shut time is found
c (i.e. a shut time set<0., or more than one shut time (ie containing bad
c open periods) between good open periods.
290	continue
	i1=istart
	goto 202
c
c Bit done when last group is abandoned uncompleted
291	continue
c
c All groups completed
299	continue
	nsect=j		!total number of groups done
c Last bin already set -this resets it wrongly!
c	do n=1,3
c	   xval(nsect+1,n)=float(ifirst(i2)+nopen(i2))	!upper bin boundary
c	enddo
c=trans # for gap after last opening period used
	if(restart) then
	  print 15,nops,nsect,nbo,nbg
c	  if(discprt) write(8,15) nops,nsect,nbo,nbg
15	  format(1x,i6,' open periods analysed in ',i4,' groups.',/,
     &  1x,i4,' open periods abandoned because of bad openings',/,
     &  1x,i4,' sections abandoned because of bad shut periods')
	else if(.not.restart) then
	  print 171,noptot,noptot1
171	  format(' NOPTOT,NOPTOT1=',2I8)
	  print 17,nops,nsect,noptot1,ngtot,nbo,nbg
c	  if(discprt) write(8,17) nops,nsect,noptot1,ngtot,nbo,nbg
17	  format(1x,i6,' open periods analysed in ',i4,' groups.',/,
     &  1x,i5,' open periods used for P(open) calculation',/,
     &  1x,i5,' shut periods used for mean gap and Po calculation',/,
     &  1x,i4,' open periods abandoned because of bad openings',/,
     &  1x,i4,' bad shut periods omitted')
	endif
	if(tcrit.gt.0.) then
	  print 16,nli,tcrit,nlo,nlg
c	  if(discprt) write(8,16) nli,tcrit,nlo,nlg
16	  format(1x,i4,' open-shut pairs excluded because one of them is'
     & ,' longer then',f7.2,' ms. Number of long openings, gaps= ',2i4)
	endif
c     & '',/,
	print 2014
	if(discprt) write(8,2014)
2014	FORMAT(/,' For whole run:')
	TT=toptot+tgtot
	tt1=toptot1+tgtot		!without excluded openings
	if(restart) then
	   popen1=toptot/TT 		!overall means
	else
	   popen1=toptot1/TT1 		!overall means
	endif
	opbar1=toptot/float(noptot)	!div by # of good openings
	gbar1=tgtot/float(ngtot)
c	fmt(2)=space
c	fmt(4)=space
c	if(nbotot.gt.0) fmt(2)=star	!asterisk before TO
c	if(nbgtot.gt.0) fmt(4)=star	!asterisk before TG
c	print 51,fmt
	tt=tt*1.e-3	!in sec for printing
	freq=float(noptot)/tt		!openings/second
	print 61,tt,opbar1,gbar1,popen1,freq,noptot,noptot1,ngtot
	if(discprt) write(8,61)tt,opbar1,gbar1,popen1,freq,
     & noptot,noptot1,ngtot
61	format(f10.3,2x,f10.4,6x,f10.4,4x,f8.5,f10.4,3i5,//)
c	print fmt,toptot,tgtot,TT,popen1,freq,noptot
c	if(discprt) write(8,fmt)toptot,tgtot,TT,popen1,freq,noptot
c	print 109
c	if(discprt) write(8,109)
c109	FORMAT(' * asterisk before value means it excludes one or more'
c     &	,' undefined intervals',/)
cC
c
c Plot the results?
c	print 25
c25	format(' Plot these results [Y] ? ')
c	read 101,ans
c	if(UC(ans).eq.'N') RETURN
c
252	print 251
251	format(
     & ' (1) Plot open times, shut times and P(open) on same graph',/,
     & ' (2) Plot open times only',/,
     & ' (3) Plot shut times only',/,
     & ' (4) Plot P(open) only',/,
     & ' (5) Finish plots',/,
     & ' Option number [1] = ')
	iplot=1
	call INPUTi(iplot)
	if(iplot.lt.0.or.iplot.gt.5) goto 252
	if(iplot.eq.0) iplot=1
	if(iplot.eq.5) goto 461
	csize=2.5
	ifont=3
	ifitype=0		!no display of fitted parameters
	ntx=5		!if not logt set initial input values
	nty=5
	itx=1
	ity=1
	ilabel=1
	titlex='Interval number '
	if(iplot.eq.1) then
	   titley='Popen, mean open (- -), shut '
	else if(iplot.eq.2) then
	   titley='Mean open time (ms) '
	else if(iplot.eq.3) then
	   titley='Mean shut time (ms) '
	else if(iplot.eq.4) then
	   titley='Mean P(open) '
	endif
	ilog=2		!log(y) vs X
c	if(debug()) ilog=0
	iscal=1		!scale internally
	xlo=-1		!whole screen
c	symsiz(1)=-1.0 	!default symbol sizes
	doframe=.true.
	landscap=.true.
	autplt=.false.
	fitted=.true.
	isval=0	!no arrow
	iask=-2	!erase on exit from VHIST; do not ask
	lt2=2
	xw=0.0
	inumy=-1
c Data sets
	ymax=0.0
	ymin=1.e30
	do i=1,nsect
	   yval(i,1)=opbar(i)
	   yval(i,2)=gbar(i)
	   yval(i,3)=popen(i)
c	   xval(i,1)=xmid(i)		!X-AXIS for VPLOT
c	   xval(i,2)=xmid(i)
c	   xval(i,3)=xmid(i)
	   if(gbar(i).gt.ymax) ymax=gbar(i)
	   if(popen(i).lt.ymin) ymin=popen(i)
	enddo
41	continue
c For VHIST bin boundaries already set in XVAL for j=1
c
	do j=1,3
c	   ndat(j)=nsect-1	!analogue of nbin
	   ndat(j)=nsect		!analogue of nbin
	   yval(0,j)=0.		!no flo bin
	   yval(nsect+1,j)=0.	!no fhi bin
	   xval(0,j)=xval(1,j)
c	   xval(nsect+1,j)=xval(nsect,j)
	enddo
c
	if(iplot.eq.1) then
	   ncurvd=3
	   do j=1,3
	      icurvd(j)=j		!display all sets
	      ijoin(j)=0		!continuous line
	   enddo
c	   ijoin(1)=2		!short dash for open
	else
	   ncurvd=1
	   j=iplot-1            !=1,2,3 for open,shut,Po
	   icurvd(1)=j
	   ijoin(1)=0		!continuous line
	endif
c
c Calc curves=line marking means
	ncalc=500		!# points for line
	dx=(xval(nsect+1,1)-xval(1,1))/float(ncalc-1)
	do j=1,3
	   ncal(j)=ncalc		!2 data points to draw straight line
	   do k=1,ncalc
		xcal(k,j)=xval(1,1)+float(k-1)*dx
	   enddo
	   iline(j)=2	!dashed line
	enddo
	do i=1,ncalc
	   ycal(i,1)=opbar1		!j=1,2,3 for open,shut Popen
	   ycal(i,2)=gbar1
	   ycal(i,3)=popen1
	enddo
	if(iplot.eq.1) then
	   ncurvc=3
	   do j=1,3
	      icurvc(j)=j		!display all sets
	   enddo
c=	   iline(1)=0        !continuous for open time
	else
	   ncurvc=1
	   j=iplot-1
	   icurvc(1)=j
	   iline(1)=2	!dashed line
	endif
cc If nfile>1 then add vertical lines to show boundaries between pooled files
c	if(nfile.gt.1) then
c	   isetcol=1
c	   ncalc=500		!# points to draw straight line
c	   if(ymin.gt.1.e-30) then
c	      aymin=alog10(ymin)
c	   else
c		aymin=-30.
c	   endif
c	   dlogy=(alog10(ymax)-aymin)/float(ncalc-1)
c	   nsav=ncurvc 	!# already defined (=1 or 3)
c	   ncurvc=ncurvc+nfile-1
c	   j=3	!3 calc curves already defined
c	   do i=1,nfile-1
c		j=j+1		!=4,5,...
c		icurvc(i+nsav)=j
c		ncal(j)=ncalc
c		do k=1,ncalc
c		   alogy=aymin+float(k-1)*dlogy
c		   ycal(k,j)=10**alogy
c		   xcal(k,j)=float(iexend(i))
c		enddo
c		iline(j)=1	!dotted line
c		icol(i+nsav+10)=11	!light blue
c	   enddo
c	endif
cc
cc
c	if(debug()) then
c	   do 84 i=1,nsect
c84	   print 85,i,xval(i,1),yval(i,1),xval(i,2),yval(i,2),
c     &	xval(i,3),yval(i,3)
c85	   format(i8,6g11.4)
c	   pause
c	endif
c
c Option to define region(s) to be analysed separately with cursors
c
	call VHIST5(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ILOG,ISCAL,
     & XMIN,XMAX,yMIN,yMAX,xtic,ytic,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,xw,lt2,inumx,inumy,
     & titlex,titley,ilabel,doframe,idiskq,
     & autplt,draft,itit,title1,csize,ifont,landscap,fitted,
     & theta,ifitype,ncomp,sval,isval,iask,
     & isetcol,ndv1,ndc1,kmax,iver)
c     & theta,ifitype,ncomp,sval,isval,iask,iver)
c
461	mbad=.true.
	if(.not.stabcut) then	!NB stabcut may be true on entry
	   print 46
46	   format(
     & ' (1) Make a new stability plot',/,
     & ' (2) Replot this stability plot (e.g. shut time only)',/,
     & ' (3) Mark BAD regions with cursors for further analysis ',/,
     & ' (4) Mark GOOD regions with cursors for further analysis ',/,
     & ' (5) Finish stability plots',/,
     & ' Option number [5] = ')
	   i=5
	   call INPUTi(i)
	   if(i.eq.0) i=5
	   if(i.eq.1) goto 47
	   if(i.eq.2) goto 252
	   if(i.eq.4) mbad=.false.
	   if(i.eq.5) goto 999	!return
	else if(stabcut) then
	   call BELL(1)
	   print 463
463	   format(' Bad sections already removed',/,
     & ' (must restart program to redefine them)',/,
     & ' (1) Make a new stability plot',/,
     & ' (2) Replot this stability plot (e.g. shut time only)',/,
     & ' (3) Finish stability plots',/,
     & ' Option number [3] = ')
	   i=3
	   call INPUTi(i)
	   if(i.eq.1) goto 47
	   if(i.eq.2) goto 252
	   if(i.eq.3) goto 999	!return
	endif
c NON-MOUSE VERSION (use arrows to move cursor, and hit character to mark
c	position)
c
c New section for marking bad bits with cursors
c
	stabcut=.true.	!to signal that bit has been cut out
	print 261
261	format(' Use vertical cursor to mark sections to be omitted')
	pause
c	call ANYKEY()
	iask=3	!leave at once, with graph on screen
	iscal=0
c redisplay
	call VHIST5(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ILOG,ISCAL,
     & XMIN,XMAX,yMIN,yMAX,xtic,ytic,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,xw,lt2,inumx,inumy,
     & titlex,titley,ilabel,doframe,idiskq,
     & autplt,draft,itit,title1,csize,ifont,landscap,fitted,
     & theta,ifitype,ncomp,sval,isval,iask,
     & isetcol,ndv1,ndc1,kmax,iver)
c
600	continue		!return here to mark good/bad bits for amp stab plot
	call CUrpos(100.,130.)		!start with vertical cursor central
	call CLRDIALOG(1,icb)	!restart at top of box
	call WDIALOG (1,'Mark the first bit ',ict)
	call WDIALOG (1,'Click left button for starting and ending'
     &	,ict)
	call WDIALOG (1,'Click right button to exit',ict)
	j=0
	ix=320
c	iy=240
	ibk=icol(71)
	icf=2
	itype=0
	line=1
	idraw=2
	islope=0
	ixlo=1
	iylo=56
	ixhi=639
	iyhi=386
	iy0=300
c	eps=1.
27	continue
	call crossh(ix,iy0,ixlo,iylo,ixhi,iyhi,ibk,icf,itype,
     &	line,idraw,islope,ax,ay,6,70,6,1.)
	iy0=300
c	call pixgra(iax(1),iay(1),rx1,ry1)
c	call pixgra(iax(2),iay(2),rx2,ry2)
c	i=iax(1)
c	j=iax(2)
	call grapix(ax(1),ay(1),iax1,iay1)
	call grapix(ax(2),ay(2),iax2,iay2)
	rx1=ax(1)
	rx2=ax(2)
c	ry1=ay(1)
c	ry2=ay(2)
	i=ifixr(rx1)
	j=ifixr(rx2)
	nbad=nbad+1
	if(nbad.gt.1.and.i.lt.iebad(nbad-1)) then
		call BELL(2)
	      call WDIALOG(2,'ERROR! WRONG BIT WILL BE BLACK',12)
	      call WDIALOG(1,
     & ' < 10 points since last bad bit; Start again',ict)
		nbad=nbad-1
		call hline(iax1,iax2,iy0,0)
		goto 270
	else
		if(i.lt.1) i=1
		if(i.gt.nint) i=nint
	      isbad(nbad)=i
	 	call INTCONV(i,cnum)
	 	call INTCONV(index(i),cnum1)
	 	call WDIALOG(1,
     & 	'Transition #'//CHARNB(cnum)//' (before resolution = #'//
     &	 CHARNB(cnum1)//')',ict)
	endif
	if(j.gt.isbad(nbad)+2) then
		if(j.lt.1) j=1
		if(j.gt.nint) j=nint
		iebad(nbad)=j
	 	call INTCONV(j,cnum)
	 	call INTCONV(index(j),cnum1)
	 	call WDIALOG(1,
     & 	'Transition #'//CHARNB(cnum)//' (before resolution = #'//
     &	 CHARNB(cnum1)//')',ict)
	else
		call BELL(2)
	      call WDIALOG(2,'ERROR! WRONG BIT WILL BE BLACK',12)
	      call WDIALOG(1,
     & 	'Bad bit shorter than 3 points: define End again',ict)
		nbad=nbad-1
		call hline(iax1,iax2,iy0,0)
		goto 270
	endif
270	ans='Y'
	if(nbad.gt.1) ans='N'
	call BELL(1)
 	call DEFOLTa(ans,defolt)
	call QDIALOG(1,'Mark another bit',defolt,ict,cans)
	call GETINPa(cans,ans)
	if(UC(ans).eq.'N') goto 99
	goto 27		!another bad bit?

c
c Bad bits defined- erase graph and remove bad bits from tint,iampl
99	continue
c If the marked sections were actually the GOOD bits, reverse good/bad here
	if(.not.mbad) then
	   do i=1,nbad
		isgood(i)=isbad(i)		!copy values
		iegood(i)=iebad(i)		!copy values
	   enddo
	   ngood=nbad
	   if(isgood(1).gt.1.and.iegood(ngood).lt.nint) then
		nbad=ngood+1		!new nbad -have bad bits at each end
		isbad(1)=1
		do 31 i=1,nbad	!nbad is actually ngood here
		 iebad(i)=isgood(i)-1
		 isbad(i+1)=iegood(i)+1
31		continue
		iebad(nbad)=nint  	!bad to end of sample
	   else
		call BELL(3)
	      call WDIALOG(2,'ERROR!',12)
	      call WDIALOG(1,
     & 	'Not yet fixed for good bits that reach ends',ict)
		nbad=0
		goto 461
	   endif
	endif
c	call ENDPLT		!needs <ENTER> so try
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
c
c Temp fix for problem that sometimes occurs when defining bad bits
	if(nbad.gt.0) then
	   nbad0=nbad
	   do i=1,nbad0
		if(isbad(i).eq.0.or.iebad(i).eq.0) then
		   if(i.le.nbad0) then
			isbad(i)=isbad(i+1)
			iebad(i)=iebad(i+1)
		   endif
		   nbad=nbad-1
		endif
	   enddo
	   if(nbad.lt.nbad0) then
		call BELL(4)
		print 32
32		format(
     &	'Problem 2 with marking bad bits: tell DC that isbad(1)=0')
	   endif
	endif
c
	print 71,jset
	if(discprt) write(8,71) jset
71	format(' Bad sections removed for SET ',i3)
c=	call REMBAD(tint,iampl,nint,nbad,isbad,iebad,pon())
	call REMBAD(tint,ampl,iprops,nint,nintt,nbad,
     &	isbad,iebad,index,.false.)
	nbad1=-nbad		!neg to signal that new values defined here
c
	if(allocated(xval)) DEALLOCATE(xval,yval,xcal,ycal)
	RETURN
c
c
500	continue	!jump here for amp stability
c
c
	if(single) then
c Pseudo-time option.  If two amplitudes are separated by a long gap then
c plot them further apart
c plot them further apart
	  print 65
65	  format('&Use pseudo-time for X axis [N] ? ')
	  ans='N'
	  call INPUTa(ans)
	  pseudo=.false.
	  if(UC(ans).eq.'Y') then
           print 67
67         format(
     &     ' Pseudo-time axis options:',/,
     &     ' (1) x increment = 1 for shut time < 1 ms',/,
     &     '     x increment = sqrt(shut time) for > 1 ms',/,
     &     ' (2) x increment = short for shut time < tcrit',/,
     &     '     x increment = long for shut time > tcrit',/,
     &     ' Option number [1] = ')
		ips=1
		call INPUTi(ips)
		pseudo=.true.
		tcrit=1.0		!ms for ips=1 option
		if(ips.eq.2) then
		   print 68
68		   format(' tcrit (ms) = ')
		   call INPUTr(tcrit)
		endif
c		print 66
c66		format(' a [1.0] = ')
c		a=1.0
c		call INPUTr(a)
	  endif
c
	  exczero=.true.
	  if(.not.pseudo) then
	     print 64
64	     format('&Include zero amplitude points [N] ? ')
	     ans='N'
	     call INPUTa(ans)
	     if(UC(ans).eq.'Y') exczero=.false.
	  endif
c
	  i1=0		!count valid values
	  xval1(1,1)=1.0		!needed for pseudo-time Xvals
	  delt1=1.0		!for ips=2
	  delt2=50.		!for ips=2
	  do 34 i=1,nint
	   amp=ampl(i)
	   if(deb) print 341,i,ampl(i),tint(i)
341	   format(i8,2g13.6)
	   if(exass.and.dubious(i)) goto 34 	!exc dubious amps
	   if(amp.eq.0.) then
	      if(pseudo) then
		   if(tint(i).le.tcrit) then
			deltx=delt1
		   else
			if(badgap(i)) then
c===orig  		   deltx=a*3.		!like 1000ms if bad
			   if(ips.eq.1) then
			      deltx=30.		!like 1000ms if bad
			   else
				deltx=delt2
			   endif
			else	!good gap>tcrit
			   if(ips.eq.1) then
c===orig			deltx=1.0 +a*alog10(tint(i))
				deltx=sqrt(tint(i))
			   else
				deltx=delt2
			   endif
			endif
		   endif
		   goto 34
		else			!not pseudo time
	   	   if(exczero) goto 34		!exclude zero amps
		endif
	   endif
c
	   if(badgap(i)) goto 34			!exclude neg/unusable always
c
	   if((tc.gt.0.).and.tint(i).lt.tc) goto 34	!exclude short ones
	      i1=i1+1
c=	      yval1(i1,1)=acal*float(iamp)
	      yval1(i1,1)=amp
		if(pseudo) then
	         if(i1.gt.1) xval1(i1,1)=xval1(i1-1,1)+deltx
		else
	         xval1(i1,1)=float(i)
		endif
c  Now an amplitude accepted, reset deltx=1 so this value will be used
c until another usable shut time encountered which will reset deltx
            deltx=delt1
	      if(deb) print 342,i1,yval(i1,1)
342		format('    accept # ',i5,' = ',g13.6)
34	  continue
	else		!if not single (use VHIST) -get groups of nval usable amps
	   ns=0	!number of groups calculated
	   i=1	!index in iampl()
	   j=0	!count number in group
	   s=0.	!sum of group
37	   continue		!return here to start next group
	   i=i+1
	   if(i.gt.nint) then
		xval(ns+2,1)=ilast	!upper boundary for last bin
		goto 38	!no more data -abandon last bin
	   endif
	   amp=ampl(i)
	   if(deb) print 341,i,ampl(i),tint(i)
	   if(badgap(i)) goto 35		!exclude neg/unusable always
	   if(exass.and.dubious(i)) goto 35 !exc dubious amps
	   if(amp.eq.0) goto 35				!exclude zero amps
	   if((tc.gt.0.).and.tint(i).lt.tc) goto 35	!exclude short ones
c==		amp=acal*float(iamp)
	      if(deb) print 342,i,amp
c342		format('    accept # ',i5,' = ',g13.6)
		j=j+1
		if(ns.gt.nbinmax+2) then
		   call BELL(1)
		   print 777,nbinmax
777		   format(' More than ',i5,' bins')
		   goto 778
		endif
		if(j.eq.1) then
		   xval(ns+1,1)=i	!lower bin bound=index in iampl() of 1st amp used
		endif
		s=s + amp				!sum for current group
		if(j.eq.nval) then		!group finished -nval values found
		   ilast=i		!index in iampl of last amplitude in group
		   ns=ns+1
		   yval(ns,1)=s/float(nval)
		   j=0 			!start next group
		   s=0.
		   if(deb) print 36,ns,yval(ns,1),xval(ns,1)
36		   format(' Mean amp for group ',i4,' = ',g13.6,
     &		' Lower bin boundary = ',g13.6)
		endif
35	   continue
	   goto 37		!next amplitude
38	   continue		!jump here when all data used
	   nsect=ns
c	   ndat(1)=nsect-1	!analogue of nbin
c	   yval(0,1)=0.		!no flo bin
c	   yval(nsect+1,1)=0.	!no fhi bin
c	   xval(0,1)=xval(1,1)
c	   xval(nsect+1,1)=xval(nsect,1)
	   ndat(1)=nsect		!=nbin
	   yval(0,1)=0.		!no flo bin
	   yval(nsect+2,1)=0.	!no fhi bin
	   xval(0,1)=xval(1,1)
	   xval(nsect+2,1)=xval(nsect+1,1)
	   if(deb) then
		do i=1,nsect
		   print 39,i,xval(i,1),yval(i,1)
		   if(discprt) write(8,39) i,xval(i,1),yval(i,1)
39		   format(1x,i4,': xval, yval = ',2g13.6)
		enddo
		if(discprt) write(8,391) ndat(1),xval(nsect+1,1)
391		format(' ndat = ',i8,' last bin boundary = ',i8)
	   endif
	endif		!end of histo data
c
	if(.not.single) then		!plot as histo
	   ndimd=3
	   ndimc=3
	   xmin=xval(1,1)		!needed for def of calc curves below
	   xmax=xval(ns+1,1)
	   ncurvd=1
	   icurvd(1)=1
	   goto 63
	endif
c
	ndat1(1)=i1
      print 509,i1
      if(discprt) write(8,509) i1
509	format(' Number of valid amplitudes plotted = ',i8)
      if(pseudo) then
	   if(ips.eq.1) then
      	if(discprt) write(8,69) delt1,tcrit
69          format(
     &     ' Pseudo-time axis:',/,
     &     '  x increment = ',f4.0,' for shut time < ',f6.1,' ms'/,
     &     '     x increment = sqrt(shut time) for > ',f6.1,' ms')
	   else if(ips.eq.2) then
      	if(discprt) write(8,70) delt1,tcrit,delt2,tcrit
70          format(
     &     ' Pseudo-time axis:',/,
     &     '  x increment = ',f5.0,' for shut time < ',f6.1,' ms',/,
     &     '  x increment = ',f5.0,' for shut time > ',f6.1,' ms')
	   endif
      endif
	xmin=xval1(1,1)-1.0 	!needed for def of xcal() below
	xmax=xval1(i1,1)+1.0
	ndimd=5
	ndimc=10
63	continue
	csize=2.5
	ifont=3
	ifitype=0		!no display of fitted parameters
	ilabel=1
	if(pseudo) then
	   if(ips.eq.1) then
		titlex='Pseudo-time (square root)'
	   else if(ips.eq.2) then
	      call DCFORMAT(tcrit,6,1,cnum1)
		call GBLANK(cnum1,11,n1,n2)
		titlex='Pseudo-time (tc = '//cnum1(n1:n2)//')'
	   endif
	else
	   titlex='Interval number'
	endif
	titley='Amplitude (pA)'
	ilog=0
	ntx=5		!if not logt set initial input values
	nty=5
	itx=1
	ity=1
	iscal=1		!scale internally
	xlo=-1		!whole screen
	symsiz(1)=-1.0 	!default symbol sizes
	doframe=.true.
	landscap=.true.
	autplt=.false.
	fitted=.true.
	ivplot=.false.
	isval=0	!no arrow
	ncjump=0
	nvjump=0
	iask=-2	!erase on exit from VPLOT; do not ask
	lt2=2
	xw=0.0
	inumy=-1
c Data sets
	if(single) then
	   ncurvd1=1
	   do j=1,ncurvd1
		icurvd1(j)=j		!display all sets
		ijoin1(j)=-1		!do not join
		isym1(j)=0		!single pixel
	   enddo
	else
	   ncurvd=1
	   do j=1,ncurvd
		icurvd(j)=j		!display all sets
		ijoin(j)=0		!continuous line
	   enddo
	endif
c
c Calc curves=line(s) marking mean(s).
508	continue
	iopt=1	!default
	if(namp.gt.0) iopt=2
	print 502,acal*float(mavamp),namp,iopt
502	format(
     & ' (1) Mark the approximate full level (',f8.2,' pA)',/,
     & ' (2) Mark the previously defined',i3,' amplitude levels',/,
     & ' (3) Define amplitude levels to be marked now',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(iopt)
	if(iopt.eq.1) then
	   namp=1
	   ampval(1)=acal*float(mavamp)
	else if(iopt.eq.3) then
	   print 503
503	   format(' Number of amplitude levels to be marked [1] = ')
	   call INPUTi(namp)
	   if(namp.lt.1) namp=1
	   do 504 i=1,namp
	   print 505,i
505	   format('& ',i3,': amplitude (pA) = ')
504	   call INPUTr(ampval(i))
	endif
c Put this on printout
	if(namp.gt.0) then
	   if(discprt) write(8,50)
50	   format(' Single channel plots have lines as follows:')
	   do i=1,namp
	      if(discprt) write(8,51) ampval(i)
51		format(' Line at ',f9.3,' pA')
	   enddo
	endif
c
c NB may have several 'calc' curves so simpler if there are not more than
c 2048 points for each!
c
	ncalc=1024
	if(single) then
	   ncurvc1=namp
	   dx=(xmax-xmin)/float(ncalc-1)
	   do j=1,ncurvc1
		ncal1(j)=ncalc
		icurvc1(j)=j		!display all sets
		iline1(j)=2		!dashed line
		icol(j+10)=9
		do i=1,ncal1(j)
		  xcal1(i,j)=xmin+float(i-1)*dx
		  ycal1(i,j)=ampval(j)
	      enddo
	   enddo
	   isetcol=0
cc        Mark boundaries of pooled files
c	   if(nfile.gt.1) then
c	    ymax=0.0
c	    ymin=1.e30
c	    do i=1,ndat1(1)
c		a=yval1(i,1)
c		if(a.gt.ymax) ymax=a
c		if(a.lt.ymin) ymin=a
c	    enddo
c	    isetcol=1
c	    ncalc1=20		!20 points to draw straight line
c	    dy=(ymax-ymin)/float(ncalc1-1)
c	    nsav=ncurvc1
c	    ncurvc1=ncurvc1+nfile-1
c	    j=nsav
c	    do i=1,nfile-1
c		j=j+1		!=4,5,...
c		icurvc1(j)=j
c		ncal1(j)=ncalc1
c		do k=1,ncalc1
c		   ycal1(k,j)=ymin+float(k-1)*dy
c		   xcal1(k,j)=float(iexend(i))
c		enddo
c		iline1(j)=1		!dotted line
c		icol(j+10)=11	!light blue
c	    enddo
c	   endif
c
	   itrace=0
	call VPLOT5(xval1,yval1,ndat1,icurvd1,ncurvd1,ijoin1,symsiz,ndimd,
     & xcal1,ycal1,ncal1,icurvc1,ncurvc1,iline1,ndimc,isym1,ilog,iscal,
     & xmin,xmax,ymin,ymax,xtic,ytic,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,plotonly,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,interp,
     & isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kmax,iver)
c     & isetcol,itrace,ndv1,ndc1)

	else
	   ncurvc=namp
	   dx=(xmax-xmin)/float(ncalc-1)
	   do j=1,ncurvc
		ncal(j)=ncalc
		icurvc(j)=j		!display all sets
		iline(j)=2		!dashed line
		do i=1,ncal(j)
		  xcal(i,j)=xmin+float(i-1)*dx
		  ycal(i,j)=ampval(j)
	      enddo
	   enddo
	   call VHIST5(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,ndimd,
     &   XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ILOG,ISCAL,
     &   XMIN,XMAX,yMIN,yMAX,xtic,ytic,xcross,ycross,ntx,nty,itx,ity,
     &   XLO,XHI,YLO,YHI,xw,lt2,inumx,inumy,
     &   titlex,titley,ilabel,doframe,idiskq,
     &   autplt,draft,itit,title1,csize,ifont,landscap,fitted,
     &   theta,ifitype,ncomp,sval,isval,iask,
     &   isetcol,ndv1,ndc1,kmax,iver)

	endif
c
4621	mbad=.true.
	if(.not.stabcut) then	!NB stabcut may be true on entry
	   print 462
462	   format(
     & ' (1) Replot with different amplitude lines',/,
     & ' (2) Make a different amplitude stability plot',/,
     & ' (3) Mark BAD regions with cursors for further analysis ',/,
     & ' (4) Mark GOOD regions with cursors for further analysis ',/,
     & ' (5) Finish stability plots',/,
     & ' Option number [5] = ')
	   i=5
	   call INPUTi(i)
	   if(i.eq.1) goto 508
	   if(i.eq.2) goto 500
	   if(i.eq.4) mbad=.false.
	   if(i.eq.5) goto 999	!return
	else if(stabcut) then
	   call BELL(1)
	   print 464
464	   format(' Bad sections already removed',/,
     & ' (must restart program to redefine them)',/,
     & ' (1) Replot with different amplitude lines',/,
     & ' (2) Make a different amplitude stability plot',/,
     & ' (3) Finish stability plots',/,
     & ' Option number [3] = ')
	   i=3
	   call INPUTi(i)
	   if(i.eq.1) goto 508
	   if(i.eq.2) goto 500
	   if(i.eq.3) goto 999	!return
	endif
c NON-MOUSE VERSION (use arrows to move cursor, and hit character to mark
c	position)
c
c New section for marking bad bits with cursors
c
	stabcut=.true.	!to signal that bit has been cut out
	print 261
	call ANYKEY()
	iask=3	!leave at once, with graph on screen
	iscal=0
c redisplay
	if(single) then
	call VPLOT5(xval1,yval1,ndat1,icurvd1,ncurvd1,ijoin1,symsiz,ndimd,
     & xcal1,ycal1,ncal1,icurvc1,ncurvc1,iline1,ndimc,isym1,ilog,iscal,
     & xmin,xmax,ymin,ymax,xtic,ytic,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,plotonly,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,interp,
     & isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kmax,iver)
c     & isetcol,itrace,ndv1,ndc1)
	else
	   call VHIST5(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,ndimd,
     &   XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ILOG,ISCAL,
     &   XMIN,XMAX,yMIN,yMAX,xtic,ytic,xcross,ycross,ntx,nty,itx,ity,
     &   XLO,XHI,YLO,YHI,xw,lt2,inumx,inumy,
     &   titlex,titley,ilabel,doframe,idiskq,
     &   autplt,draft,itit,title1,csize,ifont,landscap,fitted,
     &   theta,ifitype,ncomp,sval,isval,iask,
     &   isetcol,ndv1,ndc1,kmax,iver)
	endif
c
	goto 600	!to mark good/bad bits
c
999	continue
	if(allocated(xval)) DEALLOCATE(xval,yval,xcal,ycal)
	if(allocated(xval1)) DEALLOCATE(xval1,yval1,xcal1,ycal1)
	if(allocated(icurvw)) then
	   DEALLOCATE(icurvw,weight)
	endif
	RETURN
	END

