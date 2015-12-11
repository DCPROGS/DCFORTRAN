	subroutine OPFREQ(tint,iampl,nint,xsec,otot,gtot,nopen,ntmin)
	dimension TINT(5120)
	integer*2 iampl(5120)
c 05/28/89 12:25pm Lahey version
c Modif May 1989; now specify in call the minimum number of transitions
c =ntmin (rather than min of 40 OPENINGS) from which a value of Popen is
c to be calc
	LOGICAL OPEN,pon,slock,caplock
	logical discprt
	character*1 ans,UC
	character*8 fmt(12)
	character*8 star,space
	common/dp/discprt
	data star/'1H*,'/
	data space/'1x,'/
	data fmt(1)/'(1x,'/
	data fmt(3)/'g13.6,'/
	data fmt(5)/'g13.6,'/
	data fmt(7)/'g13.6,'/
	data fmt(9)/'g13.6,'/
	data fmt(11)/'g13.6,'/
	data fmt(12)/'i8)'/
	data fmt(2),fmt(4),fmt(6),fmt(8),fmt(10)/5*'1x,'/
c
C TO CALC OPENING FREQ ETC IN EKDIST (SEP SUBROUTINE RATHER THAN
C PART OF CALDIS
c
	pon()=slock()
c
c	print 51,fmt
c51	format(1x,11a8)
	if(.not.caplock()) goto 53
	print 52,ntmin
52	format(' Minimum number of transitions for calc of P(open)= ',i4,
     &	' O.K. [Y] ? ')
	input 101,ans
101	format(a1)
	if(UC(ans).ne.'N') goto 53
	print 54
54	format(' new minimum number= ')
	input 55,ntmin
55	format(i8)
53	continue
C INITIALISE VAR USED FOR COUNTING OPENING FREQ ETC
	OTOT=0.
	OTOTL=0.
	GTOT=0.
	GTOTL=0.
	NOPL=0
	NOPEN=0	!COUNTS OPENINGS OVER ALL EXPTS
	ntrans=0
	ntlast=0
	nbotot=0	!total number of bad (undefined length) openings
	nbgtot=0	!total number of bad (undefined length) gaps
	nbol=0		!ditto for current section
	nbgl=0
	ifirst=1	! transition # for first transition of a section
C NEXT SECTION FOR DURATIONS OF SHUT PERIODS (AND OPENING FREQ ETC)
	XSEC=XSEC*1.E3		!IN MSEC
	XSECL=XSEC
C  print COL TITLES
	print 2015
	if(pon()) write(7,2015)
	if(discprt) write(8,2015)
2015	FORMAT(
     &'    Open (ms)',T16,' Shut (ms)',T30,' Total (s)',T46,
     &' P(open)',T56,' Openings/sec',T70,' N(open)',/)
C
1	CONTINUE

C   THIS LOOP ADDS TOTAL OPEN AND GAP TIMES FOR OPENING FREQ ETC.
C ANY OPENINGS OR GAPS THAT ARE UNUSABLE (UNDEFINED LENGTH) ARE OMITTED
	do 2 i=1,nint-1		!skip last interval
	i1=i
	OPEN=IACAL(I1,iampl).NE.0
c	print 300,i,tint(i),iampl(i)
c	if(pon()) write(7,300) i,tint(i),iampl(i)
c	if(discprt) write(8,300) i,tint(i),iampl(i)
c300	format(i4,g13.6,i8)
	IF(TINT(i).gt.0.0) goto 21	!OK- carry on
	if(open) nbotot=nbotot+1	!count bad openings
	if(.not.open) nbgtot=nbgtot+1	!count bad gaps
c	print 301,nbotot,nbgtot
c	if(pon()) write(7,301) nbotot,nbgtot
c	if(discprt) write(8,301) nbotot,nbgtot
c301	format(' nbotot,nbgtot= ',2i8)
	goto 2		!skip bad intervals
21	continue
	IF(.NOT.OPEN) GTOT=GTOT+TINT(I)
	IF(OPEN) OTOT=OTOT+TINT(I)
	IF(OPEN) NOPEN=NOPEN+1
	ntrans=ntrans+1
C	NEXT BIT PRINTS OPENING FREQ ETC AS SOON AS TOTAL TIME JUST
C	 EXCEEDS SPECIFIED VALUE, BUT ONLY IF AT LEAST 20 INTERVALS
C	 COLLECTED SINCE LAST PRINTOUT
	nt=ntrans-ntlast
	NN=NOPEN-NOPL	!NO OF OPENINGS FOR THIS SECTION
c	print 303,nn,nt,gtot+otot,xsecl
c	if(pon()) write(7,303) nn,nt,gtot+otot,xsecl
c	if(discprt) write(8,303) nn,nt,gtot+otot,xsecl
c303	format(' nn,nt,gtot+otot,xsecl= ',2i5,2g13.6)
c	IF((NN.LT.40).OR.(GTOT+OTOT.LT.XSECL)) GOTO 2	!NO PRINT
      if((nt.lt.ntmin).or.(gtot+otot.lt.xsecl)) goto 2	!no print
      ilast=i		!transition number for last opening in sec
	TO=OTOT-OTOTL	!OPEN TIME FOR THIS SECTION
	TG=GTOT-GTOTL	!GAP TIME	  "
	nbo=nbotot-nbol		!no of bad openings in current section
	nbg=nbgtot-nbgl		!no of bad gaps in current section
	TT=(TO+TG)*1.E-3    !TOTAL TIME IN SECONDS (SHOULD BE NEAR XSEC)
c	print 302,nbo,nbol,nbg,nbgl,ifirst,ilast
c	if(pon()) write(7,302) nbo,nbol,nbg,nbgl,ifirst,ilast
c	if(discprt) write(8,302) nbo,nbol,nbg,nbgl,ifirst,ilast
c302	format(' nbo,nbol,nbg,nbgl,ifirst,ilast= ',6i5)
	fmt(2)=space
	fmt(4)=space
	if(nbo.gt.0) fmt(2)=star	!asterisk before TO
	if(nbg.gt.0) fmt(4)=star	!asterisk before TG
c	print 51,fmt
	print fmt,TO,TG,TT,(TO*1.E-3)/TT,FLOAT(NN)/TT,NN
	if(pon()) write(7,fmt) TO,TG,TT,(TO*1.E-3)/TT,FLOAT(NN)/TT,NN
	if(discprt) write(8,fmt) TO,TG,TT,(TO*1.E-3)/TT,FLOAT(NN)/TT,NN
c30	FORMAT(5(1X,G13.6),I8)
c31	FORMAT(1x,g12.5,'*',4(1X,G13.6),I8)	!asterisk on TO
c32	FORMAT(1x,g13.6,1x,g12.5,'*',3(1X,G13.6),I8)	!asterisk on TG
	if(caplock()) print 20,ifirst,ilast
	if(pon().and.caplock()) write(7,20) ifirst,ilast
	if(discprt.and.caplock()) write(8,20) ifirst,ilast
20	format(10x,' Index of first and last transitions= ',2i6)
	OTOTL=OTOT	!KEEP OTOT ETC VALUES AT LAST PRINT
	GTOTL=GTOT
	NOPL=NOPEN
	ntlast=ntrans
	XSECL=XSECL+XSEC
	nbol=nbotot
	nbgl=nbgtot
	ifirst=ilast+1	!trans # for 1st trans of next section
2	CONTINUE
C
	print 2014
	if(pon()) write(7,2014)
	if(discprt) write(8,2014)
2014	FORMAT(/,' For whole run:')
	TT=(OTOT+GTOT)*1.E-3	!IN SEC
	fmt(2)=space
	fmt(4)=space
	if(nbotot.gt.0) fmt(2)=star	!asterisk before TO
	if(nbgtot.gt.0) fmt(4)=star	!asterisk before TG
c	print 51,fmt
	print fmt,OTOT,GTOT,TT,(OTOT*1.E-3)/TT,FLOAT(NOPEN)/TT,NOPEN
	if(pon()) write(7,fmt) OTOT,GTOT,TT,(OTOT*1.E-3)/TT,
     & FLOAT(NOPEN)/TT,NOPEN
	if(discprt) write(8,fmt) OTOT,GTOT,TT,(OTOT*1.E-3)/TT,
     & FLOAT(NOPEN)/TT,NOPEN
	print 108
	if(pon()) write(7,108)
	if(discprt) write(8,108)
108	FORMAT(' * asterisk before value means it excludes one or more'
     &	,' undefined intervals',/)
C
	if(pon()) call flush(7)
	RETURN
	END

