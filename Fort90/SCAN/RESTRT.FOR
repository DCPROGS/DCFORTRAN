	subroutine RESTRT()
c To read/write values that allow  restart of SCAN
c
	real*8 dfinter,ttrans,tlast
c Thoughts about what these values are needed for:
c (1) For restart of SCAN (would need only tint, iampl + posn of last
c transition if carried straight on with fitting, but need more details
c to 'go back n fittings'
c (2) For open/shut point histograms: for this need ....

=================
FROM FITSUB:
====================
c NB: t0 is time of transition from 1st point in display,idata1(1)
c which corresponds to actual data point #infirst (abs index in whole CONSAM)
c so time from 1st data point in CONSAM is ttrans=t0 + finter*float(infirst-1)
c NB 30 minute record=1800 sec = 1.8x10**9 microsec -too big for single prec if
c aim was to calc all intervals from differences in such values -but used
c only to record abs time of last transition, so when next transition fitted
c the interval preceding it can be calculated -diff should never be less than
c 100 microsec, but even this accuracy implies 1 in 1.8x10**7 -still too big
c so better use real*8 for ttrans, tlast
	t0=theta(1)
	ttrans=dble(t0) + dfinter*dble(float(infirst-1))
c Next calc the interval completed by the 1st transition (unless
c it is 1st in the record, or TINT has already been set unusable=-1000.)
c First times:
	told=sngl(ttrans-tlast)		!in microsec
	tltemp=ttrans		!provisional value (until accepted) for new TLAST


=========================
BIT FROM PDP INSCAN:
====================
	Virtual tint(1024),iampl(1024)
	Virtual timsav(1024),ksect(1024)
	virtual ishut(1024)
===
	if(idisk.eq.1) call scopy('DL1:SCNSAV.DAT',SCNFIL)
	OPEN(UNIT=14,NAME=SCNFIL,TYPE='OLD',
     &	ACCESS='DIRECT',FORM='UNFORMATTED',RECORDSIZE=512,
     &	MAXREC=8)		!1024 words
12	read(14'4) iscan1,KT,NAMP,TOTAMP,IAMPSV,IBASSV,ISECT,
     &	ACAL,ICURS,NBLCK,NLAST,NSECT,DGAIN,
     &	IBOFF,NTRIG,IDISK1,ISCRIT,RMS,SMULT,FINTER,CALFAC,IEXP,NWRIT,
     &	IAVAMP,AVAMP,JFILT,JFILT1,INVERT,OPENUP,DEBUG,KT0,KT1,
     &	lastkt,nfits,tmaxf,ibdisp,isub
13	continue
	if(idig(3).eq.1) type 131,kt,namp,acal,calfac,finter,jfilt,
     &	jfilt1
131	format(' In INSCAN: kt,namp,acal,calfac,finter,jfilt,jfilt1= '
     &	/,2i8,3g13.6,2i8)
	read(14'1) (tint(i),i=1,512)
	if(kt.gt.512) read(14'2) (tint(i),i=513,1024)
	read(14'3) iampl
	read(14'5) (timsav(i),i=1,512)
	if(kt.gt.512) read(14'6) (timsav(i),i=513,1024)
	read(14'7) ksect
	read(14'8) ishut
	CLOSE(unit=14)
	goto 202	!********
	if(iscan1.eq.iscan) goto 202
	type 2031,iscan
2031	format(' Data on disc is from a different version: ISCAN= ',i5)
	if(iscan.eq.-2001) type 203
203	format(' (data is from threshold crossing program)')
	idest=99
	pause 'Type <CR> to exit'
	RETURN
202	continue
C RECORD KT IN KT0 (UNLESS WE ARE GOING DIRECTLY TO SCDAT-WRITE IN WHICH
C CASE KEEP OLD KT0) SO THAT WE CAN HAVE OPTION TO CONCATENATE ONLY THE
C NEWLY FITTED TRANSITIONS IN DSKWRT ( NEEDED IF EARLIER ONES,READ IN
C HERE, HAVE ALREADY BEEN PUT IN SCDAT). IF TINT(KT)=-1000. THEN MUST
C HAVE LEFT PREV RUN VIA E=END AND SO MAY (OR MAY NOT) HAVE PUT DATA
C IN SCDAT ALREADY. BUT IF TINT(KT) NOT -1000. THEN MUST HAVE LEFT
C PREV RUN BY CRASH (OR CONTROL C ETC) SO PREV DATA CANNOT HAVE BEEN
C PUT IN SCDAT, AND THIS OPTION NOT NEEDED. SIGNIFY THIS BY SETTING KT0
C NEGATIVE
c^^	ICURS1=ICURS
c^^	ICURS=-ICURS
C NEG ICURS PREVENTS RESET OF ICURS TO ZERO IN ROLDAT. IT CAUSES
C DASHED CURSOR IN SCROL8 (WHICH ALSO RESETS ICURS POSITIVE)
c^^	IBLCK1=IBLCK
	kt=lastkt
	tlast=timsav(kt)
	isect=iabs(ksect(kt))
	icsect=isect
	icurs=1+ifix(0.5+(tlast+0.5*tmaxf)/finter)
c
	TSECT=FLOAT(NBUF)*FINTER
c	IF(DEBUG) TYPE 3141,KT,KT0,KT1,TINT(KT)
c3141	FORMAT( ' IN INSCAN: KT,KT0,KT1,TINT(KT)= ',3I8,G13.6)
	if(debug) type 3141,ibassv,iampsv,iboff
3141	FORMAT( ' IN INSCAN: ibassv,iampsv,iboff= ',3I8)
C	IF(TINT(KT).GT.-999.) KT0=-KT0	!REMOVE FOR NOW
	DISP=.FALSE.
	TYPE 104
104	FORMAT( ' Go straight to write to permanent file? '$)
	ACCEPT 57,IANS
	IF(IANS.EQ.YES) IDEST=50
	IF(IDEST.NE.50) KT0=KT
	IF(IDEST.NE.50) TINT(KT)=0.0
C LEAVING VIA 'END' SETS TINT(KT) UNUSABLE ,WHICH WOULD PREVENT RESETTING
C OF IT TO  CORRECT VALUE AFTER RESTART SO RESET TO 0. ONCE KT0 SET
	GOTO 102	!RESET NWSAV ETC
C END OF RESTART SECTION
C
101	DISP=.FALSE.
d	goto 1011
	TYPE 800
800	FORMAT( ' Display only? '$)

==================
FROM INTACC
===================
c Note that timsav(i) is the time (mus from start of section) of the
c transition that PRECEDES the interval tint(i). The section in
c which this transition occurs is recorded in ksect(i), neg for the
c first transition in a fitting
===
	j=j+1			!when kt > 0
	TINT(KT)=temp(j)
	timsav(kt+1)=ttrans
	ksect(kt+1)=-isect	!neg value marks 1st trans of a fitting
===
	IF(NTRANS.EQ.1) GOTO 3341
	DO 3351 I=2,NTRANS
	j=j+1
	IF(TINT(KT).GT.-998.) TINT(KT)=temp(j)		!IN MSEC
c	TINT(KT)=temp(j)		!IN MSEC
	timsav(kt+1)=ttrans+dt(i)
	ksect(kt+1)=isect
	KT=KT+1
3351	continue

===========
FROM AFIT1
===============
	IAMPL(KT+J)=0		!SET AMP=0 IF CLOSED
c Store baseline for this interval (in orig CONSAM units, not
c offset by IBOFF)
	ishut(kt+j)=ibassv-iboff
===
	IAMPL(KT+J)=IX		!FOR SUBLEV ASSIGN ONE AMP EACH LOOP
	iampsv=iamp	!update (eg for 'last' in non-sublev mode)
c Store baseline for this interval (in orig CONSAM units, not
c offset by IBOFF)
	ishut(kt+j)=ibassv-iboff
	GOTO 323	!end of j loop
===
