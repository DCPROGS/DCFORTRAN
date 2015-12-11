	subroutine RESINT(TINT,NINTt,nint,acal,iavamp,imode,iampl,
     & sublev,jfilt,fc,rms,trise,ihjc)
c Modif 03/11/92 01:57pm to allow HJC method of imposition of resolution
c OR original method (in which contigous intervals below resolution get
c set bad). If HJC method used ihjc=1 on exit, otherwise ihjc=2.
c 02/18/92 01:53pm RESINT is updated version of RESINT3 with some old
c bits removed
c	IMODE=0 if called from EKDIST
c	IMODE=1 if called from HJCFIT
c	IMODE=2 if called from SCDIR
c 05/27/89 08:38pm Lahey version of RESIN3 (debug no longer a param)
c
c  NINT1=no of intervals in each file read (local to this subroutine)
c  NINTT=total no of intervals in all files before resolution imposed
c  NINT=total no of intervals AFTER resolution imposed
c
c Dec 1988: as RESIN2 but also has Trise (fixed in DSKIN1) in call so
c resolution can be expressed as a multiple of Trise, and the corresponding
c value of A/Amax printed
c Dec 87: as RESINT but fc as well as jfilt in call (for EKDIS3 only
c at present; fc may be modif for filtering by tape etc)). Calls version
c of FALSEV that also uses fc.
C
	dimension TINT(20480)		!up to 20 *1024
	integer*2 iampl(20480)
	LOGICAL DEBUG,SUBLEV,pon,slock,caplock,deb
	LOGICAL RESOLV,DIFAMP		!FUNCTIONS
	logical sbin,shist,sres,sexp
	integer*2 ktype
	logical KBHIT		!Utility lib
	character ch,getch
	common/sblk/sbin,shist,sres,sexp
	character*1 ans,UC
	logical discprt
	common/dp/discprt
	COMMON/RBLCK/TRESO,TRESG,IACRIT,MAVAMP
C
C INPUT= TINT,IAMPL VALUES FOUND IN SCAN1, I.E. WITH MAX POS
C	RESOLUTION (AND TRESO,TRESG AFTER FIRST CALL)
C OUTPUT= TINT,IAMPL WITH SHORTEST RESOLVABLE TIME TAKEN AS
C	TRESO,TRESG (MILLISECONDS) FOR MEASUREMENTS OF OPEN AND GAP TIMES
C       RESPECTIVELY. OBVIOUSLY GET CORRECT ANSWER ONLY
C	IF TRES IS EQUAL TO OR GREATER THAN (I.E.RESOLUTION IS
C	LESS THAN) THE RESOLUTION OF THE INPUT.
C  OPENINGS SEP BY GAPS (AMP=0)LESS THAN TRESG ARE CONCATENATED (TOGETHER
C WITH THE INTERMEDIATE GAP) INTO A SINGLE OPENING IF THE AMPLITUDE
C OF THE TWO OPENINGS IS THE SAME WITHIN X PERCENT OF FULL AMPLITUDE.
C IF THE OPENINGS HAVE DIFFERENT AMPS THEY ARE NOT CONCAT BUT HALF THE
C SHORT GAP LENGTH IS ADDED TO EACH.
C   UNUSABLE INTERVALS ARE SUPPOSED TO BE ALWAYS RESOLVABLE AND THEY
C ARE KEPT AS UNUSABLE (=-1000.) BUT CAUSE A CONCATENATED GROUP TO END.
C   IF 2 OR MORE UNRESOLVABLE INTERVALS OCCUR IN SUCCESSION THEN
C THEY,TOGETHER WITH THE PRECEDING AND FOLLOWING RESOLVABLE INTERVAL
C (THE END AND START TIMES,RESPECTIVELY, OF WHICH WILL BE UNSURE) ARE
C COUNTED AS A SINGLE UNUSABLE INTERVAL WITH AMPLITUDE SET TO THAT
C OF THE FIRST RESOLVABLE INTERVAL IN THE GROUP (WHICH IS A LEGITIMATE
C AMPLITUDE ESTIMATE FOR AMP DISTRIBUTION). IF SUCH A GROUP IS FOLLOWED
C BY ANOTHER UNRESOLVABLE INTERVAL THIS MUST ALSO BE CONCAT INTO
C THE UNUSABLE GROUP. THUS AFTER FINDING 2 (OR MORE) CONSECUTIVE
C UNRESOLVABLE INTERVALS THESE (=PRECEDING RESOLVABLE INTERVAL) PLUS
C ALL SUCCESIVE INTERVALS ARE ALL CONCAT INTO A SINGLE UNUSABLE GROUP
C UNTIL TWO SUCCESIVE RESOLVABLE INTERVALS ARE FOUND. FORMER OF
C THESE IS PART OF THE UNUSABLE GROUP, LATTER OF THEM HAS PROPERLY
C DEFINED STARTING POINT AND IS START OF A NEW (POSSIBLY CONCAT) INTERVAL.
C   THUS RESOLUTION OF OUTPUT IS CONSISTENT WHEREAS THAT OF INPUT IS
C NOT, BECAUSE SHORTNESS OF INTERVAL THAT IT IS DECIDED TO FIT IN
C SCFIT WILL NOT BE ENTIRELY CONSISTENT.
C	EACH NEW CONCAT OPENING IS ASSIGNED AN AMPLITUDE THAT
C IS (1)THE MEAN OF THE NON-ASSUMED AMPS OF EACH OF THE OPENINGS
C THAT MAKE IT UP, IF THERE ARE ANY, OTHERWISE (2) THE MEAN OF
C THE ASSUMED AMPS.
C
	pon()=slock()
	debug()=caplock()
c
101	FORMAT(A1)
	if(imode.eq.2) then
	   sbin=.false.			!for SCDIR
	   shist=.false.
	   sexp=.false.
	   sres=.false.
	   fc=0.5*float(jfilt)	!kHz (-3dB)
	   trise=332.1/fc		!rise time (microsec)
	endif
c If call with SRES=true want another histogram with same resolution
c so TINT() already defined- just print values ,then go to 145
203	continue
	if(.not.sres) then
c Extra bit for glutamate etc
	 print 2031
2031	 format(' Calculate resolution by Howe method [N] ? ')
	 read 101,ans
	 if(UC(ans).eq.'Y') then
	   call AAMAXSUB(trise,treso,aamaxo)
	   treso=0.001*treso		!in ms
	   tresg=treso			!in ms
	   aamaxg=aamaxo
	   zo=1000.*treso/trise		!tres in ms, trise in mus
	   zg=zo
	   call FALSEV(treso,fc,rms,iavamp,acal,frato)
	   fratg=frato
	   goto 311	!back to normal prog to print results
	 endif
	endif
	if(imode.eq.0.or.imode.eq.2) then
	   if(.not.sres) then
	     x=treso*1000.
	     print 305,x		!default from .INI file
305        FORMAT('&Min resolvable open time (microsec) [',f8.2,'] = ')
	     call INPUTr(x)
	     treso=0.001*x		!in ms (default is in ms already)
	   endif
	   if(jfilt.eq.-1) goto 202
c Filter function for unfiltered signal has JFILT set to 0- use 10kHz
	   j=jfilt
	   if(j.eq.0) then
		j=10
		jfilt=j
		print 22
		if(pon()) write(7,22)
		if(discprt) write(8,22)
22		format(
     & ' Unfiltered signal- fc set to 10kHz (5kHz,-3dB) for risetime.')
	   endif
c Used only to calc 2.5*rise time for excluding short events
	   if(debug()) print 211,float(j),fc,acal*rms,acal*float(iavamp)
211	   format(
     & ' Filter(kHz),fc, RMS noise, mean amp= ',4g13.6)
	   call FALSEV(treso,fc,rms,iavamp,acal,frato)
	   zo=1000.*treso/trise		!tres in ms, trise in mus
	   aamaxo=erfs(0.88604*zo)
202	   if(.not.sres) then
	     x=tresg*1000.
	     print 3051,x		!default from .INI file
3051	     format('&Min resolvable shut time (microsec) [',f8.2,'] = ')
	     call INPUTr(x)
	     tresg=0.001*x		!in ms (default is in ms already)
	   endif
	   if(jfilt.ne.-1) then
	    call FALSEV(tresg,fc,rms,iavamp,acal,fratg)
	    zg=1000.*tresg/trise		!tres in ms, trise in mus
	    aamaxg=erfs(0.88604*zg)
	   endif
	else if(imode.eq.1) then	!HJCFIT
	   print 20,treso
20       format(
     & '&Resolution for open and shut times (microsec) [',f8.2,'] = ')
	   call INPUTr(treso)
	   treso=0.001*treso		!in ms (default is in ms already)
	   tresg=treso			!for HJCFIT
	   if(jfilt.ne.-1) then
	    call FALSEV(treso,fc,rms,iavamp,acal,frato)
	    zo=1000.*treso/trise		!tres in ms, trise in mus
	    aamaxo=erfs(0.88604*zo)
	    call FALSEV(tresg,fc,rms,iavamp,acal,fratg)
	    zg=1000.*tresg/trise		!tres in ms, trise in mus
	    aamaxg=erfs(0.88604*zg)
	   endif
	endif
311	continue
	if(.not.sres) then
	   print 201,frato,zo,aamaxo,fratg,zg,aamaxg
201	   format(
     & ' false event rate (per sec):',/,
     & '    for openings = ',g12.5,' (= ',g11.4,' risetimes, A/Amax = ',
     & g12.5,' )',
     & '   for shuttings= ',g12.5,' (= ',g11.4,' risetimes, A/Amax = ',
     & g12.5,' )',
     & ': O.K. [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') goto 203
	endif
	if(pon()) write(7,30) 1000.*treso,zo,aamaxo,1000.*tresg,zg,aamaxg
	if(discprt) write(8,30) 1000.*treso,zo,aamaxo,1000.*tresg,
     & zg,aamaxg
	if(sres) print 30,1000.*treso,zo,aamaxo,1000.*tresg,zg,aamaxg
30	format(' Resolution (microsec):',/,
     & '   for openings = ',f8.2,' (= ',g11.4,' risetimes, A/Amax = ',
     & g12.5,' )',/,
     & '   for shuttings= ',f8.2,' (= ',g11.4,' risetimes, A/Amax = ',
     & g12.5,' )',/)
	if(pon().and.jfilt.gt.0) write(7,32) frato,fratg
	if(discprt.and.jfilt.gt.0) write(8,32) frato,fratg
	if(sexp.and.jfilt.gt.0) print 32,frato,fratg
32	format(
     & '  false event rate (per sec) for openings  = ',g13.6,/,
     & '			      for shuttings = ',g13.6)
	call flush(7)
c
	if(treso.lt.0.00001.and.tresg.lt.0.00001) then
	   nint=nintt	!no resolution imposed
	   goto 145
	endif
	if(sexp.and.sres) goto 145
	if(sexp) goto 307
C NOW SET IACRIT AND MAVAMP. THEY ARE NOT USED HERE OR IN CALDIS IF
C SUBLEV=FALSE
	IF(.NOT.SUBLEV) GOTO 307
	MAVAMP=IAVAMP	!UNLESS RESET
	print 102,ACAL*FLOAT(IAVAMP)
102	FORMAT(' Full amplitude (pA) = ',G13.6,'  O.K. [Y] ? ')
	read 101,ans
	if(UC(ans).ne.'N') GOTO 109
	print 110
110	FORMAT(' Full amp (pA, with sign) = ')
	call INPUTr(x1)
c	read 103,X1
	MAVAMP=IFIXR(X1/ACAL)	!FULL AMP READ IN, IN ADC UNITS
109	acrit=abs(0.1*ACAL*FLOAT(IAVAMP))
	print 111,acrit
111	FORMAT('& pA for ''real'' amp difference [',f6.2,'] = ')
	call INPUTr(x)
c	read 103,X
	if(x.eq.0.) x=acrit
103	FORMAT(G13.6)
	IACRIT=IFIX(0.5+ABS(X)/ACAL)	!IN ADC UNITS
c	IF(DEBUG()) print 1031,IACRIT
c1031	FORMAT( ' IACRIT= ',I8)
	if(pon()) write(7,31) ACAL*FLOAT(MAVAMP),x
	if(discprt) write(8,31) ACAL*FLOAT(MAVAMP),x
31	format(' For sublevels take:',/,3x,
     & ' full amplitude (pA)= ',f8.2,'; pA for real difference= ',f6.3)
C
307	CONTINUE
C CHECK FOR NEG INTERVALS (ALREADY DONE IN LATEST SCAN1?), AND SET THEM
C UNUSABLE SKIPPING THOSE ALREADY SET UNUSABLE.
	DO 106 I=1,NINTt-1
	IF((TINT(I).GT.-1000.1).AND.(TINT(I).LT.-999.9)) GOTO 106
	IF(TINT(I).GT.0.0) GOTO 106
	call BELL(1)
	print 107,i,TINT(i)
	if(discprt) write(8,107) i,TINT(i)
107	format('  ****tint(',i4,')= ',g13.6,'  reset to -1000.')
	TINT(I)=-1000.
106	CONTINUE
C
	IF((TINT(NINTt).GT.-1000.1).AND.(TINT(NINTt).LT.-999.9))
     & GOTO 104
	call bell(1)
	print 1051,tint(nintt)
1051	format(' Last interval is ',g13.6,' :reset as unusable [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') goto 104
	TINT(NINTt)=-1000.
	print 105
	if(pon()) write (7,105)
	if(discprt) write (8,105)
105	FORMAT (' Last interval set unusable.',/)
104	CONTINUE
C
	if(debug()) then
	   print 503
	   if(discprt) write(8,503)
503	   format(' Input',35x,' Output')
	endif
	if(sres) goto 610
	print 600
600	format('&Options for resolution:')
	print 602
602	format(
     & ' (1) Use HJC definition of e-openings and e-gaps')
	print 603
603	format(
     & ' (2) Consecutive short intervals set unusable (old def)')
	print 605,ihjc		!ihjc from input
605	format(' Option [',i2,'] = ')
	read 601,i
601	format(i8)
	if(i.eq.1.or.i.eq.2) ihjc=i
610	if(ihjc.eq.1) then
         if(pon()) write(7,602)
         if(discprt) write(8,602)
	else
         if(pon()) write(7,603)
         if(discprt) write(8,603)
	endif
c
	KI=0	!counts concatenated intervals
	I=0
	deb=debug()
C
C IF THE FIRST INTERVAL IS BOTH USABLE AND RESOLVABLE
C THIS IS STARTING POINT. IF NOT LOOK FOR FIRST INTERVAL THAT
C IS BOTH, AND IS PRECEDED BY AN RESOLVABLE INTERVAL TOO (OTHERWISE
C ITS START WILL BE DEFINED ONLY BY THE POSITION OF THE PRECEDING
C UNRESOLVABLE INTERVAL AND SO WILL BE UNRELIABLE)
	I=I+1
	IF(RESOLV(I,TINT,iampl).AND.(TINT(I).GT.0.0)) GOTO 2  !FOUND IT
1	I=I+1
	IF(RESOLV(I,TINT,iampl).AND.RESOLV(I-1,TINT,iampl).AND.
     & (TINT(I).GT.0.0)) GOTO 2
	GOTO 1
C NOW START TO LOOK FOR UNRESOLVABLE INTERVALS
2	KI=KI+1
	if(KBHIT()) then
		ch=GETCH(ktype)
		deb=.not.deb
c		if(ktype.eq.0.and.ichar(ch).eq.59) deb=.not.deb		!F1 hit
	endif
	if(deb) then
	   print 501,i,tint(i),iampl(i)
         if(discprt) write(8,501)i,tint(i),iampl(i)
	endif
	call SUMAMP(-1,J,S,J1,S1,iampl)		!INITIALISE SUMS
	TINT(ki)=TINT(i)			!START NEW CONCAT GROUP
	call SUMAMP(I,J,S,J1,S1,iampl)		!ACCUMULATE AMPS
C IS NEXT INTERVAL RESOLVABLE?
7	if(i.eq.NINTt) goto 4		!END
	if(deb) then
	   print 501,i+1,tint(i+1),iampl(i+1)
         if(discprt) write(8,501)i+1,tint(i+1),iampl(i+1)
501	   format(i8,g13.6,i8)
	endif
	IF(RESOLV(I+1,TINT,iampl)) goto 4	!YES. ASSIGN AMP & GO ON
C NOT RESOLVABLE. HAS THE INTERVAL AFTER IT GOT THE 'SAME' AMP AS
C THAT BEFORE IT? NOTE THAT IT IS SAFE TO REFER TO I+2 IN THIS SECTION
C BECAUSE LAST INTERVAL IS -1000. AND SO COUNTED AS RESOLVABLE ALWAYS.
C SO IF I=NINTt-1 WE SKIP THIS SECTION VIA LAST LINE ABOVE
C  FIRST CHECK WHETHER THERE ARE 2 SUCCESIVE IRRESOLVABLE INTERVALS (unless
c ihjc=1)
	if(deb) then
	   print 501,i+2,tint(i+2),iampl(i+2)
         if(discprt) write(8,501)i+2,tint(i+2),iampl(i+2)
	endif
	if(ihjc.eq.1) goto 71				!skip this bit
	IF(RESOLV(I+2,TINT,iampl)) GOTO 71		!NO- CARRY ON
C   2 FOUND SO SET UNUSABLE AND LOOK FOR NEXT PAIR OF RESOLVABLE
C INTERVALS BEFORE PROCEDING
	TINT(KI)=-1000.
	if(deb) then
	   print 501,i+1,tint(i+1),iampl(i+1)
         if(discprt) write(8,501)i+1,tint(i+1),iampl(i+1)
	endif
	I=I+3
61	continue
	if(deb) then
	   print 501,i,tint(i),iampl(i)
	   if(discprt) write(8,501)i,tint(i),iampl(i)
	endif
	IF((RESOLV(I,TINT,iampl).AND.RESOLV(I+1,TINT,iampl)).OR.
     & I+1.GE.NINTt) GOTO 4
	I=I+1
	goto 61
C
71	TIN=TINT(KI)
	IF(.NOT.SUBLEV) GOTO 9		!ALWAYS SAME AMP
	IF(DIFAMP(I,I+2,iampl)) GOTO 5
C
C NOW BIT TO BE DONE IF TINT(I+1) UNRESOLVABLE AND INTERVALS
C ON EACH SIDE HAVE SAME AMP. CONCATENATE.
9	TINT(KI)=TIN+TINT(I+1)+TINT(I+2)
	IF((TIN.LT.-900.).OR.(TINT(I+1).LT.-900.).OR.
     & (TINT(I+2).LT.-900.)) TINT(KI)=-1000.
C   IF ANY INTERVAL IS UNUSABLE SET WHOLE GROUP UNUSABLE
	CALL SUMAMP(I+2,J,S,J1,S1,iampl)	!ACCUMULATE AMPS
CD	print 113,2,I,KI,J,S,J1,S1
	I=I+2
	GOTO 7	!CONTINUE CONCAT UNTIL RESOLVABLE INTERVAL FOUND
C
C NEXT BIT DONE IF TINT(I+1) UNRESOLVABLE BUT INTERVALS ON EACH
C SIDE HAVE 'DIFFERENT' AMPS.
5	TIN1=TINT(I+1)
	TINT(KI)=TIN+0.5*TIN1
	IF((TIN.LT.-900.).OR.(TIN1.LT.-900.)) TINT(KI)=-1000.
	TINT(KI+1)=0.5*TIN1+TINT(I+2)
	IF((TINT(I+2).LT.-900.).OR.(TIN1.LT.-900.)) TINT(KI+1)=-1000.
C   CONCAT OF TINT(KI) NOW ENDED SO ASSIGN AMP TO IT
	IF(J1.GT.0) IAMPL(KI)=IFIXR(S1/FLOAT(J1))	!ASSUMED MEAN
	IF(J.GT.0) IAMPL(KI)=IFIXR(S/FLOAT(J))		!MEASURED MEAN
C   TINT(KI+1) (APPROX=TINT(I+2)) MAY NEED MORE THINGS CONCAT WITH IT
C   SO INITIALISE SUMS AND START NEW ONES.
	CALL SUMAMP(-1,J,S,J1,S1,iampl)		!INITIALISE AMPS
	CALL SUMAMP(I+2,J,S,J1,S1,iampl)	!ACCUMULATE AMPS
	KI=KI+1
	I=I+2
	GOTO 7		!CARRY ON CONCAT
C
C ASSIGN AMP IF CONCATENATION FINISHED
4	IF(J1.GT.0) IAMPL(KI)=IFIXR(S1/FLOAT(J1))		!ASSUMED MEAN
	IF(J.GT.0) IAMPL(KI)=IFIXR(S/FLOAT(J))		!MEASURED MEAN
	I=I+1
	if(deb) then
	   print 502,ki,tint(ki),iampl(ki)
	   if(discprt) write(8,502)ki,tint(ki),iampl(ki)
502	   format(30x,i8,g13.6,i8)
	endif
	IF(I.LE.NINTt) GOTO 2
C
C
	NINT=KI
c	if(imode.eq.2) RETURN		!for SCDIR
145	continue
	print 289,NINT
	if(pon()) write(7,289) NINT
	if(discprt) write(8,289) NINT
289	FORMAT( ' Number of resolved intervals= ',I8,/)
	  if(debug()) then
		print 23
23		format(' Print intervals and amplitudes # n1,n2 [skip] = ')
		read 24,n1,n2
24		format(2i8)
		if(n1.gt.0) then
		  if(n2.lt.n1) n2=n1
		  do 25 i=n1,n2
		  print 26,i,tint(i),iampl(i)
		  if(discprt) write(8,26) i,tint(i),iampl(i)
26		  format(i6,g13.6,i6)
25		  continue
		endif
	  endif
c
	RETURN
	END


	SUBROUTINE SUMAMP(I,J,S,J1,S1,iampl)
	integer*2 IAMPL(20480)	!up to 20 *1024
c	COMMON/ABLCK/IAMPL(1024)
C TO SUM AMPLITUDES TO CALC MEAN AMP OF A CONCATENATED GROUP
C (MEASURED AMPS IN S, ASSUMED IN S1). IF I=-1 IT INITIALISES.
C ASSUMED AMPS HAVE 10000 ADDED (IF POS) OR SUBRACTED (IF NEG)
C ONLY 'EQUAL' AMPS SHOULD BE AVERAGED SO ALL WILL BE POS OR
C ALL NEG IN GIVEN SUM, SO THE ADDED/SUBTRACTED 10000 WILL COME
C THROUGH IN THE MEAN
CD	print 10,I,J,S,J1,S1
CD10	FORMAT( ' I,J,S,J1,S1= ',I8,2X,2(I8,G13.6))
	IF(I.GE.1) GOTO 1
	J=0
	S=0.0
	J1=0
	S1=0.0
	RETURN
C
1	IF(IABS(int4(IAMPL(I))).GT.9000) GOTO 2	!ASSUMED
	J=J+1
	S=S+FLOAT(IAMPL(I))
	RETURN
C
2	J1=J1+1
	S1=S1+FLOAT(IAMPL(I))
	RETURN
	END


	LOGICAL FUNCTION RESOLV(I,TINT,iampl)
	dimension TINT(20480)
	integer*2 IAMPL(20480)	!up to 5 *1024
	COMMON/RBLCK/TRESO,TRESG,IACRIT,MAVAMP
C
C SET TRUE IF TINT(I) IS RESOLVABLE (OR UNUSABLE- ASSUMED LONG)
C NOTE: CANNOT HAVE BOTH TINT AND IAMPL IN COMMON BECAUSE THEY
C ARE EQUIVALENCED (WITH YVAL) IN MAIN PROG
	RESOLV=.TRUE.
	IF(TINT(I).LT.-900.) RETURN	!UNUSABLE
c
	IAMP=IAMPL(I)
	IF(IAMP.GT.9000) IAMP=IAMP-10000
	IF(IAMP.LT.-9000) IAMP=IAMP+10000
	IF((IAMP.EQ.0.AND.TINT(I).LT.TRESG).OR.
     & (IAMP.NE.0.AND.TINT(I).LT.TRESO)) RESOLV=.FALSE.
c	print 1,I,TRESO,TRESG,RESOLV
c1	FORMAT( ' RESOLV: I,TRESO,TRESG= ',I8,2G13.6,L2)
	RETURN
	END


	LOGICAL FUNCTION DIFAMP(I1,I2,iampl)
	integer*2 IAMPL(20480)	!up to 20 *1024
c	COMMON/ABLCK/IAMPL(1024)
	COMMON/RBLCK/TRESO,TRESG,IACRIT,MAVAMP
C
C FALSE (I.E. AMPLITUDES ARE 'SAME') IF (1) BOTH AMPS ARE ZERO
C OR (2) BOTH ARE NON-ZERO BUT DIFFER BY LESS THAN IACRIT (ADC UNITS)
C
	IAMP1=IAMPL(I1)
	IF(IAMP1.GT.9000) IAMP1=IAMP1-10000
	IF(IAMP1.LT.-9000) IAMP1=IAMP1+10000
	IAMP2=IAMPL(I2)
	IF(IAMP2.GT.9000) IAMP2=IAMP2-10000
	IF(IAMP2.LT.-9000) IAMP2=IAMP2+10000
	DIFAMP=.TRUE.
	IF(IAMP1.EQ.0.AND.IAMP2.EQ.0) DIFAMP=.FALSE.
	IF(IAMP1.NE.0.AND.IAMP2.NE.0.AND.
     & IABS(IAMP1-IAMP2).LT.IACRIT) DIFAMP=.FALSE.
	RETURN
	END


