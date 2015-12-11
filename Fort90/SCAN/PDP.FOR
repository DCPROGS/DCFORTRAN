EXTRACT FROM INTEST TO DETECT TRANSITION:
==========================================
	IT=0		!COUNTS # OF CONFIRMED TRANSITIONS FOUND
	NT=0		!COUNTS # OF CONSEC POINTS ABOVE TRIG LEVEL
C 'UP' TRUE IF NEXT TRANSITION IS UPWARDS (NUMERICALLY AND SO ON DISPLAY)
C WHETHER IT IS AN OPENING OR A SHUTTING
C IYBAR=MEAN OF POINTS PRECEDING NEXT TRANSITION SET FROM AMPLITUDES
C DETERMINED ABOVE
C
C CALC STEP AMPLITUDES IN IA(J) (NOTE THAT IACAL SET=0 FOR KT=0)
	DO 333 J=1,NTRANS
	   if(j.eq.7) goto 333		!'fit all' option
	   if(kt+j.gt.1024) goto 992	!full!
	   IA(J)=IACAL(KT+J,iampl)-IACAL(KT+J-1,iampl)
	   IF(IA(J).EQ.0) then
	      call bell(3)
	      TYPE 331,j-1,acal*float(iacal(kt+j-1,iampl)),
     &	   j,acal*float(iacal(kt+j,iampl))
331	      FORMAT(' *** TWO ADJACENT EQUAL AMPLITUDES.',/,
     &	'  amp(',i5,')= ',g13.6,/,
     &	'  amp(',i5,')= ',g13.6)
	   endif
333	continue
C
	I=IFIRST			!INDEX IN IBUF
	ICRIT=ISCRIT
	lshort=.false.		!this - last trans=NOT short interval
C NOW LOOP THROUGH ALL DISPLAYED POINTS UNTIL NTRANS TRANSITIONS FOUND
c Return to 240
240	IYBAR=IBASE+IACAL(KT+IT,iampl)	!DEFINE IYBAR
	UP=IA(IT+1).GT.0		!DEFINE UP
C AT THIS POINT IT=0,1,..,NTRANS-1
C
241	IYTEST=IYBAR-IBUF(I+1)
	IF(UP) IYTEST=-IYTEST
	IF(IYTEST.GT.ICRIT) then
C NEXT SECTION DONE WHEN PROVISIONAL TRANSITION FOUND
	   NT=NT+1
	   IF(NT.EQ.1) ISAV=I	!SAVE INDEX OF POINT JUST BEFORE TRIG
	   IF(NT.EQ.NTRIG) GOTO 237	!TRIGGER CONFIRMED
	else
	   NT=0		!SINGLE FAILURE CANCELS PROVISIONAL TRANSITION
	endif
c
	I=I+1
	IF(I.GT.ILAST) GOTO 239		!FINISH
	GOTO 241    !NOT YET CONFIRMED-LOOK AT NEXT PNT WITH SAME YBAR
C
C NEXT SECTION DONE WHEN TRANSITION CONFIRMED
237	IT=IT+1			!UPDATE NO OF TRANS FOUND
C	PREPARE FOR NEXT TRANSITION
C	NEXT MOVE PAST END OF CURRENT TRANSITION TO AVOID TRIGGERING
C	AGAIN ON SAME BIT WHICH MAY ALL BE WELL AWAY FROM NEW YBAR IF
C	NEXT TRANSITION IS CLOSE TO CURRENT ONE
	I=ISAV+NMAXF/3
C
	IF(IAMP.EQ.-9999) GOTO 2613		!IAMPSV NOT DEFINED YET
C	NOW CORRECT TRIGGER POINT ACCORDING TO HOW HIGH IT IS. E.G IF
C	TRANSITION IS OPENING (=UP.AND.OPENUP),HOW NEAR TRIGGER POINT
C	IS TO THE EVENTUAL OPEN LEVEL
	IF(ICOR.LT.0) ICOR=0
	IF(ICOR.GT.NMAXF) ICOR=NMAXF
	ISAV=ISAV-ICOR
c
C	CHECK THAT CORRECTED TRANSITION POINT IS NOT EARLIER THAN
C	PREVIOUS TRANS POINT (UNLESS THIS IS THE FIRST)
2613	ITRANS(IT)=ISAV		!STORE INDEX OF IT'TH TRANSITION POINT
	IF(IT.EQ.1) GOTO 2614
c Look at all points between last transition and current one if they
c are close. If current trans is UP, so last was DOWN look for lowest
c point, if vice versa look for highest. If both were in same direction
c then skip this correction
c NB this will not work for 2nd & 3rd trans of 3 trans in quick
c succession as x is not meaningful- in this case shortest interval
c is set above as amount skipped=nmaxf/3 elements of ibuf - no easy
c way to be sure how long (if at all) this is- may help to reduce
c any such case to duration corresp to x=0.5 say
c Filter function for unfiltered signal has JFILT set to 0- use 10kHz
	if(jfilt.eq.0) goto 20
	wc=1000./(0.5*float(jfilt*3))  !x*wc=duration (mus) for small w
	goto 21
20	wc=1000./(0.5*float(10*3))  !x*wc=duration (mus) for small w
21	if(itrans(it)-itrans(it-1).gt.nmaxf) goto 2614	!not close
	if(.not.lshort) goto 532
c last int was short too so just set this one to that corresp to x=.5
	w=0.5*wc
	lshort=.true.		!this - last trans=short interval
	goto 533
c both same direction?
	if(float(IA(IT))*float(IA(it-1)).GT.0.) goto 2614
	IF(IAMP.EQ.-9999) goto 2614	!IAMPSV NOT DEFINED YET
532	iym=-10000
	if(up) iym=10000
	do 530 j=itrans(it-1),itrans(it)
	if(up.and.ibuf(j).lt.iym) iym=ibuf(j)
	if((.not.up).and.ibuf(j).gt.iym) iym=ibuf(j)
530	continue
c calc fraction of full amp reached by last pair of trans
	x=abs(float(iym-ibase-iacal(KT+IT,iampl))/float(iampsv))
	lshort=.false.		!this - last trans=NOT short interval
	if(x.gt.0.7) goto 2614
	lshort=.true.		!this - last trans=short interval
c calc duration of last transition from x from linear approx
c x=3*Fc*w. JFILT=Fc in kHz on box: 0.5*JFILT=-3dB frequency
	w=x*wc*1.2		!empirically better duration in mus
c move back last trans by an arbitrary amount: it will be a bit late
c if closely followed by the current one- exact correction for this
c would be complex but effect clearly larger if trans are close
c i.e.if w small rel to 1/fc, ie if x is small
	itrans(it-1)=itrans(it-1)-ifix(0.7-x)*wc/finter  !try this!
c define current itrans on basis of this duration (ignore current ISAV!)
533	itrans(it)=itrans(it-1)+ifix(0.5 + w/finter)
c
2614	NT=0		!RESET READY FOR NEXT TRANS
	I=I+1		!LOOK AT NEXT POINT IN DISPLAYED BIT OF IBUF
	IF((I.GT.ILAST).OR.(IT.EQ.NTRANS)) GOTO 239	!FINISH
	GOTO 240    !LOOK FOR NEXT TRANSITION AFTER RESET OF IYBAR AND UP
C END OF SEARCH LOOP THROUGH ALL DISPLAYED POINTS
C
239	CONTINUE

