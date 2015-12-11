	program TCONV3
c To test fitting for SCAN using CONV3 to calculate the curve
c===needs checks that ke>0 and ke.le.1024!!!
c
	integer*2 ichan(0:15),iadc(0:15),nchan
	integer*2 iadc0(0:15)
	real*4 fcalc(1024)
	real*4 fclast(1024),xdisp(1024),xlast(1024)
	real*4 FILT1(50),filt(128),filtnew(100)
	integer IA(100)
c	dimension idn(19)
	real*4 DT(99),dtold(99)
c	dimension ksect(1024),timsav(1024)
c ishut(nt1)=baseline for transition #nt1
	integer*2 ishut(1024)
	integer*2 iampl(1024)
	real*4 tint(1024)
	integer iact(0:15)
	character infil*15,ftitle*80
	character*1 ans,UC
	character ch,getch
	integer*2 ktype
	logical debug,pon,slock,caplock
	logical kbhit,high,pots
c
	debug()=caplock()
c
c Read a filter
	infil='F584S4.dat'
	OPEN(UNIT=11,file=infil,status='unknown',
     &ACCESS='DIRECT',FORM='UNFORMATTED',RECL=512)
	read(11,rec=1) FTITLE,NFILT1,DTF1,filtnew
	CLOSE(unit=11)
	do i=1,nfilt1
	 filt1(i)=filtnew(i)
	enddo
c
c First put up single channel data
c Start Lahey
	call PLOTS(0,1,0)		!start Lahey graphics
c
c Draw 'single channel data' in red
	call NEWPEN(12)
	call PLOT(0.,6.,3)
	call PLOT(3.,6.,2)
	call PLOT(3.,4.,2)
	call PLOT(7.,4.,2)
	call PLOT(7.,6.,2)
	call PLOT(11.,6.,2)
c
c ishut(nt1)=baseline for transition #nt1
	do 6 i=1,100
6	ishut(i)=6.
	tint(1)=3.
	iampl(1)=0
	tint(2)=4.
	iampl(2)=-2
	tint(3)=4.
	iampl(3)=0
c
	y0=6.		!baseline
	ntrans=2
	acal=1.0
	nt1=2			!start at tint(1)
	finter=50.		!50 microsec between points
	n1=1
	n2=60
c
c
	pots=.false.
	call DCASK('Use potentiometers','y',ans)
	if(ans.eq.'Y') then
	   pots=.true.
c initialise for ADCs
	  do i=0,15
	    ichan(i)=0
	    iadc(i)=0
	  enddo
	  do 12 i=0,ntrans-1
	  ichan(i)=int2(i)
12	  nchan=int2(ntrans)
	endif
c
c NOW interpolate filter
c   From INSCAM have FILTER FUNCTION-CONTAINS VALUES OF F(T) AT EQUAL
c TIME INCREMENTS =DTF1 (MICROSECONDS),WITH Filt1(1)=0 AT T=0
c AND Filt1(NFILT1)=1 AT T=(NFILT1-1)*DTF1- total length=tmaxf
c Number of points for calc curve=<1024 say. Total length of plotted
c channels is tm=(n2-n1)*finter microsec so for 1024 calc points per
c full-length display, so want tmax/1023 musec between filter points
c Prob this is often too many points- try fixing so have about 10 points
c for the shortest interval (if more than 1 trans)
	dtf=8.
	if(ntrans.eq.1) goto 566
	tm=1.e30
	do 562 i=1,ntrans-1
562	if(tint(nt1+i-1).lt.tm) tm=tint(nt1+i-1)
	dtf=1.e3*tm/10.			!microsec
c	print 567,tm,dtf
c567	format(' tmin,dtf= ',2g13.6)
c But if the shortest interval is long this will be too few points!
c -so fixed below so that have at least 500 points per full screen
c width (which will be longer than the actual filter function usually).
c -but must not need more than 1024 points for calc function- total
c length of intervals is tm:
	tm=0.
	do 50 i=1,ntrans-1
50	tm=tm + 1.e3*tint(nt1+i-1)	!microsec
c   -time occ by calc function may be longer than this by up to
c 128 points roughly =128*dtf mus
	dtf2=tm/(1023.-128.)
	if(dtf2.gt.dtf) dtf=dtf2
C ANOTHER CHECK- unlikely to ever want fewer than say 500 points per
c full screen width=(n2-n1)*finter microsec
c	print 11,n1,n2,finter,nfilt1,dtf1,nfilt,dtf
	tm=float(n2-n1)*finter
	if(ifix(tm/dtf).gt.500) goto 566
	dtf=tm/500.
566	x=1. + float((nfilt1-1))*dtf1/dtf	!new no of points
c if no of points not an integer must adjust interval so as to get
c an exact number- round down no of points (so not >1024) and recalc
c the (slightly) inc dtf accordingly
	nfilt=ifix(x)
c	print 568,tm,dtf2,dtf,x,nfilt,nfilt1
c568	format(' tm,dtf2,dtf,x,nfilt,nfilt1= ',/,4g13.6,2i8)
	if(nfilt.gt.128) nfilt=128
	if(nfilt.lt.4) nfilt=4
	dtf=dtf1*(nfilt1-1)/(nfilt-1)
	if(nfilt.ge.16.and.(.not.debug())) goto 563
125	print 11,n1,n2,finter,nfilt1,dtf1,nfilt,dtf
11	format(' Points n1,n2; interval (mus) =',2i6,g13.6,/,
     & ' orig filter (n,dt)= ',i6,g13.6,/,
     & ' new  filter (n,dt)= ',i6,g13.6,/,
     & /,' O.K.? ')
	read 101,ans
101	format(a1)
	if(UC(ans).eq.'Y') goto 563
	print 564
564	format(' New filter interval (microsec)= ')
	read 565,dtf
565	format(g13.6)
	goto 566
563	continue
C INTERPOLATE FILT1 AT INTERVALS OF NEW DTF
	call INTBES(FILT1,0.,DTF1,NFILT1,FILT,DTF,NFILT,0,0.,0.,iout)
c from INTBES:
C RANGE OF X MUST BE SAME FOR BOTH
C TABLES SO XMAX=X0+(N-1)*DX=X0+(N1-1)*DX1 SO MUST HAVE
C DX/DX1=(N1-1)/(N-1) I.E. N1=1+ (N-1)*DX/DX1.
c or dx1=dx*(n-1)/(n1-1)
c
c CALC STEP AMPLITUDES IN IA(J) (NOTE THAT IACAL SET=0 FOR KT=0)
	kt=nt1-1
	DO 333 J=1,NTRANS
c==	if(kt+j.gt.1024) goto 992	!full!
	IA(J)=IACAL(KT+J,iampl)-IACAL(KT+J-1,iampl)
	IF(DEBUG()) print 355,IA(J),
     & KT+J,IACAL(KT+J,iampl),acal*float(IACAL(KT+J,iampl)),
     &  KT+J-1,IACAL(KT+J-1,iampl),acal*float(IACAL(KT+J-1,iampl))
355	FORMAT(' IA= ',I8,/,1X,2I8,g13.6,/,1X,2I8,g13.6)
	IF(IA(J).ne.0) goto 333
	call bell(3)
	print 331,j-1,acal*float(iacal(kt+j-1,iampl)),
     & j,acal*float(iacal(kt+j,iampl))
331	FORMAT(' *** TWO ADJACENT EQUAL AMPLITUDES.',/,
     & '  amp(',i5,')= ',g13.6,/,
     & '  amp(',i5,')= ',g13.6)
	pause 'Type <RETURN> to continue'
333	continue
C
C The 1st transition on display is at timsav(nt1) microsec from
c start of current section and is at ibuf(nt1). In PLOTSC the X
c value is calc as X=float(i-1)*xinc, i=1,npnt so IBUF(n1) is plotted
c at X=0 and IBUF(nt1) is at X=float(nt1-n1)*xinc.
c Next calc IDN() values for transitions after the 1st
	do 56 i=1,ntrans-1
	if(i.eq.1) dt(i)=1.e3*tint(nt1+i-1)
	if(i.gt.1) dt(i)=dt(i-1) + 1.e3*tint(nt1+i-1)
c	print 561,i,nt1+i-1,tint(nt1+i-1),dt(i)
c561	format(' i,nt1+i-1,tint(nt1+i-1),dt(i)= ',/,2i8,2g13.6)
c	idn(i-1)=ifixr(dt/dtf)
56	continue
c
c Scale for display
c Total length of display = '11 ms' at present (avoids scaling) -but points
c on the filter function must be dtf microsec apart.  Now have 11/11000 scren
c units per microsec
	dx=dtf*11./11000
c
c Want ICALC(1) to correspond to IBUF(n1)?? NO- start calc curve
c only at the 1st transition?- prob CAN put calc curve through
c WHOLE of plotted data (if < 100 trans anyway) if all trans on display
c are done then this means that must be at initial level at all times
c from ibuf(n1) to start of 1st trans.
c Initial level: Iampl(nt1-1)=amplitude preceding trans #nt1
c ishut(nt1)=baseline for transition #nt1
	y0=float(iacal(nt1-1,iampl) + ishut(nt1))		!=iy0
c
c	ntest1=1
c	en1=float(ntest1)
c	call TIMER(n1)
c	do k=1,ntest1		!for timing
	  call CONV3(fcalc,y0,ntrans,DT,ke,filt,nfilt,IA,dtf)
c	enddo
c	call TIMER(n2)
c	call LOCATE(1,0)
c	print 51,float(n2-n1)*10./en1
c51	format(' time (ms) for CONV3= ',f10.1)
c
c Modify DT, recalculate fcalc(), erase old and redraw
c Left/right arrows (or mouse movement) to make trans #i longer/shorter
c +/- key (or left/right mouse button) to increment/decrement transition number
c initially plot starting at x=tim0
	x0=2.		!screen coord
	delt=0.1
	itrans=1
c
c Read initial values in iadc0
	if(pots) then
	   call READAMP(ichan,nchan,iadc0)
	endif
c	if(pots) then
c         call READADC(0,iadc0(0))
c         call READADC(1,iadc0(1))
c         call READADC(2,iadc0(2))
c	endif
	fac1=500.
c	fac2=20.
	fac2=0.5
c	fac3=20.
	high=.true.
c NB do not really need to recalc all xdisp() values each time -spacing stays
c the same; only the point where display starts may be altered -e.g. need
c version of DCPIX with x0 as parameter
	iread=0		!for debug
	ir0	=0		!for debug
	ir1	=0		!for debug
	ir2	=0		!for debug
	ir3	=0		!for debug
33	continue		!redraw with new x0 or dt()
	kelast=ke
	do 30 i=1,ke
	x=x0+float(i)*dx		!screen coord (does not need recalc each time!)
	fclast(i)=fcalc(i)
	xdisp(i)=x
	xlast(i)=x
30	continue
	call SETACT(iact,-1)
	iact(0)=15		!if pixel not on, then draw white
c	call TIMER(n1)
c	ntest=1
c	en=float(ntest)
c	do k=1,ntest		!for timing
	   call DCPIX(xdisp,fcalc,ke,iact)
c	enddo
c	call TIMER(n2)
c	call LOCATE(0,0)
c	print 52,float(n2-n1)*10./en
c52	format(' time (ms) for DCPIX = ',f10.1)
c
	if(pots) goto 70
c
34	ch=GETCH(ktype)
	ival=ichar(ch)
	if(ktype.eq.8) then
	   if(ival.eq.77) d=delt		!right
	   if(ival.eq.75) d=-delt		!left
	   if(itrans.eq.1) then
		x0=x0+d
	   else
		dt(itrans-1)=dt(itrans-1)+d*1000.	!===temp -for microsec
c	      call TIMER(n1)
c	      do k=1,ntest1		!for timing
		 call CONV3(fcalc,y0,ntrans,DT,ke,filt,nfilt,IA,dtf)	!recalc
c	      enddo
c	      call TIMER(n2)
c	      call LOCATE(1,0)
c	      print 51,float(n2-n1)*10./en1
cc	      print 51,float(n1-n0)*10./en1
c		n0=n1
	   endif
	   goto 31		!redraw
	else if(ktype.eq.3) then
	   if(ival.eq.43) itrans=itrans+1		!plus
	   if(ival.eq.45) itrans=itrans-1		!minus
	   if(itrans.gt.ntrans) itrans=1
	   if(itrans.lt.1) itrans=ntrans
	   call GTEXT(0,0,' Transition # '//char(itrans+48))
	   goto 34		!get another character
	else
	   goto 999
	endif
c
31	continue
c Erase last fcalc()
	call SETACT(iact,-1)
	iact(15)=0		!erase white points only
	call DCPIX(xlast,fclast,kelast,iact)
c	call NEWPEN(0)
c	call PLOT(t0last,fclast(1),3)
c	do 32 i=1,ke
c32	call PLOT(xlast(i),fclast(i),2)
	goto 33		!plot new one
c
c
c Section for control by pots
70	continue
c Read current values
72	continue
	iread=iread+1	!for debug
	call READAMP(ichan,nchan,iadc)
c      call READADC(0,iadc(0))
c      call READADC(1,iadc(1))
c      call READADC(2,iadc(2))
	idt1=int4(iadc(0)-iadc0(0))
	idt2=int4(iadc(1)-iadc0(1))
	idt3=int4(iadc(2)-iadc0(2))
	if(iabs(idt1).eq.0) ir0=ir1+1       !for debug
	if(iabs(idt1).eq.1) ir1=ir1+1
	if(iabs(idt1).eq.2) ir2=ir2+1
	if(iabs(idt1).eq.3) ir3=ir3+1
	if(kbhit()) then
	   ch=GETCH(ktype)
	   ival=ichar(ch)
	   if(ktype.eq.8.and.ival.eq.82) then   !INS=toggle sensitivity
		if(high) then
		   fac1=fac1*5.		!low sens
		   fac2=fac2*5.		!low sens
		   fac3=fac3*5.		!low sens
		   high=.false.
		else
		   fac1=fac1/5.		!hi sens
		   fac2=fac2/5.		!hi sens
		   fac3=fac3/5.		!hi sens
		   high=.true.
		endif
	   else
		goto 999		!any other character
	   endif
	endif
	jitter=3
c	jitter=2
c	jitter=1
c	jitter=0
	if(iabs(idt1).le.jitter) then
	   idt1=0	!anti jitter
	   iadc(0)=iadc0(0)
	endif
	if(iabs(idt2).le.jitter) then
	   idt2=0	!anti jitter
	   iadc(1)=iadc0(1)
	endif
	if(debug()) then
	   print 705,idt1,idt2,idt3
705	   format(' idt1,idt2,idt3 = ',3i8)
	endif
	if(idt1.eq.0.and.idt2.eq.0) goto 72	!nothing changed so don't redisplay
c	dt1=float(iadc(0)-iadc0(0))
c	dt2=float(iadc(1)-iadc0(1))
c	dt3=float(iadc(2)-iadc0(2))
	dt1=float(idt1)
	dt2=float(idt2)
	dt3=float(idt3)
	dt1=dt1/fac1
	dt2=dt2/fac2
c===	dt3=dt3/fac3
	x0old=x0
	x0=x0+dt1		!0.1=arbitrary scale fac ===fix it!
	if(x0.lt.0.) x0=0.
c	dt1=tim0-tim0old
c	id2=ifix(dt2)
c	id3=ifix(dt3)
	dtold(1)=dt(1)
	dt(1)=dt(1)+dt2
	dtold(2)=dt(2)
	dt(2)=dt(2)+dt3
c	id2old=idn(2)
c	idn(2)=idn(2)+id2
c	id3old=idn(3)
c	idn(3)=idn(3)+id3
c	if(idn(2).lt.1) then
c	   idn(2)=1
c	   id2=idn(2)-id2old	!reset change
c	endif
c	if(idn(3).lt.idn(2)+1) then
c	   idn(3)=idn(2)+1
c	   id3=idn(3)-id3old	!reset change
c	endif
c	call LOCATE(1,0)
	if(debug()) then
	   print 73,dt1,dt2,dt3
73	   format(' dt1,dt2,dt3 = ',3g13.6)
	endif
cc	Recalc icalc() completely (call with iy0=0)
c	if(abs(dt2).gt.0.001) then		!recalc only if idn changed
	call CONV3(fcalc,y0,ntrans,DT,ke,filt,nfilt,IA,dtf)
c	endif
	if((dt1.eq.0.).and.(dt2.eq.0.).and.(dt3.eq.0.)) goto 72	!read pots
	iadc0(0)=iadc(0)	!new zero values
	iadc0(1)=iadc(1)	!new zero values
	iadc0(2)=iadc(2)	!new zero values
	goto 31			!redraw
c
c End graphics
999	pause
	call PLOT(0.,0.,999)	!end Lahey graphics
c
	if(debug()) then
	  r=float(iread)/100.
	  print 706,iread,float(ir0)/r,float(ir1)/r,
     &   float(ir2)/r,float(ir3)/r
706	   format(' iadc read',i9,' times:',/,
     &   ' idt1 = 0 for ',g13.6, 'percent',/,
     &   ' idt1 = 1 for ',g13.6, 'percent',/,
     &   ' idt1 = 2 for ',g13.6, 'percent',/,
     &   ' idt1 = 3 for ',g13.6, 'percent')
	  pause
	  endif
	end


	INTEGER FUNCTION IACAL(KT,iampl)
C TO GET AMPLITUDE IN ADC UNITS FROM IAMPL(KT). REMOVES 10000 IF
C THIS HAS BEEN ADDED  (FOR POS AMP) OR SUBRACTED (FOR NEG AMP)
C TO INDICATE 'ASSUMED' VALUE .
	integer*2 IAMPL(20480)
	integer*2 ix
C
	IX=0
	IF(KT.EQ.0) GOTO 1
	IX=IAMPL(KT)
	IF(IX.GT.9999) IX=IX-10000
	IF(IX.LT.-9999) IX=IX+10000
1	IACAL=int4(IX)
	RETURN
	END


