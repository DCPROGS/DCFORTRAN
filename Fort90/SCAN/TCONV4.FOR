	program TCONV4
c To test fitting for SCAN using CONV4 to calculate the curve -all integer
c ====The DCPIX2 option does not work too well -it erases single channel
c record at present, and does not seem to be much faster
c
	integer*2 ichan(0:15),iadc0(0:15),iadc(0:15),nchan
	logical kbhit,high,first
c
	integer icalc(1024)
	real*4 fcalc(1024),fclast(1024),xdisp(1024),xlast(1024)
c	real*4 FILT1(50),filt(128),filtnew(100)
	real*4 FILT(50),filtnew(100)
	integer ifilt(128,100)
	integer IA(100)
	integer idn(99)
c	dimension DT(99)
c ishut(nt1)=baseline for transition #nt1
	integer*2 ishut(1024)
	integer*2 iampl(1024)
	real*4 tint(1024)
	integer iact(0:15)
	character infil*15,ftitle*80
	character*1 ans,UC
	character ch,getch
	integer*2 ktype
	logical debug,pon,slock,caplock,pots
c
	debug()=caplock()
c
c Read a filter
	infil='F584S4.dat'
	OPEN(UNIT=11,file=infil,status='unknown',
     &ACCESS='DIRECT',FORM='UNFORMATTED',RECL=512)
c	read(11,rec=1) FTITLE,NFILT1,DTF1,filtnew
	read(11,rec=1) FTITLE,NFILT,DTF,filtnew
c	read(11,rec=1) FTITLE,NFILT,DTF,FILT
	CLOSE(unit=11)
	do i=1,nfilt
	 filt(i)=filtnew(i)
	enddo
c
	ntrans=3
c
	pots=.false.
	call DCASK('Use potentiometers','y',ans)
	if(ans.eq.'Y') then
	   pots=.true.
c initialise
	  do 11 i=0,15
	  ichan(i)=0
	  iadc(i)=0
11	  continue
	  do 12 i=0,ntrans-1
	  ichan(i)=int2(i)
12	  nchan=int2(ntrans)
	endif
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
	iampl(2)=-200
	tint(3)=4.
	iampl(3)=0
	tint(4)=1.
	iampl(4)=-100
c
c	iy0=6		!baseline
	nt1=2
	acal=1.
c
c CALC STEP AMPLITUDES IN IA(J) (NOTE THAT IACAL SET=0 FOR KT=0)
	kt=nt1-1
	DO 333 J=1,NTRANS
c==	if(kt+j.gt.1024) goto 992	!full!
	IA(J)=IACAL(KT+J,iampl)-IACAL(KT+J-1,iampl)
	IF(debug()) print 355,IA(J),
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
c Next calc IDN() values for transitions after the 1st: index one greater
c than in PDP version, with idn(1)=0
	idn(1)=0
	dt=0.
	do 56 i=2,ntrans
	dt=dt+1.e3*tint(nt1+i-2)
	idn(i)=ifixr(dt/dtf)
56	continue
c Calculate IFILT(i,j)
c Multiply filter func (0.-1.) by correct amp and sign (in ADC units)
c
	do 54 i=1,nfilt
	do 54 j=1,ntrans
54	ifilt(i,j)=ifixr(float(ia(j))*filt(i))
c
c
c Want ICALC(1) to correspond to IBUF(n1)?? NO- start calc curve
c only at the 1st transition?- prob CAN put calc curve through
c WHOLE of plotted data (if < 100 trans anyway) if all trans on display
c are done then this means that must be at initial level at all times
c from ibuf(n1) to start of 1st trans.
c Initial level: Iampl(nt1-1)=amplitude preceding trans #nt1
c ishut(nt1)=baseline for transition #nt1
c	iy0=iacal(nt1-1,iampl) + ishut(nt1)
	iy0=0		!so can be scaled later
c
c	call TIMER(n1)
c	do 50 i=1,100		!for timing
	call CONV4(icalc,iy0,ntrans,idn,ke,ifilt,nfilt)
c50	continue
c	call TIMER(n2)
c	print 51,float(n2-n1)*10.
51	format(' time (ms) = ',f10.1)
c
c Modify DT, recalculate fcalc(), erase old and redraw
c Left/right arrows (or mouse movement) to make trans #i longer/shorter
c +/- key (or left/right mouse button) to increment/decrement transition number
c initially plot starting at x=tim0
	tim0=2.
	idelt=10		!units of elements of filt(i)
	itrans=1
c	dx=9./float(ke)		!for now!!!
	dx=8./float(ke)		!for now!!!
	iy0=iacal(nt1-1,iampl) + ishut(nt1)
	y0=float(iy0)
c Read initial values in iadc0
c	if(pots) call READAMP(ichan,nchan,iadc0)
	if(pots) then
         call READADC(0,iadc0(0))
         call READADC(1,iadc0(1))
         call READADC(2,iadc0(2))
	endif
	fac1=500.
	fac2=20.
	fac3=20.
	high=.true.
	first=.true.
c
33	continue		!redraw
c	call TIMER(n1)
	do 30 i=1,ke
	tim=tim0+float(i)*dx
	fcalc(i)=y0+float(icalc(i))*0.01		!scale factor
	xdisp(i)=tim
30	continue
	call SETACT(iact,-1)
	iact(0)=15		!if pixel not on, then draw white
	call DCPIX(xdisp,fcalc,ke,iact)
c=	if(first) nlast=1		!to avoid error in DCPIX2
c=	call DCPIX2(xdisp,fcalc,ke,xlast,fclast,nlast,first,iact)
c=	first=.false.
	nlast=ke	!NB must define 'last' values AFTER call to DCPIX2
	do 29 i=1,ke
	fclast(i)=fcalc(i)
	xlast(i)=xdisp(i)
29	continue
c	call NEWPEN(15)
c	call PLOT(tim0,fcalc(1),3)
c	t0last=tim0
c	do 30 i=1,ke
c	tim=tim0+float(i)*dx
c	fclast(i)=fcalc(i)
c	xlast(i)=tim
c30	call PLOT(tim,fcalc(i),2)
c
34	continue
c
	if(pots) goto 70
c
	ch=GETCH(ktype)
	ival=ichar(ch)
	if(ktype.eq.8) then
	   if(ival.eq.82) then		!INS=toggle sensitivity
		if(idelt.eq.10) then
		   idelt=1
		else if(idelt.eq.1) then
		   idelt=10
		endif
	      goto 34		!get another character
	   endif
	   if(ival.eq.77) id=idelt		!right
	   if(ival.eq.75) id=-idelt		!left
	   if(itrans.eq.1) then
		tim0=tim0+float(id)*0.1		!0.1=arbitrary scale fac ===fix it!
	   else
		iold=idn(itrans)
		inew=iold+id
		idn(itrans)=inew
cc modify transition #itrans only in icalc()
		call CONV41(icalc,itrans,ntrans,inew,iold,ke,ifilt,nfilt)
c		recalc icalc() completely (call with iy0=0)
c		call CONV4(icalc,0,ntrans,idn,ke,ifilt,nfilt)		!recalc
	   endif
	   goto 31		!redraw
c=	   goto 311		!erase and redraw with DCPIX2
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
31	continue
c Erase last fcalc()
	call SETACT(iact,-1)
	iact(15)=0		!erase white points only
	call DCPIX(xlast,fclast,nlast,iact)
c	call NEWPEN(0)
c	call PLOT(t0last,fclast(1),3)
c	do 32 i=1,ke
c32	call PLOT(xlast(i),fclast(i),2)
311	continue
c	call TIMER(n2)
c	call LOCATE(0,0)
c	print 51,float(n2-n1)*10.
	goto 33		!plot new one
c
c Section for control by pots
70	continue
c Read current values
72	continue
c	call READAMP(ichan,nchan,iadc)
      call READADC(0,iadc(0))
      call READADC(1,iadc(1))
      call READADC(2,iadc(2))
	dt1=float(iadc(0)-iadc0(0))
	dt2=float(iadc(1)-iadc0(1))
	dt3=float(iadc(2)-iadc0(2))
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
	dt1=dt1/fac1
	dt2=dt2/fac2
	dt3=dt3/fac3
	tim0old=tim0
	tim0=tim0+dt1		!0.1=arbitrary scale fac ===fix it!
	if(tim0.lt.0.) tim0=0.
	dt1=tim0-tim0old
	id2=ifix(dt2)
	id3=ifix(dt3)
	id2old=idn(2)
	idn(2)=idn(2)+id2
	id3old=idn(3)
	idn(3)=idn(3)+id3
	if(idn(2).lt.1) then
	   idn(2)=1
	   id2=idn(2)-id2old	!reset change
	endif
	if(idn(3).lt.idn(2)+1) then
	   idn(3)=idn(2)+1
	   id3=idn(3)-id3old	!reset change
	endif
c	call LOCATE(1,0)
c	print 73,dt1,id2,id3
c73	format(f10.2,2i6)
c	Recalc icalc() completely (call with iy0=0)
	if(id2.ne.0) then		!recalc only if idn changed
c modify transition #itrans only in icalc()
	   itrans=2
	   call CONV41(icalc,itrans,ntrans,idn(2),id2old,ke,ifilt,nfilt)
	endif
	if(id3.ne.0) then		!recalc only if idn changed
c modify transition #itrans only in icalc()
	   itrans=3
	   call CONV41(icalc,itrans,ntrans,idn(3),id3old,ke,ifilt,nfilt)
	endif
c	if(id2.ne.0.or.id3.ne.0) then		!recalc only if idn changed
c	   call CONV4(icalc,0,ntrans,idn,ke,ifilt,nfilt)		!recalc
c	endif
	if((dt1.eq.0.).and.id2.eq.0.and.id3.eq.0) goto 72	!read pots
	iadc0(0)=iadc(0)	!new zero values
	iadc0(1)=iadc(1)	!new zero values
	iadc0(2)=iadc(2)	!new zero values
	goto 31			!redraw
c=	goto 311		!erase and redraw with DCPIX2
c	if(.not.kbhit()) goto 31		!redraw
c	print 71, dt1,dt2,dt3		!test version; dt are up to 4096
c71	format(3g13.6)
c	if(.not.kbhit()) goto 72
c End graphics
999	pause
	call PLOT(0.,0.,999)
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


