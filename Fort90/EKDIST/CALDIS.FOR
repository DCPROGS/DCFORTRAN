	subroutine CALDIS(XAXIS,XMIN,XMAX,FREQ,NBIN,XWBASE,
     & AMEAN,AREA,SD,NCOMP,ANTRUE,AHIST,OBHIST,ISG,
     & GAMDST,logt,ncalc,xcalc,fcalc,obfreq,frqcal,idtype,
     & jset,nset)
C
c This was formerly call SCVD2. Now called from SCVDU. Calcs curves when
c FITTED or PREFIT (AMEAN etc already calc in PTYP1)
c Apr87- modif for log(t) display
c DC version for 11-73. Aug 1986.
c VHIST (modified from VPLOTF) also
c does plotting too so no need for EKPLOT (unless an option is included
c to write to disk as plot queue).
C SUBROUTINE FOR OVERLAY IN EKDIST TO DO ALL DISPLAY (AS SCVDU BUT
C SUBLEV IS PARAM: NEEDED FOR WRITE TO DISK)
C LAST ROW OF PARAM IS ONLY FOR COMMUNICATION TO PLOTING DISC FILE
C	IDEST IS INTEGER VAR TO SPECIFY DESTINATION AFTER DISPLAY
C	ON ENTRY IDEST=31 FOR DISPLAY OF GAUSSIAN-TRANSFORMED TIMES
C
	real XAXIS(510),FREQ(510)
	real xcalc(2048),fcalc(2048)
	real obfreq(510),frqcal(510)
c	real AMEAN(10),AREA(10),SD(10)	!PARAM
c For nset>1
	integer ncomp(10)
	real amean(10,10),area(10,10),sd(10,10)	!for CALDIS
	real*4 antrue(10)
c
	real exp1		!function
	real w(10)		!local
	character*1 ans,UC
	LOGICAL AHIST,OBHIST	!PARAM
	LOGICAL DEBUG,GAMDST
	logical pon,logt,slock,caplock
	logical discprt
	common/dp/discprt
C
C
	pon()=slock()
	debug()=caplock()
c
c#	nterm=iterm()
	call UNDER0('true')	!so underflows set=0 (Lahey manual p12.8)
108	format(/)
c
C NOW SECTION DONE ONLY WHEN RETURNING FROM FITTING,TO CALC
C AND DISPLAY FITTED CURVE
	frqcal(1)=-1.		!if not to be plotted
	NCALC=2048		!for VHIST3
	DXCALC=(XMAX-XMIN)/FLOAT(NCALC-1)	!increment
c NB for dist of log(t) the values of XMIN,XMAX are in log10 units
c from 1st call to VHIST so dxcalc=log10(factor)
	IF(AHIST) GOTO 111
	if(idtype.eq.14.or.idtype.eq.52) goto 111	! for Popen curves
	IF(OBHIST) GOTO 127
C CALCS FOR Thist (only in this case may we have dist of log(t))
c ANTRUE ALREADY CALC IN PTYP1.
C NOW CALC VALUES
	IF(.NOT.GAMDST) GOTO 522

C DISPLAY OF GAMMA (ETC) DISTN (SEE MLFIT1)
c Fixed 06/25/01 03:48pm
	if(ncomp(jset).eq.1)  then
	   n=isg+1				!number of openings
c  Cumulative form of gamma distribution is incomplete gamma function
c which gives Prob(t>T)
	   n=isg+1				!number of openings
	   fact=1.0
	   do i=1,n-1
		fact=fact*float(i)	!factorial(n-1)
	   enddo
	   rate=1.0/amean(1,jset)	!=1/mean length of single opening
	   rn=(rate)**n/fact
	   do i=1,ncalc
		x=xmin+float(i-1)*dxcalc
		xcalc(i)=x
		if(x.ge.0.0) then
		   tn1=x**(n-1)
		   pdf=rn*tn1*exp1(-rate*x)	!divide by fact outside loop
c Note even when call UNDER0 is in force can still get NDP underflow
c with numbers less than 3.4E-4932 (approx 3.4exp(-10115)) so use EXP1
		   fcalc(i)=antrue(jset)*xwbase*pdf
		endif
	   enddo
	else if(ncomp(jset).eq.2)  then
	   print*, ' Not yet done for 2 open time components'
	endif
	goto 525	!end of gamma
c
c	IF(NCOMP(jset).EQ.2) GOTO 526
c	T=1.0
c	DO 524 I=1,ISG
c524	T=T*FLOAT(I)		!T=FACTORIAL(ISG)
c	F=AMEAN(1,jset)
c	T=1.0/(F*T)
c	DO 523 I=1,NCALC
c	X=XMIN+FLOAT(I-1)*DXCALC
c	xcalc(i)=x
c	IF(X.LT.0.0) GOTO 523	!IN CASE XMIN IS NEGATIVE
cc Note even when call UNDER0 is in force can still get NDP underflow
cc with numbers less than 3.4e-4932 (approx 3.4exp(-10115)) so use EXP1
c	FCALC(I)=ANTRUE(jset)*XWBASE*T*((X/F)**ISG)*exp1(-x/f)
c523	CONTINUE
c	GOTO 525
cC NOW GAMDST WITH NCOMP=2
c526	CONTINUE
c	DO 527 I=1,NCALC
c	X=XMIN+FLOAT(I-1)*DXCALC
c	xcalc(i)=x
c	IF(X.LT.0.0) GOTO 527	!IN CASE XMIN IS NEGATIVE
cc Note even when call UNDER0 is in force can still get NDP underflow
cc with numbers less than 3.4e-4932 (approx 3.4exp(-10115)) so check
c	F=((AREA(1,jset)/AMEAN(1,jset))**2)*X*exp1(-X/AMEAN(1,jset))
c	F=F+((AREA(2,jset)/AMEAN(2,jset))**2)*X*exp1(-X/AMEAN(2,jset))
c	C=AMEAN(1,jset)-AMEAN(2,jset)
c	F=F+(2./C)*AREA(1,jset)*AREA(2,jset)*(exp1(-X/AMEAN(1,jset))-
c     &	exp1(-X/AMEAN(2,jset)))
c	FCALC(I)=ANTRUE(jset)*XWBASE*F
c527	CONTINUE
c	GOTO 525
c
C NOW ORDINARY EXPONENTIALS
522	CONTINUE
	if(debug()) print 7771,antrue(jset),xwbase
7771	format(' antrue,xwbase= ',2g13.6,/,' i,t,f(t),g(t)= ')
c define w(j)
	do 113 i=1,ncomp(jset)
113	w(i)=area(i,jset)/amean(i,jset)
c
	DO n=1,NCALC
	   X=XMIN+FLOAT(n-1)*DXCALC	!for log(t) this is log10 value
	   if(logt) x=10.**x		!log10 taken in VHIST
	   xcalc(n)=x
	   IF(X.LT.0.0) GOTO 114	!IN CASE XMIN IS NEGATIVE
	   f=0.0
	   DO i=1,NCOMP(jset)
		x1=x/amean(i,jset)
		ex=exp1(-x1)
		f=f + W(i)*ex
	   enddo
c For log(t) distn xwbase defined in CDIST as bin width in log10 units
	   f1=f		!for debug printout
	   if(logt) f=f*x*2.30259		!f(log10(t))=2.3*t*f(t)
	   FCALC(n)=ANTRUE(jset)*XWBASE*F
	   if(debug())print 777,I,X,f1,F,FCALC(n)
	   if(debug().and.discprt)write(8,777) I,X,f1,F,FCALC(n)
777	   FORMAT(' i,t,f1,f,fcalc=',I8,4(2X,G13.6),/)
114	   CONTINUE
	enddo
c
c Calc curves now defined for exponentials or gamma dist
C NOW PLOT THESE AND ASK IF CALC FREQ DENSITY WANTED (exponentials
c only): must do this before display,while still in alpha mode
525	continue
	frqcal(1)=-1.		!if not to be plotted
	IF(GAMDST) GOTO 122	!no option to calc freq at present
	print 116		!2215 still in ALPHA
116	FORMAT('&Display calc frequency density on graph [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).NE.'Y') GOTO 122
	F=0.
	do i=1,NCOMP(jset)
	   F=F+AREA(i,jset)*(1.0-exp1(-XAXIS(1)/AMEAN(i,jset)))
	enddo
c	FCLO=ANTRUE(jset)*F		!FREQ IN LO BIN
	F=0.
	DO i=1,NCOMP(jset)
	   F=F+AREA(i,jset)*exp1(-XAXIS(NBIN+1)/AMEAN(i,jset))
	enddo
c	FCHI=ANTRUE(jset)*F		!FREQ IN HI BIN
	DO n=1,NBIN
	   F=0.
	   DO i=1,NCOMP(jset)
		F=F+AREA(i,jset)*(exp1(-XAXIS(n)/AMEAN(i,jset))-
     &	   exp1(-XAXIS(n+1)/AMEAN(i,jset)))
	   enddo
	   frqcal(n)=antrue(jset)*F		!only 1 bin width for log(t)
	   if(.not.logt) FRQCAL(n)=FRQCAL(n)*XWBASE/(XAXIS(n+1)-XAXIS(n))
	enddo
122	CONTINUE
	print 121
121	FORMAT(' Type observed and calc freq densities [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).eq.'Y') then
	  print 1061,jset
	  if(pon()) write(7,1061) jset
	  if(discprt) write(8,1061) jset
1061	  format(' OBS AND CALC FREQUENCY DENSITIES FOR SET ',i3,/,
     & ' Bin no.',8x,'X values',15x,'Obs.freq      Calc.freq')
	  DO 123 I=1,NBIN
	   print 124,I,xaxis(i),xaxis(i+1),FREQ(I),frqcal(i)
	   if(pon()) write(7,124) I,xaxis(i),xaxis(i+1),FREQ(I),frqcal(i)
	   if(discprt) write(8,124) I,xaxis(i),xaxis(i+1),FREQ(I),
     &    frqcal(i)
124	   FORMAT(I5,2X,g13.6,' - ',g13.6,2x,G13.6,2x,g13.6)
123	 CONTINUE
c#	 if(nterm.eq.0) pause 'Display next'
	 print 108
	 if(pon()) write(7,108)
	 if(discprt) write(8,108)
	endif
	RETURN
c end of section for calc freq density
c
C NEXT CALC CURVE FOR AHIST-CALC CURVE ONLY
111	CONTINUE
	print 108
	PI=3.1415927
	C=SQRT(2.*PI)
c
	DO n=1,NCALC
	   X=XMIN+FLOAT(n-1)*DXCALC
	   xcalc(n)=x
	   t=0.
	   do i=1,ncomp(jset)
		t1=(x-amean(i,jset))/sd(i,jset)
		t=t + area(i,jset)*exp1(-0.5*t1*t1)/(c*sd(i,jset))
	   enddo
	   FCALC(n)=ANTRUE(jset)*XWBASE*T
C NOTE-AT PRESENT ANTRUE IS SET TO NFIT (IN PTYP1)- NOT CORRECTED
C FOR LOSS BELOW TLOW OR ABOVE THI
	enddo
	RETURN
C
C NOW SECTION TO CALC GEOMETRICS AS FITTED IN MLFIT
127	CONTINUE
	IDX=IFIX(0.5+XWBASE)
	L=0
	ptot=0
	DO n=1,NBIN
c	   OBFREQ(n)=0.0
	   obf=0.0
	   do m=1,idx
		L=L+1			!L=1,2,...,nbin
		T=FLOAT(L-1)
		do i=1,ncomp(jset)
		   rho=1.0 - 1.0/amean(i,jset)
		   w(i)=area(i,jset)*(1.0 - rho)
c===		   obfreq(n)=obfreq(n) + antrue*w(i)*(rho**T)
		   obf=obf + w(i)*(rho**T)
		enddo
c	At this point, obf=P(i) so sum of values should be 1.0
		ptot=ptot+obf
C N.B. ANTRUE SET IN PTYP1
		if(logt) then
		   if(idx.ne.1) then
			print 32
32			format(' Cannot pool bins with log display')
			call BELL(1)
			STOP
		   endif
c		   obf=obf*float(L)*2.30259
		   obf=obf*float(L)
		endif
		obfreq(n)=antrue(jset)*obf
	   enddo
	enddo
c
	print 121	!type freqs for obhist case
	ans='N'
	call INPUTa(ans)
	if(UC(ans).NE.'Y') GOTO 1221
	if(.not.logt) then
	   print 1061,jset	!headings
	   if(pon()) write(7,1061) jset
	   if(discprt) write(8,1061) jset
	   DO I=1,NBIN
		print 124,I,xaxis(i),xaxis(i+1),FREQ(I),obfreq(i)
		if(pon()) write(7,124) I,xaxis(i),xaxis(i+1),
     &	   FREQ(I),obfreq(i)
		if(discprt) write(8,124) I,xaxis(i),xaxis(i+1),
     &	   FREQ(I),obfreq(i)
	   enddo
	else if(logt) then	!special case!
c	  NB obfreq() already multiplied by r in this case, so must remove
c        for printing
	   print 1062,jset
	   if(discprt) write(8,1062) jset
1062	   format(' OBS AND CALC FREQUENCY DENSITIES FOR SET ',i3,/,
     &    ' r value',7x,
     &    'Obs.freq    r*obs freq      calc.freq     r*calc freq')
	   do i=1,nbin
		r=float(i)
		print 125,I,freq(i),r*freq(i),obfreq(i)/r,obfreq(i)
		if(discprt) write(8,125) I,freq(i),r*freq(i),obfreq(i)/r,
     &		obfreq(i)
125		format(i5,8X,2g13.6,4x,2g13.6)
	   enddo
	endif
c
c#	if(nterm.eq.0) pause ' print <RETURN> for display'
	if(pon()) write(7,108)
	if(discprt) write(8,108)
	print 108
1221	CONTINUE
	RETURN
c
	end


