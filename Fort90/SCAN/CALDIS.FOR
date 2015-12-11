	subroutine CALDIS(XAXIS,XMIN,XMAX,FREQ,NBIN,XWBASE,
     & AMEAN,AREA,SD,NCOMP,ANTRUE,AHIST,OBHIST,ISG,
     & GAMDST,logt,ncalc,xcalc,fcalc,obfreq,frqcal,idtype)
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
	real xcalc(512),fcalc(512)
	real obfreq(510),frqcal(510)
	real AMEAN(10),AREA(10),SD(10)	!PARAM
	real exp1		!function
	real w(5)		!local
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
101	format(a1)
c
C NOW SECTION DONE ONLY WHEN RETURNING FROM FITTING,TO CALC
C AND DISPLAY FITTED CURVE
	frqcal(1)=-1.		!if not to be plotted
	NCALC=512   !1024 better but need to alter VHIST, plot queue etc!
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
	IF(NCOMP.EQ.2) GOTO 526
	T=1.0
	DO 524 I=1,ISG
524	T=T*FLOAT(I)		!T=FACTORIAL(ISG)
	F=AMEAN(1)
	T=1.0/(F*T)
	DO 523 I=1,NCALC
	X=XMIN+FLOAT(I-1)*DXCALC
	xcalc(i)=x
	IF(X.LT.0.0) GOTO 523	!IN CASE XMIN IS NEGATIVE
c Note even when call UNDER0 is in force can still get NDP underflow
c with numbers less than 3.4e-4932 (approx 3.4exp(-10115)) so use EXP1
	FCALC(I)=ANTRUE*XWBASE*T*((X/F)**ISG)*exp1(-x/f)
523	CONTINUE
	GOTO 525
C NOW GAMDST WITH NCOMP=2
526	CONTINUE
	DO 527 I=1,NCALC
	X=XMIN+FLOAT(I-1)*DXCALC
	xcalc(i)=x
	IF(X.LT.0.0) GOTO 527	!IN CASE XMIN IS NEGATIVE
c Note even when call UNDER0 is in force can still get NDP underflow
c with numbers less than 3.4e-4932 (approx 3.4exp(-10115)) so check
	F=((AREA(1)/AMEAN(1))**2)*X*exp1(-X/AMEAN(1))
	F=F+((AREA(2)/AMEAN(2))**2)*X*exp1(-X/AMEAN(2))
	C=AMEAN(1)-AMEAN(2)
	F=F+(2./C)*AREA(1)*AREA(2)*(exp1(-X/AMEAN(1))-exp1(-X/AMEAN(2)))
	FCALC(I)=ANTRUE*XWBASE*F
527	CONTINUE
	GOTO 525
c
C NOW ORDINARY EXPONENTIALS
522	CONTINUE
	if(debug()) print 7771,antrue,xwbase
7771	format(' antrue,xwbase= ',2g13.6,/,' i,t,f(t),g(t)= ')
c define w(j)
	do 113 j=1,ncomp
113	w(j)=area(j)/amean(j)
c
	DO 114 I=1,NCALC
	X=XMIN+FLOAT(I-1)*DXCALC	!for log(t) this is log10 value
	if(logt) x=10.**x		!log10 taken in VHIST
	xcalc(i)=x
	IF(X.LT.0.0) GOTO 114	!IN CASE XMIN IS NEGATIVE
	f=0.0
	DO 115 j=1,NCOMP
c	W=AREA(J)/AMEAN(J)
	x1=x/amean(j)
	ex=exp1(-x1)
c	ex=exp1(-x/amean(j))
	f=f + W(j)*ex
c	if(debug()) then
c		print 1151,I,J,AREA(J),AMEAN(J),W(j),
c     &	x1,ex,w(j)*ex,f
c		if(discprt) write(8,1151) I,J,AREA(J),AMEAN(J),W(j),
c     &	x1,ex,w(j)*ex,f
c1151		FORMAT(' i,j,a,mu,w=',i5,i3,2X,3G13.6,/,
c     &	' T=t/tau; e=exp(-T); w*e; f= ',4g13.6)
c		if((x1.gt.80.).and.(ex.gt.0.01)) then
c			call bell(1)
c			do 112 m=80,90
c			x2=-float(m)
c			print 109,x2,exp1(x2)
c112			if(discprt) write(8,109) x2,exp1(x2)
c109			format(' x2, exp1(x2)= ',4g13.6)
c		endif
c	endif
115	continue
c For log(t) distn xwbase defined in CDIST as bin width in log10 units
	f1=f		!for debug printout
	if(logt) f=f*x*2.30259		!f(log10(t))=2.3*t*f(t)
	FCALC(I)=ANTRUE*XWBASE*F
	if(debug())print 777,I,X,f1,F,FCALC(I)
	if(debug().and.discprt)write(8,777) I,X,f1,F,FCALC(I)
777	FORMAT(' i,t,f1,f,fcalc=',I8,4(2X,G13.6),/)
114	CONTINUE
c
c Calc curves now defined for exponentials or gamma dist
C NOW PLOT THESE AND ASK IF CALC FREQ DENSITY WANTED (exponentials
c only): must do this before display,while still in alpha mode
525	continue
	frqcal(1)=-1.		!if not to be plotted
	IF(GAMDST) GOTO 122	!no option to calc freq at present
	print 116		!2215 still in ALPHA
116	FORMAT('&Display or type calc frequency density [N] ? ')
	read 101,ans
	if(UC(ans).NE.'Y') GOTO 122
	F=0.
	DO 117 J=1,NCOMP
117	F=F+AREA(J)*(1.0-exp1(-XAXIS(1)/AMEAN(J)))
c	FCLO=ANTRUE*F		!FREQ IN LO BIN
	F=0.
	DO 118 J=1,NCOMP
118	F=F+AREA(J)*exp1(-XAXIS(NBIN+1)/AMEAN(J))
c	FCHI=ANTRUE*F		!FREQ IN HI BIN
	DO 119 I=1,NBIN
	F=0.
	DO 120 J=1,NCOMP
120	F=F+AREA(J)*(exp1(-XAXIS(I)/AMEAN(J))-exp1(-XAXIS(I+1)/AMEAN(J)))
	frqcal(i)=antrue*F		!only 1 bin width for log(t)
	if(.not.logt) FRQCAL(I)=FRQCAL(I)*XWBASE/(XAXIS(I+1)-XAXIS(I))
119	CONTINUE
	print 121
121	FORMAT('&Type observed and calc freq densities [N] ? ')
	read 101,ans
	if(UC(ans).NE.'Y') GOTO 122
	print 1061
	if(pon()) write(7,1061)
	if(discprt) write(8,1061)
1061	format(
     & ' Bin no.',8x,'X values',15x,'Obs.freq      Calc.freq')
	DO 123 I=1,NBIN
	print 124,I,xaxis(i),xaxis(i+1),FREQ(I),frqcal(i)
	if(pon()) write(7,124) I,xaxis(i),xaxis(i+1),FREQ(I),frqcal(i)
	if(discprt) write(8,124) I,xaxis(i),xaxis(i+1),FREQ(I),
     & frqcal(i)
124	FORMAT(I5,2X,g13.6,' - ',g13.6,2x,G13.6,2x,g13.6)
123	CONTINUE
c#	if(nterm.eq.0) pause 'Display next'
	print 108
	if(pon()) write(7,108)
	if(discprt) write(8,108)
122	CONTINUE
	RETURN
c end of section for calc freq density
c
C NEXT CALC CURVE FOR AHIST-CALC CURVE ONLY
111	CONTINUE
	print 108
	PI=3.1415927
	C=SQRT(2.*PI)
c
	DO 128 I=1,NCALC
	X=XMIN+FLOAT(I-1)*DXCALC
	xcalc(i)=x
	t=0.
	do 20 j=1,ncomp
	t1=(x-amean(j))/sd(j)
	t=t + area(j)*exp1(-0.5*t1*t1)/(c*sd(j))
20	continue
129	FCALC(I)=ANTRUE*XWBASE*T
C NOTE-AT PRESENT ANTRUE IS SET TO NFIT (IN PTYP1)- NOT CORRECTED
C FOR LOSS BELOW TLOW OR ABOVE THI
128	CONTINUE
	RETURN
C
C NOW SECTION TO CALC GEOMETRICS AS FITTED IN MLFIT
127	CONTINUE
	IDX=IFIX(0.5+XWBASE)
	L=0
	DO 4121 I=1,NBIN
	OBFREQ(I)=0.0
	DO 4122 m=1,IDX
	L=L+1
	T=FLOAT(L-1)
	do 33 j=1,ncomp
	rho=1.0 - 1.0/amean(j)
	w(j)=area(j)*(1.0 - rho)
	obfreq(i)=obfreq(i) + antrue*w(j)*(rho**T)
33	continue
C N.B. ANTRUE SET IN PTYP1
4122	CONTINUE
4121	CONTINUE
c
	print 121	!type freqs?
	read 101,ans
	if(UC(ans).NE.'Y') GOTO 1221
	print 1061	!headings
	if(pon()) write(7,1061)
	if(discprt) write(8,1061)
	DO 1231 I=1,NBIN
	print 124,I,xaxis(i),xaxis(i+1),FREQ(I),obfreq(i)
	if(pon()) write(7,124) I,xaxis(i),xaxis(i+1),FREQ(I),obfreq(i)
	if(discprt) write(8,124) I,xaxis(i),xaxis(i+1),FREQ(I),obfreq(i)
1231	CONTINUE
c#	if(nterm.eq.0) pause ' print <RETURN> for display'
	if(pon()) write(7,108)
	if(discprt) write(8,108)
	print 108
1221	CONTINUE
	RETURN
c
	end


