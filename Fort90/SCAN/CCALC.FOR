	subroutine CCALC(n1,n2,iampl,tint,timsav,ishut,ksect,filt1,
     & nfilt1,dtf1,dtf,tmaxf,acal,finter,isect,nsect,icalc,ncalc,
     & nt1,iflag)
c To calc fitted filter function for plotting on data in SCAMP
	dimension icalc(1024)
	dimension FILT1(50),filt(128)
	dimension IA(100)
c	dimension idn(19)
	dimension DT(99)
	dimension ksect(1024),timsav(1024),ishut(1024)
	dimension iampl(1024),tint(1024)
	character*1 ans,UC
	logical debug,pon
C
c
	pon()=slock()
c
c Note that timsav(i) is the time (mus from start of section) of the
c transition that PRECEDES the interval tint(i). The section in
c which this transition occurs is recorded in ksect(i), neg for the
c first transition in a fitting
c
c Calc no of transitions on display, which extends from ibuf(n1)
c to ibuf(n2)
	ntmax=100	!max no of transitions in this version
	i=1		!index for transition #
	k=iabs(ksect(1))
1	if(k.eq.isect) goto 2
	if(k.gt.isect) goto 21
	i=i+1
	k=iabs(ksect(i))
	if(k.le.nsect) goto 1
	print 3
3	format(' No more transitions')
	goto 995
21	print 22
22	format(' No transitions in this section')
	goto 995
2	nt1=i		!no of 1st transition in current section (ibuf)
c Is this transition before 1st plotted point?
c  Time from start of section to 1st plotted point, ibuf(n1), is t1 say
	t1=(n1-1)*finter	!microsec
	t2=(n2-1)*finter	!musec from start to last plotted point
	i=nt1
5	if(timsav(i).gt.t1) goto 4
	i=i+1
	if(iabs(ksect(i)).ne.isect) goto 21	!stay in section!
	goto 5
4	nt1=i		!now nt1=index of 1st trans to be plotted
	i=nt1
6	if(timsav(i).gt.t2) goto 7
c	print 76,i,ksect(i),isect,nsect,timsav(i),n1,n2,t1,t2
c76	format(' i,ksect(i),isect,nsect,timsav(i),n1,n2,t1,t2 ',/,
c    &	4i8,g13.6,/,2i8,2g13.6)
	i=i+1
	if(iabs(ksect(i)).eq.isect) goto 6
7	nt2=i-1		!now index of last trans to be plotted
c Transition numbers nt1 to nt2 are to be plotted
	iflag=0
	if(nt2-nt1+1.le.ntmax) goto 72
	iflag=1		!too many trans on display
	nt2=nt1+ntmax-1
72	ntrans=nt2-nt1+1
	print 8,ntrans,nt1,nt2
	if(pon()) write(7,8)ntrans,nt1,nt2
      if(discprt) write(8,8)ntrans,nt1,nt2 
8	format(
     & ' Number of transitions on display= ',i3,' (',i5,' to ',i5,')',
     & /,'  length (microsec)   amplitude (pA) ',/,)
	do 81 i=nt1,nt2-1
	i1=i
	print 82,i,1.e3*tint(i),acal*float(iacal(i1,iampl))
===
81	if(pon()) print  82,i,1.e3*tint(i),acal*float(iacal(i1,iampl))
82	format(i8,3x,g13.6,6x,g13.6)
	print 81
81	format(' O.K.? ')
	input 57,ans
57	format(a1)
	if(UC(ans).eq.'N') STOP
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
	if(nfilt.ge.16.and.(.not.debug)) goto 563
125	print 11,n1,n2,finter,nfilt1,dtf1,nfilt,dtf
11	format(' Points n1,n2; interval (mus) =',2i6,g13.6,/,
     & ' orig filter (n,dt)= ',i6,g13.6,/,
     & ' new  filter (n,dt)= ',i6,g13.6,/,
     & /,' O.K.? ')
	input 57,ans
	if(UC(ans).eq.'Y') goto 563
	print 564
564	format(' New filter interval (microsec)= ')
	input 565,dtf
565	format(g13.6)
	goto 566
563	continue
C INTERPOLATE FILT1 AT INTERVALS OF NEW DTF
	call INBES2(FILT1,0.,DTF1,NFILT1,FILT,DTF,NFILT,0,0.,0.,iout)
c from INBES:
C RANGE OF X MUST BE SAME FOR BOTH
C TABLES SO XMAX=X0+(N-1)*DX=X0+(N1-1)*DX1 SO MUST HAVE
C DX/DX1=(N1-1)/(N-1) I.E. N1=1+ (N-1)*DX/DX1.
c or dx1=dx*(n-1)/(n1-1)
c
c CALC STEP AMPLITUDES IN IA(J) (NOTE THAT IACAL SET=0 FOR KT=0)
	kt=nt1-1
	DO 333 J=1,NTRANS
	if(kt+j.gt.1024) goto 992	!full!
	IA(J)=IACAL(KT+J,iampl)-IACAL(KT+J-1,iampl)
	IF(DEBUG) print 355,IA(J),
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
c Want ICALC(1) to correspond to IBUF(n1)?? NO- start calc curve
c only at the 1st transition?- prob CAN put calc curve through
c WHOLE of plotted data (if < 100 trans anyway) if all trans on display
c are done then this means that must be at initial level at all times
c from ibuf(n1) to start of 1st trans.
c Initial level: Iampl(nt1-1)=amplitude preceding trans #nt1
c		ishut(nt1)=baseline for transtion #nt1
	init=iacal(nt1-1,iampl) + ishut(nt1)		!=iy0
c	print 121,iacal(nt1-1,iampl),ishut(nt1)
c121	format(' iacal(nt1-1,iampl),ishut(nt1)= ',2i8)
	ks=1
	print 122
122	format(' Wait- calculating function ',/)
	call CONV2(icalc,init,ntrans,DT,ks,k,filt,nfilt,IA,dtf)
	ncalc=k-ks+1
	print 12,ncalc
	if(pon()) write(7,12)ncalc
      if(discprt) write(8,12)ncalc 
12	format(' Number of points for fitted function= ',i8)
	if(debug) goto 123
	if(ncalc.le.1024) RETURN
123	if(ncalc.gt.1024) goto 126
	print 124
124	format(' O.K.? ')
	input 57,ans
	if(UC(ans).eq.'Y') RETURN
	goto 125
126	print 127
127	format(' Too many points')
	goto 125
	RETURN
c
992	call bell(4)
	print 9921
9921	format(' Reached 1024 transitions')
	pause ' Type <RETURN> to continue'
	RETURN
995	ncalc=0
	RETURN
	end


                                              
