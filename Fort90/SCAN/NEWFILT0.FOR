	subroutine NEWFILT(filt1,nfilt1,dtf1,tint,ntrans,ndisp,finter,
     & filt,nfilt,dtf)
	real*4 tint(1024)
	real*4 filt1(1024)	!from INSCAN/NORMFILT
	real*4 filt(128)
	character*1 ans,UC
	logical caplock
c To interpolate step response function at appropriate intervals for the
c number of points on display in SCAN
c Input:
c filt1,nfilt1,dtf1
c tint(i) (in call)=interval following first transition on display
c finter=microsec between data points
c ndisp=number of data points on display
c ntrans=number of transitions on display
c Output:
c filt,nfilt,dtf
c
c   From INSCAN have FILT1() which contains values of f(t) at equal
c time increments =DTF1 (microseconds),with filt1(1)=0 at t=0
c and filt1(nfilt1)=1 at t=(nfilt1-1)*dtf1.
c Number of points for calc curve=<1024 say. Total length of plotted
c channels is tm=ndisp*finter microsec so for 1024 calc points per
c full-length display, so want tmax/1023 musec between filter points
c Prob this is often too many points- try fixing so have about 10 points
c for the shortest interval (if more than 1 trans)
	dtf=8.
	if(ntrans.eq.1) goto 566
	tm=1.e30
	do i=1,ntrans-1
	   if(tint(i).lt.tm) tm=tint(i)
	enddo
	dtf=1.e3*tm/10.			!microsec
c But if the shortest interval is long this will be too few points!
c -so fixed below so that have at least 500 points per full screen
c width (which will be longer than the actual filter function usually).
c -but must not need more than 1024 points for calc function- total
c length of intervals is tm:
	tm=0.
	do i=1,ntrans-1
	  tm=tm + 1.e3*tint(i)	!microsec
	enddo
c   -time occ by calc function may be longer than this by up to
c 128 points roughly =128*dtf mus
	dtf2=tm/(1023.-128.)
	if(dtf2.gt.dtf) dtf=dtf2
C ANOTHER CHECK- unlikely to ever want fewer than say 500 points per
c full screen width=ndisp*finter microsec
	tm=float(ndisp)*finter
	if(ifix(tm/dtf).gt.500) goto 566
	dtf=tm/500.
566	x=1. + float((nfilt1-1))*dtf1/dtf	!new no of points
c if no of points not an integer must adjust interval so as to get
c an exact number- round down no of points (so not >1024) and recalc
c the (slightly) inc dtf accordingly
	nfilt=ifix(x)
	if(nfilt.gt.128) nfilt=128
	if(nfilt.lt.32) nfilt=32
	dtf=dtf1*(nfilt1-1)/(nfilt-1)
	if(nfilt.ge.16.and.(.not.caplock())) goto 563
125	print 11,ndisp,finter,nfilt1,dtf1,nfilt,dtf
11	format(' Points=ndisp; interval (mus) =',i6,g13.6,/,
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

	RETURN
	end
