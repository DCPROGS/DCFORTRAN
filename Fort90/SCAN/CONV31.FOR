	subroutine CONV3(fcalc,y0,ntrans,DT,ke,filt,nfilt,IA,dtf)
	real*4 fcalc(1024)
	integer IA(100)
	real*4 DT(99)
	real*4 filter(128,100),filt(128)
	logical debug,caplock,pon,slock
c CONV3 has floating point output in fcalc() -calculates fcalc(1) to fcalc(ke)
c NB: REMOVING ALL REFS TO PON() AND DEBUG() SPEED THIS ENORMOUSLY!
c CONV2 is version of CONV1 with time intervals floating rather
c than integer. Values are calc with ICALC(1) being first point
c of the first filter function (t=0), and subsequent values at intervals
c of DTF microsec=time between points of FILT(). DT (microsec) replaces
c integer array IDN() used in EKFIT7.
c To calc filter function for plotting in SCAMP (modif from CONV
c subroutine in TDAT.FOR). Now written as loop so can do up to 100 (now)
c convolutions at once. JT=1,2,..,ntrans=index of current filter.
c Allocates ICALC from element ks (input) to ke (output)
c Sign of filter already incorp in step sizes in IA()
C Y0=initial value (inc baseline).
c
c	pon()=slock()
c	debug()=caplock()
c
c Multiply filter func (0.-1.) by correct amp and sign (in ADC units)
c	ntest=100		!repeat calcs for timimg==
c	ntest=1		!repeat calcs for timimg==

	do 54 i=1,nfilt
	do 54 j=1,ntrans
54	filter(i,j)=float(ia(j))*filt(i)
c
	itim=1		!time=index counter
	tim=0.            !actual time (microsec)
	iflag=0
	tmax=float(nfilt-1)*dtf		!microsec
c
c RETURN TO LABEL 3 FOR NEXT TIME VALUE
3	continue
c	yval=float(iy0)		!init level
	yval=y0		!init level
c
c Now loop through all filters at each time point
c -filter point i corresponds to t=float(i-1)*dtf so at t=x
	do 541 jt=1,NTRANS
c
c	ix=itim
c	if(jt.gt.1) ix=itim-idn(jt-1)	!idn() as in SCAN
	t=tim
	if(jt.gt.1) t=tim-dt(jt-1)
c
c Interp in FILTER to find value at time t (INBES1 returns filter(1) if
c x.lt.x0 (=0.0) with iout=-1, or filter(nfilt) if t.gt.tmax with iout=1
c (if t is within range returns with iout=0)
c	if(debug()) print 543,t
543	format(' t= ',g13.6)
c In version with 2-dimension filter array (for speed, so filter not
c reset for every time point) insert code for interp explicitly rather
c than use subroutine
C Start of interp code:
	iout=0
	if(t.lt.0.) then
	   y=filter(1,jt)
	   iout=-1
	else if(t.gt.tmax) then
	   y=filter(nfilt,jt)
	   iout=1
	else		!t is in range
	   i=1+IFIX(t/dtf)
C THIS IS INDEX IN Y OF ELEMENT THAT CORRESPONDS TO NEAREST
C X VALUE BELOW X. THIS VALUE IS
	   tval=FLOAT(i-1)*dtf
C SO REQ t VALUE IS ABOVE THIS BY
c===
c linear interp
	   i1=i+1
	   if(i1.gt.nfilt) then
		y=filter(nfilt,jt)
	   else
	      b=(filter(i+1,jt)-filter(i,jt))/dtf
		y=filter(i,jt) + b*(t-tval)
	   endif
C I.E. A FRACTION THETA OF TABULATED INTERVAL,DX,WHERE
c===
c*	   DELX=X-Xval
c*	   THETA=DELX/dtf
c*C SO DEFINE
c*	   F=filter(i,jt)
c*	   i1=i+1
c*	   IF(i1.gt.nfilt) i1=nfilt
c*	   F1=filter(i1,jt)
c*	   i2=i+2
c*	   IF(i2.gt.Nfilt) i2=Nfilt
c*	   F2=filter(i2,jt)
c*	   i0=i-1
c*	   IF(i0.lt.1) i0=1
c*	   F0=filter(i0,jt)
c*C NOW INTERPOLATE
c*	   Y=F+THETA*(F1-F)+0.25*THETA*(THETA-1.)*(F2-F1+F0-F)
c*	   if(debug()) print 701,i,f,f1,f2,y
c*701	   format(' i,f,f1,f2,y = ',i5,4g13.6)
c*c End of Bessel interp
	endif
c
c	if(debug() ) print 544,jt,y,iout
c     if(discprt) write(8,544) jt,y,iout
544	format(' jt,y,iout= ',i5,g12.5,i3)
	if(iout.eq.1) iflag=jt
 	yval=yval + y		!add JTth filter
c
541	continue		!end of JT loop
c
c	ind=ks+itim-1	!index in fcalc
	ind=itim		!index in fcalc
	fcalc(ind)=yval
c	if(debug()) print 5451,ind,tim,yval
c	if(pon()) write(7,5451)ind,tim,yval
c      if(discprt) write(8,5451)ind,tim,yval
5451	format(' ind, t, y(t)= ',i6,2g13.6)
c	if(idig(1).eq.1) write(7,545)ind,tim,yval
c      if(discprt) write(8,545)ind,tim,yval
c545	format(' t, y(t)= ',i6,2g13.6)
	if(iflag.eq.ntrans) goto 2	!finished
	if(ind.eq.1024) goto 2		!finished (or,at least, array filled)
	itim=itim+1		!next time step
	tim=float(itim-1)*dtf
	goto 3
c
2	continue
	ke=ind		!index of last FCALC
	y0=fcalc(ke)	!final level
	return
	end



                                                                                                                                                                                                                                                              
























                                                                                                                                                                                                                                                          
