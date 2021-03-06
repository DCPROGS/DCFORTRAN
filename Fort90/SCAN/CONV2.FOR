	subroutine CONV2(icalc,iy0,ntrans,DT,ks,ke,filt,nfilt,IA,dtf)
	integer icalc(1024)
	integer IA(100)
	real*4 DT(99)
	real*4 filter(128,100),filt(128)
	logical debug,caplock,pon,slock
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
C IY0=initial value (inc baseline).
c
	pon()=slock()
	debug()=caplock()
c
c Multiply filter func (0.-1.) by correct amp and sign (in ADC units)
	do 54 i=1,nfilt
	do 54 j=1,ntrans
54	filter(i,j)=float(ia(j))*filt(i)
c
	itim=1		!time=index counter
	tim=0.
	iflag=0
c
3	continue			!reteurn here for next time value
	yval=float(iy0)		!init level
c

c Now loop through all filters at each time point
c -filter point i corresponds to t=float(i-1)*dtf so at t=x
	DO 541 Jt=1,NTRANS
c
c	ix=itim
c	if(jt.gt.1) ix=itim-idn(jt-1)	!idn() as in SCAN
	x=tim
	if(jt.gt.1) x=tim-dt(jt-1)
c
c Interp in FILTER to find value at t=x (INBES1 returns filter(1) if
c x.lt.x0 (=0.0) with iout=-1, or filter(nfilt) if x.gt.xmax with iout=1
c (if x is within range returns with iout=0)
c	call INBES1(filter,0.,dtf,Nfilt,x,y,iout)
	if(debug()) print 543,x
c      if(discprt) write(8,543)x
543	format(' x= ',g13.6)
c	call INBES2(filter,0.,dtf,Nfilt,Y1,DX1,N1,1,x,y,iout)
c   ( INBES2 used only for single point here so y1,dx1,n1 ignored)
c In version with 2-dimension filter array (for speed, so filter not
c reset for every time point) insert code for interp explicitly rather
c than use subroutine
C Start of interp code:
	iout=0
	xmax=float(nfilt-1)*dtf
	if(x.ge.0..and.x.le.xmax) goto 1	!in range
	if(x.lt.0.) goto 20
	y=filter(nfilt,jt)
	iout=1
	goto 5
20	y=filter(1,jt)
	iout=-1
	goto 5
c
1	i=1+IFIX(x/dtf)
C THIS IS INDEX IN Y OF ELEMENT THAT CORRESPONDS TO NEAREST
C X VALUE BELOW X. THIS VALUE IS
	Xval=FLOAT(i-1)*dtf
C SO REQ X VALUE IS ABOVE THIS BY
	DELX=X-Xval
C I.E. A FRACTION THETA OF TABULATED INTERVAL,DX,WHERE
	THETA=DELX/dtf
C SO DEFINE
	F=filter(i,jt)
	i1=i+1
	IF(i1.gt.nfilt) i1=nfilt
	F1=filter(i1,jt)
	i2=i+2
	IF(i2.gt.Nfilt) i2=Nfilt
	F2=filter(i2,jt)
	i0=i-1
	IF(i0.lt.1) i0=1
	F0=filter(i0,jt)
C NOW INTERPOLATE
	Y=F+THETA*(F1-F)+0.25*THETA*(THETA-1.)*(F2-F1+F0-F)
5	continue
c End of Bessel interp
c
	if(debug() ) print 544,jt,y,iout
c     if(discprt) write(8,544) jt,y,iout
544	format(' jt,y,iout= ',i5,g12.5,i3)
	if(iout.eq.1) iflag=jt
 	yval=yval + y		!add JTth filter
c
541	continue		!end of JT loop
c
c1	continue
	ind=ks+itim-1	!index in icalc
	if(ind.le.1024) goto 10
	ke=ind
	RETURN		!too many points
10	icalc(ind)=ifixr(yval)
	if(debug()) print 5451,ind,tim,yval
c	if(pon()) write(7,5451)ind,tim,yval
c      if(discprt) write(8,5451)ind,tim,yval
5451	format(' t, y(t)= ',i6,2g13.6)
c	if(idig(1).eq.1) write(7,545)ind,tim,yval
c      if(discprt) write(8,545)ind,tim,yval
c545	format(' t, y(t)= ',i6,2g13.6)
	if(iflag.eq.ntrans) goto 2	!finished
	itim=itim+1		!next time step
	tim=float(itim-1)*dtf
	goto 3
c
2	continue
	ke=ind		!index of last ICALC
	iy0=icalc(ke)	!final level
c set rest of icalc to final level
c	do 8 i=ke+1,10240^^^?
c8	icalc(i)=iy0
	return
	end



                                                                                                                                                                                                                                                              








                                                                                                                                                                                                                                                          
