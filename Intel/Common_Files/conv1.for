	subroutine CONV1(fcalc,y0,ntrans,DT,ke,filt,nfilt,stepamp,dtf)
	real*4 fcalc(5120)
	real*4 stepamp(50),dt(49)
c	real*4 filter(256,100),filt(nfilt)
	real*4 filt(nfilt)
	allocatable filter
	real*4 filter(:,:)
	
	ALLOCATE(filter(nfilt,ntrans))
c
	do i=1,nfilt
	   do j=1,ntrans
		filter(i,j)=stepamp(j)*filt(i)
	   enddo
	enddo


	itim=1		!time=index counter
	tim=0.
	iflag=0


3	continue			!return here for next time value


	yval=y0		!init level
c
c Now loop through all filters at each time point
c -filter point i corresponds to t=float(i-1)*dtf so at t=x
	do 541 jt=1,ntrans
c
	x=tim
	if(jt.gt.1) x=tim-dt(jt-1)

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

544	format(' jt,y,iout= ',i5,g12.5,i3)
	if(iout.eq.1) iflag=jt
 	yval=yval + y		!add JTth filter
c
541	continue		!end of JT loop
	ind=itim		!index in fcalc
	if(ind.le.5120) goto 10
	ke=ind
	DEALLOCATE(filter)
	RETURN		!too many points
10	fcalc(ind)=yval
	if(iflag.eq.ntrans) goto 2	!finished
	itim=itim+1		!next time step
	tim=float(itim-1)*dtf
	goto 3
c
2	continue

	ke=ind		!index of last FCALC
	DEALLOCATE(filter)
	RETURN
	end
