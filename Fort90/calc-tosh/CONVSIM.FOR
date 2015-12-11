	subroutine CONVSIM(icalc,nmax,y0,ntrans,DT,ke,
     & filt,nfilt,stepamp,dtf)
c Subroutine (based on conv1.for in scan) to convolve the results in SCSIM
c in order to generate a simulated consam file to output from scsim.
c
c JT=1,2,..,ntrans=index of current filter.
c Sign of filter already incorp in step sizes in stepamp()
C Y0=initial value (inc baseline).
c
c	real*4 fcalc(nmax)
	integer*2 icalc(nmax)
	real*4 stepamp(ntrans+1),dt(ntrans-1)
	real*4 filt(nfilt)
	allocatable filter
	real*4 filter(:,:)
	logical debug,caplock,pon,slock,deb
	pon()=slock()
	debug()=caplock()
c

	ALLOCATE(filter(nfilt,ntrans))
c
	do i=1,nfilt
	   do j=1,ntrans
		filter(i,j)=stepamp(j)*filt(i)
	   enddo
	enddo
c
	itim=1		!time=index counter
	tim=0.
	iflag=0

c RETURN HERE FOR NEXT TIME VALUE
	deb=debug()
3	continue			!return here for next time value

	yval=y0		!init level
c
c Now loop through all filters at each time point
c -filter point i corresponds to t=float(i-1)*dtf so at t=x
	do 541 jt=1,ntrans
c
	x=tim
	if(jt.gt.1) x=tim-dt(jt-1)
c
c Interp in FILTER to find value at t=x
C Start of interp code:
	iout=0
	xmax=float(nfilt-1)*dtf
	if(x.lt.0.) then
	   y=filter(1,jt)
	   iout=-1
	else if(x.gt.xmax) then
	   y=filter(nfilt,jt)
	   iout=1
	else
	   i=1+IFIX(x/dtf)
C THIS IS INDEX IN Y OF ELEMENT THAT CORRESPONDS TO NEAREST
C X VALUE BELOW X. THIS VALUE IS
	   Xval=FLOAT(i-1)*dtf
C SO REQ X VALUE IS ABOVE THIS BY
	   delx=X-Xval
C I.E. A FRACTION THETA OF TABULATED INTERVAL,DX,WHERE
	   theta=delx/dtf
C SO DEFINE
	   f=filter(i,jt)
	   i1=i+1
	   if(i1.gt.nfilt) i1=nfilt
	   f1=filter(i1,jt)
	   i2=i+2
	   if(i2.gt.Nfilt) i2=Nfilt
	   f2=filter(i2,jt)
	   i0=i-1
	   if(i0.lt.1) i0=1
	   f0=filter(i0,jt)
C NOW INTERPOLATE
	   Y=f+theta*(f1-f)+0.25*theta*(theta-1.)*(f2-f1+f0-f)
	endif
c End of Bessel interp
c
	if(iout.eq.1) iflag=jt
 	yval=yval + y		!add JTth filter
541	continue		!end of JT loop
c
	ind=itim		!index in fcalc
	if(ind.le.nmax) then
	   if(yval.gt.32767.or.yval.lt.-32767) then
		call BELL(4)
		print 71
71		format(' OFFSET WRONG IN CONVSIM')
	   endif
	   icalc(ind)=int2(yval)
	   if(deb) then
		print 20,ind,yval
20		format(' ind, yval = ',i8,2x,g13.6)
	   else if(mod(ind,10000).eq.1) then
		print 21,ind
21		format(' ',i8,' points convolved . . .')
	   endif
	   if(iflag.eq.ntrans) goto 2	!finished
	   itim=itim+1		!next time step
	   tim=float(itim-1)*dtf
	   goto 3	!DO NEXT TIME POINT
	else
	   ke=ind
	   DEALLOCATE(filter)
	   RETURN		!too many points
	endif
c
2	continue
	ke=ind		!index of last FCALC
c	y0=fcalc(ke)	!final level
	DEALLOCATE(filter)
	RETURN
	end

