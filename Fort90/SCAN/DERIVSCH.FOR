	subroutine DERIVSCH(data,ix,dx,n1,n2,ibk2,vard,
     & dispderiv,nstat,istat,derivfac)
c To search for transitions using the first derivative of trace
c ybard,vard=mean and variance of first derivative (found in IVAR3
c when rms noise determined): NB mean for deriv=0 for flat bits!
c NB this routine is called in SCAN only when two consecutive
c 'closely-spaced' openings (or shuttings) found, to see whether they
c can be fitted with 'open-shut-open'.  Thus it is known at time of
c call (a) whether to look for max or min first (specify this by
c input value of maxfirst), and (b) which section of data() array to
c search through (data(n1) to data(n2)
c
c Modif 11/29/96 03:01pm to cope with case where 1st dydt is bigger than
c sdcrit, so it starts looking for a maximum, when in fact the first value
c is aberrant (-not starting from a flat level) and in fact next points go
c down fast so a minimum follows as the first stationary point.  When
c mistakenly looking for a maximum, IT does not get defined so istat(1)=it
c gets set to 0!  Dealt with by setting it=-1 initially, and then restarting
c search for 1st staionary point one point on, if attempt made to use IT
c before it is defined
c
c Modif 08/23/94 09:01am by addition of derivfac as parameter; this is
c multiple of SD of deriv used in search for stationary points so smaller
c value (eg 2.0) gives larger chance of finding stationary points and
c so more likely to fit multiple transitions rather than sublevel
c
c OUTPUT:
c nstat=number of stationary points found in first deriv
c istat(20)=index in data() of them (set negative for min, pos for max)
	real*4 data(2048)
	integer iact(0:15)
	integer*2 ix(2048)
	real*4 dydt(2048)
c	real*4 dstat(20)
	integer*2 idydt(2048),istat(20)
	logical dispderiv
c
c	yd1=127.	!lowest point on screen (Y pixels) for display of channels
c	yd2=457.	!highest point on screen (Y pixels) for display of channels
c	iyd1=ifixr(yd1)
c	iyd2=ifixr(yd2)	!make this a param
	dmin=1.e36
	dmax=-1.e36
	n1=n1+1
	n2=n2
	nd=n2-n1  !=n2-n1+1=number of values for derivative
	do i=n1,n2
	   d=(data(i)-data(i-1))/dx		!should centralise?
	   if(d.gt.dmax) dmax=d
	   if(d.lt.dmin) dmin=d
	   dydt(i)=d                  !defined for i=n1,...,n2
	enddo
c scale values for display -say to fit between iy=130 pixels and 230 pixels
c with zero at 180
	fac=100./(dmax-dmin)
	do i=n1,n2
	   d=(dydt(i)-dmin)*fac + 130.
	   idydt(i)=ifixr(d)
	enddo
	izero=ifixr(-dmin*fac + 130.)
c
c Define critical difference in derivative. Var(dydt) was calc with
c dx=1, so correct for this also
	sdcrit=derivfac*sqrt(vard)/dx
c	sdcrit=4*sqrt(vard)/dx
c		!========================DEBUG -see TDERIV
c	sdcrit=0.5
c	do i=n1,n2
c	   dydt(i)=data(i)
c	enddo
c		!========================DEBUG
	isdcrit=ifixr(sdcrit)
c Show first deriv plot if wanted
	if(dispderiv) then
	   call FRAMEV(0,639,128,232,7)	!box for dy/dt
	   call SETACT(iact,-1)		!no draw
	   iact(ibk2)=10			!unless background col, when draw green
c NB idcpix written to show iy(1) to iy(n) so fudge call
	   call IDCPIX(ix(n1),idydt(n1),nd,iact)
	   call HLINE(ix(n1),ix(n2),izero-isdcrit,2)
	   call HLINE(ix(n1),ix(n2),izero+isdcrit,2)
	   call HLINE(ix(n1),ix(n2),izero,10)
	endif
c
c Look for maxima and minima in 1st deriv (largest of each already located
c when scaling!). NB max and mins must alternate, but 1st may be in
c either direction.
c When a point outside +/-scrit boundary is found then look at each subsequent
c point, recording largest yet in dmax(j), until a point is found that is scrit
c below the largest yet (so signalling that max has passed): then record
c dmax(j) and j=itrig(j)
c   When a max is found, it is not regarded as confirmed until points
c have fallen again, by scrit, below the max.
c
	j=0		!counts stationary points located
	i=n1-1
	it=-1		!signal that it not yet defined
12	continue
	do while(i.lt.n2)
	   i=i+1
c Look for 1st stationary point
	   if(abs(dydt(i)).gt.sdcrit) then	!point outside scrit boundaries found
		if(dydt(i).gt.sdcrit) then	!potential max found
		   goto 10		!look for max
		else
		   goto 11		!look for min
		endif
	   endif
	enddo			!***ne position 07/05/94 12:11pm
c
c Look for max
10	   continue
	   dm=0.
1	   i=i+1
	   if(i.gt.n2) goto 9
	   if(dydt(i).gt.dm) then
	      dm=dydt(i)
	      it=i
	   endif
	   if(dydt(i).lt.dm-sdcrit) then	!max now confirmed
		if(it.eq.-1) then
		  i=i-1	!incremented above, and after label 12
		  goto 12	!look again for start if it not defined
		endif
	      j=j+1
	      if(j.gt.20) goto 9
	      istat(j)=it
	      goto 11		!look for min
	   else
	      goto 1		!next i -keep trying to confirm
	   endif
c
c Look for min
11	   continue
	   dm=0.
2	   i=i+1
	   if(i.gt.n2) goto 9
	   if(dydt(i).lt.dm) then
	      dm=dydt(i)
	      it=i
	   endif
	   if(dydt(i).gt.dm+sdcrit) then	!min now confirmed
		if(it.eq.-1) then
		  i=i-1	!incremented above, and after label 12
		  goto 12	!look again for start if it not defined
		endif
	      j=j+1
	      if(j.gt.20) goto 9
c	      dstat(j)=dm
		istat(j)=-it		!set neg to signal minimum
	      goto 10		!look for max
	   else
	      goto 2		!next i -keep trying to confirm
	   endif
c***	enddo
c
c Search finished
9	continue
	nstat=j	!number of stat points found
c If displaying, show the stationary points on display
	if(dispderiv) then
	   do i=1,nstat
	     	j=i2abs(istat(i))
		if(istat(i).gt.0) then
	        call VLINE(ix(j),izero+30,izero,15)
		else if(istat(i).lt.0) then			!exclude j=0!
	        call VLINE(ix(j),izero,izero-30,15)
		endif
	   enddo
	endif
c
	RETURN
	end

