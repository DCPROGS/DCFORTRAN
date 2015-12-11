	program TDERIV1
c	subroutine DERIVSCH(data,ix,dx,n1,n2,ibk2,vard,
c     & dispderiv,nstat,istat)
c To search for transitions using the first derivative of trace
c ybard,vard=mean and variance of first derivative (found in IVAR3
c when rms noise determined): NB mean for deriv=0 for flat bits!
c NB this routine is called in SCAN only when two consecutive
c 'closely-spaced' openings (or shuttings) found, to see whether they
c can be fitted with 'open-shut-open'.  Thus it is known at time of
c call (a) whether to look for max or min first (specify this by
c input value of maxfirst), and (b) which section of data() array to
c search through (data(n1) to data(n2)
c OUTPUT:
c nstat=number of stationary points found in first deriv
c istat(20)=index in data() of them (set negative for min, pos for max)
c	real*4 data(2048)
c	integer iact(0:15)
	integer*2 ix(2048)
	real*4 dydt(2048)
c	real*4 dstat(20)
	integer*2 idydt(2048),istat(20)
c	logical dispderiv
c
	dmin=1.e36
	dmax=-1.e36
	n1=95
	n2=107
c	n1=n1+1
c	n2=n2
c	nd=n2-n1  !=n2-n1+1=number of values for derivative
c	do i=n1,n2
c	   d=(data(i)-data(i-1))/dx		!should centralise?
c	   if(d.gt.dmax) dmax=d
c	   if(d.lt.dmin) dmin=d
c	   dydt(i)=d                  !defined for i=n1,...,n2
c	enddo
c	izero=ifixr(-dmin*fac + 130.)
c
	DYDT(95)=23.5368
	DYDT(96)=-66.1033
	DYDT(97)=-86.1346
	DYDT(98)=-41.5649
	DYDT(99)=13.0203
	DYDT(100)=51.5806
	DYDT(101)=85.6338
	DYDT(102)=106.166
	DYDT(103)=94.6479
	DYDT(104)=52.5822
	DYDT(105)=13.0203
	DYDT(106)=0.00000
	DYDT(107)=12.0188

c Define critical difference in derivative. Var(dydt) was calc with
c dx=1, so correct for this also
c	sdcrit=4*sqrt(vard)/dx
	sdcrit=62.5
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
	enddo		!***new position
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
	      j=j+1
	      if(j.gt.20) goto 9
c	      dstat(j)=dm
	      istat(j)=it
c	      maxsearch=.false.	!restart 'do while' to look for min
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
c
	end

