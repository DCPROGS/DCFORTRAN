	subroutine FIXAX(xmin,xmax,xmin1,xmax1,xtic,ilog)
c 05/31/89 12:42pm Lahey/Hgraph version- (1)this needs values of
c Xtic for MAJOR (not minor as in PDP) tic length- try doubling them.
c (2) For log scales make tic=1.0 so only decades numbered.
c
c To fix min,max and tick length for arithmetic axis (ilog=0)
c or for log scale (ilog.ne.0). Modif from FIXLIN.
c***Feb 88: if ilog=-1 then uses non-log data for xmin,xmax but
c does not return tic (used in VPLOT when log scale requested
c but want xmin1 etc for non-log numbers)
c	xmin,xmax=input=min,max of data points
c	xmin1,xmax1,xtic=output
c
c calc XTIC so xtic=0.1 if x=.1-1
c	       xtic=1   if x=1-10
c	       xtic=10  if x=10-100 etc
c
c	logical caplock
c
	ntmin=2		!max and min no of major ticks
	ntmax=10
	fac=2.		!reduction fac if too many tics
c NB must have ntmax/fac > ntmin or can get infinite loop
c	ntmax=25
c
	xtic=1.		!nearest decade for log axes
c     for non-log case:
	if(ilog.eq.0) then
	   xm=abs(xmax)
	   if(abs(xmin).gt.xm) xm=abs(xmin)
	   if(abs(xm).ge.1.e-37) xtic=10.**float(ifixr(alog10(xm))-1)
c	   if(abs(xm).ge.1.e-37) xtic=10.**float(ifix(alog10(xm)))
	endif
c
c     make xmin1 the nearest integer mult of xtic below xmin etc
c
c this version leaves no margin if xmax is already an exact multiple
c of xtic; e.g sets xmax1=200 if xmax=200. To get a margin
c (eg xmax1=300. in prev case) then replace next 8 lines
c by the 4 marked c^ below  - NOW REPLACED
c for non-log case reset to min/max to zero if near to zero
	amn=amod(abs(xmin),xtic)
	amx=amod(abs(xmax),xtic)
	xmin1=xmin-amn
	if(xmin.lt.0.) xmin1=xmin-xtic+amn
	xmax1=xmax+xtic-amx
	if(xmax.lt.0.) xmax1=xmax+amx
	if(ilog.eq.0) then
	   if((xmin.gt.0.).and.xmin.le.0.3*xmax) xmin1=0.
	   if((xmax.lt.0.).and.abs(xmax).le.0.3*abs(xmin)) xmax1=0.
	endif
c
c 	now fix ticks: xmin1-xmax1 should be integer
c 	multiple of xtic.

3	n=ifix(1.e-5+(xmax1-xmin1)/xtic)
	if(n.le.ntmax.and.n.ge.ntmin) goto 9
	if(n.gt.ntmax) xtic=fac*xtic
	if(n.lt.ntmin) xtic=xtic/fac
	goto 3
9	continue
	return
	end


