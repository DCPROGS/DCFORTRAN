	subroutine MAKNUM(z,x,y,size,xaxis,ndy,ijus,cnum,it)
c=================================================================
c To write the number z, starting at position x,y
c x,y=starting pos for writing
c csize=current character size
c ndy=max no of sig figs for numbers on Y axis, used to decide left
c shift needed to leave room for numbers on Y axis (ignored for x axis
c numbering).
c IT=itx or ity, as follows
c  ITX=1 if x tics above axis (normal)
c  ITX=0 if x tics centered on axis
c  ITX=-1 if x tics below axis
c  ITY=1 if y tics to right of axis (normal)
c  ITY=0 if y tics centered on axis
c  ITY=-1 if y tics to left of axis
c CNUM outputs the text string containing the number, so it can be
c  moved etc in FIXTEXT and its postion defined by TEXTBOX.
	logical xaxis,debug,caplock
	character cnum*10

c     X axis: write 'nn' below x,y, and centered horizontally
c     Y axis: write 'nn' to left of x,y, and centered vertically
	left=-1
	center=0
	right=1
	call getsize(size,w,h)
	if(xaxis) then
	   if(it.eq.1.or.it.eq.0) then
		ijus=center
c		y=y-h/2	!down a bit more
		y=y-1.5*h	!down a bit more
		x=x-w/2.	!left a bit more too
		if(it.eq.0)  y= y- h/4	!down further if tics central
	   else if(it.eq.-1) then
		ijus=center
		y=y-2.5*h !up a bit
		x=x-w/2.	!left a bit too
	   endif
	else if(.not.xaxis) then
	   y=y-h/2
	   if(it.eq.1.or.it.eq.0) then
		ijus=right
		x=x-w/2 !left a bit more
		if(it.eq.0) x=x-w	!left further if tics central
	   else if(it.eq.-1) then
		 ijus=right
		 x= x- 2*w	!right a bit more
	   endif
	endif
	nmax=3	!max number of sig figs after decimal point
	call FIXDEC1(z,m0,n1,NMAX)
	call DCFORMAT(z,m0+n1+3,n1,cnum)
c     Get length using nblank, not nblank1, so ASCII zero not added, or included
c     in length (so, for example, if n1=0 then the decimal point should be
c     the last character, cnum(ns:ns)).
	ns=NBLANK(cnum)
	if=2
	if(m0.eq.0.and.z.lt.0.0) if=1
c     next line omits decimal point for 'integers'
	if(n1.eq.0.and.ns.gt.1.and.cnum(ns:ns).eq.'.') ns=ns-1
	cnum=cnum(if:ns)

c	call realtoch(z,cnum,10)
	nl=nblank1(cnum)
	if(ndy.ne.-100) call JUSTIFYSTRING(x,y,cnum(1:nl),0.,size,ijus)
	RETURN
	end
c=================================================================


