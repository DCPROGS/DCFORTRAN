	subroutine MAKNUM(z,x,y,size,xaxis,ndy,ijus,cnum,it)
c=================================================================
c To write the number z, starting at position x,y
c x,y=starting pos for writing
c csize=current character size
c ndy=max no of sig figs for numbers on Y axis, used to decide left
c shift needed to leave room for numbers on Y axis (ignored for x axis
c numbering).
c  IT=itx or ity, as follows
c  ITX=1 if x tics above axis (normal)
c  ITX=0 if x tics centered on axis
c  ITX=-1 if x tics below axis
c  ITY=1 if y tics to right of axis (normal)
c  ITY=0 if y tics centered on axis
c  ITY=-1 if y tics to left of axis
c  CNUM outputs the text string containing the number, so it can be
c  moved etc in FIXTEXT and its postion defined by TEXTBOX.
	logical xaxis,graph
	character cnum*11,string*11
	common/cars/dxsa,dysa,graph
	common/rub/string,ipow
c     X axis: write 'nn' below x,y, and centered horizontally
c     Y axis: write 'nn' to left of x,y, and centered vertically
c	left=-1
c	center=0
c	right=1
	if(xaxis) then
		ijus=0
		Y=Y-8.*dysa
		x=x-2.*dxsa
	else if(.not.xaxis) then
		ijus=1
		x=x-3.*dxsa
		y=y-dysa
	endif
	nmax=3  !max number of sig figs after decimal point
	call FIXDEC1(z,m0,n1,NMAX)
	call DCFORMAT(z,m0+n1+3,n1,cnum)
c     Get length using nblank, not nblank1, so ASCII zero not added, or included
c     in length (so, for example, if n1=0 then the decimal point should be
c     the last character, cnum(ns:ns)).
c	call REALTOCH(z,cnum,11)
	ns=len_trim(cnum)
	if0=2
	if(m0.eq.0.and.z.lt.0.0) if0=1
c     next line omits decimal point for 'integers'
	if(n1.eq.0.and.ns.gt.1.and.cnum(ns:ns).eq.'.') ns=ns-1
	cnum=cnum(if0:ns)
	nl=nblank1(cnum)
	nls=nblank1(string)
	if(ipow.ne.-10000.and.xaxis) then
	    cnum=cnum(1:nl)//string(1:nls)
	    x=x+0.5*dxsa
	endif
	nl=nblank1(cnum)
	if(ndy.ne.-100) then
          call JUSTIFYSTRING(x,y,cnum(1:nl),0.,size,ijus)
	endif
	RETURN
	end
c=================================================================


