	subroutine MAKEXP(z,x,y,size,xaxis,ndy,ijus,cnum,cexp,it,nd10)
c========================================================================
c To write 10**z in decent exponential format, starting at x,y
c x,y=starting pos for writing
c csize=current character size
c CNUM outputs the text string containing the number, so it can be
c  moved etc in FIXTEXT and its postion defined by TEXTBOX. In MAKEXP
c  CNUM contains only the '10'; keep the exponent separately in CEXP
c (would be better to have whole thing in one string, but attempt
c to do so below not satisfactory)
c ndy=max no of sig figs for exponent on Y axis, used to decide left
c shift needed to leave room for exponent
c of numbers on Y axis (ignored for x axis numbering). Supplied as neg value
c if ymin (log value) is <0, so need to leave more room for minus sign in
c exponent in this case.
	logical xaxis
	integer charheight,charwidth
	character cexp*10,cnum*10,str*20
	left=-1
	center=0
	right=1
	if(ndy.lt.0) ndy=iabs(ndy)+1	!leave more room
	call getSIZE(size,w,h)	!this defines ISIZE
c 	X axis: write '10' below x,y, and centered horizontally
c 	Y axis: write '10' to left of x,y, and centered vertically
	if(xaxis) then
	   if(it.eq.1.or.it.eq.0) then
		ijus=center
c		y=y- h/2	!down to leave room for superscript
		y=y-1.5*h !down to leave room for superscript
		if(it.eq.0)  y= y- h/3	!down further if tics central
	   else if(it.eq.-1) then
		ijus=center
		y=y-2.5*h
	   endif
	else if(.not.xaxis) then
	   y=y-h/2
	   if(it.eq.1.or.it.eq.0) then
		ijus=right
		x= x-ifix(0.4*ndy* w)	!left to leave room for superscript
		if(it.eq.0)  x= x- w/3	!left further if tics central
	   else if(it.eq.-1) then
		ijus=right
		x= x- 2*w	!left further if tics central
	   endif
	endif
c
	cnum='10'
	call SETSIZE(size)	!this defines ISIZE
c     The exponent to be written is converted to a character string (in CEXP)
c     by HFORMAT, in a fixed format defined by FIXDEC1 (see TFORM.FOR for tests)
	nmax=0	!max number of sig figs after decimal point
	call CHAEXI(0.8,0.8,0.6,0.3)
	call FIXDEC1(z,m0,n1,NMAX)
	call DCFORMAT(z,m0+n1+3,0,cexp)	!0 figs after dec point
	ns=NBLANK1(cexp)
	if(cexp(ns:ns).eq.'.') ns=ns-1	!remove decimal point
c 	Note that HFORMAT returns the first character as blank (see TFORM) so
c 	for z=1.2 c(1:1)=c(2:2)=blank,c(3:3)=1,c(4:4)=.,c(5:5)=2
c 	for z=-1.2 c(1:1)=blank,c(2:2)=-,c(3:3)=1,c(4:4)=.,c(5:5)=2
	if=2
	if(z.gt.0) if=3		!omit first char if pos
	cexp=cexp(if:ns)
	nl=nblank1(cexp)
	str=cnum(1:2)//char(42)//char(69)//cexp(1:nl)
	nl=nblank1(str)
	if(nd10.ne.-100) call JUSTIFYSTRING(x,y,str(1:nl),0.,size,ijus)
	RETURN
	end
c========================================================================

