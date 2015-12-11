	subroutine SPLINSUB(Xa,Ya,nfit,yp1,ypn,vstep,yout,
     & v1,v2,nval,itype,y20,y80,x20,x80,nerr,ndim)
c To do interpolation in CJFIT (called from CJVDU)
c (1) If itype=1 used for interpolation of I/V curves
c (2) If itype=2 used for interpolation of 20 and 80% risetimes
c (NB usually will have allocated Xa(nfit) etc, so ndim=nfit in call)
	real*4 xa(ndim),ya(ndim),yout(ndim)
	ALLOCATABLE::y2a
	real*4 y2a(:)
	integer*2 ktype
	logical KBHIT,first		!Utility lib
	character ch,getch
c
	nerr=0
c Note that X values must be strictly INCREASING for the interpolation
c to be done, so if they are decreasing must reverse order first. All arrays
c returned in original order.
	irev=0
	if(nfit.le.0) then
	   call BELL(2)
	   print 701,nfit
701	   format(' NFIT out of range in SPLINSUB = ',i8)
	   RETURN
	endif
c
	ALLOCATE(y2a(ndim))		!local
c
	if(xa(1).gt.xa(nfit)) then
	   irev=1
c	   call VECREV(Xa,Xa,nfit,2048,2048)
	   call VECREV(Xa,Xa,nfit,nfit,ndim)
	   call VECREV(Ya,Ya,nfit,nfit,ndim)
	endif
c
	call SPLINE(Xa,Ya,nfit,yp1,ypn,Y2a)		!calc 2nd derivs in Y2A
c	Now interpolate current points at each V required for I/V plot
c	and put them in yval(ic,j). Voltages taken at V=float(i)*vstep
c	where i= ...,-2,-1,0,1,2,...., and vstep=size of steps in the
c	applied ramp (read from CJUMP.DAT for 02/24/91). Can use ONLY
c	values between xa(1) and xa(nfit) (no extrapolation allowed)
c
	if(itype.eq.2) goto 25
c
	iv1=ifix(xa(1)/vstep)
	v1=float(iv1)*vstep
	if(v1.lt.xa(1)) then
	   v1=v1+vstep
	   iv1=iv1+1
	endif
	iv2=ifix(xa(nfit)/vstep)
	v2=float(iv2)*vstep
	if(v2.gt.xa(nfit)) then
	   v2=v2-vstep
	   iv2=iv2-1
	endif
c Now do interpolation between v=v1 and v2
	nval=iv2-iv1+1
	do 1 i=1,nval
	  vin=v1 + float(i-1)*vstep		!voltages from v1 to v2
	  klo=0		!full bisection for now
	  call SPLINT(Xa,Ya,Y2a,nfit,klo,khi,vin,yout(i))
1	continue
c
c If arrays reversed, then restore them all; NB don't reverse YOUT because
c values in it correspond to the specified Vin regardless of whether Xa,Ya
c have been reversed
	if(irev.eq.1) then
	   call VECREV(Xa,Xa,nfit,ndim,ndim)
	   call VECREV(Ya,Ya,nfit,ndim,ndim)
	endif
	DEALLOCATE(y2a)		!local
	RETURN	!from itype=1
c
c ITYPE=2 Interpolate 20-80 risetime
25	continue
	epsx=0.01		!interpolat to 10 microsec
	call LOCATE(0,0)
	print 121
121	format('&Bisection may be aborted with F2')
	y=y20
	first=.true.
30	continue		!return for y80
	x1=xa(1)          !init guess
	x2=xa(nfit)
	klo=0		!full bisection for now
	call SPLINT(Xa,Ya,Y2a,nfit,klo,khi,x1,fx1)
	klo=0		!full bisection for now
	call SPLINT(Xa,Ya,Y2a,nfit,klo,khi,x2,fx2)
	if((fx1-y)*(fx2-y).gt.0.0) goto 98	!SAME SIGN=ERROR
	if(fx1.ge.y) then
	   p=x1
	   x1=x2
	   x2=p
	endif
	nstep=ifix(alog(abs(x1-x2)/epsx)/alog(2.0)+0.5)
C
	ns=0
22	continue		!return here for next iteration
	if(KBHIT()) then
		ch=GETCH(ktype)
		if(ktype.eq.0.and.ichar(ch).eq.60) goto 97	!abort
	endif
	ns=ns+1
	xout=0.5*(x1+x2)
	klo=0		!full bisection for now
	call SPLINT(Xa,Ya,Y2a,nfit,klo,khi,xout,yout1)
c	erry=yout1-y
	if(yout1-y) 5,6,7
5	x1=xout
	goto 2
6	goto 99
7	x2=xout
2	continue
	if(ns.lt.nstep) goto 22		! otherwise use nstep
	goto 99
c
99	if(first) then
	   x20=xout
	   y=y80		!now do y80
	   first=.false.
	   goto 30
	else
	   x80=xout
	endif
	DEALLOCATE(y2a)		!local
	RETURN
c
97	nerr=2
	call LOCATE(0,0)
	print 23
23	format('&Bisection aborted')
	DEALLOCATE(y2a)		!local
	RETURN
98	nerr=1
	call LOCATE(0,0)
	print 24
24	format('&Guesses do not include result')
	DEALLOCATE(y2a)		!local
	RETURN
c
	END


