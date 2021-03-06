	subroutine BISECV(ELFUNC,x1,x2,y,Xout,Yout,epsx,epsy,
     & nerr,ndisp,omit,
     & Xobs,yobs,w,nj,juse,setx,niobs,njset,theta,jfix,ndth)
c	function ELFUNCV(X,Xobs,yobs,w,nj,juse,setx,niobs,njset,
c     & theta,jfix,ndth)
	real THETA(ndth)		!for elfunc
	integer JFIX(ndth)
c Params to be passed to SSDCV and Simplex
	real*4 xobs(niobs,njset),yobs(niobs,njset),w(niobs,njset)
	integer juse(njset),nj(njset)
	real setx(njset)
c Stuff for BISEC
	LOGICAL OMIT
	integer*2 ktype
	logical KBHIT		!Utility lib
	character ch,getch

	external ELFUNC
C
c BISECV is special version of BISEC0 which passes necessary parameters
c for ELFUNCv, in CVFIT (3rd line onwards of param)
c
c Solves func(x)=y by bisection, given Y and guesses x1 and
c x2 that include the solution.
c
c Basic bisection subroutine in which all values that are needed
c by FUNC are in common, and FUNC is a function subroutine
c Modif. so nerr=1 if f(x1) and f(x2) both less than Y
c	    nerr=2 if f(x1) and f(x2) both greater than Y
c and nerr positive if f(x) increases with x
c     nerr negative if f(x) decreases with x
c	ABORT with F2: if aborted returns with nerr=3
c
C*** VERSION OF BISECT WHICH,IF OMIT SET TO TRUE, OMITS THE FIRST
C TWO FUNCTION EVALUATIONS AT X1,X2 SO MUST ENSURE BEFORE CALL
C THAT FUNC(X1) < Y AND THAT X1,X2 REALLY INCLUDE ROOT
c EPSx=accuracy for X (ignored if neg- i.e. Nstep not calc so loops
c	until EPSy is satisfied; max Nstep set to 1000)
c EPSY=accuracy for func(X)-Y (ignored if neg)
c
c NERR=1 IF GUESSES BAD,NERR=0 OTHERWISE
C Prints progress every NDISP steps if ndisp>0
c NDISP=-1  for no print of iterations
c NDISP=-2 omits ALL printing
C
	NDSAV=0
	if(ndisp.ge.-1) print 121
121	format(' Bisection may be aborted with F2')
	IF(NDISP.GE.0) print 12
12	FORMAT( ' Step #     X(out)      Y(out)',/)
	NERR=0
	IF(OMIT) GOTO 1
c=	fx1=func(x1)
	fx1=ELFUNC(x1,Xobs,yobs,w,nj,juse,setx,niobs,njset,
     & theta,jfix,ndth)
	if(ndisp.ge.0) print 25,x1,fx1
c=	fx2=func(x2)
	fx2=ELFUNC(x2,Xobs,yobs,w,nj,juse,setx,niobs,njset,
     & theta,jfix,ndth)
	if(ndisp.ge.0) print 25,x2,fx2
25	format(' f(',g13.6,')= ',g13.6)
	if((fx1-Y)*(fx2-Y).gt.0.0) then
	   if(ndisp.ge.0) then
	      print 25,x1,fx1
	      print 25,x2,fx2
	   endif
	   goto 98			!SAME SIGN=ERROR; nerr=3
	endif
c	IF(FX1.LT.Y) GOTO 1
	if(fx1.ge.Y) then
	   P=X1
	   X1=X2
	   X2=P
	endif
1	CONTINUE
cd	print 23,x1,x2,epsx
cd23	format(' x1,x2,epsx= ',3g13.6)
	Nstep=1000	!max if epsy used to control convergence
	if(epsx.lt.0.) goto 8
	NSTEP=IFIX(ALOG(ABS(X1-X2)/EPSx)/ALOG(2.0)+0.5)
cD	print 3,NSTEP
cD3	FORMAT( ' NSTEP= ',I8)
8	continue
C
	ns=0
c	DO 2 NS=1,NSTEP
22	continue		!return here for next iteration
	if(KBHIT()) then
		ch=GETCH(ktype)
		if(ktype.eq.0.and.ichar(ch).eq.60) goto 999	!abort
	endif
	ns=ns+1
	XOUT=0.5*(X1+X2)
c=	Yout=func(Xout)
	Yout=ELFUNC(Xout,Xobs,yobs,w,nj,juse,setx,niobs,njset,
     & theta,jfix,ndth)
	IF(NDISP.LT.0.OR.((NS-NDSAV).LT.NDISP)) GOTO 10
	print 4,NS,XOUT,YOUT
4	FORMAT(I5,3X,G13.6,2X,G13.6)
	NDSAV=NS
c	IF(LEVENT(0).EQ.1) GOTO 99	!ABORT WITH EVENT 0
10	CONTINUE
	erry=yout-y
	IF(YOUT-Y) 5,6,7
5	X1=XOUT
	GOTO 2
6	GOTO 99
7	X2=XOUT
c2	CONTINUE
2	if(epsy.gt.0.) goto 21		!use epsy to control convergence
	if(ns.lt.nstep) goto 22		! otherwise use nstep
	goto 99
21	if(abs(erry).gt.epsy) goto 22
	goto 99
C
c Set NERR
c Modif. so nerr=1 if f(x1) and f(x2) both less than Y
c	    nerr=2 if f(x1) and f(x2) both greater than Y
c and nerr positive if f(x) increases with x
c     nerr negative if f(x) decreases with x
98	if(fx1.lt.y.and.fx2.lt.y) nerr=1
  	if(fx1.gt.y.and.fx2.gt.y) nerr=2
	if(abs(x2-x1).lt.1.e-30) then
	   if(ndisp.ge.-1) print 26
26	   format(' Initial guesses essentially equal!')
	else
	   if((fx2-fx1)/(x2-x1).lt.0.0) nerr=-nerr
	endif
	if(ndisp.ge.-1) print 24
24	format(' Guesses do not include result')
99	RETURN
c after aborting
999	nerr=3
	RETURN
	END



