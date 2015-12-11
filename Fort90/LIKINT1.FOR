	subroutine LIKINT1(ELFUNC,noerr,sdev,nmod,constr,
     & theta,jfix,titlep,ndth,Xobs,yobs,w,nj,juse,setx,niobs,njset)
c LIKINT1 (06/25/92 02:44pm) is version of LIKINT0 (for CVFIT at present)
c that uses SD from HESMAT (if they have been calc) to improve automatic
c initial guesses for likelihood intervals
c 01/12/96 03:13pm :theta() out of common and fixed for dimension=ndth
c (=100 at present) (also for jfix,sdev,titlep)
c 12/28/89 04:23pm Lahey version of general purpose subroutine
c for calc of likelihood intervals. The subroutine name for calc
c of the likelihood for specified parameters is given in call.
c Version for 20 parameters.
c
	dimension XLL(2,20),XUL(2,20),NPFIL(20)
	real THETA(ndth)
	real*4 sdev(ndth),se(20)
	integer JFIX(ndth)
	character*10 TITLEP(ndth)		!names of params for fit
c	character*1 UC,ans
	LOGICAL DEBUG,LOWER,UPPER,omit,noerr,constr
	logical discprt,slock,caplock,pon,prt,deb
	common/dp/discprt
	COMMON/LIKBLK/kmax,kfit,elmax,em,delmin,ndisp,npar,iw,sres
c Params to be passed to SSDCV and Simplex
	real*4 xobs(niobs,njset),yobs(niobs,njset),w(niobs,njset)
	integer juse(njset),nj(njset)
	real setx(njset)
	EXTERNAL ELFUNC
C
C Note that if KFIT=1, i.e. only one param to be estimated then
c when this is fixed at a trial value there are none to be
c estimated so SIMPLEX not needed- just call ELFUNC
c
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
	prt=pon()
	deb=debug()
	do 40 i=1,kmax
	it=i
	if(nmod.eq.7.and.i.gt.3) it=3	!repeat BMAX title for i=3,4,...
	if(jfix(i).eq.0.and.(.not.noerr)) then    !errors calc, not fixed
	   ii=ii+1
	   se(i)=Sdev(i)	!NB Sdev(i) set to -1 for fixed param
	   if(iw.eq.1.or.iw.eq.5) se(i)=sres*se(i)
	   print 41,I,TITLEP(IT),THETA(I),se(i)
c	   if(prt) write(7,41)I,TITLEP(it),THETA(I),se(i)
c	   if(discprt) write(8,41)I,TITLEP(it),THETA(I),se(i)
41	   FORMAT(' Parameter ',I2,': ',a10,'= ',G13.6,' Approx SD= ',
     &	g13.6)
	else if(jfix(i).eq.1) then	!fixed (whether errors calc or not)
411	   print 412,I,TITLEP(it),THETA(I)
c	   if(prt) write(7,412)I,TITLEP(it),THETA(I)
c         if(discprt) write(8,412)I,TITLEP(it),THETA(I)
412	   FORMAT(' Parameter ',I2,': ',a10,'= ',G13.6,' (fixed)')
	else if(noerr.and.jfix(i).eq.0) then		!no errors,not fixed
413	   print 414,I,TITLEP(it),THETA(I)
c	   if(prt) write(7,414)I,TITLEP(it),THETA(I)
c         if(discprt) write(8,414)I,TITLEP(it),THETA(I)
414	   FORMAT(' Parameter ',I2,': ',a10,'= ',G13.6)
	endif
40	continue
c
	print 17
17	FORMAT(' Compute m-unit intervals: m= ')
	call INPUTr(em)
	clim=sqrt(2.*em)		!equiv SD for Gaussian
	ELCRIT=ELMAX-EM
C INITIALISE
	do 49 i=1,2
	do 49 j=1,20
	XLL(I,J)=-2.E30
49	XUL(I,J)=-2.E30
C
32	print 42
42	FORMAT(
     & '&If calculation does not converge, hit F2 to stop',/,/,
     & ' No of parameters for which limits to be calc [0 to skip]= ')
	read 14,NP
	IF(NP.LE.0) GOTO 99
	print 54,em,clim
	if(prt) write(7,54) em,clim
	if(discprt) write(8,54) em,clim
54	format(/,' ',f8.2,'-unit Likelihood Intervals',/,
     & ' (like ',f8.2,'-SD intervals if Gaussian)',/)
c
	DO 43 J=1,NP
	print 13,j
13	FORMAT('& ',i3,': parameter number= ')
	read 14,i
	NPFIL(J)=i
14	FORMAT(I8)
c Defaults may well not be good for parameters that are neg, or for those
c where limits may include zero
	xhigh=theta(i)
	if(noerr.or.se(i).lt.0.01*theta(i)) then
	   xlow=0.1*theta(i)
	else
	   xlow=theta(i) - 2.0*clim*se(i)		!leave 2-fold margin
	endif
	if(constr.and.xlow.lt.1.e-20) xlow=1.e-20		!when constrained pos
c	print 141,xlow,xhigh
c141	format(
c     & '&Guesses for lower limit = ',g13.6,' to ',g13.6,'  O.K. [Y] ')
c	read 101,ans
c	if(UC(ans).eq.'N') then
c	   print 44
c44	   FORMAT('& Calculate lower limit [Y] ? ')
c	   read 101,ans
c	   if(UC(ans).ne.'N') then
c	      print 15
c15	      FORMAT('&  Low and high guesses for lower limit, x1,x2 = ')
c	      call INPUT2r(xlow,xhigh)
c	   endif
c	endif
	if(xlow.gt.xhigh) then
	   p=xlow
	   xlow=xhigh
	   xhigh=p
	endif
	XLL(1,J)=XLOW
	XLL(2,J)=XHIGH
47	continue
	xlow=theta(i)
	if(noerr.or.se(i).lt.0.01*theta(i)) then
	   xhigh=15.0*theta(i)
	else
	   xhigh=theta(i) + 5.0*clim*se(i)		!leave 5-fold margin
	endif
c	print 142,xlow,xhigh
c142	format(
c     & '&Guesses for upper limit = ',g13.6,' to ',g13.6,'  O.K. [Y] ')
c	read 101,ans
c	if(UC(ans).eq.'N') then
c	   print 45
c45	   FORMAT('&  Calculate upper limit [Y] ? ')
c	   read 101,ans
c	   if(UC(ans).ne.'N') then
c	      print 33
c33	      FORMAT('&  Low and high guesses for upper limit, x1,x2 = ')
c	      call INPUT2r(xlow,xhigh)
c	   endif
c	endif
	if(xlow.gt.xhigh) then
	   p=xlow
	   xlow=xhigh
	   xhigh=p
	endif
	XUL(1,J)=XLOW
	XUL(2,J)=XHIGH
43	continue          !end of param loop
C
C
C PRECISION IN simplex CONTROLLED BY DELMIN=(CHANGE IN MIN)
C SAY ABSOLUTE ERROR OF 0.002 IN L(THETA) IS CLOSE ENOUGH THEN
	omit=.false.	!so BISEC tests and prints init guesses
	DELMIN=0.005	!defaults used if debug()=false
	NEPSX=1000
c	EPSY=-1.
	epsy=0.005
	ndisp=-1000		!no print out at all in Simplex
	IF(.NOT.deb) GOTO 2
	if(kfit.eq.1) goto 21
	print 5
5	FORMAT( ' Abs accuracy for L(theta) in SIMPLEX: delmin= ')
	read 6,DEL
6	FORMAT(G13.6)
	DELMIN=DEL
	print 3
3	FORMAT( ' print every Nth iteration in SIMPLEX. N= ')
	read 4,NDISP
4	FORMAT(I8)
21	print 28
28	FORMAT(
     & ' Accuracy for param (in Bisection) set to 1 in N. N= ')
	read 14,NEPSX
	print 56
56	FORMAT( ' Abs accuracy L-Lmax in bisection (ignore if neg)= ')
	read 6,EPSY	!IGNORED IF NEG
2	CONTINUE
C
C NOW START LOOP FOR THE PARAMETERS TO BE DONE
	DO 48 IP=1,NP
	NPAR=NPFIL(IP)	!CURRENT PARAMETER NUMBER
      print 481,npar,theta(npar)
      if(pon()) write(7,481) npar,theta(npar)
      if(discprt) write(8,481) npar,theta(npar)
481	format(' Parameter',i3,' = ',g13.6)
	JFIX(NPAR)=1	!FIX THIS PARAMETER
	EPSX=THETA(NPAR)/FLOAT(NEPSX)	!ABS ACCURACY FOR BISECT
	LOWER=.FALSE.
	UPPER=.FALSE.
	IF(XLL(1,IP).GT.-1.E30) LOWER=.TRUE.	!CALC LOWER LIMIT
	IF(XUL(1,IP).GT.-1.E30) UPPER=.TRUE.	!CALC UPPER LIMIT
C NOW CALC LOWER LIMIT FIRST IF WANTED
553	continue
	IF(.NOT.LOWER) GOTO 50
	XLOW=XLL(1,IP)
	XHIGH=XLL(2,IP)
	ND1=1	!PRINT EVERY CYCLE IN BISECT
	IF(deb) print 55,XLOW,XHIGH
55	FORMAT( ' Initial guess= ',G13.6,2X,G13.6,/)
	call CLRKB()
	CALL BISECv(ELFUNC,XLOW,XHIGH,0.0,XLIM,YOUT,EPSX,EPSY,
     & NERR,ND1,omit,
     & Xobs,yobs,w,nj,juse,setx,niobs,njset,theta,jfix,ndth)
	if(iabs(nerr).ge.1.and.iabs(nerr).le.3) then
	   call BELL(2)
	   if(iabs(nerr).lt.3) then
	      print 551
551	      format(' Guesses for lower limit do not include result:')
	   endif
	   print 552
552	   format(
     & ' (1) give new guesses for lower limit and try again',/,
     & ' (2) go on to next limit',/,
     & ' (3) abandon likelihood interval calculations',/,
     & ' Option number [1] = ')
	   read 4,i
	   iopt=1
	   if(i.ge.1.and.i.le.3) iopt=i
	   if(iopt.eq.1) then
	      print 15,xll(1,ip),xll(2,ip)
15	      FORMAT('&  Present guesses = ',2g12.4,
     &	': new guesses, x1,x2 = ')
	      call INPUT2r(xlow,xhigh)
		if(xlow.gt.xhigh) then
		   p=xlow
		   xlow=xhigh
		   xhigh=p
		endif
		XLL(1,J)=xlow
		XLL(2,J)=xhigh
		goto 553
	   endif
	   if(iopt.eq.2) goto 50
	   if(iabs(nerr).eq.3) goto 992	!iopt=3
	   goto 991					!iopt=3
	endif
	print 29,EM,XLIM,xlim-theta(npar)
	if(prt) write(7,29) EM,XLIM,xlim-theta(npar)
      if(discprt) write(8,29) EM,XLIM,xlim-theta(npar)
29	FORMAT(
     & '   LOWER ',F6.2,'-unit interval= ',G13.6,' ( dev= ',g13.6,')')
	print 361,ELMAX,ELCRIT,elcrit+yout,YOUT
	if(prt) write(7,361) ELMAX,elcrit+yout,ELCRIT,YOUT
      if(discprt) write(8,361) ELMAX,elcrit+yout,ELCRIT,YOUT
361	FORMAT(
     & '           [L(max)= ',G13.6,'  L(crit)= ',G13.6,/,
     & '            L(limit)= ',g13.6,' Dev from L(crit)= ',F10.5,']',/)
	if(prt) call flush(7)
c	if(upper) then
c	   print 362
c362	   format(' Hit any key to calculate upper limit')
c	   call ANYKEY
c	endif
C
C LOWER LIMIT DONE (IF REQ). NEXT DO UPPER
50	continue			!jump here to do upper limit
	IF(.NOT.UPPER) GOTO 51		!NEXT PARAMETER
555	continue
	XLOW=XUL(2,IP)
	XHIGH=XUL(1,IP)
	ND1=1	!PRINT EVERY CYCLE IN BISECT
	IF(deb) print 55,XLOW,XHIGH
	call CLRKB()
	CALL BISECv(ELFUNC,XLOW,XHIGH,0.0,XLIM,YOUT,EPSX,EPSY,
     & NERR,ND1,omit,
     & Xobs,yobs,w,nj,juse,setx,niobs,njset,theta,jfix,ndth)
	if(iabs(nerr).ge.1.and.iabs(nerr).le.3) then
	   call BELL(2)
	   if(iabs(nerr).lt.3) then
	      print 554
554	      format(' Guesses for upper limit do not include result:')
	   endif
	   print 5521
5521	   format(
     & ' (1) give new guesses for upper limit and try again',/,
     & ' (2) go on to next limit',/,
     & ' (3) abandon likelihood interval calculations',/,
     & ' Option number [1] = ')
	   read 4,i
	   iopt=1
	   if(i.ge.1.and.i.le.3) iopt=i
	   if(iopt.eq.1) then
	      print 33,xul(1,ip),xul(2,ip)
33	      FORMAT('&  Present guesses = ',2g12.4,
     &	': new guesses, x1,x2 = ')
	      call INPUT2r(xlow,xhigh)
		if(xlow.gt.xhigh) then
		   p=xlow
		   xlow=xhigh
		   xhigh=p
		endif
		XUL(1,J)=xlow
		XUL(2,J)=xhigh
		goto 555
	   endif
	   if(iopt.eq.2) goto 51
	   jfix(npar)=0	!UNFIX this parameter if abandoned
	   if(iabs(nerr).eq.3) goto 992	!iopt=3
	   goto 991					!iopt=3
	endif
	print 37,em,XLIM,xlim-theta(npar)
	if(prt) write(7,37) em,XLIM,xlim-theta(npar)
      if(discprt) write(8,37) em,XLIM,xlim-theta(npar)
37	FORMAT(
     & '   UPPER ',F6.2,'-unit interval= ',G13.6,' ( dev= ',g13.6,')')
	if(prt) write(7,361)ELMAX,elcrit+yout,ELCRIT,YOUT
      if(discprt) write(8,361)ELMAX,elcrit+yout,ELCRIT,YOUT
	print 361,ELMAX,ELCRIT,elcrit+yout,YOUT
C
c	if(ip.lt.np) then
c	   print 363
c363	   format(' Hit any key to do the next parameter')
c	   call ANYKEY
c	endif
51	JFIX(npar)=0	!UNFIX THIS PARAM BEFORE GOING TO NEXT
48	continue		!NEXT PARAM
c
	goto 99		!finished
c
c Error returns
992	print 30
30	format(' Bisection aborted')
991	continue
99	CONTINUE
	if(prt) call flush(7)
	RETURN
	END


