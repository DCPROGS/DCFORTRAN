	subroutine EQEC50(EC50,curinfs,monot,curmax,concmax,
     & cur0,pop0,QT,conc,nlvar,vhold,vref,cfacd,dgamma,plotcur,k,kdim)
c To estimate numerically the equilibrium EC50 and maximum equilibrium
c response for a model specified by QT.
c Assumes QT has NOT got any concentrations in it on entry
c
c If the EQUILIBRIUM d/r curve is monotonic then this routine returns
c monot=true, and the unambiguous maximum reponse amd ec50 in CURMAX and EC50.
c
c But the EQUILIBRIUM d/r curve may go through a maximum, in which case
c the largest response will not be at infinite concentration, and monot=.false.
c is returned.  In this case curmax is the peak response (peak vs conc) and
c curinf returns the equil reponse for conc -> infinity. EC50 returns the conc
c for 50% of curmax measured to the left of the peak.
c
c Modified 05/22/02 07:10pm to work with mechanisms that do not give zero
c response at zero conc; now returns also cur0, pop0=current and Popen at c=0
c
c Modified 02/13/02 09:15pm by adding plotcur to arguments. If plotcur=true
c returns current as before, if plotcur=false returns Popen
c
c Modified 03/14/01 12:20pm for 100 states/10 ligands. The conc are now
c in arguments, in conc(10), and conc(nlvar) is the one that is changed
c (if nlig>1 then others are constant, as for rest of calcs, but not nec zero)
c The input value of conc() is restored before return
c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 QT(kdim,kdim),dgamma(100)
	real*4 xA,EC50,curmax,curinfs,concmax,cur0,pop0
	real*4 conc(10),concsav(10),vhold,vref,V1
	real*8 xAd,rel,rellast,cfacd
	Allocatable:: QD,pinf
	real*8 QD(:,:),pinf(:)
	logical monot,flat,plotcur
	COMMON/LIG/nlig,IL(100)
	common/KBLK/kA,kB,kC,kD
	common/cpar/ncdep,ix(100),jx(100),x
c
	ALLOCATE(QD(kdim,kdim),pinf(kdim))
	km=100
	monot=.true.	!equilib d/r curve is monotonic vs conc
	do i=1,nlig
	   concsav(i)=conc(i)
	enddo
c
c FIND MINIMUM CURRENT (conc=0) -not always zero!
	conc(nlvar)=0.0
	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	call QSETD(conc,IL,V1,QT,QD,k,.false.)
	call EQOCCd(QD,k,k-1,km,pinf)
	current=0.0d0
	Popen=0.0d0
	do i=1,kA
	   current=current+cfacd*dgamma(i)*pinf(i)
	   Popen=Popen+pinf(i)
	enddo
	cur0=sngl(current)
	pop0=sngl(Popen)
	if(plotcur) then
	   Y0=current
	else
	   Y0=Popen
	endif
c
c FIND MAXIMUM response.  NB use of difference in current
c between two succesive conc could result in convergence when max reached
c -better check by requiring twp successive differences > 0.0001?
	current=0.0d0	!initial value for curlast
	xA=1.0e-9		!start at 1 nM
	xad=1.0d-9
	fac=dsqrt(10.d0)
	rel=1.0d0
	flat=.false.
	do while(.not.flat.and.xAd.lt.100.d0)	!up to 100M if not flat
	   curlast=current
	   xA=sngl(xAd)
	   conc(nlvar)=xA		!set the conc
	   V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	   call QSETD(conc,IL,V1,QT,QD,k,.false.)
	   call EQOCCd(QD,k,k-1,km,pinf)	!calc init occs at xA0 -> p0
	   current=0.0d0
	   if(plotcur) then
		do i=1,kA
		   current=current+cfacd*dgamma(i)*pinf(i)
		enddo
	   else
		do i=1,kA
		   current=current+pinf(i)
		enddo
	   endif
	   rellast=rel
	   rel=(current-curlast)/current
	   if(rel*rellast.lt.-1.d-10) then	!goes through min/max
		monot=.false.			!not monotonic
		xA1=xAd/fac				!conc before max
		xA2=xAd				!conc after max
	   endif
c	   flat=dabs(rel).lt.1.d-12.and.dabs(rellast).lt.1.d-12
	   flat=dabs(rel).lt.1.d-6.and.dabs(rellast).lt.1.d-6
	   xAd=xAd*fac
	enddo
	Yinf=current
	cinf=xAd		!conc for this current
c
c If not monotonic then need now to find Ymax and conc=cmax for Ymax
c more accurately cmax should be between xA1 and xA2. But no simple way
c to determine the slope of equil d/r curve? Just take small steps between
c xA1 and xA2 -use bisection with function=slope of d/r curve found as
c difference in response between two conc that are current x plus/minus dc
	if(monot) then
	   Ymax=Yinf
	   cmax=cinf
	else		!find Ymax
	   x1=xA1
	   x2=xA2
	   epsx=xA1/1000.		!accuracy in conc
c	   epsy=0.00011d0		!accuracy in slope=0
	   Yerr=2.0d0*epsy	!to start
	   dcfac=1.01			!concentration increment for calc of slope
	   dlog2=0.693147180559945d0
	   nstep=ifix(sngl(dlog(dabs(x1-x2)/epsx)/dlog2+0.5))
	   do ns=1,nstep
c===	   do while(dabs(Yerr).gt.epsy)
		Xout=0.5d0*(x1+x2)
c	Yout=func(Xout)
c In this case function is slope of equilib d/r curve calc as the difference
c in response between xA/dcfac and xA*dcfac
		xA=sngl(Xout/dcfac)
c==		call QNEWC(QT,1.0,1.0,QD,xA,xB,
c==     &        ncdep,nlig,IL,IX,JX,k,km)		!Get Q(xA0) in Q0
		conc(nlvar)=xA		!set the conc
		V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
		call QSETD(conc,IL,V1,QT,QD,k,.false.)
		call EQOCCd(QD,k,k-1,km,pinf)	!calc init occs at xA0 -> p0
		Yout=0.0d0
		if(plotcur) then
		   do i=1,kA
			Yout=Yout+cfacd*dgamma(i)*pinf(i)
	         enddo
		else
		   do i=1,kA
			Yout=Yout+pinf(i)
		   enddo
		endif
		Yout1=Yout	!response at Xout/dcfac
		xA=sngl(Xout*dcfac)
c==		call QNEWC(QT,1.0,1.0,QD,xA,xB,
c==     &        ncdep,nlig,IL,IX,JX,k,km)		!Get Q(xA0) in Q0
		conc(nlvar)=xA		!set the conc
		V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
		call QSETD(conc,IL,V1,QT,QD,k,.false.)
		call EQOCCd(QD,k,k-1,km,pinf)	!calc init occs at xA0 -> p0
		Yout=0.0d0
		if(plotcur) then
		   do i=1,kA
			Yout=Yout+cfacd*dgamma(i)*pinf(i)
	         enddo
		else
		   do i=1,kA
			Yout=Yout+pinf(i)
		   enddo
		endif
		Yout2=Yout	!response at Xout*dcfac
		Yout=Yout2-Yout1		!proportional to slope of equilib d/r curve
		Yerr=Yout			!Yout=0 for max
		if(Yerr.eq.0.0d0) goto 98	!exact solution
		if(Yerr.lt.0.0d0) then
		   x1=xout
		else
		   x2=xout
		endif
	   enddo
98	   continue
	   Ymax=(Yout1+Yout2)/2.d0
	   curmax=sngl(Ymax)
	   cmax=Xout
	   concmax=sngl(cmax)
	endif
c
c Now find conc for 50% of curmax
c If monotonic this is unambiguous.  If not monotonic, then EC50 returned
c as conc (to left of peak) for 50% of peak response
c Can't put allocatable array QD in common so write code from BISECTd here
c Best in this case to use EPSY so can specify absolute error in cur/curmax
c	epsx=0.1e-9		!accuracy=0.1 nM
	epsy=0.001d0		!accuracy in cur/curmax
	Yerr=2.0d0*epsy	!to start
	x1=0.0d0
	x2=cmax
c	call BISECd(FUNC,X1,X2,Y,XOUT,YOUT,EPSx,epsy,
c     & NERR,NDISP,OMIT)
c	dlog2=0.693147180559945d0
c	nstep=ifix(sngl(dlog(dabs(x1-x2)/epsx)/dlog2+0.5))
c	do ns=1,nstep
c Danger of 'do while' form is that it can lead to infinite loop so set
c a maximum number of steps=number needed for 1 nM accuracy in conc
c	nerr=0
	epsx=0.1e-9		!accuracy=0.1 nM
	dlog2=0.693147180559945d0
	nstepmax=ifix(sngl(dlog(dabs(x1-x2)/epsx)/dlog2+0.5))
	nstep=0
	do while(dabs(Yerr).gt.epsy)
	   nstep=nstep+1
	   if(nstep.gt.nstepmax) then
c		nerr=3		!NO -reaching nstepmax is not an error
		goto 99
	   endif
	   Xout=0.5d0*(x1+x2)
	   xA=sngl(Xout)
c	   Yout=func(Xout)
	   conc(nlvar)=xA		!set the conc
	   V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	   call QSETD(conc,IL,V1,QT,QD,k,.false.)
c==	   call QNEWC(QT,1.0,1.0,QD,xA,xB,
c==     &    ncdep,nlig,IL,IX,JX,k,km)		!Get Q(xA0) in Q0
	   call EQOCCd(QD,k,k-1,km,pinf)	!calc init occs at xA0 -> p0
	   Yout=0.0d0
	   if(plotcur) then
	      do i=1,kA
		   Yout=Yout+cfacd*dgamma(i)*pinf(i)
	      enddo
	   else
	      do i=1,kA
		   Yout=Yout+pinf(i)
	      enddo
	   endif
c	   Yout=Yout/Ymax		!current as fraction of max
	   Yout=(Yout-Y0)/(Ymax-Y0)
	   Yerr=Yout - 0.5d0	!Yout=0.5 for EC50
	   if(Yerr.eq.0.0d0) goto 99	!exact solution
	   if(Yerr.lt.0.0d0) then
		x1=xout
	   else
		x2=xout
	   endif
	enddo
99	continue
c
	EC50=sngl(xout)
	curmax=sngl(Ymax)
	curinfs=sngl(Yinf)
	DEALLOCATE(QD,pinf)
	do i=1,nlig
	   conc(i)=concsav(i)
	enddo
	RETURN
	end

