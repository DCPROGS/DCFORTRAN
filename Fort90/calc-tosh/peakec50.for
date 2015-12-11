	subroutine PEAKEC50(EC50,curinfs,monotc,curmax,concmax,QT,
     & p0,cfacd,dgamma,plotcur,conc,nlvar,vhold,vref,debug,k,kdim)
c To estimate numerically the EC50 and maximum response where response is
c defined as the peak response when the response at each conc is plotted
c against time (EQEC50 is similar subroutine for equilib case)
c Call to expmaxd	requires time const etc in common/expd/ w,tau,yinf,ncomp,
c calculated for the current xA
c
c For a model specified by QT.
c Assumes QT has NOT got any concentrations in it on entry
c
c If the PEAK d/r curve is monotonic then this routine returns
c monotc=true, and the unambiguous maximum response amd ec50 in CURMAX and EC50.
c
c But the PEAK d/r curve may go through a maximum vs conc, in which case
c the largest response will not be at infinite concentration, and monotc=.false.
c is returned.  In this case curmax is the max response (peak vs conc),
c the peak occurs at concmax (if monotonic, concmac returs the highest
c concentration tested to reach the 'flat' part of the d/r curve), and
c curinf returns the peak reponse for conc -> infinity. EC50 returns the conc
c for 50% of curmax measured to the left of the peak.
c
c Input:
c	p0()=occupancies before the pulse normally same for all xA
c	QT=Q matrix with no conc
c	cfacd=nchannel*voltage (set cfacd=1.0d0 for conductance)
c	plotcur=true for current (or conductance), false for Popen
c Output:
c	monotc=true if peak response has no max when plotted against conc
c	   (monotp indicates response vs time is monotonic for each conc)
c	tpeak=time (ms) at which peak occurs (not def if monott=true)
c	ypeak=peak current (=yinf if monott=true)
c	yinf=equilib current (t->infinity) at conc xA
c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 QT(kdim,kdim),p0(kdim),dgamma(100),cfacd
	real*4 xA,EC50,curmax,curinfs,concmax,x
	real*4 conc(10),concsav(10),vhold,vref
	logical monotc,flat,debug,plotcur
	logical monott
	COMMON/LIG/nlig,IL(100)
	common/KBLK/kA,kB,kC,kD
	common/cpar/ncdep,ix(100),jx(100),x
c
	real*8 w(100),tau(100),yinf,yinf1
	common/expd/ w,tau,yinf,ncomp
	logical discprt
	common/dp/discprt
c
c
c	km=100
	monotc=.true.	!peak d/r curve is monotonic vs conc
	do i=1,nlig
	   concsav(i)=conc(i)
	enddo
c First find maximum response.  NB use of difference in current
c between two succesive conc could result in convergence when max reached
c -better check by requiring two successive differences > 0.0001?
	ypeak=0.0d0		!initial value for yplast
	xA=1.0e-9		!start at 1 nM
	xad=1.0d-9
	conc(nlvar)=xA		!set the conc
	fac=dsqrt(10.d0)
	rel=1.0d0
	flat=.false.
	do while(.not.flat.and.xAd.lt.100.d0)	!up to 100M if not flat
	   yplast=ypeak
c calc max current when current plotted vs time at conc=xA
	   call GETPEAK(conc,nlvar,p0,QT,cfacd,monott,tpeak,ypeak,yinf,
     & vhold,vref,dgamma,debug,plotcur,nerr,nerr1,nerr2,k,kdim)
	   rellast=rel
	   rel=(ypeak-yplast)/ypeak
	   if(rel*rellast.lt.-1.d-10) then	!goes through min/max
		monotc=.false.			!not monotonic
		xA1=xAd/fac				!conc before max
		xA2=xAd				!conc after max
	   endif
	   flat=dabs(rel).lt.0.00001d0.and.dabs(rellast).lt.0.00001d0
	   if(debug) then
		print 70,xAd,ypeak,yplast,rel,flat
		if(discprt) write(8,70) xAd,ypeak,yplast,rel,flat
70		format(' xA= ',g13.6,': yp,yplast,rel,flat = ',3g13.6,1x,l1)
	   endif
	   xAd=xAd*fac
	   conc(nlvar)=sngl(xAd)	!set the conc
	enddo
	Yinf1=ypeak		!define as yinf1 because yinf altered by calls to getpeak below
	cinf=xAd		!conc for this current
c
c If not monotonic then need now to find Ymax and conc=cmax for Ymax
c more accurately cmax should be between xA1 and xA2. But no simple way
c to determine the slope of equil d/r curve? Just take small steps between
c xA1 and xA2 -use bisection with function=slope of d/r curve found as
c difference in response between two conc that are current x plus/minus dc
	if(monotc) then
	   Ymax=Yinf1
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
		Xout=0.5d0*(x1+x2)
c	Yout=func(Xout)
c In this case function is slope of equilib d/r curve calc as the difference
c in response between xA/dcfac and xA*dcfac
		xA=sngl(Xout/dcfac)
		conc(nlvar)=xA		!set the conc
		call GETPEAK(conc,nlvar,p0,QT,cfacd,monott,tpeak,ypeak,yinf,
     & 	vhold,vref,dgamma,debug,plotcur,nerr,nerr1,nerr2,k,kdim)
		Yout1=Ypeak		!response at Xout/dcfac
		xA=sngl(Xout*dcfac)
		conc(nlvar)=xA		!set the conc
		call GETPEAK(conc,nlvar,p0,QT,cfacd,monott,tpeak,ypeak,yinf,
     & 	vhold,vref,dgamma,debug,plotcur,nerr,nerr1,nerr2,k,kdim)
		Yout2=Ypeak 	!response at Xout*dcfac
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
c	   concmax=sngl(cmax)
	endif
	concmax=sngl(cmax)
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
c Danger of 'do while' form is that it can lead to infinite loop so set
c a maximum number of steps=number needed for 1 nM accuracy in conc
	nerr=0
	epsx=0.1e-9		!accuracy=0.1 nM
	dlog2=0.693147180559945d0
	nstepmax=ifix(sngl(dlog(dabs(x1-x2)/epsx)/dlog2+0.5))
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
c  NB use yinf1 above, because next call alters Yinf (also
c Yinf is in common!)
	   call GETPEAK(conc,nlvar,p0,QT,cfacd,monott,tpeak,ypeak,yinf,
     & 	vhold,vref,dgamma,debug,plotcur,nerr,nerr1,nerr2,k,kdim)
	   Yout=Ypeak
	   p=Yout/Ymax		!current as fraction of max
	   Yerr=p - 0.5d0	!Yout=0.5 for EC50
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
	curinfs=sngl(Yinf1)
	do i=1,nlig
	   conc(i)=concsav(i)
	enddo
	RETURN
	end

