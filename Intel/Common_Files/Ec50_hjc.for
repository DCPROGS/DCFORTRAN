	subroutine EC50_HJC(EC50,curinfs,curmax,concmax,cur0,pop0,
     & pmax,nlvar,QT,jset,conc,k,kdim,nerr)

c EC50out,curinfs,curmax,concmax,cur0,pop0,pmax,nlv0,QT,iset,conc,k,kdim,nerr     
c To estimate numerically the equilibrium EC50 and maximum equilibrium
c response for a model specified by QT.
c Assumes QT has NOT got any concentrations in it on entry
c
c EC50_HJC is version of eqec50 for hjcfit in which conc(i,j) is 2-dimensional
c and qnew_hjc is called. EC50 now returned as real*8
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
c Modified 10/09/03 03:01pm by adding pmax to parameters, and nlvar
c (not simple to change common/ec2/ so nlvar in there called nldum here
c Modified 01/21/02 11:30am by removing some args and putting them into
c
c	common/ec2/monot,nlvar,vhold,vref,cfacd,dgamma		!for ec50_hjc
c
c Modified 05/22/02 07:10pm to work with mechanisms that do not give zero
c response at zero conc; now returns also cur0, pop0=current and Popen at c=0
c
c Modified 03/14/01 12:20pm for 100 states/10 ligands. The conc are now
c in arguments, in conc(10), and conc(nlvar) is the one that is changed
c (if nlig>1 then others are constant, as for rest of calcs, but not nec zero)
c The input value of conc() is restored before return
c

	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 QT(kdim,kdim),dgamma(100)
	real*4 xA,curmax,curinfs,concmax,cur0,pop0
	real*4 conc(10,10),concsav(10,10),vhold,vref
      real*8 xAd,rel,rellast,cfacd
	Allocatable:: QD,pinf
	real*8 QD(:,:),pinf(:)
	logical monot,flat,decline
	COMMON/LIG/nlig,IL(100)
	common/KBLK/kA,kB,kC,kD
	common/cpar/ncdep,ix(100),jx(100),x
	common/ec2/monot,nldum,vhold,vref,cfacd,dgamma	!for ec50_hjc,qset_hjc
	logical deb
	logical*4 fastblk
	real*4 aKB
	integer*4 ifb(20),jfb(20),mfb(20),iflig
	common/fblck/fastblk,aKB,nfblock,iflig,ifb,jfb ,mfb

	ALLOCATE(QD(k,k),pinf(k))
	deb=.false.		!can set =T under debugger
c	km=100
	km=kdim		!in case QT in call is not 100 x 100
	monot=.true.	!equilib d/r curve is monotonic vs conc

c  qnew_hjc expects concentration in the conc array so save original
c conc(nlvar,j) changed but j is irrelevant so set to 1

	if(jset.le.0.or.jset.gt.20) jset=1
	do i=1,nlig
	   concsav(i,jset)=conc(i,jset)
	enddo
      do i=1,k
        pinf(i)=0.0d0
        do j=1,k
            qd(i,j)=0.0d0
        enddo
      enddo
c
c FIND MINIMUM CURRENT (conc=0) -not always zero!
	conc(nlvar,jset)=0.0
c	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
c 05/23/02 05:06pm allocated size of QD added as arg of qnew_hjc
	
	call QNEW_HJC(QT,jset,conc,QD,ncdep,nlig,IL,IX,JX,k,km,k)
	call EQOC_HJC(QD,pinf,k,k,k)
	current=0.0d0
	Popen=0.0d0
	Popen0=0.0d0
	do i=1,kA
	   current=current+cfacd*dgamma(i)*pinf(i)
	   Popen=Popen+pinf(i)
	enddo
	Y0=current
	
	cur0=sngl(current)

c     ???????????????????????????
      
      pop0=sngl(Popen)
	
	popen0=Popen	!for debug
	conc(nlvar,jset)=1.0		!molar
	call QNEW_HJC(QT,jset,conc,QD,ncdep,nlig,IL,IX,JX,k,km,k)
	call EQOC_HJC(QD,pinf,k,k,k)
	Popen=0.0d0
	do i=1,kA
	   Popen=Popen+pinf(i)
	enddo
c In fastblk case, correct Popen for unresolved block
	if(fastblk.and.(nlvar.eq.iflig)) then
	   x1=1.0 + conc(iflig,jset)/aKB
	   Popen=Popen/x1
	endif
c
	decline=Popen.lt.Popen0
c
c First find maximum response.  NB use of difference in current
c between two succesive conc could result in convergence when max reached
c -better check by requiring twp successive differences > 0.0001?
c Problem can occur if 1 nM is very much below conc at which Popen starts
c to change with conc -can get convergence (flat=true) before Popen starts
c to change rather than after it has finished changing, so do not allow
c convergence until conc is at least, say, 10 mM

c	current=0.0d0	!initial value for curlast
c	rel=1.0d0

	nit=1		!count number of cycles
	current=0.0d0	!initial value for curlast
	xA=1.0e-9		!start at 1 nM
	xad=1.0d-9
	fac=dsqrt(10.d0)
!	rel=1.0d0
	flat=.false.
	do while(.not.flat.and.xAd.lt.100.d0)	!up to 100M if mot flat!
c	   curlast=current
	   xA=sngl(xAd)
2	   conc(nlvar,jset)=xA		!set the conc
c	   V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	   call QNEW_HJC(QT,jset,conc,QD,ncdep,nlig,IL,IX,JX,k,km,k)
	   call EQOC_HJC(QD,pinf,k,k,k)
	   current=0.0d0
	   Popen=0.0d0
	   do i=1,kA
		    current=current+cfacd*dgamma(i)*pinf(i)
		    Popen=Popen+pinf(i)
	   enddo
	   if(fastblk.and.(nlvar.eq.iflig)) then
		    x1=1.0 + conc(iflig,jset)/aKB
		    current=current/x1
		    Popen=Popen/x1
	   endif
	  
	   if(nit.gt.1) then
	      if(decline.and.dabs(current).lt.1.d-12) then
		        flat=dabs(curlast).lt.1.d-12
		    else
		        rel=(current-curlast)/current
		        if(nit.gt.2.and.current.gt.1.d-5) then		!rellast not defined until nit=2
			    if(rel*rellast.lt.-1.d-10) then	!goes through min/max
			        monot=.false.			!not monotonic
			        xA1=xAd/fac				!conc before max
			        xA2=xAd				!conc after max
			    endif
c			    flat=dabs(rel).lt.1.d-12.and.dabs(rellast).lt.1.d-12
			    flat=dabs(rel).lt.1.d-5.and.dabs(rellast).lt.1.d-5
		        endif
		    endif
		    if(xAd.lt.1.d-2) flat=.false.	!don't leave before 10 mM
		    rellast=rel
	   endif
	   xAd=xAd*fac
	   curlast=current
	   nit=nit+1   
	enddo
	
	Yinf=current
	Pinf1=Popen		!for debug
	cinf=dble(xA)		!conc for this current
c
c If not monotonic then need now to find Ymax and conc=cmax for Ymax
c more accurately cmax should be between xA1 and xA2. But no simple way
c to determine the slope of equil d/r curve? Just take small steps between
c xA1 and xA2 -use bisection with function=slope of d/r curve found as
c difference in response between two conc that are current x plus/minus dc

	if(monot) then
	   Ymax=Yinf
	   cmax=cinf
	   pmax=Pinf1		!for debug
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
c	      Yout=func(Xout)
c In this case function is slope of equilib d/r curve calc as the difference
c in response between xA/dcfac and xA*dcfac
		    xA=sngl(Xout/dcfac)
		    conc(nlvar,jset)=xA		!set the conc
c		    V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
		    call QNEW_HJC(QT,jset,conc,QD,ncdep,nlig,IL,IX,JX,k,km,k)
		    call EQOC_HJC(QD,pinf,k,k,k)
		    Yout=0.0d0
		    Popen=0.0d0
		    do i=1,kA
		        Yout=Yout+cfacd*dgamma(i)*pinf(i)
		        Popen=Popen + pinf(i)
	      enddo
	      if(fastblk.and.(nlvar.eq.iflig)) then
		        x1=1.0 + xA/aKB
		        Yout=Yout/x1
		        Popen=Popen/x1
		    endif
		    Yout1=Yout	!response at Xout/dcfac
		    Pout1=Popen
		    xA=sngl(Xout*dcfac)
		    conc(nlvar,jset)=xA		!set the conc
c		    V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
		    call QNEW_HJC(QT,jset,conc,QD,ncdep,nlig,IL,IX,JX,k,km,k)
		    call EQOC_HJC(QD,pinf,k,k,k)
		    Yout=0.0d0
		    Popen=0.0d0
		    do i=1,kA
		        Yout=Yout+cfacd*dgamma(i)*pinf(i)
		        Popen=Popen + pinf(i)
	      enddo
		    Yout2=Yout	!response at Xout*dcfac
		    Pout2=Popen
		    if(fastblk.and.(nlvar.eq.iflig)) then
		        x1=1.0 + xA/aKB
		        Yout2=Yout2/x1
		        Pout2=Pout2/x1
		    endif
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
	   pmax=(pout1+pout2)/2.d0
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

	epsy=0.0001d0		!accuracy in cur/curmax
	Yerr=2.0d0*epsy	!to start
	x1=0.0d0
	x2=cmax

	nerr=0
	epsx=0.1e-9		!accuracy=0.1 nM
	dlog2=0.693147180559945d0
	nstepmax=ifix(sngl(dlog(dabs(x1-x2)/epsx)/dlog2+0.5))
	nstep=0
	do while(dabs(Yerr).gt.epsy)
	   nstep=nstep+1
	   if(nstep.gt.nstepmax) then
c		    nerr=3	!NO -reaching nstepmax is not an error
		    goto 99
	   endif
c
	   Xout=0.5d0*(x1+x2)
	   xA=sngl(Xout)
c	   Yout=func(Xout)
	   conc(nlvar,jset)=xA		!set the conc
c	   V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	   call QNEW_HJC(QT,jset,conc,QD,ncdep,nlig,IL,IX,JX,k,km,k)
	   call EQOC_HJC(QD,pinf,k,k,k)
	   Yout=0.0d0
	   do i=1,kA
		    Yout=Yout+cfacd*dgamma(i)*pinf(i)
         enddo
         if(fastblk.and.(nlvar.eq.iflig)) then
		    x1=1.0 + xA/aKB
		    Yout=Yout/x1
	   endif
c	   Yout=Yout/Ymax		!current as fraction of max
	   Yout=dabs(Yout-Y0)/(Ymax-Y0)
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
	EC50=xout
	curmax=sngl(Ymax)
	curinfs=sngl(Yinf)

	DEALLOCATE(QD,pinf)
	do i=1,nlig
	   conc(i,jset)=concsav(i,jset)
	enddo
	RETURN
	end

