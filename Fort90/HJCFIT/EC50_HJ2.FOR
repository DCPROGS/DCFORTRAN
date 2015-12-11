	subroutine EC50_HJ2(EC50,concmax,pop0,
     & pmax,nlvar,jset,QT,conc,k,nerr,monot,decline,
     & tres,kA,kF,kAm,kFm,km,p0hjc,pmaxhjc,ec50hjc)

c To estimate numerically the equilibrium EC50 and maximum equilibrium
c response for a model specified by QT.
c Assumes QT has NOT got any concentrations in it on entry
c
c EC50_HJ2 is version of EC50_HJC that tries to get also the EC50 for
c the apparent P(open) as found by HJCMEAN. To get the max apparent P(open)
c it uses the concentration that is found to produce the 'max' true P(open)
c Last row of parameters added for HJC values
c tres=resolution, real*8, seconds
c Uses Popen only, not current
c NB for zero conc, mean HJC shut time should be infinite, so can't calc
c the HJC Popen.  Therefore, if pop0 is zero (< 1.e-10) set p0hjc=pop0
c
c Modified 09/01/04 12:25pm because when nlig>1 the set number is NOT irrelevant
c because the fixed conc of other ligand(s) may vary from one set to another
c and their conc may affect the ec50 for the conc of the variable ligand
c (i.e. #nlvar).  Therefore jset added to arguments.
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
c Modified 12/08/04 08:21am for case where ligand is inhibitor, high conc
c used to find pmax (actually pmin in this case) may give current=0, and hence
c divide by zero.
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
	real*8 QT(km,km)
	real*4 xA,concmax
	real*4 conc(10,10),concsav(10,10)
c	real*4 V1
	real*8 xAd,rel,rellast
	Allocatable:: QD,pinf
	real*8 QD(:,:),pinf(:)
	logical monot,flat,decline
	COMMON/LIG/nlig,IL(100)
	common/cpar/ncdep,ix(100),jx(100),x
c=	common/ec2/monot,nldum,vhold,vref,cfacd,dgamma	!for ec50_hjc,qset_hjc
	logical deb
c
	logical*4 fastblk
	real*4 aKB
	integer*4 ifb(20),jfb(20),mfb(20),iflig
	common/fblck/fastblk,aKB,nfblock,iflig,ifb,jfb ,mfb
c
c	ALLOCATE(QD(k,k),pinf(k))
	ALLOCATE(QD(km,km),pinf(km))
	deb=.false.		!can set =T under debugger
	monot=.true.	!equilib d/r curve is monotonic vs conc
c  qnew_hjc expects concentration in the conc array so save original
c conc(nlvar,j) changed but j is irrelevant so set to 1
c NO -see modification above -jset is now an argument
c===	jset=1
	do i=1,nlig
	   concsav(i,jset)=conc(i,jset)
	enddo
c
c FIND MINIMUM CURRENT (conc=0) -not always zero!
	conc(nlvar,jset)=0.0
c	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
c 05/23/02 05:06pm allocated size of QD added as arg of qnew_hjc
	call QNEW_HJC(QT,jset,conc,QD,ncdep,nlig,IL,IX,JX,k,km,km)
c QD still same as used to calculate Pop0 (ideal case)
Check ideal first
	call EQOC_HJC(QD,pinf,k,km,km)
	Popen=0.0d0
	do i=1,kA
	   Popen=Popen+pinf(i)
	enddo
	pop0=Popen
c
	if(pop0.lt.1.d-10) then
	   p0hjc=pop0	!can't calc pop0hjc if it is zero
	else
	   call HJCMEAN(QD,tres,hmopen,hmshut,
     &   k,kA,kF,kAm,kFm,km)
	   P0hjc=sngl(hmopen/(hmopen+hmshut))	!HJC Popen real*8
	endif
c
c Add section here to check whether reponse increases or decreases with
c ligand concentration (may decrease if ligand is inhibitor). To do this
c find Popen at a single high conc (say 1 molar)
	popen0=Popen
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
	decline=Popen.lt.Popen0
c
c First find maximum response.  NB use of difference in current
c between two succesive conc could result in convergence when max reached
c -better check by requiring twp successive differences > 0.0001?
	nit=1		!count number of cycles
c	Popen=0.0d0	!initial value for poplast
c	rel=1.0d0
	xA=1.0e-9		!start at 1 nM
	xad=1.0d-9
	fac=dsqrt(10.d0)
	flat=.false.
	do while(.not.flat.and.xAd.lt.100.d0)	!up to 100M if mot flat!
	   xA=sngl(xAd)
2	   conc(nlvar,jset)=xA		!set the conc
c	   V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	   call QNEW_HJC(QT,jset,conc,QD,ncdep,nlig,IL,IX,JX,k,km,km)
	   call EQOC_HJC(QD,pinf,k,km,km)
	   Popen=0.0d0
	   do i=1,kA
		Popen=Popen+pinf(i)
	   enddo
c In fastblk case, correct Popen for unresolved block
	   if(fastblk.and.(nlvar.eq.iflig)) then
		x1=1.0 + conc(iflig,jset)/aKB
		Popen=Popen/x1
	   endif
	   if(deb) then		!repeat calc with specified conc
		print 1
1		format(' conc (muM) = ')
		call INPUTr(xA)
		xA=xA*1.e-6
		goto 2
	   endif
c check for zero denom in rel (can occur at high conc of inhibitory ligand)
	   if(nit.gt.1) then
	      if(decline.and.dabs(Popen).lt.1.d-12) then
		   flat=dabs(poplast).lt.1.d-12
		else
		   rel=(Popen-poplast)/Popen
		   if(nit.gt.2.and.Popen.gt.1.d-5) then		!rellast not defined until nit=2
			if(rel*rellast.lt.-1.d-10) then	!goes through min/max
			   monot=.false.			!not monotonic
			   xA1=xAd/fac				!conc before max
			   xA2=xAd				!conc after max
			endif
c			flat=dabs(rel).lt.1.d-12.and.dabs(rellast).lt.1.d-12
			flat=dabs(rel).lt.1.d-5.and.dabs(rellast).lt.1.d-5
		   endif
		endif
		if(xAd.lt.1.d-2) flat=.false.	!don't leave before 10 mM
		rellast=rel
	   endif
	   poplast=Popen
	   nit=nit+1		!count number of cycles
	   xAd=xAd*fac
	enddo
	Pinf1=Popen		!for debug
	cinf=dble(xA)		!conc for this current
  	if(fastblk.and.(nlvar.eq.iflig)) then
	   x1=1.0 + xA/aKB
	   Pinf1=Pinf1/x1
	endif
c QD still same as used to calculate Pinf1 (ideal case)
	call HJCMEAN(QD,tres,hmopen,hmshut,
     &   k,kA,kF,kAm,kFm,km)
	Pmaxhjc=sngl(hmopen/(hmopen+hmshut))	!HJC Popen real*8
  	if(fastblk.and.(nlvar.eq.iflig)) then
	   x1=1.0 + xA/aKB
	   Pmaxhjc=Pmaxhjc/x1
	endif
c
c If not monotonic then need now to find Ymax and conc=cmax for Ymax
c more accurately cmax should be between xA1 and xA2. But no simple way
c to determine the slope of equil d/r curve? Just take small steps between
c xA1 and xA2 -use bisection with function=slope of d/r curve found as
c difference in response between two conc that are current x plus/minus dc
	if(monot) then
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
c=	   do while(dabs(Yerr).gt.epsy)
		Xout=0.5d0*(x1+x2)
c	Yout=func(Xout)
c In this case function is slope of equilib d/r curve calc as the difference
c in response between xA/dcfac and xA*dcfac
		xA=sngl(Xout/dcfac)
		conc(nlvar,jset)=xA		!set the conc
c		V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
		call QNEW_HJC(QT,jset,conc,QD,ncdep,nlig,IL,IX,JX,k,km,km)
		call EQOC_HJC(QD,pinf,k,km,km)
		Popen=0.0d0
		do i=1,kA
		   Popen=Popen + pinf(i)
	      enddo
		Pout1=Popen	!response at Xout/dcfac
	  	if(fastblk.and.(nlvar.eq.iflig)) then
		   x1=1.0 + xA/aKB
		   Pout1=Pout1/x1
		   Popen=Popen/x1
		endif
		xA=sngl(Xout*dcfac)
		conc(nlvar,jset)=xA		!set the conc
c		V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
		call QNEW_HJC(QT,jset,conc,QD,ncdep,nlig,IL,IX,JX,k,km,km)
		call EQOC_HJC(QD,pinf,k,km,km)
		Popen=0.0d0
		do i=1,kA
		   Popen=Popen + pinf(i)
	      enddo
		Pout2=Popen	!response at Xout*dcfac
	  	if(fastblk.and.(nlvar.eq.iflig)) then
		   x1=1.0 + xA/aKB
		   Pout2=Pout2/x1
		endif
c Fix so this works if pmax<pmin for inhibitory ligand
c		Pout=Pout2-Pout1		!proportional to slope of equilib d/r curve
		Pout=dabs(Pout2-Pout1)		!proportional to slope of equilib d/r curve
		Yerr=Pout			!Yout=0 for max
		if(Yerr.eq.0.0d0) goto 98	!exact solution
		if(Yerr.lt.0.0d0) then
		   x1=xout
		else
		   x2=xout
		endif
	   enddo
98	   continue
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
	epsy=0.001d0		!accuracy in cur/curmax=0.5
	Perr=2.0d0*epsy	!to start
	x1=0.0d0
	x2=cmax
c	call BISECd(FUNC,X1,X2,Y,XOUT,YOUT,EPSx,epsy,
c     & NERR,NDISP,OMIT)
c	do ns=1,nstep
c Danger of 'do while' form is that it can lead to infinite loop so set
c a maximum number of steps=number needed for 1 nM accuracy in conc
	nerr=0
	epsx=0.1e-9		!accuracy=0.1 nM
	dlog2=0.693147180559945d0
	nstepmax=ifix(sngl(dlog(dabs(x1-x2)/epsx)/dlog2+0.5))
	nstep=0
	do while(dabs(Perr).gt.epsy)
	   nstep=nstep+1
	   if(nstep.gt.nstepmax) then
c		nerr=3	!NO -reaching nstepmax is not an error
		goto 99
	   endif
c
	   Xout=0.5d0*(x1+x2)
	   xA=sngl(Xout)
	   conc(nlvar,jset)=xA		!set the conc
c	   V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	   call QNEW_HJC(QT,jset,conc,QD,ncdep,nlig,IL,IX,JX,k,km,km)
	   call EQOC_HJC(QD,pinf,k,km,km)
	   pout=0.0d0
	   do i=1,kA
		pout=pout + pinf(i)
         enddo
	   if(fastblk.and.(nlvar.eq.iflig)) then
		x1=1.0 + xA/aKB
		pout=pout/x1
	   endif
	   Pout=dabs((Pout-pop0)/(Pmax-pop0))
	   Perr=Pout - 0.5d0	!Yout=0.5 for EC50
	   if(Perr.eq.0.0d0) goto 99	!exact solution
	   if(Perr.lt.0.0d0) then
		x1=xout
	   else
		x2=xout
	   endif
	enddo
99	continue
	EC50=xout
c
c Repeat above bit for HJC Popen
	epsy=0.001d0		!accuracy in cur/curmax=0.5
	Perr=2.0d0*epsy	!to start
	x1=ec50*0.1d0	!base guess on 'ideal' ec50
	x2=ec50*10.d0
c Danger of 'do while' form is that it can lead to infinite loop so set
c a maximum number of steps=number needed for 1 nM accuracy in conc
	nerr=0
	epsx=0.1e-9		!accuracy=0.1 nM
c	dlog2=0.693147180559945d0
	nstepmax=ifix(sngl(dlog(dabs(x1-x2)/epsx)/dlog2+0.5))
	nstep=0
	do while(dabs(Perr).gt.epsy)
	   nstep=nstep+1
	   if(nstep.gt.nstepmax) then
		goto 991
	   endif
c
	   Xout=0.5d0*(x1+x2)
	   xA=sngl(Xout)
	   conc(nlvar,jset)=xA		!set the conc
c	   V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	   call QNEW_HJC(QT,jset,conc,QD,ncdep,nlig,IL,IX,JX,k,km,km)
	   call HJCMEAN(QD,tres,hmopen,hmshut,
     &  	 k,kA,kF,kAm,kFm,km)
	   pout_hjc=sngl(hmopen/(hmopen+hmshut))	!HJC Popen
	   Pout_hjc=dabs((Pout_hjc-p0hjc)/(Pmaxhjc-p0hjc))
	   if(fastblk.and.(nlvar.eq.iflig)) then
		x1=1.0 + xA/aKB
		pout_hjc=pout_hjc/x1
	   endif
	   Perr=Pout_hjc - 0.5d0	!Yout=0.5 for EC50
	   if(Perr.eq.0.0d0) goto 991	!exact solution
	   if(Perr.lt.0.0d0) then
		x1=xout
	   else
		x2=xout
	   endif
	enddo
991	continue
	EC50hjc=xout
c
c
	DEALLOCATE(QD,pinf)
	do i=1,nlig
	   conc(i,jset)=concsav(i,jset)
	enddo
	RETURN
	end

