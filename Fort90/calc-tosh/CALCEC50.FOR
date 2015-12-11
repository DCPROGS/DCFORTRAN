	subroutine CALCEC50(EC50,curmax,QT,env,gamma,k,kdim)
c To estimate numerically the equilibrium EC50 for model specified by QT
c Assumes QT has NOT got any concentrations in it on entry
c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 QT(kdim,kdim)
	real*4 xA,xB,EC50,curmax,env,x,gamma(kdim)
	Allocatable:: QD,pinf,dgamma
	real*8 QD(:,:),pinf(:),dgamma(:)
	COMMON/LIG/nlig,IL(10)
	common/KBLK/kA,kB,kC,kD
	common/cpar/ncdep,ix(10),jx(10),x
c
	ALLOCATE(QD(kdim,kdim),pinf(kdim),dgamma(kdim))
	km=10
	envd=dble(env)
	do i=1,k
	   dgamma(i)=dble(gamma(i))
	enddo
c First find maximum response.  NB use of difference in current
c between two succesive conc could result in convergence when max reached
c -better check by requiring twp successive differences > 0.0001?
	current=0.0d0	!initial value for curlast
	xA=1.0e-9		!start at 1 nM
	rel=10.0d0
	do while(dabs(rel).gt.0.00001d0)
	   curlast=current
	   call QNEWC(QT,1.0,1.0,QD,xA,xB,
     &    ncdep,nlig,IL,IX,JX,k,km)		!Get Q(xA0) in Q0
	   call EQOCCd(QD,k,k-1,km,pinf)	!calc init occs at xA0 -> p0
	   current=0.0d0
	   do i=1,kA
		current=current+envd*dgamma(i)*pinf(i)
	   enddo
	   rel=(current-curlast)/current
	   xA=xA*10.0
	enddo
	Ymax=current
	cmax=dble(xA)		!conc for this current
c Now find conc for 50% of curmax
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
	do while(dabs(Yerr).gt.epsy)
	   nstep=nstep+1
	   if(nstep.gt.nstepmax) then
c		nerr=3		!NO -reaching nstepmax is not an error
		goto 99
	   endif
	   Xout=0.5d0*(x1+x2)
	   xA=sngl(Xout)
c	   Yout=func(Xout)
	   call QNEWC(QT,1.0,1.0,QD,xA,xB,
     &    ncdep,nlig,IL,IX,JX,k,km)		!Get Q(xA0) in Q0
	   call EQOCCd(QD,k,k-1,km,pinf)	!calc init occs at xA0 -> p0
	   Yout=0.0d0
	   do i=1,kA
		Yout=Yout+envd*dgamma(i)*pinf(i)
         enddo
	   Yout=Yout/Ymax		!current as fraction of max
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
	DEALLOCATE(QD,pinf,dgamma)
	RETURN
	end

