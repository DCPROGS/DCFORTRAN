	subroutine GETPEAK(conc,nlvar,p0,QT,envd,monott,tpeak,ypeak,yinf,
     & vhold,vref,dgamma,debug,plotcur,nerr,nerr1,nerr2,k,kdim)
c	subroutine GETPEAK(xA,xB,p0,QT,envd,monott,tpeak,ypeak,yinf,
c     & dgamma,debug,plotcur,nerr,nerr1,nerr2,k,kdim)
c To calculate peak current for current versus time for a step from
c concentration xA0 (usually always 0) to concentration xA
c
c Call to expmaxd	requires time const etc in common/expd/ w,tau,yinf,ncomp,
c calculated for the current xA
c
c For a model specified by QT.
c Assumes QT has NOT got any concentrations in it on entry
c Input:
c	xA=conc (molar) for which peak response to be calc
c	p0()=occupancies before the pulse normally same for all xA
c	QT=Q matrix with no conc
c	envd=nchannel*voltage (set envd=1.0d0 for conductance)
c	plotcur=true for current (or conductance), false for Popen
c Output:
c	monott=true if response vs time is monotonic
c	tpeak=time (ms) at which peak occurs (not def if monott=true)
c	ypeak=peak current (=yinf if monott=true)
c	yinf=equilib current (t->infinity) at conc xA

	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 QT(kdim,kdim),p0(kdim),dgamma(kdim)
	allocatable::Qd,pinf,amat,eigen,bmj,bm,pt
	real*8 QD(:,:),pinf(:),Amat(:,:,:),eigen(:),bmj(:,:),bm(:),pt(:)
	real*4 x
	real*4 conc(10),vhold,vref,V1
	logical monott,debug,plotcur
	real*8 w(100),tau(100),yinf
	common/expd/ w,tau,yinf1,ncomp
	COMMON/LIG/nlig,IL(100)
	common/KBLK/kA,kB,kC,kD
	common/cpar/ncdep,ix(100),jx(100),x
	logical discprt
	common/dp/discprt
c
	n=kdim
	ALLOCATE(QD(n,n),pinf(n),Amat(n,n,n),eigen(n),
     &	bmj(n,n),bm(n),pt(n))
c
	monott=.false.
	km=kdim
c Define, for current xA, w, tau, yinf and ncomp for common/expd/, needed in expmaxd
c (NB yinf in common and in call to expmaxd are same (but need diff names)
      V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	call QSETD(conc,IL,V1,QT,QD,k,.false.)	!xA in conc(nlvar) before call
	call EQOCCd(QD,k,k-1,km,pinf)
	call QMAT5(QD,Amat,k,eigen,ibad,km,km,km)
	call PTCALC(pt,0.0,bmj,p0,pinf,eigen,amat,k,km)	!calc the bmj()
c       Calc yinf for common/expmaxd
	yinf=0.0d0
	do j=1,kA
	   if(plotcur) then
		yinf=yinf+envd*dgamma(j)*pinf(j)	!current
	   else
		yinf=yinf + pinf(j)			!Popen
	   endif
	enddo
	yinf1=yinf		!for common
c       Calc tau() and w() for common/expmaxd
	ncomp=k-1				!for expmax
	do m=1,k-1
         tau(m)=-1.d3/eigen(m)
	   bm(m)=0.0d0
	   do j=1,kA
		if(plotcur) then
		   bm(m)=bm(m) + dgamma(j)*bmj(m,j)	!for current
		else
		   bm(m)=bm(m) + bmj(m,j)			!for Popen
		endif
	   enddo
	   if(plotcur) then
		w(m)=envd*bm(m)				!for current
	   else
		w(m)=bm(m)					!for Popen
	   endif
	enddo
	thigh=-1.d0	!so set internally
	imode=1		!so texp calculation is skipped
	call EXPMAXd(tpeak,ypeak,yinf,thigh,texp,
     &      .false.,discprt,debug,km,nerr,nerr1,nerr2,imode)

	if(nerr.eq.-5) then
	   monott=.true.
	   ypeak=yinf
	endif
c
	DEALLOCATE(QD,pinf,Amat,eigen,bmj,bm,pt)
	RETURN
	end
