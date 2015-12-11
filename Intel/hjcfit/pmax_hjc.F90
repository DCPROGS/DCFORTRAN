    subroutine pmax_hjc(QT,nlvar,conc,kdim,k,monot,pmax)
! To estimate numerically the maximum popenfor a model specified by QT.
! Assumes QT has NOT got any concentrations in it on entry
! cALC MAX AS IN ec50_HJC but without all the ec50 stuff
	
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 QT(kdim,kdim),pmax
    real*4 conc(10,10),concsav(10,10)
    real*4 xA
    real*8 xad,fac,rel,xa1,xa2,plast,pm
    Allocatable:: QD,pinf
	real*8 QD(:,:),pinf(:)  
	logical monot,flat      
	COMMON/LIG/nlig,IL(100)
	common/KBLK/kA,kB,kC,kD
	common/cpar/ncdep,ix(100),jx(100),x

	ALLOCATE(QD(k,k),pinf(k))
!	deb=.false.		!can set =T under debugger
	km=kdim		!in case QT in call is not 100 x 100
!	monot=.true.	!equilib d/r curve is monotonic vs conc
!  qnew_hjc expects concentration in the conc array so save original
! conc(nlvar,j) changed but j is irrelevant so set to 1
	jset=1
	do i=1,nlig
	   concsav(i,jset)=conc(i,jset)
	enddo
! Find maximum response.  NB use of difference in popen
! between two successive conc could result in convergence when max reached
! -better check by requiring twp successive differences > 0.0001?
	pm=0.0d0	!initial value for plast
	xA=1.0e-9		!start at 1 nM
	xad=1.0d-9
	fac=dsqrt(10.d0)
	rel=1.0d0
	flat=.false.
	do while(.not.flat.and.xAd.lt.100.d0)	!up to 100M if mot flat!
	   plast=pm
	   xA=sngl(xAd)
!2	   conc(nlvar,jset)=xA		!set the conc
	   conc(nlvar,1)=xA		!set the conc
	   call QNEW_HJC(QT,jset,conc,QD,ncdep,nlig,IL,IX,JX,k,km,k)
	   call EQOC_red(QD,pinf,k,k,k)
	   pmax=0.0d0
	   do i=1,kA
		pmax=pmax+pinf(i)
	   enddo
!	   if(deb) then		!repeat calc with specified conc
!   		xA=xA*1.e-6
!           goto 2
!       endif 
	   rellast=rel
	   rel=(pm-plast)/pm
	   if(rel*rellast.lt.-1.d-10) then	!goes through min/max
		monot=.false.			!not monotonic
		xA1=xAd/fac				!conc before max
		xA2=xAd				!conc after max
	   endif
	   flat=dabs(rel).lt.1.d-5.and.dabs(rellast).lt.1.d-5		!0.01% change
	   xAd=xAd*fac
	enddo
! If not monotonic then need now to find Ymax and conc=cmax for Ymax
! Can be done as in ec50_hjc but not in yet
	DEALLOCATE(QD,pinf)
	do i=1,nlig
	   conc(i,1)=concsav(i,1)
	enddo
    return
    end
    