	subroutine HMAT_LOG(theta,var,elmax,kfit,badpar,kgood,
     & covar,tint,ampl,iprops,nd1,nd2,
     & 	Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & 	XAF,XFA,QexpQA,QexpQF,
     & 	alpha2,beta2,aloglik,kab,
     & 	kAm,kFm,km)
c
c
c HMAT_LOG is modification of HMAT_HJC to calc errors for log rates directly
c HMAT_HJC is modification of HMAT2 to do variance matrix in HJCFIT
c
c Modification of HMAT2 to do variance matrix in HJCFIT
c Unlike version in ekdist, this expects real*8 theta to contain ONLY the fitted
c parameters (not fixed or constrained or mr).
c Special version needed to pass parameters (from tint onwards) for
c	 HJCLIK(kfit,THETA,tint,ampl,iprops,nd1,nd2,km)
c Also add ktd=dimension of theta in here (previoulsy fixed at 20 or 50)
c
c Modif 10/01/01 02:04pm for 100 states (last 4 lines of parameters are
c there for transmission to HJCLIK called in VMAT_HJC)
c
c 11/13/97 03:56pm Modif throughout for 50 paramters!
c    theta(50),theta(50,5), jfix(50), titlep(5)
c
c 06/01/92 11:11am HMAT2 is version of HMAT1 with dimension of theta etc
c changed from 10 to 20 (and calls VMATD2 accordingly)
c 06/06/89 08:00pm Lahey version. Version of HESMAT for EKDIST
c
	real*8 theta(200),covar(200,200),var(200),elmax,delt
	logical badpar(200)
c
	real*4 tint(nd1,nd2),ampl(nd1,nd2)
	integer*1 iprops(nd1,nd2)
c
	allocatable::finfo,unit
	real*8 finfo(:,:),unit(:,:)
	real*8 det,u,den

c Parameters in call only for transmission to HJCLIK
	real*8 Z00A(kAm,kFm,km),Z10A(kAm,kFm,km),Z11A(kAm,kFm,km)
	real*8 Z00F(kFm,kAm,km),Z10F(kFm,kAm,km),Z11F(kFm,kAm,km)
	real*8 QEXPQA(kFm,kAM),QEXPQF(kAm,kFm)
	real*8 XAF(kAm,kFm,kAm),XFA(kFm,kAm,kFm)
  	real*8 alpha2(kab),beta2(kab),aloglik(kab)
c
	LOGICAL DEBUG
c	character*1 ans
	logical discprt,caplock
	common/dp/discprt
c
	debug()=caplock()
c
	ktd=200
	ALLOCATE(finfo(ktd,ktd),unit(ktd,ktd))
c	delt=1.0d0
	delt=0.1d0
	x=sngl(delt)
	print 31,x
31	format(
     & ' For Hessian use step size that decreases log(lik)'
     & '  by delta [',f6.3,'] = ')
	call INPUTr(x)
	delt=dble(x)
c      if(ans.eq.'N') then
c	   print 11
c11	   FORMAT('&Fraction for calc of Hessian = ')
c	   call INPUTr(fract)
c12	   FORMAT(G13.6)
c	endif
C
	call VMAT_LOG(theta,delt,finfo,covar,unit,det,badpar,kgood,
     & 	elmax,kfit,tint,ampl,iprops,nd1,nd2,
     & 	Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & 	XAF,XFA,QexpQA,QexpQF,
     & 	alpha2,beta2,aloglik,kab,
     & 	kAm,kFm,km)
c
	if(debug()) then
	   print 14
	   if(discprt) write(8,14)
14	   FORMAT(/,' Observed information matrix= ')
	   call ATYPD(FINFO,'        ',kgood,kgood,ktd,ktd)
	   print 3711
3711	   FORMAT(/,' Covariance matrix= ')
	   call ATYPD(COVAR,'        ',kgood,kgood,ktd,ktd)
	endif
	i1=0
	do i=1,kgood
	   i1=i1+1
	   var(i)=covar(i1,i1)
	enddo
C
C CALC CORRELATION MATRIX IN FINFO AND print (WITHOUT CHANGING COVAR)

      if(discprt) write(8,46) DET
	print 46,DET
46	FORMAT( ' Determinant of INFO matrix=',G13.6//)
	do i=1,kgood		!IF UNIT O.K. TO 1 IN 1E9 DO NOT PRINT
	   do j=1,kgood
		u=0.0d0
		if(i.eq.j) u=1.0D0
		if(dabs(unit(i,j)-u).gt.1.0d-9) goto 99
	   enddo
	enddo
	goto 999	!if unit is OK
c
99	call ATYPD(unit,'  unit  ',kgood,kgood,ktd,ktd)
999	CONTINUE
	DEALLOCATE(finfo,unit)
c
	RETURN
	END



