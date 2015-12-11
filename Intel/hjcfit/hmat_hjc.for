	subroutine HMAT_HJC(theta,var,elmax,kfit,badpar,kgood,
     & covar,covlog,varlog,npar,tint,ampl,iprops,nd1,nd2,
     & 	Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & 	XAF,XFA,QexpQA,QexpQF,
     & 	alpha2,beta2,aloglik,kab,
     & 	kAm,kFm,km,x,irt,EXPQF,EXPQA,qt,qd)
	use menu_f90
c
c Modif 21-04-07 ovar etc now allocated in hjcfit (size=npar)
c  Also allocation of info, unit etc was ktd=200 here, now changed to npar,npar
c
c Modification of HMAT2 to do variance matrix in HJCFIT
c Unlike version in ekdist, this expects real*8 theta to contain ONLY the fitted
c parameters (not fixed or constrained or mr).
c Special version needed to pass parameters (from tint onwards) for
c	 HJCLIK(kfit,THETA,tint,ampl,iprops,nd1,nd2,km)
c Also add ktd=dimension of theta in here (previoulsy fixed at 20 or 50)
c
c Modif 11/22/03 06:20pm to calculate, as well as covar(), also covlog() which
c is covariance matrix for log(rates) -in this case can also get CV for MR
c rates
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
c Modified 21-04-07 for case where covar etc are allocated in hjcfit to size k,k
c	real*8 theta(200),covar(200,200),var(200),elmax,delt,one
c	real*8 covlog(200,200),varlog(200)
c	logical badpar(200),bad
	real*8 theta(200),covar(npar,npar),var(npar),elmax,delt,one
	real*8 covlog(npar,npar),varlog(npar)
	logical badpar(npar),bad
c
	real*4 tint(nd1,nd2),ampl(nd1,nd2)
	integer*1 iprops(nd1,nd2)
c
      real*8 QT(100,100),QD(100,100)
	allocatable::finfo,unit
	real*8 finfo(:,:),unit(:,:)
	real*8 det,u
      real*8 EXPQA(20,20),EXPQF(100,100)
c Parameters in call only for transmission to HJCLIK
	real*8 Z00A(kAm,kFm,km),Z10A(kAm,kFm,km),Z11A(kAm,kFm,km)
	real*8 Z00F(kFm,kAm,km),Z10F(kFm,kAm,km),Z11F(kFm,kAm,km)
	real*8 QEXPQA(kFm,kAM),QEXPQF(kAm,kFm)
	real*8 XAF(kAm,kFm,kAm),XFA(kFm,kAm,kFm)
  	real*8 alpha2(kab),beta2(kab),aloglik(kab)
c
	logical discprt,deb
	common/dp/discprt
	integer IQf(100,100),irate1(200),jrate1(200)
	common/QDBLK2/npar0,IQf,irate1,jrate1,nlig   !hjclik, qset_hjc only
	common/tty/ittypanel,itty
	character cnum*22,string*256
c

c
!	ktd=200
	one=1.d0
	deb=.false.	!for now
c	deb=.true.	!for now

	npar0=npar

c	ALLOCATE(finfo(ktd,ktd),unit(ktd,ktd))
	ALLOCATE(finfo(npar,npar),unit(npar,npar))

	delt=dble(x)

c Modified 21-04-07 by adding k to arguments for array dimension
	call VMAT_HJC(theta,delt,finfo,covar,unit,det,badpar,kgood,
     & 	elmax,kfit,tint,ampl,iprops,nd1,nd2,
     & 	Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & 	XAF,XFA,QexpQA,QexpQF,
     & 	alpha2,beta2,aloglik,kab,
     & 	kAm,kFm,km,irt,npar,EXPQF,EXPQA,qt,qd)
c
	if(deb) then
		if(discprt) write(7,14)
14		FORMAT(/,' Observed information matrix= ')
		call ATYPD(FINFO,'        ',kgood,kgood,npar,npar)
	  
3711		FORMAT(/,' Covariance matrix= ')
		call ATYPD(COVAR,'        ',kgood,kgood,npar,npar)
	endif
C
      if(discprt) write(7,46) DET
	write(string,fmt='(a40,g13.6)') 
     &   ' Determinant of INFO matrix=',det
	
	CALL GMSETTEXTSETTING(ITTY,string)
46	FORMAT( ' Determinant of INFO matrix=',G13.6//)
	
	call MATMUL(FINFO,COVAR,UNIT,kgood,kgood,kgood,one,
     &	npar,npar,npar,npar,npar,npar)
c     &	ktd,ktd,ktd,ktd,ktd,ktd)

	bad=.false.
	do i=1,kgood		!IF UNIT O.K. TO 1 IN 1E7 DO NOT PRINT
	   do j=1,kgood
		u=0.0d0
		if(i.eq.j) u=1.0D0
		if(dabs(unit(i,j)-u).gt.1.0d-7) then
		    bad=.true.
		endif
	   enddo
	enddo
	if(bad) call ATYPD(unit,'  unit  ',kgood,kgood,npar,npar)
c
c Now find COV(log(rate)) by multiplying each element of finfo matrix by
c theta(i)*theta(j)
	ii=0
	do i=1,npar
	   if(.not.badpar(i)) then
		ii=ii+1
		jj=0
		do j=1,npar
		   if(.not.badpar(j)) then
			jj=jj+1
			finfo(ii,jj)=finfo(ii,jj)*theta(i)*theta(j)
		   endif
		enddo
	   endif
	enddo
c Invert to get covariance matrix for log(rates)
c	call MATINV2(FINFO,kgood,ktd,COVLOG,ktd,.true.,det,ndscale)
	call MATINV2(FINFO,kgood,npar,COVLOG,npar,.true.,det,ndscale)	
	write(string,fmt='(a40,g13.6)') 
     &   ' Determinant of INFO matrix for log(rate)=',det
	CALL GMSETTEXTSETTING(ITTY,string)
461	FORMAT( ' Determinant of INFO matrix for log(rate) = ',G13.6//)
	i1=0
	do i=1,kgood
	   i1=i1+1
	   var(i)=covar(i1,i1)
	   varlog(i)=covlog(i1,i1)
	enddo
c
999	CONTINUE
	DEALLOCATE(finfo,unit)
c
	RETURN
	END



