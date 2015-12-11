	subroutine HMAT_LOG(theta,var,elmax,kfit,badpar,kgood,
     & covar,tint,ampl,iprops,nd1,nd2,
     & 	Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & 	XAF,XFA,QexpQA,QexpQF,
     & 	alpha2,beta2,aloglik,kab,
     & 	kAm,kFm,km,delta,irt,EXPQF,EXPQA,qt,qd)
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
	real*8 Z00A(irt,irt,km),Z10A(irt,irt,km),Z11A(irt,irt,km)
	real*8 Z00F(irt,irt,km),Z10F(irt,irt,km),Z11F(irt,irt,km)
	real*8 QEXPQA(irt,irt),QEXPQF(irt,irt)
	real*8 XAF(irt,irt,irt),XFA(irt,irt,irt)
  	real*8 alpha2(kab),beta2(kab),aloglik(kab)
		CHARACTER CNUM*11,STRING*250
c
	LOGICAL deb
	real*8 EXPQA(20,20),EXPQF(100,100)
c	character*1 ans
	logical discprt,caplock
	common/dp/discprt
	common/QDBLK2/npar,IQf,irate1,jrate1,nlig
c
	real*8 QT(100,100),QD(100,100)
	integer IQf(100,100),irate1(200),jrate1(200)
	common/tty/ittypanel,itty
c
	ktd=200
	deb=.false.		!for now

	ALLOCATE(finfo(ktd,ktd),unit(ktd,ktd))
	delt=dble(delta)
	call VMAT_LOG(theta,delt,finfo,covar,unit,det,badpar,kgood,
     & 	elmax,kfit,tint,ampl,iprops,nd1,nd2,
     & 	Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & 	XAF,XFA,QexpQA,QexpQF,
     & 	alpha2,beta2,aloglik,kab,
     & 	kAm,kFm,km,irt,npar,IQf,irate1,jrate1,nlig,EXPQF,EXPQA,qt,qd)
c
	if(deb) then
c	   print 14
	   if(discprt) write(7,14)
14	   FORMAT(/,' Observed information matrix= ')
	   call ATYPD(FINFO,'        ',kgood,kgood,ktd,ktd)
c	   print 3711
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

      if(discprt) write(7,46) DET
c	print 46,DET
46	FORMAT( ' Determinant of INFO matrix=',G13.6//)
	do i=1,kgood		!IF UNIT O.K. TO 1 IN 1E7 DO NOT PRINT
	   do j=1,kgood
		u=0.0d0
		if(i.eq.j) u=1.0D0
		if(dabs(unit(i,j)-u).gt.1.0d-7) goto 99
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



