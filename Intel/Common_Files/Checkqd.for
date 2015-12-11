	subroutine CHECKQD(iasympPanel,QD,k,jset,npar,nlig,nlvar,withconc,kdim,
     &	itogrec)
c dcmodel=.false.
c To check that a final Q matrix obeys all the micro rev, constraints,
c fixed ec50, voltage dependence etc that it should do if qset_hjc and
c qsetd have worked correctly. Called in qset_hjc
c
c withconc=true if QD matrix has conc in it conc must be removed for ec50 calc)
c
c Modif 10/09/03 04:52pm -nlvar added to paramaters (nlvar2 is in common)
c
	use menu_f90
	character*11 creal,cnum1,cnum2
	CHARACTER*256 string
	allocatable::QT
	real*8 QT(:,:)
c	real*8 QD(100,100)
	real*8 QD(kdim,kdim)
	real*8 qval,qval2,ec5012,ec50out2,
     &	x1s2,x2s2,fx12,fx22
c
      common/ec1/nfixec51,fixec51,qval,ec5011,ec50out,i501,j501,x1s,x2s,
     &fx1,fx2,qval2,ec5012,ec50out2,i5012,j5012,x1s2,x2s2,fx12,fx22  
	integer NSC(50),IM(50,100),JM(50,100),IX(100),JX(100)
	COMMON/MPAR/NCYC,NSC,IM,JM		!for hjclik, checkqd, qset_hjc
	common/cpar/ncdep,IX,JX,x	!for hjclik, checkqd, qset_hjc, hjcdisp
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200) !for hjclik, checkqd, qset_hjc
	real*4 conc(10,10)		!to hold concentration ith ligand, jth set
	common/CBLK/nset,conc,jsetlast	!for HJCLIK, checkqd,qset_hjc,hjcdisp
	COMMON/LIG/nligsav,IL(100)	!for hjclik, checkqd, qset_hjc
c
	real*8 err
c	real*8 prod1,prod2
      logical*4 fastblk
	real*4 aKB
	integer*4 ifb(20),jfb(20),mfb(20),iflig
	common/fblck/fastblk,aKB,nfblock,iflig,ifb,jfb ,mfb
c
	logical discprt,debprt
	common/dp/discprt
	
	common/tty/ittypanel,itty
c
c For ec50 constraint
	logical fixec50,withconc,dcmodel
	real*4 curinfs,curmax,concmax
	real*8 ec50,ec501,xqlo,xqhi,ec50out
	
c Added 10/09/03 11:01am for constraining a 2nd ec50
	real*8 ec502,xqlo2,xqhi2
	real*4 conc_ec1(10),conc_ec2(10),concsav(10),conc1(10,10)
	
	common/ec/fixec50,imod0,ec501,i50,j50,m50,prtec50,xqlo,xqhi,dcmodel
	common/ec4/nfixec50,nlvar2,ec502,i502,j502,m502,
     &     xqlo2,xqhi2,conc_ec1,conc_ec2	 !for 2nd fixed ec50
	
	
	real*8 x1,pmax
c
	logical obeymr(50),automr(50)
	common/mr/obeymr,automr
	integer isetmr(50)
	common/mr1/isetmr
c
	
c
c	debug()=caplock()
c
c	kdim=100			!size of QD	-now argument

c Check constraints
	nligsav=nlig
	do i=1,10
	   do j=1,10
		conc1(i,j)=conc(i,j)
	   enddo
	enddo
c
      allocate(QT(k,k))
c
c NB remove conc from QD with result in QT. This may be necessary for testing
c constraints defined by efac(), as well as for ec50 constraints, e.g. when
c constraining a rate to give specified equilib constant, so QT now defined
c first.
c
	do i=1,k
	   do j=1,k
	      QT(i,j)=QD(i,j)
	   enddo
	enddo
	if(withconc.and.ncdep.gt.0) then
	   do L=1,ncdep
	      i=ix(L)
	      j=jx(L)
c Define NLIG=number of ligands and IL(i)=ligand type for ith conc-dep
c rate i=1,...,ncdep(needed only if NLIG>1)
	      x1=conc(IL(L),jset)
	      QT(i,j)=QD(i,j)/dble(x1)
	   enddo
	endif
c Must also remove conc dep in case of fast block correction
c NB need QT (initially same as QD) on RHS here, in case *x1 already
c removed above.
	if(fastblk) then
	   do n=1,nfblock
		i=ifb(n)
		j=jfb(n)
		x1=dble(conc(iflig,jset)/aKB)
		if(mfb(n).gt.0) then
		   QT(i,j)=QT(i,j)*(1.0d0 + x1)
		else
		   QT(i,j)=QT(i,j)*(1.0d0 + x1)/x1
		endif
	   enddo
	endif
c

	if(neq.gt.0) then
	   do L=1,neq
		if(efac(L).gt.0.) then
		   err=dabs(QD(ie(L),je(L))/QD(if(L),jf(L))-dble(efac(L)))
		   errmax0=1.d-8
		else
c Modeif 05/01/03 07:17am so that when efac is negative q1+q2=-efac
		   err=dabs(QD(ie(L),je(L))+QD(if(L),jf(L))+dble(efac(L)))
		   errmax0=1.d-4
		endif
		if(err.gt.errmax0) then
c		   print 1,L
		call intconv(L,cnum)
	!	imes=gmdisplaymessagebox('','ERROR in constraint '//cnum,gstop,gok)
	WRITE(STRING,fmt='(a30,g13.6)') 'ERROR in constraint ',l
			CALL GMSETTEXTSETTING(ITTY,string) 
		   if(debprt) write(7,1) L
1		   format(' ERROR in constraint ',i3)
c		   pause
		endif
	   enddo
	endif
c
c Check EC50 (NB must remove conc from QD to do this)
	if(fixec50) then
c	 allocate(QT(k,k))
	 do ifix=1,nfixec50
		do i=1,nlig
	      concsav(i)=conc1(i,1)
	   enddo
	   goto 66
		do i=1,k
		do j=1,k
		   QT(i,j)=QD(i,j)
		enddo
	   enddo
	if(withconc.and.ncdep.gt.0) then
		do L=1,ncdep
		   i=ix(L)
		   j=jx(L)
c Define NLIG=number of ligands and IL(i)=ligand type for ith conc-dep
c rate i=1,...,ncdep(needed only if NLIG>1)
		   x1=conc(IL(L),jset)
		   QT(i,j)=QD(i,j)/dble(x1)
		enddo
	   endif
66      continue	   
	   if(ifix.eq.1) then
		nlv=nlvar
		i5=i50
		j5=j50
c		m5=m50
		ec5=ec501
	do i=1,nlig
		   conc1(i,1)=conc_ec1(i)	!conc of other ligands at which ec50 determined
		enddo
	   else if(ifix.eq.2) then
		nlv=nlvar2
		i5=i502
		j5=j502
c		m5=m502
		ec5=ec502
	do i=1,nlig
		   conc1(i,1)=conc_ec2(i)	!conc of other ligands at which ec50 determined
		enddo
	   endif
	   
c Problem is that when required EC50 not achieved next bit is printed
c at every single function evaluation which slows fit -it is now
c printed in SIMPHJC so print here only if debug?
         dcmodel=.false.
         if(itogrec.eq.5) then
	      dcmodel=.true.
	      imod0=29
	   endif
	   if(dcmodel.and.nlig.eq.1) then
c		call MODEC50(imod0,QT,npar,k,pmax,ec501)	!fx1=ec50 for x1
c DC restore as in DOS 
		call MODEC50(imod0,QT,npar,k,pmax,ec50out)	!fx1=ec50 for x1
	   else
		jset1=1
c DC restore as in DOS 
c		call EC50_HJC(ec501,curinfs,curmax,concmax,cur0,pop0,
c     &	  pmax,nlv,QT,jset,conc,k,k,nerr)
		call EC50_HJC(ec50out,curinfs,curmax,concmax,cur0,pop0,
     &	  pmax,nlv,QT,jset,conc,k,k,nerr)

		if(nerr.ne.0) then
c	         call BELL(1)
c		   print 590,nerr
			imes=gmdisplaymessagebox('','Error in bisection for EC50',
     &		gstop,gok)
		   if(discprt) write(7,590) nerr
590		   format('  Error in bisection for EC50: nerr = ',i2)
		endif
	   endif
c DC restore as in DOS
c	   err=dabs(ec501/ec5 - 1.d0)
	   err=dabs(ec50out/ec5 - 1.d0)
	   if(err.gt.5.d-3) then	!error > 0.5 percent

		
	   endif
	    do i=1,nlig		!restore before ifix=2
		conc1(i,1)=concsav(i)
	   enddo
	 enddo	!end of ifix=1,nfixec50
	 
	endif
	deallocate(QT)
c
c Check micro rev
	call CHECKMR(QD)			!print check of MR


c Check V-dependence?

	RETURN
	end


