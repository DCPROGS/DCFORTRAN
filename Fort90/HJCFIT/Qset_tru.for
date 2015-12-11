	subroutine QSET_TRU(QT,theta,jset,conc,npar,irate,jrate,IQ,
     & imode,icons,k)
c Modification of QSET_HJC to generate QT from rates in theta(), and
c set (in both QT and theta) constraints etc for the true rates
c that are used to simulate tint() etc in HJCFIT. Once set these never change
c Add irate,jrate, to arguments, and remove ec50 stuff.
c QT only affected -no conc incorporated and diagonals not set.
c imode=0: m generates QT from theta() at start
c imode=1: Uses input QT and sets constraint in that (and generates theta from QT)
c
c Modif 03/19/02 05:03pm by adding parameter icons
c icons=0: as before
c icons=1: does not apply constraints even when neq>0, but does the rest
c
c Notes for qset_hjc
c Make a Q matrix for data set #jset from the current set of parameters:
c for new version of HJCFIT that takes multiple data sets.
c QD and QT are made from theta() (leaving fixed values in QT unchanged)
c QT has no concentrations etc, QD() has rates multiplied by appropriate conc
c NB micro rev parameters (and diagonals) are NOT set in QT
c
c QT and QD are in common/qdblk
c conc(i,j) in common/cblk
c
c Modif 11/27/01 02:59pm so that if called with k negative, this is signal
c to do everything EXCEPT put concentrations into output QD (and sign of k
c corrected)
c And QT,QD now made arguments (common/qdblk now split into qdblk1 and qdblk2)

c Modif 08/15/01 11:45am so that fitted values are inserted in QT as well
c as in QD (non-fitted, fixed values should be in QT before the fit,
c and never change)
c
c Modif 08/15/01 09:11am for common/indblk, so when nsetq>0 (for indep
c  models only) the rest of Q(i,j) are set at the end (in common with getqd,
c  qdefine and prtmod)
c
c Modif 07/12/00 12:17pm to add constraint by specified EC50
c
c Modif 03/20/00 05:42am Convert theta, simplex hjclik etc to real*8
c
c Modif 03/13/00 04:16pm
c Now calculate constraints BEFORE conc incorporated, wish is more
c convenient for fixing a binding constant K, by constraining k(-1)=K*k(+1)
c
	real*8 theta(200)
	real*8 QT(100,100),qsav,temp
	allocatable QTsav
	real*8 QTsav(:,:)
	integer IQ(100,100)
	integer irate(200),jrate(200)
	logical discprt,debprt
	common/dp/discprt
	common/deb1/debprt
	integer NSC(50),IM(50,100),JM(50,100)
c	integer IX(100),JX(100)
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200)
c
	real*4 conc(10,10)
c	common/CBLK/nset,conc,jsetlast		!for HJCLIK,DISP etc
c	common/cpar/ncdep,IX,JX,x
c	common/LIG/nligsav,IL(100)
	COMMON/MPAR/NCYC,NSC,IM,JM
	COMMON/indblk/nsetq,ieq(200),jeq(200),ifq(200),jfq(200),efacq(200)
c
	logical debug,caplock
c
	debug()=caplock()
c
	kdim=100			!size of QT, QD
c Copy rates from theta (still without conc) into QT
	if(imode.eq.0) then
	   do n=1,npar
		i=irate(n)
		j=jrate(n)
		QT(i,j)=theta(n)	!define QT from theta
	   enddo
	else
	   do n=1,npar
		i=irate(n)
		j=jrate(n)
		m=IQ(i,j)
		theta(m)=QT(i,j)	!define theta from QT
	   enddo
	endif
c  Next calculate  constrained values (before micro rev) (as in QSETD)
c ALSO now moved to calculate BEFORE conc incorporated, wish is more
c convenient for fixing a binding constant K, by constraining k(-1)=K*k(+1)
	if(neq.gt.0.and.icons.eq.0) then
	   do L=1,neq
		i=ie(L)
		j=je(L)
		qsav=QT(i,j)
		if(efac(L).gt.0.) then
		   QT(i,j)=dble(efac(L))*QT(if(L),jf(L))
		else
		   temp=dble(-efac(L)) - QT(if(L),jf(L))
		   if(temp.lt.0.d0) then
			temp=1.d-5	!make sure not neg
			print 10 ,ie(L),je(L),temp
10			format(' Warning: Q(',i2,',',i2,') set negative by',
     &		' constraint -reset to ',g12.5)
		   endif
		   QT(i,j)=temp
		endif
		if(qsav.ne.QT(i,j)) then
		   m=IQ(i,j)
		   theta(m)=QT(i,j)
		   print 2,i,j,m,qsav,QT(i,j)
		   if(discprt) write(8,2) i,j,m,qsav,QT(i,j)
2		   format(' Q(',i2,',',i2,') and param #',i3,
     & 		' changed from ',g13.6,' to ',g13.6,' by constraint')
		endif
	   enddo
	endif
c
C  Next micro rev- recalc those rates that are defined in terms of
c  the others by micro rev. NB already checked that no micro rev param
c  are already constrained, so no need to provide for this (cf QSETD)
c
	allocate(Qtsav(k,k))		!to check for changes in SETMR
	do i=1,k
	   do j=1,k
		QTsav(i,j)=QT(i,j)
	   enddo
	enddo
c
	call SETMR(QT,jset,conc,kdim)
c
	do i=1,k
	   do j=1,k
		if(QTsav(i,j).ne.QT(i,j)) then
		   print 3,i,j,QTsav(i,j),QT(i,j)
		   if(discprt) write(8,3) i,j,QTsav(i,j),QT(i,j)
3		   format(' QT(',i2,',',i2,')',' changed from ',g13.6,
     & 		' to ',g13.6,' by microscopic reversibility')
		endif
	   enddo
	enddo
	deallocate(QTsav)
c
	call CHECKMR(QT)			!print check of MR
c
c NB micro rev parameters (and diagonals) are NOT set in QT
c
c For independent models, set all the other Q(i,j) that are
c the same basic npar rate constants
	if(nsetq.gt.0) then
	   do L=1,nsetq
		i=ieq(L)
		j=jeq(L)
		i1=ifq(L)	!the basic rate constant, to which others set equal
		j1=jfq(L)
		qsav=QT(i,j)
		QT(i,j)=dble(efacq(L))*QT(i1,j1)
		if(qsav.ne.QT(i,j)) then
		   m=IQ(i1,j1)	!IQ() defined only for the basic rate constants
		   theta(m)=QT(i,j)
		   print 4,i,j,m,qsav,QT(i,j)
		   if(discprt) write(8,4) i,j,m,qsav,QT(i,j)
4		   format(
     &	' Q(',i2,',',i2,') and param #',i3,' changed from ',
     & 	 g13.6,' to ',g13.6,' by duplicate rate constraint')
		endif
	   enddo
	endif
c
c	doconc=.true.
c	if(k.lt.0) then
c	   k=-k
c	   doconc=.false.
c	endif
c
c  Could now set V-dep rates as in QSETD, but not in at present
c
c  Now set conc-dep rates AND define QT also, with all constraints
c etc incorporated but without conc
c	if(ncdep.gt.0) then
c	   do L=1,ncdep
c		i=ix(L)
c		j=jx(L)
cc Define NLIG=number of ligands and IL(i)=ligand type for ith conc-dep
cc rate i=1,...,ncdep(needed only if NLIG>1)
c		QT(i,j)=QD(i,j)
c		if(doconc) then
c		   x1=conc(IL(L),jset)
c		   QD(i,j)=QD(i,j)*dble(x1)
c		endif
c	   enddo
c	   jsetlast=jset	!record the conc last used to define QD
c	endif
cC  Correct rates for KMFAST case -NOT IN FOR NOW
cc   Multiply rates out of state 1 by appropriate
c   occupancy to get true rate out of 1
c	if(kmfast) then
c	   do 7 J=2,K		!not 1,K
c	   if(abs(QD(1,J)).LT.1.d-20) GOTO 7	!NO ROUTE
c	   QT(1,J)=QT(1,J)*dble(PSTAR(KMCON(J-1)))
c	   QD(1,J)=QD(1,J)*dble(PSTAR(KMCON(J-1)))
c7	   continue
c	endif
c
C  Next micro rev- recalc those rates that are defined in terms of
c  the others by micro rev. NB already checked that no micro rev param
c  are already constrained, so no need to provide for this (cf QSETD)
c
c Lastly set diagonal elements in QD (not set in QT)
c	call SETDIAG(QD,k,kdim)
c	do i=1,k
c	   QT(i,i)=0.d0
c	enddo
c
	RETURN
	end

