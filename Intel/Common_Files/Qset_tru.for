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

	real*8 theta(200)
	real*8 QT(100,100),qsav,temp
	allocatable QTsav
	real*8 QTsav(:,:)
	integer IQ(100,100)
	integer irate(200),jrate(200)
	logical discprt
	integer NSC(50),IM(50,100),JM(50,100)

	real*4 conc(10,10)


	common/dp/discprt
	
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200)
	COMMON/indblk/nsetq,ieq(200),jeq(200),ifq(200),jfq(200),efacq(200)


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
c		some warning here????	
10			format(' Warning: Q(',i2,',',i2,') set negative by',
     &		' constraint -reset to ',g12.5)
		   endif
		   QT(i,j)=temp
		endif
		if(qsav.ne.QT(i,j)) then
		   m=IQ(i,j)
		   theta(m)=QT(i,j)
		   
		   if(discprt) write(7,2) i,j,m,qsav,QT(i,j)
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
		   
		   if(discprt) write(7,3) i,j,QTsav(i,j),QT(i,j)
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
		   
		   if(discprt) write(7,4) i,j,m,qsav,QT(i,j)
4		   format(
     &	' Q(',i2,',',i2,') and param #',i3,' changed from ',
     & 	 g13.6,' to ',g13.6,' by duplicate rate constraint')
		endif
	   enddo
	endif

	RETURN
	end

