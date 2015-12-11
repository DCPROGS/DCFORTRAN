	subroutine QSET_HJC(jset,theta,kfit,k)
c Make a Q matrix for data set #jset from the current set of parameters:
c for new version of HJCFIT that takes multiple data sets.
c QD and QT are made from theta() (leaving fixed values in QT unchanged)
c QT has no concentrations etc, QD() has rates multiplied by appropriate conc
c NB micro rev parameters (and diagonals) are NOT set in QT
c
c QT and QD are in common/qdblk
c conc(i,j) in common/cblk
c
c Modif 08/15/01 11:45am so that fitted values are inserted in QT as well
c as in QD (non-fitted, fixed values should be in QT before the fit,
c and never change)
c
c Modif 08/15/01 09:11am for common/indblk, so when nqset>0 (for indep
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
c=	real*4 concA(10),concB(10)		!to hold concentrations for each expt
	real*4 conc(10,10)
	real*8 QT(100,100),QD(100,100)	!QD has conc in
	integer IQf(100,100),irate1(200),jrate1(200)
	logical discprt,debprt
	common/dp/discprt
	common/deb1/debprt
	integer NSC(50),IM(50,100),JM(50,100),IX(100),JX(100)
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200)
c
	common/CBLK/nset,conc,jsetlast		!for HJCLIK,DISP etc
c=	common/CBLK/nset,concA,concB,jsetlast		!for HJCLIK,DISP etc
	common/QDBLK/QT,QD,npar,IQf,irate1,jrate1,nlig
c==	common/QDBLK/npar,IQf,irate1,jrate1,xA1,xB1,nlig
	common/cpar/ncdep,IX,JX,x
	common/LIG/nligsav,IL(100)
	COMMON/MPAR/NCYC,NSC,IM,JM
	COMMON/indblk/nqset,ieq(200),jeq(200),ifq(200),jfq(200),efacq(200)
c For ec50 constraint
	logical fixec50,prtec50,fixec51
	real*8 ec50,xqlo,xqhi
	common/ec/fixec50,imod0,ec50,i50,j50,m50,prtec50,xqlo,xqhi
	real*8 qval,ec501,ec50out
	common/ec1/fixec51,qval,ec501,ec50out,i501,j501	!for print in SIMPHJC
	real*8 x1,x2,epsx,xout,yout,fx1,fx2,qsav,gfac,pmax,temp
	integer Leq50(5)
c For penalty function
	logical penalty
	real*8 penfunc
	common/penfac/penalty,penfunc 	!for hjclik, qset_hjc
c
	logical debug,caplock,deb
c
	debug()=caplock()
c
	kdim=100			!size of QT, QD
	penalty=.false.
	penfunc=0.0d0
c
c  Fixed parameters are already in QT
c  First insert the kfit estimated param from theta()
c  Copy fixed values to QD (other values overwritten later)
	do i=1,k
	   do j=1,k
		QD(i,j)=QT(i,j)
	   enddo
	enddo
c
c Copy current parameter estimates from theta (still without conc) into QD
	do n=1,kfit
	   i=irate1(n)
	   j=jrate1(n)
	   QT(i,j)=theta(n)	!define QT too
	   QD(i,j)=theta(n)
	enddo
c
c  Next calculate  constrained values (before micro rev) (as in QSETD)
c ALSO now moved to calculate BEFORE conc incorporated, wish is more
c convenient for fixing a binding constant K, by constraining k(-1)=K*k(+1)
	if(neq.gt.0) then
	   do L=1,neq
		QT(ie(L),je(L))=dble(efac(L))*QT(if(L),jf(L))
		QD(ie(L),je(L))=dble(efac(L))*QD(if(L),jf(L))
	   enddo
	endif
c
c Now fix one rate from EC50, if required
c NB Rates are in QD which has NOT yet got conc incorporated, as required
c by MODEC50
c NB if any other parameter is constrained to be a multiple of Q(i50,j50)
c then this parameter must be altered too, before EC50 is calculated!
c NB MODEC50 TAKES RATES FROM QT
	if(fixec50) then
c	   xqlo=0.0d0	!limits for constrained q value (now param)
c	   xqhi=1.d12
	   deb=debug()
	   dlog2=0.693147180559945d0
c	   gfac=2.d0	!factor for initial guesses
	   gfac=10.d0	!factor for initial guesses
	   qsav=QT(i50,j50)	!save original entry (?)
c       Before calculating any ec50, check whether any other rate
c       is constrained to be a multiple of QD(i50,j50)=theta0(m50)
c       (could be more than one, possibly, record them in Leq50()
c	  NB need a complete QD (except for conc) to calculate EC50 -so
c	  need to set MR also each time (don't need diagonals, because
c	  in MODEC50 just need the rate constants which are defined
c	  from the off-diag elements of QD
	   if(neq.gt.0) then
		n=0
		do L=1,neq
		   if(if(L).eq.i50.and.jf(L).eq.j50) then		!found one
			n=n+1
			Leq50(n)=L
		   endif
		enddo
		neq50=n
	   endif
c
	   if(prtec50) then	!print ec50, pmax for input QD
		call SETMR(QT,jset,conc,kdim)
	      call MODEC50(imod0,QT,npar,kdim,pmax,fx1)	!fx1=ec50 for x1
		print 6, 1.d6*fx1,pmax
		if(discprt) write(8,6) 1.d6*fx1,pmax
6		format(' EC50 = ',g13.6,', Max Popen = ',f8.4)
	   endif
	   x1=qsav/gfac
	   x2=qsav*gfac
2	   continue
	   if(x1.gt.x2) then		!ensure x2 > x1
		temp=x1
		x1=x2
		x2=temp
	   endif
c test initial guesses. If they are bad, now use improved strategy to get good
c guesses (a) when func has neg slope: if x1,x2 both give fx>ec50 then guesses
c to low so make new x1= old x2 and new x2=gfac*new x1, and so on -in this case
c can keep gfac constant and x1, x2 should both creep up until they contain
c the answer
	   QT(i50,j50)=x1
	   if(neq50.gt.0) then	!reset others that are constrained
		do n=1,neq50
		   L=Leq50(n)
		   QT(ie(L),je(L))=dble(efac(L))*QT(i50,j50)
		enddo
	   endif
	   call SETMR(QT,jset,conc,kdim)
	   call MODEC50(imod0,QT,npar,kdim,pmax,fx1)	!fx1=ec50 for x1
c
	   QT(i50,j50)=x2
	   if(neq50.gt.0) then	!reset others that are constrained
		do n=1,neq50
		   L=Leq50(n)
		   QT(ie(L),je(L))=dble(efac(L))*QT(i50,j50)
		enddo
	   endif
	   call SETMR(QT,jset,conc,kdim)
	   call MODEC50(imod0,QT,npar,kdim,pmax,fx2)	!fx2=ec50 for x2
	   if((fx1-ec50)*(fx2-ec50).gt.0.0) then		!same sign -guesses bad
		if(fx1.gt.ec50) then		!so fx2>ec50 too
		   if(fx1.gt.fx2) then		!neg slop (problem if not monotonic?)
			x1=x2
			x2=gfac*x2
		   else				!pos slope
			x2=x1
			x1=x1/gfac
		   endif
		else			!fx1 < ec50 (and fx2 < ec50)
		   if(fx1.gt.fx2) then		!neg slop (problem if not monotonic?)
			x2=x1
			x1=x1/gfac
		   else				!pos slope
			x1=x2
			x2=x2*gfac
		   endif
		endif
c		if(gfac.lt.1025.d0) then
c		   gfac=gfac*2.d0
c		   goto 2
		if(x1.gt.xqlo.and.x2.lt.xqhi) then
		   goto 2
		else
		   print 3,1.d6*ec50,x1,1.d6*fx1,x2,1.d6*fx2
		   if(debprt) write(8,3) 1.d6*ec50,x1,1.d6*fx1,x2,1.d6*fx2
3		   format(
     & ' ERROR in QSET_HJC: ec50 should be ',g13.6,' but for',/,
     & '  parameter = ',g13.6, ' ec50 = ',g13.6,' and for',/,
     & '  parameter = ',g13.6, ' ec50 = ',g13.6)
c		   STOP   !just carry on with wrong ec50 and hope it gets back later?
c		If initial guesses don't include result then use whichever of x1 and
c		x2 is the closer, and then skip bisection.  If fx2 nearer then QD
c		is already set, if not then reset using x1
		   if(dabs(fx1-ec50).lt.dabs(fx2-ec50)) then	!x1 nearer
			QT(i50,j50)=x1
			if(neq50.gt.0) then	!reset others that are constrained
			   do n=1,neq50
				L=Leq50(n)
				QT(ie(L),je(L))=dble(efac(L))*QT(i50,j50)
			   enddo
			endif
			call SETMR(QT,jset,conc,kdim)
			fx2=fx1	!so fx2 is the closest to ec50
		   endif
c		Could also try setting
c		a penalty function here (dependent on difference between ec50 that
c		is reuired and the ec50 achieved (if this used, set the penalty to
c		0.00d0 exactly when solution IS found (even though solution has
c		 limited accuracy).  Say penalty is 1 log-likelihood unit for every
c		10% error in ec50
		   penalty=.true.
		   penfunc=10.d0*dabs(ec50-fx2)/ec50
		   print 7, penfunc
		   if(debprt) write(8,7) penfunc
7		   format(' Penalty function = ',f8.3)
		   goto 5
		endif
	   endif
	   if(fx1.ge.ec50) then
		temp=x1
		x1=x2
		x2=temp
	   endif
	   epsx=x1/10000.d0	!accuracy in qd()
	   if(deb) then
		print 31,1.d6*ec50,x1,1.d6*fx1,x2,1.d6*fx2,epsx
31		format(
     &	 ' Initial guesses in QSET_HJC: ec50 should be ',g13.6,/,
     &	 '  x1 = ',g13.6, ' ec50 = ',g13.6,' and for',/,
     &	 '  x2 = ',g13.6, ' ec50 = ',g13.6,/,
     &	 '  epsx = ',g13.6,/,
     &	 ' iter      x1       x2        xout        Yout      Yerr')
	   endif
	   nstep=ifix(sngl(dlog(dabs(x1-x2)/epsx)/dlog2+0.5))
	   do ns=1,nstep
		xout=0.5d0*(x1+x2)
		QT(i50,j50)=xout
c       reset others that are constrained
		if(neq50.gt.0) then	!reset others that are constrained
		   do n=1,neq50
			L=Leq50(n)
			QT(ie(L),je(L))=dble(efac(L))*QT(i50,j50)
		   enddo
		endif
		call SETMR(QT,jset,conc,kdim)
		call MODEC50(imod0,QT,npar,kdim,pmax,Yout)
		Yerr=Yout-ec50			!Yout=calc ec50 for the current theta
		if(deb) then
		   print 4,ns,x1,x2,xout,Yout,Yerr
4		   format(1x,i4,5g13.6)
		endif
		if(Yerr.eq.0.0d0) goto 98	!exact solution
		if(Yerr.lt.0.0d0) then
		   x1=xout
		else
		   x2=xout
		endif
	   enddo
98	   continue
	   if(deb) pause
c	   QD(i50,j50)=theta0(m50)
	   QT(i50,j50)=xout
c
c  An element of QT has been changed so re-calculate MR
	   call SETMR(QT,jset,conc,kdim)
c=	   QD(i50,j50)=xout
	   QD(i50,j50)=QT(i50,j50)	!conc inserted below
	   qsav=xout
c NB this is not one of the free parameters so it is not in theta(),
c only in theta0()
c Save values in common/ec1 for print in simphjc during iterations
	   qval=xout
	   fixec51=fixec50
	   i501=i50
	   j501=j50
	   ec501=1.d6*ec50
	   ec50out=1.d6*yout
5	   continue		!jump here if bisection skipped
	endif
c
c  Could now set V-dep rates as in QSETD, but not in at present
c
c  Now set conc-dep rates AND define QT also, with all constraints
c etc incorporated but without conc
	if(ncdep.gt.0) then
	   do L=1,ncdep
		i=ix(L)
		j=jx(L)
c Define NLIG=number of ligands and IL(i)=ligand type for ith conc-dep
c rate i=1,...,ncdep(needed only if NLIG>1)
		x1=conc(IL(L),jset)
		QD(i,j)=QT(i,j)*dble(x1)
	   enddo
	   jsetlast=jset	!record the conc last used to define QD
	endif
C  Correct rates for KMFAST case -NOT IN FOR NOW
c   Multiply rates out of state 1 by appropriate
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
	call SETMR(QD,jset,conc,kdim)
c NB micro rev parameters (and diagonals) are NOT set in QT
c
c12	continue
c
c For independent models, set all the other Q(i,j) that are
c the same basic npar rate constants
	if(nqset.gt.0) then
	   do L=1,nqset
		QT(ieq(L),jeq(L))=dble(efacq(L))*QT(ifq(L),jfq(L))
		QD(ieq(L),jeq(L))=dble(efacq(L))*QD(ifq(L),jfq(L))
	   enddo
	endif
c
c Lastly set diagonal elements in QD (not set in QT)
	call SETDIAG(QD,k,kdim)
	do i=1,k
	   QT(i,i)=0.d0
	enddo
c
	call CHECKQD(QD,k,jset,npar,nlig,.true.)
c
	RETURN
	end


	subroutine SETDIAG(QD,k,kdim)
	real*8 QD(kdim,kdim)
	do i=1,k
	   QD(i,i)=0.0d0
	   do j=1,k
		if(j.ne.i) then
		   QD(i,i)=QD(i,i)-QD(i,j)
		endif
	   enddo
	enddo
	RETURN
	end

