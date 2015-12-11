	subroutine SETMR(QD,jset,conc,kdim)
	integer NSC(50),IM(50,100),JM(50,100),IX(100),JX(100)
	real*4 conc(10,10)
	real*8 fac,QD(100,100)
	common/LIG/nligsav,IL(100)
	common/mpar/NCYC,NSC,IM,JM
	common/cpar/ncdep,IX,JX,x
	logical obeymr(50),automr(50)
	common/mr/obeymr,automr
	integer isetmr(50)
	common/mr1/isetmr
	logical*4 fastblk
	real*4 aKB
	integer*4 ifb(20),jfb(20),mfb(20),iflig
	common/fblck/fastblk,aKB,nfblock,iflig,ifb,jfb,mfb
c
c
	if(NCYC.gt.0) then
c	   do L=1,NCYC
C now set in cycle order specified in isetmr()
	   nmr=0			!check number of rates set by mr
	   do L1=1,NCYC
		L=isetmr(L1)	!cycle number
c		in=-1			!reset default for each cycle
		if(.not.obeymr(L)) goto 12		!skip MR for this cycle (see GETREV)
		if(automr(L)) goto 12		!mr set automaticaly -do not change (see GETREV)
		fac=1.0d0
c calc factor
		do m=2,nsc(L)
		   i=IM(L,m)
		   j=JM(L,m)
C omit (unnecessary) micro rev correction if cycle broken at zero conc
c To do this properly need to check whether the current cycle contains
c a conc dep step (association) and if so, whether the conc of the
c appropriate ligand is zero.
		   do n=1,ncdep
			if(i.eq.ix(n).and.j.eq.jx(n)) then	!q(i,j) is conc dep
			   x1=conc(IL(n),jset)
			   if(x1.lt.1.e-20) goto 12	!next cycle
			endif
		   enddo
		   fac=fac*QD(j,i)/QD(i,j)
		enddo
		i=im(L,1)	!value to be calc for cycle #L
		j=jm(L,1)
c		qsav=QD(i,j)
		QD(i,j)=QD(j,i)*fac
		nmr=nmr+1
c		if(qsav.ne.QD(i,j)) then	!print this only in qset_tru
cc		   m=IQ(i,j)
cc		   thtrue(m)=QT(i,j)
c		   print 3,i,j,qsav,QD(i,j)
c3		   format(' QD(',i2,',',i2,')',' changed from ',g13.6,
c     & 		' to ',g13.6,' by microscopic reversibility')
c		endif

12		continue	!next cycle
	   enddo
c Check MR is OK
	   call CHECKMR(QD)			!print check of MR
	endif
	RETURN
	end
