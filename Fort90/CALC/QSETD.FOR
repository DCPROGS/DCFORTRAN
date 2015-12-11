	subroutine QSETD(conc,IL,V1,QT,QD,K,epsim)
c	subroutine QSETD(XA,XB,IL,V1,QT,QD,K,epsim)

c Modif 09/26/03 09:57am for new version of GETREV, FINDCUBE etc, so that
c order in which cycles are set is dictated by the pointer array, isetmr()
c
c Modif 11/23/01 10:48am so that obeymr now array, and obeymr(i)=F then
c microscopic reversibility check is omitted for cycle #i -so this way can
c have some cycles that do not obey MR and some that do.
c
c Modif 04/10/01 05:44pm for problem with setting rates via micro rev
c   Problem arises when, as here, Q is set starting from QT which has neither
c   diagonals nor m.r. rates in it.  If (as in nic model with singly liganded
c   openings) we determine q(2,1) (A2R* -> AR*) by m.r., this can be done
c   only when agonist conc is non-zero.  AT zero conc, code below detects
c   that cycle is broken, and does not attempt to set q(2,1) which remains
c   zero, as in QT.  However this is a dissociation rate constant and should
c   be the same regardless of agonist conc.  In old version, Q was set
c   usually by removing conc from an existing Q matrix and putting in new
c   conc (in QNEWC routine), so as long as the Q matrix was originally set
c   with a finite conc (cA1, cB1, in old version) the m.r.parameters
c   like q(2,1) would be set then, and not changed here if QSETD called
c   with zero conc that breaks the cycle
c   Thus must put the m.r. values into QT (via call at finite conc) -if
c   QSETD is called again with finite conc this value will be
c   ovewritten, but if QSETD is called with zero conc (and this breaks
c   the cycle) then existing values will be used.  This is better done
c   at the end of GETQD than here,
c
c
c Modified 01/17/01 09:33am for larger arrays, and to use conc(),
c rather then xA, xB
c Modif 02/01/01 09:48am for common/indblk, so when nsetq>0 (for indep
c models only) the rest of Q(i,j) are set at the end.
c
c ***Provision for 2nd ligand (conc XB) added: March 1988.
c
c Modif 01/21/91 09:04am so EPSIM in call -true for EPSC simulation
c in which case rates are not multiplied by concentration here.
c
c Define NLIG=number of ligands and IL(i)=ligand type for ith conc-dep
c rate i=1,...,ncdep(needed only if NLIG>1)
C TO INCORP (1)CONC (XA) AND (2)VDEP INTO QT TO PRODUCE FINAL
C Q MATRIX, QD  (QT IS UNCHANGED)- output in eac case depends
c on input. Also (3) sets constraints (4) sets micro rev param
c (input value overwritten so irrelevant.
c Note that constraint applies to microscopic rate constants
c so it must be applied before effective macroscopic rates
c calc (as Qij*pstar) in KMFAST case. Should also be before
c Vdep correction if constraint is to apply to the values
c of the rate constants at the reference pot (-80 mV at present)
c rather than those at Vkin (will be diff unless Q(ie,je) has
c same Vdep as Q(if,jf))
	real*8 QT(100,100),QD(100,100)
	real*8 f,eval,s,temp
	integer IL(100)		!for ligand type
	real*4 conc(10)
	logical kmfast,epsim
	COMMON/KM2/AKA1,BA,PSTAR(4),KMCON(9),KMFAST,aka2,arat
	COMMON/VPAR/NVDEP,IV(100),JV(100),HPAR(100)
	COMMON/CPAR/NCDEP,IX(100),JX(100),X
	COMMON/QPAR/NCON,IC(2,200)
	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200)
	COMMON/indblk/nsetq,ieq(200),jeq(200),ifq(200),jfq(200),efacq(200)
	logical obeymr(50),automr(50)
	common/mr/obeymr,automr
	integer isetmr(50)
	common/mr1/isetmr
	common/np1/npar
	common/ir/irate(200),jrate(200)
C
C COPY ALL OFF-DIAG
	do i=1,k
	   do j=1,k
		if(i.ne.j) then
		  QD(i,j)=QT(i,j)
		endif
	   enddo
	enddo
c
c First calculate  constrained values (before micro rev)
	if(neq.gt.0) then
	   do L=1,neq
c		QD(IE(L),JE(L))=dble(EFAC(L))*QD(IF(L),JF(L))
		if(efac(L).gt.0.) then
		   QD(ie(L),je(L))=dble(efac(L))*QD(if(L),jf(L))
		else
		   temp=dble(-efac(L)) - QT(if(L),jf(L))
		   if(temp.lt.0.d0) then
			temp=1.d-5	!make sure not neg
			print 11 ,ie(L),je(L),temp
11			format(' Warning: Q(',i2,',',i2,') set negative by',
     &		' constraint -reset to ',g12.5)
		   endif
		   QD(ie(L),je(L))=temp
		endif
	   enddo
	endif
c
c Set V-dep rates
C RESULT DECREASES WITH HYPERPOL (INCREASES WITH V) IF H POSITIVE,
C I.E. RATE GETS SLOWER (TAU LONGER) WITH HYPERPOL
	IF(NVDEP.EQ.0) GOTO 3
	DO 2 L=1,NVDEP
	I=IV(L)
	J=JV(L)
c	print 602,i,j,qd(i,j),hpar(l)
c602	format(' i,j,qd(i,j),hpar(l)= ',2i4,2g13.6)
	QD(I,J)=QD(I,J)*dble(EXP(V1/HPAR(L)))
2	CONTINUE
c
C set conc-dep rates
3	if(ncdep.eq.0.or.EPSIM) goto 4
	do L=1,ncdep
	   i=IX(L)
	   j=JX(L)
c Define NLIG=number of ligands and IL(i)=ligand type for ith conc-dep
c rate i=1,...,ncdep(needed only if NLIG>1)
	   x1=conc(IL(L))
	   QD(i,j)=QD(i,j)*dble(x1)
	enddo
C correct rates for KMFAST case
C IF A KMFAST MODEL MULTIPLY RATES OUT OF STATE 1 BY APPROPRIATE
C OCCUPANCY TO GET TRUE RATE OUT OF 1
4	if(kmfast) then
	   do j=2,k		!^^ not 1,k
		if(dabs(QD(1,j)).ge.1.d-20) then	!route exists
		   QD(1,j)=QD(1,j)*dble(PSTAR(KMCON(j-1)))
		endif
	   enddo
	endif
c
C Now all rates set, recalc those that are defined in terms of
c the others by micro rev
61	if(NCYC.eq.0) goto 10
C
c	do L=1,NCYC
C now set in cycle order specified in isetmr()
	do L1=1,NCYC
	   L=isetmr(L1)	!cycle number
c
	   if(.not.obeymr(L)) goto 8	!no m.r.for this cycle (see GETREV)
	   if(automr(l)) goto 8		!mr set automaticaly -do not change (see GETREV)
c
	   IN=-1			!reset default for each cycle
	   f=1.0d0
c calc factor
	   do m=2,NSC(L)
		I=IM(L,M)
		J=JM(L,M)
C omit (unnecessary) micro rev correction if cycle broken at zero conc
c Is q(i,j) a conc-dependent step? If so, is conc=zero?
		do n=1,ncdep
		   i1=IX(n)
		   j1=JX(n)
		   x1=conc(IL(n))
		   IF((x1.LT.1.E-20).AND.(QD(I1,J1).LT.1.d-20)) GOTO 8   !next cycle
		enddo
		f=f*QD(J,I)/QD(I,J)
	   enddo
	   I=IM(L,1)	!value to be calc for cycle #L
	   J=JM(L,1)
	   QD(I,J)=QD(J,I)*f
c end of regular calc
	   if(neq.gt.0) then
c  now check if any of the m.r. rate constants are also constrained-
c if so only the product Q(ie,je)*Q(if,jf) is defined by m.r. if rates
c both point the same way round the cycle.
c If the constrained pair of rates point opposite ways only their ratio
c is defined by m.r. so their ratio cannot also be constrained??
		do N=1,neq
		   if(I.eq.IE(N).and.J.eq.JE(N)) goto 15	!problem
		   if(I.eq.IF(N).and.J.eq.JF(N)) goto 16	!problem
		   goto 13		!no problem
15		   IN=IF(N)	!indices of constr param that are NOT I,J
		   JN=JF(N)
		   goto 17
16		   IN=IE(N)	!ditto
		   JN=JE(N)
17		   IEN=IE(N)	!indices of the relevant constrained pair
		   JEN=JE(N)
		   IFN=IF(N)
		   JFN=JF(N)
		   EVAL=dble(EFAC(N))
		   goto 12		!jump out when problem found
13 		   CONTINUE
		enddo
c
12		continue
c		if(IN.lt.0) goto 8		!no problem
		if(IN.ge.0) then
C   Define product f=Q(IE,JE)*Q(IF,JF)=EFAC*Q(IF,JF)**2
		   f=QD(i,j)*QD(IN,JN)
		   QD(IFN,JFN)=dSQRT(f/EVAL)
		   QD(IEN,JEN)=EVAL*QD(IFN,JFN)	!defn of constraint
		endif
C
	   endif	!end of MR when neq>0
8	   continue	!next cycle
	enddo		!end of L=1,ncyc
c Check MR is OK
c	call CHECKMR(QD)			!print check of MR
C
10	continue
c Lastly, for independent models, set all the other Q(i,j) that are
c the same basic npar rate constants
	if(nsetq.gt.0) then
	   do L=1,nsetq
		QD(ieq(L),jeq(L))=dble(efacq(L))*QD(ifq(L),jfq(L))
	   enddo
	endif

C CALC DIAGONAL ELEMENTS
c Recalculate the diagonals
	do i=1,k
	   s=0.d0
	   do j=1,k
		if(i.ne.j) s=s+qd(i,j)
	   enddo
	   qd(i,i)=-s
	enddo
c
	RETURN
	END

