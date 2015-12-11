	subroutine QSETDOLD(XA,XB,IL,V1,QT,QD,K,epsim,
     & ncon,nvdep,ncdep,ncyc,ix,jx,iv,jv,hpar,
     & nsc,im,jm,neq,kmfast,pstar,kmcon,bad)
c Version of QSETD with 10x10 arrays for use in QCONVERT
c All params now arguments because commons in main prog are new large arrays
c
c ***Provision for 2nd ligand (conc XB) added: March 1988.
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
	real*8 QT(10,10),QD(10,10)
	real*8 f,eval
	integer IL(10)		!for ligand type
	logical kmfast,epsim
	real*4 PSTAR(4)
	integer KMCON(9)
	integer IV(10),JV(10)
	real*4 HPAR(10)
	integer IX(10),JX(10)
	integer NSC(8),IM(8,15),JM(8,15)
	logical bad
	logical obeymr
	common/mr/obeymr		!true if microscopic reversibility to be obeyed
C
	bad=.false.
C COPY ALL OFF-DIAG
	DO 210 I=1,K
	DO 211 J=1,K
	IF(J.EQ.I) GOTO 211
	QD(I,J)=QT(I,J)
211	CONTINUE
210	CONTINUE
c
c Not needed in qsetdold
c First calculate  constrained values (before micro rev)
c	if(neq.gt.0) then
c	   do L=1,neq
c		QD(IE(L),JE(L))=dble(EFAC(L))*QD(IF(L),JF(L))
c	   enddo
c	endif
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
	do 5 L=1,ncdep
	I=IX(L)
	J=JX(L)
c Define NLIG=number of ligands and IL(i)=ligand type for ith conc-dep
c rate i=1,...,ncdep(needed only if NLIG>1)
	x1=xa
	if(IL(L).eq.2) x1=xb
	QD(I,J)=QD(I,J)*dble(X1)
5	CONTINUE
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
	if(.not.obeymr) goto 10		!no micro rev constraint
C
	do 8 L=1,NCYC
	IN=-1			!reset default for each cycle
	f=1.0d0
c calc factor
	do m=2,NSC(L)
	   I=IM(L,M)
	   J=JM(L,M)
C omit (unnecessary) micro rev correction if cycle broken at zero conc
	   IF((XA.LT.1.E-20).AND.(QD(I,J).LT.1.d-20)) GOTO 10
	   if(qd(i,j).lt.1.d-30) then
		bad=.true.
		RETURN
	   endif
	   f=f*QD(J,I)/QD(I,J)
	enddo
	I=IM(L,1)	!value to be calc for cycle #L
	J=JM(L,1)
	QD(I,J)=QD(J,I)*f
c end of regular calc
c	if(neq.eq.0) goto 8	!done
c  now check if any of the m.r. rate constants are also constrained-
c if so only the product Q(ie,je)*Q(if,jf) is defined by m.r. if rates
c both point the same way round the cycle.
c If the constrained pair of rates point opposite ways only their ratio
c is defined by m.r. so their ratio cannot also be constrained??
c	do 13 N=1,neq
c	if(I.eq.IE(N).and.J.eq.JE(N)) goto 15	!problem
c	if(I.eq.IF(N).and.J.eq.JF(N)) goto 16	!problem
c	goto 13		!no problem
c15	IN=IF(N)	!indices of constr param that are NOT I,J
c	JN=JF(N)
c	goto 17
c16	IN=IE(N)	!ditto
c	JN=JE(N)
c17	IEN=IE(N)	!indices of the relevant constrained pair
c	JEN=JE(N)
c	IFN=IF(N)
c	JFN=JF(N)
c	EVAL=dble(EFAC(N))
c	goto 12		!jump out when problem found
c13	CONTINUE
c
c12	continue
c	if(IN.lt.0) goto 8		!no problem
cC   Define product f=Q(IE,JE)*Q(IF,JF)=EFAC*Q(IF,JF)**2
c	f=QD(i,j)*QD(IN,JN)
c	QD(IFN,JFN)=dSQRT(f/EVAL)
c	QD(IEN,JEN)=EVAL*QD(IFN,JFN)	!defn of constraint
cC
8	continue	!next cycle
C
C CALC DIAGONAL ELEMENTS
10	continue
	DO 212 I=1,K
	QD(I,I)=0.0d0
	DO 213 J=1,K
	IF(J.EQ.I) GOTO 213
	QD(I,I)=QD(I,I)-QD(I,J)
213	CONTINUE
212	CONTINUE
	RETURN
	END

