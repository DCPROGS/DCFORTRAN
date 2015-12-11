	program QSYM
c To calculate steady state of Lauger model (slightly generalised so
c don't assume equilibrium of A,C with outside, and B with inside)
c NB working through Lauger suggests that micro rev must be applied
c to the k(0) values, not the k(V) values as in 1st version of this prog
c (when latter done NEVER get asymmetry) i.e. simple micro rev holds only
c when there is no driving force.
c NB Lauger has opposite sign for potential from that used here and in notes
	real*4 vs,cout,cin
	real*8 Q(10,10),peq(10)
	logical lauger,ivcalc
	logical discprt,prt,dprt
	logical debug,caplock
	character ans*1
	character*11 cdate,ctime
	character*40 mtitle*40,filnam*32,prtport*4      !for WINPRINT
	common/dp/discprt
	common/dpp/filnam,prtport,ndisc,jcol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
c define function
	debug()=caplock()
c
101	format(a1)
	filnam='QSYM.PRT'
	call WINPRINT   !print file control
      OPEN(unit=7,file=prtport)             !open printer
	print 1
	if(discprt) write(8,1)
1       FORMAT(' PROG: TO CALCULATE LAUGER MODEL',/)
	call DATE1(cdate)               !DC subroutine
	call TIME(ctime)
	print 2,cdate,ctime(1:8),mtitle
	if(discprt) write(8,2) cdate,ctime(1:8),mtitle
2       format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 3
3       format(
     & ' SCROLL LOCK on for printing'/
     & ' CAPS LOCK on for debugging')
c
	k=6		!number of states
	km=10
c
	z0=1.0		!charge
c Incorporate the factor of 2 (see Hille) into z to save typing
	z=z0/2.
	ans='Y'
	call DCASK('Calculate I/V',ans,ans)
	ivcalc=ans.eq.'Y'
	Vs=-100.
	V=-100.	!double prec
	dV=20.
	vmax=100.
	if(ivcalc) then
	   print 33,Vs,dv,vmax
33	   format(' V = ',f8.2,',',f8.2,',',f8.2,': O.K. [Y] ')
	   read 101,ans
c	   if(UC(ans).eq.'N') then
c		print 31
c31
	else
	   print 10,Vs
10	   format(' Membrane potential [',f8.2,'] = ')
	   call INPUTr(Vs)
	   if(Vs.ne.-100.) V=dble(Vs)
	   vmax=V	!so done once only
	endif
	V0=0.0
	V0s=0.0	!single prec
	print 34,V0s
34	format(' Reversal potential [',f8.2,'] = ')
	call INPUTr(V0s)
	if(V0s.ne.0.0) V0=dble(V0s)
c
	H=25.4	!RT/F
	u=V/H
	u0=V0/H
	efac=exp(-z0*(u-u0))	!Lauger factor in his eq 32
	d=0.6		!delta
	d1=1.-d
c Concentration
	cout=100.	!mM
	cin=1.	!mM
	print 11,cout,cin
11	format(' Ion conc (mM): out, in [',2f8.2,'] = ')
	call INPUT2r(cout,cin)
	c1=dble(cout*0.001)	!molar
	c2=dble(cin*0.001)	!molar
c Conf change rates (assumed not V-dep)
c NB In Lauger case, if aKAB0 changed from 100 to say 1000, then get asymmetry
c at V=100 mV even with symmetrical conc (c1=c2), but changing aKA21_0 from
c 1.e9 to, say 1.e7, does not cause asymmetry when c1=c2
c===	aKAB0=1000.
	aKAB0=100.
	aKAB1=10.		!micro rev below?
	aKBA0=100.
	aKBA1=10.0
	aKAC0=100.
	aKAC1=10.		!micro rev below?
	aKCA0=100.
	aKCA1=10.
	aKBC0=100.
	aKBC1=10.		!micro rev below?
c Get last 2 from micro rev (as in Lauger eq 33)
	aKCB0=(aKAB0*aKBC0*aKCA0)/(aKBA0*aKAC0)
	aKCB1=(aKAB1*aKBC1*aKCA1)/(aKBA1*aKAC1)
c
c	print 24
c	if(discprt) write(8,24)
c
c Check Lauger case which assumes equilib between A,C with outside only,
c and B with inside only
	print 31
31	format(
     &' Lauger assumption: ',/,
     & ' outside ions are in equilibrium with conformations A and C',/,
     & ' inside ions are in equilibrium with conformation B')
	ans='Y'
	call DCASK(' Use Lauger assumption',ans,ans)
	lauger=ans.eq.'Y'
c	lauger=.true.
c	lauger=.false.
	if(lauger.and.discprt) write(8,31)
c
c Start loop for membrane potential
	prt=.true.
	dprt=discprt
	V1=V
	do V=V1,Vmax,dv
	if(V.gt.V1) then
	   prt=.false.
	   dprt=.false.
	endif
c
	u=V/H
	u0=V0/H
	efac=exp(-z0*(u-u0))	!Lauger factor in his eq 32
c Now binding/unbinding rates
c   define 'hidden' assoc rates for each conformation
	aKA21_0=1.e9
	aKB21_0=1.e9
	if(lauger) aKB21_0=0.0		!Lauger case
	aKC21_0=1.e9
	aKA21_V=aKA21_0*exp(-z*d*(u-u0))
	aKB21_V=aKB21_0*exp(-z*d*(u-u0))
	aKC21_V=aKC21_0*exp(-z*d*(u-u0))
c
	aKA22_0=1.e9
	if(lauger) aKA22_0=0.0		!Lauger case
	aKB22_0=1.e9
	aKC22_0=1.e9
	if(lauger) aKC22_0=0.0		!Lauger case
	aKA22_V=aKA22_0*exp(z*d1*(u-u0))
	aKB22_V=aKB22_0*exp(z*d1*(u-u0))
	aKC22_V=aKC22_0*exp(z*d1*(u-u0))
c and 'hidden dissoc rates
	b=1.d6
	aKA12_0=b
	if(lauger) aKA12_0=0.0       !Lauger case
	aKB12_0=b
	aKC12_0=b
	if(lauger) aKC12_0=0.0		!Lauger case
	aKA12_V=aKA12_0*exp(-z*d1*(u-u0))
	aKB12_V=aKB12_0*exp(-z*d1*(u-u0))
	aKC12_V=aKC12_0*exp(-z*d1*(u-u0))
c
	aKA11_0=b
	aKB11_0=b
	if(lauger) aKB11_0=0.0		!Lauger case
	aKC11_0=b
	aKA11_V=aKA11_0*exp(z*d*(u-u0))
	aKB11_V=aKB11_0*exp(z*d*(u-u0))
	aKC11_V=aKC11_0*exp(z*d*(u-u0))
c and from these get the 'observed' rate constants for Q matrix
c   first assoc rates
	aKA2=c1*aKA21_V + c2*aKA22_V
	aKB2=c1*aKB21_V + c2*aKB22_V
	aKC2=c1*aKC21_V + c2*aKC22_V
c   and then dissoc rates
	aKA1=aKA11_V + aKA12_V
	aKB1=aKB11_V + aKB12_V
	aKC1=aKC11_V + aKC12_V
c   -but better calculate these 3 dissoc rates from micro rev;
c     have 3 micro rev eqns (Lauger eq 32) but they are not indep
c	(1) involves kB1, kA1
c	(2) involves kC1, kA1
c	(3) involves kC1, kB1
c   not clear that these have unique solution -will be easier to
c   get another 3 rates from them eg kAB1, kAC1, kBC1 (which are
c   assumed not to be V-dependent)
c For micro rev calculations, we need the assoc/dissoc rate constants at
c zero driving force so calculate these separately as aKA2_0 etc
c   first assoc rates at zero driving force
	aKA2_0=c1*aKA21_0 + c2*aKA22_0
	aKB2_0=c1*aKB21_0 + c2*aKB22_0
	aKC2_0=c1*aKC21_0 + c2*aKC22_0
c   and then dissoc rates at zero driving force
	aKA1_0=aKA11_0 + aKA12_0
	aKB1_0=aKB11_0 + aKB12_0
	aKC1_0=aKC11_0 + aKC12_0
c and use these values at zero driving force for micro rev calculations:
	aKAB1=(aKBA1*aKA1_0*aKAB0*aKB2_0)/(aKB1_0*aKBA0*aKA2_0)
	aKAC1=(aKCA1*aKA1_0*aKAC0*aKC2_0)/(aKC1_0*aKCA0*aKA2_0)
	aKBC1=(aKCB1*aKB1_0*aKBC0*aKC2_0)/(aKC1_0*aKCB0*aKB2_0)
c
	if(prt) print 24,
     & aKAB1,aKAB1,aKBA0,aKBA1,aKAC0,aKAC1,aKCA0,aKCA1,aKBC0,aKBC1,
     & aKCB0,aKCB1
	if(dprt) write(8,24)
     & aKAB0,aKAB1,aKBA0,aKBA1,aKAC0,aKAC1,aKCA0,aKCA1,aKBC0,aKBC1,
     & aKCB0,aKCB1
24	format(/,
     &' Rate constants for conformation change',/,
     &'   (0=vacant, 1=occ;  *=calc by MR from eq.32)',/,
     &' kAB0, *kAB1 = ',2g13.6,/,
     &' kBA0,  kBA1 = ',2g13.6,/,
     &' kAC0, *kAC1 = ',2g13.6,/,
     &' kCA0,  kCA1 = ',2g13.6,/,
     &' kBC0, *kBC1 = ',2g13.6,/,
     &' kCB0,  kCB1 = ',2g13.6)
	if(prt) print 25,
     & aKA21_0,aKA21_V,aKB21_0,aKB21_V,aKC21_0,aKC21_V
	if(dprt) write(8,25)
     & aKA21_0,aKA21_V,aKB21_0,aKB21_V,aKC21_0,aKC21_V
25	format(/,
     & ' Association rate constants for ion binding from outside',/,
     & '       zero driving force     actual driving force',/,
     & ' kA      ',g13.6,10x,g13.6,/,
     & ' kB      ',g13.6,10x,g13.6,/,
     & ' kC      ',g13.6,10x,g13.6,/)
	if(prt) print 26,
     & aKA22_0,aKA22_V,aKB22_0,aKB22_V,aKC22_0,aKC22_V
	if(dprt) write(8,26)
     & aKA22_0,aKA22_V,aKB22_0,aKB22_V,aKC22_0,aKC22_V
26	format(/,
     & ' Association rate constants for ion binding from inside',/,
     & '       zero driving force     actual driving force',/,
     & ' kA      ',g13.6,10x,g13.6,/,
     & ' kB      ',g13.6,10x,g13.6,/,
     & ' kC      ',g13.6,10x,g13.6,/)
	if(prt) print 27,
     & aKA11_0,aKA11_V,aKB11_0,aKB11_V,aKC11_0,aKC11_V
	if(dprt) write(8,27)
     & aKA11_0,aKA11_V,aKB11_0,aKB11_V,aKC11_0,aKC11_V
27	format(/,
     & ' Dissociation rate constants for ion unbinding to outside',/,
     & '       zero driving force     actual driving force',/,
     & ' kA      ',g13.6,10x,g13.6,/,
     & ' kB      ',g13.6,10x,g13.6,/,
     & ' kC      ',g13.6,10x,g13.6,/)
	if(prt) print 28,
     & aKA12_0,aKA12_V,aKB12_0,aKB12_V,aKC12_0,aKC12_V
	if(dprt) write(8,28)
     & aKA12_0,aKA12_V,aKB12_0,aKB12_V,aKC12_0,aKC12_V
28	format(/,
     & ' Dissociation rate constants for ion unbinding to inside',/,
     & '       zero driving force     actual driving force',/,
     & ' kA      ',g13.6,10x,g13.6,/,
     & ' kB      ',g13.6,10x,g13.6,/,
     & ' kC      ',g13.6,10x,g13.6,/)
	if(prt) print 29,
     & aKA2_0,aKA2,aKB2_0,aKB2,aKC2_0,aKC2
	if(dprt) write(8,29)
     & aKA2_0,aKA2,aKB2_0,aKB2,aKC2_0,aKC2
29	format(/,
     & ' Total association rates for ion binding',/,
     & '       zero driving force     actual driving force',/,
     & ' kA      ',g13.6,10x,g13.6,/,
     & ' kB      ',g13.6,10x,g13.6,/,
     & ' kC      ',g13.6,10x,g13.6,/)
	if(prt) print 30,
     & aKA1_0,aKA1,aKB1_0,aKB1,aKC1_0,aKC1
	if(dprt) write(8,30)
     & aKA1_0,aKA1,aKB1_0,aKB1,aKC1_0,aKC1
30	format(/,
     & ' Total dissociation rate constants for ion binding',/,
     & '       zero driving force     actual driving force',/,
     & ' kA      ',g13.6,10x,g13.6,/,
     & ' kB      ',g13.6,10x,g13.6,/,
     & ' kC      ',g13.6,10x,g13.6,/)
C
c  Now calculate some values to check against MAthcad version (lauger2.mcd)
c  to try to find where discrepancies are!
c   First calc equilib constants for ion binding (conc is included so
c	they are actually ratios of occ/vacant at zero driving force
c	as in Lauger eq 29-31)
	aKA=aKA2_0/aKA1_0
	aKB=aKB2_0/aKB1_0
	aKC=aKC2_0/aKC1_0
c   -and same values when pot=V
	aKA_V=aKA2/aKA1
	aKB_V=aKB2/aKB1
	aKC_V=aKC2/aKC1
c   Calc Lauger eq 32 -with numbers tried so far efAB is same as efac (above)
	if(Lauger) then
	   efAB=(c1*akA21_V*akB12_V*akBA0*akAB1)
	   efAB=efAB/(c2*akB22_V*akA11_V*akAB0*akBA1)
	   if(prt) print 18,efac,efAB
	   if(dprt) write(8,18) efac,efAB
18	   format(/,
     & ' exp(-z*(u-u0)) = ',g13.6,' Check Lauger eq 32 = ',g13.6)
	endif
C===
c  ==end of check calculations
c Now define elements of Q
c   state 1= A0  state 2 = A1
c   state 3= B0  state 4 = B1
c   state 5= C0  state 6 = C1
	call QZERO(Q,k)
c  Ion assoc/dissoc
	q(1,2)=aKA2
	q(2,1)=aKA1
	q(3,4)=aKB2
	q(4,3)=aKB1
	q(5,6)=aKC2
	q(6,5)=aKC1
c  Conf change -unocc
	q(1,3)=aKAB0
	q(3,1)=aKBA0
	q(3,5)=aKBC0
	q(5,3)=aKCB0
	q(5,1)=aKCA0
	q(1,5)=aKAC0
c  Conf change -occ
	q(2,4)=aKAB1
	q(4,2)=aKBA1
	q(4,6)=aKBC1
	q(6,4)=aKCB1
	q(6,2)=aKCA1
	q(2,6)=aKAC1
c Set diagonals
	do i=1,k
	   q(i,i)=0.0d0
	   do j=1,k
		if(i.ne.j) q(i,i)=q(i,i)-q(i,j)
	   enddo
	enddo
c
	if(prt) call DATYP(Q,'    Q  ',.false.,k,k,km,km)		!TYPE Q
c	pause
c Get steady state occupancies
	call EQOCC1(Q,peq,k,km)
c
	if(prt) print 20
	if(dprt) write(8,20)
20	format(/,' Steady-state occupancies',/,
     &'   (1=A0, 2=A*, 3=B0, 4=B*, 5=C0, 6=C*)')
	do i=1,k
	   if(prt) print 22,i,peq(i)
	   if(dprt) write(8,22) i,peq(i)
22	   format(' p(',i2,') = ',g14.7)
	enddo
c
	pA0=peq(1)
	pA1=peq(2)
	pB0=peq(3)
	pB1=peq(4)
c
c Check how close occupancies are to Lauger case which assumes equilib
c between A,C with outside only, and B with inside only
	if(lauger) then
	   pA11=pA0*aKA_V
	   pB11=pB0*aKB_V
c  (with forward rate=1.e9, back=1.e6, these are very close to pA1, pB1
c   respectively -so close to equilib -also provides check on calc
c   of steady state occs with EQOCC)
	   if(prt) print 21,pA11,pB11
	   if(dprt) write(8,21) pA11,pB11
21	   format(/,
     &  ' Approx equilibrium occ of A*, B* under Lauger assumptions',
     &    2g14.7,/)
	endif
c Calc transition rates
	fAB0=pA0*aKAB0
	fAB1=pA1*aKAB1
	fAB=fAB0+fAB1
	fBA0=pB0*aKBA0
	fBA1=pB1*aKBA1
	fBA=fBA0+fBA1
c  Lauger asymmetry index
	rAB=(fAB-fBA)/(fAB+fBA)
c
	if(prt) print 12,V,c1*1.d3,c2*1.d3,d
	if(dprt) write(8,12) V,c1*1.d3,c2*1.d3,d
12	format(/,' Membrane pot (mV) = ',f8.2,/,
     & ' Ion conc (mM): outside, inside = ',2f8.2,/,
     & ' delta = ',f8.3)
	if(prt) print 13,pa0,akAB0,fAB0
	if(dprt) write(8,13) pa0,akAB0,fAB0
13	format(' fAB0 = pA0*kAB0 = ',g14.7,'*',g14.7,' = ',g14.7)
	if(prt) print 14,pa1,akAB1,fAB1
	if(dprt) write(8,14) pa1,akAB1,fAB1
14	format(' fAB1 = pA1*kAB1 = ',g14.7,'*',g14.7,' = ',g14.7)
	if(prt) print 15,pB0,akBA0,fBA0
	if(dprt) write(8,15) pB0,akBA0,fBA0
15	format(' fBA0 = pB0*kBA0 = ',g14.7,'*',g14.7,' = ',g14.7)
	if(prt) print 16,pB1,akBA1,fBA1
	if(dprt) write(8,16) pB1,akBA1,fBA1
16	format(' fBA1 = pB1*kBA1 = ',g14.7,'*',g14.7,' = ',g14.7)
c
	if(ivcalc) then
	   print 32,V,V0,fab,fba,rab
	   if(discprt) write(8,32) V,V0,fab,fba,rab
32	   format(/,' V = ',f8.2,' reversal potential = ',f8.2,/,
     & ' fAB = ',g14.7,' fBA = ',g14.7,/,
     & ' rAB = ',g14.7)
	else
	   print 17,fab,fba,rab
	   if(discprt) write(8,17) fab,fba,rab
17	   format(/,' fAB = ',g14.7,' fBA = ',g14.7,/,
     & ' rAB = ',g14.7)
	endif
c
	enddo		!end of voltage loop
c
	end
