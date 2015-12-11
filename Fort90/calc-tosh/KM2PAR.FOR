	subroutine KM2PAR(readp,nmod,vref,theta,idest)
	real*8 theta(50)
	character*1 ans,UC
	logical readp,chsome
	common/rates/ak11,ak12,ak21,ak22,akb1,akb2,akd1,akd2,
     & a2,b2,alphaB,betaB			!for KM2PAR
	common/equil/ak1,ak2,ba2,bab,akb,akd	!for km2par,main,qdef1
c (0) Idest=0 on entry: To read in KM2 rate constants manually into theta
c (1) Idest=1 on entry: To assign theta(1) to ak11m,...,b1 etc,
c	assign and type equilibrium constants
c
22	format(g13.6)
101	format(a1)
	if(idest.eq.1) goto 80
C BINDING OF FIRST A
	chsome=.false.		!define ALL values
	if(.not.readp) goto 36	!all values must be defined
	print 35
35	format(' Change only specified values [Y] ? ')
	input 101,ans
	if(UC(ans).ne.'N') chsome=.true.
	if(.not.chsome) goto 36		!change all
39	print 37
37	format(' Change parameter number (0 to end)= ')
	input 38,ipar
38	format(i8)
	if(ipar.le.0) RETURN
	goto (1,2,3,4,5,6,7,8,9,10) ipar
36	print 3021,vref
3021	format(' Rate constants (at Vref= ',f6.1,',mV):')
1	print 34
34	FORMAT( ' (1) k(+1)= ')
	input 22,theta(1)
	if(chsome) goto 39
2	print 33
33	FORMAT( ' (2) k(-1) = ')
	input 22,theta(2)
	if(chsome) goto 39
C BINDING OF 2ND A
3	print 325
325	FORMAT( ' (3) k(+2)= ')
	input 22,theta(3)
	if(chsome) goto 39
4	print 326
326	FORMAT( ' (4) k(-2)= ')
	input 22,theta(4)
	if(chsome) goto 39
c Conf change
5	print 411
411	FORMAT( ' (5) alpha= ')
	input 22,theta(5)
	if(chsome) goto 39
6	print 421
421	FORMAT( ' (6) beta= ')
	input 22,theta(6)
	if(chsome) goto 39
c Block
7	if(.not.(nmod.eq.5.or.nmod.eq.7.or.nmod.eq.8.or.
     & nmod.eq.9)) goto 9	!k(+B), k(-B) in mods 5,7,8,9
	print 601
601	FORMAT( ' (7) k(+B)= ')
	input 22,theta(7)
	if(chsome) goto 39
8	print 31
31	FORMAT( ' (8) k(-B)= ')
	input 22,theta(8)
	if(chsome) goto 39
c
c Desens
9	if(.not.(nmod.eq.8)) goto 11	!k(+D), k(-D) in mod 8
	print 602
602	FORMAT( ' (9) k(+d)= ')
	input 22,theta(9)
	if(chsome) goto 39
10	if(.not.(nmod.eq.8)) goto 12	!k(+D), k(-D) in mod 8
	print 603
603	FORMAT( ' (10) k(-d)= ')
	input 22,theta(10)
	if(chsome) goto 39
c
c open/shut when blocked
11	if(.not.(nmod.eq.9)) goto 606	!alpha(B), beta(B) in mod 9
	print 607
607	FORMAT( ' (9) alpha(B)= ')
	input 22,theta(9)
	if(chsome) goto 39
12	if(.not.(nmod.eq.9)) goto 606	!alpha(B), beta(B) in mod 9
	print 604
604	FORMAT( ' (10) beta(B)= ')
	input 22,theta(10)
	if(chsome) goto 39
606	continue
c
999	continue
	RETURN
c
80	continue
	ak11=theta(1)
	ak12=theta(2)
	AK1=AK12/AK11
	print 51,AK1*1.E6
51	FORMAT( ' K1 (micromolar)= ',G13.6)
	ak21=theta(3)
	ak22=theta(4)
	AK2=AK22/AK21
	print 3261,AK2*1.E6
3261	FORMAT( ' K2 (micromolar)= ',G13.6)
c
	a2=theta(5)
	b2=theta(6)
	ba2=b2/a2
	print 610,ba2
610	FORMAT( ' Beta/Alpha= ',G13.6,/)
c
	if(.not.(nmod.eq.5.or.nmod.eq.7.or.nmod.eq.8.or.
     & nmod.eq.9)) goto 609
	akb1=theta(7)
	akb2=theta(8)
	AKB=AKB2/AKB1
	print 611,AKB*1.E6
611	FORMAT( ' KB (micromolar)= ',G13.6,/)
c
609	if(nmod.ne.8) goto 613
	akd1=theta(9)
	akd2=theta(10)
	akd=akd2/akd1
	print 612,AKd
612	FORMAT( ' K (desens)= ',G13.6,/)
c
613	if(nmod.ne.9) goto 614
	alphaB=theta(9)
	betaB=theta(10)
	bab=betaB/alphaB
	print 615,bab
615	FORMAT( ' Beta(B)/Alpha(B)= ',G13.6,/)
614	continue
	RETURN
	end

