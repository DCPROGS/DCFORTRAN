	subroutine CKPAR(nmod,vref,theta,ak11,ak12,a1,b1,akc1,
     & akb1,akb2,akb,idest)
	real*8 theta(50)
c (0) Idest=0 on entry: To read in CK rate constants manually into theta
c (1) Idest=1 on entry: To assign theta(1) to ak11m,...,b1 etc,
c	assign and type equilibrium constants
22	format(g13.6)
C BINDING OF FIRST A
	if(idest.eq.1) goto 80
	print 3021,vref
3021	format(' Rate constants (at Vref= ',f6.1,',mV):',/)
	if(nmod.eq.2) goto 20
	print 34
34	FORMAT('&(1) k(+1)= ')
	input 22,theta(1)
	print 33
33	FORMAT('&(2) k(-1) = ')
	input 22,theta(2)
	print 411
411	FORMAT('&(3) alpha= ')
	input 22,theta(3)
	print 421
421	FORMAT('&(4) beta= ')
	input 22,theta(4)
	if(nmod.ne.21) RETURN
	print 2411
	input 22,theta(5)
	print 2421
	input 22,theta(6)
	RETURN
c for nmod=2
20	print 234
234	FORMAT('&(1) beta''= ')
	input 22,theta(1)
	print 233
233	FORMAT('&(2) alpha = ')
	input 22,theta(2)
	print 2411
2411	FORMAT('&(3) k(+B) = ')
	input 22,theta(3)
	print 2421
2421	FORMAT('&(4) k(-B) = ')
	input 22,theta(4)
	RETURN
c
80	continue
	if(nmod.eq.2) goto 81
c do next bit for nmod=1,21
	ak11=theta(1)
	ak12=theta(2)
	AK1=AK12/AK11
	print 51,AK1*1.E6
51	FORMAT( ' K1 (micromolar)= ',G13.6)
	a1=theta(3)
	b1=theta(4)
	AKC1=B1/A1
	if(nmod.ne.21) RETURN
	akb1=theta(5)
	akb2=theta(6)
	AKb=AKb2/AKb1
	print 251,AKb*1.E6
	RETURN
c
c For nmod=2
81	akb1=theta(3)
	akb2=theta(4)
	AKb=AKb2/AKb1
	print 251,AKb*1.E6
251	FORMAT( ' KB = ',G13.6)
	a1=theta(2)
	b1=theta(1)
	AKC1=B1/A1
	RETURN
	end

