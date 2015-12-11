	SUBROUTINE KM2OCC(XA)
	LOGICAL KMFAST
	COMMON/KM2/AKA1,BA,PSTAR(4),KMCON(9),KMFAST,aka2,arat
	logical discprt,pon
	common/dp/discprt
c
	pon=.false.
c
C OCCS FOR KM2 STATES=STATE 1
c	print 2,xa,aka1,aka2,ba
c2	format(' xa,Ka1,ka2,ba= ',4g13.6)
	if(arat.gt.0.) aka2=arat*aka1
	CA1=XA/AKA1
	CA2=XA/AKA2
	PSTAR(4)=1./(1. + 2.*CA1 + CA1*CA2*(1.+BA))	!P(R)
	PSTAR(3)=2.*CA1*PSTAR(4)			!P(AR)
	PSTAR(2)=CA1*CA2*PSTAR(4)			!P(A2R)
	PSTAR(1)=BA*CA1*CA2*PSTAR(4)			!P(A2R*)
c1	FORMAT(' Conc= ',g13.6,
c     & ' Occupancies within state 1 (A2R*,A2R,AR,R)=',/,4F10.6)
	print 1,(PSTAR(I),I=1,4)
	if(pon) write(7,1)(PSTAR(I),I=1,4)
      if(discprt) write(7,1)(PSTAR(I),I=1,4) 
1	FORMAT(
     & ' Occs within state 1 (A2R*,A2R,AR,R)=',4F10.6)
	RETURN
	END



