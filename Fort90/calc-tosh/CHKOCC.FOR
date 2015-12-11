	subroutine CHKOCC(Xa,nmod,aka1,aka2,ba)
c For QGEN1 to check occupancies for models with two independent
c channels
	dimension ps(4),p(10)	!local
	logical discprt,pon,slock
	common/dp/discprt
C
	pon()=slock()
c
C OCCS FOR KM2 STATES=STATE 1
c	print 10,xa,aka1,aka2,ba
c10	format(' xa,Ka1,ka2,ba= ',4g13.6)
	if(nmod.ne.31) goto 99
c	if(arat.gt.0.) aka2=arat*aka1
	CA1=XA/AKA1
	CA2=XA/AKA2
	PS(4)=1./(1. + 2.*CA1 + CA1*CA2*(1.+BA))	!P(R)
	PS(3)=2.*CA1*PS(4)			!P(AR)
	PS(2)=CA1*CA2*PS(4)			!P(A2R)
	PS(1)=BA*CA1*CA2*PS(4)			!P(A2R*)
	print 1,(PS(I),I=1,4)
	if(pon()) write(7,1)(PS(I),I=1,4)
      if(discprt) write(8,1)(PS(I),I=1,4) 
1	FORMAT(
     & ' Occs for individual channels',/,' (A2R*,A2R,AR,R)=',4F10.6)
	p(1)=ps(1)*ps(1)
	p(2)=2.0*ps(1)*ps(2)
	p(3)=2.0*ps(1)*ps(3)
	p(4)=2.0*ps(1)*ps(4)
	p(5)=ps(2)*ps(2)
	p(6)=2.0*ps(2)*ps(3)
	p(7)=2.0*ps(2)*ps(4)
	p(8)=ps(3)*ps(3)
	p(9)=2.0*ps(3)*ps(4)
	p(10)=ps(4)*ps(4)
	print 2,(i,p(i),i=1,10)
	if(pon()) write(7,2)(i,p(i),i=1,10)
      if(discprt) write(8,2)(i,p(i),i=1,10) 
2	format(' Occupancies calculated from individual channel occs: ',/,
     & 10(1x,i2,3x,g13.6,/))
99	continue
	RETURN
	END



