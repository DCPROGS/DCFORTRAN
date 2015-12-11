	real*4 amean(10),area(10),sd(10)
c
	amean(1)=3.
	amean(2)=5.
	amean(3)=6.
	area(1)=0.2
c	area(2)=0.8
c	ncomp=2
	area(2)=0.2
	area(3)=0.6
	ncomp=3
	sd(1)=0.5
	sd(2)=1.0
	sd(3)=0.5
	kth=0
	call ACRITS(amean,area,sd,ncomp,kth,.false.)
	if(ncomp.eq.2) then
	 call ACGAU(Acrit,amean(1),sd(1),area(1),amean(2),sd(2),area(2))
	 print 1,acrit
1	 format(' From ACGAU, acrit = ',g13.6)
	endif
	end

