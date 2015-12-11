	real*4 amean(10),area(10)
c
	amean(1)=1.
	amean(2)=10.
	amean(3)=100.
	area(1)=0.3
c	area(2)=0.7
c	ncomp=2
	area(2)=0.3
	area(3)=0.4
	ncomp=3
	kth=0
	call TCRITS(amean,area,ncomp,kth,.false.)
	end

