	subroutine RANSEED()
	common/rand/ix,iy,iz
c To use clock to produce three random seeds, ix,iy,iz
c NB although ix,iy,iz are integer*4 here, the Wichman-Hill algorithm
c is limited to integer*2, i.e. range -32767 to +32768
c Based on Richard Chandler's RANDGEN.F
c Timer gives number of 10 ms ticks since midnight
c
	real*8 d,B
c
c	B=4.294967291d9
	B=32767.d0
c
	call TIMER(isec)		!ticks past midnight
	ismax=100*60*60*24	!ticks/day
	d=dfloat(isec)/dfloat(ismax)
	d=d*B
	d=dmod(d,B)

	ix=dint(d)
	d=d*3.0269d4
	d=dmod(d,B)
	iy=dint(d)

	d=d*3.0269d4
	d=dmod(d,B)
	iz=dint(d)

	print 1,ix,iy,iz
1	format(3i12)
	RETURN
	end

