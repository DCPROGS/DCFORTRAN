	real function RANDOM()
c	real function RANDOM(ix,iy,iz)
C
C WICHMAN & HILL AS183 algorithm. Use input values output
c from one call to start the next. Cycle length 6.95*10e12 (1000/sec
c =repeat every 220 years)
c	In orig the parameters are in COMMON for speed
	common/rand/ix,iy,iz
c
	ix=171 * mod(ix,177) - 2 *(ix/177)
	iy=172 * mod(iy,176) - 35*(iy/176)
	iz=170 * mod(iz,178) - 63*(iz/178)
c
	if(ix.lt.0) ix=ix + 30269
	if(iy.lt.0) iy=iy + 30307
	if(iz.lt.0) iz=iz + 30323
c
	random=amod(float(ix)/30269.0 + float(iy)/30307.0 +
     & float(iz)/30323.0, 1.0)
	RETURN
	end


