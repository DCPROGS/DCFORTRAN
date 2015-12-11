	real*8 x,y
c
	do i=100,1000
	   x=dfloat(i)
	   y=dexp(-x)
	   print 1,x,y
1	   format(' x, exp(-x) = ',g13.6,2x,g13.6)
	enddo
	end

