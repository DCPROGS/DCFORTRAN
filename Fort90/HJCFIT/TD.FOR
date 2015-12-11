	real*8 d

	do i=1,400
	   d=10.d0**i
c	   print 1,i,d
	   print 1,i,dlog10(d)
1	   format(2x,i5,3x,g13.6)
	enddo
	end


