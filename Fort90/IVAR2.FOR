	subroutine IVAR2(iy,n,ybar,vary)
c Mean and variance for integer*2 array
	integer*2 iy(n)
c
	sumy=float(iy(1))
	sumyy=0.0

	do i=2,n
	  yi=float(iy(i))
	  fi=float(i)
	  sumy=sumy+yi
	  sumyy=sumyy + (fi*yi-sumy)**2/(fi*(fi-1.))
	enddo

	ybar=sumy/float(n)
	vary=sumyy/float(n-1)
	RETURN
	end


