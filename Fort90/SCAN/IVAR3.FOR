	subroutine IVAR3(iy,n,idx,ybar,vary,ybard,vard)
c Mean and variance for integer*2 array as IVAR2 but also calculates
c mean (ybard) and variance (vard) of 1st derivative
c idx=separation between iy values
	integer*2 iy(n)
	real*4 dydt(2048)
c
c First calculate mean and var of iy()
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
c
c Next calculate dy/dt
	do i=1,n-1
	   d=float(iy(i+1)-iy(i))/float(idx)
	   dydt(i)=d
	enddo
c Next calculate mean and var of dy/dt
	n1=n-1
	sumy=dydt(1)
	sumyy=0.0

	do i=2,n1
	  yi=dydt(i)
	  fi=float(i)
	  sumy=sumy+yi
	  sumyy=sumyy + (fi*yi-sumy)**2/(fi*(fi-1.))
	enddo

	ybard=sumy/float(n1)
	vard=sumyy/float(n1-1)
	RETURN
	end


