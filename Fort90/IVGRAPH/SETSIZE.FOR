	subroutine setsize(size)
	call getsize(size,w,h)
	call chasiz(w,h)
	end

	subroutine getsize(size,w,h)
	call vp2enq(xp1,xp2,yp1,yp2,xv1,xv2,yv1,yv2)
	w=abs((xp2-xp1)/(xv2-xv1))*(size+2)
	h=abs((yp2-yp1)/(yv2-yv1))*(size+2)
	end
