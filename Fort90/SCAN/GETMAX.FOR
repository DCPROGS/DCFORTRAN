	subroutine GETMAX(data,i1,i2,dmin,dmax,imin,imax)
c To find min and max points between data(i1) and data(i2)
c (also return imin,imax for debugging)
	real*4 data(2048)
c
	dmin=1.e36          !extreme value=minimum
	dmax=-1.e36          !extreme value=maximum
	do i=i1,i2
        if(data(i).lt.dmin) then
		dmin=data(i)
		imin=i
	  endif
        if(data(i).gt.dmax) then
		dmax=data(i)
		imax=i
	   endif
	enddo
c
	RETURN
	end


