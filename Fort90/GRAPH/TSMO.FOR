	program tsmo
c
	real*4 z(6,6),znew(6,6)
c
	do i=1,6
	   do j=1,6
		z(i,j)=1.0
	   enddo
	enddo
	power=1.0
	call SMOOTH3D(z,znew,6,6,power,6,6)
	end

