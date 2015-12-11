c============================================================================
	subroutine calcnumall(xg,yg,cnum,angle,size,jus,
     &	ixlo,iylo,ixhi,iyhi,idim,n)
c============================================================================
c	Given CNUM(N) and CEXP(N) , calculates idim(N) : dimension of the array
c	occupied by each number as all as each array coordinates: ixlo(n).....
c============================================================================
	real*4		 xg(n), yg(n), xbox(4), ybox(4),angle(n)
	integer*4		jus(n)
	integer*4		ixlo(n),iylo(n),ixhi(n),iyhi(n),idim(n)
	character*(*)	cnum(n)

	do i=1,n
	    call FBOX1(Xg(i),Yg(i),cnum(i),ANGLE(i),
     &    SIZE,JUS(i),xbox,ybox,0)
	    call calcdim(xbox,ybox,4,ixlo(i),iylo(i),ixhi(i),iyhi(i))
	    idif=ixhi(i)-ixlo(i)
	    if(ixlo(i).lt.0) then
	    	ixlo(i)=0
	    	ixhi(i)=ixlo(i)+idif
	    endif
	    idim(i)=2*((iyhi(i)-iylo(i)+9)*(ixhi(i)-ixlo(i)+9)+1)
	enddo

	end


c============================================================================
