c============================================================================
	subroutine calcmax(xg,yg,str,angle,size,jus,
     &	n,max,sizemax)
c============================================================================
c     Given N strings , calculates the max size of an array occupied by the
c	the biggest string
c	returns : max ( max array dimension) coresponding to the string of size,
c	msize! (Not always the string of biggest size occupies the biggest array:
c	depends also of string's length!)
c============================================================================

	real*4		xg(n),yg(n),xbox(4),ybox(4),angle(n)
	integer*4		jus(n)
	character*(*)	str(n)
	max=0
	sizemax=0
	do i=1,n
	    call FBOX1(Xg(i),Yg(i),STR(i),ANGLE(i),
     &    SIZE,JUS(i),xbox,ybox,0)
	    call calcdim(xbox,ybox,4,ixlo,iylo,ixhi,iyhi)
	    idif=ixhi-ixlo
	    if(ixlo.lt.0) then
	    	ixlo=0
	    	ixhi=ixlo+idif
	    endif
	    idim=2*((iyhi-iylo+9)*(ixhi-ixlo+9)+1)
	    if(i.gt.1) then
		 if(idim.gt.max) then
			max=idim
		 endif
	    endif
	enddo

      sizemax=size
	end


c============================================================================

