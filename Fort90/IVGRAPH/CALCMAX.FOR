c============================================================================
	subroutine calcmax(ixg,iyg,str,angle,size,xjus,yjus,
     &	n,max,msize)
c============================================================================
c     Given N strings , calculates the max size of an array occupied by the
c	the biggest string
c	returns : max ( max array dimension) coresponding to the string of size,
c	msize! (Not always the string of biggest size occupies the biggest array:
c	depends also of string's length!)
c============================================================================

	integer*4		ixg(n),iyg(n),ixbox(4),iybox(4)
	integer*4		size(n),xjus(n),yjus(n),angle(n)
	character*(*)	str(n)
	max=0
	msize=0
	do i=1,n
	    call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &    SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
	    call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
	    idif=ixhi-ixlo
	    if(ixlo.lt.0) then
	    	ixlo=0
	    	ixhi=ixlo+idif
	    endif
	    idim=2*((iyhi-iylo+9)*(ixhi-ixlo+9)+1)
	    if(i.gt.1) then
		 if(idim.gt.max) then
			max=idim
		      msize=size(i)
		 endif
	    endif
	enddo

	end


c============================================================================

