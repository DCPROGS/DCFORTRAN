c============================================================================
	subroutine calcall(ixg,iyg,str,angle,size,xjus,yjus,
     &	ixlo,iylo,ixhi,iyhi,idim,n)
c============================================================================
c	Given N strings STR, calculates idim(N) : dimension of the array
c	occupied by each string, as all as each array coordinates: ixlo(n).....
c============================================================================
	integer*4		ixg(n),iyg(n),ixbox(4),iybox(4)
	integer*4		size(n),xjus(n),yjus(n),angle(n)
	integer*4		ixlo(n),iylo(n),ixhi(n),iyhi(n),idim(n)
	character*(*)	str(n)

	do i=1,n
	    call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &    SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
	    call calcdim(ixbox,iybox,4,ixlo(i),iylo(i),ixhi(i),iyhi(i))
	    idif=ixhi(i)-ixlo(i)
	    if(ixlo(i).lt.0) then
	    	ixlo(i)=0
	    	ixhi(i)=ixlo(i)+idif
	    endif
	    idim(i)=2*((iyhi(i)-iylo(i)+9)*(ixhi(i)-ixlo(i)+9)+1)
	enddo

	end


c============================================================================

