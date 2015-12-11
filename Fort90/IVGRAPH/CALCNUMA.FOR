c============================================================================
	subroutine calcnumall(ixg,iyg,cnum,angle,size,xjus,yjus,
     &	ixlo,iylo,ixhi,iyhi,idim,n,cexp,inumxy,logxy)
c============================================================================
c	Given CNUM(N) and CEXP(N) , calculates idim(N) : dimension of the array
c	occupied by each number as all as each array coordinates: ixlo(n).....
c============================================================================
	integer*4		ixg(n),iyg(n),ixbox(4),iybox(4)
	integer*4		size(n),xjus(n),yjus(n),angle(n)
	integer*4		ixlo(n),iylo(n),ixhi(n),iyhi(n),idim(n)
	integer 	      charheight,charwidth
	integer*1		inumxy(n),logxy(n)
	character*(*)	cnum(n),cexp(n)

	do i=1,n
	    call FBOX1(IXg(i),IYg(i),cnum(i),ANGLE(i),
     &    SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
	    call calcdim(ixbox,iybox,4,ixlo(i),iylo(i),ixhi(i),iyhi(i))
	   if(logxy(i).ne.0.and.inumxy(i).ne.0) then	!draw superscript
		nl=nblank1(cnum(i))
	      ih=charheight(size(i))
	      iw=charwidth(size(i))
		ixc=ixg(i)+int(nl*iw)
		iyc=iyg(i)+(ih/2)
	      if(inumxy(i).eq.2) iyc=iyg(i)-(ih/2) !draw subscript
	      call FBOX1(IXc,IYc,cexp(i),ANGLE(i),
     &      SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
	      call calcdim(ixbox,iybox,4,ixl,iyl,ixh,iyh)
		ixhi(i)=ixh
		iyhi(i)=iyh
	    endif
	    idif=ixhi(i)-ixlo(i)
	    if(ixlo(i).lt.0) then
	    	ixlo(i)=0
	    	ixhi(i)=ixlo(i)+idif
	    endif
	    idim(i)=2*((iyhi(i)-iylo(i)+9)*(ixhi(i)-ixlo(i)+9)+1)
	enddo

	end


c============================================================================
