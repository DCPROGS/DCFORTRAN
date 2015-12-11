c============================================================================
	subroutine calcone(ixg,iyg,cnum,angle,size,xjus,yjus,
     &	cexp,inumxy,logxy,ixlo,iylo,ixhi,iyhi,idim)
c============================================================================
c	Given CNUM(N) and CEXP(N) , calculates idim(N) : dimension of the array
c	occupied by each number as all as each array coordinates: ixlo(n).....
c============================================================================
	integer 	      charheight,charwidth
	integer*4		ixbox(4),iybox(4)
	logical		logxy
	    call FBOX1(IXg,IYg,cnum,ANGLE,SIZE,XJUS,YJUS,ixbox,iybox,0)
	    call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
	    if(inumxy.eq.0) goto 1
	    if(logxy) then	!draw superscript
		nl=nblank1(cnum)
	      ih=charheight(size)
	      iw=charwidth(size)
		ixc=ixg+int(nl*iw)
		if(inumxy.eq.1) iyc=iyg+(ih/2)
	      if(inumxy.eq.2) iyc=iyg-(ih/2) !draw subscript
	      call FBOX1(IXc,IYc,cexp,ANGLE,SIZE,XJUS,YJUS,ixbox,iybox,0)
	      call calcdim(ixbox,iybox,4,ixl,iyl,ixh,iyh)
		ixhi=ixh
		iyhi=iyh
	    endif
1	    idif=ixhi-ixlo
	    if(ixlo.lt.0) then
	    	ixlo=0
	    	ixhi=ixlo+idif
	    endif
	    idim=2*((iyhi-iylo+9)*(ixhi-ixlo+9)+1)
	end


c============================================================================
