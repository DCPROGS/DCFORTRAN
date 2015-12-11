	subroutine DELPIX4(ix,idata,ndisp,ndata,ipen,ibk,nbuf,novlap,
     &   sfac3,iyoff)
c Version of DELPIX (in IOLIB) to use with AIVPIX4, ie must scale
c data by dividing by 2**nscale, and adding iyoff (without altering idata())
c Analogu	------------------------------------------------------------------
	integer*2 ix(2048),id
	integer*2 idata(1-novlap:nbuf+novlap)

	j=ndata
    	do i=1,ndisp
		id=idata(j)*sfac3 + iyoff
	   	call rpixel(ix(i),id,ic)
	   	if(ic.eq.ipen) then
			call wpixel(ix(i),id,ibk)	!erase old
	   	endif
		j=j+1
	end do
	RETURN
	END


