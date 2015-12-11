c	Subroutine to delete an array of pixels

	subroutine delPIX(ix,idata,ndisp,ndata,ipen,ibk,nbuff,nover)
C	------------------------------------------------------------------
	integer*2 ix(2048)
	integer*2 idata(1-nover:nbuff+nover)      ! ie 1-81920 + overlap extensions

	j=ndata
    	do i=1,ndisp
	   	call rpixel(ix(i),idata(j),ic)
	   	if(ic.eq.ipen) then
			call wpixel(ix(i),idata(j),ibk)	!erase old
	   	endif
		j=j+1
	end do
	END
