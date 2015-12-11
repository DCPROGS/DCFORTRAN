	subroutine DRAWPIX(ix,idata,ndisp,ndata,inc,ipen,ibk,nbuff,nover)
c	Subroutine for fast scrolling backwards
	integer*2 ix(2048)
	integer*2 idata(1-nover:nbuff+nover)      ! ie 1-81920 + overlap extensions
c
	j=ndata
	k=ndata-inc
    	do i=1,ndisp
	   	call rpixel(ix(i),idata(j),ic)
	   	if(ic.eq.ipen) then
			call wpixel(ix(i),idata(j),ibk)	!erase old
	   	endif
		j=j+1
	 	call rpixel(ix(i),idata(k),ic)
	 	if(ic.eq.ibk.and.idata(k).ne.0) then
	   		call wpixel(ix(i),idata(k),ipen)	!draw new
	 	endif
		k=k+1
	end do
	END
