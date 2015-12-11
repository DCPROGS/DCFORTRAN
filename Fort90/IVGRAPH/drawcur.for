C************************************************************************
	subroutine drawcur(ix,iy,ixlo,iylo,ixhi,iyhi,icolor,itype,islope)

	if(itype.eq.0) then
		if(islope.eq.0) then 
			call vline(ix,iylo,iyhi,icolor)
		else if(islope.eq.1) then
			call hline(ixlo,ixhi,iy,icolor)
		else
			call hline(ixlo,ixhi,iy,icolor)
			call vline(ix,iylo,iyhi,icolor)
		endif
	endif
	if(itype.eq.1) then
		call hline(ixlo,ixhi,iy,icolor)
		call vline(ix,iylo,iyhi,icolor)
	endif
	end

C************************************************************************
