	subroutine drawrec(ixlo,iylo,ixhi,iyhi,icolor,icfill)



	call fillwin(ixlo,iylo,ixhi,iyhi,icfill)
	call hline(ixlo,ixhi,iylo,icolor)
	call hline(ixlo,ixhi,iyhi,icolor)
	call vline(ixlo,iylo,iyhi,icolor)
	call vline(ixhi,iylo,iyhi,icolor)
	end
