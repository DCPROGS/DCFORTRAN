C************************************************************************
	subroutine drawframe(ixlo,iylo,ixhi,iyhi,icolor)

	call hline(ixlo,ixhi,iylo,icolor)
	call hline(ixlo,ixhi,iyhi,icolor)
	call vline(ixlo,iylo,iyhi,icolor)
	call vline(ixhi,iylo,iyhi,icolor)
	end

C************************************************************************
