	subroutine FRAMEV(ixlo,ixhi,iylo,iyhi,icol)
c To draw simple box with 1 pixel border using IV routines
c ixlo etc in VGA pixel units
c (for shadow border use SHADOW.FOR)
	call HLINE(ixlo,ixhi,iyhi,icol)
	call VLINE(ixhi,iyhi,iylo,icol)
	call HLINE(ixhi,ixlo,iylo,icol)
	call VLINE(ixlo,iylo,iyhi,icol)
	RETURN
	end
