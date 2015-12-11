	subroutine popq(ixlo,iylo,ixhi,iyhi,question,ic,ibk)

	character*(*)	question
c	print*,ixlo,iylo

      call framefill(ixlo,iylo,ixhi,iyhi,ic,ibk)
	call wrstring18(ixlo+8,iylo+8,question,ic,ibk)

	end
