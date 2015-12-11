	subroutine write_text(ixc0,iyc0,strings,n,ic,ibk)
	character*(*)	strings(n)

	iyc=iyc0
	do i=1,n
		call wrstring18(ixc0,iyc,strings(i),ic,ibk)
		iyc=iyc-16
	enddo
	end
