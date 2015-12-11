c	draw a special cursor for put cursors
	subroutine draw_cursor(ixcur,iycur,icolcur,icur)


c	icur=colcur-8

	call hline(ixcur,ixcur+4,iycur,icur)
	call hline(ixcur,ixcur+4,iycur-7,icur)
	do i=ixcur+1,ixcur+3
		call vline(i,iycur-6,iycur-1,icolcur)
	end do
	call vline(ixcur+4,iycur-7,iycur,icur)
	call vline(ixcur,iycur-7,iycur,icur)

	end
