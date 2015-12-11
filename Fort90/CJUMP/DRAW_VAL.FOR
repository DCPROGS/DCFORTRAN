	subroutine draw_valve(ix,iy,length,iheigth,iwidth,icvalve,
     &icborder,icbk)

      ixblo=ix+4*iwidth/8
      iyblo=iy+4*iwidth/8
      ixbhi=ixblo+length+iwidth/8
      iybhi=iyblo+iheigth+iwidth/8
	call fillwin(ixblo,iyblo,ixbhi,iybhi,icbk)
	call zbar(ix,iy,length,iheigth,iwidth,icborder,icvalve)

	end
