	subroutine pixgra(ix,iy,xg,yg)
	call papmax(xp,yp)
	call pixpos(ix,iy,xs,ys)
	ys=yp-ys
	call spagra(xs,ys,xg,yg)
      end

	subroutine grapix(xg,yg,ix,iy)

	call graspa(xg,yg,xs,ys)
	call pospix(xs,ys,ix,iy)
	iy=479-iy
      end
