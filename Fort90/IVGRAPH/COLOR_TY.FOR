	subroutine color_type(ixlo,iylo,ixhi,iyhi,n,icolor,icfill,
     &			ix_down0,iy_down0,idy,isizex,isizey)

c=====================================================
c	draw the attributes table
c	returns:iy_down0,iy_up0,and idy which we need for sel_attr
c=====================================================
c	PRINT*,IXLO,IYLO,IXHI,IYHI
	if(icolor.eq.icfill) icolor=icfill+8
	call framefill(ixlo,iylo,ixhi,iyhi,icolor,icfill)
	idx=(ixhi-8)-(ixlo+8)+1
	isizex=int(idx/2)
	ix_down0=ixlo+8+isizex
	idy=int(((iyhi-4)-(iylo+4))/n)
	isizey=int(idy-4)/2   !isizey
	iy_down=iylo+5+int(idy/2)
	iy_up=iy_down+int((n-1)*idy)
	iy_down0=iy_down
	do i=1,n
		call drawrec(ix_down0-isizex,iy_up-isizey,ix_down0+isizex,
     &      iy_up+isizey,icfill,i-1)
		iy_up=iy_up-idy
	enddo
	end
