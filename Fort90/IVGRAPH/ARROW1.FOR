	subroutine ARROW1(ix1,iy1,ix2,iy2,alpha,id,ic)
c Draws arrow with IV assembler routine ILINE
c Alpha=angle of head (radians) eg 0.6=wide 0.3=narrow
c id=length of head
	if(ix1.eq.ix2) then
		if(iy1.le.iy2) then
			betha=1.57
		else
			betha=-1.57
		endif
	else
		betha=atan(float(iy2-iy1)/float(ix2-ix1))
	endif
	if(ix1.le.ix2) then
		iyh2=iy2-int(id*sin(betha+alpha))
		ixh2=ix2-int(id*cos(betha+alpha))
		iyh1=iy2-int(id*sin(betha-alpha))
		ixh1=ix2-int(id*cos(betha-alpha))
	else
		iyh2=iy2+int(id*sin(betha+alpha))
		ixh2=ix2+int(id*cos(betha+alpha))
		iyh1=iy2+int(id*sin(betha-alpha))
		ixh1=ix2+int(id*cos(betha-alpha))
	endif

	call iline(ix1,iy1,ix2,iy2,ic)
	call iline(ix2,iy2,ixh2,iyh2,ic)
	call iline(ixh1,iyh1,ix2,iy2,ic)
	call iline(ixh1,iyh1,ixh2,iyh2,ic)

	end
