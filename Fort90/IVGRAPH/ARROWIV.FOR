	subroutine ARROWiv(ix1,iy1,ix2,iy2,alpha,id,ic)
c Draws arrow with IV assembler routine ILINE
c Alpha=angle of head (radians) eg 0.6=wide 0.3=narrow
c id=length of head
c Puts values in common, which can be used by VARROW to draw a
c vertical arrow which is the same except for x position,
c without recalc of everything
	common/arriv/il1,il2,il3,il4

	if(ix1.eq.ix2) then
		if(iy1.le.iy2) then
			betha=1.57
		else
			betha=-1.57
		endif
	else
		betha=atan(float(iy2-iy1)/float(ix2-ix1))
	endif
c
c Define values (can be used via common, in VARROW etc)
	alength=float(id)
	il1=ifix(alength*sin(betha+alpha))
	il2=ifix(alength*cos(betha+alpha))
	il3=ifix(alength*sin(betha-alpha))
	il4=ifix(alength*cos(betha-alpha))
c
	if(ix1.le.ix2) then
		iyh2=iy2-il1
		ixh2=ix2-il2
		iyh1=iy2-il3
		ixh1=ix2-il4
	else
		iyh2=iy2+il1
		ixh2=ix2+il2
		iyh1=iy2+il3
		ixh1=ix2+il4
	endif
c
	call iline(ix1,iy1,ix2,iy2,ic)
	call iline(ix2,iy2,ixh2,iyh2,ic)
	call iline(ixh1,iyh1,ix2,iy2,ic)
	call iline(ixh1,iyh1,ixh2,iyh2,ic)

	end
