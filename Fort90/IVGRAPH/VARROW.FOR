	subroutine VARROW(ix,iy1,iy2,ic)
c To draw vertcal arrow at specified x, using calcs done in prelim
c call to ARROWiv
	common/arriv/il1,il2,il3,il4
c
	ix1=ix	!override value in common
	ix2=ix
	iyh2=iy2-il1
	ixh2=ix2-il2
	iyh1=iy2-il3
	ixh1=ix2-il4
c
c	call iline(ix1,iy1,ix2,iy2,ic)
	call iline(ix1,iy1,ix2,iyh2,ic)	!line does not enter arrow head
	call iline(ix2,iy2,ixh2,iyh2,ic)
	call iline(ixh1,iyh1,ix2,iy2,ic)
	call iline(ixh1,iyh1,ixh2,iyh2,ic)
	RETURN
	end
