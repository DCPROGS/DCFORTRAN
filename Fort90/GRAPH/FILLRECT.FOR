	subroutine FILLRECT(ix1,ix2,iy1,iy2,icol)
	real x(4),y(4)
	common/hgv/ivxlo,ivxhi,ivylo,ivyhi,ixlo,ixhi,iylo,iyhi
c Modif 09/27/93 02:31pm to skip the fill if icol=0 (black)
c  (both because not necessary, and because prog stuck at 'call FILL'
c   when plotting (though not on screen) -whatever icol was -not sure
c   what caused this crash, but this circumvents it for now!)
c To fill a rectangle with solid colour=icol e.g. as background colour
c (uses Lahey colour numbering; see COLTYP1)
c NB This fills graphboundary only, so must reset this if nec
c ix1,iy1=coordinates of bottom left corner (device coordinates)
c ix2,iy2=coordinates of top right corner
	if(icol.eq.0) RETURN    !no need to fill!
	call GRAPHBOUNDARY(ix1,ix2,iy1,iy2)	!reset
	x(1)=xworld(ix1)
	y(1)=yworld(iy1)
	x(2)=xworld(ix1)
	y(2)=yworld(iy2)
	x(3)=xworld(ix2)
	y(3)=yworld(iy2)
	x(4)=xworld(ix2)
	y(4)=yworld(iy1)
c NB icol+icol1=15, except for black
	icol1=15-icol
	if(icol1.eq.15) icol1=-1	!black (for icol=0)
	call FILL(x,y,4,icol1,0,0,1)
	call GRAPHBOUNDARY(ixlo,ixhi,iylo,iyhi)	!restore orig
	RETURN
	end





