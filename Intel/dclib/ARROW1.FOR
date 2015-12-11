	subroutine ARROW1(x1,y1,x2,y2,alpha,sx,sy,ityp,thik,ic,idev)
c Draws arrow with IV assembler routine ILINE
c Alpha=angle of head (radians) eg 0.6=wide 0.3=narrow
	if(x1.eq.x2) then
		if(y1.le.y2) then
			betha=1.57
		else
			betha=-1.57
		endif
	else
		betha=atan((y2-y1)/(x2-x1))
	endif
	if(x1.le.x2) then
	if(y2.lt.y1) then
		yh2=y2+(2.0*sy*sin(betha+alpha))
		xh2=x2+(2.0*sx*cos(betha+alpha))
		yh1=y2+(2.0*sy*sin(betha-alpha))
		xh1=x2+(2.0*sx*cos(betha-alpha))
	else
		yh2=y2-(2.0*sy*sin(betha+alpha))
		xh2=x2-(2.0*sx*cos(betha+alpha))
		yh1=y2-(2.0*sy*sin(betha-alpha))
		xh1=x2-(2.0*sx*cos(betha-alpha))
	endif
	else
		yh2=y2+(2.0*sy*sin(betha+alpha))
		xh2=x2+(2.0*sx*cos(betha+alpha))
		yh1=y2+(2.0*sy*sin(betha-alpha))
		xh1=x2+(2.0*sx*cos(betha-alpha))
	endif
	call glincols(ic,idev)
	call broken(ityp)
	call linwid(thik)
	call movto2(x1,y1)
	call linto2(x2,y2)
	call linto2(xh2,yh2)
	call linto2(xh1,yh1)
	call linto2(x2,y2)

	end
