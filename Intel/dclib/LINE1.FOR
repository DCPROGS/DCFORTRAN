	subroutine LINE1(x1,y1,x2,y2,ityp,thik,ic)


	call lincols(ic,idev)
	call broken(ityp)
	call linwid(thik)
	call movto2(x1,y1)
	call linto2(x2,y2)
	end
