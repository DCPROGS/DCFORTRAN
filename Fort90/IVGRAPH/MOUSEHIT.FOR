	logical function mousehit()
c
	call get_mouse(ibutton,ix,iy)
	mousehit=ibutton.ne.0
	return
	end

