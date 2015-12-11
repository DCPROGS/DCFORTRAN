	subroutine setsize(size)
	w=size
	h=size
	call chasiz(w,h)
	end

	subroutine getsize(size,w,h)
	w=size
	h=size
	end

	subroutine setgsize(size)
	sw=size
	sh=size
	call spagra(sw,sh,w,h)
	call chasiz(w,h)
	end

	subroutine getgsize(size,w,h)
	sw=size
	sh=size
	call spagra(sw,sh,w,h)
	end
