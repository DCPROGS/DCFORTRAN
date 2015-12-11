	program polytry

	integer*2	ix(4),iy(4)
	call mode(18)
	ix(1)=100
	iy(1)=100

	ix(2)=120
	iy(2)=200

	ix(3)=200
	iy(3)=190

	ix(4)=210
	iy(4)=110

	call clears(0,480,11)
	n=4
	ic=12
	call fill_poly(ix,iy,carg(n),carg(ic))
	pause
	call draw_poly(ix,iy,n,1)
	pause
	end


