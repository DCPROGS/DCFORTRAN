	subroutine initdev(idev)
	select case(idev)
		case(0)
			call VGA
			call errswi(-1)
			call brkswi(1)
			call gsetcols(0)
			call mode(18)
		case(4)
			call hplj
			call errswi(-1)
			call brkswi(1)
	end select
	end
