	real function select_size(i,sdim)
	select case(i)
		case(1)
			select_size=8./sdim
		case(2)
			select_size=9./sdim
		case(3)
			select_size=10./sdim
		case(4)
			select_size=11./sdim
		case(5)
			select_size=12./sdim
		case(6)
			select_size=14./sdim
		case(7)
			select_size=16./sdim
		case(8)
			select_size=18./sdim
		case(9)
			select_size=24./sdim
		case(10)
			select_size=32./sdim
		case(11)
			select_size=48./sdim
		case(12)
			select_size=72./sdim
	end select
	
	end