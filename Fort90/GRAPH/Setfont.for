C     Last change:  P     2 Apr 97    4:05 pm
	subroutine setfont(ifont)
	select case(ifont)
		case(0)
			ifont1= 0
		case(1)
			call italic(15)
			ifont1= 0
		case(2)
			ifont1= 2
		case(3)
			call italic(15)
			ifont1= 2
		case(4)
			ifont1= 3
		case(5)
			ifont1= 5
		case(6)
			ifont1= 4
		case(7)
			ifont1= 6
		case(8)
			ifont1= 7
		case(9)
			ifont1= 8
		case(10)
			ifont1=10
		case(11)
			call italic(15)
			ifont1=10
	end select
	call setfnt(ifont1)
	end
