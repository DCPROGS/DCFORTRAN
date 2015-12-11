subroutine setlinetype(linetype)
use gino_f90
select case(linetype)
	case(1)
			call gsetbrokenline(gshortdashed)
	case(2)
			call gsetbrokenline(lgshortdotted)
	case(3)
			call gsetbrokenline(gshortchained)
	case(4)
			call gsetbrokenline(glongdashed)
	case(5)
			call gsetbrokenline(glongdotted)
	case(6)
			call gsetbrokenline(glongchained)
	case(7)
		call gsetbrokenline(gdotted)
end select
end