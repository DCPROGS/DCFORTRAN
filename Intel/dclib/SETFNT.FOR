	subroutine setfnt(ifnt)
c To convert convenient font numbering (1-15 at present) into appropriate
c Gino fonts numbers, and to set the font
c Modified 07/07/97 11:49am by adding common/ginofont/ to return the Gino
c font number, as needed in PARTEXT
	common/ginofont/jfnt
c
	select case(ifnt)
		case(1)
		   jfnt=103
		case(2)
		   jfnt=100
		case(3)
		   jfnt=14
		case(4)
		   jfnt=16
		case(5)
		   jfnt=23
		case(6)
		   jfnt=10
		case(7)
		   jfnt=101
		case(8)
		   jfnt=5
		case(9)
		   jfnt=6
		case(10)
		   jfnt=20
		case(11)
		   jfnt=104
		case(12)
		   jfnt=105
		case(13)
		   jfnt=107
		case(14)
		   jfnt=2
		case(15)
		   jfnt=8
		case(16)
		   jfnt=106
		case(17)
		   jfnt=71
		case(18)
		   jfnt=70
		case(19)
		   jfnt=102
		case(20)
		   jfnt=17
	end select
	call chafnt(jfnt)
	end
