	subroutine setfnt(ifnt)
c To convert convenient font numbering (1-15 at present) into appropriate 
c Gino fonts numbers, and to set the font
c Modified 07/07/97 11:49am by adding common/ginofont/ to return the Gino
c font number, as needed in PARTEXT
	common/ginofont/jfnt
c
	select case(ifnt)
		case(0)
		   jfnt=100
		case(1)
		   jfnt=101
		case(2)
		   jfnt=102
		case(3)
		   jfnt=103
		case(4)
		   jfnt=104
		case(5)
		   jfnt=105
		case(6)
		   jfnt=106
		case(7)
		   jfnt=107
		case(8)
		   jfnt=8
		case(9)
		   jfnt=6
		case(10)
		   jfnt=10
		case(11)
		   jfnt=23
		case(12)
		   jfnt=16
		case(13)
		   jfnt=17
		case(14)
		   jfnt=14
		case(15)
		   jfnt=20
	end select
	call chafnt(jfnt)
	end
