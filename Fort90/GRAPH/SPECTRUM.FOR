	subroutine spectrum(iset)

c	PALETTE_REGISTER		COLOR MODE 18(r,g,b)    COLOR HGRAPH(r,g,b)
c 		01h			1  (0,0,0)         	1
c		02h			2  (0,0,42)        	2
c		03h			3  (0,42,0)        	3
c		04h			4  (42,0,0)        	4
c		05h			5  (42,0,42)       	5
c		06h			20 (42,21,0)       	20
c           07h			7  (42,42,42)      	7
c		08h			56 (21,21,21)      	56
c		09h			57 (21,21,63)      	9   (0,0,63)
c		10h			58 (21,63,21)      	18  (0,63,0)
c		11h			59 (21,63,63)	 	27  (0,63,63)
c		12h			60 (63,21,21)      	36  (63,0,0)
c		13h			61 (63,21,63)	 	45  (63,0,63)
c		14h			62 (63,63,21)	 	54  (63,63,0)
c		15h			63 (63,63,63)	 	63
c
c	where:
c		r	->    red
c		g	->    green
c		b	->    blue
c		0	->	0%
c		63	->	100%  saturation
c
c	If you want to change the color intensity you can call :
c	SET_RGB(color [18/HGRAPH],r,g,b)
c	for ex: in HGRAPH: SET_RGB(54,63,63,10) ->result a paler yellow
c	      : in MODE 18:SET_RGB(62,63,63,5)  ->result a brighter yellow
C
c	If you already called in MODE 18 SETHG(),which set the correspondence
c	with HGRAPH, you will call:
c		SET_RGB(54,63,63,5)  ->result a paler yellow
c
	select case(iset)
	   case(0)
		call set_palreg(9,9)
		call set_palreg(10,18)
		call set_palreg(11,27)
		call set_palreg(12,36)
		call set_palreg(13,45)
		call set_palreg(14,54)
	   case(1)
		 call change_col(0,0,0,0)
		 call change_col(1,0,0,13)
		 call change_col(2,0,0,26)
		 call change_col(3,0,0,39)
		 call change_col(4,0,0,52)
		 call change_col(5,0,0,63)

		 call change_col(6,0,13,63)
		 call change_col(7,0,26,63)
		 call change_col(8,0,39,63)
		 call change_col(9,0,52,63)
		 call change_col(10,0,63,63)

		 call change_col(11,13,63,63)
		 call change_col(12,26,63,63)
		 call change_col(13,39,63,63)
		 call change_col(14,52,63,63)
		 call change_col(15,63,63,63)


	   case(2)
		 call change_col(0,0,0,0)
		 call change_col(1,0,13,0)
		 call change_col(2,0,26,0)
		 call change_col(3,13,39,13)
		 call change_col(4,0,39,0)
		 call change_col(5,0,52,0)
		 call change_col(6,13,52,13)
		 call change_col(7,26,52,26)
		 call change_col(8,26,63,26)
		 call change_col(9,13,63,13)
		 call change_col(10,0,63,0)

		 call change_col(11,13,63,13)
		 call change_col(12,26,63,26)
		 call change_col(13,39,63,39)
		 call change_col(14,52,63,52)
		 call change_col(15,63,63,63)

	   case(3)
		 call change_col(0,0,0,0)
		 call change_col(1,0,9,0)
		 call change_col(2,0,18,0)
		 call change_col(3,0,27,0)
		 call change_col(4,0,35,0)
		 call change_col(5,0,42,0)
		 call change_col(6,0,49,0)

		 call change_col(7,0,56,0)
		 call change_col(8,0,63,0)
		 call change_col(9,9,63,9)
		 call change_col(10,18,63,18)
		 call change_col(11,27,63,27)
		 call change_col(12,35,63,35)
		 call change_col(13,42,63,42)

		 call change_col(14,56,63,56)
		 call change_col(15,63,63,63)
	   case(4)
		 call change_col(0,0,0,0)
		 call change_col(1,9,0,0)
		 call change_col(2,18,0,0)
		 call change_col(3,27,0,0)
		 call change_col(4,35,0,0)
		 call change_col(5,42,0,0)
		 call change_col(6,49,0,0)

		 call change_col(7,56,0,0)
		 call change_col(8,63,0,0)
		 call change_col(9,63,9,9)
		 call change_col(10,63,18,18)
		 call change_col(11,63,27,27)
		 call change_col(12,63,35,35)
		 call change_col(13,63,42,42)

		 call change_col(14,63,56,56)
		 call change_col(15,63,63,63)
	end select
c

	end


	subroutine change_col(ic,ir,ig,ib)
	call set_rgb(ic,ir,ig,ib)
	call set_palreg(ic,ic)

	end
