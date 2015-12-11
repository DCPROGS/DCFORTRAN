c***************************************************************************
	SUBROUTINE SETHG()
c***************************************************************************

c	Subroutine to make the color correspondence with HGRAPH:

c	calling assembly subroutine SET_PALREG(palette_register,color_value) 
c	(ilv15.asm) ,which will set the correspondence of a palette register to 
c	a displayable color;
C	SO IN FACT WHEN WE CALL SET_PIXEL(X,Y,COLOR) ,
C	COLOR MEANS THE PALLETE REGISTER WITH THE DEFAULT CORRESPONDENT COLOR

c	by default MODE 18/HGRAPH has the following correspondence:

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
	call set_palreg(9,9)
	call set_palreg(10,18)
	call set_palreg(11,27)
	call set_palreg(12,36)
	call set_palreg(13,45)
	call set_palreg(14,54)
c

	end
c**************************************************************************
