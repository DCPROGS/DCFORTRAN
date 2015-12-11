	program arcas
c============================================================================
c	Program to test pop-up menus for changing line attributes
c	calling:
c---------------------------------------------------------------------------
c	- attributes(ixlo,iylo,iyhi,strings,n,ic,ibk,title,helps,nhp,lcolor,ltype,nr)
c	ENTER:
c	n:		the number of lines you want to change attributes,
c	lcolor(n):	their color (0/1/2.......15)
c	ltype(n):	their type  (DC Types : 0/1.......6)
c     nr:         nr=ichar(a) :if PRESS '1' ->COLOR TYPE MENU
C					:IF Press '2' ->LINE TYPE MENU
c     EXIT:
c	lcolor(n):	new color (0/1/2.......15)
c	ltype(n):	new type  (DC Types : 0/1.......6)
c
c---------------------------------------------------------------------------
c	- ivline(ixlo,iylo,ixhi,iyhi,icolor,itype)
c	Draws a straight line similar with DC curve line
c
c---------------------------------------------------------------------------
c	-ilvfont(ix(k),iy(k),texts(k),rotate(k),sizes(k),justx,justy,colors(k),ibk)
c	Soubroutine to simulate READSTRING(IX,IY,CAPTION,ANGLE,SIZE)
c	ENTER:
c     ix(k):      x one string
c	iy(k):      y one string
c	rotate(k):  one angle
c     sizes(k):   one size
c     justx:      horizontal justification
c	justy:      vertical justification
c	colors(k):	one color string
c	ibk:        color background
c
c	EXIT:
c	texts(k):  	caption
c     sizes(k):   new size
c	colors(k):	new color string
c
c---------------------------------------------------------------------------
c	-textwin(texts,sizes,m,ix,iy,colors,rotate,ibk)
c	Moves 10 strings
c
c	ENTER :
c	texts(n):  	array of strings on the display(max 10)
c     ix(m):      array x strings
c	iy(m):      array y strings
c	rotate(m):  array angles
c     sizes(m):   array sizes
c     justx:      horizontal justification
c	justy:      vertical justification
c	colors(m):	array color strings
c	ibk:        color background
c
c	EXIT:
c     ix(m):      array new x strings
c	iy(m):      array new y strings
c
c============================================================================

	character*1 	strings(9),a,getch
	character*15	helps(30),title
	integer*4		ltype(9),lcolor(9)
	integer*2		ax(6),ay(6)
	integer*4		ix1(3),ix2(3),iy1(3),iy2(3),justx(8),justy(8)
	integer*4		ix(10),iy(10),sizes(10),colors(10)
	integer*4		rotate(10)
	character*60	texts(10),str
	logical mouse_on
	common/mousval/mouse_on,nbutton
	include 'g:\fortran\graph\consts.for'
	nbutton=2
	mouse_on=.true.
	call sethg()
c----------------------------------------

	call INIPLT(0,.false.,1.0)
	call VIEWPORT(0,10000,0,7500)
	call graphboundary(0,10000,0,7500)
	call scale(0.,639.,0.,479.)
	ibk=3
	call clears(0,480,ibk)
	ax(1)=50
	ay(1)=400
	ax(2)=75
	ay(2)=425
	ax(3)=100
	ay(3)=425
	ax(4)=125
	ay(4)=400
	ax(5)=100
	ay(5)=375
	ax(6)=75
	ay(6)=375
	call fillpoly(ax,ay,6,12,4)
	call ipoly(ax,ay,4,14)
	n=3
	lcolor(1)=11
	lcolor(2)=12
	lcolor(3)=14

	ltype(1)=3
	ltype(2)=5
	ltype(3)=4

c	these are for the menu:

	title='SELECT  LINE'
	strings(1)='1'
	strings(2)='2'
	strings(3)='3'
	ic=15
	ibk=3
	ixlo=300
	iylo=100
	iyhi=-1
	helps(1)='           HELP'
	nhp=1
c     coordinates lines:
	do i=1,3
		ix1(i)=100
		ix2(i)=200
	enddo
	iy1(1)=110
	iy2(1)=110
	iy1(2)=130
	iy2(2)=130
	iy1(3)=150
	iy2(3)=250
c----------------------------------------
c	for text:
	do i=1,5
	justx(i)=base
	justy(i)=base
	enddo
	do m=1,10
		sizes(m)=abs(m-6)
		p=500*m
		ix(m)=int(p)
		iy(m)=int(p)
		colors(m)=m+3
	enddo
	k=1
c**************************************************************
		rotate(1)=90
		rotate(2)=30
		sizes(3)=6
		sizes(4)=3

c	draw some lines(insteed of curved lines!):
	do i=1,3
		call ivline(ix1(i),iy1(i),ix2(i),iy2(i),lcolor(i),ltype(i))
	enddo
c	simulate some menu on the screen:
	call fillwin(0,0,640,20,1)
	call wrstring18(8,0,'1.SELECT NEW COLOR',14,1)
	call wrstring18(190,0,'2.SELECT LINE TYPE',14,1)
	call wrstring18(380,0,'3.ENTER STRING',14,1)
	call wrstring18(520,0,'4.MOVE STRING',14,1)
      call justifystring(3000,6000,title,20,5,base,base)

      call FBOX1(3000,6000,title,20,5,base,base,ixbox,iybox,1)

c	press 1/2/3 or 4 for choices:
99	a=getch(B)
	nr=ichar(a)
	if(nr.eq.27.or.nr.eq.13) then
		goto 999
	else if(nr.eq.49.or.nr.eq.50) then
		call attributes(ixlo,iylo,iyhi,strings,n,ic,ibk,
     &			title,helps,nhp,lcolor,ltype,nr)

		call fillwin(100,100,300,250,ibk)
c		redraw lines:
		do i=1,3
		call ivline(ix1(i),iy1(i),ix2(i),iy2(i),lcolor(i),ltype(i))
		enddo
	else if(nr.eq.51.and.k.lt.10) then
		call ilvfont(ix(k),iy(k),texts(k),rotate(k),sizes(k),
     &	justx,justy,colors(k),ibk)
		k=k+1
	else if(nr.eq.52.and.k.gt.1) then
		call popstr(texts,sizes,k-1,ix,iy,justx,justy,colors,
     &      rotate,ibk)
	endif
	goto 99

c====================================================
999	continue
	call ENDPLT
c====================================================


	end


