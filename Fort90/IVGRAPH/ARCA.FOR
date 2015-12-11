	program arca
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

	character*1 	a,getch
	character*11	getint,getreal,mgetint
	character*13       mgetreal
	character*15	helps(30)
	integer*4		ltype(9),lcolor(9)
	integer*4		ix1(3),ix2(3),iy1(3),iy2(3),justx(8),justy(8)
	integer*4		ix(10),iy(10),sizes(10),colors(10)
	integer*4		rotate(10),font(10)
	integer*2         kar(10)
	character*60	texts(10)
	logical mouse_on
	common/mousval/mouse_on,nbutton
	include 'g:\fortran\graph\consts.for'
	nbutton=3
	mouse_on=.true.
	call sethg()
c----------------------------------------

	call INIPLT(0,.false.,1.0)
	call VIEWPORT(0,10000,0,7500)
	call graphboundary(0,10000,0,7500)
	call scale(0.,639.,0.,479.)
	ibk=3
	call clears(0,480,ibk)
	n=3
	lcolor(1)=11
	lcolor(2)=12
	lcolor(3)=14

	ltype(1)=3
	ltype(2)=5
	ltype(3)=4

c	these are for the menu:

	ic=15

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
	justx(5)=center
	justy(5)=center
	rotate(5)=90
	sizes(5)=2
	justx(1)=center
	justy(1)=above
	sizes(1)=1
	justx(2)=center
	justy(2)=below
	justx(3)=right
	justy(3)=above
	justx(4)=right
	justy(4)=center

	do m=1,4
		sizes(m)=m+3
		colors(m)=m+3
	enddo
	rotate(2)=0
	rotate(3)=0
	rotate(4)=0
	rotate(1)=0
c**************************************************************
	k=1

c	draw some lines(insteed of curved lines!):
	do i=1,n
		call ivline(ix1(i),iy1(i),ix2(i),iy2(i),lcolor(i),ltype(i))
	enddo
c	simulate some menu on the screen:
	call fillwin(0,0,640,20,1)
	call wrstring18(1,0,'1.FIX LINE A',14,1)
	call wrstring18(120,0,'2.FIX LINE B',14,1)
	call wrstring18(230,0,'3.FIX LINE C',14,1)
	call wrstring18(340,0,'4.ENTER STRING',14,1)
	call wrstring18(470,0,'5.MOVE STRING',14,1)
	call wrstring18(590,0,'6.QUIT',14,1)

c	press 1/2/3/4/5 or 6 for choices:
99	a=getch(B)
	nr=ichar(a)
	if(nr.eq.27.or.nr.eq.54) then
		goto 999       ! exit
	else if(nr.ge.49.and.nr.le.51) then
	  select case (nr)
		case(49)
			i=1      ! first line
		case(50)
			i=2      ! second line
		case(51)
			i=3      ! third line
	  end select
	  ltyp=ltype(i)
	  call fillwin(0,0,640,20,1)
	  call wrstring18(1,0,'1.FIX LINE COLOR',14,1)
	  call wrstring18(200,0,'2.FIX LINE TYPE',14,1)
	  call wrstring18(400,0,'3.MAIN MENU',14,1)
77	  a=getch(B)
	  nr1=ichar(a)
	  if (nr1.eq.27.or.nr1.eq.51) then     ! exit
	  	call fillwin(0,0,640,20,1)
		call wrstring18(1,0,'1.FIX LINE A',14,1)
		call wrstring18(120,0,'2.FIX LINE B',14,1)
		call wrstring18(230,0,'3.FIX LINE C',14,1)
		call wrstring18(340,0,'4.ENTER STRING',14,1)
		call wrstring18(470,0,'5.MOVE STRING',14,1)
		call wrstring18(590,0,'6.QUIT',14,1)
		goto 99
	  else if(nr1.eq.49.or.nr1.eq.50) then  ! change color/type
	  	call sel_attr(ibk,lcolor(I),ltype(I),nr1)
c		redraw line:
		call ivline(ix1(i),iy1(i),ix2(i),iy2(i),ibk,ltyp)
		call ivline(ix1(i),iy1(i),ix2(i),iy2(i),lcolor(i),ltype(i))
		goto 77
 	  else
		goto 77
	  endif
	else if(nr.eq.52.and.k.lt.10) then      ! enter string
		call ilvfont(ix(k),iy(k),texts(k),rotate(k),sizes(k),
     &	justx(k),justy(k),colors(k),ibk)
		k=k+1
	else if(nr.eq.53.and.k.gt.1) then       ! move string
		call movestr(texts,sizes,k-1,ix,iy,justx,justy,colors,
     &      rotate,font,ibk)
	endif
	goto 99

c====================================================
999	continue
	call ENDPLT
c====================================================


	end


