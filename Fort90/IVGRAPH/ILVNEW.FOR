      program ilvnew
c============================================================================
c	Program to test pop-up menus for changing line attributes
c	and moving/writing strings for improving  VPLOT
c	calling:
c---------------------------------------------------------------------------
c
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
c     moveone(texts,csizes,sizes,k-1,ix,iy,justx,justy,colors,rotate,font,ibk)
c     subroutine to move ONE string,change its size,change its color,etc
c     the string is selected with the mouse
c	ENTER :
c	texts(n):  	array of strings on the display(max 10)
c     ix(n):      x strings
c	iy(n):      y strings
c	rotate(n):  angles
c     sizes(n):   sizes
c     justx:      horizontal justification
c	justy:      vertical justification
c	colors(n):	color strings
c	font(n)     font strings
c	ibk:        color background
c
c	EXIT:
c     ix(n):      new x strings
c	iy(n):      new y strings
c	rotate(n):  new angles
c     sizes(n):   new sizes
c     justx:      new horizontal justification
c	justy:      new vertical justification
c	colors(n):	new color strings
c	font(n)     new font strings
c
c============================================================================
c	sel_attr(ibk,lcolor(i),ltype(i),nr1)
c	changes line type/ line color for he selected line; in this example,
c     if nr1=49(character '1',change color, else (character '2') ,change type;
c     you can chose other options for nr1!
c============================================================================

c	allmove(outstry,ny,ixgy,iygy,xjusy,yjusy,ycolor,yangle,
c     &	iyfont,ibk,ylet,ystr,csfacy,csizey,logxy,inumy)
c     subroutine to move  ALL the strings outstrx/y,change size,change color,etc
c     with the keyboards.
c
c============================================================================
c	fillwin(ixlo,iylo,ixhi,iyhi,col)
c	wrstring(ix,iy,string,col,bkcol)
c

c============================================================================

	integer*4		ltype(9),lcolor(9)
	integer*4		ix1(3),ix2(3),iy1(3),iy2(3),justx(8),justy(8)
	integer*4		ix(10),iy(10),sizes(10),colors(10)
	integer*4		rotate(10),font(10)
	integer*4		ixgx(10),iygx(10),xjusx(10),yjusx(10)
      integer*4         xcolor(10),xangle(10)
	integer*4		ixgy(10),iygy(10),xjusy(10),yjusy(10)
      integer*4         ycolor(10),yangle(10)
	logical           mouse_on,logxy
	character*1 	a,getch
	character*3		xstr(10),ystr(10),cexp(10),ylet(10)
	character*60	texts(10)
	character*10	outstrx(10),outstry(10)
	common/mousval/mouse_on,nbutton
	include 'g:\fortran\graph\consts.for'

c**************************************************************
	nbutton=3
	mouse_on=.true.
	call sethg()

c	for numbers (axis) :

	nx=10
	ny=10
	do i=1,10
		xcolor(i)=8
		ycolor(i)=8
		xjusx(i)=base
		xjusy(i)=base
		yjusx(i)=base
		yjusy(i)=base
		ixgx(i)=1000+int(700*i)
		iygx(i)=1000
		ixgy(i)=900
		iygy(i)=1100+int(600*i)
      enddo

	call setfont(ixfont)
	csfacx=1
	csizex=3
	sizex=csfacx*csizex
	ixfont=bold

	call setfont(iyfont)
	csfacy=1
	csizey=3
	sizey=csfacy*csizey
	iyfont=bold


	cexp(1)='1'
	cexp(2)='2'
	cexp(3)='3'
	cexp(4)='4'
	cexp(5)='5'
	cexp(6)='6'
	cexp(7)='7'
	cexp(8)='8'
	cexp(9)='9'
	cexp(10)='10'
	xstr(1)='10'
	xstr(2)='20'
	xstr(3)='30'
	xstr(4)='40'
	xstr(5)='50'
	xstr(6)='60'
	xstr(7)='70'
	xstr(8)='80'
	xstr(9)='90'
	xstr(10)='100'
	ystr(1)='0.1'
	ystr(2)='0.2'
	ystr(3)='0.3'
	ystr(4)='0.4'
	ystr(5)='0.5'
	ystr(6)='0.6'
	ystr(7)='0.7'
	ystr(8)='0.8'
	ystr(9)='0.9'
	ystr(10)='1.0'
	do i=1,10
		ylet(i)='A'
	enddo
c**************************************************************
c	for lines:

	n=3
	lcolor(1)=11
	lcolor(2)=12
	lcolor(3)=14

	ltype(1)=3
	ltype(2)=5
	ltype(3)=4

c     coordinates lines:
	do i=1,3
		ix1(i)=300
		ix2(i)=400
	enddo
	iy1(1)=210
	iy2(1)=210
	iy1(2)=230
	iy2(2)=230
	iy1(3)=250
	iy2(3)=350

c**************************************************************
c	for text:

	texts(1)='x axis'
	texts(2)='y axis'
	font(1)=1
	justx(1)=base
	justy(1)=base
	sizes(1)=3
	rotate(1)=0
	colors(1)=15

	ix(1)=5000
	iy(1)=800

	font(2)=2
	justx(2)=base
	justy(2)=base
	sizes(2)=3
	rotate(2)=90
	colors(2)=15
	ix(2)=800
	iy(2)=4200

	justx(3)=right
	justy(3)=above
	font(3)=3
	rotate(3)=0
	sizes(3)=4
	colors(3)=12

	justx(4)=right
	justy(4)=center
	font(4)=4
	rotate(4)=0
	sizes(4)=4
	colors(4)=13

	justx(5)=center
	justy(5)=center
	rotate(5)=90
	sizes(5)=2
	font(5)=5
	rotate(5)=0
	colors(5)=1
c**************************************************************
	call INIPLT(0,.false.,1.0)
	call VIEWPORT(0,10000,0,7500)
	ibk=3
	call clears(0,480,ibk)

c     index for strings ( we already have 2):
	k=3
	logxy=.true.
	inumx=1
	inumy=2

c	draw some lines(insteed of curved lines!):
	do i=1,n
		call ivline(ix1(i),iy1(i),ix2(i),iy2(i),lcolor(i),ltype(i))
	enddo
c	simulate some menu on the screen:
	call fillwin(0,0,640,20,1)
	call wrstring18(1,0,'1.LINE A',14,1)
	call wrstring18(80,0,'2.LINE B',14,1)
	call wrstring18(160,0,'3.LINE C',14,1)
	call wrstring18(240,0,'4.ENTER TEXT',14,1)
	call wrstring18(360,0,'5.MOVE TEXT',14,1)
	call wrstring18(590,0,'7.QUIT',14,1)
	call wrstring18(464,0,'6.FIX NUMBERS',14,1)

c	draw axis:
	call hline(100,600,100,1)
	call vline(100,100,450,1)
	call coltyp18(colors(1))
	call setfont1(font(1))
      call justifystring(ix(1),iy(1),texts(1),
     & rotate(1),sizes(1),justx(1),justy(1))
	call setfont1(font(2))
	call coltyp18(colors(2))
      call justifystring(ix(2),iy(2),texts(2),
     & rotate(2),sizes(2),justx(2),justy(2))
	do i=1,10
		call coltyp18(ycolor(i))
		call setfont1(iyfont)
	      call juststr(ixgy(i),iygy(i),outstry(i),yangle(i),
     &      sizey,yjusx(i),yjusy(i),ylet(i),ystr(i),logxy,
     &	inumy)
		call coltyp18(xcolor(i))
		call setfont1(ixfont)
	      call juststr(ixgx(i),iygx(i),outstrx(i),xangle(i),
     &      sizex,xjusx(i),xjusy(i),xstr(i),cexp(i),logxy,
     &	inumx)
	enddo
c	press 1/2/3/4/5/6 or 7 for choices:
99	a=getch(B)
	nr=ichar(a)
	if(nr.eq.27.or.nr.eq.55) then
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
		call wrstring18(1,0,'1.LINE A',14,1)
		call wrstring18(80,0,'2.LINE B',14,1)
		call wrstring18(160,0,'3.LINE C',14,1)
		call wrstring18(240,0,'4.ENTER TEXT',14,1)
		call wrstring18(360,0,'5.MOVE TEXT',14,1)
		call wrstring18(590,0,'7.QUIT',14,1)
		call wrstring18(464,0,'6.FIX NUMBERS',14,1)
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
     &	justx(k),justy(k),colors(k),font(k),ibk)
		k=k+1
	else if(nr.eq.53.and.k.gt.1) then       ! move string
		call moveone(texts,sizes,k-1,ix,iy,justx,justy,
     &      colors,rotate,font,ibk)
	else if(nr.eq.54) then       ! move axis'numbers
	  call fillwin(0,0,640,20,1)
	  call wrstring18(1,0,'1.FIX X',14,1)
	  call wrstring18(200,0,'2.FIX Y',14,1)
	  call wrstring18(400,0,'3.MAIN MENU',14,1)
87	  a=getch(B)
	  nr2=ichar(a)
	  if (nr2.eq.27.or.nr2.eq.51) then     ! exit
	  	call fillwin(0,0,640,20,1)
		call wrstring18(1,0,'1.LINE A',14,1)
		call wrstring18(80,0,'2.LINE B',14,1)
		call wrstring18(160,0,'3.LINE C',14,1)
		call wrstring18(240,0,'4.ENTER TEXT',14,1)
		call wrstring18(360,0,'5.MOVE TEXT',14,1)
		call wrstring18(590,0,'7.QUIT',14,1)
		call wrstring18(464,0,'6.FIX NUMBERS',14,1)
		goto 99
	  else if(nr2.eq.49) then  ! move x numbers
		call setfont1(ixfont)
	      call wrstring18(1,0,'1.FIX X',12,14)
	      call allmove(outstrx,nx,ixgx,iygx,xjusx,yjusx,xcolor,xangle,
     &	ixfont,ibk,xstr,cexp,csfacx,csizex,logxy,inumx)
	      call wrstring18(1,0,'1.FIX X',14,12)
		goto 87
	  else if(nr2.eq.50) then  ! move y numbers
	      call wrstring18(200,0,'2.FIX Y',12,14)
		call setfont1(iyfont)
	      call allmove(outstry,ny,ixgy,iygy,xjusy,yjusy,ycolor,yangle,
     &	iyfont,ibk,ylet,ystr,csfacy,csizey,logxy,inumy)
	      call wrstring18(200,0,'2.FIX Y',14,12)
		goto 87
 	  else
		goto 87
	  endif
	endif
	goto 99

c====================================================
999	continue
	call ENDPLT
c====================================================


	end


