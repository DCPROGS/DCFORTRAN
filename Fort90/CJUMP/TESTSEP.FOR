	program testsep
	character  a,getch
	integer*4  	x(30),y(30),xp(30),yp(30)
	integer*4  	xc(30),yc(30),iym(30),iymo(30)


	character*1 chary(0:10),e0,e1
	character*10 stry1,stry2,str(10)
	chary(0)='2'
	chary(1)='5'
	chary(2)='5'
	chary(3)=' '
	chary(4)='0'
	chary(5)=' '
	chary(6)='2'
	chary(7)='3'
	chary(8)=' '
	chary(9)=' '
	n=9
	call mode(18)
	call clears(0,480,15)
	call draw_valve(200,200,100,70,40,12,4,8)
	call draw_valve(400,20,30,30,20,12,4,8)
	call draw_valve(20,20,24,29,4,12,4,8)
	a=getch(b)
c	call zbar(400,20,100,70,40,15,15)
	call fillwin(400,20,440,60,15)
c	pause
	call zbar(405,25,30,30,10,4,12)
	a=getch(b)
	call clears(0,480,1)
c	call solbox(ixlo,iylo,ixhi,iyhi,icborder,icbox,nvalves,
c     &nrows,icvalve,icshad,x,y,xp,yp,lv,ihv,iwv,xc,yc,irc,ic,icfill,
c     &nbut,ixm,iym,ixmo,iymo)
	call solbox(0,0,640,480,3,11,25,
     &3,12,0,x,y,xp,yp,lv,ihv,iwv,xc,yc,irc,2,10,5,ixm,iym,ixmo,iymo)
c	call movcur(ix_down,iy_down,isizex,isizey,idy,n_rows,
c     &	idx,n_columns,icur,ibk,isym,idot,irow,icolumn)
c	print*,x(1),y(1)
	call zsymbol(x(1)+int(0.7*lv),y(1)+int(0.8*ihv),8,6,6,4,12)
	call movcur(x(1)+int(0.7*lv),y(1)+int(0.8*ihv),6,6,int(2*ihv),5,
     &int(1.45*lv),10,4,11,8,1,l,k)

	a=getch(b)
	call clears(0,480,0)
	goto 99
	call extrstr(chary,stry1,stry2,n)
	print*,stry1
	print*,stry2
	call exstr(chary,str,n,m)
	do i=1,m
		print*,i,' ',str(i)
	enddo

	do i=1,m
		if(str(i)(1:1).ne.'0'.and.str(i)(1:1).ne.' ') then
     			print*,i,' ',str(i)
		endif
	enddo
	e0=str(1)(1:1)
	e1=str(2)(1:1)
	print*,e0,' ',e1
	call curon(0)
	print*,'abc'
	call curtyp(12)
	a=getch(b)
	print*,'abc'
	call curtyp(11*256+12)
	a=getch(b)
	print*,'abc'
	call curtyp(11*128+12)
	a=getch(b)
	print*,'abc'
	call curtyp(10*256+12)
	a=getch(b)
	print*,'abc'
	call curtyp(0)
	a=getch(b)

99	end
