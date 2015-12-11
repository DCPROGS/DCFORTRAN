	program testallo
	integer*1 arraya(:),arra(3,10000)
	integer*4 x1(3),x2(3),y1(3),y2(3),dim(3),x(4),y(4)
	integer*4 xjus(5),yjus(5),size(5),color(5),angle(5),font(5)
	character a,getch
	character*10 str(5)
	allocatable arraya
	include 'g:\fortran\graph\consts.for'
	str(1)='1'
	str(2)='alpha'
	str(3)='100'
	x1(1)=0
	x1(2)=100
	x1(3)=200
	x2(1)=50
	x2(2)=150
	x2(3)=250
	y1(1)=100
	y1(2)=200
	y1(3)=300
	y2(1)=150
	y2(2)=250
	y2(3)=350
	call sethg()
c----------------------------------------

	call INIPLT(0,.false.,1.0)
	call VIEWPORT(0,10000,0,7500)
	call graphboundary(0,10000,0,7500)
	call scale(0.,639.,0.,479.)
	call mode(18)
	call clears(0,480,1)

	do i=1,3
		dim(i)=2*((y2(i)-y1(i)+9)*(x2(i)-x1(i)+9)+1)
		allocate(arraya(dim(i)))
		call rdgwin1(x1(i),y1(i),x2(i),y2(i),arraya)
		do j=1,dim(i)
		    arra(i,j)=arraya(j)
		enddo
		deallocate(arraya)
	enddo

	call clears(0,480,4)
	do i=1,3
		allocate(arraya(dim(i)))
		do j=1,dim(i)
		    arraya(j)=arra(i,j)
		enddo
		call wrgwin1 (x1(i),y1(i),x2(i),y2(i),arraya)
		deallocate(arraya)
	enddo
	a=getch(b)
	x(1)=100
	y(1)=100
	x(2)=1000
	y(2)=1000
	x(3)=3000
	y(3)=3000
	do i=1,3
	   xjus(i)=base
	   yjus(i)=base
	   angle(i)=0
	   color(i)=14
	   size(i)=i
	   font(i)=greek
	   call coltyp18(color(i))
	   call setfont1(font(i))
	   call justifystring(x(i),y(i),
     &		   str(i),angle(i),size(i),xjus(i),yjus(i))

	enddo
	ibk=4
	n=3
	call moveaxis(str,size,n,x,y,xjus,yjus,color,angle,
     &			font,ibk)
	a=getch(b)
	call endplt
	end
