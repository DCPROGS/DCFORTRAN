	program newtr

	real*4	hx(0:10),hy(0:10),hz(0:10),ahx(0:10),ahy(0:10)
	real*4	ahz(0:10)
	real*4	bhx(0:10),bhy(0:10),bhz(0:10),chx(0:10),chy(0:10)
	real*4	chz(0:10)
	integer*1	back(20000),train(20000),plback(10000)
	character*1	cha,getch

	call mode(18)
	call rdgwin1(8,1,31,2,back)
	do i=1,10
		print*,back(i)
	enddo
	cha=getch(kty)
	call clears(0,480,15)
	cha=getch(kty)
	print*,cha,kty
	if (kty.eq.2) then
		goto 12
	endif
	call fillwin(100,300,200,400,2)
c	now some 3D
	xleftw=-100
	xrightw=320
	ybottomw=-100
	ytopw=300
	xleftv=0
	xrightv=640
	ybottomv=0
	ytopv=480
	dx=0.5
	dy=0.5
	dz=1
	hx(1)=100
	hy(1)=100
	hz(1)=100
	hx(2)=200
	hy(2)=100
	hz(2)=100
	hx(3)=200
	hy(3)=200
	hz(3)=100
	hx(4)=100
	hy(4)=200
	hz(4)=100
	ahx(1)=100
	ahy(1)=100
	ahz(1)=100
	ahx(2)=100
	ahy(2)=100
	ahz(2)=50
	ahx(3)=100
	ahy(3)=200
	ahz(3)=50
	ahx(4)=100
	ahy(4)=200
	ahz(4)=100
	bhx(1)=100
	bhy(1)=100
	bhz(1)=50
	bhx(2)=200
	bhy(2)=100
	bhz(2)=50
	bhx(3)=200
	bhy(3)=200
	bhz(3)=50
	bhx(4)=100
	bhy(4)=200
	bhz(4)=50

	chx(1)=200
	chy(1)=100
	chz(1)=50
	chx(2)=200
	chy(2)=200
	chz(2)=50
	chx(3)=200
	chy(3)=200
	chz(3)=100
	chx(4)=200
	chy(4)=100
	chz(4)=100
	chx(10)=150
	chy(10)=250
	chz(10)=75
	ic=1
	call initialize()

    	call set_window(carg(xleftw),carg(xrightw),carg(ybottomw),
     &	carg(ytopw))
	call set_viewport(carg(xleftv),carg(xrightv),carg(ybottomv),
     &	carg(ytopv))
c	pause
	call set_parallel(carg(dx),carg(dy),carg(dz))
c	pause
	n=4
	call poly(bhx,bhy,bhz,n,ic)
	call poly(chx,chy,chz,n,ic)
	call poly(hx,hy,hz,n,ic)
	call poly(ahx,ahy,ahz,n,ic)
c	print*,'after poly'
c	pause
	call fillwin(10,30,20,40,2)
	call make_picture_current()
	pause
	call linedraw(carg(10),carg(30),carg(50),carg(80),carg(13))
	call fillwin(10,30,20,40,3)
	pause

	call initialize()

	xleftw=-320
	xrightw=300
	ybottomw=-200
	ytopw=300
	xleftv=0
	xrightv=320
	ybottomv=0
	ytopv=200
	ic=4
    	call set_window(carg(xleftw),carg(xrightw),carg(ybottomw),
     &	carg(ytopw))
	call set_viewport(carg(xleftv),carg(xrightv),carg(ybottomv),
     &	carg(ytopv))
c	pause
	call set_parallel(carg(dx),carg(dy),carg(dz))
c	pause
	n=4
	call poly(bhx,bhy,bhz,n,ic)
	call poly(chx,chy,chz,n,ic)
	call poly(hx,hy,hz,n,ic)
	call poly(ahx,ahy,ahz,n,ic)
	call make_picture_current()

	dx=50
	dy=50
	dz=-50
	xleftw=-50
	xrightw=200
	ybottomw=-100
	ytopw=50
	xleftv=0
	xrightv=320
	ybottomv=0
	ytopv=200
	ic=8
    	call set_window(carg(xleftw),carg(xrightw),carg(ybottomw),
     &	carg(ytopw))
	call set_viewport(carg(xleftv),carg(xrightv),carg(ybottomv),
     &	carg(ytopv))
c	pause
	call set_perspective(carg(dx),carg(dy),carg(dz))
c	pause
	n=4
	call poly(bhx,bhy,bhz,n,ic)
	call poly(chx,chy,chz,n,ic)
	call poly(hx,hy,hz,n,ic)
	call poly(ahx,ahy,ahz,n,ic)
c	print*,'after poly'
c	pause
	call make_picture_current()
	pause
	call clears(0,480,1)
	call initialize()
	dx=150
	dy=150
	dz=-200
	xleftw=100
	xrightw=200
	ybottomw=50
	ytopw=250
	xleftv=0
	xrightv=620
	ybottomv=0
	ytopv=480
	ic=11
    	call set_window(carg(xleftw),carg(xrightw),carg(ybottomw),
     &	carg(ytopw))
	call set_viewport(carg(xleftv),carg(xrightv),carg(ybottomv),
     &	carg(ytopv))
c	pause
	call set_perspective(carg(dx),carg(dy),carg(dz))
c	pause
	n=4
	call enter_frame(hx,hy,hz,carg(n),carg(ic))
	call enter_frame(ahx,ahy,ahz,carg(n),carg(ic))
	call enter_frame(bhx,bhy,bhz,carg(n),carg(ic))
	call enter_frame(chx,chy,chz,carg(n),carg(ic))
	call enter_line(carg(hx(3)),carg(hy(3)),carg(hz(3)),
     &	carg(chx(10)),carg(chy(10)),carg(chz(10)),carg(ic))
	call enter_line(carg(hx(4)),carg(hy(4)),carg(hz(4)),
     &	carg(chx(10)),carg(chy(10)),carg(chz(10)),carg(ic))
	call enter_line(carg(bhx(3)),carg(bhy(3)),carg(bhz(3)),
     &	carg(chx(10)),carg(chy(10)),carg(chz(10)),carg(ic))
	call enter_line(carg(bhx(4)),carg(bhy(4)),carg(bhz(4)),
     &	carg(chx(10)),carg(chy(10)),carg(chz(10)),carg(ic))


c	call fillwin(0,100,160,160,8)
	call make_picture_current()
c	call fillwin(0,100,160,160,7)

	ivx=120
	ivy=100
	ix=8
	iy=300
	call rdgwin1(ix,iy-20,ix+55,iy+40,plback)
	call rdgwin1(ivx,100,ivx+160,160,back)
c	call fillwin(0,100,160,160,1)
c	pause
	call tren(ivx,train)
	call plane(ix,iy,15)
	cha=getch(kty)

88	continue
	do i=1,30
c

		call wrgwin1(ix,iy-20,ix+55,iy+40,plback)
		call wrgwin1(ivx,ivy,ivx+160,160,back)
		ix=ix+16
		ivx=ivx+8
		call rdgwin1(ix,iy-20,ix+55,iy+40,plback)
		call rdgwin1(ivx,ivy,ivx+160,160,back)
		call wrgwin1(ivx,ivy,ivx+160,160,train)
		call plane(ix,iy,15)
		cha=getch(kty)
		if (ktype.eq.2) goto 12
	enddo

	do i=1,30
c
		call wrgwin1(ix,iy-20,ix+55,iy+40,plback)
		call wrgwin1(ivx,ivy,ivx+160,160,back)
		ix=ix-16
		ivx=ivx-8
		call rdgwin1(ix,iy-20,ix+55,iy+40,plback)
		call rdgwin1(ivx,ivy,ivx+160,160,back)
		call wrgwin1(ivx,ivy,ivx+160,160,train)
		call plane(ix,iy,15)
		cha=getch(kty)
		if (kty.eq.2) goto 12
	enddo
	goto 88
12	end


	subroutine poly(x,y,z,n,ic)
	real*4	x(0:n)
	real*4	y(0:n)
	real*4	z(0:n)
	integer*2	ic
	do i=1,n-1
		call enter_line(carg(x(i)),carg(y(i)),
     &	carg(z(i)),
     &	carg(x(i+1)),carg(y(i+1)),carg(z(i+1)),carg(ic))
	enddo
	call enter_line(carg(x(1)),carg(y(1)),carg(z(1)),
     &	carg(x(n)),carg(y(n)),carg(z(n)),carg(ic))


	end

	subroutine tren	(ivx,train)
	integer*1	train(20000)
	call draw_with_frame(ivx,100,ivx+70,150,8,0)
	call draw_with_frame(ivx+71,100,ivx+159,130,8,0)
	call draw_with_frame(ivx+80,131,ivx+100,150,8,0)
	call draw_with_frame(ivx+140,131,ivx+159,150,8,0)
	call draw_with_frame(ivx+110,131,ivx+130,160,8,0)
	call fillwin(ivx+8,130,ivx+60,140,12)
	call rdgwin1(ivx,100,ivx+160,160,train)
	end

	subroutine plane(ix,iy,ic)
	call draw_with_frame(ix,iy,ix+50,iy+20,ic,7)
	call iline(ix+40,iy+10,ix+20,iy+40,ic)
	call iline(ix+20,iy-20,ix+20,iy+40,ic)
	call iline(ix+40,iy+10,ix+20,iy-20,ic)
	end
