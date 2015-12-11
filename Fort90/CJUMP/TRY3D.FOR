
	program try3d
	real*4	hx(0:10),hy(0:10),hz(0:10),ahx(0:10),ahy(0:10)
	real*4	ahz(0:10)
	real*4	bhx(0:10),bhy(0:10),bhz(0:10),chx(0:10),chy(0:10)
	real*4	chz(0:10)
	character*1	cha,getch

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
	hx(0)=100
	hy(0)=200
	hz(0)=100
	ahx(0)=100
	ahy(0)=200
	ahz(0)=100
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
	bhx(0)=100
	bhy(0)=200
	bhz(0)=50
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
	chx(0)=200
	chy(0)=100
	chz(0)=100
	chx(10)=150
	chy(10)=250
	chz(10)=75

c	now some 3D

	call mode(18)
	call clears(0,480,8)
	call rectangle(carg(10),carg(100),carg(100),carg(200),carg(200))
	CHA=GETCH(B)
	call clears(0,480,8)
	call initialize()
	xleftv=0
	xrightv=640
	ybottomv=0
	ytopv=480
	xleftw=-100
	xrightw=320
	ybottomw=-100
	ytopw=300

	dx=0.3
	dy=0.3
	dz=1

    	call set_window(carg(xleftw),carg(xrightw),carg(ybottomw),
     &	carg(ytopw))
	call set_viewport(carg(xleftv),carg(xrightv),carg(ybottomv),
     &	carg(ytopv))
	call set_parallel(carg(dx),carg(dy),carg(dz))
	call enter_polygon(bhx,bhy,bhz,carg(4),carg(1),carg(9))
	call enter_polygon(chx,chy,chz,carg(4),carg(2),carg(10))
	call enter_polygon(hx,hy,hz,carg(4),carg(3),carg(11))
	call enter_polygon(ahx,ahy,ahz,carg(4),carg(4),carg(12))
	call make_picture_current()

	call initialize()
	xleftw=-100
	xrightw=320
	ybottomw=0
	ytopw=400

	dx=0.5
	dy=0.5
	dz=-1

    	call set_window(carg(xleftw),carg(xrightw),carg(ybottomw),
     &	carg(ytopw))
	call set_viewport(carg(xleftv),carg(xrightv),carg(ybottomv),
     &	carg(ytopv))
	call set_parallel(carg(dx),carg(dy),carg(dz))
	call enter_polygon(bhx,bhy,bhz,carg(4),carg(1),carg(9))
	call enter_polygon(chx,chy,chz,carg(4),carg(2),carg(10))
	call enter_polygon(ahx,ahy,ahz,carg(4),carg(4),carg(12))
	call enter_polygon(hx,hy,hz,carg(4),carg(3),carg(11))
	call make_picture_current()
	CHA=GETCH(B)

	call clears(0,480,15)
	call initialize()
	dx=150
	dy=150
	dz=-200
	xleftw=100
	xrightw=400
	ybottomw=0
	ytopw=240
	xleftv=0
	xrightv=640
	ybottomv=0
	ytopv=480
    	call set_window(carg(xleftw),carg(xrightw),carg(ybottomw),
     &	carg(ytopw))
	call set_viewport(carg(xleftv),carg(xrightv),carg(ybottomv),
     &	carg(ytopv))
	call set_perspective(carg(dx),carg(dy),carg(dz))

	call enter_polygon(bhx,bhy,bhz,carg(4),carg(5),carg(13))
	call enter_polygon(chx,chy,chz,carg(4),carg(2),carg(10))
	call enter_polygon(hx,hy,hz,carg(4),carg(3),carg(11))
	call enter_polygon(ahx,ahy,ahz,carg(4),carg(4),carg(12))
	call make_picture_current()

	call initialize()
	dx=150
	dy=150
	dz=-150
	xleftw=100
	xrightw=350
	ybottomw=100
	ytopw=300
    	call set_window(carg(xleftw),carg(xrightw),carg(ybottomw),
     &	carg(ytopw))
	call set_viewport(carg(xleftv),carg(xrightv),carg(ybottomv),
     &	carg(ytopv))
c	pause
	call set_perspective(carg(dx),carg(dy),carg(dz))

	call enter_polygon(bhx,bhy,bhz,carg(4),carg(1),carg(9))
	call enter_polygon(chx,chy,chz,carg(4),carg(2),carg(10))
	call enter_polygon(hx,hy,hz,carg(4),carg(3),carg(11))
	call enter_polygon(ahx,ahy,ahz,carg(4),carg(4),carg(12))

	call make_picture_current()
	CHA=GETCH(B)

	call clears(0,480,15)
	call initialize()
	dx=150
	dy=150
	dz=-250
	xleftw=0
	xrightw=640
	ybottomw=0
	ytopw=480
	xleftv=0
	xrightv=320
	ybottomv=0
	ytopv=240
    	call set_window(carg(xleftw),carg(xrightw),carg(ybottomw),
     &	carg(ytopw))
	call set_viewport(carg(xleftv),carg(xrightv),carg(ybottomv),
     &	carg(ytopv))
	call set_perspective(carg(dx),carg(dy),carg(dz))

	call enter_polygon(bhx,bhy,bhz,carg(4),carg(5),carg(13))
	call enter_polygon(chx,chy,chz,carg(4),carg(2),carg(10))
	call enter_polygon(hx,hy,hz,carg(4),carg(3),carg(11))
	call enter_polygon(ahx,ahy,ahz,carg(4),carg(4),carg(12))
	call make_picture_current()

	call initialize()
	xleftv=320
	xrightv=640
	ybottomv=0
	ytopv=240
	dx=150
	dy=150
	dz=-200
    	call set_window(carg(xleftw),carg(xrightw),carg(ybottomw),
     &	carg(ytopw))
	call set_viewport(carg(xleftv),carg(xrightv),carg(ybottomv),
     &	carg(ytopv))
c	pause
	call set_perspective(carg(dx),carg(dy),carg(dz))

	call enter_polygon(bhx,bhy,bhz,carg(4),carg(1),carg(9))
	call enter_polygon(chx,chy,chz,carg(4),carg(2),carg(10))
	call enter_polygon(hx,hy,hz,carg(4),carg(3),carg(11))
	call enter_polygon(ahx,ahy,ahz,carg(4),carg(4),carg(12))

	call make_picture_current()
	call initialize()
	dx=150
	dy=150
	dz=-100
	xleftw=0
	xrightw=640
	ybottomw=0
	ytopw=480
	xleftv=0
	xrightv=320
	ybottomv=240
	ytopv=480
    	call set_window(carg(xleftw),carg(xrightw),carg(ybottomw),
     &	carg(ytopw))
	call set_viewport(carg(xleftv),carg(xrightv),carg(ybottomv),
     &	carg(ytopv))
	call set_perspective(carg(dx),carg(dy),carg(dz))

	call enter_polygon(bhx,bhy,bhz,carg(4),carg(5),carg(13))
	call enter_polygon(chx,chy,chz,carg(4),carg(2),carg(10))
	call enter_polygon(hx,hy,hz,carg(4),carg(3),carg(11))
	call enter_polygon(ahx,ahy,ahz,carg(4),carg(4),carg(12))
	call make_picture_current()

	call initialize()
	xleftv=320
	xrightv=640
	ybottomv=240
	ytopv=480
	dx=150
	dy=150
	dz=-50
    	call set_window(carg(xleftw),carg(xrightw),carg(ybottomw),
     &	carg(ytopw))
	call set_viewport(carg(xleftv),carg(xrightv),carg(ybottomv),
     &	carg(ytopv))
c	pause
	call set_perspective(carg(dx),carg(dy),carg(dz))

	call enter_polygon(bhx,bhy,bhz,carg(4),carg(1),carg(9))
	call enter_polygon(chx,chy,chz,carg(4),carg(2),carg(10))
	call enter_polygon(hx,hy,hz,carg(4),carg(3),carg(11))
	call enter_polygon(ahx,ahy,ahz,carg(4),carg(4),carg(12))

	call make_picture_current()
	CHA=GETCH(B)
	end
