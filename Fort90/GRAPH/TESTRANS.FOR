	program testrans
	character ans,getch
	character*8 text
      call GINO
	call VGA
	call papenq(xp,yp,ipap)
	vxlo=0	! for VIEWPORT
	vxhi=xp
	vylo=0
	vyhi=yp
	   xlo=0.32*xp         !portrait
	   xhi=0.67*xp	!axis length x=3500 y=4200
	   ylo=0.2*yp
	   yhi=0.75*yp

	xmin=0.
	ymin=0.
	xmax=2.
	ymax=100.
	text='second w'
	call axiset
	call GRAPHBOUNDARY(xlo,xhi,ylo,yhi)

	call gSCALE(xmin,xmax,ymin,ymax,1)
	call spagra(vxlo,vylo,wxmin,wymin)
	call spagra(vxhi,vyhi,wxmax,wymax)
c*******************************
c	call gsetcols(0)
	call mode(18)

c	call FILLWIN(0,0,639,479,1)			!dialog area
	call wrstring18(400,80,'first w',14,1)
	call wrstring18(400,160,'second w',14,1)
	call wrstring18(400,240,'third w',14,1)

	call gramov(0.5,100.)
	call wrstring18(400,160,'second w',15,14)
	call chastr('first g')
	ans=getch(ib)
	call wrstring18(480,160,text,15,1)
	call gramov(0.5,80.)
	call chastr('second g')
	call wrstring18(400,280,'third w',15,1)
	ans=getch(ib)
	call vhelp(10)
	ans=getch(ib)
	call linwid(0.5)
	call vhelp(10)
	ans=getch(ib)
      call mode(3)
	end
