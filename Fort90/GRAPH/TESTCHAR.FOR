c==================================================================
	program testchar
c==================================================================
	integer*1 arr(20000)
	real xbox(4),ybox(4)
	character*50 str,new
	icolor=14
	ibk=0
	ix=0
	iy=400
	xmin=0.
	ymin=0.
	xmax=0.02
	ymax=100.

      call GINO
	call VGA
	call papenq(xp,yp,ipap)
	xlo=0.3*xp         !portrait
	xhi=0.8*xp
	ylo=0.2*yp
	yhi=0.8*yp
	call axiset
	call axipos(1,xlo,ylo,xhi-xlo,1)
	call axipos(1,xlo,ylo,yhi-ylo,2)
	call axisca(1,10,xmin,xmax,1)
	call axisca(1,10,ymin,ymax,2)
	x=0.017
	y=50.
	str='abc*Ndefg*Nhijkl*Nmnoprs*Ntuvw'
	new='*F100am *F010abcd *E100'
	nl=nblank1(new)
	call realength(new,nr)
	i1=ichar('*')
	i2=ichar('a')
	angle=0.
	size=2.5
	ijus=1
	imode=1
	call graspa(x,y,xsc,ysc)
      call justifystring(Xsc,Ysc,new,ANGLE,SIZE,IJUS)
      call FBOX1(Xsc,Ysc,new,ANGLE,SIZE,IJUS,xbox,ybox,imode)
	y=25.
	call graspa(x,y,xsc,ysc)
      call justifystring(Xsc,Ysc,STR,ANGLE,SIZE,IJUS)
      call FBOX1(Xsc,Ysc,STR,ANGLE,SIZE,IJUS,xbox,ybox,imode)
	x=0.01
	call graspa(x,y,xsc,ysc)
      call justifystring(Xsc,Ysc,STR,ANGLE,SIZE,0)
      call FBOX1(Xsc,Ysc,STR,ANGLE,SIZE,0,xbox,ybox,imode)
	x=0.003
	call graspa(x,y,xsc,ysc)
      call justifystring(Xsc,Ysc,STR,ANGLE,SIZE,-1)
      call FBOX1(Xsc,Ysc,STR,ANGLE,SIZE,-1,xbox,ybox,imode)
	do i=1,4
	 print*,xbox(i),ybox(i)
	enddo
	print*,i1,',',i2,' ',nl,' ',nr
	pause
	call vhelp(8)
	pause
	call ginend
	end
