
	program calc_c

	character*10 s(6)
	ici=33
	icf=23
	icp=28
	icoli=8
	icolf=10
	icolp=15
	iy2=96
	iy1=52
	mf=8
	mi=6
	id2=7
	id1=19
	print*,'Enter birth date:'
	print*,'Year:'
	read*,iy1
	print*,'Month:'
	read*,mi
	print*,'Day:'
	read*,id1
	print*,'Enter current date:'
	print*,'Year:'
	read*,iy2
	print*,'Month:'
	read*,mf
	print*,'Day:'
	read*,id2
	iy=iy2-iy1-2
	select case(mi)
	   case(1)
		idi=365-id1
	   case(2)
		idi=334-id1
	   case(3)
		idi=306-id1
	   case(4)
		idi=275-id1
	   case(5)
		idi=245-id1
	   case(6)
		idi=214-id1
	   case(7)
		idi=184-id1
	   case(8)
		idi=153-id1
	   case(9)
		idi=122-id1
	   case(10)
		idi=92-id1
	   case(11)
		idi=61-id1
	   case(12)
		idi=31-id1
	end select
	if(mod(iy1,4).eq.0.and.mi.lt.3) idi=idi+1

	select case(mf)
	   case(1)
		idf=id2
		m0=31
	      iz=182
		s(1)='D'
		s(2)='J'
		s(3)='F'
		s(4)='M'
		s(5)='A'
		s(6)='M'
		m1=31
		m2=28
	     if(mod(iy2,4).eq.0) m2=29
		m3=31
		m4=30
	   case(2)
		idf=31+id2
		m0=31
	      iz=181
		m1=28
	      if(mod(iy2,4).eq.0) m1=29
		m2=31
		m3=30
		m4=31
		s(1)='J'
		s(2)='F'
		s(3)='M'
		s(4)='A'
		s(5)='M'
		s(6)='J'
	   case(3)
		idf=59+id2
		m0=28
	      iz=181
		m1=31
		m2=30
		m3=31
		m4=30
		s(1)='F'
		s(2)='M'
		s(3)='A'
		s(4)='M'
		s(5)='J'
		s(6)='J'
	   case(4)
		m0=31
		idf=90+id2
	      iz=184
		m1=30
		m2=31
		m3=30
		m4=31
		s(1)='M'
		s(2)='A'
		s(3)='M'
		s(4)='J'
		s(5)='J'
		s(6)='A'
	   case(5)
		idf=121+id2
		m0=30
	      iz=183
		m1=31
		m2=30
		m3=31
		m4=31
		s(1)='A'
		s(2)='M'
		s(3)='J'
		s(4)='J'
		s(5)='A'
		s(6)='S'
	   case(6)
		idf=150+id2
		m0=31
	      iz=184
		m1=30
		m2=31
		m3=31
		m4=30
		s(1)='M'
		s(2)='J'
		s(3)='J'
		s(4)='A'
		s(5)='S'
		s(6)='O'
	   case(7)
		idf=181+id2
		m0=30
	      iz=183
		m1=31
		m2=31
		m3=30
		m4=31
		s(1)='J'
		s(2)='J'
		s(3)='A'
		s(4)='S'
		s(5)='O'
		s(6)='N'
	   case(8)
		idf=211+id2
		m0=31
	      iz=184
		m1=31
		m2=30
		m3=31
		m4=30
		s(1)='July'
		s(2)='August'
		s(3)='September'
		s(4)='October'
		s(5)='November'
		s(6)='December'
	   case(9)
		idf=242+id2
		m0=31
	      iz=184
		m1=30
		m2=31
		m3=30
		m4=31
		s(1)='A'
		s(2)='S'
		s(3)='O'
		s(4)='N'
		s(5)='D'
		s(6)='J'
	   case(10)
		idf=273+id2
		m0=30
	      iz=181
		m1=31
		m2=30
		m3=31
		m4=30
		s(1)=''
		s(2)=''
		s(3)=''
		s(4)=''
		s(5)=''
		s(6)=''
	   case(11)
		idf=303+id2
		m0=31
	      iz=182
		m1=30
		m2=31
		m3=31
		m4=28
		s(1)=''
		s(2)=''
		s(3)=''
		s(4)=''
		s(5)=''
		s(6)=''
	      if(mod(iy2,4).eq.0) m4=29
	   case(12)
		idf=334+id2
		m0=30
	      iz=181
		m1=31
		m2=31
		m3=28
	      if(mod(iy2,4).eq.0) m3=29
		m4=31
		s(1)=''
		s(2)=''
		s(3)=''
		s(4)=''
		s(5)=''
		s(6)=''
	end select
	if(mod(iy2,4).eq.0) then
         if(mf.gt.3) idf=idf+1
	   if(mf.le.3) iz=iz+1
	   if(mf.eq.3) m0=29
	endif
	iz3=m0+m1+m2
	do i=id1+1,id2-1
	   if (mod(i,4).eq.0) id=id+1
	enddo
	id=id+idi+idf+iy*365
	call gino
	call vga
	call gsetcols(0)
1	call mode(18)
	call papenq(xp,yp,ienq)
	x0=0.1*xp
	y0=yp/2
	call fillwin(0,0,639,479,1)
	call lincols(14)
	call movto2(x0,0.25*yp)
	call linto2(x0,0.75*yp)
	call linto2(x0+iz,0.75*yp)
	call movto2(x0,y0)
	call linto2(x0+iz,y0)
	call movto2(x0+iz,0.25*yp)
	call linto2(x0+iz,0.75*yp)

	y1=y0-2.
	y2=y0+2.
	xd=x0
	do i=2,iz
	  xd=xd+1
	  call movto2(xd,y1)
	  call linto2(xd,y2)
	enddo
c	call lincols(8)
	call movto2(x0+m0,yp/4)
	call linto2(x0+m0,y2+2)
	call movto2(x0+m0+m1,yp/4)
	call linto2(x0+m0+m1,y2+2)
	call movto2(x0+m0+m1+m2,yp/4)
	call linto2(x0+m0+m1+m2,y2+2)
	call movto2(x0+m0+m1+m2+m3,yp/4)
	call linto2(x0+m0+m1+m2+m3,y2+2)
	call movto2(x0+m0+m1+m2+m3+m4,yp/4)
	call linto2(x0+m0+m1+m2+m3+m4,y2+2)
	call movto2(x0+m0+id2,0.25*yp)
	call linto2(x0+m0+id2,0.75*yp)
	call movto2(x0,yp/4)
	call linto2(x0+iz,yp/4)
	dx=1.
	call draw_sin(id,ici,icoli,x0,y0,iz,m0,id2,dx)
	call draw_sin(id,icf,icolf,x0,y0,iz,m0,id2,dx)
	call draw_sin(id,icp,icolp,x0,y0,iz,m0,id2,dx)
	call lincols(12)
	call movto2(x0+m0+id2,0.25*yp)
	call linto2(x0+m0+id2,0.75*yp)
	call movto2(x0+m0+id2,y0-10.)
c	call chastr('7')
	sy=yp/4+5.
	sx=x0+5.
	do i=1,6
	   call movto2(sx,sy)
	   call chastr(s(i))
	   sx=sx+m0
	enddo
	pause
	call fillwin(0,0,639,479,1)
	dx=2.
	call draw_sin(id,ici,icoli,x0,y0,iz3,m0,id2,dx)
	call draw_sin(id,icf,icolf,x0,y0,iz3,m0,id2,dx)
	call draw_sin(id,icp,icolp,x0,y0,iz3,m0,id2,dx)
	xd=x0
	call lincols(14)
	do i=2,iz3
	  xd=xd+2
	  call movto2(xd,y1)
	  call linto2(xd,y2)
	enddo
	call movto2(x0,0.25*yp)
	call linto2(x0,0.75*yp)
	call movto2(x0,y0)
	call linto2(x0+2*iz3,y0)
	call movto2(x0+2*iz3,0.25*yp)
	call linto2(x0+2*iz3,0.75*yp)
	call linto2(x0,0.75*yp)
	call movto2(x0+2*m0,yp/4)
	call linto2(x0+2*m0,y2+2)
	call movto2(x0+2*(m0+m1),yp/4)
	call linto2(x0+2*(m0+m1),y2+2)
	call movto2(x0+2*(m0+id2),y2-2)
	call linto2(x0+2*(m0+id2),y2+2)
	call movto2(x0,yp/4)
	call linto2(x0+2*iz3,yp/4)
	sx=x0+5.
	do i=1,3
	   call movto2(sx,sy)
	   call chastr(s(i))
	   SX=sx+2*m0
	enddo
	pause
	call devend
	end

	subroutine draw_sin(id,ic,icol,x0,y0,iz,m0,id2,dx)
	ir=id-(id/ic)*ic
	ira=ic+ir-m0-id2
	if(ira.lt.0) ira=ic+ira
	x=x0
	y=sin(float(ira)*6.28/float(ic))
	y=ic*y
	y=y+y0
	call lincols(icol)
	call movto2(x,y)
	do i=ira+1,ira+iz
	   x=x+dx
	   y=sin((float(i)/float(ic))*6.28)
	   y=ic*y
	   y=y+y0
	   call linto2(x,y)
	enddo
	end



