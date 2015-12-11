	program tpopm

	character*30 strin(30),helps(5),title,options(10)
	character*1 charout
	character*64 header
	integer kline(30)
	real ax(10),ay(10)
	character*1	ans,getch
	integer iax(10),iay(10)

c
c
	options(1)='option #1'
	options(2)='option #2'
	options(3)='option #3'
	options(4)='option #4'
	options(5)='option #5'
	options(6)='option #6'
	options(7)='option #7'
	options(8)='option #8'
	options(9)='option #9'
	options(10)='option #10'

	title='Test menu'
c
	strin(1)='String #1'
	strin(2)='sTring #2'
	strin(3)='stRing #3'
	strin(4)='strIng #4'
	strin(5)='striNg #5'
	strin(6)='strinG #6'
	strin(7)='string #7'
	strin(8)='string #8'
	strin(9)='string #9'
	strin(10)='string #10'
	strin(11)='String #11'
	strin(12)='sTring #12'
	strin(13)='stRing #13'
	strin(14)='strIng #14'
	strin(15)='striNg #15'
	strin(16)='strinG #16'
	strin(17)='string #17'
	strin(18)='string #18'
	strin(19)='string #19'
	strin(20)='string #20'
	nval=20

	helps(1)='First help string'
	helps(2)='2nd help string'
	nhelp=2
      call gino
	call vga
	call gsetcols(0)
	call errswi(-1)
	call brkswi(1)
	call chaswi(1)
	call grfmod (1)
	call harcha
	call mode(18)
	xmin=-300.
	ymin=-300.
	xmax= 300.
	ymax= 300.
	call SETMOUSE()	!sets values in common/mousval/
	ixlo=320
c	iylo=240
	iyhi=470	!DEFINE TOP R CORNER
	ic=15
	ibk=8
	icf=14
c	ich=12
	icup=12
c	goto 100
	iy=0

	do while(iy.lt.480)		!draw some background graphics
	   call HLINE(0,639,iy,12)
	   iy=iy+20
	enddo
	ix=0
	do while(ix.lt.639)		!draw some background graphics
	   call VLINE(ix,0,479,11)
	   ix=ix+20
	enddo
	iline=3
	call POPMENU(ixlo,-1,iyhi,strin,10,ic,icf,icup,ibk,title,
     & helps,nhelp,iline,charout,ival)
	print*,iline
	pause
	iline=3
	nval=20
	nhelp=2
	nopt=10
	nrow=8
	ich=12
	header='test page'
	call POPage(ixlo,iylo,iyhi,strin,nval,ic,icf,icup,ibk,title,
     & helps,nhelp,iline,charout,ival,nrow)
	print*,iline
	pause
	iline=3
	nhelp=2
	nval=20
	call POPMENU(ixlo,iylo,iyhi,strin,nval,ic,icf,icup,ibk,title,
     & helps,nhelp,iline,charout,ival)
	print*,iline
	pause
	iline=3
	nhelp=2
	call POProws(ixlo,-1,iyhi,strin,nval,ic,icf,icup,ibk,title,
     & helps,nhelp,iline,charout,ival,kline)
	do i=1,nval
	     if( kline(i).ne.-1)  print*, kline(i)
	enddo
	pause
	call fillwin(0,0,640,479,1)
	call crosspix(320,240,1,1,639,479,1,13,0,1,2,0,iax,iay,
     &      6,70,6,4)
		print*,iax(1),iay(1)
		print*,iax(2),iay(2)
	pause
	call fillwin(0,0,640,479,1)
	call crosspix(320,240,1,1,639,479,1,13,0,3,2,0,iax,iay,
     &      6,70,6,4)
		print*,iax(1),iay(1)
		print*,iax(2),iay(2)
	pause
	call fillwin(0,0,640,479,1)
	call crosspix(320,240,1,1,639,479,1,13,0,3,2,1,iax,iay,
     &      6,70,6,4)
		print*,iax(1),iay(1)
		print*,iax(2),iay(2)
	pause
	call GRAPHBOUNDARY(0.,200.,0.,150.)
	call gSCALE(xmin,xmax,ymin,ymax,is)
100   continue
c	call clears(1,479,12)
	call fillwin(0,0,640,479,1)
c	subroutine crossh(ix,iy,ixlo,iylo,ixhi,iyhi,ibk,icf,itype,
c     &	line,idraw,islope,ax,ay,irhi,iclo,m,eps)
	call crossh(320,240,1,1,639,479,1,13,0,1,2,0,ax,ay,
     &      6,70,6,1.)
		print*,ax(1),ay(1)
		print*,ax(2),ay(2)
		pause

	call crossh(320,240,1,1,639,479,1,13,0,2,2,0,ax,ay,
     &      6,70,6,1.)
		print*,ax(1),ay(1)
		print*,ax(2),ay(2)
		pause
	call fillwin(0,0,640,479,1)
		call crossh(320,240,1,1,639,479,1,13,0,1,2,1,ax,ay,
     &	6,70,6,0.1)
		print*,ax(1),ay(1)
		print*,ax(2),ay(2)
		pause

	call fillwin(0,0,640,479,1)
		call crossh(320,240,1,1,639,479,1,13,0,1,2,1,ax,ay,
     &	6,70,6,1.)
		print*,ax(1),ay(1)
		print*,ax(2),ay(2)
		pause
	call fillwin(0,0,640,479,1)
		call crossh(320,240,1,1,639,479,1,13,0,2,2,1,ax,ay,
     &	6,70,6,1.)
		print*,ax(1),ay(1)
		print*,ax(2),ay(2)
		pause

	call fillwin(0,0,640,479,1)
		call crossh(320,240,1,1,639,479,1,13,0,2,2,2,ax,ay,
     &	6,70,6,1.)
		print*,ax(1),ay(1)
		print*,ax(2),ay(2)
		pause
	call fillwin(0,0,640,479,1)
	call mode(3)
	end

