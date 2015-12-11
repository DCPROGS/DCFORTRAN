	program tpopage

	character*30 strin(50),helps(5),title
	character*1 charout
	character*1	ans,getch

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
	strin(21)='String #21'
	strin(22)='sTring #22'
	strin(23)='stRing #23'
	strin(24)='strIng #24'
	strin(25)='striNg #25'
	strin(26)='strinG #26'
	strin(27)='string #27'
	strin(28)='string #28'
	strin(29)='string #29'
	strin(30)='string number 30'
	strin(31)='string #31'
	strin(32)='string #32'

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
	iline=24
	nval=32
	nhelp=3
	nrow=20
	ich=12
	call POPage(ixlo,iylo,iyhi,strin,nval,ic,icf,icup,ibk,title,
     & helps,nhelp,iline,charout,ival,nrow)
	print*,iline
	ans=getch(k)
	iline=3
	nval=20
	nhelp=2
	nrow=8
	ich=12
	call POPage(ixlo,iylo,iyhi,strin,nval,ic,icf,icup,ibk,title,
     & helps,nhelp,iline,charout,ival,nrow)
	print*,iline
	ans=getch(k)
	call fillwin(0,0,640,479,1)
	call mode(3)
	end

