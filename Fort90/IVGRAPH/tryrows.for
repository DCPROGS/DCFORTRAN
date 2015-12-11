	program tryrows
c================================================================
c    Test the subroutine POPLINES (click again option)
c    in which you select several lines with the keyboards or left mouse.
c    Del/Center mouse to cancel a line
c    Esc/Right mouse to exit
c    the array kline(1:ival) will contain the nr of lines selected
c    where imax is the max of lines selected
c    when a line is not selected kline(ival+1:nval)=-1
c    ex: kline(1)=10,kline (2)=3 and kline(3:nval)=-1 so you selected
c    the line/ strings 3 and 10
c================================================================
	character*30 strin(30),helps(5),title
	character*1 charout,ans,getch
	integer kline(30)
	real ax(10),ay(10)
	character*15 dtext(30)
	character*11 charar(30)
	character*20 addstr
c
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
	iy=5

	iline=3
	nhelp=2
	call POPlines(ixlo,-1,iyhi,strin,nval,ic,icf,icup,ibk,title,
     & helps,nhelp,iline,charout,ival,kline)
	if(ival.gt.0) then
	do i=1,ival
	     call intconv(kline(i),charar(i))
	     dtext(i)='line = '//charar(i)(1:3)
	     call set_cursor(3,iy)
	     call SET_STRING(dtext(i),12)
	     iy=iy+1
	enddo
	endif
	ans=getch(k)
	iline=3
	nhelp=2
	call POPlines(ixlo,-1,iyhi,strin,nval,ic,icf,icup,ibk,title,
     & helps,nhelp,iline,charout,ival,kline)
	iy=iy+3
	if(ival.gt.0) then
	do i=1,ival
	     call intconv(kline(i),charar(i))
	     dtext(i)='line = '//charar(i)(1:3)
	     call set_cursor(3,iy)
	     call SET_STRING(dtext(i),12)
	     iy=iy+1
	enddo
	endif
	ans=getch(k)
	addstr='fixed'
	iline=3
	nhelp=2
	call POPadd(ixlo,-1,iyhi,strin,nval,ic,icf,icup,ibk,title,
     & helps,nhelp,iline,charout,ival,kline,addstr)
	iy=iy+3
	if(ival.gt.0) then
	do i=1,ival
	     call intconv(kline(i),charar(i))
	     dtext(i)='line = '//charar(i)(1:3)
	     call set_cursor(3,iy)
	     call SET_STRING(dtext(i),12)
	     iy=iy+1
	enddo
	endif
	addstr='-1'
	ans=getch(k)
	iline=3
	nhelp=2
	call POPadd(ixlo,-1,iyhi,strin,nval,ic,icf,icup,ibk,title,
     & helps,nhelp,iline,charout,ival,kline,addstr)
	iy=iy+3
	if(ival.gt.0) then
	do i=1,ival
	     call intconv(kline(i),charar(i))
	     dtext(i)='line = '//charar(i)(1:3)
	     call set_cursor(3,iy)
	     call SET_STRING(dtext(i),12)
	     iy=iy+1
	enddo
	endif
	ans=getch(k)
	call mode(3)
	end

