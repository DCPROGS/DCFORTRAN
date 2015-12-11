	character*30 strin(10),header
c
	header=' header'
	strin(1)='string #1'
	strin(2)='string #2'
	strin(3)='string #3'
	strin(4)='string #4'
	strin(5)='string #5'
	strin(6)='string #6'
	strin(7)='string #7'
	strin(8)='string #8'
	strin(9)='string #9'
	strin(10)='string #10'
	nval=10
	nrow=4
c
	call mode(18)
	call SETMOUSE()	!sets values in common/mousval/
	ixlo=320
c	iylo=240
	iyhi=470	!DEFINE TOP R CORNER
	ic=15
	ibk=8
	ich=12
c
	call clears(0,480,1)
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
c
c	call POPTEXT(ixlo,-1,iyhi,strin,n,ic,ibk,icup)	!define top R corner
	call POPTABLE(ixlo,-1,iyhi,strin,header,helps,0,
     &	nrow,nval,ic,ibk,icf,ich)
	call anykey
	call clears(0,480,1)
	call mode(3)
	end

