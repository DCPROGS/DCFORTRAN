c*******************************************************************
c ITPOP1 tests DC versions: POPTEXT, POPMENU
c	I Vais
c     Pharmacology
c
c*******************************************************************

	program ITPOP1

c*******************************************************************
C	PROGRAM TO TEST POPKEYS : simple menu dragged by mouse and arrow keys
c			    POPALL  : menu+scrolling bar, dragged by mouse (title='nil')
c					: menu+scrolling bar+mouse click ,dragged by mouse
c					  (title='box'/title)

c*******************************************************************

	character*30 strin(5),title
	character*1	charout

	strin(1)='First boring string'
	strin(2)='Second handsome string'
	strin(3)='Third new string'
	strin(4)='Big fourth string'
c	strin(5)='Esc to exit'
	n=4

	call mode(18)
	call SETMOUSE()	!sets values in common/mousval/
	ixlo=320
c	iylo=240
	iyhi=240	!DEFINE TOP R CORNER
	ic=15
	ibk=8
	icup=12

c*******************************************************************

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
c==	iylo=200
c	call show_mouse()		!inside poptext
c	call POPTEXT(ixlo,iylo,strin,n,ic,ibk,icup)
c	call POPTEXT(ixlo,iylo,iyhi,strin,n,ic,ibk,icup)
	call POPTEXT(ixlo,-1,iyhi,strin,n,ic,ibk,icup)	!define top R corner
c	iylo=200
c	ixlo=50
c	call popkeys(ixlo,iylo,'1',1,ic,ibk,icup)
c	ixlo=100
c	call popkeys(ixlo,iylo,'2',1,ic,ibk,icup)
c	ixlo=150
c	call popkeys(ixlo,iylo,'3',1,ic,ibk,icup)
c	ixlo=200
c	call popkeys(ixlo,iylo,'4',1,ic,ibk,icup)
c	ixlo=250
c	call popkeys(ixlo,iylo,'5',1,ic,ibk,icup)
cc*******************************************************************

	call hide_mouse()
	call clears(0,480,1)
	title='nil'
	iy=0
	do while(iy.lt.480)		!draw some background graphics
	   call HLINE(0,639,iy,10)
	   iy=iy+20
	enddo
	ix=0
	do while(ix.lt.639)		!draw some background graphics
	   call VLINE(ix,0,479,11)
	   ix=ix+20
	enddo
	call show_mouse()
	ixlo=100
c==	iylo=300
	iyhi=400
c    	call POPMENU(ixlo,iylo,strin,n,ic,icup,ibk,title,iret,charout,
c     &		ivalue)
c    	call POPMENU(ixlo,iylo,iyhi,strin,n,ic,icup,ibk,title,iret,charout,
c     &		ivalue)
     	call POPMENU(ixlo,-1,iyhi,strin,n,ic,icup,ibk,title,iret,charout,
     &		ivalue)
	CALL LOCATE(20,0)
	print 10,iret,charout,ivalue
	call anykey
c*******************************************************************

	call hide_mouse()
	call clears(0,480,1)
	title='box'

	iy=0
	do while(iy.lt.480)		!draw some background graphics
	   call HLINE(0,639,iy,10)
	   iy=iy+20
	enddo
	ix=0
	do while(ix.lt.639)		!draw some background graphics
	   call VLINE(ix,0,479,11)
	   ix=ix+20
	enddo
	call show_mouse()
	ixlo=100
	iylo=300
     	call POPMENU(ixlo,iylo,-1,strin,n,ic,icup,ibk,title,iret,charout,
     &		ivalue)
	CALL LOCATE(20,0)
	print 10,iret,charout,ivalue
	call anykey

c*******************************************************************

	call hide_mouse()
	call clears(0,480,1)
	title='ILVMENUS'
	iy=0
	do while(iy.lt.480)		!draw some background graphics
	   call HLINE(0,639,iy,10)
	   iy=iy+20
	enddo
	ix=0
	do while(ix.lt.639)		!draw some background graphics
	   call VLINE(ix,0,479,11)
	   ix=ix+20
	enddo
	call show_mouse()
	ixlo=100
c	iylo=300
	iyhi=240
     	call POPMENU(ixlo,-1,iyhi,strin,n,ic,icup,ibk,title,iret,charout,
     &		ivalue)
	CALL LOCATE(20,0)
	print 10,iret,charout,ivalue
	call anykey

c*******************************************************************
	call hide_mouse()
	call clears(0,480,1)
	title='TITLE'
	iy=0
	do while(iy.lt.480)		!draw some background graphics
	   call HLINE(0,639,iy,10)
	   iy=iy+20
	enddo
	ix=0
	do while(ix.lt.639)		!draw some background graphics
	   call VLINE(ix,0,479,11)
	   ix=ix+20
	enddo
	call show_mouse()
	ixlo=100
	iyhi=450
     	call POPMENU(ixlo,-1,iyhi,strin,n,ic,icup,ibk,title,iret,charout,
     &		ivalue)
	CALL LOCATE(20,0)
	print 10,iret,charout,ivalue
	call anykey

c*******************************************************************

	call clears(0,480,1)

	call mode(3)
	print 10,iret,charout,ivalue
10	format(
     & ' line number = ',i3,'; character = ',a1,'; ASCII value = ',i3)
	pause

	end

