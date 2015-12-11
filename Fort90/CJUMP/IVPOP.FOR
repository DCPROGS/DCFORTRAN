c*************************************************************************
c
c	Ioana Vais
c	Pharmacology
c
c*************************************************************************

	subroutine ivpop(ixlo,iylo,strings,values,n,ic,ibk,icf)

c*************************************************************************

c	SUBROUTINE TO DRAW A MENU WITH READ/WRITE WINDOW INCLUDED

c	the mode used is 18 (12h)
c	IXLO,IYLO  : coordonates of lower left corner of the window; I gave it like
c	             this because is easier to control mouse;
c	STRINGS(N) : array of strings you want to write in the window
c	N          : array dimension
c	IC         : color of writing
C	ICF        : color frame
c	IBK        : color of background

c     first time left button pressed: if inside menu draw frame;
c	if outside menu doesn't do anything; then move mouse,
c	draw/redraw frame at mouse position when left button pressed;
c	left button released -> draw menu at the new position
c	right button pressed -> go out (outside menu area)
c				   -> on the selected string give possibility
c                          -> for edit calling VTITENT
c     if you press the arrows keys (up/down/left/right) -> menu shift
c*************************************************************************
c
c	CALLING : WINDIM,FRAMEFILL,RDFRAME,WRFRAME,DRAWFRAME
C		  : SHOW_MOUSE,HIDE_MOUSE,MOUSE_UPDATE,SET_MOUSE
C	        : WRSTRING18
C		  : RDGWIN1,WRGWIN1
C             : VTITENT
c*************************************************************************



	character*(*)	strings(n)
	character*1		ans,getch
	integer*1		array1(:),array2(:)
	integer*1		arr1(:),arr2(:),arr3(:),arr4(:)
	logical		kbhit
	logical mouse_on
	common/mousval/mouse_on,nbutton
	allocatable :: array1,array2
	allocatable :: arr1,arr2,arr3,arr4

	mouse_on=.true.
	iyhi=-1
	call windim(ixlo,iylo,ixhi,iyhi,idimen,strings,n,ixc0,iyc0)
	idx=ixhi-ixlo
	idy=iyhi-iylo
	dx=float(idx+1)/2
	dy=4*float(idy+1)
	nh=int(dx)
	nv=int(dy)
	if(iyhi.gt.479) then
		iylo=479-idy
	endif
	if(iylo.lt.0) then
		iylo=0
	endif
	if(ixhi.gt.639) then
		ixlo=639-idx
	endif
	iyhi=-1
	call windim(ixlo,iylo,ixhi,iyhi,idimen,strings,n,ixc0,iyc0)
	allocate(array1(idimen))
	allocate(array2(idimen))
	allocate(arr1(nh),arr2(nh),arr3(nv),arr4(nv))
	call hide_mouse()
	ixlo0=ixlo
	iylo0=iylo
	ixhi0=ixhi
	iyhi0=iyhi
	call rdgwin1(ixlo,iylo,ixhi,iyhi,array1)
	call FRAMEFILL(ixlo,iylo,ixhi,iyhi,ic,ibk)
	iyc=iyc0
	do i=1,n
		call wrstring18(ixc0,iyc,strings(i),ic,ibk)
		iyc=iyc-16
	enddo
	call rdgwin1(ixlo,iylo,ixhi,iyhi,array2)   !menu
	call rdframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,arr3,arr4,
     &			nh,nv)
	call show_mouse()
	ret=1
1	call mouse_update(ist,ix,iy)
	if (kbhit()) then
	   ans=getch(ktype)
	   ival=ichar(ans)	!here you may give other options
	   if(ktype.eq.8) then	!move menu with ARROW keys
		call hide_mouse()
		call WRGWIN1(ixlo,iylo,ixhi,iyhi,array1)	!redraw orig graph
		if(ival.eq.73.or.ival.eq.77.or.ival.eq.81) then		!shift right
		 if(ixhi.le.632)then
		   ixlo=ixlo+8
	         ixhi=ixhi+8
		 else
		   goto 10
		 endif
		endif
		if(ival.eq.71.or.ival.eq.75.or.ival.eq.79) then		!shift left
		 if(ixlo.ge.8)then
	         ixlo=ixlo-8
	         ixhi=ixhi-8
		 else
		   goto 10
		 endif
		endif
		if(ival.eq.71.or.ival.eq.72.or.ival.eq.73) then		!shift up
		 if(iyhi.le.464)then
	         iylo=iylo+16
	         iyhi=iyhi+16
		 else
		   goto 10
		 endif
		endif
		if(ival.eq.79.or.ival.eq.80.or.ival.eq.81) then		!shift down
		 if(iylo.ge.16)then
	         iylo=iylo-16
	         iyhi=iyhi-16
		 else
		   goto 10
		 endif
		endif
10		call RDGWIN1(ixlo,iylo,ixhi,iyhi,array1)  !keep new orig in array1
		call WRGWIN1(ixlo,iylo,ixhi,iyhi,array2)	!redraw menu at new pos
			ixlo0=ixlo
			iylo0=iylo
			ixhi0=ixhi
			iyhi0=iyhi
        	call show_mouse()
		goto 1
	   else if(ival.eq.27.or.ival.eq.13) then
	  	call hide_mouse()
	  	call WRGWIN1(ixlo,iylo,ixhi,iyhi,array1)
	  	deallocate(array1)
	  	deallocate(array2)
	  	deallocate(arr1,arr2,arr3,arr4)
        	call show_mouse()
	  	goto 99
	   endif
	endif
	if (ist.eq.1) then
	  if(ix.lt.ixhi.and.ix.gt.ixlo.and.iy.lt.iyhi.and.
     &			iy.gt.iylo+8) then
		idcx=abs(ix-ixlo0)
		idcy=abs(iy-iylo0)
		call hide_mouse()
		call drawframe(ixlo,iylo,ixhi,iyhi,icf)
2		call mouse_update(ist,ix,iy)
		if (ist.eq.1) then
			if(ix.eq.ix0.and.iy.eq.iy0) goto 3
			call hide_mouse()
			if(ret.eq.1) then
				call drawframe(ixlo,iylo,ixhi,iyhi,ibk)
				ret=0
			else
			call wrframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &				arr3,arr4,nh,nv)
			endif
			ixlo=(ix-idcx)-mod((ix-idcx),8)  !
			iylo=(iy-idcy)-mod((iy-idcy),16) !
			if(ixlo.lt.0) then
				ixlo=0
			endif
			if(iylo.lt.0) then
				iylo=0
			endif
			iyhi=iylo+idy
			ixhi=ixlo+idx
			if(ixhi.gt.639) then
				ixhi=639
				ixlo=ixhi-idx
			endif
			if(iyhi.gt.479) then
				iyhi=479
				iylo=iyhi-idy
			endif
			call rdframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,arr3,arr4,
     &			nh,nv)
			call drawframe(ixlo,iylo,ixhi,iyhi,icf)
3			continue
			ix0=ix
			iy0=iy
			call show_mouse()
			goto 2
		else if (ist.eq.0) then
			ret=1
			call hide_mouse()
			call wrframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &				arr3,arr4,nh,nv)
			if(ixlo.lt.0.or.ixhi.ge.640.or.
     &			iylo.lt.0.or.iyhi.ge.480) then
				ixlo=ixlo0
				iylo=iylo0
				ixhi=ixhi0
				iyhi=iyhi0
			endif
			call wrgwin1(ixlo0,iylo0,ixhi0,iyhi0,array1)
			call rdgwin1(ixlo,iylo,ixhi,iyhi,array1)
			call wrgwin1(ixlo,iylo,ixhi,iyhi,array2)
			ixlo0=ixlo
			iylo0=iylo
			ixhi0=ixhi
			iyhi0=iyhi
			ix0=ix
			iy0=iy
			call show_mouse()
			goto 1
		else
			call hide_mouse()
			call wrframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &				arr3,arr4,nh,nv)
			call wrgwin1(ixlo0,iylo0,ixhi0,iyhi0,array1)
			deallocate(array1,array2)
			deallocate(arr1,arr2,arr3,arr4)
			call show_mouse()
			goto 99
		endif
	  else
		goto 1
	  endif
	else if(ist.eq.2) then
	   if(ix.lt.ixhi.and.ix.gt.ixlo.and.iy.gt.iylo.and.
     &	iy.lt.iyhi) then
	    	if(iy.gt.iylo+16.and.iy.lt.iyhi-16) then  !change string if you wish
			ky=iy-mod(iy+16,16)
			k=n-int(float(iy-iylo-16)/16)
			call hide_mouse()
			call modstr(ixlo,ixhi,ky,strings,n,k,ic,ibk,icf)
	     		call rdgwin1(ixlo,iylo,ixhi,iyhi,array2)
 			call show_mouse()
			goto 1
		else
			goto 1
		endif
	  else
			call hide_mouse()
			call wrframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &				arr3,arr4,nh,nv)
			call wrgwin1(ixlo0,iylo0,ixhi0,iyhi0,array1)
			deallocate(array1,array2)
			deallocate(arr1,arr2,arr3,arr4)
			call show_mouse()
			goto 99
	  endif
	else
		goto 1
	endif
99	end
c*******************************************************************************
