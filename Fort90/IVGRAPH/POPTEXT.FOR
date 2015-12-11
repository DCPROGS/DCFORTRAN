	subroutine POPTEXT(ixlo,iylo,iyhi,strings,n,ic,ibk,icf)

c*************************************************************************
c
c	Ioana Vais
c	Pharmacology
c     David's subroutine POPUP1 with mouse dragging
c
c*************************************************************************

c DC modif of POPKEYS that inserts quit help automatically on bottom line
c Designed to put up text (eg help info) with no 'menu choice'
c Window can be moved by keys or dragging with mouse
c
c	Assumes that we are in graphics (VGA, mode 18) before calling
c
c	IXLO,IYLO,IYHI: coordinates of lower left corner of the window
c Modif 11/21/93 09:24am so all three are parameters, but call with iyhi=-1
c		if window position to be defined by bottom left (ixlo,iylo)
c		OR call with iylo=-1 to if window position to be defined by
c		top left (ixlo,iyhi)
c	STRINGS(N) : array of strings you want to write in the window
c	N          : array dimension
c	IC         : color of writing
C	ICF        : color frame
c	IBK        : color of background

c     first time left button pressed: if inside menu draw frame;
c	if outside menu doesn't do anything; then move mouse,
c	draw/redraw frame at mouse position when left button pressed;
c	left button released -> draw menu at the new position
c	right button pressed -> go out
c     if you press the arrows keys (up/down/left/right) -> menu shift
c*************************************************************************
c
c	CALLING : WINDIM,FRAMEFILL,RDFRAME,WRFRAME,DRAWFRAME
C		  : SHOW_MOUSE,HIDE_MOUSE,MOUSE_UPDATE,SET_MOUSE
C	        : WRSTRING18
C		  : RDGWIN1,WRGWIN1
C
c*************************************************************************



	character*(*) strings(n)
	character*1	ans,getch
	integer*1 array1(:),array2(:)
	integer*1 arr1(:),arr2(:),arr3(:),arr4(:)
	logical kbhit
	logical mouse_on
	common/mousval/mouse_on,nbutton
	allocatable :: array1,array2
	allocatable :: arr1,arr2,arr3,arr4
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi
c
c NB could skip mouse sections if istatus.ne.65535 (mouse_on=false)
	call show_mouse()
c
c==	iyhi=-1	!now in call
	ixc0=-1
	size=3.
	if(ic.eq.-2) size=12.*25.4/72.
	if(ic.eq.-3) size=size*vyhi/159.7

c NB do not call with n+1 because then would attempt to look at strings(n+1)
c which may not exist -call with ixc0=-1 instead
	if(ic.eq.-1) n = n/2+1
	call windim(ixlo,iylo,ixhi,iyhi,idimen,strings,n,ixc0,iyc0)
	idx=ixhi-ixlo
	idy=iyhi-iylo
	if(ic.eq.-2) then
		ixlo=ixlo-8
		ixhi=ixhi+8
		iylo=iylo-idy+48
	      idy=iyhi-iylo
	      idx=ixhi-ixlo
	endif
	dx=float(idx+1)/2
	dy=4*float(idy+1)
	nh=int(dx)
	nv=int(dy)
	if(iyhi.gt.479) then
	   iyhi=479
	   iylo=479-idy
	endif
	if(iylo.lt.0) then
	   iylo=0
	   iyhi=idy
	endif
	if(ixhi.gt.639) then
	   ixhi=639
	   ixlo=639-idx
	endif
	if(ixlo.lt.0) then
	   ixlo=0
	   ixhi=idx
	endif
c==	iyhi=-1	!now in call
	ixc0=-1
c NB do not call with n+1 because then would attempt to look at strings(n+1)
c which may not exist -call with ixc0=-1 instead
	call windim(ixlo,iylo,ixhi,iyhi,idimen,strings,n,ixc0,iyc0)
	if(ic.eq.-2.or.ic.eq.-3) then
		ixhi=ixhi+8
		ixlo=ixhi-idx
		iylo=iyhi-idy
	      idimen=(iyhi-iylo+9)*(ixhi-ixlo+9)+1
	endif
	allocate(array1(idimen))
	allocate(array2(idimen))
	allocate(arr1(nh),arr2(nh),arr3(nv),arr4(nv))
	call hide_mouse()
	ixlo0=ixlo
	iylo0=iylo
	ixhi0=ixhi
	iyhi0=iyhi
	ixc1=ixc0+(ixhi-ixlo)/2-8
	call rdgwin1(ixlo,iylo,ixhi,iyhi,array1)
	call FRAMEFILL(ixlo,iylo,ixhi,iyhi,icf,ibk)
	iyc=iyc0
	if(ic.eq.-1.or.ic.eq.-2.or.ic.eq.-3) then
	   call chajus(-1)
	   call setsize(size)
	   call setfnt(1)
	   if(ic.eq.-3) then
		xc0=(ixc0+16)*vxhi/640.
		yc=iyc*vyhi/480.
	   else
	      call pixpos(ixc0,iyc,xc0,yc)
	      yc=159.7-yc
	   endif
	   call movto2(xc0,yc)
	   call lincols(4)
	   call chastr(strings(1))
	   iyc=iyc-16
	   if(ic.eq.-2) iyc=iyc-16
	   i=2
	   call lincols(icf)
	   if(ic.eq.-2.or.ic.eq.-3) then
		do i=2,n
	         if(ic.eq.-3) then
		      xc0=(ixc0+16)*vxhi/640.
		      yc=iyc*vyhi/480.
	         else
	            call pixpos(ixc0,iyc,xc0,yc)
		      yc=159.7-yc
		   endif
		   call movto2(xc0,yc)
		   call setfnt(i-2)
	         call chastr(strings(i))
		   iyc=iyc-16
	   	   if(ic.eq.-2) iyc=iyc-16
		enddo
		goto 111
	   else
	      do j=1,n-1
	         call pixpos(ixc0,iyc,xc0,yc)
		   yc=159.7-yc
		   call movto2(xc0,yc)
	         call chastr(strings(i))
		   if(i.le.(2*n-2)) then
	            call pixpos(ixc1,iyc,xc1,yc)
		      yc=159.7-yc
		      call movto2(xc1,yc)
	            call chastr(strings(i+1))
		   endif
		   iyc=iyc-16
		   i=i+2
	      enddo
	   endif
 	   call pixpos(ixc0,iyc,xc0,yc)
 	   yc=159.7-yc
 	   call movto2(xc0,yc)
 	   if(ic.ne.-3) call chastr('Any key/R mouse to quit')
111	   call chajus(0)
	   call setfnt(0)
	else
	   do i=1,n
		call wrstring18(ixc0,iyc,strings(i),ic,ibk)
		iyc=iyc-16
	   enddo
c        enter automatic last line
	   call WRSTRING18(ixc0,iyc,'Any key/R mouse to quit',12,ibk)
		iyc=iyc-16
	endif
c

	call rdgwin1(ixlo,iylo,ixhi,iyhi,array2)   !menu
	call rdframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,arr3,arr4,
     &			nh,nv)
	call show_mouse()
	ifirst=1
c	ret=1
1	call mouse_update(ibutton,ix,iy)
	if(kbhit()) then
	   ans=getch(ktype)
	   ival=ichar(ans)	!here you may give other options
	   if(ktype.eq.8) then	!move menu with ARROW keys
		call hide_mouse()
		call WRGWIN1(ixlo,iylo,ixhi,iyhi,array1)	!redraw orig graph
		if(ival.eq.73.or.ival.eq.77.or.ival.eq.81) then		!shift right
		   ixlo=ixlo+8
	         ixhi=ixhi+8
		endif
		if(ival.eq.71.or.ival.eq.75.or.ival.eq.79) then		!shift left
	         ixlo=ixlo-8
	         ixhi=ixhi-8
		endif
		if(ival.eq.71.or.ival.eq.72.or.ival.eq.73) then		!shift up
	         iylo=iylo+8
	         iyhi=iyhi+8
		endif
		if(ival.eq.79.or.ival.eq.80.or.ival.eq.81) then		!shift down
	         iylo=iylo-8
	         iyhi=iyhi-8
		endif
		call RDGWIN1(ixlo,iylo,ixhi,iyhi,array1)  !keep new orig in array1
		call WRGWIN1(ixlo,iylo,ixhi,iyhi,array2)	!redraw menu at new pos
			ixlo0=ixlo
			iylo0=iylo
			ixhi0=ixhi
			iyhi0=iyhi
        	call show_mouse()
		goto 1
	   else if(ival.eq.27.or.ival.eq.13) then
C	   else		!any other key to exit
	  	call hide_mouse()
	  	call WRGWIN1(ixlo,iylo,ixhi,iyhi,array1)
	  	deallocate(array1)
	  	deallocate(array2)
	  	deallocate(arr1,arr2,arr3,arr4)
        	call show_mouse()
	  	goto 99
	   endif
	endif
	if (ibutton.eq.1) then
	  if(ix.lt.ixhi.and.ix.gt.ixlo.and.iy.lt.iyhi.and.
     &			iy.gt.iylo+8) then
		idcx=abs(ix-ixlo0)
		idcy=abs(iy-iylo0)
		call hide_mouse()
		call drawframe(ixlo,iylo,ixhi,iyhi,icf)
2		call mouse_update(ibutton,ix,iy)
		if (ibutton.eq.1) then
			if(ix.eq.ix0.and.iy.eq.iy0) goto 3
			call hide_mouse()
			if(ifirst.eq.1) then
			   call drawframe(ixlo,iylo,ixhi,iyhi,ibk)
			   ifirst=0
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
     &		   nh,nv)
			call drawframe(ixlo,iylo,ixhi,iyhi,icf)
3			continue
			ix0=ix
			iy0=iy
			call show_mouse()
			goto 2
		else if (ibutton.eq.0) then
			ifirst=1
			call hide_mouse()
			call wrframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &		   arr3,arr4,nh,nv)
			if(ixlo.lt.0.or.ixhi.ge.640.or.
     &		   iylo.lt.0.or.iyhi.ge.480) then
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
	else if(ibutton.eq.2) then
		call set_mouse(640,480)
		call wrgwin1(ixlo0,iylo0,ixhi0,iyhi0,array1)
		call set_mouse(ix,iy)
		deallocate(array1,array2)
		deallocate(arr1,arr2,arr3,arr4)
		goto 99
	else
		goto 1
	endif
99	continue
	call hide_mouse()
      call setfnt(0)

	RETURN
	end
