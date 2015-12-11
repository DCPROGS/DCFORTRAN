	subroutine POPPARS(ixlo,iylo,iyhi,strings,header,helps,nhelp,
     &	nrow,nval,ic,ibk,icf,ich,options,nopt,iopt)
c Modification of POPTEXT that is designed to display long table of
c numbers (nval rows) in a window that shows NROW values at a time.
c UP/DOWN arrows scroll up and down the table
c PAGE UP/PAGE DOWN move up/down
c HEADER=constant column headings on top line
c Modif 12/02/93 07:27am so help strings (nhelp in number) are parameters
c  -these can describe what cols of table represent.  If present (nhelp>0)
c  they are added before the contents of help window below.
c Window can be moved by keys or dragging with mouse
c Assumes that we are in graphics (VGA, mode 18) before calling
c NB Assumes mouse_on in common was set by prelim call to SETMOUSE
c
c	IXLO,IYLO,IYHI: coordinates of lower left corner of the window
c Modif 11/21/93 09:24am so all three are parameters, but call with iyhi=-1
c		if window position to be defined by bottom left (ixlo,iylo)
c		OR call with iylo=-1 to if window position to be defined by
c		top left (ixlo,iyhi)
c	STRINGS(nval) : array of NVAL rows you want to write in the window
c	NVAL       : total number of rows
c	NROW	     : number of rows shown at a time in window
c	IC         : color of writing
C	ICF        : color frame
c	IBK        : color of background
c	ICH        : colour for header and footer
c
c     first time left button pressed: if inside menu draw frame;
c	if outside menu doesn't do anything; then move mouse,
c	draw/redraw frame at mouse position when left button pressed;
c	left button released -> draw menu at the new position
c	right button pressed -> go out
c     if you press the arrows keys (up/down/left/right) -> menu shift
c
	character*(*) strings(nval),helps(nhelp),header,options(10)
	character*1	ans,getch
	character*55 helpst(20)		!help strings
	integer*1 array1(:),array2(:)
	integer*1 arr1(:),arr2(:),arr3(:),arr4(:)
	logical kbhit,moved
	logical mouse_on,mous_set,outext
	common/mousval/mouse_on,nbutton	!common with SETMOUSE
	allocatable :: array1,array2
	allocatable :: arr1,arr2,arr3,arr4
c
c NB could skip mouse sections if istatus.ne.65535 (mouse_on=false)
	mous_set=.false.	!initialise
	outext=.false.
	iopt=1
	call show_mouse()
c
	if(nhelp.ge.1) then
	   do i=1,nhelp
		helpst(i)=helps(i)
	   enddo
	   helpst(nhelp+1)='--------------'
	   nhelp=nhelp+1	!for line above
	endif
	helpst(nhelp+1)='SCROLL VALUES:'
	helpst(nhelp+2)=' Arrow keys move up/down'
	helpst(nhelp+3)=' Page-up/down move 1 page'
	helpst(nhelp+4)=' Home/end go to start/end'
	helpst(nhelp+5)='MOVE WINDOW'
	helpst(nhelp+6)=' Use ctrl-arrow key'
	helpst(nhelp+7)=' Drag (L mouse button)'
	helpst(nhelp+8)='EXIT WINDOW'
	helpst(nhelp+9)=' Any other key'
	helpst(nhelp+10)=' or R mouse button'
	nhelp1=nhelp+10
c==	iyhi=-1	!now in call
	ixc0=-2	!leave 2 extra lines in WINDIM
	call windim(ixlo,iylo,ixhi,iyhi,idimen,strings,nrow+2+nrow,
     &ixc0,iyc0)
	idx=ixhi-ixlo
	idy=iyhi-iylo
	dx=float(idx+1)/2
	dy=4*float(idy+1)
	nh=int(dx)
	nv=int(dy)
	if(iyhi.gt.480) then
		iylo=480-idy
	endif
	if(iylo.lt.0) then
		iylo=0
	endif
	if(ixhi.gt.639) then
		ixlo=639-idx
	endif
c==	iyhi=-1	!now in call
	ixc0=-2
	call windim(ixlo,iylo,ixhi,iyhi,idimen,strings,nrow+2+nopt,
     & ixc0,iyc0)
	allocate(array1(idimen))
	allocate(array2(idimen))
	allocate(arr1(nh),arr2(nh),arr3(nv),arr4(nv))
	call hide_mouse()
	ixlo0=ixlo
	iylo0=iylo
	ixhi0=ixhi
	iyhi0=iyhi
	call rdgwin1(ixlo,iylo,ixhi,iyhi,array1)
	call FRAMEFILL(ixlo,iylo,ixhi,iyhi,icf,ibk)
	iyc=iyc0
c Write header
	call wrstring18(ixc0,iyc,header,ich,ibk)
	iyc=iyc-16
	iyc1=iyc	!store y value for first line of data
	iyc2=iyc1+18	!upper edge of area to clear so header not cleared
c Write first nrow values
	ifirst=1	!string shown in first row in window
	ilast=nrow	!string shown in last row in window
	if(ilast.gt.nval) ilast=nval
	do i=ifirst,ilast
	   call wrstring18(ixc0,iyc,strings(i),ic,ibk)
	   iyc=iyc-16
	enddo
	iyc3=iyc+12	!lower edge of area to clear so footer not cleared
c enter automatic last line
	call WRSTRING18(ixc0,iyc,'F1=HELP',ich,ibk)

	iyc=iyc-16
	do i=1,2
	   call wrstring18(ixc0,iyc,options(i),ich,ibk)
	   iyc=iyc-16
	enddo
c
	call rdgwin1(ixlo,iylo,ixhi,iyhi,array2)   !menu
	call rdframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,arr3,arr4,
     &			nh,nv)
	call show_mouse()
	ifst=1
	moved=.false.
c	ret=1
1	continue
      call show_mouse()
c Adjust ixc0,iyc1,iyc2,iyc3 in case window has been moved (set flag if moved?)
	if(moved) then
	   moved=.false.	!reset
	   ixc0=ixlo+8
	   iyc0=iyhi-32
	   iyc1=iyc0-16
	   iyc2=iyc1+18	!upper edge of area to clear so header not cleared
	   iyc3=iyc1-nrow*16+12	!lower edge of area to clear so footer not cleared
	endif
c

c	print*,'aici'
	if(kbhit()) then
	   ans=getch(ktype)
	   ival=ichar(ans)	!here you may give other options
c Arrow keys move whole window like mouse apart from up/down arrow (use ctrl-up
c and ctrl-down).  Left/right arrows move table.
c Up/down arrow; page-up/page-dn; home/end move the numbers.
	   if(ktype.eq.0.and.ival.eq.59) then	!F1=help
		ixloh=ixlo+50     !position for help box
		iyhih=iyhi-50
		if(ixhi.gt.639-50) ixloh=ixlo-50
c		if(iylo.lt.50) iyhih=iyhi+50
		if(iyhih.gt.480) iyhih=470
		call POPTEXT(ixloh,-1,iyhih,helpst,nhelp1,12,ibk,icf)	!=popkeys
		outext=.true.  !signal to wait for mouse reset after leaving poptext
		mous_set=.false.	!initialise
		call show_mouse()
		call set_mouse(320,240)
		goto 1
	   endif
	   if(ktype.eq.3.and.ival.ge.48.and.ival.le.57) then	!option
		call chtoreal(ans,ropt)
		if(int(ropt).le.nopt) iopt=int(ropt)
	  	call hide_mouse()
	  	call WRGWIN1(ixlo,iylo,ixhi,iyhi,array1)
	  	deallocate(array1)
	  	deallocate(array2)
	  	deallocate(arr1,arr2,arr3,arr4)
	  	goto 99
	   endif
	   if(ival.eq.141.or.ival.eq.145.or.ival.eq.116.or.
     &	ival.eq.115) then	!move menu with ctrl-ARROW keys
		moved=.true.
		call hide_mouse()
		call WRGWIN1(ixlo,iylo,ixhi,iyhi,array1)	!redraw orig graph
		if(ival.eq.116) then		!shift right
		   ixlo=ixlo+8
	         ixhi=ixhi+8
		else if(ival.eq.115) then		!shift left
	         ixlo=ixlo-8
	         ixhi=ixhi-8
		else if(ival.eq.141) then	!shift up
	         iylo=iylo+8
	         iyhi=iyhi+8
		else if(ival.eq.145) then	!shift down
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
	   else if(ival.eq.80) then		!down arrow
		call FILLWIN(ixlo+5,iyc3+5,ixhi-5,iyc2-5,ibk)
c		call FRAMEFILL(ixlo,iyc3,ixhi,iyc2,icf,ibk)	!clear window
		if(ifirst+nrow-1.lt.nval) then
		   ifirst=ifirst+1		!string shown in first row in window
		endif
		ilast=ifirst+nrow-1	!string shown in last row in window
		iyc=iyc1
		do i=ifirst,ilast
		   call wrstring18(ixc0,iyc,strings(i),ic,ibk)
		   iyc=iyc-16
		enddo
	   else if(ival.eq.72) then		!up arrow
c		call FRAMEFILL(ixlo,iyc3,ixhi,iyc2,icf,ibk)	!clear window
		call FILLWIN(ixlo+5,iyc3+5,ixhi-5,iyc2-5,ibk)
		ifirst=ifirst-1		!string shown in first row in window
		ilast=ifirst+nrow-1	!string shown in last row in window
		if(ifirst.lt.1) then
		   ifirst=1
		   ilast=ifirst+nrow-1	!string shown in last row in window
		endif
		if(ilast.gt.nval) ilast=nval
		iyc=iyc1
		do i=ifirst,ilast
		   call wrstring18(ixc0,iyc,strings(i),ic,ibk)
		   iyc=iyc-16
		enddo
	   else if(ival.eq.73) then		!page-up
		call FILLWIN(ixlo+5,iyc3+5,ixhi-5,iyc2-5,ibk)
c		call FRAMEFILL(ixlo,iyc3,ixhi,iyc2,icf,ibk)	!clear window
		ifirst=ifirst-nrow	!string shown in first row in window
		ilast=ifirst+nrow-1	!string shown in last row in window
		if(ifirst.lt.1) then
		   ifirst=1
		   ilast=ifirst+nrow-1	!string shown in last row in window
		endif
		if(ilast.gt.nval) ilast=nval
		iyc=iyc1
		do i=ifirst,ilast
		   call wrstring18(ixc0,iyc,strings(i),ic,ibk)
		   iyc=iyc-16
		enddo
	   else if(ival.eq.81) then		!page-down
		call FILLWIN(ixlo+5,iyc3+5,ixhi-5,iyc2-5,ibk)
c		call FRAMEFILL(ixlo,iyc3,ixhi,iyc2,icf,ibk)	!clear window
		ifirst=ifirst+nrow	!string shown in first row in window
		ilast=ifirst+nrow-1	!string shown in last row in window
		if(ilast.gt.nval) then
		   ilast=nval
		   ifirst=ilast-nrow+1
		   if(ifirst.lt.1) ifirst=1
		endif
		iyc=iyc1
		do i=ifirst,ilast
		   call wrstring18(ixc0,iyc,strings(i),ic,ibk)
		   iyc=iyc-16
		enddo
	   else if(ival.eq.71) then		!home
		call FILLWIN(ixlo+5,iyc3+5,ixhi-5,iyc2-5,ibk)
c		call FRAMEFILL(ixlo,iyc3,ixhi,iyc2,icf,ibk)	!clear window
		ifirst=1	!string shown in first row in window
		ilast=nrow	!string shown in last row in window
		if(ilast.gt.nval) ilast=nval
		iyc=iyc1
		do i=ifirst,ilast
		   call wrstring18(ixc0,iyc,strings(i),ic,ibk)
		   iyc=iyc-16
		enddo
	   else if(ival.eq.79) then		!end
		call FILLWIN(ixlo+5,iyc3+5,ixhi-5,iyc2-5,ibk)
c		call FRAMEFILL(ixlo,iyc3,ixhi,iyc2,icf,ibk)	!clear window
		ilast=nval
		ifirst=ilast-nrow+1
		if(ifirst.lt.1) ifirst=1
		iyc=iyc1
		do i=ifirst,ilast
		   call wrstring18(ixc0,iyc,strings(i),ic,ibk)
		   iyc=iyc-16
		enddo
c	   else if(ival.eq.27.or.ival.eq.13) then
	   else		!any other key to exit
	  	call hide_mouse()
	  	call WRGWIN1(ixlo,iylo,ixhi,iyhi,array1)
	  	deallocate(array1)
	  	deallocate(array2)
	  	deallocate(arr1,arr2,arr3,arr4)
	  	goto 99
	   endif
     	call show_mouse()
	goto 1
	endif       !end of kbhit
c
c NB check mous_set after kbhit section because could have multiple-click
c effect after leaving poptext
	if(mouse_on) then
         call show_mouse()
22	   call mouse_update(ibutton,ix,iy)
	   if(outext.and.ibutton.ne.0) goto 22
		outext=.false.
c==================================================
	   if(ibutton.eq.0) mous_set=.true.
	endif
	if(mous_set) then
	 if(ibutton.ne.0) mous_set=.false.	!until reset above
	 if(ibutton.eq.1) then
	  if(ix.lt.ixhi.and.ix.gt.ixlo.and.iy.lt.iyhi.and.
     &			iy.gt.iylo+8) then
		moved=.true.
		idcx=abs(ix-ixlo0)
		idcy=abs(iy-iylo0)
		call hide_mouse()
		call drawframe(ixlo,iylo,ixhi,iyhi,icf)
2		call mouse_update(ibutton,ix,iy)
		if (ibutton.eq.1) then
			if(ix.eq.ix0.and.iy.eq.iy0) goto 3
			call hide_mouse()
			if(ifst.eq.1) then
			   call drawframe(ixlo,iylo,ixhi,iyhi,ibk)
			   ifst=0
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
			if(iyhi.gt.480) then
			   iyhi=480
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
		else if(ibutton.eq.0) then
			ifst=1
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
		call show_mouse()
		goto 1
	  endif
	 else if(ibutton.eq.2) then
c		call set_mouse(640,480)
		call hide_mouse()
		call wrgwin1(ixlo0,iylo0,ixhi0,iyhi0,array1)
c		call set_mouse(ix,iy)
		deallocate(array1,array2)
		deallocate(arr1,arr2,arr3,arr4)
		goto 99
	 else
		goto 1
	 endif
	endif		!end of if(mous_set)
99	continue
	RETURN
	end
