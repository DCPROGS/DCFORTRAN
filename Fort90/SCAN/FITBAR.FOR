	subroutine FITBAR(ix,dx,if1,if2,ndfit,yd1,yd2,basevga,
     & opendown,adjust,isavb1,isavb2,isavb3)
c To draw, and adjust, the bar marking the fitted range in SCAN
c	yd1,yd2 are boundaries of data region of screen (pixel units)
c	ix() is ordinate for drawing data points (pixel units)
c	adjust=false to draw only
c	adjust=true to adjust a line that has ALREADY been drawn
c		from ix(if1) to ix(if2), and return new values of if1, if2
c
	character*55 helpst(7)		!help strings
	integer*2 ix(2048)
	integer*1 isavb1(320),isavb2(320),isavb3(320) !store pixels
	character*1 ch,getch
	integer*2 ktype
	logical opendown,adjust,mouse_on,mous_set,kbhit
	common/mousval/mouse_on,nbutton
	common/popvalx/mxlo,mylo,myhi,ictx,ibkx,icfx	!values for poptext calls
c
	if(adjust) goto 1
c Display bar to mark fitted region
	if(opendown) then
	   iy=ifix(0.5*(yd2+basevga))       !bar above trace
	   iy1=iy-15
c	   call VLINE(ix(if1),iy1,iy,12)
c	   call VLINE(ix(if2),iy1,iy,12)
	else
	   iy=ifix(0.5*(yd1+basevga))       !bar below trace
	   iy1=iy+15
c	   call VLINE(ix(if1),iy,iy1,12)
c	   call VLINE(ix(if2),iy,iy1,12)
	endif
	call RDGWIN1(ix(if1),iy,ix(if2),iy,isavb1)
	call RDGWIN1(ix(if1),iy,ix(if1),iy1,isavb2)
	call RDGWIN1(ix(if2),iy,ix(if2),iy1,isavb3)
	call HLINE(ix(if1),ix(if2),iy,12)
	call VLINE(ix(if1),iy,iy1,12)	!do here once req for y2>y1 fixed
	call VLINE(ix(if2),iy,iy1,12)
	RETURN
c
c Adjust region to be fitted if nec (pause to ask if OK, or use default 1st
c time and then give chance to alter it?)
c Use left and right arrow keys to alter if1,and ctrl-l/r arrow to alter if2
1	continue
	mous_set=.false.	!initialise
c
	helpst(1)='Arrow keys move low end:'
	helpst(2)='  ctrl-arrow moves high end;'
	helpst(3)='  hit <enter> when done.'
	nhelp=3
	if(mouse_on) then
	   helpst(4)='OR'
	   helpst(5)='Mark low end with left mouse button;'
	   helpst(6)='mark high end with right mouse button;'
	   helpst(7)='<enter>, or both buttons when done.'
	   nhelp=7
	endif
10	continue
	if(kbhit()) then
	 ch=GETCH(ktype)
	 ival=ICHAR(ch)
	 if(ktype.eq.0.and.ival.eq.59) then		!F1=help
	    call POPTEXT(mxlo,mylo,myhi,helpst,nhelp,ictx,ibkx,icfx)
	 else if(ktype.eq.8) then
c	restore orig graph before if1,if2 altered
	   call HIDE_MOUSE()
	   call WRGWIN1(ix(if1),iy,ix(if2),iy,isavb1)
	   call WRGWIN1(ix(if1),iy,ix(if1),iy1,isavb2)
	   call WRGWIN1(ix(if2),iy,ix(if2),iy1,isavb3)
	   call SHOW_MOUSE()
	   if(ival.eq.75) then		!left arrow
		if1=if1-1
		if(if1.lt.1) if1=1
	   else if(ival.eq.77) then	!right arrow
		if1=if1+1
		if(if1.gt.if2-1) if1=if2-1
	   else if(ival.eq.115) then		!ctrl-left arrow
		if2=if2-1
		if(if2.lt.if1+1) if2=if1+1
	   else if(ival.eq.116) then	!ctrl-right arrow
		if2=if2+1
		if(if2.gt.ndfit) if2=ndfit
	   endif
c       and then draw new bar
	   call HIDE_MOUSE()
	   call RDGWIN1(ix(if1),iy,ix(if2),iy,isavb1)
	   call RDGWIN1(ix(if1),iy,ix(if1),iy1,isavb2)
	   call RDGWIN1(ix(if2),iy,ix(if2),iy1,isavb3)
	   call HLINE(ix(if1),ix(if2),iy,12)
	   call VLINE(ix(if1),iy,iy1,12)	!do here once req for y2>y1 fixed
	   call VLINE(ix(if2),iy,iy1,12)
	   call SHOW_MOUSE()
	 else if(ival.eq.13) then	!enter
	   goto 99
	 endif	!end of ktype.eq.8
	endif		!end of kbhit()
	if(mouse_on) then
c (get_button updates ix,iy only when left button hit if ident=0;
c             updates ix,iy only when right button hit if ident=1)
c	    call get_button(ibutton,ident,icount,ix2,iy2)
	    call get_mouse(ibutton,ix2,iy2)
	    if(ibutton.eq.0) mous_set=.true.
	    call SHOW_MOUSE()
	endif
	if(mous_set) then
c	   call get_button(ibutton,ident,icount,ix1,iy) !already got ibutton above!
	   if(ibutton.ne.0) then
		mous_set=.false.	!until reset above
c	restore orig graph before if1,if2 altered
	      call HIDE_MOUSE()
	      call WRGWIN1(ix(if1),iy,ix(if2),iy,isavb1)
	      call WRGWIN1(ix(if1),iy,ix(if1),iy1,isavb2)
	      call WRGWIN1(ix(if2),iy,ix(if2),iy1,isavb3)
	      call SHOW_MOUSE()
		if(ibutton.eq.1) then		!set left end
c NB ix(i)=ifixr(float(i-1)*dx=pixel units
		  if1=1 + ifixr(float(ix2)/dx)
		  if(if1.ge.if2-3) if1=if2-3
		  if(if1.lt.1) if1=1
		else if(ibutton.eq.2) then
		  if2=1 + ifixr(float(ix2)/dx)
		  if(if2.le.if1+3) if2=if1+3
		  if(if2.gt.ndfit) if2=ndfit
		else if(ibutton.eq.3) then       !L AND R buttons to finish
		   goto 99
		else if(nbutton.eq.3.and.ibutton.eq.4) then
		   goto 99
		endif
c       and then draw new bar
		call HIDE_MOUSE()
		call RDGWIN1(ix(if1),iy,ix(if2),iy,isavb1)
		call RDGWIN1(ix(if1),iy,ix(if1),iy1,isavb2)
		call RDGWIN1(ix(if2),iy,ix(if2),iy1,isavb3)
		call HLINE(ix(if1),ix(if2),iy,12)
		call VLINE(ix(if1),iy,iy1,12)	!do here once req for y2>y1 fixed
		call VLINE(ix(if2),iy,iy1,12)
		call SHOW_MOUSE()
	   endif	!end of ibutton.ne.0
	endif		!end of mouse options
	goto 10	!get another character
c
99	continue
	call HIDE_MOUSE()
	RETURN
	end


