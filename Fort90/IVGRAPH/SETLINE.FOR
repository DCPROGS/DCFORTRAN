	subroutine SETLINE(horizont,value,eps,m,n,amin,amax,
     & icol,irhi,iclo,ibk,delete,ikey)
c
c This is more general version of MOVELINE (DC 09/03/94 10:26pm) that
c (a) moves either horizontal OR vertical line on screen (horizont=T/F)
c	VALUE=real*4 value to input/output value for x/y in Hgraph world units.
c     amin,amax=length of line (input/output) in world units, adjusted with keys
c (b) puts up popup window showing current X or Y value (in Hgraph world coord)
c   irhi,iclo=row/col # for top left corner of box (irhi negative=no box display)
c   m,n=format for displayed number=Fm.n
c   ibk=background colour (to erase box)
c (c) gives greater accuracy than screen resolution, by changing line position
c	in small increments, and showing current value in window in these small
c	increments; line on screen is updated (by one pixel) only when the rounded
c	value of line position in pixel units changes.
c (d) If DEL hit then line deleted ob screen and return with delete=true
c	to take further action in calling prog.
c
c Modified 01/24/96 02:05pm so that if delete is true on entry, line
c is deleted and routine left immediately.
c Modified 11/04/93 01:34pm so that subroutine is left when any keypad
c character is hit (44-57 -includes also comma=44), OR <enter>.
c Also returns ikey value (as in CKEY)
c
c 1st call WRGWIN1 to restore graph background in position where line orig drawn
c Then call RDGWIN1 to record graph background on line to be overwritten
c by following call to HLINE
c
c	integer*1 isavlin(320)	!should need 4*80 bytes for 640 pixels
c	integer*1 isavlin(1920)	!need 4*480=1920 for vert line, but 320 for horizont
	ALLOCATABLE:: isavlin
	integer*1 isavlin(:)
	integer*2 ktype
c	character*1	a,getch,LC
	character*1	a,getch
	character cnum*11
	character*36 helpst(10)		!help strings
	logical horizont,showbox,redraw,fine,delete
	logical mouse_on,mous_set,kbhit
	common/mousval/mouse_on,nbutton
c
	call NUMCLR
c	delete=.false.
	ALLOCATE(isavlin(1920))
	helpst(1)='DEL to delete line, OR'
	helpst(2)='Move line with cursor (arrow) keys.'
	helpst(3)=' move start with arrow key.'
	helpst(4)=' move end with ctrl-arrow.'
	helpst(5)=' INS toggles sensitivity. '
	nhelp=5
	if(mouse_on) then
	   helpst(6)='OR'
	   helpst(7)='Drag line with any mouse button;'
	   helpst(8)=' left button alters start too;'
	   helpst(9)=' right button alters end too;'
	   helpst(10)=' centre button drags only.'
	   nhelp=10
	endif
c Define box for display of number.
	call SHOW_MOUSE()
	icbd=0
	showbox=irhi.gt.0
c Convert min/max (ends of line) to pixel units
	if(horizont) then
	   xpix=float(ISCRX(amin))*0.0639	!see ILV routine WDTOPIX
	   ixmin=ifixr(xpix)
	   xpix=float(ISCRX(amax))*0.0639	!see ILV routine WDTOPIX
	   ixmax=ifixr(xpix)
	else
	   ypix=float(ISCRY(amin))*0.0639	!see ILV routine WDTOPIX
	   iymin=ifixr(ypix)
	   ypix=float(ISCRY(amax))*0.0639	!see ILV routine WDTOPIX
	   iymax=ifixr(ypix)
	endif
c
	eps=abs(eps)
	epsav=eps
	fine=.true.
c make course increment, eps1, ('not fine') movement=5 pixel steps
	x=float(5)/0.0639   !convert to world coord (see PIXTOWD.FOR)
	if(horizont) then
	   eps1=YWORLD(ifixr(x))-YWORLD(0.)
	else
	   eps1=XWORLD(ifixr(x))-XWORLD(0.)
	endif
c	subroutine DEFBOX(ibox,irhi,iclo,nrow,ncol,icb)
c	call DEFBOX(1,3,72,1,5,icbd)
	if(showbox) then
	   ncol=m+1
	   call DEFBOX(1,irhi,iclo,1,ncol,icbd)	!12 cols for cnum*11
	   call OPENBOX(1,7,.true.)
c decide on format for displayed value to get 3 sig figs
c	   call REALTOCH(value,cnum,11)
	   call DCFORMAT(value,m,n,cnum)
c	   n=NBLANK(cnum)
	   call WRITBOX(1,cnum(1:ncol),1,11)
	endif
c Define initial values of pixel coord for VALUE
	if(horizont) then
	   ypix=float(ISCRY(value))*0.0639	!see ILV routine WDTOPIX
	   iypix=ifixr(ypix)
	   iylast=iypix
	else	!vertical -change x value
	   xpix=float(ISCRX(value))*0.0639	!see ILV routine WDTOPIX
	   ixpix=ifixr(xpix)
	   ixlast=ixpix
	endif
c Draw initial line
	if(horizont) then
	   call RDGWIN1(0,iypix,639,iypix,isavlin) !store graphics
c	   call HLINE(0,639,iylast,icol)
	   call HLINE(ixmin,ixmax,iypix,icol)
	else
	   call RDGWIN1(ixpix,0,ixpix,479,isavlin)
c	   call VLINE(ixlast,0,478,icol)
	   call VLINE(ixlast,iymin,iymax,icol)
	endif
c
c
	if(delete) goto 2		!delete only if delete true on entry
c
c
c Now the loop
c Increment x/y value in steps of EPS say
1	continue
	redraw=.false.
	if(kbhit()) then
	 a=getch(ktype)
	 ival=ichar(a)
	 if(ktype.eq.8) then
	   if(ival.eq.82) then 	!INS=fine/course
		if(fine) then
		   eps=eps1
		else
		   eps=epsav
		endif
	      fine=.not.fine
	   endif
c
	   if(ival.eq.83) then		!DELETE
		delete=.true.
		goto 2
	   endif
c
	   if(horizont) then
		if(ival.eq.72) then		!up
		   value=value+eps
		else if(ival.eq.80) then	!down
		   value=value-eps
		else if(ival.eq.75) then	!left
		   ixmin=ixmin-1
		else if(ival.eq.77) then	!right
		   ixmin=ixmin+1
		else if(ival.eq.116) then     !ctrl-right arrow
		   ixmax=ixmax+1
		else if(ival.eq.115) then     !ctrl-left arrow
		   ixmax=ixmax-1
		endif
		if(ival.eq.75.or.ival.eq.77.or.ival.eq.115.or.
     &   			ival.eq.116) then
		   redraw=.true.
		   goto 3
		endif
		ypix=float(ISCRY(value))*0.0639	!see ILV routine WDTOPIX
		iypix=ifixr(ypix)
		redraw=iypix.ne.iylast.and.iypix.ge.0.and.iypix.le.479
	   else	!vertical -change x value
		if(ival.eq.77) then		!right
		   value=value+eps
		else if(ival.eq.75) then	!left
		   value=value-eps
		else if(ival.eq.80) then	!down
		   iymin=iymin-1
		else if(ival.eq.72) then	!up
		   iymin=iymin+1
		else if(ival.eq.145) then     !ctrl-down arrow
		   iymax=iymax-1
		else if(ival.eq.141) then     !ctrl-up arrow
		   iymax=iymax+1
		endif
		if(ival.eq.72.or.ival.eq.80.or.ival.eq.141.or.
     &   			ival.eq.145) then
		   redraw=.true.
		   goto 3
		endif
		xpix=float(ISCRX(value))*0.0639	!see ILV routine WDTOPIX
		ixpix=ifixr(xpix)
		redraw=ixpix.ne.ixlast.and.ixpix.ge.0.and.ixpix.le.639
	   endif
	 else if(ktype.eq.0.and.ival.eq.59) then	!F1=help
	   mxlo=70		!ixlo for poptext boxes -in common/popvals/
	   mylo=-1		!iylo for poptext boxes (-1 -> define top LH corner)
	   myhi=400		!iyhi for poptext boxes (-1 -> define bottom LH corner)
	   ictx=2		!dark green text
	   ibkx=15		!white background
	   icfx=10		!bright green border
	   call POPTEXT(mxlo,mylo,myhi,helpst,nhelp,ictx,ibkx,icfx)
	   call SHOW_MOUSE()
	 else
	   if((ival.ge.42.and.ival.le.57).or.ival.eq.13) goto 2  !keypad # or <enter>
	   if(ktype.eq.16.and.ival.eq.27) goto 2		!ESC
	 endif
c
	else if(mouse_on) then		!if not kbhit, check mouse
	   call get_mouse(ibutton,ix,iy)
c	   if(ibutton.eq.1.or.ibutton.eq.2) then
	   if(ibutton.gt.0) then
	      if(horizont) then
		   if(ibutton.eq.1) ixmin=ix
		   if(ibutton.eq.2) ixmax=ix
		   y=float(iy)/0.0639   !convert to world coord (see PIXTOWD.FOR)
		   value=YWORLD(ifixr(y))
		   redraw=iy.ge.0.and.iy.le.479
		   if(ibutton.eq.4) then	!centre button -redraw only if changed
			redraw=redraw.and.iy.ne.iylast
		   endif
		   iypix=iy
	      else
		   if(ibutton.eq.1) iymax=iy
		   if(ibutton.eq.2) iymin=iy
		   x=float(ix)/0.0639   !convert to world coord (see PIXTOWD.FOR)
		   value=XWORLD(ifixr(x))
		   redraw=ix.ge.0.and.ix.le.639
		   if(ibutton.eq.4) then	!centre button -redraw only if changed
			redraw=redraw.and.ix.ne.ixlast
		   endif
		   ixpix=ix
		endif
	   endif
	endif
c
	if(showbox) then
c	   call REALTOCH(value,cnum,11)
cc	   n=NBLANK(cnum)
cc	   call WRITBOX(1,cnum(1:n),1,11)
	   call DCFORMAT(value,m,n,cnum)
	   call WRITBOX(1,cnum(1:ncol),1,11)
	endif
c if VALUE changed enough so that pixel # has altered, then redraw the line.
3	continue
	if(redraw) then
	   call HIDE_MOUSE()
	   if(horizont) then
		call WRGWIN1(0,iylast,639,iylast,isavlin)
		call RDGWIN1(0,iypix,639,iypix,isavlin) !store graph to be
c		call HLINE(0,639,iypix,icol)
		if(ixmin.lt.0) ixmin=0
		if(ixmax.gt.639) ixmax=639
		if(ixmax-ixmin.lt.8) ixmax=ixmin+8
	      call HLINE(ixmin,ixmax,iypix,icol)
		iylast=iypix
	   else
		call WRGWIN1(ixlast,0,ixlast,479,isavlin)
		call RDGWIN1(ixpix,0,ixpix,479,isavlin) !store graph to be
c		call VLINE(ixpix,0,478,icol)
		if(iymin.lt.0) iymin=0
		if(iymax.gt.479) iymax=479
		if(iymax-iymin.lt.8) iymax=iymin+8
		call VLINE(ixpix,iymin,iymax,icol)
		ixlast=ixpix
	   endif
	   call SHOW_MOUSE()
	endif
c
	goto 1
c
c  Decode ikey
2	continue
c delete graphics
	call HIDE_MOUSE()
	call ENDBOX(1,ibk)
c leave the line on display unless deleted!
	if(delete) then
	 if(horizont) then
	   call WRGWIN1(0,iypix,639,iypix,isavlin) !restore
	 else
	   call WRGWIN1(ixpix,0,ixpix,479,isavlin)
	 endif
	endif
c  Return VALUE, amin, amax and IKEY to main prog
	call SETCKEY(ktype,ival,ikey)
	if(horizont) then
	   x=float(ixmin)/0.0639   !convert to world coord (see PIXTOWD.FOR)
	   amin=XWORLD(ifixr(x))
	   x=float(ixmax)/0.0639   !convert to world coord (see PIXTOWD.FOR)
	   amax=XWORLD(ifixr(x))
	else
	   y=float(iymin)/0.0639   !convert to world coord (see PIXTOWD.FOR)
	   amin=YWORLD(ifixr(y))
	   y=float(iymax)/0.0639   !convert to world coord (see PIXTOWD.FOR)
	   amax=YWORLD(ifixr(y))
	endif
	DEALLOCATE(isavlin)
	RETURN
	end
