	subroutine SETLINE(horizont,value,eps,m,n,amin,amax,
     & icol,irhi,iclo,ibk,delete,ikey,dim)
c=====================================================================
c    26/06/1997
c=====================================================================
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
c
c Modified 11/04/93 01:34pm so that subroutine is left when any keypad
c character is hit (44-57 -includes also comma=44), OR <enter>.
c Also returns ikey value (as in CKEY)
c
c 1st call WRGWIN1 to restore graph background in position where line orig drawn
c Then call RDGWIN1 to record graph background on line to be overwritten
c by following call to HLINE
c
	integer*1 isavlin(2000)	!should need 4*80 bytes for 640 pixels
	integer*2 ktype
	character*1	a,getch
	character cnum*11
	character*36 helpst(8)		!help strings
	logical horizont,showbox,redraw,fine,delete
	logical mouse_on,mous_set,kbhit,logx,logy,sqrty
	common/mousval/mouse_on,nbutton
	common/logval/logx,logy,sqrty
c
	helpst(1)='Move line with cursor (arrow) keys.'
	helpst(2)=' move start with arrow key.'
	helpst(3)=' move end with ctrl-arrow.'
	nhelp=3
	if(mouse_on) then
	   helpst(4)='OR'
	   helpst(5)='Drag line with any mouse button;'
	   helpst(6)=' left button alters start too;'
	   helpst(7)=' right button alters end too;'
	   helpst(8)=' centre button drags only.'
	   nhelp=8
	endif
c Define box for display of number.
	icbd=0
	showbox=irhi.gt.0
c Convert min/max (ends of line) to pixel units


	eps=abs(eps)
	epsav=eps
	fine=.true.
	call pixgra(5,0,xg,y0)
	call pixgra(0,5,x0,yg)
c make course increment, eps1, ('not fine') movement=5 pixel steps
	if(horizont) then
	   call grapix(0.,value,idummy,iylast)
	   call grapix(amin,0.,ixmin,idummy)
	   call grapix(amax,0.,ixmax,idummy)
	   eps1=yg-y0
	else
	   call grapix(value,0.,ixlast,idummy)
	   call grapix(0.,amin,idummy,iymin)
	   call grapix(0.,amax,idummy,iymax)
	   eps1=xg-x0
	endif
	if(showbox) then
	   ncol=m+1
	   call DEFBOX(1,irhi,iclo,1,ncol,icbd)	!12 cols for cnum*11
	   call OPENBOX(1,7,.true.)
c decide on format for displayed value to get 3 sig figs
	   value1=value
	   if(horizont) then
	     if(logy) value1=10**value
	     if(sqrty) value1=value**2
	   else
	     if(logx) value1=10**value
	   endif
	   call DCFORMAT(value1,m,n,cnum)
	   call WRITBOX(1,cnum(1:ncol),1,11)
	endif
c Define initial values of pixel coord for VALUE
	iypix=iylast
	ixpix=ixlast
c====================================
c Draw initial line
	if(delete) then
	   ifirst=1
         goto 11
	endif
	if(horizont) then
	   call RDGWIN1(0,iylast,639,iylast,isavlin) !store graphics
	   call HLINE(ixmin,ixmax,iylast,icol)
	else
	   call RDGWIN1(ixlast,55,ixlast,387,isavlin)
         call VLINE(ixlast,iymin,iymax,icol)
	endif

c
c
c Now the loop
c Increment x/y value in steps of EPS say
c1	continue
11	call SHOW_MOUSE()
1	call mouse_update(ibutton,ix,iy)
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
	   if(horizont) then
		if(ival.eq.72) then
		   value=value+eps
		else if(ival.eq.80) then
		   value=value-eps
		else if(ival.eq.75) then
		   ixmin=ixmin-1
		else if(ival.eq.77) then
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
		call grapix(0.,value,idummy,iypix)
		redraw=iypix.ne.iylast.and.iypix.ge.0.and.iypix.le.479
	   else	!vertical -change x value
		if(ival.eq.77) then
		   value=value+eps
		else if(ival.eq.75) then
		   value=value-eps
		else if(ival.eq.72) then
		   iymin=iymin+1
		else if(ival.eq.80) then
		   iymin=iymin-1
		endif
		if(ival.eq.72.or.ival.eq.80) then
		   redraw=.true.
		   goto 3
		endif
		call grapix(value,0.,ixpix,idummy)
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
      endif
c
c	else if(mouse_on) then		!if not kbhit, check mouse
c	   call get_mouse(ibutton,ix,iy)
	if(ibutton.gt.0) then
		call pixgra(ix,iy,valx,valy)
	      if(horizont) then
		   if(ibutton.eq.1) ixmin=ix
		   if(ibutton.eq.2) ixmax=ix
		   value=valy
		   redraw=iy.ge.55.and.iy.le.387
		   if(ibutton.eq.4) then	!centre button -redraw only if changed
			redraw=redraw.and.iy.ne.iylast
		   endif
		   iypix=iy
	      else
		   if(ibutton.eq.1) then
			  if(iy.lt.55) iy=55
                    iymin=iy
		   else if(ibutton.eq.2) then
			  if(iy.gt.387) iy=387
                    iymax=iy
		   endif
		   value=valx
		   redraw=ix.ge.0.and.ix.le.639
		   if(ibutton.eq.4) then	!centre button -redraw only if changed
			redraw=redraw.and.ix.ne.ixlast
		   endif
		   ixpix=ix
		endif
	endif
c	endif
c
	if(showbox) then
	   value1=value
	   if(horizont) then
	     if(logy) value1=10**value
	     if(sqrty) value1=value**2
	   else
	     if(logx) value1=10**value
	   endif
	   call DCFORMAT(value1,m,n,cnum)
	   call WRITBOX(1,cnum(1:ncol),1,11)
	endif
c if VALUE changed enough so that pixel # has altered, then redraw the line.
3	continue
	if (redraw) then
	   call HIDE_MOUSE()
	   if (horizont) then
		if (ifirst.eq.1.and.delete) then
	         call hline(1,639,iylast,ibk)
		   if(dim.ge.0.4) then
                    call hline(1,639,iylast-1,ibk)
                    call hline(1,639,iylast+1,ibk)
		   endif
		   ifirst=0
		   goto 77
		endif
		call WRGWIN1(0,iylast,639,iylast,isavlin)
77		call RDGWIN1(0,iypix,639,iypix,isavlin) !store graph to be
		if(ixmin.lt.0) ixmin=0
		if(ixmax.gt.639) ixmax=639
		if(ixmax-ixmin.lt.8) ixmax=ixmin+8
	      call HLINE(ixmin,ixmax,iypix,icol)
		iylast=iypix
	   else
		if (ifirst.eq.1.and.delete) then
	         call VLINE(ixlast,55,387,ibk)
		   if(dim.ge.0.4) then
                    call vline(ixlast-1,55,387,ibk)
                    call vline(ixlast+1,55,387,ibk)
		   endif
		   ifirst=0
		   goto 78
		endif
		call WRGWIN1(ixlast,55,ixlast,
     &      387,isavlin)
78		call RDGWIN1(ixpix,55,ixpix,387,isavlin) !store graph to be
		if(iymin.lt.55) iymin=55
		if(iymax.gt.387) iymax=387
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
c  Return VALUE, amin, amax and IKEY to main prog
	call SETCKEY(ktype,ival,ikey)
	if(horizont) then
		call pixgra(ixmin,iypix,amin,value)
		call pixgra(ixmax,iypix,amax,value)
	else
		call pixgra(ixpix,iymin,value,amin)
		call pixgra(ixpix,iymax,value,amax)
	endif

88	RETURN
	end
