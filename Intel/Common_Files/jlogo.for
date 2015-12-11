	subroutine JLOGO(xmin,xmax,njump,t1,t2,
     & xoff1,idev,thk,idraw,y1,y2)
	use gino_f90
c To draw jump logos in VPLOTR. Time of start,end of each jump=t1,t2
c Level, in screen units (0-7000), =iy1 at 'holding' level; iy2 during jump.

	real*4 t1(10),t2(10)	!for jump logos and t=0 def.
	real*4 Lthk
c
	ixmin=(xmin)
	ixmax=(xmax)
	
	if(njump.gt.0) then
	   
	      call LINWID(thk)
	   

c Seems to be easier not to write as explicit loop!
c START
c 1st skip through all jumps that end BEFORE the current Xmin so entirely
c off screen until 1st one found that ends AFTER xmin (NB could start before
c xmin, and also could start and/or end after xmax, so no part of jump #i2
c is necessarilly on screen

	   x0=xmin		!first time
c	   ix0=ISCRX(x0)
	   i1=1
231	   continue		!loop back to here for next jump
	   do 229 i=i1,njump
	    i2=i
	    x=t2(i)-xoff1
	    if(x.ge.xmin) goto 230	!skip out
229	   continue
c If this point reached all jumps end below xmin so draw line at iy1 level
c and finish
	   call MOVTO2(xmin,y1)  	  !horizontal at holding level
	   call LINTO2(xmax,y1)    !horizontal at holding level
	   goto 99
c
c Now jump #i2 may be partly on screen (see above)
c so start plotting
230	   continue
	   i=i2		!start with #i2
	   x=t1(i)-xoff1
c	   ix=ISCRX(x)
	   if(x.gt.xmax) then	!does not start until after xmax
		call MOVTO2(x0,y1)		!so plot up to xmax at iy1 level
		call LINTO2(xmax,y1)
		goto 99				!and end
	   endif
	   if(x.ge.x0) then
	   	call MOVTO2(x0,y1)     !move to start pos
c	      ix=ISCRX(x)
	      call LINTO2(x,y1)    !horizontal at holding level
		call LINTO2(x,y2)    !1st vertical-start of jump
	   else if(x.lt.x0) then
		call MOVTO2(x0,y2)     !move to xmin at iy2 level
	   endif
c now at 'jump' level, iy2, so move to end of this jump (if on screen) and
c continue with other jumps (if on screen)
	   x=t2(i)-xoff1
c	If jump ends below xmax then draw end of jump
c	If jump ends above xmax then just draw line to xmax at iy2 level
c	  and then return (xmax reached so finished)
	   if(x.gt.xmax) then
	      call MOVTO2(xmax,y2)		!horizontal during jump
		goto 99			!finished
	   else
c	      ix=ISCRX(x)
	      call linTO2( x, y2)			!horizontal during jump
		call linTO2( x, y1)	!2nd vertical=end of jump
	   endif
c If not finished then we are now back at iy1 level, at the end of a jump;
c is there another jump at last partially on screen?
	   x0=x
!	   ix0=ix
	   i1=i+1
	   if(i1.le.njump) goto 231
c if this was last jump then complete by drawing line at iy1 up to xmax
	   call linTO2( xmax, y1)	!2nd vertical=end of jump
	endif
	call gFlushGraphics()		
99	continue
	RETURN
	end

