	program TMOUS
	logical kbhit
c
c	;RESET_MOUSE returns istatus=65535 if mouse functional; 0 otherwise
c
c     ;For 2 or 3 button mouse
c	;ibutton=0 -> no button hit since last call
c	;ibutton=1 -> left button hit since last call
c	;ibutton=2 -> right button hit since last call
c	;ibutton=3 -> both buttons hit since last call
c	;For 3 button mouse:
c	;ibutton=4 -> center button hit since last call
c	;it is modified for mode 18
c
c NB when repeatedly called in a loop:
c     GET_MOUSE and UPDATE_MOUSE seem identical -ix,iy continuously change
c	  at every call, regardless of whether a button hit or not (returns
c	  ibutton=0 when no button hit)
c	GET_BUTTON returns the ix,iy that were in effect last time the
c	  left button was hit; values do not change when mouse moved,
c	  until left button is hit. (Right and centre button have no effect)
c	ident=0 updates only when left button pressed (with or without others)
c	ident any other value updates only when right button pressed
c
c; NB MOUSE ROUTINE TIMINGS (Dell 66MHz)
c; mouse_reset  820000 microsec (0.82 sec) (!!)
c; get_mouse        87 microsec
c; get_button       91 microsec
c; mouse_update    164 microsec

12	print 11
11	format(' ident = ')
	read *,ident
	call CLS
	call MODE(18)
	call reset_mouse(istatus,nbutton)
	call show_mouse()
	call LOCATE(0,0)
	print 1,istatus,nbutton
1	format(' Reset gives istatus, nbutton = ',2i8)
	nhit=0
c	ident=2
10	continue
	call LOCATE(2,2)
	call get_mouse(ibutton,ix,iy)
	print 2,ibutton,ix,iy
2	format(' Get_mouse gives ibutton, ix,iy= ',3i8)
	call mouse_update(ibutton,ix,iy)
	print 3,ibutton,ix,iy
3	format(' mouse_update gives ibutton, ix,iy= ',3i8)
c
c	call get_button(status,button_identifier,button_counter,ix,iy)
c	ident=1
	call get_button(ibutton,ident,icount,ix,iy)
	nhit=nhit+icount
	print 4,ibutton,ident,icount,nhit,ix,iy
4	format(' Get_button gives ibutton,ident,icount,nhit,ix,iy= ',6i4)
c	call get_button(ibutton,ident,icount,ix,iy)
c	print 4,ibutton,ident,icount,ix,iy
c	if(icount.ne.0) then
c	   call LOCATE(10,2)
c	   print 5,icount
c5	   format(' icount = ',i8)
c	   pause
c	endif
	if(kbhit()) then
	   goto 99
	endif
	goto 10
c
99	continue
	call MODE(3)
	goto 12
	end

