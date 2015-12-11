	subroutine SETMOUSE()
c To set up mouse at start of program, and communicate whether present
c (mouse_on=true), and number of buttons via common /mousval/
c NB reset_mouse takes of the order of 1 second!!
	logical mouse_on
	common/mousval/mouse_on,nbutton
c
	call reset_mouse(istatus,nbutton)
	mouse_on=istatus.eq.65535
	call HIDE_MOUSE()
	RETURN
	end


