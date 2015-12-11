	subroutine IFRAME1(xbox,ybox)
	real xbox(4),ybox(4)
c IFRAME1 is like IFRAME but position of all four corners is specified
c (e.g. as found in TEXTBOX) so sloping rectangle can be drawn.
c To draw box (version of FRAME that uses device coordinates)
	call MOVTO2( xbox(1), ybox(1))
	call LINTO2( xbox(2), ybox(2))
	call LINTO2( xbox(3), ybox(3))
	call LINTO2( xbox(4), ybox(4))
	call LINTO2( xbox(1), ybox(1))
	return
	end


