	subroutine IFRAME(xlo,xhi,ylo,yhi)
	call MOVTO2( xlo, ylo)
	call LINTO2( xlo, yhi)
	call LINTO2( xhi, yhi)
	call LINTO2( xhi, ylo)
	call LINTO2( xlo, ylo)
	return
	end

