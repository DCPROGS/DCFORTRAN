	subroutine FRAMER(rxbox,rybox)
	real rxbox(4),rybox(4)
c To draw box (version of FRAME that uses all four corners in rxbox,rybox)
c
	lt=0		!continuous line
	call MOVTO2(rxbox(1),rybox(1))
	call LINTO2(rxbox(2),rybox(2))
	call LINTO2(rxbox(3),rybox(3))
	call LINTO2(rxbox(4),rybox(4))
	call LINTO2(rxbox(1),rybox(1))
	return
	end

