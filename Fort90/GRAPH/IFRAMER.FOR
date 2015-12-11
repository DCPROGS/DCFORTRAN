	subroutine IFRAMER(ixbox,iybox)
	integer ixbox(4),iybox(4)
	real ax(4),ay(4)
c IFRAMER is like IFRAME1 but converted to world coord before drawing,
c so should be immune to changes in scaling
c IFRAME1 is like IFRAME but position of all four corners is specified
c (e.g. as found in TEXTBOX) so sloping rectangle can be drawn.
c To draw box (version of FRAME that uses device coordinates)
	do 1 i=1,4
	ax(i)=XWORLD(ixbox(i))
1	ay(i)=YWORLD(iybox(i))
c
	lt=0		!continuous line
	call MOVETO(ax(1),ay(1),0,lt)
	call MOVETO(ax(2),ay(2),1,lt)
	call MOVETO(ax(3),ay(3),1,lt)
	call MOVETO(ax(4),ay(4),1,lt)
	call MOVETO(ax(1),ay(1),1,lt)
	return
	end

