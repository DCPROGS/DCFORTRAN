	subroutine DRAWPROG(ixbox,ixframe,iyframe,icframe,iprogx1,iprogx2,
	&    icprog,ixcr,iycr,iccr,ixmark,iymark,icmark,ibdx,ibk,iyd2)
c To draw progress line in SCAN (replacement for old put_cursors and
c newprogline) after deleting any previous progress line completely
c Definitions (all pixel units)
c ixbox=x-coord for top left corner of frame
c ixframe=length of frame
c
c Draw the frame for progress line
c (Called with X origin ixbox=0)
	ifram1=iccr-8
	ifram2=icmark-8
c DC ixframe+ixbox altered to ixframe+ixbox-1
c Delete any previous line by filling with background colour
	call FILLWIN(0,iyd2+2,639,479,ibk)
c Draw top and bottom of progress line frame
	call HLINE(ixbox,ixframe+ixbox-1,iyframe,icframe)
	call HLINE(ixbox,ixframe+ixbox-1,iyframe-5,icframe)
c
c Rather than read pixels to avoid deleting section markers, easier to
c draw progress line first, then draw markers on top this will certainly
c be fastest for complete redraw of progress line (-but probably not
c when progress line moved on by small amount, when we still do not want
c to delete section markers!)
c Upper line up to iprogx1
c  (cannot use FILLWIN as x must be multiple of 8 pixels? -use HLINE*2 instead)
c==	call FILLWIN(ixbox+1,iyframe-1,ixbox+iprogx1,iyframe-2,icprog)
	ix=ixbox+iprogx1
	call HLINE(ixbox+1,ix,iyframe-1,icprog)
	call HLINE(ixbox+1,ix,iyframe-2,icprog)
c Lower line up to iprogx2
c==	call FILLWIN(ixbox+1,iyframe-3,ixbox+iprogx2,iyframe-4,icprog)
	ix=ixbox+iprogx2
	call HLINE(ixbox+1,ix,iyframe-3,icprog)
	call HLINE(ixbox+1,ix,iyframe-4,icprog)
c
c Draw verticals at ends and to mark sections
	if(ibdx.ge.2) then
	   do i=ixbox,ixframe+ixbox-1,ibdx	!=1,ibdx,2ibdx,3ibdx (ibdx=pixels/section)
		call VLINE(i,iyframe-5,iyframe,icframe)
	   end do
	endif
	call VLINE(ixframe+ixbox-1,iyframe-5,iyframe,icframe)		!last vertical
c Draw cursor markers
	call draw_cursor(ixcr+ixbox,iycr,iccr,ifram1)
	call draw_cursor(ixmark+ixbox,iymark,icmark,ifram2)
c
	RETURN
	end

