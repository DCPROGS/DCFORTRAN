	subroutine DCALBAR(rx,ry,rlthk,idev,idraw,itx,ity,ntx,nty,tlen)
c To draw idraw=1 (or delete, idraw=-1) calibration bars
c (call with idraw=idraw(1))
c	rx(1),ry(1)=origin of X-calibration bars (world units),
c	rx(2),ry(2)=end of X calibration bars
c	rx(3),ry(3)=origin of Y-calibration bars (world units),
c	rx(4),ry(4)=end of Y calibration bars
c (horizontal and vertical so ry(1)=ry(2), and rx(3)=rx(4)
c	idraw(1) controls whether bars are drawn or not
c	At present no numbering allowed on calibration bars, just
c	axis labels (e.g. '5pA' and '1ms')
	real RX(100),RY(100)
	logical calbarX,calbarY
c
	if(idraw.eq.-1) call lincols(ibk,idev)
	calbarX=ntx.eq.-1000
	calbarY=nty.eq.-1000

	call LINWID(rlthk)
	if(calbarX) then
	   call graspa(rx(1),ry(1),x,y)
	   call MOVTO2(x,y)
	   call graspa(rx(2),ry(2),x,y)
	   call LINTO2(x,y)     !x-bar
	endif
	if(calbarY) then
	   call graspa(rx(3),ry(3),x,y)
	   call MOVTO2(x,y)
	   call graspa(rx(4),ry(4),x,y)
	   call LINTO2(x,y)     !x-bar
	endif
c add 'tic's at ends of bars
	   do 12 i=1,4
	    call graspa(rx(i),ry(i),x,y)
	    if(i.le.2.and.calbarX) then	!x-bar
	      roff=(itx-1)*tlen/2	!=0 for itx=1; =-it/2 for itx=0; =-it for itx=-1
	      call MOVTO2(x,y+roff)       !start pos to draw tic
	      call LINTO2(x,y+roff+tlen)	!draw tic
	    else if(i.gt.2.and.calbarY) then	!y-bar
		roff=(ity-1)* tlen/2	!=0 for ity=1; =-it/2 for ity=0; =-it for ity=-1
		call MOVTO2( x+roff, y)       !start pos to draw tic
		call LINTO2( x+roff+ tlen, y)	!draw tic
	    endif
12	   continue
c
	RETURN
	end

