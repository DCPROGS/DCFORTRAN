	subroutine DCALBAR(x1,y1,x2,y2,x3,y3,x4,y4,thick,idev,idraw,
     &  itx,ity,ntx,nty,tlenx,tleny,ibk)
c-------------------------------------------------------------------
c To draw idraw=1 (or delete, idraw=-1) calibration bars
c-------------------------------------------------------------------
	logical calbarX,calbarY

	if(idraw.eq.-1) call lincol(ibk)
	calbarX=ntx.eq.-1000
	calbarY=nty.eq.-1000
	call LINWID(thick)
	if(calbarX) then
	   call MOVTO2(x1,y1)
	   call LINTO2(x2,y2)     !x-bar
	endif
	if(calbarY) then
	   call MOVTO2(x3,y3)
	   call LINTO2(x4,y4)     !y-bar
	endif
c add 'tic's at ends of bars
	if(calbarX) then	!x-bar
	  roff=(itx-1)*tlenx/2	!=0 for itx=1; =-it/2 for itx=0; =-it for itx=-1
	  call MOVTO2(x1,y1+roff)       !start pos to draw tic
	  call LINTO2(x1,y1+roff+tlenx)	!draw tic
	  call MOVTO2(x2,y2+roff)       !start pos to draw tic
	  call LINTO2(x2,y2+roff+tlenx)	!draw tic
      endif
	if(calbarY) then	!y-bar
	  roff=(ity-1)* tleny/2	!=0 for ity=1; =-it/2 for ity=0; =-it for ity=-1
	  call MOVTO2( x3+roff, y3)       !start pos to draw tic
	  call LINTO2( x3+roff+ tleny, y3)	!draw tic
	  call MOVTO2( x4+roff, y4)       !start pos to draw tic
	  call LINTO2( x4+roff+ tleny, y4)	!draw tic
	endif
c
	RETURN
	end

