c=======================================================================
	subroutine iDRAWNUM(cnum,cexp,ixoff,iyoff,sizen,ix,iy,ixj,iyj,
     &		iang,logxy,inumxy)
c=======================================================================
c This a modified routine
c To draw a number at predefined position in LAXES1 and FIXNUMB
c 1. ix,iy are now in HGRAPH disply coordinates(0-10000;0-75000)
c 2. You can draw a number, or  a number + exponent, or a number+index
c=======================================================================
	logical logxy
	character*(*) cnum,cexp
	ixc=ix
	iyc=iy
	nl=NBLANK1(cnum)
	call SETCSIZE(sizen,isize)		!set isize
	call JUSTIFYSTRING(ixc,iyc,cnum(1:nl),iang,isize,ixj,iyj)
	if(logxy.and.inumxy.eq.1) then	!draw superscript
		call GETCURRPOS(ixc,iyc)
		ixc=ixc+ixoff
		iyc=iyc+iyoff
		nl=NBLANK1(cexp)
		call SETCSIZE(0.8*sizen,isize)	!this defines ISIZE
	      call JUSTIFYSTRING(ixc,iyc,cexp(1:nl),iang,isize,0,0)
		call SETCSIZE(sizen,isize)	!reset
	else if(logxy.and.inumxy.eq.2) then	!draw subscript
		call GETCURRPOS(ixc,iyc)
		ixc=ixc+ixoff
		iyc=iyc-iyoff
		nl=NBLANK1(cexp)
		call SETCSIZE(0.8*sizen,isize)	!this defines ISIZE
	      call JUSTIFYSTRING(ixc,iyc,cexp(1:nl),iang,isize,0,0)
		call SETCSIZE(sizen,isize)	!reset
	endif

	RETURN
	end

c=======================================================================
