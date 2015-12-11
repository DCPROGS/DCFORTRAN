c=======================================================================
	subroutine iDRAWNUM(cnum,cexp,ixoff,iyoff,chw,chh,ix,iy,ij,
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
c	call SETCSIZE(sizen,isize)		!set isize
c	call JUSTIFYSTRING(ixc,iyc,cnum(1:nl),iang,isize,ixj,iyj)
c	call CHASIZ(chw,chh)
c	call CHAJUS(ij)
c	call CHABEG(ixc,iyc)
c	call CHAANG(iang)
c	call CHASTR(cnum(1:nl))
	call JUSTIFYSTRING(ixc,iyc,cnum(1:nl),iang,chw,chh,ij)

	if(logxy.and.inumxy.eq.1) then	!draw superscript
c		call GETCURRPOS(ixc,iyc)
		call CURSOR(ikey,ixc,iyc)	
		ixc=ixc+ixoff
		iyc=iyc+iyoff
		nl=NBLANK1(cexp)
c		call SETCSIZE(0.8*sizen,isize)	!this defines ISIZE
	      call JUSTIFYSTRING(ixc,iyc,cexp(1:nl),iang,0.8*chw,0.8*chh,0)
c		call SETCSIZE(sizen,isize)	!reset
	else if(logxy.and.inumxy.eq.2) then	!draw subscript
c		call GETCURRPOS(ixc,iyc)
		call CURSOR(ikey,ixc,iyc)	
		ixc=ixc+ixoff
		iyc=iyc-iyoff
		nl=NBLANK1(cexp)
c		call SETCSIZE(0.8*sizen,isize)	!this defines ISIZE
		call CURSOR(ikey,ixc,iyc)	
	      call JUSTIFYSTRING(ixc,iyc,cexp(1:nl),iang,0.8*chw,0.8*chh,0)
c		call SETCSIZE(sizen,isize)	!reset
	endif

	RETURN
	end

c=======================================================================
