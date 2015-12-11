c========================================================================
	subroutine juststr(ix,iy,outstr,iang,sizen,ixj,iyj,instr,index,
     &		       logxy,inumxy)
c========================================================================
c	INPUT:
c	ix,iy 	: string's display coordinates (0-10000,0-7500)
c	iang  	: string's angle
c	sizen 	: string's real size
c	ixj,iyj     : string's
c	instr		: input string
c	index		: index/exponent for string
c	logxy		: .true.-> write exponent
c	numxy       :   = 0 -> write only instr
c	            :   = 1 -> write with exponent
c			:   = 2 -> write with index
c	OUTPUT:
C	outstr      : resulting string

c========================================================================

	logical logxy
	character*(*) instr,index,outstr
	character*10  getint

	ixc=ix
	iyc=iy
	nl=NBLANK1(instr)
	call SETCSIZE(sizen,isize1)		!set isize
	outstr=instr(1:nl)
	if(logxy.and.inumxy.eq.1) then	!draw superscript
		call SETCSIZE(0.8*sizen,isize2)	!this defines ISIZE
	      nl1=NBLANK1(index)
		outstr=outstr(1:nl)//char(21)//char(19)
     &      //getint(isize2,nm)(1:nm)//index(1:nl1)
		call SETCSIZE(sizen,isize2)	!reset
	else if(inumxy.eq.2) then
		call SETCSIZE(0.8*sizen,isize2)	!this defines ISIZE
	      nl1=NBLANK1(index)
		outstr=outstr(1:nl)//char(4)//char(19)
     &      //getint(isize2,nm)(1:nm)//index(1:nl1)
		call SETCSIZE(sizen,isize2)	!reset
	endif
	nl=NBLANK1(outstr)
	call JUSTIFYSTRING(ixc,iyc,outstr(1:nl),iang,isize1,ixj,iyj)

	RETURN
	end
c========================================================================
