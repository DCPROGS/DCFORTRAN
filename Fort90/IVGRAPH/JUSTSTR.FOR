c=======================================================================
	subroutine juststr(ix,iy,outstr,iang,sizen,ixj,iyj,instr,index,
     &	     logxy,inumxy)
c=======================================================================
c Anoher option to draw a number at predefined position in LAXES1 and
c FIXNUMB
c Similar with DRAWNUM, but:
c 1. ix,iy are now in HGRAPH disply coordinates(0-10000;0-75000)
c 2. You can draw a number, or  a number + exponent, or a number+index
c 3. The resulting string is returned in one single string :outstr,
c    which is much easier to move,rotate,etc.
c 4. Doesn't use ixoff or iyoff
c=======================================================================
c	INPUT:
c	ix,iy 	: string's display coordinates (0-10000,0-7500)
c	iang  	: string's angle
c	csize 	: string's real size
c	csfac       : size factor
c	ixj,iyj     : string's justification
c	instr		: input string
c	index		: index/exponent for string
c	logxy		: .true.-> write exponent
c	numxy       :   = 0 -> write only instr
c	            :   = 1 -> write with exponent
c			:   = 2 -> write with index
c	OUTPUT:
C	outstr      : resulting string

c=======================================================================

	logical logxy
	character*(*) instr,index,outstr
	character*10  getint
	ixc=ix
	iyc=iy
	nl=NBLANK1(instr)
	call SETCSIZE(sizen,isize)		!set isize
	outstr=instr(1:nl)                  !draw input
	if(logxy.and.inumxy.eq.1) then	!draw superscript (exponent)
		call SETCSIZE(0.8*sizen,indsize)	!this defines ISIZE
	      nl1=NBLANK1(index)
		outstr=outstr(1:nl)//char(21)//char(19)
     &      //getint(indsize,nm)(1:nm)//index(1:nl1)
		call SETCSIZE(sizen,indsize)	!reset
	else if(inumxy.eq.2) then           !draw subscript (index)
		call SETCSIZE(0.8*sizen,indsize)	!this defines ISIZE
	      nl1=NBLANK1(index)
		outstr=outstr(1:nl)//char(4)//char(19)
     &      //getint(indsize,nm)(1:nm)//index(1:nl1)
		call SETCSIZE(sizen,indsize)	!reset
	endif
	nl=NBLANK1(outstr)
	call JUSTIFYSTRING(ixc,iyc,outstr(1:nl),iang,isize,ixj,iyj)

	RETURN
	end
c=======================================================================
