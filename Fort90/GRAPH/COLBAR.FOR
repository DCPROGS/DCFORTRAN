	subroutine COLBAR(ivert)
	character*3 num
c==========================================================================
c For Horizontal bar

      iw=16		!box width
	ih=16		!height of box
c For vertical bar
	ixlo = 0
	if(ivert.eq.0) then  !h
	   iylo = 351
	   iyhi = iylo+ih
	else                 !v
	   iylo = 127
	   ixhi = ixlo+iw
	endif
	do icol=0,15
	  if(ivert.eq.0) then
	     ixlo=ixlo+iw
	     ixhi=ixlo+iw
	  else
	     iylo=iylo+ih
	     iyhi=iylo+ih
	  endif
	  call drawrec(ixlo,iylo,ixhi,iyhi,15,icol)
	  call INTCONV(icol,num)
	  nl=nblank1(num)
	  if (icol.lt.10) then
	     call wrstring18(ixlo+8,iylo+1,num(1:nl),15,icol)
	  else
	     call wrstring18(ixlo,iylo+1,num(1:nl),0,icol)
	  endif
	enddo
	RETURN
	end
