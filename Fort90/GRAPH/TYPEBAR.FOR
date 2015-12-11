	subroutine typebar(ivert)
	character*3 num
	character*15 text
c==========================================================================
c For Horizontal bar
      iw=96		!box width
	ih=16       !height of box
	ixlo = 0
	x=10.
	iwr=15
	ifill=4
	if(ivert.eq.1) then  !lines
	   iylo = 367
	   n=15
	else                 !curves
	   iylo = 367
	   n=5
	endif
	ixhi = ixlo+iw
	iyhi = iylo+ih
	call lincols(iwr)
	do icol=0,n
	  call drawrec(ixlo,iylo,ixhi,iyhi,iwr,ifill)
	  call intconv(icol,num)
	  nl=nblank1(num)
	  if (icol.lt.10) then
	     call wrstring18(ixlo+8,iylo+1,num(1:nl),iwr,ifill)
	  else
	     call wrstring18(ixlo,iylo+1,num(1:nl),iwr,ifill)
	  endif
	  call broken(icol)
	  if(ivert.eq.2.and.icol.ne.0) then
	     if(icol.eq.1) call broken(2)
	     if(icol.eq.2) call broken(1)
	     if(icol.eq.3) call broken(4)
	     if(icol.eq.4) call broken(6)
	     if(icol.eq.5) call broken(12)
	  endif
	  y=iylo*159.67/480.+2.
	  if(ivert.ne.3) then
	     call movto2(x,y)
	     call linto2(x+20.,y)
	  endif
	  if(ivert.eq.2.and.icol.eq.5) then
	     call broken(0)
	     call lincols(4)
	     call movto2(x+15.,y)
	     call linto2(x+17.,y)
	     call lincols(15)
	  endif
	  iylo=iylo-ih
	  iyhi=iyhi-ih
	enddo
	call broken(0)
	call chafnt(1)
	call setsize(2.5)
	RETURN
	end
