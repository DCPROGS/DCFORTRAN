	subroutine FIXTEXT(ix,iy,text,iangle,csfac,csize,ifont,
     & ixjus,iyjus,idraw,rxbox,rybox,nvp,ifitype,theta,ncomp,ntdel)
c To move text string, change its size...
c Modif 07/07/93 09:45am to use dialog box
c Arguments in call will usually be array elements (values for text
c string #n)
c Last line of params is only to allow change of font for param values,
c which necessitates total regeneration of PARVAL string via call to PARTEXT
c IDRAW=0  omit
c IDRAW=1  draw (no box)
c IDRAW=-1 draw with box
	character*(*) text
      character cnum1*11
	integer ival1(150)		!====debug
	character*1 ch,ch1,getch
	integer fndcur		!note new usage
	integer*2 ktype
	real rxbox(4),rybox(4)
c For call to PARTEXT need:
	real theta(20)
c
	ntdel=0		!see below
c
	if(nvp.lt.6.or.nvp.gt.30) then
	   call BELL(2)
c	   call SCROLL(0,0,2,79,1)		!scroll lines (0,2) only
c	   call LOCATE(0,2)		!row=0,col 0
c	   print 3,nvp
c3	   format(' BOX # ',i3,' IS NOT A TEXT STRING: TRY AGAIN')
	   call INTCONV(nvp,cnum1)
	   call WDIALOG(1,'BOX #'//CHARNB(cnum1)
     &	//' IS NOT A TEXT STRING: TRY AGAIN',12)
	   nvp=-1
	   RETURN
	endif
c
c Start with course movements
	im=200	!for move
	ia=45		!for angle
	cs=1.		!for size
	nl=NBLANK1(text)
	do i=1,nl
	  ival1(i)=ichar(text(i:i))		!===debug
	enddo
	size=csfac*csize
	call SETCSIZE(size,isize)		!set isize
	call SETFONT1(ifont)		    	!set font
c Look for a keystroke
1	continue
	imsav=im		!bug fix
	ch1=GETCH(ktype)
	ival=ichar(ch1)
c MYSTERY BUG -value of im changes when GETCH is called!!! No idea why
c but seem to be cured of im reset here (just like problem in FIXNUMB)
	im=imsav		!bug fix
c Test whether to finish
	if(ktype.eq.16) goto 99				!ESC
	if(ktype.eq.4.and.ival.eq.13) goto 99	!ENTER
c Toggle the sensitivity
	if(ktype.eq.8.and.ival.eq.82) then	!INS toggles fine/course
	   if(im.eq.10) then
		im=200	!for move
		ia=45		!for angle
		cs=1.		!for size
	   else
		im=10
		ia=5
		cs=0.1
	   endif
	goto 1	!another key
	endif
c Next keys that require text deletion and redraw
c To move text to new position marked by cursor, mark the origin with
c a diamond, and put up cursors for new origin position before
c deleting the original. NB can't use SYMBOL routine to draw this
c because it will not draw outside the graphboundary, so must use
c PLOT calls which use device (not world) coord. Have no way to
c to filled symbol when this used?
	if(ktype.eq.0.and.(ival.ge.60.and.ival.le.68)) then	!F2-F10
c	   call SYMBOL(rx,ry,-4,2.5,12)
	   call COLTYP(0)
	   call ISYMB(ix,iy,50)			!diamond at ix,iy -see below
	   call PUTCUR(5000,5000)		!start pos for cursor
	   ch=char(FNDCUR(ixnew,iynew))		!posn to move text #it to
	endif
c First delete text
	call COLTYP(-1)				!erase
	call JUSTIFYSTRING(ix,iy,text(1:nl),iangle,isize,ixjus,iyjus)
c and delete the box, if there is one
	if(idraw.eq.-1) call TEXTBOX(ix,iy,text(1:nl),iangle,
     & isize,ixjus,iyjus,rxbox,rybox,-1)		!delete box
	if(ktype.eq.0.and.(ival.ge.60.and.ival.le.68)) then	!F2-F10
	   call ISYMB(ix,iy,50)		!diamond at ix,iy -see below
	endif
	call COLTYP(0)				!restore
c
c Check which key hit
c
	if(ktype.eq.0.and.(ival.ge.60.and.ival.le.68)) then	!F2-F10
	   ix=ixnew
	   iy=iynew
	else if(ktype.eq.8) then
c Section for ktype=8 (arrow key)
c calc new position
	   if(ival.eq.77.or.ival.eq.73.or.ival.eq.81) ix=ix+im	!right
	   if(ival.eq.75.or.ival.eq.71.or.ival.eq.79) ix=ix-im	!left
	   if(ival.eq.71.or.ival.eq.72.or.ival.eq.73) iy=iy+im	!up
	   if(ival.eq.79.or.ival.eq.80.or.ival.eq.81) iy=iy-im	!down
	   if(ival.eq.83) then			!DEL toggles idraw
		if(idraw.ne.0) then
		   idraw=0
		else
		   idraw=1
		endif
c if newtext deleted (nvp=1-5) then remove it altogether, and reduce ntext:
c problem is that have not got access to other newtext parameters which
c need to be rearranged, so set ntdel flag and use it to rearrange newtext
c after leaving FIXTEXT (ntdel set=0 on entry so no need to set before call)
		if(nvp.ge.11.and.nvp.le.30) ntdel=1
c		pause 'toggle idraw'
	   endif
c
	else if(ktype.eq.3) then
	   if(ival.eq.43) size=size+cs		!plus
	   if(ival.eq.45) size=size-cs		!minus
	   call SETCSIZE(size,isize)		!set isize
c
	else if(ktype.eq.2) then
	   if(ival.eq.42.or.ival.eq.47) then
	      if(ival.eq.42) ifont=ifont+1  		!*
	      if(ival.eq.47) ifont=ifont-1  		!/
	      call SETFONT1(ifont)		!set font
c		To reset font for param values must regenerate PARVAL
		if(nvp.eq.6) call PARTEXT(text,ifitype,theta,ncomp,
     &      ifont,size)
		nl=NBLANK1(text)
	   else if(ival.eq.98.or.ival.eq.66) then	 !'b' or 'B' toggles box round text
		idraw=-idraw             !toggle idraw
	   else if(ival.eq.114) then		!'r' increases iangle
		iangle=iangle+ia
	   else if(ival.eq.82) then		!'R' decreases iangle
		iangle=iangle-ia
	   endif
	endif
c and redraw
c	print 10,idraw
c10	format(' idraw(in fixtext)= ',i8)
c
	iangle=mod(iangle,360)	!keep in range 0-360
	if(iangle.lt.0)iangle=360+iangle
	if(idraw.ne.0) call JUSTIFYSTRING(ix,iy,text(1:nl),iangle,isize,
     & ixjus,iyjus)
c Now call TEXTBOX to record position of the new text string (and, if
c idraw=-1, to actually draw the box). Must do after every alteration
c so it can be correctly erased (and box drawn if req)
	imode=0				!return box position
	if(idraw.eq.-1) imode=1		!draw box too
	call TEXTBOX(ix,iy,text(1:nl),iangle,isize,
     & ixjus,iyjus,rxbox,rybox,imode)
c Adjust csfac for return to calling prog
	csfac=size/csize
	goto 1	!another move?
c
99	continue
	RETURN
	end

	subroutine ISYMB(ix,iy,n)	!diamond at ix,iy
	   call PLOT(ix,iy+n,0)
	   call PLOT(ix+n,iy,1)
	   call PLOT(ix,iy-n,1)
	   call PLOT(ix-n,iy,1)
	   call PLOT(ix,iy+n,1)
	RETURN
	end
