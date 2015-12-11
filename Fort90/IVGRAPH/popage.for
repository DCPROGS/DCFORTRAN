	subroutine POPAGE(ixlo,iylo,iyhi,strings,n,ic,icf,icup,ibk,
     & title,helps,nhelp,iline,charout,ival,nrow)

c**************************************************************************
c
c	Ioana Vais
c	Pharmacology
c
c**************************************************************************
c	SUBROUTINE TO DRAW A MENU WITH READ/WRITE WINDOW INCLUDED
c	Assumes that we are in graphics (VGA, mode 18) before calling
c NB Assumes mouse_on in common was set by prelim call to SETMOUSE
c
c Modif 11/02/97 03:54pm so centre button works like double click
c Modif 01/14/94 09:30pm so that menu appears initially with highlight
c on line number specified by input value of iline (if in range 1 to n)
c
c Modif 12/02/93 07:27am so help strings (nhelp in number) are parameters
c  -these can describe what cols of table represent.  If present (nhelp>0)
c  they are added before the contents of help window below. (as POPTABLE)
c
c	IXLO,IYLO,IYHI: coordinates of lower left corner of the window
c Modif 11/21/93 09:24am so all three are parameters, but call with iyhi=-1
c		if window position to be defined by bottom left (ixlo,iylo)
c		OR call with iylo=-1 to if window position to be defined by
c		top left (ixlo,iyhi)
c	STRINGS(N) : array of strings you want to write in the window
c	N       : array dimension
c	NROW	     : number of rows shown at a time in window
c	IC         : color of writing
C	ICUP       : color upper case
c	IBK        : color of background
c	TITLE      : menu title -> 'nil' = no title
c						   = scroll bar
c						   = dragging from all menu surface
c					   'box' = dragging box
c						   = scroll bar
c						   = mouse click
c					    title= menu with dragging title
c						   = scroll bar
c						   = mouse click

c	iline       : line number returned for options (and input
c			  values specifies line highlighted initially)
c     CHAROUT    : character pressed returned for options
c	IVAL       : ICHAR(CHAROUT) ;returned for options

c     first time left button pressed: if inside menu/box/title draw frame;
c	if outside menu doesn't do anything; then move mouse,
c	draw/redraw frame at mouse position when left button pressed;
c	left button released -> draw menu at the new position
c	right button pressed -> go out
c	for box/title options : mouse click on line

c*************************************************************************
c
c	CALLING : WINDIM,FRAMEFILL,RDFRAME,WRFRAME,DRAWFRAME,DRAWREC
C		  : SHOW_MOUSE,HIDE_MOUSE,MOUSE_UPDATE,SET_MOUSE
C	        : WRSTRING18,ISETSTRING
C		  : RDGWIN1,WRGWIN1,FILLWIN
C
c*************************************************************************

	character*(*) strings(n),helps(nhelp),title
	character*55 helpst(30)		!help strings
	character*1		chararray(:),charout,getch
	integer*1		array1(:),array2(:)
	integer*1		arr1(:),arr2(:),arr3(:),arr4(:)
	logical kbhit
c	logical moved
	character*11 cnum
	logical mouse_on,mous_set,outext
	common/mousval/mouse_on,nbutton

	allocatable :: array1,array2
	allocatable :: arr1,arr2,arr3,arr4
	allocatable :: chararray

	allocate(chararray(n))
c
	if(ic.eq.icup) then
		invc=ibk
	else
		invc=icup
	endif
	lpage=int(float(n)/float(nrow))+1

c	icf=icup		!frame colour -now parameter!
c
c NB could skip mouse sections if istatus.ne.65535 (mouse_on=false)
	mous_set=.false.	!initialise
	call show_mouse()
c
c==	iyhi=-1	!now in call
	length=NBLANK(title)
	if(title.eq.'nil'.or.title.eq.'box') then
	   goto 200
	else
	   iyc0=-1
	endif
c Call with n+1 to allow room for extra line 'Type ESC to finish'
200	continue
	ixc0=-1
c NB do not call with n+1 (to leave room for extra 'help' line) because then
c would attempt to look at strings(n+1) which may not exist -call
c with ixc0=-1 instead
C	npl=2
C	if(n.gt.nrow) npl=3
	npl=1
	call windim2(ixlo,iylo,ixhi,iyhi,idimen,strings,n,nrow+npl,
     &ixc0,iyc0)
	idx=ixhi-ixlo
	idy=iyhi-iylo
	dx=float(idx+1)/2
	dy=4*float(idy+1)
	nh=int(dx)
	nv=int(dy)
	if(iyhi.gt.479) then
	   iylo=479-idy
	endif
	if(iylo.lt.0) then
	   iylo=0
	   iyhi=iylo+idy
	endif
	iylo1=iylo+16	!excluding bottom (non-scrolling) line
	if(ixhi.gt.639) then
	   ixlo=639-idx
	endif
c==	iyhi=-1	!now in call
	if(title.eq.'nil'.or.title.eq.'box') then
	   goto 202
	else
	   iyc0=-1
	endif
c Call with n+1 to allow room for extra line 'Type ESC to finish'
202	continue
	ixc0=-1
c#### ixco=-2
c NB do not call with n+1 (to leave room for extra 'help' line) because then
c would attempt to look at strings(n+1) which may not exist -call
c with ixc0=-1 instead
	npl=1
	call windim2(ixlo,iylo,ixhi,iyhi,idimen,strings,n,nrow+npl,
     & ixc0,iyc0)
	allocate(array1(idimen))
	allocate(array2(idimen))
	allocate(arr1(nh),arr2(nh),arr3(nv),arr4(nv))
	ixlo0=ixlo
	iylo0=iylo
	ixhi0=ixhi
	iyhi0=iyhi
	call hide_mouse()
	call rdgwin1(ixlo,iylo,ixhi,iyhi,array1)
	call FRAMEFILL(ixlo,iylo,ixhi,iyhi,icf,ibk)
	iyc=iyc0
c Check input value of iline
	if(iline.lt.1) iline=1
	if(iline.gt.n) iline=n
c Draw text in frame
	ipage=int(float(iline)/float(nrow))+1
	ifirst=nrow*(ipage-1)+1
	ilast=ifirst+nrow-1
c	ifirst=1	!string shown in first row in window
c	ilast=nrow	!string shown in last row in window
	if(ilast.gt.n) ilast=n
	do j=ifirst,ilast
	  if(j.ne.iline) then
	   call isetstring(ixc0,iyc,strings(j),ic,icup,ibk,chararray(j))
	  else
	   call fillwin(ixlo+5,iyc,ixhi-5,iyc+16,ic)
	   call isetstring(ixc0,iyc,strings(j),ibk,invc,ic,chararray(j))
	  endif
	  iyc=iyc-16
	enddo
c	iyc3=iyc+12	!lower edge of area to clear so footer not cleared
c enter automatic last line
      iycp=iyc0-16*(nrow+1)
	call WRSTRING18(ixc0,iycp+16,'F1 Help',icup,ibk)
	call intconv(ipage,cnum)
	call WRSTRING18(ixc0,iycp,'Page '//cnum(1:2),icup,ibk)
	if (n.le.nrow) goto 100
c
c
100   continue
	if(nhelp.ge.1) then
	   do i=1,nhelp
		helpst(i)=helps(i)
	   enddo
	   helpst(nhelp+1)='--------------'
	   nhelp=nhelp+1	!for line above
	endif
c
	if(title.eq.'box') then
	   ixmh=ixhi-5
	   ixml=ixhi-15
	   iymh=iyhi-5
	   iyml=iyhi-15
c=	   call drawrec(ixml,iyml,ixmh,iymh,ic,icup)
	   call drawrec(ixml,iyml,ixmh,iymh,icup,ic)
c define help strings
	   helpst(nhelp+1)='CHOOSE OPTION:'
	   helpst(nhelp+2)='  Move bar with arrows, or'
	   helpst(nhelp+3)='  Hit highlighted letter, or'
	   helpst(nhelp+4)='  Click/double click option'
	   helpst(nhelp+5)='LEAVE MENU:'
	   helpst(nhelp+6)='  Hit ENTER or R/C mouse button'
	   helpst(nhelp+7)='  Hit ESC to cancel menu'
	   helpst(nhelp+8)='MOVE MENU: drag (L button):'
	   helpst(nhelp+9)='  on box in top R corner'
	   helpst(nhelp+10)=' Page-up/down move 1 page'
	   helpst(nhelp+11)=' Home/end go to start/end'
	   nhelp=nhelp+11
	else if(title.eq.'nil') then
	   ixmh=ixhi
	   ixml=ixlo
	   iymh=iyhi
	   iyml=iylo+8
c define help strings
	   helpst(nhelp+1)='CHOOSE OPTION:'
	   helpst(nhelp+2)='  Move bar with arrows, or'
	   helpst(nhelp+3)='  Hit highlighted letter'
	   helpst(nhelp+4)='LEAVE MENU:'
	   helpst(nhelp+5)='  Hit ENTER or R mouse button'
	   helpst(nhelp+6)='  Hit ESC to cancel menu'
	   helpst(nhelp+7)='MOVE MENU: drag (L button):'
	   helpst(nhelp+8)='  anywhere within menu'
	   helpst(nhelp+10)=' Page-up/down move 1 page'
	   helpst(nhelp+11)=' Home/end go to start/end'
	   nhelp=nhelp+11
	else
	   deltax=(float(idx)-float(length)*8)/2
	   ideltax=int(deltax-modulo(deltax,8.))
	   ixml=ixlo+ideltax
	   ixmh=ixhi-ideltax
	   if(modulo(length,2).ne.0) ixmh=ixmh+8
	   iymh=iyhi
	   iyml=iyhi-20
c=	   call drawrec(ixml,iyml,ixmh,iymh,ic,icup)
	   call drawrec(ixml,iyml,ixmh,iymh,icup,ic)
	   call wrstring18(ixml+8,iyc0+32,title,icf,ic)
c define help strings
	   helpst(nhelp+1)='CHOOSE OPTION:'
	   helpst(nhelp+2)='  Move bar with arrows, or'
	   helpst(nhelp+3)='  Hit highlighted letter, or'
	   helpst(nhelp+4)='  Click/double click option'
	   helpst(nhelp+5)='LEAVE MENU:'
	   helpst(nhelp+6)='  Hit ENTER or R mouse button'
	   helpst(nhelp+7)='  Hit ESC to cancel menu'
	   helpst(nhelp+8)='MOVE MENU: drag (L button):'
	   helpst(nhelp+9)='  on title area'
	   helpst(nhelp+10)=' Page-up/down move 1 page'
	   helpst(nhelp+11)=' Home/end go to start/end'
	   nhelp=nhelp+11
	endif
c
	call rdgwin1(ixlo,iylo,ixhi,iyhi,array2)
	call rdframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,arr3,arr4,nh,nv)
	call show_mouse()
c
c==	i=1
c==	iline=i
	i=iline
	iyc=iyc0
c	iyc1=iyc-16	!store y value for first line of data
c	iyc2=iyc1+18	!upper edge of area to clear so header not cleared
c	jy=iyc0 - 16*(iline-1)		!y-coord for line #iline
	jy=iyc0 - 16*(iline-nrow*(ipage-1)-1)
	k=i
	k0=i
	ky=jy
c==	iline=1
c
1	continue	!return to look for next keyboard/mouse hit
c1	call mouse_update(ibutton,ix,iy)
	if(kbhit()) then
	   charout=getch(ktype)
	   ival=ichar(charout)
	   if(ktype.eq.0.and.ival.eq.59) then	!F1=help
		ixloh=ixlo+50     !position for help box
		iyhih=iyhi-50
		if(ixlo.gt.639-50) ixloh=ixlo-50
		if(iylo.lt.50) iyhih=iyhi+50
		call poptext(ixloh,-1,iyhih,helpst,nhelp,ibk,ic,icf)	!=popkeys
		outext=.true.
		mous_set=.false.
		call show_mouse()
	   else if(ktype.eq.3.and.ival.ge.48.and.ival.le.57) then	!option
		call chtoreal(charout,rline)
		if(int(rline).le.n) iline=int(rline)
	  	goto 98
	   else if(ival.eq.141.or.ival.eq.145.or.ival.eq.116.or.
     &	ival.eq.115) then	!move menu with ctrl-ARROW keys
c		moved=.true.
		call hide_mouse()
		call WRGWIN1(ixlo,iylo,ixhi,iyhi,array1)	!redraw orig graph
		if(ival.eq.116) then		!shift right
		   ixlo=ixlo+8
	         ixhi=ixhi+8
		else if(ival.eq.115) then		!shift left
	         ixlo=ixlo-8
	         ixhi=ixhi-8
		else if(ival.eq.141) then	!shift up
	         iylo=iylo+8
	         iyhi=iyhi+8
		else if(ival.eq.145) then	!shift down
	         iylo=iylo-8
	         iyhi=iyhi-8
		endif
		call RDGWIN1(ixlo,iylo,ixhi,iyhi,array1)  !keep new orig in array1
		call WRGWIN1(ixlo,iylo,ixhi,iyhi,array2)	!redraw menu at new pos
		ixlo0=ixlo
		iylo0=iylo
		ixhi0=ixhi
		iyhi0=iyhi
        	call show_mouse()
	   else if(ival.eq.27.or.ival.eq.13) then		!ESC or ENTER
c		pause '98.1'	!debug
	  	goto 98
	   else
c Check whether to leave via capitalised letter
	   	do j=1,n
		   if(charout.eq.chararray(j)) then	!leave via capitalised letter
		   	iline=j
c			call anykey		!debug
			goto 99
		   endif
	   	end do
	   	if(ktype.eq.8) then		!up/down arrow to scroll highlight
		   if(ix.ge.ixlo-8.and.ix.le.ixhi.and.iy.ge.iylo.
     &	     and.iy.le.iyhi+8) then
		   	call hide_mouse()					!up arrow
		   endif
	     	   select case(ival)
		      case(72)
		    	   call FILLWIN(ixlo+5,jy,ixhi-5,jy+16,ibk)	!un-highlight
		   	   call ISETSTRING(ixc0,jy,strings(i),ic,
     &	   		icup,
     &	   		ibk,chararray(i))
c==		   	   if((jy+32).lt.iyhi) then
		   	   if(i.gt.ifirst) then
		   	      jy=jy+16
			      i=i-1
			      iline=i
			      call FILLWIN(ixlo+5,jy,ixhi-5,jy+16,
     &                                  ic)
			      call ISETSTRING(ixc0,jy,strings(i),
     &		   	ibk,invc,
     &		   	ic,chararray(i))
		   	   else		!go to bottom=string(n)
				i=ilast
				iline=i
				jy=iyc0-(iline-ifirst)*16
				call FILLWIN(ixlo+5,jy,ixhi-5,jy+16,ic)
				call ISETSTRING(ixc0,jy,strings(i),ibk,invc,
     &				ic,chararray(i))
		   	   endif
			   call rdgwin1(ixlo,iylo,ixhi,iyhi,array2)
		      case(80)			!down arrow
		   	   call FILLWIN(ixlo+5,jy,ixhi-5,jy+16,ibk)	!un-highlight
		   	   call ISETSTRING(ixc0,jy,strings(i),ic,icup,
     &	     		     ibk,chararray(i))
		   	   if(i.lt.ilast) then
			   	jy=jy-16
			     	i=i+1
			     	iline=i
			     	call FILLWIN(ixlo+5,jy,ixhi-5,jy+16,ic)
			     	call ISETSTRING(ixc0,jy,strings(i),ibk,invc,
     &			    ic,chararray(i))
		   	   else		!go to top = string(1)
		    		jy=iyc0
			    	i=ifirst
				iline=i
				call FILLWIN(ixlo+5,jy,ixhi-5,jy+16,ic)
				call ISETSTRING(ixc0,jy,strings(i),ibk,invc,
     &				ic,chararray(i))
		   	   endif
			   call rdgwin1(ixlo,iylo,ixhi,iyhi,array2)
	   		case(73) 	!page-up
			   call FILLWIN(ixlo+5,iylo+48,ixhi-5,iyhi-48,ibk)
c		         call FRAMEFILL(ixlo,iyc3,ixhi,iyc2,icf,ibk)	!clear window
		         ifirst=ifirst-nrow	!string shown in first row in window
		         ilast=ifirst+nrow-1	!string shown in last row in window
		         if(ifirst.lt.1) then
		         	ifirst=1
		         	ilast=ifirst+nrow-1	!string shown in last row in window
		         endif
			   if(ilast.gt.n) ilast=n
		    	   jy=iyc0
			   i=ifirst
			   iline=i
			   ipage=int(float(ifirst)/float(nrow))+1
		   	   iycp=iyc0-16*(nrow+1)
			   call intconv(ipage,cnum)
	               call fillwin(ixlo+5,jy,ixhi-5,jy+16,ic)
	               call isetstring(ixc0,jy,strings(i),ibk,invc,ic,
     &		   chararray(i))
		         iyc=iyc0
		   	   iyc=iyc-16
			   do j=ifirst+1,ilast
			      call ISETSTRING(ixc0,iyc,strings(j),
     &		   	ic,icup,
     &		   	ibk,chararray(j))
		   		iyc=iyc-16
			   enddo
			   ipage=int(float(ifirst)/float(nrow))+1
			   call FILLWIN(ixlo+5,iylo+16,ixhi-5,iylo+32,ibk)
c			   call WRSTRING18(ixc0,iycp+16,'F1 Help',icup,ibk)
			   call intconv(ipage,cnum)
			   if(ipage.eq.lpage) then
			      call WRSTRING18(ixc0,iycp,'Last Page',icup,
     &		      ibk)
			   else
			      call WRSTRING18(ixc0,iycp,'Page '//cnum(1:2),
     &		      icup,ibk)
			   endif
			   call rdgwin1(ixlo,iylo,ixhi,iyhi,array2)
	   		case(81) 		!page-down
		         ifirst=ifirst+nrow	!string shown in first row in window
			   if(ifirst.ge.n) then
				ifirst=ifirst-nrow
			   else if(ifirst.lt.n) then
		         ilast=ifirst+nrow-1	!string shown in last row in window
			   call FILLWIN(ixlo+5,iylo+48,ixhi-5,iyhi-48,ibk)
c			   call FRAMEFILL(ixlo,iyc3,ixhi,iyc2,icf,ibk)	!clear window
		         if(ilast.gt.n) then
		            ilast=n
c		   		ifirst=ilast-nrow+1
c		   	   	if(ifirst.lt.1) ifirst=1
			   endif
		    	   jy=iyc0
			   i=ifirst
			   iline=i
	               call fillwin(ixlo+5,jy,ixhi-5,jy+16,ic)
	               call isetstring(ixc0,jy,strings(i),ibk,invc,ic,
     &		   chararray(i))
			   iyc=iyc0
		   	   iyc=iyc-16
			   do j=ifirst+1,ilast
			      call ISETSTRING(ixc0,iyc,strings(j),
     &		   	ic,icup,
     &		   	ibk,chararray(j))
		   		iyc=iyc-16
			   enddo
			   ipage=int(float(ifirst)/float(nrow))+1
		   	   iycp=iyc0-16*(nrow+1)
			   call intconv(ipage,cnum)
			   call FILLWIN(ixlo+5,iylo+16,ixhi-5,iylo+32,ibk)
c			   call WRSTRING18(ixc0,iycp+16,'F1 Help',icup,ibk)
			   if(ipage.eq.lpage) then
			      call WRSTRING18(ixc0,iycp,'Last Page',icup,
     &		      ibk)
			   else
			      call WRSTRING18(ixc0,iycp,'Page '//cnum(1:2),
     &			icup,ibk)
			   endif
			   call rdgwin1(ixlo,iylo,ixhi,iyhi,array2)
			   endif
	   		case(71)	!home
			   call FILLWIN(ixlo+5,iylo+48,ixhi-5,iyhi-48,ibk)
c		         call FRAMEFILL(ixlo,iyc3,ixhi,iyc2,icf,ibk)	!clear window
		         ifirst=1	!string shown in first row in window
		         ilast=nrow	!string shown in last row in window
		         if(ilast.gt.n) ilast=n
			   i=ifirst
			   iline=i
		    	   jy=iyc0
	               call fillwin(ixlo+5,jy,ixhi-5,jy+16,ic)
	               call isetstring(ixc0,jy,strings(i),ibk,invc,ic,
     &		   chararray(i))
			   iyc=iyc0
		   	   iyc=iyc-16
			   do j=ifirst+1,ilast
			      call ISETSTRING(ixc0,iyc,strings(j),
     &		   	ic,icup,
     &		   	ibk,chararray(j))
		   		iyc=iyc-16
			   enddo
		   	   iycp=iyc0-16*(nrow+1)
			   call FILLWIN(ixlo+5,iylo+16,ixhi-5,iylo+32,ibk)
c			   call WRSTRING18(ixc0,iycp+16,'F1 Help',icup,ibk)
			   call WRSTRING18(ixc0,iycp,'Page 1',icup,ibk)
			   call rdgwin1(ixlo,iylo,ixhi,iyhi,array2)
	   		case(79) 	!end
			   call FILLWIN(ixlo+5,iylo+48,ixhi-5,iyhi-48,ibk)
c		 	   call FRAMEFILL(ixlo,iyc3,ixhi,iyc2,icf,ibk)	!clear window
			   ilast=n
			   ifirst=nrow*(lpage-1)+1
			   if(ifirst.lt.1) ifirst=1
			   i=ifirst
			   iline=i
		    	   jy=iyc0
	               call fillwin(ixlo+5,jy,ixhi-5,jy+16,ic)
	               call isetstring(ixc0,jy,strings(i),ibk,invc,ic,
     &		   chararray(i))
			   iyc=iyc0
		   	   iyc=iyc-16
			   do j=ifirst+1,ilast
			      call ISETSTRING(ixc0,iyc,strings(j),
     &		   	ic,icup,
     &		   	ibk,chararray(j))
		   		iyc=iyc-16
			   enddo
		   	   iycp=iyc0-16*(nrow+1)
			   call FILLWIN(ixlo+5,iylo+16,ixhi-5,iylo+32,ibk)
c			   call WRSTRING18(ixc0,iycp+16,'F1 Help',icup,ibk)
			   call WRSTRING18(ixc0,iycp,'Last page',icup,ibk)
			   call rdgwin1(ixlo,iylo,ixhi,iyhi,array2)
	     	   end select
		   k=i
		   k0=i
		   ky=jy
		   endif
	     	   call show_mouse()
		   goto 1			!up arrow
	   endif
	endif		!end of 'if kbhit'
c
c NB check mous_set after kbhit section because could have multiple-click
c effect after leaving poptext
	if(mouse_on) then
         call show_mouse()
22	   call mouse_update(ibutton,ix,iy)
	   if(outext.and.ibutton.ne.0) goto 22
		outext=.false.
	   if(ibutton.eq.0) mous_set=.true.
	endif
	if(mous_set) then
	 if(ibutton.ne.0) mous_set=.false.	!until reset above
	 if(ibutton.eq.1) then
 	  if(title.eq.'nil') then
 		ixmh=ixhi
 		ixml=ixlo
 		iymh=iyhi
 		iyml=iylo+8
 	  else if(title.eq.'box') then
 	    	ixmh=ixhi-5
 	    	ixml=ixhi-15
 	    	iymh=iyhi-5
 	    	iyml=iyhi-15
 	  else
 	    	ixmh=ixhi-ideltax
 	    	ixml=ixlo+ideltax
		if(modulo(length,2).ne.0) ixmh=ixmh+8
 	    	iymh=iyhi
 	    	iyml=iyhi-20
 	  endif
	  if(ix.lt.ixmh.and.ix.gt.ixml.and.iy.lt.iymh.and.
     &			iy.gt.iyml) then	     !hit on drag area
		idcx=abs(ix-ixlo0)
		idcy=abs(iy-iylo0)
		call hide_mouse()
		call drawframe(ixlo,iylo,ixhi,iyhi,icf)
2		call mouse_update(ibutton,ix,iy)
		if(ibutton.eq.1) then		!left button
			if(ix.eq.ix0.and.iy.eq.iy0) goto 3
			call hide_mouse()
			if(iline.eq.1) then
			   call drawframe(ixlo,iylo,ixhi,iyhi,ibk)
			   iline=0
			else
			   call wrframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &				arr3,arr4,nh,nv)
			endif
			ixlo=(ix-idcx)-modulo((ix-idcx),8)  !
			iylo=(iy-idcy)-modulo((iy-idcy),16) !
			if(ixlo.lt.0) then
				ixlo=0
			endif
			if(iylo.lt.0) then
				iylo=0
			endif
			iyhi=iylo+idy
			ixhi=ixlo+idx
			if(ixhi.gt.639) then
				ixhi=639
				ixlo=ixhi-idx
			endif
			if(iyhi.gt.479) then
				iyhi=479
				iylo=iyhi-idy
			endif
			iylo1=iylo+16	!excluding bottom (non-scrolling) line
			call rdframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,arr3,arr4,
     &			nh,nv)
			call drawframe(ixlo,iylo,ixhi,iyhi,icf)
3			continue
			ix0=ix
			iy0=iy
			call show_mouse()
			goto 2
		else if(ibutton.eq.0) then		!release mouse
c			iline=1
			call hide_mouse()
			call wrframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &				arr3,arr4,nh,nv)
			if(ixlo.lt.0.or.ixhi.ge.640.or.
     &			iylo.lt.0.or.iyhi.ge.480) then
				ixlo=ixlo0
				iylo=iylo0
				ixhi=ixhi0
				iyhi=iyhi0
			endif
			iylo1=iylo+16	!excluding bottom (non-scrolling) line
			call wrgwin1(ixlo0,iylo0,ixhi0,iyhi0,array1)
			call rdgwin1(ixlo,iylo,ixhi,iyhi,array1)
			call wrgwin1(ixlo,iylo,ixhi,iyhi,array2)
			ixlo0=ixlo
			iylo0=iylo
			ixhi0=ixhi
			iyhi0=iyhi
			ix0=ix
			iy0=iy
			if(title.eq.'nil'.or.title.eq.'box') then
				iyc0=iyhi-32
			else
				iyc0=iyhi-48
			endif
			ixc0=ixlo+8
c			i=1
			k=i
			k0=i
			jy=iyc0-(iline-ifirst)*16
			ky=jy
			call show_mouse()
			goto 1
		else if(ibutton.eq.2.or.ibutton.eq.4) then !right/centre button
			call hide_mouse()
			call wrframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &				arr3,arr4,nh,nv)
c		pause '98.2'	!debug
			goto 98
		endif
	  else if (ix.gt.ixlo+5.and.ix.lt.ixhi-5.and.iy.gt.iylo1+16.
     &           and.iy.lt.iyc0+16) then		!click on a line
		call timer(it)    ! for double clicking
		if(title.eq.'nil') goto 1
		call hide_mouse()
c unhighlight present line
		call fillwin(ixlo+5,ky,ixhi-5,ky+16,ibk)
		call ISETSTRING(ixc0,ky,strings(k),ic,icup,ibk,
     &		chararray(k))
c===========problem with next 2 lines?
		ky=iy-modulo(iy+16,16)
ccc		k=ilast-int(float(iy-iylo1-16)/16)
		k=ifirst+int(float(iyc0+16-iy)/16.)
c highlight line on which mouse clicked
		call fillwin(ixlo+5,ky,ixhi-5,ky+16,ic)
		call ISETSTRING(ixc0,ky,strings(k),ibk,invc,ic,chararray(k))
c		call anykey		!debug
		i=k
		jy=ky
		iline=i
	      call rdgwin1(ixlo,iylo,ixhi,iyhi,array2)
		call show_mouse()
		if(k.eq.k0)then
			it2=it
 			idt=it2-it1
 			if(idt.lt.25) then
				goto 98
 			else
 				k0=k
 				it1=it
 				goto 1
 			endif
		endif
		k0=k
		it1=it
		goto 1
	  else
		goto 1
	  endif

	 else if(ibutton.eq.2) then		!right button
c		pause '98.3'	!debug
		goto 98
	 else if(ibutton.eq.4) then		!centre button
		goto 98
	 else
		goto 1
	 endif
	endif		!end of if(mous_set)
	goto 1
c
c If leave with enter/right button exit, replace charout by the value
c appropriate to the line (#iline) which is highlighted
98	continue
c	pause '98'		!debug
	if(ival.eq.27) then
	   iline=0
	else		!enter/right button
	   charout=chararray(iline)
	   ival=ichar(charout)
	endif
c Jump to 99 if leave via coloured letter
99	continue
c	pause '99'		!debug
	call hide_mouse()
	call WRGWIN1(ixlo,iylo,ixhi,iyhi,array1)
	deallocate(array1)
	deallocate(array2)
	deallocate(arr1,arr2,arr3,arr4)
	deallocate(chararray)
      call show_mouse()
c
909	continue
	call hide_mouse()
	RETURN
	end
