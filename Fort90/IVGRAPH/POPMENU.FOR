	subroutine POPMENU(ixlo,iylo,iyhi,strings,n,ic,icf,icup,ibk,
     & title,helps,nhelp,iline,charout,ival)

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
c	N          : array dimension
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
	character*1		chararray(32),charout,getch
	integer*1		array1(:),array2(:)
	integer*1		arr1(:),arr2(:),arr3(:),arr4(:)
	logical		kbhit
	logical mouse_on,mous_set,outext
	common/mousval/mouse_on,nbutton

	allocatable :: array1,array2
	allocatable :: arr1,arr2,arr3,arr4


c
	if(ic.eq.icup) then
		invc=ibk
	else
		invc=icup
	endif

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
	call windim(ixlo,iylo,ixhi,iyhi,idimen,strings,n,ixc0,iyc0)
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
c NB do not call with n+1 (to leave room for extra 'help' line) because then
c would attempt to look at strings(n+1) which may not exist -call
c with ixc0=-1 instead
	call windim(ixlo,iylo,ixhi,iyhi,idimen,strings,n,ixc0,iyc0)
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
c=c Write first line highlighted
c=	call fillwin(ixlo+5,iyc,ixhi-5,iyc+16,ic)
c=	call isetstring(ixc0,iyc,strings(1),ibk,icup,ic,chararray(1))
c=C Write rest of lines
c=	iyc=iyc-16
c=  	do i=2,n
c=	   call isetstring(ixc0,iyc,strings(i),ic,icup,ibk,chararray(i))
c=	   iyc=iyc-16
c=	enddo
c Check input value of iline
	if(iline.lt.1) iline=1
	if(iline.gt.n) iline=n
c Draw text in frame
  	do i=1,n
	  if(i.ne.iline) then
	   call isetstring(ixc0,iyc,strings(i),ic,icup,ibk,chararray(i))
	  else
	   call fillwin(ixlo+5,iyc,ixhi-5,iyc+16,ic)
	   call isetstring(ixc0,iyc,strings(i),ibk,invc,ic,chararray(i))
	  endif
	  iyc=iyc-16
	enddo
c enter automatic last line
	call WRSTRING18(ixc0,iyc,'F1=HELP',icup,ibk)
c
c
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
	   nhelp=nhelp+9
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
	   nhelp=nhelp+8
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
	   nhelp=nhelp+9
	endif
c
	call rdgwin1(ixlo,iylo,ixhi,iyhi,array2)
	call rdframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,arr3,arr4,nh,nv)
	call show_mouse()
c
c==	i=1
c==	iline=i
	i=iline
	jy=iyc0 - 16*(iline-1)		!y-coord for line #iline
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
		   	   if(i.gt.1) then
		   	      jy=jy+16
			      i=i-1
			      iline=i
			      call FILLWIN(ixlo+5,jy,ixhi-5,jy+16,
     &                                  ic)
			      call ISETSTRING(ixc0,jy,strings(i),
     &		   	ibk,invc,
     &		   	ic,chararray(i))
		   	   else		!go to bottom=string(n)
				jy=iyc0-(n-1)*16
				i=n
				iline=i
				call FILLWIN(ixlo+5,jy,ixhi-5,jy+16,ic)
				call ISETSTRING(ixc0,jy,strings(i),ibk,invc,
     &				ic,chararray(i))
		   	   endif
		      case(80)			!down arrow
		   	   call FILLWIN(ixlo+5,jy,ixhi-5,jy+16,ibk)	!un-highlight
		   	   call ISETSTRING(ixc0,jy,strings(i),ic,icup,
     &	     		     ibk,chararray(i))
c==		   	   if(jy-16.gt.iylo1) then
		   	   if(i.lt.n) then
c==			   	call FILLWIN(ixlo+5,jy,ixhi-5,jy+16,ibk)
c==			   	call ISETSTRING(ixc0,jy,strings(i),ic,icup,
c==     &			     ibk,chararray(i))
			   	jy=jy-16
			     	i=i+1
			     	iline=i
			     	call FILLWIN(ixlo+5,jy,ixhi-5,jy+16,ic)
			     	call ISETSTRING(ixc0,jy,strings(i),ibk,invc,
     &			    ic,chararray(i))
		   	   else		!go to top = string(1)
		    		jy=iyc0
			    	i=1
				iline=i
				call FILLWIN(ixlo+5,jy,ixhi-5,jy+16,ic)
				call ISETSTRING(ixc0,jy,strings(i),ibk,invc,
     &				ic,chararray(i))
		   	   endif
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
			iline=1
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
			i=1
			k=i
			k0=i
			jy=iyc0
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
c	  else if (ix.gt.ixlo+5.and.ix.lt.ixhi-5.and.iy.gt.iylo+16.
c     &           and.iy.lt.iyc0+16) then		!click on a line
c=	  else if (ix.gt.ixlo+5.and.ix.lt.ixhi-5.and.iy.gt.iylo+32.
c=    &           and.iy.lt.iyc0+16) then		!click on a line
	  else if (ix.gt.ixlo+5.and.ix.lt.ixhi-5.and.iy.gt.iylo1+16.
     &           and.iy.lt.iyc0+16) then		!click on a line
		call timer(it)    ! for double clicking
		if(title.eq.'nil') goto 1
		call hide_mouse()
c==		call wrstring18(200,440,'WHAT I HAVE TO DO NOW ?',14,1)
c unhighlight present line
		call fillwin(ixlo+5,ky,ixhi-5,ky+16,ibk)
		call ISETSTRING(ixc0,ky,strings(k),ic,icup,ibk,
     &		chararray(k))
c===========problem with next 2 lines?
		ky=iy-modulo(iy+16,16)
c==		k=n-int(float(iy-iylo-16)/16)
		k=n-int(float(iy-iylo1-16)/16)
c highlight line on which mouse clicked
		call fillwin(ixlo+5,ky,ixhi-5,ky+16,ic)
		call ISETSTRING(ixc0,ky,strings(k),ibk,invc,ic,chararray(k))
c		call anykey		!debug
		i=k
		jy=ky
		iline=i
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
      call show_mouse()
c
909	continue
	call hide_mouse()
	RETURN
	end
