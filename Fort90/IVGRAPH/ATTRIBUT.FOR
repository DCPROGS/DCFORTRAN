c**************************************************************************

	subroutine attributes(ixlo,iylo,iyhi,strings,n,ic,ibk,title,
     &		    helps,nhp,lcolor,ltype,number)

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
c Modif 12/02/93 07:27am so help strings (nhelp in number) are parameters
c  -these can describe what cols of table represent.  If present (nhelp>0)
c  they are added before the contents of help window below. (as POPTABLE)
c
c	IXLO,IYLO,IYHI: coordinates of lower left corner of the window
c Modif 11/21/93 09:24am so all three are parameters, but call with iyhi=-1
c		if window position to be defined by bottom left (ixlo,iylo)
c		OR call with iylo=-1 to if window position to be defined by
c		top left (ixlo,iyhi)
c	strings(irec,nmax) : array of strings you want to write in the window
c	LTYPE(N)    : linetype of the line currently on screen
c	LCOLOR(N)   : color of each line
c	N    		: array dimension ;nr of lines on the display
c	IC      	: color of writing
c	IBK       	: color of background
c	TITLE      	: menu title -> 'nil' = no title
c						   = scroll bar
c						   = dragging from all menu surface
c					   'box' = dragging box
c						   = scroll bar
c						   = mouse click
c					    title= menu with dragging title
c						   = scroll bar
c						   = mouse click
c	NUMBER	:49 (corresponding to char '1') COLOR MENU
c	      	:50 (corresponding to char '2') TYPE  MENU
c     YOU MAY CHANGE THEM ACCORDING TO YOUR MENU SETTINGS!!!!
C
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
c	FOR MENU DRAWING ONLY,I USED VILINE SIMILAR  WITH DC CURVED LINES
c     THE PROGRAM RETURNS LTYPE CORRESPONDING TO ITYPE FOR CURVED LINES
c*************************************************************************


	character*1 	strings(n)
	character*(*)	helps(nhp),title
	character*55 	helpst(30)		!help strings
	character*1		charout,getch
	integer*1		array1(:),array2(:),array3(:)
	integer*1		arr1(:),arr2(:),arr3(:),arr4(:)
	integer*4		ltype(n),lcolor(n),lattr(20)
	logical		kbhit,left
	logical mouse_on,mous_set,outext
	common/mousval/mouse_on,nbutton

	allocatable :: array1,array2,array3
	allocatable :: arr1,arr2,arr3,arr4
	left=.true.
	isym=12
	nhelp=nhp
	icf1=0
	if(ibk.le.8) then
	   icf=ibk+8		!frame colour
	else
	   icf=ibk-8
	endif		!frame colour
	icq=14
c     NB could skip mouse sections if istatus.ne.65535 (mouse_on=false)
	mous_set=.false.	!initialise
	call show_mouse()
	ixhit=639
	iyhit=479
	ixlot=568
	length=NBLANK(title)
	if(title.eq.'nil'.or.title.eq.'box') then
	   goto 200
	else
	   iyc0=-1
	endif
200	continue
	ixc0=-1
	call windim(ixlo,iylo,ixhi,iyhi,idimen,strings,n,ixc0,iyc0)
	ixhi=ixhi+104
	idimen=(iyhi-iylo+9)*(ixhi-ixlo+9)+1
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
	endif
	iylo1=iylo+16	!excluding bottom (non-scrolling) line
	if(ixhi.gt.ixlot-1) then
	   ixlo=ixlot-1-idx
	endif
	if(title.eq.'nil'.or.title.eq.'box') then
	   goto 202
	else
	   iyc0=-1
	endif
202	continue
	ixc0=-1
	call windim(ixlo,iylo,ixhi,iyhi,idimen,strings,n,ixc0,iyc0)
	ixhi=ixhi+104
	idimen=(iyhi-iylo+9)*(ixhi-ixlo+9)+1
	allocate(array1(idimen))
	allocate(array2(idimen))
	allocate(arr1(nh),arr2(nh),arr3(nv),arr4(nv))
	ixlo0=ixlo
	iylo0=iylo
	ixhi0=ixhi
	iyhi0=iyhi
	ict=15
	call hide_mouse()
c	draw line color/line type menu
	select case(number)
	   case(49)    	!only if press '1'
		nt=16
		iylot=160
		ixlot=584
		idimen3=(iyhit-iylot+9)*(ixhit-ixlot+9)+1
		allocate(array3(idimen3))
		call rdgwin1(ixlot,iylot,ixhit,iyhit,array3)
		call color_type(ixlot,iylot,ixhit,iyhit,nt,ibk,ict,
     &	ix_down0,iy_down0,idya,isizex,isizey)
		do m=1,n
		   lattr(m)=lcolor(m)
		enddo
	   case(50)		!only if press '2'
		nt=6
		iylot=320
		ixlot=568
		idimen3=(iyhit-iylot+9)*(ixhit-ixlot+9)+1
		allocate(array3(idimen3))
		call rdgwin1(ixlot,iylot,ixhit,iyhit,array3)
		call line_type(ixlot,iylot,ixhit,iyhit,nt,ibk,ict,
     &	ix_down0,iy_down0,idya,isizex,isizey)
		do m=1,n
		   lattr(m)=ltype(m)
		enddo
	end select
c	draw the main menu:
	call rdgwin1(ixlo,iylo,ixhi,iyhi,array1)
	iyc=iyc0
	call framefill(ixlo,iylo,ixhi,iyhi,ic,ibk)
	call drawframe(ixlo+5,iyc,ixhi-5,iyc+16,icf1)
	call drawframe(ixlo+6,iyc+1,ixhi-6,iyc+15,icf1)
	do i=1,n
	   call viline(ixc0+8,iyc+8,ixhi-16,iyc+8,lcolor(i),ltype(i))
	   call wrstring18(ixc0,iyc,strings(i),ic,ibk)
	   iyc=iyc-16
	enddo
c 	enter automatic last line:
	call WRSTRING18(ixc0,iyc,'F1=HELP',4,ibk)
	if(nhelp.ge.1) then
	   do i=1,nhelp
		helpst(i)=helps(i)
	   enddo
	   helpst(nhelp+1)='----------------------------'
	   nhelp=nhelp+1	!for line above
	endif
	if(title.eq.'box') then
	   ixmh=ixhi-5
	   ixml=ixhi-15
	   iymh=iyhi-5
	   iyml=iyhi-15
	   call drawrec(ixml,iyml,ixmh,iymh,icf,ic)
c        define help strings:
	   helpst(nhelp+1)='CHOOSE OPTION:'
	   helpst(nhelp+2)='  Move bar with arrows, or'
	   helpst(nhelp+3)='  Hit highlighted letter, or'
	   helpst(nhelp+4)='  Click option with mouse (L)'
	   helpst(nhelp+5)='LEAVE MENU:'
	   helpst(nhelp+6)='  Hit ENTER or R mouse button'
	   helpst(nhelp+7)='  Hit ESC to cancel menu'
	   helpst(nhelp+8)='MOVE MENU: drag (L button):'
	   helpst(nhelp+9)='  on box in top R corner'
	   nhelp=nhelp+9
	else if(title.eq.'nil') then
	   ixmh=ixhi
	   ixml=ixlo
	   iymh=iyhi
	   iyml=iylo+8
c 	   define help strings:
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
	   ideltax=int(deltax-mod(deltax,8))
	   ixml=ixlo+ideltax
	   ixmh=ixhi-ideltax
	   if(mod(length,2).ne.0) ixmh=ixmh+8
	   iymh=iyhi
	   iyml=iyhi-20
	   call drawrec(ixml,iyml,ixmh,iymh,icf,ic)
	   call wrstring18(ixml+8,iyc0+32,title,ibk,ic)
c 	   define help strings:
	   helpst(nhelp+1)='SELECT LINE :'
	   helpst(nhelp+2)='  Move bar with ARROWS, or Click'
	   helpst(nhelp+3)='  LEFT MOUSE BUTTON on selected'
	   helpst(nhelp+4)='  line inside LINE MENU'
	   helpst(nhelp+5)='SELECT LINE ATTRIBUTE (COLOR/TYPE)'
	   helpst(nhelp+6)='  Press RIGHT ARROW and'
	   helpst(nhelp+7)='  Move bar with ARROWS, or Click'
	   helpst(nhelp+8)='  LEFT MOUSE BUTTON on selected'
	   helpst(nhelp+9)='  line inside COLOR/TYPE MENU'
	   helpst(nhelp+10)='CANCEL MENU:'
	   helpst(nhelp+11)='  Click RIGHT mouse button or Press'
	   helpst(nhelp+12)='  <Enter>/<ESC>'
	   helpst(nhelp+13)='MOVE MENU: drag LEFT MOUSE BUTTON'
	   helpst(nhelp+14)='  on title area'
	   nhelp=nhelp+14
	endif
	i=1
	iret=i
	jy=iyc0
	k=i
	ky=jy
	kt=lattr(i)+1
	kyt=iy_down0+(nt-kt)*idya
	call zsymbol(ix_down0,kyt,isym,isizex,isizey,0,ict)
	call rdgwin1(ixlo,iylo,ixhi,iyhi,array2)
	call rdframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,arr3,arr4,nh,nv)
	call show_mouse()
	iret=1
c
c===========================================================================
c
1	continue	!return to look for next keyboard/mouse hit
	if(kbhit()) then
	   charout=getch(ktype)
	   ival=ichar(charout)
	   if(ktype.eq.0.and.ival.eq.59) then	!F1=help
		ixloh=ixlo+50     !position for help box
		iyhih=iyhi-50
		if(ixlo.gt.639-50) ixloh=ixlo-50
		if(iylo.lt.50) iyhih=iyhi+50
		call poptext(ixloh,-1,iyhih,helpst,nhelp,15,2,icq)	!=popkeys
		outext=.true.
		mous_set=.false.
		call show_mouse()
	   else if(ival.eq.27.or.ival.eq.13) then		!ESC or ENTER
		goto 99
	   else
	   	if(ktype.eq.8) then		!up/down arrow to scroll highlight
		   call hide_mouse()
	     	   select case(ival)
		      case(72)
			   if(left) then
			      call drawframe(ixlo+5,jy,ixhi-5,jy+16,ibk)
			      call drawframe(ixlo+6,jy+1,ixhi-6,jy+15,ibk)
		            call zsymbol(ix_down0,kyt,isym,isizex,isizey,
     &		      ict,ict)
			   	if(i.gt.1) then
		   	         jy=jy+16
			         i=i-1
			         iret=i
		               kt=lattr(i)+1
		               kyt=iy_down0+(nt-kt)*idya
			     	   call drawframe(ixlo+5,jy,ixhi-5,jy+16,icf1)
			         call drawframe(ixlo+6,jy+1,ixhi-6,jy+15,icf1)
		               call zsymbol(ix_down0,kyt,isym,isizex,
     &			   isizey,0,ict)
		   	   	else		!go to bottom=string(n)
				   jy=iyc0-(n-1)*16
				   i=n
				   iret=i
		   		   kt=lattr(i)+1
		               kyt=iy_down0+(nt-kt)*idya
				   call drawframe(ixlo+5,jy,ixhi-5,jy+16,icf1)
			         call drawframe(ixlo+6,jy+1,ixhi-6,jy+15,icf1)
		               call zsymbol(ix_down0,kyt,isym,isizex,
     &			   isizey,0,ict)
		   	   	endif
			   else
				call zsymbol(ix_down0,kyt,isym,isizex
     &		      ,isizey,ict,ict)
			      if(kt.gt.1) then
				   kt=kt-1
		               kyt=iy_down0+(nt-kt)*idya
				   call zsymbol(ix_down0,kyt,isym,isizex
     &			   ,isizey,12,ict)
		   	       else		!go to bottom=string(n)
				   kyt=iy_down0
				   kt=nt
		               call zsymbol(ix_down0,kyt,isym,isizex
     &			   ,isizey,12,ict)
		   	      endif
				if(kt.le.nt) lattr(i)=kt-1
				if(number.eq.49) then
				   lcolor(i)=lattr(i)
				else
				   ltype(i)=lattr(i)
				endif
				call hide_mouse()
				call fillwin(ixlo+14,ky+2,ixhi-7,ky+14,ibk)
				call viline(ixc0+8,jy+8,ixhi-16,jy+8,lcolor(i),
     &			ltype(i))
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array2)
			   endif
		      case(80)			!down arrow
			   if(left) then
		         	call zsymbol(ix_down0,kyt,isym,isizex,isizey,ict,
     &		      ict)
		   	      call drawframe(ixlo+5,jy,ixhi-5,jy+16,ibk)	!un-highlight
			      call drawframe(ixlo+6,jy+1,ixhi-6,jy+15,ibk)
		   	      if(i.lt.n) then
			   	   jy=jy-16
			     	   i=i+1
			     	   iret=i
		               kt=lattr(i)+1
		               kyt=iy_down0+(nt-kt)*idya
			     	   call drawframe(ixlo+5,jy,ixhi-5,jy+16,icf1)
			         call drawframe(ixlo+6,jy+1,ixhi-6,jy+15,icf1)
		               call zsymbol(ix_down0,kyt,isym,isizex,
     &			   isizey,0,ict)
		   	   	else		!go to top = string(1)
		    		   jy=iyc0
			    	   i=1
				   iret=i
		               kt=lattr(i)+1
		   		   kyt=iy_down0+(nt-kt)*idya
				   call drawframe(ixlo+5,jy,ixhi-5,jy+16,icf1)
			         call drawframe(ixlo+6,jy+1,ixhi-6,jy+15,icf1)
				   call zsymbol(ix_down0,kyt,isym,isizex,
     &			   isizey,0,ict)
		   	   	endif
			   else
				call zsymbol(ix_down0,kyt,isym,isizex
     &		      ,isizey,ict,ict)
				if(kt.lt.nt) then
				   kt=kt+1
		   		   kyt=iy_down0+(nt-kt)*idya
				   call zsymbol(ix_down0,kyt,isym,isizex
     &			   ,isizey,12,ict)
			      else
				   kyt=iy_down0+(nt-1)*idya
				   kt=1
		               call zsymbol(ix_down0,kyt,isym,isizex
     &			   ,isizey,12,ict)
				endif
				if(kt.le.nt) lattr(i)=kt-1
				if(number.eq.49) then
				   lcolor(i)=lattr(i)
				else
				   ltype(i)=lattr(i)
				endif
				call hide_mouse()
				call fillwin(ixlo+14,ky+2,ixhi-7,ky+14,ibk)
				call viline(ixc0+8,jy+8,ixhi-16,jy+8,lcolor(i),
     &			ltype(i))
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array2)
			   endif
		   	case(75)
			   call drawframe(ixlo+5,jy,ixhi-5,jy+16,0)
			   call drawframe(ixlo+6,jy+1,ixhi-6,jy+15,0)
		         call zsymbol(ix_down0,kyt,isym,isizex,isizey,0,
     &		   ict)
			   left=.true.
		   	case(77)
			   call drawframe(ixlo+5,jy,ixhi-5,jy+16,12)
			   call drawframe(ixlo+6,jy+1,ixhi-6,jy+15,12)
		         call zsymbol(ix_down0,kyt,isym,isizex,isizey,12,
     &		   ict)
			   left=.false.
	     	   end select
		   k=i
		   ky=jy
		   endif
		call rdgwin1(ixlo,iylo,ixhi,iyhi,array2)
	     	   call show_mouse()
		   goto 1			!up arrow
	   	endif
	endif		!end of 'if kbhit'
c
c===========================================================================
c
c     NB check mous_set after kbhit section because could have multiple-click
c     effect after leaving poptext
333	if(mouse_on) then
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
		   if(mod(length,2).ne.0) ixmh=ixmh+8
 	    	   iymh=iyhi
 	    	   iyml=iyhi-20
 	  	endif
	  	if(ix.lt.ixmh.and.ix.gt.ixml.and.iy.lt.iymh.and.
     &	iy.gt.iyml) then	     !hit on drag area
		   idcx=abs(ix-ixlo0)
		   idcy=abs(iy-iylo0)
		   call hide_mouse()
		   call drawframe(ixlo,iylo,ixhi,iyhi,icf)
2		   call mouse_update(ibutton,ix,iy)
		   if(ibutton.eq.1) then		!left button
			if(ix.eq.ix0.and.iy.eq.iy0) goto 3
			call hide_mouse()
			if(iret.eq.1) then
			   call drawframe(ixlo,iylo,ixhi,iyhi,ibk)
			   iret=0
			else
			   call wrframe(ixlo,iylo,ixhi,iyhi,arr1,arr2
     &		   ,arr3,arr4,nh,nv)
			endif
			ixlo=(ix-idcx)-mod((ix-idcx),8)  !
			iylo=(iy-idcy)-mod((iy-idcy),16) !
			if(ixlo.lt.0) then
			   ixlo=0
			endif
			if(iylo.lt.0) then
			   iylo=0
			endif
			iyhi=iylo+idy
			ixhi=ixlo+idx
			if(ixhi.gt.ixlot-1) then
			   ixhi=ixlot-1
			   ixlo=ixhi-idx
			endif
			if(iyhi.gt.479) then
			   iyhi=479
			   iylo=iyhi-idy
			endif
			iylo1=iylo+16	!excluding bottom (non-scrolling) line
			call rdframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,arr3
     &		,arr4,nh,nv)
			call drawframe(ixlo,iylo,ixhi,iyhi,icf)
3			continue
			ix0=ix
			iy0=iy
			call show_mouse()
			goto 2
		   else if(ibutton.eq.0) then		!release mouse
			iret=1
			call hide_mouse()
			call wrframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &		arr3,arr4,nh,nv)
			if(ixlo.lt.0.or.ixhi.gt.ixlot-1.or.iylo.lt.0.or.
     &		iyhi.gt.479) then
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
		      jy=iyc0
		      ky=jy
		      goto 1
		   else if(ibutton.eq.2) then			!right button
			call hide_mouse()
			call wrframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &		arr3,arr4,nh,nv)
			goto 99
		   endif !end of drag
	      else if (ix.gt.ixlo+5.and.ix.lt.ixhi-5.and.iy.gt.iylo1+16.
     &      and.iy.lt.iyc0+16) then		!click on a line
		   call hide_mouse()
		   left=.true.
		   if(title.eq.'nil') goto 1
		   call hide_mouse()
		   call drawframe(ixlo+5,ky,ixhi-5,ky+16,ibk)
		   call drawframe(ixlo+6,ky+1,ixhi-6,ky+15,ibk)
		   call zsymbol(ix_down0,kyt,isym,isizex,isizey,ict,ict)
		   ky=iy-mod(iy+16,16)
		   k=n-int(float(iy-iylo1-16)/16)
		   kt=lattr(k)+1
		   kyt=iy_down0+(nt-kt)*idya
		   call drawframe(ixlo+5,ky,ixhi-5,ky+16,icf1)
		   call drawframe(ixlo+6,ky+1,ixhi-6,ky+15,icf1)
		   call zsymbol(ix_down0,kyt,isym,isizex,isizey,0,ict)
		   i=k
		   jy=ky
		   iret=i
		   call rdgwin1(ixlo,iylo,ixhi,iyhi,array2)
		   call show_mouse()
		   goto 1   !end of choose line
	      else if(ix.lt.ixhit.and.ix.gt.ixlot.and.iy.lt.iyhit.and.
     &    	iy.gt.iylot) then	     !hit
		   left=.false.
		   call hide_mouse()
	         call drawframe(ixlo+5,jy,ixhi-5,jy+16,12)
	         call drawframe(ixlo+6,jy+1,ixhi-6,jy+15,12)
		   call zsymbol(ix_down0,kyt,isym,isizex,isizey,ict,ict)
		   kt=nt-int(float(iy-iylot)/idya)
		   kyt=iy_down0+(nt-kt)*idya
		   call zsymbol(ix_down0,kyt,isym,isizex,isizey,12,ict)
		   if(kt.le.nt) lattr(k)=kt-1
		   if(number.eq.49) then
			lcolor(k)=lattr(k)
		   else
			ltype(k)=lattr(k)
		   endif
		   call hide_mouse()
		   call fillwin(ixlo+14,ky+2,ixhi-7,ky+14,ibk)
		   call viline(ixc0+8,jy+8,ixhi-16,jy+8,lcolor(k),ltype(k))
		   call rdgwin1(ixlo,iylo,ixhi,iyhi,array2)
		   call show_mouse()
		   goto 1 !end of choose attribute
	      else
		   goto 1
	   	endif !end of ibutton=1
	   else if(ibutton.eq.2) then		!right button
		goto 99
	   else
		goto 1
	   endif
	endif		!end of if(mous_set)
	goto 1
c
c===========================================================================
c
99	continue
	call hide_mouse()
	call WRGWIN1(ixlo,iylo,ixhi,iyhi,array1)
	call WRGWIN1(ixlot,iylot,ixhit,iyhit,array3)
	deallocate(array1)
	deallocate(array2)
	deallocate(array3)
	deallocate(arr1,arr2,arr3,arr4)
      call show_mouse()
	call hide_mouse()
	RETURN
	end
