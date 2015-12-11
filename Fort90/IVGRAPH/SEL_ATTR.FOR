	subroutine sel_attr(ibk,lcolor,ltype,number)
c*********************************************************************

	character*1		charout,getch
	integer*1		array3(:)
	integer*4		ltype,lcolor,lattr
	logical		kbhit
	logical mouse_on,mous_set,outext
	common/mousval/mouse_on,nbutton
	allocatable :: array3
	isym=12
	ic=ibk+8
	ixhit=639
	iyhit=479
c	draw line color/line type menu
	select case(number)
	   case(49)    	!only if press '1'
		nt=16
		iylot=160
		iylo=144
		ixlot=584
		idimen3=(iyhit-iylo+9)*(ixhit-ixlot+9)+1
		allocate(array3(idimen3))
		call rdgwin1(ixlot,iylo,ixhit,iyhit,array3)
		call color_type(ixlot,iylot,ixhit,iyhit,nt,ic,ibk,
     &	ix_down0,iy_down0,idya,isizex,isizey)
		lattr=lcolor
	   case(50)		!only if press '2'
		nt=6
		iylot=320
		iylo=304
		ixlot=568
		idimen3=(iyhit-iylo+9)*(ixhit-ixlot+9)+1
		allocate(array3(idimen3))
		call rdgwin1(ixlot,iylo,ixhit,iyhit,array3)
		call line_type(ixlot,iylot,ixhit,iyhit,nt,ic,ibk,
     &	ix_down0,iy_down0,idya,isizex,isizey)
		lattr=ltype
	end select
	call framefill(ixlot,iylo,ixhit,iylot,ic,ibk)
	call ivline(ixlot+4,iylo+8,ixhit-4,iylo+8,lcolor,ltype)
	kt=lattr+1
	kyt=iy_down0+(nt-kt)*idya
	call zsymbol(ix_down0,kyt,isym,isizex,isizey,12,ibk)
	call show_mouse()
c
c===========================================================================
c
1	continue	!return to look for next keyboard/mouse hit
	if(kbhit()) then
	   charout=getch(ktype)
	   ival=ichar(charout)
	   if(ival.eq.27.or.ival.eq.13) then		!ESC or ENTER
		goto 99
	   else
	   	if(ktype.eq.8) then		!up/down arrow to scroll highlight
		   call hide_mouse()
	     	   select case(ival)
		      case(72)
				call zsymbol(ix_down0,kyt,isym,isizex
     &		      ,isizey,ibk,ibk)
			      if(kt.gt.1) then
				   kt=kt-1
		               kyt=iy_down0+(nt-kt)*idya
				   call zsymbol(ix_down0,kyt,isym,isizex
     &			   ,isizey,12,ibk)
		   	       else		!go to bottom=string(n)
				   kyt=iy_down0
				   kt=nt
		               call zsymbol(ix_down0,kyt,isym,isizex
     &			   ,isizey,12,ibk)
		   	      endif
				if(kt.le.nt) lattr=kt-1
				if(number.eq.49) then
				   lcolor=lattr
				else
				   ltype=lattr
				endif
				call hide_mouse()
				call fillwin(ixlot+4,iylo+4,ixhit-4,iylot-4,ibk)
				call ivline(ixlot+4,iylo+8,ixhit-4,iylo+8,
     &			lcolor,ltype)
		      case(80)			!down arrow
				call zsymbol(ix_down0,kyt,isym,isizex
     &		      ,isizey,ibk,ibk)
				if(kt.lt.nt) then
				   kt=kt+1
		   		   kyt=iy_down0+(nt-kt)*idya
				   call zsymbol(ix_down0,kyt,isym,isizex
     &			   ,isizey,12,ibk)
			      else
				   kyt=iy_down0+(nt-1)*idya
				   kt=1
		               call zsymbol(ix_down0,kyt,isym,isizex
     &			   ,isizey,12,ibk)
				endif
				if(kt.le.nt) lattr=kt-1
				if(number.eq.49) then
				   lcolor=lattr
				else
				   ltype=lattr
				endif
				call hide_mouse()
				call fillwin(ixlot+4,iylo+4,ixhit-4,iylot-4,ibk)
				call ivline(ixlot+4,iylo+8,ixhit-4,iylo+8,
     &			lcolor,ltype)
	     	   end select
		endif
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
	      if(ix.lt.ixhit.and.ix.gt.ixlot.and.iy.lt.iyhit.and.
     &    	iy.gt.iylot) then	     !hit
		   call hide_mouse()
		   call zsymbol(ix_down0,kyt,isym,isizex,isizey,ibk,ibk)
		   kt=nt-int(float(iy-iylot)/idya)
		   kyt=iy_down0+(nt-kt)*idya
		   call zsymbol(ix_down0,kyt,isym,isizex,isizey,12,ibk)
		   if(kt.le.nt) lattr=kt-1
		   if(number.eq.49) then
			lcolor=lattr
		   else
			ltype=lattr
		   endif
		   call hide_mouse()
		   call fillwin(ixlot+4,iylo+4,ixhit-4,iylot-4,ibk)
		   call ivline(ixlot+4,iylo+8,ixhit-4,iylo+8,lcolor,ltype)
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
	call WRGWIN1(ixlot,iylo,ixhit,iyhit,array3)
	deallocate(array3)
      call show_mouse()
	call hide_mouse()
	RETURN
	end
