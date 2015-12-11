	subroutine crosspix(ix,iy,ixlo,iylo,ixhi,iyhi,ibk,icf,itype,
     &	line,ndraw,islope,iax,iay,ieps)

c*************************************************************************
c
c	Ioana Vais
c	Pharmacology
c     ix,iy =current cursor position in pixels
c     ixlo,iylo,ixhi,iyhi : min,max crosshair pixels
c     ibk = color background
c     itype=0 crosshair itype>0 not yet done
c     line=0 no line beetwen points
c	line=1 draw line beetwen points marked
c     line=2 draw vertical or horizontal dotted line at marked  point
c	ndraw  = n points drawn
c     ndraw <= 0 no symbol drawn
c	islope= 0 vertical cursor
c	islope= 1 horizontal cursor
c	islope= 2 crosshair
c     iax,iay array to return the coordinates clicked
c*************************************************************************

C	ICF        : color cursor
c	IBK        : color of background

c*************************************************************************

	character*1	ans,getch
	integer*1 arr1(:),arr2(:)
	integer	iax(10),iay(10)
	logical kbhit
	logical mouse_on,mous_set,sym,fine
	common/mousval/mouse_on,nbutton
	allocatable :: arr1,arr2

c
c NB could skip mouse sections if istatus.ne.65535 (mouse_on=false)
	call show_mouse()
c
	iymark=iy
	ixmark=ix
	do i=1,10
	   iax(i)=0.
	   iay(i)=0.
	enddo
	idraw=abs(ndraw)
	sym=.false.
	mous_set=.false.	!initialise
	icol1=15
	icolor=14
	idx=ixhi-ixlo
	idy=iyhi-iylo
	dx=float(idx+1)/2
	dy=4*float(idy+1)
	nh=int(dx)
	nv=int(dy)
	if(iyhi.gt.479) then
	   iyhi=479
	   iylo=479-idy
	endif
	if(iylo.lt.0) then
	   iylo=0
	   iyhi=idy
	endif
	if(ixhi.gt.639) then
	   ixhi=639
	   ixlo=639-idx
	endif
	if(ixlo.lt.0) then
	   ixlo=0
	   ixhi=idx
	endif
	allocate(arr1(nh),arr2(nv))
	ix0=ix
	iy0=iy
	ixmin=ixlo	!for dotted line
	iymin=iylo
	ixmax=ixhi
	iymax=iyhi
	call set_mouse(ix,iy)
	call hide_mouse()
	call rdcursor(ix,iy,ixlo,iylo,ixhi,iyhi,arr1,arr2,nh,nv,itype)
	call drawcur(ix,iy,ixlo,iylo,ixhi,iyhi,icf,itype,islope)
	ifirst=1
	icount=1
	ibutton=0
	ieps1=1
	iepsav=ieps
	fine=.true.
1	continue
	if(mouse_on) then
22	    call mouse_update(ibutton,ix,iy)
	    if(ibutton.ne.0) goto 22
	    if(ibutton.eq.0) mous_set=.true.
	endif
	if(mous_set) then
	  if(ibutton.ne.0) mous_set=.false.	!until reset above
2	  continue
	  if(kbhit()) then
	   ans=getch(ktype)
	   ival=ichar(ans)	!here you may give other options
	   if(ktype.eq.8) then	!move menu with ARROW keys
	      if(ival.eq.82) then 	!INS=fine/course
		   if(fine) then
		     ieps=ieps1
		   else
		     ieps=iepsav
		   endif
	         fine=.not.fine
		   goto 2
	      endif
		if(ival.eq.73.or.ival.eq.77.or.ival.eq.81) then		!shift right
		   ix=ix+ieps
		endif
		if(ival.eq.71.or.ival.eq.75.or.ival.eq.79) then		!shift left
		   ix=ix-ieps
		endif
		if(ival.eq.71.or.ival.eq.72.or.ival.eq.73) then		!shift up
		   iy=iy+ieps
		endif
		if(ival.eq.79.or.ival.eq.80.or.ival.eq.81) then		!shift down
		   iy=iy-ieps
		endif

		if(islope.eq.0) then
			if(ix.eq.ix0) goto 2
		else
			if(iy.eq.iy0) goto 2
		endif
	      call wrcur(ix0,iy0,ixlo,iylo,ixhi,iyhi,arr1,arr2,nh,nv,
     &	itype)
	      call rdcursor(ix ,iy ,ixlo,iylo,ixhi,iyhi,arr1,arr2,nh,nv,
     &	itype)
		call drawcur(ix ,iy ,ixlo,iylo,ixhi,iyhi,icf,itype,islope)
		call set_mouse(ix,iy)
		ix0=ix
		iy0=iy
		goto 2
	   else if (ival.eq.13) then
	     if(idraw.ne.0) then
		 if(icount.le.idraw) then
		    if(ndraw.gt.0) then
		    if(islope.eq.0) then
		       if(line.eq.2) then
	      	     call wrcur(ix0,iy0,ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &		     nh,nv,itype)
			     call dvline(ix,iymin,iymax,icol1,22)
		           call rdcursor(ix,iy,ixlo,iylo,ixhi,iyhi,arr1,
     &		     arr2,nh,nv,itype)
		           call drawcur(ix,iy,ixlo,iylo,ixhi,iyhi,icf,itype
     &		     ,islope)
			 else
		          call fillwin(ix-4,iymark-4,ix+4,iymark+4,icolor)
		       endif
		    else if(islope.eq.1) then
		       if(line.eq.2) then
	               call wrcur(ix0,iy0,ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &		   nh,nv,itype)
			   call dhline(ixmin,ixmax,iy,icol1,22)
		         call rdcursor(ix,iy,ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &		   nh,nv,itype)
		         call drawcur(ix,iy,ixlo,iylo,ixhi,iyhi,icf,itype,
     &		   islope)
			 else
		            call fillwin(ixmark-4,iy-4,ixmark+4,iy+4,icolor)
			 endif
		    else
			 if(line.eq.2) then
	               call wrcur(ix0,iy0,ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &		   nh,nv,itype)
			   call dvline(ix,iymin,iymax,icol1,22)
			   call dhline(ixmin,ixmax,iy,icol1,22)
		         call rdcursor(ix,iy,ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &		   nh,nv,itype)
		         call drawcur(ix,iy,ixlo,iylo,ixhi,iyhi,icf,itype,
     &		   islope)
			 else
		            call fillwin(ix-4,iy-4,ix+4,iy+4,icolor)
			 endif
		    endif
		    endif
		    if(icount.gt.1) sym=.true.
		    if(sym) then
		      if(islope.eq.0) then
			   if(line.eq.1) call hline(ixp,ix,iymark,icolor)
		    	else if(islope.eq.1) then
			   if(line.eq.1) call vline(ixmark,iyp,iy,icolor)
		      else
			   if(line.eq.1) call iline(ixp,iyp,ix,iy,icolor)
		      endif
		    endif
		    iax(icount)=ix
		    iay(icount)=iy
		    if(icount.eq.idraw) then
	         	call wrcur(ix,iy,ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &		nh,nv,itype)
	         	deallocate(arr1,arr2)
			goto 99
		    endif
		    ixp=ix
		    iyp=iy
		    icount=icount+1
		    goto 2
		 endif
	     endif
	     call wrcur(ix,iy,ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &     nh,nv,itype)
	     deallocate(arr1,arr2)
	     goto 99
	   else if (ival.eq.27) then
		call wrcur(ix,iy,ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &		   nh,nv,itype)
		deallocate(arr1,arr2)
	      goto 99
	   else		!any other key to exit
		goto 2
	   endif
	endif
	if(ibutton.eq.0) then
            call mouse_update(ibutton,ix,iy)
	      if (ibutton.eq.0) then
		   if(ix.eq.ix0.and.iy.eq.iy0) goto 2
		   call hide_mouse()
		   if(ifirst.eq.1) then
		      call drawcur(ix0,iy0,ixlo,iylo,ixhi,iyhi,ibk,itype,
     &		islope)
		      ifirst=0
		   else
		      call wrcur(ix0,iy0,ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &				nh,nv,itype)
		   endif
		   if(ix.lt.0) then
		      ix=0
		   endif
		   if(iy.lt.0) then
		      iy=0
		   endif
		   if(ix.gt.639) then
		      ix=639
		   endif
		   if(iy.gt.479) then
		      iy=479
		   endif
		   call rdcursor(ix,iy,ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &		   nh,nv,itype)
		   call drawcur(ix,iy,ixlo,iylo,ixhi,iyhi,icf,itype,islope)
3		   continue
		   ix0=ix
		   iy0=iy
  		   goto 2
	      else if (ibutton.eq.1) then
	         if(idraw.ne.0) then
		      if(icount.le.idraw) then
		       if(ndraw.gt.0) then
		        if(islope.eq.0) then
		          if(line.eq.2) then
	      		call wrcur(ix0,iy0,ixlo,iylo,ixhi,iyhi,arr1,
     &			arr2,nh,nv,itype)
			     	call dvline(ix,iymin,iymax,icol1,22)
		   		call rdcursor(ix,iy,ixlo,iylo,ixhi,iyhi,arr1,
     &			arr2,nh,nv,itype)
		   		call drawcur(ix,iy,ixlo,iylo,ixhi,iyhi,icf,
     &			itype,islope)
			    else
		          	call fillwin(ix-4,iymark-4,ix+4,iymark+4,icolor)
		          endif
		        else if(islope.eq.1) then
		          if(line.eq.2) then
	      		call wrcur(ix0,iy0,ixlo,iylo,ixhi,iyhi,arr1,
     &			arr2,nh,nv,itype)
				call dhline(ixmin,ixmax,iy-4,icol1,22)
		   		call rdcursor(ix,iy,ixlo,iylo,ixhi,iyhi,arr1,
     &			arr2,nh,nv,itype)
		   		call drawcur(ix,iy,ixlo,iylo,ixhi,iyhi,icf,
     &			itype,islope)
			    else
		            call fillwin(ixmark-4,iy-4,ixmark+4,iy+4,icolor)
			    endif
		        else
			    if(line.eq.2) then
	      		call wrcur(ix0,iy0,ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &			nh,nv,itype)
				call dvline(ix,iymin,iymax,icol1,22)
				call dhline(ixmin,ixmax,iy,icol1,22)
		   		call rdcursor(ix,iy,ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &		   	nh,nv,itype)
		   		call drawcur(ix,iy,ixlo,iylo,ixhi,iyhi,icf,itype,
     &			islope)
			    else
		            call fillwin(ix-4,iy-4,ix+4,iy+4,icolor)
			    endif
		        endif
		      endif
		      if(icount.gt.1) sym=.true.
			if(sym) then
			    if(islope.eq.0) then
			     if(line.eq.1) call hline(ixp,ix,iymark,icolor)
			    else if(islope.eq.1) then
			     if(line.eq.1) call vline(ixmark,iyp,iy,icolor)
			    else
			     if(line.eq.1) call iline(ixp,iyp,ix,iy,icolor)
			    endif
			endif
		      iax(icount)=ix
		      iay(icount)=iy
			if(icount.eq.idraw) then
	         		call wrcur(ix,iy,ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &		   	nh,nv,itype)
	         		deallocate(arr1,arr2)
				goto 99
			endif
		      icount=icount+1
		      ixp=ix
		      iyp=iy
		      goto 1
		      endif
	         endif
	         call wrcur(ix,iy,ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &		   nh,nv,itype)
	         deallocate(arr1,arr2)
	         goto 99
 	      else if(ibutton.eq.2) then
		   call wrcur(ix,iy,ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &		   nh,nv,itype)
		   deallocate(arr1,arr2)
		   goto 99
	      else
		   goto 1
	      endif
	      goto 1
	   else if(ibutton.eq.2) then
		call wrcur(ix,iy,ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &      nh,nv,itype)
		deallocate(arr1,arr2)
		goto 99
	   else
	      goto 1
	   endif
	endif
99	continue
	if(islope.eq.0) then
	   do i=1,icount
		iay(i)=iymark
	   enddo
	else if(islope.eq.1) then
	   do i=1,icount
		iax(i)=ixmark
	   enddo
	endif
	call ENDBOX(1,ibk)

	RETURN
	end
