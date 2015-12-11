	subroutine crossy(ix,iy,ixlo,iylo,ixhi,iyhi,ibk,icf,itype,
     &	line,idraw,islope,iax,iay,irhi,iclo,m)

c*************************************************************************
c
c	Ioana Vais
c	Pharmacology
c     ix,iy =current cursor position in pixels
c     ixlo,iylo,ixhi,iyhi : min,max crosshair pixels
c     ibk = color background
c     itype=0 crosshair itype>0 not yet done
c     line=0 no line beetwen points
c	line=1 draw line ; line> 1 can be extendet to draw what ever you fancy
c	idraw=0 no symbol ; idraw=1 draw 1 symbol at click
c	idraw = n points drawn
c	islope= 0 horizontal line drawn
c	islope= 1 vertical line drawn  beetwen points
c	islope= 2 line drawn  beetwen points
c     iax,iay array to return the coordinates clicked
c*************************************************************************

C	ICF        : color cursor
c	IBK        : color of background

c*************************************************************************

	character*1	ans,getch
	integer*1 arr1(:),arr2(:)
	integer   iax(10),iay(10)
	character cnum*11
	logical kbhit,hit
	logical mouse_on,mous_set,sym,showbox
	common/mousval/mouse_on,nbutton
	allocatable :: arr1,arr2
c
c NB could skip mouse sections if istatus.ne.65535 (mouse_on=false)
	call show_mouse()
c
	iymark=iy
	ixmark=ix
	do i=1,10
	   iax(i)=0
	   iay(i)=0
	enddo
	sym=.false.
	mous_set=.false.	!initialise
	isizex=8
	isizey=8
	isym=9
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
	call hide_mouse()
	call rdcursor(ix,iy,ixlo,iylo,ixhi,iyhi,arr1,arr2,nh,nv,itype)
	call drawcur(ix,iy,ixlo,iylo,ixhi,iyhi,icf,itype,islope)
	icbd=0
	showbox=irhi.gt.0
      call pixgra(ix,iy,xcount,ycount)
	if(showbox) then
	   ncol=m+1
	   call DEFBOX(1,irhi,iclo,1,ncol,icbd)	!12 cols for cnum*11
	   call OPENBOX(1,7,.true.)
	   if(islope.eq.0) then
	     value1=xcount
	   else
	     value1=ycount
	   endif
	   call DCFORMAT(value1,m,n,cnum)
	   call WRITBOX(1,cnum(1:ncol),1,11)
	endif
	ifirst=1
	icount=0
	ibutton=0
	ixk=ix
	ix0k=ix0
	iyk=iy
	iy0k=iy0
1	continue
c	call mouse_update(ibutton,ix,iy)
	if(mouse_on) then
c         call show_mouse()
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
c		call hide_mouse()
	      call wrcur(ix0,iy0,ixlo,iylo,ixhi,iyhi,arr1,arr2,nh,nv,
     &	itype)
		if(ival.eq.73.or.ival.eq.77.or.ival.eq.81) then		!shift right
		   ix=ix+8
		endif
		if(ival.eq.71.or.ival.eq.75.or.ival.eq.79) then		!shift left
	         ix =ix -8
		endif
		if(ival.eq.71.or.ival.eq.72.or.ival.eq.73) then		!shift up
	         iy =iy +8
		endif
		if(ival.eq.79.or.ival.eq.80.or.ival.eq.81) then		!shift down
	         iy =iy -8
		endif
	      call rdcursor(ix ,iy ,ixlo,iylo,ixhi,iyhi,arr1,arr2,nh,nv,
     &	itype)
		call drawcur(ix ,iy ,ixlo,iylo,ixhi,iyhi,icf,itype,islope)
		ix0=ix
		iy0=iy
		call pixgra(ix,iy,xcount,ycount)
		if(showbox) then
	      	if(islope.eq.0) then
	        		value1=xcount
	     		else
	        		value1=ycount
	     		endif
	   		call DCFORMAT(value1,m,n,cnum)
	   		call WRITBOX(1,cnum(1:ncol),1,11)
		endif
c        	call show_mouse()
		goto 2
	   else if (ival.eq.13) then
	     if(idraw.ne.0) then
		 if(icount.lt.idraw) then
c		    call ZSYMBOL(ix,iy,isym,isizex,isizey,icolor,icfill)
		    if(islope.eq.0) then
		       call fillwin(ix-4,iymark-4,ix+4,iymark+4,icolor)
		    else if(islope.eq.1) then
		       call fillwin(ixmark-4,iy-4,ixmark+4,iy+4,icolor)
		    else
		       call fillwin(ix-4,iy-4,ix+4,iy+4,icolor)
		    endif
		    if(icount.gt.0) sym=.true.
		    if(sym.and.line.eq.1) then
		      if(islope.eq.0) then
c				call drawframe(ixp,iyp,ix-1,iy+1,icolor)
				call hline(ixp,ix,iymark,icolor)
c				call hline(ixp,ix,iymark,icolor)
		    	else if(islope.eq.1) then
				call vline(ixmark,iyp,iy,icolor)
c				call vline(ixmark,iyp,iy,icolor)
		      else
				call ilvline(ixp,iyp,ix,iy,icolor)
		      endif
		    endif
		    icount=icount+1
		    iax(icount)=ix
		    iay(icount)=iy
		    ixp=ix
		    iyp=iy
		    goto 2
		 endif
	     endif
	     call wrcur(ix,iy,ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &		   nh,nv,itype)
	     deallocate(arr1,arr2)
	   else if (ival.eq.27) then
		call wrcur(ix,iy,ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &		   nh,nv,itype)
		deallocate(arr1,arr2)
	   else		!any other key to exit
	      call wrcur(ix,iy,ixlo,iylo,ixhi,iyhi,arr1,arr2,nh,nv,
     &	itype)
	  	deallocate(arr1,arr2)
	   endif
	   goto 99
	endif
c	else 
	if(ibutton.eq.0) then
            call mouse_update(ibutton,ix,iy)
c2	      call mouse_update(ibutton,ix,iy)
	      if (ibutton.eq.0) then
		   if(ix.eq.ix0.and.iy.eq.iy0) goto 3
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
		   call pixgra(ix,iy,xcount,ycount)
		   if(showbox) then
	      	if(islope.eq.0) then
	        		value1=xcount
	     		else
	        		value1=ycount
	     		endif
	   		call DCFORMAT(value1,m,n,cnum)
	   		call WRITBOX(1,cnum(1:ncol),1,11)
		   endif
c		   call show_mouse()
  		   goto 2
	      else if (ibutton.eq.1) then
	         if(idraw.ne.0) then
		      if(icount.lt.idraw) then
c		        call ZSYMBOL(ix,iy,isym,isizex,isizey,icolor,icfill)
		        if(islope.eq.0) then
		          call fillwin(ix-4,iymark-4,ix+4,iymark+4,icolor)
		        else if(islope.eq.1) then
		          call fillwin(ixmark-4,iy-4,ixmark+4,iy+4,icolor)
		        else
		          call fillwin(ix-4,iy-4,ix+4,iy+4,icolor)
		        endif
		        if(icount.gt.0) sym=.true.
		        if(sym.and.line.eq.1) then
		          if(islope.eq.0) then
				call hline(ixp,ix,iymark,icolor)
c				call hline(ixp,ix,iyp,icolor)
c				call drawframe(ixp,iyp,ix-1,iy-1,icolor)
		    	    else if(islope.eq.1) then
				call vline(ixmark,iyp,iy,icolor)
c				call vline(ixp+1,iyp,iy,icolor)
		    	    else
				call ilvline(ixp,iyp,ix,iy,icolor)
		          endif
		        endif
		        icount=icount+1
		        iax(icount)=ix
		        iay(icount)=iy
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
     &		   nh,nv,itype)
		goto 99
	   else
	      goto 1
	   endif
	endif
99	continue
	call ENDBOX(1,ibk)

	RETURN
	end
