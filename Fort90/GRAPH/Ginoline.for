C     Last change:  P     2 Apr 97    4:08 pm
c*************************************************************************
	subroutine ginoLINES(ikey,nline,xlb,xle,ylb,yle,iltype,
     & nhline,yhline,xhlb,xhle,ilhtype,ihlinrel,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,
     & lth,icol,ibk)

c=======================================================================
c*************************************************************************
c     if you pick an axis number and change color,font,size when you exit
c	it will return the initial values! you can change them only with fixnumbs2
c	because you must have the same color,font,size for axis nr!
c
c	subroutine to move ONE selected string TEXT at a time, written with
c	HGRAPH similar with FIXTEXT , but now you enter ALL selected n strings
c	from the screen, with specific properties.
c	ATTENTION : ixg(n),iyg(n) coordinates in HGRAPH display (0-10000,0-7500)
c	calls:	CALCDIM,INSIDE,FBOX1,PIXTOHG,WRGWIN1,RDGWIN1
c*************************************************************************

c To adjust free, horizontal and vertical lines and grid lines in VPLOT
	integer icol(100)
	real lth(100),lt4
c arrays for lines
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	integer iltype(10)				!line type for ditto
c new arrays for horizontal lines
	real*4 yhline(10)		!record y value
	real*4 xhlb(10),xhle(10)     !start/end of HLINE
	integer ilhtype(10)	!line type for horizontal lines
	integer*1 ihlinrel(10)	!=1 if line pos rel to line 1
c new arrays for vertical lines
	real*4 xvline(10)		!record x value
	real*4 yvlb(10),yvle(10)     !start/end of VLINE
	integer ilvtype(10)	!line type for vertical lines
	integer*1 ivlinrel(10)	!=1 if line pos rel to line 1
	integer Lb(30)		!for DCMENU
      real           xg(:),yg(:),thick(:),type(:)
      real		   xg0(:),yg0(:)
	integer 	   colors(:)
	real   	   xbox(4),ybox(4),size0(:)
	integer 	   font0(:),temp(100),first,flag
	integer*1	   array0(:)
	character*1    ans,getch
	logical        mouse_on,kbhit,outtext,mouse_set,logx,logy,ivl
	logical isnum
	common/mousval/mouse_on,nbutton

	allocatable :: array0,xg0,yg0,thick,type
	allocatable :: colors,xg,yg

c	HELP LINES:
C========================================================
	helpst(1)='Left button pressed :'
	helpst(2)=' - inside box : move selected line'
	helpst(3)='Right button pressed :'
	helpst(4)=' - escape'
	helpst(5)='Central button pressed :'
	helpst(6)=' - select line to move with keyboards:'
	helpst(7)='       ->: move right'
	helpst(8)='       <-: move left'
	helpst(9)='       Del: delete'
	helpst(10)='      /\: up'
	helpst(11)='      \/: down'
	helpst(12)='      + : increase thick'
	helpst(13)='      - : decrease thick'
	helpst(14)='      * : increase type'
	helpst(15)='      / : decrease type'
	helpst(16)='    PgUp: increase color'
	helpst(17)='    PgDn: decrease color'
	helpst(18)='    <CR>: end selection'
c============================================================


	ivl=isnum()
	if(ivl) call NUMCLR
	n=nline+nhline+nvline
	allocate(colors(n),xg(n),yg(n))
      allocate(thick(n),type(n))
	allocate(xg0(n),yg0(n))

      do i=1,nline
	   type(i)=iltype(i)
	   colors(i)=icol(61+i-1)
	   thick(i)=lth(61+i-1)
	   xlo(i)=xlb(i)
	   xhi(i)=xle(i)
 	   ylo(i)=ylb(i)
 	   yhi(i)=yle(i)
	enddo
	do i=1,nhline
	   type(i+nline)=ilhtype(i)       
	   colors(i+nline)=icol(72+i-1)   
	   thick(i+nline)=lth(72+i-1)           
	   xlo(i+nline)=xhlb(i)                 
	   xhi(i+nline)=xhle(i)                 
	   ylo(i+nline)=0.9*yhline(i)           
	   yhi(i+nline)=1.1*yhline(i)           
	enddo
	do i=1,nvline
	   type(i+nline+nhline)=ilvtype(i)       
	   colors(i+nline+nhline)=icol(82+i-1)   
	   thick(i+nline+nhline)=lth(82+i-1)           
	   xlo(i+nline+nhline)=0.9*xhline(i)                 
	   xhi(i+nline+nhline)=1.1*xhline(i)                 
	   ylo(i+nline+nhline)=yvlb(i)           
	   yhi(i+nline+nhline)=yvle(i)           
	enddo
	do i=1,n
	   if(logx) then
	     xlo(i)=alog10(xlo(i))
	     xhi(i)=alog10(xhi(i))
	   endif
 	   if(logy) then
 	     ylo(i)=alog10(ylo(i))
 	     yhi(i)=alog10(yhi(i))
 	   endif
	   if(sqrty) then
 	     ylo(i)=sqrt(ylo(i))
 	     yhi(i)=sqrt(yhi(i))
         endif
	enddo
	nhelp=21
	icq=14     		! color help menu
	call vp2enq(xp1,xp2,yp1,yp2,xv1,xv2,yv1,yv2)
	dm=0.01
	deltax=0.01*abs(xp2-xp1)           ! movement
	deltay=0.01*abs(yp2-yp1)           ! movement
	ang=45.             ! angle
	cs=1.
	mouse_set=.false.
1	continue
	if(kbhit()) then
	   ans=getch(ktype)
	   ival=ichar(ans)
	   if(ktype.eq.0.and.ival.eq.59) then		!F1=help
		ixloh=320    !position for help box
		iyhih=320
		call poptext(ixloh,-1,iyhih,helpst,nhelp,15,2,icq)	!=popkeys
		ibutton=0
		outtext=.true.
		mouse_set=.false.
		call show_mouse()
		goto 1
	   else if(ival.eq.27.or.ival.eq.13) then		!ESC or ENTER
		goto 99
	   endif
	endif       ! end if(kbhit())
	if (mouse_on) then
	   call show_mouse()
82	   call mouse_update(ibutton,ix,iy)
	   call pixpos(ix,iy,xm,ym)
	   ym=yp2-ym
	   if(outtext.and.ibutton.ne.0) goto 82
	   outtext=.false.
	   if(ibutton.eq.0) mouse_set=.true.
	endif      ! end if mouse_on
	if(mouse_set) then
	   if(ibutton.ne.0) mouse_set=.false.	!until reset above
	   if(ibutton.eq.0) then
	    	if(ix.eq.ix0.and.iy.eq.iy0) goto 1
	    	if(m.eq.0) goto 10
		call hide_mouse()
	    	do k=1,m
      	   call LBOX1(xlo(temp(k)),ylo(temp(k)),xhi(temp(k)),
     &	   yhi(temp(k)),type(temp(k)),thick(temp(k)),xbox,ybox,1)  !screen-screen
	    	enddo
	    	m=0
10	    	do i=1,n
      	   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),SIZE(i),
     &	   JUS(i),xbox,ybox,0)
		   call pixpos(ix,iy,xm,ym) !pixel to screen
	   	   ym=yp2-ym
	   	   call inside(xm,ym,xbox,ybox,flag) !screen-screen
	         if(flag.eq.1.and.jdraw(i).ne.0) then
		    	m=m+1
		    	temp(m)=i
		   endif
	    	enddo
	    	if(m.gt.0) then       		! show box(es) if inside
		   do k=1,m
		   	call lincols(colors(temp(k)))    !!!!
			call setfnt(font(temp(k)))
      	    	call FBOX1(xg(temp(k)),yg(temp(k)),
     &		str(temp(k)),ANGLE(temp(k)),SIZE(temp(k)),
     &	    	JUS(temp(k)),xbox,ybox,1)
		   enddo
		endif
		ix0=ix
		iy0=iy
		call show_mouse()
		goto 1   !end button=0				! end of ibutton=0
	   else if(ibutton.eq.1) then        ! left button pressed
		m=0
		call hide_mouse()
	    	do i=1,n
		   call setfnt(font(i))
      	   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &	   SIZE(i),JUS(i),xbox,ybox,0)
		   call pixpos(ix,iy,xh,yh)
	   	   yh=yp2-yh
c		   yh=480-yh
	   	   call inside(xh,yh,xbox,ybox,flag)
	         if(flag.eq.1.and.jdraw(i).ne.0) then
			first=1
			m=m+1
			temp(m)=i
			call setfnt(font(i))
		   	call lincols(colors(i))    !!!!
      	      call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &		SIZE(i),JUS(i),xbox,ybox,1)
			call show_mouse()
			call calcdim(xbox,ybox,4,ixlo,iylo,ixhi,iyhi) !screen to 18
			idif=ixhi-ixlo
			if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idif
			endif
			idim=2*((iyhi-iylo+9)*(ixhi-ixlo+9)+1)
			allocate(array0(idim))
			xg0(i)=xg(i)
			yg0(i)=yg(i)
2		    	call mouse_update(ibutton,ix,iy)
	  	    	if(ibutton.eq.1) then          ! move string if inside
			   if(ix.eq.ix0.and.iy.eq.iy0) goto 2
			   call hide_mouse()
			   if(first.eq.1) then
			   	call lincols(ibk)
      	         	call FBOX1(xg(i),yg(i),str(i),
     &		   	ANGLE(i),SIZE(i),
     &	         	JUS(i),xbox,ybox,1)
		            nl=nblank1(str(i))
				if(imd.ne.0) then
			   	call linwid(thick(i))
			   	call justifystring(xg(i),yg(i),
     &		   	str(i)(1:nl),angle(i),size(i),jus(i))
				endif
				first=0
				goto 31
			   endif
			   call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
31			   continue
 			   idx=ix-ix0
 			   idy=iy-iy0
 		   	   call pixpos(idx,idy,dxh,dyh)
 			   dyh=yp2-dyh
 			   xg(i)=xg(i)+dxh
 			   yg(i)=yg(i)+dyh
c		   	   call pixpos(ix,iy,xg(i),yg(i))
c			   yg(i)=y2p-yg(i)
      	   	   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &               SIZE(i),JUS(i),xbox,ybox,0)
			   call calcdim(xbox,ybox,4,ixlo,iylo,ixhi,iyhi)
			   idif=ixhi-ixlo
			   if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idif
			   endif
			   call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
		   	   call lincols(colors(i))
      	   	   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &               SIZE(i),JUS(i),xbox,ybox,1)
		         nl=nblank1(str(i))
			   if(imd.ne.0) then
			   	call linwid(thick(i))
			   call justifystring(xg(i),yg(i),
     &		   str(i)(1:nl),angle(i),size(i),jus(i))
			   endif
			   ix0=ix
			   iy0=iy
			   call show_mouse()
			   goto 2
		    	else if (ibutton.eq.0) then
			   if(imd.eq.0) then
			   call hide_mouse()
		         nl=nblank1(str(i))
			   call lincols(ibk)
			   call linwid(thick(i))
			   call justifystring(xg0(i),yg0(i),
     &		   str(i)(1:nl),angle(i),size(i),jus(i))
			   call lincols(colors(i))
			   call justifystring(xg(i),yg(i),
     &		   str(i)(1:nl),angle(i),size(i),jus(i))
			   call show_mouse()
			   endif
			   deallocate(array0)
			   goto 1
		   	else
			   call hide_mouse()
			   call lincols(ibk)
		         if(jdraw(i).eq.-1) then
			      call lincols(colors(i))
		         else
		            call lincols(ibk)
		         endif
      	   	   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &               SIZE(i),JUS(i),xbox,ybox,1)
			   deallocate(array0)
			   call show_mouse()
			   goto 99
		   	endif
		   else
			outtext=.true.
		   endif
	    	enddo
	      if(flag.eq.0) then
	    	do i=1,n        	! show all boxes if outside
		   if(jdraw(i).ne.0) then
		   m=m+1
		   temp(m)=i
		   call setfnt(font(i))
		   call lincols(colors(i))
      	   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &	   SIZE(i),JUS(i),xbox,ybox,1)
		   endif
	    	enddo
		call show_mouse()
	    	goto 1            ! end of ibutton.eq.1
 		endif
	   else if(ibutton.eq.2) then   ! centre button pressed:->right!!!
		first=1
		m=0
	    	do i=1,n
		   call setfnt(font(i))
      	   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &         SIZE(i),JUS(i),xbox,ybox,0)
		   call pixpos(ix,iy,xh,yh)
	   	   yh=yp2-yh
c		   yh=480-yh
	   	   call inside(xh,yh,xbox,ybox,flag)
	         if(flag.eq.1.and.jdraw(i).ne.0) then  ! if inside select box to move with keyboards
			m=m+1
			temp(m)=i
			call calcdim(xbox,ybox,4,ixlo,iylo,ixhi,iyhi)!screen:pixels
			idif=ixhi-ixlo
			if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idif
			endif
			idim=2*((iyhi-iylo+9)*(ixhi-ixlo+9)+1)
			xg0(i)=xg(i)
			yg0(i)=yg(i)
			angle0(i)=angle(i)
			size0(i)=size(i)
			font0(i)=font(i)
			ls=nblank1(str(i))
			call getsize(size(i),w,h)
			allocate(array0(idim))
		  	call hide_mouse()
c===
c                 redraw selected box in red
		   	call lincols(12)
      	   	call FBOX1(Xg(i),Yg(i),str(i),ANGLE(i),
     &               SIZE(i),JUS(i),xbox,ybox,1)
c===
33	      	ans=getch(ktype)
	      	ival=ichar(ans)
	      	if(ktype.eq.8) then	!move menu with ARROW keys
		  	   if(ival.eq.77) then	!right
				call setfnt(font(i))
				if(first.eq.1) then
		   		   call lincols(ibk)
      	   		   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                     SIZE(i),JUS(i),xbox,ybox,1)
		         	   nl=nblank1(str(i))
				   if(imd.ne.0) then
			         call linwid(thick(i))
			         call justifystring(xg(i),yg(i),
     &		         str(i)(1:nl),angle(i),size(i),jus(i))
				   endif
				   first=0
				   goto 21
				endif
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
21				if(xg(i)-deltax.lt.yp2-(ls+1)*w) then
                           xg(i)=xg(i)+deltax
				endif
      	   		call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                  SIZE(i),JUS(i),xbox,ybox,0)
				call calcdim(xbox,ybox,4,ixlo,iylo,ixhi,iyhi)
				idif=ixhi-ixlo
			      if(ixlo.lt.0) then
				   ixlo=0
				   ixhi=ixlo+idif
				endif
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
				call lincols(colors(I))
      	   		call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                  SIZE(i),JUS(i),xbox,iybox,1)
		            nl=nblank1(str(i))
			   	if(imd.ne.0) then
			         call linwid(thick(i))
			      call justifystring(xg(i),yg(i),
     &		      str(i)(1:nl),angle(i),size(i),jus(i))
				endif
		  	   endif
		  	   if(ival.eq.75) then		!shift left
				call setfnt(font(i))
				if(first.eq.1) then
		   		   call lincols(ibk)
      	   		   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                     SIZE(i),JUS(i),xbox,ybox,1)
		               nl=nblank1(str(i))
			   	   if(imd.ne.0) then
			         call linwid(thick(i))
			         call justifystring(xg(i),yg(i),
     &		         str(i)(1:nl),angle(i),size(i),jus(i))
				   endif
				   first=0
				   goto 22
				endif
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
22		    		xg(i)=xg(i)-deltax
      	   		call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                  SIZE(i),JUS(i),xbox,ybox,0)
				call calcdim(xbox,ybox,4,ixlo,iylo,ixhi,iyhi)
				idif=ixhi-ixlo
			      if(ixlo.lt.0) then
				   ixlo=0
				   ixhi=ixlo+idif
			      endif
			      call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
				call lincols(COLORS(I))
      	   		call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                  SIZE(i),JUS(i),xbox,ybox,1)
		            nl=nblank1(str(i))
			   	if(imd.ne.0) then
			         call linwid(thick(i))
			      call justifystring(xg(i),yg(i),
     &		      str(i)(1:nl),angle(i),size(i),jus(i))
				endif
		  	   endif
		  	   if(ival.eq.72) then	!shift up
				call setfnt(font(i))
				if(first.eq.1) then
				   call lincols(ibk)
				   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                     SIZE(i),JUS(i),xbox,ybox,1)
				   nl=nblank1(str(i))
				   if(imd.ne.0) then
			         call linwid(thick(i))
					call justifystring(xg(i),yg(i),
     &		      	str(i)(1:nl),angle(i),size(i),jus(i))
				   endif
				   first=0
				   goto 23
				endif
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
23		    		yg(i)=yg(i)+deltay
				call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                  SIZE(i),JUS(i),xbox,ybox,0)
				call calcdim(xbox,ybox,4,ixlo,iylo,ixhi,
     &			iyhi)
				idif=ixhi-ixlo
				if(ixlo.lt.0) then
					ixlo=0
					ixhi=ixlo+idif
				endif
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
				call lincols(colors(I))
				call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                  SIZE(i),JUS(i),xbox,ybox,1)
				nl=nblank1(str(i))
				if(imd.ne.0) then
			         call linwid(thick(i))
				call justifystring(xg(i),yg(i),
     &		      str(i)(1:nl),angle(i),size(i),jus(i))
				endif
		  	   endif
		  	   if(ival.eq.80) then	!shift down
				call setfnt(font(i))
				if(first.eq.1) then
				   call lincols(ibk)
				   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                     SIZE(i),JUS(i),xbox,ybox,1)
				   nl=nblank1(str(i))
				   if(imd.ne.0) then
			         call linwid(thick(i))
					call justifystring(xg(i),yg(i),
     &		      	str(i)(1:nl),angle(i),size(i),jus(i))
				   endif
				   first=0
				   goto 24
			      endif
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
24		    		yg(i)=yg(i)-deltay
				call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                  SIZE(i),JUS(i),xbox,ybox,0)
				call calcdim(xbox,ybox,4,ixlo,iylo,ixhi,
     &                  iyhi)
				idif=ixhi-ixlo
				if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idif
				endif
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
				call lincols(colors(I))
				call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                  SIZE(i),JUS(i),xbox,ybox,1)
				nl=nblank1(str(i))
				if(imd.ne.0) then
			         call linwid(thick(i))
				call justifystring(xg(i),yg(i),
     &		      str(i)(1:nl),angle(i),size(i),jus(i))
				endif
		  	   endif
		  	   if(ival.eq.73) then	!PgDn:rotate clockwise
				   call setfnt(font(i))
				   if(first.eq.1) then
		   		   	call lincols(ibk)
      	   		   	call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                     	SIZE(i),JUS(i),xbox,ybox,1)
		            	nl=nblank1(str(i))
			         call linwid(thick(i))
			         	call justifystring(xg(i),yg(i),
     &		         	str(i)(1:nl),angle(i),size(i),jus(i))
					first=0
					goto 25
				   endif
				   call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
25				   continue
				   angle(i)=angle(i)+ang
      	   		   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                     SIZE(i),JUS(i),xbox,ybox,0)
				   call calcdim(xbox,ybox,4,ixlo,iylo,ixhi,
     &                     iyhi)
				   idif=ixhi-ixlo
				   if(ixlo.lt.0) then
				   	ixlo=0
				   	ixhi=ixlo+idif
			         endif
				   call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
				   call lincols(colors(I))
      	   		   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                     SIZE(i),JUS(i),xbox,ybox,1)
		               nl=nblank1(str(i))
			   	   if(imd.ne.0) then
			         call linwid(thick(i))
			         call justifystring(xg(i),yg(i),
     &		         str(i)(1:nl),angle(i),size(i),jus(i))
				   endif
		  	   endif
		  	   if(ival.eq.81) then	!PgUp:rotate anticlockwise
				   if(first.eq.1) then
		   		   	call lincols(ibk)
				      call setfnt(font(i))
      	   		   	call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                     	SIZE(i),JUS(i),xbox,ybox,1)
		            	nl=nblank1(str(i))
			         call linwid(thick(i))
			         	call justifystring(xg(i),yg(i),
     &		         	str(i)(1:nl),angle(i),size(i),jus(i))
					first=0
					goto 125
				   endif
				   call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
125				   continue
				   angle(i)=angle(i)-ang
				   call setfnt(font(i))
      	   		   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                     SIZE(i),JUS(i),xbox,ybox,0)
				   call calcdim(xbox,ybox,4,ixlo,iylo,ixhi,
     &			   iyhi)
				   idif=ixhi-ixlo
				   if(ixlo.lt.0) then
				   	ixlo=0
				   	ixhi=ixlo+idif
			         endif
				   call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
				   call lincols(colors(I))
				   call setfnt(font(i))
      	   		   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                     SIZE(i),JUS(i),xbox,ybox,1)
		               nl=nblank1(str(i))
			   	   if(imd.ne.0) then
			         call linwid(thick(i))
			         call justifystring(xg(i),yg(i),
     &		         str(i)(1:nl),angle(i),size(i),jus(i))
				   endif
		  	   endif
			   if(ival.eq.79) then     ! End:increase color
				   if (colors(i).lt.15) colors(i)=colors(i)+1
			   	   if(imd.eq.0.and.jdraw(i).ne.0) then
				      call setfnt(font0(i))
		   		      call setsize(size0(i))
				      call lincols(ibk)
		                  nl=nblank1(str(i))
			            call linwid(thick(i))
			   	      call justifystring(xg0(i),yg0(i),
     &		   	      str(i)(1:nl),angle0(i),size0(i),jus(i))
				   endif
				   call lincols(colors(i))
				   call setfnt(font(i))
		               nl=nblank1(str(i))
			         call linwid(thick(i))
			         call justifystring(xg(i),yg(i),
     &		         str(i)(1:nl),angle(i),size(i),jus(i))
      	   		   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                     SIZE(i),JUS(i),xbox,ybox,1)
			   endif
			   if(ival.eq.71) then     ! Home:decrease color
				   if (colors(i).gt.0) colors(i)=colors(i)-1
			   	   if(imd.eq.0) then
				      call setfnt(font0(i))
				      call lincols(ibk)
		                  nl=nblank1(str(i))
			         call linwid(thick(i))
			   	      call justifystring(xg0(i),yg0(i),
     &		   	      str(i)(1:nl),angle0(i),size0(i),jus(i))
				   endif
				   call setfnt(font(i))
				   call lincols(colors(i))
		               nl=nblank1(str(i))
			         call linwid(thick(i))
			         call justifystring(xg(i),yg(i),
     &		         str(i)(1:nl),angle(i),size(i),jus(i))
      	   		   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                     SIZE(i),JUS(i),xbox,ybox,1)
			   endif
			   if(ival.eq.82) then     ! Ins:toggle sensitivity
				   if(dm.eq.(0.005)) then
					deltax=0.01*abs(xp2-xp1)           ! movement
					deltay=0.01*abs(yp2-yp1)           ! movement
					ang=45.
					cs=1.
				   else
					dm=0.005
					deltax=0.005*abs(xp2-xp1)           ! movement
					deltay=0.005*abs(yp2-yp1)           ! movement
					ang=5.
					cs=0.2
				  endif
				endif
				if(ival.eq.83) then     ! DEL:toggle idraw
					if(jdraw(i).eq.0) then
			   		   jdraw(i)=1
					   call setfnt(font(i))
					   call lincols(colors(i))
					   nl=nblank1(str(i))
			         call linwid(thick(i))
					   call justifystring(xg(i),yg(i),
     &		               str(i)(1:nl),angle(i),size(i),jus(i))
					else
			   		   jdraw(i)=0
					   call setfnt(font(i))
					   call lincols(ibk)
					   nl=nblank1(str(i))
			         call linwid(thick(i))
					   call justifystring(xg(i),yg(i),
     &		               str(i)(1:nl),angle(i),size(i),jus(i))
					endif
				endif
		  		goto 33
			   else if(ktype.eq.3.or.ktype.eq.0) then
				size0(i)=size(i)
				if(ival.eq.43) then        ! +: increase size
	                     size(i)=size(i)+cs		!plus
				else if(ival.eq.45) then   ! -: decrease size
	                     size(i)=size(i)-cs		!minus
				endif
				if(first.eq.1) then
				   call setfnt(font(i))
		   		   call lincols(ibk)
      	   		   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                     SIZE0(i),JUS(i),xbox,ybox,1)
		               nl=nblank1(str(i))
			         call linwid(thick(i))
			         call justifystring(xg(i),yg(i),
     &		         str(i)(1:nl),angle(i),size0(i),jus(i))
				   first=0
				   goto 45
				endif
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
45	   		      continue
				call setfnt(font(i))
      	   		call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                  SIZE(i),JUS(i),xbox,ybox,0)
				call calcdim(xbox,ybox,4,ixlo,iylo,ixhi,iyhi)
			      idif=ixhi-ixlo
			      if(ixlo.lt.0) then
				   ixlo=0
				   ixhi=ixlo+idif
			      endif
				deallocate(array0)
				idim=2*((iyhi-iylo+9)*(ixhi-ixlo+9)+1)
				allocate(array0(idim))
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
				call lincols(colors(I))
      	   		call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                  SIZE(i),JUS(i),xbox,ybox,1)
				call setfnt(font(i))
				if(imd.ne.0) then
		            nl=nblank1(str(i))
			         call linwid(thick(i))
			      call justifystring(xg(i),yg(i),
     &		      str(i)(1:nl),angle(i),size(i),jus(i))
				endif
				goto 33
			   else if(ktype.eq.2) then
				if(ival.eq.98) then     ! b:toggle  box draw
					if(jdraw(i).eq.-1) then
					   do l=1,100000
						sk=3.5678*l
					   enddo
					   call lincols(ibk)
      	   		         call FBOX1(xg(i),yg(i),str(i),
     &				   ANGLE(i),
     &                           SIZE(i),JUS(i),xbox,ybox,1)
			   		   jdraw(i)=1
					else if(jdraw(i).eq.1) then
					   do l=1,1000000
						sk=3.5678*67.0*l
					   enddo
					   call lincols(colors(i))
      	   		         call FBOX1(xg(i),yg(i),str(i),
     &				   ANGLE(i),
     &                           SIZE(i),JUS(i),xbox,ybox,1)
			   		   jdraw(i)=-1
					endif
				      goto 33
				endif
				if(ival.eq.42) then           ! increase font
				   ifont=font(i)+1
				else if(ival.eq.47) then      ! decrease font
				   if(ifont.gt.0) ifont=font(i)-1
				endif
				if(first.eq.1) then
		   		   call lincols(ibk)
				   call setfnt(font(i))
      	   		   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                     SIZE(i),JUS(i),xbox,ybox,1)
		               nl=nblank1(str(i))
			         call linwid(thick(i))
			         call justifystring(xg(i),yg(i),
     &		         str(i)(1:nl),angle(i),size(i),jus(i))
				   first=0
				   goto 55
				endif
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
55				font(i)=ifont
				call setfnt(font(i))
      	   		call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                  SIZE(i),JUS(i),xbox,ybox,0)
				call calcdim(xbox,ybox,4,ixlo,iylo,ixhi,iyhi)
				idif=ixhi-ixlo
				if(ixlo.lt.0) then
				   ixlo=0
				   ixhi=ixlo+idif
				endif
				deallocate(array0)
				idim=2*((iyhi-iylo+9)*(ixhi-ixlo+9)+1)
				allocate(array0(idim))
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
				call lincols(colors(i))
      	   		call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &                  SIZE(i),JUS(i),xbox,ybox,1)
		            nl=nblank1(str(i))
			         call linwid(thick(i))
			      call justifystring(xg(i),yg(i),
     &		      str(i)(1:nl),angle(i),size(i),jus(i))
				goto 33
			   else if(ival.eq.13)then       ! set option
			   	if(imd.eq.0.and.jdraw(i).ne.0) then
				   call lincols(ibk)
				   call setfnt(font0(i))
		            nl=nblank1(str(i))
			         call linwid(thick(i))
			   	call justifystring(xg0(i),yg0(i),
     &		   	str(i)(1:nl),angle0(i),size0(i),jus(i))
			   	call lincols(colors(i))
				   call setfnt(font(i))
			         call linwid(thick(i))
			      call justifystring(xg(i),yg(i),
     &		      str(i)(1:nl),angle(i),size(i),jus(i))
				endif
				if(jdraw(i).ne.-1) then
				call lincols(ibk)
      	   	   	call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &               	SIZE(i),JUS(i),xbox,ybox,1)
				endif
        	  		call show_mouse()
				deallocate(array0)
				goto 1
	      	   else		!any other key to exit
	  	  		call hide_mouse()
				call lincols(ibk)
      	   	   	call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &               	SIZE(i),JUS(i),xbox,ybox,1)
        	  		call show_mouse()
				deallocate(array0)
	  	  		goto 99
	      	endif 		! end getch()
		   endif 			! end if(flag.eq.1)
		enddo				! end if(ibutton.eq.4)
	   else if(ibutton.eq.4) then ! right button pressed ->centre!!
	  	call hide_mouse()
	      call lincols(ibk)
		do k=1,m
      	   call FBOX1(xg(temp(k)),yg(temp(k)),
     &	   str(temp(k)),ANGLE(temp(k)),
     &         SIZE(temp(k)),JUS(temp(k)),
     &	   xbox,ybox,1)
            enddo
        	call show_mouse()
	    	goto 99     		! end of ibutton.eq.2
	   endif                      ! end mouse buttons selection
	endif                         ! end if (mouse_set)
99	continue
	call hide_mouse()

	 parval=str(1)
	 icol(26)=colors(1)
	 ifnt(6)=font(1)
	 rotate(6)=angle(1)
	 csize(6)=size(1)
	 call spagra(xg(1),yg(1),rx(6),ry(6))
c	 rx(6)=xg(1)
c	 ry(6)=yg(1)
	 ijus(6)=jus(1)
	 if(idraw(6).ne.-2) idraw(6)=jdraw(1)

	 if(colors(2).ne.icl) then
	 	icol(23)=colors(2)
	 else
	 	icol(23)=colors(3)
	 endif
	 xtitle=str(2)
	 ifnt(8)=font(2)
	 rotate(8)=angle(2)
	 csize(8)=size(2)
	 call spagra(xg(2),yg(2),rx(8),ry(8))
c	 rx(8)=xg(2)
c	 ry(8)=yg(2)
	 ijus(8)=jus(2)
	 if(idraw(8).ne.-2) idraw(8)=jdraw(2)

	 ytitle=str(3)
	 ifnt(9)=font(3)
	 rotate(9)=angle(3)
	 csize(9)=size(3)
	 call spagra(xg(3),yg(3),rx(9),ry(9))
c	 rx(9)=xg(3)
c	 ry(9)=yg(3)
	 ijus(9)=jus(3)
	 if(idraw(9).ne.-2) idraw(9)=jdraw(3)

	 title1=str(4)
	 icol(25)=colors(4)
	 ifnt(10)=font(4)
	 rotate(10)=angle(4)
	 csize(10)=size(4)
 	 call spagra(xg(4),yg(4),rx(10),ry(10))
c	 rx(10)=xg(4)
c	 ry(10)=yg(4)
	 ijus(10)=jus(4)
	 if(idraw(10).ne.-2) idraw(10)=jdraw(4)
	 if(colors(5).ne.ic0) then
		icol(24)=colors(5)
	 else
		icol(24)=colors(5+numbx)
	 endif
	 if(font(5).ne.ift0) then
		ifnt(7)=font(5)
	 else
		ifnt(7)=font(5+numbx)
	 endif
	 if(size(5).ne.csf0) then
		csize(7)=size(5)
	 else
		csize(7)=size(5+numbx)
	 endif
	 ifnt(7)=font(5)
	 csize(7)=size(5)
	 do k=1,numbx
	   	if(.not.logx)  then
		   cnumx(k)=str(k+4)
		endif
 	 	call spagra(xg(k+4),yg(k+4),rx(k+30),ry(k+30))
c	    	rx(k+30)=xg(k+4)
c		ry(k+30)=yg(k+4)
		rotate(k+30)=angle(k+4)
		ijus(k+30)=jus(k+4)
	      idraw(k+30)=jdraw(k+4)
	 enddo
	 do k=1,numby
	   	if(.not.logy)  then
		   cnumy(k)=str(k+4+numbx)
		endif
 	 	call spagra(xg(k+4+numbx),yg(k+4+numbx),rx(k+55),ry(k+55))
c	    	rx(k+55)=xg(k+4+numbx)
c		ry(k+55)=yg(k+4+numbx)
		rotate(k+55)=angle(k+4+numbx)
		ijus(k+55)=jus(k+4+numbx)
	      idraw(k+55)=jdraw(k+4+numbx)
	 enddo
	 if(ntext.gt.0) then
	   j=1
	   do k=1,ntext
	      idraw(k+10)=jdraw(k+4+numbx+numby)
		if(idraw(k+10).ne.0) then
		   newtext(j)=str(k+4+numbx+numby)
		   icol(j+30)=colors(k+4+numbx+numby)
		   ifnt(j+10)=font(k+4+numbx+numby)
		   csize(j+10)=size(k+4+numbx+numby)
 	 	   call spagra(xg(k+4+numbx+numby),yg(k+4+numbx+numby),
     &         rx(j+10),ry(j+10))
c		   rx(j+10)=xg(k+4+numbx+numby)
c		   ry(j+10)=yg(k+4+numbx+numby)
		   rotate(j+10)=angle(k+4+numbx+numby)
		   ijus(j+10)=jus(k+4+numbx+numby)
c---------------ioana
		   idraw(j+10)=idraw(k+10)
		   j=j+1
		endif
	   enddo
	   ntext=j-1
	endif
	deallocate(angle,colors,font,xg,yg,size)
      deallocate(str,jdraw,jus,thick)
	deallocate(xg0,yg0,size0,angle0,font0)

	ivl=isnum()
	if(.not.ivl) call NUMSET

	RETURN
	end
