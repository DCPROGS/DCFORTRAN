c*************************************************************************
	subroutine onenum(cnum,cexp,n,ixg,iyg,xjus,yjus,
     &           colors,angle,sizen,inumxy,logxy,jdraw,ibk,
     &           ixoff,iyoff,ixlo,iylo,ixhi,iyhi)

c*************************************************************************
c	to draw the numbers uses IDRAWNUM because now we move the resulting
c*************************************************************************
c	INPUT:
c	ixg,iyg 	: strings' display coordinates (0-10000,0-7500)
c	angle  	: strings' angles
c	colors  	: strings' colors
c	xjus,yjus   : strings'justification
c	cnum		: input strings
c	cexp		: index/exponent for strings
c	logxy		: .true.-> write exponent
c	numxy       :   = 0 -> write only instr
c	            :   = 1 -> write with exponent
c			:   = 2 -> write with index
c	ibk		: background's color
c
c	OUTPUT      :
c	ixg,iyg 	: strings' new display coordinates (0-10000,0-7500)
c	angle  	: strings' new angles
c========================================================================

	integer*4		size(:),ixg(n),iyg(n),jdraw(n)
	integer*4		colors(n),xjus(n),yjus(n)
	integer*4		angle(n),inumxy(n)
	integer*4		ixlo(n),iylo(n),ixhi(n),iyhi(n)
	integer*1		array0(:),temp(:)
	character*(*)	cnum(n),cexp(n)
	character*1 	ans,getch
	logical		kbhit,mouse_on,mouse_set,outtext,logxy(n)
	allocatable		array0,size ,temp
	allocate(size(n),temp(n))


	mouse_set=.false.
	mouse_on=.true.
	do i=1,n
	   call SETCSIZE(sizen,size(i))		!set isize
	enddo
	im=125            ! movement
	ia=45             ! angle
	first=1
	m=0
1	continue
	if(kbhit()) then
	   ans=getch(ktype)
	   ival=ichar(ans)
	   if(ival.eq.27.or.ival.eq.13) goto 99
	endif
	if (mouse_on) then
	   call show_mouse()
82	   call mouse_update(ibutton,ix,iy)
	   if(outtext.and.ibutton.ne.0) goto 82
	   outtext=.false.
	   if(ibutton.eq.0) mouse_set=.true.
	endif      ! end if mouse_on
	if(mouse_set) then
	   if(ibutton.ne.0) mouse_set=.false.	!until reset above
	   if(ibutton.eq.0) then
	    	if(ix.eq.ix0.and.iy.eq.iy0) goto 1
	    	if(m.eq.0) goto 10
		  do k=1,m
c		   call set_cursor(1,1)
c		   print*,k,temp(k),m,n
      	   call drawframe(ixlo(temp(k)),iylo(temp(k)),ixhi(temp(k)),
     &         iyhi(temp(k)),ibk)
		  enddo
		call show_mouse()
	    	m=0
10	    	do i=1,n
      	   call calcone(IXg(i),IYg(i),cnum(i),ANGLE(i),SIZE(i),
     &	   XJUS(i),YJUS(i),cexp(i),inumxy(i),logxy(i),
     &	   ixlo(i),iylo(i),ixhi(i),iyhi(i),idim)
	   	   call inside1(ix,iy,ixlo(i),iylo(i),ixhi(i),iyhi(i),flag)
	         if(flag.eq.1) then
		    	m=m+1
		    	temp(m)=i
		   endif
	    	enddo
	    	if(m.gt.0) then       		! show box(es) if inside
		call hide_mouse()
		   do k=1,m
      	      call drawframe(ixlo(temp(k)),iylo(temp(k)),
     &            ixhi(temp(k)),iyhi(temp(k)),colors(temp(k)))
		   enddo
		call show_mouse()
		endif
		ix0=ix
		iy0=iy
		goto 1  				! end of ibutton=0
	   else if(ibutton.eq.1) then        ! left button pressed
		m=0
		call hide_mouse()
	    	do i=1,n
      	   call calcone(IXg(i),IYg(i),cnum(i),ANGLE(i),SIZE(i),
     &	   XJUS(i),YJUS(i),cexp(i),inumxy(i),logxy(i),
     &	   ixlo(i),iylo(i),ixhi(i),iyhi(i),idim)
	   	   call inside1(ix,iy,ixlo(i),iylo(i),ixhi(i),iyhi(i),flag)
	         if(flag.eq.1) then
			first=1
			m=m+1
			temp(m)=i
      	      call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            iyhi(i),colors(i))
			call show_mouse()
			allocate(array0(idim))
2		    	call mouse_update(ibutton,ix,iy)
	  	    	if(ibutton.eq.1) then          ! move string if inside
			   if(ix.eq.ix0.and.iy.eq.iy0) goto 2
			   call hide_mouse()
			   if(first.eq.1) then
      	            call drawframe(ixlo(i),iylo(i),ixhi(i),
     &                  iyhi(i),ibk)
		   	      call coltyp18(ibk)
	                  call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &                  sizen,ixg(i),iyg(i),xjus(i),yjus(i),
     &     		      angle(i),logxy(i),inumxy(i))
				first=0
				goto 31
			   endif
			   call wrgwin1(ixlo(i),iylo(i),ixhi(i),iyhi(i),
     &               array0) !display
31			   idx=ix-ix0
			   idy=iy-iy0
		   	   call pixtohg(idx,idy,idxh,idyh)
			   ixg(i)=ixg(i)+idxh
			   iyg(i)=iyg(i)+idyh
      	   	   call calcone(IXg(i),IYg(i),cnum(i),ANGLE(i),
     &	         SIZE(i),XJUS(i),YJUS(i),cexp(i),inumxy(i),logxy(i),
     &	         ixlo(i),iylo(i),ixhi(i),iyhi(i),idim)
			   call rdgwin1(ixlo(i),iylo(i),ixhi(i),iyhi(i),
     &               array0) !display
      	         call drawframe(ixlo(i),iylo(i),ixhi(i),
     &               iyhi(i),colors(i))
		   	   call coltyp18(colors(i))
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),iyg(i),xjus(i),yjus(i),
     &     		   angle(i),logxy(i),inumxy(i))
			   ix0=ix
			   iy0=iy
			   call show_mouse()
			   goto 2
		    	else if (ibutton.eq.0) then
			   deallocate(array0)
			   goto 1
		   	else
			   call hide_mouse()
      	         call drawframe(ixlo(i),iylo(i),ixhi(i),
     &               iyhi(i),ibk)
			   deallocate(array0)
			   call show_mouse()
			   goto 99
		   	endif
		   else
			outtext=.true.
		   endif
	    	enddo
c		m=0
	    	do i=1,n        	! show all boxes if outside
		   m=m+1
		   temp(m)=i
      	   call drawframe(ixlo(i),iylo(i),ixhi(i),
     &         iyhi(i),colors(i))
	    	enddo
		call show_mouse()
	    	goto 1            ! end of ibutton.eq.1
	   else if(ibutton.eq.4) then   ! centre button pressed:
		first=1
		m=0
	    	do i=1,n
      	   call calcone(IXg(i),IYg(i),cnum(i),ANGLE(i),SIZE(i),
     &	   XJUS(i),YJUS(i),cexp(i),inumxy(i),logxy(i),
     &	   ixlo(i),iylo(i),ixhi(i),iyhi(i),idim)
	   	   call inside1(ix,iy,ixlo(i),iylo(i),ixhi(i),iyhi(i),flag)
	         if(flag.eq.1) then  ! if inside select box to move with keyboards
			m=m+1
			temp(m)=i
			allocate(array0(idim))
		  	call hide_mouse()
33	      	ans=getch(ktype)
	      	ival=ichar(ans)
	      	if(ktype.eq.8) then	!move menu with ARROW keys
		  	   if(ival.eq.77) then	!right
				if(first.eq.1) then
      	      	   call drawframe(ixlo(i),iylo(i),ixhi(i),
     &                     iyhi(i),ibk)
		   	         call coltyp18(ibk)
	                     call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &                     sizen,ixg(i),iyg(i),xjus(i),yjus(i),
     &     		         angle(i),logxy(i),inumxy(i))
				   first=0
				   goto 21
				endif
				call wrgwin1(ixlo(i),iylo(i),ixhi(i),iyhi(i),
     &			array0) !display
21		    		ixg(i)=ixg(i)+im
      	   		call calcone(IXg(i),IYg(i),cnum(i),ANGLE(i),
     &	   		SIZE(i),XJUS(i),YJUS(i),cexp(i),inumxy(i),
     &	   		logxy(i),ixlo(i),iylo(i),ixhi(i),iyhi(i),idim)
				call rdgwin1(ixlo(i),iylo(i),ixhi(i),iyhi(i),
     &			array0) !display
      	            call drawframe(ixlo(i),iylo(i),ixhi(i),
     &                  iyhi(i),colors(i))
		   	      call coltyp18(colors(i))
	                  call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &                  sizen,ixg(i),iyg(i),xjus(i),yjus(i),
     &     		      angle(i),logxy(i),inumxy(i))
		  	   endif
		  	   if(ival.eq.75) then		!shift left
				if(first.eq.1) then
      	               call drawframe(ixlo(i),iylo(i),ixhi(i),
     &                     iyhi(i),ibk)
		   	         call coltyp18(ibk)
	                     call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &                     sizen,ixg(i),iyg(i),xjus(i),yjus(i),
     &     		         angle(i),logxy(i),inumxy(i))
				   first=0
				   goto 22
				endif
				call wrgwin1(ixlo(i),iylo(i),ixhi(i),iyhi(i),
     &			array0) !display
22		    		ixg(i)=ixg(i)-im
      	   		call calcone(IXg(i),IYg(i),cnum(i),ANGLE(i),
     &	   		SIZE(i),XJUS(i),YJUS(i),cexp(i),inumxy(i),
     &	   		logxy(i),ixlo(i),iylo(i),ixhi(i),iyhi(i),idim)
			      call rdgwin1(ixlo(i),iylo(i),ixhi(i),iyhi(i),
     &			array0) !display
      	      	call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            	iyhi(i),colors(i))
		   	      call coltyp18(colors(i))
	                  call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &                  sizen,ixg(i),iyg(i),xjus(i),yjus(i),
     &     		      angle(i),logxy(i),inumxy(i))
		  	   endif
		  	   if(ival.eq.72) then	!shift up
				if(first.eq.1) then
      	      	   call drawframe(ixlo(i),iylo(i),ixhi(i),
     &                     iyhi(i),ibk)
		   	         call coltyp18(ibk)
	                     call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &                     sizen,ixg(i),iyg(i),xjus(i),yjus(i),
     &     		         angle(i),logxy(i),inumxy(i))
				   first=0
				   goto 23
				endif
				call wrgwin1(ixlo(i),iylo(i),ixhi(i),iyhi(i),
     &			array0) !display
23		    		iyg(i)=iyg(i)+im
      	   		call calcone(IXg(i),IYg(i),cnum(i),ANGLE(i),
     &	   		SIZE(i),XJUS(i),YJUS(i),cexp(i),inumxy(i),
     &	   		logxy(i),ixlo(i),iylo(i),ixhi(i),iyhi(i),idim)
			      call rdgwin1(ixlo(i),iylo(i),ixhi(i),iyhi(i),
     &			array0) !display
      	      	call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            	iyhi(i),colors(i))
		   	      call coltyp18(colors(i))
	                  call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &                  sizen,ixg(i),iyg(i),xjus(i),yjus(i),
     &     		      angle(i),logxy(i),inumxy(i))
		  	   endif
		  	   if(ival.eq.80) then	!shift down
				if(first.eq.1) then
      	      	   call drawframe(ixlo(i),iylo(i),ixhi(i),
     &                     iyhi(i),ibk)
		   	         call coltyp18(ibk)
	                     call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &                     sizen,ixg(i),iyg(i),xjus(i),yjus(i),
     &     		         angle(i),logxy(i),inumxy(i))
				   first=0
				   goto 24
				endif
				call wrgwin1(ixlo(i),iylo(i),ixhi(i),iyhi(i),
     &			array0) !display
24		    		iyg(i)=iyg(i)-im
      	   		call calcone(IXg(i),IYg(i),cnum(i),ANGLE(i),
     &	   		SIZE(i),XJUS(i),YJUS(i),cexp(i),inumxy(i),
     &	   		logxy(i),ixlo(i),iylo(i),ixhi(i),iyhi(i),idim)
			      call rdgwin1(ixlo(i),iylo(i),ixhi(i),iyhi(i),
     &			array0) !display
      	      	call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            	iyhi(i),colors(i))
		   	      call coltyp18(colors(i))
	                  call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &                  sizen,ixg(i),iyg(i),xjus(i),yjus(i),
     &     		      angle(i),logxy(i),inumxy(i))
		  	   endif
		  	   if(ival.eq.73) then	!PgDn:rotate clockwise
				if(first.eq.1) then
      	      	   call drawframe(ixlo(i),iylo(i),ixhi(i),
     &                     iyhi(i),ibk)
		   	         call coltyp18(ibk)
	                     call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &                     sizen,ixg(i),iyg(i),xjus(i),yjus(i),
     &     		         angle(i),logxy(i),inumxy(i))
				   first=0
				   goto 25
				endif
			      call wrgwin1(ixlo(i),iylo(i),ixhi(i),iyhi(i),
     &			array0) !display
25				angle(i)=angle(i)+ia
      	   		call calcone(IXg(i),IYg(i),cnum(i),ANGLE(i),
     &	   		SIZE(i),XJUS(i),YJUS(i),cexp(i),inumxy(i),
     &	   		logxy(i),ixlo(i),iylo(i),ixhi(i),iyhi(i),idim)
			      call rdgwin1(ixlo(i),iylo(i),ixhi(i),iyhi(i),
     &                  array0) !display
      	      	call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            	iyhi(i),colors(i))
		   	      call coltyp18(colors(i))
	                  call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &                  sizen,ixg(i),iyg(i),xjus(i),yjus(i),
     &     		      angle(i),logxy(i),inumxy(i))
		  		endif
		  	   if(ival.eq.81) then	!PgUp:rotate anticlockwise
				if(first.eq.1) then
      	      	   call drawframe(ixlo(i),iylo(i),ixhi(i),
     &                     iyhi(i),ibk)
		   	         call coltyp18(ibk)
	                     call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &                     sizen,ixg(i),iyg(i),xjus(i),yjus(i),
     &     		         angle(i),logxy(i),inumxy(i))
				   first=0
				   goto 125
			      endif
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
125				angle(i)=angle(i)-ia
      	   		call calcone(IXg(i),IYg(i),cnum(i),ANGLE(i),
     &	   		SIZE(i),XJUS(i),YJUS(i),cexp(i),inumxy(i),
     &	   		logxy(i),ixlo(i),iylo(i),ixhi(i),iyhi(i),idim)
			      call rdgwin1(ixlo(i),iylo(i),ixhi(i),iyhi(i),
     &                  array0) !display
      	      	call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            	iyhi(i),colors(i))
		   	      call coltyp18(colors(i))
	                  call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &                  sizen,ixg(i),iyg(i),xjus(i),yjus(i),
     &     		      angle(i),logxy(i),inumxy(i))
		  	   endif
			   if(ival.eq.82) then     ! Ins:toggle sensitivity
				   if(im.eq.10) then
					im=100
					ia=45
				   else
					im=16
					ia=5
				  endif
			   endif
			   if(ival.eq.83) then     ! DEL:toggle idraw
				   if(jdraw(i).eq.0) then
				      jdraw(i)=1
		   	      	call coltyp18(colors(i))
	                  	call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &                  	sizen,ixg(i),iyg(i),xjus(i),yjus(i),
     &     		      	angle(i),logxy(i),inumxy(i))
				   else
				      jdraw(i)=0
		   	      	call coltyp18(ibk)
	                  	call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &                  	sizen,ixg(i),iyg(i),xjus(i),yjus(i),
     &     		      	angle(i),logxy(i),inumxy(i))
				   endif
			   endif
		  	   goto 33
		      else if(ktype.eq.2)then       ! set option
			   if(ival.eq.98) then     ! b:toggle box
				   if(jdraw(i).eq.-1) then
				      jdraw(i)=1
      	      	      call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            	      iyhi(i),ibk)
				   else if(jdraw(i).eq.1)then
				      jdraw(i)=-1
      	      	      call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            	      iyhi(i),colors(i))
				   endif
			   endif
		  	   goto 33
		      else if(ktype.eq.4.and.ival.eq.13)then       ! set option
      	         call drawframe(ixlo(i),iylo(i),ixhi(i),
     &               iyhi(i),ibk)
        	  	   call show_mouse()
			   deallocate(array0)
			   goto 1
	            else		!any other key to exit
			   call hide_mouse()
			   call drawframe(ixlo(i),iylo(i),ixhi(i),
     &                  iyhi(i),ibk)
			   call show_mouse()
			   deallocate(array0)
			   goto 99
	      	endif 		! end getch()
		   endif 			! end if(flag.eq.1)
		enddo				! end if(ibutton.eq.4)
	   else if(ibutton.eq.2) then ! right button pressed
	  	call hide_mouse()
		do k=1,m
      	   call drawframe(ixlo(i),iylo(i),ixhi(i),
     &         iyhi(i),ibk)
            enddo
        	call show_mouse()
	    	goto 99     		! end of ibutton.eq.2
	   endif                      ! end mouse buttons selection
	endif                         ! end if (mouse_set)
99	continue
	call hide_mouse()
	deallocate(size)
	RETURN
	end
c============================================================================
