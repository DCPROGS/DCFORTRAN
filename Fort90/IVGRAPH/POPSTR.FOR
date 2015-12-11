c*************************************************************************
	subroutine popstr(str,size,n,ixg,iyg,xjus,yjus,colors,angle,ibk)
c*************************************************************************

	character*(*)	str(n)

	integer*4	size(n),ixg(n),iyg(n),ixbox(4),iybox(4)
	integer*4	colors(n),xjus(n),yjus(n),temp(30)
	integer*4	angle(n)
	character*1 ans,getch
	logical mouse_on,kbhit
	common/mouse_on/nbutton
	mouse_on=.true.
	if (mouse_on) then
	   call show_mouse()
1	   call mouse_update(ibutton,ix,iy)
	   if(ibutton.eq.0) then
	    	if(ix.eq.ix0.and.iy.eq.iy0) goto 1
	    	if(m.eq.0) goto 10
		call coltyp18(ibk)
	    	do k=1,m
		   call coltyp18(ibk)
      	   call FBOX1(IXg(temp(k)),IYg(temp(k)),STR(temp(k)),
     &	   ANGLE(temp(k)),SIZE(temp(k)),
     &	   XJUS(temp(k))
     &	   ,YJUS(temp(k)),ixbox,iybox,1)
	    	enddo
		call coltyp18(ibk)
c	    	do i=1,n
c      	   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),SIZE(i),
c     &	   XJUS(i),YJUS(i),ixbox,iybox,1)
c		enddo
	    	m=0
10	    	do i=1,n
      	   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),SIZE(i),
     &	   XJUS(i),YJUS(i),ixbox,iybox,0)
		   call pixtohg(ix,iy,ixh,iyh)
	   	   call inside(ixh,iyh,ixbox,iybox,flag)
	         if(flag.eq.1) then
		    	m=m+1
		    	temp(m)=i
		   endif
	    	enddo
	    	if(m.gt.0) then
		   call coltyp18(1)
		   do k=1,m
      	    	call FBOX1(IXg(temp(k)),IYg(temp(k)),
     &		STR(temp(k)),ANGLE(temp(k)),SIZE(temp(k)),
     &	    	XJUS(temp(k)),YJUS(temp(k)),ixbox,iybox,1)
		   enddo
		endif
		ix0=ix
		iy0=iy
		goto 1
	   else if(ibutton.eq.1) then
		m=0
	    	do i=1,n
      	   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &	   SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
		   call pixtohg(ix,iy,ixh,iyh)
	   	   call inside(ixh,iyh,ixbox,iybox,flag)
	         if(flag.eq.1) then
			m=m+1
			temp(m)=i
      	      call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &		SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
2		    	call mouse_update(ibutton,ix,iy)
	  	    	if(ibutton.eq.1) then
			   if(ix.eq.ix0.and.iy.eq.iy0) goto 2
			   call hide_mouse()
			   call coltyp18(ibk)
      	         call FBOX1(IXgo,IYgo,STR(i),
     &		   ANGLE(i),SIZE(i),XJUS(i),
     &	         YJUS(i),ixbox,iybox,1)
c		   	   call coltyp18(ibk)
			   call justifystring(ixgo,iygo,
     &		   str(i),angle(i),size(i),xjus(i),yjus(i))
c			   ixgo=ixg(i)
c			   iygo=iyg(i)
			   idx=ix-ix0
			   idy=iy-iy0
		   	   call pixtohg(idx,idy,idxh,idyh)
			   ixg(i)=ixg(i)+idxh
			   iyg(i)=iyg(i)+idyh
			   call coltyp18(1)
      	         call FBOX1(IXg(i),IYg(i),STR(i),
     &		   ANGLE(i),SIZE(i),XJUS(i),
     &	         YJUS(i),ixbox,iybox,1)
		   	   call coltyp18(colors(i))
			   call justifystring(ixg(i),iyg(i),
     &		   str(i),angle(i),size(i),xjus(i),yjus(i))
			   ix0=ix
			   iy0=iy
			   ixgo=ixg(i)
			   iygo=iyg(i)
			   call show_mouse()
			   goto 2
		    	else if (ibutton.eq.0) then
		   	   call coltyp18(ibk)
			   call justifystring(ixgo,iygo,
     &		   str(i),angle(i),size(i),xjus(i),yjus(i))
		   	   call coltyp18(colors(i))
			   call justifystring(ixg(i),iyg(i),
     &		   str(i),angle(i),size(i),xjus(i),yjus(i))
			   ix0=ix
			   iy0=iy
			   call show_mouse()
			   goto 1
		   	else
			   call hide_mouse()
			   call show_mouse()
			   goto 99
		   	endif
		   endif
	    	enddo
	    	do i=1,n
		   m=m+1
		   temp(m)=i
		   call coltyp18(12)
      	   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &	   SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
	    	enddo
	    	goto 1
	   else if(ibutton.eq.4) then
	   print*,'centre'
		m=0
	    	do i=1,n
      	   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &         SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
		   call pixtohg(ix,iy,ixh,iyh)
	   	   call inside(ixh,iyh,ixbox,iybox,flag)
	         if(flag.eq.1) then
			m=m+1
			temp(m)=i

c	    		if(kbhit()) then
		  		call hide_mouse()
33	      	   ans=getch(ktype)
	      	   ival=ichar(ans)	!here you may give other options
	      	   if(ktype.eq.8) then	!move menu with ARROW keys
		  		if(ival.eq.73.or.ival.eq.77.or.ival.eq.81) then		!shift right
		   		   call coltyp18(ibk)
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         call justifystring(ixg(i),iyg(i),
     &		         str(i),angle(i),size(i),xjus(i),yjus(i))
		    		   ixg(i)=ixg(i)+80
		   		   call coltyp18(1)
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         call justifystring(ixg(i),iyg(i),
     &		         str(i),angle(i),size(i),xjus(i),yjus(i))
		  		endif
		  		if(ival.eq.71.or.ival.eq.75.or.ival.eq.79) then		!shift left
		   		   call coltyp18(ibk)
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         call justifystring(ixg(i),iyg(i),
     &		         str(i),angle(i),size(i),xjus(i),yjus(i))
		    		   ixg(i)=ixg(i)-80
		   		   call coltyp18(1)
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         call justifystring(ixg(i),iyg(i),
     &		         str(i),angle(i),size(i),xjus(i),yjus(i))
		  		endif
		  		if(ival.eq.71.or.ival.eq.72.or.ival.eq.73) then		!shift up
		   		   call coltyp18(ibk)
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         call justifystring(ixg(i),iyg(i),
     &		         str(i),angle(i),size(i),xjus(i),yjus(i))
		    		   iyg(i)=iyg(i)+80
		   		   call coltyp18(1)
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         call justifystring(ixg(i),iyg(i),
     &		         str(i),angle(i),size(i),xjus(i),yjus(i))
		  		endif
		  		if(ival.eq.79.or.ival.eq.80.or.ival.eq.81) then		!shift down
		   		   call coltyp18(ibk)
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         call justifystring(ixg(i),iyg(i),
     &		         str(i),angle(i),size(i),xjus(i),yjus(i))
		    		   iyg(i)=iyg(i)-80
		   		   call coltyp18(1)
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         call justifystring(ixg(i),iyg(i),
     &		         str(i),angle(i),size(i),xjus(i),yjus(i))
		  		endif
		  		if(ival.eq.82) then		!shift right
		   		   call coltyp18(ibk)
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         call justifystring(ixg(i),iyg(i),
     &		         str(i),angle(i),size(i),xjus(i),yjus(i))
				   call set_cursor(1,5)
				   print*,angle(i)
				   angle(i)=angle(i)+10
		   		   call coltyp18(1)
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         call justifystring(ixg(i),iyg(i),
     &		         str(i),angle(i),size(i),xjus(i),yjus(i))
		  		endif
c        	  		call show_mouse()
		  		goto 33
			   else if(ival.eq.13)then
        	  		call show_mouse()
				goto 1
	      	   else		!any other key to exit
	  	  		call hide_mouse()
        	  		call show_mouse()
	  	  		goto 99
	      	   endif
c	    		endif
c			goto
		   endif
		enddo
	   else if(ibutton.eq.2) then
	    	goto 99
	   endif
	endif
99	continue
	call hide_mouse()
	RETURN
	end
