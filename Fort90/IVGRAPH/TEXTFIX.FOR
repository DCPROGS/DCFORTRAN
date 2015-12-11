	subroutine textfix(str,size,n,ixg,iyg,xjus,yjus,colors,angle,ibk)
c
c

	character*(*)	str(n)

	integer*4	size(n),ixg(n),iyg(n),ixbox(4),iybox(4)
	integer*4	colors(n),xjus(n),yjus(n)
	real*4	angle(n)
	logical mouse_on
	common/mouse_on/nbutton
	mouse_on=.true.
	k=1
	do i=1,n
c		ixg(i)=ixg(i)*10000/640
c		iyg(i)=iyg(i)*7500/480
c		print*,ixg(i),iyg(i)
	enddo
	icol=12
	if(mouse_on) then
	  call show_mouse()
1	  call mouse_update(ibutton,ix,iy)
	  if(ibutton.eq.0) then
		if(ix.eq.ix0.and.iy.eq.iy0) goto 1
      	     call FBOX1(IXg(k),IYg(k),STR(k),ANGLE(k),SIZE(k),
     &		XJUS(k)
     &	     ,YJUS(k),ixbox,iybox,0)
		   call pixtohg(ix,iy,ixh,iyh)

	   	   call inside(ixh,iyh,ixbox,iybox,flag)
		     call coltyp18(ibk)
	         if(flag.eq.0) then
		     call coltyp18(ibk)
      	     call FBOX1(IXg(k),IYg(k),STR(k),ANGLE(k),SIZE(k),
     &		XJUS(k)
     &	     ,YJUS(k),ixbox,iybox,1)
		endif
		do i=1,n
      	   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),SIZE(i),XJUS(i),
     &	   YJUS(i),ixbox,iybox,0)
		   call pixtohg(ix,iy,ixh,iyh)

	   	   call inside(ixh,iyh,ixbox,iybox,flag)
		   call set_cursor(1,10)
c		   print*,flag
	         if(flag.eq.1) then
		     call coltyp18(1)
      	     call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),SIZE(i),
     &		XJUS(i)
     &	     ,YJUS(i),ixbox,iybox,1)
4		     call mouse_update(ibutton,ix,iy)
	   		  if(ibutton.eq.1) then
			  endif
		     ix0=ix
			iy0=iy
			k=i
		     goto 1
		   endif
		enddo
2		call mouse_update(ibutton,ix,iy)
	   	if(ibutton.eq.1) then
		     do i=1,n
			 call coltyp18(icol)
      	       call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),SIZE(i),
     &             XJUS(i),
     &	       YJUS(i),ixbox,iybox,1)
   		     enddo
		     goto 2
		else
			 do i=1,n
			 call coltyp18(ibk)
      	       call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),SIZE(i),
     &             XJUS(i),
     &	       YJUS(i),ixbox,iybox,1)
			 enddo
		     goto 1
		endif
	  else if(ibutton.eq.2) then
		goto 99
	  else
	     goto 1
	  endif
	endif
99	continue
	call hide_mouse()
	end
