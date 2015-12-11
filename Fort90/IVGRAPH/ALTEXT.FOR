	subroutine altext(str,size,n,ixg,iyg,xjus,yjus,colors,angle,ibk)
c
c

	character*(*)	str(n)

	integer*4	size(n),ixg(n),iyg(n),ixbox(4),iybox(4)
	integer*4	colors(n),xjus(n),yjus(n),temp(30)
	real*4	angle(n)
	logical mouse_on
	common/mouse_on/nbutton
	mouse_on=.true.
	k=1
c	do i=1,n
c		ixg(i)=ixg(i)*10000/640
c		iyg(i)=iyg(i)*7500/480
c		print*,ixg(i),iyg(i)
c	enddo
	icol=12
	if(mouse_on) then
	  call show_mouse()
1	  call mouse_update(ibutton,ix,iy)
	  if(ibutton.eq.0) then
		if(ix.eq.ix0.and.iy.eq.iy0) goto 1
		if(m.eq.0) goto 10
		do k=1,m
		   call coltyp18(ibk)
      	   call FBOX1(IXg(temp(k)),IYg(temp(k)),STR(temp(k)),
     &	   ANGLE(temp(k)),SIZE(temp(k)),
     &	   XJUS(temp(k))
     &	  ,YJUS(temp(k)),ixbox,iybox,1)
		enddo
		m=0
10		do i=1,n
      	   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),SIZE(i),XJUS(i),
     &	   YJUS(i),ixbox,iybox,0)
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
      	      call FBOX1(IXg(temp(k)),IYg(temp(k)),STR(temp(k)),
     &		ANGLE(temp(k)),SIZE(temp(k)),
     &		XJUS(temp(k))
     &	      ,YJUS(temp(k)),ixbox,iybox,1)
		   enddo
		   if(m.eq.1) then
4		   	call mouse_update(ibutton,ix,iy)
	         	call set_cursor(1,10)
		      print*,m,ibutton
	   	      if(ibutton.eq.1) then
			   call set_cursor(1,1)
			   print*,'now'
			   if(ix.eq.ix1.and.iy.eq.iy1) goto 4
			   idx=ix-ix1
			   idy=iy-iy1
			   call pixtohg(idx,idy,idxh,idyh)
			   call coltyp18(ibk)
      	         call FBOX1(IXg(temp(1)),IYg(temp(1)),STR(temp(1)),
     &		   ANGLE(temp(1)),SIZE(temp(1)),
     &		   XJUS(temp(1))
     &	         ,YJUS(temp(1)),ixbox,iybox,1)
			   ixgold=ixg(temp(1))
			   iygold=iyg(temp(1))
			   call coltyp18(1)
			   ixg(temp(1))=ixg(temp(1))+idxh
			   iyg(temp(1))=iyg(temp(1))+idyh
      	         call FBOX1(IXg(temp(1)),IYg(temp(1)),STR(temp(1)),
     &		   ANGLE(temp(1)),SIZE(temp(1)),
     &		   XJUS(temp(1))
     &	         ,YJUS(temp(1)),ixbox,iybox,1)
			   ix1=ix
			   iy1=iy
			   goto 4
			else if(ibutton.eq.0) then
			   call coltyp18(ibk)
			   call justifystring(ixgold,iygold,
     &		   str(temp(1)),angle(temp(1)),size(temp(1)),
     &		   XJUS(temp(1))
     &	         ,YJUS(temp(1)))
			   call coltyp18(colors(temp(1)))
			   call justifystring(ixg(temp(1)),iyg(temp(1)),
     &		   str(temp(1)),angle(temp(1)),size(temp(1)),
     &		   XJUS(temp(1))
     &	         ,YJUS(temp(1)))
			   ix0=ix
			   iy0=iy
			   goto 1
			else
			   call coltyp18(ibk)
      	         call FBOX1(IXg(temp(1)),IYg(temp(1)),STR(temp(1)),
     &		   ANGLE(temp(1)),SIZE(temp(1)),
     &		   XJUS(temp(1))
     &	         ,YJUS(temp(1)),ixbox,iybox,1)
			   goto 99
		      endif
		   endif
		   ix0=ix
		   iy0=iy
		   k=i
		   goto 1
		endif
2		call mouse_update(ibutton,ix,iy)
	   	if(ibutton.eq.1) then
		   do i=1,n
		      call coltyp18(icol)
      	      call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),SIZE(i),
     &            XJUS(i),
     &	      YJUS(i),ixbox,iybox,1)
   		   enddo
		   goto 2
		else
		   do i=1,n
			call coltyp18(ibk)
      	      call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),SIZE(i),
     &            XJUS(i),
     &	      YJUS(i),ixbox,iybox,1)
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
