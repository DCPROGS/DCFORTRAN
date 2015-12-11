c*************************************************************************
	subroutine movestr(str,size,n,ixg,iyg,xjus,yjus,colors,angle,
     &			font,ibk)
c*************************************************************************

	character*(*)	str(n)

	integer*4	size(n),ixg(n),iyg(n),ixbox(4),iybox(4)
	integer*4	colors(n),xjus(n),yjus(n),temp(30),font(n)
	integer*4	angle(n)
	integer*1	array1(:)
	character*55 	helpst(30)		!help strings
	character*1 ans,getch
	logical mouse_on,kbhit,outtext,mouse_set
	common/mousval/mouse_on,nbutton
	allocatable :: array1

	helpst(1)='Left button pressed :'
	helpst(2)=' - inside box : move selected string'
	helpst(3)=' - outside    : show all boxes'
	helpst(4)='Right button pressed :'
	helpst(5)=' - escape'
	helpst(6)='Central button pressed :'
	helpst(7)=' - select box to move with keyboards:'
	helpst(8)='       ->: move right'
	helpst(9)='       <-: move left'
	helpst(10)='      /\: up'
	helpst(11)='      \/: down'
	helpst(12)='      + : increase size'
	helpst(13)='      - : decrease size'
	helpst(14)='      * : increase font number'
	helpst(15)='      / : decrease font number'
	helpst(16)='      R : increase angle'
	helpst(17)='      r : decrease angle'
	helpst(18)='    <CR>: end selection'
	nhelp=18
	icq=14
	mouse_set=.false.
1	continue
	if(kbhit()) then
	   ans=getch(ktype)
	   ival=ichar(ans)
	   if(ktype.eq.0.and.ival.eq.59) then	!F1=help
		ixloh=320    !position for help box
		iyhih=320
		call poptext(ixloh,-1,iyhih,helpst,nhelp,15,2,icq)	!=popkeys
		ibutton=0
		outtext=.true.
		mouse_set=.false.
		call show_mouse()
		goto 1
	   endif
	endif
	if (mouse_on) then
	   call show_mouse()
82	   call mouse_update(ibutton,ix,iy)
	   if(outtext.and.ibutton.ne.0) goto 82
	   outtext=.false.
	   if(ibutton.eq.0) mouse_set=.true.
	endif
	if(mouse_set) then
	   if(ibutton.ne.0) mouse_set=.false.	!until reset above

	   if(ibutton.eq.0) then
	    	if(ix.eq.ix0.and.iy.eq.iy0) goto 1
	    	if(m.eq.0) goto 10
		call coltyp18(ibk)
		call hide_mouse()
	    	do k=1,m
		   call coltyp18(ibk)
		   call setfont1(font(temp(k)))
      	   call FBOX1(IXg(temp(k)),IYg(temp(k)),STR(temp(k)),
     &	   ANGLE(temp(k)),SIZE(temp(k)),
     &	   XJUS(temp(k))
     &	   ,YJUS(temp(k)),ixbox,iybox,1)
	    	enddo
		call coltyp18(ibk)
	    	m=0
10	    	do i=1,n
		   call setfont1(font(i))
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
			call setfont1(font(temp(k)))
      	    	call FBOX1(IXg(temp(k)),IYg(temp(k)),
     &		STR(temp(k)),ANGLE(temp(k)),SIZE(temp(k)),
     &	    	XJUS(temp(k)),YJUS(temp(k)),ixbox,iybox,1)
		   enddo
		endif
		ix0=ix
		iy0=iy
		call show_mouse()
		goto 1  ! end of ibutton=0
	   else if(ibutton.eq.1) then
		m=0
		call hide_mouse()
	    	do i=1,n
		   call setfont1(font(i))
      	   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &	   SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
		   call pixtohg(ix,iy,ixh,iyh)
	   	   call inside(ixh,iyh,ixbox,iybox,flag)
	         if(flag.eq.1) then
			first=1
			m=m+1
			temp(m)=i
c			call setfont1(font(i))
      	      call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &		SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			call show_mouse()
			call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
			idif=ixhi-ixlo
			if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idif
			endif
			idim=2*((iyhi-iylo+9)*(ixhi-ixlo+9)+1)
			allocate(array1(idim))
2		    	call mouse_update(ibutton,ix,iy)
	  	    	if(ibutton.eq.1) then
			   if(ix.eq.ix0.and.iy.eq.iy0) goto 2
			   call hide_mouse()
			   if(first.eq.1) then
			   	call coltyp18(ibk)
      	         	call FBOX1(IXg(i),IYg(i),STR(i),
     &		   	ANGLE(i),SIZE(i),XJUS(i),
     &	         	YJUS(i),ixbox,iybox,1)
			   	call justifystring(ixg(i),iyg(i),
     &		   	str(i),angle(i),size(i),xjus(i),yjus(i))
				first=0
				goto 31
			   endif
			   call wrgwin1(ixlo,iylo,ixhi,iyhi,array1) !display
31			   idx=ix-ix0
			   idy=iy-iy0
		   	   call pixtohg(idx,idy,idxh,idyh)
			   ixg(i)=ixg(i)+idxh
			   iyg(i)=iyg(i)+idyh
      	   	   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &               SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
			   call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
			idif=ixhi-ixlo
			if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idif
			endif
			   call rdgwin1(ixlo,iylo,ixhi,iyhi,array1) !display
			   call coltyp18(1)
      	   	   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &               SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
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
			   deallocate(array1)
			   goto 1
		   	else
			   call hide_mouse()
			   call coltyp18(ibk)
      	   	   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &               SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			   deallocate(array1)
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
		call show_mouse()
	    	goto 1
	   else if(ibutton.eq.4) then
		first=1
c	   print*,'centre'
		m=0
	    	do i=1,n
		   call setfont1(font(i))
      	   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &         SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
		   call pixtohg(ix,iy,ixh,iyh)
	   	   call inside(ixh,iyh,ixbox,iybox,flag)
	         if(flag.eq.1) then
			m=m+1
			temp(m)=i
			call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
			idif=ixhi-ixlo
			if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idif
			endif
			idim=2*((iyhi-iylo+9)*(ixhi-ixlo+9)+1)
			allocate(array1(idim))
c	    		if(kbhit()) then
		  		call hide_mouse()
33	      	   ans=getch(ktype)
	      	   ival=ichar(ans)	!here you may give other options
	      	   if(ktype.eq.8) then	!move menu with ARROW keys
		  		if(ival.eq.73.or.ival.eq.77.or.ival.eq.81) then		!shift right
				   if(first.eq.1) then
		   		   	call coltyp18(ibk)
      	   		   	call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     	SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         	call justifystring(ixg(i),iyg(i),
     &		         	str(i),angle(i),size(i),xjus(i),yjus(i))
					first=0
					goto 21
				   endif
				   call wrgwin1(ixlo,iylo,ixhi,iyhi,array1) !display
21		    		   ixg(i)=ixg(i)+80
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
				   call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
			idif=ixhi-ixlo
			if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idif
			endif
				   call rdgwin1(ixlo,iylo,ixhi,iyhi,array1) !display
				   call coltyp18(colors(I))
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         call justifystring(ixg(i),iyg(i),
     &		         str(i),angle(i),size(i),xjus(i),yjus(i))
		  		endif
		  		if(ival.eq.71.or.ival.eq.75.or.ival.eq.79) then		!shift left
				   if(first.eq.1) then
		   		   	call coltyp18(ibk)
      	   		   	call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     	SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         	call justifystring(ixg(i),iyg(i),
     &		         	str(i),angle(i),size(i),xjus(i),yjus(i))
					first=0
					goto 22
				   endif
				   call wrgwin1(ixlo,iylo,ixhi,iyhi,array1) !display
22		    		   ixg(i)=ixg(i)-80
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
				   call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
			idif=ixhi-ixlo
			if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idif
			endif
				   call rdgwin1(ixlo,iylo,ixhi,iyhi,array1) !display
				   call coltyp18(COLORS(I))
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         call justifystring(ixg(i),iyg(i),
     &		         str(i),angle(i),size(i),xjus(i),yjus(i))
		  		endif
		  		if(ival.eq.71.or.ival.eq.72.or.ival.eq.73) then		!shift up
				   if(first.eq.1) then
		   		   	call coltyp18(ibk)
      	   		   	call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     	SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         	call justifystring(ixg(i),iyg(i),
     &		         	str(i),angle(i),size(i),xjus(i),yjus(i))
					first=0
					goto 23
				   endif
				   call wrgwin1(ixlo,iylo,ixhi,iyhi,array1) !display
23		    		   iyg(i)=iyg(i)+80
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
				   call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
			idif=ixhi-ixlo
			if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idif
			endif
				   call rdgwin1(ixlo,iylo,ixhi,iyhi,array1) !display
				   call coltyp18(colors(I))
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         call justifystring(ixg(i),iyg(i),
     &		         str(i),angle(i),size(i),xjus(i),yjus(i))
		  		endif
		  		if(ival.eq.79.or.ival.eq.80.or.ival.eq.81) then		!shift down
				   if(first.eq.1) then
		   		   	call coltyp18(ibk)
      	   		   	call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     	SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         	call justifystring(ixg(i),iyg(i),
     &		         	str(i),angle(i),size(i),xjus(i),yjus(i))
					first=0
					goto 24
				   endif
				   call wrgwin1(ixlo,iylo,ixhi,iyhi,array1) !display
24		    		   iyg(i)=iyg(i)-80
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
				   call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
			idif=ixhi-ixlo
			if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idif
			endif
				   call rdgwin1(ixlo,iylo,ixhi,iyhi,array1) !display
				   call coltyp18(colors(I))
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         call justifystring(ixg(i),iyg(i),
     &		         str(i),angle(i),size(i),xjus(i),yjus(i))
		  		endif
		  		if(ival.eq.82) then		!shift right
				   if(first.eq.1) then
		   		   	call coltyp18(ibk)
      	   		   	call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     	SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         	call justifystring(ixg(i),iyg(i),
     &		         	str(i),angle(i),size(i),xjus(i),yjus(i))
					first=0
					goto 25
				   endif
				   call wrgwin1(ixlo,iylo,ixhi,iyhi,array1) !display
25				   angle(i)=angle(i)+10
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
				   call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
			idif=ixhi-ixlo
			if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idif
			endif
				   call rdgwin1(ixlo,iylo,ixhi,iyhi,array1) !display
				   call coltyp18(colors(I))
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         call justifystring(ixg(i),iyg(i),
     &		         str(i),angle(i),size(i),xjus(i),yjus(i))
		  		endif
		  		goto 33
			   else if(ktype.eq.3) then
				if(ival.eq.43) then
				   if(isize.lt.7) isize=size(i)+1
				else if(ival.eq.45) then
				   if(isize.gt.0) isize=size(i)-1
				endif
				if(first.eq.1) then
		   		   call coltyp18(ibk)
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         call justifystring(ixg(i),iyg(i),
     &		         str(i),angle(i),size(i),xjus(i),yjus(i))
				   first=0
				   goto 45
				endif
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array1) !display
45				size(I)=isize
      	   		call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
				call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
			idif=ixhi-ixlo
			if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idif
			endif
				deallocate(array1)
				idim=2*((iyhi-iylo+9)*(ixhi-ixlo+9)+1)
				allocate(array1(idim))
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array1) !display
				call coltyp18(colors(I))
      	   		call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			      call justifystring(ixg(i),iyg(i),
     &		      str(i),angle(i),size(i),xjus(i),yjus(i))
				goto 33
			   else if(ktype.eq.2) then
				if(ival.eq.42) then
				   ifont=ifont+1
				else if(ival.eq.47) then
				   if(ifont.gt.0) ifont=ifont-1
				endif
				if(first.eq.1) then
		   		   call coltyp18(ibk)
      	   		   call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			         call justifystring(ixg(i),iyg(i),
     &		         str(i),angle(i),size(i),xjus(i),yjus(i))
				   first=0
				   goto 55
				endif
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array1) !display
				font(i)=ifont
55				call setfont1(font(i))
      	   		call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
				call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
			idif=ixhi-ixlo
			if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idif
			endif
				deallocate(array1)
				idim=2*((iyhi-iylo+9)*(ixhi-ixlo+9)+1)
				allocate(array1(idim))
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array1) !display
				call coltyp18(colors(I))
      	   		call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			      call justifystring(ixg(i),iyg(i),
     &		      str(i),angle(i),size(i),xjus(i),yjus(i))
				goto 33
			else if(ival.eq.13)then
				call coltyp18(ibk)
      	   	   	call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &               	SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
        	  		call show_mouse()
				deallocate(array1)
				ixgo=ixg(i)
				iygo=iyg(i)
				index=i
				goto 1
	      	   else		!any other key to exit
	  	  		call hide_mouse()
				call coltyp18(ibk)
      	   	   	call FBOX1(IXg(i),IYg(i),STR(i),ANGLE(i),
     &               	SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
        	  		call show_mouse()
				deallocate(array1)
	  	  		goto 99
	      	   endif
		   endif
		enddo
	   else if(ibutton.eq.2) then
	  	call hide_mouse()
	      call coltyp18(ibk)
		do k=1,m
      	   call FBOX1(IXg(temp(k)),IYg(temp(k)),
     &	   STR(temp(k)),ANGLE(temp(k)),
     &         SIZE(temp(k)),XJUS(temp(k)),YJUS(temp(k)),
     &	   ixbox,iybox,1)
            enddo
		call set_cursor(1,20)
c		print*,i,m,temp(k)
        	call show_mouse()
	    	goto 99
	   endif
	endif
99	continue
	call hide_mouse()
	RETURN
	end
