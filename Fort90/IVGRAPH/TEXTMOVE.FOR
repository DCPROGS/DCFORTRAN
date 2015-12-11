c*************************************************************************
	subroutine textmove(TEXT,csfac,csize,n,ixg,iyg,xjus,yjus,colors,
     &           angle,font,ibk,jdraw,imd)
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

	real*4		csfac(n)
	integer*4		size(:),ixg(n),iyg(n),ixbox(4),iybox(4)
	integer*4		colors(n),xjus(n),yjus(n),temp(100),font(n)
	integer*4		angle(n),jdraw(n),ixg0(:),iyg0(:)
	integer*1		array0(:)
	character*(*)	TEXT(n)
	character*55 	helpst(30)		!help strings
	character*1 	ans,getch
	logical mouse_on,kbhit,outtext,mouse_set
	common/mousval/mouse_on,nbutton
	allocatable :: array0,size,ixg0,iyg0
c	HELP LINES:
C========================================================
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
	helpst(16)='    PgUp: increase angle'
	helpst(17)='    PgDn: decrease angle'
	helpst(18)='     Ins: toggle sensitivity'
	helpst(19)='    Home: decrease color'
	helpst(20)='     End: increase color'
	helpst(21)='    <CR>: end selection'
c============================================================
	allocate(size(n),ixg0(n),iyg0(n))
	nhelp=21
	icq=14     		! color help menu
	im=100            ! movement
	ia=45             ! angle
	cs=1.
	do i=1,n
		sizen=csfac(i)*csize
		call setcsize(sizen,size(i))
	enddo
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
		   call setfont1(font(temp(k)))
		   sizen=csfac(temp(k))*csize
		   call setcsize(sizen,size(temp(k)))
		   if( jdraw(temp(k)).eq.-1) then
			call coltyp18(colors(temp(k)))
		   else
		      call coltyp18(ibk)
		   endif
      	   call FBOX1(IXg(temp(k)),IYg(temp(k)),TEXT(temp(k)),
     &	   ANGLE(temp(k)),SIZE(temp(k)),
     &	   XJUS(temp(k))
     &	   ,YJUS(temp(k)),ixbox,iybox,1)
	    	enddo
	    	m=0
10	    	do i=1,n
		   sizen=csfac(i)*csize
		   call setcsize(sizen,size(i))
		   call setfont1(font(i))
      	   call FBOX1(IXg(i),IYg(i),text(i),ANGLE(i),SIZE(i),
     &	   XJUS(i),YJUS(i),ixbox,iybox,0)
		   call pixtohg(ix,iy,ixh,iyh)
	   	   call inside(ixh,iyh,ixbox,iybox,flag)
	         if(flag.eq.1) then
		    	m=m+1
		    	temp(m)=i
		   endif
	    	enddo
	    	if(m.gt.0) then       		! show box(es) if inside
		   do k=1,m
		      sizen=csfac(temp(k))*csize
		      call setcsize(sizen,size(temp(k)))
		   	call coltyp18(colors(temp(k)))    !!!!
			call setfont1(font(temp(k)))
      	    	call FBOX1(IXg(temp(k)),IYg(temp(k)),
     &		TEXT(temp(k)),ANGLE(temp(k)),SIZE(temp(k)),
     &	    	XJUS(temp(k)),YJUS(temp(k)),ixbox,iybox,1)
		   enddo
		endif
		ix0=ix
		iy0=iy
		call show_mouse()
		goto 1  				! end of ibutton=0
	   else if(ibutton.eq.1) then        ! left button pressed
		m=0
		call hide_mouse()
	    	do i=1,n
		   call setfont1(font(i))
		   sizen=csfac(i)*csize
		   call setcsize(sizen,size(i))
c		   nl=nblank1(text(i))
      	   call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &	   SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
		   call pixtohg(ix,iy,ixh,iyh)
	   	   call inside(ixh,iyh,ixbox,iybox,flag)
	         if(flag.eq.1) then
			first=1
			m=m+1
			temp(m)=i
			call setfont1(font(i))
		   	call coltyp18(colors(i))    !!!!
c		   nl=nblank1(text(i))
		      sizen=csfac(i)*csize
		      call setcsize(sizen,size(i))
      	      call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &		SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			call show_mouse()
			call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
			idif=ixhi-ixlo
			if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idif
			endif
			idim=2*((iyhi-iylo+9)*(ixhi-ixlo+9)+1)
			allocate(array0(idim))
			ixg0(i)=ixg(i)
			iyg0(i)=iyg(i)
2		    	call mouse_update(ibutton,ix,iy)
	  	    	if(ibutton.eq.1) then          ! move string if inside
			   if(ix.eq.ix0.and.iy.eq.iy0) goto 2
			   call hide_mouse()
			   if(first.eq.1) then
			   	call coltyp18(ibk)
		            sizen=csfac(i)*csize
		            call setcsize(sizen,size(i))
      	         	call FBOX1(IXg(i),IYg(i),TEXT(i),
     &		   	ANGLE(i),SIZE(i),XJUS(i),
     &	         	YJUS(i),ixbox,iybox,1)
		            nl=nblank1(text(i))
				if(imd.ne.0) then
			   	call justifystring(ixg(i),iyg(i),
     &		   	TEXT(i)(1:nl),angle(i),size(i),xjus(i),yjus(i))
				endif
				first=0
				goto 31
			   endif
			   call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
31			   idx=ix-ix0
			   idy=iy-iy0
		   	   call pixtohg(idx,idy,idxh,idyh)
			   ixg(i)=ixg(i)+idxh
			   iyg(i)=iyg(i)+idyh
		         sizen=csfac(i)*csize
		         call setcsize(sizen,size(i))
      	   	   call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &               SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
			   call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
			   idif=ixhi-ixlo
			   if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idif
			   endif
			   call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
		   	   call coltyp18(colors(i))
		         sizen=csfac(i)*csize
		         call setcsize(sizen,size(i))
      	   	   call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &               SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		         nl=nblank1(text(i))
			   if(imd.ne.0) then
			   call justifystring(ixg(i),iyg(i),
     &		   TEXT(i)(1:nl),angle(i),size(i),xjus(i),yjus(i))
			   endif
			   ix0=ix
			   iy0=iy
			   call show_mouse()
			   goto 2
		    	else if (ibutton.eq.0) then
			   if(imd.eq.0) then
			   call hide_mouse()
		         nl=nblank1(text(i))
			   call coltyp18(ibk)
			   call justifystring(ixg0(i),iyg0(i),
     &		   TEXT(i)(1:nl),angle(i),size(i),xjus(i),yjus(i))
			   call coltyp18(colors(i))
			   call justifystring(ixg(i),iyg(i),
     &		   TEXT(i)(1:nl),angle(i),size(i),xjus(i),yjus(i))
			   call show_mouse()
			   endif
			   deallocate(array0)
			   goto 1
		   	else
			   call hide_mouse()
			   call coltyp18(ibk)
		         if(jdraw(i).eq.-1) then
			      call coltyp18(colors(i))
		         else
		            call coltyp18(ibk)
		         endif
		         sizen=csfac(i)*csize
		         call setcsize(sizen,size(i))
      	   	   call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &               SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
c			   if(imd.eq.0) then
c		         nl=nblank1(text(i))
c			   call justifystring(ixg(i),iyg(i),
c     &		   TEXT(i)(1:nl),angle(i),size(i),xjus(i),yjus(i))
c			   endif
			   deallocate(array0)
			   call show_mouse()
			   goto 99
		   	endif
		   else
			outtext=.true.
		   endif
	    	enddo
	    	do i=1,n        	! show all boxes if outside
		   m=m+1
		   temp(m)=i
		   call setfont1(font(i))
		   call coltyp18(colors(i))
		   sizen=csfac(i)*csize
		   call setcsize(sizen,size(i))
      	   call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &	   SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
	    	enddo
		call show_mouse()
	    	goto 1            ! end of ibutton.eq.1
	   else if(ibutton.eq.4) then   ! centre button pressed:
		first=1
		m=0
	    	do i=1,n
		   call setfont1(font(i))
		   sizen=csfac(i)*csize
		   call setcsize(sizen,size(i))
      	   call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &         SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
		   call pixtohg(ix,iy,ixh,iyh)
	   	   call inside(ixh,iyh,ixbox,iybox,flag)
	         if(flag.eq.1) then  ! if inside select box to move with keyboards
			m=m+1
			temp(m)=i
			call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
			idif=ixhi-ixlo
			if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idif
			endif
			idim=2*((iyhi-iylo+9)*(ixhi-ixlo+9)+1)
			ixg0(i)=ixg(i)
			iyg0(i)=iyg(i)
			allocate(array0(idim))
		  	call hide_mouse()
33	      	ans=getch(ktype)
	      	ival=ichar(ans)
	      	if(ktype.eq.8) then	!move menu with ARROW keys
		  	   if(ival.eq.77) then	!right
				call setfont1(font(i))
				if(first.eq.1) then
		   		   call coltyp18(ibk)
		   		   sizen=csfac(i)*csize
		   		   call setcsize(sizen,size(i))
      	   		   call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		         	   nl=nblank1(text(i))
				   if(imd.ne.0) then
			         call justifystring(ixg(i),iyg(i),
     &		         TEXT(i)(1:nl),angle(i),size(i),xjus(i),
     &			   yjus(i))
				   endif
				   first=0
				   goto 21
				endif
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
21		    		ixg(i)=ixg(i)+im
		   		sizen=csfac(i)*csize
		   		call setcsize(sizen,size(i))
      	   		call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
				call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
				idif=ixhi-ixlo
			      if(ixlo.lt.0) then
				   ixlo=0
				   ixhi=ixlo+idif
				endif
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
				call coltyp18(colors(I))
		   		sizen=csfac(i)*csize
		   		call setcsize(sizen,size(i))
      	   		call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		            nl=nblank1(text(i))
			   	if(imd.ne.0) then
			      call justifystring(ixg(i),iyg(i),
     &		      TEXT(i)(1:nl),angle(i),size(i),xjus(i),yjus(i))
				endif
		  	   endif
		  	   if(ival.eq.75) then		!shift left
				call setfont1(font(i))
				if(first.eq.1) then
		   		   call coltyp18(ibk)
		   		   sizen=csfac(i)*csize
		   		   call setcsize(sizen,size(i))
      	   		   call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		               nl=nblank1(text(i))
			   	   if(imd.ne.0) then
			         call justifystring(ixg(i),iyg(i),
     &		         TEXT(i)(1:nl),angle(i),size(i),xjus(i),
     &			   yjus(i))
				   endif
				   first=0
				   goto 22
				endif
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
22		    		ixg(i)=ixg(i)-im
		   		sizen=csfac(i)*csize
		   		call setcsize(sizen,size(i))
      	   		call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
				call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
				idif=ixhi-ixlo
			      if(ixlo.lt.0) then
				   ixlo=0
				   ixhi=ixlo+idif
			      endif
			      call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
				call coltyp18(COLORS(I))
		   		sizen=csfac(i)*csize
		   		call setcsize(sizen,size(i))
      	   		call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		            nl=nblank1(text(i))
			   	if(imd.ne.0) then
			      call justifystring(ixg(i),iyg(i),
     &		      TEXT(i)(1:nl),angle(i),size(i),xjus(i),yjus(i))
				endif
		  	   endif
		  	   if(ival.eq.72) then	!shift up
				call setfont1(font(i))
				if(first.eq.1) then
				   call coltyp18(ibk)
				   sizen=csfac(i)*csize
				   call setcsize(sizen,size(i))
				   call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
				   nl=nblank1(text(i))
				   if(imd.ne.0) then
					call justifystring(ixg(i),iyg(i),
     &		      	TEXT(i)(1:nl),angle(i),size(i),xjus(i),
     &			      yjus(i))
				   endif
				   first=0
				   goto 23
				endif
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
23		    		iyg(i)=iyg(i)+im
				sizen=csfac(i)*csize
				call setcsize(sizen,size(i))
				call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
				call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,
     &			iyhi)
				idif=ixhi-ixlo
				if(ixlo.lt.0) then
					ixlo=0
					ixhi=ixlo+idif
				endif
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
				call coltyp18(colors(I))
				sizen=csfac(i)*csize
				call setcsize(sizen,size(i))
				call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
				nl=nblank1(text(i))
				if(imd.ne.0) then
				call justifystring(ixg(i),iyg(i),
     &		      TEXT(i)(1:nl),angle(i),size(i),xjus(i),
     &			yjus(i))
				endif
		  	   endif
		  	   if(ival.eq.80) then	!shift down
				call setfont1(font(i))
				if(first.eq.1) then
				   call coltyp18(ibk)
				   sizen=csfac(i)*csize
				   call setcsize(sizen,size(i))
				   call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
				   nl=nblank1(text(i))
				   if(imd.ne.0) then
					call justifystring(ixg(i),iyg(i),
     &		      	TEXT(i)(1:nl),angle(i),size(i),xjus(i),
     &                        yjus(i))
				   endif
				   first=0
				   goto 24
			      endif
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
24		    		iyg(i)=iyg(i)-im
				sizen=csfac(i)*csize
				call setcsize(sizen,size(i))
				call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
				call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,
     &                  iyhi)
				idif=ixhi-ixlo
				if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idif
				endif
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
				call coltyp18(colors(I))
				sizen=csfac(i)*csize
				call setcsize(sizen,size(i))
				call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
				nl=nblank1(text(i))
				if(imd.ne.0) then
				call justifystring(ixg(i),iyg(i),
     &		      TEXT(i)(1:nl),angle(i),size(i),xjus(i),
     &                  yjus(i))
				endif
		  	   endif
		  	   if(ival.eq.73) then	!PgDn:rotate clockwise
				   call setfont1(font(i))
				   if(first.eq.1) then
		   		   	call coltyp18(ibk)
		   		      sizen=csfac(i)*csize
		   		      call setcsize(sizen,size(i))
      	   		   	call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                     	SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		            	nl=nblank1(text(i))
c			   	      if(imd.ne.0) then
			         	call justifystring(ixg(i),iyg(i),
     &		         	TEXT(i)(1:nl),angle(i),size(i),xjus(i),
     &                        yjus(i))
c				      endif
					first=0
					goto 25
				   endif
				   call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
25				   angle(i)=angle(i)+ia
		   		   sizen=csfac(i)*csize
		   		   call setcsize(sizen,size(i))
      	   		   call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
				   call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,
     &                     iyhi)
				   idif=ixhi-ixlo
				   if(ixlo.lt.0) then
				   	ixlo=0
				   	ixhi=ixlo+idif
			         endif
				   call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
				   call coltyp18(colors(I))
		   		   sizen=csfac(i)*csize
		   		   call setcsize(sizen,size(i))
      	   		   call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		               nl=nblank1(text(i))
c			   	   if(imd.ne.0) then
			         call justifystring(ixg(i),iyg(i),
     &		         TEXT(i)(1:nl),angle(i),size(i),xjus(i),
     &			   yjus(i))
c				   endif
		  	   endif
		  	   if(ival.eq.81) then	!PgUp:rotate anticlockwise
				   if(first.eq.1) then
		   		   	call coltyp18(ibk)
				      call setfont1(font(i))
		   		      sizen=csfac(i)*csize
		   		      call setcsize(sizen,size(i))
      	   		   	call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                     	SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		            	nl=nblank1(text(i))
c			   	      if(imd.ne.0) then
			         	call justifystring(ixg(i),iyg(i),
     &		         	TEXT(i)(1:nl),angle(i),size(i),xjus(i),
     &                        yjus(i))
c				      endif
					first=0
					goto 125
				   endif
				   call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
125				   angle(i)=angle(i)-ia
				   call setfont1(font(i))
		   		   sizen=csfac(i)*csize
		   		   call setcsize(sizen,size(i))
      	   		   call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
				   call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,
     &			   iyhi)
				   idif=ixhi-ixlo
				   if(ixlo.lt.0) then
				   	ixlo=0
				   	ixhi=ixlo+idif
			         endif
				   call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
				   call coltyp18(colors(I))
				   call setfont1(font(i))
		   		   sizen=csfac(i)*csize
		   		   call setcsize(sizen,size(i))
      	   		   call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		               nl=nblank1(text(i))
c			   	   if(imd.ne.0) then
			         call justifystring(ixg(i),iyg(i),
     &		         TEXT(i)(1:nl),angle(i),size(i),xjus(i),
     &                     yjus(i))
c				   endif
		  		endif
				if(ival.eq.79) then     ! End:increase color
				   if (colors(i).lt.15) colors(i)=colors(i)+1
				   call coltyp18(colors(i))
				   call setfont1(font(i))
		               nl=nblank1(text(i))
		   		   sizen=csfac(i)*csize
		   		   call setcsize(sizen,size(i))
			         call justifystring(ixg(i),iyg(i),
     &		         TEXT(i)(1:nl),angle(i),size(i),xjus(i),
     &                     yjus(i))
      	   		   call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
				endif
				if(ival.eq.71) then     ! Home:decrease color
				   if (colors(i).gt.0) colors(i)=colors(i)-1
				   call setfont1(font(i))
				   call coltyp18(colors(i))
		               nl=nblank1(text(i))
		   		   sizen=csfac(i)*csize
		   		   call setcsize(sizen,size(i))
			         call justifystring(ixg(i),iyg(i),
     &		         TEXT(i)(1:nl),angle(i),size(i),xjus(i),
     &                     yjus(i))
      	   		   call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
				endif
			      if(ival.eq.82) then     ! Ins:toggle sensitivity
				   if(im.eq.16) then
					im=100
					ia=45
					cs=1.
				   else
					im=16
					ia=5
					cs=0.2
				  endif
				endif
				if(ival.eq.83) then     ! DEL:toggle idraw
					if(jdraw(i).eq.0) then
			   		   jdraw(i)=1
					   call setfont1(font(i))
					   call coltyp18(colors(i))
					   nl=nblank1(text(i))
					   sizen=csfac(i)*csize
					   call setcsize(sizen,size(i))
					   call justifystring(ixg(i),iyg(i),
     &		               TEXT(i)(1:nl),angle(i),size(i),xjus(i),
     &                           yjus(i))
					else
			   		   jdraw(i)=0
					   call setfont1(font(i))
					   call coltyp18(ibk)
					   nl=nblank1(text(i))
					   sizen=csfac(i)*csize
					   call setcsize(sizen,size(i))
					   call justifystring(ixg(i),iyg(i),
     &		               TEXT(i)(1:nl),angle(i),size(i),xjus(i),
     &                           yjus(i))
					endif
				endif
		  		goto 33
			   else if(ktype.eq.3.or.ktype.eq.0) then
				if(ival.eq.43) then        ! +: increase size
	                     sizen=sizen+cs		!plus
	                     csfac(i)=sizen/csize
				else if(ival.eq.45) then   ! -: decrease size
		   		   sizen=csfac(i)*csize
	                     sizen=sizen-cs		!plus
	                     csfac(i)=sizen/csize
				endif
				if(first.eq.1) then
				   call setfont1(font(i))
		   		   call coltyp18(ibk)
c		   		   sizen=csfac(i)*csize
c		   		   call setcsize(sizen,size(i))
      	   		   call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		               nl=nblank1(text(i))
			         call justifystring(ixg(i),iyg(i),
     &		         TEXT(i)(1:nl),angle(i),size(i),xjus(i),
     &                     yjus(i))
				   first=0
				   goto 45
				endif
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
45	   		      continue
		   		sizen=csfac(i)*csize
				call SETCSIZE(sizen,size(i))		!set isize
				call setfont1(font(i))
      	   		call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
				call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
			      idif=ixhi-ixlo
			      if(ixlo.lt.0) then
				   ixlo=0
				   ixhi=ixlo+idif
			      endif
				deallocate(array0)
				idim=2*((iyhi-iylo+9)*(ixhi-ixlo+9)+1)
				allocate(array0(idim))
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
				call coltyp18(colors(I))
		   		sizen=csfac(i)*csize
		   		call setcsize(sizen,size(i))
			      ih=charheight(size(i))
			      iw=charwidth(size(i))
      	   		call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
				call setfont1(font(i))
		            nl=nblank1(text(i))
			      call justifystring(ixg(i),iyg(i),
     &		      TEXT(i)(1:nl),angle(i),size(i),xjus(i),yjus(i))
				goto 33
			   else if(ktype.eq.2) then
				if(ival.eq.98) then     ! b:toggle  box draw
					if(jdraw(i).eq.-1) then
			   		   jdraw(i)=1
					   call coltyp18(ibk)
		   			   sizen=csfac(i)*csize
		   		         call setcsize(sizen,size(i))
      	   		         call FBOX1(IXg(i),IYg(i),TEXT(i),
     &				   ANGLE(i),
     &                           SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
					else if(jdraw(i).eq.1) then
			   		   jdraw(i)=-1
					   call coltyp18(colors(i))
		   			   sizen=csfac(i)*csize
		   		         call setcsize(sizen,size(i))
      	   		         call FBOX1(IXg(i),IYg(i),TEXT(i),
     &				   ANGLE(i),
     &                           SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
					endif
				      goto 33
				endif
				if(ival.eq.42) then           ! increase font
				   ifont=font(i)+1
				else if(ival.eq.47) then      ! decrease font
				   if(ifont.gt.0) ifont=font(i)-1
				endif
				if(first.eq.1) then
		   		   call coltyp18(ibk)
				   call setfont1(font(i))
		   		   sizen=csfac(i)*csize
		   		   call setcsize(sizen,size(i))
      	   		   call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		               nl=nblank1(text(i))
			         call justifystring(ixg(i),iyg(i),
     &		         TEXT(i)(1:nl),angle(i),size(i),xjus(i),
     &                     yjus(i))
				   first=0
				   goto 55
				endif
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
55				font(i)=ifont
				call setfont1(font(i))
		   		sizen=csfac(i)*csize
		   		call setcsize(sizen,size(i))
      	   		call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
				call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
				idif=ixhi-ixlo
				if(ixlo.lt.0) then
				   ixlo=0
				   ixhi=ixlo+idif
				endif
				deallocate(array0)
				idim=2*((iyhi-iylo+9)*(ixhi-ixlo+9)+1)
				allocate(array0(idim))
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
				call coltyp18(colors(i))
		   		sizen=csfac(i)*csize
		   		call setcsize(sizen,size(i))
      	   		call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		            nl=nblank1(text(i))
			      call justifystring(ixg(i),iyg(i),
     &		      TEXT(i)(1:nl),angle(i),size(i),xjus(i),yjus(i))
				goto 33
			   else if(ival.eq.13)then       ! set option
				if(jdraw(i).ne.-1) then
				call coltyp18(ibk)
		   		sizen=csfac(i)*csize
		   		call setcsize(sizen,size(i))
      	   	   	call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &               	SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
				endif
			   	if(imd.eq.0) then
				call coltyp18(ibk)
		            nl=nblank1(text(i))
			   	call justifystring(ixg0(i),iyg0(i),
     &		   	TEXT(i)(1:nl),angle(i),size(i),xjus(i),yjus(i))
			   	call coltyp18(colors(i))
			      call justifystring(ixg(i),iyg(i),
     &		      TEXT(i)(1:nl),angle(i),size(i),xjus(i),yjus(i))
				endif
        	  		call show_mouse()
				deallocate(array0)
				goto 1
	      	   else		!any other key to exit
	  	  		call hide_mouse()
				call coltyp18(ibk)
		   		sizen=csfac(i)*csize
		   		call setcsize(sizen,size(i))
      	   	   	call FBOX1(IXg(i),IYg(i),TEXT(i),ANGLE(i),
     &               	SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
        	  		call show_mouse()
				deallocate(array0)
	  	  		goto 99
	      	endif 		! end getch()
		   endif 			! end if(flag.eq.1)
		enddo				! end if(ibutton.eq.4)
	   else if(ibutton.eq.2) then ! right button pressed
	  	call hide_mouse()
	      call coltyp18(ibk)
		do k=1,m
		   sizen=csfac(temp(k))*csize
		   call setcsize(sizen,size(temp(k)))
      	   call FBOX1(IXg(temp(k)),IYg(temp(k)),
     &	   TEXT(temp(k)),ANGLE(temp(k)),
     &         SIZE(temp(k)),XJUS(temp(k)),YJUS(temp(k)),
     &	   ixbox,iybox,1)
c	         if(imd.eq.0) then
c		   nl=nblank1(text(i))
c	         call justifystring(ixg(i),iyg(i),
c     &	   TEXT(i)(1:nl),angle(i),size(i),xjus(i),yjus(i))
c		   endif
            enddo
        	call show_mouse()
	    	goto 99     		! end of ibutton.eq.2
	   endif                      ! end mouse buttons selection
	endif                         ! end if (mouse_set)
99	continue
	call hide_mouse()
	deallocate(size,ixg0,iyg0)
	RETURN
	end
