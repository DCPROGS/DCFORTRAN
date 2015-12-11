      subroutine ginotext(parval,xtitle,ytitle,title1,cnumx,cnumy,
     &  cexpx,cexpy,newtext,numbx,numby,ntext,inumx,inumy,logx,logy,
     &  rx,ry,ijus,icol,rotate,ifnt,idraw,csize,ibk,imd,lth,
     &  nrealt,nrealx,nrealy,nreal)

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

	real    	   RX(100),RY(100),rotate(100),csize(30),lth(100)
	integer 	   ifnt(30),icol(100),IJUS(100),idraw(100)
      real           xg(:),yg(:),angle(:),size(:),thick(:)
      real		   xg0(:),yg0(:),angle0(:)
	integer 	   font(:),colors(:),JUS(:),jdraw(:)
	real   	   xbox(4),ybox(4),size0(:)
	integer 	   font0(:),temp(100),first,flag
	integer*1	   array0(:)
	character*1    ans,getch
	character*10   cnumx(25),cnumy(25),cexpx(25),cexpy(25)
	character*11   cnum
	character*64   TITLE1
	character*75   xtitle,ytitle	!output from LAXES
	character*80   newtext(20),nwtext		!extra text
	character*150  str(:)		!extra text
	character*200  parval		!parameter values
	logical        mouse_on,kbhit,outtext,mouse_set,logx,logy,ivl
	logical isnum
	common/mousval/mouse_on,nbutton

	allocatable :: array0,xg0,yg0,angle0,size0,font0,thick
	allocatable :: angle,colors,font,xg,yg,str,jdraw,jus,size

c============================================================


	ivl=isnum()
	if(ivl) call NUMCLR
	n=4+numby+numbx+ntext
c	n=5+numby+numbx+ntext
	allocate(angle(n+1),colors(n+1),font(n+1),xg(n+1),yg(n+1))
      allocate(str(n+1),jdraw(n+1),jus(n+1),thick(n+1),size(n+1))
	allocate(xg0(n+1),yg0(n+1),size0(n+1),angle0(n+1),font0(n+1))
	do i=1,n
	str(i)(1:50)='                                                  '
	str(i)(51:100)='
     &'
	str(i)(101:150)='
     & '
	enddo

	ic0=icol(24)
	icl=icol(23)
	ift0=ifnt(7)
	csf0=csize(7)

	nl=nblank1(parval)
	jdraw(1)=idraw(6)
	if (nl.eq.1) then
		jdraw(1)=0
	endif
	str(1)=parval(1:nl)
	colors(1)=icol(26)
	thick(1)=lth(26)
	font(1)= ifnt(6)
	angle(1)=rotate(6)
	size(1)=csize(6)
c=================================rx graf;xg screen
	call graspa(rx(6),ry(6),xg(1),yg(1))
c	xg(1)=rx(6)
c	yg(1)=ry(6)
	jus(1)=ijus(6)

	nl=nblank1(xtitle)
	jdraw(2)=idraw(8)
	if (nl.eq.1) jdraw(2)=0
	str(2)=xtitle(1:nl)
	colors(2)=icol(23)
	thick(2)=lth(23)
	font(2)=ifnt(8)
	angle(2)=rotate(8)
	size(2)=csize(8)
	call graspa(rx(8),ry(8),xg(2),yg(2))
c	xg(2)=rx(8)
c	yg(2)=ry(8)
	jus(2)=ijus(8)

	nl=nblank1(ytitle)
	jdraw(3)=idraw(9)
	if (nl.eq.1) jdraw(3)=0
	str(3)=ytitle(1:nl)
	colors(3)=icol(23)
	thick(3)=lth(23)
	font(3)=ifnt(9)
	angle(3)=rotate(9)
	size(3)=csize(9)
	call graspa(rx(9),ry(9),xg(3),yg(3))
c	xg(3)=rx(9)
c	yg(3)=ry(9)
	jus(3)=ijus(9)

	nl=nblank1(title1)
	jdraw(4)=idraw(10)
	if (nl.eq.1) jdraw(4)=0
	str(4)=title1(1:nl)
	colors(4)=icol(25)
	thick(4)=lth(25)
	font(4)=ifnt(10)
	angle(4)=rotate(10)
	size(4)=csize(10)
	call graspa(rx(10),ry(10),xg(4),yg(4))
c	xg(4)=rx(10)
c	yg(4)=ry(10)
	jus(4)=ijus(10)

	do k=1,numbx
	   nl=nblank1(cnumx(k))
	   str(k+4)= cnumx(k)(1:nl)
c=============================================================
c!	to be fixed:
c	the best option is to use JUSTSTR.FOR to draw numbers:this imply to
c	change DRAWNUM  all over in the program;
c     for the moment I draw with IDRAWNUM  in FIXNUMB2) witch draws in the
c	same way as DRAWNUM  , but for F90 version please have a look at JUSTSTR.!
c	because in TEXTMOVE the number+exponent has to be treated as one string!
c	and DRAWNUM  writes 2 strings which can not be concatenated !!!
c     JUSTSTR concatenates cnum and cexp and gives you a resulting string!
c===========================================================================
	   if(logx) then
c	      str=cnum(1:2)//char(42)//char(69)//cexp(1:nl)
		call SETSIZE(csize(7))	!this defines ISIZE
		call CHAEXI(0.8,0.8,0.6,0.3)
		nl1=NBLANK1(cexpx(k))
		nl=nblank1(cnumx(k))
		str(k+4)=cnumx(k)(1:nl)//char(42)//char(69)
     &      //cexpx(k)(1:nl1)
	   endif
	   colors(k+4)=icol(24)
	   thick(k+4)=lth(24)
	   font(k+4)=ifnt(7)
	   size(k+4)=csize(7)
	   call graspa(rx(k+30),ry(k+30),xg(k+4),yg(k+4))
c	   xg(k+4)=rx(k+30)
c	   yg(k+4)=ry(k+30)
	   angle(k+4)=rotate(k+30)
	   jus(k+4)=ijus(k+30)
	   jdraw(k+4)=idraw(k+30)
	enddo
	do k=1,numby
	   nl=nblank1(cnumy(k))
	   str(k+4+numbx)= cnumy(k)(1:nl)
c!	to be fixed:
	   if(logy) then
		call SETSIZE(csize(7))	!this defines ISIZE
		call CHAEXI(0.8,0.8,0.6,0.3)
	      nl1=NBLANK1(cexpy(k))
		nl=nblank1(cnumy(k))
	      str(k+4+numbx)=cnumy(k)(1:nl)//char(42)//char(69)
     &      //cexpy(k)(1:nl1)
	   endif
	   colors(k+4+numbx)=icol(24)
	   thick(k+4+numbx)=lth(24)
	   font(k+4+numbx)=ifnt(7)
	   size(k+4+numbx)=csize(7)
	   call graspa(rx(k+55),ry(k+55),xg(k+4+numbx),yg(k+4+numbx))
c	   xg(k+4+numbx)=rx(k+55)
c	   yg(k+4+numbx)=ry(k+55)
	   angle(k+4+numbx)=rotate(k+55)
	   jus(k+4+numbx)=ijus(k+55)
	   jdraw(k+4+numbx)=idraw(k+55)
	enddo
	if(ntext.gt.0) then
	   do k=1,ntext
	      nwtext=newtext(k)
	      nt=nblank1(nwtext)
	      str(k+4+numbx+numby)= nwtext(1:nt)
c	      str(k+4+numbx+numby)= nwtext(1:nreal(k))
	      colors(k+4+numbx+numby)=icol(k+30)
	      thick(k+4+numbx+numby)=lth(k+30)
	      font(k+4+numbx+numby)=ifnt(k+10)
	      size(k+4+numbx+numby)=csize(k+10)
	      call graspa(rx(k+10),ry(k+10),xg(k+4+numbx+numby),
     &      yg(k+4+numbx+numby))
c	      xg(k+4+numbx+numbx)=rx(k+10)
c	      yg(k+4+numbx+numby)=ry(k+10)
	      angle(k+4+numbx+numby)=rotate(k+10)
	      jus(k+4+numbx+numby)=ijus(k+10)
	      jdraw(k+4+numbx+numby)=idraw(k+10)
	   enddo
	endif

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
c		ixloh=320    !position for help box
c		iyhih=320
c		call poptext(ixloh,-1,iyhih,helpst,nhelp,15,2,icq)	!=popkeys
	      call VHELP(10)
		ibutton=0
		outtext=.true.
		mouse_set=.false.
		call show_mouse()
		goto 1
	   else if(ktype.eq.0.and.ival.eq.60) then	!F2=help
	      call vhelp(-2)
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
		   call setfnt(font(temp(k)))
		   if( jdraw(temp(k)).eq.-1) then
			call lincols(colors(temp(k)),idev)
		   else
		      call lincols(ibk,idev)
		   endif
      	   call FBOX1(xg(temp(k)),yg(temp(k)),str(temp(k)),
     &	   ANGLE(temp(k)),SIZE(temp(k)),JUS(temp(k)),xbox,ybox,1)  !screen-screen
	    	enddo
	    	m=0
10	    	do i=1,n
		   call setfnt(font(i))
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
		   	call lincols(colors(temp(k)),idev)    !!!!
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
		   	call lincols(colors(i),idev)    !!!!
      	      call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &		SIZE(i),JUS(i),xbox,ybox,1)
			call show_mouse()
			call calcdim(xbox,ybox,4,ixlo,iylo,ixhi,iyhi) !screen to 18
			idifx=ixhi-ixlo
			idify=iyhi-iylo
			if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idifx
			endif
			idim=4*((iyhi-iylo+9)*(ixhi-ixlo+9)+1)
c			if(idifx.lt.idify) then
c			    idim=idify*idify/2+1
c			else
c			    idim=idifx*idifx/2+1
c			endif
			allocate(array0(idim))
			xg0(i)=xg(i)
			yg0(i)=yg(i)
2		    	call mouse_update(ibutton,ix,iy)
	  	    	if(ibutton.eq.1) then          ! move string if inside
			   if(ix.eq.ix0.and.iy.eq.iy0) goto 2
			   call hide_mouse()
			   if(first.eq.1) then
			   	call lincols(ibk,idev)
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
			   idifx=ixhi-ixlo
			   if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idifx
			   endif
			   call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
		   	   call lincols(colors(i),idev)
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
			call setfnt(font(i))
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
			idifx=ixhi-ixlo
			idify=iyhi-iylo
			if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idifx
			endif
			idim=4*((iyhi-iylo+9)*(ixhi-ixlo+9)+1)
c			if(idifx.lt.idify) then
c			    idim=idify*idify/2+1
c			else
c			    idim=idifx*idifx/2+1
c			endif
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
				idifx=ixhi-ixlo
			      if(ixlo.lt.0) then
				   ixlo=0
				   ixhi=ixlo+idifx
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
				idifx=ixhi-ixlo
			      if(ixlo.lt.0) then
				   ixlo=0
				   ixhi=ixlo+idifx
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
				idifx=ixhi-ixlo
				if(ixlo.lt.0) then
					ixlo=0
					ixhi=ixlo+idifx
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
				idifx=ixhi-ixlo
				if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idifx
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
		  	   if(ival.eq.73) then	!PgUp:rotate anticlockwise
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
				   idifx=ixhi-ixlo
				   if(ixlo.lt.0) then
				   	ixlo=0
				   	ixhi=ixlo+idifx
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
		  	   if(ival.eq.81) then	!PgDown:rotate clockwise
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
				   idifx=ixhi-ixlo
				   if(ixlo.lt.0) then
				   	ixlo=0
				   	ixhi=ixlo+idifx
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
			      idifx=ixhi-ixlo
			      if(ixlo.lt.0) then
				   ixlo=0
				   ixhi=ixlo+idifx
			      endif
				deallocate(array0)
				idim=4*((iyhi-iylo+9)*(ixhi-ixlo+9)+1)
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
				idifx=ixhi-ixlo
				if(ixlo.lt.0) then
				   ixlo=0
				   ixhi=ixlo+idifx
				endif
				deallocate(array0)
				idim=4*((iyhi-iylo+9)*(ixhi-ixlo+9)+1)
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
