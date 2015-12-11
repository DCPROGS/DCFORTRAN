c*************************************************************************
      subroutine fixtext2(parval,xtitle,ytitle,title1,cnumx,cnumy,
     &  cexpx,cexpy,newtext,numbx,numby,ntext,inumx,inumy,logx,logy,
     &  csfac,rx,ry,ixjus,iyjus,icol,iangle,ifnt,idraw,csize,ibk,imd)

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

	real 	  	csfac(30),csfac1(:)
	real    	RX(100),RY(100)
	integer*4	size(:),ixbox(4),iybox(4)
	integer*4	temp(100)
	integer*4	jdraw(:),ixg0(:),iyg0(:),angle0(:),size0(:)
	integer 	IXJUS(100),IYJUS(100),idraw(100)
	integer 	XJUS(:),YJUS(:),font0(:)
	integer 	iangle(100),icol(100)
	integer 	angle(:),colors(:),font(:)
	integer 	ifnt(30),ixg(:),iyg(:)
	integer*1	array0(:)
	character*1 ans,getch
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25)
	character*11 cnum
	character*44 TITLE1
	character*55 helpst(30)		!help strings
	character*75 xtitle,ytitle	!output from LAXES
	character*80 newtext(20)		!extra text
	character*150 str(:),parval		!extra text
	logical mouse_on,kbhit,outtext,mouse_set,logx,logy
	common/mousval/mouse_on,nbutton

	allocatable :: array0,size,ixg0,iyg0,jdraw,csfac1,xjus,yjus
	allocatable :: angle,colors,font,ixg,iyg,str,angle0,size0,font0

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



	n=4+numby+numbx+ntext
	allocate(angle(n),colors(n),font(n),ixg(n),iyg(n))
      allocate(str(n),jdraw(n),csfac1(n),xjus(n),yjus(n))
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
	csf0=csfac(7)

	nl=nblank1(parval)
	str(1)=parval(1:nl)
	colors(1)=icol(26)
	font(1)= ifnt(6)
	angle(1)=iangle(6)
	csfac1(1)=csfac(6)
	call graspa(rx(6),ry(6),ixg(1),iyg(1))
	ixg(1)=iscrx(rx(6))
	iyg(1)=iscry(ry(6))
	xjus(1)=ixjus(6)
	yjus(1)=iyjus(6)
	jdraw(1)=idraw(6)
	nl=nblank1(xtitle)
	str(2)=xtitle(1:nl)
	colors(2)=icol(23)
	font(2)=ifnt(8)
	angle(2)=iangle(8)
	csfac1(2)=csfac(8)
	call graspa(rx(8),ry(8),ixg(2),iyg(2))
	xjus(2)=ixjus(8)
	yjus(2)=iyjus(8)
	jdraw(2)=idraw(8)

	nl=nblank1(ytitle)
	str(3)=ytitle(1:nl)
	colors(3)=icol(23)
	font(3)=ifnt(9)
	angle(3)=iangle(9)
	csfac1(3)=csfac(9)
	call graspa(rx(9),ry(9),ixg(3),iyg(3))
	xjus(3)=ixjus(9)
	yjus(3)=iyjus(9)
	jdraw(3)=idraw(9)

	nl=nblank1(title1)
	str(4)=title1(1:nl)
	colors(4)=icol(25)
	font(4)=ifnt(10)
	angle(4)=iangle(10)
	csfac1(4)=csfac(10)
	call graspa(rx(10),ry(10),ixg(4),iyg(4))
	xjus(4)=ixjus(10)
	yjus(4)=iyjus(10)
	jdraw(4)=idraw(10)

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
	   if(logx.and.inumx.eq.1) then
	 	sizen=csfac(7)*csize
		call SETCSIZE(0.8*sizen,indsize)	!this defines ISIZE
		nl1=NBLANK1(cexpx)
		call INTCONV(indsize,cnum)
		nm=nblank1(cnum)
		str(k+4)=cnumx(k)(1:nl)//char(21)//char(19)
     &      //cnum(1:nm)//cexpx(k)(1:nl1)
c     &      //getint(indsize,nm)(1:nm)//cexpx(k)(1:nl1)
		call SETCSIZE(sizen,indsize)	!reset
	   endif
	   colors(k+4)=icol(24)
	   font(k+4)=ifnt(7)
	   csfac1(k+4)=csfac(7)
	   ixg(k+4)=iscrx(rx(k+30))
	   iyg(k+4)=iscry(ry(k+30))
	   angle(k+4)=iangle(k+30)
	   xjus(k+4)=ixjus(k+30)
	   yjus(k+4)=iyjus(k+30)
	   jdraw(k+4)=idraw(k+30)
	enddo
	do k=1,numby
	   nl=nblank1(cnumy(k))
	   str(k+4+numbx)= cnumy(k)(1:nl)
c!	to be fixed:
	   if(logy.and.inumy.eq.1) then
	 	sizen=csfac(7)*csize
	      call SETCSIZE(0.8*sizen,indsize)	!this defines ISIZE
	      nl1=NBLANK1(cexpy)
		call INTCONV(indsize,cnum)
		nm=nblank1(cnum)
	      str(k+4+numbx)=cnumy(k)(1:nl)//char(21)//char(19)
     &      //cnum(1:nm)//cexpy(k)(1:nl1)
	      call SETCSIZE(sizen,indsize)	!reset
	   endif
	   colors(k+4+numbx)=icol(24)
	   font(k+4+numbx)=ifnt(7)
	   csfac1(k+4+numbx)=csfac(7)
	   ixg(k+4+numbx)=iscrx(rx(k+55))
	   iyg(k+4+numbx)=iscry(ry(k+55))
	   angle(k+4+numbx)=iangle(k+55)
	   xjus(k+4+numbx)=ixjus(k+55)
	   yjus(k+4+numbx)=iyjus(k+55)
	   jdraw(k+4+numbx)=idraw(k+55)
	enddo
	if(ntext.gt.0) then
	   do k=1,ntext
	   str(k+4+numbx+numby)= newtext(k)
	   colors(k+4+numbx+numby)=icol(k+10)
	   font(k+4+numbx+numby)=ifnt(k+10)
	   csfac1(k+4+numbx+numby)=csfac(k+10)
	   ixg(k+4+numbx+numby)=iscrx(rx(k+10))
	   iyg(k+4+numbx+numby)=iscry(ry(k+10))
	   angle(k+4+numbx+numby)=iangle(k+10)
	   xjus(k+4+numbx+numby)=ixjus(k+10)
	   yjus(k+4+numbx+numby)=iyjus(k+10)
	   jdraw(k+4+numbx+numby)=idraw(k+10)
	   enddo
	endif



	allocate(size(n),ixg0(n),iyg0(n),size0(n),angle0(n),font0(n))
	nhelp=21
	icq=14     		! color help menu
	im=100            ! movement
	ia=45             ! angle
	cs=1.
	do i=1,n
		sizen=csfac1(i)*csize
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
		   sizen=csfac1(temp(k))*csize
		   call setcsize(sizen,size(temp(k)))
		   if( jdraw(temp(k)).eq.-1) then
			call coltyp18(colors(temp(k)))
		   else
		      call coltyp18(ibk)
		   endif
      	   call FBOX1(IXg(temp(k)),IYg(temp(k)),str(temp(k)),
     &	   ANGLE(temp(k)),SIZE(temp(k)),
     &	   XJUS(temp(k))
     &	   ,YJUS(temp(k)),ixbox,iybox,1)  !screen-screen
	    	enddo
	    	m=0
10	    	do i=1,n
		   sizen=csfac1(i)*csize
		   call setcsize(sizen,size(i))
		   call setfont1(font(i))
      	   call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),SIZE(i),
     &	   XJUS(i),YJUS(i),ixbox,iybox,0)
		   call pixtohg(ix,iy,ixh,iyh) -> call pixpos(ix,iy,ixh,iyh)
	   	   call inside(ixh,iyh,ixbox,iybox,flag)  screen-screen
	         if(flag.eq.1) then
		    	m=m+1
		    	temp(m)=i
		   endif
	    	enddo
	    	if(m.gt.0) then       		! show box(es) if inside
		   do k=1,m
		      sizen=csfac1(temp(k))*csize
		      call setcsize(sizen,size(temp(k)))
		   	call coltyp18(colors(temp(k)))    !!!!
			call setfont1(font(temp(k)))
      	    	call FBOX1(IXg(temp(k)),IYg(temp(k)),
     &		str(temp(k)),ANGLE(temp(k)),SIZE(temp(k)),
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
		   sizen=csfac1(i)*csize
		   call setcsize(sizen,size(i))
c		   nl=nblank1(str(i))
      	   call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &	   SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
		   call pixtohg(ix,iy,ixh,iyh)
	   	   call inside(ixh,iyh,ixbox,iybox,flag)
	         if(flag.eq.1) then
			first=1
			m=m+1
			temp(m)=i
			call setfont1(font(i))
		   	call coltyp18(colors(i))    !!!!
c		   nl=nblank1(str(i))
		      sizen=csfac1(i)*csize
		      call setcsize(sizen,size(i))
      	      call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &		SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			call show_mouse()
			call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)  screen-18
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
		            sizen=csfac1(i)*csize
		            call setcsize(sizen,size(i))
      	         	call FBOX1(IXg(i),IYg(i),str(i),
     &		   	ANGLE(i),SIZE(i),XJUS(i),
     &	         	YJUS(i),ixbox,iybox,1)
		            nl=nblank1(str(i))
				if(imd.ne.0) then
			   	call justifystring(ixg(i),iyg(i),
     &		   	str(i)(1:nl),angle(i),size(i),ijus(i))
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
		         sizen=csfac1(i)*csize
		         call setcsize(sizen,size(i))
      	   	   call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &               SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
			   call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
			   idif=ixhi-ixlo
			   if(ixlo.lt.0) then
				ixlo=0
				ixhi=ixlo+idif
			   endif
			   call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
		   	   call coltyp18(colors(i))
		         sizen=csfac1(i)*csize
		         call setcsize(sizen,size(i))
      	   	   call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &               SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		         nl=nblank1(str(i))
			   if(imd.ne.0) then
			   call justifystring(ixg(i),iyg(i),
     &		   str(i)(1:nl),angle(i),size(i),ijus(i))
			   endif
			   ix0=ix
			   iy0=iy
			   call show_mouse()
			   goto 2
		    	else if (ibutton.eq.0) then
			   if(imd.eq.0) then
			   call hide_mouse()
		         nl=nblank1(str(i))
			   call coltyp18(ibk)
			   call justifystring(ixg0(i),iyg0(i),
     &		   str(i)(1:nl),angle(i),size(i),ijus(i))
			   call coltyp18(colors(i))
			   call justifystring(ixg(i),iyg(i),
     &		   str(i)(1:nl),angle(i),size(i),ijus(i))
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
		         sizen=csfac1(i)*csize
		         call setcsize(sizen,size(i))
      	   	   call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &               SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
c			   if(imd.eq.0) then
c		         nl=nblank1(str(i))
c			   call justifystring(ixg(i),iyg(i),
c     &		   str(i)(1:nl),angle(i),size(i),ijus(i))
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
		   sizen=csfac1(i)*csize
		   call setcsize(sizen,size(i))
      	   call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &	   SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
	    	enddo
		call show_mouse()
	    	goto 1            ! end of ibutton.eq.1
	   else if(ibutton.eq.4) then   ! centre button pressed:
		first=1
		m=0
	    	do i=1,n
		   call setfont1(font(i))
		   sizen=csfac1(i)*csize
		   call setcsize(sizen,size(i))
      	   call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
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
			sizen0=sizen
			ixg0(i)=ixg(i)
			iyg0(i)=iyg(i)
			angle0(i)=angle(i)
			size0(i)=size(i)
			font0(i)=font(i)
			allocate(array0(idim))
		  	call hide_mouse()
33	      	ans=getch(ktype)
	      	ival=ichar(ans)
	      	if(ktype.eq.8) then	!move menu with ARROW keys
		  	   if(ival.eq.77) then	!right
				call setfont1(font(i))
				if(first.eq.1) then
		   		   call coltyp18(ibk)
		   		   sizen=csfac1(i)*csize
		   		   call setcsize(sizen,size(i))
      	   		   call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		         	   nl=nblank1(str(i))
				   if(imd.ne.0) then
			         call justifystring(ixg(i),iyg(i),
     &		         str(i)(1:nl),angle(i),size(i),
     &			   ijus(i))
				   endif
				   first=0
				   goto 21
				endif
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
21		    		ixg(i)=ixg(i)+im
		   		sizen=csfac1(i)*csize
		   		call setcsize(sizen,size(i))
      	   		call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
				call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
				idif=ixhi-ixlo
			      if(ixlo.lt.0) then
				   ixlo=0
				   ixhi=ixlo+idif
				endif
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
				call coltyp18(colors(I))
		   		sizen=csfac1(i)*csize
		   		call setcsize(sizen,size(i))
      	   		call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		            nl=nblank1(str(i))
			   	if(imd.ne.0) then
			      call justifystring(ixg(i),iyg(i),
     &		      str(i)(1:nl),angle(i),size(i),ijus(i))
				endif
		  	   endif
		  	   if(ival.eq.75) then		!shift left
				call setfont1(font(i))
				if(first.eq.1) then
		   		   call coltyp18(ibk)
		   		   sizen=csfac1(i)*csize
		   		   call setcsize(sizen,size(i))
      	   		   call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		               nl=nblank1(str(i))
			   	   if(imd.ne.0) then
			         call justifystring(ixg(i),iyg(i),
     &		         str(i)(1:nl),angle(i),size(i),
     &			   ijus(i))
				   endif
				   first=0
				   goto 22
				endif
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
22		    		ixg(i)=ixg(i)-im
		   		sizen=csfac1(i)*csize
		   		call setcsize(sizen,size(i))
      	   		call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,0)
				call calcdim(ixbox,iybox,4,ixlo,iylo,ixhi,iyhi)
				idif=ixhi-ixlo
			      if(ixlo.lt.0) then
				   ixlo=0
				   ixhi=ixlo+idif
			      endif
			      call rdgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
				call coltyp18(COLORS(I))
		   		sizen=csfac1(i)*csize
		   		call setcsize(sizen,size(i))
      	   		call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		            nl=nblank1(str(i))
			   	if(imd.ne.0) then
			      call justifystring(ixg(i),iyg(i),
     &		      str(i)(1:nl),angle(i),size(i),ijus(i))
				endif
		  	   endif
		  	   if(ival.eq.72) then	!shift up
				call setfont1(font(i))
				if(first.eq.1) then
				   call coltyp18(ibk)
				   sizen=csfac1(i)*csize
				   call setcsize(sizen,size(i))
				   call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
				   nl=nblank1(str(i))
				   if(imd.ne.0) then
					call justifystring(ixg(i),iyg(i),
     &		      	str(i)(1:nl),angle(i),size(i),
     &			      ijus(i))
				   endif
				   first=0
				   goto 23
				endif
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
23		    		iyg(i)=iyg(i)+im
				sizen=csfac1(i)*csize
				call setcsize(sizen,size(i))
				call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
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
				sizen=csfac1(i)*csize
				call setcsize(sizen,size(i))
				call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
				nl=nblank1(str(i))
				if(imd.ne.0) then
				call justifystring(ixg(i),iyg(i),
     &		      str(i)(1:nl),angle(i),size(i),
     &			ijus(i))
				endif
		  	   endif
		  	   if(ival.eq.80) then	!shift down
				call setfont1(font(i))
				if(first.eq.1) then
				   call coltyp18(ibk)
				   sizen=csfac1(i)*csize
				   call setcsize(sizen,size(i))
				   call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
				   nl=nblank1(str(i))
				   if(imd.ne.0) then
					call justifystring(ixg(i),iyg(i),
     &		      	str(i)(1:nl),angle(i),size(i),
     &                        ijus(i))
				   endif
				   first=0
				   goto 24
			      endif
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
24		    		iyg(i)=iyg(i)-im
				sizen=csfac1(i)*csize
				call setcsize(sizen,size(i))
				call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
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
				sizen=csfac1(i)*csize
				call setcsize(sizen,size(i))
				call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
				nl=nblank1(str(i))
				if(imd.ne.0) then
				call justifystring(ixg(i),iyg(i),
     &		      str(i)(1:nl),angle(i),size(i),
     &                  ijus(i))
				endif
		  	   endif
		  	   if(ival.eq.73) then	!PgDn:rotate clockwise
				   call setfont1(font(i))
				   if(first.eq.1) then
		   		   	call coltyp18(ibk)
		   		      sizen=csfac1(i)*csize
		   		      call setcsize(sizen,size(i))
      	   		   	call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &                     	SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		            	nl=nblank1(str(i))
c			   	      if(imd.ne.0) then
			         	call justifystring(ixg(i),iyg(i),
     &		         	str(i)(1:nl),angle(i),size(i),
     &                        ijus(i))
c				      endif
					first=0
					goto 25
				   endif
				   call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
25				   continue
c			   	   if(imd.eq.0) then
c				     call coltyp18(ibk)
c		                 nl=nblank1(str(i))
c			   	     call justifystring(ixg0(i),iyg0(i),
c     &		   	     str(i)(1:nl),angle0(i),size0(i),
c     &                       ijus(i))
c				   endif
				   angle(i)=angle(i)+ia
		   		   sizen=csfac1(i)*csize
		   		   call setcsize(sizen,size(i))
      	   		   call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
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
		   		   sizen=csfac1(i)*csize
		   		   call setcsize(sizen,size(i))
      	   		   call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		               nl=nblank1(str(i))
c				   call set_cursor(1,1)
c				   print*,imd
			   	   if(imd.ne.0) then
			         call justifystring(ixg(i),iyg(i),
     &		         str(i)(1:nl),angle(i),size(i),
     &			   ijus(i))
				   endif
		  	   endif
		  	   if(ival.eq.81) then	!PgUp:rotate anticlockwise
				   if(first.eq.1) then
		   		   	call coltyp18(ibk)
				      call setfont1(font(i))
		   		      sizen=csfac1(i)*csize
		   		      call setcsize(sizen,size(i))
      	   		   	call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &                     	SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		            	nl=nblank1(str(i))
c			   	      if(imd.ne.0) then
			         	call justifystring(ixg(i),iyg(i),
     &		         	str(i)(1:nl),angle(i),size(i),
     &                        ijus(i))
c				      endif
					first=0
					goto 125
				   endif
				   call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
125				   continue
c			   	   if(imd.eq.0) then
c				     call coltyp18(ibk)
c		                 nl=nblank1(str(i))
c			   	     call justifystring(ixg0(i),iyg0(i),
c     &		   	     str(i)(1:nl),angle0(i),size0(i),ijus(i))
c				   endif
				   angle(i)=angle(i)-ia
				   call setfont1(font(i))
		   		   sizen=csfac1(i)*csize
		   		   call setcsize(sizen,size(i))
      	   		   call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
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
		   		   sizen=csfac1(i)*csize
		   		   call setcsize(sizen,size(i))
      	   		   call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		               nl=nblank1(str(i))
			   	   if(imd.ne.0) then
			         call justifystring(ixg(i),iyg(i),
     &		         str(i)(1:nl),angle(i),size(i),
     &                     ijus(i))
				   endif
		  		endif
				if(ival.eq.79) then     ! End:increase color
				   if (colors(i).lt.15) colors(i)=colors(i)+1
			   	if(imd.eq.0) then
				   call setfont1(font0(i))
		   		   call setcsize(sizen0,size0(i))
				call coltyp18(ibk)
		            nl=nblank1(str(i))
			   	call justifystring(ixg0(i),iyg0(i),
     &		   	str(i)(1:nl),angle0(i),size0(i),ijus(i))
				endif
				   call coltyp18(colors(i))
				   call setfont1(font(i))
		               nl=nblank1(str(i))
		   		   sizen=csfac1(i)*csize
		   		   call setcsize(sizen,size(i))
			         call justifystring(ixg(i),iyg(i),
     &		         str(i)(1:nl),angle(i),size(i),
     &                     ijus(i))
      	   		   call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
				endif
				if(ival.eq.71) then     ! Home:decrease color
				   if (colors(i).gt.0) colors(i)=colors(i)-1
			   	if(imd.eq.0) then
				   call setfont1(font0(i))
		   		   call setcsize(sizen0,size0(i))
				call coltyp18(ibk)
		            nl=nblank1(str(i))
			   	call justifystring(ixg0(i),iyg0(i),
     &		   	str(i)(1:nl),angle0(i),size0(i),ijus(i))
				endif
				   call setfont1(font(i))
				   call coltyp18(colors(i))
		               nl=nblank1(str(i))
		   		   sizen=csfac1(i)*csize
		   		   call setcsize(sizen,size(i))
			         call justifystring(ixg(i),iyg(i),
     &		         str(i)(1:nl),angle(i),size(i),
     &                     ijus(i))
      	   		   call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
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
					   nl=nblank1(str(i))
					   sizen=csfac1(i)*csize
					   call setcsize(sizen,size(i))
					   call justifystring(ixg(i),iyg(i),
     &		               str(i)(1:nl),angle(i),size(i),
     &                           ijus(i))
					else
			   		   jdraw(i)=0
					   call setfont1(font(i))
					   call coltyp18(ibk)
					   nl=nblank1(str(i))
					   sizen=csfac1(i)*csize
					   call setcsize(sizen,size(i))
					   call justifystring(ixg(i),iyg(i),
     &		               str(i)(1:nl),angle(i),size(i),
     &                           ijus(i))
					endif
				endif
		  		goto 33
			   else if(ktype.eq.3.or.ktype.eq.0) then
				if(ival.eq.43) then        ! +: increase size
	                     sizen=sizen+cs		!plus
	                     csfac1(i)=sizen/csize
				else if(ival.eq.45) then   ! -: decrease size
		   		   sizen=csfac1(i)*csize
	                     sizen=sizen-cs		!plus
	                     csfac1(i)=sizen/csize
				endif
				if(first.eq.1) then
				   call setfont1(font(i))
		   		   call coltyp18(ibk)
c		   		   sizen=csfac1(i)*csize
c		   		   call setcsize(sizen,size(i))
      	   		   call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		               nl=nblank1(str(i))
			         call justifystring(ixg(i),iyg(i),
     &		         str(i)(1:nl),angle(i),size(i),
     &                     ijus(i))
				   first=0
				   goto 45
				endif
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
45	   		      continue
		   		sizen=csfac1(i)*csize
				call SETCSIZE(sizen,size(i))		!set isize
				call setfont1(font(i))
      	   		call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
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
		   		sizen=csfac1(i)*csize
		   		call setcsize(sizen,size(i))
			      ih=charheight(size(i))
			      iw=charwidth(size(i))
      	   		call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
				call setfont1(font(i))
				if(imd.ne.0) then
		            nl=nblank1(str(i))
			      call justifystring(ixg(i),iyg(i),
     &		      str(i)(1:nl),angle(i),size(i),ijus(i))
				endif
				goto 33
			   else if(ktype.eq.2) then
				if(ival.eq.98) then     ! b:toggle  box draw
					if(jdraw(i).eq.-1) then
					   do l=1,100000
						sk=3.5678*l
					   enddo
c					   print*,i
					   call coltyp18(ibk)
		   			   sizen=csfac1(i)*csize
		   		         call setcsize(sizen,size(i))
      	   		         call FBOX1(IXg(i),IYg(i),str(i),
     &				   ANGLE(i),
     &                           SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
			   		   jdraw(i)=1
					else if(jdraw(i).eq.1) then
					   do l=1,1000000
						sk=3.5678*67.0*l
					   enddo
					   call coltyp18(colors(i))
		   			   sizen=csfac1(i)*csize
		   		         call setcsize(sizen,size(i))
      	   		         call FBOX1(IXg(i),IYg(i),str(i),
     &				   ANGLE(i),
     &                           SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
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
		   		   call coltyp18(ibk)
				   call setfont1(font(i))
		   		   sizen=csfac1(i)*csize
		   		   call setcsize(sizen,size(i))
      	   		   call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &                     SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		               nl=nblank1(str(i))
			         call justifystring(ixg(i),iyg(i),
     &		         str(i)(1:nl),angle(i),size(i),
     &                     ijus(i))
				   first=0
				   goto 55
				endif
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array0) !display
55				font(i)=ifont
				call setfont1(font(i))
		   		sizen=csfac1(i)*csize
		   		call setcsize(sizen,size(i))
      	   		call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
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
		   		sizen=csfac1(i)*csize
		   		call setcsize(sizen,size(i))
      	   		call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &                  SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
		            nl=nblank1(str(i))
			      call justifystring(ixg(i),iyg(i),
     &		      str(i)(1:nl),angle(i),size(i),ijus(i))
				goto 33
			   else if(ival.eq.13)then       ! set option
			   	if(imd.eq.0) then
				call coltyp18(ibk)
				   call setfont1(font0(i))
		   		   call setcsize(sizen0,size0(i))
		            nl=nblank1(str(i))
			   	call justifystring(ixg0(i),iyg0(i),
     &		   	str(i)(1:nl),angle0(i),size0(i),ijus(i))
			   	call coltyp18(colors(i))
				   call setfont1(font(i))
		   		   call setcsize(sizen,size(i))
			      call justifystring(ixg(i),iyg(i),
     &		      str(i)(1:nl),angle(i),size(i),ijus(i))
				endif
				if(jdraw(i).ne.-1) then
				call coltyp18(ibk)
		   		sizen=csfac1(i)*csize
		   		call setcsize(sizen,size(i))
      	   	   	call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
     &               	SIZE(i),XJUS(i),YJUS(i),ixbox,iybox,1)
				endif
        	  		call show_mouse()
				deallocate(array0)
				goto 1
	      	   else		!any other key to exit
	  	  		call hide_mouse()
				call coltyp18(ibk)
		   		sizen=csfac1(i)*csize
		   		call setcsize(sizen,size(i))
      	   	   	call FBOX1(IXg(i),IYg(i),str(i),ANGLE(i),
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
		   sizen=csfac1(temp(k))*csize
		   call setcsize(sizen,size(temp(k)))
      	   call FBOX1(IXg(temp(k)),IYg(temp(k)),
     &	   str(temp(k)),ANGLE(temp(k)),
     &         SIZE(temp(k)),XJUS(temp(k)),YJUS(temp(k)),
     &	   ixbox,iybox,1)
c	         if(imd.eq.0) then
c		   nl=nblank1(str(i))
c	         call justifystring(ixg(i),iyg(i),
c     &	   str(i)(1:nl),angle(i),size(i),ijus(i))
c		   endif
            enddo
        	call show_mouse()
	    	goto 99     		! end of ibutton.eq.2
	   endif                      ! end mouse buttons selection
	endif                         ! end if (mouse_set)
99	continue
	call hide_mouse()
	deallocate(size,ixg0,iyg0,size0,angle0,font0)
	 parval=str(1)
	 icol(26)=colors(1)
	 ifnt(6)=font(1)
	 iangle(6)=angle(1)
	 csfac(6)=csfac1(1)
	 rx(6)=xworld(ixg(1))
	 ry(6)=yworld(iyg(1))
	 ixjus(6)=xjus(1)
	 iyjus(6)=yjus(1)
	 idraw(6)=jdraw(1)
	 if(colors(2).ne.icl) then
	 	icol(23)=colors(2)
	 else
	 	icol(23)=colors(3)
	 endif
	 xtitle=str(2)
	 ifnt(8)=font(2)
	 iangle(8)=angle(2)
	 csfac(8)=csfac1(2)
	 rx(8)=xworld(ixg(2))
	 ry(8)=yworld(iyg(2))
	 ixjus(8)=xjus(2)
	 iyjus(8)=yjus(2)
	 idraw(8)=jdraw(2)

	 ytitle=str(3)
	 ifnt(9)=font(3)
	 iangle(9)=angle(3)
	 csfac(9)=csfac1(3)
	 rx(9)=xworld(ixg(3))
	 ry(9)=yworld(iyg(3))
	 ixjus(9)=xjus(3)
	 iyjus(9)=yjus(3)
	 idraw(9)=jdraw(3)

	 title1=str(4)
	 icol(25)=colors(4)
	 ifnt(10)=font(4)
	 iangle(10)=angle(4)
	 csfac(10)=csfac1(4)
	 rx(10)=xworld(ixg(4))
	 ry(10)=yworld(iyg(4))
	 ixjus(10)=xjus(4)
	 iyjus(10)=yjus(4)
	 idraw(10)=jdraw(4)
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
	 if(csfac1(5).ne.csf0) then
		csfac(7)=csfac1(5)
	 else
		csfac(7)=csfac1(5+numbx)
	 endif
	 ifnt(7)=font(5)
	 csfac(7)=csfac1(5)
	 do k=1,numbx
	   	if(.not.logx)  then
		   cnumx(k)=str(k+4)
		endif
	    	rx(k+30)=xworld(ixg(k+4))
		ry(k+30)=yworld(iyg(k+4))
		iangle(k+30)=angle(k+4)
		ixjus(k+30)=xjus(k+4)
		iyjus(k+30)=yjus(k+4)
	      idraw(k+30)=jdraw(k+4)
	 enddo
	 do k=1,numby
	   	if(.not.logy)  then
		   cnumy(k)=str(k+4+numbx)
		endif
	    	rx(k+55)=xworld(ixg(k+4+numbx))
		ry(k+55)=yworld(iyg(k+4+numbx))
		iangle(k+55)=angle(k+4+numbx)
		ixjus(k+55)=xjus(k+4+numbx)
		iyjus(k+55)=yjus(k+4+numbx)
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
		   csfac(j+10)=csfac1(k+4+numbx+numby)
		   rx(j+10)=xworld(ixg(k+4+numbx+numby))
		   ry(j+10)=yworld(iyg(k+4+numbx+numby))
		   iangle(j+10)=angle(k+4+numbx+numby)
		   ixjus(j+10)=xjus(k+4+numbx+numby)
		   iyjus(j+10)=yjus(k+4+numbx+numby)
		   j=j+1
		endif
	   enddo
	   ntext=j-1
	 endif
	deallocate(csfac1,ixg,iyg,colors,angle,font,xjus,yjus,str,jdraw)


	RETURN
	end
