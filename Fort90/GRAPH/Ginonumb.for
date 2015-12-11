C     Last change:  P     2 Apr 97    4:12 pm
c==========================================================================
      subroutine GINONUMB(cnumx,cnumy,cexpx,cexpy,numbx,numby,inumx,
     &  inumy,logx,logy,rx,ry,ijus,icol,rotate,ifnt,idraw,
     &  size,ibk,imd,lth,ikey)
c==========================================================================

	real    	   RX(100),RY(100),rotate(100),lth
	integer 	   IJUS(100),idraw(100)
	integer*4	   ixlo(:),iylo(:),ixhi(:),iyhi(:),idim(:)
      real           xg(:),yg(:),angle(:)
      real		   xg0(:),yg0(:),angle0(:)
	integer 	   JUS(:),jdraw(:)
	integer 	   first
	integer*1	   arrayn(:,:)
	character*1    ans,getch
	character*10   cnumx(25),cnumy(25),cexpx(25),cexpy(25)
	character*55   helpst(30)		!help strings
	character*20   str(:)
	logical        isnum,ivl,kbhit,logx,logy

	allocatable :: ixlo,iylo,ixhi,iyhi,idim
	allocatable :: arrayn,xg0,yg0,angle0
	allocatable :: angle,xg,yg,str,jdraw,jus

c	HELP LINES:
C========================================================
	helpst(1)='   ->: move right'
	helpst(2)='   <-: move left'
	helpst(3)='  /\: up'
	helpst(4)='  \/: down'
	helpst(5)='  + : increase size'
	helpst(6)='  - : decrease size'
	helpst(7)='  * : increase font number'
	helpst(8)='  / : decrease font number'
	helpst(9)='PgUp: increase angle'
	helpst(10)='PgDn: decrease angle'
	helpst(11)=' Ins: toggle sensitivity'
	helpst(12)='Home: decrease color'
	helpst(13)=' End: increase color'
c============================================================

	ivl=isnum()
	if(ivl) call NUMCLR
	if (ikey.eq.1.or.ikey.eq.3) then
	   if(ikey.eq.1) then
		n=numbx
	   else
		n=numbx+numby
	   endif
	   allocate(angle(n),xg(n),yg(n))
         allocate(str(n),jdraw(n),jus(n))
	   allocate(xg0(n),yg0(n),angle0(n))
	   allocate(ixlo(n),iylo(n),ixhi(n),iyhi(n),idim(n))
	   do i=1,n
	      str(i)(1:20)='                    '
	   enddo
	   do k=1,numbx
	      nl=nblank1(cnumx(k))
	      str(k)= cnumx(k)(1:nl)
	      if(logx) then
		   call SETSIZE(size)	!this defines ISIZE
		   call CHAEXI(0.8,0.8,0.6,0.3)
		   nl1=NBLANK1(cexpx(k))
		   nl=nblank1(cnumx(k))
		   str(k)=cnumx(k)(1:nl)//char(42)//char(69)
     &         //cexpx(k)(1:nl1)
	      endif
	      call graspa(rx(k+30),ry(k+30),xg(k),yg(k))
	      angle(k)=rotate(k+30)
	      jus(k)=ijus(k+30)
	      jdraw(k)=idraw(k+30)
	   enddo
	endif
	if (ikey.eq.2.or.ikey.eq.3) then
	   if(ikey.eq.2) then
		n=numby
		i1=1
	      allocate(angle(n),xg(n),yg(n))
            allocate(str(n),jdraw(n),jus(n))
	      allocate(xg0(n),yg0(n),angle0(n))
	      allocate(ixlo(n),iylo(n),ixhi(n),iyhi(n),idim(n))
	      do i=1,n
	         str(i)(1:20)='                    '
	      enddo
	   else
		i1=numbx+1
	   endif
	   l=1
	   do k=i1,n
	      nl=nblank1(cnumy(l))
	      str(k)= cnumy(l)(1:nl)
	      if(logy) then
		   call SETSIZE(size)	!this defines ISIZE
		   call CHAEXI(0.8,0.8,0.6,0.3)
		   nl1=NBLANK1(cexpy(l))
		   nl=nblank1(cnumy(l))
		   str(k)=cnumy(l)(1:nl)//char(42)//char(69)
     &         //cexpy(l)(1:nl1)
	      endif
	      call graspa(rx(l+55),ry(l+55),xg(k),yg(k))
	      angle(k)=rotate(l+55)
	      jus(k)=ijus(l+55)
	      jdraw(k)=idraw(l+55)
		l=l+1
	   enddo
	endif
	nhelp=13
	icq=14     		! color help menu
	call vp2enq(xp1,xp2,yp1,yp2,xv1,xv2,yv1,yv2)
	dm=0.01
	deltax=0.01*abs(xp2-xp1)           ! movement
	deltay=0.01*abs(yp2-yp1)           ! movement
	ang=45.             ! angle
	cs=1.
	call calcmax(xg,yg,str,angle,size,jus,n,max,sizemax)
	max=int((25/sizemax)*max)
	allocate(arrayn(1:n,1:max))
	first=1
1	ans=getch(ktype)
	ival=ichar(ans)
	if(ktype.eq.0.and.ival.eq.59) then		!F1=help
	   ixloh=320    !position for help box
	   iyhih=320
	   call poptext(ixloh,-1,iyhih,helpst,nhelp,15,2,icq)	!=popkeys
	   goto 1
	else if(ival.eq.27.or.ival.eq.13) then		!ESC or ENTER
	   goto 99
	else if(ktype.eq.8) then	!move menu with ARROW keys
		if(ival.eq.77) then	!right
		   if(first.eq.1) then
		      call setfnt(ifnt)
		      call linwid(lth)
		   	call lincols(ibk)
			do i=1,n
			   nl=nblank1(str(i))
			   call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			enddo
		      first=0
		      goto 21
		   endif
		   call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
21             continue
		   do i=1,n
		   	xg(i)=xg(i)+deltax
		   enddo
		   call calcnumall(xg,yg,str,angle,size,jus,
     &	   ixlo,iylo,ixhi,iyhi,idim,n)
		   call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		   call lincols(icol)
		      call setfnt(ifnt)
		      call linwid(lth)
		   do i=1,n
			   nl=nblank1(str(i))
			if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	      str(i)(1:nl),angle(i),size,jus(i))
		   enddo
		endif
		if(ival.eq.75) then	!right
		   call setfnt(ifnt)
		   if(first.eq.1) then
		   	call lincols(ibk)
		      call linwid(lth)
			do i=1,n
			   nl=nblank1(str(i))
			   call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			enddo
		      first=0
		      goto 22
		   endif
		   call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
22             continue
		   do i=1,n
		   	xg(i)=xg(i)-deltax
		   enddo
		   call calcnumall(xg,yg,str,angle,size,jus,
     &	   ixlo,iylo,ixhi,iyhi,idim,n)
		   call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		   	call lincols(icol)
		      call setfnt(ifnt)
		      call linwid(lth)
			do i=1,n
			   nl=nblank1(str(i))
			   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			enddo
		endif
		if(ival.eq.72) then	!up
		   if(first.eq.1) then
		   	call lincols(ibk)
		      call setfnt(ifnt)
		      call linwid(lth)
			do i=1,n
			   nl=nblank1(str(i))
			   call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			enddo
		      first=0
		      goto 23
		   endif
		   call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
23             continue
		   do i=1,n
		   	yg(i)=yg(i)+deltay
		   enddo
		   call calcnumall(xg,yg,str,angle,size,jus,
     &	   ixlo,iylo,ixhi,iyhi,idim,n)
		   call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		      call setfnt(ifnt)
		      call linwid(lth)
		   	call lincols(icol)
			do i=1,n
			   nl=nblank1(str(i))
			   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			enddo
		endif
		if(ival.eq.80) then	!down
		   if(first.eq.1) then
		   	call lincols(ibk)
		      call setfnt(ifnt)
		      call linwid(lth)
			do i=1,n
			   nl=nblank1(str(i))
			   call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			enddo
		      first=0
		      goto 24
		   endif
		   call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
24             continue
		   do i=1,n
		   	yg(i)=yg(i)-deltay
		   enddo
		   call calcnumall(xg,yg,str,angle,size,jus,
     &	   ixlo,iylo,ixhi,iyhi,idim,n)
		   call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		   call lincols(icol)
		      call setfnt(ifnt)
		      call linwid(lth)
		   do i=1,n
			   nl=nblank1(str(i))
			   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
		   enddo
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
		   do i=1,n
			if(jdraw(i).eq.0) then
			   jdraw(i)=1
			   call setfnt(ifnt)
			   call lincols(icol)
			   nl=nblank1(str(i))
			   call linwid(lth)
			   call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			else
			   jdraw(i)=0
			   call setfnt(ifnt)
			   call lincols(ibk)
			   nl=nblank1(str(i))
			   call linwid(lth)
			   call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			endif
		   enddo
		endif
		if(ival.eq.79) then     ! End:increase color
		   if(ikey.ne.3) goto 1
		   if(icol.le.15) icol=icol+1
		   call lincols(icol)
		   call setfnt(ifnt)
		   call linwid(lth)
		   do i=1,n
		      nl=nblank1(str(i))
		      if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	      str(i)(1:nl),angle(i),size,jus(i))
		   enddo
		endif
		if(ival.eq.71) then     ! Home:decrease color
		   if(ikey.ne.3) goto 1
		   if(icol.ge.1) icol=icol-1
		   call setfnt(ifnt)
		   call linwid(lth)
		   call lincols(icol)
		   do i=1,n
			   nl=nblank1(str(i))
			   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
		   enddo
		endif
		if(ival.eq.73) then	!PgDn:rotate clockwise
			if(first.eq.1) then
		      call setfnt(ifnt)
		      call linwid(lth)
			call lincols(ibk)
			do i=1,n
			   nl=nblank1(str(i))
			   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			enddo
			first=0
			goto 55
			endif
			call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
55		      continue
			do i=1,n
		      angle(i)=angle(i)+ang
	            enddo		!plus
		      call calcnumall(xg,yg,str,angle,size,jus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n)
			call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		      call setfnt(ifnt)
			call lincols(icol)
		      call linwid(lth)
			do i=1,n
			   nl=nblank1(str(i))
			   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			enddo
		endif
		if(ival.eq.81) then	!PgUp:rotate anticlockwise
			if(first.eq.1) then
			call lincols(ibk)
		      call setfnt(ifnt)
		      call linwid(lth)
			do i=1,n
			   nl=nblank1(str(i))
			   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			enddo
			first=0
			goto 56
			endif
			call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
56		      continue
			do i=1,n
		      angle(i)=angle(i)-ang
	            enddo		!
		      call calcnumall(xg,yg,str,angle,size,jus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n)
			call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		      call setfnt(ifnt)
			call lincols(icol)
		      call linwid(lth)
			do i=1,n
			   nl=nblank1(str(i))
			   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			enddo
		endif
	      goto 1
	else if((ktype.eq.3.or.ktype.eq.0).and.ikey.eq.3) then
		if(ikey.ne.3) goto 1
		if(ival.eq.43) then        ! +: increase size
			if(first.eq.1) then
		      call setfnt(ifnt)
		      call linwid(lth)
			call lincols(ibk)
			do i=1,n
			   nl=nblank1(str(i))
			   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			enddo
			first=0
			goto 45
			endif
			call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
45		      continue
	            if(size.le.5.) size=size+cs		!plus
		      call calcnumall(xg,yg,str,angle,size,jus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n)
			call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		      call setfnt(ifnt)
		      call linwid(lth)
			call lincols(icol)
			do i=1,n
			   nl=nblank1(str(i))
			   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			enddo
		endif
		if(ival.eq.45) then        ! -: decrease size
			if(first.eq.1) then
			call lincols(ibk)
		      call setfnt(ifnt)
		      call linwid(lth)
			do i=1,n
			   nl=nblank1(str(i))
			   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			enddo
			first=0
			goto 46
			endif
			call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
46		      continue
	            if(size.ge.1.) size=size-cs		!plus
		      call calcnumall(xg,yg,str,angle,size,jus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n)
			call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		      call setfnt(ifnt)
		      call linwid(lth)
			call lincols(icol)
			do i=1,n
			   nl=nblank1(str(i))
			   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			enddo
		endif
		goto 1
	else if(ktype.eq.2) then
		if(ikey.ne.3) goto 1
		if(ival.eq.42) then        ! *: increase font
			if(first.eq.1) then
			call lincols(ibk)
		      call setfnt(ifnt)
		      call linwid(lth)
			do i=1,n
			   nl=nblank1(str(i))
			   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			enddo
			first=0
			goto 47
			endif
			call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
47		      continue
	            ifnt=ifnt+1		!plus
		      call calcnumall(xg,yg,str,angle,size,jus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n)
			call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		      call setfnt(ifnt)
			call lincols(icol)
		      call linwid(lth)
			do i=1,n
			   nl=nblank1(str(i))
			   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			enddo
		endif
		if(ival.eq.47) then        ! /:decrease font
			if(first.eq.1) then
		      call setfnt(ifnt)
		      call linwid(lth)
			call lincols(ibk)
			do i=1,n
			   nl=nblank1(str(i))
			   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			enddo
			first=0
			goto 48
			endif
			call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
48		      continue
	            ifnt=ifnt-1		!plus
		      call calcnumall(xg,yg,str,angle,size,jus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n)
			call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		      call setfnt(ifnt)
		      call linwid(lth)
			call lincols(icol)
			do i=1,n
			   nl=nblank1(str(i))
			   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			enddo
		endif
		if(ival.eq.98) then        ! b : boxes
		   do i=1,n        	! show all boxes if outside
		      if(jdraw(i).eq.1) then
			   call setfnt(ifnt)
			   call lincols(icol)
		         call linwid(lth)
			   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &	         SIZE,JUS(i),xbox,ybox,1)
			   jdraw(i)=-1
		      else if(jdraw(i).eq.-1) then
			   call setfnt(ifnt)
			   call lincols(ibk)
		         call linwid(lth)
			   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &	         SIZE,JUS(i),xbox,ybox,1)
			   jdraw(i)=1
		      endif
		   enddo
		endif
	goto 1
	endif
c	   goto 1
c	endif       ! end if(kbhit())
 99	if (ikey.eq.1.or.ikey.eq.3) then
	   do k=1,numbx
	      nl=nblank1(str(k))
c	      cnumx(k)(1:nl)=str(k)
	      call spagra(xg(k),yg(k),rx(k+30),ry(k+30))
	      rotate(k+30)=angle(k)
	      ijus(k+30)=jus(k)
	      idraw(k+30)=jdraw(k)
	   enddo
	endif
	if (ikey.eq.2.or.ikey.eq.3) then
	   if(ikey.eq.2) then
		n=numby
		i1=1
	   else
		i1=numbx+1
	   endif
	   l=1
	   do k=i1,n
	      nl=nblank1(str(k))
c	      cnumy(l)(1:nl)=str(k)
	      call spagra(xg(k),yg(k),rx(l+55),ry(l+55))
	      rotate(l+55)=angle(k)
	      ijus(l+55)=jus(k)
	      idraw(l+55)= jdraw(k)
		l=l+1
	   enddo
	endif
	deallocate(arrayn,xg0,yg0,angle0)
	deallocate(angle,xg,yg,str,jdraw,jus)
	deallocate (ixlo,iylo,ixhi,iyhi,idim)
	end
