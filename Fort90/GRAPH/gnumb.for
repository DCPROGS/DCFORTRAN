      subroutine GNUMB(cnumx,cnumy,cexpx,cexpy,numbx,numby,inumx,
     &  inumy,logx,logy,rx,ry,ijus,icol,rotate,ifnt,idraw,
     &  size,ibk,imd,rlth,ikey,idev)
c==========================================================================

	real    	   RX(100),RY(100),rotate(100),rlth
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
	character*64   TITLE1,strings(10),title
	character*20   str(:)
	logical        isnum,ivl,kbhit,logx,logy,help,outtext
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	common/mousval/mouse_on,nbutton
	common/hlp/help		!for QDIALOG

	allocatable :: ixlo,iylo,ixhi,iyhi,idim
	allocatable :: arrayn,xg0,yg0,angle0
	allocatable :: angle,xg,yg,str,jdraw,jus
	   nxlo=8
	   nylo=16
	   nyhi=-1
	   ictm=15		!text white
	   ibkm=1		!background dark grey
	   icfm=13	!frame dark blue
	   icupm=12		!upper case red
	   TITLE     ='   OPTIONS      '
	   strings(1)='1. Move         '
         strings(2)='2. change Size  '
	   strings(3)='3. change Font  '
	   strings(4)='4. change Color '
	   strings(5)='5. change Angle '
	   strings(6)='6. toggle Draw  '
	   strings(7)='7. toggle Box   '
	   strings(8)='8. End          '
	   nval=8

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
	      if(logx.and.inumx.eq.1) then
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
	      if(logy.and.inumy.eq.1) then
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
	size0=size
	ifont0=ifnt
	do i=1,n
	   angle0(i)=angle(i)
	enddo
777	nhelp=0
	call POPMENU(nxlo,nylo,nyhi,strings,nval,ictm,icfm,
     &     icupm,ibkm,title,helps,nhelp,iline,charout,jval)
	if(iline.ge.1.and.iline.le.nval) then	!iline=0 for ESC=cancel
		ihelp=iline
	else
		ihelp=8
	endif


c
	SELECT CASE(IHELP)
	   CASE(1)		!MOVE
733		ans=getch(ktype)
		ival=ichar(ans)
		if(ktype.eq.0.and.ival.eq.59) then		!F1=h      elp
		   call VHELP(14)
		   ibutton=0
		   outtext=.true.
		else if(ktype.eq.0.and.ival.eq.60) then	!F2=help
		   call vhelp(-2)
		   ibutton=0
		   outtext=.true.
		endif
		if(ktype.eq.8) then	!move menu with ARROW keys
		  if(ival.eq.77) then	!right
		   if(first.eq.1) then
		      call setfnt(ifnt)
		      call linwid(rlth)
		   	call lincols(ibk,idev)
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
		   call lincols(icol,idev)
		      call setfnt(ifnt)
		      call linwid(rlth)
		   do i=1,n
			   nl=nblank1(str(i))
			if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	      str(i)(1:nl),angle(i),size,jus(i))
		   enddo
		  endif
		  if(ival.eq.75) then	!right
		   call setfnt(ifnt)
		   if(first.eq.1) then
		   	call lincols(ibk,idev)
		      call linwid(rlth)
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
		   	call lincols(icol,idev)
		      call setfnt(ifnt)
		      call linwid(rlth)
			do i=1,n
			   nl=nblank1(str(i))
			   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			enddo
		  endif
		  if(ival.eq.72) then	!up
		   if(first.eq.1) then
		   	call lincols(ibk,idev)
		      call setfnt(ifnt)
		      call linwid(rlth)
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
		      call linwid(rlth)
		   	call lincols(icol,idev)
			do i=1,n
			   nl=nblank1(str(i))
			   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			enddo
		  endif
		  if(ival.eq.80) then	!down
		   if(first.eq.1) then
		   	call lincols(ibk,idev)
		      call setfnt(ifnt)
		      call linwid(rlth)
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
		   call lincols(icol,idev)
		      call setfnt(ifnt)
		      call linwid(rlth)
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
		  goto 733
		else
		  iline=8
		  goto 777
		endif

	   CASE(2)		!SIZE
		wpo=size*4.
		call DEFOLTr(wpo,defolt)
		call QDIALOG(1,'Size ',defolt,11,cans)
		call GETINPr(cans,wpo)
		size=wpo*0.25
		if(first.eq.1) then
		call setfnt(ifnt)
		call linwid(rlth)
		call lincols(ibk,idev)
		do i=1,n
		   nl=nblank1(str(i))
		   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	   str(i)(1:nl),angle(i),size0,jus(i))
		enddo
		first=0
		goto 45
		endif
		call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
45		continue
		call calcnumall(xg,yg,str,angle,size,jus,
     &	ixlo,iylo,ixhi,iyhi,idim,n)
		call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		call setfnt(ifnt)
		call linwid(rlth)
		call lincols(icol,idev)
		do i=1,n
		   nl=nblank1(str(i))
		   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	   str(i)(1:nl),angle(i),size,jus(i))
		enddo
		size0=size
	      iline=8
		goto 777

	   CASE(3)           !Font
c	      ifont0=ifnt
	      iak=ifnt
	      call DEFOLTi(iak,defolt)
333	      call QDIALOG(1,'Font [F1=HELP]',defolt,11,cans)
	      if(help) then				!F1 hit in qdialog
	   	   call vhelp(13)		!outputs imod
	   	   goto 333
	      else
	   	   call GETINPi(cans,iak)
	      endif
	      ifnt=iak
		if(first.eq.1) then
			call lincols(ibk,idev)
		      call setfnt(ifont0)
		      call linwid(rlth)
			do i=1,n
			   nl=nblank1(str(i))
			   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	         str(i)(1:nl),angle(i),size,jus(i))
			enddo
			first=0
			goto 47
		endif
		call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
47		continue
		call calcnumall(xg,yg,str,angle,size,jus,
     &	ixlo,iylo,ixhi,iyhi,idim,n)
		call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		call setfnt(ifnt)
		call lincols(icol,idev)
		call linwid(rlth)
		do i=1,n
		   nl=nblank1(str(i))
		   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	   str(i)(1:nl),angle(i),size,jus(i))
		enddo
		iline=8
		ifont0=ifnt
		goto 777

	   case(4)          ! Color
		iak=icol
		call DEFOLTi(iak,defolt)
444	      call QDIALOG(1,'Color [F1=HELP]',defolt,11,cans)
		if(help) then				!F1 hit in qdialog
			call vhelp(12)		!outputs imod
			goto 444
		else
			call GETINPi(cans,iak)
		endif
		icol=iak
		call lincols(icol,idev)
		call setfnt(ifnt)
		call linwid(rlth)
		do i=1,n
		   nl=nblank1(str(i))
		   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	   str(i)(1:nl),angle(i),size,jus(i))
		enddo
		iline=8
		goto 777

	   CASE(5)           !ANGLE
		ak=angle0(1)
 	   	call DEFOLTr( ak,defolt)
	      call QDIALOG(1,'Angle ',defolt,11,cans)
	   	call GETINPr(cans, ak)
		do i=1,n
		   angle(i)=ak
		enddo
		if(first.eq.1) then
		call setfnt(ifnt)
		call linwid(rlth)
		call lincols(ibk,idev)
		do i=1,n
		   nl=nblank1(str(i))
		   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	   str(i)(1:nl),angle0(i),size,jus(i))
		enddo
		first=0
		goto 55
		endif
		call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
55		continue
		call calcnumall(xg,yg,str,angle,size,jus,
     &	ixlo,iylo,ixhi,iyhi,idim,n)
		call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		call setfnt(ifnt)
		call lincols(icol,idev)
		call linwid(rlth)
		do i=1,n
		   nl=nblank1(str(i))
		   if(jdraw(i).ne.0) call justifystring(xg(i),yg(i),
     &	   str(i)(1:nl),angle(i),size,jus(i))
		enddo
		iline=8
		do i=1,n
			angle0(i)=angle(i)
		enddo
		goto 777

         CASE(6)		!DRAW
		do i=1,n
		if(jdraw(i).eq.0) then
		   jdraw(i)=1
		   call setfnt(ifnt)
		   call lincols(icol,idev)
		   nl=nblank1(str(i))
		   call linwid(rlth)
		   call justifystring(xg(i),yg(i),
     &	      str(i)(1:nl),angle(i),size,jus(i))
		else
		   jdraw(i)=0
		   call setfnt(ifnt)
		   call lincols(ibk,idev)
		   nl=nblank1(str(i))
		   call linwid(rlth)
		   call justifystring(xg(i),yg(i),
     &	      str(i)(1:nl),angle(i),size,jus(i))
		endif
		enddo
		iline=8
		goto 777

	   CASE(7)           !BOX

		   do i=1,n        	! show all boxes if outside
		      if(jdraw(i).eq.1) then
			   call setfnt(ifnt)
			   call lincols(icol,idev)
		         call linwid(rlth)
			   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &	         SIZE,JUS(i),xbox,ybox,1)
			   jdraw(i)=-1
		      else if(jdraw(i).eq.-1) then
			   call setfnt(ifnt)
			   call lincols(ibk,idev)
		         call linwid(rlth)
			   call FBOX1(xg(i),yg(i),str(i),ANGLE(i),
     &	         SIZE,JUS(i),xbox,ybox,1)
			   jdraw(i)=1
		      endif
		   enddo
		   iline=8
		   goto 777

	   case(8)
	     goto 99
	end select
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
