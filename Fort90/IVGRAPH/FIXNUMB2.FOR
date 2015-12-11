c***********************************************************************
	subroutine FIXNUMB2(imode,nbox,csfac,csize,ifont,inumx,inumy,
     & logx,logy,rx,ry,angle,xjus,yjus,idraw,rxbox,rybox,
     & numbx,numby,cnumx,cnumy,cexpx,cexpy,icol,ibk)
c	subroutine movenum(cnum,csfac,csize,n,ixg,iyg,xjus,yjus,colors,
c     &			angle,ifont,ibk,cexp,inumxy,logxy)
c***********************************************************************
c	subroutine to move ALL the strings CNUM(+CEXP) at a time ,
c	written with HGRAPH,on the screen using FBOX1
c	similar to FIXNUMBERS
c	ATTENTION : ixg(n),iyg(n) coordinates in HGRAPH display
c     (0-10000,0-7500)
c	I presumed that all the strings have the same size and font as in
c	VPLOT!
c	For writing the numbers, calls IDRAWNUM
c	To each number corresponds a window , so we read and write n-windows
c	at a movement! so the problem with a two-dimensions( M-dim maybe!)
c	was solved in READALL and WRITALL ;
c     now we are able to design some crazy cards'games!
c     Other calls:CALCMAX : calculates maximum dimension for the array
c		     :calcnumall : calculates n-window's dimensions
c***********************************************************************
c IMODE=2 Move or delete all X axis numbers at once
c IMODE=3 Move or delete all Y axis numbers at once
c IMODE=4 Change size or font for ALL axis numbers
c IDRAW=0  omit
c IDRAW=1  draw (no box)
c IDRAW=-1 draw with box
c***********************************************************************


	integer*1		arrayn(:,:)
	integer*4		size(:),ixg(:),iyg(:),iangle(:),inumxy(:)
	integer*4		colors(:),ixjus(:),iyjus(:),ifont
	integer*4		ixlo(:),iylo(:),ixhi(:),iyhi(:),idim(:)

	integer*4		xjus(100),yjus(100)
	integer*4		angle(100),idraw(100),jdraw(:)
	real*4 		RX(100),RY(100)
	real*4 		rxbox(4,100),rybox(4,100)
	integer 	      charheight,charwidth
	character*(*)	cnumx(100),cexpx(100),cnumy(100),cexpy(100)
	character*(10)	cnum(:),cexp(:)
	character*55 	helpst(30)		!help strings
	character*1 	ans,getch
	logical		kbhit,logx,logy,logxy(:)
	allocatable size,arrayn,ixlo,iylo,ixhi,iyhi,idim,inumxy,jdraw
	allocatable iangle,ixg,iyg,colors,ixjus,iyjus,cnum,cexp,logxy

	sizen=csfac*csize
	call setcsize(sizen,isize)
	call SETFONT1(ifont)		    	!set font
	ih=charheight(isize)
	iw=charwidth(isize)
	ixoff=-ifix(float(iw)/5.)	!for numbers in exponent form
	iyoff=ifix(ih/2)
	select case(imode)
	 case(1)
	    n=numby+numbx
	    allocate(iangle(n),ixg(n),iyg(n),colors(n),ixjus(n),iyjus(n),
     &    cnum(n),cexp(n),inumxy(n),logxy(n),jdraw(n))
	    allocate(ixlo(n),iylo(n),ixhi(n),iyhi(n),idim(n),size(n))
	    i=1
	    do k=1,numbx
		 j=k+30
		 logxy(i)=logx
		 inumxy(i)=inumx
		 cnum(i)=cnumx(i)
		 cexp(i)=cexpx(i)
		 jdraw(i)=idraw(j)
		 ixg(i)=ISCRX(rx(j))
		 iyg(i)=ISCRY(ry(j))
		 colors(i)=icol
		 ixjus(i)=xjus(j)
		 iyjus(i)=yjus(j)
		 iangle(i)=angle(j)
		 i=i+1
	    enddo
	    do k=1,numby
		 j=k+55
		 logxy(i)=logy
		 inumxy(i)=inumy
		 cnum(i)=cnumy(i-numbx)
		 cexp(i)=cexpy(i-numbx)
		 jdraw(i)=idraw(j)
		 ixg(i)=ISCRX(rx(j))
		 iyg(i)=ISCRY(ry(j))
		 colors(i)=icol
		 ixjus(i)=xjus(j)
		 iyjus(i)=yjus(j)
		 iangle(i)=angle(j)
		 i=i+1
	    enddo
	    sizen=csfac*csize
	    call onenum(cnum,cexp,n,ixg,iyg,ixjus,iyjus,
     &    colors,iangle,sizen,inumxy,logxy,jdraw,ibk,
     &    ixoff,iyoff,ixlo,iylo,ixhi,iyhi)
	    goto 990
	 case(2)
	    n=numbx
	    allocate(iangle(n),ixg(n),iyg(n),colors(n),ixjus(n),iyjus(n),
     &    cnum(n),cexp(n),inumxy(n),logxy(n),jdraw(n))
	    allocate(ixlo(n),iylo(n),ixhi(n),iyhi(n),idim(n),size(n))
	    do i=1,numbx
		 j=i+30
		 inumxy(i)=inumx
		 logxy(i)=logx
		 cnum(i)=cnumx(i)
		 cexp(i)=cexpx(i)
		 jdraw(i)=idraw(j)
		 ixg(i)=ISCRX(rx(j))
		 iyg(i)=ISCRY(ry(j))
		 colors(i)=icol
		 ixjus(i)=xjus(j)
		 iyjus(i)=yjus(j)
		 iangle(i)=angle(j)
		 if(i.lt.5) then
c		 	print*,n,i,ixg(i),j,rx(j)
		 endif
	    enddo
	 case(3)
	    n=numby
	    allocate(iangle(n),ixg(n),iyg(n),colors(n),ixjus(n),iyjus(n),
     &	cnum(n),cexp(n),inumxy(n),logxy(n),jdraw(n))
	    allocate(ixlo(n),iylo(n),ixhi(n),iyhi(n),idim(n),size(n))
	    do i=1,numby
		 j=i+55
		 logxy(i)=logy
		 inumxy(i)=inumy
		 cnum(i)=cnumy(i)
		 cexp(i)=cexpy(i)
		 jdraw(i)=idraw(j)
		 ixg(i)=ISCRX(rx(j))
		 iyg(i)=ISCRY(ry(j))
		 colors(i)=icol
		 ixjus(i)=xjus(j)
		 iyjus(i)=yjus(j)
		 iangle(i)=angle(j)
		 if(i.lt.5) then
c		 	print*,n,i,ixg(i),j,rx(j)
		 endif
	    enddo
	 case(4)
	    n=numby+numbx
	    allocate(iangle(n),ixg(n),iyg(n),colors(n),ixjus(n),iyjus(n),
     &	cnum(n),cexp(n),inumxy(n),logxy(n),jdraw(n))
	    allocate(ixlo(n),iylo(n),ixhi(n),iyhi(n),idim(n),size(n))
	    i=1
	    do k=1,numbx
		 j=k+30
		 logxy(i)=logx
		 inumxy(i)=inumx
		 cnum(i)=cnumx(i)
		 cexp(i)=cexpx(i)
		 ixg(i)=ISCRX(rx(j))
		 iyg(i)=ISCRY(ry(j))
		 jdraw(i)=idraw(j)
		 colors(i)=icol
		 ixjus(i)=xjus(j)
		 iyjus(i)=yjus(j)
		 iangle(i)=angle(j)
		 i=i+1
	    enddo
	    do k=1,numby
		 j=k+55
		 logxy(i)=logy
		 inumxy(i)=inumy
		 cnum(i)=cnumy(i-numbx)
		 cexp(i)=cexpy(i-numbx)
		 jdraw(i)=idraw(j)
		 ixg(i)=ISCRX(rx(j))
		 iyg(i)=ISCRY(ry(j))
		 colors(i)=icol
		 ixjus(i)=xjus(j)
		 iyjus(i)=yjus(j)
		 iangle(i)=angle(j)
		 i=i+1
	    enddo
c	    print*,n,i,ixg(n),j,rx(n)
	end select

c	HELP LINES:
C========================================================
	helpst(1)='       ->: move right'
	helpst(2)='       <-: move left'
	helpst(3)='      /\: up'
	helpst(4)='      \/: down'
	helpst(5)='      + : increase size'
	helpst(6)='      - : decrease size'
	helpst(7)='      * : increase font number'
	helpst(8)='      / : decrease font number'
	helpst(9)='     Ins: toggle sensitivity'
	helpst(10)='    Home: decrease color'
	helpst(11)='     End: increase color'
	helpst(12)='    <CR>: end selection'
c============================================================
	nhelp=12
	icq=14     		! color help menu
	im=100            ! movement
	cs=1.             ! size
	ia=45			! angle
	first=1
	do i=1,n
	   call SETCSIZE(sizen,size(i))		!set isize
	enddo

	call calcmax(ixg,iyg,cnum,iangle,size,ixjus,iyjus,n,max,msize)
	max=int((28/msize)*max)
	allocate(arrayn(1:n,1:max))
	call calcnumall(ixg,iyg,cnum,iangle,size,ixjus,iyjus,
     &	   ixlo,iylo,ixhi,iyhi,idim,n,cexp,inumxy,logxy)
1	continue
	if(kbhit()) then
	   ans=getch(ktype)
	   ival=ichar(ans)
	   if(ktype.eq.0.and.ival.eq.59) then		!F1=help
		ixloh=320    !position for help box
		iyhih=320
		call poptext(ixloh,-1,iyhih,helpst,nhelp,15,2,icq) !=popkeys
		goto 1
	   else if(ival.eq.27.or.ival.eq.13) then		!ESC or ENTER
		goto 99
	   else if(ktype.eq.8) then	!move menu with ARROW keys
		if(ival.eq.77) then	!right
		   if(imode.eq.4) goto 1
		   if(first.eq.1) then
		   	call coltyp18(ibk)
			do i=1,n
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),iyg(i),ixjus(i),iyjus(i),
     &     		   iangle(i),logxy(i),inumxy(i))
      	         call drawframe(ixlo(i),iylo(i),ixhi(i),
     &               iyhi(i),ibk)
			if(jdraw(i).eq.-1) then
      	      call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            iyhi(i),ibk)
			endif
			enddo
		      first=0
		      goto 21
		   endif
		   call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
21             continue
		   do i=1,n
		   	ixg(i)=ixg(i)+im
		   enddo
		   call calcnumall(ixg,iyg,cnum,iangle,size,ixjus,iyjus,
     &	   ixlo,iylo,ixhi,iyhi,idim,n,cexp,inumxy,logxy)
		   call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		   do i=1,n
		      call coltyp18(colors(i))
			if(jdraw(i).ne.0) then
	            call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &            sizen,ixg(i),
     &            iyg(i),ixjus(i),iyjus(i),iangle(i),logxy(i),inumxy(i))
			endif
			if(jdraw(i).eq.-1) then
      	      call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            iyhi(i),colors(i))
			endif
		   enddo
		endif
		if(ival.eq.75) then		!shift left
		   if(imode.eq.4) goto 1
		   if(first.eq.1) then
		   	call coltyp18(ibk)
			do i=1,n
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),iyg(i),ixjus(i),iyjus(i),
     &               iangle(i),logxy(i),inumxy(i))
			if(jdraw(i).eq.-1) then
      	      call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            iyhi(i),ibk)
			endif
			enddo
		      first=0
		      goto 22
		   endif
		   call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
22		   continue
		   do i=1,n
		      ixg(i)=ixg(i)-im
		   enddo
		   call calcnumall(ixg,iyg,cnum,iangle,size,ixjus,iyjus,
     &	   ixlo,iylo,ixhi,iyhi,idim,n,cexp,inumxy,logxy)
		   call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		   do i=1,n
		      call coltyp18(colors(i))
			if(jdraw(i).ne.0) then
	            call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &            sizen,ixg(i),
     &            iyg(i),ixjus(i),iyjus(i),iangle(i),logxy(i),inumxy(i))
			endif
			if(jdraw(i).eq.-1) then
      	      call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            iyhi(i),colors(i))
			endif
		   enddo
		endif
		if(ival.eq.72) then	!shift up
		   if(imode.eq.4) goto 1
		   if(first.eq.1) then
		   	call coltyp18(ibk)
			do i=1,n
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),iyg(i),ixjus(i),iyjus(i),
     &               iangle(i),logxy(i),inumxy(i))
      	         call drawframe(ixlo(i),iylo(i),ixhi(i),
     &               iyhi(i),ibk)
			if(jdraw(i).eq.-1) then
      	      call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            iyhi(i),ibk)
			endif
			enddo
			first=0
			goto 23
		   endif
		   call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
23		   continue
		   do i=1,n
			iyg(i)=iyg(i)+im
		   enddo
		   call calcnumall(ixg,iyg,cnum,iangle,size,ixjus,iyjus,
     &	   ixlo,iylo,ixhi,iyhi,idim,n,cexp,inumxy,logxy)
		   call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		   do i=1,n
			call coltyp18(colors(I))
			if(jdraw(i).ne.0) then
	            call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &            sizen,ixg(i),
     &            iyg(i),ixjus(i),iyjus(i),iangle(i),logxy(i),inumxy(i))
			endif
			if(jdraw(i).eq.-1) then
      	      call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            iyhi(i),colors(i))
			endif
		   enddo
		endif
		if(ival.eq.80) then	!shift down
		   if(imode.eq.4) goto 1
		   if(first.eq.1) then
		   	call coltyp18(ibk)
			do i=1,n
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),iyg(i),ixjus(i),iyjus(i),
     &               iangle(i),logxy(i),inumxy(i))
      	         call drawframe(ixlo(i),iylo(i),ixhi(i),
     &               iyhi(i),ibk)
			   if(jdraw(i).eq.-1) then
      	            call drawframe(ixlo(i),iylo(i),ixhi(i),
     &                  iyhi(i),ibk)
			   endif
			enddo
			first=0
			goto 24
		   endif
		   call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
24		   continue
		   do i=1,n
			iyg(i)=iyg(i)-im
		   enddo
		   call calcnumall(ixg,iyg,cnum,iangle,size,ixjus,iyjus,
     &	   ixlo,iylo,ixhi,iyhi,idim,n,cexp,inumxy,logxy)
		   call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		   do i=1,n
			call coltyp18(colors(I))
			if(jdraw(i).ne.0) then
	            call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &            sizen,ixg(i),
     &            iyg(i),ixjus(i),iyjus(i),iangle(i),logxy(i),inumxy(i))
			endif
			if(jdraw(i).eq.-1) then
      	      call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            iyhi(i),colors(i))
			endif
		   enddo
		endif
		if(ival.eq.73) then	!PgDn:rotate clockwise
		   if(first.eq.1) then
		      do i=1,n
		         call coltyp18(ibk)
		   	call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),iyg(i),ixjus(i),iyjus(i),
     &               iangle(i),logxy(i),inumxy(i))
			if(jdraw(i).eq.-1) then
      	      call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            iyhi(i),ibk)
			endif
		      enddo
		      first=0
		      goto 250
		   endif
		   call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
250            continue
		   do i=1,n
		   iangle(i)=iangle(i)+ia
		   enddo
		   call calcnumall(ixg,iyg,cnum,iangle,size,ixjus,iyjus,
     &	   ixlo,iylo,ixhi,iyhi,idim,n,cexp,inumxy,logxy)
		   call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		   do i=1,n
			call coltyp18(colors(I))
			if(jdraw(i).ne.0) then
	            call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &            sizen,ixg(i),
     &            iyg(i),ixjus(i),iyjus(i),iangle(i),logxy(i),inumxy(i))
			endif
			if(jdraw(i).eq.-1) then
      	      call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            iyhi(i),colors(i))
			endif
		   enddo
		endif
		if(ival.eq.81) then	!PgUp:rotate anticlockwise
		   if(first.eq.1) then
		      do i=1,n
		         call coltyp18(ibk)
		   	call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),iyg(i),ixjus(i),iyjus(i),
     &               iangle(i),logxy(i),inumxy(i))
			if(jdraw(i).eq.-1) then
      	      call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            iyhi(i),ibk)
			endif
		      enddo
		      first=0
		      goto 251
		   endif
		   call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
251		   continue
		   do i=1,n
		   iangle(i)=iangle(i)-ia
		   enddo
		   call calcnumall(ixg,iyg,cnum,iangle,size,ixjus,iyjus,
     &	   ixlo,iylo,ixhi,iyhi,idim,n,cexp,inumxy,logxy)
		   call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		   do i=1,n
			call coltyp18(colors(I))
			if(jdraw(i).ne.0) then
	            call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &            sizen,ixg(i),
     &            iyg(i),ixjus(i),iyjus(i),iangle(i),logxy(i),inumxy(i))
			endif
			if(jdraw(i).eq.-1) then
      	      call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            iyhi(i),colors(i))
			endif
		   enddo
		endif
		if(ival.eq.79) then     ! End:increase color
		   if(imode.ne.4) goto 1
		   do i=1,n
		      if(colors(i).lt.15) colors(i)=colors(i)+1
		   enddo
		   do i=i,n
			call coltyp18(colors(i))
			if(jdraw(i).ne.0) then
	            call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &            sizen,ixg(i),
     &            iyg(i),ixjus(i),iyjus(i),iangle(i),logxy(i),inumxy(i))
			endif
			if(jdraw(i).eq.-1) then
      	      call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            iyhi(i),colors(i))
			endif
		   enddo
		endif
		if(ival.eq.71) then     ! Home:decrease color
		   if(imode.ne.4) goto 1
		   do i=1,n
		      if(colors(i).gt.0) colors(i)=colors(i)-1
		   enddo
		   do i=1,n
			call coltyp18(colors(i))
			if(jdraw(i).ne.0) then
	            call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &            sizen,ixg(i),
     &            iyg(i),ixjus(i),iyjus(i),iangle(i),logxy(i),inumxy(i))
			endif
			if(jdraw(i).eq.-1) then
      	      call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            iyhi(i),colors(i))
			endif
		   enddo
		endif
		if(ival.eq.82) then     ! Ins:toggle sensitivity
		   if(im.eq.16) then
			im=100
			cs=1.
			ia=45
		   else
		      im=16
		      cs=0.2
			ia=5
		   endif
		endif
		if(ival.eq.83) then     ! DEL:toggle idraw
		   do i=1,n
			if(jdraw(i).eq.0) then
			   jdraw(i)=1
			   call coltyp18(colors(i))
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),iyg(i),ixjus(i),
     &		   iyjus(i),iangle(i),logxy(i),inumxy(i))
			else
			   jdraw(i)=0
			   call coltyp18(ibk)
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),iyg(i),ixjus(i),
     &		   iyjus(i),iangle(i),logxy(i),inumxy(i))
			endif
		   enddo
		endif
		goto 1
	   else if(ktype.eq.3.or.ktype.eq.0) then
		if(imode.ne.4) goto 1
		if(ival.eq.43) then        ! +: increase size
			if(first.eq.1) then
			call coltyp18(ibk)
			do i=1,n
	      	   call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),iyg(i),ixjus(i),iyjus(i),
     &               iangle(i),logxy(i),inumxy(i))
      	         call drawframe(ixlo(i),iylo(i),ixhi(i),
     &               iyhi(i),ibk)
			   if(jdraw(i).eq.-1) then
      	      	call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            	iyhi(i),ibk)
			   endif
			enddo
			first=0
			goto 45
			endif
			call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
45		      continue
	            sizen=sizen+cs		!plus
			do i=1,n
	   		   call SETCSIZE(sizen,size(i))		!set isize
			enddo
			csfac=sizen/csize
			isize=size(1)
			ih=charheight(isize)
			iw=charwidth(isize)
			ixoff=-ifix(float(iw)/5.)	!for numbers in exponent form
			iyoff=ih/2
		      call calcnumall(ixg,iyg,cnum,iangle,size,ixjus,iyjus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n,cexp,inumxy,logxy)
			call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
			do i=1,n
			   call coltyp18(colors(i))
			   if(jdraw(i).ne.0) then
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),iyg(i),ixjus(i),iyjus(i),
     &               iangle(i),logxy(i),inumxy(i))
			   endif
			  if(jdraw(i).eq.-1) then
      	        call drawframe(ixlo(i),iylo(i),ixhi(i),
     &              iyhi(i),colors(i))
			  endif
			enddo
	      else if(ival.eq.45) then   ! -: decrease size
			if(first.eq.1) then
			call coltyp18(ibk)
			do i=1,n
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),iyg(i),ixjus(i),iyjus(i),
     &               iangle(i),logxy(i),inumxy(i))
      	         call drawframe(ixlo(i),iylo(i),ixhi(i),
     &               iyhi(i),ibk)
			   if(jdraw(i).eq.-1) then
      	      	call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            	iyhi(i),ibk)
			   endif
			enddo
			first=0
			goto 55
			endif
			call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
55		      continue
	            sizen=sizen-cs		!plus
			do i=1,n
	   		   call SETCSIZE(sizen,size(i))		!set isize
			enddo
			csfac=sizen/csize
			isize=size(1)
			ih=charheight(isize)
			iw=charwidth(isize)
			ixoff=-ifix(float(iw)/5.)	!for numbers in exponent form
			iyoff=ih/2
		      call calcnumall(ixg,iyg,cnum,iangle,size,ixjus,iyjus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n,cexp,inumxy,logxy)
			call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
			do i=1,n
			   if(jdraw(i).ne.0) then
			   call coltyp18(colors(I))
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),iyg(i),ixjus(i),iyjus(i),
     &               iangle(i),logxy(i),inumxy(i))
			   endif
			   if(jdraw(i).eq.-1) then
      	        	call drawframe(ixlo(i),iylo(i),ixhi(i),
     &              	iyhi(i),colors(i))
			   endif
			enddo
		endif
		goto 1
	   else if(ktype.eq.2) then
		if(imode.ne.4) goto 1
		if(ival.eq.98) then     ! b:toggle draw box
		   do i=1,n
		   if(jdraw(i).eq.-1) then
		      jdraw(i)=1
      	      call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            iyhi(i),ibk)
		   else if(jdraw(i).eq.1) then
		      jdraw(i)=-1
      	      call drawframe(ixlo(i),iylo(i),ixhi(i),
     &            iyhi(i),colors(i))
		   endif
		   enddo
		   goto 1
		endif
		if(ival.eq.42) then        ! +: increase font
			if(first.eq.1) then
			call coltyp18(ibk)
			do i=1,n
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),iyg(i),ixjus(i),iyjus(i),
     &               iangle(i),logxy(i),inumxy(i))
			   if(jdraw(i).eq.-1) then
      	         	call drawframe(ixlo(i),iylo(i),ixhi(i),
     &               	iyhi(i),ibk)
			   endif
			enddo
			first=0
			goto 85
			endif
			call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
85		      continue
			ifont=ifont+1
			call setfont1(ifont)
		      call calcnumall(ixg,iyg,cnum,iangle,size,ixjus,iyjus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n,cexp,inumxy,logxy)
			call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
			call setfont1(ifont)
			do i=1,n
			   call coltyp18(colors(i))
			   if(jdraw(i).ne.0) then
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),iyg(i),ixjus(i),iyjus(i),
     &               iangle(i),logxy(i),inumxy(i))
			   endif
			   if(jdraw(i).eq.-1) then
      	        	call drawframe(ixlo(i),iylo(i),ixhi(i),
     &              	iyhi(i),colors(i))
			   endif
			enddo
	      else if(ival.eq.47) then   ! -: decrease font
			if(first.eq.1) then
			call coltyp18(ibk)
			do i=1,n
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),iyg(i),ixjus(i),iyjus(i),
     &               iangle(i),logxy(i),inumxy(i))
			   if(jdraw(i).eq.-1) then
      	         	call drawframe(ixlo(i),iylo(i),ixhi(i),
     &               	iyhi(i),ibk)
			   endif
			enddo
			first=0
			goto 95
			endif
			call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
95		      continue
			ifont=ifont-1
			call setfont1(ifont)
		      call calcnumall(ixg,iyg,cnum,iangle,size,ixjus,iyjus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n,cexp,inumxy,logxy)
			call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
			call setfont1(ifont)
			do i=1,n
			   call coltyp18(colors(I))
			   if(jdraw(i).ne.0) then
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),iyg(i),ixjus(i),iyjus(i),
     &               iangle(i),logxy(i),inumxy(i))
			   endif
			   if(jdraw(i).eq.-1) then
      	        	call drawframe(ixlo(i),iylo(i),ixhi(i),
     &              	iyhi(i),colors(i))
			   endif
			enddo
		endif
		goto 1
	   endif
	endif       ! end if(kbhit())
	goto 1
99    continue
	deallocate(arrayn)
990	select case(imode)
	 case(1)
	    i=1
	    do k=1,numbx
		 j=k+30
		 rx(j)=xworld(ixg(i))
		 ry(j)=yworld(iyg(i))
		 angle(j)=iangle(i)
		 idraw(j)=jdraw(i)
		 rxbox(1,j)=xworld(ixlo(i))
		 rybox(1,j)=yworld(iylo(i))
		 rxbox(2,j)=xworld(ixlo(i))
		 rybox(2,j)=yworld(iyhi(i))
		 rxbox(3,j)=xworld(ixhi(i))
		 rybox(3,j)=yworld(iyhi(i))
		 rxbox(4,j)=xworld(ixhi(i))
		 rybox(4,j)=yworld(iylo(i))
		 i=i+1
	    enddo
	    do k=1,numby
		 j=k+55
		 rx(j)=xworld(ixg(i))
		 ry(j)=yworld(iyg(i))
		 angle(j)=iangle(i)
		 idraw(j)=jdraw(i)
		 rxbox(1,j)=xworld(ixlo(i))
		 rybox(1,j)=yworld(iylo(i))
		 rxbox(2,j)=xworld(ixlo(i))
		 rybox(2,j)=yworld(iyhi(i))
		 rxbox(3,j)=xworld(ixhi(i))
		 rybox(3,j)=yworld(iyhi(i))
		 rxbox(4,j)=xworld(ixhi(i))
		 rybox(4,j)=yworld(iylo(i))
		 i=i+1
	    enddo
	 case(2)
	    do i=1,numbx
		 j=i+30
		 rx(j)=xworld(ixg(i))
		 ry(j)=yworld(iyg(i))
		 angle(j)=iangle(i)
		 idraw(j)=jdraw(i)
		 rxbox(1,j)=xworld(ixlo(i))
		 rybox(1,j)=yworld(iylo(i))
		 rxbox(2,j)=xworld(ixlo(i))
		 rybox(2,j)=yworld(iyhi(i))
		 rxbox(3,j)=xworld(ixhi(i))
		 rybox(3,j)=yworld(iyhi(i))
		 rxbox(4,j)=xworld(ixhi(i))
		 rybox(4,j)=yworld(iylo(i))
	    enddo
	 case(3)
	    do i=1,numby
		 j=i+55
		 rx(j)=xworld(ixg(i))
		 ry(j)=yworld(iyg(i))
		 angle(j)=iangle(i)
		 idraw(j)=jdraw(i)
		 rxbox(1,j)=xworld(ixlo(i))
		 rybox(1,j)=yworld(iylo(i))
		 rxbox(2,j)=xworld(ixlo(i))
		 rybox(2,j)=yworld(iyhi(i))
		 rxbox(3,j)=xworld(ixhi(i))
		 rybox(3,j)=yworld(iyhi(i))
		 rxbox(4,j)=xworld(ixhi(i))
		 rybox(4,j)=yworld(iylo(i))
	    enddo
	 case(4)
	    icol=colors(1)
	    i=1
	    do k=1,numbx
		 j=k+30
		 rx(j)=xworld(ixg(i))
		 ry(j)=yworld(iyg(i))
		 angle(j)=iangle(i)
		 idraw(j)=jdraw(i)
		 rxbox(1,j)=xworld(ixlo(i))
		 rybox(1,j)=yworld(iylo(i))
		 rxbox(2,j)=xworld(ixlo(i))
		 rybox(2,j)=yworld(iyhi(i))
		 rxbox(3,j)=xworld(ixhi(i))
		 rybox(3,j)=yworld(iyhi(i))
		 rxbox(4,j)=xworld(ixhi(i))
		 rybox(4,j)=yworld(iylo(i))
		 i=i+1
	    enddo
	    do k=1,numby
		 j=k+55
		 rx(j)=xworld(ixg(i))
		 ry(j)=yworld(iyg(i))
		 angle(j)=iangle(i)
		 idraw(j)=jdraw(i)
		 rxbox(1,j)=xworld(ixlo(i))
		 rybox(1,j)=yworld(iylo(i))
		 rxbox(2,j)=xworld(ixlo(i))
		 rybox(2,j)=yworld(iyhi(i))
		 rxbox(3,j)=xworld(ixhi(i))
		 rybox(3,j)=yworld(iyhi(i))
		 rxbox(4,j)=xworld(ixhi(i))
		 rybox(4,j)=yworld(iylo(i))
		 i=i+1
	    enddo
	end select
	deallocate(ixlo,iylo,ixhi,iyhi,idim,size)
	deallocate(iangle,ixg,iyg,colors,ixjus,iyjus)

	end

c=======================================================================

