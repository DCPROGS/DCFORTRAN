c***********************************************************************
	subroutine movenum(cnum,csfac,csize,n,ixg,iyg,xjus,yjus,colors,
     &			angle,ifont,ibk,cexp,inumxy,logxy)
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
c		     :CALCNUM : calculates n-window's dimensions
c***********************************************************************


	integer*4		size(:),ixg(n),iyg(n)
	integer*4		colors(n),xjus(n),yjus(n),ifont
	integer*4		angle(n)
	integer*4		ixlo(:),iylo(:),ixhi(:),iyhi(:),idim(:)
	integer*1		arrayn(:,:)
	integer 	      charheight,charwidth
	character*(*)	cnum(n),cexp(n)
	character*55 	helpst(30)		!help strings
	character*1 	ans,getch
	logical		kbhit,logxy
	allocatable		size,arrayn,ixlo,iylo,ixhi,iyhi,idim

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
	im=125            ! movement
	cs=1.              ! size
	first=1
	allocate(ixlo(n),iylo(n),ixhi(n),iyhi(n),idim(n),size(n))
	sizen=csfac*csize
	do i=1,n
	   call SETCSIZE(sizen,size(i))		!set isize
	enddo
	isize=size(1)
	call SETFONT1(ifont)		    	!set font
	ih=charheight(isize)
	iw=charwidth(isize)
	ixoff=-ifix(float(iw)/5.)	!for numbers in exponent form
	iyoff=ih/2

	call calcmax(ixg,iyg,cnum,angle,size,xjus,yjus,n,max,msize)
	max=int((28/msize)*max)
	allocate(arrayn(1:n,1:max))
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
		   if(first.eq.1) then
		   	call coltyp18(ibk)
			do i=1,n
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),
     &               iyg(i),xjus(i),xjus(i),angle(i),logxy,inumxy)
			enddo
		      first=0
		      goto 21
		   endif
		   call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
21             continue
		   do i=1,n
		   	ixg(i)=ixg(i)+im
		   enddo
		   call calcnum(ixg,iyg,cnum,angle,size,xjus,yjus,
     &	   ixlo,iylo,ixhi,iyhi,idim,n,cexp,inumxy,logxy)
		   call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		   do i=1,n
		      call coltyp18(colors(i))
	            call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &            sizen,ixg(i),
     &            iyg(i),xjus(i),xjus(i),angle(i),logxy,inumxy)
		   enddo
		endif
		if(ival.eq.75) then		!shift left
		   if(first.eq.1) then
		   	call coltyp18(ibk)
			do i=1,n
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),
     &               iyg(i),xjus(i),xjus(i),angle(i),logxy,inumxy)
			enddo
		      first=0
		      goto 22
		   endif
		   call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
22		   continue
		   do i=1,n
		      ixg(i)=ixg(i)-im
		   enddo
		   call calcnum(ixg,iyg,cnum,angle,size,xjus,yjus,
     &	   ixlo,iylo,ixhi,iyhi,idim,n,cexp,inumxy,logxy)
		   call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		   do i=1,n
		      call coltyp18(colors(i))
	            call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &            sizen,ixg(i),
     &            iyg(i),xjus(i),xjus(i),angle(i),logxy,inumxy)
		   enddo
		endif
		if(ival.eq.72) then	!shift up
		   if(first.eq.1) then
		   	call coltyp18(ibk)
			do i=1,n
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),
     &               iyg(i),xjus(i),xjus(i),angle(i),logxy,inumxy)
			enddo
			first=0
			goto 23
		   endif
		   call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
23		   continue
		   do i=1,n
			iyg(i)=iyg(i)+im
		   enddo
		   call calcnum(ixg,iyg,cnum,angle,size,xjus,yjus,
     &	   ixlo,iylo,ixhi,iyhi,idim,n,cexp,inumxy,logxy)
		   call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		   do i=1,n
			call coltyp18(colors(I))
	            call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &            sizen,ixg(i),
     &            iyg(i),xjus(i),xjus(i),angle(i),logxy,inumxy)
		   enddo
		endif
		if(ival.eq.80) then	!shift down
		   if(first.eq.1) then
		   	call coltyp18(ibk)
			do i=1,n
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),
     &               iyg(i),xjus(i),xjus(i),angle(i),logxy,inumxy)
			enddo
			first=0
			goto 24
		   endif
		   call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
24		   continue
		   do i=1,n
			iyg(i)=iyg(i)-im
		   enddo
		   call calcnum(ixg,iyg,cnum,angle,size,xjus,yjus,
     &	   ixlo,iylo,ixhi,iyhi,idim,n,cexp,inumxy,logxy)
		   call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
		   do i=1,n
			call coltyp18(colors(I))
	            call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &            sizen,ixg(i),
     &            iyg(i),xjus(i),xjus(i),angle(i),logxy,inumxy)
		   enddo
		endif
		if(ival.eq.79) then     ! End:increase color
		   do i=1,n
		      if(colors(i).lt.15) colors(i)=colors(i)+1
		   enddo
		   do i=1,n
			call coltyp18(colors(i))
	            call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &            sizen,ixg(i),
     &            iyg(i),xjus(i),xjus(i),angle(i),logxy,inumxy)
		   enddo
		endif
		if(ival.eq.71) then     ! Home:decrease color
		   do i=1,n
		      if(colors(i).gt.1) colors(i)=colors(i)-1
		   enddo
		   do i=1,n
			call coltyp18(colors(i))
	            call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &            sizen,ixg(i),
     &            iyg(i),xjus(i),xjus(i),angle(i),logxy,inumxy)
		   enddo
		endif
		if(ival.eq.82) then     ! Ins:toggle sensitivity
		   if(im.eq.10) then
			im=125
			cs=1.
		   else
		      im=16
		      cs=0.1
		   endif
		endif
		goto 1
	   else if(ktype.eq.3.or.ktype.eq.0) then
		if(ival.eq.43) then        ! +: increase size
			if(first.eq.1) then
			call coltyp18(ibk)
			do i=1,n
	      	   call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),
     &               iyg(i),xjus(i),xjus(i),angle(i),logxy,inumxy)
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
		      call calcnum(ixg,iyg,cnum,angle,size,xjus,yjus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n,cexp,inumxy,logxy)
			call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
			do i=1,n
			   call coltyp18(colors(i))
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),
     &               iyg(i),xjus(i),xjus(i),angle(i),logxy,inumxy)
			enddo
	      else if(ival.eq.45) then   ! -: decrease size
			if(first.eq.1) then
			call coltyp18(ibk)
			do i=1,n
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),
     &               iyg(i),xjus(i),xjus(i),angle(i),logxy,inumxy)
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
		      call calcnum(ixg,iyg,cnum,angle,size,xjus,yjus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n,cexp,inumxy,logxy)
			call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
			do i=1,n
			   call coltyp18(colors(I))
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),
     &               iyg(i),xjus(i),xjus(i),angle(i),logxy,inumxy)
			enddo
		endif
		goto 1
	   else if(ktype.eq.2) then
		if(ival.eq.42) then        ! +: increase font
			if(first.eq.1) then
			call coltyp18(ibk)
			do i=1,n
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),
     &               iyg(i),xjus(i),xjus(i),angle(i),logxy,inumxy)
			enddo
			first=0
			goto 85
			endif
			call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
85		      continue
			ifont=ifont+1
			call setfont1(ifont)
		      call calcnum(ixg,iyg,cnum,angle,size,xjus,yjus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n,cexp,inumxy,logxy)
			call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
			call setfont1(ifont)
			do i=1,n
			   call coltyp18(colors(i))
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),
     &               iyg(i),xjus(i),xjus(i),angle(i),logxy,inumxy)
			enddo
	      else if(ival.eq.47) then   ! -: decrease font
			if(first.eq.1) then
			call coltyp18(ibk)
			do i=1,n
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),
     &               iyg(i),xjus(i),xjus(i),angle(i),logxy,inumxy)
			enddo
			first=0
			goto 95
			endif
			call writall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max)
95		      continue
			ifont=ifont-1
			call setfont1(ifont)
		      call calcnum(ixg,iyg,cnum,angle,size,xjus,yjus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n,cexp,inumxy,logxy)
			call readall(ixlo,iylo,ixhi,iyhi,idim,arrayn,n,max) !display
			call setfont1(ifont)
			do i=1,n
			   call coltyp18(colors(I))
	               call iDRAWNUM(cnum(i),cexp(i),ixoff,iyoff,
     &               sizen,ixg(i),
     &               iyg(i),xjus(i),xjus(i),angle(i),logxy,inumxy)
			enddo
		endif
		goto 1
	   endif
	endif       ! end if(kbhit())
	goto 1
99    continue
	deallocate(arrayn,ixlo,iylo,ixhi,iyhi,idim,size)

	end

c=======================================================================

