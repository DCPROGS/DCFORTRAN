c*************************************************************************
	subroutine moveall(str,size,n,ixg,iyg,xjus,yjus,colors,angle,
     &			font,ibk)
c*************************************************************************
c	subroutine to move ALL the strings STR at a time , written with HGRAPH
c*************************************************************************


	integer*4		size(n),ixg(n),iyg(n)
	integer*4		colors(n),xjus(n),yjus(n),font(n)
	integer*4		angle(n)
	integer*4		ixlo(:),iylo(:),ixhi(:),iyhi(:),idim(:)
	integer*1		array1(:,:)
	character*(*)	str(n)
	character*55 	helpst(30)		!help strings
	character*1 	ans,getch
	logical		kbhit
	allocatable		array1,ixlo,iylo,ixhi,iyhi,idim
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
	helpst(9)='    PgUp: increase angle'
	helpst(10)='    PgDn: decrease angle'
	helpst(11)='     Ins: toggle sensitivity'
	helpst(12)='    Home: decrease color'
	helpst(13)='     End: increase color'
	helpst(14)='    <CR>: end selection'
c============================================================
	nhelp=14
	icq=14     		! color help menu
	im=125            ! movement
	ia=45             ! angle
	first=1
	allocate(ixlo(n),iylo(n),ixhi(n),iyhi(n),idim(n))

	call calcmax(ixg,iyg,str,angle,size,xjus,yjus,n,max,msize)
	max=int((14/msize)*max)
	allocate(array1(n,max))
1	continue
	if(kbhit()) then
	   ans=getch(ktype)
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
		   	call coltyp18(ibk)
			do i=1,n
			   call justifystring(ixg(i),iyg(i),
     &		   str(i),angle(i),size(i),xjus(i),yjus(i))
			enddo
		      first=0
		      goto 21
		   endif
		   call writall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max)
21             continue
		   do i=1,n
		   	ixg(i)=ixg(i)+im
		   enddo
		   call calcall(ixg,iyg,str,angle,size,xjus,yjus,
     &	   ixlo,iylo,ixhi,iyhi,idim,n)
		   call readall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max) !display
		   do i=1,n
		      call coltyp18(colors(i))
	            call justifystring(ixg(i),iyg(i),
     &	      str(i),angle(i),size(i),xjus(i),yjus(i))
		   enddo
		endif
		if(ival.eq.75) then		!shift left
		   if(first.eq.1) then
		   	call coltyp18(ibk)
			do i=1,n
			   call justifystring(ixg(i),iyg(i),
     &		   str(i),angle(i),size(i),xjus(i),yjus(i))
			enddo
		      first=0
		      goto 22
		   endif
		   call writall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max)
22		   continue
		   do i=1,n
		      ixg(i)=ixg(i)-im
		   enddo
		   call calcall(ixg,iyg,str,angle,size,xjus,yjus,
     &	   ixlo,iylo,ixhi,iyhi,idim,n)
		   call readall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max) !display
		   do i=1,n
		      call coltyp18(colors(i))
		      call justifystring(ixg(i),iyg(i),
     &	      str(i),angle(i),size(i),xjus(i),yjus(i))
		   enddo
		endif
		if(ival.eq.72) then	!shift up
		   if(first.eq.1) then
		   	call coltyp18(ibk)
			do i=1,n
			   call justifystring(ixg(i),iyg(i),
     &		   str(i),angle(i),size(i),xjus(i),yjus(i))
			enddo
			first=0
			goto 23
		   endif
		   call writall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max)
23		   continue
		   do i=1,n
			iyg(i)=iyg(i)+im
		   enddo
		   call calcall(ixg,iyg,str,angle,size,xjus,yjus,
     &	   ixlo,iylo,ixhi,iyhi,idim,n)
		   call readall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max) !display
		   do i=1,n
			call coltyp18(colors(I))
			call justifystring(ixg(i),iyg(i),
     &		str(i),angle(i),size(i),xjus(i),yjus(i))
		   enddo
		endif
		if(ival.eq.80) then	!shift down
		   if(first.eq.1) then
		   	call coltyp18(ibk)
			do i=1,n
			   call justifystring(ixg(i),iyg(i),
     &		   str(i),angle(i),size(i),xjus(i),yjus(i))
			enddo
			first=0
			goto 24
		   endif
		   call writall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max)
24		   continue
		   do i=1,n
			iyg(i)=iyg(i)-im
		   enddo
		   call calcall(ixg,iyg,str,angle,size,xjus,yjus,
     &	   ixlo,iylo,ixhi,iyhi,idim,n)
		   call readall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max) !display
		   do i=1,n
			call coltyp18(colors(I))
			call justifystring(ixg(i),iyg(i),
     &		str(i),angle(i),size(i),xjus(i),yjus(i))
		   enddo
		endif
		if(ival.eq.73) then	!PgDn:rotate clockwise
		   if(first.eq.1) then
		   	call coltyp18(ibk)
			do i=1,n
			   call justifystring(ixg(i),iyg(i),
     &		   str(i),angle(i),size(i),xjus(i),yjus(i))
			enddo
			first=0
			goto 25
		   endif
		   call writall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max)
25		   continue
		   do i=1,n
			angle(i)=angle(i)+ia
		   enddo
		   call calcall(ixg,iyg,str,angle,size,xjus,yjus,
     &	   ixlo,iylo,ixhi,iyhi,idim,n)
		   call readall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max) !display
		   do i=1,n
			call coltyp18(colors(I))
			call justifystring(ixg(i),iyg(i),
     &		str(i),angle(i),size(i),xjus(i),yjus(i))
		   enddo
		endif
		if(ival.eq.81) then	!PgUp:rotate anticlockwise
		   if(first.eq.1) then
		   	call coltyp18(ibk)
			do i=1,n
			   call justifystring(ixg(i),iyg(i),
     &		   str(i),angle(i),size(i),xjus(i),yjus(i))
			enddo
			first=0
			goto 125
		   endif
		   call writall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max)
125		   continue
		   do i=1,n
			angle(i)=angle(i)-ia
		   enddo
		   call calcall(ixg,iyg,str,angle,size,xjus,yjus,
     &	   ixlo,iylo,ixhi,iyhi,idim,n)
		   call readall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max) !display
		   do i=1,n
			call coltyp18(colors(I))
			call justifystring(ixg(i),iyg(i),
     &		str(i),angle(i),size(i),xjus(i),yjus(i))
		   enddo
		endif
		if(ival.eq.79) then     ! End:increase color
		   do i=1,n
		      if(colors(i).lt.15) colors(i)=colors(i)+1
		   enddo
		   do i=1,n
			call coltyp18(colors(i))
			call justifystring(ixg(i),iyg(i),
     &		str(i),angle(i),size(i),xjus(i),yjus(i))
		   enddo
		endif
		if(ival.eq.71) then     ! Home:decrease color
		   do i=1,n
		      if(colors(i).gt.1) colors(i)=colors(i)-1
		   enddo
		   do i=1,n
			call coltyp18(colors(i))
			call justifystring(ixg(i),iyg(i),
     &		str(i),angle(i),size(i),xjus(i),yjus(i))
		   enddo
		endif
		if(ival.eq.82) then     ! Ins:toggle sensitivity
		   if(im.eq.10) then
			im=125
			ia=45
		   else
		      im=16
		      ia=5
		   endif
		endif
		goto 1
	   else if(ktype.eq.3.or.ktype.eq.0) then
		if(ival.eq.43) then        ! +: increase size
			if(first.eq.1) then
			call coltyp18(ibk)
			do i=1,n
			   call justifystring(ixg(i),iyg(i),
     &		   str(i),angle(i),size(i),xjus(i),yjus(i))
			enddo
			first=0
			goto 45
			endif
			call writall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max)
45		      continue
			do i=1,n
			   if (size(i).lt.7) size(i)=size(i)+1
			enddo
			call calcall(ixg,iyg,str,angle,size,xjus,yjus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n)
			call readall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max) !display
			do i=1,n
			   call coltyp18(colors(i))
			   call justifystring(ixg(i),iyg(i),
     &		   str(i),angle(i),size(i),xjus(i),yjus(i))
			enddo
	      else if(ival.eq.45) then   ! -: decrease size
			if(first.eq.1) then
			call coltyp18(ibk)
			do i=1,n
			   call justifystring(ixg(i),iyg(i),
     &		   str(i),angle(i),size(i),xjus(i),yjus(i))
			enddo
			first=0
			goto 55
			endif
			call writall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max)
55		      continue
			do i=1,n
			   if (size(i).gt.0) size(i)=size(i)-1
			enddo
			call calcall(ixg,iyg,str,angle,size,xjus,yjus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n)
			call readall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max) !display
			do i=1,n
			   call coltyp18(colors(I))
			   call justifystring(ixg(i),iyg(i),
     &		   str(i),angle(i),size(i),xjus(i),yjus(i))
			enddo
		endif
		goto 1
	   else if(ktype.eq.2) then
		if(ival.eq.42) then        ! +: increase font
			if(first.eq.1) then
			call coltyp18(ibk)
			do i=1,n
			   call justifystring(ixg(i),iyg(i),
     &		   str(i),angle(i),size(i),xjus(i),yjus(i))
			enddo
			first=0
			goto 85
			endif
			call writall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max)
85		      continue
			do i=1,n
c			   if (font(i).lt.7)
			   font(i)=font(i)+1
			   call setfont1(font(i))
			enddo
			call calcall(ixg,iyg,str,angle,size,xjus,yjus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n)
			call readall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max) !display
			do i=1,n
			   call coltyp18(colors(i))
			   call setfont1(font(i))
			   call justifystring(ixg(i),iyg(i),
     &		   str(i),angle(i),size(i),xjus(i),yjus(i))
			enddo
	      else if(ival.eq.47) then   ! -: decrease font
			if(first.eq.1) then
			call coltyp18(ibk)
			do i=1,n
			   call justifystring(ixg(i),iyg(i),
     &		   str(i),angle(i),size(i),xjus(i),yjus(i))
			enddo
			first=0
			goto 95
			endif
			call writall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max)
95		      continue
			do i=1,n
c			   if (font(i).gt.0)
			   font(i)=font(i)-1
			   call setfont1(font(i))
			enddo
			call calcall(ixg,iyg,str,angle,size,xjus,yjus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n)
			call readall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max) !display
			do i=1,n
			   call coltyp18(colors(I))
			   call setfont1(font(i))
			   call justifystring(ixg(i),iyg(i),
     &		   str(i),angle(i),size(i),xjus(i),yjus(i))
			enddo
		endif
		goto 1
	   endif
	endif       ! end if(kbhit())
	goto 1
99    continue
	deallocate(array1,ixlo,iylo,ixhi,iyhi,idim)

	end

c============================================================================

