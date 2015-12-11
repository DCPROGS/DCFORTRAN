c*************************************************************************
	subroutine allmove(str,n,ixg,iyg,xjus,yjus,colors,angle,
     &			ifont,ibk,instr,index,csfac,csize,logxy,inumxy)
c*************************************************************************
c	subroutine to move ALL the strings STR=INSTR(+INDEX) at a time ,
C	written with HGRAPH
c	to draw the numbers uses JUSTIFYSTRING because now we move the resulting
c	string ; to change size uses JUSTSTR to maintain the proportion for
c     exponent/index size!
c	To each number corresponds a window , so we read and write n-windows at
c	a movement! so the problem with a two-dimensions allocatable array
c     ( M-dim maybe!) was solved in READALL and WRITALL ; 
c 	now we are able to design some crazy cards'games!
c     Other calls:CALCMAX : calculates maximum dimension for the array
c		     :CALCALL : calculates n-windows' dimensions
c*************************************************************************
c	INPUT:
c	ixg,iyg 	: strings' display coordinates (0-10000,0-7500)
c	angle  	: strings' angles
c	colors  	: strings' colors
c	ifont  	: strings' font (all same font!)
c	csize 	: strings' real size (all same size!)
c	csfac       : size factor
c	xjus,yjus   : strings'justification
c	instr		: input strings
c	index		: index/exponent for strings
c	logxy		: .true.-> write exponent
c	numxy       :   = 0 -> write only instr
c	            :   = 1 -> write with exponent
c			:   = 2 -> write with index
C	outstr      : composed strings ( returned by JUSTSTR)
c	ibk		: background's color
c
c	OUTPUT      :
c	ixg,iyg 	: strings' new display coordinates (0-10000,0-7500)
c	angle  	: strings' new angles
c	colors  	: strings' new colors
c	ifont  	: strings' new font (all same font!)
c	csfac       : new size factor
c========================================================================

	integer*4		size(:),ixg(n),iyg(n)
	integer*4		colors(n),xjus(n),yjus(n)
	integer*4		angle(n)
	integer*4		ixlo(:),iylo(:),ixhi(:),iyhi(:),idim(:)
	integer*1		array1(:,:)
	character*(*)	str(n),instr(n),index(n)
	character*55 	helpst(30)		!help strings
	character*1 	ans,getch
	logical		kbhit
	allocatable		array1,ixlo,iylo,ixhi,iyhi,idim,size
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
	cs=1.              ! size
	first=1
	allocate(ixlo(n),iylo(n),ixhi(n),iyhi(n),idim(n),size(n))
	sizen=csfac*csize
	do i=1,n
	   call SETCSIZE(sizen,size(i))		!set isize
	enddo

	call calcmax(ixg,iyg,str,angle,size,xjus,yjus,n,max,msize)
	max=int((28/msize)*max)
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
			cs=1.
		   else
		      im=16
		      ia=5
		      cs=0.1
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
	            sizen=sizen+cs		!plus
			do i=1,n
	   		   call SETCSIZE(sizen,size(i))		!set isize
			enddo
			csfac=sizen/csize
			call calcall(ixg,iyg,str,angle,size,xjus,yjus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n)
			call readall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max) !display
			do i=1,n
			   call coltyp18(colors(i))
			   call juststr(ixg(i),iyg(i),str(i),angle(i),sizen,
     &		   xjus(i),yjus(i),instr(i),index(i),
     & 		   logxy,inumxy)
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
	            sizen=sizen-cs		!plus
			do i=1,n
	   		   call SETCSIZE(sizen,size(i))		!set isize
			enddo
			csfac=sizen/csize
			call calcall(ixg,iyg,str,angle,size,xjus,yjus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n)
			call readall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max) !display
			do i=1,n
			   call coltyp18(colors(I))
			   call juststr(ixg(i),iyg(i),str(i),angle(i),sizen,
     &		   xjus(i),yjus(i),instr(i),index(i),
     & 		   logxy,inumxy)
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
			ifont=ifont+1
			call setfont1(ifont)
			call calcall(ixg,iyg,str,angle,size,xjus,yjus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n)
			call readall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max) !display
c			call setfont1(ifont)
			do i=1,n
			   call coltyp18(colors(i))
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
			ifont=ifont-1
			call setfont1(ifont)
			call calcall(ixg,iyg,str,angle,size,xjus,yjus,
     &	      ixlo,iylo,ixhi,iyhi,idim,n)
			call readall(ixlo,iylo,ixhi,iyhi,idim,array1,n,max) !display
c			call setfont1(ifont)
			do i=1,n
			   call coltyp18(colors(I))
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

