c************************************************************************
	subroutine ilvfont(ix,iy,ourstring,irot,isize,justx,justy,
     &			icolor,ifont,ibk)

c************************************************************************
c
c	Soubroutine to simulate READSTRING(IX,IY,CAPTION,ANGLE,SIZE)
c     ix:         x string
c	iy:         y string
c	ourstring:  caption
c	irot:       angle
c     isize:      size
c     justx:      horizontal justification
c	justy:      vertical justification
c	icolor:	color string
c	ibk:        color background
c
c************************************************************************
	character*1 	char1,char2,char3,getch
	character     ourstring*(*),transit(60)
	character*55 	helpst(30)		!help strings
	integer*1		arra(:)
	logical	back
	allocatable :: arra
	ixlo=280
	iylo=440
	ixhi=360
	iyhi=480
	idimen=(iyhi-iylo+9)*(ixhi-ixlo+9)+1
	allocate(arra(idimen))
	call rdgwin1(ixlo,iylo,ixhi,iyhi,arra)
	call framefill(ixlo,iylo,ixhi,iyhi,15,4)
	call WRSTRING18(288,448,'F1=HELP',15,4)
	back=.false.

	helpst(1)='^F[m]   :change current font setting'
	helpst(2)='^F*     :revert to initial font'
	helpst(3)='^G      :change current font to greek'
	helpst(4)='^S[n]   :change current size setting'
	helpst(5)='^S*     :revert to initial size'
	helpst(6)='^N      :next line'
	helpst(7)='^B      :backspace'
	helpst(8)='^U      :half line up'
	helpst(9)='^D      :half line down'
	helpst(10)='^L[n]   :change string size'
	helpst(11)='^C[p]   :change string color '
	helpst(12)='<F12>[m]:shortcut for ^Fm and ^F*'
	helpst(13)='<Alt>[n]:shortcut for ^Sn and ^S*'
	helpst(14)='<ALT>+  :increase string size'
	helpst(15)='<Alt>-  :decrease string size'
	helpst(16)='m = 0/1/2/3/4/5/6/7/8/9/A/B/*'
	helpst(17)='n = 0/1/2/3/4/5/6/7/8/*'
	helpst(18)='p = 0/1/2/3/4/5/6/7/8/9/A/B/C/D/E/F'
	nhelp=18
	icq=14
	call coltyp1(icolor)
	i=1
	n=1
	call putcur(ix,iy)
	call fndcur(ix,iy)
	call getcurspos(ix,iy)
	ixc=ix
	iyc=iy-20
	call hgline(ixc,iyc,4)
	isize0=isize
888	continue
	char1=getch(ktype1)
	ival1=ichar(char1)
c	print*,ktype1,ival1
c	call hgcur(ix,iy,isize,4)
	if(ival1.eq.13.or.ival1.eq.27) then
		goto 777
	else if(ktype1.eq.0.and.ival1.eq.59) then	!F1=help
		ixloh=320    !position for help box
		iyhih=320
		call poptext(ixloh,-1,iyhih,helpst,nhelp,15,2,icq)	!=popkeys
		goto 888
	else
		if(ktype1.eq.32) then
			select case(ival1)
			case(2)         !^B backspace
			  if(i.gt.1) then

				transit(i)=ourstring(i:i)
				ourstring(i:i)=char1
				call hgline(ixc,iyc,ibk)
c				call setfont1(ifont)
				call justifystring(ix,iy,ourstring(1:n),
     &				irot,isize,justx,justy)
				i=i-1
c				print*,'b=',i,n
				ixc=ixc-idx
				call hgline(ixc,iyc,4)
				back=.true.
			  endif
			case(4)        !^D down
				ourstring(i:i)=char1
				call justifystring(ix,iy,ourstring(1:n),
     &				irot,isize,justx,justy)
					i=i+1
					n=n+1
			case(6)        !^F 0/1/2/3/4/5/6/7/8/9/a/b/*
				char2=getch(ktype2)     !change font
				ival2=ichar(char2)
				if(ival2.ge.48.and.ival2.le.57.or.ival2.eq.97.
     &			or.ival2.eq.98.or.ival2.eq.42) then
					ourstring(i:i+1)=char1//char2
					call justifystring(ix,iy,ourstring(1:n+1),
     &				irot,isize,justx,justy)
					i=i+2
					n=n+2
				else
					goto 888
				endif
			case(7)           ! ^G change font to greek
				ourstring(i:i+1)=char(6)//'a'
c				call justifystring(ix,iy,ourstring(1:i+1),
				call justifystring(ix,iy,ourstring(1:n+1),
     &			irot,isize,justx,justy)
				i=i+2
				n=n+2
				back=.false.
			case(12)        !^L 0/1/2/3/4/5/6/7/8/9
				  isize0=isize
				  char2=getch(ktype2)     !change size string
				  ival2=ichar(char2)
				if(ival2.ge.48.and.ival2.le.57) then
				  call hgline(ixc,iyc,ibk)
				  call chtoreal(char2,size)
				  call coltyp1(ibk)
				  call justifystring(ix,iy,ourstring(1:n),
     &			  irot,isize,justx,justy)
				  isize=int(size)
				  call coltyp1(icolor)
				  call justifystring(ix,iy,ourstring(1:n),
     &			  irot,isize,justx,justy)
				  call curlength(isize,idx)
				  ixc=ix+nchar*idx
				  call hgline(ixc,iyc,4)
				  isize0=isize
				else
					goto 888
				endif
			case(3)
		      	char2=getch(ktype2)     !change color string
				ival2=ichar(char2)
				if(ival2.ge.47.and.ival2.le.57) then
					call coltyp1(ibk)
					call justifystring(ix,iy,ourstring(1:n),
     &				irot,isize,justx,justy)
					call chtoreal(char2,color)
				else if (ival2.ge.97.and.ival2.le.102) then
					call coltyp1(ibk)
					call justifystring(ix,iy,ourstring(1:n),
     &				irot,isize,justx,justy)
					select case (ival2)
						case(97)
							color=10
						case(98)
							color=11
						case(99)
							color=12
						case(100)
							color=13
						case(101)
							color=14
						case(102)
							color=15
					end select
				else
					goto 888
				endif
				icolor=int(color)
				call coltyp1(icolor)
				call justifystring(ix,iy,ourstring(1:n),
     &			irot,isize,justx,justy)
			case(14)       ! ^N next
				ourstring(i:i)=char1
				call justifystring(ix,iy,ourstring(1:n),
     &				irot,isize,justx,justy)
					i=i+1
					irows=irows+1
					n=n+1
			case(16)       !^ P   !back to normal for font
				ourstring(i:i+1)=char(6)//'*'
				call justifystring(ix,iy,ourstring(1:n+1),
     &			irot,isize,justx,justy)
				i=i+2
				n=n+2
			case(19)       !^S 0/1/2/3/4/5/6/7/8/9/* change size
				char2=getch(ktype2)
				ival2=ichar(char2)
				if(ival2.ge.48.and.ival2.le.57.or.ival2.eq.42)
     &				then
					ourstring(i:i+1)=char(19)//char2
					call justifystring(ix,iy,ourstring(1:n+1),
     &				irot,isize,justx,justy)
					n=n+2
				call chtoreal(char2,size)
				isize0=int(size)
				else
					goto 888
				endif
			case(21)       ! ^U up
				ourstring(i:i)=char1
				call justifystring(ix,iy,ourstring(1:n),
     &				irot,isize,justx,justy)
					i=i+1
					n=n+1
			end select
		else if(ktype1.eq.64) then    !all string
			if (ival1.eq.46) then	!Alt-C 0/1....e(14)/f(15)
		      	char2=getch(ktype2)     !change color string
				ival2=ichar(char2)
				if(ival2.ge.47.and.ival2.le.57) then
					call coltyp1(ibk)
					call justifystring(ix,iy,ourstring(1:n),
     &				irot,isize,justx,justy)
					call chtoreal(char2,color)
				else if (ival2.ge.97.and.ival2.le.102) then
					call coltyp1(ibk)
					call justifystring(ix,iy,ourstring(1:n),
     &				irot,isize,justx,justy)
					select case (ival2)
						case(97)
							color=10
						case(98)
							color=11
						case(99)
							color=12
						case(100)
							color=13
						case(101)
							color=14
						case(102)
							color=15
					end select
				else
					goto 888
				endif
				icolor=int(color)
				call coltyp1(icolor)
				call justifystring(ix,iy,ourstring(1:n),
     &			irot,isize,justx,justy)
			else if (ival1.eq.31) then	!Alt-S 0/.../9
		      	char2=getch(ktype2)     !change size string
				ival2=ichar(char2)
				if (ival2.ge.48.and.ival2.le.57) then   !Alt-S 0/1/2...9 change size string
				  call hgline(ixc,iyc,ibk)
				  call coltyp1(ibk)
				  call justifystring(ix,iy,ourstring(1:n),
     &			  irot,isize,justx,justy)
				  call chtoreal(char2,size)
				  isize=int(size)
				  call coltyp1(icolor)
				  call justifystring(ix,iy,ourstring(1:n),
     &			  irot,isize,justx,justy)
				  call curlength(isize,idx)
				  ixc=ixc+idx
				  call hgline(ixc,iyc,4)
				else
					goto 888
				endif
			else	if (ival1.ge.120.and.ival1.le.129) then   !Alt-0/1/2...9 change size string
				ival1=ival1-71                        !shortcut for ^Sn
				if(ival1.eq.129) ival1=48
				char3=char(ival1)
				ourstring(i:i+1)=char(19)//char3
				call justifystring(ix,iy,ourstring(1:n+1),
     &			irot,isize,justx,justy)
				i=i+2
				n=n+2
				call chtoreal(char3,size)
				isize0=int(size)
			else if(ival1.eq.130) then                    !Alt--
				  call hgline(ixc,iyc,ibk)
				  call coltyp1(ibk)
				  call justifystring(ix,iy,ourstring(1:n),
     &			  irot,isize,justx,justy)
				  isize=isize-1
				  call coltyp1(icolor)
				  call justifystring(ix,iy,ourstring(1:n),
     &			  irot,isize,justx,justy)
				  call curlength(isize,idx)
				  isize0=isize
				  ixc=ix+nchar*idx
				  call hgline(ixc,iyc,4)
			else if(ival1.eq.131) then                    !Alt-+
				  call coltyp1(ibk)
				  call justifystring(ix,iy,ourstring(1:n),
     &			  irot,isize,justx,justy)
				  call hgline(ixc,iyc,ibk)
				  isize=isize+1
				  call coltyp1(icolor)
				  call justifystring(ix,iy,ourstring(1:n),
     &			  irot,isize,justx,justy)
				  call curlength(isize,idx)
				  ixc=ix+nchar*idx
				  isize0=isize
				  call hgline(ixc,iyc,4)
			else if(ival1.eq.33) then !change font string
			endif
		else if(ktype1.eq.0) then    !all string
			back=.false.
			if (ival1.eq.134) then	!F12 0/1/2/.../9/*
				char2=getch(ktype2)     !change font
				ival2=ichar(char2)
c				print*,ktype2,char2,ival2
				if(ival2.ge.48.and.ival2.le.57.or.ival2.eq.97.
     &			or.ival2.eq.98.or.ival2.eq.42) then
					ourstring(i:i+1)=char(6)//char2
					call justifystring(ix,iy,ourstring(1:n+1),
     &				irot,isize,justx,justy)
					i=n+2
				else
					goto 888
				endif
			endif
		else if(ktype1.eq.8) then
			if (ival1.eq.8) then	!backspace and delete
				if(i.ge.2) then
				  call hgline(ixc,iyc,ibk)
				  call coltyp1(ibk)
				  call justifystring(ix,iy,ourstring(1:n),
     &			  irot,isize,justx,justy)
				  do j=i-1,n-1
					transit(j)=ourstring(j+1:j+1)
				  enddo
				  do j=i-1,n-1
					ourstring(j:j)=transit(j)
				  enddo
				  i=i-1
				  n=n-1
				  call coltyp1(icolor)
				  call justifystring(ix,iy,ourstring(1:n),
     &			  irot,isize,justx,justy)
				  ixc=ixc-idx
				  call hgline(ixc,iyc,4)
				  isize0=isize
c			     else if (i.eq.2) then
c				  call hgline(ixc,iyc,ibk)
c				  call coltyp1(ibk)
c				  call justifystring(ix,iy,ourstring(1:n),
c     &			  irot,isize,justx,justy)
c				  call coltyp1(icolor)
c				  i=i-1
c				  n=n-1
c				  print*,i,n
c				  ixc=ixc-idx
c				  call hgline(ixc,iyc,4)
c				  isize0=isize
				else if (i.eq.1) then
			     endif
			else if (ival1.eq.75) then
			   if(i.gt.1) then
				call hgline(ixc,iyc,ibk)
				i=i-1
				 ixc=ixc-idx
				call hgline(ixc,iyc,4)
			   endif
			else if (ival1.eq.77) then
			   if(i.lt.n) then
				call hgline(ixc,iyc,ibk)
				i=i+1
				ixc=ixc+idx
				call hgline(ixc,iyc,4)
			   endif
			endif
c		else if(ktype1.eq.2.and.ival1.eq.32) then
c			call hgline(ixc,iyc,ibk)
c			i=i+1
c			ixc=ixc+idx
c			call hgline(ixc,iyc,4)
		else                    !write character
			call hgline(ixc,iyc,ibk)
 			call coltyp1(ibk)
			if(back) then
 			do j=i,n
 				ourstring(j:j)=transit(j)
 			enddo
			endif
c			print*,ourstring
 			call justifystring(ix,iy,ourstring(1:n),
     &		irot,isize,justx,justy)
 			transit(i)=char1
 			do j=i+1,n+1
 				transit(j)=ourstring(j-1:j-1)
 			enddo
 			transit(i)=char1
 			do j=i,n+1
 				ourstring(j:j)=transit(j)
c 				print*,j,ourstring(j:j)
 			enddo
c			ourstring(i:i)=char1
c	            if(ktype1.eq.2.and.ival1.eq.32) ourstring(i:i)=' '
c			pause
 			call coltyp1(icolor)
			call setfont1(ifont)
			i=i+1
			n=n+1
c			print*,ourstring
			call justifystring(ix,iy,ourstring(1:n),
     &		irot,isize,justx,justy)
			call curlength(isize0,idx)
			ixc=ixc+idx
			call hgline(ixc,iyc,4)
			nchar=nchar+1
c			print*,i,n
			back=.false.
		endif
		goto 888
	endif
777	continue
	call hgline(ixc,iyc,ibk)
	call wrgwin1(ixlo,iylo,ixhi,iyhi,arra)
	deallocate(arra)
	end

