	subroutine movercur(ix_down,iy_down,isizex,isizey,idy,n_rows,
     &			  icur,ibk,isym,idot,irow)

	character*1 	charout,getch
	logical 		kbhit
c
c
c
c
c	print*,irow

	if(irow.eq.0) then
		irow=1
	endif
	i=irow
	dy_up=float(n_rows-1)*float(idy)
	iy_up=iy_down+int(dy_up)
	dy=float(n_rows-irow)*float(idy)
	jy=iy_down+int(dy)

1	continue
c	if(kbhit()) then
	   charout=getch(ktype)
	   ival=ichar(charout)
	   if(ival.eq.27.or.ival.eq.13) then		!ESC or ENTER
	  	goto 98
	   else
	   	if(ktype.eq.8) then		!up/down arrow to scroll highlight
		   call hide_mouse()					!up arrow
	     	   select case(ival)
		      case(72)
				call ISYMB2(ix_down,jy,isym,isizex,isizey,ibk,
     &				     ibk,idot)
		   	   	if(i.gt.1) then
		   	      	jy=jy+idy
			      	i=i-1
			      	irow=i
					call ISYMB2(ix_down,jy,isym,isizex,isizey
     &				,icur,ibk,idot)
		   	   	else		!go to bottom=string(n)
					jy=iy_down
					i=n_rows
					irow=i
					call ISYMB2(ix_down,jy,isym,isizex,isizey
     &				,icur,ibk,idot)
		   	   	endif
		      case(80)			!down arrow
				call ISYMB2(ix_down,jy,isym,isizex,isizey,ibk,
     &				     ibk,idot)
		   	   	if(i.lt.n_rows) then
		   	      	jy=jy-idy
			      	i=i+1
			      	irow=i
					call ISYMB2(ix_down,jy,isym,isizex,isizey
     &				,icur,ibk,idot)
		   	   	else		!go to bottom=string(n)
					jy=iy_up
					i=1
					irow=i
					call ISYMB2(ix_down,jy,isym,isizex,isizey
     &				,icur,ibk,idot)
		   	   	endif
		      case(81)
				call ISYMB2(ix_down,jy,isym,isizex,isizey,ibk,
     &				     ibk,idot)
				jy=iy_down
				i=n_rows
				irow=i
				call ISYMB2(ix_down,jy,isym,isizex,isizey,icur,
     &				     ibk,idot)
		      case(73)			!down arrow
				call ISYMB2(ix_down,jy,isym,isizex,isizey,ibk,
     &				     ibk,idot)
				jy=iy_up
				i=1
				irow=i
				call ISYMB2(ix_down,jy,isym,isizex,isizey,icur,
     &				     ibk,idot)
	     	   end select
		   call show_mouse()
		   goto 1
		endif
	   endif
c	endif		!end of 'if kbhit'

98	continue
c	call hide_mouse()
c	call ISYMB2(ix_down,jy,isym,isizex,isizey,ibk,ibk,idot)
c	call show_mouse()
	irow=i
c	print*,irow
	end
