	subroutine movcur(ix_down,iy_down,isizex,isizey,idy,n_rows,
     &	idx,n_columns,icur,ibk,isym,idot,irow,icolumn)

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
	if(icolumn.eq.0) then
		icolumn=1
	endif
	i=irow
	l=icolumn
	dy_up=float(n_rows-1)*float(idy)
	iy_up=iy_down+int(dy_up)
	dy=float(n_rows-irow)*float(idy)
	jy=iy_down+int(dy)
	dx_up=float(n_columns-1)*float(idx)
	ix_up=ix_down+int(dx_up)
	dx=float(n_columns-icolumns)*float(idx)
	jx=ix_down+int(dx)

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
				call zsymbol(jx,jy,isym,isizex,isizey,ibk,
     &				     ibk)
		   	   	if(i.gt.1) then
		   	      	jy=jy+idy
			      	i=i-1
			      	irow=i
					call zsymbol(jx,jy,isym,isizex,isizey
     &				,icur,ibk)
		   	   	else		!go to bottom=string(n)
					jy=iy_down
					i=n_rows
					irow=i
					call zsymbol(jx,jy,isym,isizex,isizey
     &				,icur,ibk)
		   	   	endif
		      case(80)			!down arrow
				call zsymbol(jx,jy,isym,isizex,isizey,ibk,
     &				     ibk)
		   	   	if(i.lt.n_rows) then
		   	      	jy=jy-idy
			      	i=i+1
			      	irow=i
					call zsymbol(jx,jy,isym,isizex,isizey
     &				,icur,ibk)
		   	   	else		!go to bottom=string(n)
					jy=iy_up
					i=1
					irow=i
					call zsymbol(jx,jy,isym,isizex,isizey
     &				,icur,ibk)
		   	   	endif
		      case(81)
				call zsymbol(jx,jy,isym,isizex,isizey,ibk,
     &				     ibk)
				jy=iy_down
				i=n_rows
				irow=i
				call zsymbol(jx,jy,isym,isizex,isizey,icur,
     &				     ibk)
		      case(73)			!down arrow
				call zsymbol(jx,jy,isym,isizex,isizey,ibk,
     &				     ibk)
				jy=iy_up
				i=1
				irow=i
				call zsymbol(jx,jy,isym,isizex,isizey,icur,
     &				     ibk)
		      case(77)           !right
				call zsymbol(jx,jy,isym,isizex,isizey,ibk,
     &				     ibk)
		   	   	if(l.lt.n_columns) then
		   	      	jx=jx+idx
			      	l=l+1
			      	icolumns=l
					call zsymbol(jx,jy,isym,isizex,isizey
     &				,icur,ibk)
		   	   	else		!go to left
					jx=ix_down
					l=1
					icolumns=l
					call zsymbol(jx,jy,isym,isizex,isizey
     &				,icur,ibk)
		   	   	endif
		      case(75)	    !left
				call zsymbol(jx,jy,isym,isizex,isizey,ibk,
     &				     ibk)
		   	   	if(l.gt.1) then
		   	      	jx=jx-idx
			      	l=l-1
			      	icolumn=l
					call zsymbol(jx,jy,isym,isizex,isizey
     &				,icur,ibk)
		   	   	else		!go to right
					jx=ix_up
					l=n_columns
					icolumns=l
					call zsymbol(jx,jy,isym,isizex,isizey
     &				,icur,ibk)
		   	   	endif
		      case(71)         !home
				call zsymbol(jx,jy,isym,isizex,isizey,ibk,
     &				     ibk)
				jx=ix_down
				l=1
				icolumns=l
				call zsymbol(jx,jy,isym,isizex,isizey,icur,
     &				     ibk)
		      case(79)			!end
				call zsymbol(jx,jy,isym,isizex,isizey,ibk,
     &				     ibk)
				jx=ix_up
				l=n_columns
				icolumns=l
				call zsymbol(jx,jy,isym,isizex,isizey,icur,
     &				     ibk)
	     	   end select
		   call show_mouse()
		   goto 1
		endif
	   endif
c	endif		!end of 'if kbhit'

98	continue
c	call hide_mouse()
c	call zsymbol(ix_down,jy,isym,isizex,isizey,ibk,ibk)
c	call show_mouse()
	irow=i
	icolumn=l
c	print*,irow
	end
