	subroutine move_text(strings,sizes,n,ixg,iyg,colors,rotate,ibk)
c
c    subroutine to move maximum 10 strings
c

	character*(*)	strings(n)

	integer*4	sizes(n),ixg(n),iyg(n),ixlen(10),iylen(10)
	integer*4	colors(n),idim(10)
	integer*4	rotate(n)
	integer*4	ixl(10),iyl(10),ixh(10),iyh(10),idx(10),idy(10)
	integer*1	array1(:),array2(:),array3(:),array4(:),array5(:)
	integer*1 	array6(:),array7(:),array8(:),array9(:),array10(:)
	integer*1	arr1(:),arr2(:),arr3(:),arr4(:),nut(10)
	logical mouse_on
	common/mouse_on/nbutton
	allocatable :: array1,array2,array3,array4,array5
	allocatable :: array6,array7,array8,array9,array10
	allocatable :: arr1,arr2,arr3,arr4

	do i=1,n
		nut(i)=0
	enddo
	icf=15
	mouse_on=.true.
	do i=1,10
		idim(i)=1
	enddo

	do i=1,n
		call boxstring(strings(i),sizes(i),ixmin1,iymin1,ixlen(i),
     &	iylen(i))
		dixl=640*float(ixg(i))/10000
		ixl(i)=int(dixl)
		ixl(i)=ixl(i)-mod(ixl(i),16)
		dx=640*float(ixlen(i))/10000
		idx(i)=int(dx)+8
		ixh(i)=ixl(i)+idx(i)
		diyl=480*float(iyg(i))/7500
		iyl(i)=int(diyl)-6
		dy=480*float(iylen(i))/7500
		idy(i)=int(dy)+8
		rad=sin(rotate(i)*3.14/180.)
		roty=dx*rad
		iroty=int(roty)
		iyh(i)=iyl(i)+idy(i)
		if (rotate(i).gt.0) then
			iyh(i)=iyh(i)+iroty
			idy(i)=idy(i)+iroty
		else if (rotate(i).lt.0) then
			iyl(i)=iyl(i)+iroty
			idy(i)=idy(i)-iroty
		endif
		idim(i)=2*((iyh(i)-iyl(i)+9)*(ixh(i)-ixl(i)+9)+1)
c		print*,rotate(i),rad,iroty,iyl(i),iyh(i)
	enddo
c	pause
	allocate(array1(idim(1)),array2(idim(2)),array3(idim(3)))
	allocate(array4(idim(4)),array5(idim(5)),array6(idim(6)))
	allocate(array7(idim(7)),array8(idim(8)),array9(idim(9)))
	allocate(array10(idim(10)))
	if(mouse_on) then
	call show_mouse()
1	call mouse_update(ibutton,ix,iy)
	   if(ibutton.eq.1) then
	  	if(ix.lt.ixh(1).and.ix.gt.ixl(1).and.iy.lt.iyh(1).and.
     &	iy.gt.iyl(1)) then	     !hit on drag area
			index=1
			ixlo=ixl(1)
			ixhi=ixh(1)
			iylo=iyl(1)
			iyhi=iyh(1)
			idxw=idx(1)
			idyw=idy(1)
		else if(ix.lt.ixh(2).and.ix.gt.ixl(2).and.iy.lt.iyh(2).and.
     &	iy.gt.iyl(2)) then	     !hit on drag area
			index=2
			ixlo=ixl(2)
			ixhi=ixh(2)
			iylo=iyl(2)
			iyhi=iyh(2)
			idxw=idx(2)
			idyw=idy(2)
		else if(ix.lt.ixh(3).and.ix.gt.ixl(3).and.iy.lt.iyh(3).and.
     &	iy.gt.iyl(3)) then	     !hit on drag area
			index=3
			ixlo=ixl(3)
			ixhi=ixh(3)
			iylo=iyl(3)
			iyhi=iyh(3)
			idxw=idx(3)
			idyw=idy(3)
		else if(ix.lt.ixh(4).and.ix.gt.ixl(4).and.iy.lt.iyh(4).and.
     &	iy.gt.iyl(4)) then	     !hit on drag area
			index=4
			ixlo=ixl(4)
			ixhi=ixh(4)
			iylo=iyl(4)
			iyhi=iyh(4)
			idxw=idx(4)
			idyw=idy(4)
		else if(ix.lt.ixh(5).and.ix.gt.ixl(5).and.iy.lt.iyh(5).and.
     &	iy.gt.iyl(5)) then	     !hit on drag area
			index=5
			ixlo=ixl(5)
			ixhi=ixh(5)
			iylo=iyl(5)
			iyhi=iyh(5)
			idxw=idx(5)
			idyw=idy(5)
		else if(ix.lt.ixh(6).and.ix.gt.ixl(6).and.iy.lt.iyh(6).and.
     &	iy.gt.iyl(6)) then	     !hit on drag area
			index=6
			ixlo=ixl(6)
			ixhi=ixh(6)
			iylo=iyl(6)
			iyhi=iyh(6)
			idxw=idx(6)
			idyw=idy(6)
		else if(ix.lt.ixh(7).and.ix.gt.ixl(7).and.iy.lt.iyh(7).and.
     &	iy.gt.iyl(7)) then	     !hit on drag area
			index=7
			ixlo=ixl(7)
			ixhi=ixh(7)
			iylo=iyl(7)
			iyhi=iyh(7)
			idxw=idx(7)
			idyw=idy(7)
		else if(ix.lt.ixh(8).and.ix.gt.ixl(8).and.iy.lt.iyh(8).and.
     &	iy.gt.iyl(8)) then	     !hit on drag area
			index=8
			ixlo=ixl(8)
			ixhi=ixh(8)
			iylo=iyl(8)
			iyhi=iyh(8)
			idxw=idx(8)
			idyw=idy(8)
		else if(ix.lt.ixh(9).and.ix.gt.ixl(9).and.iy.lt.iyh(9).and.
     &	iy.gt.iyl(9)) then	     !hit on drag area
			index=9
			ixlo=ixl(9)
			ixhi=ixh(9)
			iylo=iyl(9)
			iyhi=iyh(9)
			idxw=idx(9)
			idyw=idy(9)
		else if(ix.lt.ixh(10).and.ix.gt.ixl(10).and.iy.lt.iyh(10).
     &	and.iy.gt.iyl(10)) then	     !hit on drag area
			index=10
			ixlo=ixl(10)
			ixhi=ixh(10)
			iylo=iyl(10)
			iyhi=iyh(10)
			idxw=idx(10)
			idyw=idy(10)
		else
			goto 1
		endif
		ixlo0=ixlo
		iylo0=iylo
		ixhi0=ixhi
		iyhi0=iyhi
		dx=float(idxw+1)/2
		dy=4*float(idyw+1)
		nh=int(dx)
		nv=int(dy)
		allocate(arr1(nh),arr2(nh),arr3(nv),arr4(nv))
		call hide_mouse()
		call rdframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,arr3,arr4,nh,nv)
		call show_mouse()
		idcx=abs(ix-ixlo0)
		idcy=abs(iy-iylo0)
		call hide_mouse()
		call drawframe(ixlo,iylo,ixhi,iyhi,icf)
2		call mouse_update(ibutton,ix,iy)
		if(ibutton.eq.1) then		!left button
			if(ix.eq.ix0.and.iy.eq.iy0) goto 3
			call hide_mouse()
			if(iret.eq.1) then
			   call drawframe(ixlo,iylo,ixhi,iyhi,ibk)
			   iret=0
			else
			   call wrframe(ixlo,iylo,ixhi,iyhi,arr1,arr2
     &		   ,arr3,arr4,nh,nv)
			endif
			ixlo=(ix-idcx)-mod((ix-idcx),16)  !
			iylo=(iy-idcy)
			if(ixlo.lt.0) then
			   ixlo=0
			endif
			if(iylo.lt.0) then
			   iylo=0
			endif
			iyhi=iylo+idyw
			ixhi=ixlo+idxw
			if(ixhi.gt.639) then
			   ixhi=639
			   ixlo=ixhi-idxw
			endif
			if(iyhi.gt.479) then
			   iyhi=479
			   iylo=iyhi-idyw
			endif
			call rdframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,arr3
     &		,arr4,nh,nv)
			call drawframe(ixlo,iylo,ixhi,iyhi,icf)
3			continue
			ix0=ix
			iy0=iy
			call show_mouse()
			goto 2
		 else if(ibutton.eq.0) then		!release mouse
			iret=1
			call hide_mouse()
			call wrframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &		arr3,arr4,nh,nv)
			if(ixlo.lt.0.or.ixhi.gt.639.or.iylo.lt.0.or.
     &		iyhi.gt.479) then
		         ixlo=ixlo0
			   iylo=iylo0
			   ixhi=ixhi0
			   iyhi=iyhi0
                  endif
			if(nut(index).eq.0) then
			  call fillwin(ixlo0,iylo0,ixhi0,iyhi0,ibk)
			else
			  select case(index)
			  case(1)
			  call wrgwin1(ixlo0,iylo0,ixhi0,iyhi0,array1)
			  case(2)
			  call wrgwin1(ixlo0,iylo0,ixhi0,iyhi0,array2)
			  case(3)
			  call wrgwin1(ixlo0,iylo0,ixhi0,iyhi0,array3)
			  case(4)
			  call wrgwin1(ixlo0,iylo0,ixhi0,iyhi0,array4)
			  case(5)
			  call wrgwin1(ixlo0,iylo0,ixhi0,iyhi0,array5)
			  case(6)
			  call wrgwin1(ixlo0,iylo0,ixhi0,iyhi0,array6)
			  case(7)
			  call wrgwin1(ixlo0,iylo0,ixhi0,iyhi0,array7)
			  case (8)
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array8)
			  case (9)
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array9)
			  case (10)
				call wrgwin1(ixlo,iylo,ixhi,iyhi,array10)
			  end select
			endif
			  select case(index)
			  case (1)
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array1)
			  case (2)
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array2)
			  case (3)
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array3)
			  case (4)
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array4)
			  case (5)
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array5)
			  case (6)
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array6)
			  case (7)
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array7)
			  case (8)
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array8)
			  case (9)
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array9)
			  case (10)
				call rdgwin1(ixlo,iylo,ixhi,iyhi,array10)
			  end select
			nut(index)=1
			ixlo0=ixlo
			iylo0=iylo
			ixhi0=ixhi
			iyhi0=iyhi
			ix0=ix
			iy0=iy
			ixl(index)=ixlo
			ixh(index)=ixhi
			iyl(index)=iylo
			iyh(index)=iyhi
			dixg=float(ixl(index))*10000/640
			diyg=float(iyl(index)+6)*7500/480
			ixg(index)=int(dixg)
			iyg(index)=int(diyg)
			call coltyp1(colors(index))
			call justifystring(ixg(index),iyg(index),
     &		strings(index),rotate(index),sizes(index),base,base)
			deallocate(arr1,arr2,arr3,arr4)
			call show_mouse
		      goto 1
		else if(ibutton.eq.2) then			!right button
			call hide_mouse()
			call wrframe(ixlo,iylo,ixhi,iyhi,arr1,arr2,
     &		arr3,arr4,nh,nv)
			ixl(index)=ixlo
			ixh(index)=ixhi
			iyl(index)=iylo
			iyh(index)=iyhi
			deallocate(arr1,arr2,arr3,arr4)
			goto 99
		endif !end of drag
	   else if(ibutton.eq.2) then		!right button
		goto 99
	   else
		goto 1
	   endif
	endif		!end of if(mous_set)
c
c===========================================================================
c
99	continue
	call hide_mouse()
	deallocate(array1,array2,array3,array4,array5,array6,array7)
	deallocate(array8,array9,array10)
	end
