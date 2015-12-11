c**************************************************************************
	subroutine fill_with_pattern(ix,iy,n,icf,icb,ipattern)
	integer*2 ix(n),iy(n)
	call drawpoly(ix,iy,n,icb)

c	find_top
	iy_top=iy(1)
	do i=2,n
	    	if (iy(i).gt.iy_top) then
			iy_top=iy(i)
		endif
	enddo

	iy_bottom=iy(1)
	do i=2,n
	    	if (iy(i).lt.iy_bottom) then
			iy_bottom=iy(i)
		endif
	enddo

	ix_right=ix(1)
	do i=2,n
	    	if (ix(i).gt.ix_right) then
			ix_right=ix(i)
		endif
	enddo

	ix_left =ix(1)
	iy_left =iy(1)

	do i=2,n
	    	if (ix_left.gt.ix(i)) then
			ix_left =ix(i)
			iy_left =iy(i)
		endif
	enddo
	i=ix_left+1

1	continue
	call scanud(i,ix_left,ix_right,iy_left,iy_bottom,iy_top,
     &			iy_down,iy_up,1,icb)
	iyt1=iy_up
	iyt2=iy_down
	call scanud(i,ix_left,ix_right,iy_left-1,iy_bottom,iy_top,
     &			iy_down,iy_up,-1,icb)
	iyb1=iy_up
	iyb2=iy_down
	if(iyt2.eq.1000.and.iyb2.ne.1000) then
		iylo=iyb1
		iyhi=iyb2
	else if(iyt2.ne.1000.and.iyb2.eq.1000) then
		iylo=iyt1
		iyhi=iyt2
	else if(iyt2.ne.1000.and.iyb2.ne.1000) then
		iylo=iyb2
		iyhi=iyt2
	else
		goto 2
	endif
	call dvline(i,iylo,iyhi,icf,ipattern)
2	i=i+1
	if (i.lt.ix_right) goto 1
	end

c**************************************************************************
c**************************************************************************
	subroutine scanud(i,ix_left,ix_right,iy_left,iy_bottom,iy_top,
     &			iy_down,iy_up,id,icb)
	integer*2	m(640)

	k=1
	j=iy_left
1	call rpixel(i,j,ic)
	if (ic.eq.icb) then
		m(k)=j
		k=k+1
	endif
	j=j+id
	if(id.eq.1) then
		if (j.le.iy_top) goto 1
	else if(id.eq.-1) then
		if (j.ge.iy_bottom) goto 1
	endif

	k=k-1

	if (k.eq.1) then
		if(iy_left.eq.iy_top) then
		iy_down=1000
       		goto 2
		endif
		iy_down=m(1)-id
		iy_up=iy_left
	else if(k.eq.2) then
		if (abs(m(1)-m(2)).gt.1) then
			iy_up=m(1)+id
			iy_down=m(2)-id
		else
			if(i.eq.ix_right-1) then
		iy_down=1000
       		goto 2
		endif
			if(i.eq.ix_left+1) then
		iy_down=1000
       		goto 2
		endif
			iy_down=m(1)-id
			iy_up=iy_left
		endif
	else if(k.eq.3) then
		if (abs(m(1)-m(2)).gt.1) then
			iy_up=m(1)+id
			iy_down=m(2)-id
		else if(abs(m(2)-m(3)).gt.1) then
			iy_up=m(2)+id
			iy_down=m(3)-id
		else
			if(i.eq.ix_right-1) then
		iy_down=1000
       		goto 2
		endif
			if(i.eq.ix_left+1) then
		iy_down=1000
       		goto 2
		endif
			iy_down=m(1)-id
			iy_up=iy_left
		endif
	else if(k.ge.4) then
		if (abs(m(1)-m(2)).gt.1) then
			iy_up=m(1)+id
			iy_down=m(2)-id
		else if(abs(m(2)-m(3)).gt.1) then
			iy_up=m(2)+id
			iy_down=m(3)-id
		else if(abs(m(3)-m(4)).gt.1) then
			iy_up=m(3)+id
			iy_down=m(4)-id
		else
			iy_down=m(1)-id
			iy_up=iy_left
		endif
	else
		iy_down=1000
	endif

2	end


c**************************************************************************
