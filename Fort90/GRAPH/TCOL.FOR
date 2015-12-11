c to test pixel position of columns
	integer*4 iac(0:641)
	character*1 string(0:90)
	character*11 cnum
	character*1 ch,getch
	integer*2 ktype

	call MODE(18)		!clears screen
	call CLEARS(0,479,3)
c
	do i=0,90
	   j=mod(i,10)
	   string(i)=char(j+48)
	enddo
	do j=0,90
	   call set_cursor(j,0)
	   call SET_STRING(string(j),12)
	enddo
c now pause
	ch=getch(ktype)
	ix=320	!initially
	call VLINE(ix,0,479,13)
	call set_cursor(60,0)
	print *,ix
	call MOVE_VLINE(ix,14,iac,0)
	call MODE(3)
	end

	subroutine MOVE_VLINE(ix,icl,iac,ii)

c	This moves a VERTICAL line on the screen without erasing the
c	background
C	-----------------------------------------------------------------

	integer*4		iac(0:641)
	character*1		a,getch,LC

	a=getch(ktype)
	if(ktype.eq.2) a=LC(a)	!convert to lower case
	m=ichar(a)
	if (ktype.eq.8) then
	do while(m.ne.13)
	   if (m.eq.77) then
		if(ix.lt.639) then
		   ix0=ix
		   ix=ix+1
		   j=ii
		   do i=0,479
	 	      call wpixel(ix0,j,iac(i))
			call rpixel(ix,j,iac(i))
			j=j+1
  		   end do
		   call VLINE(ix,ii+1,ii+479,icl)
		endif
	   else if (m.eq.75) then
		if(ix.gt.0) then
		   ix0=ix
		   ix=ix-1
		   j=ii
		   do i=0,479
	 	      call wpixel(ix0,j,iac(i))	!rewrite prev data
			call rpixel(ix,j,iac(i))	!record data in new pos before drawing line
			j=j+1
   		   end do
		   call VLINE(ix,ii+1,ii+479,icl)
		endif
	   endif
c	   call set_cursor(60,0)
	   call LOCATE(0,50)
	   print *,ix
c	   call SET_STRING(strings(i),12)
	   a=getch(ktype)
	   if(ktype.eq.2) a=LC(a)	!convert to lower case
	   m=ichar(a)

	end do
	endif
	end

