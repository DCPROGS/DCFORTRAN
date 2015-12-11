c to test pixel position of rows
	integer*4		iac(0:641)
	character*20 strings(0:30)
	character*11 cnum
	character*1 ch,getch
	integer*2 ktype

	call MODE(18)		!clears screen
	call CLEARS(0,479,3)
c
	do i=0,30
	   call INTCONV(i,cnum)
	   n=NBLANK(cnum)
	   strings(i)='      '//cnum(1:n)//': line of text'
	enddo
	do i=0,30
	   call set_cursor(0,i)
	   call SET_STRING(strings(i),12)
	enddo
c letters in row #irow have bases at iy=3+(29-irow)*16
	do irow=0,29
	   iy=3+(29-irow)*16
	   call hline(0,639,iy,13)
	enddo
c now pause
	ch=getch(ktype)
	iybar=200	!initially
	call hline(0,639,iybar,13)
	call set_cursor(60,0)
	print *,iybar
	call MOVE_LINE(iybar,14,iac,0)
	call MODE(3)
	end

c	This moves a horizontal line on the screen without erasing the
c	background
	subroutine MOVE_LINE(iybar,icl,iac,ii)

C	-----------------------------------------------------------------

	integer*4		iac(0:641)
	character*1		a,getch,LC

	a=getch(ktype)
	if(ktype.eq.2) a=LC(a)	!convert to lower case
	m=ichar(a)
	if (ktype.eq.8) then
	do while(m.ne.13)
	   if (m.eq.72) then
		if(iybar.lt.478) then
		   iyo=iybar
		   iybar=iybar+1
		   j=ii
		   do i=0,639
	 	      call wpixel(j,iyo,iac(i))
			call rpixel(j,iybar,iac(i))
			j=j+1
  		   end do
		   call hline(ii+1,ii+639,iybar,icl)
		endif
	   else if (m.eq.80) then
		if(iybar.gt.0) then
		   iyo=iybar
		   iybar=iybar-1
		   j=ii
		   do i=0,639
	 	      call wpixel(j,iyo,iac(i))	!rewrite prev data
			call rpixel(j,iybar,iac(i))	!record data in new pos before drawing line
			j=j+1
   		   end do
		   call hline(ii+1,ii+639,iybar,icl)
		endif
	   endif
c	   call set_cursor(60,0)
	   call LOCATE(0,50)
	   print *,iybar
c	   call SET_STRING(strings(i),12)
	   a=getch(ktype)
	   if(ktype.eq.2) a=LC(a)	!convert to lower case
	   m=ichar(a)

	end do
	endif
	end

