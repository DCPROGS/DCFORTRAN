	integer*4		iac(0:641)
	character*1 ch,getch
	integer*2 ktype

	call MODE(18)
	ibk=3
	call CLEARS(0,479,ibk)
	ixlo=11
	ixhi=496
	iylo=365
	iyhi=418
	ic=8
c NB at present ixhi should be integer multiple of 8
	ixhi=8*(ixhi/8) + 8	!round up
	call FILLWIN(ixlo,iylo,ixhi,iyhi,ic)
	ch=getch(ktype)
	iylo=iylo-10
	iyhi=iyhi-10
	ic=ic+1
c===
	ixlo=ixlo+1
	ixhi=ixhi-1
	call FILLWIN(ixlo,iylo,ixhi,iyhi,ic)
c===
c	call FILLWIN(ixlo+1,iylo,ixhi-1,iyhi,ic)
c now pause
	ch=getch(ktype)
	ix=320	!initially
	call VLINE(ix,0,479,13)
	call set_cursor(60,0)
	print *,ix
	call MOVE_VLINE(ix,14,iac,30)
	iylo=iylo-10
	iyhi=iyhi-10
	ic=ic+1
	call FILLWIN(ixlo+2,iylo,ixhi-2,iyhi,ic)
	ch=getch(ktype)
	iylo=iylo-10
	iyhi=iyhi-10
	ic=ic+1
	call FILLWIN(ixlo+3,iylo,ixhi-3,iyhi,ic)
	ch=getch(ktype)
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


