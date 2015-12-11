c*******************************************************************************
	recursive subroutine ilvmenu(ix,iyf,strings,n,nmax,ic,ibk,irec)

c	SUBROUTINE TO DRAW A MENU WITH READ/WRITE WINDOW INCLUDED
c	ix,iyf : coordonates of upper left corner of the window; I gave it like
c	this because is easier to write strings in normal way;
c	the mode used is 18 (12h)
c	strings(irec,nmax) :array of strings you want to write in the window
c	ic: color of writing
c	ibk:color of background
c	useful only when you want to write something on the screen
c	irec:	nr of menues

	character*1		ans,getch
	character*(*)	strings(irec,nmax)
	integer*1		arra(:)
	integer*2		n(irec),ic(irec),ibk(irec)
	allocatable :: arra

c	recalculate ix to be divisible by 8
	ix=ix-mod(ix,8)
c	recalculate iy to be divisible by 16
	iyf=iyf-mod(iyf,16)
c	calculate the maximum length of strings(i)
	lenmax=nblank(strings(irec,1))
	do i=2,n(irec)
		if(nblank(strings(irec,i)).ge.lenmax) then
			lenmax=nblank(strings(irec,i))
		endif
	enddo
c	calculate ixf of window;
	ixf=ix+(lenmax+2)*8-1 !here you can make it bigger
c	calculate iy of window
	iy=iyf-(n(irec)+2)*16


c	ixc0,iyc0 : writing coordinates
	ixc0=ix+8
	iyc0=iyf-32

	idimen=(iyf-iy+9)*(ixf-ix+9)+1
	allocate(arra(idimen))
	call rdgwin1(ix,iy,ixf,iyf,arra)
	call framefill(ix,iy,ixf,iyf,ic(irec),ibk(irec))
	iyc=iyc0
	do i=1,n(irec)
		call wrstring18(ixc0,iyc,strings(irec,i),ic(irec),ibk(irec))
		iyc=iyc-16
	enddo
1	ans=getch(b)
	in=ichar(ans)	!here you may give other options
	if(in.eq.13.and.irec.ge.2) then
		ix=ix+16
		iyf=iyf-32
		irec=irec-1
		call ilvmenu(ix,iyf,strings,n,nmax,ic,ibk,irec)
		ix=ix-16
		iyf=iyf+32
		irec=irec+1
	else if(in.eq.27) then
		goto 2
	else
		goto 1
	endif
	goto 1
2	call wrgwin1(ix,iy,ixf,iyf,arra)
	deallocate(arra)
	end

c*******************************************************************
