
	subroutine modstr(ixlo,ixhi,iy,title,nr,nrc,ic,ibk,icc)

c	similar with DC TITENT
c	uses wrstring18 for writing with background
c	ixlo,ixhi,iy coordinates of selected string
c	title	:string to be modified
c	ic	:colour writing
c     ibk	:colour background
c     icc	:colour cursor

	character*(*) title(nr)
	character	title1(:)
	character*1 ch,getch
	logical mouse_on
	common/mousval/mouse_on,nbutton

	allocatable :: title1
	nlen=int(float(ixhi-ixlo+1)/8-2)
	allocate(title1(nlen))
	call GBLANK(title(nrc),nlen,n1,n2)	!check where current 'title' starts/ends
	ix=ixlo+8
	i=1
	call setcur18(ix,iy)
	call hline(ix,ix+7,iy-1,icc)
c	call hline(ix,ix+7,iy+1,icc)
	call set_cursor(1,1)
c	print*,ix,i,iy
10	continue
	call GBLANK(title(nrc),nlen,n1,n2)	!check where current 'title' starts/ends
	ch=GETCH(ktype)

	ival=ichar(ch)
	if(ktype.eq.4.and.ival.eq.13) goto 99	! ENTER key to end
c Home/End/Arrow keys -move cursor only

	if(ktype.eq.8.and.ival.eq.75) then		!LEFT arrow key
	   	if(ix.gt.ixlo+8) then
			call hline(ix,ix+7,iy-1,ibk)
c			call hline(ix,ix+7,iy+1,ibk)
			ix=ix-8	!move cursor back
			i=i-1
			call hline(ix,ix+7,iy-1,icc)
c			call hline(ix,ix+7,iy+1,icc)
	   	endif
	   	call setcur18(ix,iy)
	   	goto 10
	else if(ktype.eq.8.and.ival.eq.77) then		!RIGHT arrow key
	   	if(ix.lt.ixhi-16) then
c			call hline(ix,ix+7,iy+1,ibk)
			call hline(ix,ix+7,iy-1,ibk)
			ix=ix+8	!move cursor on
			i=i+1
			call hline(ix,ix+7,iy-1,icc)
c			call hline(ix,ix+7,iy+1,icc)
	   	endif
	   	call setcur18(ix,iy)
	   	goto 10
	else if(ktype.eq.8.and.ival.eq.71) then		!HOME key
		call hline(ix,ix+7,iy-1,ibk)
c		call hline(ix,ix+7,iy+1,ibk)
	   	ix=ixlo+n1*8
		i=n1
		call hline(ix,ix+7,iy-1,icc)
c		call hline(ix,ix+7,iy+1,icc)
	   	call setcur18(ix,iy)
	   	goto 10
	else if(ktype.eq.8.and.ival.eq.79) then		!END key
		call hline(ix,ix+7,iy-1,ibk)
c		call hline(ix,ix+7,iy+1,ibk)
		ix=ixlo+n2*8			!posn after end of text
		i=n2
c		call hline(ix,ix+7,iy+1,icc)
		call hline(ix,ix+7,iy-1,icc)
	   	call setcur18(ix,iy)
	   	goto 10
	else if(ktype.eq.8.and.ival.eq.72) then		!up key
		if(nrc.gt.1) then
		call hline(ix,ix+7,iy-1,ibk)
c		call hline(ix,ix+7,iy+1,ibk)
		iy=iy+16		!posn after end of text
		nrc=nrc-1
c		call hline(ix,ix+7,iy+1,icc)
		call hline(ix,ix+7,iy-1,icc)
	   	call setcur18(ix,iy)
		endif
	   	goto 10
	else if(ktype.eq.8.and.ival.eq.80) then		!down key
		if(nrc.lt.nr) then
		call hline(ix,ix+7,iy-1,ibk)
c		call hline(ix,ix+7,iy+1,ibk)
		iy=iy-16		!posn after end of text
		nrc=nrc+1
c		call hline(ix,ix+7,iy+1,icc)
		call hline(ix,ix+7,iy-1,icc)
	   	call setcur18(ix,iy)
		endif
	   	goto 10
	endif
c Other keys -move cursor and print character on screen (and replace in 'title')
c DEL moves all characters one place to left, overwriting the char at the
c cursor posn
	if(ktype.eq.8.and.ival.eq.8) then		!BACKSPACE key
	   	if(i.gt.1.and.ix.gt.ixlo+8) then
			call hline(ix,ix+7,iy-1,ibk)
c			call hline(ix,ix+7,iy+1,ibk)
	   		do j=i-1,nlen-1
				title1(j)=title(nrc)(j+1:j+1)
			enddo
	   		do j=i-1,nlen-1
				title(nrc)(j:j)=title1(j)
			enddo
			title(nrc)(nlen:nlen)=' '

			call fillwin(ixlo+5,iy,ixhi-8,iy+15,ibk)
			call wrstring18(ixlo+8,iy,title(nrc),ic,ibk)
			ix=ix-8		!move cursor back
			i=i-1
	   		call setcur18(ix,iy)
			call hline(ix,ix+7,iy-1,icc)
c			call hline(ix,ix+7,iy+1,icc)
	   	endif
	else if(ktype.eq.8.and.ival.eq.83) then		!DEL key
		if(i.le.n2) then
	   		do j=i,n2
	   			title(nrc)(j:j)=title(nrc)(j+1:j+1)
			enddo
			call fillwin(ixlo+5,iy,ixhi-8,iy+15,ibk)
			call wrstring18(ixlo+8,iy,title(nrc),ic,ibk)
			call hline(ix,ix+7,iy-1,icc)
c			call hline(ix,ix+7,iy+1,icc)
	   		call setcur18(ix,iy)
		endif
	else
	   	if(i.ge.nlen) goto 10
		call hline(ix,ix+7,iy-1,ibk)
c		call hline(ix,ix+7,iy+1,ibk)
	   	do j=i+1,nlen
			title1(j)=title(nrc)(j-1:j-1)
		enddo
	   	title1(i)=ch		!put new character into 'title' (i=0-n so mc+1)
	   	do j=i,nlen
			title(nrc)(j:j)=title1(j)
		enddo
		call fillwin(ixlo+5,iy,ixhi-8,iy+15,ibk)
		call wrstring18(ixlo+8,iy,title(nrc),ic,ibk)
		i=i+1
		ix=ix+8
		call hline(ix,ix+7,iy-1,icc)
c		call hline(ix,ix+7,iy+1,icc)
	endif
	goto 10				!next char
99	continue	!ensure that TITLE ends with ASCII 0
	call hline(ix,ix+7,iy-1,ibk)
c	call hline(ix,ix+7,iy+1,ibk)

	RETURN
	END

