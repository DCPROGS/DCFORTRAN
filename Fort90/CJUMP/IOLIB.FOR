
c===========================================================
c	module iolib.for
c	contains all Fortran subroutines used in the program
c============================================================

c	Subroutine for fast scrolling backwards

	subroutine DRAWPIX(ix,idata,ndisp,ndata,inc,ipen,ibk,nbuff,nover)
C	------------------------------------------------------------------
	integer*2 ix(2048)
	integer*2 idata(1-nover:nbuff+nover)      ! ie 1-81920 + overlap extensions

c
	j=ndata
	k=ndata-inc
    	do i=1,ndisp
	   	call rpixel(ix(i),idata(j),ic)
	   	if(ic.eq.ipen) then
			call wpixel(ix(i),idata(j),ibk)	!erase old
	   	endif
		j=j+1
	 	call rpixel(ix(i),idata(k),ic)
	 	if(ic.eq.ibk.and.idata(k).ne.0) then
	   		call wpixel(ix(i),idata(k),ipen)	!draw new
	 	endif
		k=k+1
	end do
	END

C************************************************************************
c	Subroutine to delete an array of pixels

	subroutine delPIX(ix,idata,ndisp,ndata,ipen,ibk,nbuff,nover)
C	------------------------------------------------------------------
	integer*2 ix(2048)
	integer*2 idata(1-nover:nbuff+nover)      ! ie 1-81920 + overlap extensions

	j=ndata
    	do i=1,ndisp
	   	call rpixel(ix(i),idata(j),ic)
	   	if(ic.eq.ipen) then
			call wpixel(ix(i),idata(j),ibk)	!erase old
	   	endif
		j=j+1
	end do
	END


C************************************************************************

c	This subroutine load a display array

	subroutine	load_array(screen, ix_in, iy_in, ix_up, iy_up)
C	------------------------------------------------------
	integer*2 	screen (ix_in:ix_up, iy_in:iy_up)

	do i=iy_in,iy_up
		do j=ix_in,ix_up
			call rpixel(j,i,icolor)
			screen(j,i)=icolor
		end do
	end do

	end

C************************************************************************


c     This will restore the screen after erasing the window
	subroutine	restore_array(screen, ix_in, iy_in, ix_up, iy_up,ibk)
C	-------------------------------------------------------------
	integer*2 	screen (ix_in:ix_up, iy_in:iy_up)

	do i=iy_in,iy_up
		do j=ix_in,ix_up
			if(screen(j,i).ne.ibk) then
				call wpixel(j,i,screen(j,i))
			endif
		end do
	end do
	end

C************************************************************************

C************************************************************************

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
			if((iybar+1).lt.480) then
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
			if(iybar.gt.80) then
				iyo=iybar
				iybar=iybar-1
				j=ii
				do i=0,639
		 			call wpixel(j,iyo,iac(i))
					call rpixel(j,iybar,iac(i))
				j=j+1
     				end do
				call hline(ii+1,ii+639,iybar,icl)
			endif
		endif
		a=getch(ktype)
		if(ktype.eq.2) a=LC(a)	!convert to lower case
		m=ichar(a)

	end do
	endif
	end


C************************************************************************

c	subroutine to draw a filled rectangle with the frame with another color
	subroutine drawrec(ix1,ix2,iy1,iy2,icolor,icline)



	call fillwin(ix1,ix2,iy1,iy2,icolor)
	call hline(ix1,ix2,iy1,icline)
	call hline(ix1,ix2,iy2,icline)
	call vline(ix1,iy1,iy2,icline)
	call vline(ix2,iy1,iy2,icline)
	end

C************************************************************************
	subroutine drawframe(ix1,ix2,iy1,iy2,icline)

	call hline(ix1,ix2,iy1,icline)
	call hline(ix1,ix2,iy2,icline)
	call vline(ix1,iy1,iy2,icline)
	call vline(ix2,iy1,iy2,icline)
	end

C************************************************************************

c*******************************************************************************

c	draw a special cursor for put cursors
	subroutine draw_cursor(ixcur,iycur,icolcur,icur)


c	icur=colcur-8

	call hline(ixcur,ixcur+4,iycur,icur)
	call hline(ixcur,ixcur+4,iycur-7,icur)
	do i=ixcur+1,ixcur+3
		call vline(i,iycur-6,iycur-1,icolcur)
	end do
	call vline(ixcur+4,iycur-7,iycur,icur)
	call vline(ixcur,iycur-7,iycur,icur)

	end


c*******************************************************************************
	subroutine put_cursors(ii,ixframe,iyframe,icframe,iprogx1,icprog,
	&		     ixcr,iycr,iccr,ixmark,iymark,icmark,ibdx,ibk)

c called with X origin ii=0
	ifram1=iccr-8
	ifram2=icmark-8
c DC ixframe+ii altered to ixframe+ii-1
c Draw top and bottom of progress line frame
	call hline(ii,ixframe+ii-1,iyframe,icframe)
	call hline(ii,ixframe+ii-1,iyframe-5,icframe)
c Draw verticals at ends and to mark sections
	do i=ii,ixframe+ii-1,ibdx	!=1,ibdx,2ibdx,3ibdx (ibdx=pixels/section)
		call vline(i,iyframe-5,iyframe,icframe)
	end do
	call vline(ixframe+ii-1,iyframe-5,iyframe,icframe)
c=====================================================================
c Draw current progress
	do i=1+ii,iprogx1+ii
		call rpixel(i,iyframe-1,innc)
		if (innc.eq.ibk) call wpixel(i,iyframe-1,icprog)
		call rpixel(i,iyframe-2,innc)
		if (innc.eq.ibk) call wpixel(i,iyframe-2,icprog)
		call rpixel(i,iyframe-3,innc)
		if (innc.eq.ibk) call wpixel(i,iyframe-3,icprog)
		call rpixel(i,iyframe-4,innc)
		if (innc.eq.ibk) call wpixel(i,iyframe-4,icprog)
	end do
c Draw markers
	call draw_cursor(ixcr+ii,iycr,iccr,ifram1)
	call draw_cursor(ixmark+ii,iymark,icmark,ifram2)
	end

c*******************************************************************************

	subroutine move_red_cursor(ixcr,iycr,iccr,ixframe,ibk)

	character*1	      ch,getch,LC

     	ifram1=iccr-8
	ch=getch(ktype)
	if(ktype.eq.2) ch=LC(ch)	!convert to lower case
	ival=ichar(ch)
	do while (ival.ne.13)
		if (ktype.eq.8) then
				if(ival.eq.77) then
					call draw_cursor(ixcr,iycr,ibk,ibk)
					if (ixcr.lt.ixframe) ixcr=ixcr+5
					call draw_cursor(ixcr,iycr,iccr,ifram1)
				else if(ival.eq.75) then
					call draw_cursor(ixcr,iycr,ibk,ibk)
					if (ixcr.gt.5) ixcr=ixcr-5
					call draw_cursor(ixcr,iycr,iccr,ifram1)
				else if(ival.eq.71) then		!HOME key
					call draw_cursor(ixcr,iycr,ibk,ibk)
					ixcr=0
					call draw_cursor(ixcr,iycr,iccr,ifram1)
				else if(ival.eq.79) then		!END key
					call draw_cursor(ixcr,iycr,ibk,ibk)
					ixcr=ixframe-4
					call draw_cursor(ixcr,iycr,iccr,ifram1)
				endif
		endif
		ch=getch(ktype)
		if(ktype.eq.2) ch=LC(ch)	!convert to lower case
		ival=ichar(ch)
	end do
	end
c*******************************************************************************

	subroutine move_progress_line(isec0,isecf,isec1,iyframe,icprog,
     &					ibk,flag_pline)


      if(flag_pline.eq.0) then
		do iisecf=isec0,isecf  	!MOVE UPPER LINE
			call rpixel(iisecf,iyFRAME-1,icp)
			if (icp.eq.ibk) call wpixel(iisecf,iyframe-1,icprog)
			call rpixel(iisecf,iyframe-2,icp)
			if (icp.eq.ibk) call wpixel(iisecf,iyframe-2,icprog)
		end do

		do iisecf=isec0,isec1  !MOVE LOWER LINE
			call rpixel(iisecf,iyframe-3,icp)
			if (icp.eq.ibk) call wpixel(iisecf,iyframe-3,icprog)
				call rpixel(iisecf,iyframe-4,icp)
			if (icp.eq.ibk) call wpixel(iisecf,iyframe-4,icprog)
		end do

	else if(flag_pline.eq.1) then
		do iisecf=isecf,isec0  !MOVE upper LINE
			call rpixel(iisecf,iyframe-1,icp)
			if (icp.eq.icprog) call wpixel(iisecf,iyframe-1,ibk)
				call rpixel(iisecf,iyframe-2,icp)
			if (icp.eq.icprog) call wpixel(iisecf,iyframe-2,ibk)
		end do

	endif
	end
c*******************************************************************************
	subroutine MOVE_PROG_LINE(indfst,backward)
c DC version of Ioana's move_progress_line.for
c Upper and lower lines are each 2 pixels deep but both lines of pixels are
c same colour so no need to read both? In fact is it nec to read EITHER
c -surely can just draw/erase as nec.? but need to read if vertical partitions
c thet show section boundaries are not to be deleted when rolling backwards
c
c iprogx0=current position of lower line on entry (and reset to new position
c 	on exit)
c Input:
c iprogx1=X position required (in # of pixels) for upper line
c iprogx2=X position required (in # of pixels) for lower line
c (NB at present iprogx2=iprogx1 normally, but need iprogx2 to restore
c prog line after leaving expmode)
c
c Will need colour code later to show which parts are (a) fitted, (b) bad
c (c) not yet analysed
c
c Note that if rolling back, iprogx0>iprogx1
	logical backward
	common/prog/iprogx0,iprogx1,iprogx2,ipmax,ibdpp,iyframe,icprog,ibk
c
c calc  X pixel # for 1st pint on display, from indfst
	iprogx1=1 + indfst/ibdpp	!current X coord (# of pixels) for upper prog line
c==	iprogx1=ipxs+ifixr(float(isdfst)/float(ibdpp))
c==	ipxs=(isec-1)*ibdx+1	!ibdx=pixels/section on prog line
c==	indfst=isdfst + (isec-1)*nbuf
c==	ibdx=ifix(640./float(nsec))     ! pixels per section for prog line
c==	ibdpp=nbuf/ibdx		! data points per pixel on progress line

	if(backward) then
		do i=iprogx1,iprogx0  !erase upper LINE only, backwards
			call rpixel(i,iyframe-1,icp)
			if (icp.eq.icprog) call wpixel(i,iyframe-1,ibk)
			call rpixel(i,iyframe-2,icp)
			if (icp.eq.icprog) call wpixel(i,iyframe-2,ibk)
		end do
      else		!not backward
		do i=iprogx0,iprogx1  	!draw UPPER LINE
			call rpixel(i,iyFRAME-1,icp)
			if (icp.eq.ibk) call wpixel(i,iyframe-1,icprog)
			call rpixel(i,iyframe-2,icp)
			if (icp.eq.ibk) call wpixel(i,iyframe-2,icprog)
		end do

		do i=iprogx0,iprogx2  !draw LOWER LINE
			call rpixel(i,iyframe-3,icp)
			if (icp.eq.ibk) call wpixel(i,iyframe-3,icprog)
			call rpixel(i,iyframe-4,icp)
			if (icp.eq.ibk) call wpixel(i,iyframe-4,icprog)
		end do
	endif
c Update values before leaving
	m=max(iprogx0,iprogx1,iprogx2)
	if(m.gt.ipmax) ipmax=m	!save max value reached (for newprogline)
	iprogx0=iprogx1
	iprogx2=iprogx1
	RETURN
	end

	subroutine NEWPROGLINE()
	common/prog/iprogx0,iprogx1,iprogx2,ipmax,ibdpp,iyframe,icprog,ibk
c To draw entirely new progress line up to iprogx1,iprogx2
c Delete old line (also deletes section markers in this form)
	do i=1,ipmax	!delete up to furthest forward point
	   do j=1,4
		call wpixel(i,iyframe-j,ibk)
	   enddo
	enddo
c draw upper line
	do i=1,iprogx1
	   do j=1,4
		call wpixel(i,iyframe-j,icprog)
	   enddo
	enddo
c draw lower line
	do i=1,iprogx2
	   do j=1,4
		call wpixel(i,iyframe-j,icprog)
	   enddo
	enddo
	ipmax=1		!reset for next time
	RETURN
	end
c***************************************************************************

	subroutine first_help1(ibk)
c (old version -fast version is called first_help)
	allocatable :: helpy
	integer*2 helpy(:,:),ktype
	character*1 ch,a,getch,LC
	character*53 hstring(43)
	integer nt(10)	!for timer

	call TIMER(nt(1))
	hstring(1)='FORWARDS : press <F> or move the scroll bar on '
	hstring(2)='   <Forwards> option and press <Enter>.'

	hstring(3)='BACKWARDS : press <B> or move the scroll bar on '
	hstring(4)='   <Backwards> option and press <Enter>.'

	hstring(5)='EXPAND : press <E> or move the scroll bar on <Expand>'
	hstring(6)='   option and press <Enter>; This will take you to  '
	hstring(7)='   the Slow Scrolling Mode.'
	hstring(8)='ENABLE CURSOR : press <A> or move the scroll bar on'
	hstring(9)='   <enAble cursor> option and press <Enter>;move the'
	hstring(10)='  red cursor at the section you want to analyse ,'
	hstring(11)='  using the arrows, and press <Enter> again.'
	hstring(12)='INCREASE SPEED : press<I> or move the scroll bar on '
	hstring(13)='  <Increase speed> option and press <Enter>.'
	hstring(14)='DECREASE SPEED : press<C> or move the scroll bar on '
	hstring(15)='  <deCrease speed> option and press <Enter>.'
	hstring(16)='CHANGE NDISPLAY : press<P> or move the scroll bar on'
	hstring(17)='  <change ndisPlay> option and press <Enter> ;enter'
	hstring(18)='  the new number of points to be shown on the screen'
	hstring(19)='OPTIONS :  '
	hstring(20)=' '
	hstring(21)='UPPER BAR : press <R> or move the scroll bar on     '
	hstring(22)='  <uppeR bar> option and press <Enter>;move the bar '
	hstring(23)='  at the level you want, using the arrows, and press'
	hstring(24)='  <Enter> again.'
	hstring(25)='LOWER BAR : press <L> or move the scroll bar on     '
	hstring(26)='  <Lower bar> option and press <Enter>;move the bar '
	hstring(27)='  at the level you want, using the arrows, and press'
	hstring(28)='  <Enter> again.'
	hstring(29)='THRESHOLD BAR : press <T> or move the scroll bar on '
	hstring(30)='  <Threshold bar> option and press <Enter>;move the '
	hstring(31)='  bar at the level you want, using the arrows, and '
	hstring(32)='  press <Enter> again.'
	hstring(33)='DISPLAY DATA :'
	hstring(34)=' '
	hstring(35)='NEXT SECTION : press <N> or move the scroll bar on'
	hstring(36)='  <Next section> option and press <Enter>.'
	hstring(37)='STOP : press <S> or move the scroll bar on <Stop>'
	hstring(38)='  option and press <Enter>.'
	hstring(39)='QUIT : press <Q> or move the scroll bar on <Quit>'
	hstring(40)='  option and press <Enter>.'
	hstring(41)='HELP : press <H> or move the scroll bar on <Help>'
	hstring(42)='  option and press <Enter>.'
	ilmax=43

	ic0=0
	ic1=11
	ic2=14
	ixin=96
	ixup=536
	iyin=128
	iyup=432
	ixh1=ixin-8
	ixh2=ixup+8
	iyh1=iyin-8
	iyh2=iyup+8
	call TIMER(nt(2))

	allocate(helpy (ixh1:ixh2,iyh1:iyh2))
	call TIMER(nt(3))
	call load_array(helpy, ixh1,iyh1,ixh2,iyh2)
	call TIMER(nt(4))
c	call loadar(ixh1,ixh2,iyh1,iyh2,helpy)
	ixc=int(float(ixin)/8.)+1
	iyc=30-int(float(iyup)/16.)


	call fillwin(ixh1,ixh2,iyh1,iyh2,ic0)
	call hline (ixh1+2,ixh2-2,iyh1+2,ic1)
	call hline (ixh1+2,ixh2-2,iyh2-2,ic1)

	call hline (ixh1+2,ixh2-2,iyh2-44,ic1)
	call hline (ixh1+2,ixh2-2,iyh1+34,ic1)


	call vline (ixh1+2,iyh1+2,iyh2-2,ic1)
	call vline (ixh2-2,iyh1+2,iyh2-2,ic1)


	call set_cursor(ixc+22,iyc)
	call put_string('HELP',ic1,ic2,ch)
	call set_cursor(ixc,iyc+18)
	call put_string('<PgDn>=next page',ic1,ic2,ch)
	call set_cursor(ixc+20,iyc+18)
	call put_string('<PgUp>=previous page',ic1,ic2,ch)
	call set_cursor(ixc+43,iyc+18)
	call put_string('<Esc>=exit',ic1,ic2,ch)
	call TIMER(nt(5))


	i=1
	idi=14
	iyc=iyc+3
	call show_page(i,idi,ixc,iyc,hstring,ic1,ic2)
	call TIMER(nt(6))
	i=1
	a=getch(ktype)
	if(ktype.eq.2) a=LC(a)	!convert to lower case
	m=ichar(a)
	do while(m.ne.27)
		select case(m)
			case(81)
				if (i.lt.(ilmax-idi)) then
					i=i+idi
					call fillwin(ixh1+8,ixh2-8,
     &iyh1+36,iyh2-46,ic0)
					call show_page(i,idi,ixc,iyc,hstring,
     &				ic1,ic2)
				endif
			case(73)
				if(i.gt.idi) then
					i=i-idi
					call fillwin(ixh1+8,ixh2-8,
     &iyh1+36,iyh2-46,ic0)
					call show_page(i,idi,ixc,iyc,hstring,
     &				ic1,ic2)
				endif
		end select
		a=getch(ktype)
		if(ktype.eq.2) a=LC(a)	!convert to lower case
		m=ichar(a)

	end do
	call TIMER(nt(7))
	call fillwin(ixh1,ixh2,iyh1,iyh2,ibk)
	call restore_array(helpy, ixh1,iyh1,ixh2,iyh2,ibk)
c	call restar(helpy, ixh1,iyh1,ixh2,iyh2,ibk)
	call TIMER(nt(8))
	deallocate (helpy)
	call TIMER(nt(9))
c type timer
	call LOCATE(4,0)
	do i=1,8
	   print 1,i,10.*float(nt(i+1)-nt(i))
1	   format(1x,i2,': time (ms) = ',f10.1)
	enddo
	pause
c
	RETURN
	end

c*******************************************************************************


	subroutine show_page(i,idi,ixc,iyc,hstring,ic1,ic2)
	character*1 ch
	character*53	hstring(43)
	iyci=iyc
	do j=i,i+idi-1
		call set_cursor(ixc,iyci)
		call put_string(hstring(j),ic1,ic2,ch)
	iyci=iyci+1
	end do

	end

c*******************************************************************************


	subroutine find_extr(idata,ih,minim,maxim,inx,ixmin,ixmax,
     &				iybar1,iybar2,ix,nbuff,nover)

	integer*2 	idata(1-nover:nbuff+nover)
	integer*2	ixmin(50),ixmax(50),inx
	integer*2	minim(50),maxim(50),ix(2048)


	first=1
	i=1
	do while(idata(ih).lt.iybar1)
1	continue
	ih=ih+1
	inx=inx+1
	if (idata(ih).gt.idata(ih+1)) goto 1
	if (first.eq.1) then
		minim(1)=idata(ih)
		ixmin(1)=ix(inx-1)
		iybar2=minim(1)
	else
c		if (maxim(i-1)-minim(i).gt.idelta) then
			minim(i)=idata(ih)
c		end if
		ixmin(i)=ix(inx-1)
	end if
 2	continue
	ih=ih+1
	inx=inx+1
	if (idata(ih).lt.idata(ih+1)) goto 2
c	if (maxim(i)-minim(i).gt.idelta) then
		maxim(i)=idata(ih)
		ixmax(i)=ix(inx-1)
c	endif
	i=i+1
	first=0
	if (i.gt.45) goto 3

	end do

3	do i=1,40
c		if (maxim(i).ne.0) call wpixel(ixmax(i),maxim(i),4)
c		if (minim(i).ne.0) call wpixel(ixmin(i),minim(i),0)
	end do
	end
c*******************************************************************************
c*******************************************************************************



	SUBROUTINE ENTER_STRING(IX,IY,STRING,ICOLOR,IBK)
C	------------------------------------------------------------------
c	ix,iy in graphic
c     This calculates the mapping from
c	graphic 640 x 480 to alphanumeric 80 x 30
c	calculates length of string
c	put string in 2 colors (all upper cases in other color)
c	restore foreground

c     IX must 8 multiple
c	IY must be 16 multiple

C     USES DET_STRIG WITCH RETURNS UPPER CASE CHARACTER

	character*(*)     string
	character*1		charac

	n=len(string)
	ix=ix-mod(ix,8)
	iy=iy-mod(iy,16)
	ixf=ix+n*8
	iyf=iy+16

c	calculate the mapping for text:

	ixc=int(float(ix)/8.)
	iyc=29-int(float(iy)/16.)

c     put string:

	call set_cursor(ixc,iyc)
	call det_string(string,icolor,charac)

c     restore background:

	do i=ix,ixf
		do j=iy,iyf
			call rpixel(i,j,ic)
			if (ic.eq.0) call wpixel(i,j,ibk)
		enddo
	enddo
	end

C************************************************************************

	SUBROUTINE ENTER_STR(IX,IY,STRING,ICOLOR,IBK)
C	------------------------------------------------------------------
c	ix,iy in graphic
c     This calculates the mapping from
c	graphic 640 x 480 to alphanumeric 80 x 30
c	calculates length of string
c	put string in 1 color
c	restore foreground

c     IX must 8 multiple
c	IY must be 16 multiple
	character*(*)     string

	n=len(string)
	ix=ix-mod(ix,8)
	iy=iy-mod(iy,16)
	ixf=ix+n*8
	iyf=iy+16
c	print*,n,ixf,iyf
c	calculate the mapping for text:

	ixc=int(float(ix)/8.)
	iyc=29-int(float(iy)/16.)

c     put string:

	call set_cursor(ixc,iyc)
	call set_string(string,icolor)

c     restore background:

	do i=ix,ixf
		do j=iy,iyf
			call rpixel(i,j,ic)
			if (ic.eq.0) call wpixel(i,j,ibk)
		enddo
	enddo
	end

C************************************************************************

	SUBROUTINE ENTER_STR0(IX,IY,STRING,ICOLOR)
C	------------------------------------------------------------------
C	FOR IBK=0 ;DOESN'T NEED TO RESTORE THE FOREGROUND
c	ix,iy in graphic
c     This calculates the mapping from
c	graphic 640 x 480 to alphanumeric 80 x 30
c	calculates length of string
c	put string in 1 color
c     IX must 8 multiple
c	IY must be 16 multiple

	character*(*)     string

	ix=ix-mod(ix,8)
	iy=iy-mod(iy,16)
c	calculate the mapping for text:

	ixc=int(float(ix)/8.)
	iyc=29-int(float(iy)/16.)

c     put string:

	call set_cursor(ixc,iyc)
	call set_string(string,icolor)

	end

C************************************************************************
	subroutine first_help(ic1,ic2)


	character*1 	ch,a,getch
	character*53	rind(43)
	integer*1		h_array(:)
	allocatable :: h_array

	rind(1)='FORWARDS : press <F> or move the scroll bar on '
	rind(2)='   <Forwards> option and press <Enter>.'

	rind(3)='BACKWARDS : press <B> or move the scroll bar on '
	rind(4)='   <Backwards> option and press <Enter>.'

	rind(5)='EXPAND : press <E> or move the scroll bar on <Expand>'
	rind(6)='   option and press <Enter>; This will take you to  '
	rind(7)='   the Slow Scrolling Mode.'
	rind(8)='ENABLE CURSOR : press <A> or move the scroll bar on'
	rind(9)='   <enAble cursor> option and press <Enter>;move the'
	rind(10)='  red cursor at the section you want to analyse ,'
	rind(11)='  using the arrows, and press <Enter> again.'
	rind(12)='INCREASE SPEED : press<I> or move the scroll bar on '
	rind(13)='  <Increase speed> option and press <Enter>.'
	rind(14)='DECREASE SPEED : press<C> or move the scroll bar on '
	rind(15)='  <deCrease speed> option and press <Enter>.'
	rind(16)='CHANGE NDISPLAY : press<P> or move the scroll bar on'
	rind(17)='  <change ndisPlay> option and press <Enter> ;enter'
	rind(18)='  the new number of points to be shown on the screen'
	rind(19)='OPTIONS :  '
	rind(20)=' '
	rind(21)='UPPER BAR : press <R> or move the scroll bar on     '
	rind(22)='  <uppeR bar> option and press <Enter>;move the bar '
	rind(23)='  at the level you want, using the arrows, and press'
	rind(24)='  <Enter> again.'
	rind(25)='LOWER BAR : press <L> or move the scroll bar on     '
	rind(26)='  <Lower bar> option and press <Enter>;move the bar '
	rind(27)='  at the level you want, using the arrows, and press'
	rind(28)='  <Enter> again.'
	rind(29)='THRESHOLD BAR : press <T> or move the scroll bar on '
	rind(30)='  <Threshold bar> option and press <Enter>;move the '
	rind(31)='  bar at the level you want, using the arrows, and '
	rind(32)='  press <Enter> again.'
	rind(33)='DISPLAY DATA :'
	rind(34)=' '
	rind(35)='NEXT SECTION : press <N> or move the scroll bar on'
	rind(36)='  <Next section> option and press <Enter>.'
	rind(37)='STOP : press <S> or move the scroll bar on <Stop>'
	rind(38)='  option and press <Enter>.'
	rind(39)='QUIT : press <Q> or move the scroll bar on <Quit>'
	rind(40)='  option and press <Enter>.'
	rind(41)='HELP : press <H> or move the scroll bar on <Help>'
	rind(42)='  option and press <Enter>.'
	ilmax=43

	ic0=0
	ixh1=88
	ixh2=552
	iyh1=112
	iyh2=448
	ixin=ixh1+8
	iyup=iyh2-16
	ixc=int(float(ixin)/8.)+1
	iyc=30-int(float(iyup)/16.)

	idimen=(iyh2-iyh1+1)*(ixh2-ixh1+1)
	allocate(h_array(idimen))
	call rdgwin1(ixh1,iyh1,ixh2,iyh2,h_array)

	call fillwin(ixh1,ixh2,iyh1,iyh2,ic0)
	call hline (ixh1+2,ixh2-2,iyh1+2,ic1)
	call hline (ixh1+2,ixh2-2,iyh2-2,ic1)

	call hline (ixh1+2,ixh2-2,iyh2-44,ic1)
	call hline (ixh1+2,ixh2-2,iyh1+34,ic1)


	call vline (ixh1+2,iyh1+2,iyh2-2,ic1)
	call vline (ixh2-2,iyh1+2,iyh2-2,ic1)


	call set_cursor(ixc+22,iyc)
	call put_string('HELP',ic1,ic2,ch)
	call set_cursor(ixc,iyc+18)
	call put_string('<PgDn>=next page',ic1,ic2,ch)
	call set_cursor(ixc+20,iyc+18)
	call put_string('<PgUp>=previous page',ic1,ic2,ch)
	call set_cursor(ixc+43,iyc+18)
	call put_string('<Esc>=exit',ic1,ic2,ch)


	i=1
	idi=14
	iyc=iyc+3
	call show_page(i,idi,ixc,iyc,rind,ic1,ic2)
	i=1
	a=getch(b)
	m=ichar(a)
	do while(m.ne.27)
		select case(m)
			case(81)
				if (i.lt.(ilmax-idi)) then
					i=i+idi
					call fillwin(ixh1+8,ixh2-8,
     &iyh1+36,iyh2-46,ic0)
					call show_page(i,idi,ixc,iyc,rind,
     &				ic1,ic2)
				endif
			case(73)
				if(i.gt.idi) then
					i=i-idi
					call fillwin(ixh1+8,ixh2-8,
     &iyh1+36,iyh2-46,ic0)
					call show_page(i,idi,ixc,iyc,rind,
     &				ic1,ic2)
				endif
		end select
		a=getch(b)
		m=ichar(a)

	end do
	call wrgwin1(ixh1,iyh1,ixh2,iyh2,h_array)
	deallocate (h_array)
	end

c*******************************************************************************

	subroutine one_page_menu(ix,iyf,strings,n,ic,ibk)

c	SUBROUTINE TO DRAW A MENU WITH READ/WRITE WINDOW INCLUDED
c	ix,iyf : coordonates of upper left corner of the window; I gave it like
c	this because is easier to write strings in normal way;
c	the mode used is 18 (12h)
c	strings(n) :array of strings you want to write in the window
c	ic: color of writing
c	ibk:color of background
c	useful only when you want to write something on the screen

	character*1	ans,getch
	character*(*)	strings(n)
	integer*1	arra(:)
	allocatable :: arra

c	recalculate ix to be divisible by 8
	ix=ix-mod(ix,8)
c	recalculate iy to be divisible by 16
	iyf=iyf-mod(iyf,16)
c	calculate the maximum length of strings(i)
	lenmax=len(strings(1))
	do i=2,n
		if(len(strings(i)).ge.lenmax) then
			lenmax=len(strings(i))
		endif
	enddo
c	calculate ixf of window;
	ixf=ix+(lenmax+2)*8 !here you can make it bigger
c	calculate iy of window
	iy=iyf-(n+2)*16
c	ixc0,iyc0 : writing coordinates
	ixc0=ix+8
	iyc0=iyf-32

	idimen=(iyf-iy+9)*(ixf-ix+9)+1
	allocate(arra(idimen))
	call rdgwin1(ix,iy,ixf,iyf,arra)
	call fillwin(ix,ixf,iy,iyf,ibk)
	call hline(ix+2,ixf-2,iy+2,ic)
	call hline(ix+4,ixf-4,iy+4,ic)
	call hline(ix+4,ixf-4,iyf-4,ic)
	call hline(ix+2,ixf-2,iyf-2,ic)
	call vline(ix+2,iy+2,iyf-2,ic)
	call vline(ix+4,iy+4,iyf-4,ic)
	call vline(ixf-4,iy+4,iyf-4,ic)
	call vline(ixf-2,iy+2,iyf-2,ic)
c	call clgwin(ix,iy,ixf,iyf,ibk)
	if(ibk.eq.0) then
		iyc=iyc0
		do i=1,n
			call enter_str0(ixc0,iyc,strings(i),ic)
			iyc=iyc-16
		enddo
	else
		iyc=iyc0
		do i=1,n
			call enter_str(ixc0,iyc,strings(i),ic,ibk)
			iyc=iyc-16
		enddo
	endif
1	ans=getch(b)
	in=ichar(ans)	!here you may give other options
	if(in.ne.27) goto 1
	call wrgwin1(ix,iy,ixf,iyf,arra)
	deallocate(arra)
	end
c*******************************************************************************

	recursive subroutine multiple_menu(ix,iyf,strings,n,ic,ibk,irec)

c	SUBROUTINE TO DRAW A MENU WITH READ/WRITE WINDOW INCLUDED
c	ix,iyf : coordonates of upper left corner of the window; I gave it like
c	this because is easier to write strings in normal way;
c	the mode used is 18 (12h)
c	strings(n) :array of strings you want to write in the window
c	ic: color of writing
c	ibk:color of background
c	useful only when you want to write something on the screen
c	irec: nr of recursions (windows)
	character*1		ans,getch
	character*(*)	strings(n)
	integer*1		arra(:)
	allocatable :: arra

c	recalculate ix to be divisible by 8
	ix=ix-mod(ix,8)
c	recalculate iy to be divisible by 16
	iyf=iyf-mod(iyf,16)
c	calculate the maximum length of strings(i)
	lenmax=len(strings(1))
	do i=2,n
		if(len(strings(i)).ge.lenmax) then
			lenmax=len(strings(i))
		endif
	enddo
c	calculate ixf of window;
	ixf=ix+(lenmax+2)*8 !here you can make it bigger
c	calculate iy of window
	iy=iyf-(n+2)*16
c	ixc0,iyc0 : writing coordinates
	ixc0=ix+8
	iyc0=iyf-32

	idimen=(iyf-iy+9)*(ixf-ix+9)+1
	allocate(arra(idimen))
	call rdgwin1(ix,iy,ixf,iyf,arra)
	call fillwin(ix,ixf,iy,iyf,ibk)
	call hline(ix+2,ixf-2,iy+2,ic)
	call hline(ix+4,ixf-4,iy+4,ic)
	call hline(ix+4,ixf-4,iyf-4,ic)
	call hline(ix+2,ixf-2,iyf-2,ic)
	call vline(ix+2,iy+2,iyf-2,ic)
	call vline(ix+4,iy+4,iyf-4,ic)
	call vline(ixf-4,iy+4,iyf-4,ic)
	call vline(ixf-2,iy+2,iyf-2,ic)
c	call clgwin(ix,iy,ixf,iyf,ibk)
	if(ibk.eq.0) then
		iyc=iyc0
		do i=1,n
			call enter_str0(ixc0,iyc,strings(i),ic)
			iyc=iyc-16
		enddo
	else
		iyc=iyc0
		do i=1,n
			call enter_str(ixc0,iyc,strings(i),ic,ibk)
			iyc=iyc-16
		enddo
	endif
1	ans=getch(b)
	in=ichar(ans)	!here you may give other options
	if(in.eq.13.and.irec.ge.2) then
		ix=ix+16
		iyf=iyf-32
		irec=irec-1
		call multiple_menu(ix,iyf,strings,n,ic+1,ibk+1,irec)
c3		ans=getch(b)
c		in=ichar(ans)	!here you may give other options
c		if(in.ne.27) goto 3


		ix=ix-16
		iyf=iyf+32
		irec=irec+1
	else if(in.eq.27) then
		goto 2
	else
		goto 1
	endif
	goto 1
c3		ans=getch(b)
c		in=ichar(ans)	!here you may give other options
c		if(in.ne.27) goto 1
2	call wrgwin1(ix,iy,ixf,iyf,arra)
	deallocate(arra)
	end



c*******************************************************************************

	recursive subroutine multiple_menu2(ix,iyf,strings,n,nmax,
     &							ic,ibk,irec)

c	SUBROUTINE TO DRAW A MENU WITH READ/WRITE WINDOW INCLUDED
c	ix,iyf : coordonates of upper left corner of the window; I gave it like
c	this because is easier to write strings in normal way;
c	the mode used is 18 (12h)
c	strings(irec,nmax) :array of strings you want to write in the window
c	ic(irec): color of writing
c	ibk(irec):color of background
c	useful only when you want to write something on the screen
c	irec: nr of recursions (windows)
c	n(irec) array of integers for the numbeer of strings/window
c     first on the screen window(irec) then window(irec-1),......

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
	lenmax=len(strings(irec,1))
	do i=2,n(irec)
		if(len(strings(irec,i)).ge.lenmax) then
			lenmax=len(strings(irec,i))
		endif
	enddo
c	calculate ixf of window;
	ixf=ix+(lenmax+2)*8 !here you can make it bigger
c	calculate iy of window
	iy=iyf-(n(irec)+2)*16
c	ixc0,iyc0 : writing coordinates
	ixc0=ix+8
	iyc0=iyf-32

	idimen=(iyf-iy+9)*(ixf-ix+9)+1
	allocate(arra(idimen))
	call rdgwin1(ix,iy,ixf,iyf,arra)
	call fillwin(ix,ixf,iy,iyf,ibk(irec))
	call hline(ix+2,ixf-2,iy+2,ic(irec))
	call hline(ix+4,ixf-4,iy+4,ic(irec))
	call hline(ix+4,ixf-4,iyf-4,ic(irec))
	call hline(ix+2,ixf-2,iyf-2,ic(irec))
	call vline(ix+2,iy+2,iyf-2,ic(irec))
	call vline(ix+4,iy+4,iyf-4,ic(irec))
	call vline(ixf-4,iy+4,iyf-4,ic(irec))
	call vline(ixf-2,iy+2,iyf-2,ic(irec))
	if(ibk(irec).eq.0) then
		iyc=iyc0
		do i=1,n(irec)
			call enter_str0(ixc0,iyc,strings(irec,i),ic(irec))
			iyc=iyc-16
		enddo
	else
		iyc=iyc0
		do i=1,n(irec)
			call enter_str(ixc0,iyc,strings(irec,i),ic(irec),
     &		ibk(irec))
			iyc=iyc-16
		enddo
	endif
1	ans=getch(b)
	in=ichar(ans)	!here you may give other options
	if(in.eq.13.and.irec.ge.2) then
		ix=ix+16
		iyf=iyf-32
		irec=irec-1
		call multiple_menu2(ix,iyf,strings,n,nmax,ic,ibk,irec)
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



c*******************************************************************************
