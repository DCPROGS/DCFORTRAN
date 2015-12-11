	subroutine TITENT1(query,title,nlen,blank)
c	character*1 TITLE(nlen)
	character query*(*),title*(*)
c	integer*2 ktype,mr2,mc2
	integer*4 ktype
	integer*2 mr2,mc2
	integer*2 videotyp
	character*1 ch,getch,ch1
	logical blank,vga,overtype
c Version of TITENT that prints the input title, followed by enough
c dots to mark remaining characters, to allow easy modification of
c an existing title.
c Modif 09/24/94 08:11pm to add insert/overtype
c Modif 02/09/93 03:22pm to draw cursor when in VGA mode
c Modif 02/07/93 04:16pm to declare title*(*) rather than title(nlen)
c Modif 02/07/93 03:22pm by including call to NBLANK1 to ensure that
c string ended by char(0) as required by new Hgraph (and C).
c If called with BLANK=true then input array is blanked (extra arg
c needed compared with TITENT1 used for Lahey V2.n)
c Problem: cursor is invisible in graphics mode; the calls to getcurrpos
c and upgcur should fix this (but do not!) -this seems to control only a
c graphics underline-cursor, not that used by the fortran print statements
c TTIT.FOR contains various attempts to solve this, but none work while in
c graphics mode -will need a separate version of titent written in Hgraph
c routines to get a visible cursor!
C Typical call:
c CALL TITENT('Enter title for directory:',TITLE,36)
c
	overtype=.false.		!default on entry
c CURTYP(istart*256+iend): iend controls width of cursor (12=full width)
c and istart controls height in 12 units, starting from top, so istart=11
c fills the bottom 2 rows of pixels only.
	call CURTYP(11*256+12)
c	call CURTYP(3*256+9)		!small block
c
	vga=VIDEOTYP().eq.18
	if(blank) then
	   do 12 i=1,nlen
12	   title(i:i)=' '
	   n2=0
	else
	   call GBLANK(title,nlen,n1,n2)
	endif
	if(n2.gt.nlen) n2=nlen
	print *,charnb(QUERY)
	print 11
11	format('&  (use home/end/arrows/bs/ins/del)')
	print 1
1	format(' .')	!first one
	do 2 i=1,nlen-1
2	print 3
3	format('&.')
	print 4
4	format('+')
c
c Note that cursor appears initially at position following last non-blank
c character (#n2) of input title, ie at title(n2+1), but columns are
c numbered from zero, so this corresponds to mc=n2
	if(n2.gt.0) then
	  do 6 i=1,n2
	   print 7,title(i:i)
7	   format('&',a1)
6	  continue
	endif
	call GETPOS(mr2,mc2)	!get current cursor position
	mc=int4(mc2)		!NB this numbers columns 0 to nlen-1
c	print*,mr2,mc
c Read character
	if(vga) then
	   icolor=12
	   ibk=0		!background colour to delete cursor
	   call draw_cur(mr2,mc,icolor)
	endif
10	continue
	call GBLANK(title,nlen,n1,n2)	!check where current 'title' starts/ends
	ch=GETCH(ktype)
c
	ival=ichar(ch)
	if(ktype.eq.4.and.ival.eq.13) goto 99	! ENTER key to end
c Home/End/Arrow keys -move cursor only

	if(ktype.eq.8.and.ival.eq.75) then		!LEFT arrow key
	   if(mc.gt.0) then
		if(vga) call DRAW_CUR(mr2,mc,ibk)
		mc=mc-1		!move cursor back
		if(vga) call DRAW_CUR(mr2,mc,icolor)
	   endif
	   call LOCATE(mr2,int2(mc))
		goto 10
	else if(ktype.eq.8.and.ival.eq.77) then		!RIGHT arrow key
	   if(mc.lt.nlen-1) then
		if(vga) call DRAW_CUR(mr2,mc,ibk)
		mc=mc+1	!move cursor on
		if(vga) call DRAW_CUR(mr2,mc,icolor)
	  endif
	   call LOCATE(mr2,int2(mc))
	   goto 10
	else if(ktype.eq.8.and.ival.eq.71) then		!HOME key
		if(vga) call DRAW_CUR(mr2,mc,ibk)
	   mc=0
		if(vga) call DRAW_CUR(mr2,mc,icolor)
	   call LOCATE(mr2,int2(mc))
	   goto 10
	else if(ktype.eq.8.and.ival.eq.79) then		!END key
		if(vga) call DRAW_CUR(mr2,mc,ibk)
	   mc=n2			!posn after end of text
		if(vga) call DRAW_CUR(mr2,mc,icolor)
	   if(mc.gt.nlen-1) mc=nlen-1
	   call LOCATE(mr2,int2(mc))
	   goto 10
	else if(ktype.eq.8.and.ival.eq.82) then		!INS key
	   overtype=.not.overtype		!can also change cursor shape?
	   if(overtype) then
		if(vga) then
		   icolor=11
		else
		   call CURTYP(12)
		endif
	   else
		if(vga) then
		   icolor=12
		else
		   call CURTYP(11*256+12)
		endif
	   endif
	   goto 10
	endif
c Other keys -move cursor and print character on screen (and replace in 'title')
c DEL moves all characters one place to left, overwriting the char at the
c cursor posn
	if(ktype.eq.8.and.ival.eq.8) then		!BACKSPACE key
	   if(mc.lt.1) goto 10
	   if(vga) call DRAW_CUR(mr2,mc,ibk)	!delete
	   mc=mc-1		!move back
	   call LOCATE(mr2,int2(mc))
	   do m=mc,n2-1
	      m2=m+2
	      if(m2.gt.nlen) m2=nlen	!shouldn't happen -guard against bugs!
	      if(m+1.lt.1) goto 10
		if(m.lt.n2-1) then
	         ch=title(m2:m2)		!for screen
		   ch1=ch				!for title
		else		!m.eq.n2-1
	         ch='.' 		!replace last '.'
		   ch1=' '
		endif
	      print 7,ch
	      title(m+1:m+1)=ch1
	   enddo
	   if(vga) call DRAW_CUR(mr2,mc,icolor)
	   call LOCATE(mr2,int2(mc))	!restore to orig posn after 'print'
	else if(ktype.eq.8.and.ival.eq.83) then		!DEL key
	   if(vga) call DRAW_CUR(mr2,mc,ibk)
	   do m=mc,n2-1
	      m2=m+2
	      if(m2.gt.nlen) m2=nlen	!shouldn't happen -guard against bugs!
		if(m.lt.n2-1) then
	         ch=title(m2:m2)		!for screen
		   ch1=ch				!for title
		else		!m.eq.n2-1
	         ch='.' 		!replace last '.'
		   ch1=' '
		endif
	      print 7,ch
	      title(m+1:m+1)=ch1
	   enddo
	   call LOCATE(mr2,int2(mc))	!restore to orig posn after 'print'
	   if(vga) call DRAW_CUR(mr2,mc,icolor)
	else
	   if(overtype) then
	      if(mc+1.gt.nlen) goto 10
	      title(mc+1:mc+1)=ch		!put new character into 'title' (mc=0-n so mc+1)
		if(vga) call DRAW_CUR(mr2,mc,ibk)
	      print 7,ch		!put character on screen
	      mc=mc+1			!because of print statement
	      if(vga) call DRAW_CUR(mr2,mc,icolor)
	      call LOCATE(mr2,int2(mc))
	   else		!insert mode
		if(n2+1.gt.nlen) goto 10	!full, so skip
c          Move existing characters to right
		do m=n2,mc+1,-1
		   title(m+1:m+1)=title(m:m)
		   call LOCATE(mr2,int2(m))
		   print 7,title(m+1:m+1)
		enddo
c         Insert new char
		title(mc+1:mc+1)=ch		!put new character into 'title' (mc=0-n so mc+1)
		call LOCATE(mr2,int2(mc))
		print 7,ch		!put character on screen
		mc=mc+1			!because of print statement
	      if(vga) call DRAW_CUR(mr2,mc,icolor)
		call LOCATE(mr2,int2(mc))
	   endif
	endif
	goto 10				!next char
c
99	i=NBLANK1(title)		!ensure that TITLE ends with ASCII 0
	RETURN
	END


	subroutine draw_cur(mr2,mc,icolor)
c
c	Draw a rectangular cursor in graphic mode 18 ( 640 x 480 ) but
c	using the HGRAPH viewport ( 10000 x 7000)
c	for normal text ( 80 x 30 ).
c	Hgraph corresponds to 640 x 447 pixels and the text starts with
c     the row=2

	integer*2 	mr2,mc
c	print*,mr2
	mr=float(mr2)-2.
	y=7000.-7000./28.*mr
	x=10000./80.*float(mc)
	ix=ifix(x)
	iy=ifix(y)
	call coltyp1(icolor)
	if (mc.eq.0) then
		call IFRAME(ix,ix+110,iy-250,iy)
	else
		call IFRAME(ix-15,ix+110,iy-250,iy)
	endif
	end

