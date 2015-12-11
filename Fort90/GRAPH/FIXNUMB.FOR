	subroutine FIXNUMB(imode,nbox,csfac,csize,ifnt,inumx,inumy,
     & logx,logy,rx,ry,angle,ixjus,iyjus,idraw,rxbox,rybox,
     & numbx,numby,cnumx,cnumy,cexpx,cexpy)
c
c To fix axis numbers (much like FIXTEXT)
c
c IMODE=1 Move,delete or rotate individual number (#inum) (or change format
c	 eg no of figs after decimal point?- or do this using 'new text'?)
c	The number to be operated on is specified by its text box number=nbox
c (=index in postion arrays angle,...,RYBOX)
c IMODE=2 Move or delete all X axis numbers at once
c IMODE=3 Move or delete all Y axis numbers at once
c IMODE=4 Change size or font for ALL axis numbers
c
c For IMODE=2 can also hit F2 to define new Y-position for the X axis
c For IMODE=3 can also hit F2 to define new X-position for the Y axis
c
c IDRAW=0  omit
c IDRAW=1  draw (no box)
c IDRAW=-1 draw with box
c	character ch,getch,ch1,fndcur
	character ch,getch,ch1
	integer fndcur
	integer*2 ktype
	real RX(100),RY(100)
	real rxbox(4,100),rybox(4,100)
	integer IXJUS(100),IYJUS(100)
c	integer iangle(100),idraw(100)
	integer idraw(100)
	real angle(100)
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25)
	integer charheight,charwidth
	logical logx,logy,xaxis
c	logical caplock
c
c
	if(imode.eq.1.and.nbox.lt.31.or.nbox.gt.80) then
	   call SCROLL(0,0,2,79,1)		!scroll lines (0,2) only
	   call LOCATE(0,2)		!row=0,col 0
	   print 3,nbox
3	   format(' BOX # ',i3,' IS NOT AN AXIS NUMBER: TRY AGAIN')
	   call BELL(2)
	   nbox=-1
	   RETURN
	endif
c Initialise
	if(imode.eq.1) xaxis=nbox.ge.31.and.nbox.le.55
	if(imode.eq.2) xaxis=.true.
	if(imode.eq.3) xaxis=.false.
	itog=0
c Start with course movements
	id=200	!for move
	da=45.  	!for angle
	cs=1.		!for size
	sizen=csfac*csize
	call SETCSIZE(sizen,isize)		!set isize
	call SETFONT1(ifnt)		    	!set font
	ih=charheight(isize)
	iw=charwidth(isize)
	ixoff=-ifix(float(iw)/5.)	!for numbers in exponent form
	iyoff=ih/2
c Look for a keystroke
1	continue
c      ch=char(ITTINR())	!(this gets letters etc but not arrows etc?)
	ch=GETCH(ktype)
	ival=ichar(ch)
c MYSTERY BUG -value of xaxis changes when GETCH is called!!! No idea why
c but seems to be cured if Xaxis reset here
	if(imode.eq.1) xaxis=nbox.ge.31.and.nbox.le.55
	if(imode.eq.2) xaxis=.true.
	if(imode.eq.3) xaxis=.false.
c===end of bug fix
c Test whether to finish
	if(ktype.eq.16) goto 99				!ESC
	if(ktype.eq.4.and.ival.eq.13) goto 99	!ENTER
c Toggle the sensitivity
	if(ktype.eq.8.and.ival.eq.82) then	!INS toggles fine/course
	   if(id.eq.10) then
		id=200	!for move
		da=20.		!for angle
		cs=1.		!for size
	   else
		id=10
		da=1.
		cs=0.25
	   endif
	endif
c Next keys that require text deletion and redraw
c To move all X (or all Y) numbers to new position marked by cursor,
c mark the origin (the first number on the appropriate axis) with
c a diamond, and put up cursors for new origin position before
c deleting the original. NB can't use SYMBOL routine to draw this
c because it will not draw outside the graphboundary, so must use
c PLOT calls which use device (not world) coord. Have no way to
c to filled symbol when this used?
	if(ktype.eq.0.and.(ival.ge.60.and.ival.le.68)) then	!F2-F10
	   if(imode.eq.1.or.imode.eq.4) goto 1	!F key not valid
	   call SCROLL(0,0,2,79,1)		!scroll lines (0,2) only
	   call LOCATE(0,2)		!row=0,col 0
	   if(imode.eq.2) print 30
30		format(
     &   '&Define new Y-position for the X axis with horizontal cursor')
	   if(imode.eq.3) print 31
31		format(
     &   '&Define new X-position for the Y axis with vertical cursor')
	   call PUTCUR(5000,5000)		!start pos for cursor
c	   ch1 = FNDCUR(ixnew,iynew)			!posn to move text #it to
	   idum=FNDCUR(ixnew,iynew)			!posn to move text #it to
	endif
c
c First DELETE NUMBER
	call COLTYP(-1)				!erase
	if(imode.eq.1) then
	   j=nbox
	   if(xaxis) then
	      i=j-30		!index=31-55 for x axis
		call DRAWNUMB(cnumx(i),cexpx(i),ixoff,iyoff,sizen,
     &      rx(j),ry(j),ixjus(j),iyjus(j),angle(j),logx,inumx)
	   else
	      i=j-55		!index=56-80 for y axis
		call DRAWNUMB(cnumy(i),cexpy(i),ixoff,iyoff,sizen,
     &      rx(j),ry(j),ixjus(j),iyjus(j),angle(j),logy,inumy)
	   endif
	endif
	if(imode.eq.2.or.imode.eq.4) then
	   do 60 i=1,numbx
		j=i+30		!index=31-55 for x axis
		call DRAWNUMB(cnumx(i),cexpx(i),ixoff,iyoff,sizen,
     &      rx(j),ry(j),ixjus(j),iyjus(j),angle(j),logx,inumx)
60	   continue
	endif
	if(imode.eq.3.or.imode.eq.4) then
	   do 61 i=1,numby
		j=i+55		!index=56-80 for x axis
		call DRAWNUMB(cnumy(i),cexpy(i),ixoff,iyoff,sizen,
     &      rx(j),ry(j),ixjus(j),iyjus(j),angle(j),logy,inumy)
61	   continue
	endif
	call COLTYP(0)				!restore
c
c Check which key hit
	ixd=0
	iyd=0
	goto(11,12,12,14) imode
c
11	continue	!IMODE=1 (move,delete,rotate single number)
	if(ktype.eq.8) then
c Section for ktype=8 (arrow key)
c calc new position
	   ix=ISCRX(rx(nbox))
	   iy=ISCRY(ry(nbox))
	   if(ival.eq.77.or.ival.eq.73.or.ival.eq.81) ix=ix+id	!right
	   if(ival.eq.75.or.ival.eq.71.or.ival.eq.79) ix=ix-id	!left
	   if(ival.eq.71.or.ival.eq.72.or.ival.eq.73) iy=iy+id	!up
	   if(ival.eq.79.or.ival.eq.80.or.ival.eq.81) iy=iy-id	!down
	   if(ival.eq.83) then			!DEL toggles idraw
		if(idraw(nbox).ne.0) then
		   idraw(nbox)=0
		else
		   idraw(nbox)=1
		endif
c		pause 'toggle idraw'
	   endif
	else if(ktype.eq.2) then
	   if(ival.eq.114) then		!'r' increases angle
		angle(nbox)=angle(nbox)+da
	   else if(ival.eq.82) then		!'R' decreases angle
		angle(nbox)=angle(nbox)-da
	   endif
	endif
	goto 100	!redraw number
c
12	continue	!IMODE=2 or 3 (move,delete all X (or Y) axis numbers)
	if(ktype.eq.0.and.(ival.ge.60.and.ival.le.68)) then	!F2-F10
c Calc iyd as diff from ry for 1st number on the X axis
	   if(imode.eq.2) iyd=iynew-ISCRY(ry(31))
	   if(imode.eq.3) ixd=ixnew-ISCRX(rx(56))
	else if(ktype.eq.8) then
c Section for ktype=8 (arrow key)
c calc new position
	   if(ival.eq.77.or.ival.eq.73.or.ival.eq.81) ixd=id	!right
	   if(ival.eq.75.or.ival.eq.71.or.ival.eq.79) ixd=-id	!left
	   if(ival.eq.71.or.ival.eq.72.or.ival.eq.73) iyd=id	!up
	   if(ival.eq.79.or.ival.eq.80.or.ival.eq.81) iyd=-id	!down
	   itog=0
	   if(ival.eq.83) itog=1	!signal to toggle idraw() below
	endif
	goto 100	!redraw number
c
14	continue	!IMODE=4 (change size or font for ALL axis numbers)
	if(ktype.eq.3) then
	   if(ival.eq.43) sizen=sizen+cs		!plus
	   if(ival.eq.45) sizen=sizen-cs		!minus
	   call SETCSIZE(sizen,isize)		!set isize
c        Adjust csfac for return to calling prog
	   csfac=sizen/csize
	else if(ktype.eq.2) then
	   if(ival.eq.42) ifnt=ifnt+1  		!*
	   if(ival.eq.47) ifnt=ifnt-1  		!/
	   call SETFONT1(ifnt)		!set font
	endif
c
c REDRAW NUMBERS and record new position if altered
100	continue
	if(imode.eq.1) then
	   if(idraw(nbox).eq.0) goto 1	!do not redraw
	   j=nbox
	   angle(j)=amod(angle(j),360.)		!keep in range 0-360
	   if(angle(j).lt.0) angle(j)=360.+angle(j)
	   rx(j)=xworld(ix)
	   ry(j)=yworld(iy)
	   if(xaxis) then
	      i=j-30		!index=31-55 for x axis
		call DRAWNUMB(cnumx(i),cexpx(i),ixoff,iyoff,sizen,
     &      rx(j),ry(j),ixjus(j),iyjus(j),angle(j),logx,inumx)
		nl=nblank1(cnumx(i))
		call TEXTBOX(ix,iy,cnumx(i)(1:nl),angle(j),isize,
     & 	ixjus(j),iyjus(j),rxbox(1,j),rybox(1,j),0)
	   else
	      i=j-55		!index=56-80 for y axis
		call DRAWNUMB(cnumy(i),cexpy(i),ixoff,iyoff,sizen,
     &      rx(j),ry(j),ixjus(j),iyjus(j),angle(j),logy,inumy)
		nl=nblank1(cnumy(i))
		call TEXTBOX(ix,iy,cnumy(i)(1:nl),angle(j),isize,
     & 	ixjus(j),iyjus(j),rxbox(1,j),rybox(1,j),0)
	   endif
	endif		!end of imode=1
	if(imode.eq.2.or.imode.eq.4) then
	   do 62 i=1,numbx
		j=i+30		!index=31-55 for x axis
		if(imode.eq.2) then
c	         ix=ix+ixd	!alter posn of all numbers by same amount
c	         iy=iy+iyd
		   ix=ISCRX(rx(j)) + ixd	!alter posn of all numbers by same amount
		   iy=ISCRY(ry(j)) + iyd
	         rx(j)=xworld(ix)
	         ry(j)=yworld(iy)
c 		If itog=1 then toggle all idraw
		   if(itog.eq.1) then
			if(idraw(j).ne.0) then
			   idraw(j)=0
			else
			   idraw(j)=1
			endif
		   endif
		endif
c		if(caplock()) print 200,j,idraw(j),i,cnumx(i),rx(j),ry(j),
c     &	rxbox(1,j),rybox(1,j)
c200		format(' FIXNUMB:',
c     &	'j,idraw(j),i,cnum(i),rx(j),ry(j),rxbox(1,j),rybox(1,j)= ',
c     &	/,2i4,4x,i4,1x,a10,4g13.6)
		if(idraw(j).eq.0) goto 62	!skip draw
		call DRAWNUMB(cnumx(i),cexpx(i),ixoff,iyoff,sizen,
     &      rx(j),ry(j),ixjus(j),iyjus(j),angle(j),logx,inumx)
		nl=nblank1(cnumx(i))
		call TEXTBOX(ix,iy,cnumx(i)(1:nl),angle(j),isize,
     & 	ixjus(j),iyjus(j),rxbox(1,j),rybox(1,j),0)
62	   continue
	endif		!end of imode=2 or 4
	if(imode.eq.3.or.imode.eq.4) then
	   do 63 i=1,numby
		j=i+55		!index=56-80 for y axis
		if(imode.eq.3) then
c	         ix=ix+ixd	!alter posn of all numbers by same amount
c	         iy=iy+iyd
		   ix=ISCRX(rx(j)) + ixd	!alter posn of all numbers by same amount
		   iy=ISCRY(ry(j)) + iyd
	         rx(j)=xworld(ix)
	         ry(j)=yworld(iy)
c 		If itog=1 then toggle all idraw
		   if(itog.eq.1) then
			if(idraw(j).ne.0) then
			   idraw(j)=0
			else
			   idraw(j)=1
			endif
		   endif
		endif
c		if(caplock()) print 200,j,idraw(j),i,cnumy(i),rx(j),ry(j),
c     &	rxbox(1,j),rybox(1,j)
		if(idraw(j).eq.0) goto 63	!skip draw
		call DRAWNUMB(cnumy(i),cexpy(i),ixoff,iyoff,sizen,
     &      rx(j),ry(j),ixjus(j),iyjus(j),angle(j),logy,inumy)
		nl=nblank1(cnumy(i))
		call TEXTBOX(ix,iy,cnumy(i)(1:nl),angle(j),isize,
     & 	ixjus(j),iyjus(j),rxbox(1,j),rybox(1,j),0)
63	   continue
	endif		!end of imode=3 or 4
c
	GOTO 1	!ANOTHER MOVE?
c
99	continue
	RETURN
	end

	subroutine DRAWNUMB(cnum,cexp,ixoff,iyoff,
     & sizen,rx,ry,ixj,iyj,ang,logxy,inumxy)
	logical logxy
	character*10 cnum,cexp

c To draw a number at predefined position in LAXES1 and FIXNUMB
c
	   ix=ISCRX(rx)
	   iy=ISCRY(ry)
	   nl=NBLANK1(cnum)
	   call SETCSIZE(sizen,isize)		!set isize
	   call JUSTIFYSTRING(ix,iy,cnum(1:nl),ang,isize,ixj,iyj)
	   if(logxy.and.inumxy.eq.1) then	!draw superscript
		call GETCURRPOS(ix,iy)
		ix=ix+ixoff
		iy=iy+iyoff
		call PLOT(ix,iy,0)	!move to superscript pos
		nl=NBLANK1(cexp)
		call SETCSIZE(0.8*sizen,isize)	!this defines ISIZE
		call WRITESTRING(cexp(1:nl),0,isize)
		call SETCSIZE(sizen,isize)	!reset
	   endif
c
	RETURN
	end


