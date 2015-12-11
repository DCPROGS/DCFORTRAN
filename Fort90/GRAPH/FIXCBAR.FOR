	subroutine FIXCBAR(rx,ry,rlthk,idev,itx,ity,ntx,nty,tlen,idraw)
c To move calibration bars (length fixed)
c To draw idr=1 (or delete, idr=-1) calibration bars
c	rx(1),ry(1)=origin of X-calibration bars (world units),
c	rx(2),ry(2)=end of X calibration bars
c	rx(3),ry(3)=origin of Y-calibration bars (world units),
c	rx(4),ry(4)=end of Y calibration bars
c (horizontal and vertical so ry(1)=ry(2), and rx(3)=rx(4)
c	idraw(1) controls whether bars are drawn or not
	integer*2 ktype
	integer irx(4),iry(4)
	real RX(100),RY(100)
	logical both,movx
c	character*1 ch,ans,UC,getch,fndcur
	character*1 ch,ans,UC,getch
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	integer fndcur		!note new usage
	integer Lb(12)
	character*78 text(12)
c
101	format(a1)
c	lastrow=2			!NB args of SCROLL=integer
c	lrow=int2(lastrow)	!but args of LOCATE are int*2
	ict=11	!text colour for dialog box 1
	xcalib=rx(2)-rx(1)
	ycalib=ry(4)-ry(3)
	do 13 i=1,4
c	 irx(i)=ISCRX(rx(i))
c	 iry(i)=ISCRY(ry(i))
13	continue
c
	if(ntx.ne.-1000.or.nty.ne.-1000) then	!only x OR y has calib bar
	   both=.true.
	   goto 40
	endif
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row=lrow, col 0
c	print 3082
c3082	format('&Move X and Y bars together [Y] = ')
c	read 101,ans
	ans='Y'
 	call DEFOLTa(ans,defolt)
	call QDIALOG(1,'Move X and Y bars together'
     &	,defolt,ict,cans)
	call GETINPa(cans,ans)
	both=UC(ans).ne.'N'
c	call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c	call LOCATE(lrow,0)		!row=lrow, col 0
	if(.not.both) then
c	   print 31
c31	   format('&Move X bar then hit ENTER or ESC')
	   call WDIALOG(1,'Move X bar then hit ENTER or ESC',ict)
	   movx=.true.
	else
c	   print 32
c32	   format('&Move bars then hit ENTER or ESC')
	   call WDIALOG(1,'Move bars then hit ENTER or ESC',ict)
	endif
c
40	continue
	call DCMENU(0,5,Lb,text,0,0)	!delete all
	call SETLBOX(2,Lb,1)
	text(1)=
     & 'Move=ARROWS (or F2=cursor); invert=I'
      text(2)=
     & 'Delete/undel=DEL; hi/low sensitivity=INS; end=ENTER'
	call DCMENU(2,1,Lb,text,14,8)
c
	idraw=1
c Start with course movements
	rid=0.1*ycalib !for move
c Look for a keystroke
1	continue
	ch=GETCH(ktype)
	ival=ichar(ch)
c Test whether to finish
	if(ktype.eq.16) goto 99				!ESC
	if(ktype.eq.4.and.ival.eq.13) goto 99	!ENTER
c
c Toggle the sensitivity
	if(ktype.eq.8.and.ival.eq.82) then	!INS toggles fine/course
	   if(rid.eq.0.1*ycalib) then
		rid=0.01*ycalib	!for move
	   else
		rid=0.1*ycalib
	   endif
	goto 1	!another key
	endif
c Next keys that require deletion of bar(s) and redraw
c First delete bar(s) (id=-1)
	call DCALBAR(rx,ry,rlthk,idev,-1,itx,ity,ntx,nty,tlen)
c Check which key hit
	if(ktype.eq.8) then
c Section for ktype=8 (arrow key)
c calc new position
	   if(ival.eq.71.or.ival.eq.72.or.ival.eq.73) then		!up
		if(both) then
		   ry(1)=ry(1)+rid
		   ry(2)=ry(1)
		   ry(3)=ry(3)+rid
		   ry(4)=ry(3)+ycalib
		else if(movx) then
		    ry(1)= ry(1)+rid
		    ry(2)= ry(1)
		else		!move Y bar only
		    ry(3)= ry(3)+rid
		    ry(4)=ry(3)+ycalib
		endif
	   endif
	   if(ival.eq.79.or.ival.eq.80.or.ival.eq.81) then	!down
		if(both) then
		    ry(1)= ry(1)-rid
		    ry(2)= ry(1)
		    ry(3)= ry(3)-rid
		    ry(4)= ry(3)+ycalib
		else if(movx) then
		   ry(1)=ry(1)-rid
		   ry(2)=ry(1)
		else		!move Y bar only
		   ry(3)=ry(3)-rid
		   ry(4)=ry(3)+ycalib
		endif
	   endif
	   if(ival.eq.71.or.ival.eq.75.or.ival.eq.79) then	!left
		if(both) then
		    rx(1)= rx(1) - rid
		    rx(2)= rx(1) + xcalib
		    rx(3)= rx(3) - rid
		    rx(4)= rx(3)
		else if(movx) then
		    rx(1)= rx(1) - rid
		    rx(2)= rx(1) + xcalib
		else		!move Y bar only
		    rx(3)= rx(3)-rid
		    rx(4)= rx(3)
		endif
	   endif
	   if(ival.eq.73.or.ival.eq.77.or.ival.eq.81) then	!right
		if(both) then
		    rx(1)= rx(1)+rid
		    rx(2)=rx(1)+xcalib
		    rx(3)= rx(3)+rid
		    rx(4)= rx(3)
		else if(movx) then
		    rx(1)= rx(1)+rid
		    rx(2)=rx(1)+xcalib
		else		!move Y bar only
		    rx(3)= rx(3)+rid
		    rx(4)= rx(3)
		endif
	   endif
c
	   if(ival.eq.83) then			!DEL toggles idraw
		if(idraw.eq.1) then
		   idraw=0
		else
		   idraw=1
		endif
	   endif
c
	   do 11 i=1,4
c		rx(i)=XWORLD(irx(i))
c		ry(i)=YWORLD(iry(i))
11       continue
	endif			!end of ktype.eq.8
c F2 key
	if(ktype.eq.0.and.ival.eq.60) then	!F2
c		call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c		call LOCATE(lrow,0)		!row=lrow, col 0
		if(both) then
 		   call WDIALOG(1,'Mark origin of calibration bars',ict)
c		   print 34
c34		   format('&Mark origin of calibration bars')
		else if(movx) then
c		   print 35
c35		   format('&Mark origin of X-calibration bar')
 		   call WDIALOG(1,'Mark origin of X-calibration bar',ict)
		else		!move Y bar only
c		   print 36
c36		   format('&Mark origin of Y-calibration bar')
 		   call WDIALOG(1,'Mark origin of Y-calibration bar',ict)
		endif
		x=(rx(1))
		y=(ry(1))
c		call PUTCUR(ix+100,iy+100)
c      	ch=char(FNDCUR(ix1,iy1))	!note new usage
		call CURPOS(x,y)
		CALL CURSOR(IKEY,x1,y1) !note new usage
      	ch=char(ikey)
      	x=x1
	      y=y1
		if(both) then
c	rx(1),ry(1)=origin of X-calibration bars (world units),
c	rx(2),ry(2)=end of X calibration bars
c	rx(3),ry(3)=origin of Y-calibration bars (world units),
c	rx(4),ry(4)=end of Y calibration bars
c (horizontal and vertical so ry(1)=ry(2), and rx(3)=rx(4)
		   rx(1)=x
		   ry(1)=y
		   rx(2)=rx(1)+xcalib
		   ry(2)=ry(1)
		   rx(3)=x
		   ry(3)=y
		   rx(4)=rx(3)
		   ry(4)=ry(3)+ycalib
		else if(movx) then
		   rx(1)=x
		   ry(1)=y
		   rx(2)=rx(1)+xcalib
		   ry(2)=ry(1)
		else		!move Y bar only
		   rx(3)=x
		   ry(3)=y
		   rx(4)=rx(3)
		   ry(4)=ry(3)+ycalib
		endif
		do 14 i=1,4             !in case moved after this
c		 irx(i)=ISCRX(rx(i))
c		 iry(i)=ISCRY(ry(i))
14		continue
	endif
c Invert (moves X-bar to top of ybar, and reverses tics: y-bar unchanged)
	if(ktype.eq.2.and.ival.eq.73.or.ival.eq.105) then	 !'i' or 'I' inverts bars
	   if(itx.eq.1) then
		ry(1)=ry(4)		!so x-bar is at TOP of ybar
		ry(2)=ry(1)		!so horizontal
		itx=-1		!reverse tics
	   else if(itx.eq.-1) then
		ry(1)=ry(3)		!so x-bar is at BOTTOM of ybar
		ry(2)=ry(1)		!so horizontal
		itx=1			!normal tics
	   endif
	   do 15 i=1,4             !in case moved after this
c		 irx(i)=ISCRX(rx(i))
c		 iry(i)=ISCRY(ry(i))
15	   continue
	endif
c
c and redraw
	call DCALBAR(rx,ry,rlthk,idev,idraw,itx,ity,ntx,nty, tlen)
	goto 1	!another move?
c
99	continue
	if(.not.both) then
	   if(movx) then
c		call SCROLL(0,0,lastrow,79,1)		!scroll lines (0,lastrow) only
c		call LOCATE(lrow,0)		!row=lrow, col 0
c		print 33
c33	      format('&Move Y bar then hit ENTER or ESC')
 		call WDIALOG(1,'Move Y bar then hit ENTER or ESC',ict)
	      movx=.false.
	      goto 1
	   endif
	endif
	RETURN
	end
