	subroutine FIXLINES(ikey,nline,xlb,xle,ylb,yle,iltype,
     & nhline,yhline,xhlb,xhle,ilhtype,ihlinrel,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,
     & lth,icol,ibk)
c To adjust free, horizontal and vertical lines and grid lines in VPLOT
	integer icol(100)
	integer*2 lth(100),ilt2
c arrays for lines
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	integer iltype(10)				!line type for ditto
	integer ixb(4),iyb(4)				!for 'inside'
c new arrays for horizontal lines
	real*4 yhline(10)		!record y value
	real*4 xhlb(10),xhle(10)     !start/end of HLINE
	integer ilhtype(10)	!line type for horizontal lines
	integer*1 ihlinrel(10)	!=1 if line pos rel to line 1
c new arrays for vertical lines
	real*4 xvline(10)		!record x value
	real*4 yvlb(10),yvle(10)     !start/end of VLINE
	integer ilvtype(10)	!line type for vertical lines
	integer*1 ivlinrel(10)	!=1 if line pos rel to line 1
c
	real*4 delt(10)
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character cnum*11,ans*1,ch*1
	logical relval,delete,delrel,skip,horiz
	character*36 helpst(5)		!help strings
	integer Lb(30)		!for DCMENU
	character*78 text(18)	!for DCMENU
	common/DMENU/csizb,ifonb,csize,ifont2,nboxlast,nblast		!for DCMENU
	common/hgv/ivxlo,ivxhi,ivylo,ivyhi,ixlo,ixhi,iylo,iyhi
c
	logical mouse_on
c	logical mous_set,kbhit
	common/mousval/mouse_on,nbutton
c
c
	ict=11
	icol1=14	!yellow text/border for boxes in DRAWBOX
	icol2=8	!grey background for boxes
	icmark=0	!black
	if(ibk.eq.0) icmark=14
c
	if(ikey.eq.7) then		!del free line
	   n=nline
	   call INTCONV(n,cnum)
	   call QDIALOG(1,'Delete line number (1 to '
     &     //CHARNB(cnum)//')',' ',ict,cans)
	   call GETINPi(cans,n)
	   call COLTYP1(ibk)		!backround
	   call MOVETO(xlb(n),ylb(n),0,iltype(n))
c	   call MOVETO(xle(n),yle(n),-1,iltype(n))
	   call MOVETO(xle(n),yle(n),1,iltype(n))
	   j=0
	   do i=1,nline
	      if(i.ne.n) then	!skip deleted one
		   j=j+1
		   xlb(j)=xlb(i)
		   ylb(j)=ylb(i)
		   xle(j)=xle(i)
		   yle(j)=yle(i)
		   iltype(j)=iltype(i)
		endif
	   enddo
2702	   continue
	   nline=nline-1
	endif
c
	if(ikey.eq.8.or.ikey.eq.9) then
	 if(nhline+nvline.eq.1) then
	   n=1
	   goto 2
c Select line with mouse; imark=# of pixels each side of line that is
c counted as a hit
	 else if(mouse_on) then
	   imark=3
	   call WDIALOG(1,'Click on the line to be altered',11)
	   ntry=0
1	   if(ntry.gt.4) goto 999
	   call SHOW_MOUSE()
	   call get_mouse(ibutton,ix,iy)
	   if(ibutton.gt.0) then	!see which line we are on, if any
		in=0		!count lines found
		do i=1,nhline
		   iy0=ifixr(ISCRY(yhline(i))*0.0639)	!pixel units
		   iy1=iy0-imark
		   iy2=iy0+imark
		   if(iy.ge.iy1.and.iy.le.iy2) then
			horiz=.true.
			n=i
			in=in+1
c;		      FILLWIN(X1,Y1,X2,Y2,COLOR)
			call HIDE_MOUSE()
	    		call FILLWIN(ix-20,iy1,ix+20,iy0-1,icmark)  !mark line
	    		call FILLWIN(ix-20,iy0+1,ix+20,iy2,icmark)  !mark line
		   endif
		enddo
		do i=1,nvline
		   ix0=ifixr(ISCRX(xvline(i))*0.0639)	!pixel units
		   ix1=ix0-imark
		   ix2=ix0+imark
		   if(ix.ge.ix1.and.ix.le.ix2) then
			horiz=.false.
			n=i
			in=in+1
			call HIDE_MOUSE()
	    		call FILLWIN(ix1,iy-20,ix0-1,iy+20,icmark) !mark line
	    		call FILLWIN(ix0+1,iy-20,ix2,iy+20,icmark) !mark line
		   endif
		enddo
c -test same idea for free line
c		subroutine inside(ix,iy,ixbox,iybox,flag)
		do i=1,nline
		   ix0=ifixr(ISCRX(xlb(i))*0.0639)	!pixel units
		   ix1=ifixr(ISCRX(xle(i))*0.0639)	!pixel units
		   iy0=ifixr(ISCRY(ylb(i))*0.0639)	!pixel units
		   iy1=ifixr(ISCRY(yle(i))*0.0639)	!pixel units
		   ixb(1)=ix0
		   ixb(2)=ix0
		   ixb(3)=ix1
		   ixb(4)=ix1
		   iyb(1)=iy0-imark
		   iyb(2)=iy0+imark
		   iyb(3)=iy1+imark
		   iyb(4)=iy1-imark
		   call INSIDE(ix,iy,ixb,iyb,iflag)
		   if(iflag.eq.1) then
			n=i
			in=in+1
			call HIDE_MOUSE()
	    		call FILLWIN(ix-6,iy-6,ix+6,iy+6,icmark) !mark line
		   endif
		enddo
c
		if(in.ne.1) then
		   ntry=ntry+1
		   call WDIALOG(1,'Try again',12)
		   call BELL(1)
		   goto 1
		else
		   call INTCONV(n,cnum)
		   call WDIALOG(1,
     &		'Line '//charnb(cnum)//' to be altered',ict)
		   call HIDE_MOUSE()
		   goto 2
		endif
	   endif
	   goto 1	!look again until mouse clicked
	 endif
	else		!no mouse
	   if(ikey.eq.8) then
		call INTCONV(nhline,cnum)
	   else if(ikey.eq.9) then
		call INTCONV(nvline,cnum)
	   endif
	   n=1
 	   call DEFOLTi(n,defolt)
	   call QDIALOG(1,
     &	'Line # to be moved/deleted (1 - '//charnb(cnum)//')',
     &	 defolt,11,cans)
	   call GETINPi(cans,n)
	   call WDIALOG(1,'-Hit ENTER/ESC when done: F1 for help',11)
	endif
2	continue
c
151	call DCMENU(0,4,Lb,text,0,0)		!delete all
	nbox=10	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	Lb(7)=0
	Lb(8)=0
	Lb(9)=0
	call NUMSET			!set num lock on ready for response
	text(1)='1: MOVE LINE     '
	text(2)='2: DELETE LINE   '
	text(3)='3: FIX LINE TYPE '
	text(4)='4:FIX LINE COLOUR'
	text(5)='5:LINE THICKNESS '
	text(6)='6:LINE TYPE HELP '
	text(10)='0:  DONE        '
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
c152	ikey=nkey()
152	call CKEY(ch,ikey)
	if(ikey.lt.-1.or.ikey.gt.10) goto 152
	if(ikey.eq.10) goto 999
c Move and delete are done in SETLINE
	if(ikey.eq.6.or.ikey.eq.-1) then
c		0=solid line
c		1=dotted line
c		2-6=dashed line with increasing dash length
c		7=dash-dot
c		8=dash-dot-dot
	   helpst(1)='Line type 0-8 are:'
	   helpst(2)=' 0=solid line; 1=dotted line'
	   helpst(3)=' 2-6 dashed (inc dash length)'
	   helpst(4)=' 7=dash-dot; 8=dash-dot-dot'
	   nhelp=4
	   mxlo=70		!ixlo for poptext boxes -in common/popvals/
	   mylo=-1		!iylo for poptext boxes (-1 -> define top LH corner)
	   myhi=400		!iyhi for poptext boxes (-1 -> define bottom LH corner)
	   ictx=2		!dark green text
	   ibkx=15		!white background
	   icfx=10		!bright green border
	   call POPTEXT(mxlo,mylo,myhi,helpst,nhelp,ictx,ibkx,icfx)
	   goto 151
	endif
c
	if(ikey.eq.1.or.ikey.eq.2) then
	   if(horiz) then
		ikey=1
	   else
		ikey=2
	   endif
	   call WDIALOG(1,'Move line with mouse/arrows: F1 for help',ict)
	endif
c     Colour/line thickness in icol(72-81) horizontal,icol(82-91) vertical
c
	if(ikey.ge.3.and.ikey.le.5) then
	   if(horiz) then
	      i1=72+n-1
		ilhtsav=ilhtype(n)
	   else
	      i1=82+n-1
		ilvtsav=ilvtype(n)
	   endif
	   ilt2=lth(i1)
	endif

	SELECT CASE(ikey)

	  CASE(1)			!move/delete horizontal lines
c     if line to be moved is #1 then check if others are defined rel to #1
c
	   relval=.false.
	   if(n.eq.1) then
		do i=2,nhline
		   if(ihlinrel(i).eq.1) then
			relval=.true.
			delt(i)=yhline(i) - yhline(1)	!record diffs
		   endif
		enddo
	   endif
c
	   val=yhline(n)
	   eps=0.003*val
	   amin=xhlb(n)
	   amax=xhle(n)
c	delete existing line (immediately redrawn in SETLINE after RDGWIN1)
	   call COLTYP1(ibk)
	   call MOVETO(xhlb(n),yhline(n),0,ilhtype(n))
	   call MOVETO(xhle(n),yhline(n),1,ilhtype(n))
	   call SETLINE(.true.,val,eps,6,2,amin,amax,12,6,70,
     &	ibk,delete,ik)     !format=F6.2 at present
c If line 1 is deleted, should all those defined relative to it be deleted
c too? Could leave them, and convert them to non-relative lines
	   if(delete) then
		delrel=.false.
		if(n.eq.1.and.nhline.gt.1) then
		   nrel=0
		   do i=1,nhline
			if(ihlinrel(i).eq.1) nrel=nrel+1
		   enddo
		   if(nrel.gt.0) then
			ans='N'
			call DEFOLTa(ans,defolt)
			call QDIALOG(1,
     &		'Delete also the lines defined relative to #1',
     & 		 defolt,11,cans)
			call GETINPa(cans,ans)
			if(ans.eq.'Y') delrel=.true.
		   endif
		endif
c
	      j=0
	      do i=1,nhline
		   if(delrel) then
			skip=ihlinrel(i).eq.1.or.i.eq.n	!skip deleted ones
		   else
	       	skip=i.ne.n			!skip deleted one
			if(n.eq.1) ihlinrel(i)=0	!all absolute now
		   endif
		   if(skip) then			!skip deleted ones
			j=j+1
			xhlb(j)=xhlb(i)
			yhline(j)=yhline(i)
			xhle(j)=xhle(i)
			ilhtype(j)=ilhtype(i)
			ihlinrel(j)=ihlinrel(i)
		   endif
	      enddo
	   endif
	   if(delete) then
		if(delrel) then
		   nhline=nhline-1-nrel
		else
		   nhline=nhline-1
		endif
		goto 999
	   endif
	   yhline(n)=val		!record y value
	   xhlb(n)=amin        !start/end of HLINE
	   xhle(n)=amax
	   if(relval) then
		ans='Y'
c Move relative lines ALWAYS!
c		call DEFOLTa(ans,defolt)
c		call QDIALOG(1,
c     &	'Move also the lines defined relative to #1',
c     &	 defolt,11,cans)
c		call GETINPa(cans,ans)
		if(ans.eq.'Y') then
		   do i=2,nhline
			yhline(i)=delt(i) + yhline(1)
		      xhlb(i)=amin        !make same length as line 1
			xhle(i)=amax
		   enddo
		endif
	   endif

	  CASE(2)			!move/delet vertical lines
c     if line to be moved is #1 then check if others are defined rel to #1
c
	   relval=.false.
	   if(n.eq.1) then
		do i=2,nvline
		   if(ivlinrel(i).eq.1) then
			relval=.true.
			delt(i)=xvline(i) - xvline(1)	!record diffs
		   endif
		enddo
	   endif
c
	   val=xvline(n)
	   eps=0.003*val
	   amin=yvlb(n)
	   amax=yvle(n)
c	delete existing line (immediately redrawn in SETLINE after RDGWIN1)
	   call COLTYP1(ibk)
	   call MOVETO(xvline(n),yvlb(n),0,ilvtype(n))
	   call MOVETO(xvline(n),yvle(n),1,ilvtype(n))
	   call SETLINE(.false.,val,eps,6,2,amin,amax,12,6,70,
     &	ibk,delete,ik)     !format=F6.2 at present
c If line 1 is deleted, should all those defined relative to it be deleted
c too? Could leave them, and convert them to non-relative lines
	   if(delete) then
		delrel=.false.
		if(n.eq.1.and.nvline.gt.1) then
		   nrel=0
		   do i=1,nvline
			if(ivlinrel(i).eq.1) nrel=nrel+1
		   enddo
		   if(nrel.gt.0) then
			ans='N'
			call DEFOLTa(ans,defolt)
			call QDIALOG(1,
     &		'Delete also the lines defined relative to #1',
     & 		 defolt,11,cans)
			call GETINPa(cans,ans)
			if(ans.eq.'Y') delrel=.true.
		   endif
		endif
c
	      j=0
	      do i=1,nvline
		   if(delrel) then
			skip=ivlinrel(i).eq.1.or.i.eq.n	!skip deleted ones
		   else
	       	skip=i.ne.n			!skip deleted one
			if(n.eq.1) ivlinrel(i)=0	!all absolute now
		   endif
		   if(skip) then			!skip deleted ones
			j=j+1
			yvlb(j)=yvlb(i)
			xvline(j)=xvline(i)
			yvle(j)=yvle(i)
			ilvtype(j)=ilvtype(i)
			ivlinrel(j)=ivlinrel(i)
		   endif
	      enddo
	   endif
	   if(delete) then
		if(delrel) then
		   nvline=nvline-1-nrel
		else
		   nvline=nvline-1
		endif
		goto 999
	   endif
	   xvline(n)=val		!record y value
	   yvlb(n)=amin        !start/end of HLINE
	   yvle(n)=amax
	   if(relval) then
		ans='Y'
c Move relative lines ALWAYS!
c		call DEFOLTa(ans,defolt)
c		call QDIALOG(1,
c     &	'Move also the lines defined relative to #1',
c     &	 defolt,11,cans)
c		call GETINPa(cans,ans)
		if(ans.eq.'Y') then
		   do i=2,nvline
			xvline(i)=delt(i) + xvline(1)
		      yvlb(i)=amin        !make same length as line 1
			yvle(i)=amax
		   enddo
		endif
	   endif

	CASE(3)
	   if(horiz) then
		i=ilhtype(n)
	   else
		i=ilvtype(n)
	   endif
	   call DEFOLTi(i,defolt)
	   call QDIALOG(1,'Line type (0-8)',defolt,ict,cans)
	   call GETINPi(cans,i)
	   if(horiz) then
		ilhtype(n)=i
	   else
		ilvtype(n)=i
	   endif

	CASE(4)
	   call COLBAR(1,0,1200,ixlo,ixhi,iylo,iyhi,ifont2)
 	   call DEFOLTi(icol(i1),defolt)
	   call QDIALOG(1,
     &	'Colour; at present',defolt,ict,cans)
	   call GETINPi(cans,icol(i1))

	CASE(5)
	   ilt4=int4(lth(i1))
 	   call DEFOLTi(ilt4,defolt)
	   call QDIALOG(1,
     &	'Line thickness (integer); at present',defolt,ict,cans)
	   call GETINPi(cans,ilt4)
	   lth(i1)=int2(ilt4)

c
	END SELECT
c
c delete box that marks chosen line
	if(horiz) then
	   call FILLWIN(ix-20,iy1,ix+20,iy2,ibk)
	else
	   call FILLWIN(ix1,iy-20,ix2,iy+20,ibk)
	endif
	if(ikey.ge.3.and.ikey.le.5) then	!delete and redraw line
	   call COLTYP1(ibk)					!delete
	   call LINWID(int4(ilt2))
	   if(horiz) then
		call MOVETO(xhlb(n),yhline(n),0,ilhtsav)
		call MOVETO(xhle(n),yhline(n),1,ilhtsav)
	      call COLTYP1(icol(i1))				!redraw
	      call LINWID(int4(lth(i1)))
		call MOVETO(xhlb(n),yhline(n),0,ilhtype(n))
		call MOVETO(xhle(n),yhline(n),1,ilhtype(n))
	   else
		call MOVETO(xvline(n),yvlb(n),0,ilvtsav)
		call MOVETO(xvline(n),yvle(n),1,ilvtsav)
	      call COLTYP1(icol(i1))				!redraw
	      call LINWID(int4(lth(i1)))
		call MOVETO(xvline(n),yvlb(n),0,ilvtype(n))
		call MOVETO(xvline(n),yvle(n),1,ilvtype(n))
	   endif
	endif
	goto 151	!more changes?
999	continue
	RETURN
	end

