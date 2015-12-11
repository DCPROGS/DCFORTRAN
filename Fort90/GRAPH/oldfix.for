	subroutine FIXLINES(ikey,nline,xlb,xle,ylb,yle,iltype,
     & nhline,yhline,xhlb,xhle,ilhtype,ihlinrel,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,
     & lth,icol,ibk)
c==========================================================================
c To adjust free, horizontal and vertical lines and grid lines in VPLOT
	integer icol(100)
	real lth(100),lt4
c arrays for lines
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	integer iltype(10)				!line type for ditto
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
	integer Lb(30)		!for DCMENU
	character*78 text(30)	!for DCMENU
	character  chint*3, ans,getch
c	integer itype(10),lcol(10),thick(10)
      real*4  xlo(10),ylo(10),xhi(10),yhi(10)
c
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character cnum*11,ch*1
	logical delete,horiz,help,logx,logy,sqrty,kbhit
	logical mouse_on
	common/DMENU/ifonb,csize,ifont2,nboxlast,nblast		!for DCMENU
	common/hlp/help		!for QDIALOG
	common/mousval/mouse_on,nbutton
	common/logval/logx,logy,sqrty,pon

	ict=11
	icol1=14	!yellow text/border for boxes in DRAWBOX
	icol2=8	!grey background for boxes
	nbox=10 !number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call DCMENU(0,5,Lb,text,0,0)		!delete all
	Lb(7)=0
	Lb(8)=0
C	Lb(9)=0
	if (ikey.eq.7) Lb(1)=0
	call NUMSET			!set num lock on ready for response
	text(1)='1: MOVE LINE     '
	text(2)='2: DELETE LINE   '
	text(3)='3: FIX LINE TYPE '
	text(4)='4: FIX LINE COLOUR'
	text(5)='5: LINE THICKNESS '
	text(6)='6: LINE TYPE HELP '
	text(9)='9: PICK LINE     '
	text(10)='0: DONE        '
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
c	ix0=ix
c	iy0=iy

	if (ikey.eq.7) then
	   call wdialog(1,'MODIFY LINE',14 )
	else if (ikey.eq.8) then
	   call wdialog(1,'MODIFY HORIZONTAL LINE',14 )
	else
	   call wdialog(1,'MODIFY VERTICAL LINE',14 )
	endif
 1    continue
	if (ikey.eq.7) then
		n1=nline
		do i=1,n1
		   xlo(i)=xlb(i)
	         xhi(i)=xle(i)
 	         ylo(i)=ylb(i)
 	         yhi(i)=yle(i)
		enddo
	else if (ikey.eq.8) then
		n1=nhline
		do i=1,n1
		   xlo(i)=xhlb(i)
	         xhi(i)=xhle(i)
 	         ylo(i)=0.9*yhline(i)
 	         yhi(i)=1.1*yhline(i)
		enddo
	else
		n1=nvline
		do i=1,n1
		   xlo(i)=0.9*xvline(i)
	         xhi(i)=1.1*xvline(i)
 	         ylo(i)=yvlb(i)
 	         yhi(i)=yvle(i)
		enddo
	endif
      if(xlo(i).gt.xhi(i)) then
         t=xhi(i)
         xhi(i)=xlo(i)
         xlo(i)=t
         t=yhi(i)
         yhi(i)=ylo(i)
         ylo(i)=t
      endif
	do i=1,n1
	   if(logx) then
	     xlo(i)=alog10(xlo(i))
	     xhi(i)=alog10(xhi(i))
	   endif
 	   if(logy) then
 	     ylo(i)=alog10(ylo(i))
 	     yhi(i)=alog10(yhi(i))
 	   endif
	   if(sqrty) then
 	     ylo(i)=sqrt(ylo(i))
 	     yhi(i)=sqrt(yhi(i))
         endif
	enddo
      call WDIALOG(1,'Enter new option/line (if no changes press Done)',
     &ict)

151	call CKEY(ch,key)
	if(key.lt.-1.or.key.gt.10) goto 151
	if(key.eq.10) goto 9
	if(key.eq.6.or.ikey.eq.-1) then
	   call VHELP(7)		!line types
	   goto 151
	endif
	if(key.eq.9) then
	   nd=1
 	   call DEFOLTi(nd,defolt)
	   call QDIALOG(1,'1.Use mouse ; 2.Numerically ',
     &   defolt,ict,cans)
	   if(cans.eq.' ') cans='1'
	   call GETINPi(cans,nd)
	   if (nd.eq.2) goto 44
	if (mouse_on) then
	   call wdialog(1,'Pick line with mouse (Right button to finish)',
     &   ict)
	   call show_mouse()
82	   call mouse_update(ibutton,ix,iy)
	   if (ibutton.eq.2) then
		  call hide_mouse()
              goto 9
	   endif
	   call pixgra(ix,iy,xm,ym)
	   do i=1,n1
		if (xm.ge.xlo(i).and.xm.le.xhi(i)) then
             if(ylo(i).lt.yhi(i).and.ym.le.yhi(i).and.ym.ge.ylo(i)) then
	         if(ibutton.eq.1) then
			n=i
			call hide_mouse()
			goto 33
		   endif
		 else if(ym.ge.yhi(i).and.ym.le.ylo(i)) then
	         if(ibutton.eq.1) then
			n=i
			call hide_mouse()
			goto 33
		   endif
		 endif
		endif
	   enddo
	   goto 82
	else
	   call wdialog(1,'Sorry no mouse',12)
	endif
44	   call INTCONV(n1,cnum)
	   n=1
 	   call DEFOLTi(n,defolt)
	   call QDIALOG(1,'Line # (1 to '//CHARNB(cnum)//') to be altered
     &(Esc to finish)',defolt,ict,cans)
	   if(cans.eq.'') then		!ESC
		goto 9
	   endif
	   if(cans.eq.' ') cans='1'
	   call GETINPi(cans,n)

33    continue
	call intconv(n,chint)
	call wdialog(1,'Modify line nr '//chint,12)
	if(ikey.eq.8) then
	    isav=ilhtype(n)
	    i=ilhtype(n)
	    i1=72+n-1
	else if(ikey.eq.9) then
	    isav=ilvtype(n)
	    i=ilvtype(n)
	    i1=82+n-1
	else if(ikey.eq.7) then
	    isav=iltype(n)
	    i=iltype(n)
	    i1=61+n-1
	endif
	dim=lth(i1)
	call linwid(lth(i1))
	call lincols(15)
	call BROKEN(isav)
	if(ikey.eq.7) then
	   xbeg=(xlb(n))
	   xend=(xle(n))
 	   ybeg=(ylb(n))
 	   yend=(yle(n))
	   if(logx) then
	     xbeg=alog10(xlb(n))
	     xend=alog10(xle(n))
	   endif
 	   if(logy) then
 	     ybeg=alog10(ylb(n))
 	     yend=alog10(yle(n))
 	   endif
	   if(sqrty) then
 	     ybeg=sqrt(ylb(n))
 	     yend=sqrt(yle(n))
         endif
	else if(ikey.eq.8) then
		xbeg=(xhlb(n))
		xend=(xhle(n))
            ybeg=(yhline(n))
            yend=(yhline(n))
	   if(logx) then
		xbeg=alog10(xhlb(n))
		xend=alog10(xhle(n))
	   endif
	   if(logy) then
             ybeg=alog10(yhline(n))
             yend=alog10(yhline(n))
	   endif
	   if(sqrty) then
 		ybeg=sqrt(yhline(n))
 		yend=sqrt(yhline(n))
         endif
	else if(ikey.eq.9) then
	      xbeg=(xvline(n))
		xend=(xvline(n))
            ybeg=(yvlb(n))
            yend=(yvle(n))
	   if(logx) then
		xbeg=alog10(xvline(n))
		xend=alog10(xvline(n))
	   endif
	   if(logy) then
             ybeg=alog10(yvlb(n))
             yend=alog10(yvle(n))
	   endif
	   if(sqrty) then
 		ybeg=sqrt(yvlb(n))
 		yend=sqrt(yvle(n))
         endif
	endif
	call gramov(xbeg,ybeg)
	call gralin(xend,yend)
	goto 151
	endif

c151	continue
	select case(key)
	    case(1)
	          call WDIALOG(1,'Move line with mouse/arrows:F1 for help'
     &	    ,ict)
		    call WDIALOG(1,'-Hit ENTER/ESC when done',11)
		    delete=.true.
		    if(ikey.eq.8) then
	             val=ybeg
	             eps=0.003*val
	             amin=xbeg
	             amax=xend
			 horiz=.true.
		    else
	             val=xbeg
	             eps=0.003*val
	             amin=ybeg
	             amax=yend
			 horiz=.false.
		    endif
	          call SETLINE(horiz,val,eps,6,2,amin,amax,15,6,70,
     &	    ibk,delete,ik,dim)     !format=F6.2 at present
		    delete=.false.
		    if(ikey.eq.8) then
			  xbeg=amin
			  xend=amax
			  ybeg=val
			  yend=val
			  if(logy) val=10**val
			  if(sqrty) val=val**2
			  if (logx) then
			     amin=10**amin
			     amax=10**amax
			  endif
			  yhline(nhline)=val	!record y value
			  xhlb(nhline)=amin   	!start/end of HLI
			  xhle(nhline)=amax
		    else
			  ybeg=amin
			  yend=amax
			  xbeg=val
			  xend=val
			  if(logx) val=10**val
			  if (logy) then
			     amin=10**amin
			     amax=10**amax
			  endif
			  if (sqrty) then
			     amin=amin**2
			     amax=amax**2
			  endif
			  xvline(nvline)=val
			  yvlb(nvline)=amin
			  yvle(nvline)=amax
		    endif
	    case(2)
	       call lincols(ibk)		!backround
	       if(ikey.eq.7) then		!del free line
		    call BROKEN(iltype(n))
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
		    nline=nline-1
	       else if(ikey.eq.8) then
		    call BROKEN(ilhtype(n))
		    j=0
		    do i=1,nhline
		    if(i.ne.n) then	!skip deleted one
		      j=j+1
		      xhlb(j)=xhlb(i)
		      yhline(j)=yhline(i)
		      xhle(j)=xhle(i)
		      ilhtype(j)=ilhtype(i)
		    endif
		    enddo
		    nhline=nhline-1
	       else if(ikey.eq.9) then
		    call BROKEN(ilvtype(n))
		    j=0
		    do i=1,nvline
		    if(i.ne.n) then	!skip deleted one
		      j=j+1
		      xvline(j)=xvline(i)
		      yvlb(j)=yvlb(i)
		      yvle(j)=yvle(i)
		      ilvtype(j)=ilvtype(i)
		    endif
		    enddo
		    nvline=nvline-1
	       endif
	       call gramov(xbeg,ybeg)
	       call gralin(xend,yend)
		 goto 999
	    case(3)
		 call DEFOLTi(i,defolt)
11	       call QDIALOG(1,'Line type (0-15: F1=help)',defolt,ict,cans)
		 if(help) then
		    call VHELP(7)
		 goto 11
		 else
		    call GETINPi(cans,i)
		 endif
		 if(ikey.eq.8) then
		    ilhtype(n)=i
		 else if(ikey.eq.9) then
		    ilvtype(n)=i
		 else
		    iltype(n)=i
		 endif
	    case(4)
	       call COLBAR(1)
 	       call DEFOLTi(icol(i1),defolt)
	       call QDIALOG(1,
     &	 '   Colour; at present',defolt,ict,cans)
	       call GETINPi(cans,icol(i1))
	    case(5)
	       lt4=lth(i1)
 	       call DEFOLTr(lt4,defolt)
	       call QDIALOG(1,
     &	'Line thickness (real); at present',defolt,ict,cans)
	       call GETINPr(cans,lth(i1))
	end select
	call lincols(ibk)
      call LINWID(lt4)
	call BROKEN(isav)
	call gramov(xbeg,ybeg)
      call gralin(xend,yend)
	call lincols(icol(i1))				!redraw
      call LINWID(lth(i1))
	if(ikey.eq.8) then
		call BROKEN(ilhtype(n))
	else if(ikey.eq.9) then
		call BROKEN(ilvtype(n))
	else if(ikey.eq.7) then
		call BROKEN(iltype(n))
	endif
	call gramov(xbeg,ybeg)
	call gralin(xend,yend)
c====================================================================
c	goto 151	!more changes?
999	continue
	goto 1
9	call broken(0)
	end





































































































































































































































