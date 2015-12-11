C     Last change:  P     2 Apr 97    4:04 pm
	subroutine DCMENU(nbox,nbline,Lbox,text,icol1,icol2)
	integer Lbox(*)
	integer Lbox1(30)
	character*78 text(*)
	real x(4),y(4)		!for FILL
	common/DMENU/ifonb,size,ifont,nboxlast,nblast
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,vymenu
c DCMENU is version of DCBOX that has many args in COMMON
c To DELETE all boxes for which Lbox(i)=1, call
c    with icol1=icol2=screen background col
c  Modif so that icol1=icol2 is signal to delete (set to screen background col)
c  Modif 02/19/93 12:12pm to set linewidth to 20 (line width now shows
c   on screen as well as plotter!)
c  Modif 01/21/93 10:53am so that:
c	  if called with nbox=0 then draws ALL boxes regardless of Lbox.
c	  if nbox=-n then draws box #n only
c	else if icol1=icol2=0 then
c	  if called with nbox=0 then deletes ALL boxes regardless of Lbox.
c	  if nbox=-n then deletes box #n only
c	(To delete/draw more than one box selectively must reset Lbox)
c DCBOX is new version of DRAWBOX that can draw any number of menu boxes on
c any number of lines in graphics mode. This version need no COMMON (but could
c be more convenient to put icol1,icol2,csizb,ifonb,ifont into common (and
c also csize from main prog, so size as well as font can be restored on exit).
c And ixlo,ivxlo etc so viewport and graphboundary can be restored too.
c
c
c Note that nbox & nbline determine the number of lines used, and the exact
c position of box #i.
c
c   nbox=total number of boxes (but box ACTUALLY drawn only if Lb(i)=1)
c   nbline=number of boxes per line (so number of lines = 1 + (nbox-1)/nbline)
c   Lbox(i)=1 if box i to be drawn or deleted
c   Lbox(i)=-1 if box i for italic
c   Lbox(i)=0 if box i NOT to be touched
c   icol1=colour for text; icol2=background (fill) colour
c   ifonb=font to use for text in boxes
c   ifont=the current font number (so font can be restored after
c	drawing boxes)
c
c Next line added to prevent error if try to delete boxes before any drawn
	call papmax(papx,papy)
	if(nbox.le.0.and.(nboxlast.eq.0.or.nblast.eq.0)) then
	   if(nbox.gt.0) then
		nboxlast=nbox
	   else if(nbox.lt.0) then
		nboxlast=iabs(nbox)
	   else
		nboxlast=1
	   endif
	   if(nbline.gt.0) then
		nblast=nbline
	   else
		nblast=1
	   endif
	   RETURN	!nothing to delete (?)
	endif
	call LINWID(0.0)
	do i=1,30		!so values in call not changed
	  Lbox1(i)=Lbox(i)
	enddo
	if(nbox.ge.1) then
	   nboxlast=nbox
	   nblast=nbline
	else		!nbox.le.0
	   if(nbox.eq.0) then
	      do i=1,30
	         Lbox1(i)=1		!so all deleted/drawn
	      enddo
	   else
	      do i=1,30
	         Lbox1(i)=0		!so all unaffected
	      enddo
	      n=iabs(nbox)
	      Lbox1(n)=1	!except box n deleted/drawn
	      if(Lbox(n).eq.-1) Lbox1(n)=-1	!preserve italics
	   endif
	   nbox=nboxlast	!reset nbox AFTER iabs(nbox) used above
	   nbline=nblast
	endif
	nline=1 + (nbox-1)/nbline
	en=float(nbline)
	w=(vxhi-vxlo)/(1.04*en)
	h=(vymenu-vylo)/(1.04*nline)
	gh=0.04*w		     		!horizontal gap between boxes
	gv=0.04*h			!vertical gap between boxes
	gh2=vxlo +gh/2
	gv2=vylo +gv/2
	ti1=h/4.0		!start point for text
	ti2=w/8.0		!start point for text
	imax=0
	do i=1,nbox
	   if(Lbox1(i).ne.0) then
	      imax=i		!=largest box # to be drawn on exit
	   endif
	enddo
	size=1
	call setsize(size)
	hb=nline*(h+gv)	!overall height of boxes for nline lines
c Now the drawing loop
	if(imax.gt.12) imax=12
	do i=1,imax
	 if(Lbox1(i).ne.0) then
	    if(Lbox1(i).gt.0) then
		 call setfnt(ifonb)    	!set font for text
	    else if(Lbox1(i).lt.0) then
		 call setfnt(ifonb+1)    	!set
	    endif
	    line=1 + (i-1)/nbline			!line # for current box=1,2,...
	    yoff=(nline-line)*(h+gv)	!y offset = nline-1,..,2,1,0*(ih+ig)
	    j1=i - nbline*(line-1)		!ie box i is j1'th box on current line
	    xlo1=gh2 + (j1-1)*(w+gh)	!do not overwrite input ixlo
	    xhi1=xlo1+w
	    ylo1=gv2+yoff
	    yhi1=ylo1+h
	    x(1)=xlo1
	    y(1)=ylo1
	    x(2)=xlo1
	    y(2)=yhi1
	    x(3)=xhi1
	    y(3)=yhi1
	    x(4)=xhi1
	    y(4)=ylo1
	    n=4
	    if(icol1.eq.icol2) then		!deleting
	       call lincols(icol1)
	       call linvis(1)
	       call movto2(x(1),y(1))
	       call pofto2(0,0,0,x,y,n)
	    else
c 	       Write the text (unless deleting)
	       call lincols(icol2)
	       call movto2(x(1),y(1))
	       call pofto2(0,0,0,x,y,n)
		 xstr=xlo1+ti2
		 ystr=ylo1+ti1
	       call MOVTO2(xstr,ystr)	!move to start point for text
	       nl=NBLANK1(text(i))
	       call linvis(1)
	       call lincols(icol1)
	       call setsize(size)
	       CALL CHAANG(0)
		 call chajus(-1)
	       call CHASTR(text(i)(1:nl))
	    endif
c         Draw frame in same colour as text
	    call movto2(x(1),y(1))
	    do k=1,n
		 call linto2(x(k),y(k))
	    enddo
	    call linto2(x(1),y(1))
	 endif
	enddo
c
	call lincols(15)
	RETURN
	end

