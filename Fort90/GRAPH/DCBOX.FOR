	subroutine DCBOX(nbox,nbline,Lbox,text,icol1,icol2,
     & csizb,ifonb,csize,ifont)
	integer Lbox(nbox)
	character*78 text(nbox)
	real x(4),y(4)		!for FILL
c DCBOX is new version of DRAWBOX that can draw any number of menu boxes on
c any number of lines in graphics mode. This version need no COMMON (but could
c be more convenient to put icol1,icol2,csizb,ifonb,ifont into common (and
c also csize from main prog, so size as well as font can be restored on exit)
c
c To DELETE all boxes for which Lbox(i)=1, call with icol1=icol2=0
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
c
	nline=1 + (nbox-1)/nbline
	en=float(nbline)
      iw=ifix(10000./(1.04*en))	!box width
	igh=iw/25		     		!horizontal gap between boxes
	igv=igh/2				!vertical gap between boxes
	if(igv.gt.100) igv=100
	igh2=igh/2
	igv2=igv/2
	ih=ifix(7000.*csizb)/55		!height of box
	i1=ifix(float(ih)/4.0)		!start point for text
	imax=0
	do i=1,nbox
	 if(Lbox(i).ne.0) then
	   imax=i		!=largest box # to be drawn on exit
	 endif
	end do
	call SETCSIZE(csizb,isize)
	ihb=nline*(ih+igv)	!overall height of boxes for nline lines
	call VIEWPORT(0,10000,0,ihb)
c Fill will not work unless graph boundary set:
 	call graphboundary(0,10000,0,7000)	!reset later?
c
c Now the drawing loop
	do 1 i=1,imax
	if(Lbox(i).eq.0) then
		goto 1		!skip box
	else if(Lbox(i).gt.0) then
		call SETFONT1(ifonb)    	!set font for text
	else if(Lbox(i).lt.0) then
		call SETFONT1(ifonb+1)    	!set 
	endif
	line=1 + (i-1)/nbline			!line # for current box=1,2,...
	iyoff=(nline-line)*(ih+igv)	!y offset = nline-1,..,2,1,0*(ih+ig)
c
	j1=i - nbline*(line-1)		!ie box i is j1'th box on current line
	ixlo=igh2 + (j1-1)*(iw+igh)
	ixhi=ixlo+iw
	iylo=igv2+iyoff
	iyhi=iylo+ih
c	print 51,it2-it1
c Fill in the background colour
	x(1)=xworld(ixlo)
	y(1)=yworld(iylo)
	x(2)=xworld(ixlo)
	y(2)=yworld(iyhi)
	x(3)=xworld(ixhi)
	y(3)=yworld(iyhi)
	x(4)=xworld(ixhi)
	y(4)=yworld(iylo)
	n=4
	if(icol1.eq.0.and.icol2.eq.0) then	!deleting
	   call FILL1(x,y,n,0,0,0,1)
	   call COLTYP1(0)
	else
c Write the text (unless deleting)
	   call FILL1(x,y,n,icol2,0,0,1)
	   call COLTYP1(icol1)
	   call PLOT(ixlo+i1/2,iylo+i1,0)	!move to start point for text
	   nl=NBLANK(text(i))
	   call WRITESTRING(text(i)(1:nl),0,isize)
	endif
c Draw frame in same colour as text
	call IFRAME(ixlo,ixhi,iylo,iyhi)
1	continue
c
	call SETFONT1(ifont)		    	!restore font
c	call COLTYP(0)				!restore normal write
	call COLTYP1(7)				!restore normal write
	call SETCSIZE(csize,isize)		!restore size
	call VIEWPORT(0,10000,0,7000)		!restore viewport
	RETURN
	end

