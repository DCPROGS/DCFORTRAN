	subroutine DELBOX(Lbox)
	integer Lbox(12),Lblast(12)
c	character*78 textlast(12)
	real x(4),y(4)		!for FILL
	logical upper
c	common/DBOX/Lblast,textlast,nblast,cslast,iflast,ihb
c	common/DBOX/Lblast,nblast,cslast,ihb
	common/DBOX/Lblast,nblast,cslast,ihb,csize,ifonb,ifont
c Modified 11/04/89 05:59pm so IFONT is on 0-11 scale
c
c To delete boxes drawn by DRAWBOX.
c Deletes box i where Lbox(i) in call is non-zero.
c Values for text,csize,font etc taken from common/DBOX/
c to make sure they are same as those used for last call to DRAWBOX,
c as are the exact values for Lbox (unless they are zero)
c#	do 13 i=1,12
c#13	if(Lbox(i).ne.0) Lbox(i)=Lblast(i)	!restore orig value if to be erased
c Test whether any boxes drawn yet (safer if nblast explicitly initialised
c to zero)
	if(nblast.le.0) RETURN	!nothing to erase
c
	nbline=nblast
	csize=cslast
c	ifont=iflast
c
	en=float(nbline)
      iw=ifix(10000./(1.04*en))	!box width
	ig=iw/25		     		!gap between boxes
	if(ig.gt.100) ig=100
	ig2=ig/2
	ih=ifix(7000.*csize)/55		!height of box
c=	i1=ifix(float(ih)/4.0)		!start point for text
	nbox=0
	imax=0
	do 11 i=1,12
c#	if(Lbox(i).eq.0) goto 11
	if(Lblast(i).eq.0) goto 11
	nbox=nbox+1
	imax=i		!=largest box # to be erased on exit
11	continue
c	icol=-1		!erase
c	call COLTYP(icol)
c
c	call SETCSIZE(csize,isize)
	call VIEWPORT(0,10000,0,ihb)
c Fill will not work unless graph boundary set:
 	call graphboundary(0,10000,0,7000)	!reset later?
c
c Now the drawing loop
	do 1 i=1,imax
c#	j=Lbox(i)
c#	if(j.eq.0) then
	if(Lbox(i).eq.0) then
		goto 1		!do not erase
c=	else if(Lblast(i).gt.0) then
c=		call SETFONT(0,.false.)    	!set simplest font
c=	else if(Lblast(i).lt.0) then
c=		call SETFONT(0,.true.)    	!set italic (j.lt.0)
	endif
	upper=(imax.gt.nbline.or.iabs(Lblast(i)).eq.2).and.i.le.nbline
	iyoff=0	   		!lower line
	if(upper) iyoff=ih+ig
c
	j1=i
	if(i.gt.nbline) j1=i-nbline
	ixlo=ig2 + (j1-1)*(iw+ig)
	ixhi=ixlo+iw
	iylo=ig2+iyoff
	iyhi=ig2+ih+iyoff
c Fill with black to erase box and text
	x(1)=xworld(ixlo)
	y(1)=yworld(iylo)
	x(2)=xworld(ixlo)
	y(2)=yworld(iyhi)
	x(3)=xworld(ixhi)
	y(3)=yworld(iyhi)
	x(4)=xworld(ixhi)
	y(4)=yworld(iylo)
	n=4
c	call COLTYP(-1)	!fill with black to erase box and text
	call COLTYP1(0)	!fill with black to erase box and text
	call IFRAME(ixlo,ixhi,iylo,iyhi)
	ic=0			!icol=0 for black in Lahey (-1 in Hgraph)
	call FILL1(x,y,n,ic,0,0,1)
c=	call PLOT(ixlo+i1/2,iylo+i1,0)	!move to start point for text
c=	nl=NBLANK(textlast(i))
c=	call WRITESTRING(textlast(i)(1:nl),0,isize)
1	continue
c
c	call SETFONT(ifont,.false.)    	!restore font
c	call SETFONT1(ifont)		    	!restore font
c	call COLTYP(0)				!restore normal write
	call COLTYP1(7)				!restore normal write
	call VIEWPORT(0,10000,0,7000)		!restore viewport
	RETURN
	end



