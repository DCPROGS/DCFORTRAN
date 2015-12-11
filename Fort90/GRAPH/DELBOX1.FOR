	subroutine DELBOX1(ibox)
	integer Lblast(12)
c	character*78 textlast(12),text1
	real x(4),y(4)		!for FILL
	logical upper
	common/DBOX/Lblast,nblast,cslast,ihb,csize,ifonb,ifont
c	common/DBOX/Lblast,nblast,cslast,ihb
c	common/DBOX/Lblast,textlast,nblast,cslast,iflast,ihb
c DELBOX1 is version of DELBOX to erase a single box (based on DRAWBOX1);
c Size, font etc as last call to DRAWBOX itself.
c Modified 11/04/89 05:52pm so input value of IFONT is now on 0-11 scale
c ibox=box # to be drawn
c ltype=type (as specified in Lbox for DRAWBOX) ie
c                  ltype  =  1 if box i to be drawn normally
c				  =  2 if box i to be forced to upper line*
c				  =  0 if box i omitted
c				  = -1,-2 if box i to use italic (/script?/reverse?)
c NB Can safely take Ltype from Lblast because DRAWBOX now modified so that
c Lblast reset to 2 (or -2) for all boxes drawn on upper line, so this prog
c (and DRAWBOX1) can tell if, say, box1 should be on upper or lower line.
c Use font, size etc as defined by last DRAWBOX call
c	text1=textlast(ibox)
	ltype=lblast(ibox)
	if(ltype.eq.0) RETURN	!last LB() was 0 so nothing to erase
	nbline=nblast
	csize=cslast
c	ifont=iflast
c
	en=float(nblast)
      iw=ifix(10000./(1.04*en))	!box width
	ig=iw/25		     		!gap between boxes
	if(ig.gt.100) ig=100
	ig2=ig/2
	ih=ifix(7000.*csize)/55		!height of box
c	i1=ifix(float(ih)/4.0)		!start point for text
c	call SETCSIZE(csize,isize)
c	icol=-1	!erase
c	call COLTYP1(icol)
	call VIEWPORT(0,10000,0,ihb)
c Fill will not work unless graph boundary set:
 	call graphboundary(0,10000,0,7000)	!reset later?
c
c Now draw
	j=ltype
	if(j.eq.0) then
		goto 1		!no box
c=	else if(j.gt.0) then
c=		call SETFONT(0,.false.)    	!set simplest font
c=	else
c=		call SETFONT(0,.true.)    	!set italic (j.lt.0)
	endif
	upper=iabs(j).eq.2
	iyoff=0	   		!lower line
	if(upper) iyoff=ih+ig
c
	j1=ibox
	if(ibox.gt.nbline) j1=ibox-nbline
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
c
c=	call PLOT(ixlo+i1/2,iylo+i1,0)	!move to start point for text
c=	nl=NBLANK(text1)
c=	call WRITESTRING(text1(1:nl),0,isize)
1	continue
c
c	call SETFONT(ifont,.false.)    	!restore font
c	call SETFONT1(ifont)    	!restore font
c	call COLTYP(0)				!restore normal write
	call COLTYP1(7)				!restore normal write
	call VIEWPORT(0,10000,0,7000)		!restore viewport
	RETURN
	end


