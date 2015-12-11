	subroutine DRAWBOX1(ibox,text1,icol1,icol2)
c	subroutine DRAWBOX1(ibox,text1,ifonb,ifont,icol1,icol2)
	integer Lblast(12)
c	character*78 textlast(12),text1
	character*78 text1
	real x(4),y(4)		!for FILL
	logical upper
	common/DBOX/Lblast,nblast,cslast,ihb,csize,ifonb,ifont
c	common/DBOX/Lblast,nblast,cslast,ihb
c	common/DBOX/Lblast,textlast,nblast,cslast,iflast,ihb
c 08/11/92 03:01pm Version with colour for Lahey/Hgraph V5
c  Can set text (icol1) and background (icol2) colours diff from original
c	and font (=ifont) can also be different (but size is same)
c
c DRAWBOX1 is version of DRAWBOX to draw a single box; must have same
c size, font etc as last call to DRAWBOX itself
c Modif 11/01/90 10:41am so negative ibox in call gives italic
c
c Modified 11/04/89 05:52pm so input value of IFONT is now on 0-11 scale
c ibox=box # to be drawn
c text1=character*78 array with text
c ltype=type (as specified in Lbox for DRAWBOX) ie
c                  ltype  =  1 if box i to be drawn normally
c				  =  2 if box i to be forced to upper line*
c				  =  0 if box i omitted
c				  = -1,-2 if box i to use italic (/script?/reverse?)
c NB Can safely take Ltype from Lblast because DRAWBOX now modified so that
c Lblast reset to 2 (or -2) for all boxes drawn on upper line, so this prog
c (and DELBOX1) can tell if, say, box1 should be on upper or lower line.
c
c Use font, size etc as defined by last DRAWBOX call
	if(ibox.ge.0) then
	   ltype=Lblast(ibox)
	else if(ibox.lt.0) then
	   ibox=-ibox			!make positive
	   ltype=-iabs(Lblast(ibox))	!make negative
	   Lblast(ibox)=ltype		!keep value in Lblast
	endif
	nbline=nblast
	csize=cslast
c	ifont=iflast
c
c Put this text in common
c	textlast(ibox)=text1
c
	en=float(nblast)
      iw=ifix(10000./(1.04*en))	!box width
	ig=iw/25		     		!gap between boxes
	if(ig.gt.100) ig=100
	ig2=ig/2
	ih=ifix(7000.*csize)/55		!height of box
	i1=ifix(float(ih)/4.0)		!start point for text
	call SETCSIZE(csize,isize)
c	ihb=2*(ih+ig)	!for 2 lines -value taken from COMMON
	call VIEWPORT(0,10000,0,ihb)
c Fill will not work unless graph boundary set:
 	call graphboundary(0,10000,0,7000)	!reset later?
c
c Now draw
	j=ltype
	if(j.eq.0) then
		goto 1		!no box
	else if(j.gt.0) then
c		call SETFONT(0,.false.)    	!set simplest font
		call SETFONT1(ifonb)    	!set font for text
	else
c		call SETFONT(0,.true.)    	!set italic (j.lt.0)
		call SETFONT1(ifonb+1)    	!set italic
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
c	icol=icol2		!background fill
c	call COLTYP1(icol)
c	call FILL1(x,y,n,icol,0,0,1)
	call FILL1(x,y,n,icol2,0,0,1)
c Write the text
	icol=icol1		!for text
	call COLTYP1(icol)
c
	call PLOT(ixlo+i1/2,iylo+i1,0)	!move to start point for text
	nl=NBLANK(text1)
	call WRITESTRING(text1(1:nl),0,isize)
c Draw frame in same colour as text
	call IFRAME(ixlo,ixhi,iylo,iyhi)
1	continue
c
c	call SETFONT(ifont,.false.)    	!restore font
	call SETFONT1(ifont)    	!restore font
c	call COLTYP(0)				!restore normal write
	call COLTYP1(7)				!restore normal write
	call VIEWPORT(0,10000,0,7000)		!restore viewport
	RETURN
	end


