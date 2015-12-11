	subroutine DRAWBOX(nbline,Lbox,text,icol1,icol2)
	integer Lbox(12),Lblast(12)
	character*78 text(12)
	real x(4),y(4)		!for FILL
	logical upper
	common/DBOX/Lblast,nblast,cslast,ihb,csize,ifonb,ifont   !Lahey 5.x version
c 08/11/92 03:01pm Version with colour for Lahey/Hgraph V5
c Differs from old version thus:
c (1) icol1=colour for text; icol2=background (fill) colour
c (2) ifonb=font to use for text in boxes
c	ifont=the current font number (so font can be restored after
c	drawing boxes)
c (3) Deletion of boxes is now done with FILL, so no need to keep
c	textlast,iflast in common, but do need Lblast,nblast,cslast,ihb
c	so that DELBOX knows where the boxes are
c
c To draw boxes like KEYS5, using Hgraph
c (Use DRAWBOX1 to draw a single box)
c Modified 10/16/90 02:04pm to alter LBLAST to 2 (or -2) for boxes on
c upper line, for use by DELBOX1 and DRAWBOX1
c Modified 11/04/89 05:52pm so input value of IFONT is now on 0-11 scale
c
c Nbline= no of boxes per line (eg 4,5,6)
c Lbox(i), (i=1,2,...,12) =  1 if box i to be drawn normally
c				  =  2 if box i to be forced to upper line*
c				  =  0 if box i omitted
c				  = -1,-2 if box i to use italic (/script?/reverse?)
c TEXT(i) = characters for box(i), i=1,2,..,12
c Csize=0.5-18.0=size (2. usually good)
c IFONT= current font so can be reset after boxes drawn
c*Nbox=total no of boxes drawn (if > nbline get 2 lines) (calc internally)
c   If Nbox < Nbline normally get all on bottom row, but boxes 1-nbline
c	put on upper row if Lbox(i) set negative
c
c Make local copy of input variables which are kept until call of DELBOX
c with SAVE instruction (NB compile /nosave) so delete has same values
c as were used for last DRAWBOX call, even if Lbox, Text etc have been
c changed in the calling prog in the meantime.
	do 12 i=1,12
12	Lblast(i)=Lbox(i)
c12	textlast(i)=text(i)
	nblast=nbline
	cslast=csize
c	iflast=ifont
	en=float(nbline)
      iw=ifix(10000./(1.04*en))	!box width
	ig=iw/25		     		!gap between boxes
	if(ig.gt.100) ig=100
	ig2=ig/2
	ih=ifix(7000.*csize)/55		!height of box
	i1=ifix(float(ih)/4.0)		!start point for text
	nbox=0
	imax=0
	do 11 i=1,12
	if(Lbox(i).eq.0) goto 11
	nbox=nbox+1
	imax=i		!=largest box # to be drawn on exit
11	continue
	call SETCSIZE(csize,isize)
	ihb=2*(ih+ig)	!for 2 lines
	call VIEWPORT(0,10000,0,ihb)
c Fill will not work unless graph boundary set:
 	call graphboundary(0,10000,0,7000)	!reset later?
c
c Now the drawing loop
	do 1 i=1,imax
	j=Lbox(i)
	if(j.eq.0) then
		goto 1		!no box
	else if(j.gt.0) then
c		call SETFONT(0,.false.)    	!set simplest font
		call SETFONT1(ifonb)    	!set font for text
	else
c		call SETFONT(0,.true.)    	!set italic (j.lt.0)
		call SETFONT1(ifonb+1)    	!set font for text
	endif
	upper=(imax.gt.nbline.or.iabs(j).eq.2).and.i.le.nbline
c reset lblast for boxes on upper line, for use by DELBOX1
	if(upper) then
	   if(lblast(i).gt.0) then
		lblast(i)=2
	   else
		lblast(i)=-2
	   endif
	endif
	iyoff=0	   		!lower line
	if(upper) iyoff=ih+ig
c
	j1=i
	if(i.gt.nbline) j1=i-nbline
	ixlo=ig2 + (j1-1)*(iw+ig)
	ixhi=ixlo+iw
	iylo=ig2+iyoff
	iyhi=ig2+ih+iyoff
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
c	icol=icol2		!background fill
c	call COLTYP1(icol)
c	call FILL1(x,y,n,icol,0,0,1)
	call FILL1(x,y,n,icol2,0,0,1)
c Write the text
	icol=icol1		!for text
	call COLTYP1(icol)
	call PLOT(ixlo+i1/2,iylo+i1,0)	!move to start point for text
	nl=NBLANK(text(i))
	call WRITESTRING(text(i)(1:nl),0,isize)
c Draw frame in same colour as text
	call IFRAME(ixlo,ixhi,iylo,iyhi)
1	continue
c
	call SETFONT1(ifont)		    	!restore font
c	call COLTYP(0)				!restore normal write
	call COLTYP1(7)				!restore normal write
c	call VIEWPORT(0,10000,0,7000)		!restore viewport
	RETURN
	end

