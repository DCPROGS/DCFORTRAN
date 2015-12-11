	subroutine TEXTBOX(ix,iy,text,iangle,isize,ixjus,iyjus,
     & rxbox,rybox,imode)
c This routine will draw or erase a box round the string TEXT, and also
c return the values ixlo,...,iyhi, that are used, in LOCTEXT, to find whether
c a specified point lies within this text box.
c This version draws only the BOX, not the text (see### below).
c  IMODE=1	draws box
c  IMODE=-1	erases box
c  IMODE=0	does neither (just returns ixlo,...,iyi)
c
c FBOX is subroutine based on Hgraph example FIG72.FOR that returns in
c IXBOX,IYBOX the coordinates of the corners of the box
	character*(*) text
	integer ixbox(4),iybox(4)
	real rxbox(4),rybox(4)
c	logical debug,caplock
c
c	debug()=caplock()
c
	if(imode.eq.-1) call COLTYP(-1)
c	nl=nblank1(text)
c      CALL FBOX(ix,iy,text(1:nl),iangle,isize,ixjus,iyjus,
c     & ixbox,iybox,imode)

c================================================
      CALL FBOX1(ix,iy,text,iangle,isize,ixjus,iyjus,
     & ixbox,iybox,imode)
c
c	print 20,ixbox,iybox
c20	format(4i8,/,4i8)
	do 1 i=1,4
	rxbox(i)=XWORLD(ixbox(i))
	rybox(i)=YWORLD(iybox(i))
1	continue
c	if(debug()) print 2,(rxbox(i),i=1,4),(ixbox(i),i=1,4)
c2	format(' TEXTBOX: rxbox,ixbox=',4g10.3,4i6)
c	if(debug()) print 2,xworld(ix),yworld(iy),rxbox(1),rybox(1)
c2	format(' TEXTBOX: rx,ry,rxbox(1),rybox(1)=',4g10.3)
c	if(debug()) print*,text
	call COLTYP(0)		!restore normal write
	RETURN
      END


