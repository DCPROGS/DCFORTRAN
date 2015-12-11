	subroutine TEXTBOX(x,y,text,angle,size,ijus,
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
	real sxbox(4),sybox(4)
	real rxbox(4),rybox(4)
	common/devnum/idev
c	logical debug,caplock
c
c	debug()=caplock()
c
	if(imode.eq.-1) call lincols(0,idev)
	nl=nblank1(text)
c     CALL FBOX(ix,iy,text(1:nl),iangle,isize,ixjus,iyjus,
	if(nl.eq.1) goto 2
      CALL FBOX1(x,y,text,angle,size,ijus,sxbox,sybox,imode)
	do 1 i=1,4
	call spagra(sxbox(i),sybox(i),rxbox(i),rybox(i))
1	continue


2	RETURN
      END


