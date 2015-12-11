	subroutine OPENDIALOG(irhi,iclo,nrow,ncol,icf,icb)
c To open a window for text dialogue (asking questions etc) in graphics mode
c Box has top left corner at row, column=irhi,iclo (irhi=0,29; iclo=0,79)
c nrow, ncol=number of rows and cols of text to fit in box
c
c Frame colour=icf
c Background colour=icb
c nrow=number of rows of text to be held in box
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c
c	In VGA each text line occupies 16 pixels (letters occupy 11 pixels
c with space of 5 pixels between lines).  The top line (row 0) has
c letters with bases at iy=467 pixels, and the bottom line (row 29) has
c letters with bases at iy=3 pixels
c Thus letters in row #irow have bases at iy=3+(29-irow)*16
c	There are 80 columns of characters (icol=0,..,79) each occupying 8 pixels
c with col zero starting at ix=0.  In general ix for start (left edge) of
c character in column icol is at ix=8*icol (ie last col starts at ix=632)
	character*78 dtext(20)  !up to 20 rows
	integer idrow(20)
	common/dial/dtext,idrow,irh,icl,nro,ib,ixlo,ixhi,iylo,iyhi
c
	irh=irhi	!copy for COMMON
	icl=iclo
	nro=nrow
	ib=icb
c Initialise idrow for use in WDIALOG
	do i=1,20
	   idrow(i)=0
	enddo
c
	margin=5	!5 pixels
	ixlo=iclo*8 - margin
	ixhi=ixlo + ncol*8 + margin
c add another 16 to iyhi because 3+(29-irhi) is posn for BASE of text
c -actually adding 10 better
	iyhi=3 + (29-irhi)*16 +10 + margin
	iylo=iyhi - nrow*16 - margin
	call FILLWIN(ixlo+1,ixhi-1,iylo+1,iyhi-1,icb)
c===window is shorter than frame at right hand end!!
	call HLINE(ixlo,ixhi,iylo,icf)
	call VLINE(ixhi,iylo,iyhi,icf)
	call HLINE(ixhi,ixlo,iyhi,icf)	!hline draws 'backwards' OK
c	call VLINE(ixlo,iyhi,iylo,icf)	!but vline does not
	call VLINE(ixlo,iylo,iyhi,icf)
c
	RETURN
	end

	subroutine ENDDIALOG(ibk)
	character*78 dtext(20)
	integer idrow(20)
	common/dial/dtext,idrow,irhi,iclo,nrow,icb,ixlo,ixhi,iylo,iyhi
c To delete a dialog box bt filling with screen background colour=ibk
	call FILLWIN(ixlo,ixhi,iylo,iyhi,ibk)
	RETURN
	end
