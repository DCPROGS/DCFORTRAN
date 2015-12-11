	subroutine WDIALOG(text,icol)
c To write scrolling text in a dialogue box opened with OPENDIALOG
c Keeps text already written to row i in DTEXT(i)
c Sets idrow(i)=1 of row i has already been written
c When SCROLDIAL is called it writes TEXT to the first available blank
c row.  If all rows already full it writes to the bottom row, and scrolls
c other rows up
	character*78 dtext(20)
	character text*(*)
	integer idrow(20)
	logical full
	common/dial/dtext,idrow,irhi,iclo,nrow,icb,ixlo,ixhi,iylo,iyhi
c
	n=NBLANK(text)
	do i=1,nrow
	   if(idrow(i).eq.0) then
		full=.false.
		irow=i
		goto 1
	   endif
	enddo
	full=.true.		!no blank lines
1	continue
	if(.not.full) then
	   call set_cursor(iclo,irow+irhi-1)          !args = col,row
	   call SET_STRING(text(1:n),icol)
	   idrow(irow)=1				!this row now used
	   dtext(irow)=text			!keep the text in it
	else
	   do i=1,nrow-1
		dtext(i)=dtext(i+1)		!scroll
	   enddo
	   dtext(nrow)=text			!save new one
	   call FILLWIN(ixlo+1,ixhi-1,iylo+1,iyhi-1,icb)	!clear the box
	   do irow=1,nrow
		n=NBLANK(dtext(irow))
	      call set_cursor(iclo,irow+irhi-1)          !args = col,row
		call SET_STRING(dtext(irow)(1:n),icol)
	   enddo
	endif
	RETURN
	end

