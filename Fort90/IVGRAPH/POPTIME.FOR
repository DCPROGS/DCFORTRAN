	subroutine POPTIME(ixlo,iylo,strings,n,ic,ibk,icf,secs)
c POPUP box (eg for warnings) that appears for SECS seconds and then
c disappears (based on Ioana's POPKEYS)
c
c	the mode used is 18 (12h)
c	IXLO,IYLO  : coordonates of lower left corner of the window; I gave it like
c	             this because is easier to control mouse;
c	STRINGS(N) : array of strings you want to write in the window
c	N          : array dimension
c	IC         : color of writing
C	ICF        : color frame
c	IBK        : color of background
	character*(*) strings(n)
	integer*1 array1(:)
	allocatable :: array1
c
	ntic=ifix(secs*100.)	!number of 10ms tics for TIMER
c
	iyhi=-1
	call windim(ixlo,iylo,ixhi,iyhi,idimen,strings,n,ixc0,iyc0)
	idx=ixhi-ixlo
	idy=iyhi-iylo
	if(iyhi.gt.479) then
		iylo=479-idy
	endif
	if(iylo.lt.0) then
		iylo=0
	endif
	if(ixhi.gt.639) then
		ixlo=639-idx
	endif
	iyhi=-1
	call windim(ixlo,iylo,ixhi,iyhi,idimen,strings,n,ixc0,iyc0)
	allocate(array1(idimen))
	call RDGWIN1(ixlo,iylo,ixhi,iyhi,array1)   !record orig graphics
	call FRAMEFILL(ixlo,iylo,ixhi,iyhi,icf,ibk)
	iyc=iyc0
	do i=1,n
		call wrstring18(ixc0,iyc,strings(i),ic,ibk)
		iyc=iyc-16
	enddo
c
	call TIMER(n1)
	nend=n1+ntic
	n2=0
	do while(n2.lt.nend)
	   call TIMER(n2)
	enddo
c
	call WRGWIN1(ixlo,iylo,ixhi,iyhi,array1)
	deallocate(array1)
	RETURN
	end
