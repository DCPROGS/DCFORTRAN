	subroutine CONVQREC(jstrec,jstrec1,lstrec,lstrec1,iunit,iw)
	integer*2 jstrec(200),lstrec(200)
	integer*4 jstrec1(100),lstrec1(100)		!for old queue
c To convert format of record 1 of plot queue to hold 200 plots
c If iw=0 then converts arrays, but leaves queue file unchanged on disc
c If iw=1 then converts arrays, and writes new format to queue file on disc
c
	call BELL(2)
	read(iunit,rec=1) nplot,jstrec1,lstrec1	!read old format
	do 218 i=1,nplot
	jstrec(i)=jstrec1(i)
218	lstrec(i)=lstrec1(i)
c
c	iver=1001		!version number
	iver=1100		!version number
	do 219 i=nplot+1,200
	jstrec(i)=0
219	lstrec(i)=0
	if(iw.eq.1) then
	   write(iunit,rec=1) nplot,jstrec,lstrec,iver
	endif
c
	RETURN
	end

