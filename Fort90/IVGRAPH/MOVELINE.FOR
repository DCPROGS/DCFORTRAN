	subroutine MOVELINE(iybar,icl,isavlin,ikey)
c
c	This moves a horizontal line on the screen without erasing the
c	background
c Modified 11/04/93 01:34pm so that subroutine is left when any keypad
c character is hit (44-57 -includes also comma=44), OR <enter>.
c Also returns ikey value (as in CKEY)
c
c This is version of MOVE_LINE (MOVELINE.OLD) that uses RDGWIN1 and WRGWIN1
c to store and restore background as for popup windows (line=window
c that is one pixel high/wide)
c
c 1st call WRGWIN1 to restore graph background in position where line orig drawn
c Then call RDGWIN1 to record graph background on line to be overwritten
c by following call to HLINE
c
	integer*1 isavlin(320)	!should need 4*80 bytes for 640 pixels
	integer*2 ktype
c	character*1	a,getch,LC
	character*1	a,getch

1	a=getch(ktype)
c	if(ktype.eq.2) a=LC(a)	!convert to lower case
	m=ichar(a)
	if (ktype.eq.8) then
	   if (m.eq.72) then
		if((iybar+1).lt.480) then
		   iyo=iybar
		   iybar=iybar+1
		   call WRGWIN1(0,iyo,639,iyo,isavlin)
		   call RDGWIN1(0,iybar,639,iybar,isavlin) !store graph to be
		   call hline(1,639,iybar,icl)
		endif
	   else if (m.eq.80) then
		if(iybar.gt.80) then
		   iyo=iybar
		   iybar=iybar-1
		   call WRGWIN1(0,iyo,639,iyo,isavlin)
		   call RDGWIN1(0,iybar,639,iybar,isavlin)
		   call hline(1,639,iybar,icl)
		endif
	   endif
	else
	   if((m.ge.42.and.m.le.57).or.m.eq.13) goto 2	!keypad or <enter>
	endif
	goto 1
c  Decode ikey
2	continue
	call SETCKEY(ktype,m,ikey)
c
	RETURN
	end
