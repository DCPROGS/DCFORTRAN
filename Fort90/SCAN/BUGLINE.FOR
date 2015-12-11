	subroutine BUGLINE(iybar,nscale,iyoff,icl)
c Version of MOVELINE to identify data values for debugging SCAN -arrows
c move horizontal line with continuous readout of the corresponding value
c of DATA(i). Hit ESC to leave.
c	This moves a horizontal line on the screen without erasing the
c	background
c
c 1st call WRGWIN1 to restore graph background in position where line orig drawn
c Then call RDGWIN1 to record graph background on line to be overwritten
c by following call to HLINE
c
	integer*1 isavlin(320)	!should need 4*80 bytes for 640 pixels
	integer*2 ktype
	character*1	a,getch
	character cnum*11		!for debug
	common/dialval1/ictd,icfd,icbd  !colours for dialog box #1 (text,frame,back)
c
c Open box to write value of data() to
	call OPENBOX(3,7,.true.)
	is=2**nscale
c
1	a=getch(ktype)
c	if(ktype.eq.2) a=LC(a)	!convert to lower case
	m=ichar(a)
	if(ktype.eq.8) then
	   if(m.eq.72) then
		if((iybar+1).lt.480) then
		   iyo=iybar
		   iybar=iybar+1
		   call WRGWIN1(0,iyo,639,iyo,isavlin)
		   call RDGWIN1(0,iybar,639,iybar,isavlin) !store graph to be
		   call hline(1,639,iybar,icl)
		endif
	   else if(m.eq.80) then
		if(iybar.gt.80) then
		   iyo=iybar
		   iybar=iybar-1
		   call WRGWIN1(0,iyo,639,iyo,isavlin)
		   call RDGWIN1(0,iybar,639,iybar,isavlin)
		   call hline(1,639,iybar,icl)
		endif
	   endif
	   iy=(iybar-iyoff)*is
         call INTCONV(iy,cnum)
	   call CLRBOX(3,0)
	   call WRITBOX(3,cnum,1,10)
	else
	   if(m.eq.27.or.m.eq.13) goto 2	!esc/enter
	endif
	goto 1
c  Decode ikey
2	continue
	call ENDBOX(3,8)  !8=background colour for expand/fit
c
	RETURN
	end
