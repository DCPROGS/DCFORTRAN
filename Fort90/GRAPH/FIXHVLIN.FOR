	subroutine FIXHVLIN(nhline,yhline,xhlb,xhle,ilhtype,ihlinrel,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ibk)
c To adjust horizontal and vertical lines in VPLOT
c new arrays for horizontal lines
	real*4 yhline(10)		!record y value
	real*4 xhlb(10),xhle(10)     !start/end of HLINE
	integer ilhtype(10)	!line type for horizontal lines
	integer*1 ihlinrel(10)	!=1 if line pos rel to line 1
c new arrays for vertical lines
	real*4 xvline(10)		!record x value
	real*4 yvlb(10),yvle(10)     !start/end of VLINE
	integer ilvtype(10)	!line type for vertical lines
	integer*1 ivlinrel(10)	!=1 if line pos rel to line 1
c
	real*4 ydelt(10)
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character ch*1,cnum*11,ans*1
	logical relval
c
	integer Lb(30)		!for DCMENU
	character*78 text(18)
c
	common/DMENU/csizb,ifonb,csize,ifont2,nboxlast,nblast		!for DMENU
c
	icol1=14	!yellow text/border for boxes in DRAWBOX
	icol2=8	!grey background for boxes
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,0)
	Lb(1)=1
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	text(1)='1:MOVE HOR. LINE  '
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
c152	ikey=nkey()
152	call CKEY(ch,ikey)
c	if(ikey.lt.-1.or.ikey.gt.15) goto 151
	SELECT CASE(ikey)
	  CASE(1)
	   call INTCONV(nhline,cnum)
	   n=1
 	   call DEFOLTi(n,defolt)
	   call QDIALOG(1,
     &	'Line # to be moved (1 - '//charnb(cnum)//')',
     &	 defolt,11,cans)
	   call GETINPi(cans,n)
	   call WDIALOG(1,'-Hit ENTER/ESC when done: F1 for help',11)
c     if line to be moved is #1 then check if others are defined rel to #1
c
	   relval=.false.
	   if(n.eq.1) then
		do i=2,nhline
		   if(ihlinrel(i).eq.1) then
			relval=.true.
			ydelt(i)=yhline(i) - yhline(1)	!record diffs
		   endif
		enddo
	   endif
c
	   val=yhline(n)
	   eps=0.003*val
	   amin=xhlb(n)
	   amax=xhle(n)
	   call SETLINE(.true.,val,eps,6,2,amin,amax,12,6,70,
     &	ibk,ik)     !format=F6.2 at present
	   yhline(n)=val		!record y value
	   xhlb(n)=amin        !start/end of HLINE
	   xhle(n)=amax
	   if(relval) then
		ans='Y'
c Move relative lines ALWAYS!
c		call DEFOLTa(ans,defolt)
c		call QDIALOG(1,
c     &	'Move also the lines defined relative to #1',
c     &	 defolt,11,cans)
c		call GETINPa(cans,ans)
		if(ans.eq.'Y') then
		   do i=2,nhline
			yhline(i)=ydelt(i) + yhline(1)
		      xhlb(i)=amin        !make same length as line 1
			xhle(i)=amax
		   enddo
		endif
	   endif

	END SELECT
c
	RETURN
	end

