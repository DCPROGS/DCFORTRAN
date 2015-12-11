	   subroutine dragbox(ixlo,iylo,ixhi,iyhi,ixlov,iylov,
     &	ixhiv,iyhiv,imode,ibk,icf)

	character*11 cnumx1,cnumx2,cnumy1,cnumy2
	character*30	text1(2),text2(2)
	logical showbox
	character	ans,getch
	logical mouse_on,mous_set,kbhit
	common/mousval/mouse_on,nbutton

	call drawrec(ixlo,iylo,ixlo+8,iylo+8,icf,icf)
	call drawrec(ixlo,iyhi-8,ixlo+8,iyhi,icf,icf)
	call drawrec(ixhi-8,iyhi-8,ixhi,iyhi,icf,icf)
	call drawrec(ixhi-8,iylo,ixhi,iylo+8,icf,icf)
	if(ixhi.lt.iyhi) then
		a=1.4
		hi=297.
		wi=210.
	else
		a=2.
		hi=210.
		wi=297.
	endif
	xlo=float(ixlo)/a
	ylo=float(iylo)/a
	xhi=float(ixhi)/a
	yhi=float(iyhi)/a
	xhi=wi-xhi
	yhi=hi-yhi
	irhi=1
	iclo=14
	nrow=2
	ncol=22
	irhi2=1
	iclo2=40
	nrow2=2
	ncol2=22
	icbd=0
	m=6
	n=2
	call DCFORMAT(xlo,m,n,cnumx1)
	call DCFORMAT(ylo,m,n,cnumy1)
	call DCFORMAT(xhi,m,n,cnumx2)
	call DCFORMAT(yhi,m,n,cnumy2)
	call DEFBOX(1,irhi,iclo,nrow,ncol,icbd)	!12 cols for cnum*11
	call OPENBOX(1,7,.true.)
	call DEFBOX(2,irhi2,iclo2,nrow2,ncol2,icbd)	!12 cols for cnum*11
	call OPENBOX(2,7,.true.)
	text1(1)='Xleft   = '//cnumx1(1:6)//' [mm] '
	text1(2)='Xright  = '//cnumx2(1:6)//' [mm] '
	text2(1)='Ybottom = '//cnumy1(1:6)//' [mm] '
	text2(2)='Ytop    = '//cnumy2(1:6)//' [mm] '
	call writbox(1,text1,2,11)
	call writbox(2,text2,2,11)
	ix0=ix
	iy0=iy
	call show_mouse()
	ifirst=1
c	ret=1
1	call mouse_update(ibutton,ix,iy)
	if(kbhit()) then
	   ans=getch(ktype)
	   ival=ichar(ans)	!here you may give other options
	   if(ktype.eq.0.and.ival.eq.60) then	!F2=help
	      call vhelp(-2)
		ibutton=0
		call show_mouse()
	      goto 1
	   else if(ival.eq.27.or.ival.eq.13) then		!ESC or ENTER
		goto 99
	   endif
	endif
	if (ibutton.eq.1) then

	  if(ix.lt.ixhi.and.ix.gt.ixhi-8.and.iy.lt.iyhi.and.
     &			iy.gt.iyhi-8) then
		call hide_mouse()
		call drawframe(ixlo,iylo,ixhi,iyhi,icf)
2		call mouse_update(ibutton,ix,iy)
		if (ibutton.eq.1) then
			if(ix.eq.ix0.and.iy.eq.iy0) goto 3
			if(ix.le.ixhiv-16.and.ix.gt.ixlo+16.and.
     &		iy.lt.iyhiv-16.and.iy.gt.iylo+16) then
			call hide_mouse()
			call drawframe(ixlo,iylo,ixhi,iyhi,ibk)
			call drawrec(ixhi-8,iyhi-8,ixhi,iyhi,ibk,ibk)
			call drawrec(ixhi-8,iylo,ixhi,iylo+8,ibk,ibk)
			call drawrec(ixlo,iyhi-8,ixlo+8,iyhi,ibk,ibk)
			ixhi=ix
			iyhi=iy
			call drawframe(ixlo,iylo,ixhi,iyhi,icf)
			call drawrec(ixhi-8,iyhi-8,ixhi,iyhi,icf,icf)
			call drawrec(ixhi-8,iylo,ixhi,iylo+8,icf,icf)
			call drawrec(ixlo,iyhi-8,ixlo+8,iyhi,icf,icf)
			xlo=float(ixlo)/a
			ylo=float(iylo)/a
			xhi=float(ixhi)/a
			yhi=float(iyhi)/a
			xhi=wi-xhi
			yhi=hi-yhi
			call DCFORMAT(xlo,m,n,cnumx1)
			call DCFORMAT(ylo,m,n,cnumy1)
			call DCFORMAT(xhi,m,n,cnumx2)
			call DCFORMAT(yhi,m,n,cnumy2)
			text1(1)='xleft   = '//cnumx1(1:6)//' [mm] '
			text1(2)='xright  = '//cnumx2(1:6)//' [mm] '
			text2(1)='ybottom = '//cnumy1(1:6)//' [mm] '
			text2(2)='ytop    = '//cnumy2(1:6)//' [mm] '
			call writbox(1,text1,2,11)
			call writbox(2,text2,2,11)
			endif
3			continue
			ix0=ix
			iy0=iy
			call show_mouse()
			goto 2
		else if (ibutton.eq.0) then
			goto 1
		else
			goto 99
		endif
	  else if(ix.lt.ixhi.and.ix.gt.ixhi-8.and.iy.gt.iylo.and.
     &			iy.lt.iylo+8) then
		call hide_mouse()
		call drawframe(ixlo,iylo,ixhi,iyhi,icf)
21		call mouse_update(ibutton,ix,iy)
		if (ibutton.eq.1) then
			if(ix.eq.ix0.and.iy.eq.iy0) goto 31
			if(ix.le.ixhiv-16.and.ix.gt.ixlo+16.and.
     &		iy.lt.iyhi-16.and.iy.gt.iylov+16) then
			call hide_mouse()
			call drawframe(ixlo,iylo,ixhi,iyhi,ibk)
			call drawrec(ixhi-8,iylo,ixhi,iylo+8,ibk,ibk)
			call drawrec(ixhi-8,iyhi-8,ixhi,iyhi,ibk,ibk)
			call drawrec(ixlo,iylo,ixlo+8,iylo+8,ibk,ibk)
			ixhi=ix
			iylo=iy
			call drawframe(ixlo,iylo,ixhi,iyhi,icf)
			call drawrec(ixhi-8,iylo,ixhi,iylo+8,icf,icf)
			call drawrec(ixhi-8,iyhi-8,ixhi,iyhi,icf,icf)
			call drawrec(ixlo,iylo,ixlo+8,iylo+8,icf,icf)
			xlo=float(ixlo)/a
			ylo=float(iylo)/a
			xhi=float(ixhi)/a
			yhi=float(iyhi)/a
			xhi=wi-xhi
			yhi=hi-yhi
			call DCFORMAT(xlo,m,n,cnumx1)
			call DCFORMAT(ylo,m,n,cnumy1)
			call DCFORMAT(xhi,m,n,cnumx2)
			call DCFORMAT(yhi,m,n,cnumy2)
			text1(1)='xleft   = '//cnumx1(1:6)//' [mm] '
			text1(2)='xright  = '//cnumx2(1:6)//' [mm] '
			text2(1)='ybottom = '//cnumy1(1:6)//' [mm] '
			text2(2)='ytop    = '//cnumy2(1:6)//' [mm] '
			call writbox(1,text1,2,11)
			call writbox(2,text2,2,11)
			endif
31			continue
			ix0=ix
			iy0=iy
			call show_mouse()
			goto 21
		else if (ibutton.eq.0) then
			goto 1
		else
			goto 99
		endif
	  else if(ix.gt.ixlo.and.ix.lt.ixlo+8.and.iy.gt.iylo.and.
     &			iy.lt.iylo+8) then
		call hide_mouse()
		call drawframe(ixlo,iylo,ixhi,iyhi,icf)
22		call mouse_update(ibutton,ix,iy)
		if (ibutton.eq.1) then
			if(ix.eq.ix0.and.iy.eq.iy0) goto 32
			if(ix.le.ixhi-16.and.ix.gt.ixlov+16.and.
     &		iy.lt.iyhi-16.and.iy.gt.iylov+16) then
			call hide_mouse()
			call drawframe(ixlo,iylo,ixhi,iyhi,ibk)
			call drawrec(ixlo,iylo,ixlo+8,iylo+8,ibk,ibk)
			call drawrec(ixlo,iyhi-8,ixlo+8,iyhi,ibk,ibk)
			call drawrec(ixhi-8,iylo,ixhi,iylo+8,ibk,ibk)
			ixlo=ix
			iylo=iy
			call drawframe(ixlo,iylo,ixhi,iyhi,icf)
			call drawrec(ixlo,iylo,ixlo+8,iylo+8,icf,icf)
			call drawrec(ixlo,iyhi-8,ixlo+8,iyhi,icf,icf)
			call drawrec(ixhi-8,iylo,ixhi,iylo+8,icf,icf)
			xlo=float(ixlo)/a
			ylo=float(iylo)/a
			xhi=float(ixhi)/a
			yhi=float(iyhi)/a
			xhi=wi-xhi
			yhi=hi-yhi
			call DCFORMAT(xlo,m,n,cnumx1)
			call DCFORMAT(ylo,m,n,cnumy1)
			call DCFORMAT(xhi,m,n,cnumx2)
			call DCFORMAT(yhi,m,n,cnumy2)
			text1(1)='xleft   = '//cnumx1(1:6)//' [mm] '
			text1(2)='xright  = '//cnumx2(1:6)//' [mm] '
			text2(1)='ybottom = '//cnumy1(1:6)//' [mm] '
			text2(2)='ytop    = '//cnumy2(1:6)//' [mm] '
			call writbox(1,text1,2,11)
			call writbox(2,text2,2,11)
			endif
32			continue
			ix0=ix
			iy0=iy
			call show_mouse()
			goto 22
		else if (ibutton.eq.0) then
			goto 1
		else
			goto 99
		endif
	  else if(ix.gt.ixlo.and.ix.lt.ixlo+8.and.iy.lt.iyhi.and.
     &			iy.gt.iyhi-8) then
		call hide_mouse()
		call drawframe(ixlo,iylo,ixhi,iyhi,icf)
23		call mouse_update(ibutton,ix,iy)
		if (ibutton.eq.1) then
			if(ix.eq.ix0.or.iy.eq.iy0) goto 33
			if(ix.le.ixhi-16.and.ix.gt.ixlov+16.and.
     &		iy.lt.iyhiv-16.and.iy.gt.iylo+16) then
			call hide_mouse()
			call drawframe(ixlo,iylo,ixhi,iyhi,ibk)
			call drawrec(ixlo,iyhi-8,ixlo+8,iyhi,ibk,ibk)
			call drawrec(ixlo,iylo,ixlo+8,iylo+8,ibk,ibk)
			call drawrec(ixhi-8,iyhi-8,ixhi,iyhi,ibk,ibk)
			ixlo=ix
			iyhi=iy
			call drawframe(ixlo,iylo,ixhi,iyhi,icf)
			call drawrec(ixlo,iyhi-8,ixlo+8,iyhi,icf,icf)
			call drawrec(ixlo,iylo,ixlo+8,iylo+8,icf,icf)
			call drawrec(ixhi-8,iyhi-8,ixhi,iyhi,icf,icf)
			xlo=float(ixlo)/a
			ylo=float(iylo)/a
			xhi=float(ixhi)/a
			yhi=float(iyhi)/a
			xhi=wi-xhi
			yhi=hi-yhi
			call DCFORMAT(xlo,m,n,cnumx1)
			call DCFORMAT(ylo,m,n,cnumy1)
			call DCFORMAT(xhi,m,n,cnumx2)
			call DCFORMAT(yhi,m,n,cnumy2)
			text1(1)='xleft   = '//cnumx1(1:6)//' [mm] '
			text1(2)='xright  = '//cnumx2(1:6)//' [mm] '
			text2(1)='ybottom = '//cnumy1(1:6)//' [mm] '
			text2(2)='ytop    = '//cnumy2(1:6)//' [mm] '
			call writbox(1,text1,2,11)
			call writbox(2,text2,2,11)
			endif
33			continue
			ix0=ix
			iy0=iy
			call show_mouse()
			goto 23
		else if (ibutton.eq.0) then
			goto 1
		else
			goto 99
		endif
	  else
		goto 1
	  endif
	else if(ibutton.eq.2) then
		goto 99
	else
		goto 1
	endif
99	continue
	call hide_mouse()
	end
















































