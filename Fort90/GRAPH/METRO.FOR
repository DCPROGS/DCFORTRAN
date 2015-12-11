	program METRO
c Metronome program
c ESC=end
c ENTER=restart from 1
c +,-= increase/decrease rate
c
	logical kbhit,erase,restart
	character ch*1,getch*1,string*70
	integer*2 ktype
	character cnum*11
c
	print 1
1	format(' Beat rate (per minute) [60] = ')
	rate=60.
	call INPUTr(rate)
	print 10
10	format(' ONE beep every nth tick (0=no beep) [0] = ')
	nbeep1=0
	call INPUTi(nbeep1)
	print 11
11	format('&THREE beeps every mth tick (0=no beep) [0] = ')
	nbeep2=0
	call INPUTi(nbeep2)
	ntic=ifixr(60.*100./rate)	!# of 10ms ticks between beats
c
	idev=0
	call INIPLT(idev,.false.,2.0)
	call VIEWPORT(0,10000,0,7484)
c Help
	call SETFONT1(4)
	csize=1.1
	call SETCSIZE(csize,isize)    !sets isize too
	call COLTYP1(12)
	call LINWID(10)
	string='ESC=end; ENTER=restart; SPACE=pause; +,- = change rate'
	m1=nblank(string)
	call JUSTIFYSTRING(500,200,string(1:m1),0,isize,3,1)
c
	ifont=6
	call SETFONT1(ifont)
	ix=2000
	iy=2000
	call DRATE(rate,oldrate,0)	!draw only
	csize=18.
	call SETFONT1(ifont)
	call SETCSIZE(csize,isize)    !sets isize too
	call COLTYP1(14)
	call LINWID(60)
	erase=.false.	!first time only
	restart=.false.	!first time
c
	call TIMER(nlast)
	n=0
	nlast=0
2	call TIMER(n1)
	n2=n1-nlast
	if(n2.lt.ntic) then
	   goto 2
	else
	   nlast=n1
c	   call LOCATE(3,0)
c	   print*,n2
	   if(erase) then
	      call COLTYP1(0)
	      call JUSTIFYSTRING(ix,iy,cnum(1:m),0,isize,2,2)	!erase previous
c	       call FILLWIN(0,55,639,387,0)
	   endif
	   n=n+1
	   erase=.true.
	   call INTCONV(n,cnum)
	   m=nblank(cnum)
	   call COLTYP1(14)
	   call JUSTIFYSTRING(ix,iy,cnum(1:m),0,isize,2,2)	!2=centered
	   if(nbeep2.gt.0) then
		if(mod(n,nbeep2).eq.0) then
		   call BELL(3)
		   goto 4		!skip single beep
		endif
	   endif
	   if(restart) then
		call ANYKEY
		restart=.false.
	   endif
	   if(nbeep1.gt.0) then
		if(mod(n,nbeep1).eq.0) then
		   call BELL(1)
		endif
	   endif
4	   if(kbhit()) then
		ch=GETCH(ktype)	!get waiting character and carry on
		ival=ichar(ch)
		if(ktype.eq.16) then
		   goto 3			!ESC to end
		else if(ival.eq.13) then		!enter to restart from 1
		   n=0
		   nlast=0
		   restart=.true.	!so pauses after 1 displayed
		else if(ktype.eq.2.and.ival.eq.32) then	!'space' to pause
		   call ANYKEY
		else if(ktype.eq.3.and.(ival.eq.43.or.ival.eq.45)) then
		   oldrate=rate
		   if(ival.eq.43) then		!+ key
			rate=rate+5.
		   else if(ival.eq.45) then		!- key
			if(rate.ge.10.) rate=rate-5.
		   endif
		   call DRATE(rate,oldrate,1)	!erase and redraw only
		   ntic=ifixr(60.*100./rate)	!# of 10ms ticks between beats
		   call TIMER(nlast)
		   csize=18.
		   call SETFONT1(ifont)
		   call SETCSIZE(csize,isize)    !sets isize too
		   call COLTYP1(14)
		   call LINWID(60)
		endif
	   endif
	   goto 2
	endif
c
3	call VIDEOMOD(3)	 !utility lib
	end

	subroutine DRATE(rate,oldrate,imode)
	character string*60,cnum*11
c
	ix1=1000
	iy1=3500
	csize=1.5
	call SETCSIZE(csize,isize)    !sets isize too
	call LINWID(10)
	if(imode.eq.1) then	!erase and redraw
	   call DCFORMAT(oldrate,8,1,cnum)
	   call COLTYP1(0)
c	   m=nblank(cnum)
	   call GBLANK(cnum,11,m1,m2)
	   string='Beat rate '//cnum(m1:m2)//' per minute'
	   m1=nblank(string)
	   call JUSTIFYSTRING(ix1,iy1,string(1:m1),0,isize,3,1)
	endif
	call COLTYP1(10)
	call DCFORMAT(rate,8,1,cnum)
c	m=nblank(cnum)
	call GBLANK(cnum,11,m1,m2)
	string='Beat rate '//cnum(m1:m2)//' per minute'
	m1=nblank(string)
	call JUSTIFYSTRING(ix1,iy1,string(1:m1),0,isize,3,1)
c
	RETURN
	end
