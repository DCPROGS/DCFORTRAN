	subroutine SETLEV(iampl,iprops,ntmax,kt,t0,ntm,
     & y0,dt,ntrans,filt,nfilt,stepamp,dtf,trise,x0,dxf,
     & nscale,iyoff,cfac,ibk,ibk2,tshift,ylevel,delt,
     & opening,opendown,mono,base,basevga,shut,if1,if2,inext,
     & ilast,ilast1,sfac3,avamp,t0cor,tf50,icbl,icw,icw1,
     & ioff1,scrit,ntrig,shutprev,timamax,finter,npfilt,
     & disptran,data,ix,dx,prevlevel,ndfit,reject,
     & tstring,microsec,bdisp,itriglev,itrigvga,idata1,
     & yd1,yd2,iyd1,iyd2,setshut,accept,redraw,imode)
c
c For MANUAL ONE-AMPLITUDE  option: to set initial guesses for
c transition times (as in FIXGUESS) and to set the (fixed) amplitude
c following each of the transitions (initial guesses are based on
c autosearch in GETTRANS, but with a preset number of transitions,
c so some could be in totally wrong place).
c  imode=0 as before
c  imode=1 for manual display and adjustment of 'cursor fit' in
c   fitsub6, so uses input ylevel(1), ylevel(2) and does not search
c   for transitions
c
c NB ylevel(nt)=current level (intermed units) BEFORE transition #nt
c    ylevel(nt+1)=current level (intermed units) AFTER transition #nt
c
c Problem: in this subroutine (though not in FIXGUESS) args of GET_MOUSE
c have to be integer*2 to work correctly!
c
	integer*1 isavbase(320),isavopen(320),isavy1(320),isavy2(320)
	integer*1 isavy3(320)
	integer*1 iprops(ntmax)	!properties of ith interval (see SCAN)
	integer*2 iampl(0:ntmax)
	integer*2 idata1(2048)	!for data to be fitted (to redraw after baseline moved)
	integer*2 ix(2048)
	integer*2 ixcalc(5120),iycalc(5120)	!up to 5120 points for convol trace
	integer*2 iyvga,iyv,iyv1
	integer*2    ioff2
	integer   iact(0:15),ixlast(50)
	real*4    data(2048)
	real*4    dt(49),stepamp(50)
	real*4    ylevel(50)		!temp storage for transition search results
	real*4    delt(0:50)
	real*4    ycalc(5120)
	real*4    filt(640)
	real*4    timamax(49)
	character*55 helpst(15)		!help strings
	character*1  ch,getch
	character*11 cnum
	character*12 tstring
	character*3  microsec
	logical bigstep,usekey,mouse_on,mous_set,kbhit,first,mono,reject
	logical opening(50),opendown,shut,shutprev,disptran,noredo
c
	logical setshut(0:50),accept,redraw
C 	For DCMENU
	integer 	Lb(30)
	character*78 text(30)
c
	logical cjumpfst
	common/jump/cjumpfst
	common/mousval/mouse_on,nbutton
	common/popvalx/mxlo,mylo,myhi,ictx,ibkx,icfx	!values for poptext calls
	common/DMENU/ifonb,csizem,ifont2,nboxlast,nblast		!for DCMENU
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi
c
	accept=.false.
	redraw=.false.
c Set colours and positions for transition markers
	ic1=15			!colour for inactive transition marker
	ic2=12			!colour for active transition marker
	if(mono) then
	   ic1=12
	   ic2=15
	endif
	if(opendown) then
	   iyt1=iyd2-26	!position for lines marking transitions
	   iyt2=iyt1+25
	else
	   iyt1=iyd1+1	!position for lines marking transitions
	   iyt2=iyt1+25
	endif
c
102	continue			!return here to redo amplitude
c Initialise
	xshift=tshift*cfac	!tshift in pixel units
	bigstep=.false.
	usekey=.false.
	reject=.false.
c
c If initially open and NOT sublevel mode, then open level=prevlevel
c and next level must be shut, so no need to set any levels here
c	if(.not.sublev.and.(.not.SHUT)) then
	noredo=.false.		!so can't redo amplitude if initially open
	if(.not.SHUT) then
	   iampl(kt+1)=0		!set provisional amplitude
	   iprops(kt+1)=0
	   ylevel(1)=prevlevel	!corresponding iampl(kt) already set
	   ylevel(2)=base       !level AFTER 1st transition
	   openlev=ylevel(1)	!needed for GETONE
	   opening(1)=.false.
	   if(ntrans.gt.1) opening(2)=.true.
	   noredo=.true.		!so can't redo amplitude if initially open
	   goto 260
	endif
c
c Next bit, down to 260, done only when initially shut, so
	if(imode.eq.0) then
	   ylevel(1)=base
	   ylevel(2)=avamp+base	!give init value for up/down arrow
	endif
	opening(1)=.true.
	if(ntrans.gt.1) opening(2)=.false.
c
	tstep=10.		!10 microsec steps
	astep=10.         !intermed units
	mous_set=.false.	!initialise
	iyvga=-100		!until first yellow line drawn
c
c Define help strings for amplitude setting:
	helpst(1)='First set the (fixed) amplitude (one'
	helpst(2)='   value for all openings in this fit).'
	helpst(3)='  Set amplitude manually (with up/down'
	helpst(4)='     arrow keys or keys 2,8),'
	helpst(5)='   OR keys 1,3,5 = preset open levels;'
	helpst(6)='  INS/key 0 toggles key sensitivity.'
	helpst(7)='If necessary, reset baseline or   '
	helpst(8)='  label the amplitude dubious, then '
	helpst(9)='  hit <enter> when done.            '
	nhelp=9
	if(mouse_on) then
	   helpst(10)='OR, with mouse,'
	   helpst(11)=' centre button sets approx amplitude; '
	   helpst(12)='Hit both buttons or <enter> when done'
	   nhelp=12
	endif
c
c Draw up amplitudes
	if(imode.eq.0) then
c  Last amp
	   if(ilast.ne.0) then
		iyl=ifixr(base + float(ilast))		!intermed units
		iyv=int2(iyl)		!integer*2 for scalint
		call SCALINT(iyv,1,nscale,iyoff)	!pixel units
		iyl=iyv
		call RDGWIN1(0,iyl,639,iyl,isavy1)  !keep data overwritten
		call DHLINE(0,639,iyl,4,22)		!show last amp (short dash)
	   endif
c  Last but one amp
	   if(ilast1.ne.0) then
		iyl1=ifixr(base + float(ilast1))
		iyv1=int2(iyl1)
		call SCALINT(iyv1,1,nscale,iyoff)
		iyl1=iyv1
		call RDGWIN1(0,iyl1,639,iyl1,isavy2)  !keep data overwritten
		call DHLINE(0,639,iyl1,9,88)	!show last but one amp (longer dash)
	   endif
c Mean full amp
	   oplev=avamp+base
	   openvga=oplev*sfac3 + float(iyoff)	!as in SCALINT (sfac3=1/2**nscale)
	   iopenvga=ifixr(openvga)
	   call RDGWIN1(0,iopenvga,639,iopenvga,isavopen)  !keep data overwritten
	   call HLINE(0,639,iopenvga,13)	   			!show mean full amp level
	else if(imode.eq.1) then
	   iyl=ifixr(base + float(iampl(kt+1)))		!intermed units
	   iyv=int2(iyl)		!integer*2 for scalint
	   call SCALINT(iyv,1,nscale,iyoff)	!pixel units
	   iyl=iyv
	   call RDGWIN1(0,iyl,639,iyl,isavy1)  !keep data overwritten
	   call DHLINE(0,639,iyl,14,22)		!show last amp (short dash)
	endif
c
c First check for, and remove, any very short guesses (if ntrans>1)
	tmin1=0.5*trise		!OK?
	if(ntrans.gt.1) then
	   if(dt(1).lt.tmin1) dt(1)=tmin1
	   if(ntrans.gt.2) then
	      do i=2,ntrans-1
	        if(dt(i)-dt(i-1).lt.tmin1) dt(i)=dt(i-1)+tmin1
		enddo
	   endif
	endif

c Put up new menu
	icol1=14	!yellow text/border for boxes in DCMENU
	icol2=8	!grey background for boxes
	nbox=15
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	Lb(1)=0		!drawn below
	Lb(3)=0		!drawn below
	Lb(5)=0		!drawn below
	Lb(4)=0
	Lb(6)=0
	Lb(7)=0
	Lb(12)=0
	call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
	text(1)='1: LAST BUT ONE'	!dark blue   9
	text(2)='2: REDUCE AMP'		!2=down arrow on keypad
	text(3)='3: USE LAST AMP'	!dark red    4
	text(5)='5: USE MEAN AMP'	!purple     13
	text(8)='8. INCREASE AMP'     !8=up arrow on keypad
	text(9)='9: SET DUBIOUS'
	if(bigstep) then
	   text(10)='10:MOVE SLOWER'
	else
	   text(10)='10:MOVE FASTER'
	endif
	text(11)='+:ADJUST BASELINE'	!can do here, in initial amp-setting bit
	text(13)='x: CANCEL FIT'
	text(14)='\:SCROLL DIALOG'
c	text(15)='.:  HELP (F1)'
	text(15)='.:ACCEPT AS SHOWN'
	nbox=15
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
c	draw special colour boxes individually
	if(ilast1.ne.0) call DCMENU(-1,5,Lb,text,9,15)	!draw box 1 only
	if(ilast.ne.0) call DCMENU(-3,5,Lb,text,4,15)	!draw box 3 only
	call DCMENU(-5,5,Lb,text,13,15)	!draw box 5 only
c
	call WDIALOG(1,
     &  'Set the amplitude now (F1=help)',-icw)
c
c	call NUMSET			!set num lock on ready for response
c152	call CKEY(ch,ikey)
	GOTO 14		!return to label 3 later to act on options below
c
13	continue		!return here with ikey from GETCH below
c
	SELECT CASE(ikey)

	  case(9)         !set open amplitude as dubious
c NB -need to set ALL open amps in fitting as dubious!
	      if(SHUT) then	!starts in shut state
		   do k=kt+1,kt+ntrans,2
		     iprops(k)=IBSET(iprops(k),0) !set bit 0='1' for each opening
		   enddo
	      else		!starts in open state
		   do k=kt,kt+ntrans,2
		     iprops(k)=IBSET(iprops(k),0) !set bit 0='1' for each opening
		   enddo
		endif
		call BELL(1)
	      call WDIALOG(1,
     & 	'Amplitude(s) marked as dubious',icw)

	  case(10)
	   if(bigstep) then
		astep=10.   !intermed units
	   else
		astep=100.  !intermed units
	   endif
	   bigstep=.not.bigstep
	   if(bigstep) then
		text(10)='10:MOVE SLOWER'
	   else
		text(10)='10:MOVE FASTER'
	   endif
	   call DCMENU(-10,5,Lb,text,icol1,icol2)	!redraw box 10
c
	case(11)		!adjust baseline
c		NB basevga=standard screen position of baseline, which never changes
c            and so screen position of trigger line, itrigvga, should not
c		 change either.  But if baseline drifts and is reset here, then
c		 base=data value (intermed units) changes, and iyoff=offset
c		 needed to keep display at standard position also changes. And
c		 itriglev=data value for triggering should change by same amount
c		 as base (so difference between them, and hence tstring, same)
	      call WDIALOG(1,
     & 'Adjust baseline level with arrow keys; then <enter>',-icw)
	      ibasevga=ifixr(basevga)
	      call NUMSET
	      call MOVELINE(ibasevga,icbl,isavbase,i)
	      ybasevga=float(ibasevga)	!new ibasevga
	      basevga=yd1 + bdisp*(yd2-yd1)	!BASELINE AFTER OFFSET -same as before
	      ioff1=ifixr(basevga-ybasevga)	!offset to adjust baseline to bdisp
	      iyoff=iyoff+ioff1		!new offset for next display
c          previously ended here -but now redraw with baseline shifted
	   	if(itrigvga.eq.-1) then
		   if(opendown) then
	         	itrigvga=ifix(basevga-0.12*(yd2-yd1))		!init value for trigger level
		   else
	   	   	itrigvga=ifix(basevga+0.12*(yd2-yd1))		!init value for trigger level
		   endif
	      endif
c          Recalculate changed base and itriglev here (only)
		itriglev=(itrigvga-iyoff)/sfac3	!in intermed units (as idata)
	      base=(basevga-float(iyoff))/sfac3	!convert to intermed units (like curlev etc)
c  Recalculate duration of opening corresponding to trigger level
	      if(kt.gt.0) then
      	 aam=abs((float(itriglev)-base)/avamp)
		 call AMAX(aam,trise,ttrig,nerr)
		 if(nerr.eq.0) then
         	   call INTCONV(ifixr(ttrig),cnum)
		   n=NBLANK(cnum)
		   tstring='trig='//cnum(1:n)//microsec
		 else
		   tstring='trig= ?     '
		 endif
	      endif
	      call WRITBOX(2,tstring,1,10)	!display trigger level
c         Redraw data and baseline, but must recalc idata1 with new offset!
c	      do i=1,ndfit
c		   idata1(i)=ifixr(data1(i))		!in intermed units (in COMMON)
c	      enddo
cc and equivalent integer data in pixel units for display
c		call SCALINT(idata1,ndfit,nscale,iyoff)
c or, faster, keep unscaled version of data in idata0(), and rescale
c with new offset using SCALINT1.
c       Still faster:   prob can apply ioff1 directly to current idata1()?
		ioff2=int2(ioff1)
	      do i=1,ndfit
		   idata1(i)=idata1(i)+ioff2		!in intermed units (in COMMON)
	      enddo
		call FILLWIN(0,iyd1,639,iyd2,ibk2)	!clear data area
	      call SETACT(iact,15)		!draw white whatever
c	      call SETACT(iact,-1)		!no draw
c	      iact(ibk2)=15			!unless background col, when draw white
		call IDCPIX(ix,idata1,ndfit,iact)
		ibasevga=ifixr(basevga)
		call RDGWIN1(0,ibasevga,639,ibasevga,isavbase)  !keep data overwritten
		call HLINE(0,639,ibasevga,icbl)	   !show baseline
		call BELL(1)
	      call WDIALOG(1,
     &       'Now set the open level',-icw)

	  case(13)		!cancel fit
		reject=.true.
		RETURN

	  case(14)			!scroll dialog box
		call SCROLDIAL(1)

	  case(15)
c	   call POPTEXT(mxlo,mylo,myhi,helpst,nhelp,ictx,ibkx,icfx)
		accept=.true.
		redraw=.true.
		do i=1,ntrans
		   if(setshut(i)) then
			iampl(kt+i)=0
		   else
	      	iampl(kt+i)=int2(ifixr(ylevel(i+1)-base))
		   endif
		enddo
		goto 99

	END SELECT
c Jump to here (label 14) after 1st displaying menu
c
14	continue
	call HIDE_MOUSE()
12	continue
c	call NUMCLR		!so either set of arrow keys can be used
c Use NUMSET:see above (modif for Tosh) NB still get ktype=8 if sep arrows used
	call NUMSET
	if(kbhit()) then
	   ch=GETCH(ktype)
	   ival=ichar(ch)	!here you may give other options
c with NUMLOCK off, get ktype=3 for +,- whether on keypad or
c not, but 0-9 (and ./del) give ktype=8 with numlock off)
c Left or right arrow will give ikey=99 (not valid CKEY outputs); otherwise
c return to options following ckey above, with usekey=true, then return to 2
c for another character
	   if(ival.eq.13.or.ival.eq.27) then
		goto 100			!amplitude defined
	   endif
c
	   call SETCKEY(ktype,ival,ikey)		!in DCLIB now
c NB returns ikey=99 if ktype=8 (except for DEL='.' which returns ikey=15)
c eg '+' key has ktype=3, ival=43 whether numlock is on or off and so
c returns ikey=11
	   if((ktype.eq.8.and.ival.eq.82).or.ikey.eq.0) ikey=10     !zero key with numlock off
c	   if(ikey.eq.-1) ikey=15		!F1=help
	   if(ikey.eq.-1) then
	      call POPTEXT(mxlo,mylo,myhi,helpst,nhelp,ictx,ibkx,icfx)
	   endif
c
	   if(ikey.eq.2) then
		ival=80		!simulate down arrow
		ktype=8
		ikey=99
	   else if(ikey.eq.8) then
		ival=72		!simulate up arrow
		ktype=8
		ikey=99
	   endif
c
c Set predetermined amplitudes
	   if(ikey.eq.1.or.ikey.eq.3.or.ikey.eq.5) then
		if(ikey.eq.1) then		!last but one amp
		   ylevel(2)=float(iyl1)
		   iampl(kt+1)=ifixr(ylevel(2)-base)  	!set provisional amplitude
		else if(ikey.eq.3) then		!last amp
		   ylevel(2)=float(iyl)           !last amp
		   iampl(kt+1)=ifixr(ylevel(2)-base)  	!set provisional amplitude
		else if(ikey.eq.5) then		!mean amp
		   ylevel(2)=oplev
		   iampl(kt+1)=ifixr(ylevel(2)-base)  	!set provisional amplitude
		endif
c      Draw line for chosen amplitude in yellow
		call HIDE_MOUSE()
		i4vga=iyvga
	      if(i4vga.gt.0) call WRGWIN1(0,i4vga,639,i4vga,isavy3)	!delete prev yellow line
		iy=ifixr(ylevel(2))		!intermed units
		iyvga=int2(iy)		!integer*2 for scalint
		call SCALINT(iyvga,1,nscale,iyoff)	!pixel units
		i4vga=iyvga
		call RDGWIN1(0,i4vga,639,i4vga,isavy3)  !keep data overwritten
		call HLINE(0,639,i4vga,14)
		call SHOW_MOUSE()
c      and redefine stepamp(), from new ylevel()
		do i=1,ntrans
		   stepamp(i)=ylevel(i+1)-ylevel(i)		!in intermed units
		enddo
	   else if(ikey.ge.9.and.ikey.le.14) then	!eg ikey =9-15
	      usekey=.true.
		goto 13
	   endif
c next bit done if ikey=99, or ikey=2,8 which simulate arrow keys
c
	   i1=itrans-1
	   if(ktype.eq.8) then	!move with ARROW keys
		sign=0.
c Now up/down arrow to alter amplitude (if 0 initially may need to unfix it!)
		if(ival.eq.72.or.ival.eq.80) then 		!up=72/down=80
		   sign=1.			  		!shift up
		   if(ival.eq.80) sign=-1.		!shift down
		   ylevel(2)=ylevel(2) + sign*astep
		   iampl(kt+1)=ifixr(ylevel(2)-base)  	!set provisional amplitude
c		Delete previous yellow line
		   call HIDE_MOUSE()
	         if(i4vga.gt.0) call WRGWIN1(0,i4vga,639,i4vga,isavy3)	!delete
c           Draw yellow line at new level
		   iy=ifixr(ylevel(2))		!intermed units
		   iyvga=int2(iy)		!integer*2 for scalint
		   call SCALINT(iyvga,1,nscale,iyoff)	!pixel units
		   i4vga=iyvga
		   call RDGWIN1(0,i4vga,639,i4vga,isavy3)  !keep data overwritten
		   call HLINE(0,639,i4vga,14)
		   call SHOW_MOUSE()
c      and redefine stepamp(), from new ylevel()
		   do i=1,ntrans
			stepamp(i)=ylevel(i+1)-ylevel(i)		!in intermed units
		   enddo
		endif
	   endif		!end of ktype=8
	endif			!end of kbhit
c
	if(mouse_on) then
	   call get_mouse(ibutton,ix2,iy2)
	   if(ibutton.eq.0) mous_set=.true.
         call show_mouse()
	endif
	if(mous_set) then
	   if(ibutton.ne.0) mous_set=.false.	!until reset above
	   if(ibutton.eq.3) then       !L AND R buttons to finish
		goto 99
	   else if(nbutton.eq.3.and.ibutton.eq.4) then	!centre button=ampl
		ylevel(2)=float(iy2-iyoff)/sfac3
		iampl(kt+1)=ifixr(ylevel(2)-base)  	!set provisional amplitude
c		Delete previous yellow line
		   call HIDE_MOUSE()
	         if(i4vga.gt.0) call WRGWIN1(0,i4vga,639,i4vga,isavy3)	!delete prev yellow line
c           Draw yellow line at new level
		   iy=ifixr(ylevel(2))		!intermed units
		   iyvga=int2(iy)		!integer*2 for scalint
		   call SCALINT(iyvga,1,nscale,iyoff)	!pixel units
		   i4vga=iyvga
		   call RDGWIN1(0,i4vga,639,i4vga,isavy3)  !keep data overwritten
		   call HLINE(0,639,i4vga,14)
c      and redefine stepamp(), from new ylevel()
		   call SHOW_MOUSE()
		do i=1,ntrans
		   stepamp(i)=ylevel(i+1)-ylevel(i)		!in intermed units
		enddo
	   endif
	endif		!end of mouse options
c
	goto 12	!get another character
c==	goto 152		!get another key, until enter is hit
c
c Jump to here once amplitude defined; this is for case when initially shut
c so iampl(kt)=0 already defined. Define ylevel(2), and corresponding
c iampl(kt+1), for amplitude following the first transitions here.
100	continue
	openlev=ylevel(2)		!needed for GETONE
	iampl(kt+1)=int2(ifixr(ylevel(2)-base))
c
c Delete amplitude lines
	call WRGWIN1(0,iopenvga,639,iopenvga,isavopen)
	call WRGWIN1(0,iyl1,639,iyl1,isavy2)
	call WRGWIN1(0,iyl,639,iyl,isavy1)
	call WRGWIN1(0,i4vga,639,i4vga,isavy3)	!delete line if OK
c
260	continue		!jump here if initially open, so no amps to define
c ylevel(1)=level before first transition, and
c ylevel(2)=level after 1st trans were set above; set rest of amplitudes
c so they alternate now
c Also set iampl() now; iampl(kt), and iampl(kt+1) already set.
c
	do i=3,ntrans+1
	   ylevel(i)=ylevel(i-2) 		!set alternating levels
	   iampl(kt+i-1)=iampl(kt+i-3)
	   if(i.le.ntrans) opening(i)=opening(i-2)
	enddo
c
c All amplitudes defined now; next search for transitions, using these fixed
c open and shut levels.
	if(imode.eq.1) goto 20		!skip search
c
	call GETONE(data,ix,nt,ntm,if1,if2,inext,itrig,it1,
     & scrit,ntrig,shut,shutprev,opendown,delt,ylevel,base,
     & t0,opening,trise,timamax,avamp,finter,npfilt,disptran,openlev)
c
c What if the requested number of transitions is not found? Must add more
c transitions to make up the requested number -make alternately open/shut
c -must define the necessary extra ylevel(), t0 and delt() values
	if(nt.lt.ntrans) then
	   imode1=1		!for single amplitude mode
	   call GETREST(nt,ntrans,ndfit,finter,shutprev,imode1,
     &    ylevel,base,avamp,prevlevel,inext,t0,delt,opening)
	endif
c
c Now draw initial guess based on durations found in GETONE
c Calc step amplitudes in stepamp()
	do i=1,ntrans
	   stepamp(i)=ylevel(i+1)-ylevel(i)		!in intermed units
	enddo
c Define time intervals for CONV in DT()
	y0=ylevel(1)	!initial value
	x0=t0*cfac
	x0=float(it1-1)*dx	!'time' in x units for 1st transition
c tf50=time (mus) to 50% point on filter. Convert this to x units:
c dxf x units=dtf microsec so dxf/dtf x units/mus
c add empirical shift t0cor (defined above)
	xcor=(tf50-t0cor)*dxf/dtf
	x0=x0-xcor
	t0=t0-tf50+t0cor	!NB VERY IMPORTANT TO CORRECT INIT GUESS FOR T0 TOO
c
	do i=1,ntrans-1
	   if(i.eq.1) then
	      dt(i)=delt(i)
	   else
	      dt(i)=dt(i-1) + delt(i)
	   endif
	enddo
c
20	continue
	call BELL(1)
	call WDIALOG(1,
     &'Now set initial guesses for transition times (F1=help)',-icw)
c (code above this point is in FITSUB6 for the sublevel case, but for
c single-amp case, transitions are sought after amp defined. In sublevel
c case, SETSUBL is called next)
c
c Define help for new menu
	helpst(1)='Set guess for position of each transition.'
	helpst(2)='Start with 1st transition.'
	helpst(3)='  Move transition with L/R arrow keys:'
	helpst(4)='  INS/key 0 toggles sensitivity;'
	helpst(5)='  + key increments transition #;'
	helpst(6)='  - key decrements transition #;'
	helpst(7)='  Hit <enter> to fit, when all finished'
	nhelp=7
	if(mouse_on) then
	   helpst(8)='OR'
	   helpst(9)='Mark position with left mouse button;'
	   helpst(10)=' right button increments transition #;'
	   helpst(11)=' centre button decrements transition #;'
	   helpst(12)='Hit both buttons or enter when done'
	   nhelp=12
	endif
c
c   NOW DRAW NEW MENU (then DISPLAY DATA and init guess)
	redraw=.false.
	first=.true.
	usekey=.false.
	call HIDE_MOUSE()
	nbox=15
	call SETLBOX(nbox,Lb,0)
	Lb(1)=1
	if(noredo) Lb(1)=0
	Lb(10)=1
	Lb(11)=1
	Lb(12)=1
	Lb(13)=1
	Lb(14)=1
	Lb(15)=1
	call DCMENU(0,5,Lb,text,ibk,ibk)		!delete all
	text(1)='1:REDO AMPLITUDE'
	if(bigstep) then
	   text(10)='10:MOVE SLOWER'
	else
	   text(10)='10:MOVE FASTER'
	endif
	text(11)='+:NEXT TRANSITION'
	text(12)='-:PREV TRANSITION'
	text(13)='x: CANCEL FIT'
	text(14)='\:SCROLL DIALOG'
c	text(15)='.:  HELP (F1)'
	text(15)='.:ACCEPT AS SHOWN'
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
c
c Go straight on with transition #1 as active one; get characters
c with GETCH below (but numlock off, so beware!)
c	call NUMSET			!set num lock on ready for response
c152	call CKEY(ch,ikey)
	call WDIALOG(1,
     & 'Now adjust guesses for position of transitions (F1=Help)',-icw)
	itrans=1		!current transition for adjustment
      call INTCONV(itrans,cnum)
	call OPENBOX(3,7,.true.)
	call WRITBOX(3,'trans # '//charnb(cnum),1,10)
	GOTO 4		!return to label 3 later to act on options below
c
3	continue		!return here with ikey from GETCH below
c	if(ikey.lt.-1.or.ikey.gt.15) goto 152	!also arrows allowed
c	if(ikey.eq.-1) ikey=15		!F1=help
	if(ikey.eq.-1) then
	   call POPTEXT(mxlo,mylo,myhi,helpst,nhelp,ictx,ibkx,icfx)
	endif
c
c NB should never return to 3 with  ikey=2-9 or ... -they are dealt with
c in the kbhit loop
	SELECT CASE(ikey)
	  case(1)		!redo amplitude
		call HIDE_MOUSE()		!delete conv curve
		call SETACT(iact,-1)	!no draw
c		iact(14)=ibk2	  !unless yellow, when draw background col
		iact(11)=ibk2	  !unless blue, when draw background col
		call IDCPIX(ixcalc,iycalc,ke,iact)
		goto 102

	  case(10)
	   if(bigstep) then
		tstep=10.	!10 microsec steps
	   else
		tstep=100.	!100 microsec steps
	   endif
	   bigstep=.not.bigstep
	   if(bigstep) then
		text(10)='10:MOVE SLOWER'
	   else
		text(10)='10:MOVE FASTER'
	   endif
	   call DCMENU(-10,5,Lb,text,icol1,icol2)	!redraw box 10
c
	  case(11)			!increment transition #
	   itrans=itrans+1
	   if(itrans.gt.ntrans) itrans=1
         call INTCONV(itrans,cnum)
	   call WRITBOX(3,'trans # '//charnb(cnum),1,10)
	   redraw=.true.

	  case(12)			!decrement transition #
	   itrans=itrans-1
	   if(itrans.lt.1) itrans=ntrans
         call INTCONV(itrans,cnum)
	   call WRITBOX(3,'trans # '//charnb(cnum),1,10)
	   redraw=.true.
c
	  case(13)		!cancel fit
		reject=.true.
		RETURN

	  case(14)			!scroll dialog box
		call SCROLDIAL(1)

	  case(15)
c	   call POPTEXT(mxlo,mylo,myhi,helpst,nhelp,ictx,ibkx,icfx)
		accept=.true.
		redraw=.true.
		do i=1,ntrans
		   if(setshut(i)) then
			iampl(kt+i)=0
		   else
	      	iampl(kt+i)=int2(ifixr(ylevel(i+1)-base))
		   endif
		enddo
		goto 99

	END SELECT
c
c Jump to here (label 4) with itrans=1 after 1st displaying menu, to display
c the data, init guess and transition markers.
c
4	continue
	call HIDE_MOUSE()
c Now show these guesses on screen as calc curve
c and mark transitions with white lines (or red line for that currently
c being adjusted)
c
c Draw transition markers
	do i=1,ntrans
	   if(i.eq.1) then
		t=t0
	   else
		t=t0+dt(i-1)
	   endif
c	   ix1=ifixr(t*dxf/dtf)
c	   ix1=ifixr(t*cfac)
	   ix1=ifixr((t+tshift)*cfac)	!tshift centralises bar over transition
c   erase previous transition marker (overwritten with background colour
c -does not restore graphics at present)
	   if(.not.first) then
	      call VLINE(ixlast(i),iyt1,iyt2,ibk2)
	   endif
	   ixlast(i)=ix1
	   ic=ic1
	   if(i.eq.itrans) ic=ic2
	   call VLINE(ix1,iyt1,iyt2,ic)
c	   call VARROW(ix1,420,380,ic)
	enddo
c
	first=.false.
c
	if(usekey) then
	   usekey=.false.		!reset
	   if(ikey.ne.11.and.ikey.ne.12) goto 2	!redraw if ikey=11,12
	endif
c
1	continue	!return to recalc and redraw
c Update transition marker
c NB need to redraw all, because if an attempt was made to move, say, trans #2
c past trans #3, then trans #3 will have moved too
	call HIDE_MOUSE()
	do i=1,ntrans
	   call VLINE(ixlast(i),iyt1,iyt2,ibk2)	!erase previous mark
	   if(i.eq.1) then
		t=t0
	   else
		t=t0+dt(i-1)
	   endif
	   ix1=ifixr((t+tshift)*cfac)	!tshift centralises bar over transition
	   ixlast(i)=ix1
	   ic=ic1
	   if(i.eq.itrans) ic=ic2
	   call VLINE(ix1,iyt1,iyt2,ic)
	enddo
c calc new curve
	call CONV1(ycalc,y0,ntrans,dt,ke,filt,nfilt,stepamp,dtf)
	if(ke.gt.5120) ke=5120
	do i=1,ke
	   ixcalc(i)=ifixr(x0 + float(i-1)*dxf)
	   iycalc(i)=int2(ifixr(ycalc(i)))
	enddo
c ycalc() is in units as given by readsec, without nscale or offset -must
c scale for display
	call SCALINT(iycalc,ke,nscale,iyoff)
c Display the convolved trace for init guesses, starting at time of 1st transition
	call SETACT(iact,-1)		!no draw
c	iact(ibk2)=14			!unless background col, when draw yellow
	iact(ibk2)=11			!unless background col, when draw blue
c    Overwrite colours 4,9,13 also (=amplitude lines)
c	iact(4)=14			      !also overdraw 4
c	iact(9)=14			      !also overdraw 9
c	iact(13)=14			      !also overdraw 13
	iact(4)=11			      !also overdraw 4
	iact(9)=11			      !also overdraw 9
	iact(13)=11			      !also overdraw 13
	call IDCPIX(ixcalc,iycalc,ke,iact)
	call SHOW_MOUSE()
c Now adjust values
c
2	continue
	call NUMCLR		!so either set of arrow keys can be used
	if(kbhit()) then
	   ch=GETCH(ktype)
	   ival=ichar(ch)	!here you may give other options
c with NUMLOCK off, as here, get ktype=3 for +,- whether on keypad or
c not, but 0-9 (and ./del) give ktype=8 with numlock off)
c Left or right arrow will give ikey=99 (not valid CKEY outputs); otherwise
c return to options following ckey above, with usekey=true, then return to 2
c for another character
	   if(ival.eq.13.or.ival.eq.27) then
c		Set last iampl before returning -already done
		redraw=.true.
		goto 99
	   endif
c
	   call SETCKEY(ktype,ival,ikey)		!in DCLIB now
c NB returns ikey=99 if ktype=8 (except for DEL='.' which returns ikey=15)
c eg '+' key has ktype=3, ival=43 whether numlock is on or off and so
c returns ikey=11
	   if(ktype.eq.8.and.ival.eq.82) ikey=10     !zero key with numlock off
	   if(ktype.eq.8.and.ival.eq.79) ikey=1      !1 key with numlock off
c
	   if(ikey.ne.99) then	!eg ikey =10,11,12,14,15
	      usekey=.true.
		if(ikey.eq.11.or.ikey.eq.12) then
		   redraw=.true.		!delete yellow curve and call RDGWIN1
		else
	         goto 3
		endif
	   endif
c
c	   i1=itrans-1
	   if(ktype.eq.8) then	!move with ARROW keys
		redraw=.true.
		sign=0.
		if(ival.eq.77.or.ival.eq.75) then 		!shift left/right
		   sign=1.			  		!shift right
		   if(ival.eq.75) sign=-1.		!shift left
		   if(itrans.eq.1) then
			t0=t0+sign*tstep
			if(t0.lt.0.) t0=tmin1
			x0=t0*cfac		!cfac=dx/finter
		   else		!itrans>1
			dt(itrans-1)=dt(itrans-1)+sign*tstep
			if(itrans.eq.2) then
			   if(dt(1).lt.tmin1) dt(1)=tmin1
			endif
			do i=2,ntrans-1
			   if(dt(i).le.dt(i-1)+tmin1) then
				dt(i)=dt(i-1)+tmin1
			   endif
			enddo
c			else
c			   if(dt(i1).lt.dt(i1-1)) dt(i1)=dt(i1-1)+tmin1
c			endif
		   endif
		endif
	   endif		!end of ktype=8
c
	   if(redraw) then
		redraw=.false.
c Delete previous calc curve
c (if last ycalc etc kept so not overwritten by new then can delete
c old immediately before drawing new)
		call HIDE_MOUSE()
		call SETACT(iact,-1)	!no draw
c		iact(14)=ibk2	  !unless yellow, when draw background col
		iact(11)=ibk2	  !unless blue, when draw background col
		call IDCPIX(ixcalc,iycalc,ke,iact)
		if(ikey.eq.11.or.ikey.eq.12) then
		   call SHOW_MOUSE()
		   goto 3
		else
		   call SHOW_MOUSE()
		   goto 1	!recalc and redraw
		endif
	   endif		!end of ktype=8 or ikey=13
	endif			!end of kbhit
c Insert mouse options here; do only if ibutton=0 at present -this will
c prevent loop being done several times while finger is on button -must
c release button (so ibutton -> 0) before reading ibutton again
c (get_button updates ix,iy only when left button hit if ident=0;
c             updates ix,iy only when right button hit if ident=1)
	if(mouse_on) then
c	   call get_button(ibutton,ident,icount,ix,iy)
	   call get_mouse(ibutton,ix2,iy2)
	   if(ibutton.eq.0) mous_set=.true.
         call show_mouse()
	endif
	if(mous_set) then
	   if(ibutton.ne.0) mous_set=.false.	!until reset above
	   if(ibutton.eq.1) then
		if(itrans.eq.1) then
c	        NB x0 defines where display of calc curve starts
		   x0=float(ix2) -xshift
		   t0=x0/cfac		!cfac=dx/finter
		   if(t0.lt.0.) t0=tmin1
		else		!itrans>1
		   t=float(ix2)/cfac - tshift
		   dt(itrans-1)=t-t0
		   if(itrans.eq.2) then
			if(t.le.t0+tmin1) dt(1)=tmin1
		   endif
		   do i=2,ntrans-1
			if(dt(i).le.dt(i-1)+tmin1) then
			   dt(i)=dt(i-1)+tmin1
			endif
		   enddo
		endif
c Delete previous calc curve
		ikey=0
		goto 31
	   else if(ibutton.eq.2) then
		ikey=11		!increment itrans
	      usekey=.true.
		goto 31
	   else if(ibutton.eq.3) then       !L AND R buttons to finish
		goto 99
	   else if(nbutton.eq.3.and.ibutton.eq.4) then
		ikey=12		!centre button=decrement itrans
	      usekey=.true.
		goto 31
	   endif
	endif		!end of mouse options
c
	goto 2	!get another character
c If ikey=11 or 12 then amplitude lines will be moved so must delete yellow
c calc curve so RDGWIN does not read yellow bits!
c Delete amplitude lines if their position is to be changed
31	continue
	call SETACT(iact,-1)	!no draw
c	iact(14)=ibk2	  !unless yellow, when draw background col
	iact(11)=ibk2	  !unless blue, when draw background col
      call HIDE_MOUSE()
	call IDCPIX(ixcalc,iycalc,ke,iact)
	if(ikey.eq.11.or.ikey.eq.12) then	!delete existing line
	   call SHOW_MOUSE()
	   goto 3
	else
	   call SHOW_MOUSE()
	   goto 1
	endif
c
c Finished:
99	continue
c
c  Redefine delt (used to be in FITSUB)
	do i=1,ntrans-1
	   if(i.eq.1) then
		delt(i)=dt(i)
	   else
		delt(i)=dt(i)-dt(i-1)
	   endif
	enddo
c
c Before leaving, hide mouse
	if(mouse_on) call HIDE_MOUSE()
c and delete all markers
	do i=1,ntrans
	   call VLINE(ixlast(i),iyt1,iyt2,ibk2)
	enddo
c
c delete textbox for 'trans #'
	call ENDBOX(3,ibk2)
c
	SHUT=iampl(kt+ntrans).eq.0	!shut after this fitting
c
	RETURN
	end

