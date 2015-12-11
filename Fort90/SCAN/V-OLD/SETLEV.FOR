	subroutine SETLEV(iampl,iprops,ntmax,kt,t0,
     & y0,dt,ntrans,filt,nfilt,stepamp,dtf,trise,x0,dxf,
     & nscale,iyoff,cfac,ibk,ibk2,tshift,ylevel,delt,
     & kfix,opening,opendown,mono,base,basevga,
     & ilast,ilast1,sfac3,avamp,icbl,calfac2)
c
c TO BE DONE:
c=========can get 'amp good', 'amp dubious', 'try again' here too?
c==========check that SHUT,SHUTPREV etc all set OK in fitsub4
c
c For MANUAL ONE-AMPLITUDE  option: to set initial guesses for
c transition times (as in FIXGUESS) and to set the (fixed) amplitude
c following each of the transitions (initial guesses are based on
c autosearch in GETTRANS, but with a preset number of transitions,
c so some could be in totally wrong place).
c NB ylevel(nt)=current level (intermed units) BEFORE transition #nt
c    ylevel(nt+1)=current level (intermed units) AFTER transition #nt
c
c Problem: in this subroutine (though not in FIXGUESS) args of GET_MOUSE
c have to be integer*2 to work correctly!
c
	integer*2 iampl(ntmax)
	integer*1 iprops(ntmax)	!properties of ith interval (see SCAN)
	integer*2 ixcalc(2048),iycalc(2048)	!up to 2048 points for convol trace
	integer*2 ibutton,ix2,iy2
	integer*1 isavopen(320),isavy1(320),isavy2(320),isavy3(320)
	integer kfix(0:50)
	integer iact(0:15),ixlast(50)
	real*4 dt(49),stepamp(50)
	real*4 ylevel(50)		!temp storage for transition search results
	real*4 delt(0:50)
	real*4 ycalc(2048)
	real*4 filt(640)
	character*1 ch,getch
	character*11 cnum
	integer*2 ktype
	logical bigstep,usekey,mouse_on,mous_set,kbhit,first,mono
	logical opening(50),setshut(0:50),down,opendown,redraw,shut
	logical sets,sets1
	character*55 helpst(15)		!help strings
C For DCMENU
	integer Lb(18)
	character*78 text(18)
c
	common/mousval/mouse_on,nbutton
	common/popvalx/mxlo,mylo,myhi,ictx,ibkx,icfx	!values for poptext calls
	common/datpos/yd1,yd2,iyd1,iyd2	!position of data area
c
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
c Initialise
	xshift=tshift*cfac	!tshift in pixel units
	bigstep=.false.
	usekey=.false.
c
c If initially open and NOT sublevel mode, then open level=prevlevel
c and next level must be shut, so no need to set any levels here
c	if(.not.sublev.and.(.not.SHUT)) then
	if(.not.SHUT) then
	   iampl(kt+1)=0		!set provisional amplitude
	   iprops(kt+1)=0
	   ylevel(1)=prevlevel
	   ylevel(2)=base       !level AFTER 1st transition
	   goto 260			!now within SETLEV
	endif
c
c Modify ylevel, and set iampl, and stepamp, on basis of values
c that were set shut (via kfix) in GETTRANS, using value of 'base' (which
c may have been altered by 'adjust baseline' in FITSUB4
c Also set as in CHKSHUT (which is called before leaving to check again)
	do i=0,ntrans
	   y1=ylevel(i+1)
	   if(kfix(i).eq.1.or.abs(y1-base).lt.0.4*scrit) then
c	   if(kfix(i).eq.1) then	!use kfix, from GETTRANS to define shut
		ylevel(i+1)=base
		iampl(kt+i)=0
	   endif
	enddo
	do i=1,ntrans
	   stepamp(i)=ylevel(i+1)-ylevel(i)		!in intermed units
	enddo
	redraw=.false.
	first=.true.
	tstep=10.		!10 microsec steps
	astep=10.         !intermed units
	mous_set=.false.	!initialise
c Define help strings for amplitude setting:
	helpst(1)='First set the (fixed) amplitude (one'
	helpst(2)='   value for all openings in this fit).'
	helpst(3)='  Move amplitude with up/down arrow,'
	helpst(4)='     OR use keys 1-3 to set open level'
	helpst(5)='     OR set shut level with key 4;  '
	helpst(6)='  INS/key 0 toggles sensitivity;'
	nhelp=6
	if(mouse_on) then
	   helpst(7)='OR, with mouse,'
	   helpst(8)=' centre button sets approx amplitude; '
	   helpst(9)='Hit both buttons or enter when done'
	   nhelp=9
	endif
=============
	helpst(1)='(1) Set guess for position of each transition.'
	helpst(2)='(2) Set (fixed) amplitude AFTER each transition.'
	helpst(3)='Start with 1st transition.'
	helpst(4)='  Move transition with L/R arrow keys:'
	helpst(5)='  Move amplitude with up/down arrow,'
	helpst(6)='     OR set shut level with key 4;  '
	helpst(7)='  INS/key 0 toggles sensitivity;'
	helpst(8)='  + key increments transition #;'
	helpst(9)='  - key decrements transition #;'
	helpst(10)='  Hit <enter> to fit, when all finished'
	nhelp=10
	if(mouse_on) then
	   helpst(11)='OR'
	   helpst(12)='Mark position with left mouse button;'
	   helpst(13)=' right button increments transition #;'
c	   helpst(14)=' centre button decrements transition #;'
	   helpst(14)=' centre button sets approx amplitude; '
	   helpst(15)='Hit both buttons or enter when done'
	   nhelp=15
	endif
c
=========================

c Draw up amplitudes
c  Last amp
	   if(ilast.ne.0) then
		iy=ifixr(base + float(ilast))		!intermed units
		iyv=int2(iy)		!integer*2 for scalint
		call SCALINT(iyv,1,nscale,iyoff)	!pixel units
		call RDGWIN1(0,iyv,639,iyv,isavy1)  !keep data overwritten
		call DHLINE(0,639,iyv,4,22)		!show last amp (short dash)
	   endif
c  Last but one amp
	   if(ilast1.ne.0) then
		iy1=ifixr(base + float(ilast1))
		iyv1=int2(iy1)
		call SCALINT(iyv1,1,nscale,iyoff)
		call RDGWIN1(0,iyv1,639,iyv1,isavy2)  !keep data overwritten
		call DHLINE(0,639,iyv1,9,88)	!show last but one amp (longer dash)
	   endif
c Mean full amp
	   oplev=avamp+base
	   openvga=oplev*sfac3 + float(iyoff)	!as in SCALINT (sfac3=1/2**nscale)
	   iopenvga=ifixr(openvga)
	   call RDGWIN1(0,iopenvga,639,iopenvga,isavopen)  !keep data overwritten
	   call HLINE(0,639,iopenvga,13)	   			!show mean full amp level
c
c First check for, and remove, any very short guesses (if ntrans>1)
	tmin1=0.25*trise		!OK?
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
c	icol3=12	!red text/border
c	icol4=15	!white fill
	nbox=15
	call SETLBOX(5,Lb,1)
	Lb(8)=1
	Lb(10)=1
	Lb(14)=1
	Lb(15)=1
	call DCMENU(0,5,Lb,text,ibk,ibk)		!delete all
	text(1)='1: LAST BUT ONE'		!dark blue   9
	text(2)='2: USE LAST AMP'		!dark red    4
	text(3)='3: USE MEAN AMP'		!purple     13
	text(4)='4: SHUT LEVEL'
	text(5)='5: SET MANUALLY'
	text(8)='8:SET AMP DUBIOUS'
c=========can get 'try again' here too?
	if(bigstep) then
	   text(10)='10:MOVE SLOWER'
	else
	   text(10)='10:MOVE FASTER'
	endif
	text(14)='\:SCROLL DIALOG'
	text(15)='.:  HELP (F1)'
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
c	draw special colour boxes individually
	if(ilast1.ne.0) call DCMENU(-1,5,Lb,text,9,15)	!draw box 1 only
	if(ilast.ne.0) call DCMENU(-2,5,Lb,text,4,15)	!draw box 2 only
	call DCMENU(-3,5,Lb,text,13,15)	!draw box 3 only
c
c Go straight on with transition #1 as active one; get characters
c with GETCH below (but numlock off, so beware!)
	call NUMSET			!set num lock on ready for response
152	call CKEY(ch,ikey)

===============
	itrans=1		!current transition for adjustment
      call INTCONV(itrans,cnum)
	call OPENBOX(3,7,.true.)
	call WRITBOX(3,'trans # '//charnb(cnum),1,10)
	GOTO 4		!return to label 3 later to act on options below
===============
c
3	continue		!return here with ikey from GETCH below
c	if(ikey.lt.-1.or.ikey.gt.15) goto 152	!also arrows allowed
	if(ikey.eq.-1) ikey=15		!F1=help
	if(iabs(ikey).eq.16) goto 100		!ESC or ENTER -amplitude defined!
c
c NB should never return to 3 with  ikey=1-4 or ... -they are dealt with
c in the kbhit loop
	SELECT CASE(ikey)
	    case(1)                   	!last but one amp
		ylevel(2)=float(iy1)
		iampl(kt+1)=ifixr(ylevel(2)-base)  	!set provisional amplitude

	    case(2)
		ylevel(2)=float(iy)           !last amp
		iampl(kt+1)=ifixr(ylevel(2)-base)  	!set provisional amplitude

	    case(3)             		!mean amp
		ylevel(2)=oplev
		iampl(kt+1)=ifixr(ylevel(2)-base)  	!set provisional amplitude

	    case(4)		!shut
		iampl(kt+1)=0		!set provisional amplitude
		iprops(kt+1)=0
		ylevel(2)=base

	  case(5)		!set amp manually
		call WDIALOG(1,
     &  'Set open level with arrow keys: then choose menu option',icw)
		oplev=avamp+base
c         draw line in yellow before call to moveline so it appears as
c	    soon as key 3 hit
	      call HLINE(0,639,iopenvga,14)		!show mean full amp level
		call NUMSET		!NB must use SEPARATE arrow keys in MOVELINE
		call MOVELINE(iopenvga,14,isavopen,ikey1)	!returns ikey
		openvga=float(iopenvga)
		oplev=(openvga-float(iyoff))/sfac3	!convert to intermed units (like curlev etc)
		ylevel(2)=oplev
c update 'last but one' etc (only when manual fit used -> measured amp)
		if(ilast.ne.0) ilast1=ilast
		ilast=ifixr(ylevel(2)-base)	!update 'last'
		iampl(kt+1)=int2(ilast)  	!set provisional amplitude

	  case(8)         !set open amplitude as dubious
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

	  case(10)
	   if(bigstep) then
		tstep=10.	!10 microsec steps
		astep=10.   !intermed units
	   else
		tstep=100.	!100 microsec steps
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
	  case(13)				!adjust baseline	-not in here now

	  case(14)			!scroll dialog box
		call SCROLDIAL(1)

	  case(15)
	   call POPTEXT(mxlo,mylo,myhi,helpst,nhelp,ictx,ibkx,icfx)

	END SELECT
	goto 152		!get another key, until enter is hit
c
c Jump to here once amplitude defined
100	continue
c Delete amplitude lines
	call WRGWIN1(0,iopenvga,639,iopenvga,isavopen)
	call WRGWIN1(0,iyv1,639,iyv1,isavy2)
	call WRGWIN1(0,iyv,639,iyv,isavy1)
c ylevel(1)=level before first transition, was estimated in GETALL
c (or set to prevlevel, if initially open)
c and ylevel(2)=level after 1st trans was set above; set rest of amplitudes
c so they alternate now
	nt1=ntrans
	do i=3,nt1+1
	   ylevel(i)=ylevel(i-2) 		!set alternating levels
	enddo
c
c All amplitudes defined now, so draw initial guess based on durations
c found in GETALL (but ignoring amplitudes found there
========DEFINE STEPAMP AND DRAW INIT GUESS
========no- getall does not alternate open/shut, so bound to have trans in
=====wrong positions -need a version that finds only alternating transitions!
=====use FINDALT to do this
=========up to here
c
c Jump to here (label 4) with itrans=1 after 1st displaying menu, to display
c the data, transition markers and 'last', 'last-but-one' etc amplitude lines
c
4	continue
	call HIDE_MOUSE()
c
c Now show these guesses on screen as calc curve
c and mark transitions with white lines (or red line for that currently
c being adjusted)
c -or use MOVELINE?? Put mouse in MOVELINE?? Rename as MOVELINH (horizontal)
c and make also MOVELINV (vertical) for use here
c
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
c Now draw the 'last', 'last-but-one' etc amplitude lines
c Use diff colours/dash patterns for each
c Use rdgwin1 to save underlying pixels, but do this the first time only,
c before step response function drawn, so only data points recorded -otherwise
c - problem is that RDGWIN1 may record parts of (yellow) step response function
c (when it coincides with one of the amplitude lines) and redraw the yellow
c bits in undesired places when WRGWIN1 called!  To avoid this, use RDGWIN1
c here the first time only; otherwise call RDGWIN1 straight agter yellow
c points are deleted by IDCPIX below, before returning (for ikey=11,12) to
c redefine ilast,ilast1.
c (NB write and erase of levels can be outside loop if 'last', 'last but one'
c not updated for each level defined)
c  Last amp
	if(ilast.ne.0) then
	   iy=ifixr(base + float(ilast))		!intermed units
	   iyv=int2(iy)		!integer*2 for scalint
	   call SCALINT(iyv,1,nscale,iyoff)	!pixel units
c Record underlying graphics before drawing line (AND before drawing yellow curve)
	   call RDGWIN1(0,iyv,639,iyv,isavy1)  !keep data overwritten
	   call DHLINE(0,639,iyv,4,22)		!show last amp (short dash)
	endif
c  Last but one amp
	if(ilast1.ne.0) then
	   iy1=ifixr(base + float(ilast1))
	   iyv1=int2(iy1)
	   call SCALINT(iyv1,1,nscale,iyoff)
c Record underlying graphics before drawing line (AND before drawing yellow curve)
	   call RDGWIN1(0,iyv1,639,iyv1,isavy2)  !keep data overwritten
	   call DHLINE(0,639,iyv1,9,88)	!show last but one amp (longer dash)
	endif
c Mean full amp
	oplev=avamp+base
	openvga=oplev*sfac3 + float(iyoff)	!as in SCALINT (sfac3=1/2**nscale)
	iopenvga=ifixr(openvga)
c Record underlying graphics before drawing line (AND before drawing yellow curve)
	call RDGWIN1(0,iopenvga,639,iopenvga,isavopen)  !keep data overwritten
	call HLINE(0,639,iopenvga,13)	   			!show mean full amp level
c Amplitude lines drawn
c
	if(usekey) then
	   usekey=.false.		!reset
	   if(ikey.ne.11.and.ikey.ne.12) goto 2	!redraw if ikey=11,12
	endif
c
1	continue	!return to recalc and redraw
c update transition marker
c  delete previous transition marker
	call HIDE_MOUSE()
	call VLINE(ixlast(itrans),iyt1,iyt2,ibk2)
	if(itrans.eq.1) then
	   t=t0
	else
	   t=t0+dt(itrans-1)
	endif
	ix1=ifixr((t+tshift)*cfac)	!tshift centralises bar over transition
	ixlast(itrans)=ix1
	ic=ic2			!draw current marker
	call VLINE(ix1,iyt1,iyt2,ic)
c calc new curve
	call CONV1(ycalc,y0,ntrans,dt,ke,filt,nfilt,stepamp,dtf)
	if(ke.gt.2048) ke=2048
	do i=1,ke
	   ixcalc(i)=ifixr(x0 + float(i-1)*dxf)
	   iycalc(i)=int2(ifixr(ycalc(i)))
	enddo
c ycalc() is in units as given by readsec, without nscale or offset -must
c scale for display
	call SCALINT(iycalc,ke,nscale,iyoff)
c Display the convolved trace for init guesses, starting at time of 1st transition
	call SETACT(iact,-1)		!no draw
	iact(ibk2)=14			!unless background col, when draw yellow
c    Overwrite colours 4,9,13 also (=amplitude lines)
	iact(4)=14			      !also overdraw 4
	iact(9)=14			      !also overdraw 9
	iact(13)=14			      !also overdraw 13
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
c		Set last iampl before returning
		redraw=.true.
		if(setshut(itrans)) then
		   iampl(kt+itrans)=0
		else
	         iampl(kt+itrans)=int2(ifixr(ylevel(itrans+1)-base))
		endif
		goto 99
	   endif
c
	   call SETCKEY(ktype,ival,ikey)		!in DCLIB now
c NB returns ikey=99 if ktype=8 (except for DEL='.' which returns ikey=15)
c eg '+' key has ktype=3, ival=43 whether numlock is on or off and so
c returns ikey=11
	   if(ktype.eq.8.and.ival.eq.82) ikey=10     !zero key with numlock off
c
	   if(ikey.ge.1.and.ikey.le.4) then		!set predefined amplitude
		redraw=.true.
		if(ikey.eq.1) then		!last but one amp
		   ylevel(itrans+1)=float(iy1)  !update iampl when itrans changed
		   setshut(itrans)=.false.
		else if(ikey.eq.2) then	!last amp
		   ylevel(itrans+1)=float(iy)  !update iampl when itrans changed
		   setshut(itrans)=.false.
		else if(ikey.eq.3) then	!mean amp
		   ylevel(itrans+1)=oplev  !update iampl when itrans changed
		   setshut(itrans)=.false.
		else if(ikey.eq.4) then	!set shut level following transition #itrans
		   ylevel(itrans+1)=base
		   iprops(kt+itrans)=0
		   setshut(itrans)=.true.		!so iampl() set to 0
c==		   iampl(kt+itrans)=0	!iampl updated when itrans changed
		endif
c      and redefine stepamp(), from new ylevel()
c Check for two contiguous equal amplitudes, which look confusing, and would
c be invalid unless fixed
c		if(ylevel(itrans+1).eq.ylevel(itrans+2)) then	!next interval same
		if(abs(ylevel(itrans+1)-ylevel(itrans+2)).le.2.) then	!next is 'same'
		   if(ylevel(itrans+1).eq.base) ylevel(itrans+2)=0.5*oplev
c=		else if(itrans.gt.1.and.
c=     &	     (abs(ylevel(itrans+1)-ylevel(itrans)).le.2.) then !last is 'same'
		else if(abs(ylevel(itrans+1)-ylevel(itrans)).le.2.) then !last is 'same'
		   call POPTIME(250,130,
     & 	    'WARNING: THIS AMPLITUDE SAME AS LAST',1,14,12,14,2.0)
		endif
c
		do i=1,ntrans
		   stepamp(i)=ylevel(i+1)-ylevel(i)		!in intermed units
	         down=ylevel(i+1).lt.ylevel(i)
	         opening(i)=(down.eqv.opendown)	!needed in scanfunc
		enddo
	   else if(ikey.ne.99) then	!eg ikey =5-15
c Delete amplitude lines before returning to 3, to act on ikey
		call HIDE_MOUSE()
	      call WRGWIN1(0,iopenvga,639,iopenvga,isavopen)
	      call WRGWIN1(0,iyv1,639,iyv1,isavy2)
	      call WRGWIN1(0,iyv,639,iyv,isavy1)
		call SHOW_MOUSE()
	      usekey=.true.
		if(ikey.eq.11.or.ikey.eq.12) then
		   redraw=.true.		!delete yellow curve and call RDGWIN1
		else
	         goto 3
		endif
	   endif
c
	   i1=itrans-1
	   if(ktype.eq.8) then	!move with ARROW keys
		redraw=.true.
		sign=0.
		if(ival.eq.77.or.ival.eq.75) then 		!shift left/right
		   sign=1.			  		!shift right
		   if(ival.eq.75) sign=-1.		!shift left
		   if(i1.eq.0) then
			t0=t0+sign*tstep
			if(t0.lt.0.) t0=0.0
			x0=t0*cfac		!cfac=dx/finter
		   else		!i1.ge.1
			dt(i1)=dt(i1)+sign*tstep
			if(i1.eq.1) then
			   if(dt(i1).lt.tmin1) i1=tmin1
			else
			   if(dt(i1).lt.dt(i1-1)) dt(i1)=dt(i1-1)+tmin1
			endif
		   endif
c Now up/down arrow to alter amplitude (if 0 initially may need to unfix it!)
		else if(ival.eq.72.or.ival.eq.80) then 		!up=72/down=80
		   setshut(itrans)=.false.
c==		   kfix(itrans)=0		!in case already set to base, above
		   sign=1.			  		!shift up
		   if(ival.eq.80) sign=-1.		!shift down
		   ylevel(itrans+1)=ylevel(itrans+1) + sign*astep
c      and redefine stepamp(), from new ylevel()
		   do i=1,ntrans
			stepamp(i)=ylevel(i+1)-ylevel(i)		!in intermed units
		      down=ylevel(i+1).lt.ylevel(i)
	      	opening(i)=(down.eqv.opendown)	!needed in scanfunc
		   enddo
		endif
	   endif		!end of ktype=8
c

c==	   if(ktype.eq.8.or.ikey.eq.13) then
	   if(redraw) then
		redraw=.false.
c Delete previous calc curve
c (if last ycalc etc kept so not overwritten by new then can delete
c old immediately before drawing new)
		call HIDE_MOUSE()
		call SETACT(iact,-1)	!no draw
		iact(14)=ibk2	  !unless yellow, when draw background col
		call IDCPIX(ixcalc,iycalc,ke,iact)
c Delete amplitude lines if their position is to be changed
		if(ikey.eq.11.or.ikey.eq.12) then
	         call WRGWIN1(0,iopenvga,639,iopenvga,isavopen)
	         call WRGWIN1(0,iyv1,639,iyv1,isavy2)
	         call WRGWIN1(0,iyv,639,iyv,isavy1)
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
		else
		   t=float(ix2)/cfac - tshift
		   dt(itrans-1)=t-t0
		   if(itrans.eq.2) then
			if(t.le.t0+tmin1) dt(1)=tmin1
		   else
			if(dt(itrans-1).le.dt(itrans-2)+tmin1) then
			   dt(itrans-1)=dt(itrans-2)+tmin1
			endif
		   endif
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
	   else if(nbutton.eq.3.and.ibutton.eq.4) then	!centre button=ampl
		ylevel(itrans+1)=float(iy2-iyoff)/sfac3
c      and redefine stepamp(), from new ylevel()
		do i=1,ntrans
		   stepamp(i)=ylevel(i+1)-ylevel(i)		!in intermed units
		   down=ylevel(i+1).lt.ylevel(i)
	         opening(i)=(down.eqv.opendown)	!needed in scanfunc
		enddo
		ikey=0
		goto 31
c=		ikey=12		!centre button=decrement itrans
c=	      usekey=.true.
c=		goto 31
	   endif
	endif		!end of mouse options
c
	goto 2	!get another character
c If ikey=11 or 12 then amplitude lines will be moved so must delete yellow
c calc curve so RDGWIN does not read yellow bits!
c Delete amplitude lines if their position is to be changed
31	continue
	call SETACT(iact,-1)	!no draw
	iact(14)=ibk2	  !unless yellow, when draw background col
      call HIDE_MOUSE()
	call IDCPIX(ixcalc,iycalc,ke,iact)
	if(ikey.eq.11.or.ikey.eq.12) then	!delete existing line
         call WRGWIN1(0,iopenvga,639,iopenvga,isavopen)
         call WRGWIN1(0,iyv1,639,iyv1,isavy2)
         call WRGWIN1(0,iyv,639,iyv,isavy1)
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
c Before leaving, check any levels that are not already set shut, to see
c if they should be.
	sets1=.false.	!set true if ANY interval reset to shut
	do i=1,ntrans
	   k=kt+i		!index for iampl()
	   y1=ylevel(i+1)
	   call CHKSHUT(y1,base,scrit,k,i,delt,t0,dxf,dtf,
     &	  tshift,ibk2,calfac2,SHUT,sets)
	   if(shut) iampl(k)=0
	   if(sets) then
	      setshut(i)=.true.
	      ylevel(i+1)=base
	      sets1=.true.	!set true if ANY interval reset to shut
	   endif
	enddo
	if(sets1) then
	   do i=1,ntrans
		stepamp(i)=ylevel(i+1)-ylevel(i)	!recalc stepamp
	   enddo
	   ikey=0
	   goto 31		!redraw with amps set at 0
	endif
c
c Before leaving, hide mouse
	if(mouse_on) call HIDE_MOUSE()
c and delete all markers
	do i=1,ntrans
	   call VLINE(ixlast(i),iyt1,iyt2,ibk2)
	enddo
c
c and delete amplitude lines before returning
	call WRGWIN1(0,iopenvga,639,iopenvga,isavopen)
	call WRGWIN1(0,iyv1,639,iyv1,isavy2)
	call WRGWIN1(0,iyv,639,iyv,isavy1)
c delete textbox for 'trans #'
	call ENDBOX(3,ibk2)
c
	RETURN
	end

