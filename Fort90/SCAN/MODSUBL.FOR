	subroutine MODSUBL(iampl,iprops,ntmax,kt,t0,
     & y0,dt,ntrans,filt,nfilt,stepamp,dtf,trise,x0,dxf,
     & nscale,iyoff,cfac,ibk,ibk2,tshift,ylevel,delt,
     & kfix,opening,opendown,mono,base,scrit,shut,
     & ilast,ilast1,sfac3,avamp,calfac2,rms1,reject,
     & accept,setshut,icw,icw1,yd1,yd2,iyd1,iyd2)	!position of data area
c
c
c For MANUAL SUBLEVEL option: to ADJUST initial guesses as found
c by GETALL to make suitable initial guesses for the
c transition times and to set the (fixed) amplitude
c following each of the transitions (initial guesses are based on
c autosearch in GETTRANS, but with a preset number of transitions,
c so some could be in totally wrong place).
c (If guesses from GETALL are lousy the SETSUBL used instead to SET
c guesses from scratch)
c
c Now (03/22/94 01:57pm) used also to reset guesses in FITSUB (setshut
c added to parameters for use in fitsub to set kfix() etc)
c
c NB ylevel(nt)=current level (intermed units) BEFORE transition #nt
c    ylevel(nt+1)=current level (intermed units) AFTER transition #nt
c
c Problem: in this subroutine (though not in FIXGUESS) args of GET_MOUSE
c have to be integer*2 to work correctly!
c
c (FIXGUESS alters theta, as defined for ifitmode=0,1 (and kfix), but this
c is used only for ifitmode=2,3 where theta has no amplitude params and
c amplitudes are fixed, so adjust amplitudes via ylevel() values (->stepamp)
c here -no need for theta OR kfix.)
cc	subroutine FIXGUESS(t0sav,y0sav,dtsav,ntrans,filt,nfilt,
cc     & stepamp,dtf,trise,x0,dxf,nscale,iyoff,cfac,ibk,ibk2,
cc     & tshift,theta,t0,dt,delt,jfix,kfix,kmax,opening,
cc     & opendown,tshort,iprops,kt,base,ntmax)
cc To alter, manually, the initial guesses for durations of events for
cc refitting when initial auto-fit is not satisfactory
cc NB ntrans canNOT be altered!
cc 12/28/93 03:09pm Now can alter amplitudes too
cc Also now resets jfix(),kfix(),iprops(),opening() on the basis of the
cc new guesses
c
c (0) First check that there are no very short values -make all guesses
c at least 0.25*risetime in duration so easily visible?
c============?
c
c Change durations with pots as on PDP, redrawing convolved trace
c	repeatedly
c
	integer*2 iampl(0:ntmax)
	integer*1 iprops(ntmax)	!properties of ith interval (see SCAN)
	integer*2 ixcalc(5120),iycalc(5120)	!up to 5120 points for convol trace
	integer*2 ibutton,ix2,iy2
	integer*1 isavopen(320),isavy1(320),isavy2(320)
	integer kfix(0:50)
	integer iact(0:15),ixlast(50)
	integer ixn(25)
	real*4 dt(49),stepamp(50)
	real*4 ylevel(50)		!temp storage for transition search results
	real*4 delt(0:50)
	real*4 ycalc(5120)
	real*4 filt(640)
	character*1 ch,getch
	character*11 cnum
	logical bigstep,usekey,mouse_on,mous_set,kbhit,first,mono
	logical opening(50),setshut(0:50),down,opendown,redraw,shut
	logical sets,sets1,reject,accept,autoset
	character*55 helpst(17)		!help strings
C For DCMENU
	integer Lb(30)
	character*78 text(30)
c
	common/mousval/mouse_on,nbutton
	common/popvalx/mxlo,mylo,myhi,ictx,ibkx,icfx	!values for poptext calls
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
c Modify ylevel, amd set setshut,iampl, and stepamp, on basis of values
c that were set shut (via kfix) in GETTRANS, using value of 'base' (which
c may have been altered by 'adjust baseline' in FITSUB6
c Also set as in CHKSHUT (which is called before leaving to check again)
	do i=0,ntrans
	   setshut(i)=.false.
	   y1=ylevel(i+1)
	   if(kfix(i).eq.1.or.abs(y1-base).lt.rms1) then	!as in CHKSHUT
c	   if(kfix(i).eq.1.or.abs(y1-base).lt.0.4*scrit) then
c	   if(kfix(i).eq.1) then	!use kfix, from GETTRANS to define shut
		j=kt+i-1
		if(j.gt.0) then		!precaution at start
		   if(iampl(j).ne.0) then   !prevent consec zero amps!
			setshut(i)=.true.
			ylevel(i+1)=base
			iampl(kt+i)=0
		   endif
		endif
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
c Define help strings
	helpst(1)='(1) Set guess for position of each transition.'
	helpst(2)='(2) Set (fixed) amplitude AFTER each transition.'
	helpst(3)='Start with 1st transition.'
	helpst(4)='  Move transition with L/R arrow keys:'
	helpst(5)='  Move amplitude with up/down arrow,'
	helpst(6)='     OR set shut level with key 7;  '
	helpst(7)='     OR use keys 1,3,5 to set open level'
	helpst(8)='  INS/key 0 toggles sensitivity;'
	helpst(9)='  + key increments transition #;'
	helpst(10)='  - key decrements transition #;'
	helpst(11)='  Hit <enter> to fit, when all finished'
	helpst(12)='  OR hit ''.'' key to ACCEPT as shown (no fit)'
	nhelp=12
	if(mouse_on) then
	   helpst(13)='OR'
	   helpst(14)='Mark position with left mouse button;'
	   helpst(15)=' right button increments transition #;'
c	   helpst(16)=' centre button decrements transition #;'
	   helpst(16)=' centre button sets approx amplitude; '
	   helpst(17)=' Both buttons/enter when done'
	   nhelp=17
	endif
c First check for, and remove, any very short guesses (if ntrans>1)
	tmin1=0.25*trise		!OK?
c	tmin1=0.5*trise			!OK?
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
	icol3=12	!red text/border
	icol4=15	!white fill
	nbox=15
	call SETLBOX(nbox,Lb,1)
c Now use 2,4,6,8 for shifts so can use num keys as well as arrows (good
c for Tosh etc)
	Lb(1)=0		!drawn below
	Lb(3)=0		!drawn below
	Lb(5)=0		!drawn below
	Lb(7)=0		!drawn below
	call DCMENU(0,5,Lb,text,ibk,ibk)		!delete all
	text(1)='1: LAST BUT ONE'	!dark blue   9
	text(2)='2: REDUCE AMP'		!2=down arrow on keypad
	text(3)='3: USE LAST AMP'	!dark red    4
	text(4)='4: SHIFT LEFT'	!dark blue   9
	text(5)='5: USE MEAN AMP'	!purple     13
	text(6)='6: SHIFT RIGHT'
	text(7)='7: SHUT LEVEL'
	text(8)='8. INCREASE AMP'     !8=up arrow on keypad
	text(9)='9: SET DUBIOUS'
c=	text(1)='1: LAST BUT ONE'		!dark blue   9
c=	text(2)='2: USE LAST AMP'		!dark red    4
c=	text(3)='3: USE MEAN AMP'		!purple     13
c=	text(4)='4: SHUT LEVEL'
c=	text(8)='8: SET DUBIOUS'
	if(bigstep) then
	   text(10)='10:MOVE SLOWER'
	else
	   text(10)='10:MOVE FASTER'
	endif
	text(11)='+:NEXT TRANSITION'
	text(12)='-:PREV TRANSITION'
c===	text(13)='*:ADJUST BASELINE'	!do only on prev menu in FITSUB6
	text(13)='x:CANCEL THIS FIT'
	text(14)='\:SCROLL DIALOG'
c	text(15)='.:HELP ( = F1)'
	text(15)='.:ACCEPT AS SHOWN'
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
c	draw special colour boxes individually
	if(ilast1.ne.0) call DCMENU(-1,5,Lb,text,9,15)	!draw box 1 only
	if(ilast.ne.0) call DCMENU(-3,5,Lb,text,4,15)	!draw box 3 only
	call DCMENU(-5,5,Lb,text,13,15)	!draw box 5 only (mean)
	call DCMENU(-7,4,Lb,text,icol3,icol4)	!draw box 4 only -shut level
c
c Go straight on with transition #1 as active one; get characters
c with GETCH below (but numlock off, so beware!)
c	call NUMSET			!set num lock on ready for response
c152	call CKEY(ch,ikey)
	itrans=1		!current transition for adjustment
      call INTCONV(itrans,cnum)
	call OPENBOX(3,7,.true.)
	call WRITBOX(3,'trans # '//charnb(cnum),1,10)
	GOTO 4		!return to label 3 later to act on options below
c
3	continue		!return here with ikey from GETCH below
c	if(ikey.lt.-1.or.ikey.gt.15) goto 152	!also arrows allowed
c
c NB should never return to 3 with  ikey=1-8 or ... -they are dealt with
c in the kbhit loop
	reject=.false.
	accept=.false.
c
	SELECT CASE(ikey)
	  case(1)
	  case(2)
	  case(3)
	  case(4)

	  case(9)         !set open amplitude as dubious
	   kval=kt+itrans
	   if(iampl(kval).eq.0) then	!skip if 'cancel'
	      call BELL(2)
	      call WDIALOG(1,
     &       'Only openings can be set dubious -try again',-icw1)
	   else
	      iprops(kval)=IBSET(iprops(kval),0)   !set bit 0='1'
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
	  case(11)			!increment transition #
c NB update ilast,ilast1 when we move on to next/prev transition!
	   if(ilast.ne.0) ilast1=ilast
c	   ilast=ifixr(ylevel(itrans+1)-base)	!update 'last'
	   iamp=ifixr(ylevel(itrans+1)-base)
	   if(setshut(itrans)) then		!do not update ilast
		iampl(kt+itrans)=0
	   else
		iampl(kt+itrans)=int2(iamp)  	!set provisional amplitude
		ilast=iamp
		setshut(itrans)=.false.
	   endif
c=	   ilast=ifixr(ylevel(itrans+1)-base)	!update 'last'
c=	   if(setshut(itrans)) then
c=		iampl(kt+itrans)=0
c=	   else
c=		iampl(kt+itrans)=int2(ilast)  	!set provisional amplitude
c=		setshut(itrans)=.false.
c=	   endif
c
	   itrans=itrans+1
	   if(itrans.gt.ntrans) itrans=1
         call INTCONV(itrans,cnum)
	   call WRITBOX(3,'trans # '//charnb(cnum),1,10)
	   redraw=.true.

	  case(12)			!decrement transition #
	   if(ilast.ne.0) ilast1=ilast
c	   ilast=ifixr(ylevel(itrans+1)-base)	!update 'last'
	   iamp=ifixr(ylevel(itrans+1)-base)
	   if(setshut(itrans)) then		!do not update ilast
		iampl(kt+itrans)=0
	   else
		iampl(kt+itrans)=int2(iamp)  	!set provisional amplitude
		ilast=iamp
		setshut(itrans)=.false.
	   endif
c
	   itrans=itrans-1
	   if(itrans.lt.1) itrans=ntrans
         call INTCONV(itrans,cnum)
	   call WRITBOX(3,'trans # '//charnb(cnum),1,10)
	   redraw=.true.
c
	  case(13)				!cancel fit
		reject=.true.
		goto 999

	  case(14)			!scroll dialog box
		call NUMCLR		!so either arrows usable
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
c here the first time only; otherwise call RDGWIN1 straight after yellow
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
c	   call VARROW(ix1,420,380,ic)
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
c Change all guesses to blue now
c	iact(ibk2)=14			!unless background col, when draw yellow
cc    Overwrite colours 4,9,13 also (=amplitude lines)
c	iact(4)=14			      !also overdraw 4
c	iact(9)=14			      !also overdraw 9
c	iact(13)=14			      !also overdraw 13
	iact(ibk2)=11			!unless background col, when draw blue
c    Overwrite colours 4,9,13 also (=amplitude lines)
	iact(4)=11			      !also overdraw 4
	iact(9)=11			      !also overdraw 9
	iact(13)=11			      !also overdraw 13
	call IDCPIX(ixcalc,iycalc,ke,iact)
	call SHOW_MOUSE()
c Now adjust values
c
2	continue
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
c		Set last iampl before returning
c         NO -set all of them, otherwise some iampl() may not be set at all
c		e.g. if transition #itrans has never been adjusted!
		redraw=.true.
		do i=1,ntrans
		   if(setshut(i)) then
			iampl(kt+i)=0
		   else
	      	iampl(kt+i)=int2(ifixr(ylevel(i+1)-base))
		   endif
		enddo
		goto 99
	   endif
c
	   call SETCKEY(ktype,ival,ikey)		!in DCLIB now
	   if(ikey.eq.-1) then
c=		ikey=15			!F1 for help
		call POPTEXT(mxlo,mylo,myhi,helpst,nhelp,ictx,ibkx,icfx)
	   endif
c NB returns ikey=99 if ktype=8 (except for DEL='.' which returns ikey=15)
c eg '+' key has ktype=3, ival=43 whether numlock is on or off and so
c returns ikey=11
	   if((ktype.eq.8.and.ival.eq.82).or.ikey.eq.0) ikey=10     !zero key with numlock off
c
	   if(ikey.eq.2) then
		ival=80		!simulate down arrow
		ktype=8
		ikey=99
	   else if(ikey.eq.4) then
		ival=75		!simulate left arrow
		ktype=8
		ikey=99
	   else if(ikey.eq.6) then
		ival=77		!simulate left arrow
		ktype=8
		ikey=99
	   else if(ikey.eq.8) then
		ival=72		!simulate up arrow
		ktype=8
		ikey=99
	   endif
c
c Set predetermined amplitudes
	   if(ikey.eq.1.or.ikey.eq.3.or.ikey.eq.5.or.ikey.eq.7) then
		redraw=.true.
		if(ikey.eq.1) then		!last but one amp
		   ylevel(itrans+1)=float(iy1)  !update iampl when itrans changed
		   setshut(itrans)=.false.
		else if(ikey.eq.3) then		!last amp
		   ylevel(itrans+1)=float(iy)  !update iampl when itrans changed
		   setshut(itrans)=.false.
		else if(ikey.eq.5) then		!mean amp
		   ylevel(itrans+1)=oplev  !update iampl when itrans changed
		   setshut(itrans)=.false.
		else if(ikey.eq.7) then	!set shut level following transition #itrans
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
     & 	    'WARNING: THIS AMPLITUDE SAME AS LAST',1,14,12,14,1.0)
		endif
c
		do i=1,ntrans
		   stepamp(i)=ylevel(i+1)-ylevel(i)		!in intermed units
	         down=ylevel(i+1).lt.ylevel(i)
	         opening(i)=(down.eqv.opendown)	!needed in scanfunc
		enddo
	   else if(ikey.ge.9.and.ikey.le.15) then	!eg ikey =9-15
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
c next bit done if ikey=99, or ikey=2,4,6,8 which simulate arrow keys
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
c	iact(14)=ibk2	  !unless yellow, when draw background col
	iact(11)=ibk2	  !unless blue, when draw background col
      call HIDE_MOUSE()
	call IDCPIX(ixcalc,iycalc,ke,iact)
	if(ikey.eq.11.or.ikey.eq.12) then	!delete existing line
         call WRGWIN1(0,iopenvga,639,iopenvga,isavopen)
         call WRGWIN1(0,iyv1,639,iyv1,isavy2)
         call WRGWIN1(0,iyv,639,iyv,isavy1)
	   call SHOW_MOUSE()
	   goto 3
	else
	   if(accept) goto 999	!straight out after redraw
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
	imode=0	!not yet fixed to check all at once as in FITSUB
	n=0
	do i=1,ntrans
	   k=kt+i		!index for iampl()
	   y1=ylevel(i+1)
	   call CHKSHUT(y1,base,scrit,k,i,delt,t0,dxf,dtf,
     &	  tshift,ibk2,calfac2,rms1,avamp,SHUT,sets,autoset,
     &	   opendown,reject,n,ixn,imode)
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
999	continue
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


