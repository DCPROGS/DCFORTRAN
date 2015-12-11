	subroutine SETSUBL(iampl,iprops,ntmax,kt,t0,
     & y0,dt,ntrans,filt,nfilt,stepamp,dtf,trise,x0,dxf,
     & nscale,iyoff,cfac,ibk,ibk2,tshift,ylevel,delt,
     & kfix,opening,opendown,mono,base,scrit,shut,
     & ilast,ilast1,sfac3,avamp,calfac2,rms1,reject,accept,
     & icw,icw1,tshift1,xshift1,ibad,setshut,
     & finter,npfilt,data,if1,if2,timamax,yd1,yd2,iyd1,iyd2)
c
c (last line of params needed for SETSHORT)
c TO BE DONE:
c
c For MANUAL SUBLEVEL option: to SET, from scratch, initial guesses for
c transition times and to set the (fixed) amplitude
c following each of the transitions (used when initial guesses from
c autosearch in GETTRANS are too bad to be adjusted conveniently)
c  This version uses BOTH mouse coordinates -click on END of each
c transition to mark (a) amplitude after the transition, and (b) where
c the transition ends. Goes sequentially through the transitions until
c all are set (with option to restart them).  Once all are set (ALLSET=true)
c reverts to behaving like MODSUBL so these guesses can be adjusted.
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
	real*4 dt(49),stepamp(50)
	real*4 ylevel(50)		!temp storage for transition search results
	real*4 delt(0:50)
	real*4 ycalc(5120)
	real*4 filt(640)
	character*1 ch,getch
	character*11 cnum
	logical bigstep,usekey,mouse_on,mous_set,kbhit,first,mono
	logical opening(50),setshut(0:50),down,opendown,redraw,shut
	logical sets,sets1,reject,accept,allset,autoset
	character*55 helpst(17)		!help strings
	character*55 helps1(8)		!help strings
	integer ixn(25)
C For DCMENU
	integer Lb(30)
	character*78 text(30)
	character*78 text1(4)
c
	common/mousval/mouse_on,nbutton
	common/dialval1/ictd,icfd,icbd  !colours for dialog box #1 (text,frame,back)
	common/popvalx/mxlo,mylo,myhi,ictx,ibkx,icfx	!values for poptext calls
c
	ibad=0
	if(.not.mouse_on) then
	   call POPTIME(250,130,
     &     'MOUSE NOT DETECTED: CANNOT USE THIS METHOD',1,14,12,14,1.5)
	   ibad=1
	   RETURN
	endif
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
c Next bit removed because initial guess from GETALL is ignored here
c Modify ylevel, amd set setshut,iampl, and stepamp, on basis of values
c that were set shut (via kfix) in GETALL
	do i=0,ntrans	!just initialise setshut
	   setshut(i)=.false.
	enddo
c	do i=0,ntrans
c	   setshut(i)=.false.
c	   y1=ylevel(i+1)
c	   if(kfix(i).eq.1.or.abs(y1-base).lt.0.4*scrit) then
cc	   if(kfix(i).eq.1) then	!use kfix, from GETTRANS to define shut
c		setshut(i)=.true.
c		ylevel(i+1)=base
c		iampl(kt+i)=0
c	   endif
c	enddo
c	do i=1,ntrans
c	   stepamp(i)=ylevel(i+1)-ylevel(i)		!in intermed units
c	enddo
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
	   helpst(17)=' centre button sets approx amplitude; '
	   helpst(17)=' Both buttons/enter when done'
	   nhelp=17
	endif
c Define 'short' guess
	tmin2=0.10*trise		!OK?
	tmin1=0.25*trise			!OK?
c
c Put up new menu
	icol1=14	!yellow text/border for boxes in DCMENU
	icol2=8	!grey background for boxes
	icol3=12	!red text/border
	icol4=15	!white fill
	call DCMENU(0,5,Lb,text,ibk,ibk)		!delete all
c First helps (while guesses being set)
      call INTCONV(ntrans,cnum)
	n=NBLANK(cnum)
	helps1(1)='Define both amplitude AND time of each transition'
	helps1(2)='  by click with LEFT mouse button at END of the'
	helps1(3)='  each of the '//cnum(1:n)//' transitions (to indicate'
	helps1(4)='  amplitude that FOLLOWS the transition).'
	helps1(5)='To insert a short event (2 transitions at once)'
	helps1(6)='  click with RIGHT mouse button just BEFORE the'
	helps1(7)='  point where the brief event starts.'
	helps1(8)='Key 1: restarts definition if you mess it up.'
	nhelp1=8
c First menu (while guesses being set)
	nbox=3
	text1(1)='1:RESTART GUESSES'
c	text1(2)='2:INSERT SHORT GAP'
c	text1(3)='3:INSERT SHORT OP'
	text1(3)='3:HELP (=F1)     '
	call SETLBOX(nbox,Lb,1)
	LB(2)=0
	call DCMENU(nbox,4,Lb,text1,icol1,icol2)
C Define 2nd ('modify guesses') menu now (drawn once allset=true.)
	nbox=15
	call SETLBOX(nbox,Lb,1)
c Now use 2,4,6,8 for shifts so can use num keys as well as arrows (good
c for Tosh etc)
	Lb(1)=0		!drawn below
	Lb(3)=0		!drawn below
	Lb(5)=0		!drawn below
	Lb(7)=0		!drawn below
	text(1)='1: LAST BUT ONE'	!dark blue   9
	text(2)='2: REDUCE AMP'		!2=down arrow on keypad
	text(3)='3: USE LAST AMP'	!dark red    4
	text(4)='4: SHIFT LEFT'
	text(5)='5: USE MEAN AMP'	!purple     13
	text(6)='6: SHIFT RIGHT'
	text(7)='7: SHUT LEVEL'
	text(8)='8. INCREASE AMP'     !8=up arrow on keypad
	text(9)='9: SET DUBIOUS'
	if(bigstep) then
	   text(10)='10:MOVE SLOWER'
	else
	   text(10)='10:MOVE FASTER'
	endif
	text(11)='+:NEXT TRANSITION'
	text(12)='-:PREV TRANSITION'
	text(13)='x:CANCEL THIS FIT'
	text(14)='\:SCROLL DIALOG'
c	text(15)='.:HELP ( = F1)'
	text(15)='.:ACCEPT AS SHOWN'
c Do not draw above menu until allset=true
c	call DCMENU(nbox,5,Lb,text,icol1,icol2)
cc	draw special colour boxes individually
c	if(ilast1.ne.0) call DCMENU(-1,5,Lb,text,9,15)	!draw box 1 only
c	if(ilast.ne.0) call DCMENU(-3,5,Lb,text,4,15)	!draw box 3 only
c	call DCMENU(-5,5,Lb,text,13,15)	!draw box 5 only (mean)
c	call DCMENU(-7,4,Lb,text,icol3,icol4)	!draw box 4 only -shut level
c
c Go straight on with transition #1 as active one; get characters
c with GETCH below (but numlock off, so beware!)
c	call NUMSET			!set num lock on ready for response
c152	call CKEY(ch,ikey)
30	continue		!return here to restart setting of guesses
	itrans=1		!current transition for adjustment
	itmax=0		!largest itrans posn of which has been defined
      call INTCONV(itrans,cnum)
	call OPENBOX(3,7,.true.)
	call WRITBOX(3,'trans # '//charnb(cnum),1,10)
c
	allset=.false.    !set true once all transitions initially defined
	tsh=tshift1		!until allset=true
	xsh=xshift1
c
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
      	call INTCONV(kval,cnum)
		n=NBLANK(cnum)
	      call WDIALOG(1,
     &       'Amplitude of opening '//cnum(1:n)//' marked as dubious',
     &		-icw1)
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
c Jump to here (label 4) with itrans=1 after 1st displaying menu, to get
c the position of first transition
c
4	continue
	call HIDE_MOUSE()
c
	do i=1,itmax
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
c
c Update transition marker
c NB need to redraw all, because if an attempt was made to move, say, trans #2
c past trans #3, then trans #3 will have moved too
	call HIDE_MOUSE()
	if(.not.allset) then
	   call BELL(1)
         call INTCONV(itrans,cnum)
	   call WRITBOX(3,'trans # '//charnb(cnum),1,10)
	   call WDIALOG(1,
     & 'Click to mark ampl and position at END of trans # '//cnum,icw)
	   call WDIALOG(1,
     & ' or R click at START of brief event.  (F1=help).',-icw)
	endif
c	do i=1,ntrans
	do i=1,itmax
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
c	call CONV1(ycalc,y0,ntrans,dt,ke,filt,nfilt,stepamp,dtf)
	call CONV1(ycalc,y0,itmax,dt,ke,filt,nfilt,stepamp,dtf)
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
c All guesses now blue
	iact(ibk2)=11			!unless background col, when draw yellow
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
c NB returns ikey=99 if ktype=8 (except for DEL='.' which returns ikey=15)
c eg '+' key has ktype=3, ival=43 whether numlock is on or off and so
c returns ikey=11
	   if((ktype.eq.8.and.ival.eq.82).or.ikey.eq.0) ikey=10     !zero key with numlock off
c
	   if(.not.allset) then	!act on keys 1-3 while setting guesses
		if(ikey.eq.-1.or.ikey.eq.3) then
		   call POPTEXT(mxlo,mylo,myhi,helps1,nhelp1,ictx,ibkx,icfx)
		   call HIDE_MOUSE()
		else if(ikey.eq.1) then		!restart guesses
		   call SETACT(iact,-1)	!no draw
c		   iact(14)=ibk2	  !unless yellow, when draw background col
		   iact(11)=ibk2	  !unless blue, when draw background col
		   call IDCPIX(ixcalc,iycalc,ke,iact)
		   call SHOW_MOUSE()
		   allset=.false.
		   goto 30
		endif
		goto 32	!straight to mouse options
	   endif	!end of bit done before allset=true
c Next bit done after allset=true
c=	   if(ikey.eq.-1) ikey=15	!help
	   if(ikey.eq.-1) then
		call POPTEXT(mxlo,mylo,myhi,helpst,nhelp,ictx,ibkx,icfx)
	   endif
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
c		   kfix(itrans)=0		!in case already set to base, above
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
		iact(11)=ibk2	  !unless yellow, when draw background col
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
32	continue
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
		   x0=float(ix2) -xsh
		   t0=x0/cfac		!cfac=dx/finter
		   if(t0.lt.0.) t0=tmin1
		else		!itrans>1
		   t=float(ix2)/cfac - tsh
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
		if(.not.allset) then
c          Define amplitude from iy2
		   ylevel(itrans+1)=float(iy2-iyoff)/sfac3
c      and redefine stepamp(), from new ylevel()
		   do i=1,ntrans
			stepamp(i)=ylevel(i+1)-ylevel(i)		!in intermed units
			down=ylevel(i+1).lt.ylevel(i)
	      	opening(i)=(down.eqv.opendown)	!needed in scanfunc
		   enddo
c
	   	   itrans=itrans+1
		   if(itrans.gt.itmax-1) itmax=itrans-1
		endif
c
c Delete previous calc curve
		ikey=0
		goto 31
	   else if(ibutton.eq.2) then		!R button
		if(allset) then
		   ikey=11		!increment itrans
	         usekey=.true.
		   goto 31
		else			!insert short event if not allset
		   if(itrans.le.ntrans-1) then
			call SETSHORT(ix2,iy2,ylevel,t0,dt,itrans,ntrans,cfac,
     & 		  finter,npfilt,data,if1,if2,opendown,base,avamp,
     &		  timamax,tmin2)
	    		do i=1,ntrans
			   stepamp(i)=ylevel(i+1)-ylevel(i)		!in intermed units
			   down=ylevel(i+1).lt.ylevel(i)
	      	   opening(i)=(down.eqv.opendown)	!needed in scanfunc
		      enddo
	   	      itrans=itrans+2
		      if(itrans.gt.itmax-1) itmax=itrans-1
c====			if(itrans.eq.ntrans) itrans=itrans+1   !to trigger bit below
		   else
		      call POPTIME(250,130,
     &    'NOT ENOUGH TRANSITIONS LEFT TO INSERT 2 MORE',1,14,12,14,1.5)
		   endif
		endif
		goto 31
	   else if(ibutton.eq.3) then       !L AND R buttons to finish
		goto 99
c         centre button=amplitude when allset:
	   else if(nbutton.eq.3.and.ibutton.eq.4.and.allset) then
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
c Before deleting curve, check if allset yes; if now allset the
c put up new menu
	if(.not.allset) then
	   if(itrans.gt.ntrans) then
		itrans=1
		itmax=ntrans
		allset=.true.
		tsh=tshift		!for 'modsubl' mode to adjust guesses
		xsh=xshift
c         Erase prev message (by writing again in backround col, icbd, to avoid
c		problems with string length in wdialog)
   		call WDIALOG(1,
     & 'Click to mark ampl and position at END of trans # '//cnum,-icbd)
		call WDIALOG(1,
     &	 'Now adjust these guesses (F1=help)',-icw)
		call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
		call DCMENU(nbox,5,Lb,text,icol1,icol2)
c	draw special colour boxes individually
		if(ilast1.ne.0) call DCMENU(-1,5,Lb,text,9,15)	!draw box 1 only
		if(ilast.ne.0) call DCMENU(-3,5,Lb,text,4,15)	!draw box 3 only
		call DCMENU(-5,5,Lb,text,13,15)	!draw box 5 only (mean)
		call DCMENU(-7,4,Lb,text,icol3,icol4)	!draw box 4 only -shut level
	   endif
	endif
c Now delete old curve
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
	imode=0	!not yet fixed to check all at once as in FITSUB
	n=0
	sets1=.false.	!set true if ANY interval reset to shut
	do i=1,ntrans
	   k=kt+i		!index for iampl()
	   y1=ylevel(i+1)
	   call CHKSHUT(y1,base,scrit,k,i,delt,t0,dxf,dtf,
     &	  tshift,ibk2,calfac2,rms1,avamp,SHUT,sets,autoset,
     &	  opendown,reject,n,ixn,imode)
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


