	subroutine FIXGUESS(t0sav,y0sav,dtsav,ntrans,filt,nfilt,
     & stepamp,dtf,trise,x0,dxf,nscale,iyoff,cfac,ibk,ibk2,
     & tshift,theta,t0,dt,delt,jfix,kfix,kmax,opening,
     & opendown,tshort,iprops,kt,base,ntmax)
c To alter, manually, the initial guesses for durations of events for
c refitting when initial auto-fit is not satisfactory
c NB ntrans canNOT be altered!
c 12/28/93 03:09pm Now can alter amplitudes too
c Also now resets jfix(),kfix(),iprops(),opening() on the basis of the
c new guesses
c
c Use the initial guesses found BEFORE fit done (kept in t0sav,dtsav), because
c when fitting goes wrong can get crazy values
c
c (0) First check that there are no very short values -make all guesses
c at least 1 risetime in duration so easily visible
c
c Options:
c (1) Change durations with pots as on PDP, redrawing convolved trace
c	repeatedly
c (2) Put up box (labelled 1,2,...,ntrans) + arrow to mark guessed
c	position of each transition
c	(a) drag each box to required position, redrawing convolved
c		trace only when mouse button released
c	(b) hit number key to indicate which box (transition) to alter
c		and move box with arrow keys -redraw trace when <enter> hit
c
	integer*2 ixcalc(5120),iycalc(5120)	!up to 5120 points for convol trace
	integer*1 iprops(ntmax)	!properties of ith interval (see SCAN)
	integer jfix(50),kfix(0:50)
	integer iact(0:15),ixlast(50)
	real*4 dtsav(49),stepamp(50)
	real*4 theta(50)
	real*4 delt(0:50),dt(49)
	real*4 ycalc(5120)
	real*4 filt(640)
	character*1 ch,getch
	integer*2 ktype
	logical bigstep,usekey,mouse_on,mous_set,kbhit,first
	logical opening(50),down,opendown,altered
c	integer*1 isav(320,9)	!to save orig graphics covered by each cursor
c	integer*1 isavlin(320)
	character*55 helpst(14)		!help strings
C For DCMENU
	integer Lb(30)
	character*78 text(30)
	common/mousval/mouse_on,nbutton
	common/popvalx/mxlo,mylo,myhi,ictx,ibkx,icfx	!values for poptext calls
	common/datpos/yd1,yd2,iyd1,iyd2	!position of data area
c
c Initialise
	xshift=tshift*cfac	!tshift in pixel units
	bigstep=.false.
	usekey=.false.
	altered=.false.		!guesses altered
	first=.true.
	tstep=10.		!10 microsec steps
	astep=10.         !intermed units
	mous_set=.false.	!initialise
c Define help strings
	helpst(1)='Set position of each transition.'
	helpst(2)='Start with 1st transition.'
	helpst(3)='  Move transition with L/R arrow keys:'
	helpst(4)='  Move amplitude with up/down arrow,'
	helpst(5)='     OR set shut level with key *;  '
	helpst(6)='  INS/key 0 toggles sensitivity;'
	helpst(7)='  + key increments transition #;'
	helpst(8)='  - key decrements transition #;'
	helpst(9)='  Hit <enter> when done.'
	nhelp=9
	if(mouse_on) then
	   helpst(10)='OR'
	   helpst(11)='Mark position with left mouse button;'
	   helpst(12)=' right button increments transition #;'
	   helpst(13)=' centre button decrements transition #;'
	   helpst(14)=' Both buttons/enter when done'
	   nhelp=14
	endif
c First check for, and remove, any very short guesses (if ntrans>1)
	tmin1=0.5*trise		!OK?
	if(ntrans.gt.1) then
	   if(dtsav(1).lt.tmin1) dtsav(1)=tmin1
	   if(ntrans.gt.2) then
	      do i=2,ntrans-1
	        if(dtsav(i)-dtsav(i-1).lt.tmin1) dtsav(i)=dtsav(i-1)+tmin1
		enddo
	   endif
	endif
c Put up new menu
	icol1=14	!yellow text/border for boxes in DCMENU
	icol2=8	!grey background for boxes
	icol3=12	!red text/border
	icol4=15	!white fill
	nbox=15
	call SETLBOX(nbox,Lb,0)
	do i=1,ntrans		!up to 9 trans
	   Lb(i)=1
	enddo
	Lb(10)=1
	Lb(11)=1
	Lb(12)=1
	Lb(13)=1
	Lb(14)=1
	Lb(15)=1
	call DCMENU(0,5,Lb,text,ibk,ibk)		!delete all
	text(1)='1: 1st TRANSITION'
	text(2)='2: 2nd TRANSITION'
	text(3)='3: 3rd TRANSITION'
	text(4)='4: 4th TRANSITION'
	text(5)='5: 5th TRANSITION'
	text(6)='6: 6th TRANSITION'
	text(7)='7: 7th TRANSITION'
	text(8)='8: 8th TRANSITION'
	text(9)='9: 9th TRANSITION'
	if(bigstep) then
	   text(10)='10:MOVE SLOWER'
	else
	   text(10)='10:MOVE FASTER'
	endif
	text(11)='+:NEXT TRANSITION'
	text(12)='-:PREV TRANSITION'
	text(13)='x:SET SHUT LEVEL'
	text(14)='\:SCROLL DIALOG'
	text(15)='.:  HELP (F1)'
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
	call DCMENU(-1,5,Lb,text,icol3,icol4)	!highlight box 1 only
c Go straight on with transition #1 as active one; get characters
c with GETCH below (but numlock off, so beware!)
c	call NUMSET			!set num lock on ready for response
c152	call CKEY(ch,ikey)
	itrans=1		!current transition for adjustment
	ikey=1		!initially
3	continue		!return here with ikey from GETCH below
c	if(ikey.lt.-1.or.ikey.gt.15) goto 152	!also arrows allowed
	if(ikey.eq.-1) ikey=15		!F1=help
	if(ikey.ge.1.and.ikey.le.9) then
	   call DCMENU(-itrans,5,Lb,text,icol1,icol2)	!redraw prev itrans
	   itrans=ikey		!current transition for adjustment
	   call DCMENU(-itrans,5,Lb,text,icol3,icol4)	!highlight box itrans
	endif
c
	if(ikey.eq.10) then
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
	endif
c
	if(ikey.eq.11) then
	   call DCMENU(-itrans,5,Lb,text,icol1,icol2)	!redraw prev itrans
	   itrans=itrans+1
	   if(itrans.gt.ntrans) itrans=1
	   call DCMENU(-itrans,5,Lb,text,icol3,icol4)	!highlight box itrans
	endif

	if(ikey.eq.12) then
	   call DCMENU(-itrans,5,Lb,text,icol1,icol2)	!redraw prev itrans
	   itrans=itrans-1
	   if(itrans.lt.1) itrans=ntrans
	   call DCMENU(-itrans,5,Lb,text,icol3,icol4)	!highlight box itrans
	endif
c
c ikey=13 dealt with locally below
	if(ikey.eq.14) then	!scroll dialog
		call SCROLDIAL(1)
	endif

	if(ikey.eq.15) then	!help
	   call POPTEXT(mxlo,mylo,myhi,helpst,nhelp,ictx,ibkx,icfx)
	endif
c
c Now show these guesses on screen as calc curve
c and mark transitions with yellow lines (or white line for that currently
c being adjusted)
c===============
c -or use MOVELINE?? Put mouse in MOVELINE?? Rename as MOVELINH (horizontal)
c and make also MOVELINV (vertical) for use here
	ic=15			!colour for arrows
	if(opendown) then
	   iyt1=iyd2-26	!position for lines marking transitions
	   iyt2=iyt1+25
	else
	   iyt1=iyd1+1	!position for lines marking transitions
	   iyt2=iyt1+25
	endif
c
	do i=1,ntrans
	   if(i.eq.1) then
		t=t0sav
	   else
		t=t0sav+dtsav(i-1)
	   endif
c	   ix1=ifixr(t*dxf/dtf)
c	   ix1=ifixr(t*cfac)
	   ix1=ifixr((t+tshift)*cfac)	!tshift centralises bar over transition
c	   call WRGWIN1(ix1,380,ix1,420,isav(i))	!save graphics
c   erase previous transition marker (overwritten with background colour
c -does not restore graphics at present)
	   if(.not.first) then
	      call VLINE(ixlast(i),iyt1,iyt2,ibk2)
	   endif
	   ixlast(i)=ix1
	   ic=15
	   if(i.eq.itrans) ic=12
	   call VLINE(ix1,iyt1,iyt2,ic)
c	   call VARROW(ix1,420,380,ic)
	enddo
	first=.false.
c
	if(usekey) then
	   usekey=.false.		!reset
	   goto 2
	endif
c
1	continue	!return to recalc and redraw
c update transition marker
c  delete previous transition marker
	call VLINE(ixlast(itrans),iyt1,iyt2,ibk2)
	if(itrans.eq.1) then
	   t=t0sav
	else
	   t=t0sav+dtsav(itrans-1)
	endif
c=	ic=15
c=	if(i.eq.itrans) ic=12
	ic=12		!current transition has red marker!
	ix1=ifixr((t+tshift)*cfac)	!tshift centralises bar over transition
	ixlast(itrans)=ix1
	call VLINE(ix1,iyt1,iyt2,ic)
c calc new curve
	call CONV1(ycalc,y0sav,ntrans,DTsav,ke,filt,nfilt,stepamp,dtf)
	if(ke.gt.5120) ke=5120
	do i=1,ke
	   ixcalc(i)=ifixr(x0 + float(i-1)*dxf)
	   iycalc(i)=int2(ifixr(ycalc(i)))
	enddo
c ycalc() is in units as given by readsec, without nscale or offset -must
c scale for display
	call SCALINT(iycalc,ke,nscale,iyoff)
c Display the convolved trace for init guesses, starting at time of 1st transition
	call HIDE_MOUSE()
	call SETACT(iact,-1)		!no draw
c	iact(ibk2)=14			!unless background col, when draw yellow
	iact(ibk2)=11			!unless background col, when draw blue
	call IDCPIX(ixcalc,iycalc,ke,iact)
	call SHOW_MOUSE()
c================
c====now adjust values
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
	   if(ival.eq.13.or.ival.eq.27) goto 99
	   call SETCKEY(ktype,ival,ikey)		!in DCLIB now
	   if((ktype.eq.8.and.ival.eq.82).or.ikey.eq.0) ikey=10     !zero key with numlock off
c
	   if(ikey.eq.13) then	!set shut level following transition #itrans
		kfix(itrans)=3		!special signal, reset below
		j1=ntrans+itrans+1	!theta(j1)=amplitude after trans #itrans
		theta(j1)=base
c      and redefine stepamp()
		j1=ntrans+1
		do j=1,ntrans
		   stepamp(j)=theta(j1+1)-theta(j1)
	         down=theta(j1+1).lt.theta(j1)
	         opening(j)=(down.eqv.opendown)	!needed in scanfunc
	         j1=j1+1
		enddo
	   else if(ikey.ne.99) then
	      usekey=.true.
	      goto 3
	   endif
	   i1=itrans-1
	   if(ktype.eq.8) then	!move with ARROW keys
		sign=0.
		if(ival.eq.77.or.ival.eq.75) then 		!shift left/right
		   altered=.true.		!guesses altered
		   sign=1.			  		!shift right
		   if(ival.eq.75) sign=-1.		!shift left
		   if(i1.eq.0) then
			t0sav=t0sav+sign*tstep
			x0=t0sav*cfac		!cfac=dx/finter
		   else
			dtsav(i1)=dtsav(i1)+sign*tstep
		   endif
c Now up/down arrow to alter amplitude (if 0 initially may need to unfix it!)
		else if(ival.eq.72.or.ival.eq.80) then 		!up=72/down=80
		   altered=.true.		!guesses altered
		   kfix(itrans)=0		!in case already set to base, above
		   sign=1.			  		!shift up
		   if(ival.eq.80) sign=-1.		!shift down
		   j1=ntrans+itrans+1	!theta(j1)=amplitude after trans #itrans
		   theta(j1)=theta(j1) + sign*astep
c            and redefine stepamp()
		   j1=ntrans+1
		   do j=1,ntrans
		     stepamp(j)=theta(j1+1)-theta(j1)
		     down=theta(j1+1).lt.theta(j1)
	           opening(j)=(down.eqv.opendown)	!needed in scanfunc
		     j1=j1+1
		   enddo
		endif
	   endif		!end of ktype=8
c

	   if(ktype.eq.8.or.ikey.eq.13) then
c Delete previous calc curve
c (if last ycalc etc kept so not overwritten by new then can delete
c old immediately before drawing new)
		call HIDE_MOUSE()
		call SETACT(iact,-1)	!no draw
c		iact(14)=ibk2	  !unless yellow, when draw background col
		iact(11)=ibk2	  !unless blue, when draw background col
		call IDCPIX(ixcalc,iycalc,ke,iact)
		call SHOW_MOUSE()
		goto 1	!recalc and redraw
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
		altered=.true.		!guesses altered
		if(itrans.eq.1) then
c	        NB x0 defines where display of calc curve starts
		   x0=float(ix2) -xshift
		   t0sav=x0/cfac		!cfac=dx/finter
		   t0sav=x0/cfac
		else
		   t=float(ix2)/cfac - tshift
		   dtsav(itrans-1)=t-t0sav
		   if(itrans.eq.2) then
			if(t.le.t0sav+10.) dtsav(1)=10.
		   else
			if(dtsav(itrans-1).le.dtsav(itrans-2)+10.) then
			   dtsav(itrans-1)=dtsav(itrans-2)+10.
			endif
		   endif
		endif
c Delete previous calc curve
		call SETACT(iact,-1)	!no draw
c		iact(14)=ibk2	  !unless yellow, when draw background col
		iact(11)=ibk2	  !unless blue, when draw background col
	      call HIDE_MOUSE()
		call IDCPIX(ixcalc,iycalc,ke,iact)
		call SHOW_MOUSE()
		goto 1	!recalc and redraw
	   else if(ibutton.eq.2) then
		ikey=11		!increment itrans
	      usekey=.true.
		goto 3
	   else if(ibutton.eq.3) then       !L AND R buttons to finish
		goto 99
	   else if(nbutton.eq.3.and.ibutton.eq.4) then
		ikey=12		!centre button=decrement itrans
	      usekey=.true.
		goto 3
	   endif
	endif		!end of mouse options
c
	goto 2	!get another character
99	continue
c Before leaving, hide mouse
	if(mouse_on) call HIDE_MOUSE()
c and delete all markers
	do i=1,ntrans
	   call VLINE(ixlast(i),iyt1,iyt2,ibk2)
	enddo
c
c Finally redefine kfix() etc based on new guesses
	if(altered) then
c	    Redefine t0,dt(),delt (used to be in FITSUB)
		t0=t0sav
		do i=1,ntrans-1
		   dt(i)=dtsav(i)
		   if(i.eq.1) then
		      delt(i)=dt(i)
		   else
		      delt(i)=dt(i)-dt(i-1)
		   endif
	      enddo
c  Zero jfix,kfix before resetting them
c (but do not change kfix(0) or corresponding jfix(ntrans+1) )
c		do i=0,ntrans
		do i=1,ntrans
		   if(kfix(i).ne.3) kfix(i)=0	!don't reset if base set above
		enddo
		kfix(ntrans)=0			!last trans need never be fixed!
		do i=1,kmax
		   if(i.ne.ntrans+1) jfix(i)=0
		enddo
c
		do i=1,ntrans-1
		   if(delt(i).lt.tshort) then		!short event  -fix amplitude
c============PROBLEM 'not opening' does not imply SHUT!
		      if(opening(i).and.kfix(i).ne.3) then	!=short opening
		         kfix(i)=2  		!fixed at open level
			   jfix(ntrans+i+1)=1
c			   iprops(kt+i)=2    !fixed at mean open level (avamp)
			   iprops(kt+i)=IBSET(iprops(kt+i),1)	!set bit 1 = '2'
			else if(.not.opening(i).or.kfix(i).eq.3) then	!shutting
		         kfix(i)=1  		!fixed at baseline level
			   jfix(ntrans+i+1)=1
c			   iprops(kt+i)=2    !fixed at mean shut level (base)
			   iprops(kt+i)=IBSET(iprops(kt+i),1)	!set bit 1 = '2'
			endif
		   else
			kfix(i)=0		!reset kfix=3 when interval NOT short
		   endif
		enddo
	endif		!end of 'if altered'
	RETURN
	end

