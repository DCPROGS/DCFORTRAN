	subroutine DEFAMP(ntrans,base,basevga,ylevel,shut,calfac2,
     & nscale,iyoff,sfac3,ibk,iampl,iprops,ifitmode,isavbase,
     & icol1,icol2,icw,icbl,avamp,fitall,kt,iflag,refit,ntmax)
c To define amplitudes manually in SCAN (ifitmode=2-3)
c Defines single amplitude if sublevel=false
c Defines ntrans amplitudes if sublevel=true
c
c Returns amplitude(s), in intermediate units in ylevel(nt)
c Definition of ylevel() used in FITSUB:
c NB ylevel(nt)=current level (intermed units) BEFORE transition #nt
c    ylevel(nt+1)=current level (intermed units) AFTER transition #nt
c Convenient to use index one smaller here, to define initially
c i.e. in ylevel(1) if not sublevel option
c 	    ylevel(1) to ylevel(ntrans) if sublevel option
c but before return, index increased by 1 so this subroutine returns
c i.e. in ylevel(2) if not sublevel option (whether FITALL true or not)
c 	    ylevel(2) to ylevel(ntrans+1) if sublevel option (FITALL=false always)
c
c Sets iampl() and iprops() (temporarily?) here, but kt not updated
c until fit is confirmed
c
c FITTING MODES
c ifitmode=0: automatic detection/fit of all amps and durations
c ifitmode=1: automatic with only one amplitude fitted
c ifitmode=2: manual definition of amplitude; only durations fitted (prev 4)
c ifitmode=3: manual sublevel mode; only durations fitted (previously 5)
c
c Arrays for up to 50 transitions at present
	integer*2 iampl(ntmax),iyv,iyv1,iyvga
	integer*1 iprops(ntmax)	!properties of ith interval (see SCAN)
c arrays to hold graphics overwritten by line
	integer*1 isavopen(320),isavbase(320),isavy1(320),isavy2(320)
	integer*1 isavy3(320)
	real*4 ylevel(50)		!temp storage for amplitude levels (intermed)
c=	real*4 stepamp(50)
c=	logical dubious	!amplitude ill-defined (not merely short!) (='assume' in PDP)
	logical opset,fitall,sublev,shut,refit
c=	logical help			!for QDIALOG etc
	character*11 cnum
c=	character*50 helpst(6)		!help strings
c=	character defolt*30,cans*30		!to hold text & result of QDIALOG
c=	character ans
c	character getch,ch,UC
c	integer*2 ktype
C For DCMENU
	integer Lb(30)
	character*78 text(30)
c
c	common/hlp/help		!for QDIALOG etc
c
	if(iflag.eq.1) then	!returning from FIXGUESS
	   iflag=0			!reset
	   goto 151
	endif
c
c Choose options for manual fit
	fitall=.false.	!search for all transitions on screen
	opset=.false.
	sublev=ifitmode.eq.3	!sublevel mode
c
151	nbox=15
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	if(sublev) Lb(9)=0	!must specify number of transitions!
c Can do new guesses (key 12) only if refit=true, so dtsav etc already defined
	if(.not.refit) Lb(12)=0
	Lb(14)=0		!not used yet
	call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
	text(1)='1: FIT 1 TRANS'
	text(2)='2: FIT 2 TRANS'
	text(3)='3: FIT 3 TRANS'
	text(4)='4: FIT 4 TRANS'
	text(5)='5: FIT 5 TRANS'
	text(6)='6: FIT 6 TRANS'
	text(7)='7: FIT 7 TRANS'
	text(8)='8: FIT 8 TRANS'
	text(9)='9: FIT ALL'
	text(10)='10: QUEUE PLOT '	!=========move PLOT to key 10 throughout?
	text(11)='+:ADJUST BASELINE'
	text(12)='-:ALTER GUESSES'
	text(13)='x:CANCEL THIS FIT'
	text(14)='/:'
	text(15)='.:OPTIONS/HELP (F1)'
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
150	call CKEY(ch,ikey)
	if(ikey.lt.-1.or.ikey.gt.15) goto 150
	if(ikey.eq.14) goto 150       !not used yet!
	if(ikey.eq.-1) ikey=15		!F1=help
c==	if(ikey.eq.15) then
c==	   iret=1
c==	   goto 237		!help
c==	endif
156	continue
	if(ikey.eq.13) then	!cancel this fit
	   iflag=2		!signal to reject in FITSUB4
	   RETURN
	endif
	if(ikey.eq.10) then	!plot
	   call SCQPLT(idiskq,ndev,indfst,isdfst,ndisp1,idata,nbuf,
     &    novlap,srate,calfac,calfac2,ioff,cdate,adctime,mono,iesc)
	   if(iesc.ne.0) then	!plot cancelled -get another option
		call BELL(1)
		call WDIALOG(1,
     &	' Plot cancelled -choose another option',icw1)
	   endif
	   goto 150	!get another option
	endif
c
c Set number of transitions to be fitted
	if(ikey.eq.9) fitall=.true.
	if(ikey.ge.1.and.ikey.le.8) ntrans=ikey
c
	if(ikey.eq.11) then
	   call WDIALOG(1,'Now adjust baseline level',icw)
	   ibasevga=ifixr(basevga)
	   call NUMSET
	   call MOVELINE(ibasevga,icbl,isavbase,i)
	   basevga=float(ibasevga)
	   base=(basevga-float(iyoff))/sfac3	!convert to intermed units (like curlev etc)
	   if(.not.opset) then			!set number of transitions
		call BELL(1)
	      call WDIALOG(1,
     &	'Now choose number of transitions',icw)
		goto 150
	   else
		goto 160	!open level already set
	   endif
c======reset base iyoff etc as in fitsub? -done after fit accepted!
	endif
c
c FIXGUESS requires many params, so easier to call from FITSUB4 and then
c return to DEFAMP, rather than calling here
	if(ikey.eq.12) then		!new guesses for durations
	   iflag=1		!signal to call FIXGUESS and return to DEFAMP
	   goto 99
	endif
c
c If initially open and NOT sublevel mode, then open level already set
c as prevlevel in fitsub4, and next level must be shut, so no need to
c set any levels here
	if(.not.sublev.and.(.not.shut)) then
	   iampl(kt+1)=0		!set provisional amplitude
	   iprops(kt+1)=0
	   ylevel(1)=base
	   goto 160
	endif
c SET OPEN LEVEL NEXT (sep steps not needed if pots used)
c Either (1) set with cursor; (2) use last amp; (3) use last but one;
c (4) use mean full amp
c first identify 'last', 'last but one' amplitude
	ilast=0			!not yet def
c==?	if(iampsv.ne.-9999) ilast=iampsv
	ilast1=0			!not yet def
	ikt=kt
604	if(ikt.le.0) goto 603	!no value found
	itest=iampl(ikt)
	if(itest.ne.0.and.iprops(ikt).eq.0) then
	   ilast=itest		!ilast found
	   ikt=ikt-1		!go back one more
	else
	   ikt=ikt-1
	   if(ikt.le.0) goto 603	!no value found
	   goto 604				!look at prev amplitude
	endif
c look for last but one
605	if(ikt.le.0) goto 603	!no value found
	itest=iampl(ikt)
	if(itest.ne.0.and.iprops(ikt).eq.0) then
	   ilast1=itest			!last but one found
	else
	   ikt=ikt-1
	   if(ikt.le.0) goto 603	!no value found
	   goto 605				!look at next prev amp
	endif
603	continue
c
c If in sublevel mode then use loop here to identify each amplitude level
	nt1=1				!do loop once unless sublevel mode
	if(sublev) nt1=ntrans
c
	do nt=1,nt1
c Draw lines for these levels (use rdgwin1 to save underlying points)
c Use diff colours/dash patterns for each
c NB write and erase of levels can be outside loop if 'last', 'last but one'
c not updated for each level defined
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
c	   if(nt.eq.1) then
1541		nbox=15
		call SETLBOX(nbox,Lb,0)
		Lb(3)=1	!draw 3,5,10 in regular colours
		Lb(11)=1
		Lb(15)=1
		if(sublev) Lb(5)=1	!show 'shut' option
		call NUMSET			!set num lock on ready for response
		call DCMENU(0,5,Lb,text,ibk,ibk)		!delete all
		text(1)='1: LAST BUT ONE'		!dark blue   9
		text(2)='2: USE LAST AMP'		!dark red    4
		text(3)='3: SET MANUALLY'
		text(4)='4: USE MEAN AMP'		!purple     13
		text(5)='5: SHUT LEVEL'
		text(11)='+:ADJUST BASELINE'
		text(15)='.:OPTIONS/HELP'
		call DCMENU(nbox,5,Lb,text,icol1,icol2)
c	draw special colour boxes individually
		if(ilast1.ne.0) call DCMENU(-1,5,Lb,text,9,15)	!draw box 1 only
		if(ilast.ne.0) call DCMENU(-2,5,Lb,text,4,15)	!draw box 2 only
		call DCMENU(-4,5,Lb,text,13,15)	!draw box 4 only
c	   endif
c If SUBLEVEL case then, before reading key input, say which transition number
c we are up to and draw up best guess of where it is
1542	   if(sublev) then
		call BELL(1)
		call INTCONV(nt,cnum)
		call WDIALOG(1,
     &	 'Set level AFTER transition #'//cnum(1:2),icw)
	   endif
154	   call CKEY(ch,ikey)
	   if(ikey.lt.-1.or.ikey.gt.15) goto 154
	   if(ikey.eq.-1) ikey=15		!F1=help
c===============insert help
c
c Another chance to move baseline from menu above
	   if(ikey.eq.11) then
		call WDIALOG(1,
     & 'Adjust baseline with arrow keys: then hit <enter>',icw)
		ibasevga=ifixr(basevga)
		call NUMSET
		call MOVELINE(ibasevga,icbl,isavbase,i)
		basevga=float(ibasevga)
		base=(basevga-float(iyoff))/sfac3	!convert to intermed units (like curlev etc)
c -reset iyoff etc after fit accepted, in fitsub4
		goto 1542
	   endif
c	Put up new menu as soon as key hit for previous one (so next choice
c	is clear) but do not ask for key yet, until action take on key hit
c	for above menu (NB moveline altered so that any numerical key causes
c	it to be left, not only <enter>)
	   nbox=4
	   call SETLBOX(nbox,Lb,1)
	   call NUMSET			!set num lock on ready for response
	   call DCMENU(0,5,Lb,text,ibk,ibk)		!delete all
	   text(1)='1: TRY AGAIN'
	   text(2)='2: AMPLITUDE DUBIOUS'
	   text(3)='3: AMPLITUDE GOOD'
	   text(4)='4:   HELP'
	   call DCMENU(nbox,4,Lb,text,icol1,icol2)
	   ikey1=0		!unless changed by moveline
c
c Now act on ikey for 'use last amplitude' menu
c -For 'set with cursor', put up open-level cursor at vga value
c corresponding to AVAMP, and adjust it
c   ylevel=avamp+base in intermed units as used for fitting
c
	   SELECT CASE(ikey)
	    case(1)
		ylevel(nt)=float(iy1)
		iampl(kt+nt)=ifixr(ylevel(nt)-base)  	!set provisional amplitude

	    case(2)
		ylevel(nt)=float(iy)
		iampl(kt+nt)=ifixr(ylevel(nt)-base)  	!set provisional amplitude

	    case(3)		!set manually
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
		ylevel(nt)=oplev
c update 'last but one' etc (only when manual fit used -> measured amp)
		if(ilast.ne.0) ilast1=ilast
		ilast=ifixr(ylevel(nt)-base)	!update 'last'
		iampl(kt+nt)=int2(ilast)  	!set provisional amplitude

	    case(4)
		ylevel(nt)=oplev
		iampl(kt+nt)=ifixr(ylevel(nt)-base)  	!set provisional amplitude

	    case(5)		!shut
		iampl(kt+nt)=0		!set provisional amplitude
		iprops(kt+nt)=0
		ylevel(nt)=base

	   end select
c
	   if(sublev.and.ikey.ne.3) then	!draw the chosen level
		iy=ifixr(ylevel(nt))		!intermed units
		iyvga=int2(iy)		!integer*2 for scalint
		call SCALINT(iyvga,1,nscale,iyoff)	!pixel units
		call RDGWIN1(0,iyvga,639,iyvga,isavy3)  !keep data overwritten
		call HLINE(0,639,iyvga,14)
	   endif
c Now define ikey for 'amplitude good' menu
170	   continue
	   if(ikey1.ge.1.and.ikey1.le.4) then
		ikey=ikey1		!use ikey output by MOVELINE
	   else if(sublev.and.ikey.eq.5) then    !assume 'amp good' for shut level
		ikey=3		!'good'
	   else
	      call CKEY(ch,ikey)	!get key for 'amplitude good' menu
	      if(ikey.lt.-1.or.ikey.gt.4) goto 170
	   endif
c===============insert help
	   call WRGWIN1(0,iyvga,639,iyvga,isavy3)	!delete line if OK
	   if(ikey.eq.1) then		!try again
		call BELL(1)
		call WDIALOG(1,'Choose new amplitude option',icw)
		goto 1541
	   endif
	   if(ikey.eq.2) then
c		iprops(kt+nt)=1		!mark as dubious
		iprops(kt+nt)=IBSET(iprops(kt+nt),0)	!set bit 0 (='1')
	   else if(ikey.eq.3) then
		iprops(kt+nt)=0		!mark as good
	   endif
c
	   opset=.true.				!open level now set
c Open level now set
c Delete lines before returning
	   call WRGWIN1(0,iopenvga,639,iopenvga,isavopen)
	   call WRGWIN1(0,iyv1,639,iyv1,isavy2)
	   call WRGWIN1(0,iyv,639,iyv,isavy1)
	end do	!end of loop for each sublevel
160	continue
c
c Now increment index of ylevel() before returning (see above)
	if(sublev) then
	   do i=ntrans,1,-1
		ylevel(i+1)=ylevel(i)
	   enddo
	else
	   ylevel(2)=ylevel(1)
	endif
c
99	continue
	RETURN
	end


