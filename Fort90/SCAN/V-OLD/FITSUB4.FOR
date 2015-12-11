	subroutine FITSUB4(idata1,ix,dx,ndfit,infirst,base,basevga,
     & ampdone,ntrig,navtest,scrit,tf50,trise,opendown,
     & nscale,iyoff,ioff1,sfac3,dfinter,timamax,mono,open,shut,
     & nshutfit,tlast,prevlevel,shutprev,infit,ixfprev,ibk,ibk2,
     & iautosub,vard,disptran,dispderiv,dispguess,refit,
     & newguess,t0sav,y0sav,dtsav,dt,delt,iampcrit,
     & calfac1,calfac2,tint,iampl,iprops,ifitmode,isavbase,
     & iyd1,iyd2,yd1,yd2,icol1,icol2,icw,icw1,icbl,
     & avtot,navamp,avamp,kt,timsav,nfits,ifits,ibaselin,
     & ntmax,nfmax)
c
c FITSUB4=version of FITSUB for ifitmode=2,3
c i.e. manual definition of amplitude; only durations fitted, amplitudes
c all defined in advance.  Uses SCNFUNC2 for fit, with nevfix=50, so fits
c only the area covered by transitions and decides its own fit range. Thus
c no need to specify (or adjust) fit range here.
c
c If entered from FITSUB will have refit=true (when 'refit: manual' or
c 'refit: sublevel' chosen in FITSUB).  May also have newguess=true if
c init guesses were reset in FITSUB) in which case dt(), delt() are
c set already so do not search for transitions or set dt(), delt() again.
c
c FITTING MODES
c ifitmode=0: automatic detection/fit of all amps and durations
c ifitmode=1: automatic with only one amplitude fitted
c ifitmode=2: manual definition of amplitude; only durations fitted (prev 4)
c ifitmode=3: manual sublevel mode; only durations fitted (previously 5)
c
c Modif 10/13/93 09:22am so that auto-fit of a burst that contains both
c long and short openings will constrain amplitude of each short opening
c to be equal to that of the nearest long opening, rather than equal
c to AVAMP (fixamp=true when this is to be attempted).
c
c Modif 09/28/93 03:28pm so that if initially shut then input value for
c FINDTRANS is not previous BASE, but the mean of 1st 2 data values (assumes
c that when initially shut the data to be fitted will be preceded by a
c reasonable length of baseline)
c
c DEFINITIONS:
c infirst=isprev + (isec-1)*nbuf = absolute index of 1st point in idata1(),
c	needed to calc transition times.
c dfinter=real*8 version of finter
c dt(i), i=1,ntrans-1:   dt(1)=time from 1st to 2nd transition
c				 dt(2)=time from 1st to 3rd transition
c				 dt(3)=time from 1st to 4th transition etc
c delt(i), i=0,ntrans-1: delt(0)=time from prev transition to 1st transition fitted
c	     (microsec)	 delt(1)=time from 1st to 2nd transition
c				 delt(2)=time from 2nd to 3rd transition
c				 delt(3)=time from 3rd to 4th transition etc
c
c
c
c SCALING:
c RAW UNITS=ADC units read from disc
c INTERMEDIATE UNITS=units output by readsec (ADC*sfac1) (no offset)
C		e.g. ybase,base,itriglev
c PIXEL UNITS=(values in intermed units)/2**nscale+iyoff (with offset)
c		e.g. ybasevga,basevga,itrigvga
c
c TIME SCALING:
c Data is displayed vs ix(i)=ifixr(float(i-1)*dx) (in pixel units=0-639).
c  Real time between between data points=finter (microseconds).  Real time
c between points of step response function=dtf microseconds, so x interval
c between points for plotting step resp must be dxf=dx*dtf/finter
c
c
c Arrays for up to 50 transitions at present
c Declarations for temporary data files
	real*8 dfinter,ttrans,tlast
	real*8 timsav(nfmax)
	integer*2 ifits(nfmax),ibaselin(nfmax)
c
	integer*2 idata1(2048)	!for data to be fitted (and as temp file)
	real*4 data(2048)
	integer*2 ix(2048)
c	integer*2 istat(20)	!for search of dy/dt
	integer iact(0:15)
	real*4 filt1(1024)	!from INSCAN/NORMFILT
	real*4 filt(640)
	real*4 tint(ntmax)
	integer*2 iampl(ntmax)
	integer*1 iprops(ntmax)	!properties of ith interval (see SCAN)
	real*4 ylevel(50)		!temp storage for transition search results
	real*4 timamax(49)
	real*4 delt(0:50)		!temp storage for transition search results
	real*4 ycalc(2048)
	integer*2 ixcalc(2048),iycalc(2048)	!up to 2048 points for convol trace
c arrays to hold graphics overwritten by line
	integer*1 isavbase(320),isavy1(320),isavy2(320)
	integer*1 isavb1(320),isavb2(320),isavb3(320)!store pixels for fit range bar
	real*4 stepamp(50),dt(49),dtsav(49),t0sav
	logical ampdone	!true when prelim amp measurement has been done
	logical opendown,down,trigfound,mono,open,shut,shutprev
	logical opensav,opening(50),disptran,dispderiv,dispguess,downprev
	logical dubious	!amplitude ill-defined (not merely short!) (='assume' in PDP)
	logical refit,fitall,sublev,newguess,btest
	logical help			!for QDIALOG etc
	logical abort
	character*55 helpst(17)		!help strings
	logical fixamp,shutp		!for fixamp
	real*4 ypeak(50)			!for fixamp
	integer mfix(50)			!for fixamp
	character*11 cnum1(50),cnum2(50),cnum3(50),cnum0,cnum
	character*70 string		!to put result in dialog box
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character ans,ch,UC
c	character getch
c	integer*2 ktype
c for simplex
	real*4 theta(50)
	integer jfix(50),kfix(0:50)
C For DCMENU
	integer Lb(18)
	character*78 text(18)
c
	common/popvalx/mxlo,mylo,myhi,ictx,ibkx,icfx	!values for poptext calls
	common/dialval1/ictd,icfd,icbd  !colours for dialog box #1 (text,frame,back)
	common/fitblk/data,if1,if2,ntrans,neval,nevfix,itype,stepamp,y0
	common/fixblk/fixamp,tshort,tlong,ypeak,opening,shutp,mfix
	common/filtblk/filt1,nfilt1,dtf1,npfilt,finter
	common/hlp/help		!for QDIALOG etc
	common/abt/abort
	EXTERNAL SCNFUNC2
c
	sublev=ifitmode.eq.3
c
	kfix(0)=0
	do i=1,50
	 jfix(i)=0
	 kfix(i)=0
	enddo
c
	do j=1,70
	   string(j:j)=char(32)
	enddo
c
c Define time needed to centralise an arrow pointing to open or shut
c period (in CHKSHUT, or VARROW below)
	tshift=0.70*finter*float(npfilt)	!factor of 0.75 is experimental!
	t0cor=0.25*trise
c
c Define amplitudes:
c (also option to alter guesses, but only if refit=true so dtsav,t0sav, y0sav
c are already defined)
	iflag=0
156	call DEFAMP(ntrans,base,basevga,ylevel,shut,calfac2,
     & nscale,iyoff,sfac3,ibk,iampl,iprops,ifitmode,isavbase,
     & icol1,icol2,icw,icbl,avamp,fitall,kt,iflag,refit)
	if(iflag.eq.1) then	!do FIXGUESS and return to DEFAMP
	   newguess=.true.
	   cfac=dx/finter	!factor to convert x0 to t0
	   call BELL(1)
	   call WDIALOG(1,
     &'Adjust transition times with arrow keys/mouse (F1=help)'
     &    ,icw)
	   call FIXGUESS(t0sav,y0sav,dtsav,ntrans,filt,nfilt,
     &    stepamp,dtf,trise,x0,dxf,nscale,iyoff,cfac,ibk,ibk2,
     &    tshift,theta,t0,dt,delt,jfix,kfix,kmax,opening,
     &    opendown,tshort,iprops,kt,base)
cc	    Redefine t0,dt(),delt -now within FIXGUESS
c	   t0=t0sav
c	   do i=1,ntrans-1
c	      dt(i)=dtsav(i)
c	      if(i.eq.1) then
c	         delt(i)=dt(i)
c	      else
c	         delt(i)=dt(i)-dt(i-1)
c	      endif
c	   enddo
	   call WDIALOG(1,'Now choose another menu option',-icw)
c	   call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
	   goto 156	!return to defamp
	else if(iflag.eq.2) then	!Cancel fitting ='reject'
	   iflag=0
	   ikey=2
	   goto 102
	endif
c Now look for ntrans transitions (or for all in fit region, if fitall=true,
c in which case ntrans not yet defined).
c Transitions located in FINDTRAN using the fixed amplitudes returned
c in ylevel() by DEFAMP.  Value for ylevel(1), level BEFORE first transition,
c is not returned by DEFAMP, but is set below
c
	if(SHUT) then
	   curlev=base
	else
	   curlev=prevlevel
	endif
	SHUTPREV=SHUT		!save value of SHUT before 1st transition
					!=value at end of previous fitting
	open=.not.shut
c
c Complete setting of amplitudes in ylevel() (already completed in DEFAMP
c for sublevel case, apart from ylevel(1)), which is set next.
	ylevel(1)=curlev		!set level before 1st transition
	if(.not.sublev) then
	   nt1=ntrans		!from DEFAMP
	   if(fitall) nt1=49    !ntrans not defined yet
	   do i=3,nt1+1
		ylevel(i)=ylevel(i-2) 		!set alternating levels
	   enddo
	endif
c
c Calc step amplitudes in stepamp()
	do i=1,ntrans
	  stepamp(i)=ylevel(i+1)-ylevel(i)		!in intermed units
	enddo
c
	y0=ylevel(1)		!initial value
	y0sav=y0		!for FIXGUESS
c
c If init guesses have been redefined then do not want to alter then again:
c guesses for times already set
c	if(refit.and.newguess) then
	if(newguess) then
	   newguess=.false.	!reset
	   goto 160
	endif
c
c SEARCH FOR TRANSITIONS:
c Set NTM=max number of transitions to be sought in data().
c For prelim fit of single full opening (at this point have no amplitude
c yet) set ntm=2
	ntm=49
	if(.not.fitall) ntm=ntrans	!defined in DEFAMP
	if(.not.ampdone) ntm=2
c
c Now start loop to search for transitions (note: for MANUAL fit
c go round loop only ntm times (do not need to estimate, from
c curav, amplitude after last transition in this case)
c When initially open, use previous level (prevlevel) as start point for
c looking for transition
c
c If refit than if1,if2 already set so skip next bit
	if(.not.refit) then
	   if1=1		!first point to be searched
	   if2=ndfit	!=whole display
c Check that search for transitions starts after the end of prev fitting
c -if not, start search at last trans + npfilt (latter gets past last trans)
c If trace has been rolled back (without using 'goback n fittings' option)
c the value of if1 here may be greater than ndfit!.  Need some criterion
c for the number of points that must be left for it to be worth trying to fit-
c say minimum of 0.2*ndfit
c=============better return to prev fit, rather than just reject here?!
	   inf1=infirst+if1-1	!abs index of data(if1)
	   if(inf1.lt.infit) then
		if1=if1+(infit-inf1)+npfilt
		if(if1.gt.0.8*ndfit) then
		  call BELL(1)
c		  call POPTIME(250,130,'TOO FEW POINTS AFTER PREVIOUS FIT',
c     &		1,15,12,14,2.0)
		  helpst(1)='ALREADY FITTED PAST HERE'
		  helpst(2)='Attempt to fit rejected-   '
		  helpst(3)='Suggest using GO TO . .option'
		  helpst(4)='to ''go back to last fitting'''
	   	  call POPTEXT(mxlo,mylo,myhi,helpst,4,ictx,ibkx,icfx)
		  ikey=2		!reject
		  goto 102
		endif
	   endif
	endif
	if(if1.lt.1) if1=1
	if(if2.gt.ndfit) if2=ndfit
c
	nt=0			!counts # of transitions found
	inext=if1		!start search at beginning of fitted region
	trigfound=.true.	!so doesn't jump out at next line
c
c==	do while (nt.le.ntm.and.trigfound)
	do while (nt.lt.ntm.and.trigfound)
c	  NB Findtrans uses value of shut as input ONLY; does not set it
	   if(nt.gt.0) itriglst=itrig
c Want to avoid trying to fit any incomplete transition close to right
c edge of screen; previously search went only up to ndfit-npfilt=if2, but
c need to exclude any such incomplete transition from fit range too, but
c also want to use points right up to last for fitting if there is no
c transition too close to end.  Therefore search right up to point #ndfit,
c but correct if1,if2 below if a transition is found too close to right
c edge of screen.
c==	   iflast=if2-npfilt
	   curlev=ylevel(nt+1)		!pre-set amplitudes
	   downprev=down
	   call FINDTRAN(data,inext,if2,curlev,scrit,ntrig,
     &	navtest,shut,opendown,itrig,curav,nav,down,trigfound)
c
c Now ylevel(nt)=current level (intermed units) BEFORE transition found on
c previous cycle, and curav=amplitude before transition found on this cycle,
c ie AFTER trans found on previous cycle, so if these similar then transition
c found on previous cycle was probably actually a double transition, so check
c this now, before processing the transition just found. Update inextprv
c only after this has been done.
c Also define kfix(0,..ntrans) here, for use in fixing params (via jfix()) later
c Note that interval #k refers to interval AFTER transition #k
c MANUAL FITS MUST NOT INSERT EXTRA TRANSITIONS UNLESS FITALL=TRUE -WANT
c TO GET THE NUMBER OF TRANSITIONS THAT WERE REQUESTED!
	   if(nt.gt.0.and.fitall.and.ampdone) then	!nt>0 so ylevel defined
c	    if(abs(curav-ylevel(nt)).lt.scrit) then	!'same' amp before & after trans
	    if(abs(curav-ylevel(nt)).lt.float(iampcrit)) then
c		'same' amp before & after trans so call misstrig
		call MISSTRIG(data,nt,ylevel,kfix,delt,timamax,opening,
     &     base,avamp,inextprv,inext,curav,open,shut,opensav,downprev,
     &	tprev,shutprev,tmin)
	    endif
	   endif
c
	   inextprv=inext
c
c Now process the transition found on this cycle
c At this point we define the transition located by FINDTRANS (even if an
c extra transition has been inserted above when short gap/op located), and
c DOWN still has the value output by FINDTRANS.
	   if(trigfound) then
		nt=nt+1
c===		ylevel(nt)=curav	!level before trigger -predefined here!
c For each transition, record whether it is an opening or shutting
	      opening(nt)=(down.eqv.opendown)	!nt not yet updated
c===		open=abs(curav-base).gt.scrit		!open before trigger
		if(nt.eq.1) then
		   it1=itrig	!'time' of transition #nt
		   t0=float(it1-1)*finter   !=0 when it1=1 ie first point on display
		   tprev=t0
		else
		   t=float(itrig-1)*finter  !=0 when itrig=1 ie first point on display
		   delt(nt-1)=t-tprev
		   tprev=t
		endif
c -set inext, and mark trigger points
	      inext=itrig+npfilt	!leave time for filter to complete
		if(disptran) then
		   call VLINE(ix(itrig),235,450,11) !mark transition light blue
		   call VLINE(ix(inext),235,450,13)	!mark inext purple
		endif
c Decide whether open AFTER trigger, ready for next FINDTRANS.
c If shut before trigger then MUST be open afterwards, but if open
c before trigger not so easy to tell whether shut after it: can go
c only on curnew value which is mean of only two points -would be easier
c to wait until after next FINDTRANS to get better value, but FINDTRANS needs
c to know whether open or shut initially -well needs to know if SHUT (only to
c prevent transitions in wrong direction) but OK to enter with 'not known'
c in which case transitions in either direction allowed
		opensav=open			!open BEFORE trigger
		if(sublev) then
		   open=iampl(kt+nt).ne.0	!iampl set in DEFAMP
		else
		   open=.not.open			!open/shut alternate
		endif
		SHUT=.not.open		!shut AFTER current transition
c
c=========== -check missev, newtran are OK for manual fit
c
c SPECIAL CASE WHEN TWO TRANSITIONS IN SAME DIRECTION ARE 'CLOSE'
c   When Findtrans finds two consecutive openings then free fit
c will either (a) insert an extra shutting (above) if level is 'same' before
c and after the transition, OR (b) fit a sublevel (if level is not 'same').
c In former case opening(nt-1) and opening(nt) will be different at this
c point so next bit will be skipped. But in latter case, if transitions
c are 'close' together there may well actually be an
c 'short opening-short gap-long(ish) opening' rather than a sublevel.
c Thus, if iautosub=1 ('avoid sublevels if possible') look again using
c first derivative method for transitions
c Define here the time (tclose, microsec) below which transitions
c are deemed to be 'close'
c MANUAL FITS MUST NOT INSERT EXTRA TRANSITIONS (UNLESS FITALL=TRUE) -WANT
c TO GET THE NUMBER OF TRANSITIONS THAT WERE REQUESTED!
		tclose=4.0*trise
		if(fitall.and.nt.gt.1.and.(opening(nt-1).eqv.opening(nt))
     &		.and.delt(nt-1).lt.tclose.and.iautosub.eq.1) then
		   call MISSEV(data,nt,ix,dx,ibk2,vard,dispderiv,ylevel,
     & 		itriglst,kfix,delt,opening,base,avamp,finter,
     &		inext,t0,tprev,shutprev)
		endif
c
c SPECIAL CASE WHEN TWO TRANSITIONS IN OPPOSITE DIRECTIONS ARE 'CLOSE'
c   Short openings or shuttings for which 2nd trigger 'only just'
c found have estimated durations that are rather too long -fix now
c by re-estimating from max amplitude (as done above when 2nd trans
c missed altogether) -do it only of max amp is substantially less
c than avamp!  If max excursion is clearly below avamp, then
c (a) use it to revise guess of duration, and (b) FIX the amplitude
c (to zero or avamp).  This ONLY changes initial guess for duration
c of an event: does not insert new transitions
c (should these be in 'if trigfound' loop?)
c Use 3*trise as criterion to try this method?; but use max excursion (and
c fix amp), only if duration estimated here is < 2*trise
		if(nt.gt.1.and.(opening(nt-1).neqv.opening(nt)).and.
     &		   delt(nt-1).lt.2.*trise) then
		   call NEWLEN(data,nt,itriglst,inext,avamp,base,trise,
     & 	     tprev,delt,ylevel,timamax,kfix,down,opendown,shutprev)
		endif
c
	   endif	!end of 'if trigfound'
	enddo		!end of 'do while' loop
c
	if(nt.eq.0) then
	   call BELL(1)
	   call POPTIME(250,130,
     & 'NO TRANSITIONS FOUND -TRY AGAIN',1,14,12,14,2.0)
	   ikey=2		!reject
	   goto 102
	endif
c
c ALL TRANSITIONS ON DISPLAY NOW FOUND.  If SHUT (ie last trans was to shut
c state so shut at RHS of display) then avoid fitting too many shut points
c by limiting the fitted region to NSHUTFIT (eg 30) shut points after last
c transition.  Mark the fitted region and give chance to adjust it if req.
c Last transition was completed at point #inext
c At this point SHUT refers to state AFTER the current FITTING
c At this point SHUTPREV refers to state BEFORE the current FITTING
	if(refit) then
	   inf1=infirst+if1-1	!abs index of data(if1)
	else
c       Fix for case where last trigger is near to right hand edge
	   if(ndfit-itrig.lt.npfilt) then
		if(if2.gt.ndfit-npfilt) then
		  if2=ndfit-npfilt    !end fit range before last trigger point
		  ntrans=ntrans-1	    !omit last transition
		  if(ntrans.eq.0) then
		     call BELL(1)
		     call POPTIME(250,130,
     &	      'NO TRANSITIONS FOUND -TRY AGAIN',1,14,12,14,2.0)
		     ikey=2		!reject
		     goto 102
		   endif
		endif
	   endif
	   if(SHUTPREV.and.(it1-if1).gt.nshutfit) then  !reset start of fitted region
		if1=it1-nshutfit
	   endif
c Check that revised first fitted point is still after the end of prev fitting
	   inf1=infirst+if1-1	!abs index of data(if1)
	   if(inf1.lt.infit) then
		if1=if1+(infit-inf1)+npfilt
	   endif
	   if(if1.lt.1) if1=1
	   if(SHUT.and.(if2-inext).gt.nshutfit) then	!reset end of fitted region
		if2=inext+nshutfit
	   endif
	   if(if2.gt.ndfit) if2=ndfit
	   if(if2.lt.if1) if2=if1+npfilt
	endif
c
c===	ntrans=nt	!already defined in DEFAMP
c
c Define time intervals for CONV in DT()
	do i=1,ntrans-1
	   if(i.eq.1) then
		dt(i)=delt(i)
	   else
		dt(i)=dt(i-1) + delt(i)
	   endif
	   dtsav(i)=dt(i)		!save in guesses need to be altered
	enddo
c
160	continue   !jump here if guesses for interval set manually (newguess=true)
c Interpolate the step resp function at suitable intervals for this display
	call NEWFILT(filt1,nfilt1,dtf1,ntrans,ndfit,finter,
     & filt,nfilt,dtf)
c
c====debug -insert errors in guesses
c	y0=y0+10./sfac3
cc	it1=it1-3
c	t0=t0-150.
c	dt(1)=dt(1)+400.
c	stepamp(1)=stepamp(1)-25./sfac3
c	stepamp(2)=stepamp(2)+35./sfac3
c====end debug
c
c Calculate x axis for convolved trace
c Data is displayed vs ix(i)=ifixr(float(i-1)*dx) (in pixel units=0-639).
c  Real time between between data points=finter (microseconds).  Real time
c between points of step response function=dtf microseconds, so x interval
c between points for plotting step resp must be dxf=dx*dtf/finter
c ie dxf/dx=dtf/finter
c The first transition is at idata1(it1), and this is at real time
c (it1-1)*finter from 1st data point on display.  It is plotted at
c x=(it1-1)*dx =(it1-1)*dxf*finter/dtf.  Convolved trace has
c x axis that corresponds to real time = (i-1)*dxf
	dxf=dx*dtf/finter
	x0=float(it1-1)*dx	!'time' in x units for 1st transition
c tf50=time (mus) to 50% point on filter. Convert this to x units:
c dxf x units=dtf microsec so dxf/dtf x units/mus
c (subtracting tf50 is a bit crude -better find point where data is half
c way up and align this with 50% point of step-resp function)
c	x50=tf50*dxf/dtf
c	x0=x0-x50
c	t0=t0-tf50	!NB VERY IMPORTANT TO CORRECT INIT GUESS FOR T0 TOO
c add empirical shift t0cor (defined above)
	xcor=(tf50-t0cor)*dxf/dtf
	x0=x0-xcor
	t0=t0-tf50+t0cor	!NB VERY IMPORTANT TO CORRECT INIT GUESS FOR T0 TOO
	t0sav=t0		!keep in cases init guesses need to be altered
c (without above correction init guess will not be as seen on screen!)
c
c NOW DO LS FIT
c Parameters:
c Try to make param not highly correlated by using transition times
c all measured from t=0 in theta, rather than using diffs
c theta(1)= t0 = time in microsec from 1st point on display TO 1st transition
c theta(2)= time in microsec from 1st point on display TO 2nd transition
c . . .
c theta(nt)= time in microsec from 1st point on display TO (nt)th transition
c FOR MANUAL and SUBLEVEL ALL AMPLITUDES ARE FIXED IN ADVANCE -NONE ARE
c ESTIMATED SO ONLY nt PARAMETERS
c
c Return here to refit (with smae ifitmode=2,3, and same amps) after
c altering baseline or after altering guesses for times.
100	continue
c Initial guesses: time params (none fixed)
	theta(1)=t0
	if(ntrans.gt.1) then
	   do i=2,ntrans
		theta(i)=t0 + dt(i-1)
	   enddo
	endif
c NO AMPLITUDE PARAMETERS!
	kmax=ntrans		!total # of parameters (inc fixed ones)
c
c======= WHOLE FIXAMP SECTION REMOVED -ALL OPENINGS HAVE PRESET AMPS
c
c DISPLAY INITIAL GUESS
c Calculate convolution in ycalc(1) to ycalc(ke)
	if(dispguess) then
	   call CONV1(ycalc,y0,ntrans,DT,ke,filt,nfilt,stepamp,dtf)
	   if(ke.gt.2048) ke=2048
	   do i=1,ke
		ixcalc(i)=ifixr(x0 + float(i-1)*dxf)
		iycalc(i)=int2(ifixr(ycalc(i)))
	   enddo
c ycalc() is in units as given by readsec, without nscale or offset -must
c scale for display
	   call SCALINT(iycalc,ke,nscale,iyoff)
c Display the convolved trace, starting at time of 1st transition
	   call SETACT(iact,-1)		!no draw
	   iact(ibk2)=14			!unless background col, when draw yellow
	   call IDCPIX(ixcalc,iycalc,ke,iact)
	endif
c
c
c Prepare for SIMPLEX
	nevfix=50	!use fixed data set after 50 func eval
c	nevfix=-1	!fixed data set
c
c	errfac=1.e-3
	errfac=0.01		!put errfac in defaults?
	delmin=-1.		!do not use delmin for convergence
	stpfac=0.1		!0.02 and 0.3 both worse in one test
c	nfix=0		!no param fixed
c standard value of confac (=contraction factor in simplex) is 0.5, but
c crude tests show 0.2 better when guess is good, and little worse
c when guess is bad -make it a default parameter?
	confac=0.25
	ndisp=0	!no iterations on disc
c	ndisp=20	!every 20th on disc (if discprt=true)
c	ndisp=1
	nevmax=5000
	neval=0		!for common in scanfunc (use neval1 for simplex call)
c	print 523,errfac
c523	format(' ERROR=guess*',g13.6,/,
c     & ' Print every Nth estimate [20] :  N = ')
c	call INPUTi(ndisp)
	call WDIALOG(1,'Fitting now (abort with F2). . .',-icw1)
	call SIMPLEX1(kmax,theta,stpfac,errfac,neval1,nevmax,
     & smin,SCNFUNC2,Ndisp,jfix,delmin,confac,irestrt,iconv)
	if(abort) then
	   call BELL(1)
	   call POPTIME(250,130,'FITTING ABORTED',1,14,12,14,2.0)
	endif
c
c FIT DONE -DISPLAY THE RESULT
c   Mark region actually fitted (nevfix=50 so if1, if2 fixed in SCNFUNC2)
	call FITBAR(ix,dx,if1,if2,ndfit,yd1,yd2,basevga,
     & opendown,.false.,isavb1,isavb2,isavb3)	!draw only
c Decode final fitted params
c Do some calcs now, so values can be calc before accepting:
c NB: t0 is time of transition from 1st point in display,idata1(1)
c which corresponds to actual data point #infirst (abs index in whole CONSAM)
c so time from 1st data point in CONSAM is ttrans=t0 + finter*float(infirst-1)
c NB 30 minute record=1800 sec = 1.8x10**9 microsec -too big for single prec if
c aim was to calc all intervals from differences in such values -but used
c only to record abs time of last transition, so when next transition fitted
c the interval preceding it can be calculated -diff should never be less than
c 100 microsec, but even this accuracy implies 1 in 1.8x10**7 -still too big
c so better use real*8 for ttrans, tlast
	t0=theta(1)
	ttrans=dble(t0) + dfinter*dble(float(infirst-1))
c Next calc the interval completed by the 1st transition (unless
c it is 1st in the record, or TINT has already been set unusable=-1000.)
	told=sngl(ttrans-tlast)		!in microsec
	tltemp=ttrans		!provisional value (until accepted) for new TLAST
	if(ntrans.gt.1) then
	   do i=2,ntrans
		j=i-1  			!j=1,...,ntrans-1
		dt(j)=theta(i) - t0
		if(j.eq.1) then
		   delt(j)=dt(j)		!delt(j)=provisional tint() values
		else
		   delt(j)=dt(j)-dt(j-1)
		endif
	   enddo
	   tltemp=ttrans + dble(dt(ntrans-1))	!provisional TLAST value
	endif
c set delt(0) and test for neg intervals
	negflag=0
c	if(kt.gt.0.and.(tint(kt).gt.-998.)) then
	if(kt.gt.0.and.(.not.BTEST(iprops(kt),3))) then !bit 3='8' set for unusable
	   delt(0)=told         !time from prev trans to 1st fitted
	   do i=0,ntrans-1
		if(delt(i).lt.5.) negflag=1
	   enddo
	endif
c======= STEPAMP NOT CHANGED BY FIT
c	j1=ntrans+1
c	y0=theta(j1)		!initial level before trans #1
c	y1=theta(2*ntrans+1)	!final level after transition #ntrans
c	do j=1,ntrans
c	  stepamp(j)=theta(j1+1)-theta(j1)
c	  j1=j1+1
c	enddo
c
c Display fit BEFORE checking shut levels
c  First erase init guess (if drawn)
	if(dispguess) then
	   call SETACT(iact,-1)		!no draw
	   iact(14)=ibk2			!unless yellow, when draw background col
	   call IDCPIX(ixcalc,iycalc,ke,iact)
	endif
c Calculate convolution in ycalc(1) to ycalc(ke)
	call CONV1(ycalc,y0,ntrans,DT,ke,filt,nfilt,stepamp,dtf)
	x0=t0*dxf/dtf
c modify next bit so fitted curve in xcalc, ycalc includes the flat bit
c at either end (makes it easier to delete calc curve!)
	if(ke.gt.2048) ke=2048
	do i=1,ke
	   ixcalc(i)=ifixr(x0 + float(i-1)*dxf)
	   iycalc(i)=int2(ifixr(ycalc(i)))
	enddo
c ycalc() is in units as given by readsec, without nscale or offset -must
c scale for display
	icc=12	!red for fitted trace
	if(mono) icc=15
	call SCALINT(iycalc,ke,nscale,iyoff)
c	call SETACT(iact,icc)		!draw red
	call SETACT(iact,-1)		!no draw
	iact(ibk2)=icc			!unless background col, when draw red
c Now extend calc curve to cover whole fitted range from data(if1) to
c data(if2).  These regions are flat, so can draw with HLINE
c Use RDGWIN1 to keep graphical data overwritten by HLINE to allow restoration
c when red fitted line is deleted
	call RDGWIN1(ix(if1),iycalc(1),ixcalc(1),iycalc(1),isavy1)
	call HLINE(ix(if1),ixcalc(1),iycalc(1),icc)	!before start of resp func
	call RDGWIN1(ixcalc(ke),iycalc(ke),ix(if2),iycalc(ke),isavy2)
	call HLINE(ixcalc(ke),ix(if2),iycalc(ke),icc)   !after end of resp func
	call IDCPIX(ixcalc,iycalc,ke,iact)			!draw resp function
c
c
c PRINT RESULTS OF FIT
c  -write (up to) 3 values of kt, time, amp per line in dialog box
c	if(.not.ampdone) goto 20
	if(.not.ampdone) then
	   avamp=stepamp(1)  !temp value for printing initial 'full amp' in SCAN
	   goto 999
	endif
c(if dialog box 2 is in data area must call OPENDIALOG each time or border lost)
c	call OPENDIALOG(2,7,.true.)	!draw box 2, for 'open'/'shut'
c	if(shut) then			!display final level
c	   call WDIALOG(2,'SHUT',11)
c	else
c	   call WDIALOG(2,'OPEN',12)
c	endif
	call OPENBOX(1,7,.true.)	!draw box 2, for 'open'/'shut'
	if(shut) then			!display final level
	     call WRITBOX(1,'SHUT',1,11)
	else
	   call WRITBOX(1,'OPEN',1,12)
	endif
c
	i=0
	k1=kt+ntrans		!new kt (after accepted)
	nst=0				!number of sets of values concat into string
	do k=kt,k1
	 i=i+1	!=1,2,..,ntrans+1
	 if(k.gt.0) then
         call INTCONV(k,cnum1(i))	!transition # (if accepted)
	   if(k.lt.k1) then
		t=delt(i-1)*1.e-3		!duration (ms)
		if(t.lt.1000.) then
	         call DCFORMAT(t,7,3,cnum2(i))		!duration
		else if((t.ge.1000.).and.(t.lt.10000.)) then
	         call DCFORMAT(t,7,2,cnum2(i))		!duration
		else
	         call DCFORMAT(t,7,1,cnum2(i))		!duration
		endif
	   else		!k=kt+ntrans
	      cnum2(i)=' ?        '
	   endif
c===	   call DCFORMAT(amp(i)*calfac2,6,2,cnum3(i))	!amplitude
	   if(iampl(k).eq.0) then
		cnum3(i)=' 0         '              !amplitude
	   else
	      a=calfac2*float(iampl(k))
	      call DCFORMAT(a,6,2,cnum3(i))		!amplitude
	   endif
	   n0=NBLANK(string)
	   n1=NBLANK(cnum1(i))
	   n2=NBLANK(cnum2(i))
	   n3=NBLANK(cnum3(i))
	   if(n0.eq.0) then
	      string=cnum1(i)(1:n1)//' '//cnum2(i)(1:n2)//'ms'//
     &	  ' '//cnum3(i)(1:n3)//'pA'
		nst=1		!number of sets of values concat into string
	   else
	      string=string(1:n0)//'; '//cnum1(i)(1:n1)//' '//
     &	  cnum2(i)(1:n2)//'ms'//' '//cnum3(i)(1:n3)//'pA'
		nst=nst+1	!number of sets of values concat into string
	   endif
c	   dubious=iprops(k).eq.1
	   dubious=BTEST(iprops(k),0)		!bit 0 set (='1')
	   if(dubious) then
	      n0=NBLANK(string)
		string=string(1:n0)//'*'	!asterisk for dubious amplitude
	   endif
c Indicate fixed values (kfix(0) part applies to where 1st level is an
c opening that is fixed at level of last open level in prev fit; iprops not
c set in this case)
c	   if(iprops(k).eq.2.or.
	   if(BTEST(iprops(k),1).or.			!bit 1 set ='2'
     &	(k.eq.kt.and.kfix(0).ne.0)) then
	      n0=NBLANK(string)
		string=string(1:n0)//char(240)   !equiv sign for fixed amplitude
	   endif
	   if(k.eq.k1.or.nst.eq.3) then	!write line to dialog box now
	      call WDIALOG(1,string,icw)
	      do j=1,70
	   	   string(j:j)=char(32)	!blank ready for next
		enddo
	   endif
	 endif
	enddo
c
c Reject fit with neg values AFTER values printed in dialog box
	if(negflag.ne.0) then
	   call BELL(1)
	   call POPTIME(250,130,'NEGATIVE INTERVAL: FIT REJECTED',
     &    1,15,12,14,2.0)
	   negflag=0
	   ikey=2		!reject
	   goto 102
	endif
c
c ACCEPT FIT?  If not, what next?
151	continue
	nbox=15
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	Lb(1)=0
	call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
	Lb(8)=0
	Lb(13)=0
	text(1)='1:'
	text(2)='2: REJECT FIT  '
	text(3)='3: ACCEPT FIT'
c	text(4)='4:REFIT: ONE AMP'
c	text(5)='5:REFIT: MANUAL'
c	text(6)='6:REFIT: SUBLEVEL'
c	text(7)='7:REFIT: AUTO'
	text(4)='4:REFIT: AUTO'
	text(5)='5:REFIT: ONE AMP'
	text(6)='6:REFIT: MANUAL'
	text(7)='7:REFIT: SUBLEVEL'
	text(8)='8:'
	text(9)='9:SET GAP UNUSABLE'
	text(10)='10: QUEUE PLOT '
	text(11)='+:ADJUST BASELINE'
	text(12)='-:ALTER GUESSES'
	text(13)='*: '
	text(14)='/:SCROLL DIALOG'
	text(15)='.:  HELP (F1)'
c Use HELP here only to explain menu options: put SCROLL DIALOG in above
c menu, and do 'new param' and 'new rms noise' only from roll-mode
c or expmode menus (not while waiting for accept/reject/refit option)
c=======other options needed: RESET BASELINE (under options/help?)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
152	call CKEY(ch,ikey)
	if(ikey.lt.-1.or.ikey.gt.15) goto 152
	if(ikey.eq.-1) ikey=15		!F1=help
c==	if(ikey.eq.-1.or.ikey.eq.15) then	!F1 key
c==	   iret=1
c==	   goto 237		!help
c==	endif
155	continue
c
c Reject fit when ikey=2, or any REFIT option chosen
c All REFIT options return to SCAN and data redrawn completely, so
c no need to delete fit here!
	if(ikey.eq.2.or.(ikey.ge.4.and.ikey.le.7)) then
	   SHUT=shutprev	!restore value
c NB do not need prevsav! -assign new prevlevel ONLY when fit accepted below
c	   prevlevel=prevsav	!restore prevlevel now fit rejected
	   do k=kt+1,kt+ntrans
		iampl(k)=0	!reset iampl() to zero
		iprops(k)=0	!reset iprops() to zero
	   enddo
c	   if(ikey.ne.2) then		!delete present fit if refit to be done
c		call SETACT(iact,-1)		!no draw
c		iact(icc)=ibk2			!unless red, when draw background col
c		call IDCPIX(ixcalc,iycalc,ke,iact)
cc         and restore data overwritten by HLINE before and after response func
c		call WRGWIN1(ix(if1),iycalc(1),ixcalc(1),iycalc(1),isavy1)
c		call WRGWIN1(ixcalc(ke),iycalc(ke),ix(if2),iycalc(ke),isavy2)
c	   endif
	endif
c
102	continue		!jump here with ikey=2 to reject if neg interval found
	select case(ikey)
	   case(1)
		goto 152	!get another key

	   case(2)		!REJECT fit
		refit=.false.	!reset
		newguess=.false.	!reset
		goto 999		!continue without updating kt, tint etc

	   case(3)		!ACCEPT fit
c 		This bit allocates TINT(kt) to TINT(kt+ntrans-1)- the next value
c		has defined amp (so printed above) but not yet defined length
c		Define tint(kt) (finishing prev interval when poss) to
c		tint(kt+ntrans-1). Record the interval completed by the 1st trans
c		unless it is 1st in record or tint already set unusable=-1000.
c		Prob better NOT to skip if already set unusable- may want to go back
c		over an unusable bit and use it! Set UNUSABLE retrospectively only
c=== THAT IAMPL(KT+1) TO IAMP(KT+NTRANS) ALREADY ASSIGNED ABOVE.
		refit=.false.	!reset
		newguess=.false.	!reset
c Now accept provisional tint,iampl,tlast=tltemp etc
c If OPEN at end of fitting, save level for use in next fitting (save anyway)
		prevlevel=ylevel(ntrans+1)
		tlast=tltemp
c update number of fits, timsav etc
		nfits=nfits+1
		timsav(nfits)=ttrans
		ifits(nfits)=kt+1		!points to element of tint(), iampl()
		ibaselin(nfits)=ifixr(base)
c update tint()
		if(kt.gt.0) then
		   tint(kt)=delt(0)*1.e-3	!prev interval completed
		endif
		if(ntrans.eq.1) then
		   kt=kt+1		!update kt
		else if(ntrans.gt.1) then
		 do i=1,ntrans
		   kt=kt+1		!update kt
		   tint(kt)=delt(i)*1.e-3	!in msec
		 enddo
		endif
c NOW re-adjust base,basevga, iyoff etc? NB basevga= position on screen that
c data is to have baseline adjusted to, so don't alter this
		ybasevga=base*sfac3 + float(iyoff)
		ioff1=ifixr(basevga-ybasevga)		!offset to adjust baseline to bdisp
		iyoff=iyoff+ioff1		!new offset for next display
c  Now accepted, update the 'fitted up to here' marker
c Note that infirst=isprev + (isec-1)*nbuf = absolute index of 1st point
c in idata1(), so abs index of last trigger point fitted is infit=infirst+itrig
		infit=infirst+itrig
		call MARK(infit,infst,ndfit,ixfprev,ix,14,ibk)
c		if(ixfprev.ge.0) then
c		   call VLINE(ixfprev,150,450,ibk)	 !-erase
c		endif
c		call VLINE(ix(itrig),150,450,14) !-mark last trigger point
c		ixfprev=ix(itrig)   !x value where last trigger drawn (to erase it)
c
	   case(4)			!refit auto
		ifitmode=0
		refit=.true.
		goto 999	!skip average amplitude bit

	   case(5)			!auto -one amp (first reject fit just done)
		ifitmode=1
		refit=.true.
		goto 999	!skip average amplitude bit

	   case(6)			!refit manual
		ifitmode=2
		refit=.true.
		goto 999	!skip average amplitude bit

	   case(7)			!MANUAL SUBLEVEL MODE (first reject fit just done)
		ifitmode=3
		refit=.true.
		goto 999	!skip average amplitude bit

	   case(8)
		goto 152	!get another key

	   case(9)		!set unusable
51		k1=kt-8	!display tint(kt-8) to tint(kt)
		call SELDATA(k1,kt,tint,iampl,calfac2,kval,ntmax)
		if(kval.ne.-1) then		!not 'cancel'
		   if(iampl(kval).ne.0) then	!skip if 'cancel'
		     call BELL(2)
		     call WDIALOG(1,
     &	    'Only shuttings can be set unusable -try again',-icw1)
		     goto 51
		   endif
		   iprops(kval)=IBSET(iprops(kval),3)   !set bit 3='8'
		endif
		call BELL(1)
		call WDIALOG(1,
     &	 'Now accept/reject/refit',-icw1)
		goto 152	!get another option

	   case(10)		!plot
c           calfac=pA/ADC (ymin,ymax not needed in AUTPLOT?)
		call SCQPLT(idiskq,ndev,indfst,isdfst,ndisp1,idata,nbuf,
     &      novlap,srate,calfac,calfac2,ioff,cdate,adctime,mono,iesc)
		if(iesc.ne.0) then	!plot cancelled -get another option
		   call BELL(1)
		   call WDIALOG(1,
     &		'Plot cancelled -choose another option',-icw1)
		endif
		goto 152	!get another option

	   case(11)       !adjust baseline
	      call WDIALOG(1,'Now adjust baseline level',icw)
	      ibasevga=ifixr(basevga)
c After changing baseline OR guesses for times, do refit straight away
c (with same ifitmode=3 or 4 as before), using same amplitudes and number
c of transitions as before. First redraw data (as in SCAN)
		call FILLWIN(0,iyd1,639,iyd2,ibk2)	!clear data area
	      call SETACT(iact,15)		!draw white whatever
c	      call SETACT(iact,-1)		!no draw
c	      iact(ibk2)=15			!unless background col, when draw white
		call IDCPIX(ix,idata1,ndfit,iact)
		call RDGWIN1(0,ibasevga,639,ibasevga,isavbase)  !keep data overwritten
		call HLINE(0,639,ibasevga,icbl)	   !show baseline
	      call NUMSET
	      call MOVELINE(ibasevga,icbl,isavbase,i)
	      basevga=float(ibasevga)
	      base=(basevga-float(iyoff))/sfac3	!convert to intermed units (like curlev etc)
c======reset base iyoff etc as in fitsub? -done after fit accepted!
		goto 100
c		call BELL(1)
c	      call WDIALOG(1,
c     &       'Next choose another menu option',-icw)
c		goto 152	!get another option

	   case(12)		!new guesses for durations
		newguess=.true.
c After changing baseline OR guesses for times, do refit straight away
c (with same ifitmode=3 or 4 as before), using same amplitudes and number
c of transitions as before. First redraw data (as in SCAN)
		call FILLWIN(0,iyd1,639,iyd2,ibk2)	!clear data area
	      call SETACT(iact,15)		!draw white whatever
c	      call SETACT(iact,-1)		!no draw
c	      iact(ibk2)=15			!unless background col, when draw white
		call IDCPIX(ix,idata1,ndfit,iact)
	      ibasevga=ifixr(basevga)
		call RDGWIN1(0,ibasevga,639,ibasevga,isavbase)  !keep data overwritten
		call HLINE(0,639,ibasevga,icbl)	   !show baseline
c
		cfac=dx/finter	!factor to convert x0 to t0
		call BELL(1)
		call WDIALOG(1,
     &'Adjust guesses for transitions with arrow keys/mouse (F1=help)'
     &      ,icw)
		call FIXGUESS(t0sav,y0sav,dtsav,ntrans,filt,nfilt,
     & 	 stepamp,dtf,trise,x0,dxf,nscale,iyoff,cfac,ibk,ibk2,
     &       tshift,theta,t0,dt,delt,jfix,kfix,kmax,opening,
     &       opendown,tshort,iprops,kt,base)
		goto 100
c		call WDIALOG(1,'Now choose another menu option',-icw)
c		call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
c		goto 151	!redraw menu and get another option

	   case(13)
		goto 151		!back to main ACCEPT menu

	   case(14)
		call SCROLDIAL(1)
		goto 151		!back to main ACCEPT menu

	   case(15)		!HELP OPTIONS
		helpst(1)=
     &	  '2.REJECT FIT: return to roll mode and carry on    '
		helpst(2)=
     &	  '4.REFIT ONE AMP: fit the transitions just found but  '
		helpst(3)=
     &	  '   estimating only one open level for all openings   '
		helpst(4)=
     &	  '5.REFIT MANUAL: define a single open level with      '
		helpst(5)=
     &	  '  cursors and fit specified number of transitions    '
		helpst(6)=
     &	  '6.REFIT SUBLEVEL: defined amplitudes separately with '
		helpst(7)=
     &	  '  cursor for each of a specified # of transitions    '
		helpst(8)=
     &	  '7.REFIT AUTO: fits all transitions and amplitudes    '
		helpst(9)=
     &	  ' (in the defined fit range) automatically.           '
		helpst(10)=
     &	  '9.SET GAP UNUSABLE: in case of breakdown or double   '
		helpst(11)=
     &	  '  opening insert an unusable shut period, separating '
		helpst(12)=
     &	  '  two good openings.                                 '
		helpst(13)=
     &	  '+:ALTER RANGE: alter the range of points to be fitted'
		helpst(14)=
     &	  '  -arrow keys move low end; ctrl-arrow moves high end'
c
		ntext=14
	   	call POPTEXT(mxlo,mylo,myhi,helpst,ntext,ictx,ibkx,icfx)
		goto 151
	END SELECT
c
20	continue	!jump to here until ampdone=true
c Calc approx average amplitude now.
c (1) First value=that found by initial 'fit a full amplitude'
c	  (set in SCAN after leaving FITSUB)
c (2) For next 10 values that are within 20% of first, ask whether to average
c (3) Thereafter average all values that are within 10% of first 10
c
c Better to start on avamp only AFTER initial amplitude done
c	if(navamp.eq.0) then
c	   avtot=stepamp(1)
c	   navamp=1
c	   avamp=avtot
c	else if(navamp.ge.1.and.navamp.le.10.and.kt.ge.ntrans) then
	if(navamp.le.10.and.kt.ge.ntrans) then
	   if(ifitmode.eq.2) then
		k1=kt-ntrans+1
		k2=k1			!ask once only for manual
	   else if(ifitmode.eq.3) then
		k1=kt-ntrans+1
		k2=kt			!ask for each amp if 'sublev'
	   endif
	   do k=k1,k2
		a=float(iampl(k))
		if(iampl(k).ne.0.and.iprops(k).eq.0.and.
     &		   abs(a-avamp).lt.0.2*abs(avamp)) then
		   call DCFORMAT(avamp*calfac2,7,2,cnum0)
		   call DCFORMAT(a*calfac2,7,2,cnum)
		   n0=nblank(cnum0)
		   n1=nblank(cnum)
		   call WDIALOG(1,'Amplitude (pA)'//cnum(1:n1)//
     &	    ' (present mean = '//cnum0(1:n0)//')',icw1)
		   call BELL(1)
		   ans='Y'
	 	   call DEFOLTa(ans,defolt)
		   call QDIALOG(1,
     &	' -average this as a full amplitude',defolt,-icw1,cans)
		   call GETINPa(cans,ans)
		   if(ans.ne.'N') then
			avtot=avtot + a
			navamp=navamp+1
	   		avamp=avtot/float(navamp)	!in intermediate units
		   endif
c         Erase message (by writing again in backround col, icbd, to avoid
c		problems with string length in wdialog)
		   call QDIALOG(1,
     &	' -average this as a full amplitude',defolt,-icbd,cans)
		endif
	   enddo
	else if(navamp.gt.10) then
	   do k=kt-ntrans+1,kt
		if(iampl(k).ne.0.and.iprops(k).eq.0) then
		   a=float(iampl(i))
		   if(abs(a-avamp).lt.0.1*abs(avamp)) then
			avtot=avtot + a
			navamp=navamp+1
		   endif
		endif
	   enddo
	   avamp=avtot/float(navamp)	!in intermediate units
	endif
c
999	continue
	RETURN
	end

