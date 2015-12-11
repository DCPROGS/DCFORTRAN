	subroutine FITSUB6(idata1,ix,dx,ndfit,infirst,base,basevga,
     & ampdone,ntrig,navtest,scrit,tf50,trise,opendown,goback,
     & nscale,iyoff,ioff1,sfac3,dfinter,timamax,mono,open,shut,
     & nshutfit,tlast,prevlevel,shutprev,shutsav,infit,ixfprev,
     & iautosub,vard,disptran,dispderiv,dispguess,refit,
     & newguess,t0sav,y0sav,dtsav,dt,delt,iampcrit,rms1,
     & calfac1,calfac2,tint,iampl,iprops,ifitmode,ifitold,isavbase,
     & iyd1,iyd2,yd1,yd2,icol1,icol2,icw,icw1,icbl,ibk,ibk2,
     & avtot,navamp,avamp,kt,timsav,nfits,ifits,ibaselin,ndat,
     & ntmax,nfmax,tsfac,tlfac,tcfac,accepted,
     & tstring,microsec,bdisp,itriglev,itrigvga,
     & imin,imax,ifreq1,ifreq2,ifreq3,idata0,
     & idiskq,ndev,indfst,srate,calfac,ioff,cdate,adctime,
     & cfac1,nscale1,iyoffz,izoomfac,errfac,confac,derivfac,
     & cjump,tzerod,intzero,ix0prev,ktjump,nfjump,tsample,
     & ijump,njump,njfit,jumps,njdim)
c
c FITSUB6=version of FITSUB for ifitmode=2,3
c i.e. manual definition of amplitude; only durations fitted, amplitudes
c all defined in advance.  Uses SCNFUNC2 for fit, with nevfix=50, so fits
c only the area covered by transitions and decides its own fit range. Thus
c no need to specify (or adjust) fit range here.
c
c Line 12 of parameters is for point amp histo
c Line 13 is for plot queue
c Line 14 is for zoom (values from expmode)
c 07/15/94 06:46pm Zoom of Y axis added for problem with seeing small
c channels in presence of large ones.
c===========NB fix of baseline offset (after accept, or adj baseline)
c======when zoomed is not yet fixed -involves iyoff, sfac3 etc which all change!
c
c Modif 04/07/94 09:14am by adding SETVALS to set iampl() etc for 'use this
c guess' option.
c
c If entered from FITSUB will have refit=true (when 'refit: manual' or
c 'refit: sublevel' chosen in FITSUB).  May also have newguess=true if
c init guesses were reset in FITSUB) in which case dt(), delt() are
c set already so do not search for transitions or set dt(), delt() again.
c
c IDATA1 scaled (in SCAN) by 2**nscale and add offset i.e. in vga pixel units
c so used only for display here all calcs are done with data(i) which is
c still in intermediate units, as is idata0()
c To cope with zoom, need to do same for iycalc.  Values are calc in iycalc0()
c and scaled values are kept in iycalc1()
c
c FITTING MODES
c ifitmode=0: automatic detection/fit of all amps and durations
c ifitmode=1: automatic with only one amplitude fitted
c ifitmode=2: manual definition of amplitude; only durations fitted (prev 4)
c ifitmode=3: manual sublevel mode; only durations fitted (previously 5)
c
c Modif 09/14/94 05:19pm so only 5 values averaged for AVAMP, and value
c not changed thereafter (unless altered manually). Also asks only
c once per fitting now.
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
	real*8 dfinter,ttrans,tlast,tltemp,tzerod,tsample
	real*8 timsav(nfmax)
	integer*2 ifits(nfmax),ibaselin(nfmax)
c
	integer*2 idata1(2048)	!for data to be fitted (and as temp file)
	integer*2 idata0(2048)	!unscaled version of idata1 for AIVPIX
	real*4 data(2048)
	integer*4 ifreq1(imin:imax),ifreq2(imin:imax),ifreq3(imin:imax)
	integer*2 ix(2048),ioff2
c	integer*2 istat(20)	!for search of dy/dt
	integer iact(0:15)
	real*4 filt1(1024)	!from INSCAN/NORMFILT
	real*4 filt(640)
	real*4 tint(ntmax)
	integer*2 iampl(0:ntmax)
	integer*1 iprops(ntmax)	!properties of ith interval (see SCAN)
	integer*1 iprop1sav
	real*4 ylevel(50)		!temp storage for transition search results
	real*4 dtrigm(50)		!needed for getall; not used here
	real*4 timamax(49)
	real*4 delt(0:50)		!temp storage for transition search results
	real*4 ycalc(5120)
c    up to 5120 points for convol trace
	integer*2 ixcalc(5120),iycalc0(5120),iycalc1(5120)
	real*4 ydcalc(2048)		!calc values from SCANFUNC (in common/ycal/)
c arrays to hold graphics overwritten by line
	integer*1 isavbase(320),isavy1(320),isavy2(320)
	integer*1 isavb1(320),isavb2(320),isavb3(320)!store pixels for fit range bar
	real*4 stepamp(50),dt(49),dtsav(49),t0sav
	logical ampdone	!true when prelim amp measurement has been done
	logical opendown,mono,open,shut,shutprev,shutsav
	logical opening(50),disptran,dispderiv,dispguess
	logical dubious	!amplitude ill-defined (not merely short!) (='assume' in PDP)
	logical refit,sublev,newguess,btest,accepted,zoomed
	logical shut1,flat,shutrun(50)	!for point amp
	logical setshut(0:50)	!not used here -needed for setsubl/modsubl calls
	integer i1r(50),i2r(50)			!ditto
	logical help			!for QDIALOG etc
	logical abort,reject
	logical goback
	logical debug,caplock,discprt,queued
	character*55 helpst(17)		!help strings
	logical fixamp,shutp		!for fixamp
	real*4 ypeak(50)			!for fixamp
	integer mfix(50)			!for fixamp
	character*11 cnum1(50),cnum2(50),cnum3(50),cnum0,cnum
	character*11 cnum11,cnum21,cnum31	!for cjump
	character*70 string		!to put result in dialog box
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character*12 tstring
	character*3 microsec
	character ans,ch
c For cjump
	integer*2 jumps(njdim)
	logical cjump,cjumpfst,fitpast
c	character getch,UC
c	integer*2 ktype
c for simplex
	real*4 theta(50)
	integer jfix(50),kfix(0:50)
c For plot queue
	character ndev*2,cdate*11,adctime*8
C For DCMENU
	integer Lb(18)
	character*78 text(18)
c
	common/popvalx/mxlo,mylo,myhi,ictx,ibkx,icfx	!values for poptext calls
	common/dialval1/ictd,icfd,icbd  !colours for dialog box #1 (text,frame,back)
	common/fitblk/data,if1,if2,ntrans,neval,nevfix,itype,stepamp,y0
	common/ycal/ydcalc	!calc values from SCANFUNC
	common/fixblk/fixamp,tshort,tlong,ypeak,opening,shutp,mfix
	common/filtblk/filt1,nfilt1,dtf1,npfilt,finter
	common/hlp/help		!for QDIALOG etc
	common/ampmark/nampmark,iamark(10)
	common/dp/discprt
	common/abt/abort
	common/jump/cjumpfst
	EXTERNAL SCNFUNC2
c
c
	debug()=caplock()
c
	zoomed=izoomfac.gt.1
	sfac31=sfac3*float(izoomfac) 	!converts intermed to pixel units
c
c Define cjumpfst =true for 1st interval in each sweep (ktjump is updated
c only when interval accepted -also want setting of initial 'full amp' to
c be done as though this was not a c-jump).
c    For first interval in jump, check first that prev interval was
c the last in prev jump (iprops=64) -should always be so but bug
c (assoc with restart?) sometimes means it is not!
	if(cjump) then
	   cjumpfst=cjump.and.ktjump.eq.0.and.ampdone
	   if(cjumpfst) then
		ktj0=kt+1		!value of kt to be replaced by latency later
		if(ijump.gt.1) then
		   if((.not.BTEST(iprops(kt),6)).and.
     &			(.not.BTEST(iprops(kt),5))) then
			call BELL(3)
			call INTCONV(kt,cnum)
		      call WDIALOG(1,
     &	'Last interval #'//charnb(cnum)//
     &	' should have props=32 or 64!',12)
		   endif
		endif
	   endif
	endif
c
	accepted=.false.
	queued=.false.	!so values not printed to disc
	sublev=ifitmode.eq.3
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
	tshort=tsfac*trise
c	tlong=tlfac*trise
	tclose=tcfac*trise
	ibasevga=ifixr(basevga)
c Define time needed to centralise an arrow pointing to open or shut
c period (in CHKSHUT, or VARROW below)
c NB should not use npfilt for this since npfilt defined as number of
c points to jump after trigger! Try defining in terms of trise?
c	tshift=0.70*finter*float(npfilt)	!factor of 0.75 is experimental!
c	tshift1=1.3*finter*float(npfilt)	!used when END of trans set in SETSUBL
	tshift=1.10*trise				!factor is experimental!
	tshift1=2.0*trise				!used when END of trans set in SETSUBL
	cfac=dx/finter	!factor to convert x0 to t0
	xshift=tshift*cfac	!tshift in pixel units
	xshift1=tshift1*cfac
	t0cor=0.25*trise
c
c DEFINE if1,if2 here (needed to set amps for sublevel mode, as well as
c  to search for transitions in manual mode.
c If refit than if1,if2 already set so skip next bit
	if(.not.refit) then
	   if1=1		!first point to be searched
	   if2=ndfit	!=whole display
	   inf2=infirst+if2-1	!abs index of data(if2)
	   if(inf2.gt.ndat) if2=ndat-infirst+1	!do not search past end of data!
c Check that search for transitions starts after the end of prev fitting
c -if not, start search at last trans + npfilt (latter gets past last trans)
c If trace has been rolled back (without using 'goback n fittings' option)
c the value of if1 here may be greater than ndfit!.  Need some criterion
c for the number of points that must be left for it to be worth trying to fit-
c say minimum of 0.2*ndfit
	   inf1=infirst+if1-1	!abs index of data(if1)
	   if(cjumpfst) then
	      if(inf1.lt.intzero) then	!do not start fit before t=0
	         inf1=intzero
		   if1=inf1-infirst+1
		   if(if1.lt.1.or.if1.gt.if2-20) then
		     call BELL(2)
		     call POPTIME(250,130,'ATTEMPT TO FIT BEFORE t=0!',
     &		1,15,12,14,2.0)
		     ikey=2		!reject
		     goto 102
		   endif
		endif
	   endif
	   fitpast=inf1.lt.infit
	   if(cjump) then
		fitpast=fitpast.and.njump.eq.njfit
		if(njump.lt.njfit) fitpast=.true.
	   endif
	   if(fitpast) then
		if(.not.cjump) then
		   if1=if1+(infit-inf1)+npfilt
		   fitpast=if1.gt.0.8*ndfit
		else		!cjump
		   if(njump.eq.njfit) then	!in correct jump
			if1=if1+(infit-inf1)+npfilt
			fitpast=if1.gt.0.8*ndfit
		   else if(njump.lt.njfit) then
			fitpast=.true.
		   endif
		endif
		if(fitpast) then
c=		if1=if1+(infit-inf1)+npfilt
c=		if(if1.gt.0.8*ndfit) then
		  call BELL(1)
		  helpst(1)='ALREADY FITTED PAST HERE'
		  helpst(2)='Attempt to fit rejected-   '
		  helpst(3)='Suggest using GO TO . .option'
		  helpst(4)='to ''go back to last fitting'''
		  nh=4
		  if(cjump.and.njump.lt.nj) then
			nj=jumps(ijump)
			call INTCONV(nj,cnum)
			n=NBLANK(cnum)
		      helpst(1)='ALREADY FITTED PAST HERE'
		      helpst(2)='Attempt to fit rejected-   '
			helpst(3)='Use the 5.GO TO... option to go'
			helpst(4)='to ''Start of c-jump #'' '//cnum(1:n)
			nh=4
		  endif
	   	  call POPTEXT(mxlo,mylo,myhi,helpst,nh,ictx,ibkx,icfx)
		  ikey=2		!reject
		  goto 102
		endif
	   endif
	endif
	if(if1.lt.1) if1=1
	if(if2.gt.ndfit) if2=ndfit
	inf2=infirst+if2-1	!abs index of data(if2)
	if(inf2.gt.ndat) if2=ndat-infirst+1	!do not fit past end of data!
c
c DEFINE AMPLITUDES:
c (also option to alter guesses, but only if refit=true so dtsav,t0sav, y0sav
c are already defined)
c (previously done in subroutine DEFAMP but many params needed do easier
c to put code in here).
c Set iampl() and iprops() (temporarily?) here, but kt not updated
c until fit is confirmed
c
c
	call WDIALOG(1,
     &  'Choose number of transitions',-icw)
251	nbox=15
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
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
	text(9)='9: FIT n TRANS'
	text(10)='10: QUEUE PLOT '	!move PLOT to key 10 throughout
	text(11)='+:ADJUST BASELINE'
	text(12)='-:ALTER GUESSES'
	text(13)='*:CANCEL THIS FIT'
	text(15)='.:OPTIONS/HELP (F1)'
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
250	call CKEY(ch,ikey)
	if(ikey.lt.-1.or.ikey.gt.15) goto 250
	if(ikey.eq.14) goto 250       !not used yet!
	if(ikey.eq.-1) ikey=15		!F1=help
256	continue
c
c Set number of transitions to be fitted and interpolate the
c step resp function at suitable intervals for this display
	if(ikey.ge.1.and.ikey.le.9) then
	   if(ikey.le.8) then
		ntrans=ikey
c         Erase message (by writing again in backround col, icbd, to avoid
c		problems with string length in wdialog)
		call WDIALOG(1,
     &	  'Choose number of transitions',-icbd)
	   else
25	      call BELL(1)
	      call QDIALOG(1,
     &	'Number of transitions to be fitted',' ',icw,cans)
		call GETINPi(cans,ntrans)
		if(ntrans.gt.49) then		!kmax=ntrans here
		   call BELL(1)
		   call POPTIME(200,150,
     & 	  'TOO MANY TRANSITIONS -MAX=49',1,14,12,14,1.5)
		   goto 25
  		endif
	   endif
	   call NEWFILT(filt1,nfilt1,dtf1,ntrans,ndfit,finter,
     &    filt,nfilt,dtf)
	endif
	dxf=dx*dtf/finter
c Set NTM=max number of transitions to be sought in data().
c For prelim fit of single full opening (at this point have no amplitude
c yet) set ntm=2
	ntm=ntrans		!i.e. the specified number
	if(.not.ampdone) ntm=2
c
	SELECT CASE(ikey)
	 case(10)		!plot
c NB When SCQPLT called from FITSUB/FITSUB6 substitute 1,2048 for novlap,nbuf
c  and call with idata0() rather than idata(), and specify first point
c  to be kept in isdfst=1, number to be fitted=ndfit subs for ndisp1.
		call SCQPLT(idiskq,ndev,indfst,1,ndfit,idata0,2048,
     &      0,srate,calfac,calfac2,ioff,cdate,adctime,mono,iesc,
     &      ntrans,y0,t0,nfilt,dtf,dt,stepamp,filt,tif1,tif2,base)
		call BELL(1)
		if(iesc.ne.0) then	!plot cancelled -get another option
		   call WDIALOG(1,
     &		'Plot cancelled -choose another menu option',-ictd)
		else
		   call WDIALOG(1,
     &	    'Plot queued -choose another option',-ictd)
		endif
	      goto 250	!get another option
c
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
		call CALLINE(ibasevga,cfac1,opendown,ndfit,finter,-1)	!calibration
	      call SETACT(iact,15)		!draw white whatever
c	      call SETACT(iact,-1)		!no draw
c	      iact(ibk2)=15			!unless background col, when draw white
		call IDCPIX(ix,idata1,ndfit,iact)
		ibasevga=ifixr(basevga)
		call RDGWIN1(0,ibasevga,639,ibasevga,isavbase)  !keep data overwritten
		call HLINE(0,639,ibasevga,icbl)	   !show baseline
	      if(nampmark.gt.0) then
		   do i=1,nampmark
		      iy=ibasevga+iamark(i)	!sign of iamark set in inscan
		      call DHLINE(0,639,iy,10,22)
		   enddo
	      endif
c 		call FITBAR(ix,dx,if1,if2,ndfit,yd1,yd2,basevga,
c     &	 opendown,.false.,isavb1,isavb2,isavb3)	!draw fit range bar
		call BELL(1)
c	      call WDIALOG(1,
c     &       'Next choose another menu option',-icw)
	      call WDIALOG(1,
     &       'Now choose number of transitions',-icw)
	   goto 250
c
	 case(12)
	   newguess=.true.
	   call BELL(1)
	   call WDIALOG(1,
     &'Adjust transition times with arrow keys/mouse (F1=help)'
     &    ,-icw)
	   call FIXGUESS(t0sav,y0sav,dtsav,ntrans,filt,nfilt,
     &    stepamp,dtf,trise,x0,dxf,nscale1,iyoffz,cfac,ibk,ibk2,
     &    tshift,theta,t0,dt,delt,jfix,kfix,kmax,opening,
     &    opendown,tshort,iprops,kt,base,ntmax)
	   call WDIALOG(1,'Now choose another menu option',-icw)
c	   call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
	   goto 251	!return to menu

	 case(13)			!cancel fit
	   ikey=2
	   goto 102		!reject fit

	case(15)		!help
		helpst(1)=
     &	  '1-9: Specify the number of transitions to be fitted'
		helpst(2)=
     &	  '(this must be done -others are optional).          '
		helpst(3)=
     &	  '10.QUEUE PLOT: add data on screen to plot queue.   '
		helpst(4)=
     &	  '11.ADJUST BASELINE: adjust position of shut level  '
		helpst(5)=
     &	  '   manually.                                       '
		helpst(6)=
     &	  '12.ALTER GUESSES: change position of initial guesses'
		helpst(7)=
     &	  '  for the positions of the transitions (refit only).'
		helpst(8)=
     &	  '13.CANCEL FIT: return to roll mode and start again. '
c
		ntext=8
	   	call POPTEXT(mxlo,mylo,myhi,helpst,ntext,ictx,ibkx,icfx)
	  	goto 250
c
	END SELECT
c
c Now identify 'last', 'last but one' amplitude for use in SETLEV, SETSUBL
	call GETLAST(kt,iampl,iprops,ntmax,ialast,ialast1)
c
c If initially open and NOT sublevel mode, then open level=prevlevel
c and next level must be shut, so no need to set any levels here
c
c NB Do not define SHUTPREV here because if last fit was rejected more
c than once then value of SHUT may be wrong! Or, at least, do not redefine
c SHUT until a fit is accepted.
c NB do not reset SHUT here after 'REDO last n transitions'.
	if(goback) then
	   goback=.false.
	else
	   SHUT=SHUTSAV		!NB SHUTSAV is updated only when fit accepted
	endif
	inext=if1	!for FINDALL below
	SHUTPREV=SHUT		!save value of SHUT before 1st transition
					!=value at end of previous fitting
c
c CJUMP case!
c For first event after t=0, channel could be open at t=0, but no prevlevel
c defined so start search at curlevel=mean of first 2 points -but GETALL
c requires SHUT to be defined (FINDALL uses it only to set uplegal)
c -to be on safe side, check whether open at t=0 every time
	if(cjumpfst) then
	   curlev=0.5*(data(if1)+data(if1+1))
	   ans='Y'
 	   call DEFOLTa(ans,defolt)
	   if(abs(curlev-base).gt.scrit) then
	      call QDIALOG(1,
     &	'Channel is OPEN at t=0: O.K.)',defolt,icw,cans)
	   	call GETINPa(cans,ans)
		if(ans.eq.'Y') then
		   SHUT=.false.
		   SHUTPREV=.false.
		   SHUTSAV=.false.
		   prevlevel=avamp	!as guess
		   kfix(0)=0		! -but not fixed
	   	endif
	   else
	      call QDIALOG(1,
     &	'Channel is SHUT at t=0: O.K.)',defolt,icw,cans)
	   	call GETINPa(cans,ans)
		if(ans.eq.'N') then
		   SHUT=.false.
		   SHUTPREV=.false.
		   SHUTSAV=.false.
		   prevlevel=avamp
		   kfix(0)=0		! -but not fixed
	   	endif
	   endif
	endif
c
	if(.not.sublev) then
	   call SETLEV(iampl,iprops,ntmax,kt,t0,ntm,
     &    y0,dt,ntrans,filt,nfilt,stepamp,dtf,trise,x0,dxf,
     &    nscale1,iyoffz,cfac,ibk,ibk2,tshift,ylevel,delt,
     &    opening,opendown,mono,base,basevga,shut,if1,if2,inext,
     &    ialast,ialast1,sfac31,avamp,t0cor,tf50,icbl,icw,icw1,
     &    ioff1,scrit,ntrig,shutprev,timamax,finter,npfilt,
     &    disptran,data,ix,dx,prevlevel,ndfit,reject,
     &    tstring,microsec,bdisp,itriglev,itrigvga,idata1)
	    if(reject) then
	       ikey=2		!reject
	       goto 102
	    endif
c
	else if(sublev) then
c NB if fit is rejected several times, shut (and hence shutprev) may be
c incorrect, so define from shutsav which is updated only when fit is accepted
	   if(SHUT) then
	      curlev=0.5*(data(if1)+data(if1+1))
c		curlev=0.5*(data(1)+data(2))
	   else
		curlev=prevlevel
	   endif
c
	   call GETALL(data,ix,dx,nt,ntm,if1,if2,inext,itrig,it1,
     &    scrit,curlev,ntrig,navtest,shut,shutprev,open,opendown,
     &    kfix,delt,ylevel,base,t0,opening,trise,iautosub,timamax,
     &    avamp,ibasevga,tmin,finter,ibk2,vard,dispderiv,iampcrit,
     &    npfilt,disptran,ampdone,tclose,dtrigm,rms1,nscale1,iyoffz,
     &    derivfac)
c
c What if the requested number of transitions is not found? Must add more
c transitions to make up the requested number -make alternately open/shut
c -must define the necessary extra ylevel(), t0 and delt() values
	   if(nt.ge.2) then		!eliminate direct open-open < ampcrit
	     t0s=t0
	     call CHKGUESS(nt,t0,delt,ylevel,kfix,opening,scrit,
     &      base,iampcrit,shutprev,prevlevel)
		if(t0.ne.t0s) then		!t0 altered by chkguess, so fix1 it1
		   it1=2+ifix(t0/finter)
		endif
	   endif
	   if(nt.lt.ntrans) then
	      imode=0		!for sublevel mode
		call GETREST(nt,ntrans,ndfit,finter,shutprev,imode,
     &	 ylevel,base,avamp,prevlevel,inext,t0,delt,opening)
	   endif
c
c Calc step amplitudes in stepamp()
	   do i=1,ntrans
		stepamp(i)=ylevel(i+1)-ylevel(i)		!in intermed units
	   enddo
c Define time intervals for CONV in DT()
	   y0=ylevel(1)	!initial value
	   y0sav=y0		!for FIXGUESS
	   t0sav=t0
	   x0=t0*cfac
c======== which x0? -they are the same!
c==	   x00=x0		!for debugger
	   x0=float(it1-1)*dx	!'time' in x units for 1st transition
c tf50=time (mus) to 50% point on filter. Convert this to x units:
c dxf x units=dtf microsec so dxf/dtf x units/mus
c add empirical shift t0cor (defined above)
	   xcor=(tf50-t0cor)*dxf/dtf
	   x0=x0-xcor
	   t0=t0-tf50+t0cor	!NB VERY IMPORTANT TO CORRECT INIT GUESS FOR T0 TOO
	   t0sav=t0		!keep in cases init guesses need to be altered
c
	   do i=1,ntrans-1
		if(i.eq.1) then
		   dt(i)=delt(i)
		else
		   dt(i)=dt(i-1) + delt(i)
		endif
		dtsav(i)=dt(i)
	   enddo
c  Modified to show guess here, and decide whether to use it or not
	   call CONV1(ycalc,y0,ntrans,dt,ke,filt,nfilt,stepamp,dtf)
	   if(ke.gt.5120) ke=5120
	   do i=1,ke
		ixcalc(i)=ifixr(x0 + float(i-1)*dxf)
		iycalc0(i)=int2(ifixr(ycalc(i)))
	   enddo
c ycalc() is in units as given by readsec, without nscale or offset -must
c scale for display
c=	   call SCALINT(iycalc,ke,nscale1,iyoffz)
	   call SCALINT1(iycalc0,ke,nscale1,iyoffz,iycalc1)
c Display the convolved trace for init guesses, starting at time of 1st transition
	   call SETACT(iact,-1)		!no draw
	   iact(ibk2)=14			!unless background col, when draw yellow
c    Overwrite colours 4,9,13 also (=amplitude lines)
	   iact(4)=14			      !also overdraw 4
	   iact(9)=14			      !also overdraw 9
	   iact(13)=14			      !also overdraw 13
	   call IDCPIX(ixcalc,iycalc1,ke,iact)
	   call BELL(1)
c	   call WDIALOG(1,
c     &'Set amplitudes and guesses for transition times (F1=help)',-icw)
c	   ans='N'
c30	   call DEFOLTa(ans,defolt)
c	   call QDIALOG(1,
c     &   'Modify this guess (Y) (or make new guess (N))',
c     &	defolt,-icw1,cans)
c	   call GETINPa(cans,ans)
cc	  delete fitted curve again
cc	   call HIDE_MOUSE()
c	   call WDIALOG(1,
c     &   'Modify this guess (Y) (or make new guess (N))',-icbd)   !delete message
	   call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
	   text(1)='1: HELP (=F1)'
	   text(2)='2: MODIFY THIS GUESS'
	   text(3)='3: MAKE A NEW GUESS'
	   text(4)='4: USE THIS GUESS'
	   nbox=4
	   call SETLBOX(4,Lb,1)
	   call NUMSET			!set num lock on ready for response
	   call DCMENU(nbox,4,Lb,text,icol1,icol2)
252	   call CKEY(ch,ikey)
	   if(ikey.lt.-1.or.ikey.gt.4) goto 252
	   if(ikey.eq.-1.or.ikey.eq.1) then		!F1=help
		helpst(1)='2: Use keys/mouse to adjust the amplitudes,'
		helpst(2)='   and initial guesses for transition times,'
		helpst(3)='   that are shown by the yellow curve.     '
		helpst(4)='3: Yellow curve is bad guess so ignore it  '
		helpst(5)='   and define a new guess with mouse (which'
		helpst(6)='   can then be adjusted as above).'
		helpst(7)='4: Use yellow curve as guess, as it is. '
		ntext=7
	   	call POPTEXT(mxlo,mylo,myhi,helpst,ntext,ictx,ibkx,icfx)
		call HIDE_MOUSE()
		goto 252
	   endif
c       Delete yellow curve
	   call SETACT(iact,-1)	!no draw
	   iact(14)=ibk2	  !unless yellow, when draw background col
	   call IDCPIX(ixcalc,iycalc1,ke,iact)
	   if(ikey.eq.2) then
	     call MODSUBL(iampl,iprops,ntmax,kt,t0,
     &      y0,dt,ntrans,filt,nfilt,stepamp,dtf,trise,x0,dxf,
     &      nscale1,iyoffz,cfac,ibk,ibk2,tshift,ylevel,delt,
     &      kfix,opening,opendown,mono,base,scrit,shut,
     &      ialast,ialast1,sfac31,avamp,calfac2,rms1,reject,
     & 	setshut,icw,icw1)
	   else if(ikey.eq.3) then
	     call SETSUBL(iampl,iprops,ntmax,kt,t0,
     &      y0,dt,ntrans,filt,nfilt,stepamp,dtf,trise,x0,dxf,
     &      nscale1,iyoffz,cfac,ibk,ibk2,tshift,ylevel,delt,
     &      kfix,opening,opendown,mono,base,scrit,shut,
     &      ialast,ialast1,sfac31,avamp,calfac2,rms1,reject,
     & 	icw,icw1,tshift1,xshift1,ibad,setshut,
     &	finter,npfilt,data,if1,if2,timamax)
		if(ibad.eq.1) then
		   goto 252
		endif
	   else if(ikey.eq.4) then
c NB even when 'use this guess' chosen, must set check which are
c shut and set iampl(). This now done by SETVALS
		call SETVALS(iampl,ntmax,kt,ntrans,base,scrit,delt,dt,
     &	 ylevel,t0,dxf,dtf,tshift,ibk2,kfix,calfac2,rms1,avamp,
     &	 SHUT,setshut,stepamp,opendown)
		 reject=.false.
	   endif
	   if(reject) then
	      ikey=2		!reject
	      goto 102
	   endif
	endif
c
c	goto 100
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
c Return here to refit (with same ifitmode=2,3, and same amps) after
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
	   if(ke.gt.5120) ke=5120
	   do i=1,ke
		ixcalc(i)=ifixr(x0 + float(i-1)*dxf)
		iycalc0(i)=int2(ifixr(ycalc(i)))
	   enddo
c ycalc() is in units as given by readsec, without nscale or offset -must
c scale for display
c=	   call SCALINT(iycalc,ke,nscale1,iyoffz)
	   call SCALINT1(iycalc0,ke,nscale1,iyoffz,iycalc1)
c Display the convolved trace, starting at time of 1st transition
	   call SETACT(iact,-1)		!no draw
	   iact(ibk2)=14			!unless background col, when draw yellow
	   call IDCPIX(ixcalc,iycalc1,ke,iact)
	endif
c
c
c Prepare for SIMPLEX
	nevfix=50	!use fixed data set after 50 func eval
c	nevfix=-1	!fixed data set
c
c	errfac=1.e-3
c	errfac=0.01		!errfac now a parameter?
	delmin=-1.		!do not use delmin for convergence
c	stpfac=0.1		!0.02 and 0.3 both worse in one test
	stpfac=0.05		!seems to avoid some problems ?
c	nfix=0		!no param fixed
c standard value of confac (=contraction factor in simplex) is 0.5, but
c crude tests show 0.2 better when guess is good, and little worse
c when guess is bad -make it a default parameter?
c In another example, with poor guesses, confac=0.25 was disastrous, but
c 0.5 was OK so restore 0.5
c	confac=0.25
c	confac=0.5	!now set in paramwin
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
c
	irestrt=1		!allow one restart in simplex, at most
	call SIMPLEX2(kmax,theta,stpfac,errfac,neval1,nevmax,
     & smin,SCNFUNC2,Ndisp,jfix,delmin,confac,irestrt,iconv)
	if(abort) then
	   call BELL(1)
	   call POPTIME(200,150,'FITTING ABORTED',1,14,12,14,1.0)
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
c Problem arises when t0 is erroneously fiited by simplex to a value that
c is beyond right hand edge of screen: this gives a negative kemax, and hence
c negative ke, when fitted curve is calculated below, which causes a
c non-Lahey crash.  Better catch this here!
c	t0max=float(ndfit-1)*finter
	t0max=float(ndfit-4)*finter	!leave margin of 3 points before RH edge
	t0=theta(1)
	if(t0.gt.t0max.or.t0.lt.0.) then
	   call BELL(1)
	   call POPTIME(200,150,'FIT OFF SCREEN: REJECTED',
     &    1,15,12,14,1.5)
	   ikey=2		!reject
	   goto 102
	endif
	ttrans=dble(t0) + dfinter*dble(float(infirst-1))
	ttrans1=ttrans		!make copy for timsav()
c For cjump case, ttrans is measured separately for each jump, but timsav()
c calculated from 1st point of 1st jump (as though all jumps were contiguous)
c Now ttrans, tlast etc measured from 1st point of current jump
	if(cjump) then
	   ttrans=ttrans			!in microsec -for tltemp->new tlast
	   ttrans1=tsample*float(ijump-1) + ttrans1	!for timsav
	endif
c Next calc the interval completed by the 1st transition (unless
c it is 1st in the record, or TINT has already been set unusable=-1000.)
c For cjump case, tlast initialized to 0.0d0 so for first interval in
c each sweep (i.e. ktjump=0) told=ttrans=time from t=0 to first transition.
c First do the times:
	told=sngl(ttrans-tlast)		!in microsec (used only if kt>0)
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
		if(delt(i).lt.0.) negflag=1
	   enddo
	else
	   do i=1,ntrans-1
		if(delt(i).lt.0.) negflag=1
	   enddo
	endif
	if(cjumpfst) then
	   delt(0)=told         !time from t=0 to 1st fitted trans
	   tint1sav=(delt(0) - sngl(tzerod))*1.e-3	!save latency (in ms)
	   do i=0,ntrans-1
		if(delt(i).lt.0.) negflag=1
	   enddo
	   iprop1sav=IBSET(iprops(kt+1),5)   !first in sweep; set bit 5 ='32'
	endif
c
c Display fit BEFORE checking shut levels
c  First erase init guess (if drawn)
	if(dispguess) then
	   call SETACT(iact,-1)		!no draw
	   iact(14)=ibk2			!unless yellow, when draw background col
	   call IDCPIX(ixcalc,iycalc1,ke,iact)
	endif
c Calculate convolution in ycalc(1) to ycalc(ke)
	call CONV1(ycalc,y0,ntrans,DT,ke,filt,nfilt,stepamp,dtf)
	x0=t0*dxf/dtf
c modify next bit so fitted curve in xcalc, ycalc includes the flat bit
c at either end (makes it easier to delete calc curve!)
c Check that last point is on screen (ixcalc <=639); if not, reduce ke further
c (Could also reduce so that calc curve does not go beyond fitted region,
c  i.e. use ix(if2) rather than 639 here)
c NB If t0 is beyond right edge of screen, so x0>639, get negative ke here,
c and non-Lahey crash.  Excessive t0 values are intercepted above
c to prevent this.
	kemax=ifix(1. + ((639.-x0)/dxf))
	if(ke.gt.kemax) ke=kemax
	if(ke.gt.5120) ke=5120
	do i=1,ke
	   ixcalc(i)=ifixr(x0 + float(i-1)*dxf)
	   iycalc0(i)=int2(ifixr(ycalc(i)))
	enddo
c ycalc() is in units as given by readsec, without nscale or offset -must
c scale for display
	icc=12	!red for fitted trace
	if(mono) icc=15
c=	call SCALINT(iycalc,ke,nscale1,iyoffz)
	call SCALINT1(iycalc0,ke,nscale1,iyoffz,iycalc1)
c	call SETACT(iact,icc)		!draw red
	call SETACT(iact,-1)		!no draw
	iact(ibk2)=icc			!unless background col, when draw red
c Now extend calc curve to cover whole fitted range from data(if1) to
c data(if2).  These regions are flat, so can draw with HLINE
c Use RDGWIN1 to keep graphical data overwritten by HLINE to allow restoration
c when red fitted line is deleted
	if(ix(if1).lt.ixcalc(1)) then
	   call RDGWIN1(ix(if1),iycalc1(1),ixcalc(1),iycalc1(1),isavy1)
	   call HLINE(ix(if1),ixcalc(1),iycalc1(1),icc)	!before start of resp func
	endif
	if(ixcalc(ke).lt.ix(if2)) then
	   call RDGWIN1(ixcalc(ke),iycalc1(ke),ix(if2),iycalc1(ke),isavy2)
	   call HLINE(ixcalc(ke),ix(if2),iycalc1(ke),icc)   !after end of resp func
	endif
	call IDCPIX(ixcalc,iycalc1,ke,iact)			!draw resp function
c
c For cjump case, save initial amplitude here, for use later (this could
c be before fit, since amplitudes fixed before fitting done)
	if(shutprev) then 	!have already confirmed whether shut at t=0
	   iamp1sav=0
	else
c=	   a=theta(ntrans+1) - base 	!level before 1st transition
c=	   iamp1sav=int2(ifixr(a))
	   iamp1sav=iampl(kt)		! -now fixed (before fit)
	endif
c
c PRINT RESULTS OF FIT
c  -write (up to) 3 values of kt, time, amp per line in dialog box
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
c Try new layout for fit results. Use 3 lines for every fit -1st =trans#
c 2nd = duration, 3rd=amp; use 6 characters for every number+2 spaces (or
c space+'*' etc) -should get 9 numbers/line?
	i=0
	k1=kt+ntrans		!new kt (after accepted)
c
	koff=0	!normally
	if(cjumpfst) then
	   koff=1			!offset of index to display 1st fit of jump
	   cnum11='latency'		!insert latency before normal intervals
	   call TFORMAT(tint1sav,cnum21)
	   n2=NBLANK(cnum21)
	   cnum21=cnum21(1:n2)//char(242)	!'>=' for 1st interval in jump
	   if(iamp1sav.eq.0) then
		cnum31='   0       '              !amplitude
	   else
	      a=calfac2*float(iamp1sav)
	      call DCFORMAT(a,6,2,cnum31)		!amplitude
	   endif
	endif		!end of jump stuff
c
	do k=kt,k1
	 i=i+1	!=1,2,..,ntrans+1
	 if(k.gt.0) then
         call INTCONV(k+koff,cnum1(i)) !transition # (inc by koff for cjump)
	   call CJUSTIF(cnum1(i),6)
c
	   if(k.lt.k1) then
		t=delt(i-1)*1.e-3		!duration (ms)
		call TFORMAT(t,cnum2(i))
c		if(t.lt.1.0) then
c	         call DCFORMAT(t,6,3,cnum2(i))		!duration
c		else if((t.ge.1.0).and.(t.lt.10.)) then
c	         call DCFORMAT(t,6,2,cnum2(i))		!duration
c		else if((t.ge.10.).and.(t.lt.100.)) then
c	         call DCFORMAT(t,6,1,cnum2(i))		!duration
c		else
c	         call DCFORMAT(t,6,0,cnum2(i))		!duration
c		endif
	   else		!k=kt+ntrans
	      cnum2(i)='   ?       '
	      call CJUSTIF(cnum1(i),6)
	   endif
	   call CJUSTIF(cnum2(i),6)
c
	   if(iampl(k).eq.0) then
		cnum3(i)='   0       '              !amplitude
	   else
	      a=calfac2*float(iampl(k))
	      call DCFORMAT(a,6,2,cnum3(i))		!amplitude
	   endif
	   n3=NBLANK(cnum3(i))
	   dubious=BTEST(iprops(k),0)		!bit 0 set (='1')
	   if(dubious) then
	      n3=NBLANK(cnum3(i))
		cnum3(i)=cnum3(i)(1:n3)//'*'	!asterisk for dubious amplitude
	   endif
c Indicate fixed values (kfix(0) part applies to where 1st level is an
c opening that is fixed at level of last open level in prev fit; iprops not
c set in this case)
c	   if(iprops(k).eq.2.or.
	   if(BTEST(iprops(k),1).or.			!bit 1 set ='2'
     &	(k.eq.kt.and.kfix(0).ne.0)) then
	      n3=NBLANK(cnum3(i))
		cnum3(i)=cnum3(i)(1:n3)//char(240)   !equiv sign for fixed amplitude
	   endif
	   call CJUSTIF(cnum3(i),6)
	 endif
	enddo
c
	npl=8		!number of values per line
	ns1=1
	if(kt.eq.0) ns1=2
	if(cjumpfst) ns1=2	!even when kt>0 ???
	nsinc=ntrans+1
	if(nsinc.gt.npl) nsinc=npl
	iset=0	!record # of sets of 3 lines (for cjump)
21	continue	!return for next set of 3 lines if nec
	iset=iset+1	!record # of sets of 3 lines (for cjump)
	ns2=ns1+nsinc-1
	if(cjumpfst.and.iset.eq.1) ns2=ns2-1	!leave room for latency
	if(ns2.gt.ntrans+1) ns2=ntrans+1
c  Now make first line and write to box  -problem is that cnum1(i)(1:6)
c does not leave the correct field width if there are trailing blanks
c because n0=nblank(string) does not count them
	do i=1,70
	   string(i:i)=' '
	enddo
	string(1:3)=' #:'
	if=5		!next character position in string to be written to
	if(cjumpfst.and.iset.eq.1) then		!insert latency
	   string(if:if+6)=cnum11(1:7)
	   if=if+8
	endif
	do i=ns1,ns2
	   string(if:if+6)=cnum1(i)(1:7)
	   if=if+8
	enddo
      n0=NBLANK(string)
      call WDIALOG(1,string(1:n0),10)
c  now make 2nd line and write to box
	do i=1,70
	   string(i:i)=' '
	enddo
	string(1:3)='ms:'
	if=5		!next character position in string to be written to
	if(cjumpfst.and.iset.eq.1) then		!insert latency
	   string(if:if+6)=cnum21(1:7)
	   if=if+8
	endif
	do i=ns1,ns2
	   string(if:if+6)=cnum2(i)(1:7)
	   if=if+8
	enddo
      n0=NBLANK(string)
      call WDIALOG(1,string(1:n0),14)
c  now make 3rd line and write to box
	do i=1,70
	   string(i:i)=' '
	enddo
	string(1:3)='pA:'
	if=5		!next character position in string to be written to
	if(cjumpfst.and.iset.eq.1) then		!insert latency
	   string(if:if+6)=cnum31(1:7)
	   if=if+8
	endif
	do i=ns1,ns2
c         n0=NBLANK(string)
c	   string=string(1:n0)//' '//cnum3(i)(1:6)
	   string(if:if+6)=cnum3(i)(1:7)
	   if=if+8
	enddo
      n0=NBLANK(string)
      call WDIALOG(1,string(1:n0),11)
	if(ns2.lt.ntrans+1) then
c=	   ns1=ns1+npl
	   ns1=ns2+1            !try this (better for cjump case)
	   goto 21
	endif
c
c Reject fit with neg values AFTER values printed in dialog box
	if(negflag.ne.0) then
	   call BELL(1)
	   call POPTIME(200,150,'NEGATIVE INTERVAL: FIT REJECTED',
     &    1,15,12,14,1.5)
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
	text(1)='1:'
	text(2)='2: REJECT FIT  '
	text(3)='3: ACCEPT FIT'
	text(4)='4:REFIT: AUTO'
	text(5)='5:REFIT: ONE AMP'
	text(6)='6:REFIT: MANUAL'
	text(7)='7:REFIT: SUBLEVEL'
	text(8)='8:'
	text(9)='9:SET GAP UNUSABL'
	text(10)='10: QUEUE PLOT '
	text(11)='+:ADJUST BASELINE'
	text(12)='-:ALTER GUESSES'
	if(zoomed) then
	   text(13)='*: UNZOOM      '
	else
	   text(13)='*: ZOOM        '
	endif
	text(14)='/:SCROLL DIALOG'
	text(15)='.:HELP FOR MENU'
c Use HELP here only to explain menu options: put SCROLL DIALOG in above
c menu, and do 'new param' and 'new rms noise' only from roll-mode
c or expmode menus (not while waiting for accept/reject/refit option)
c=======other options needed: RESET BASELINE (under options/help?)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
152	call CKEY(ch,ikey)
1521	if(ikey.lt.-1.or.ikey.gt.15) goto 152
c==	if(ikey.eq.-1) ikey=15		!F1=help -no diff help box for F1
155	continue
c
c Reject fit when ikey=2, or any REFIT option chosen
c All REFIT options return to SCAN and data redrawn completely, so
c no need to delete fit here!
	accepted=.false.
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
c		call IDCPIX(ixcalc,iycalc1,ke,iact)
cc         and restore data overwritten by HLINE before and after response func
c		call WRGWIN1(ix(if1),iycalc1(1),ixcalc(1),iycalc1(1),isavy1)
c		call WRGWIN1(ixcalc(ke),iycalc(ke),ix(if2),iycalc1(ke),isavy2)
c	   endif
	endif
c
	if(ikey.eq.-1) then		!F1 help
		helpst(1)=
     &	  'DISPLAY OF FITTED VALUES:                         '
		helpst(2)=
     &	  '? indicates that duration not defined until next     '
		helpst(3)=
     &	  '  transition is fitted.                              '
		helpst(4)=
     &	  '* indicates that amplitude is set as ''dubious''.    '
		helpst(5)=char(240)//
     &	   ' indicates that amplitude is fixed, rather than     '
		helpst(6)=
     &	  '  being fitted to the trace.                         '
		helpst(7)=
     &	  'Blue arrow: marks shutting fixed to baseline level.  '
		helpst(8)=
     &	  'Purple arrow: marks opening fixed to current mean    '
		helpst(9)=
     &	  '   full amplitude level.                             '
		helpst(10)=
     &	  'Green arrow/lines: mark short opening with amplitude '
		helpst(11)=
     &	  '   constrained to nearest (larger) long open level.  '
c
		ntext=11
	   	call POPTEXT(mxlo,mylo,myhi,helpst,ntext,ictx,ibkx,icfx)
		goto 151
	endif
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
c
c NB don't accept anything until coloured points for point amp histo seen!
c GETSH1 modif 03/22/94 05:27pm because now called BEFORE kt updated
c
c Add relevant points to the point amplitude frequencies.
c In order to see blue/green points before recording values need to do
c loop to record where the flat bits (runs) are, and to draw blue/green
c points, before actually allocating any points to ifreq().
c ifreq1,ifreq2,ifreq3=all point, shut point, open point
c    Add shut points to ifreq2; open points to ifreq3 -can do this most
c    easily by looking for horizontal bits of ydcalc(i), i=if1,..,if2=calc
c    value corresponding to data(m), in common from SCANFUNC, SCNFUNC1 -can
c    judge whether shut or open from corresponding iampl(). The points
c    t0, t0+dt(1),... are at start of filter func for each transition (see
c    debug with VLINE above).  A short shutting gives 2 closely spaced lines
c    both of which appeat to precede the blip in the data.  However any flat
c    section on calc curve is ENDED by the line marking the NEXT transition,
c    and this can be used to identify which iampl() belongs to which flat bit.
		nrun=0	!number of flat bits found
		i1=if1
		i2=if1
		do i=if1+1,if2
		   flat=ydcalc(i).eq.ydcalc(i-1)
		   if(flat) then
			i2=i2+1     !if equal, increment index of last point in run
		   endif
		   if(.not.flat.or.i.eq.if2) then	!check n if last point reached
c	Record run of equal points just ended if have at least 2 points
			n=i2-i1+1
			if(n.ge.2) then
			   nrun=nrun+1
			   i1r(nrun)=i1	!record 1st point in run
			   i2r(nrun)=i2	!record last point in run
c			   shut1=abs(ydcalc(i1)-base).lt.scrit !run is of shut points
			   call GETSH1(shut1,shutprev,shut,t0,dt,i1,i2,kt,
     &			finter,ntrans,iampl,ntmax)	!get shut1
			   shutrun(nrun)=shut1		!record if run is shut
			   if(shut1) then
			      call SETACT(iact,11)		!blue=shut
			      call IDCPIX(ix(i1),idata1(i1),n,iact)
			   else
			      call SETACT(iact,10)		!green=open
			      call IDCPIX(ix(i1),idata1(i1),n,iact)
			   endif
			endif
			i1=i		!start new run
			i2=i		!start new run
		   endif
		enddo
c For now, require to hit 3 TWICE to accept, so can view green/blue points
		call CKEY(ch,ikey)
		if(ikey.ne.3) goto 1521		!if not accepted twice
c
c -now finally accepted, so allocate tint etc
		SHUTSAV=SHUT	!NB SHUTSAV is updated only when fit accepted
		refit=.false.	!reset
		newguess=.false.	!reset
c Now accept provisional tint,iampl,tlast=tltemp etc
c If OPEN at end of fitting, save level for use in next fitting (save anyway)
		prevlevel=ylevel(ntrans+1)
		tlast=tltemp
c update number of fits, timsav etc
c ifits(i) = index of tint(), iampl() for the duration and amplitude
c of the interval following first transition of fitting #i
		nfits=nfits+1
		timsav(nfits)=ttrans
		ifits(nfits)=kt+1		!points to element of tint(), iampl()
		ibaselin(nfits)=ifixr(base)
		if(cjump) nfjump=nfjump+1
c update tint()
c In case of cjump, tint(kt) has already been allocated to interval between
c last transition and end of previous jump for the first fit in all jumps
c after the first one, so do not overwrite it in next line.
		if(cjump) then
		   if(ktjump.gt.0) tint(kt)=delt(0)*1.e-3	!prev interval completed
		else
		   if(kt.gt.0) tint(kt)=delt(0)*1.e-3	!prev interval completed
		endif
		if(ntrans.gt.1) then
		   do i=1,ntrans-1
			kt=kt+1		!update kt
			if(cjump) ktjump=ktjump+1
			tint(kt)=delt(i)*1.e-3	!in msec
		   enddo
		endif
		kt=kt+1		!update kt (whether ntrans=1 or more)
		if(cjump) ktjump=ktjump+1
c
		if(debug().or.queued) then
		   queued=.false.	!so values not printed to disc
		   if(discprt) then
			write(8,52) if2-if1+1,if1,if2,ndfit
52			format(' Number of points fitted = ',i8,
     &		' (',i5,' to ',i5,': ',i6,' points displayed)',/,
     &		'   #    duration (ms)  amplitude (pA)  properties')
			k1=kt-ntrans
			if(k1.le.0) k1=1
			do i=k1,kt
		         a=calfac2*float(iampl(i))
			   write(8,53) i,tint(i),a,iprops(i)
53			   format(i7,2x,f11.4,3x,f10.2,6x,i3)
			enddo
		   endif
		endif
c
c -now finally accepted, so allocate points to ifreq()
c  Points if1 to if2 were fitted -add all to ifreq1()		ibase=ifixr(base)
		ibase=ifixr(base)
		do i=if1,if2
		   j=idata0(i)-ibase
		   ifreq1(j)=ifreq1(j) + 1
		enddo
		do i=1,nrun
		   i1=i1r(i)
		   i2=i2r(i)
		   if(shutrun(i)) then
			do m=i1,i2
			   j=idata0(m)-ibase
			   ifreq2(j)=ifreq2(j) + 1
			enddo
		   else
			do m=i1,i2
			   j=idata0(m)-ibase
			   ifreq3(j)=ifreq3(j) + 1
			enddo
		   endif
		enddo
c
c NOW re-adjust base,basevga, iyoff etc? NB basevga= position on screen that
c data is to have baseline adjusted to, so don't alter this
		ybasevga=base*sfac3 + float(iyoff)
		ioff1=ifixr(basevga-ybasevga)		!offset to adjust baseline to bdisp
		iyoff=iyoff+ioff1		!new offset for next display
c  Now accepted, update the 'fitted up to here' marker
c Note that infirst=isprev + (isec-1)*nbuf = absolute index of 1st point
c in idata1(), so abs index of last trigger point fitted is infit=infirst+itrig
c -but now base the new infit on fitted params, not trigger point!
		if(.not.cjump) then
		   infit=1 + ifix(sngl(tlast/dfinter)) + npfilt
		else
		   infit=1 + ifix(sngl((tlast)/dfinter)) + npfilt
		   njfit=njump	!record jump # that contains most recent fit
		endif
c		call MARK(infit,infst,ndfit,ixfprev,ix,14,ibk,
c     & 	  cjump,intzero,ix0prev)
		accepted=.true.
c Lastly, for first fit of each jump, insert the first element, and
c move all others up one place (cjumpfst will be set false on next
c entry to FITSUB/6, but don't set it false here as it is needed
c e.g. in listdata)
c After first sweep, want to move up only those that were fitted in this sweep
c i.e. from ktj0 on, and insert latency into tint(ktj0)
		if(cjumpfst) then
		   do i=kt,ktj0,-1
			tint(i+1)=tint(i)
			iampl(i+1)=iampl(i)
			iprops(i+1)=iprops(i)
		   enddo
		   tint(ktj0)=tint1sav
		   iampl(ktj0)=iamp1sav
		   iprops(ktj0)=iprop1sav
		   kt=kt+1		!now have one more
		endif
c
	   case(4)			!refit auto
		ifitold=ifitmode
		ifitmode=0
		refit=.true.
		goto 999	!skip average amplitude bit

	   case(5)			!auto -one amp (first reject fit just done)
		ifitold=ifitmode
		ifitmode=1
		refit=.true.
		goto 999	!skip average amplitude bit

	   case(6)			!refit manual
		ifitold=ifitmode
		ifitmode=2
		refit=.true.
		goto 999	!skip average amplitude bit

	   case(7)			!MANUAL SUBLEVEL MODE (first reject fit just done)
		ifitold=ifitmode
		ifitmode=3
		refit=.true.
		goto 999	!skip average amplitude bit

	   case(8)
		goto 152	!get another key

	   case(9)		!set unusable
51		k1=kt-8	!display tint(kt-8) to tint(kt)
		imode=0
		call SELDATA(k1,kt,tint,iampl,calfac2,kval,ntmax,imode)
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
c NB When SCQPLT called from FITSUB/FITSUB6 substitute 1,2048 for novlap,nbuf
c  and call with idata0() rather than idata(), and specify first point
c  to be kept in isdfst=1, number to be fitted=ndfit subs for ndisp1.
		tif1=float(if1-1)*finter	!time for point if1 (microsec from 1st point)
		tif2=float(if2-1)*finter	!time for point if1 (microsec from 1st point)
		call SCQPLT(idiskq,ndev,indfst,1,ndfit,idata0,2048,
     &      ntrans,srate,calfac,calfac2,ioff,cdate,adctime,mono,iesc,
     &      ntrans,y0,t0,nfilt,dtf,dt,stepamp,filt,tif1,tif2,base)
		call BELL(1)
		if(iesc.ne.0) then	!plot cancelled -get another option
		   call WDIALOG(1,
     &		'Plot cancelled -choose another menu option',-ictd)
		else
		   call WDIALOG(1,
     &	    'Plot queued -choose another option',-ictd)
		   queued=.true.	!so values printed to disc
		endif
		goto 152	!get another option

	   case(11)       !adjust baseline
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
		call CALLINE(ibasevga,cfac1,opendown,ndfit,finter,-1)	!calibration
	      call SETACT(iact,15)		!draw white whatever
c	      call SETACT(iact,-1)		!no draw
c	      iact(ibk2)=15			!unless background col, when draw white
		call IDCPIX(ix,idata1,ndfit,iact)
		ibasevga=ifixr(basevga)
		call RDGWIN1(0,ibasevga,639,ibasevga,isavbase)  !keep data overwritten
		call HLINE(0,639,ibasevga,icbl)	   !show baseline
	      if(nampmark.gt.0) then
		   do i=1,nampmark
		      iy=ibasevga+iamark(i)	!sign of iamark set in inscan
		      call DHLINE(0,639,iy,10,22)
		   enddo
	      endif
 		call FITBAR(ix,dx,if1,if2,ndfit,yd1,yd2,basevga,
     &	 opendown,.false.,isavb1,isavb2,isavb3)	!draw fit range bar
c		call BELL(1)
c	      call WDIALOG(1,
c     &       'Next choose another menu option',-icw)
		goto 100

	   case(12)		!new guesses for durations
		newguess=.true.
c After changing baseline OR guesses for times, do refit straight away
c (with same ifitmode=3 or 4 as before), using same amplitudes and number
c of transitions as before. First redraw data (as in SCAN)
		call FILLWIN(0,iyd1,639,iyd2,ibk2)	!clear data area
		call CALLINE(ibasevga,cfac1,opendown,ndfit,finter,-1)	!calibration
	      call SETACT(iact,15)		!draw white whatever
c	      call SETACT(iact,-1)		!no draw
c	      iact(ibk2)=15			!unless background col, when draw white
		call IDCPIX(ix,idata1,ndfit,iact)
	      ibasevga=ifixr(basevga)
		call RDGWIN1(0,ibasevga,639,ibasevga,isavbase)  !keep data overwritten
		call HLINE(0,639,ibasevga,icbl)	   !show baseline
	      if(nampmark.gt.0) then
		   do i=1,nampmark
		      iy=ibasevga+iamark(i)	!sign of iamark set in inscan
		      call DHLINE(0,639,iy,10,22)
		   enddo
	      endif
c
		cfac=dx/finter	!factor to convert x0 to t0
		call BELL(1)
		call WDIALOG(1,
     &'Adjust guesses for transitions with arrow keys/mouse (F1=help)'
     &      ,icw)
		call FIXGUESS(t0sav,y0sav,dtsav,ntrans,filt,nfilt,
     & 	 stepamp,dtf,trise,x0,dxf,nscale1,iyoffz,cfac,ibk,ibk2,
     &       tshift,theta,t0,dt,delt,jfix,kfix,kmax,opening,
     &       opendown,tshort,iprops,kt,base,ntmax)
		goto 100
c		call WDIALOG(1,'Now choose another menu option',-icw)
c		call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
c		goto 151	!redraw menu and get another option

	   case(13)
c         erase before changing nscale etc
c		call SETACT(iact,-1)		!no draw
c		iact(14)=ibk2			!unless yellow, when draw background col
		call SETACT(iact,ibk2)		!draw background col whatever
		call CALLINE(ibasevga,cfac1,opendown,ndfit,finter,ibk2) !erase
		if(zoomed) then		!unzoom
		   izoomfac=1		!factor to zoom Y values by
		   zoomed=.false.
		   call IDCPIX(ix,idata1,ndfit,iact)	!delete data
		   call IDCPIX(ixcalc,iycalc1,ke,iact)	!delete response
c
		   nscale1=nscale		!restore
		   iyoffz=iyoff		!restore
		   cfac1=calfac1 		!restore
		   sfac31=sfac3	 	!converts intermed to pixel units
c
		   text(13)='*: ZOOM        '
		   call DCMENU(-13,5,Lb,text,icol1,icol2)
		else				!zoom
		   izoomfac=4		!factor to zoom Y values by
		   zoomed=.true.
		   call DEFOLTi(izoomfac,defolt)
		   call QDIALOG(1,
     &    'Zoom factor (must be 1, 2, 4, 8, ...)',defolt,ictd,cans)
		   call GETINPi(cans,izoomfac)
		   z=log(float(izoomfac))/log(2.)
		   iz=ifixr(z)
		   call IDCPIX(ix,idata1,ndfit,iact)	!delete data
		   call IDCPIX(ixcalc,iycalc1,ke,iact)	!delete response
		   nscale1=nscale-iz
		   iyoffz=ifixr(basevga - (basevga-float(iyoff))*(2.**iz))
		   cfac1=calfac1/float(izoomfac)
c==		   sfac3=1.0/float(2**nscale) 	!converts intermed to pixel units
		   sfac31=sfac3*float(izoomfac) 	!converts intermed to pixel units
		   text(13)='*: UNZOOM      '
		   call DCMENU(-13,5,Lb,text,icol1,icol2)
		endif
c  redraw data, calc line, calibration with new values
		call SCALINT1(idata0,ndfit,nscale1,iyoffz,idata1)
		call SETACT(iact,15)		!draw data white whatever
		call IDCPIX(ix,idata1,ndfit,iact)		!draw data
		call SCALINT1(iycalc0,ke,nscale1,iyoffz,iycalc1)
		call SETACT(iact,12)		!draw fit red whatever
		call IDCPIX(ixcalc,iycalc1,ke,iact)		!draw calc line
		call CALLINE(ibasevga,cfac1,opendown,ndfit,finter,-1) !redraw cal
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
c	else if(navamp.ge.1.and.navamp.le.5.and.kt.ge.ntrans) then
	if(navamp.le.5.and.kt.ge.ntrans) then
c	   if(ifitmode.eq.2) then
c		k1=kt-ntrans+1
c		k2=k1			!ask once only for manual
c	   else if(ifitmode.eq.3) then
c		k1=kt-ntrans+1
c		k2=kt			!ask for each amp if 'sublev'
c	   endif
	   k1=kt-ntrans+1
	   k2=k1			!ask once only per fitting
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
c Now leave avamp unchanged after first 5
c	else if(navamp.gt.10) then
c	   do k=kt-ntrans+1,kt
c		if(iampl(k).ne.0.and.iprops(k).eq.0) then
c		   a=float(iampl(i))
c		   if(abs(a-avamp).lt.0.1*abs(avamp)) then
c			avtot=avtot + a
c			navamp=navamp+1
c		   endif
c		endif
c	   enddo
c	   avamp=avtot/float(navamp)	!in intermediate units
	endif
c
999	continue
	RETURN
	end

