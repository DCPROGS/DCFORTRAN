	subroutine FITSUB(idata1,ix,dx,ndfit,infirst,base,basevga,
     & ampdone,ntrig,navtest,scrit,tf50,trise,opendown,goback,
     & nscale,iyoff,ioff1,sfac3,dfinter,timamax,mono,open,shut,
     & nshutfit,tlast,prevlevel,shutprev,shutsav,infit,ixfprev,
     & iautosub,vard,disptran,dispderiv,dispguess,refit,newrange,
     & newguess,t0sav,y0sav,dtsav,dt,delt,iampcrit,tmin,ndat,
     & tsfac,tlfac,tcfac,accepted,ntmax,nfmax,
     & calfac1,calfac2,tint,iampl,iprops,ifitmode,ifitold,isavbase,
     & iyd1,iyd2,yd1,yd2,icol1,icol2,icw,icw1,icbl,ibk,ibk2,
     & avtot,navamp,avamp,kt,timsav,nfits,ifits,ibaselin,rms1,
     & tstring,microsec,bdisp,itriglev,itrigvga,
     & imin,imax,ifreq1,ifreq2,ifreq3,idata0,
     & idiskq,ndev,indfst,srate,calfac,ioff,cdate,adctime,
     & cfac1,nscale1,iyoffz,izoomfac,errfac,confac,derivfac,
     & cjump,tzerod,intzero,ix0prev,ktjump,nfjump,tsample,
     & ijump,njump,njfit,jumps,kjumps,njdim,avampsav,
     & itsimp,minmeth,nbasemin,fastfit)
c
c To locate and fit transitions in data1(1) to data1(ndfit) in SCAN
c
c FITSUB=FULLY AUTOMATIC FIT OF EVERYTHING (ifitmode=0)
c    and FIT -ONE AMPLITUDE (ifitmode=1)
c
c Line 12 of parameters is for point amp histo
c Line 13 is for plot queue
c Line 14 is for zoom (values from expmode)
c Line 15 is for cjump values
c
c Modif 05/20/98 09:11am so that delt(0)=told is set even when interval
c    has already been set bad (needed if we 'redo last fit', to get back
c     to the right place
c Modif 03/29/98 09:22pm so after 'alter guess' the result can be accepted
c without any fitting at all (see dispfit)
c Modif 04/19/96 09:29am to set step() and crtstp() (in common block)
c separately for each parameter (see changes to simplex2 -these values
c will be used if simplex2 called with stpfac=<0).  This is necessitated
c by fact that interval lengths are fitted as time from t=t0, rather
c than as actual lengths.  This means that if there is a short event, a
c long way from t=0, then a 10% step in the time from t=t0 makes
c an enormous difference to guessed length of the short interval! Thus
c for each transition, make the initial step length stpfac (e.g. 0.1)
c times the duration of the shorter of (a) the interval that precedes
c it or (b) the interval that follows it (except last interval, for which
c there is none following it).
c
c This is 2nd attempt to cope with CJUMP data.  Problem is that we have an
c extra interval at both start and end, compared with usual data (viz. time
c from t=0 to first transition, and time from last transition to end of sample).
c The extra initial interval causes chaos with numbering (see FITSUB1), so
c try here to keep the 1st interval separately (in tint1,iampl1,iprops1),
c rather than inserting them into tint(),iampl(),iprops() which are then
c filled as usual.  Then, when a sweep is completed, move all the elements
c up by one place and insert 1st interval into tint(1),iampl(1),iprops(1)
c (or just put them into data file as separate numbers!?)
c
c 07/15/94 06:46pm Zoom of Y axis added for problem with seeing small
c channels in presence of large ones.
c===========NB fix of baseline offset (after accept, or adj baseline)
c======when zoomed is not yet fixed -involves iyoff, sfac3 etc which all change!
c
c (1) When REFIT -ONE AMP used, after fully-automatic fit, DEF_ONE
c  is used to fix guesses and parameter assignments to values
c  suitable for ifitmode=1.  After fit SETTHETA used to reassign parameters,
c  jfix, kfix etc so it looks as though fully automatic fit was used, so
c  rest of calcs can be done as though ifitmode=0
c (2) If FITSUB entered with ifitmode=1 direct from EXPMODE then at present
c  uses same protocol as ifitmode=0 to locate transitions (inc sublevels!)
c  and then uses DEF_ONE and SET_THETA as above
c
c If entered from FITSUB6 will have refit=true (when 'refit: auto' or
c 'refit: 1-amp' chosen in FITSUB6).  May also have newguess=true if
c init guesses were reset in FITSUB6, in which case dt(), delt() are
c set already, but may be for different number of transitions from that
c found by FINDALL loop, so ignore them, and do auto-fit as if not a refit.
c   If initial guesses altered (newguess set true) after fit in this subroutine
c then refitting is done with the new guesses (by return to 100) without
c leaving.
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
c OLD definitions:
c ifitmode=0: automatic detection/fit of all amps and durations
c ifitmode=1: automatic with only one amplitude fitted
c ifitmode=2: automatic with amplitude fixed at LAST fitted value
c ifitmode=3: automatic with amplitude fixed at MEAN fitted value
c ifitmode=4: manual definition of amplitude; only durations fitted
c ifitmode=5: manual sublevel mode; only durations fitted
c
c Modif 07/27/95 12:04pm for variable filter -make ttrans=timsav the time
c when filter is halfway up (so indep of fc), rather than t0, by adding
c tf50 here (value of tf50 will depend on fc for filter).  Zoom/unzoom
c can now be done only in EXPMODE.
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
c FIXED PARAMETERS:
c Fixed amplitudes defined initially in kfix(i), i=0,...,ntrans; i=0 applies
c to current level before the first transition, kfix(i) applies to the
c amplitude of event following the ith transition.  Set kfix(i)=0 if not
c fixed; =1 if fixed at baseline; =2 if fixed at an open level
c (distinguishing these makes allocation of amplitudes easier). Later kfix(i)
c is used to define jfix(j) in form needed for Simplex.
c
c Keep track of baseline: SHUT=true when current level is baseline
c -when shut need look in only ONE direction for transition, but when
c not shut transition may be in either direction
c
c Declarations for temporary data files
	real*8 dfinter,ttrans,ttrans1,tlast,tltemp,tzerod,tsample
	real*8 timsav(nfmax),tcheck
c=	integer*2 ifits(nfmax),ibaselin(nfmax),ioff2
	integer*4 ifits(nfmax)
	integer*2 ibaselin(nfmax),ioff2
c
	integer*2 idata1(2048)	!for data to be fitted (and as temp file)
	integer*2 idata0(2048)	!unscaled version of idata1 for AIVPIX
	real*4 data(2048)
	integer*4 ifreq1(imin:imax),ifreq2(imin:imax),ifreq3(imin:imax)
	integer*2 ix(2048)
c	integer*2 istat(20)	!for search of dy/dt
	integer iact(0:15)
	integer ixn(25)
	real*4 filt1(1024)	!from INSCAN/NORMFILT
	real*4 tint(ntmax),tint1sav
	integer*2 iampl(0:ntmax),iy2,iamp1sav,iamp2sav
	integer*1 iprops(ntmax),iprop1sav,iprop2sav !properties of ith interval (see SCAN)
c arrays to hold graphics overwritten by line
	integer*1 isavbase(320),isavy1(320),isavy2(320)
	integer*1 isavb1(320),isavb2(320),isavb3(320)!store pixels for fit range bar
	real*4 ylevel(50)		!temp storage for transition search results
c	real*4 amp(50)		!temp storage for provisional fitted amplitudes
	real*4 timamax(49)
	real*4 delt(0:50)		!temp storage for transition search results
	real*4 dtrigm(50)
	real*4 ycalc(5120)
c    up to 5120 points for convol trace
	integer*2 ixcalc(5120),iycalc0(5120),iycalc1(5120)
	real*4 ydcalc(2048)		!calc values from SCANFUNC (in common/ycal/)
	real*4 stepamp(50),dt(49),dtsav(49),t0sav
	logical ampdone	!true when prelim amp measurement has been done
	logical opendown,mono,open,shut,shutprev,endshut,shutsav
	logical shut1,flat,shutrun(50)	!for flat runs eg for point amp
	integer i1r(50),i2r(50),krun(50)	!ditto
	integer krun1(50),nrun1(50)		!ditto
	logical opening(50),setshut(0:50),disptran,dispderiv,dispguess
	logical dubious	!amplitude ill-defined (not merely short!) (='assume' in PDP)
	logical refit,newrange,newguess,btest,accepted,sets,sets1
	logical refit1,autoset,setall,dispfit
	logical goback
	logical abort
	logical help			!for QDIALOG etc
	logical fastfit
	logical debug,caplock,discprt,queued,reject,deb
	character*55 helpst(19)		!help strings
	logical fixamp,shutp		!for fixamp
	real*4 ypeak(50)			!for fixamp
	integer mfix(50)			!for fixamp
	character*11 cnum1(50),cnum2(50),cnum3(50),cnum0,cnum
	character*11 cnum11,cnum21,cnum31	!for cjump
	character*14 clong1,clong2	!for double precision numbers in ascii
	character*70 string		!to put result in dialog box
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character*12 tstring
	character*3 microsec
	character ans,ch,UC
c For cjump
c=	integer*2 jumps(njdim),kjumps(njdim)
	integer*2 jumps(njdim)
	integer*4 kjumps(njdim)
	logical cjump,cjumpfst,fitpast
c	character getch
c	integer*2 ktype
c For plot queue
	character ndev*2,cdate*11,adctime*8
c for simplex
	real*4 theta(50)
	integer jfix(50),kfix(0:50)
C For DCMENU
	integer Lb(30)
	character*78 text(30)
c For popup 'fitting . . . '
c	integer*1 array1(:)	!for 'popup' notices
c	allocatable :: array1
c
	common/fitblk/data,if1,if2,ntrans,neval,nevfix,itype,stepamp,y0
	common/ycal/ydcalc	!calc values from SCANFUNC
	common/fixblk/fixamp,tshort,tlong,ypeak,opening,shutp,mfix
c NB keep current step response func in filt1()
	common/filtblk/filt1,nfilt1,dtf1,npfilt,finter,filt(640),filtf
	common/hlp/help		!for QDIALOG etc
	common/popvalx/mxlo,mylo,myhi,ictx,ibkx,icfx	!values for poptext calls
	common/dialval1/ictd,icfd,icbd  !colours for dialog box #1 (text,frame,back)
	common/ampmark/nampmark,iamark(10)
	common/dp/discprt
	common/abt/abort
	common/jump/cjumpfst
	common/steps/step(50),crtstp(50)
	EXTERNAL SCANFUNC,SCNFUNC1
c additions for dfpmin
	real*4 theta0(50)
	logical dodfp
	common/consts/c1,c2,tf50c		!for dsfunc (need copy of tf50)
	common/min/method
	common/km/kmax		!for dsfunc
	logical silent
	common/err/nerr,silent	!for dfpmin1,sfunc,dsfunc
	logical cluster,student
	common/user/student,cluster,iwindows
	EXTERNAL SFUNC,DSFUNC	!for dfpmin1
c
	debug()=caplock()
c
c=	zoomed=izoomfac.gt.1	!now a parameter
	tf50c=tf50		!copy for common/consts/
	c1=5.336446*0.001*filtf		!called k in stepresp.mcd
	c2=-c1/sqrt(3.141593)
	dispfit=.false.
c
	deb=debug()
	sfac31=sfac3*float(izoomfac) 	!converts intermed to pixel units
c
c Define cjumpfst =true for 1st interval in each sweep (ktjump is updated
c only when interval accepted -also want setting of initial 'full amp' to
c be done as though this was not a c-jump).
c For first interval in jump, check first that prev interval was
c the last in prev jump (iprops=64) -should always be so but bug
c (assoc with restart?) sometimes means it is not!
	cjumpfst=.false.
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
	fixamp=.false.          !always false if not auto-fitting!
	isflag=0			!flag for checkfit
	accepted=.false.
	queued=.false.	!so values not printed to disc
c If enter with refit=true from fitsub6, should ignore it
c (see notes at top) and do fit as usual, so set refit=false here (used
c only within fitsub, if range or init guesses altered below)
	if(ifitold.eq.2.or.ifitold.eq.3) then	!arrived for refit from fitsub6
	   refit=.false.	!(otherwise reset later, after setting if1,if2)
	endif
	kfix(0)=0
	do i=1,50
	 jfix(i)=0
	 kfix(i)=0
	enddo
c
	ibasevga=ifixr(basevga)
	tshort=tsfac*trise
	tlong=tlfac*trise
	tclose=tcfac*trise
c
	do j=1,70
	   string(j:j)=char(32)
	enddo
c
c	do i=1,ndfit			!now in SCAN
c	   data(i)=float(idata1(i))		!in intermed units
c	enddo
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
c Also in calc of t0 from trigger point, subtraction of tf50 is a bit crude,
c and tends to make t0 a bit early, so add empirical t0cor, viz
	t0cor=0.25*trise
c
c
c Show the data with IDCPIX=version of DCPIX that uses Ioana's pixel routines
c RPIXEL and WPIXEL.
c	call SETACT(iact,-1)		!no draw
c	iact(ibk2)=15			!unless background col, when draw white
c	call IDCPIX(ix,idata1,ndfit,iact)
c
c SEARCH FOR TRANSITIONS:
c Set NTM=max number of transitions to be sought in data().
c For prelim fit of single full opening (at this point have no amplitude
c yet) set ntm=2
c For FIT-ONE-AMP kmax=ntrans+3 so could make ntm=47, but for FIT-AUTO
c kmax=2*ntrans+1 so ntm=24
	ntm=24
	if(.not.ampdone) ntm=2
c
c Now start loop to search for transitions (note: goes round the
c 'do while' loop ntm+1 times; last time used to estimate, from
c curav, amplitude after last transition)
c
c Modif 09/28/93 03:28pm so that if initially shut then input value is
c not previous BASE, but the mean of 1st 2 data values
C NB THIS ASSUMES THAT WHEN INITIALLY SHUT THE DATA TO BE FITTED WILL BE
c PRECEDED BY A REASONABLE LENGTH OF BASELINE!
c Modif again 02/09/94 05:43pm so that first two points in fit region
c rather than first 2 points on display are used!
c
c When initially open, use previous level (prevlevel) as start point for
c looking for transition.  After GOBACK to a fitting that ended in OPEN
c the value of prevlevel is set in GOBACKS
c
c Check that search for transitions starts after the end of prev fitting
c -if not, start search at last trans + npfilt (latter gets past last trans)
c If trace has been rolled back (without using 'goback n fittings' option)
c the value of if1 here may be greater than ndfit!.  Need some criterion
c for the number of points that must be left for it to be worth trying to fit-
c say minimum of 0.2*ndfit
c===better return to prev fit, rather than just reject here?!
c If refit.and.newrange than if1,if2 already set so skip next bit
	if(.not.(refit.and.newrange)) then
	   if1=1		!first point to be searched
	   if2=ndfit	!=whole display
	   inf1=infirst+if1-1	!abs index of data(if1)
	   inf2=infirst+if2-1	!abs index of data(if2)
	   if(inf2.gt.ndat) if2=ndat-infirst+1	!do not search past end of data!
	   if(cjumpfst) then
	      if(inf1.lt.intzero) then	!do not start fit before t=0
	         inf1=intzero
		   if1=inf1-infirst+1
		   if(if1.lt.1.or.if1.gt.if2-20) then
		     call NEENER()
c		     call BELL(2)
		     call POPTIME(250,130,'ATTEMPT TO FIT BEFORE t=0!',
     &		1,15,12,14,1.0)
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
		  call BELL(1)
		  helpst(1)='ALREADY FITTED PAST HERE'
		  helpst(2)='Attempt to fit rejected-   '
		  helpst(3)=' suggest using GO TO... option'
		  helpst(4)='to ''Go to most recent fit'''
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
c		  call POPTEXT(mxlo,mylo,myhi,helpst,4,14,ibk,14)
		  ikey=2		!reject
		  goto 102
		endif
	   endif
	endif
c
c NB if fit is rejected several times, shut (and hence shutprev) may be
c incorrect, so define from shutsav which is updated only when fit is accepted
c NB do not reset SHUT here after 'REDO last n transitions'.
	if(goback) then
	   goback=.false.
	else
	   SHUT=SHUTSAV		!NB SHUTSAV is updated only when fit accepted
	endif
	if(SHUT) then
	   curlev=0.5*(data(if1)+data(if1+1))
	else   		!initially open
	   curlev=prevlevel        !normal case
	endif
	SHUTPREV=SHUT		!save value of SHUT before 1st transition
					!=value at end of previous fitting
c
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
	   if(abs(curlev-base).gt.scrit*1.5) then		!increase criterion
		call BELL(2) 				!warn that default=Y
	      call QDIALOG(1,
     &	'Channel is OPEN at t=0: O.K.)',defolt,12,cans)
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
c SEARCH FOR TRANSITIONS:
c
	call GETALL(data,ix,dx,nt,ntm,if1,if2,inext,itrig,it1,
     & scrit,curlev,ntrig,navtest,shut,shutprev,open,opendown,
     & kfix,delt,ylevel,base,t0,opening,trise,iautosub,timamax,
     & avamp,ibasevga,tmin,finter,ibk2,vard,dispderiv,iampcrit,
     & npfilt,disptran,ampdone,tclose,dtrigm,rms1,nscale1,iyoffz,
     & derivfac)
c
	if(nt.ge.2) then		!eliminate direct open-open < ampcrit
	   t0s=t0
	   call CHKGUESS(nt,t0,delt,ylevel,kfix,opening,scrit,
     &    base,iampcrit,shutprev,prevlevel)
	   if(t0.ne.t0s) then		!t0 altered by chkguess, so fix1 it1
		it1=2+ifix(t0/finter)
	   endif
	endif
	if(nt.eq.0) then
	   call BELL(1)
	   call POPTIME(200,150,
     & 'NO TRANSITIONS FOUND -TRY AGAIN',1,14,12,14,1.0)
	   ikey=2		!reject
	   goto 102
	endif
	if(nt.gt.24) then		!2*24+1=49
	   if(.not.fastfit) then
		call NEENER()
		call POPTIME(200,150,
     &	 'TOO MANY TRANSITIONS -TRY AGAIN',1,14,12,14,1.0)
		ikey=2		!reject
		goto 102
	   else	!for fastfit case adjust # of transitions automatically
cc	      call POPTIME(200,150,
cc     &    'TOO MANY TRANSITIONS -RESET TO 24',1,14,12,14,1.0)
cc	      nt=24
c===redefine number of points to display -need longish interval at end of
c  fit range (pref shut if possible)
c		dx=639.0/float(ndfit-1)		!use ndisp (not ndisp1) here
c		dxsav=dx
c		do i=1,ndisp
c		   ix(i)=ifixr(float(i-1)*dx)	!=0 for 1st point
c		   ixsav(i)=ix(i)
c		enddo
c===
	   endif
	endif
c
c ALL TRANSITIONS ON DISPLAY NOW FOUND.
c
c
c Adjust fit range and draw fit bar.
c (1) If SHUT (ie last trans was to shut state so shut at RHS of display)
c     then avoid fitting too many shut points by limiting the fitted region
c     to NSHUTFIT (eg 30) shut points after last transition.
c      Mark the fitted region and give chance to adjust it if req.
c (2) if ntm (=max # of transitions to be located) is reached then ntrans=nt
c	will actually be ntm+1 after the 'do while' loop;  -in this case
c	value of ntrans must be reduced to ntm now, and need to reset if2
c	so that it is before the (nt+1) the transition, which is not to
c	be fitted, ie set if2=inext
c  (3) Want to avoid trying to fit any incomplete transition close to right
c     edge of screen; previously search went only up to ndfit-npfilt=if2, but
c     need to exclude any such incomplete transition from fit range too, but
c     also want to use points right up to last for fitting if there is no
c     transition too close to end.  Therefore search right up to point #ndfit,
c     but correct if1,if2 below if a transition is found too close to right
c    edge of screen.
c NB Last transition was completed at point #inext
c  and next to last was at point #inextprv
c At this point SHUT refers to state AFTER the current FITTING
c At this point SHUTPREV refers to state BEFORE the current FITTING
c NB last time round loop will increment nt to ntm+1 so correct here
	if(nt.le.ntm) then
	   ntrans=nt
	else if(nt.eq.ntm+1) then
	   ntrans=ntm
	endif
c
	if(refit.and.newrange) then	!do not change if1,if2
	   inf1=infirst+if1-1	!abs index of data(if1)
	else					!adjust if1,if2
c       Fix if2 for case where ntm reached (ntrans=ntm set above) (case 2 above)
         if(nt.eq.ntm+1) then
		if(if2.gt.itrig-npfilt) then
		  if2=itrig-npfilt    !end fit range before last trigger point
		endif
	   endif
c       Fix for case where last trigger is near to right hand edge
	   if(ndfit-itrig.lt.npfilt) then
		if(if2.gt.abs(ndfit-npfilt)) then
		  if2=ndfit-npfilt    !end fit range before last trigger point
		  ntrans=ntrans-1	    !omit last transition
		  if(ntrans.eq.0) then
		     call BELL(1)
		     call POPTIME(200,150,
     &	      'NO TRANSITIONS FOUND -TRY AGAIN',1,14,12,14,1.0)
		     shut=shutprev	!reset
		     ikey=2		!reject
		     goto 102
		   endif
		endif
	   endif
	   if(cjumpfst) goto 22	!do not change if1
	   if(SHUTPREV.and.(it1-if1).gt.nshutfit) then  !reset start of fitted region
		if1=it1-nshutfit
	   endif
c Check that revised first fitted point is still after the end of prev fitting
	   inf1=infirst+if1-1	!abs index of data(if1)
	   fitpast=inf1.lt.infit
	   if(cjump) fitpast=fitpast.and.njump.le.njfit
	   if(fitpast) then
		if1=if1+(infit-inf1)+npfilt
	   endif
	   if(if1.lt.1) if1=1
22	   continue
	   if(SHUT.and.(if2-inext).gt.nshutfit) then	!reset end of fitted region
		if2=inext+nshutfit
	   endif
	   if(if2.gt.ndfit) if2=ndfit
	   if(if2.lt.if1) if2=if1+npfilt
	   inf2=infirst+if2-1	!abs index of data(if2)
	   if(inf2.gt.ndat) if2=ndat-infirst+1	!do not fit past end of data!
	endif
c
c Now if1,if2 set (or skipped) can reset REFIT
	refit=.false.
c
	call FITBAR(ix,dx,if1,if2,ndfit,yd1,yd2,basevga,
     & opendown,.false.,isavb1,isavb2,isavb3)	!draw only
c
c Interpolate the step resp function at suitable intervals for this display
c===NEWFILT not nec really -used only for DISPLAY -better just omit
c===points as req from the 1 us interp version in filt1?
	call NEWFILT(filt1,nfilt1,dtf1,ntrans,ndfit,finter,
     & filt,nfilt,dtf)
c
c Calc step amplitudes in stepamp() (in prev integer version was called IA())
	do i=1,ntrans
	  stepamp(i)=ylevel(i+1)-ylevel(i)		!in intermed units
	enddo
c
	y0=ylevel(1)	!initial value
	y0sav=y0		!for FIXGUESS
c
c= Define time intervals for CONV in DT() -NOW IN SETPARAM

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
c===disp init guess was here
c
c Mark region to be fitted (and adjust it if req) before fitting
c NOW DO LS FIT
c Parameters:
c Try to make param not highly correlated by using transition times
c all measured from t=0 in theta, rather than using diffs
c theta(1)= t0 = time in microsec from 1st point on display TO 1st transition
c theta(2)= time in microsec from 1st point on display TO 2nd transition
c . . .
c theta(nt)= time in microsec from 1st point on display TO (nt)th transition
c
c AND, when ifitmode=0
c theta(nt+1)='amplitude' (floating Y units) before 1st trans
c theta(nt+2)='amplitude' (ditto) between 1st and 2nd trans
c theta(nt+3)='amplitude' (ditto) between 2nd and 3rd trans
c . . .
c theta(2*nt)='amplitude' (ditto) between (nt-1)th and nt'th transition
c theta(2*nt+1)='amplitude' (ditto) after (nt)th transition
c Total number of param=2*nt+1  (some of these may be fixed!)
c
c OR,  when ifitmode=1.
c  (1) when shut before and after:
c theta(nt+1)=open level
c theta(nt+2)=baseline before 1st trans
c theta(nt+3)=baseline after last transition
c  (2) when shut initially, open at end
c theta(nt+1)=open level
c theta(nt+2)=baseline before 1st trans
c  (3) when open initially, shut at end
c theta(nt+1)=open level
c theta(nt+2)=baseline after last trans
c  (4) when open at each end
c theta(nt+1)=open level
c theta(nt+2)=baseline (FIXED parameter)
c
c Initial guesses
c  Now call SETPARAM to define dt(),dtsav(),kmax,theta(1,..,kmax),kfix,jfix,
c on basis of values for ntrans,delt(),t0,y0,kfix,shutprev,prevlevel,stepamp
c  define theta,dt etc
104	continue		!return to re-fit after ALTER GUESS
	call SETPARAM(ntrans,delt,t0,y0,kfix,shutprev,
     &  prevlevel,stepamp,dt,dtsav,kmax,theta,jfix)
c Modif 04/19/96 09:29am to set step() and crtstp() (in common block)
c separately for each parameter (see changes to simplex2 -these values
c will be used if simplex2 called with stpfac=<0).  This is necessitated
c by fact that interval lengths are fitted as time from t=t0, rather
c than as actual lengths.  This means that if there is a short event, a
c long way from t=0, then a 10% step in the time from t=t0 makes
c an enormous difference to guessed length of the short interval! Thus
c for each transition, make the initial step length stpfac (e.g. 0.1)
c times the duration of the shorter of (a) the interval that precedes
c it or (b) the interval that follows it (except last interval, for which
c there is none following it).
	stpfac=0.2
	if(ntrans.gt.1) then
	   do i=1,ntrans
		if(i.eq.1) then
		   s=AMIN1(theta(1),delt(0))
		   step(i)=stpfac*s
		   crtstp(i)=errfac*s
		else if(i.lt.ntrans) then
		   s=AMIN1(delt(i-1),delt(i))
		   step(i)=stpfac*s
		   crtstp(i)=errfac*s
		else if(i.eq.ntrans) then
		   s=delt(i-1)
		   step(i)=stpfac*s
		   crtstp(i)=errfac*s
		endif
	   enddo
c set step() for the amplitude parameters as before
	   do i=ntrans+1,kmax
		step(i)=stpfac*theta(i)
		crtstp(i)=errfac*theta(i)
	   enddo
c Call simplex2 with stpfac negative so above values are used in SIMPLEX2
	   stpfac=-stpfac
	endif
c
c NB if not refit then iampl() not defined for present data, but needed
c for def_one
	if(ifitmode.eq.1.and.(.not.refit)) then
	  call DEF_ONE(kt,t0,delt,dt,shutprev,shut,jfix,kfix,ntrans,
     &   scrit,base,prevlevel,avamp,theta,itype,y0,stepamp,kmax,
     &   refit,iampl,ntmax,dtrigm,tshort)
	   if(ntrans.le.0) then
		ikey=2		!reject
		goto 102
	   endif
	   goto 100
	endif
c Insert section here to define FIXAMP=true if auto-fit of a burst that
c contains both long and short openings should try to constrain amplitude
c of each short opening to be equal to that of the nearest long opening,
c rather than equal to AVAMP.  Problem is that really need to know which
c amplitudes are 'open' and which are 'shut'.  But openings judged, from init
c guesses, to be so short that they need to be fixed have kfix(nt)=2 set
c already -use this as criterion to set FIXAMP=.true. for now.  But try
c to make it smarter by revising interactively based on current estimates
c of durations of openings
c Modif 03/22/94 04:51pm in SCANFUNC so short opening is only counted
c as such when it is followed by a shutting. If there are no opening-shutting
c pairs that describe a short opening then can set fixamp=false here -problem
c is that do not know if it is short until fitting done (judged separately
c for each iteration in scanfunc).  But if all such openings are longer
c than say 2*tshort then can probably safely set fixamp=false now.
103	continue	!return to refit after CHECKFIT
	fixamp=.false.
	if(ntrans.ge.3) then
	   do nt=1,ntrans-1	!NB fixamp true only if ntrans=>3
		if(opening(nt).and.(.not.opening(nt+1)).and.
     &		delt(nt).lt.2.*tshort) then
		   fixamp=.true.
		   goto 1041		!one eligible opening found so set fixamp
		endif
	   enddo
1041	   continue
	   if(.not.fixamp) goto 100
	   shutp=shutprev		!copy for common
c
c Define ypeak(nt) for use in SCANFUNC.  This is lowest ('most open') point
c in data between transition #nt and #nt+1.  From init guesses, first transition
c is at time t0 from 1st point on display, and then transition #i is at
c time t0+delt(i-1) (i=2,...,ntrans).  Also time=float(i-1)* finter
c so index=1+ifixr(time/finter)
c=======THIS VERSION IS FOR OPENDOWN=TRUE ONLY
	   do nt=1,ntrans-1
		if(opening(nt)) then
		   dext=1.e36          !extreme value=minimum
		   if(nt.eq.1) then
			t1=t0
		   else
		      t1=t0+dt(nt-1)+tf50
		   endif
		   t2=t0+dt(nt)+tf50
		   i1=1 + ifixr(t1/finter)
		   i2=1 + ifixr(t2/finter)
		   if(i1.lt.1) i1=1
		   if(i2.gt.2048) i2=2048
		   do i=i1,i2
			if(data(i).lt.dext) then
			   dext=data(i)	!lowest point on screen
			endif
		   enddo
		   ypeak(nt)=dext
c========mark peaks for debug
c		   iy2=ifixr(dext)
c		   ix2=(i1+i2)/2
c		   call SCALINT(iy2,1,nscale1,iyoffz)
c		   call HLINE(ix(ix2)-4,ix(ix2)+4,iy2,15)
		endif
	   end do
	endif
c
c Return here to refit with 1 amplitude only
100	continue
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
c==	   call SCALINT(iycalc,ke,nscale1,iyoffz)
	   call SCALINT1(iycalc0,ke,nscale1,iyoffz,iycalc1)
c Display the convolved trace for init guesses, starting at time of 1st transition
	   call SETACT(iact,-1)		!no draw
c	   iact(ibk2)=14			!unless background col, when draw yellow
	   iact(ibk2)=11			!unless background col, when draw blue
	   call IDCPIX(ixcalc,iycalc1,ke,iact)
	endif
c
c Prepare for SIMPLEX
c	nevfix=50	!use fixed data set after 50 func eval
	nevfix=-1	!fixed data set
c
c	errfac=1.e-3
c	errfac=0.01		!put errfac in defaults?
c 	errfac=0.005	!errfac now a parameter
	delmin=-1.		!do not use delmin for convergence
c=	stpfac=0.1		!0.02 and 0.3 both worse in one test -NOW SET ABOVE
c==	stpfac=0.05		!seems to avoid some problems ?
c	nfix=0		!no param fixed
c standard value of confac (=contraction factor in simplex) is 0.5, but
c crude tests show 0.2 better when guess is good, and little worse
c when guess is bad -make it a default parameter?
c In another example, with poor guesses, confac=0.25 was disastrous, but
c 0.5 was OK so restore 0.5
c	confac=0.25
c	confac=0.5	!now set in paramwin
	ndisp=-1000	!silent
c	ndisp=0	!no iterations on disc
c	ndisp=20	!every 20th on disc (if discprt=true)
c	ndisp=1
	nevmax=5000
	neval=0		!for common in scanfunc (use neval1 for simplex call)
c	print 523,errfac
c523	format(' ERROR=guess*',g13.6,/,
c     & ' Print every Nth estimate [20] :  N = ')
c	call INPUTi(ndisp)
c Popup notice to say 'fitting' (this could slow down very fast fits?
c -popup only for ntrans>1 or >2?) -better just use WDIALOG?
	call WDIALOG(1,'Fitting now (abort with F2). . .',-icw1)
c	irestrt=1		!allow one restart in simplex, at most
	irestrt=0		!allow no restarts in simplex
c minmeth=1 Simplex only
c minmeth=2 Simplex initially, then dfpmin
c minmeth=3 Dfpmin only
c
c	if(minmeth.eq.0) minmeth=2		!temporary
	if(minmeth.eq.2) nevmax=itsimp
c
	if(ifitmode.eq.0) then
	   dodfp=minmeth.eq.2.or.minmeth.eq.3
	   if(minmeth.eq.1.or.minmeth.eq.2) then
	     call SIMPLEX3(kmax,theta,stpfac,errfac,neval1,nevmax,
     &     smin,SCANFUNC,Ndisp,jfix,delmin,confac,irestrt,iconv)
	   endif
c	   smin=SCANFUNC(kmax,theta)		!check for debug
c	   smin1=SFUNC(theta)		!check for debug
	   if(minmeth.eq.2.and.neval1.ge.nevmax) then
  	     call WDIALOG(1,' -swapping to DFPMIN now',-11)
		if(minmeth.eq.2) then		!save in case better than dfpmin
		   do i=1,kmax
			theta0(i)=theta(i)
		   enddo
		endif
	   else
		dodfp=.false.	!simplex converged so no need for dfp
	   endif
	   if((minmeth.eq.2.and.dodfp).or.minmeth.eq.3) then
		gtol=1.e-4
		stpmx=0.1
c		ndisp=0
		ndisp=-1000	!silent
		method=0	!method for derivs in dsfunc
      	call DFPMIN1(theta,kmax,gtol,iter,smin1,sfunc,dsfunc,
     &       stpmx,jfix,ndisp)
c		smin1=SFUNC(theta)		!check for debug
		if(minmeth.eq.2) then
c Calc improvement in log-likelihood produced by dfpmin1, over simplex when
c both use (NB SSD are div by if2-if1+1 in sfunc, scanfunc)
c In DFPMIN (other error codes from DSFUNC?)
c===insert check for adjacent equal amps or zero lenght intervals in
c===output from simplex before going on to dfpmin1
c nerr=1	Reached itmax					Premature end
c nerr=2	Disc negative in LNSRCH				disc set to 0.0
c nerr=3	Overflow of b*b in LNSRCH			Run aborted
		   k=0
		   do i=1,kmax
			if(jfix(i).eq.0) k=k+1
		   enddo
		   np=if2-if1+1
		   s2=smin1/float(np-k)		!error varaince
	         dll=abs(0.5*(smin-smin1)*float(if2-if1+1))/s2
	         call DCFORMAT(dll,8,2,cnum)
		   n=NBLANK(cnum)
		   if(smin1.gt.smin) then
			do i=1,kmax
			   theta(i)=theta0(i)	!revert to simplex results
			enddo
		      call WDIALOG(1,
     &	' -Simplex was better fit by L = '//cnum(1:n),11)
		   else
		      call WDIALOG(1,
     &	' -DFPMIN improved fit by L = '//cnum(1:n),11)
		   endif
		   if(nerr.ne.0) then
			call TONE(110,30)	!low note!
			call INTCONV(nerr,cnum)
		      n=NBLANK(cnum)
		      call WDIALOG(1,
     & ' -DFPMIN returned error # '//cnum(1:n),icw1)
			call POPTIME(200,150,
     &		 'DFPMIN aborted',1,14,12,14,0.5)
c    & ' -DFPMIN returned error # '//cnum(1:n)//': hit any key',icw1)
c			call ANYKEY()
		   endif
		endif
	   endif
	else if(ifitmode.eq.1) then
	   call SIMPLEX3(kmax,theta,stpfac,errfac,neval1,nevmax,
     &     smin,SCNFUNC1,Ndisp,jfix,delmin,confac,irestrt,iconv)
c       Redefine theta so elements are as for ifitmode=0, for calcs below
	   call SETTHETA(theta,kmax,jfix,kfix,itype,ntrans)
	endif
	if(abort) then
	   call BELL(2)
	   ans='Y'
 	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     &	'FITTING ABORTED 	-Reject it now',defolt,icw,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'Y') then
		SHUT=shutprev	!restore value
		goto 999	!reject
	   endif
c	   call POPTIME(200,150,'FITTING ABORTED',1,14,12,14,1.0)
	endif
c
c If fixamp used to constrain amplitude of a short opening to be same
c as that of an adjacent long one (in SCANFUNC) then reset final values
c to be equal here (averaging over vertices before leaving SIMPLEX may
c make them not exactly equal -in fact may be grossly different!
	if(fixamp) then
	 do i1=1,ntrans-1
	   if(mfix(i1).ne.-1) then	!amp #i1 has been constrained
		 i2=mfix(i1)
		 THETA(ntrans+i1+1)=THETA(ntrans+i2+1)
	   endif
	 enddo
	endif
c FIT DONE -DISPLAY THE RESULT
c===(could record values of ycalc() from SCANFUNC and display them here
c but they are calc only at times of data points, so would need to be
c interpolated to get decent looking trace, so, for now, recalculate
c the response function entirely using CONV1)
c
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
c so better use real*8 for ttrans, tlast.
c Modif 07/27/95 12:04pm for variable filter -make ttrans=timsav the time
c when filter is halfway up (so indep of fc), rather than t0, by adding
c tf50 here (value of tf50 will depend on fc for filter).
c    Problem arises when t0 is erroneously fiited by simplex to a value that
c is beyond right hand edge of screen: this gives a negative kemax, and hence
c negative ke, when fitted curve is calculated below, which causes a
c non-Lahey crash.  Better catch this here!
105	continue
c	t0max=float(ndfit-1)*finter
	t0max=float(ndfit-4)*finter	!leave margin of 3 points before RH edge
	t0=theta(1)
	if(t0.gt.t0max.or.t0.lt.0.) then
	   call BELL(1)
	   call POPTIME(200,150,'FIT OFF SCREEN: REJECTED',
     &    1,15,12,14,1.0)
	   ikey=2		!reject
	   goto 102
	endif
c===	ttrans=dble(t0) + dfinter*dble(float(infirst-1))
	ttrans=dble(t0) + tf50 + dfinter*dble(float(infirst-1))
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
c***	if(kt.gt.0.and.(.not.BTEST(iprops(kt),3))) then !bit 3='8' set for unusable
	if(kt.gt.0) then
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
c next lines moved to after 'check for adjacent zero amps'
c	   tint1sav=(delt(0) - sngl(tzerod))*1.e-3	!save latency (in ms)
	   iprop1sav=IBSET(iprops(kt+1),5)   !first in sweep; set bit 5 ='32'
	   do i=0,ntrans-1
		if(delt(i).lt.0.) negflag=1
	   enddo
	endif
c then amplitudes:
	j1=ntrans+1
	y0=theta(j1)		!initial level before trans #1
	y1=theta(2*ntrans+1)	!final level after transition #ntrans
	do j=1,ntrans
	  stepamp(j)=theta(j1+1)-theta(j1)
	  j1=j1+1
	enddo
c
c Display fit BEFORE checking shut levels
c  First erase init guess (if drawn)
	if(dispguess) then
	   call SETACT(iact,-1)		!no draw
c	   iact(14)=ibk2			!unless yellow, when draw background col
	   iact(11)=ibk2			!unless blue, when draw background col
	   call IDCPIX(ixcalc,iycalc1,ke,iact)
	endif
c Calculate convolution in ycalc(1) to ycalc(ke)
	call CONV1(ycalc,y0,ntrans,DT,ke,filt,nfilt,stepamp,dtf)
	x0=t0*dxf/dtf
c modify next bit so fitted curve in xcalc, ycalc includes the flat bit
c at either end (makes it easier to delete calc curve!)
c Check that last point is on screen (ixcalc <=639); if not, reduce ke further
c (Could also reduce so that calc curve does not go beyond fitted region,
c  i.e. use ix(if2) rather than 639 here).
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
c===debug===
c	call VLINE(ixcalc(1),iyd1,iyd2,15)	!line at x0
c	if(ntrans.gt.1) then
c	   do i=2,ntrans
c		t=t0+dt(i-1)
c		ix1=ifixr(t*dxf/dtf)
c		call VLINE(ix1,iyd1,iyd2,15)	!line at each trans
c	   enddo
c	endif
c===end debug
c ycalc() is in units as given by readsec, without nscale or offset -must
c scale for display
	icc=12	!red for fitted trace
	if(mono) icc=15
      call SCALINT1(iycalc0,ke,nscale1,iyoffz,iycalc1)
c	call SETACT(iact,icc)		!draw red
	call SETACT(iact,-1)		!no draw
	iact(ibk2)=icc			!unless background col, when draw red
c Now extend calc curve to cover whole fitted range from data(if1) to
c data(if2).  These regions are flat, so can draw with HLINE
c Use RDGWIN1 to keep graphical data overwritten by HLINE to allow restoration
c when red fitted line is deleted
	call RDGWIN1(ix(if1),iycalc1(1),ixcalc(1),iycalc1(1),isavy1)
	call RDGWIN1(ixcalc(ke),iycalc1(ke),ix(if2),iycalc1(ke),isavy2)
	if(ix(if1).lt.ixcalc(1)) then
	   call HLINE(ix(if1),ixcalc(1),iycalc1(1),icc)	!before start of resp func
	endif
	if(ixcalc(ke).lt.ix(if2)) then
	   call HLINE(ixcalc(ke),ix(if2),iycalc1(ke),icc)   !after end of resp func
	endif
	call IDCPIX(ixcalc,iycalc1,ke,iact)			!draw resp function
c
c Calculate amplitudes of transitions -provisional values in amp(i)
c for i=1,2,...,ntrans.  (Note that amp(i) was used for other things earlier)
c NB: If pre and post levels are estimated, there will be ntrans+1 estimated
c levels, BUT the first of them will already have been estimated by the
c previous fitting.
c  If previous fitting ended in SHUT state (iampl(kt)=0) then
c not changed now (iampl(kt)=0 still, whatever the initial level estimated
c here).
c  If previous fitting ended in OPEN state, then there is a slight
c problem -the open level will be estimated again here, and will not be
c exactly the same!  Have several options
c	(1) Do NOT fit initial level if previous fitting ended in OPEN
c	   state, but fix it at the previously estimated value
c	(2) Estimate initial level and use average of this and value
c	   from previous fitting; problem with this is that although this
c	   average value can be superimposed on the current data, it cannot
c	   be seen how well it fits the data used in previous fitting
c USE METHOD 1 FOR NOW! so if fitting ends in open state, save the level
c	  fitted for use in next fitting
c=======need some sort of interp in case where shut at both start and end
c  (1) Calculation assuming initial baseline valid for all amplitudes
c
c=====surely next bit should be AFTER redef of 'base' -but need to know
c whether 'shut' to recalc 'base' so circular!!
c NOW reset SHUT for final level
c	SHUT=abs(y1-base).lt.scrit		!final level is SHUT
	if(abs(y1-base).lt.0.5*scrit) then
	   SHUT=.true.		!final level is SHUT
	else if(abs(y1-base).gt.2.0*scrit) then
	   SHUT=.false.		!final level is OPEN
	else		!borderline case!
	   ans='Y'
21	   call BELL(1)
 	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     &	'Final level is SHUT (R or 2=reject fit; F1=help) : O.K.)'
     &	,defolt,12,cans)
	   if(help) then
		helpst(1)='The final level differs from previous baseline:'
		helpst(2)=' -if final level is SHUT answer Y (default)'
		helpst(3)=' -if OPEN then answer N'
		helpst(4)=' -type R or 2 to reject fit and try again'
c		call POPTEXT(ixlo,iylo,iyhi,string,n,ic,ibk,icf)
	   	call POPTEXT(mxlo,mylo,myhi,helpst,4,ictx,ibkx,icfx)
		goto 21
	   else
		call GETINPa(cans,ans)
	   endif
c	   SHUT=ans.eq.'Y'
	   setshut(ntrans)=.false.
	   if(ans.eq.'Y') then
		SHUT=.true.
	      setshut(ntrans)=.true.
		sets1=.true.	!set true if ANY interval reset to shut
	   else if(UC(ans).eq.'R'.or.UC(ans).eq.'2') then
		SHUT=shutprev	!restore value
		goto 999	!reject
c		ikey=2	!reject fit
c		goto 155
	   endif
	endif
	endshut=SHUT	!record value to set iampl=0 later
c NB do not need prevsav! -assign new prevlevel ONLY when fit accepted below
c If OPEN at end of fitting, save level for use in next fitting (save anyway)
c
c Define tentative amplitudes in amp(i).  First revise current baseline
c estimate -again this would be better AFTER CHKSHUT etc, but value for BASE
c is needed, so must do this first!
c====NB (1) Rather than averaging pre- and post-baseline, could use
c		sloping baseline to estimate amplitudes
c	  (2) Could try to utilise info about baseline from shut levels
c		(if not fixed) within the fitting, rather than using only
c		the initial and/or final levels
	basesav=base				!to restore baseline if rejected
	if(shutprev.and.shut) then		!shut at both start and end
	   base=(theta(ntrans+1)+theta(2*ntrans+1))/2. !average pre- and post-baseline
	else if(shutprev.and.(.not.shut)) then	!initially shut
	   base=theta(ntrans+1)			!use pre-baseline
	else if(shut.and.(.not.shutprev)) then	!shut at end but not start
	   base=theta(2*ntrans+1)			!use post-baseline
	endif
c (if shut at neither end, then current value for BASE is used unchanged)
c
c Now define tentative amplitudes: will be simpler to put them in the
c final array, iampl(k), here (as done on PDP) so baseline values appear
c as exactly 0, and to ease handling of amplitudes of events completed
c by the first transition in current fitting.  If fit is rejected these
c values will get overwritten (or could reset to 0 explicitly if fit
c is not accepted).  All amplitudes near baseline must be checked (in
c same way as post-baseline) to see if they are SHUT, so iampl() can
c be set to 0 exactly
c For cjump case, save initial amplitude here, for use later
	if(shutprev) then 	!have already confirmed whether shut at t=0
	   iamp1sav=0
	else
	   a=theta(ntrans+1) - base 	!level before 1st transition
	   iamp1sav=int2(ifixr(a))
	endif
c
c===start of new bit
c New method for assigning amplitudes 11/30/96 01:58pm!  Before all were taken
c with respect to mean of initial and final baseline (or one or the other
c if both not determined).  Now calculate amplitudes as difference between
c open level and mean of the shut levels on each side of it, provided the
c shut level is at least nbasemin points long.  This differs from original
c method only if the fit contains at least one long shut period within it, so
c must have ntrans.ge.3.  Problem, as always, is to know which intervals ARE
c shut, in order to choose appropriate shut intervals for defining amplitudes
c Better go through values that are fixed shut, and CHKSHUT, so values
c deemed to be shut already have iampl=0 set
c
c Set the iampl()=0 only
	j1=ntrans
	k=kt
	sets1=.false.	!set true if ANY interval reset to shut
	autoset=.false.	!set true if interval set shut without asking
	nar=0			!counts arrows
c Insert here a bit to check all shut levels at once -if not ALL accepted
c then check individually as before
	do i=1,ntrans
	   setshut(i)=.false.
	   j1=j1+1		!index for theta =ntrans+1, ntrans+2,...
c       NB y1 value is needed for CHKSHUT
	   y1=theta(j1+1)	!=theta(ntrans+2) first time (level after 1st trans)
	   k=k+1		!index for iampl =kt+1, kt+2,...,kt+ntrans
	   iampl(k)=9999
	   if(kfix(i).eq.1) then
		iampl(k)=0		!value was fixed at baseline
		iprops(k)=IBSET(iprops(k),1)	!amplitude fixed; set bit 1 ='2'
	   else if(kfix(i).eq.2) then
		iprops(k)=IBSET(iprops(k),1)	!amplitude fixed; set bit 1 ='2'
	   else
		if(i.lt.ntrans) then		!last value set below via endshut
		   imode=1		!draws arrows but does not returm 'shut' or 'sets'
		   call CHKSHUT(y1,base,scrit,k,i,delt,t0,dxf,dtf,
     &	   tshift,ibk2,calfac2,rms1,avamp,SHUT,sets,autoset,
     &	   opendown,reject,nar,ixn,imode)
		   if(reject) then
			reject=.false.
			shut=shutprev
			goto 999
		   endif
		endif
	   endif
      enddo
	setall=.false.
	if(nar.ge.1) then
	   ans='Y'
	   call TONE2(333,264,20)
 	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     &   'Set ALL levels with red arrow as SHUT: O.K. (2=reject)'
     &   ,defolt,12,cans)
	   call GETINPa(cans,ans)
c   erase arrows
	   do i=1,nar
		call VARROW(ixn(i),445,380,ibk2)	!erase arrow
	   enddo
	   if(ans.eq.'Y') setall=.true.
	   if(UC(ans).eq.'2') then
		SHUT=shutprev	!restore value
		goto 999	!reject
	   endif
	endif
c Now repeat: must call chkshut again to select which levels are doubtful
c but if setall=true then it does not ask (imode=2) but returns shut=true
c and sets=true (for doubtful ones only). If setall=false then chkshut
c asks about each doubtful shut level separately (imode=0) as before.
	j1=ntrans
	k=kt
	sets1=.false.	!set true if ANY interval reset to shut
	autoset=.false.	!set true if interval set shut without asking
	n=0
	do i=1,ntrans
	   setshut(i)=.false.
	   j1=j1+1		!index for theta =ntrans+1, ntrans+2,...
c       NB y1 value is needed for CHKSHUT
	   y1=theta(j1+1)	!=theta(ntrans+2) first time (level after 1st trans)
c	   amp(i)=y1-base
	   k=k+1		!index for iampl =kt+1, kt+2,...,kt+ntrans
c	   iampl(k)=int2(ifixr(amp(i)))
	   iampl(k)=9999
	   if(kfix(i).eq.1) then
		iampl(k)=0		!value was fixed at baseline
		iprops(k)=IBSET(iprops(k),1)	!amplitude fixed; set bit 1 ='2'
	   else if(kfix(i).eq.2) then
		iprops(k)=IBSET(iprops(k),1)	!amplitude fixed; set bit 1 ='2'
	   else
		if(i.lt.ntrans) then		!last value set below via endshut
		   if(setall) then
			imode=2	!so chkshut returns sets=true, shut=true
		   else
			if(nar.gt.1) then
			   imode=0	!chkshut asks
			else if(nar.eq.1) then
			   imode=3	!no need to ask
			endif
		   endif
		   call CHKSHUT(y1,base,scrit,k,i,delt,t0,dxf,dtf,
     &		tshift,ibk2,calfac2,rms1,avamp,SHUT,sets,autoset,
     &	      opendown,reject,n,ixn,imode)
		   if(reject) then
			reject=.false.
			shut=shutprev
			goto 999
		   endif
		   if(shut) iampl(k)=0
		   if(sets) then
		     setshut(i)=.true.
c//===do we want to set ylevel=base here -what is it used for?
c//===ylevel is used in checkfit,setsubl, modsubl -but checkfit defines
c//===ylevel, apart from ylevel(1)!!
c///		     ylevel(i+1)=base
		     sets1=.true.	!set true if ANY interval reset to shut
		   endif
		endif
	   endif
      enddo
	if(endshut) then		!level after last transition already checked above
	   shut=.true.		!set final value now
	   iampl(kt+ntrans)=0
c//===do we want to set ylevel=base here -what is it used for?
c///	   ylevel(ntrans+1)=base
	else
	   shut=.false.		!set final value now
	endif
c
c Now go through these again to find which shut times are long enough
c to define amplitudes -for this purpose, use ydcalc() (from scanfunc, sfunc
c etc to define flat sections: now locate runs of flat points here, in same
c way as after 'accept', but here we require nbasemin points to constitute
c a run, rather than 2 points
c List possibilities
c
c  (1) shutprev and shut: always have initial and final shut
c
c    ntrans     shuttings   max extra
c		    at ends	    shuttings pos
c	2	   	2		0
c	3				0
c	4				1
c	5				1
c	6                       2
c	7                       2
c	8				3
c
c  (2) shutprev and (not shut) -have initial (but no final) shut points
c    or (not shutprev) and shut -have final (but no initial) shut points
c
c     ntrans     shuttings   max extra
c		    at ends	    shuttings pos
c	2	   	1		0
c	3				1
c	4				1
c	5				2
c	6                       2
c	7                       3
c	8				3
c
c  (3) (not shutprev) and (not shut) -no shut points at either end
c
c    ntrans     shuttings   max extra
c		    at ends	    shuttings pos
c	2	   	0		1
c	3				1
c	4				2
c	5				2
c	6                       3
c	7                       3
c	8				4
c
c Thus use original method if there are no extra shuttings that can be
c used to estimate baseline the new way i.e. when
c	(1) ntrans=1
c	(2) shutprev and shut and ntrans=2 or 3
c	(3) shutprev and (not shut) or vice versa, and ntrans=2
c	(4) any other case MAY have 1 or more shuttings, but use
c		original method if there are not any
c Obviously need to count and locate the 'extra' shut times that are
c available in the current fit before doing anything.  The 1st and last
c state (shut, not shut) should agree with values of shutprev and shut.
c First check this. The tentative numbering of the iampl() being defined
c is iampl(kt+1) to iampl(kt+ntrans)
	if(shutprev.and.iampl(kt).ne.0) then
	   call BELL(4)
	   call WDIALOG(1,'shutprev error in fitsub',12)
	endif
	if(shut.and.iampl(kt+ntrans).ne.0) then
	   call BELL(4)
	   call WDIALOG(1,'shut error in fitsub',12)
	endif
c First locate all runs (inc start and end) of 2 or more points in ydcalc()
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
c	         shut1=abs(ydcalc(i1)-base).lt.scrit !run is of shut points
	         call GETSH1(shut1,k,shutprev,shut,t0,dt,i1,i2,kt,
     &     	    finter,ntrans,iampl,ntmax)	!get shut1
		   shutrun(nrun)=shut1		!record if run is shut
		   krun(nrun)=k		!index of iampl() etc corresp to this run
		endif
		i1=i		!start new run
		i2=i		!start new run
	   endif
	enddo
c Locate all shuttings that are appropriate for estimation of amplitudes.
c These include initial and final shut levels, regardless of the number
c of points in them (is this OK??), plus
c any 'extra' shuttings that may be present -must select shut runs,
c other than start or end, that are longer than nbasemin points long
c -if any found, colour them blue.  Also form weighted average of the
c fitted levels to each valid shut run (if any) to make a new value of BASE here
	nsrun=0
	s=0.0
	den=0.0
	do i=1,nrun
	   np=i2r(i)-i1r(i)+1
	   if(shutrun(i)) then
		if(i.eq.1.or.i.eq.nrun) then
		   nmin=2
		else
		   nmin=nbasemin
		endif
		if(np.ge.nmin) then
		   nsrun=nsrun+1
		   krun1(nsrun)=krun(i)	!index in iampl() etc of valid shut run
		   nrun1(nsrun)=i		!#, among all runs, of valid shut run
		   i1=i1r(i)
		   call SETACT(iact,11)		!light blue=shut
		   call IDCPIX(ix(i1),idata1(i1),np,iact)
		   j=krun(i)-kt+ntrans+1
		   s=s + theta(j)*float(np)	!numerator for weighted mean
		   den=den + float(np)		!denom for ditto
		endif
	   endif
	enddo
	if(nsrun.gt.0) then	!redefine base
	   base=s/den
	endif
c
c Now assign the tentative non-zero amplitudes
c NB nsrun=0 only if there is no baseline anywhere in the fit, in which case
c 'base' has its current value, Otherwise redefine 'base' redefined above
c as the weighted average of the fits to all valid shut runs
	if(nsrun.eq.0) then	  !do from last 'base' to be defined
	   j1=ntrans
	   k=kt
	   do i=1,ntrans
		j1=j1+1	!index for theta =ntrans+1, ntrans+2,...
	      y1=theta(j1+1)  !=theta(ntrans+2) first time (level after 1st trans)
		k=k+1		!index for iampl =kt+1, kt+2,...,kt+ntrans
		if(iampl(k).ne.0) then
	         a=y1-base
		   iampl(k)=int2(ifixr(a))
		endif
	   enddo
	else		!use baseline within fit to define amplitudes
	   j1=ntrans
	   k=kt
	   do i=1,ntrans
		j1=j1+1	!index for theta =ntrans+1, ntrans+2,...
	      y1=theta(j1+1)  !=theta(ntrans+2) first time (level after 1st trans)
		k=k+1		!index for iampl =kt+1, kt+2,...,kt+ntrans
c           Look for valid shut run that precedes and/or follows iampl(k)
c           ie transition #i. If there is one both before and after
c		then use the average of them (weighted with the number of points
c		in them
		if(iampl(k).ne.0) then		!opening
		   nlo=-1
		   nhi=-1
		   do n=nsrun,1,-1
			if(krun1(n).lt.k) then
			   nlo=n			!# of shut run just below k
			   goto 10
			endif
		   enddo
10		   do n=1,nsrun
			if(krun1(n).gt.k) then
			   nhi=n			!# of shut run just above k
			   goto 11
			endif
		   enddo
11		   if(nlo.gt.0.and.nhi.gt.0) then	!shut times before and after
			n=nrun1(nlo)
			enplo=float(i2r(n)-i1r(n)+1)	!# of points, for weighting
			n=nrun1(nhi)
			enphi=float(i2r(n)-i1r(n)+1)
c Note that iampl(kt+1) is amp after 1st transition which is in theta(ntrans+2)
c if index of iampl() is k, the corresponding level in theta is j=ntrans+k-kt+1
			j=krun1(nlo)-kt+ntrans+1
			slev1=theta(j)
			j=krun1(nhi)-kt+ntrans+1
			slev2=theta(j)
			base1=(enplo*slev1+enphi*slev2)/(enplo+enphi)
		   else if(nlo.gt.0.and.nhi.lt.0) then	!before only
			j=krun1(nlo)-kt+ntrans+1
			base1=theta(j)
		   else if(nlo.lt.0.and.nhi.gt.0) then	!after only
			j=krun1(nhi)-kt+ntrans+1
			base1=theta(j)
		   else
			call BELL(4)
			call WDIALOG(1,'ERROR: should be at least 1 run',12)
		   endif
	         a=y1-base1
		   iampl(k)=int2(ifixr(a))	!open amplitude define
		endif
	   enddo	!end of j=1,ntrans
	endif
c
c Redefine stepamp() in case needed for refit (concat below may redefine it again)
c NB in cjump case, if initially open at start of any sweep (not just kt=0),
c then do not want iampl(0)=0, but should be the estimated amplitude before
c the first transition which is now in iamp1sav.
	if(cjumpfst.and.(.not.shutprev)) then	!open at t=0 for jump
	   iampl(0)=iamp1sav
	endif
c
c Redefine stepamp() in case needed for refit (concat below may redefine it again)
c NO -just set fixed values into theta when and if refit wanted, not here
c=	do i=1,ntrans			!redefine stepamp()
c=	   j=i+kt-1    !=kt, kt+1, ..., kt+ntrans-1
c=	   stepamp(i)=float(iampl(j+1)-iampl(j))		!in intermed units
c=	enddo
c
c CHECK FOR ADJACENT ZERO AMPS
c Now check whether the setting of an amplitude as shut, in CHKSHUT, has
c resulted in adjacent shut periods (because there was a shut interval on
c either, or both, sides of that which has been set shut.  If so then
c concatenate the shut times, and fix them to shut level in case refit
c is requested.  This concat method is tested in TTC2.for.
c NB if numbering of delt() altered, must also change numbering in iampl(),
c kfix(), iprops(). and (if fixamp) mfix().
c Modified 07/24/94 04:20pm to reallocate mfix() correctly; iampl() and iprops()
c seem to be renumbered OK, but not kfix() which is set to 1 for those that
c have been set shut above, but otherwise set=0 (all kfix=2 lost) -is this OK?
c (marking of values as 'fixed' with equiv sign is done via iprops(), which is
c reset correctly)
c For first fit in a jump,  do not want to refer to iampl(kt) which is the
c amplitude of the last event in previous jump!  But there is another
c complication too -first latency has not yet beem inserted in iampl() etc
c (but already defined in iamp1sav etc) so iampl(kt+1) is, at this point, not
c the amplitude at t=0, but the amp after the first transition (usually open).
c But if this is zero, and 1st latency (iamp1sav) is also zero, these values
c need to be concatenated, so need separate bit before 'concat loop' starts,
c to cope with first latency.  Simplest solution seems to be to replace
c temporarily iampl(kt), iprops(kt), with iamp1sav etc, storing orig values
c in iamp2sav etc, so they can be restored after check for adjacent zero amps.
c (duration is still in delt(0) -define itint1sav after this section).
c========NB stepamp also defined above using #kt -OK?
c========any similar problem at end of jump?
c========any similar problem in GOBACKS?
	if(cjumpfst.and.kt.gt.0) then
	   iamp2sav=iampl(kt)		!save value to restore below
	   iprop2sav=iprops(kt)
	   iampl(kt)=iamp1sav		!temporarily
	   iprops(kt)=iprop1sav		!temporarily
	endif
c
	nt=ntrans
	i=0		!index for delt()=0,1,...
	i1=0		!index for concatenated delt()=0,1,...
	k=kt		!index for iampl(), iprops()
c Need to set delt(ntrans)=0 so that 'length to be added to following gap'
c is zero in case where no zero amps are concatenated at the end of the fitting
	delt(ntrans)=0.0
c Keep delt(0) in case consec zero amps are concatenated at start of fitting
c -needed to adjust t0
	d0sav=delt(0)
c
1	   continue		!start of concatenation loop
c NB iampl(0) now defined (=0 -or, for cjump, may be iamp1sav)
	if(iampl(k).eq.0.and.iampl(k+1).eq.0) then	!two adjacent zero amp
	   delt(i1)=delt(i1)+delt(i+1)
c== new bit, replaces 'c=', below
	   kfix(i1)=1			!concat zero amp fixed for refit
	   do n=i+1,nt-1			!Renumber all kfix() later than this one
		kfix(n)=kfix(n+1)
		opening(n)=opening(n+1)	!ditto for opening() (eg needed in SCANFUNC)
	   enddo
	   kfix(nt)=0		!no longer defined since nt reduced by one
c==orig version:
	   if(setshut(i+1)) then	!ie iampl(k+1) was set=0 in CHKSHUT
c=		kfix(i1)=1
		setshut(i1)=.true.	!transfer setshut() value (for refit)
	   else
c=		kfix(i1)=0
		setshut(i1)=.false.
	   endif
c==end of orig version
	   if(fixamp.and.i.gt.0) then	!ntrans reduced, so renumber mfix()
		do n=i+1,nt
		   if(mfix(n).gt.i) then
			mfix(n-1)=mfix(n)-1
			mfix(n)=-1
		   endif
		enddo
	   endif
	   i=i+1
         k=k+1
	   nt=nt-1
	else
	   i=i+1
         k=k+1
	   i1=i1+1		!start new concat group
	   delt(i1)=delt(i)
	   if(setshut(i)) then	!ie iampl(k+1) was set=0 in CHKSHUT
c=		kfix(i1)=1
		setshut(i1)=.true.	!transfer setshut() value (for refit)
	   else
c=		kfix(i1)=0
		setshut(i1)=.false.
	   endif
	endif
	if(i1+kt.ne.0) iampl(i1+kt)=iampl(k)
	if(i1+kt.ne.0.and.k.gt.0) iprops(i1+kt)=iprops(k)
	if(i.lt.ntrans) goto 1      !goes up to iampl(kt+ntrans)
c  If some concatenation has been done here then redefine theta,dt,stepamp
c  etc (and redisplay?) from the new delt(), iampl(). Also ask whether
c to refit with amps fixed to zero if some amps were set to zero in CHKSHUT
c (kfix, but not jfix, set above)
c NB after above loop:
c (1) if two or more zero amps concatenated at the START of the fitting, then
c	new delt(0) contains revised length of gap that precedes the first
c	transition of the current fit.
c (2) if two or more zero amps concatenated at the END of the fitting, then
c	then the new delt(ntrans) (which is not normally defined) contains the
c	duration to be added to the gap following the (revised) last transition
c	of the current fitting, ie must be added to tltemp=provisional tlast
	if(nt.lt.ntrans) then		!some concatenation done!
	   ntrans=nt			!new ntrans
	   tltemp=tltemp - dble(delt(ntrans))
c Also correct ttrans1=timsav() as for t0 -this is time of first fit which
c will be later if zeros concatenated at start of fit
	   ttrans1=ttrans1+delt(0)-d0sav  !also correct ttrans1 for timsav()
	   t0=t0+delt(0)-d0sav
	   t0sav=t0
	   x0=t0*dxf/dtf	!for display of guess, in case t0 altered
c Not generally desirable to define stepamp from iampl()
c with new fitting method, but not much option if concatenation has been
c done above (unless method of doing it is changed), but probably does not
c matter unless refitted?
	   do i=1,ntrans			!redefine stepamp()
		j=i+kt-1    !=kt, kt+1, ..., kt+ntrans-1
		stepamp(i)=float(iampl(j+1)-iampl(j))		!in intermed units
	   enddo
c  Now call SETPARAM to redefine dt(),dtsav(),kmax,theta(1,..,kmax),kfix,jfix,
c on basis of new values for ntrans,delt(),t0,y0,kfix,shutprev,prevlevel,stepamp
c  define theta,dt etc
	   call SETPARAM(ntrans,delt,t0,y0,kfix,shutprev,
     &	 prevlevel,stepamp,dt,dtsav,kmax,theta,jfix)
	endif		!end of redefinition after zero amps concatenated
c
c Now define tint1sav and set iprops for 1st latency
	if(cjumpfst) then
	   if(kt.gt.0) then
		iampl(kt)=iamp2sav		!restore value
		iprops(kt)=iprop2sav		!restore
	   endif
	   tint1sav=(delt(0) - sngl(tzerod))*1.e-3	!define latency (in ms)
c===debug -mark exact posn of 1st latency
c Data is displayed vs ix(i)=ifixr(float(i-1)*dx) (in pixel units=0-639).
c  Real time between between data points=finter (microseconds).
c Now delt(0) is in microsec, delt(0)/finter=number of points, and points are
c dx pixels apart, so want number of pixels from indfst=isdfst for jumps
c dx=pixels per data point=pixels per finter microsec, so
c dx/finter=pixels per microsec.
c For fit of first latency, delt(0) is measured from data(1).  First point
c on display is at (indfst-1)*finter microsec from start of jump
	   x1=delt(0)*dx/finter		!=pixels for delt(0) microsec
	   x1=x1-float(indfst-1)*dx
	   ix1=ifixr(x1)
	   if(ix1.ge.0.and.ix1.le.639) then
	      call VLINE(ix1,iyd1+40,iyd2-20,15)	 !line at 1st latency
	   endif
c===end debug
	endif
c
c If one or more intervals has been set to shut in CHKSHUT, ask whether
c to refit with this interval fixed at shut level (set kfix,jfix,iprops)
c With new method for defining amplitudes, the only time that this is likely
c to be desirable is when a short shutting has been fitted as though it was
c going to a non-zero amplitude -if it is now specified to be shut, its
c amplitude will be set to zero in iampl(), but the estimate of its duration
c may be noticeably changed by refitting with amplitude fixed at baseline.
c It is neither necessary nor desirable to redraw unless refit is to be
c done
	refit1=.false.
	if(sets1) then		!redisplay with values set to shut, and refit?
	   ans='Y'
	   call TONE(264,20)
c	   call TONE2(333,264,20)
c	   call BELL(1)
 	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     &	'Refit with amplitudes that were reset fixed at shut level'
     &	,defolt,icw,cans)
	   call GETINPa(cans,ans)
	   refit1=ans.eq.'Y'
	endif
c===	if(refit1.or.autoset) then		!redraw with zero amps so set
	if(refit1) then				!redraw with zero amps so set
	   call SETACT(iact,-1)		!delete old fit
	   iact(icc)=ibk2			!unless red, when draw background col
	   call IDCPIX(ixcalc,iycalc1,ke,iact)
	   call WRGWIN1(ix(if1),iycalc1(1),ixcalc(1),iycalc1(1),isavy1)
	   call WRGWIN1(ixcalc(ke),iycalc1(ke),ix(if2),iycalc1(ke),
     &		isavy2)
c Values are no longer set to baseline in stepamp, so the fixed values
c in theta() are set to base here, in next loop
c=	   j1=ntrans+1		!redefine amplitude guesses in theta
c=	   do j=1,ntrans
c=		j1=j1+1			!=ntrans+2,...,2*ntrans+1
c=		theta(j1)=theta(j1-1)+stepamp(j)
c=	   enddo
	   do i=1,ntrans
		if(setshut(i)) then
		   kfix(i)=1			!fixed at baseline
		   jfix(ntrans+i+1)=1    !fix this parameter; arg starts at ntrans+2
		   theta(ntrans+i+1)=base
		   k=kt+i
		   iprops(k)=IBSET(iprops(k),1)	!amplitude fixed; set bit 1 ='2'
		endif
	   enddo
c No longer redraw if not being refitted, so just reset 'autoset' (is it
c needed any longer?)
	   autoset=.false.
c=		if(autoset) then	!no refit wanted -so reset, redraw and carry on
c=		   autoset=.false.
c=		   call CONV1(ycalc,y0,ntrans,DT,ke,filt,nfilt,stepamp,dtf)
c=		   if(ke.gt.5120) ke=5120
c=		   do i=1,ke
c=			ixcalc(i)=ifixr(x0 + float(i-1)*dxf)
c=			iycalc0(i)=int2(ifixr(ycalc(i)))
c=		   enddo
c=c ycalc() is in units as given by readsec, without nscale or offset -must
c=c scale for display
c=		   call SCALINT1(iycalc0,ke,nscale1,iyoffz,iycalc1)
c=c Display the convolved trace for init guesses, starting at time of 1st transition
c=		   call SETACT(iact,-1)		!no draw
c=		   iact(ibk2)=12			!unless background col, when draw red
c=c  Now extend calc curve to cover whole fitted range from data(if1) to data(if2)
c=		   call RDGWIN1(ix(if1),iycalc1(1),ixcalc(1),iycalc1(1),
c=     &		isavy1)
c=		   call RDGWIN1(ixcalc(ke),iycalc1(ke),ix(if2),iycalc1(ke),
c=     &		isavy2)
c=		   if(ix(if1).lt.ixcalc(1)) then
c=		     call HLINE(ix(if1),ixcalc(1),iycalc1(1),icc)	!before start of resp func
c=	 	   endif
c=		   if(ixcalc(ke).lt.ix(if2)) then
c=		     call HLINE(ixcalc(ke),ix(if2),iycalc1(ke),icc)   !after end of resp func
c=		   endif
c=		   call IDCPIX(ixcalc,iycalc1,ke,iact)
c=		else
c=		   if(ifitmode.eq.1) then
c=			ikey=5		!refit 1 amp
c=			goto 155
c=		   else
c=			goto 100		!refit auto
c=		   endif
c=		endif
	   if(ifitmode.eq.1) then
		ikey=5		!refit 1 amp
		goto 155
	   else
		goto 100		!refit auto
	   endif
	endif		!end of 'if refit'
c
c Recalc curve if any values changed? (eg values forced to be SHUT -in redo
c fit altogether with nore values fixed?)
c
c NOW re-adjust base,iyoff etc? NB basevga= position on screen that
c data is to have baseline adjusted to, so don't alter this
c
c Mark position of fixed amplitudes with arrows
c -use mfix() returned from SCANFUNC to mark amplitudes of short openings
c that have been constrained to be equal to that of a longer opening
c in the same fit
c Time to middle of event is sum of the delt(i) up to the transition before
c the event, plus half the length of the event (+ empirical tshift!)
c Mark values fixed at baseline in light blue
c Mark values fixed at avamp in purple
c Mark values constrained to adjacent open level green
	k=kt
	do i1=1,ntrans-1
	   k=k+1
c	   if(iprops(k).eq.2.or.fixamp) then	!calc t
	   if(BTEST(iprops(k),1).or.fixamp) then	!calc t; test bit 1='2'
		t=theta(i1)				!NB theta() contains t0 already
		t=t + 0.5*delt(i1) +tshift		!microsec
		ix1=ifixr(t*dxf/dtf)
	   endif
	   if(BTEST(iprops(k),1)) then	!calc t; test bit 1='2'
c	   if(iprops(k).eq.2) then
		ic=13					!purple
		if(iampl(k).eq.0) ic=11		!light blue
		call VARROW(ix1,420,380,ic)
		iy2=ifixr(theta(ntrans+i1+1))	!level at which fixed
		call SCALINT(iy2,1,nscale1,iyoffz)
		call HLINE(ix1-4,ix1+4,iy2,ic)	!mark level at which fixed
	   endif
c (if amplitude of short opening fixed before simplex so iprops()=2 then
c may do both prev bit AND next (fixamp) bit (2nd arrow overwrites 1st)
	   if(fixamp.and.mfix(i1).ne.-1) then	!amp #i1 has been constrained
c		iprops(k)=4					!or ADD 4 to existing value?
		iprops(k)=IBSET(iprops(k),2)	!amplitude fixed; set bit 2 ='4'
		ic=10       !green
		call VARROW(ix1,420,380,ic)
c add dashed line from here to the amplitude which this was constrained to
c be equal - this is from opening following transition # jlong=mfix(nt)
c so draw dashed line from ix1-4 back to start of this opening, or from
c ix1+4 forward to end of this opening
		iy2=ifixr(theta(ntrans+i1+1))	!level at which constrained
		call SCALINT(iy2,1,nscale1,iyoffz)
		call HLINE(ix1-4,ix1+4,iy2,ic)  !mark level at which constrained
		i2=mfix(i1)			!index of opening with this amp
		if(i2.lt.i1) then		!short opening after the long one
c		   t=theta(i2)
		   t=theta(i2+1)		!so want END of opening=start of next
		   ix2=ifixr(t*dxf/dtf)
		   call DHLINE(ix1-5,ix2,iy2,ic,22)
		else
		   t=theta(i2)+finter*float(npfilt)	!start of open+transition time
		   ix2=ifixr(t*dxf/dtf)
		   call DHLINE(ix1+5,ix2,iy2,ic,22)
		endif
	   endif
	enddo
c
c PRINT RESULTS OF FIT
c  -write (up to) 3 values of kt, time, amp per line in dialog box
	if(.not.ampdone) then
	   avamp=stepamp(1)  !temp value for printing initial 'full amp' in SCAN
	   avampsav=avamp
	   avtot=avamp
	   navamp=1
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
	   if(iwindows.le.2) then
	      cnum21=cnum21(1:n2)//char(242)	!'>=' for 1st interval in jump
	   else
	      cnum21=cnum21(1:n2)//char(251)	!superscript 1 for 1st interval in jump
	   endif
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
		t=delt(i-1)*1.e-3		!duration (ms) (i-1=0,1,..,ntrans-1)
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
	   else		!k=kt+ntrans (i=ntrans+1)
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
		if(iwindows.le.2) then
		   cnum3(i)=cnum3(i)(1:n3)//char(240)   !equiv sign for fixed amplitude
		else
		   cnum3(i)=cnum3(i)(1:n3)//char(31)   !triangle for fixed amplitude
c		   if(n3+3.le.6) then
c		      cnum3(i)=cnum3(i)(1:n3)//'(f)'   !'f' for fixed amplitude
c		   else
c		      cnum3(i)=cnum3(i)(1:n3)//'f'   !'f' for fixed amplitude
c		   endif
		endif
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
20	continue	!return for next set of 3 lines if nec
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
	   goto 20
	endif
c
c Reject fit with neg values AFTER values printed in dialog box
	if(negflag.ne.0) then
c	   call BELL(1)
	   call NEENER()
	   call POPTIME(200,150,'NEGATIVE INTERVAL: FIT REJECTED',
     &    1,15,12,14,1.0)
	   negflag=0
	   ikey=2		!reject
	   goto 102
	endif
c
	if(dispfit) then		!return to accept after displaying 'alter guess'
	   ikey=3
	   goto 102
	endif
c
c Before accept/reject/refit, check results of fit to see
c  (1) Whether any fitted durations are too short to be of interest
c	  (below e.g. 15 microsec)
c  (2) Whether any fixed or constrained amplitudes are, after fitting,
c	actually long enough that they need not have been constrained/fixed
c -if such cases found then offer an automatic refit with ntrans fixed and
c appropriately adjusted init guesses/constraints
c	if(kt.gt.0.and.ntrans.gt.1.and.isflag.eq.0) then
	if(ntrans.gt.1.and.isflag.eq.0) then
c==========use same tshort here (2*trise at present) -used to fix amplitude
c///define ylevel(1) here -is this OK for ifitmode=1 too?
	   ylevel(1)=theta(ntrans+1)		!rest are defined in CHECKFIT
	   call CHECKFIT(kt,ntrans,delt,iampl,iprops,jfix,kfix,
     & 	theta,dt,stepamp,ylevel,tmin,ifitmode,iampcrit,base,refit,
     & 	tshort,avamp,opening,opendown,kmax,t0,ntmax)
	   if(refit) then
	      if(ntrans.eq.0) then
		   ikey=2		!reject
		   goto 102
		endif
		isflag=1		!signal to not ask again
		kmax=2*ntrans+1	!new value
		x0=t0*dxf/dtf	!for display of guess, in case t0 altered
c redraw data (as at 155 below)
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
		if(ifitmode.eq.0) then
		   goto 103
		else if(ifitmode.eq.1) then
		  call DEF_ONE(kt,t0,delt,dt,shutprev,shut,jfix,kfix,ntrans,
     &	  scrit,base,prevlevel,avamp,theta,itype,y0,stepamp,kmax,
     &   	  refit,iampl,ntmax,dtrigm,tshort)
		  if(ntrans.le.0) then
		     ikey=2		!reject
		     goto 102
		   endif
		   goto 100
		endif
	   endif
	endif
c
c ACCEPT FIT?  If not, what next?
151	continue
	nbox=15
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	if(newguess) then		!no accept/reject options until refitted
c	   Lb(2)=0			! -but allow reject in case refit hopeless!
c	   Lb(3)=0			!also allow 'accept without fit'?
	   Lb(12)=0
	endif
c NB now have only option to ZOOM/UNZOOM in EXPMODE -removed from here
c	call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
c	text(1)='1:'
	text(1)='1:ALTER RANGE'
	text(2)='2: REJECT FIT  '
	text(3)='3: ACCEPT FIT'
	if(newguess) text(3)='3: ACCEPT (NO FIT)'
	text(4)='4:REFIT: AUTO'
	text(5)='5:REFIT: ONE AMP'
	text(6)='6:REFIT: MANUAL'
	text(7)='7:REFIT: SUBLEVEL'
	text(8)='8:SET AMP DUBIOUS'
	text(9)='9:SET GAP UNUSABL'
	text(10)='10: QUEUE PLOT '
	text(11)='+:ADJUST BASELINE'
	text(12)='-:ALTER GUESSES'
	if(minmeth.eq.1) then
	   text(13)='x: SIMPLEX FIT   '
	else if(minmeth.eq.2) then
	   text(13)='x:SIMPLEX/DFP FIT'
	else if(minmeth.eq.3) then
	   text(13)='x: DFP FIT       '
	endif
	text(14)='/:SCROLL DIALOG'
	text(15)='.:HELP FOR MENU'
c Use HELP here only to explain menu options: put SCROLL DIALOG in above
c menu, and do 'new param' and 'new rms noise' only from roll-mode
c or expmode menus (not while waiting for accept/reject/refit option)
C Modif 04/11/94 11:55am so that key 15 gives help for menu options
c but F1 gives sep help box explaining display of fitted values (etc?)
c=======other options needed: RESET BASELINE (under options/help?)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
152	call CKEY(ch,ikey)
1521	if(ikey.lt.-1.or.ikey.gt.15) goto 152
	if(newguess) then
c	   if(ikey.eq.3.or.ikey.eq.12) then		!not allowed
c		call BELL(2)
c		goto 152
c	   endif
	   Lb(2)=1	!restore values set to 0 after 'alter guesses'
	   Lb(3)=1
	   Lb(12)=1
	endif
155	continue
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
		if(iwindows.le.2) then
		   helpst(5)=char(240)//
     &	   ' indicates that amplitude is fixed, rather than     '
		else
c		   helpst(5)='''f'''//
c     &	   ' indicates that amplitude is fixed, rather than     '
		   helpst(5)=char(31)//
     &	   ' indicates that amplitude is fixed, rather than     '
		endif
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
		if(cjump) then
		   if(iwindows.le.2) then
			helpst(12)=char(242)//
     &	   ' indicates first interval in the jump              '
		   else
			helpst(12)=char(251)//
     &	   ' indicates first interval in the jump              '
		   endif
		   ntext=12
		endif
	   	call POPTEXT(mxlo,mylo,myhi,helpst,ntext,ictx,ibkx,icfx)
		goto 151
	endif
c Reject fit when ikey=2, or any REFIT option chosen (or
c alter range, alter guesses) EXCEPT for 'REFIT: ONE AMP' (ikey=5) for which
c we want to keep the iampl(), shut,shutprev etc values for now,
c to recalc init guess in DEF_ONE.
c For refit ikey=4,6,7 data redrawn in SCAN
c For refit ikey=5 (AUTO-1 AMP) refit is done within FITSUB so redraw here
	accepted=.false.
	if(ikey.eq.5.or.ikey.eq.11.or.ikey.eq.12) then
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
c         Next bit deleted fitted curve only -but left arrows etc which are
c		not wanted for refit
c		call SETACT(iact,-1)		!no draw
c		iact(icc)=ibk2			!unless red, when draw background col
c		call IDCPIX(ixcalc,iycalc1,ke,iact)
cc         and restore data overwritten by HLINE before and after response func
c		call WRGWIN1(ix(if1),iycalc1(1),ixcalc(1),iycalc1(1),isavy1)
c		call WRGWIN1(ixcalc(ke),iycalc1(ke),ix(if2),iycalc1(ke),isavy2)
c	   endif
	endif
c Do not restore pre-fit values for ikey=11,12 in case these are followed
c by ikey=5 (refit one amp) which needs these values!
	if(ikey.eq.2.or.ikey.eq.4.or.ikey.eq.6.or.ikey.eq.7) then  !not ikey=5!
		SHUT=shutprev	!restore value
		base=basesav
		do k=kt+1,kt+ntrans
c    		   if(deb.and.BTEST(iprops(k),6)) then
    		   if(BTEST(iprops(k),6)) then
	            call INTCONV(k,cnum)
		      call WDIALOG(1,
     &           'iprops('//charnb(cnum)//') =64 reset to 0',11)
		   endif
		   iampl(k)=0	!reset iampl() to zero
		   iprops(k)=0	!reset iprops() to zero
		enddo
c	   endif
	endif
c
c ifitmode=0: automatic detection/fit of all amps and durations
c ifitmode=1: automatic with only one amplitude fitted             (key 4)
c ifitmode=2: manual definition of amplitude; only durations fitted (key 1)
c ifitmode=3: manual sublevel mode; only durations fitted           (key 7)
102	continue		!jump here with ikey=2 to reject if neg interval found
	select case(ikey)
	   case(1)       !alter fit range
		newrange=.true.
		call BELL(1)
		call WDIALOG(1,
     &'Adjust range to be fitted with arrow keys/mouse (F1=help)'
     &      ,icw)
		call FITBAR(ix,dx,if1,if2,ndfit,yd1,yd2,basevga,
     &	 opendown,.true.,isavb1,isavb2,isavb3)	!adjust=true
		call DCMENU(-7,5,Lb,text,icol1,icol2)	!put up 'refit auto' option
		call WDIALOG(1,'Now choose another menu option',-icw)
		goto 152	!get another option

	   case(2)		!REJECT fit
		refit=.false.	!reset
		newrange=.false.	!reset
		newguess=.false.	!reset
		isflag=0		!reset flag for CHECKFIT
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
c Although nrun was determined above, better do it again here, in case
c there has been any concatenation etc after fir completed.
		if(newguess.and.(.not.dispfit)) then
		   newguess=.false.
		   dispfit=.true.
		   goto 105			!display result after 'alter guess'
		endif
		dispfit=.false.
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
			   call GETSH1(shut1,k,shutprev,shut,t0,dt,i1,i2,kt,
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
		newrange=.false.	!reset
		newguess=.false.	!reset
		isflag=0		!reset flag for CHECKFIT
c Now accept provisional tint,iampl,tlast=tltemp etc
c If OPEN at end of fitting, save level for use in next fitting (save anyway)
		prevlevel=theta(2*ntrans+1)
		tlast=tltemp
c update number of fits, timsav etc
c ifits(i) = index of tint(), iampl() for the duration and amplitude
c of the interval following first transition of fitting #i
		nfits=nfits+1
		timsav(nfits)=ttrans1
		ifits(nfits)=kt+1		!points to element of tint(), iampl()
		ibaselin(nfits)=ifixr(base)
		if(cjump) nfjump=nfjump+1
c -check timsav>previous value to make sure it always increases!
		if(nfits.gt.1.and.timsav(nfits).lt.timsav(nfits-1)) then
         	   call INTCONV(nfits,cnum11)
		   call DCFORMAT(timsav(nfits-1),7,2,cnum21)
		   call DCFORMAT(timsav(nfits),7,2,cnum31)
		   call NEENER()
c		   call BELL(3)
		   call WDIALOG(1,' ERROR: nfits = '//charnb(cnum11)//
     &		' timsav = '//charnb(cnum21)//', '//
     &		charnb(cnum31)//' (tell DC!)',12)
		endif
c
c Update tint()
c In case of cjump, tint(kt) has already been allocated to interval between
c last transition and end of previous jump for the first fit in all jumps
c after the first one, so do not overwrite it in next line.
		if(cjump) then
		   if(ktjump.gt.0) then
			tint(kt)=delt(0)*1.e-3	!prev interval completed
c=			cumtint=cumtint+tint(kt)	!for check now below
		   endif
		else
		   if(kt.gt.0) tint(kt)=delt(0)*1.e-3	!prev interval completed
		endif
		if(ntrans.gt.1) then
		   do i=1,ntrans-1
			kt=kt+1		!update kt
			tint(kt)=delt(i)*1.e-3	!in msec
			if(cjump) then
			   ktjump=ktjump+1
c=			   cumtint=cumtint+tint(kt)	!for check -now below
			endif
		   enddo
		endif
c
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
c -now finally accepted, so allocate points to ifreq()
c  Points if1 to if2 were fitted -add all to ifreq1()
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
c     When shut AT both ends, mean may be best for fitting amps, but
c		post-baseline is best estimate of BASE for future, so redefine base
		if(shutprev.and.shut) then		!shut at both start and end
		   base=theta(2*ntrans+1)	 !post-baseline
		endif
c
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
c		call MARK(infit,infirst,ndfit,ixfprev,ix,14,ibk2,
c     & 	  cjump,intzero,ix0prev,njump,njfit)
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
c=		   cumtint=cumtint+tint(ktj0)		!for check -now below
		   iampl(ktj0)=iamp1sav
		   iprops(ktj0)=iprop1sav
		   kt=kt+1		!now have one more
		endif
c Now check for cjumps that accumulated intervals in the current jump
c agree with tlast (also need to check timsav()??).  Rather than accumulating
c total interval so far as tint() assigned, recalculate it each time (this
c makes it it easier to cope with 'redo fit' -rather than vaing to subtract
c something from cumtint, just sum up to the revised kt0
		if(cjump) then
		   k1=kjumps(ijump)	!index of 1st latency for current jump
		   k2=ifits(nfits)-1	!index of interval before fit in current jump
		   if(cjumpfst) k2=k2+1	!include latency
		   s=0.0
		   do k=k1,k2
			s=s + tint(k)
		   enddo
c tcheck is timsav(nfits) minus tsample*dble(float(ijump-1))+tzero, i.e
c timsav() except that started from position of jump in the current jump
		   tcheck=timsav(nfits)-tzerod-tsample*dble(float(ijump-1)) !in mus
		   tcheck=tcheck*1.0d-3		!in ms
		   diff=sngl(tcheck) - s
		   if(abs(diff).gt.0.1) then
			call DCFORMAT(s,10,2,clong1)
			call DCFORMAT(sngl(tcheck),10,2,clong2)
			call LJUSTIF(clong1,14)
			call LJUSTIF(clong2,14)
c			call BELL(3)
			call NEENER()
			call WDIALOG(1,'ERROR: interval='//
     &		charnb(clong1)//' disagrees with TIMSAV='//
     &		charnb(clong2)//' (tell DC!)',12)
			call WDIALOG(1,'Hit any key to continue',icw)
			call ANYKEY()
		   endif
		   do k=k2+1,kt		!add in current fit to s
			s=s + tint(k)
		   enddo
		   cumtint=s
 		   tcheck=(tlast-tzerod)*1.0d-3
		   diff=sngl(tcheck-cumtint)
		   if(abs(diff).gt.0.1) then
			call DCFORMAT(cumtint,10,2,clong1)
			call DCFORMAT(sngl(tcheck),10,2,clong2)
			call LJUSTIF(clong1,14)
			call LJUSTIF(clong2,14)
c			call BELL(3)
			call NEENER()
			call WDIALOG(1,'ERROR: total interval='//
     &		charnb(clong1)//' disagrees with TLAST='//
     &		charnb(clong2)//' (tell DC!)',12)
			call WDIALOG(1,'Hit any key to continue',icw)
			call ANYKEY()
		   endif
		endif

	   case(4)			!refit auto
c		If parameter guesses changed, and auto-fit requested again,
c		 then goto 100 to display new guess and fit using them (and
c		 present time guesses) without re-searching for transitions.
c		  Otherwise start whole fit again after returning to SCAN
c		 to redraw data -also start again if ifitmode has changed!
		ifitold=ifitmode
		ifitmode=0
		refit=.true.
		if(newguess.and.ifitold.eq.0) then
		   newguess=.false.
c		   goto 100
		   goto 104
		else
		   newguess=.false.
		   goto 999	!redo fit entirely -skip average amplitude bit
		endif

	   case(5)			!auto -one amp
c 	     Put guess for the 1 amp in theta(ntrans+1) in def_one: this may
c		(if there were contiguous sublevels) change delt,dt,kmax,stepamp)
		ifitmode=1
		refit=.true.
		call DEF_ONE(kt,t0,delt,dt,shutprev,shut,jfix,kfix,ntrans,
     &	 scrit,base,prevlevel,avamp,theta,itype,y0,stepamp,kmax,
     &   	 refit,iampl,ntmax,dtrigm,tshort)
		if(ntrans.le.0) then
		  ikey=2		!reject
		  goto 102
		endif
		goto 100	!do ifitmode=1 within FITSUB
c		goto 104	!do ifitmode=1 within FITSUB
c		goto 999	!skip average amplitude bit

	   case(6)			!refit manual
		ifitmode=2
		refit=.true.
		newguess=.false.
		goto 999	!skip average amplitude bit and refit in FITSUB4

	   case(7)			!MANUAL SUBLEVEL MODE (first reject fit just done)
		ifitmode=3
		refit=.true.
		newguess=.false.
		goto 999	!skip average amplitude bit

	   case(8)        !set dubious
50		k1=kt-8	!display tint(kt-8) to tint(kt)
		call SELDATA(k1,kt,tint,iampl,calfac2,kval,ntmax,1)
		if(kval.ne.-1) then		!not 'cancel'
		   if(iampl(kval).eq.0) then	!skip if 'cancel'
		     call BELL(2)
		     call WDIALOG(1,
     &	    'Only openings can be set dubious -try again',-icw1)
		     goto 50
		   endif
		   iprops(kval)=IBSET(iprops(kval),0)   !set bit 0='1'
		endif
		call BELL(1)
		call WDIALOG(1,
     &	 'Now accept/reject/refit',-icw1)
		goto 152	!get another option

	   case(9)		!set unusable
51		k1=kt-8	!display tint(kt-8) to tint(kt)
		call SELDATA(k1,kt,tint,iampl,calfac2,kval,ntmax,0)
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
     &	 'Now accept/reject/refit',-icw)
		goto 152	!get another option

	   case(10)		!plot
c           calfac=pA/ADC (ymin,ymax not needed in AUTPLOT?)
c NB When SCQPLT called from FITSUB/FITSUB6 substitute 1,2048 for novlap,nbuf
c  and call with idata0() rather than idata(), and specify first point
c  to be kept in isdfst=1, number to be fitted=ndfit subs for ndisp1.
		tif1=float(if1-1)*finter	!time for point if1 (microsec from 1st point)
		tif2=float(if2-1)*finter	!time for point if1 (microsec from 1st point)
		call SCQPLT(idiskq,ndev,indfst,1,ndfit,idata0,2048,
     &      0,srate,calfac,calfac2,ioff,cdate,adctime,mono,iesc,
     &      ntrans,y0,t0,nfilt,dtf,dt,stepamp,filt,tif1,tif2,base,
     &	cjump,njump)
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

	   case(11)
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
		call BELL(1)
	      call WDIALOG(1,
     &       'Next choose another menu option',-icw)
	      goto 152

	   case(12)		!new guesses for durations
c Fixed 03/18/94 05:38pm so number of transitions can be altered too (set
c as in FITSUB6, but here the amplitudes will be estimated, not fixed), and
c (also add option to modify existing guesses or define new ones as
c in FITSUB6?).
c
c Redisplay fit in yellow, as new init guesses
		call SETACT(iact,-1)		!no draw
c		iact(ibk2)=14			!unless background col, when draw yellow
	      iact(ibk2)=11			!unless background col, when draw blue
c    Overwrite colours 4,9,13 also (=amplitude lines)
c		iact(4)=14			      !also overdraw 4
c		iact(9)=14			      !also overdraw 9
c		iact(13)=14			      !also overdraw 13
		iact(4)=11			      !also overdraw 4
		iact(9)=11			      !also overdraw 9
		iact(13)=11			      !also overdraw 13
		call HIDE_MOUSE()
		call IDCPIX(ixcalc,iycalc1,ke,iact)
		call SHOW_MOUSE()
c
	      call BELL(1)
         	call INTCONV(ntrans,cnum)
		n=nblank(cnum)
		nsav=ntrans
		call DEFOLTi(ntrans,defolt)
		call QDIALOG(1,
     &     'At present '//cnum(1:n)//' transitions fitted: new number',
     &	defolt,ictd,cans)
		call GETINPi(cans,ntrans)
		cfac=dx/finter	!factor to convert x0 to t0
		call WDIALOG(1,
     &'Define guesses for transitions with arrow keys/mouse (F1=help)'
     &      ,icw)
		call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
		kfix(0)=0		!reset jfix,kfix
		do i=1,50
		   jfix(i)=0
		   kfix(i)=0
		enddo
		if(ntrans.ne.nsav) then
		   ikey=3			!can't modify old guess if ntrans changed
		   goto 253
		endif
c
		ans='Y'
252	 	call DEFOLTa(ans,defolt)
		call QDIALOG(1,
     &       'Modify this guess (N=make new guess: F1=help)',
     &	 defolt,-icw1,cans)
		call GETINPa(cans,ans)
		if(help) then
		   helpst(1)='Y: Use keys/mouse to adjust the amplitudes,'
		   helpst(2)='   and initial guesses for transition times,'
		   helpst(3)='   that are shown by the yellow curve.     '
		   helpst(3)='   that are shown by the blue curve.       '
		   helpst(4)='N: Blue curve is bad guess so ignore it    '
		   helpst(5)='   and define a new guess with mouse (which'
		   helpst(6)='   can then be adjusted as above).'
		   ntext=6
	   	   call POPTEXT(mxlo,mylo,myhi,helpst,ntext,ictx,ibkx,icfx)
		   call HIDE_MOUSE()
		   goto 252
		endif
		if(ans.eq.'Y') then
		   ikey=2
		else
		   ikey=3
		endif
c
c		text(1)='1: HELP (=F1)'
c		text(2)='2: MODIFY THIS GUESS'
c		text(3)='3: MAKE A NEW GUESS'
c		nbox=3
c		call SETLBOX(3,Lb,1)
c		call NUMSET			!set num lock on ready for response
c		call DCMENU(nbox,4,Lb,text,icol1,icol2)
c252		call CKEY(ch,ikey)
c		if(ikey.lt.-1.or.ikey.gt.3) goto 252
c		if(ikey.eq.-1.or.ikey.eq.1) then		!F1=help
c		   helpst(1)='2: Use keys/mouse to adjust the amplitudes,'
c		   helpst(2)='   and initial guesses for transition times,'
c		   helpst(3)='   that are shown by the yellow curve.     '
c		   helpst(3)='   that are shown by the blue curve.       '
cc		   helpst(4)='3: Yellow curve is bad guess so ignore it  '
c		   helpst(4)='3: Blue curve is bad guess so ignore it    '
c		   helpst(5)='   and define a new guess with mouse (which'
c		   helpst(6)='   can then be adjusted as above).'
c		   ntext=6
c	   	   call POPTEXT(mxlo,mylo,myhi,helpst,ntext,ictx,ibkx,icfx)
c		   call HIDE_MOUSE()
c		   goto 252
c		endif
c		call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
c       Delete yellow curve -now blue!
253		continue
		call GETLAST(kt,iampl,iprops,ntmax,ialast,ialast1)
	      call SETACT(iact,-1)	!no draw
c		iact(14)=ibk2	  !unless yellow, when draw background col
		iact(11)=ibk2	  !unless blue, when draw background col
		call IDCPIX(ixcalc,iycalc1,ke,iact)
		if(ikey.eq.2) then
		   call MODSUBL(iampl,iprops,ntmax,kt,t0,
     &          y0,dt,ntrans,filt,nfilt,stepamp,dtf,trise,x0,dxf,
     &          nscale1,iyoffz,cfac,ibk,ibk2,tshift,ylevel,delt,
     &          kfix,opening,opendown,mono,base,scrit,shut,
     &          ialast,ialast1,sfac31,avamp,calfac2,rms1,reject,
     & 	    setshut,icw,icw1,yd1,yd2,iyd1,iyd2)
		else if(ikey.eq.3) then
 		   call SETSUBL(iampl,iprops,ntmax,kt,t0,
     &	     y0,dt,ntrans,filt,nfilt,stepamp,dtf,trise,x0,dxf,
     &	     nscale1,iyoffz,cfac,ibk,ibk2,tshift,ylevel,delt,
     &	     kfix,opening,opendown,mono,base,scrit,shut,
     &	     ialast,ialast1,sfac31,avamp,calfac2,rms1,reject,
     &	     icw,icw1,tshift1,xshift1,ibad,setshut,
     &	     finter,npfilt,data,if1,if2,timamax,yd1,yd2,iyd1,iyd2)
		   if(ibad.eq.1) then
		      goto 252
		   endif
		endif
		if(reject) then
		   reject=.false.
		   ikey=2		!reject
	         goto 102
		endif
c       Now check new guesses and set kfix(),iprops()
c		on the basis of the new guesses, before refitting;
c		[NB delt(), opening() are set in setsubl/modsubl,
c		and jfix() will be set in SETPARAM]
		do i=1,ntrans-1
		   if(delt(i).lt.tshort) then		!short event  -fix amplitude
c============PROBLEM 'not opening' does not imply SHUT!
		      if(opening(i).and.(.not.setshut(i))) then	!=short opening
		         kfix(i)=2  		!fixed at open level
			   jfix(ntrans+i+1)=1
			   iprops(kt+i)=IBSET(iprops(kt+i),1)	!set bit 1 = '2'
			else if(.not.opening(i).or.setshut(i)) then   !short shutting
		         kfix(i)=1  		!fixed at baseline level
			   jfix(ntrans+i+1)=1
			   iprops(kt+i)=IBSET(iprops(kt+i),1)	!set bit 1 = '2'
			endif
		   endif
		enddo
c
		newguess=.true.
		call BELL(1)
		call WDIALOG(1,'Now choose refit menu option',-icw)
		call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
c  show main menu but without accept/reject/alter guesses
		goto 151	!redraw menu
c		goto 104	!refit

	   case(13)
		minmeth=minmeth+1
		if(minmeth.gt.3) minmeth=1
		goto 151		!redraw menu

	   case(14)
		call SCROLDIAL(1)
		goto 151		!back to main ACCEPT menu

	   case(15)		!HELP OPTIONS
		helpst(1)=
     &	  '2.REJECT FIT: return to roll mode and carry on.   '
		helpst(2)=
     &	  '4.REFIT AUTO: fits all transitions and amplitudes    '
		helpst(3)=
     &	  ' (in the defined fit range) automatically.           '
		helpst(4)=
     &	  '5.REFIT ONE AMP: fit the transitions just found but  '
		helpst(5)=
     &	  '   estimating only one open level for all openings.  '
		helpst(6)=
     &	  '6.REFIT MANUAL: define a single open level with      '
		helpst(7)=
     &	  '  cursors and fit specified number of transitions.   '
		helpst(8)=
     &	  '7.REFIT SUBLEVEL: defined amplitudes separately with '
		helpst(9)=
     &	  '  cursor for each of a specified # of transitions.   '
		helpst(10)=
     &	  '8.SET AMP DUBIOUS: mark ill-defined (rather than just'
		helpst(11)=
     &	  '  brief) amplitude as being ''dubious''.             '
		helpst(12)=
     &	  '9.SET GAP UNUSABLE: in case of breakdown or double   '
		helpst(13)=
     &	  '  opening insert an unusable shut period, separating '
		helpst(14)=
     &	  '  two good openings.                                 '
		helpst(15)=
     &	  '+:ADJUST BASELINE: alter shut level.                 '
		helpst(16)=
     &	  '-:ALTER GUESSES: set initial guesses for duration of '
		helpst(17)=
     &	  '  events manually (then refit by AUTO etc).          '
		helpst(18)=
     &	  '*:ALTER RANGE: alter the range of points to be fitted'
		helpst(19)=
     &	  '  -arrow keys move low end; ctrl-arrow moves high end'
c
		ntext=19
	   	call POPTEXT(mxlo,mylo,myhi,helpst,ntext,ictx,ibkx,icfx)
		goto 151
	END SELECT
c
c Calc approx average amplitude now.
c (1) First value=that found by initial 'fit a full amplitude'
c	  (set in SCAN after leaving FITSUB)
c (2) For next 10 values that are within 20% of first, ask whether to average
c (3) Thereafter average all values that are within 10% of first 10
c Better to start on avamp only AFTER initial amplitude done
cc	if(navamp.eq.0) then
cc	   avtot=stepamp(1)
cc	   navamp=1
cc	   avamp=avtot
cc	else if(navamp.ge.1.and.navamp.le.5.and.kt.ge.ntrans) then
	if(navamp.le.5.and.kt.ge.ntrans) then
c	   if(ifitmode.eq.0) then
c		k1=kt-ntrans+1
c		k2=kt			!ask for each amp if 'fit auto'
c	   else if(ifitmode.eq.1) then
c		k1=kt-ntrans+1
c		k2=k1			!ask once only for 'fit one amp'
c	   endif
	   k1=kt-ntrans+1
	   k2=k1			!ask once only per fitting
	   do k=k1,k2
		a=float(iampl(k))
c		if(iampl(k).ne.0.and.iprops(k).eq.0.and.
c     &		   abs(a-avamp).lt.0.2*abs(avamp)) then
		if(iampl(k).ne.0.and.iprops(k).eq.0) then
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
		   avampsav=avamp
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
      call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
c
	RETURN
	end

