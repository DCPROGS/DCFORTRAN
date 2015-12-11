	subroutine EXPMODE(IDATA,isdfst,indfst,ndisp1,xtrig,triggered,
     & accepted,fcdone,iyoff,iyoffprev,
     & nsec,iyd1,iyd2,ibk,ibk1,ibk2,expfac,js1,js2,isec,nbuf,novlap,
     & sdone,usepots,mono,baseOK,fitmode,ifitmode,basevga,
     & nscale,sfac3,icol1,icol2,icol3,icol4,icbl,isavbase,
     & ipen,idata0,ndfit,ix,dx,infirst,infit,ixfprev,shut,kt,
     & idiskq,ndev,srate,calfac,calfac2,ioff,cdate,adctime,
     & calfac1,opendown,cfac1,nscale1,iyoffz,izoomfac,base,
     & izoom,fczoom,filtzoom,filtdone,zoomed,nread,iscrit,
     & cjump,intzero,ix0prev,njump,njfit,tjlen,ndat,
     & avamp,avampz,avampsav,iampcrit,ampfac,scrit,smult,facjump,
     & tf1,tf50,tf99,ffilt,seqdisp,inseq1,inseq2,minmeth,
     & allpoint,ingood,ispnt1,inpnt1,ispnt2,inpnt2,bpnt1,bpnt2,ipsec1)
c
c Expand, contract, shift code for SCAN
c  Line 6 of params is for plot queue
c cfac1...zoomed for zoom.
c   js1=is1 - novlap = index of 1st point in idata (inc overlap)
c   js2=is2 + novlap = index of last point in idata (inc overlap)
c Returned with fitmode=true when section on screen is
c to be fitted; idata0(1) to idata0(ndfit) contain data to be fitted
c (idata0(i)=unscaled version of data to be fitted; for use in IDCPIX it
c must first be scaled useing SCALINT or SCALINT1)
c
c Modif 04/12/99 09:15am for '*. CURSOR FIT', ifitmode=4
c Modif 03/20/97 09:49pm so expand is about trigger position (xtrig*ndisp)
c rather than about left edge -calculate trigger position before the
c initial expansion!  Make expansion point a bit before the trigger
c This is done only if 'triggered' is true on entry.  After a fit then
c now returns directly to expmode rather than to rollmode (if goblue=false)
c and if fit was accepted (accepted=true) then on return to expmode
c show the trace initially just as it was an leaving expmode before the fit
c but then, as soon as expand is hit, display so that the arrow marking the last
c fit jumps to the xtrig position at left of screen.  This happens only
c for the first hit on expand, and is also prevented as soon as shift is done
c (firstexp set false to prevent it)
c
c Modified 07/31/95 06:52pm for zoom/filter: lines 10 and 11 of params are
c for SETFILT, so scrit etc, and filt1(), can be set as appropriate for
c the filtering in use (filt1, filt1z, filt1n are in commons)
c
	logical seqdisp	!true when sequences from ekdist.tmp being displayed
	integer*2 idata(1-novlap:nbuf+novlap),ix(2048),idum2
c Also declare idataf() for filtered section
	allocatable	:: idatsav
	integer*2 idatsav(:)
	integer*2 idata0(2048)	!for data to be fitted (and as temp file)
c	integer*1 isavopen(320),isavbase(320)!to hold graphics overwritten by line
	integer*1 isavbase(320)!to hold graphics overwritten by line
	character*1 ch,getch
	integer*2 ktype,id2
	character ndev*2,cdate*11,adctime*8		!for plot queue
	character*55 helpst(23)		!help strings
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	logical debug,caplock,kbhit,fitmode,usepots,sdone,baseOK,mono
	logical shut,opendown,zoomed,cjump,popon,triggered,accepted
	logical filtzoom,filtdone,allocated,firstexp
	logical backward		!LOCAL variable, for progline
	logical allpoint,ingood,fcdone
c	integer iax(10),iay(10)	!for CROSSH
c	character*11 cnum,cnum1
c For SETFILT
	real*4 filt1(1024)	!from INSCAN/NORMFILT
	real*4 filt1z(1024)	!from INSCAN/NORMFILT
	real*4 filt1n(1024)	!from INSCAN/NORMFILT
	common/filtblk/filt1,nfilt1,dtf1,npfilt,finter,filt(640),filtf
	common/filtblkz/filt1n,nfilt1n,dtf1n,filt1z,nfilt1z,dtf1z
c For Amplicon ADCs
c	integer*2 ichan(0:15),iadc(0:15),iadc0(0:15),nadc
C For DCMENU
	integer Lb(30)
	character*78 text(30)
c
	logical discprt
	common/ampmark/nampmark,iamark(10)
	common/dp/discprt
	common/popvalx/mxlo,mylo,myhi,ictx,ibkx,icfx	!values for poptext calls
	common/dialval1/ictd,icfd,icbd  !colours for dialog box #1 (text,frame,back)
c
	SAVE idatsav
c
	debug()=caplock()
c
c Initial display starts at input value of indfst, expanded by expfac
c Store values from roll mode -NO, these are kept in main prog.
c	ndispsav=ndisp1
c	indsav=indfst
c	isdsav=isdfst
	ndisp=ndisp1
	nscale1=nscale	!for zoom
	iyoffz=iyoff	!for zoom
	cfac1=calfac1	!for zoom
	popon=.true.	!to prevent repeated call to poptime in loop
	firstexp=.true.	!false after the first expansion
c
	fitmode=.false.
c
c Initialise for ADCs
c Channel 0 = shift left/right
c Channel 1 = expand/contract
c	if(usepots) then
c	   do i=0,15
c	    ichan(i)=0
c	    iadc(i)=0
c	   enddo
c	   ichan(0)=0
c	   ichan(1)=1
c	   nadc=2	!read channel 0,1
c	endif
c
c Area for data
	ixd1=0
	ixd2=640
c iyd1,iyd2 are params

c	parameters for expand mode:

c Start:clear part of the screen:
	call FILLWIN(ixd1,iyd1,ixd2,iyd2,ibk2)
	ixfprev=-1 		!x value where previous trigger point was drawn
	ix0prev=-1 		!x value where previous t=0 point was drawn (cjump)
	if(baseOK) then
	   ibasevga=ifixr(basevga)
	   call RDGWIN1(0,ibasevga,639,ibasevga,isavbase)  !keep data overwritten
	   call HLINE(0,639,ibasevga,icbl)	   !show baseline
	   if(nampmark.gt.0) then
		do i=1,nampmark
		   iy=ibasevga+iamark(i)	!sign of iamark set in inscan
		   call DHLINE(0,639,iy,10,22)
		enddo
	   endif
	endif
c
c Put up menu
	call DCMENU(0,4,Lb,text,ibk,ibk)  !delete all (to non-data background col)
150	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	if(.not.sdone) Lb(4)=0	!draw separately
	Lb(13)=0
	Lb(14)=0		!until filter/zoom fixed
	if(cjump) Lb(15)=0		!now used for cursor fit
151	continue
	text(1)='1: REROLL '
	text(2)='2: CONTRACT'
	text(3)='3: EXPAND  '
	if(sdone) then
	   if(fcdone) then
		text(4)='4: FIT  (AUTO)'
	   else
		text(4)='4: FIT Fc and AMP'
	   endif
	else
	   text(4)='4.ACCEPT BASELINE'
	endif
	text(5)='5:FIT: ONE AMP'
	text(6)='6:FIT: MANUAL'
	text(7)='7:FIT: SUBLEVEL'
	text(8)='8:SHIFT LEFT'
	text(9)='9:SHIFT RIGHT'
	text(10)='10: QUEUE PLOT '
	text(11)='+:BIGGER SHIFT'
	text(12)='-:SMALLER SHIFT'
	text(13)='x: CURSOR FIT  '
	if(zoomed) then
	   text(14)='/: UNZOOM      '
	else
	   text(14)='/: ZOOM/FILTER '
	endif
c	text(13)='x:EXPAND MORE'
c	text(14)='/:EXPAND LESS'
c	text(15)='.:HELP (or F1)'
	text(15)='.: CURSOR FIT  '
	if(allpoint.and.sdone) then
	   Lb(5)=0
	   Lb(6)=0
	   Lb(7)=0
	   Lb(13)=0
	   if(ingood) then
		text(4)='4.END GOOD BIT'
	   else
		text(4)='4.START GOOD BIT'
	   endif
	endif
	if(.not.fcdone) then
	   Lb(5)=0
	   Lb(6)=0
	   Lb(7)=0
	endif
c
	call DCMENU(nbox,5,Lb,text,icol1,icol2)	!response read after kbhit
	if((.not.sdone).or.(.not.fcdone)) then
	   call DCMENU(-4,5,Lb,text,icol3,icol4)	!highlight box 4
	endif
c
C	CALCULATE THE FIRST EXPANSION
c===========
c Modif 03/20/97 09:49pm so expand is about trigger position (xtrig*ndisp)
c rather than about left edge -calculate trigger position before the
c initial expansion!  Make expansion point a bit before the trigger
	itrig=isdfst+ifix(xtrig*float(ndisp)) - 2		!old ndisp
	inc2=20		!initial value for shift left/right by keys
c=	inc=inc2			!initially
	ndisp=ifixr(float(ndisp)/expfac)	!initial expansion
	if(triggered) then
	   expf=2.0		!factor for expand/contract after the first
	else
	   expf=1.5		!factor for expand/contract after the first
	endif
c
c===========
c Modif 03/20/97 09:49pm so expand is about trigger position (xtrig*ndisp)
c rather than about left edge -calculate trigger position before the
c initial expansion!
	if(triggered) then
	   isdfst=itrig-ifix(xtrig*float(ndisp))
c adjust everything else for new first point
	   isdlast=isdfst+ndisp-1
	   indfst=isdfst + (isec-1)*nbuf	!for MARK
	   indlast=indfst+ndisp-1
c=======need to check if 1st/last point in section reached?
	endif
c Read initial values of ADC for rolling display
c	if(usepots) call READAMP(ichan,nadc,iadc0)
	call NUMSET			!num lock on for kbhit options
c
c	JUMP here when you change ndisp (expand/contract)
1	continue
c
	call CALLINE(ibasevga,cfac1,opendown,ndisp,finter,-1)	!calibration
	if(ndisp.lt.8.or.ndisp.gt.2048) goto 4
	dx=(639.0-float(ixd1))/float(ndisp)
	nhalfd=ndisp/2
c define ix()
c11	continue	!jump here when unusual dx defined (eg for truncated display)
	do i=1,ndisp
	   ix(i)=ifixr(float(i-1)*dx) + ixd1
	end do
c
	ifirst=1	!for first call to AIVPIX

C	JUMP  WHEN YOU CAN'T EXPAND/CONTRACT ANYMORE:

4	continue
c	Draw data:
c	call AIVPIX4(ix,idata(isdfst),ndisp,idata(isprev),
c     &	ipen,ifirst,ibk2,nscale1,iyoffz,iyoffprev)
	call AIVPIX5(ix,idata(isdfst),ndisp,idata(isprev),
     &	ipen,ifirst,ibk2,nscale1,iyoffz,iyoffprev,ndprev)
	if(.not.seqdisp) then
	   call MARK(infit,indfst,ndisp,ixfprev,ix,14,ibk2,
     &     cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
	else
	   call MARK(inseq1,indfst,ndisp1,isxp1,ix,12,ibk2,
     &       cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
	   call MARK(inseq2,indfst,ndisp1,isxp2,ix,12,ibk2,
     &       cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
	endif
	if(allpoint) then
	  call OPENBOX(6,7,.true.)
	  if(ingood) then
	     call WRITBOX(6,'RECORDING    ',1,12)
	  else
	     call WRITBOX(6,'NOT RECORDING',1,9)
	  endif
	endif
	if(kt.gt.0) then
	  call OPENBOX(1,7,.true.)	!draw box 2, for 'open'/'shut'
	  if(shut) then			!display final level
	     call WRITBOX(1,'SHUT',1,11)
	  else
	   call WRITBOX(1,'OPEN',1,12)
	  endif
	endif
	isprev=isdfst	!record value in case needed in DELPIX
	ndprev=ndisp	!ditto
	iyoffprev=iyoffz
c	pause
c	JUMP WHEN YOU MOVE backwards/forwards :

5	continue

c	JUMP WHEN  changing increment :

2	continue
	idest=0	!reset
c Read pots
c	if(usepots) then
c	   call READAMP(ichan,nadc,iadc)
c	   idt0=int4(iadc(0)-iadc0(0))		!shift
c	   idt1=int4(iadc(1)-iadc0(1))		!expand/contract
cc idt will be about +/- 2048 (if pot is initially central!) Fix shift so
cc about 500 units=shift of ndisp points?
cc PROBLEM: jitter causes next bit to be executed even when pots not moved
cc so INC defined here overides that from arrow keys, which therefore do
cc not cause shift
cc PROBLEM: if INC is near zero from last pot read, then when arrow key
cc is read it uses this INC to do shift i.e. shifts by very little: define
cc INC2 for use by arrows to overcome this
cc PROBLEM: idt=diff between one cycle and next, but iadc0 updated frequently
cc so knob must be moved fast to get much difference. If ndisp is small then
cc inc1 calc as below will often be zero eg for ndisp=32, 32/800=0.04 so need
cc idt=13 to prevent inc1 rounding to zero.  Shift sensitivity is OK for
cc largish ndisp (contracted trace) when calc as
cc inc1=ifixr(float(idt0*ndisp)/800.), but too low for expanded trace
cc -seems better with iadc0 updated only when inc.ne.0, but too much jitter
cc for large NDISP (very contracted traces)
c	   if(idt0.ne.0) then		!shift
cc	      inc1=ifixr(float(idt0*ndisp)/800.)
c		inc1=ifixr(float(idt0*ndisp)/600.)
c		call LOCATE(3,0)
c		print *,inc
c		if(inc1.ne.0) then	!shift forward (amp line/sec # not redrawn)
c	   	   iadc0(0)=iadc(0)		!reset init value
c		   inc=inc1		!change inc only if inc1.ne.0 (anti-jitter)
cc		   isdlast=isdfst+ndisp-1
c		   isdfst=isdfst+inc
c		   isdlast=isdfst+ndisp-1
c		   if(isdfst.le.js1) isdfst=js1
c		   if(isdlast.gt.js2) isdlast=js2
c		   ndisp=isdlast-isdfst+1	!keep ix() same for shift
c		   indfst=isdfst + (isec-1)*nbuf	!for MARK
c		   indlast=indfst+ndisp-1
c 		   call AIVPIX5(ix,idata(isdfst),ndisp,idata(isprev),
c     &		ipen,ifirst,ibk2,nscale1,iyoffz,iyoffprev,ndprev)
cc		   call MARK(infit,indfst,ndisp,ixfprev,ix,14,ibk2,
cc     &		  cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
c		   if(.not.seqdisp) then
c			call MARK(infit,indfst,ndisp,ixfprev,ix,14,ibk2,
c     &		  cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
c		   else
c		      call MARK(inseq1,indfst,ndisp1,isxp1,ix,12,ibk2,
c     &	       cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
c		      call MARK(inseq2,indfst,ndisp1,isxp2,ix,12,ibk2,
c     &	       cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
c		   endif
c		   if(allpoint) then
c		     call OPENBOX(6,7,.true.)
c		  	if(ingood) then
c		  	call WRITBOX(6,'RECORDING    ',1,12)
c		     else
c		  	call WRITBOX(6,'NOT RECORDING',1,9)
c		     endif
c		   endif
c		   isprev=isdfst	!record value in case needed in DELPIX
c		   ndprev=ndisp	!ditto
c		   iyoffprev=iyoffz
c		   indfst=isdfst + (isec-1)*nbuf	!for progline
c		   indlast=indfst+ndisp-1
c		   backward=inc.lt.0
cc		   call MOVE_PROG_LINE(indfst,backward)
c		   call MOVEPROG(indfst,backward)
cc		   goto 52		!look at keyboard -check idt1 first
c	      endif
c	   endif		!end of shift by pots
cc expand/contract by pots
c	   if(idt1.ne.0) then 	!expand/contract
c		ndelt=ifixr(float(idt1)/10.)
c		if(ndelt.ne.0) then
cc erase in assembler using aivpix4 with ifirst=-1
c		   ifirst=-1
cc		   call AIVPIX4(ix,idum,ndprev,idata(isprev),
cc     &	    ipen,ifirst,ibk2,nscale1,idum,iyoffprev)
c		   call AIVPIX5(ix,idum2,ndprev,idata(isprev),
c     &	    ipen,ifirst,ibk2,nscale1,idum,iyoffprev,ndprev)
c		   if(.not.seqdisp) then
c			call MARK(infit,indfst,ndprev,ixfprev,ix,14,ibk2,
c     &	        cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
c		   else
c		      call MARK(inseq1,indfst,ndisp1,isxp1,ix,12,ibk2,
c     &	       cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
c		      call MARK(inseq2,indfst,ndisp1,isxp2,ix,12,ibk2,
c     &	       cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
c		   endif
c	     	   if(allpoint) then
c			call OPENBOX(6,7,.true.)
c			if(ingood) then
c			   call WRITBOX(6,'RECORDING    ',1,12)
c			else
c			   call WRITBOX(6,'NOT RECORDING',1,9)
c			endif
c		   endif
c	         iadc0(1)=iadc(1)		!reset init value
c		   ndisp=ndisp+ndelt
c		   if(ndisp.lt.8) ndisp=8
c		   if(ndisp.gt.2048) ndisp=2048
c		   idest=1		!goto 1 after checking keyboard
c		   goto 52		!look at keyboard
c		endif
c	   endif		!end expand/contract by pots
c	endif		!end of usepots
c
c
c Read character from keyboard
52	continue
	if(kbhit()) then
c	   achar=getch(ktype)
c	   call GETIKEY(ikey)
	   ch=GETCH(ktype)
	   ival=ichar(ch)	!here you may give other options
	   if(ktype.eq.8.and.ival.eq.77) then
		ikey=9		!right arrow same as key 9
	   else if(ktype.eq.8.and.ival.eq.75) then
		ikey=8		!left arrow same as key 8
	   else
	      call SETCKEY(ktype,ival,ikey)		!in DCLIB now
	      if((ktype.eq.8.and.ival.eq.82).or.ikey.eq.0) ikey=10     !zero key with numlock off
	   endif
c	   if(ikey.eq.-1.or.ikey.eq.15) then		!F1=help
	   if(ikey.eq.-1) then		!F1=help
		helpst(1)=
     &	  '1.REROLL: return to roll mode and carry on.   '
		helpst(2)=
     &	  '2.CONTRACT: Contract the display (+,- control amount)'
		helpst(3)=
     &	  '3.EXPAND: Expand the display     (+,- control amount)'
	   if(sdone) then
		if(.not.allpoint) then
		   if(fcdone) then
		     helpst(4)=
     &	    '4.FIT (AUTO): Fit all transitions shown on screen   '
		     helpst(5)=
     &	    '  automatically (usually better to EXPAND first).   '
		   else
		     helpst(4)=
     &	    '4.FIT Fc + AMP: Fit the SINGLE transition on screen '
		     helpst(5)=
     &	    '  get estimate of effective filter fc and amplitude '
		   endif
		else
		   if(ingood) then
		      helpst(4)=
     &	    '4.END GOOD BIT: last data point to be included in   '
		      helpst(5)=
     &	    ' the all-point amplitude histo is at right of screen'
		   else
		      helpst(4)=
     &	    '4.START GOOD BIT: first data point to be included in'
		      helpst(5)=
     &	    ' the all-point amplitude histo is at left of screen.'
		   endif
		endif
	   else
		helpst(4)=
     &	  '4.ACCEPT BASELINE. hit when screen is showing only a'
		helpst(5)=
     &	  '  piece of ''typical'' baseline with no transitions.'
	   endif
		helpst(6)=
     &	  '5.FIT ONE AMP. Fit all transitions on the screen,    '
		helpst(7)=
     &	  '   estimating only one open level for all openings.  '
		helpst(8)=
     &	  '6.FIT MANUAL. define a single open level with        '
		helpst(9)=
     &	  '  cursors and fit specified number of transitions.   '
		helpst(10)=
     &	  '7.FIT SUBLEVEL. defined amplitudes separately with   '
		helpst(11)=
     &	  '  cursor for each of a specified # of transitions.   '
		helpst(12)=
     &	  '8.SHIFT LEFT. Shift display left (+,- control amount)'
		helpst(13)=
     &	  '  (left arrow key does the same)                     '
		helpst(14)=
     &	  '9:SHIFT RIGHT.Shift display right(+,- control amount)'
		helpst(15)=
     &	  '  (right arrow key does the same)                     '
		helpst(16)=
     &	  '+:BIGGER SHIFT.Increase amount of shift and expand or'
		helpst(17)=
     &	  '  contract for each key press.                       '
		helpst(18)=
     &	  '-:SMALLER SHIFT. Decrease amount of shift/expand     '
		helpst(19)=
     &	  '/:ZOOM/FILTER: Amplify and/or filter the display     '
		helpst(20)=
     &	  '  (filtering set in blue parameter menu) NOT WORKING '
		helpst(21)=
     &	  '.:CURSOR FIT. If amplitude not definable, use cursor '
		helpst(22)=
     &	  '  to mark start and end of opening; amplitude will be'
		helpst(23)=
     &	  '  defined as mean of the points between cursors.      '
		ntext=23
	      call POPTEXT(mxlo,mylo,myhi,helpst,ntext,ictx,ibkx,icfx)
	      goto 98	!carry on
	   endif
	   if(ikey.le.0) goto 98	!carry on
c
c If fit chosen, copy data to be fitted into idata0()
	   if((ikey.ge.4.and.ikey.le.7).or.ikey.eq.15) then
		ndfit=ndisp			!return ndisp
c		if(.not.fcdone) ndisp=iend-istart+1
		do i=isprev,isprev+ndisp-1
		  j=i-isprev+1
		  idata0(j)=idata(i)
		enddo
	   endif
c
	   SELECT CASE(ikey)
		case(1)		!reroll
		   call NUMCLR
		   goto 99

		case(2)    ! 2. contract
c		   Delete last trace
c		   call DELPIX4(ix,idata,ndprev,isprev,ipen,ibk2,
c     &	    nbuf,novlap,sfac3,iyoffprev)
c erase in assembler using aivpix with ifirst=-1
		   ifirst=-1
c		   call AIVPIX4(ix,idum,ndprev,idata(isprev),
c     &	    ipen,ifirst,ibk2,nscale1,idum,iyoffprev)
		   call AIVPIX5(ix,idum2,ndprev,idata(isprev),
     &	    ipen,ifirst,ibk2,nscale1,idum,iyoffprev,ndprev)
		   if(.not.seqdisp) then
		      call MARK(infit,indfst,ndprev,ixfprev,ix,14,ibk2,
     &	       cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
		   else
		      call MARK(inseq1,indfst,ndisp1,isxp1,ix,12,ibk2,
     &	       cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
		      call MARK(inseq2,indfst,ndisp1,isxp2,ix,12,ibk2,
     &	       cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
		   endif
	     	   if(allpoint) then
			call OPENBOX(6,7,.true.)
			if(ingood) then
			   call WRITBOX(6,'RECORDING    ',1,12)
			else
			   call WRITBOX(6,'NOT RECORDING',1,9)
			endif
		   endif
		   call CALLINE(ibasevga,cfac1,opendown,ndisp,finter,ibk2) !erase
		   ndisp=ifixr(float(ndisp)*expf)
		   if(ndisp.gt.2048) ndisp=2048
c
c Modif 03/20/97 09:49pm so contraction is about trigger position (xtrig*ndisp)
c rather than about left edge, so isdfst decreased
c new first point for display
		   if(triggered) then
			isdfst=itrig-ifix(xtrig*float(ndisp))
		   else if(accepted.and.firstexp) then
c after fit accepted then, for the FIRST expansion only, shift also so that
c the position of the last fit (yellow arrow) is moved to xtrig position
			isfit=infit - (isec-1)*nbuf	!index in section of yellow arrow
			isdfst=isfit-ifix(xtrig*float(ndisp))
		   endif
		   if(triggered.or.(accepted.and.firstexp)) then	!isdfst changed
c adjust everything else for new first point
			isdlast=isdfst+ndisp-1
c		If end of data, idata(js2), reached then display the last possible
c		set of ndisp points in the section (do not change ndisp, unless
c		requested display is longer than idata() -will never happen)
			if(isdfst.le.js1) then
			   isdfst=js1
			   isdlast=isdfst+ndisp-1
			   if(isdlast.gt.js2) then
				isdlast=js2
				ndisp=isdlast-isdfst+1	!keep ix() same for shift
			   endif
			endif
			if(isdlast.gt.js2) then
		         isdlast=js2
			   isdfst=isdlast-ndisp+1
c=========next bit taken from 'shift' -OK?
			   if(isdfst.le.js1) then	!not likely to happen!
				isdfst=js1
				ndisp=isdlast-isdfst+1	!keep ix() same for shift
			   endif
			endif
			indfst=isdfst + (isec-1)*nbuf	!for MARK
			indlast=indfst+ndisp-1
c============need to put display in here to (as in case(8))??
c            When end of idata() reached, ie idata(js2), COULD display fewer
c		points, by defining dx with ndisp above, before defining
c		reduced ndisp,so display truncated -but this causes problems
c		with value of ndisp for subsequent displays, so here, as when
c		shifting, when contracted so end of idata() reached, stop with
c		full screen display of maximally-contracted trace (i.e. show
c		from fixed isdfst, up to last data point, idata(js2), scaled
c		to fit whole screen
			if(isdlast.gt.js2) then
			   isdlast=js2
			   ndisp=isdlast-isdfst+1
			endif
		   else	!not triggered
			ndisp=ifixr(float(ndisp)*expf)
			if(ndisp.gt.2048) ndisp=2048
			isdlast=isdfst+ndisp-1		!new isdlast
			if(isdlast.gt.js2) then
			   isdlast=js2
			   ndisp=isdlast-isdfst+1
			endif
		   endif
		   indfst=isdfst + (isec-1)*nbuf	!for MARK
		   indlast=indfst+ndisp-1
		   firstexp=.false.
		   goto 1	!define dx, ix() for current ndisp

		case(3)	! 3. expand
c Another version: AIVPIX should now erase ONLY when ifirst=-1
		   ifirst=-1
c		   call AIVPIX4(ix,idum,ndprev,idata(isprev),
c     &	    ipen,ifirst,ibk2,nscale1,idum,iyoffprev)
		   call AIVPIX5(ix,idum2,ndprev,idata(isprev),
     &	    ipen,ifirst,ibk2,nscale1,idum,iyoffprev,ndprev)
		   if(.not.seqdisp) then
			call MARK(infit,indfst,ndprev,ixfprev,ix,14,ibk2,
     &	       cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
		   else
		      call MARK(inseq1,indfst,ndisp1,isxp1,ix,12,ibk2,
     &	       cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
		      call MARK(inseq2,indfst,ndisp1,isxp2,ix,12,ibk2,
     &	       cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
		   endif
	     	   if(allpoint) then
			call OPENBOX(6,7,.true.)
			if(ingood) then
			   call WRITBOX(6,'RECORDING    ',1,12)
			else
			   call WRITBOX(6,'NOT RECORDING',1,9)
			endif
		   endif
		   call CALLINE(ibasevga,cfac1,opendown,ndisp,finter,ibk2) !erase
		   ndisp=ifixr(float(ndisp)/expf)
		   if(ndisp.lt.8) ndisp=8
c=========
c Modif 03/20/97 09:49pm so expand is about trigger position (xtrig*ndisp)
c rather than about left edge
		   if(triggered) then
c new first point for display
			isdfst=itrig-ifix(xtrig*float(ndisp))
		   else if(accepted.and.firstexp) then
c after fit accepted then, for the FIRST expansion only, shift also so that
c the position of the last fit (yellow arrow) is moved to xtrig position
			isfit=infit - (isec-1)*nbuf	!index in section of yellow arrow
			isdfst=isfit-ifix(xtrig*float(ndisp))
		   endif
		   if(triggered.or.(accepted.and.firstexp)) then	!isdfst changed
c adjust everything else for new first point
			isdlast=isdfst+ndisp-1
c		If end of data, idata(js2), reached then display the last possible
c		set of ndisp points in the section (do not change ndisp, unless
c		requested display is longer than idata() -will never happen)
			if(isdfst.le.js1) then
			   isdfst=js1
			   isdlast=isdfst+ndisp-1
			   if(isdlast.gt.js2) then
				isdlast=js2
				ndisp=isdlast-isdfst+1	!keep ix() same for shift
			   endif
			endif
			if(isdlast.gt.js2) then
		         isdlast=js2
			   isdfst=isdlast-ndisp+1
c=========next bit taken from 'shift' -OK?
			   if(isdfst.le.js1) then	!not likely to happen!
				isdfst=js1
				ndisp=isdlast-isdfst+1	!keep ix() same for shift
			   endif
			endif
c==========need similar bit for when ist point #js1 reached
		      indfst=isdfst + (isec-1)*nbuf	!for MARK
			indlast=indfst+ndisp-1
		   endif
		   firstexp=.false.
		   goto 1

		case(4)	!4. fit/accept baseline
c for fitting record (or calc baseline) copy displayed data to idata(1)
		   if(allpoint.and.sdone) goto 80	!start/end bit for point amp
		   baseOK=.true.
		   if(sdone) then       !auto-fit
			fitmode=.true.	!fit the data; record in infirst abs index
			ifitmode=0
			infirst=isprev + (isec-1)*nbuf     !of 1st point in idata0()
			call NUMCLR
			goto 99
		   else
		      goto 999	!do rms and baseline (fillwin is after label31)
		   endif
c          Now bit for 'all point hist only' mode -NB start and end may be
c		in different sections
80		if(allpoint) then
		   if(ingood) then	!end good bit and add to histo
			ingood=.false.
			call CLRBOX(6,icbd)
			call WRITBOX(6,'NOT RECORDING',1,9)
			inpnt2=indlast	!last point in good bit
			infit=indlast
			ispnt2=isdlast
			bpnt2=base		!baseline at end
		      call NUMCLR
		      goto 99		!re-roll
		   else			!start new good bit
			ingood=.true.
			call CLRBOX(6,icbd)
			call WRITBOX(6,'RECORDING   ',1,12)
			inpnt1=indfst	!first point in good bit
c		     check for overlap with last section
			if(inpnt1.lt.inpnt2) inpnt1=inpnt2+1
			ispnt1=isdfst
			ipsec1=isec		!section number for first point
			bpnt1=base	!baseline at start
		   endif
		endif

		case(5)     !auto-fit -one amplitude
		   fitmode=.true.	!fit the data; record in infirst abs index
		   ifitmode=1
		   infirst=isprev + (isec-1)*nbuf     !of 1st point in idata0()
		   call NUMCLR
		   goto 99

		case(6)     !fit -manual
		   fitmode=.true.	!fit the data; record in infirst abs index
		   ifitmode=2	!manual definition of 1 amplitude
		   infirst=isprev + (isec-1)*nbuf     !of 1st point in idata0()
		   call NUMCLR
		   goto 99

		case(7)     !fit -sublevel
		   fitmode=.true.	!fit the data; record in infirst abs index
		   ifitmode=3	!manual definition of sublevel amplitudes
		   infirst=isprev + (isec-1)*nbuf     !of 1st point in idata0()
		   call NUMCLR
		   goto 99

		case(8)    ! shift left (forwards)
c For shifts forwards increase idsfst, with ix() constant, and ndisp constant
		   isdfst=isdfst+inc2
		   isdlast=isdfst+ndisp-1
c		If end of data, idata(js2), reached then display the last possible
c		set of ndisp points in the section (do not change ndisp, unless
c		requested display is longer than idata() -will never happen)
		   if(isdlast.gt.js2) then
		      isdlast=js2
			isdfst=isdlast-ndisp+1
			if(isdfst.le.js1) then	!not likely to happen!
			   isdfst=js1
			   ndisp=isdlast-isdfst+1	!keep ix() same for shift
			endif
		   endif
		   indfst=isdfst + (isec-1)*nbuf	!for MARK
		   indlast=indfst+ndisp-1
c  Erase and draw
c 		   call AIVPIX4(ix,idata(isdfst),ndisp,idata(isprev),
c     &		ipen,ifirst,ibk2,nscale1,iyoffz,iyoffprev)
 		   call AIVPIX5(ix,idata(isdfst),ndisp,idata(isprev),
     &		ipen,ifirst,ibk2,nscale1,iyoffz,iyoffprev,ndprev)
		   if(.not.seqdisp) then
		 	call MARK(infit,indfst,ndisp,ixfprev,ix,14,ibk2,
     &	 	 cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
		   else
		      call MARK(inseq1,indfst,ndisp1,isxp1,ix,12,ibk2,
     &	       cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
		      call MARK(inseq2,indfst,ndisp1,isxp2,ix,12,ibk2,
     &	       cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
		   endif
	     	   if(allpoint) then
			call OPENBOX(6,7,.true.)
			if(ingood) then
			   call WRITBOX(6,'RECORDING    ',1,12)
			else
			   call WRITBOX(6,'NOT RECORDING',1,9)
			endif
		   endif
		   isprev=isdfst	!record value in case needed in DELPIX
		   ndprev=ndisp	!ditto
		   iyoffprev=iyoffz
c=		   inc=inc2		!needed??
		   indfst=isdfst + (isec-1)*nbuf	!for progline
		   indlast=indfst+ndisp-1
		   backward=.false.
c		   call MOVE_PROG_LINE(indfst,backward)
		   call MOVEPROG(indfst,backward)
		   indlast=indfst+ndisp-1
		   if(indlast.ge.ndat.and.popon) then
      		call BELL(1)
			call POPTIME(250,130,'END OF DATA',1,15,12,14,1.0)
			popon=.false.
		   else
			popon=.true.	!call poptime again only after moving back
		   endif
		   firstexp=.false.
		   goto 5

		case(9)    ! shift right (backwards)
c If shifting back and run out of points (isdfst<js1) then must either
c (a) start display later than ix(1)
c or (b) stop display when last full screen on show
		   isdfst=isdfst-inc2
		   isdlast=isdfst+ndisp-1
		   if(isdfst.le.js1) then
			isdfst=js1
			isdlast=isdfst+ndisp-1
			if(isdlast.gt.js2) then
			   isdlast=js2
			   ndisp=isdlast-isdfst+1	!keep ix() same for shift
			endif
		   endif
		   indfst=isdfst + (isec-1)*nbuf	!for MARK
		   indlast=indfst+ndisp-1
c  Erase and draw
c 		   call AIVPIX4(ix,idata(isdfst),ndisp,idata(isprev),
c     &		ipen,ifirst,ibk2,nscale1,iyoffz,iyoffprev)
 		   call AIVPIX5(ix,idata(isdfst),ndisp,idata(isprev),
     &		ipen,ifirst,ibk2,nscale1,iyoffz,iyoffprev,ndprev)
		   if(.not.seqdisp) then
			call MARK(infit,indfst,ndisp,ixfprev,ix,14,ibk2,
     &	       cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
		   else
		      call MARK(inseq1,indfst,ndisp1,isxp1,ix,12,ibk2,
     &	       cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
		      call MARK(inseq2,indfst,ndisp1,isxp2,ix,12,ibk2,
     &	       cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
		   endif
	     	   if(allpoint) then
			call OPENBOX(6,7,.true.)
			if(ingood) then
			   call WRITBOX(6,'RECORDING    ',1,12)
			else
			   call WRITBOX(6,'NOT RECORDING',1,9)
			endif
		   endif
		   isprev=isdfst	!record value in case needed in DELPIX
		   ndprev=ndisp	!ditto
		   iyoffprev=iyoffz
c=		   inc=inc2		!needed?
		   indfst=isdfst + (isec-1)*nbuf	!for progline
		   indlast=indfst+ndisp-1
		   backward=.true.
c		   call MOVE_PROG_LINE(indfst,backward)
		   call MOVEPROG(indfst,backward)
		   if(indfst.le.1) then
	      	call BELL(1)
			call POPTIME(250,130,'START OF DATA',1,15,12,14,1.0)
			popon=.false.
		   else
			popon=.true.	!call poptime again only after moving back
		   endif
		   firstexp=.false.
		   goto 5

		case(10)	!plot
c		  NB call with ndisp=current # of points on display (not ndisp1)
c		 Call with ntrans=0 here -no fitted curve
		  call SCQPLT(idiskq,ndev,indfst,isdfst,ndisp,idata,nbuf,
     &   	   novlap,srate,calfac,calfac2,ioff,cdate,adctime,mono,iesc,
     &         0,y0,t0,nfilt,dtf,dt,stepamp,filt,tif1,tif2,base,
     &	   cjump,njump)
		   call BELL(1)
		   if(iesc.ne.0) then	!plot cancelled -get another option
		     call WDIALOG(1,
     &		'Plot cancelled -choose another option',-ictd)
			goto 52
		   else
		     call WDIALOG(1,
     &		'Plot queued -choose another option',-ictd)
		   endif

		case(11)	!increase shift increment
		   if(inc2.le.nhalfd) inc2=inc2*2
		   if(expf.lt.15.) expf=expf*1.5	!also change expand/contract
		   goto 2

		case(12)	!decrease shift increment
		   if(inc2.ge.4) inc2=inc2/2
		   if(expf.gt.1.1) expf=1.0 + (expf-1.0)/1.5	!also expand/contract
		   goto 2

	   case(13)

	   case(14)
c erase in assembler using aivpix with ifirst=-1, before nscale1 changed
		ifirst=-1
		if(zoomed) then		!UNZOOM
		   izoomfac=1		!factor to zoom Y values by
		   zoomed=.false.
c		   call AIVPIX4(ix,idum,ndprev,idata(isprev),
c     &	    ipen,ifirst,ibk2,nscale1,idum,iyoffprev)	!erase before nscale changed
		   call AIVPIX5(ix,idum2,ndprev,idata(isprev),ipen,ifirst,
     &	    ibk2,nscale1,idum,iyoffprev,ndprev) !erase before nscale changed
		   call CALLINE(ibasevga,cfac1,opendown,ndisp,finter,ibk2) !erase
		   nscale1=nscale		!restore
		   iyoffz=iyoff		!restore
		   cfac1=calfac1 		!restore
		   if(filtzoom) then
		      do i=js1,js2
			   id2=idata(i)			!swap idata, idatsav
			   idata(i)=idatsav(i)
			   idatsav(i)=id2
		      enddo
	   		call SETFILT(zoomed,filtzoom,ffilt,fczoom,tf1,tf50,tf99,
     &    		npfilt,nshutfit,scrit,iampcrit,finter,facjump,
     &	      smult,iscrit,ampfac,avamp,avampsav,avampz,filt1,nfilt1,
     &    		dtf1,filt1z,nfilt1z,dtf1z,filt1n,nfilt1n,dtf1n)
		   endif
		   call CALLINE(ibasevga,cfac1,opendown,ndisp,finter,-1) !redraw cal
		   text(14)='/: ZOOM/FILTER '
		   call DCMENU(-14,5,Lb,text,icol1,icol2)
		else				!ZOOM
		   zoomed=.true.
		   izoomfac=izoom		!default value (if not 1)
		   call DEFOLTi(izoomfac,defolt)
		   call QDIALOG(1,
     &    'Zoom factor (must be 1, 2, 4, 8, ...)',defolt,ictd,cans)
		   call GETINPi(cans,izoomfac)
		   izoom=izoomfac		!redefine default
		   z=log(float(izoomfac))/log(2.)
		   iz=ifixr(z)
c		   call AIVPIX4(ix,idum,ndprev,idata(isprev),
c     &	    ipen,ifirst,ibk2,nscale1,idum,iyoffprev)	!erase before nscale changed
		   call AIVPIX5(ix,idum2,ndprev,idata(isprev),ipen,ifirst,
     &	    ibk2,nscale1,idum,iyoffprev,ndprev) !erase before nscale changed
		   call CALLINE(ibasevga,cfac1,opendown,ndisp,finter,ibk2) !erase
		   nscale1=nscale-iz
		   iyoffz=ifixr(basevga - (basevga-float(iyoff))*(2.**iz))
		   cfac1=calfac1/float(izoomfac)
c
c             If filtering required, and not yet done, then filter whole
c		   section now
		   if(filtzoom.and.(.not.filtdone)) then
			if(allocated(idatsav)) DEALLOCATE(idatsav)
		      ALLOCATE(idatsav(1-novlap:nbuf+novlap))
			do i=js1,js2		!includes overlap (if any)
			   idatsav(i)=idata(i)
			enddo
		      call WDIALOG(1,
     &		'Filtering current section . . .',-ictd)
			fc=1000.*fczoom		!in Hz for GFILTER
c                 NB must call with idatsav(js1) [not idatsav=idatsav(1)]
			call GFILTER(idatsav(js1),idata(js1),nread,srate,fc)
		      call WDIALOG(1,
     &		'Filtering completed.           ',-ictd)
			filtdone=.true.
	   		call SETFILT(zoomed,filtzoom,ffilt,fczoom,
     &		 tf1,tf50,tf99,
     &    		 npfilt,nshutfit,scrit,iampcrit,finter,facjump,
     &	    smult,iscrit,ampfac,avamp,avampsav,avampz,filt1,nfilt1,
     &    		 dtf1,filt1z,nfilt1z,dtf1z,filt1n,nfilt1n,dtf1n)
		   else if(filtzoom.and.filtdone) then	!filtering done
		      do i=js1,js2
			   id2=idata(i)			!swap idata, idatsav
			   idata(i)=idatsav(i)
			   idatsav(i)=id2
		      enddo
	   		call SETFILT(zoomed,filtzoom,ffilt,fczoom,
     &		 tf1,tf50,tf99,
     &    		 npfilt,nshutfit,scrit,iampcrit,finter,facjump,
     &	    smult,iscrit,ampfac,avamp,avampsav,avampz,filt1,nfilt1,
     &    		 dtf1,filt1z,nfilt1z,dtf1z,filt1n,nfilt1n,dtf1n)
		   endif
c
		   call CALLINE(ibasevga,cfac1,opendown,ndisp,finter,-1) !redraw cal
		   text(14)='/: UNZOOM      '
		   call DCMENU(-14,5,Lb,text,icol1,icol2)
		endif
		goto 4		!redraw

c		case(13)	!expand more
c		   if(expf.lt.15.) expf=expf*1.5
c		   goto 2
c		case(14)	!expand less
c		   if(expf.gt.1.1) expf=1.0 + (expf-1.0)/1.5
c		   goto 2
		case(15)
		   fitmode=.true.	!fit the data; record in infirst abs index
		   ifitmode=4	!cursor fit
		   infirst=isprev + (isec-1)*nbuf     !of 1st point in idata0()
		   call NUMCLR
		   goto 99

	   END SELECT
	endif		!end of 'kbhit' options
98	continue
c
	if(idest.eq.1) then
	   goto 1	!recalc after expand/contract via pots
	else
	   goto 2	!re-read pots/keyboard (trace not redrawn)
	endif
c

99	continue
c
c If filter to be fitted then mark the region to be fitted at start
c of expmode
c
999	continue
	call DCMENU(0,4,Lb,text,ibk,ibk)  !delete all (to non-data background col)
c
	RETURN
	end
