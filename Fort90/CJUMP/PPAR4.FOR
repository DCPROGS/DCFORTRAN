	subroutine PPAR4(cdate,ctime,iTSAMP,iTPREc,nsamp,idd,
     & calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     & vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     & amVpA1,ftape,gain,nsweep,swtime,ismode,swval,nkeep,ikeep,kstep,
     & jkeep,nsamp1,tkpre,tkpost,options,nopt,iopt,iset,titles,isweep,
     & intpatch,ivhclamp,ffilt,randomiz,iperm)
c To type/print parameter values.  Last row of param added for CJUMP2.
c IPRT=0	No print to screen
c IPRT=1   Print brief details to screen
c IPRT=2   Print full details to screen
c IDPRT=0	No print to disc
c IDPRT=1   Print brief details to disc (for each jump recorded)
c IDPRT=2   Print full details to disc (only when params changed)
c Show details for multiple sweeps only when FULL details requested
c and if in graphics mode, details of IKEEP not put on screen even
c then to avoid disturbing boxes.
c (the brief details are to show what happens in individual sweeps)
c
	character    ans
	character*11 cdate,ctime
	character*11 getint,getint1,getint2,getint3,getint4
	character*13 getreal,getreal1,getreal2
	character*20 que(6)
	character*64 dtext(100),helps(10),header
	character*(*) options(nopt),titles
	integer*2 	ivolt1(10),ivolt2(10),ivhold,intpatch,ivhclamp  !pots for each V jump (integer mV)
	integer*4 	ikeep(4,2,30),mstep(5),iperm(30)
      integer*2   kstep(5),ipatch
	integer*2 	ndtext,ic,ibk
	integer*4 	ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 	ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	real*4 	vstep(10) 		!for GETSTEP
	real*4 	swval(30) 		!values that change between sweeps
	real*4 	alenv(10),agapv(10),alenc(10),agapc(10)
	logical 	pon,slock,vjump,ramp,sampv,control
	logical 	mouse_on,randomiz
	common/mousval/mouse_on,nbutton

c
	pon()=slock()
c
	mouse_on=.true.
	nbutton=3
	ixlo=16
	iyhi=480
	ic=14
	ibk=1
	icf=15
	ich=12
	ndtext=0
	header='PARAMETERS'
	nhelp=2
	helps(1)='CHOOSE OPTION'
	helps(2)= ' Press number'
	nrow=12
	que(1)='(1) outside-out'
	que(2)='(2) inside-out'
	que(3)='(3) cell-attached'
	que(4)='(4) whole-cell'
	que(5)='(5) simulated data'

	do i=1,100
		dtext(i)=
     &'                                                                '
	enddo
	do 20 i=1,10	!convert to real msec for printing
	   alenv(i)=1.e-3*float(ilenv(i))
	   agapv(i)=1.e-3*float(igapv(i))
	   alenc(i)=1.e-3*float(ilenc(i))
	   agapc(i)=1.e-3*float(igapc(i))
20	continue
	call GETSTEP(nvjump,ivolt1,ivolt2,ilenv,iDd,nvramp,vstep)
	itsamp1=itsamp/1000
	tprec1=float(itPREc)/1000.
	ndtext=ndtext+1         	!1
	dtext(ndtext)='DATE '//cdate//' TIME '//ctime
	ndtext=ndtext+1         	!2
	dtext(ndtext)='ADC SAMPLE'
	if(intpatch.ge.1) then
	   ipatch=intpatch
	   ndtext=ndtext+1               !6/1
	   dtext(ndtext)='Patch type : '//que(ipatch)
	endif
	ndtext=ndtext+1         	!2
	call realtoch(ffilt,getreal,13)
	nm=nblank1(getreal)
	dtext(ndtext)='Filter setting (Hz) : '//getreal(1:nm)
	ndtext=ndtext+1         	!2
	if(iset.eq.-1) then
	   call intconv(isweep,getint)
	   nm=nblank1(getint)
	   dtext(ndtext)='Sweep '//getint(1:nm)//'. :  '//
     &   titles
	else
	   call intconv(iset,getint)
	   nm=nblank1(getint)
	   dtext(ndtext)='Set '//getint(1:nm)//'. :  '//titles
	endif
	ndtext=ndtext+1         	!2
	call intconv(nseep,getint)
	nm=nblank1(getint)
	dtext(ndtext)='NSWEEP : '//getint(1:nm)
	ndtext=ndtext+1         	!2
	call intconv(irate,getint)
	nm=nblank1(getint)
	dtext(ndtext)='Sample rate (Hz) = '//
     &   getint(1:nm)
	ndtext=ndtext+1               !3
	call intconv(itsamp1,getint)
	nm=nblank1(getint)
	call intconv(nsamp,getint1)
	nm1=nblank1(getint1)
	dtext(ndtext)='Sample length = '//
     &   getint(1:nm)//
     &   ' ms ('//getint1(1:nm1)//' points)'
	ndtext=ndtext+1               !4
	call intconv(idd,getint)
	nm=nblank1(getint)
	dtext(ndtext)='Microseconds between DAC points = '//
     &   getint(1:nm)
	dx=1.e3/float(irate)
	if(nkeep.eq.1.and.ikeep(1,1,1).eq.1.and.
     &	int4(ikeep(1,2,1)).eq.nsamp) then
	   ndtext=ndtext+1            !5/1
	   dtext(ndtext)='Whole ADC sample kept'
	else
	   ndtext=ndtext+1            !5/1
	   call intconv(nkeep,getint)
	   nm=nblank1(getint)
	   call intconv(nsamp1,getint1)
	   nm1=nblank1(getint1)
	   dtext(ndtext)=
     &   'Number of sections of ADC sample kept='
     &   //getint(1:nm)//' ('//getint1(1:nm)//
     &   ' points)'
	   ndtext=ndtext+1            !1/2
	   dtext(ndtext)=
     &   'Outside specified sections keep also every nth point: '
	   ndtext=ndtext+1            !2/2
	   do lk=1,5
		mstep(lk)=int4(kstep(lk))
	   enddo
	   call intconv(mstep(1),getint)
	   nm=nblank1(getint)
	   call intconv(mstep(2),getint1)
	   nm1=nblank1(getint1)
	   call intconv(mstep(3),getint2)
	   nm2=nblank2(getint1)
	   call intconv(mstep(4),getint3)
	   nm3=nblank1(getint3)
	   call intconv(mstep(5),getint4)
	   nm4=nblank1(getint4)
	   dtext(ndtext)=getint(1:nm)//
     &   getint1(1:nm1)//getint2(1:nm2)//
     &   getint3(1:nm3)//getint4(1:nm4)
	   n=nsweep
	   if(jkeep.eq.0) n=1	!same for all sweeps
         do 27 m=1,n
		call realtoch(tkpre,getreal,13)
		nm=nblank1(getreal)
		call realtoch(tkpost,getreal1,13)
		nm1=nblank1(getreal1)
	      if(jkeep.eq.0) then
	         ndtext=ndtext+1      !3/2
	   	   dtext(ndtext)=' For all sweeps:'
	      else if(jkeep.eq.-1) then
		   ndtext=ndtext+1      !3/2
		   dtext(ndtext)=' Keep from '//
     &         getreal(1:nm)//'ms before each C-jump to '
     &         //'ms after each'//getreal1(1:nm1)
	      else if(jkeep.eq.-2) then
		   ndtext=ndtext+1      !3/2
		   dtext(ndtext)=' Keep from '//
     &         getreal(1:nm)//'ms before each V-jump to '
     &         //'ms after each'//getreal(1:nm1)
		endif
	      if(jkeep.ne.0) then
	         call CALCNS0(ikeep,nkeep,kstep,nsamp,nsamp1,m)
		   ndtext=ndtext+1      !4/2
	   	   call intconv(m,getint)
	         nm=nblank1(getint)
	   	   call intconv(nsamp1,getint1)
	         nm1=nblank1(getint1)
               dtext(ndtext)=' For sweep number '//
     &         getint(1:nm)//' ('//
     &         getint1(1:nm1)//' points kept)'
	      endif
	      do 28 i=1,nkeep
		   t1=float(ikeep(i,1,m)-1)*dx
		   t2=float(ikeep(i,2,m)-1)*dx
		   ndtext=ndtext+1      !5,6,7,8,9/2
	   	   call intconv(i,getint)
	         nm=nblank1(getint)
		   call realtoch(t1,getreal,13)
		   nmr=nblank1(getreal)
		   call realtoch(t2,getreal1,13)
		   nmr1=nblank1(getreal1)
	         dtext(ndtext)=' ('//getint(1:nm)//
     &	   ') Keep from '//getreal(1:nmr)//' to '//
     &         getreal(1:nmr1)//' ms (point '//getint(1:nm)//
     &         ' to '//getint(1:nm)
28	      continue
27	      continue
	endif
	ndtext=ndtext+1               !6/1
	mvhold=int4(ivhold)
	call intconv(mvhold,getint)
	nm=nblank1(getint)
	dtext(ndtext)='Holding potential set by 1401 (mV) = '//
     &	getint(1:nm)
	ndtext=ndtext+1               !6/1
	mvclamp=int4(ivhclamp)
	call intconv(mvclamp,getint)
	nm=nblank1(getint)
	dtext(ndtext)='Holding potential on patch clamp (mV) = '//
     &	getint(1:nm)
	if(ncjump.eq.0) goto 68
	ndtext=ndtext+1               !7/1
	dtext(ndtext)='CONCENTRATION JUMP'
	ndtext=ndtext+1               !8/1
	call realtoch(tprec1,getreal,13)
	nmr=nblank1(getreal)
	dtext(ndtext)='   Time to start of (first) C-jump (ms) = '
     &   //getreal(1:nmr)
	ndtext=ndtext+1               !9/1
	call intconv(nac,getint)
	nm=nblank1(getint)
	call realtoch(dnac,getreal,13)
	nmr=nblank1(getreal)
	dtext(ndtext)='   (ie ADC point # '//getint(1:nm)//
     &   ' is '//getreal(1:nmr)//' microsec'
	ndtext=ndtext+1               !10/1
      dtext(ndtext)='   before start of C-jump)'
	do 62 i=1,ncjump
	   	ndtext=ndtext+1         ! 11/1
		call intconv(i,getint)
		nm=nblank1(getint)
		call realtoch(alenc(i),getreal,13)
		nmr=nblank1(getreal)
		dtext(ndtext)='   Concentration pulse '//
     &      getint(1:nm)//':duration (ms)= '//
     &      getreal(1:nmr)
		if(i.eq.ncjump) goto 62
	   	ndtext=ndtext+1         ! 12/1
		call realtoch(agapc(i),getreal,13)
		nmr=nblank1(getreal)
		dtext(ndtext)='   Gap between this pulse and next (ms) = '
     &      //getreal(1:nmr)
62    continue

68	continue
	if(.not.vjump)  goto 777       !goto 999
c=========================================================================
	itprev1=itPREv/1000
	ndtext=ndtext+1			!1/2
	dtext(ndtext)='VOLTAGE JUMPS/RAMPS'
	ndtext=ndtext+1               !2/2
	call intconv(itprev1,getint)
	nm=nblank1(getint)
	dtext(ndtext)='   Time to start of (first) V-jump (ms) = '
     &      //getint(1:nm)
	ndtext=ndtext+1               !3/2
	call intconv(nav,getint)
	nm=nblank1(getint)
	call realtoch(dnav,getreal,13)
	nmr=nblank1(getreal)
	dtext(ndtext)='   (ie ADC point # '//getint(1:nm)
     &      //' is '//getreal(1:nm)//' microsec'
	ndtext=ndtext+1               !4/2
      dtext(ndtext)='   before start of V-jump)'
	do 621 i=1,nvjump
	   ramp=ivolt1(i).ne.ivolt2(i)	!this one is a ramp
	   call intconv(i,getint)
	   nm=nblank1(getint)
	   call realtoch(alenv(i),getreal,13)
	   nmr=nblank1(getreal)
	   if(.not.ramp) then
		mpiz=ivolt1(i)
		ndtext=ndtext+1         !5/2
		if(ismode.eq.2) then
		dtext(ndtext)='   #'//getint(1:nm)//
     &	': Voltage jump;duration(ms) = '//getreal(1:nmr)
		else
		dtext(ndtext)='   #'//getint(1:nm)//
     &	': Voltage jump;duration(ms) = '//getreal(1:nmr)
     &      //';'
		ndtext=ndtext+1         !5/2
	      call intconv(mpiz,getint)
	      nm=nblank1(getint)
		dtext(ndtext)='   potential (mV) = '//getint(1:nm)
		endif
	   else
		mpiz1=int4(ivolt1(i))
		mpiz2=int4(ivolt2(i))
		ndtext=ndtext+1                !5/2
		dtext(ndtext)='   #'//getint(1:nm)//
     &      ': Voltage ramp;duration (ms) = '//getreal
     &      (1:nmr)
		ndtext=ndtext+1                !6/2
	      call intconv(mpiz1,getint)
	      nm=nblank1(getint)
	      call intconv(mpiz2,getint1)
	      nm1=nblank1(getint1)
	      call realtoch(vstep(i),getreal1,13)
	      nmr1=nblank1(getreal1)
            dtext(ndtext)='   from '//getint(1:nm)
     &      //' mv to '//getint(1:nm)//' mV (step size = '//
     &      getreal1(1:nmr)//' mV)'
	   endif
	   if(i.eq.nvjump) goto 621
	   ndtext=ndtext+1            !8/2
	   call realtoch(agapv(i),getreal,13)
	   nmr=nblank1(getreal)
	   dtext(ndtext)='   Gap between this one and next (ms) = '
     &   //getreal(1:nmr)
621	continue

	if(sampv) then
		ndtext=ndtext+1               !9/2
		dtext(ndtext)=
     &	'   Membrane potential sampled on ADC1'
	endif
	if(control) then
		ndtext=ndtext+1               !10/2
		dtext(ndtext)='   CONTROL: V-jump only (no C-jump)'
	endif

c Print calibration
	ndtext=ndtext+1               !11/2
	call realtoch(amvpa1(i),getreal,13)
	nmr=nblank1(getreal)
	dtext(ndtext)='   Calibration: mV/pA = '//
     &	getreal(1:nmr)//' : '
	ndtext=ndtext+1               !12/2
	call realtoch(gain,getreal,13)
	nmr=nblank1(getreal)
	call realtoch(ftape,getreal1,13)
	nmr1=nblank1(getreal1)
      dtext(ndtext)='   tape factor, gain = '//getreal1(1:nmr1)//
     &	' , '//getreal(1:nmr)
	ndtext=ndtext+1               !10/2
	call realtoch(calfac,getreal,13)
	nmr=nblank1(getreal)
	dtext(ndtext)='   Current units per ADC unit; calfac = '
     &      //getreal(1:nmr)
	ndtext=ndtext+1               !11/2
	call realtoch(calvolt,getreal,13)
	nmr=nblank1(getreal)
	dtext(ndtext)='   mV out from clamp per mV membrane pot ='
     &      //getreal(1:nmr)
c================================================================
777	continue
c
c Details for multiple sweeps (nsweep,swtime,ismode,swval)
	call intconv(nsweep,getint)
	nm=nblank1(getint)
	call realtoch(swtime,getreal,13)
	nmr=nblank1(getreal)
	if(nsweep.gt.1) then
	   ndtext=ndtext+1            !1
	   dtext(ndtext)='MULTIPLE SWEEPS'
	   ndtext=ndtext+1            !1
	   dtext(ndtext)='   Series of '
     &   //getint(1:nm)//
     &   ' sweeps at intervals of '//getreal(1:nmr)//
     &    ' s '
	   if(ismode.eq.1) then
		ndtext=ndtext+1         !2
		dtext(ndtext)='   Identical sweeps'
	   else if(ismode.eq.2) then
		ndtext=ndtext+1         !2
		dtext(ndtext)='   Jump potentials (mV) = '
	   else if(ismode.eq.3) then
		ndtext=ndtext+1         !2
		dtext(ndtext)='   C-jump lengths (ms) = '
	   else if(ismode.eq.4) then
		ndtext=ndtext+1         !2
		dtext(ndtext)='   Gaps bet C-jumps (ms) = '
	   endif
	   if(ismode.gt.1) then
	      ndtext=ndtext+1
		call realtoch(swval(1),getreal,13)
		nmr=nblank1(getreal)
		dtext(ndtext)='   '//getreal(1:nmr)//' ; '
		do 5 i=2,nsweep
		   md=nblank1(dtext(ndtext))
		   call realtoch(swval(i),getreal,13)
		   nmr=nblank1(getreal)
		   dtext(ndtext)=dtext(ndtext)(1:md)//
     &	   getreal(1:nmr)//' ; '
		   if(md.gt.35.and.first.eq.0) then
			first=1
	      	ndtext=ndtext+1
			dtext(ndtext)=' & '
		   endif
5		continue
	   endif
	   if(randomiz.and.ismode.eq.2) then
	      ndtext=ndtext+1
            dtext(ndtext)='   Random sequence :'
	      ndtext=ndtext+1
		isweep=1
         	jswp=iperm(isweep)
		call realtoch(swval(jswp),getreal,13)
		nmr=nblank1(getreal)
		dtext(ndtext)='   '//getreal(1:nmr)//' ; '
		do 55 i=2,nsweep
         	   jswp=iperm(i)
		   md=nblank1(dtext(ndtext))
		   call realtoch(swval(jswp),getreal,13)
		   nmr=nblank1(getreal)
		   dtext(ndtext)=dtext(ndtext)(1:md)//
     &	   getreal(1:nm)//' ; '
		   if(md.gt.35.and.first.eq.0) then
			first=1
	      	ndtext=ndtext+1
			dtext(ndtext)=' & '
		   endif
55		continue
	   endif
	endif

999	continue
	nval=ndtext
	if(ndtext.le.nrow) nval=nrow
	call POPPARS(ixlo,-1,iyhi,dtext,header,helps,nhelp,
     &	nrow,nval,ic,ibk,icf,ich,options,nopt,iopt,ans,ival)

	RETURN      !from PPAR
	end


