	subroutine PPAR4(iprt,idprt,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,
     &iDd,calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     & vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     & amVpA1,ftape,gain,nsweep,swtime,ismode,swval,nkeep,ikeep,kstep,
     & jkeep,nsamp1,tkpre,tkpost,iramp)
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
	real*4 vstep(10) 		!for GETSTEP
	real*4 swval(30) 		!values that change between sweeps
	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	integer*4 iramp
	real*4 alenv(10),agapv(10),alenc(10),agapc(10)
	integer*2 ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
	integer*2 ikeep(4,2,30),kstep(5)
	character*11 cdate,ctime
	character*10 getint
	character*13 getreal
	character*78 dtext(30,2)
	integer*4 videotyp

	logical pon,slock,vjump,ramp,sampv,control
	logical discprt
	common/dp/discprt
	common/dtext/ndtext
c
	pon()=slock()
c
	do 20 i=1,10	!convert to real msec for printing
	   alenv(i)=1.e-3*float(ilenv(i))
	   agapv(i)=1.e-3*float(igapv(i))
	   alenc(i)=1.e-3*float(ilenc(i))
	   agapc(i)=1.e-3*float(igapc(i))
20	continue
c      if(iprt.eq.2) print 60,irate
c      if(pon()) write(7,60) irate
c      if(idprt.eq.2) write(8,60) irate
c60	format(' Sample rate (Hz) = ',i8)
c===== vstep set to 1 ms for now for interpolation
	call GETSTEP(nvjump,ivolt1,ivolt2,ilenv,iDd,nvramp,vstep)
c
	idp=idprt
	if(.not.discprt) idp=0	!disc file not open
	itsamp1=itsamp/1000
c	itprec1=itPREc/1000
	tprec1=float(itPREc)/1000.
c	iTPOSTc=iTSAMP-iTPREc
c Brief print to disc

c============================================================================

	if(idp.eq.1.or.iprt.eq.1) then
79	   format('&',/)
	   if(iprt.eq.1) print 70, ctime,itsamp1,ivhold
	   if(pon()) write(7,70) ctime,itsamp1,ivhold
	   if(idp.eq.1) write(8,70) ctime,itsamp1,ivhold
70	   format(1x,a11,': ADC ',i5,'ms; ','Vhold ',i4)
	   nchar=36
	   if(ncjump.gt.0) then
c		nchar=nchar+14+8*ncjump
		nchar=nchar+14+7*ncjump+7*(ncjump-1)      !length + gap
		if(nchar.gt.79) then
		   if(iprt.eq.1) print 79
		   if(pon()) write(7,79)
		   if(idp.eq.1) write(8,79)
		   nchar=0
		endif
		if(iprt.eq.1) print 72
		if(pon()) write(7,72)
		if(idp.eq.1) write(8,72)
72		format('&; C-jump (ms) ')
c		if(iprt.eq.1) print 73, (alenc(j),j=1,ncjump)
c		if(pon()) write(7,73) (alenc(j),j=1,ncjump)
c		if(idp.eq.1) write(8,73) (alenc(j),j=1,ncjump)
		do 731 j=1,ncjump
		if(iprt.eq.1) print 73,alenc(j)
		if(pon()) write(7,73) alenc(j)
		if(idp.eq.1) write(8,73) alenc(j)
73		format('&',f7.1)
		if(j.eq.ncjump) goto 731
		if(iprt.eq.1) print 732,agapc(j)
		if(pon()) write(7,732) agapc(j)
		if(idp.eq.1) write(8,732) agapc(j)
732		format('&(',f7.1,')')
731		continue
	   endif
	   if(nvjump.gt.0) then
		do 751 i=1,nvjump
		if(ivolt1(i).eq.ivolt2(i)) then
		  nchar=nchar+29
		  if(nchar.gt.79) then
			if(iprt.eq.1) print 79
			if(pon()) write(7,79)
			if(idp.eq.1) write(8,79)
		      nchar=0
		  endif
		  if(iprt.eq.1) print 74
		  if(pon()) write(7,74)
		  if(idp.eq.1) write(8,74)
74	        format('&; V-jump ')
		  if(iprt.eq.1) print 75, alenv(i),ivolt1(i)
		  if(pon()) write(7,75) alenv(i),ivolt1(i)
		  if(idp.eq.1) write(8,75) alenv(i),ivolt1(i)
75		  format('&',f8.1,'ms to ',i4,'mV')
		else
		  nchar=nchar+36
		  if(nchar.gt.79) then
			if(iprt.eq.1) print 79
			if(pon()) write(7,79)
			if(idp.eq.1) write(8,79)
		      nchar=0
		  endif
		  if(iprt.eq.1) print  76
		  if(pon()) write(7,76)
		  if(idp.eq.1) write(8,76)
76	        format('&; V-ramp ')
		  if(iprt.eq.1) print 77, alenv(i),ivolt1(i),ivolt2(i)
		  if(pon()) write(7,77) alenv(i),ivolt1(i),ivolt2(i)
		  if(idp.eq.1) write(8,77)alenv(i),ivolt1(i),ivolt2(i)
77		  format('&',f8.1,'ms; ',i4,' to ',i4,'mV;')
		endif
751		continue
	   endif
	   if(iprt.le.1.and.idp.le.1) RETURN		!after brief print
	endif
c
c=====================================================================

c

c     if(iprt.eq.2) print 61,cdate,ctime,irate,itsamp1,nsamp,iDd
c--------------------------------------------------------------
	if(iptr.eq.2) then
	   ndtext=ndtext+1
	   dtext(ndtext,2)=cdate//ctime
	   ndtext=ndtext+1
	   dtext(ndtext,2)='Sample rate (Hz) ='//getint(irate,8)
	   ndtext=ndtext+1
	   dtext(ndtext,2)='Sample length ='//getint(itsamp1,6)//'ms (
     &   '//getint(nsamp,6)//'points'
	   ndtext=ndtext+1
	   dtext(ndtext,2)='Microseconds between DAC points = '//
     &   getint(iDd,10)
	endif
c------------------------------------------------------------------
      if(pon()) write(7,61)cdate,ctime,irate,itsamp1,nsamp,iDd
      if(idp.eq.2) write(8,61)cdate,ctime,irate,itsamp1,nsamp,iDd
61	format(1x,a11,3x,a11,/,' Sample rate (Hz) = ',i8,
     & ' Sample length = ',i6,' ms (',i6,' points)',/,
     & ' Microseconds between DAC points = ',i10)

	nline=3
c Print IKEEP etc
292	continue
	dx=1.e3/float(irate)
	if(nkeep.eq.1.and.ikeep(1,1,1).eq.1.and.
     & int4(ikeep(1,2,1)).eq.nsamp) then
c	   if(iprt.eq.2) print 29
c---------------------------------------------------------
	   if(iprt.eq.2) then
	   ndtext=ndtext+1
	      dtext(ndtext,2)=' Whole ADC sample kept'
	   endif
c------------------------------------------------------------
	   if(pon()) write(7,29)
         if(idp.eq.2) write(8,29)
29	    format(' Whole ADC sample kept')
	else
c	   if(iprt.eq.2) print 281,nkeep,nsamp1
c--------------------------------------------------------------
	   if(iprt.eq.2) then
	   ndtext=ndtext+1
	      dtext(ndtext,2)=' Number of sections of ADC sample kept = '//
     &      getint(nkeep,3)//' ('//getint(nsamp1,5)//' points)'
	   endif
c--------------------------------------------------------------
	   if(pon()) write(7,281) nkeep,nsamp1
         if(idp.eq.2) write(8,281) nkeep,nsamp1
281	   format(
     & ' Number of sections of ADC sample kept = ',i3,' (',i5,
     & ' points)')

c==============================================
c somewhere else!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	   if(VIDEOTYP().eq.18) goto 981	!don't type rest if in graphics
	   nline=nline+2
c	   if(iprt.eq.2) print 98,(kstep(i),i=1,nkeep+1)
	   if(pon()) write(7,98) (kstep(i),i=1,nkeep+1)
	   if(idp.eq.2) write(8,98) (kstep(i),i=1,nkeep+1)
98	   format(
     &  '  -Outside specified sections keep also every nth point: n= ',
     &	5i3)
	   n=nsweep
c	   if(jkeep.le.0) n=1	!same for all sweeps,or same rel to jumps
	   if(jkeep.eq.0) n=1	!same for all sweeps
c
	   do 27 m=1,n
	   nline=nline+1
	   if(nline.gt.20.and.iprt.gt.0) then
		print 650
650		format(' Hit any key to continue')
		call ANYKEY
		nline=0
	   endif
	   if(jkeep.eq.0) then
c	     if(iprt.eq.2) print 2831

	     if(pon()) write(7,2831)
           if(idp.eq.2) write(8,2831)
2831	     format(' For all sweeps:')
	   else if(jkeep.eq.-1) then
c	     if(iprt.eq.2) print 284,tkpre,tkpost
	     if(pon()) write(7,284) tkpre,tkpost
           if(idp.eq.2) write(8,284) tkpre,tkpost
284	     format(
     &	' Keep from ',f8.1,'ms before each C-jump to ',f8.1,
     &	'ms after each')
	   else if(jkeep.eq.-2) then
c	     if(iprt.eq.2) print 285,tkpre,tkpost
	     if(pon()) write(7,285) tkpre,tkpost
           if(idp.eq.2) write(8,285) tkpre,tkpost
285	     format(
     &	' Keep from ',f8.1,'ms before each V-jump to ',f8.1,
     &	'ms after each')
	   endif
c	   else if(jkeep.eq.1) then
	   if(jkeep.ne.0) then
c also print nsamp1
	     call CALCNS0(ikeep,nkeep,kstep,nsamp,nsamp1,m)
c	     if(iprt.eq.2) print 283,m,nsamp1
	     if(pon()) write(7,283) m,nsamp1
           if(idp.eq.2) write(8,283) m,nsamp1
283	     format(' For sweep number ',i3,' (',i5,' points kept)')
	   endif
	   do 28 i=1,nkeep
		t1=float(ikeep(i,1,m)-1)*dx
		t2=float(ikeep(i,2,m)-1)*dx
c	      if(iprt.eq.2) print 282,i,t1,t2,ikeep(i,1,m),ikeep(i,2,m)
	      if(pon()) write(7,282) i,t1,t2,ikeep(i,1,m),ikeep(i,2,m)
            if(idp.eq.2) write(8,282) i,t1,t2,ikeep(i,1,m),ikeep(i,2,m)
282		format(' (',i2,
     & ') Keep from ',g13.6,' to ',g13.6,' ms (point ',i5,' to ',i5,')')
	      nline=nline+1
	      if(nline.gt.20.and.iprt.gt.0) then
		   print 650
		   call ANYKEY
		   nline=0
	      endif
28	   continue
27	   continue
c
	endif
c========


c=========================================================
c
981	continue
	if(ncjump.eq.0) goto 66
c      if(iprt.eq.2) print 65,tPREc1,nAc,dnAc
c--------------------------------------------------
	if(iprt.eq.2) then
	   ndtext=ndtext+1
	   dtext(ndtext,2)=' Concentration jump'
	   ndtext=ndtext+1
	   dtext(ndtext,2)='Time to start of (first) C-jump (ms) = '//
     &   getreal(tprec1,8)
	   ndtext=ndtext+1
	   dtext(ndtext,2)='(ie ADC point #'//getint(nax,6)//' is '//
     &   getreal(dnac,9)// 'microsec before start of C-jump)'
	endif
c------------------------------------------------------------
      if(pon()) write(7,65) tPREc1,nAc,dnAc
      if(idp.eq.2) write(8,65) tPREc1,nAc,dnAc
65	format(
     & ' Concentration jump',/,
     & '   Time to start of (first) C-jump (ms) = ',f8.2,/,
     & '  (ie ADC point #',i6,' is ',f9.2,
     & ' microsec before start of C-jump)')
c     & '  (ie ADC point #',i6,' coincides with start of C-jump)')
	 nline=nline+4
	 if(nline.gt.20.and.iprt.gt.0) then
		print 650
		call ANYKEY
		nline=0
	 endif
	do 62 i=1,ncjump
c     if(iprt.eq.2) print 63,i,alenc(i)
c-----------------------------------------------
	if(iprt.eq.2) then
	   	ndtext=ndtext+1
		dtext(ndtext,2)=' Concentration pulse '//getint(i,2)//
     &      ':duration (ms)= '//getreal(alenc(i),10)
	endif
c------------------------------------------------
      if(pon()) write(7,63) i,alenc(i)
      if(idp.eq.2) write(8,63) i,alenc(i)
63	format(
     & ' Concentration pulse ',i2,': duration (ms) = ',f10.1)
	if(i.eq.ncjump) goto 62
c      if(iprt.eq.2) print 64,agapc(i)
c--------------------------------------------------
	if(iprt.eq.2) then
	   	ndtext=ndtext+1
		dtext(ndtext,2)= '  gap between this pulse and next (ms) = '
     &      //getreal(agapc(i),10 )
	endif
c---------------------------------------------------
      if(pon()) write(7,64) agapc(i)
      if(idp.eq.2) write(8,64) agapc(i)
64	format(
     & '    gap between this pulse and next (ms) = ',f10.1)
	 nline=nline+2
	 if(nline.gt.20.and.iprt.gt.0) then
		print 650
		call ANYKEY
		nline=0
	 endif
62	continue

c
66	continue
c      if(iprt.eq.2) print 611,ivhold
c-----------------------------------------------------------------
	if(iprt.eq.2) then
		ndtext=ndtext+1
		dtext(ndtext,2)=' Holding potential (mV) = '//
     &	getint(ivhold,4)
	endif
c-----------------------------------------------------------------
      if(pon()) write(7,611) ivhold
      if(idp.eq.2) write(8,611) ivhold
611	format(' Holding potential (mV) = ',i4)
c
	if(.not.vjump) RETURN
c
c	iTPOSTv=iTSAMP-iTPREv
	itprev1=itPREv/1000
c	iTPOSTv1=iTPOSTv/1000
c     if(iprt.eq.2) print 612,iTPREv1,iTPOSTv1,nAv
c     if(pon()) write(7,612) iTPREv1,iTPOSTv1,nAv
c     if(idp.eq.2)write(8,612) iTPREv1,iTPOSTv1,nAv
c     & ' Sample length (ms): before, after 1st V-jump = ',2i6,/,
c     & ' (ie ADC point #',i6,' coincides with start of V-jump)')
c      if(iprt.eq.2) print 612,iTPREv1,nAv,dnAv
c-----------------------------------------------------------------
	if(iprt.eq.2) then
		ndtext=ndtext+1
		dtext(ndtext,2)=' Voltage jumps/ramps'
		ndtext=ndtext+1
		dtext(ndtext,2)='Time to start of (first) V-jump (ms) = '
     &      //getint(itprev1,6)
		ndtext=ndtext+1
		dtext(ndtext,2)='(ie ADC point #'//getint(nav,6)//' is '//
     &	getreal(dnav,9)//
     &      ' microsec before start of V-jump)'
	endif
c-----------------------------------------------------------------
      if(pon()) write(7,612) iTPREv1,nAv,dnAv
      if(idp.eq.2)write(8,612) iTPREv1,nAv,dnAv
612	format(
     & ' Voltage jumps/ramps',/,
     & '   Time to start of (first) V-jump (ms) = ',i6,/,
     & '  (ie ADC point #',i6,' is ',f9.2,
     & ' microsec before start of V-jump)')
	 nline=nline+4
	 if(nline.gt.20.and.iprt.gt.0) then
		print 650
		call ANYKEY
		nline=0
	 endif
c
	do 621 i=1,nvjump
	ramp=ivolt1(i).ne.ivolt2(i)	!this one is a ramp
	if(.not.ramp) then
c         if(iprt.eq.2) print 631,i,alenv(i),ivolt1(i)
c----------------------------------------------------------------------
	    if(iprt.eq.2) then
		ndtext=ndtext+1
		dtext(ndtext,2)='#'//getint(i,2)//': Voltage jump; duration
     &      (ms) = '//getreal(alenv(i),10)//'; potential (mV)'//
     &      getint(ivolt1(i),4)
	   endif
c--------------------------------------------------------------------
         if(pon()) write(7,631) i,alenv(i),ivolt1(i)
         if(idp.eq.2) write(8,631) i,alenv(i),ivolt1(i)
631	   format(' #',i2,
     & ': Voltage jump; duration (ms) = ',f10.1,'; potential (mV)',i4)
	   nline=nline+1
	else
c         if(iprt.eq.2) print 632,i,alenv(i),ivolt1(i),ivolt2(i),vstep(i)
c----------------------------------------------------------------------
	    if(iprt.eq.2) then
		ndtext=ndtext+1
		dtext(ndtext,2)='#'//getint(i,2)//': Voltage ramp; duration
     &      (ms) = '//getreal(alenv(i),10)//'from'//getint(ivolt1(i),5)
     &      //'mv to'//getint(ivolt2(i),5)
		ndtext=ndtext+1
		dtext(ndtext,2)='(step size = '//getreal(vstep(i),10)//'mV)'
	   endif
c--------------------------------------------------------------------

         if(pon()) write(7,632) i,alenv(i),ivolt1(i),ivolt2(i),vstep(i)
         if(idp.eq.2) write(8,632)i,alenv(i),ivolt1(i),ivolt2(i),
     &	vstep(i)
632	   format(' #',i2,
     & ': Voltage ramp; duration (ms) = ',f10.1,'; from',i5,' mV to',i5,
     &  ' mV',/,'   (step size = ',f10.3,' mV)')
	   nline=nline+2
	endif
	if(i.eq.nvjump) goto 6211
c      if(iprt.eq.2) print 641,agapv(i)
c
c--------------------------------------------------------------------
	    if(iprt.eq.2) then
		ndtext=ndtext+1
		dtext(ndtext,2)='gap between this one and next (ms) = '//
     &      getreal(agapv(i),10)
	    endif
c--------------------------------------------------------------------

      if(pon()) write(7,641) agapv(i)
      if(idp.eq.2) write(8,641) agapv(i)
641	format(
     & '    gap between this one and next (ms) = ',f10.1)
	nline=nline+1
6211	 if(nline.gt.20.and.iprt.gt.0) then
		print 650
		call ANYKEY
		nline=0
	 endif
621	continue
c
	if(sampv) then
c        if(iprt.eq.2) print 51
c-----------------------------------------------------
	    if(iprt.eq.2) then
		ndtext=ndtext+1
		dtext(ndtext,2)=' Membrane potential sampled on ADC1'
	    endif
c------------------------------------------------
        if(pon()) write(7,51)
        if(idp.eq.2) write(8,51)
51	  format(' Membrane potential sampled on ADC1')
	endif
	if(control) then
c        if(iprt.eq.2) print 52
c-----------------------------------------------------
	    if(iprt.eq.2) then
		ndtext=ndtext+1
		dtext(ndtext,2)=' CONTROL: V-jump only (no C-jump)'
	    endif
c------------------------------------------------
        if(pon()) write(7,52)
        if(idp.eq.2) write(8,52)
52	  format(' CONTROL: V-jump only (no C-jump)')
	endif



c
c Details for multiple sweeps (nsweep,swtime,ismode,swval)
	if(nsweep.gt.1) then
c         if(iprt.eq.2) print 53,nsweep,swtime
c-----------------------------------------------------
	    if(iprt.eq.2) then
		ndtext=ndtext+1
		dtext(ndtext,2)=' SERIES of '//getint(nsweeps,3)//'sweeps at
     & 	intervals of '//getreal(swtime,8)//' seconds'
	    endif
c------------------------------------------------
         if(pon()) write(7,53) nsweep,swtime
         if(idp.eq.2) write(8,53) nsweep,swtime
53	   format(
     & ' SERIES of ',i3,' sweeps at intervals of ',f8.2,' seconds')
	   nline=nline+2
	   if(ismode.eq.2) then
c            if(iprt.eq.2) print 54
c-----------------------------------------------------
	    if(iprt.eq.2) then
		ndtext=ndtext+1
		dtext(ndtext,2)=' Jump potentials (mV) = '
	    endif
c------------------------------------------------
            if(pon()) write(7,54)
            if(idp.eq.2) write(8,54)
54		format(' Jump potentials (mV) = ')
	      nchar=23
	   else if(ismode.eq.3) then
c            if(iprt.eq.2) print 55
c-----------------------------------------------------
	    if(iprt.eq.2) then
		ndtext=ndtext+1
		dtext(ndtext,2)=' C-jump lengths (ms) = '
	    endif
c------------------------------------------------
            if(pon()) write(7,55)
            if(idp.eq.2) write(8,55)
55		format(' C-jump lengths (ms) = ')
	      nchar=22
	   else if(ismode.eq.4) then
c            if(iprt.eq.2) print 56
c-----------------------------------------------------
	    if(iprt.eq.2) then
		ndtext=ndtext+1
		dtext(ndtext,2)=' Gaps bet C-jumps (ms) = '
	    endif
c------------------------------------------------
            if(pon()) write(7,56)
            if(idp.eq.2) write(8,56)
56		format(' Gaps bet C-jumps (ms) = ')
	      nchar=24
	   endif
	   if(ismode.gt.1) then
		ncl=nchar		!number of char printed on each line
		do 5 i=1,nsweep
		ncl=ncl+7		!number of char printed AFTER next one
80		format('&',f7.1)
81		format(f7.1)
		if(ncl.gt.79) then	!if next would go past end of line, start new line
c               if(iprt.eq.2) print 81,swval(i)
c-----------------------------------------------------
	    if(iprt.eq.2) then
		ndtext=ndtext+1
		dtext(ndtext,2)=getreal(swval(i),7)
	    endif
c------------------------------------------------
               if(pon()) write(7,81) swval(i)
               if(idp.eq.2) write(8,81) swval(i)
		   nline=nline+1
		   ncl=7
		else
c               if(iprt.eq.2) print 80,swval(i)
c-----------------------------------------------------
	    if(iprt.eq.2) then
		ndtext=ndtext+1
		dtext(ndtext,2)='&'//getreal(swval(i),7)
	    endif
c------------------------------------------------
               if(pon()) write(7,80) swval(i)
               if(idp.eq.2) write(8,80) swval(i)
		endif
	      if(nline.gt.20.and.iprt.gt.0) then
		  print 650
		  call ANYKEY
		  nline=0
	      endif
5		continue
	   endif
	endif

c========================================
c????????????????????????????????????????
c
c Print calibration
	nline=nline+3
	if(nline.gt.20.and.iprt.gt.0) then
	   print 650
	   call ANYKEY
	   nline=0
	endif
c      if(iprt.eq.2) print 50,amVpA1,ftape,gain,calfac,calvolt
c-----------------------------------------------------
	    if(iprt.eq.2) then
		ndtext=ndtext+1
		dtext(ndtext,2)=' Calibration: mV/pA = '//getreal(amvpa1,7)
     &      //': tape factor, gain = '//getreal(ftape,13)//
     &	getreal(gain,13)
		dtext(ndtext,2)=' Current units per ADC unit; calfac = '//
     &      getreal(calfac,13)
		dtext(ndtext,2)=' mV out from clamp per mV membrane pot = '
     &      //getreal(calvolt,13)
	    endif
c------------------------------------------------
      if(pon()) write(7,50) amVpA1,ftape,gain,calfac,calvolt
      if(idp.eq.2) write(8,50) amVpA1,ftape,gain,calfac,calvolt
50	format(
     & ' Calibration: mV/pA = ',f7.1,': tape factor, gain = ',2g13.6,/,
     & ' Current units per ADC unit; calfac = ',g13.8,/,
     & ' mV out from clamp per mV membrane pot = ',g13.6,/)

      call defdialog(2,23,2,20,68,icb)
      call opendialog(2,7,.true.)
	if(ndtext.gt.20) call scroldial(2)

c
	RETURN      !from PPAR
	end


	character*(*) function getint(input,n)

	call intconv(input,getint)
	getint=getint(1:n)

	end

	character*(*) function getreal(rinput,n)

	call realtoch(rinput,getreal,n)

	end
