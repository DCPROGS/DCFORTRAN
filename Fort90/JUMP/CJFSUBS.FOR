c Lahey V5 version of CJFSUBS for CJFIT (12/02/94 12:51pm)

	subroutine PPAR(iprt,idprt,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,
     &iDd,calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     & vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     & iver1,amVpA1,ftape,gain,errfac,nkeep,ikeep1,kstep,keepall,
     & ivhclamp,ipatch,ffilt)
c To type/print parameter values.
c IPRT=0	No print to screen
c IPRT=1   Print brief details to screen
c IPRT=2   Print full details to screen
c IDPRT=0	No print to disc
c IDPRT=1   Print brief details to disc (for each jump recorded)
c IDPRT=2   Print full details to disc (only when params changed)
c      ipatch=(1) outside-out', (2) inside-out', (3) cell-attached',/,
c	 (4) whole-cell', (5) simulated data'
	real*4 vstep(10) 		!for GETSTEP
	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	real*4 alenv(10),agapv(10),alenc(10),agapc(10)
	integer*2 ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
	integer*2 ivhclamp,ipatch,ivh,iv1,iv2
	character*11 cdate,ctime
	character*14 ptype
	logical pon,prt,slock,vjump,ramp,sampv,control
c for CJUMP3 data
	integer*2 iver1
	integer*4 ikeep1(4,2)
	integer*2 kstep(5)
	logical keepall
c
	logical discprt
	common/dp/discprt
c
	pon()=slock()
c
	prt=pon()
	do 20 i=1,10	!convert to real msec for printing
	   alenv(i)=1.e-3*float(ilenv(i))
	   agapv(i)=1.e-3*float(igapv(i))
	   alenc(i)=1.e-3*float(ilenc(i))
	   agapc(i)=1.e-3*float(igapc(i))
20	continue
	if(ipatch.eq.1) then
	   ptype='outside-out   '
	else if(ipatch.eq.2) then
	   ptype='inside-out    '
	else if(ipatch.eq.3) then
	   ptype='cell-attached '
	else if(ipatch.eq.4) then
	   ptype='whole-cell    '
	else if(ipatch.eq.5) then
	   ptype='simulated data'
	else
	   ptype='              '
	endif
c Calc actual holding potential
	ivh=ivhold+ivhclamp
	if(ipatch.eq.2.or.ipatch.eq.3) ivh=-ivh	!reverse pipette pot for i/o or c/a
	call GETSTEP(nvjump,ivolt1,ivolt2,ilenv,iDd,nvramp,vstep)
	idp=idprt
	if(.not.discprt) idp=0	!disc file not open
	itsamp1=itsamp/1000
	tprec1=float(itPREc)/1000.
c	itprec1=itPREc/1000
c	iTPOSTc=iTSAMP-iTPREc
c Brief print to disc
	if(idp.eq.1.or.iprt.eq.1) then
79	   format('&',/)
c	   if(iprt.eq.1) print 70, ctime(1:8),itsamp1,ivh
	   if(iprt.eq.1) print 70, ctime,itsamp1,ivh
	   if(prt) write(7,70) ctime,itsamp1,ivh
	   if(idp.eq.1) write(8,70) ctime,itsamp1,ivh
70	   format(1x,a11,': ADC ',i5,'ms; ','Vhold ',i4)
c	   nchar=33
	   nchar=36
	   if(ncjump.gt.0) then
c		nchar=nchar+14+8*ncjump
		nchar=nchar+14+7*ncjump+7*(ncjump-1)
		if(nchar.gt.79) then
		   if(iprt.eq.1) print 79
		   if(prt) write(7,79)
		   if(idp.eq.1) write(8,79)
		   nchar=0
		endif
		if(iprt.eq.1) print 72
		if(prt) write(7,72)
		if(idp.eq.1) write(8,72)
72		format('&; C-jump (ms) ')
c		if(iprt.eq.1) print 73, (alenc(j),j=1,ncjump)
c		if(prt) write(7,73) (alenc(j),j=1,ncjump)
c		if(idp.eq.1) write(8,73) (alenc(j),j=1,ncjump)
c73		format('&',f8.1)
		do 731 j=1,ncjump
		if(iprt.eq.1) print 73,alenc(j)
		if(prt) write(7,73) alenc(j)
		if(idp.eq.1) write(8,73) alenc(j)
73		format('&',f7.1)
		if(j.eq.ncjump) goto 731
		if(iprt.eq.1) print 732,agapc(j)
		if(prt) write(7,732) agapc(j)
		if(idp.eq.1) write(8,732) agapc(j)
732		format('&(',f7.1,')')
731		continue
	   endif
	   if(nvjump.gt.0) then
		do 751 i=1,nvjump
		iv1=ivolt1(i)+ivhclamp
		iv2=ivolt2(i)+ivhclamp
		if(ipatch.eq.2.or.ipatch.eq.3) then
		   iv1=-iv1	!reverse pipette pot for i/o or c/a
		   iv2=-iv2
		endif
		if(ivolt1(i).eq.ivolt2(i)) then
		  nchar=nchar+29
		  if(nchar.gt.79) then
			if(iprt.eq.1) print 79
			if(prt) write(7,79)
			if(idp.eq.1) write(8,79)
		      nchar=0
		  endif
		  if(iprt.eq.1) print 74
		  if(prt) write(7,74)
		  if(idp.eq.1) write(8,74)
74	        format('&; V-jump ')
		  if(iprt.eq.1) print 75, alenv(i),iv1
		  if(prt) write(7,75) alenv(i),iv1
		  if(idp.eq.1) write(8,75) alenv(i),iv1
75		  format('&',f8.1,'ms to ',i4,'mV')
		else
		  nchar=nchar+36
		  if(nchar.gt.79) then
			if(iprt.eq.1) print 79
			if(prt) write(7,79)
			if(idp.eq.1) write(8,79)
		      nchar=0
		  endif
		  if(iprt.eq.1) print  76
		  if(prt) write(7,76)
		  if(idp.eq.1) write(8,76)
76	        format('&; V-ramp ')
		  if(iprt.eq.1) print 77, alenv(i),iv1,iv2
		  if(prt) write(7,77) alenv(i),iv1,iv2
		  if(idp.eq.1) write(8,77)alenv(i),iv1,iv2
77		  format('&',f8.1,'ms; ',i4,' to ',i4,'mV;')
		endif
751		continue
	   endif
	   if(iprt.le.1.and.idp.le.1) RETURN		!after brief print
	endif

	if(iprt.eq.2) then
	   print 59,ptype,ffilt
59	   format(' Patch type = ',a14,'; filter (-3 dB) (Hz) = ',f8.1)
	endif
	if(prt) then
	   write(7,59) ptype,ffilt
	endif
c	if(idp.eq.2.and.ipatch.ge.1.and.ipatch.le.5) then
	if(idp.eq.2) then
	   write(8,59) ptype,ffilt
	endif
      if(iprt.eq.2) print 61,cdate,ctime,irate,itsamp1,nsamp,iDd
      if(prt) write(7,61)cdate,ctime,irate,itsamp1,nsamp,iDd
      if(idp.eq.2) write(8,61)cdate,ctime,irate,itsamp1,nsamp,iDd
61	format(1x,a11,3x,a11,/,' Sample rate (Hz) = ',i8,
     & ' Sample length = ',i6,' ms (',i6,' points)',/,
     & ' Microseconds between DAC points = ',i10)
c
c Print bits of orig sample were kept (if not all)
	if(.not.keepall) then
	   dx=1.e3/float(irate)
	   do 28 i=1,nkeep
		t1=float(ikeep1(i,1)-1)*dx
		t2=float(ikeep1(i,2)-1)*dx
	      if(iprt.eq.2) print 282,i,t1,t2,ikeep1(i,1),ikeep1(i,2)
	      if(prt) write(7,282) i,t1,t2,ikeep1(i,1),ikeep1(i,2)
            if(idp.eq.2) write(8,282) i,t1,t2,ikeep1(i,1),ikeep1(i,2)
282		format(' (',i2,
     & ') Keep from ',g13.6,' to ',g13.6,' ms (point ',i5,' to ',i5,')')
28	   continue
c
	   if(iprt.eq.2) print 98,(kstep(i),i=1,nkeep+1)
	   if(prt) write(7,98) (kstep(i),i=1,nkeep+1)
	   if(idp.eq.2) write(8,98) (kstep(i),i=1,nkeep+1)
98	   format(
     &  '  -Outside these sections keep also every nth point: n= ',5i3)
	endif
c C-jumps
	if(ncjump.eq.0) goto 66
      if(iprt.eq.2) print 65,tPREc1,nAc,dnAc
      if(prt) write(7,65) tPREc1,nAc,dnAc
      if(idp.eq.2) write(8,65) tPREc1,nAc,dnAc
65	format(
     & ' Concentration jump',/,
     & '   Time to start of (first) C-jump (ms) = ',f8.2,/,
     & '  (ie ADC point #',i6,' is ',f9.2,
     & ' microsec before start of C-jump)')
c     & '  (ie ADC point #',i6,' coincides with start of C-jump)')
	do 62 i=1,ncjump
      if(iprt.eq.2) print 63,i,alenc(i)
      if(prt) write(7,63) i,alenc(i)
      if(idp.eq.2) write(8,63) i,alenc(i)
63	format(
     & ' Concentration pulse ',i2,': duration (ms) = ',f10.1)
	if(i.eq.ncjump) goto 62
      if(iprt.eq.2) print 64,agapc(i)
      if(prt) write(7,64) agapc(i)
      if(idp.eq.2) write(8,64) agapc(i)
64	format(
     & '    gap between this pulse and next (ms) = ',f10.1)
62	continue
c
c
66	continue
      if(iprt.eq.2) print 611,ivhold,ivhclamp,ivh
      if(prt) write(7,611) ivhold,ivhclamp,ivh
      if(idp.eq.2) write(8,611) ivhold,ivhclamp,ivh
611	format(
     & ' Holding potential (mV) set on 1401 = ',i4,/
     & ' Holding potential (mV) set on clamp = ',i4,/
     & ' Actual holding potential (mV) = ',i4)
c
	if(.not.vjump) RETURN
c
c	iTPOSTv=iTSAMP-iTPREv
	itprev1=itPREv/1000
c	iTPOSTv1=iTPOSTv/1000
c     if(iprt.eq.2) print 612,iTPREv1,iTPOSTv1,nAv
c     if(prt) write(7,612) iTPREv1,iTPOSTv1,nAv
c     if(idp.eq.2)write(8,612) iTPREv1,iTPOSTv1,nAv
c     & ' Sample length (ms): before, after 1st V-jump = ',2i6,/,
c     & ' (ie ADC point #',i6,' coincides with start of V-jump)')
      if(iprt.eq.2) print 612,iTPREv1,nAv,dnAv
      if(prt) write(7,612) iTPREv1,nAv,dnAv
      if(idp.eq.2)write(8,612) iTPREv1,nAv,dnAv
612	format(
     & ' Voltage jumps/ramps',/,
     & '   Time to start of (first) V-jump (ms) = ',i6,/,
     & '  (ie ADC point #',i6,' is ',f9.2,
     & ' microsec before start of V-jump)')
c
	do 621 i=1,nvjump
	ramp=ivolt1(i).ne.ivolt2(i)	!this one is a ramp
	iv1=ivolt1(i)+ivhclamp
	iv2=ivolt2(i)+ivhclamp
	if(ipatch.eq.2.or.ipatch.eq.3) then
	   iv1=-iv1	!reverse pipette pot for i/o or c/a
	   iv2=-iv2
	endif
	if(.not.ramp) then
         if(iprt.eq.2) print 631,i,alenv(i),iv1
         if(prt) write(7,631) i,alenv(i),iv1
         if(idp.eq.2) write(8,631) i,alenv(i),iv1
631	   format(' #',i2,
     & ': Voltage jump; duration (ms) = ',f10.1,'; potential (mV)',i4)
	else
         if(iprt.eq.2) print 632,i,alenv(i),iv1,iv2,vstep(i)
         if(prt) write(7,632) i,alenv(i),iv1,iv2,vstep(i)
         if(idp.eq.2) write(8,632)i,alenv(i),iv1,iv2,
     &	vstep(i)
632	   format(' #',i2,
     & ': Voltage ramp; duration (ms) = ',f10.1,'; from',i5,' mV to',i5,
     &  ' mV',/,'   (step size = ',f10.3,' mV)')
	endif
	if(i.eq.nvjump) goto 621
      if(iprt.eq.2) print 641,agapv(i)
      if(prt) write(7,641) agapv(i)
      if(idp.eq.2) write(8,641) agapv(i)
641	format(
     & '    gap between this one and next (ms) = ',f10.1)
621	continue
c
	if(sampv) then
        if(iprt.eq.2) print 51
        if(prt) write(7,51)
        if(idp.eq.2) write(8,51)
51	  format(' Membrane potential sampled on ADC1')
	endif
	if(control) then
        if(iprt.eq.2) print 52
        if(prt) write(7,52)
        if(idp.eq.2) write(8,52)
52	  format(' CONTROL: V-jump only (no C-jump)')
	endif
c
90	continue
	if(iver1.gt.-1003) then
        if(iprt.eq.2) print 50,calfac,calvolt
        if(prt) write(7,50) calfac,calvolt
        if(idp.eq.2) write(8,50) calfac,calvolt
50	  format(' Current units per ADC unit; calfac = ',g13.8,/,
     & ' mV out from clamp per mV membrane pot = ',g13.6,/)
	else
        if(iprt.eq.2) print 53,amVpA1,ftape,gain,calfac,calvolt
        if(prt) write(7,53) amVpA1,ftape,gain,calfac,calvolt
        if(idp.eq.2) write(8,53) amVpA1,ftape,gain,calfac,calvolt
53	  format(
     & ' Calibration: mV/pA = ',f7.1,': tape factor, gain = ',2g13.6,/,
     & ' Current units per ADC unit; calfac = ',g13.8,/,
     & ' mV out from clamp per mV membrane pot = ',g13.6,/)
	endif
c
	RETURN      !from PPAR
	end


	subroutine PREC(i,title1,cdate,ctime,naver,navc,iav,vjump,
     & control,iver1,iprt)
c Call with iprt=1 for screen only; iprt=2 for screen + printout (if
c pon/discprt also set on); iprt=3 for printout only (no screen)
c Altered 03/12/91 09:34am so that direct print to printer is controlled
c ONLY by pon() (to allow list of disc contents to be printed easily)
	integer*2 iver1
      character cdate*11,ctime*11
	character title1*79
	character istr*11,jstr*6,text*20
	logical prt,vdu,pprt,dprt,vjump,control
	logical discprt,pon,slock
	common/dp/discprt
c
	pon()=slock()
c
	if(iver1.eq.-1001) then
	   jstr='CJUMP2'
	else if(iver1.eq.-1003) then
	   jstr='CJUMP3'
	else if(iver1.eq.-1004) then
	   jstr='CJUMP4'
	else
	   jstr='CJUMP '
	endif
	vdu=iprt.eq.1.or.iprt.eq.2
	prt=iprt.eq.2.or.iprt.eq.3
	pprt=pon()
	dprt=prt.and.discprt
102	format(/)
      if(pprt) write(7,102)
      if(dprt) write(8,102)
	n=naver
	if(control) n=navc
	if(iav.eq.0) then
	   text='Single sweep '
	else
	   call INTCONV(n,istr)
	   text='Mean of '//istr(1:4)//' sweeps'
	endif
	n=NBLANK(text)
	if(.not.vjump) then
	   if(vdu) print 231,i,cdate,ctime,text
         if(pprt) write(7,231) i,cdate,ctime,text
         if(dprt) write(8,231) i,cdate,ctime,text
231	   format(/,' Sweep',i4,
     &   ': C-jump;',a11,1x,a11,1x,a20)
	else if(vjump.and.control) then
	   if(vdu) print 232,i,cdate,ctime,text
         if(pprt) write(7,232) i,cdate,ctime,text
         if(dprt) write(8,232) i,cdate,ctime,text
232	   format(/,' Sweep',i4,
     &   ': Control V-jump;',a11,1x,a11,1x,a20)
	else if(vjump.and.(.not.control)) then
	   if(vdu) print 233,i,cdate,ctime,text
         if(pprt) write(7,233) i,cdate,ctime,text
         if(dprt) write(8,233) i,cdate,ctime,text
233	   format(/,' Sweep',i4,
     &   ': V-jump + agonist;',a11,1x,a11,1x,a20)
	endif
	n=nblank(title1)
	if(vdu) print *,jstr//': '//title1(1:n)
	if(pprt) write(7,*) jstr//': '//title1(1:n)
	if(dprt) write(8,*) jstr//': '//title1(1:n)
	RETURN	!from PREC
	end


	subroutine GETSTEP(nvjump,ivolt1,ivolt2,ilenv,iDd,
     & nvramp,vstep)
c To calculate size of the individual V-steps during a ramp;
c Output=nvramp, the number of ramps with V-step for each (in mV) in
c vstep(i) i=1,...,nvjump (value defined only for those 'jumps' that
c are actually ramps (other elements set to zero)
	integer*4 ilenv(10)	!lengths of V-jumps (integer*4 microsec)
	integer*2 ivolt1(10),ivolt2(10)  !pots for each V jump (integer mV)
	real*4 vstep(10)
	logical ramp
c
	nvramp=0
	do 9 i=1,nvjump
	 vstep(i)=0.0
       ramp=ivolt1(i).ne.ivolt2(i)
	 if(ramp) then
	   nvramp=nvramp+1
	   idelt=ilenv(i)/iDd   !# of elements of iDACv corresp to ilen
	   v1=float(int4(ivolt1(i)))
	   v2=float(int4(ivolt2(i)))
c	NB dv is positive if v2>v1. In this case, since v1,v2 are on opp
c	sides of Vhold, slope is neg for 1st n1 point, pos for next n2 (iv1 to
c	iv2) then neg for return to ivhold
	   vstep(i)=abs(2.0*(v2-v1)/float(idelt))	!in mV
	 endif
9	continue
	RETURN	!from GETSTEP
	end

	subroutine ILCONV(ilenc,igapc,ilenv,igapv,
     &	ilen2c,igap2c,ilen2v,igap2v,iDd,iconv)
c  If iconv=0 then converts ilenc etc to ilen2c etc
c  If iconv=1 then converts ilen2c etc to ilenc etc
c Scaling of ilenc etc for int*2 versions on disc
c  (1) If iDd=1000 ( 1kHz  DAC rate) then keep in msec (up to 32.7 sec)
c  (2) If iDd=10000 ( 100 Hz  DAC rate) then keep in 10ms units (up to 327 sec)
c  (3) If iDd=100 ( 10 kHz  DAC rate) then keep in 0.1ms units (up to 3.27 sec)
c i.e. keep ilenc,igapc etc in number of DAC tics
c length in microsec=iDd*ilenc(i)
	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	integer*2 ilen2c(10),igap2c(10)	!integer*2 versions for disc
	integer*2 ilen2v(10),igap2v(10)
	logical discprt,pon,slock,prt
	common/dp/discprt
c
	pon()=slock()
c
	prt=pon()
	if(iDd.ne.100.and.iDd.ne.1000.and.iDd.ne.10000) then
	   iDd=1000
	   print 1
	   if(prt) write(7,1)
	   if(discprt) write(8,1)
1	   format(' DAC rate assumed to be 1 kHz')	!for early files
	endif
c
	if(iconv.eq.0) then
	   do 10 i=1,10
	   ilen2c(i)=int2(ilenc(i)/iDd)     !integer*2 DAC ticks
	   igap2c(i)=int2(igapc(i)/iDd)
	   ilen2v(i)=int2(ilenv(i)/iDd)
	   igap2v(i)=int2(igapv(i)/iDd)
10	   continue
	else if(iconv.eq.1) then
	   do 20 i=1,10
	   ilenc(i)=iDd*int4(ilen2c(i))     !integer*4 microsec
	   igapc(i)=iDd*int4(igap2c(i))
	   ilenv(i)=iDd*int4(ilen2v(i))
	   igapv(i)=iDd*int4(igap2v(i))
20	   continue
	endif
	RETURN
	end

	subroutine CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,
     &	keepall,ndim)
c=	integer*2 ikeep1(4,2),kstep(5)
	integer*4 ikeep1(4,2)
	integer*2 kstep(5)
	integer*1 jmask(ndim)
	logical keepall
c CALCNS2 is same as CALCNS1 but input is ikeep1 (ie values for a single
c sweep, so parameter m not needed)
c
c Much simplified version of MODKEEP that returns only (1) keepall, (2) nsamp1
c =number of points kept on disc and (2) JMASK() -array to speed location of
c points to be kept: jmask=1 to keep, =0 to omit (for multiple sweeps jmask
c may need to be reset each cycle (e.g. for double pulse expts) -can do by
c calling this subroutine
	keepall=nkeep.eq.1.and.ikeep1(1,1).eq.1.and.
     &  ikeep1(1,2).eq.nsamp
	if(keepall) then
	   do 1 i=1,nsamp
1	   jmask(i)=1
	   nsamp1=nsamp
	   RETURN
	endif
	nsamp1=0	!# of points actually kept
	do 2 i=1,nsamp
2	jmask(i)=0
c
	do 28 i=1,nkeep
c	  nsamp1=nsamp1+ikeep1(i,2)-ikeep1(i,1)+1
	   n1=ikeep1(i,1)		      !1st point of kept bit #i
	   n2=ikeep1(i,2)		      !last point of kept bit #i
	   do 11 j=n1,n2
		nsamp1=nsamp1+1
		jmask(j)=1			!keep all points in 'kept section'
11	   continue
c Now the bits outside the 'kept sections'
	  if(i.eq.1.and.ikeep1(1,1).gt.1) then
c	add these extra points to nsamp1
	    n2=ikeep1(1,1)-1
	    n=int4(kstep(1))
	    do 5 k=1,n2,n
	    nsamp1=nsamp1+1
	    jmask(k)=1
5	    continue
	  endif
	  if(i.gt.1) then
	   	n1=ikeep1(i-1,2)+kstep(i)
	   	n2=ikeep1(i,1)-1    !1st point of next kept bit -1
	      n=int4(kstep(i))
	      do 6 k=n1,n2,n
	      nsamp1=nsamp1+1
	      jmask(k)=1
6		continue
	  endif
28	continue	!end of i=1,nkeep loop
c
c points kept after the last bit
	  if(ikeep1(nkeep,2).lt.nsamp) then
c	add these extra points to nsamp1
	    n1=ikeep1(nkeep,2)+int4(kstep(nkeep+1))
	    n=int4(kstep(nkeep+1))
	    do 7 k=n1,nsamp,n
	    nsamp1=nsamp1+1
	    jmask(k)=1
7	    continue
	  endif
c
	RETURN
	end



