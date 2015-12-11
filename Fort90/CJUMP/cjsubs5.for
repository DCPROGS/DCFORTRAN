c=============================================================
	subroutine DEFLEN(itsamp,ibox,icqd)
	integer*2 videotyp
	character def*30,cans*30
	logical   alpha
	common/mousval/mouse_on,nbutton
c Note ITSAMP now kept in microsec, but ask for msec for convenience
c If better resolution needed later, could as for microsec (or
c floating microsec)
	alpha=videotyp().eq.3
	if(.not.alpha) then
	   i1=itsamp/1000
	   call defolti(i1,def)
         call qdialog(ibox,'Total duration of ADC sample (integer ms) ',
     &   def,icqd,cans)
	   call getinpi(cans,i1)
	   goto 8001
	endif
	print 51,itsamp/1000
51	format(
     & '&Total duration of ADC sample (integer ms) [',i6,'] = ')
	read 4,i1
4	format(i8)
8001	if(i1.ne.0) itsamp=i1*1000
	RETURN
	end
c==============================================

	subroutine DEFDRATE(iDd,ipre1,icount1,noquery,nerr)
c To choose DAC output rate, and set ipre1,icount1 accordingly. If the
c value of iDd is different from its input value, nerr=1, otherwise 0
	integer*2 videotyp
	integer*4 ipre1,icount1
	logical noquery
	logical mouse_on
	logical alpha
      character 	 charout,title*15
	character*72 que(20),helps(5)
	common/mousval/mouse_on,nbutton
c
	alpha=videotyp().eq.3
	mouse_on=.true.
	title='DAC resolution'
	ixlow=8
	iylow=-1
	iyhiw=400
	ic=15
	icf=8
	ibkw=4
	icup=15
	nerr=0
	id1=iDd
	if(iDd.eq.1000.or.iDd.eq.0) i=1
	if(iDd.eq.10000) i=2
	if(iDd.eq.100) i=3
	if(iDd.eq.10) i=4       !for

	if(noquery) goto 75	!define ipre1,icount1 corresp to iDd
	if(.not.alpha) then
         que(1)='(1)    1 ms (output rate   1 kHz ; output length up to
     &5 s)'
         que(2)='(2)   10 ms (output rate 0.1 kHz ; output length up to
     &50 s)'
         que(3)='(3)  0.1 ms (output rate  10 kHz ; output length up to
     &0.5 s)'
         que(4)='(4) 0.01 ms (output rate 100 kHz ; output length up to
     &0.05 s)'
	   nq=4
c	   i1=1
	   i1=i
	   nhelp=1
	   call POPMENU(ixlow,iylow,iyhiw,que,nq,ic,icf,icup,ibkw,
     &   title,helps,nhelp,i1,charout,ival)
	   goto 8002
	endif
	print 74,i
74	format(
     & ' (1) DAC resolution 1ms (output length up 5 seconds)',/,
     & ' (2) DAC resolution 10ms (output length up 50 seconds)',/,
     & ' (3) DAC resolution 0.1ms (output length up 0.5 seconds)',/,
     & ' (4) DAC resolution 0.01ms (output length up 0.05 seconds)',/,
     & ' Option number [',i2,'] = ')
	read 4,i1
4	format(i8)
8002	if(i1.ge.1.and.i1.le.4) i=i1
75	continue
	if(i.eq.1) then
	   ipre1=100
	   icount1=40		!4 MHz/4000 = 1 kHz
	   idD=1000		!1000mus=1 ms between DAC outputs
	else if(i.eq.2) then
	   ipre1=100
	   icount1=400		!4 MHz/40000 = 100 Hz
	   idD=10000		!10000mus=10 ms between DAC outputs
	else if(i.eq.3) then
	   ipre1=100
	   icount1=4		!4 MHz/400 = 10000 Hz
	   idD=100			!100mus=0.1 ms between DAC outputs
c
	else if(i.eq.4) then
	   ipre1=10
	   icount1=4		!4 MHz/40 = 100000 Hz
	   idD=10			!10mus=0.01 ms between DAC outputs
c
	endif
	if(iDd.ne.id1) nerr=1
	RETURN
	end

c===========================================

	subroutine DEFCONC(ncjump,iTPREc,ilenc,igapc,iDACc,idD,nDc,
     & jclast,noquery,reverse,idac2,nerr,idim,ibox,icwd,icqd,iflag)
	integer*2 iDACc(idim),iOFF,iON,videotyp
	integer*4 ilenc(10),igapc(10)	!lengths of pulses and gaps between them
      character*1 ans,UC
      character 	 getint*11,getint1*11,getreal*13,cans*30,def*30
	logical 	 mouse_on,alpha
	logical noquery,reverse
      character 	 charout,title*15
	character*72 que(20),helps(5)
	common/mousval/mouse_on,nbutton
c
c Modified 10/26/93 02:55pm so that iDACc(j) is defined with a ramp rise
c and fall (for now).  This is to prevent damage of the piezo material of the
c P-244.40 which could occur if driven with a step input.
c
c To set times for 0-5-0 volt pulses in array iDACc() for output to DAC via
c MEMDAC; sets specified points to 32752 = +4.9976V
c
c Modified 03/18/91 02:43pm so that DAC rate, iDd=microsec/point, can
c	be changed if necessary. Returns integer*4 NERR=0 if all OK; NERR=1 if
c	OK but iDd has been changed so need to iDACv for v-jumps too;
c	and NERR=2 if satisfactory DAC rate not found so output invalid.
c Modified 02/08/91 09:08am so piezo voltage set in separate subroutine
c (so ivdac2 no longer na argument)
c Modified 02/06/91 12:16pm so piezo voltage set in separate subroutine (in
c CJSUBS.FOR) - must be defined before DEFCONC called.
c Modified 10/30/90 09:52am (1) so voltage can be less than 5V for smaller
c excursion of piezo and (2) so can set ncjump=0 if V-jump only required (in
c this case none of the output values are defined)
c Uses 1ms ticks at present (iDd=1000mus=1ms on input) with up to 1000
c outputs(assumes the 4MHz clock='H'), ie after 1 sec there is no further
c output to DAC and setting stays at the last one requested
c iTPREc=duration of sample that precedes start of the (first) DAC pulse
c (ie between trigger and moment that DAC (first) goes high)
c (NB MEMDAC raises DAC, at earliest (if iDACc(1) is high), one DAC clock
c interval after the trigger, so iTPREc must be at least 1ms at present,
c with DAC rate set to 1kHz)
c	NB: MEMDAC updates all the specified DACS at each clock tick so,
c unlike ADCMEM, there is no need to change clock rate if number of channels
c is altered!
c	NB Both ADCMEM and MEMDAC when triggered should sample ADC/set DAC at the
c moment of the trigger- NO- experimentally seems that ADCMEM does this, but
c MEMDAC puts out first value one tick AFTER the trigger (eg if IDAC(1) is high
c and rest zero, and rate is 1kHz the DAC sets from 1-2 ms (not from 0-1ms) but
c ADC starts sample at 0 (=trigger time).  When DAC rate=2kHz DAC output
c is set from 0.5-1.0ms (1 tick=0.5ms)
c Thus iADCc(nAc) is at time = (nAc-1)*dA from moment of trigger (t=0)
c  and DAC goes high at time nDc*dD where iDACc(nDc) is 1st element set high
c where dA=time between ADC samples=1/ADC freq=integer # of 0.25 mus ticks
c and dD=time between DAC outputs=1/DAC freq=integer # of 0.25 mus ticks=1ms now
c To get an ADC sample, viz that in ADC(nAc), coincident with the moment
c the (first) DAC (c-jump) pulse starts then must have
c		ITPREc = (nAc-1)*dA = nDc*dD = time from trigger to start of pulse
c ie
c		nAc = 1 + nDc*dD/dA  must be exactly an integer 		(1)
c
c When V-jumps done, we also require (preferably) that the moment of each
c V-jump is also exactly coincident with one of the ADC sample points, and
c sample rates, dD,dA are same, so similarly, if iTPREv=time to start of 1st
c V-jump
c
c		iTPREv = (nAv-1)*dA = nDv*dD
c
c where iADC(nAv) is coincindent with the moment of the (1st) V-jump when
c iDACv(nDv) is the first element to depart from resting pot.
c 	However for V-RAMPS it does not really matter whether an ADC sample
c is coincident with start of ramp, as long as both I and V are sampled
c sufficiently often during the ramp to cover the range
c	In fact coincidence is not so important for c-jump or v-jump either
c but if points not coincident then must keep not only nAc (nAv) but also
c the time difference from iADC(nAc) to moment that jump starts, so that
c relaxation can be plotted with the correct t=0.
c When ADC sample NOT coincident can calc nAc (nAv) as in eq(1) above
c except that calc done in floating point. If nAc=11.75 say this means
c that moment of jump corresponds to 0.75*dA msec after iADC(11), ie the
c jump starts at (11.75-1)*dA msec from the moment of the trigger =nD*dD.
c
c NB Size (in bytes), isz and iszout, MUST be multiple of 4 (for 2-byte
c data ie 12 bit accuracy)
c
c
	nerr=0		!no errors
	alpha=videotyp().eq.3
c
	mouse_on=.true.
	title='DAC resolution'
	ixlow=8
	iylow=-1
	iyhiw=400
	ic=15
	icf=8
	ibkw=4
	icup=15
c
101   format(a1)
	if(noquery.and.ncjump.eq.0) then
	   jclast=0		!signal to DEFDAC
	   RETURN
	endif
	if(noquery) goto 113
	if(.not.alpha) then
	   ans='Y'
	   call defolta(ans,def)
	   call qdialog(ibox,' Do concentration jump (output on DAC2)',
     &   def,icqd,cans)
	   call getinpa(cans,ans)
	   goto 8003
	endif
	print 5
5	format(' Do concentration jump (output on DAC2) [Y] ? ')
      read 101,ans
8003  if(UC(ans).eq.'N') then
	   ncjump=0
	   jclast=0		!signal to DEFDAC
	   RETURN
	endif
c
	if(iflag.eq.0) goto 113
112	continue
	id1=idd
	if(iDd.eq.1000.or.iDd.eq.0) i=1
	if(iDd.eq.10000) i=2
	if(iDd.eq.100) i=3
	if(iDd.eq.10) i=4       !for
	if(.not.alpha) then
         que(1)='(1)    1 ms (output rate   1 kHz ; output length up to
     &5 s)'
         que(2)='(2)   10 ms (output rate 0.1 kHz ; output length up to
     &50 s)'
         que(3)='(3)  0.1 ms (output rate  10 kHz ; output length up to
     &0.5 s)'
         que(4)='(4) 0.01 ms (output rate 100 kHz ; output length up to
     &0.05 s)'
	   nq=4
c	   i1=1
	   i1=i
	   nhelp=1
	   call POPMENU(ixlow,iylow,iyhiw,que,nq,ic,icf,icup,ibkw,
     &   title,helps,nhelp,i1,charout,ival)
	   goto 78
	endif
	fac=float(iDd)/1000.
	print 71,fac
71	format('&Resolution for durations = ',f5.1,' ms',/)
78	continue
	if(i1.ge.1.and.i1.le.4) i=i1
	if(i.eq.1) then
	   ipre1=100
	   icount1=40		!4 MHz/4000 = 1 kHz
	   idD=1000		!1000mus=1 ms between DAC outputs
	else if(i.eq.2) then
	   ipre1=100
	   icount1=400		!4 MHz/40000 = 100 Hz
	   idD=10000		!10000mus=10 ms between DAC outputs
	else if(i.eq.3) then
	   ipre1=100
	   icount1=4		!4 MHz/400 = 10000 Hz
	   idD=100			!100mus=0.1 ms between DAC outputs
c
	else if(i.eq.4) then
	   ipre1=10
	   icount1=4		!4 MHz/40 = 100000 Hz
	   idD=10			!10mus=0.01 ms between DAC outputs
c
	endif

	if(iDd.ne.id1) nerr=1
113	continue
c
	iOFF=0
	iON=idac2
	if(reverse) then
	  iOFF=idac2
	  iON=0
	endif
c
	do 10 i=1,idim
10	iDACc(i)=iOFF
c
	if(noquery) then
	   nDc=iTPREc/idD
	   goto 60		!set iDACc
	endif
51	continue
	if(.not.alpha) then
	   itmin=idd/1000.
	   if(itmin<1) itmin=1
	   i1=iTPREc/1000
	   call defolti(i1,def)
	   call intconv(itmin,getint)
	   nm=nblank1(getint)
         call qdialog(ibox,
     &   'Duration of sample before 1st C-jump pulse (min '//
     &   getint(1:nm)//' ms) ',
     &   def,icqd,cans)
	   call getINPi(cans,i1)
	   goto 8004
	endif
	print 50,iTPREc/1000
50	format(
     & '&Duration of sample before 1st C-jump pulse (ms) [',i6,'] = ')
	call INPUTi(i1)
8004	if(i1.ne.0) iTPREc=i1*1000
c Need (see start of main prog)
c		iTPREc = (nAc-1)*dA = nDc*dD = time from trigger to start of pulse
c where dD=1ms for DAC freq= 1 kHz, so nDc=index of first point in iDACc() to be
c set high, is iTPREc/dD;
60	continue		!jump here if noquery
	nerr=ICHECK(iTPREc,iDd,ibox,icwd,icqd)		!both in microsec
	if(nerr.le.1) then
	   nDc=iTPREc/idD
	else
	   goto 51
	endif
	if(nDc.lt.1) then
	   call BELL(2)
	   if(.not.alpha) then
	      call wdialog(ibox,' Time to 1st pulse too short',icwd)
	   	goto 51
	   endif
	   print 81
81	   format(' Time to 1st pulse too short',/)
	   goto 51
	endif
c
	if(noquery) goto 61
	if(.not.alpha) then
	   i=ncjump
	   call defolti(i,def)
 	   call qdialog(ibox,'Number of C-jump pulses ',def,icqd,cans)
	   call getINPi(cans,i)
	   goto 8005
	endif
	print 1,ncjump
1	format('&Number of C-jump pulses [',i3,'] = ')
	read 2,i
2	format(i8)
8005	if(i.ne.0) ncjump=i
c
c Ask for msec for convenience, and because ilen() etc are only int*2 at
c present
c If better resolution needed later, could as for microsec (or
c floating microsec)
c(ICHECK would be OK, for example, with ilen=0.2ms, iDd=100mus)
c 03/16/91 10:06pm Need better resolution now, so now use ILENC etc as
c integer*4 values in microseconds throughout prog (but convert to integer*2
c for disc read/write for compatibility with earlies versions -int*2
c need scaling to keep in range)
c Scaling of ilenc etc for int*2 versions on disc
c  (1) If iDd=1000 ( 1kHz  DAC rate) then keep in msec (up to 32.7 sec)
c  (2) If iDd=10000 ( 100 Hz  DAC rate) then keep in 10ms units (up to 327 sec)
c  (3) If iDd=100 ( 10 kHz  DAC rate) then keep in 0.1ms units (up to 3.27 sec)
c  (4) if iDd=10 (100 kHz DAC rate) then keep in 0.01ms units (up to 327mus)
c i.e. keep ilenc,igapc etc in number of DAC tics
c i.e. units of ilen,igap, in ms, =float(iDd)/1000.
c and length in microsec=iDd*int4(ilenc(i))
c

61	continue		!jump here if noquery
	maxi=5*idd-i1
	do 7 i=1,ncjump
	   if(noquery) goto 62
31	   continue
	   call intconv(i,getint)
	   nm=nblank1(getint)
	   call intconv(maxi,getint1)
	   nm1=nblank1(getint1)
	   if(.not.alpha) then
	      r=1.e-3*float(ilenc(i))
	      call defoltr(r,def)
	      call qdialog(ibox,'Pulse '//getint(1:nm)//
     &      ' : duration of pulse (max '//getint1(1:nm1)//
     &	' ms) ',def,icqd,cans)
	      call getinpr(cans,r)
	      goto 8006
	   endif
	   print 3,i,1.e-3*float(ilenc(i))		!msec
3	   format(
     &   '&Pulse ',i2,':',/,
     &   '  duration of pulse (ms) [',f10.1,'] = ')
	   call INPUTR(r)		!no error if integer supplied!
8006	   if(r.ne.0.) ilenc(i)=int(r*1000.)       !microsec
62	   continue
	   nerr=ICHECK(ilenc(i),iDd,ibox,icwd,icqd)
	   if(nerr.eq.1) goto 112		!start again with new iDd
	   if(nerr.eq.2) goto 31
	   if(i.eq.ncjump) goto 7
	   if(noquery) goto 63
41	   continue
	   if(.not.alpha) then
	      r=1.e-3*float(igapc(i))
	      call defoltr(r,def)
            call qdialog(ibox,'& gap between this pulse and next (ms) ',
     &      def,icqd,cans)
	      call getINPr(cans,r)
	      goto 8007
	   endif
	   print 4,1.e-3*float(igapc(i))
4	   format(
     &   '& gap between this pulse and next (ms) [',f10.1,'] = ')
	   call INPUTR(r)		!no error if integer supplied!
8007	   if(r.ne.0.) igapc(i)=int(r*1000.)
63	   continue
	   nerr=ICHECK(igapc(i),iDd,ibox,icwd,icqd)
	   if(nerr.eq.1) goto 112		!start again with new iDd
	   if(nerr.eq.2) goto 41
	   maxi=maxi-ilenc(i)/1000-igapc(i)/1000
7	continue
c Now set iDACc(nDc) to iDACc(nDc+ilen(1)-1) for 1st pulse etc
	j1=nDc
	do i=1,ncjump
	   idelt=ilenc(i)/iDd   !# of elements of iDACc corresp to ilen
	   j2=j1+idelt-1
	   if((j2+1.gt.idim)) then
	      call BELL(2)
	      call intconv(j2,getint)
	      nm=nblank1(getint)
	      call intconv(idim,getint1)
	      nm1=nblank1(getint1)
		if(.not.alpha) then
	         call wdialog(ibox,' DAC2 array too long: '//
     &         getint(1:nm)//' elements, max = '//getint1
     &         (1:nm),icwd)
	         noquery=.false.
	   	   goto 112
		endif
	      print 611,j2,idim
611	      format(' DAC2 array too long: ',i5,' elements, max = ',i5,/)
	      noquery=.false.
	      goto 112
	   else
	  	do j=j1,j2
	         iDACc(j)=iON
	      enddo
	   endif
         idelt=igapc(i)/iDd   !# of elements of iDACc corresp to igap
	   j1=j2+idelt+1
	enddo
c
c After this loop, iDACc(j2) is the last element to be set high. After this
c need only one element set low, and DAC output will stay low thereafter
c so iDACc(j2+1) is last element needed
	jclast=j2+1
	RETURN	!end of DEFCONC
	end


c=====================================================================

      subroutine DEFVOLT(ivhold,comfac,vjump,nvjump,nvramp,ilenv,igapv,
     &ivolt1,ivolt2,iDACv,jvlast,iTPREv,iDd,nDv,noquery,nerr,idim,sampv,
     &ibox,icwd,icqd,ismode)
c To define resting pot, and, if required, V-jumps and ramps
c Note that nvjump=total number including number of ramps (nvramp)
c Modified 03/18/91 02:43pm so that DAC rate, iDd=microsec/point, can
c	be changed if necessary. Returns integer*4 NERR=0 if all OK; NERR=1 if
c	OK but iDd has been changed so need to iDACv for v-jumps too;
c	and NERR=2 if satisfactory DAC rate not found so output invalid.
c SAMPV=true if ANY V-ramps are to be done, to signal that voltage is to
c be recorded on ADC1. For notes on timing, see DEFCONC
c	ivolt=int2(ifixr(v*comfac*32768./5000.))	!V in mV; ivolt is in ADC units
c conversely
c	V=float(ivolt*5000/32768)/comfac
c	if(ivolt.gt.32752) ivolt=32752
c	if(ivolt.lt.-32768) ivolt=-32768

	integer*2 iDACv(idim),videotyp
	integer*2 ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
c	integer*2 iv   				!hold potential (ADC units)
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
      character*1 ans,UC
	character def*30,cans*30,getint*11,getint1*11,getreal*13
	logical vjump,ramp,noquery,sampv
	logical debug,caplock
	logical discprt,alpha
	common/dp/discprt
	common/mousval/mouse_on,nbutton
c
	debug()=caplock()
	alpha=videotyp().eq.3
c
101   format(a1)
4	format(i8)
	if(noquery) then
	   vhold=float(ivhold)
	   call SETPOT(vhold,comfac,iv)
	endif
c
112	continue
	if(vjump.and..not.noquery) then
	   if(.not.alpha.and.ismode.ne.2) then
	   	ans='Y'
	   	call defolta(ans,def)
	      call qdialog(ibox,'Do voltage jumps/ramps',def,icqd,
     &	cans)
		call getinpa(cans,ans)
	   else if(ismode.ne.2) then
	      print 5
5	      format(' Do voltage jumps/ramps [Y] ? ')
            read 101,ans
	   endif
         if(UC(ans).eq.'N') vjump=.false.
	else if(.not.vjump.and..not.noquery) then
	   if(.not.alpha) then
	   	ans='N'
	   	call defolta(ans,def)
	   	call qdialog(ibox,'Do voltage jumps/ramps',def,icqd,
     &	cans)
		call getinpa(cans,ans)
	   else
	      print 6
6	      format('&Do voltage jumps/ramps [N] ? ')
         	read 101,ans
	   endif
         if(UC(ans).eq.'Y') then
		vjump=.true.
		noquery=.false.
	   endif
	endif
	vhold=float(ivhold)
	v1=comfac*vhold
	iv=int2(ifixr(v1*32768./5000.))
	iv=16*int2(ifixr(float(iv)/16.0))
	if(iv.gt.32752) iv=32752
	if(iv.lt.-32768) iv=-32768
c
60	continue
	sampv=.false.
	if(.not.vjump) RETURN
c
c When vjump=true, get the jump pattern now
	do 10 i=1,idim
10	iDACv(i)=iv		!initialise whole array to holding pot
c
c Note iTPREv now kept in microsec, but ask for msec for convenience
c If better resolution needed later, could as for microsec (or
c floating microsec)
c Sample rates, dD,dA are same, as for c-jumps
c If iTPREv=time to start of 1st Vjump
c		iTPREv = (nAv-1)*dA = nDv*dD
c =time from trigger to start of 1st jump
c where iADC(nAv) is coincindent with the moment of the (1st) V-jump when
c iDACv(nDv) is the first element to depart from resting pot.
c dD=1000mus for DAC freq=1 kHz (input),so nDv=index of first point in iDACv()
c to be set high, is ITPRE/dD; use idD [eg= 1000mus (= 1ms)] defined above
	if(noquery) then
	   nDv=iTPREv/idD
	   goto 61		!set iDACv
	endif
c
51	continue
	if(.not.alpha) then
	   ri1=(float(iTPREv)/1000.)
	   i1=int(ri1)
	   call defolti(i1,def)
         call qdialog(ibox,
     &   'Duration of sample before 1st V-jump/ramp(ms) '
     &   ,def,icqd,cans)
	   call getINPi(cans,i1)
	   goto 8009
	endif
	print 50,iTPREv/1000
50	format(
     & '&Duration of sample before 1st V-jump/ramp (ms) [',i6,'] = ')
	call INPUTi(i1)
8009	if(i1.ne.0) iTPREv=i1*1000
61	continue
	nerr=ICHECK(iTPREv,iDd,ibox,icwd,icqd)		!both in microsec
	if(nerr.le.1) then
	   nDv=iTPREv/idD
	else
	   goto 51
	endif
	if(nDv.lt.1) then
	   call BELL(2)
	   if(.not.alpha) then
	   	call wdialog(ibox,' Time to 1st jump too short',icwd)
	   	goto 51
	   else
	   	print 81
81	   	format(' Time to 1st jump too short',/)
	   	goto 51
	   endif
	endif
c
111	continue
	if(noquery) goto 62
	   if(.not.alpha) then
		i=nvjump
		call defolti(i,def)
		call qdialog(ibox,'Number of V-jumps/ramps per sweep',def,
     &      icqd,cans)
		call getINPi(cans,i)
		goto 8010
	endif
	print 11,nvjump
11	format('&Number of V-jumps/ramps [',i3,'] = ')
	read 4,i
8010	if(i.ne.0) nvjump=i
c
62	continue
	nvramp=0
	do 17 i=1,nvjump
	   call intconv(i,getint)
	   nm=nblank1(getint)
	   ramp=ivolt1(i).ne.ivolt2(i)
	   if(noquery) goto 63
	   j=1
	   if(ramp) j=2
	   	if(.not.alpha) then
	   	call defolti(j,def)
         	call qdialog(ibox, 'Pulse # '//getint(1:nm)//' : '//
     &   	' (1) Jump / (2) Ramp ',def,icqd,cans)
	   	call getinpi(cans,j)
	   	goto 8011
	   endif
	   print 131,i,j
131	   format(
     &   '&Pulse #',i2,':',/,
     &   ' (1) Jump',/,
     &   ' (2) Ramp',/,
     &   '  Option number [',i2,'] = ')
	   read 4,j
8011	   if(j.ne.0) ramp=j.eq.2
c
c        03/17/91 11:26am: ilen,igap now int*4 microsec (see DEFCONC)
c        Ask for msec for convenience, and because ilen() etc are only int*2 at
c        present
c        If better resolution needed later, could as for microsec (or
c        floating microsec)
c        (ICHECK would be OK, for example, with ilen=0.2ms, iDd=100mus)
32	   continue
	   if(.not.alpha) then
	   	fac=float(iDd)/1000.
		call realtoch(fac,getreal,13)
		nm=nblank1(getreal)
	   	call wdialog(ibox,'Resolution for durations = '//
     &   	getreal(1:nm)//' ms',icwd)
	   	r=1.e-3*float(ilenv(i))
	   	call defoltr(r,def)
         	call qdialog(ibox,'&  duration of jump/ramp (ms) ',def,icqd,
     &   	cans)
	   	call getINPR(cans,r)		!no error if integer supplied!
	   	goto 8013
	   endif
	   fac=float(iDd)/1000.
	   print 71,fac
71	   format('&Resolution for durations = ',f5.1,' ms',/)
	   print 13,1.e-3*float(ilenv(i))
13	   format(
     &   '&  duration of jump/ramp (ms) [',f10.1,'] = ')
	   call INPUTR(r)		!no error if integer supplied!
8013	   if(r.ne.0.) ilenv(i)=int(r*1000.)       !microsec
63	   continue
	   nerr=ICHECK(ilenv(i),iDd,ibox,icwd,icqd)
	   if(nerr.eq.1) goto 112		!start again with new iDd
	   if(nerr.eq.2) goto 32
	   if(noquery) goto 64
c
c 	   No easy way to specify default for pots, as may WANT pot=0
3121	   continue
	   if(.not.ramp) then
315      	continue
	   	if(.not.alpha.and.ismode.ne.2) then
		   npiz=ivolt1(i)
	   	   call defolti(npiz,def)
         	   call qdialog(ibox,'& potential during jump (integer mV) '
     &   	   ,def,icqd,cans)
	   	   call getinpi(cans,npiz)
		   ivolt1(i)=npiz
	   	   ivolt2(i)=ivolt1(i)		!to signal that this is jump, not ramp
	   	   if(ivolt1(i).eq.0) then
			ans='Y'
		      call defolta(ans,def)
		      call qdialog(ibox,'& potential during jump=0 mV: O.K.'
     &	      ,def,icqd,cans)
			call getinpa(cans,ans)
	            if(UC(ans).eq.'N') goto 315
	   	   endif
	      else  if (ismode.ne.2) then
	         print 31
31	   	   format(
     & 	   '&  potential during jump (integer mV) = ')
	   	   read 4,ivolt1(i)
	   	   ivolt2(i)=ivolt1(i)		!to signal that this is jump, not ramp
	   	   if(ivolt1(i).eq.0) then
		      print 314
314		      format('& potential during jump = 0 mV: O.K. [Y] ? ')
		      read 101,ans
	            if(UC(ans).eq.'N') goto 315
	         endif
	      endif
	   else
312	      continue
	      if(.not.alpha) then
	   	   call defolt2i(ivolt1(i),ivolt2(i),def)
         	   call qdialog(ibox,
     &   	   '&  ramp between potentials v1,v2 (integer mV): v1,v2 ',
     &   	   def,icqd,cans)
	   	   call getinp2i(cans,ivolt1(i),ivolt2(i))
	   	   if(ivolt1(i).eq.ivolt2(i)) goto 312
	   	   if(ivolt1(i).eq.ivhold.or.ivolt2(i).eq.ivhold) then
		      call BELL(1)
	     	      call wdialog(ibox,
     &		'One of these is same as holding pot',
     &            icwd)
		      goto 312
	         endif
	   	else
	         print 311
311	   	   format(
     & 	   '&  ramp between potentials v1,v2 (integer mV): v1,v2= ')
	   	   read 41,ivolt1(i),ivolt2(i)
41	   	   format(2i8)
	   	   if(ivolt1(i).eq.ivolt2(i)) goto 312
	   	   if(ivolt1(i).eq.ivhold.or.ivolt2(i).eq.ivhold) then
		      call BELL(1)
		      print 411
411		      format('&One of these is same as holding pot')
		      goto 312
	         endif
	      endif
         endif
c
64	   continue
	   if(i.eq.nvjump.or.noquery) goto 17
42	   continue
	   if(.not.alpha) then
	   	r=1.e-3*float(igapv(i))
	   	call defoltr(r,def)
         	call qdialog(ibox,
     &	'& gap between this pulse and next (reaL ms)',def,icqd,cans)
	   	call getINPR(cans,r)		!no error if integer supplied!
	   	goto 8014
	   endif
	   print 14,1.e-3*float(igapv(i))
14	   format(
     &   '&  gap between this pulse and next (reaL ms) [',f10.1,'] = ')
	   call INPUTR(r)		!no error if integer supplied!
8014	   if(r.ne.0.) igapv(i)=int(r*1000.)       !microsec
65	   continue
	   nerr=ICHECK(igapv(i),iDd,ibox,icwd,icqd)
	   if(nerr.eq.1) goto 112		!start again with new iDd
	   if(nerr.eq.2) goto 42
17	continue
c
c Now set iDACv(nDv) to iDACv(nDv+ilen(1)-1) for 1st pulse etc
611	continue
	j1=nDv		!1st point away from rest pot
	nvramp=0
	do 9 i=1,nvjump
	   ramp=ivolt1(i).ne.ivolt2(i)
	   if(ramp) nvramp=nvramp+1
	   idelt=ilenv(i)/iDd   !# of elements of iDACv corresp to ilen
	   j2=j1+idelt-1		  !last point away from rest pot
	   if(j2+1.gt.idim) then
	      call BELL(2)
	      call intconv(j2,getint)
	      nm=nblank1(getint)
	      call intconv(idim,getint1)
	      nm1=nblank1(getint1)
		if(.not.alpha) then
	         call wdialog(ibox,' DAC3 array too long: '//
     &	   getint(1:nm)//' elements, max = '//getint1
     &	   (1:nm1),icwd)
		   goto 8015
		endif
	      print 631,j2,idim
631	      format(' DAC3 array too long: ',i5,' elements, max = ',i5,/)
8015	      noquery=.false.
	      goto 112
	   endif
	IF(.NOT.RAMP) THEN		!jump
	   av=float(ivolt1(i))*comfac*32768./5000.
	   iv=int2(ifixr(aV))   !in ADC units
c	fix IV to nearest integer mult of 16
	   iv=16*int2(ifixr(float(iv)/16.0))
	   if(iv.gt.32752) iv=32752
	   if(iv.lt.-32768) iv=-32768
	   do 16 j=j1,j2
16	   iDACv(j)=iv
	ELSE IF(RAMP) THEN 	!ramp from ivolt1(i) to ivolt2(i), jump back
	   sampv=.true.		!if 1 or more ramps done

c	Better go BOTH ways, ie slope from ivhold to ivolt1, reverse slope to
c	 ivolt2, then reverse slope again until back to ivhold, for case where
c	iv1 and iv2 are on opp sides of ivhold (normal case).(If iv1 and iv2
c	are on same side of ivhold, ignore the one that is closer to ivhold
c	and slope to the other, then reverse slope back to ivhold)
c 		Points #j1 to j2 are all away from holding pot=j2-j1+1 points.
c	 Say 1st section contains n1 points, starting with #j1 and including
c	the point at iv1.
c	2nd section contains n2 points, starting at the point after that at
c	iv1 and including the point at iv2.
c	3rd section starts at the point following that at iV2 and INCLUDES
c	the first point at ivhold.
c	Thus n1+n2+n3=N=j2-j1+2
c	Want n1 and n3 in proportion to dv1=(iv1-ivhold)/(iv1-iv2), and
c	dv2=(iv2-ivhold)/(iv1-iv2), so dv1+dv2=1, and n1+n3=n2. Thus
c	dv1*n2 + n2 + dv2*n2 = N
c	 So n2=N/2 (half the points for the long
c	bit from iv1 to iv2).  Can achieve this exactly only if N is even, and
c	n1=(dv1/dv)*n2, and n3=(dv2/dv)*n2, are integers. Otherwise voltage
c	at peaks will not be exactly iv1,iv2. Since actual voltage is being
c	recorded this does not matter much, so round to nearest integers
	   iv1=int4(ivolt1(i))
	   iv2=int4(ivolt2(i))
	   v1=float(iv1)
	   v2=float(iv2)
	   vh=float(ivhold)
c	NB dv is positive if v2>v1. In this case, since v1,v2 are on opp
c	sides of Vhold, slope is neg for 1st n1 point, pos for next n2 (iv1 to
c	iv2) then neg for return to ivhold
	   dV=2.0*(v2-v1)/float(j2-j1+1)	!in mV
	   if(noquery) goto 665
	   if(.not.alpha) then
		 ans='Y'
		 call defolta(ans,def)
		call realtoch(dv,getreal,13)
		nm=nblank1(getreal)
		 call qdialog(ibox,'Potential will change in steps of '//
     &	 getreal(1:nm)//' mV ',def,icqd,cans)
		 call getinpa(cans,ans)
		 goto 8016
	   endif
	   print 313,dV
313	   format('&Potential will change in steps of ',f7.2,
     &	' mV: O.K. [Y] ')
	   read 101,ans
8016	   if(UC(ans).eq.'N') goto 111

665	   N=j2-j1+2
	   n2=N/2
	   if(2*n2.lt.N) n2=n2+1	!round up if N is odd
	   en2=float(n2)
	   dv1=abs((v1-vh)/(v1-v2))
	   n1=ifixr(dv1*en2)
	   k=0
c	NB do not alter j1,j2!!
	   v=vh	!start at Vhold
	   do 161 j=j1,j2
		k=k+1
		sign=1.0
		if(k.le.n1.or.k.ge.n1+n2) sign=-1.0
		v=v + sign*dV
	      av=v*comfac*32768./5000.
		iv=int2(ifixr(aV))   !in ADC units
		iv=16*int2(ifixr(float(iv)/16.0))	!nearest mult of 16
		if(iv.gt.32752) iv=32752
		if(iv.lt.-32768) iv=-32768
	      iDACv(j)=iv
161	   continue
c	===Insert bit here for case where iv1,iv2 are both on same side
c	of ivhold (see above)
	ENDIF
	idelt=igapv(i)/iDd	   !# of elements of iDACv corresp to gap
	j1=j2+idelt+1
9	continue
c
c After this loop, iDAC(j2) is the last element to be set high. After this
c need only one element set low, and DAC output will stay low thereafter
c so iDAC(j2+1) is last element needed
	jvlast=j2+1

	RETURN            !end of DEFVOLT
	end

c==========================================================================

	subroutine DEFDAC(iDAC,iDACc,iDACv,jclast,jvlast,jmax,vjump,
     & control,iszout,idimc,idimv,idim2)
	integer*4 iszout
	integer*2 iDAC(idim2)!for DAC output
	integer*2 iDACc(idimc),iDACv(idimv)   !for DAC2 (c-jump) and DAC3 (v-jump)
	logical vjump,control
	logical debug,caplock
	common/mousval/mouse_on,nbutton
c To define the array, iDAC, that is actually used to control DACs.  If
c V-jumps not done then then this is simply iDACc(). If both done
c then iDACc and iDACv must be interleaved (DACs 2 and 3 are updated
c simultaneously), and to define the length of the array that is downloaded
c to 1401 (jclast=last point needed for c-jump=DAC2, jvlast=last point
c needed for v-jump=DAC3)
c If C-jump not done via DAC2 (signalled by jclast=0) then have only the
c voltage output (DAC3), same whether 'control' or 'drug'.
c
c NB Size (in bytes), isz and iszout, MUST be multiple of 4 (for 2-byte
c data ie 12 bit accuracy)
c	iszout=4000		!bytes=int2(2*idim2)=size of iDAC to be downloaded
c
	debug()=caplock()
c
	if(.not.vjump) then
c Section for conc jump only
	   nbyte=2*(jclast)
	   irem=MOD(nbyte,4)			!must be 0 or 2
	   nbyte=nbyte+irem			!round up so divisible by 4
	   iszout=nbyte			!bytes=size of iDAC to be downloaded
	   jclast=nbyte/2			!reset, in case nbyte increased above
	   nval=jclast
	   do 1 i=1,nval
	   iDAC(i)=iDACc(i)
1	   continue
c
	else if((vjump.and.control).or.jclast.eq.0) then		!V-jump only
	   nbyte=2*(jvlast)
	   irem=MOD(nbyte,4)			!must be 0 or 2
	   nbyte=nbyte+irem			!round up so divisible by 4
	   iszout=nbyte			!bytes=size of iDAC to be downloaded
	   jvlast=nbyte/2			!reset, in case nbyte increased above
	   nval=jvlast
	   do 2 i=1,nval
	   iDAC(i)=iDACv(i)
2	   continue
c
	else if(vjump.and.(.not.control).and.jclast.gt.0) then
c
c     Section when both v-jump and c-jump done
c        Length to be downloaded will depend on whether DAC2 or DAC3 is the
c       last one to be changed. Must load all points from both iDACv and iDACc
c       for i=1,jmax where jmax=max(jclast,jvlast), so total number of points
c       in iDAC() will be 2*jmax (jmax points from each
	   j=0		!index for iDACc, iDACv
	   jmax=jclast
	   if(jvlast.gt.jclast) jmax=jvlast
	   nval=2*jmax		!number of points in iDAC to be downloaded
	   nbyte=2*nval
	   irem=MOD(nbyte,4)			!must be 0 or 2
	   nbyte=nbyte+irem			!round up so divisible by 4
	   iszout=nbyte		!bytes=size of iDAC to be downloaded
	   nval1=nbyte/2			!reset, in case nbyte increased above
	   nval=nval1/2			!for debug below
	   do 3 i=1,nval1-1,2		!index for iDAC=1,3,5
		j=j+1
		iDAC(i)=iDACc(j)		!i=1,3,5,...,nval-1
		iDAC(i+1)=iDACv(j)		!i+1=2,4,6,...,nval
3	   continue
	endif
c Debug:

	if(debug()) then
32	   continue
	   print 33,nval
33	   format(' Number of (pairs of) values downloaded = ',i8,/,
     &   ' print iDACc,iDACv,iDAC from #i1 to i2 [0 to end];i1,i2= ')
	   read 31,i1,i2
31	   format(2i8)
	   if(i1.le.0) goto 99
c	   n=i2-i1+1
	   print 331
331	   format(
     &   '    i    j    iDACc(i) iDACv(i) iDAC(j-1) iDAC(j)')
	   do 40 i=i1,i2
	      j=2*i
	      print 5,i,j,iDACc(i),iDACv(i),iDAC(j-1),iDAC(j)
5	      format(1x,2i5,4i8)
	   if(mod(i-i1+1,24).eq.0) then
		print 511
511		format(' Hit any key to continue')
		call ANYKEY()
	   endif
40	   continue
	   goto 32
99	continue
	endif
c
	RETURN            !end of DEFDAC
	end

c===========================================================================

	subroutine DEFADC(nprime,irate,idD,nDc,nDv,srate,ndiv1,ndiv2,ndiv,
     & dA,nAc,dnAc,nAv,dnAv,ipre,icount,iexact,ncjump,nvjump,sampv,
     & ipre1,icount1,nerr,noquery,ibox,icwd,icqd)
c    Modified 03/27/91 03:10pm so that DAC rate can be reset here (arguments
c ipre1,icount1,nerr added): nerr=0 if DAC rate not changed, nerr=1 if
c changed (so DAC arrays need to be recalc and downloaded)
c    Modified 11/03/90 05:33pm to return also dnAc=number of musec from
c iADC(nAc) to start of 1st C-jump pulse (and dnAv similar for V-jump)
c for use in case where there is no ADC sample exactly coincident with start
c of the first c-jump,vjump (see notes on timing at start of main prog).
c
c To calc ADC frequency vs ticks-between-samples with 4MHz clock
c In general, finds ipre,icount to give, as close as possible, the requested
c frequency (IRATE Hz).  But in this application, in order to get an ADC sample
c synchronised exactly with the moment that DAC pulse rises, must use
c (1) only rates that can be achieved exactly, and (2) sampling freq must
c be such that
c		nAc = 1 + nDc*dD/dA  must be exactly an integer
c where iADCc(nAc) is at time = (nAc-1)*dA from moment of trigger (t=0)
c and DAC goes high at time nDc*dD where iDAC(nDc) is 1st element set high
c where dA=time between ADC samples=1/ADC freq=integer # of 0.25 mus ticks
c and dD=time between DAC outputs=1/DAC freq=integer # of 0.25 mus ticks=1ms now
c To get an ADC sample, viz that in ADC(nAc), coincident with the moment
c the (first) DAC pulse starts then must have
c		iTPREc = (nAc-1)*dA = nDc*dD = time from trigger to start of pulse
c
c For V-jumps, sample rates, dD,dA are same, as for c-jumps
c If iTPREv=time to start of 1st Vjump
c		iTPREv = (nAv-1)*dA = nDv*dD
c =time from trigger to start of 1st jump
c where iADC(nAv) is coincindent with the moment of the (1st) V-jump when
c iDACv(nDv) is the first element of iDACv to depart from resting pot.
c  dD=1ms for DAC freq= 1 kHz (input), so nDv=index of first point in iDACv() to be
c set high, is ITPREv/dD
c
c INPUT:
c	nprime(),irate (integer Hz)
c	idD=integer mus between DAC outputs
c	nDc=as above (defined in DEFCONC)
c	nDv=as above (defined in DEFVOLT)
c	sampv=false if sampling current only; true if sampling ADC0 AND ADC1
c
c OUTPUT:
c  iexact=0 if requested frequency, IRATE (Hz), can be achieved exactly
c  iexact=-1 if freq can be achieved exactly, but the requested frequency is
c	such that there is not an ADC sample coincident with start of (first)
c	DAC c-jump pulse
c  iexact=-2 if freq can be achieved exactly, but the requested frequency is
c	such that there is not an ADC sample coincident with start of (first)
c	V-jump pulse
c  iexact=1 if freq cannot be achieved with integer # (ndiv) of 0.25 mus ticks
c  iexact=2 if freq could be achieved, except that ndiv is prime and so
c		cannot be factored into ipre*icount (each >=2)
c  ndiv=number of 0.25 mus ticks between samples (ndiv/4=mus bet samples)
c  dA=number of mus between ADC samples (1 channel)
c  nAc= index in iADC() of the sample coincident with start of 1st DAC pulse
c  nAv= index in iADC() of the sample coincident with start of 1st V-jump
c  srate=sample frequency (Hz) (1 channel)
	integer*2   videotyp
	integer*4 	ipre,icount,ipre1,icount1
	integer 	nprime(1900)	!holds primes up to 16381 (see PRIMGEN.FOR)
	character   def*30,cans*30,getint*11,getreal*13
      logical     discprt,pon,slock,debug,caplock,sampv,noquery,alpha
	character*1 ans,UC
	common/dp/discprt
	common/mousval/mouse_on,nbutton
c
	pon()=slock()
	debug()=caplock()
	alpha=videotyp().eq.3
c
101	format(a1)
c
31	continue
	iexact=0		!result is exact
	if(noquery) goto 32
	if(.not.alpha) then
	   i=irate
	   call defolti(i,def)
	   call qdialog(ibox,' Sampling frequency (integer Hz) ',def,icqd
     &   ,cans)
	   call getinpi(cans,i)
	else
	   print 3,irate
3	   format(' Sampling frequency (integer Hz) [',i6,'] = ')
	   read 2,i
2	   format(i8)
	endif
	if(i.gt.0) irate=i
32	continue
	irate1=irate
	if(sampv) irate1=2*irate	!for 2 channels
	rate=float(irate1)
	nclock=4000000	!Hz for 'H' parameter
	clock=float(nclock)
	rem=AMOD(clock,rate)
	if(abs(rem).gt.1.e-20) iexact=1
	if(iexact.ne.0) then
	   call BELL(1)
	   if(.not.alpha) then
            call wdialog(ibox,
     &     'Time between ADC samples must be multiple of 0.25 mus',icwd)
	   else
	      print 41
41	      format(
     &      ' Time between ADC samples must be multiple of 0.25 mus',/)
	   endif
	   goto 31		!try again
	endif
	div=clock/rate	!=number of 0.25 microsecond ticks between samples
	ndiv=IFIXR(div)
c Now define ndiv1,ndiv2 (both in range 2-32767) such that ndiv=ndiv1*ndiv2
c Start with ndiv1=2, and increase it if necessary.
	errmin=1.e35
	do 21 i=1,1900
	   np=nprime(i)	!1st 1900 primes (up to 16381) (inc the 1st=2)
	   ndiv1=np
	   ndiv2=ndiv/np
	   if(ndiv2.eq.1) goto 22	!must be=>2, so use vals with smallest error so far
	   ndtry=ndiv1*ndiv2
	   if(debug()) then
	      error=float(ndtry)/float(ndiv)
	      if(error.lt.1.0) error=1.0/error
	      error=error-1.0
	      print 24,i,ndiv,ndtry,ndiv1,ndiv2,error
24	      format(1x,i5,' n,n1*n2,n1,n2,err = ',4i8,3x,f10.6,/)
	      if(ndtry.eq.ndiv) goto 23		!exact solution found
	   else
	      if(ndtry.eq.ndiv) goto 23		!exact solution found
c	      If no exact factors found then look for least error (NOT used
c	      in CJUMP for which sample rate must be exact)
	      error=float(ndtry)/float(ndiv)
	      if(error.lt.1.0) error=1.0/error
	      error=error-1.0
	   endif
	if(error.lt.errmin) then
	   errmin=error
	   imin=i		!record index for least error
	endif
21	continue
c
c No exact factorisation- use best found
22	continue
	iexact=2
	ndiv1=nprime(imin)		!most precise factors found
	ndiv2=ndiv/ndiv1
	call intconv(ndiv,getint)
	nm=nblank1(getint)
	if(iexact.ne.0) then
	   call BELL(1)
	   if(.not.alpha) then
            call wdialog(ibox,' # of clock ticks = '//
     &      getint(1:nm)//' -cannot factorise exactly',icwd)
	   else
	      print 42,ndiv
42	      format(
     &      ' # of clock ticks = ',i7,' -cannot factorise exactly',/)
	   endif
	   goto 31		!try again
	endif
c
c Jump here if exact solution found
23	continue	!ndiv1,ndiv2 now defined as precisely as possible
	ndiv=ndiv1*ndiv2	!# of 0.25 mus ticks between ADC samples
	dA=0.25*float(ndiv)	!# of microsec between ADC samples
	if(sampv) dA=2.0*dA	!for each channel separately
c
c Now test whether nAc = 1 + nDc*dD/dA  is exactly an integer as required
c for c-jump
	if(ncjump.gt.0) then
46	 dD=float(iDd)	!microsec between DAC outputs
	 fnAc=1.0 + float(nDc)*dD/dA
	 dnAc=AMOD(fnAc,1.0)*dA	!number of musec from iADc(nAc) to start of pulse
	 pact=abs(dnac)

	 if(abs(dnAc).lt.1.e-6) then
	   nAc=ifixr(fnAc)
	 else
	   nAc=ifix(fnAc)		!round down, so dnAc is pos
	   iexact=-1
	   call BELL(1)
	   call intconv(nac,getint)
	   nm=nblank1(getint)
	   call realtoch(dnac,getreal,13)
	   nmr=nblank1(getreal)
	   if(.not.alpha) then
	      call wdialog(ibox,
     &      'No ADC sample coincident with start of 1st C-jump pulse',
     &	icwd)
	      call wdialog(ibox,'which is '//getreal(1:nmr)//
     &      ' microsec after iADC ( '//getint(1:nm)// ' )',icwd)
		 ans='N'
		 call defolta(ans,def)
	      call qdialog(ibox,'O.K.',def,icqd,cans)
		 call getinpa(cans,ans)
	   else
	      print 43,dnAc,nAc
43	      format(
     &    ' No ADC sample coincident with start of 1st C-jump pulse',/,
     &    ' which is ',f9.1,' microsec after iADC(',i5,'): O.K. [N] ? ')
	      read 101,ans
	   endif
	   if(UC(ans).ne.'Y') then
c	   otherwise ask if DAC rate to be changed
	      if(.not.alpha) then
	   	   call intconv(idd,getint)
	   	   nm=nblank1(getint)
		   ans='Y'
		   call defolta(ans,def)
     		   call qdialog(ibox,' DAC now at '//getint(1:nm)//
     &	   ' microsec/point: alter it ',def,icqd,cans)
	   	   call getinpa(cans,ans)

	      else
		   print 44,iDd
44		   format(
     &	   ' DAC now at ',i5,' microsec/point: alter it [Y] ? ')
		   read 101,ans
	      endif
	      if(UC(ans).ne.'N') then
		   call DEFDRATE(iDd,ipre1,icount1,.false.,nerr)
	   	   call intconv(idd,getint)
	   	   nm=nblank1(getint)
		   if(nerr.eq.1) then
	            if(.not.alpha) then
			   call wdialog(ibox,' DAC rate changed to '//
     &               getint(1:nm)// ' microsec/point',icwd)
	            else
			   print 45,iDd
			   if(discprt) write(8,45) iDd
45			    format(
     &	          ' DAC rate changed to ',i5,' microsec/point')
	            endif
			goto 31		!check that new iDd is OK
		   endif
		endif
		goto 31		!check that new iDd is OK
	   endif
c Carry on if OK to haave no coincident point
	 endif
	endif
c
c And test whether nAv = 1 + nDv*dD/dA  is exactly an integer as required
c for V-jump
	if(nvjump.gt.0) then
	 dD=float(iDD)	!microsec between DAC outputs
	 fnAv=1.0 + float(nDv)*dD/dA
	 dnAv=AMOD(fnAv,1.0)*dA	!number of musec from iADC(nAv) to start of pulse
	 if(abs(dnAv).lt.1.e-6) then
	   nAv=ifixr(fnAv)
	 else
	   nAv=ifix(fnAv)		!round down, so dnAv is pos
	   iexact=-2
	   call BELL(1)
	   if(.not.alpha) then
	   	call intconv(nav,getint)
	   	nm=nblank1(getint)
	   call realtoch(dnav,getreal,13)
	   nmr=nblank1(getreal)
	      call wdialog(ibox,
     &   'No ADC sample coincident with start of 1st V-jump pulse',icwd)
	      call wdialog(ibox,'which is '//getreal(1:nmr)//
     &   ' microsec after iADC ( '//getint(1:nm)//' )',icwd)
		 ans='N'
		 call defolta(ans,def)
	       call qdialog(ibox,'O.K.',def,icqd,cans)
	       call getinpa(cans,ans)
	   else
	      print 431,dnAv,nAv
431	      format(
     & ' No ADC sample coincident with start of 1st V-jump pulse',/,
     & ' which is ',f9.1,' microsec after iADC(',i5,'): O.K. [N] ? ')
	   endif
	   if(UC(ans).ne.'Y') then
c	      otherwise ask if DAC rate to be changed
	      if(.not.alpha) then
	   	   call intconv(idd,getint)
	   	   nm=nblank1(getint)
		   ans='Y'
		   call defolta(ans,def)
     		   call qdialog(ibox,' DAC now at '//getint(1:nm)//
     &	   ' microsec/point: alter it ',' [Y] = ',icqd,cans)
		   call getinpa(cans,ans)
	      else
		   print 441,iDd
441		   format(
     &	' DAC now at ',i5,' microsec/point: alter it [Y] ? ')
		  read 101,ans
	      endif
		if(UC(ans).ne.'N') then
		   call DEFDRATE(iDd,ipre1,icount1,.false.,nerr)
		   if(nerr.eq.1) then
			print 45,iDd
			if(discprt) write(8,45) iDd
			goto 31		!check that new iDd is OK
		   endif
		endif
		goto 31		!check that new iDd is OK
	   endif
c Carry on if OK to have no coincident point
	 endif
	endif
c
c Satisfactory sample rate now found!
	d1=float(ndiv1*ndiv2)		!actual divisor=ticks bet samples
	srate=clock/d1    		!actual sample rate
	if(sampv) srate=srate/2.0	!return 1-channel sample rate
c Define ipre,icount
	ipre=ndiv1
	icount=ndiv2
c
	RETURN		!from DEFADC
	end



c========================================================
	subroutine DEFNSAMP(kmax,iTSAMP,dA,nsamp,isz,ibad,sampv,
     & ikeep,nkeep,kstep,jkeep,irate,nsweep,nsamp1,ibox,icwd,icqd)
c===========================
      integer*2	kstep(5),videotyp
	integer*4 	isz
	integer*4 	ikeep(4,2,30)
      character 	charout,titlew*15
	character 	def*30,cans*30,getint*11,getint1*11,getreal*13
	character*1 ans,UC
	character*72 que(20),helps(5)
	logical 	sampv,mkeep,alpha
	common/mousval/mouse_on,nbutton
c
	alpha=videotyp().eq.3
	titlew='OPTIONS'
	   ixlow=200
	   iyhiw=400
	   ic=15
	   icup=15
	   icf=8
	   ibkw=4
	   nhelp=1
c
101	format(a1)
4	format(i8)
c Define number of points in sample ( and isz, for ADCMEM)
c The ADC value iADC(nAc) is coincident with start of (first) DAC pulse
c and nBc=number of post-trigger ADC samples is given by iTPOSTc=nBc*dA where
c dA=number of musec between ADC samples
c NB Size (in bytes), isz and iszout, MUST be multiple of 4 (for 2-byte
c data ie 12 bit accuracy); ie nsamp must be even
	ibad=0
	nsamp=ifixr(float(iTSAMP)/dA)		!both microsec
	irem=MOD(nsamp,2)
	if(irem.eq.1) nsamp=nsamp+1
c
	nsmax=kmax	!but may be only kmax if controls done!
	if(nsamp.gt.kmax) then				!nsamp=<kmax always OK
	   if(alpha) then
	      print 2,nsamp,nsmax,kmax,kmax
2	      format('&Sample has ',i7,' points at present:',/,
     & ' Maximum number of points = ',i7,' unless it is wished to show',
     & /,'''drug'' and ''control'' traces together in CJFIT in which',/,
     & ' case maximum is ',i7,': make the maximum ',i7,' [N] ? ')
	      read 101,ans
	   else
	   	call intconv(nsamp,getint)
	   	nm=nblank1(getint)
	      call wdialog(ibox,'Sample has '//getint(1:nm)//
     &      ' points at present:',icwd)
	   	call intconv(nsmax,getint)
	   	nm=nblank1(getint)
	      call wdialog(ibox,'Maximum number of points = '//
     &      getint(1:nm)//' unless it is wished to show',icwd)
	      call wdialog(ibox,' ''drug'' and ''control'' traces together
     &in CJFIT, ',icwd)
	   	call intconv(kmax,getint)
	   	nm=nblank1(getint)
	      call wdialog(ibox,'  in which case maximum is '//
     &	      getint(1:nm)//' : ',icwd)
		   ans='N'
		   call defolta(ans,def)
	   	call intconv(kmax,getint)
	   	nm=nblank1(getint)
	      call qdialog(ibox,'Make the maximum '//getint(1:nm)
     &      , def,icdq,cans)
		call getinpa(cans,ans)
	   endif
	   if(UC(ans).eq.'Y') nsmax=kmax
	endif
c
	if(nsamp.le.nsmax) then
	   iopt=1
	   if(alpha) then
	      print 1,nsamp
1	      format('&Sample has ',i6,' points:',/,
     &	' (1) Keep the whole sample',/,
     &	' (2) Define parts of sample to be kept',/,
     &	' (3) Redefine the sampling rate etc',/,
     &	' Option number [1] = ')
	      read 4,i
	   else
	   	call intconv(nsamp,getint)
	   	nm=nblank1(getint)
		call wdialog(ibox,'Sample has '//getint(1:nm)//
     &	' points',icwd)
		que(1)='(1) Keep the whole sample'
		que(2)='(2) Define parts of sample to be kept'
		que(3)='(3) Redefine the sampling rate etc'
		nop=3
		i=1
	   	nhelp=1
	   	call POPMENU(ixlow,-1,iyhiw,que,nop,ic,icf,icup,ibkw,
     &   	titlew,helps,nhelp,i,charout,ival)
	   endif
	   if(i.ge.1.and.i.le.3) iopt=i
	else
	   call BELL(2)
	   iopt=2
	   if(alpha) then
	      print 7,nsamp,nsmax
7	      format(
     &     ' Sample has ',i8,' points: maximum = ',i6,' at present:',/,
     &	' (2) Define parts of sample to be kept',/,
     &	' (3) Redefine the sampling rate etc',/,
     &	' Option number [2] = ')
	      read 4,i
	   else
	   	call intconv(nsamp,getint)
	   	nm=nblank1(getint)
	   	call intconv(nsmax,getint1)
	   	nm1=nblank1(getint1)
		call wdialog(ibox,'Sample has '//getint(1:nm)//
     &	' points : maximum = '//getint1(1:nm1)//
     &      ' at present',icwd)
		que(1)='(1) Define parts of sample to be kept'
		que(2)='(2) Redefine the sampling rate etc'
		nop=2
		j=1
		ival=1
	   	nhelp=1
	   	call POPMENU(ixlow,-1,iyhiw,que,nop,ic,icf,icup,ibkw,
     &   	titlew,helps,nhelp,j,charout,ival)
		if(j.eq.1) i=2
		if(j.eq.2) i=3
	   endif
	   if(i.ge.2.and.i.le.3) iopt=i
	endif
c Define isz,ist etc
c	ist now defined after DEFCONC so ADC data follows iADC() immediately
c	ist=2000	!DAC data is in bytes 0-1998 so start ADC data at byte 2000
	isz=2*nsamp		!sample size in bytes=multiple of 4
	if(sampv) isz=4*nsamp	!for 2 ADC channels
c
	if(iopt.eq.1) then
	   nkeep=1
	   do 27 m=1,20
	   ikeep(1,1,m)=1
27	   ikeep(1,2,m)=nsamp
	   mkeep=.false.
	else if(iopt.eq.2) then
	   mkeep=.true.
	else if(iopt.eq.3) then
	   ibad=1
	   RETURN
	endif
	if(mkeep) then
	   if(nsweep.gt.1) then
		if(jkeep.eq.0) iopt1=1
		if(jkeep.eq.-1) iopt1=2
		if(jkeep.eq.-2) iopt1=3
		if(jkeep.eq.1) iopt1=4
c		if(ismode.eq.4) iopt1=2
		if(alpha) then
		   print 272,iopt1
272		   format(
     &	' (1) Keep the same points for each sweep ',/,
     &	' (2) Keep the same points relative to time of C-jumps',/,
     &	' (3) Keep the same points relative to time of V-jumps',/,
     &    ' (4) Specify the points to be kept separately for each sweep'
     &	,/,' Option number [',i2,'] = ')
		read 4,i
		else
		que(1)='(1) Keep the same points for each sweep '
		que(2)='(2) Keep the same points relative to time of Cjumps'
		que(3)='(3) Keep the same points relative to time of Vjumps'
		que(4)=
     &     '(4) Specify the points to be kept separately for each sweep'
		nop=4
		i=iopt1
	   	nhelp=1
	   	call POPMENU(ixlow,-1,iyhiw,que,nop,ic,icf,icup,ibkw,
     &   	titlew,helps,nhelp,i,charout,ival)
		endif
		if(i.ge.1.and.i.le.4) iopt1=i
		if(iopt1.eq.1) then
		   n=1
		   jkeep=0
		else if(iopt1.eq.2) then
		   n=1
		   jkeep=-1
		else if(iopt1.eq.3) then
		   n=1
		   jkeep=-2
		else if(iopt1.eq.4) then
		   n=nsweep
		   jkeep=1
		endif
		do 458 m=1,n
458		  call MODKEEP(ikeep,nkeep,kstep,irate,nsamp,nsamp1,nsmax,m
     &        ,ibox,icwd,icqd)
	   else if(nsweep.eq.1) then
		call MODKEEP(ikeep,nkeep,kstep,irate,nsamp,nsamp1,nsmax,1,
     &      ibox,icwd,icqd)
	   endif
	   mkeep=.false.	!reset
	endif
	RETURN            !end of DEFNSAMP
	end

c=================================================
c==============================================================

	subroutine PREC(i,title1,cdate,ctime,naver,navc,iav,vjump,
     & control,iprt,ibox,icwd)
c Call with iprt=1 for screen only; iprt=2 for screen + printout (if
c pon/discprt also set on); iprt=3 for printout only (no screen)
c Altered 03/12/91 09:34am so that direct print to printer is controlled
c ONLY by pon() (to allow list of disc contents to be printed easily)
	integer*2 videotyp
      character cdate*11,ctime*11
	character title1*70,getint*11
	character istr*11,text*20
	logical prt,vdu,pprt,dprt,vjump,control
	logical discprt,pon,slock,alpha
	common/dp/discprt
	common/mousval/mouse_on,nbutton
c
	pon()=slock()
c
	alpha=videotyp().eq.3
	vdu=iprt.eq.1.or.iprt.eq.2
	prt=iprt.eq.2.or.iprt.eq.3
c	pprt=prt.and.pon()
	pprt=pon()
	dprt=prt.and.discprt
102	format(/)
      if(pprt) write(7,102)
      if(dprt) write(8,102)
	n=naver
	if(control) n=navc
	if(iav.eq.0) then
	   text=' Single sweep'
	else
	   call INTCONV(n,istr)
	   text=' Mean of '//istr(1:4)//' sweeps'
	endif
	call intconv(i,getint)
	nm=nblank1(getint)
	if(.not.vjump) then
	   if(.not.alpha) then
		call wdialog(ibox,' Sweep '//getint(1:nm)//
     &	': C-jump;'//cdate//ctime,icwd)
		call wdialog(ibox,text,icwd)
	      goto 8020
	   endif
c	   if(vdu) print 231,i,cdate,ctime,text
8020     if(pprt) write(7,231) i,cdate,ctime,text
         if(dprt) write(8,231) i,cdate,ctime,text
231	   format(' Sweep ',i4,
     &   ': C-jump;',a11,2x,a11,2x,a20)
	else if(vjump.and.control) then
	   if(.not.alpha) then
		call wdialog(ibox,' Sweep '//getint(1:nm)//
     &	': Control V-jump; '//cdate//'  '//ctime,icwd)
	   goto 8021
	   endif
	   if(vdu) print 232,i,cdate,ctime,text
8021     if(pprt) write(7,232) i,cdate,ctime,text
         if(dprt) write(8,232) i,cdate,ctime,text
232	   format(' Sweep ',i4,
     &   ': Control V-jump;',a11,2x,a11,2x,a20)
	else if(vjump.and.(.not.control)) then
	   if(.not.alpha) then
		call wdialog(ibox,' Record '//getint(1:nm)//
     &	': V-jump with agonist; '//cdate//'  '//ctime,icwd)
		call wdialog(ibox,text,icwd)
	   goto 8022
	   endif
c	   if(vdu) print 233,i,cdate,ctime,text
8022     if(pprt) write(7,233) i,cdate,ctime,text
         if(dprt) write(8,233) i,cdate,ctime,text
233	   format(' Record ',i4,
     &   ': V-jump with agonist;',a11,2x,a11,2x,a20)
	endif
	   if(.not.alpha) then
		nm=nblank1(title1)
		call wdialog(ibox,title1(1:nm),icwd)
	   goto 8023
	   endif
c	if(vdu) print *,title1
8023	if(pprt) write(7,*) title1
	if(dprt) write(8,*) title1
	RETURN
	end        !PREC

c==================================================

	integer function ICHECK(ilen,iDd,ibox,icwd,icqd)
c To check if ilen is an integer multiple of iDd
c Returns ICHECK=0 if OK
c	    ICHECK=1 if OK after changing iDd, so must reload everything
c	    ICHECK=2 if not a multiple (and has not been fixed by changed iDd)
	integer*2	videotyp
	logical alpha
	character getint*11,getint1*11,cans*30
	integer*4 ilen,iDd
      character*1 ans,UC
	logical discprt
	common/dp/discprt
c
101   format(a1)
c
	icheck=0
	alpha=videotyp().eq.3
9	continue
	if(mod(ilen,iDd).ne.0) then
	   icheck=2
	   call BELL(2)
	   if(alpha) then
	      print 8,ilen,iDd
8	      format(' ',
     &      i10,' microseconds is not an integer multiple of DAC rate= '
     &      ,i10)
	   else
		call intconv(ilen,getint)
		nm=nblank1(getint)
		call intconv(idd,getint1)
		nm1=nblank1(getint1)
		call wdialog(ibox,getint(1:nm)//
     &	' microseconds is not an integer multiple of DAC rate = '
     &      //getint1(1:nm1),icwd)
	    endif

	   if(ilen.lt.iDd.and.iDd.ge.1000) then
		if(alpha) then
		   print 1,iDd
1		   format(
     &         ' DAC now at ',i5,' microsec/point: decrease it [Y] ? ')
		   read 101,ans
		else
		   call intconv(idd,getint)
		   nm=nblank1(getint)
		   ans='Y'
		   call defolta(ans,def)
		   call qdialog(ibox,' DAC now at '//getint(1:nm)//
     &	   ' microsec/point: decrease it',def,icqd,cans)
		   call getinpa(cans,ans)
		endif
		if(UC(ans).ne.'N') then
		   iDd=iDd/10
		   icheck=1		!signifies that iDd changed!
		   if(alpha) then
		      print 10,iDd
		   else
		      call intconv(idd,getint)
		      nm=nblank1(getint)
		      call wdialog(ibox,' DAC rate changed to '//
     &	      getint(1:nm)//' microsec/point -restart',icwd)
		   endif
		   if(discprt) write(8,10) iDd
10		   format(
     &	' DAC rate changed to ',i5,' microsec/point -restart')
		   goto 9		!check that new iDd is OK
		endif
	   endif
	endif
	RETURN
	end             !ICHECK

c==================================================
	subroutine SETPIEZO(ivdac2,idac2,reverse,noquery,ibox,icqd)
c To reset Piezo voltage. After call to this, must call DEFCONC again with
c noquery=true, to redefine iDACc, then DEFDAC to redefine iDAC, then
c download iDAC
	integer*2	videotyp
	logical alpha
	character cans*30,def*30
      character*1 ans,UC
	logical reverse,noquery
	common/mousval/mouse_on,nbutton
c

	alpha=videotyp().eq.3
101	format(a1)
2	format(i8)
c
	if(noquery) goto 10
	if (alpha) then
	   print 441,ivdac2
441	   format( ' Output voltage for piezo (integer mV) [',i5,'] = ')
	   read 2,i
	else
	   i=ivdac2
	   call defolti(i,def)
	   call qdialog(ibox,' Output voltage for piezo (integer mV)',
     &   def,icqd,cans)
	   call getinpi(cans,i)
	endif
	if(i.ne.0) ivdac2=i
10	continue
	v=float(ivdac2)
	idac2=ifixr(v*32768./5000.)
	if(idac2.gt.32752) idac2=32752
	if(idac2.lt.-32768) idac2=-32768
c
	if(noquery) goto 11
	if(reverse) then
	   if(alpha) then
	   	print 43
43	   	format('&Piezo to be energized in ''off'' position [Y] ? ')
	   	read 101,ans
	   else
		   ans='Y'
		   call defolta(ans,def)
		call qdialog(ibox,
     &	'&Piezo to be energized in ''off'' position',
     &	def,icqd,cans)
		call getinpa(cans,ans)
	   endif
	   if(UC(ans).eq.'N') reverse=.false.
	else
	   if(alpha) then
	      print 44
44	      format('&Piezo to have 0 V in ''off'' position [Y] ? ')
	      read 101,ans
	   else
		   ans='Y'
		   call defolta(ans,def)
		call qdialog(ibox,
     &	'Piezo to have 0 V in ''off'' position',
     &	def,icqd,cans)
		call defolta(cans,ans)
	   endif
	   if(UC(ans).eq.'N') reverse=.true.
	endif

11	continue
	if(reverse) then
	   call SETDAC2(float(ivdac2))	!set to full scale
	else
	  call SETDAC2(0.0)		!set DAC2 to 0mv
	endif
c
	RETURN
	end                    !SETPIEZO

c============================================

	subroutine SETHOLD(ivhold,ivhclamp,comfac,ivhdac,noquery,
     &  ibox,icqd)
c To reset holding potential only; holding pot is reset here but after
c calling this must call DEFVOLT (with noquery=true) to set iDACv (then SETDAC
c to set iDAC, and download iDAC to 1401, so next jump done with correct
c holding pot)
	integer*2	videotyp
	logical alpha
	integer*2 ivhold,ivhclamp
	logical noquery
      character*1 ans,UC
	character cans*30,def*30
	common/mousval/mouse_on,nbutton
c
101   format(a1)
c
	alpha=videotyp().eq.3
	if(noquery) goto 10
32	continue
	if(alpha) then
	   print 3,ivhold
3	   format(
     &   ' Present holding potential = ',i4,': new value (integer mV)=')
	   read 4,i
	   print 35,ivhold
35	   format(
     &   ' Holding potential set on patch clamp box = ',i4,
     &'  : new value (integer mV)=')
	   read 4,i
c======add ivhclamp
	else
	   i=ivhold
	   call defolti(i,def)
	   call qdialog(ibox,'Holding potential set by 1401 (mV) ',
     &    def,icqd,cans)
	   call getinpi(cans,i)
c
	   i1=ivhclamp
	   call defolti(i1,def)
	   call qdialog(ibox,'Holding potential set on patch clamp (mV) ',
     &    def,icqd,cans)
	   call getinpi(cans,i1)
	endif
	if(i.eq.0) then
	   if(alpha) then
	   	print 31
31	   	format('&Are you sure you want 0 mV [Y] ? ')
         	read 101,ans
	   else
		   ans='Y'
		   call defolta(ans,def)
		call qdialog(ibox,'Are you sure you want 0 mV ',def,
     &      icqd,cans)
		call getinpa(cans,ans)
	   endif
         if(UC(ans).eq.'N') goto 32
	endif
	ivhold=int2(i)
	ivhclamp=int2(i1)
4	format(i8)
c
	if(alpha) then
	   print 1,comfac
1	   format(
     &   '&Voltage command/membrane potential ratio [',f10.3,'] = ')
	   read 2,x
2	   format(g13.6)
	else
	   x=comfac
	   call defoltr(x,def)
	   call qdialog(ibox,'Voltage command/membrane potential ratio',
     &   def,icqd,cans)
	   call getinpr(cans,x)
	endif
	if(x.gt.1.e-10) comfac=x
c
10	continue
	vhold=float(ivhold)
	call SETPOT(vhold,comfac,ivhdac)
c
14	continue
	RETURN
	end                    !SETHOLD

c==========================================================

	subroutine MODKEEP(ikeep,nkeep,kstep,irate,nsamp,nsamp1,nsmax,m,
     &ibox,icwd,icqd)
	integer*4 ikeep(4,2,30)
	integer*2 kstep(5),videotyp
	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	real*4 swval(30)
      character 	ans,UC,getch,def*30
      character 	getint*11,getreal*13,getreal1*13
      character	cans*30,charout,title*15
	character*11 getint1,getint2,getint3,getint4
	character*72	que(20),helps(5)
	logical extra,alpha
	logical mouse_on
	common/mousval/mouse_on,nbutton
	common/fixswp/ismode,nsweep,swval,itPREc,itPREv,ncjump,nvjump,
     & ilenc,igapc,ilenv,igapv,tkpre,tkpost,jkeep		!for MODKEEP
	mouse_on=.true.
	alpha=videotyp().eq.3
	title='OPTIONS'
	ixlow=8
	iylow=-1
	iyhiw=400
	ic=15
	icf=8
	ibkw=4
	icup=15
c To modify ikeep,nkeep,kstep
101	format(a1)
4	format(i8)
274	dx=1.e3/float(irate)
	iopt=3	!default
	if(nkeep.eq.1.and.ikeep(1,1,m).eq.1.and.
     & int4(ikeep(1,2,m)).eq.nsamp) then
	   call wdialog(ibox,' Keeping whole sample at present',icwd)
	   iopt=1
	endif
	call intconv(nsamp,getint)
	nm=nblank1(getint)
      que(1)='(1)Keep the whole sample ('//getint(1:nm)//
     &') on disc'
      que(2)='(2)Keep only parts of the sample on disc'
      que(3)='(3)Keep parts on disc at full sample rate+keep reduced rat
     &e between them'
	nq=3
	i=iopt
	nhelp=1
	call POPMENU(ixlow,iylow,iyhiw,que,nq,ic,icf,icup,ibkw,
     &   title,helps,nhelp,i,charout,ival)
	if(i.ge.1.and.i.le.3) iopt=i
	if(iopt.eq.1) then
	   nkeep=1
	   ikeep(1,1,m)=1
	   ikeep(1,2,m)=nsamp
	   nsamp1=nsamp
	   extra=.false.
	endif
	extra=iopt.eq.3
	if(.not.extra) then
	   do 71 i=1,5
71	   kstep(i)=0
	endif
	if(iopt.eq.1) goto 999
470	continue		!return here for new tkpre,tkpost if values bad
	if(jkeep.le.-1) then
	   x=tkpre
	   call defoltr(x,def)
         call qdialog(ibox,'&length of sample BEFORE start of each jump
     &(ms)',def,icqd,cans)
	   call getINPr(cans,x)
	   if(x.gt.0.) tkpre=x
	   x=tkpost
	   call defoltr(x,def)
         call qdialog(ibox,'&length of sample AFTER start of each jump
     &(ms) ',def,icqd,cans)
	   call getINPr(cans,x)
	   if(x.gt.0.) tkpost=x
	   if(jkeep.eq.-1) nkeep=ncjump
	   if(jkeep.eq.-2) nkeep=nvjump
	endif
	i=nkeep
	call defolti(i,def)
      call qdialog(ibox,'&Number of sections of ADC sample to be kept ',
     &def,icqd,cans)
	call getINPi(cans,i)
	if(i.ge.1.and.i.le.4) nkeep=i
	nsamp1=0	!# of points actually kept (need array?)
c
	t0=float(itPREc)
	if(jkeep.eq.-2) t0=float(itPREv)
	t0=t0/1000.		!time (ms) to start of 1st jump for sweep m
c
	do 28 i=1,nkeep
	   if(jkeep.le.-1) then
		t1= t0 - tkpre
		t2= t0 + tkpost
		i1=1 + ifixr(t1/dx)
		i2=1 + ifixr(t2/dx)
	      ikeep(i,1,m)=i1
	      ikeep(i,2,m)=i2
	   else		!use existing defaults in ikeep
	 	i1=int4(ikeep(i,1,m))
	 	i2=int4(ikeep(i,2,m))
	 	t1=float(i1-1)*dx
	 	t2=float(i2-1)*dx
	   endif
	   ans='Y'
	   call defolta(ans,def)
	   call intconv(i,getint)
	   nm=nblank1(getint)
	   call intconv(i1,getint1)
	   nm1=nblank1(getint1)
	   call intconv(i2,getint2)
	   nm2=nblank1(getint2)
	   call realtoch(t1,getreal,13)
	   nmr=nblank1(getreal)
	   call realtoch(t2,getreal1,13)
	   nmr1=nblank1(getreal1)
	   call qdialog(ibox,'&('//getint(1:nm)//') Keep from '//
     &   getreal(1:nm)//'(#'//getint(1:nm)//') to '//
     &   getreal(1:nm)//'(#'//getint(1:nm)//
     &   ') ms:',def,icqd,cans)
	   call getinpa(cans,ans)
	   if(UC(ans).eq.'N') then
		call defolt2R(t1,t2,def)
		call qdialog(ibox,'&  Keep from t1 to t2 msec: t1,t2 ',def,
     &	icqd,cans)
		call getINP2R(cans,t1,t2)
		ikeep(i,1,m)=int4(1 + ifixr(t1/dx))
		ikeep(i,2,m)=int4(1 + ifixr(t2/dx))
	      i1=int4(ikeep(i,1,m))
	      i2=int4(ikeep(i,2,m))
	   endif
	   nsamp1=nsamp1+int4(ikeep(i,2,m)-ikeep(i,1,m)+1)
c For each bit kept, ask also if some points to be before it
c Keep reduced sample rate before 1st bit?
	   if(extra.and.i.eq.1.and.ikeep(1,1,m).gt.1) then
		n=kstep(1)
		call defolti(n,def)
            call qdialog(ibox,'& Keep also every nth point before first
     & bit: n ',def,icqd,cans)
	      call getinpi(cans,n)
	      if(n.ge.1) kstep(1)=int2(n)
c	add these extra points to nsamp1
	      n2=int4(ikeep(1,1,m))-1
	      n=int4(kstep(1))
	      do 5 k=1,n2,n
5	      nsamp1=nsamp1+1
	   endif
	   if(extra.and.i.gt.1) then
		n=kstep(i)
		call defolti(n,def)
	      call intconv(i-1,getint)
	      nm=nblank1(getint)
	      call intconv(i,getint1)
	      nm1=nblank1(getint1)
            call qdialog(ibox,'& Keep also every nth point between bits'
     &      //getint(1:nm)//' and '//getint(1:nm)//': n ',
     &      def,icqd,cans)
	      call getinpi(cans,n)
	      if(n.ge.1) kstep(i)=int2(n)
	      if(i.gt.1) then
	   	   n1=int4(ikeep(i-1,2,m)+kstep(i))
	   	   n2=int4(ikeep(i,1,m))-1    !1st point of next kept bit -1
	         n=int4(kstep(i))
	         do 6 k=n1,n2,n
6	         nsamp1=nsamp1+1
	      endif
	   endif
	   if(jkeep.eq.-1) then
	      if(ismode.eq.4) then
		   t0=t0+float(ilenc(i))/1000. + swval(m)     !swval=gap for sweep m
	      else
		   t0=t0+float(ilenc(i)+igapc(i))/1000.  !time to start of next jump
	      endif
	   else if(jkeep.eq.-2) then
	      t0=t0+float(ilenv(i)+igapv(i))/1000.	!time to start of next jump
	   endif
28	continue	!end of i=1,nkeep loop
c
c Ask if any points to be kept after the last bit
	   if(extra.and.ikeep(nkeep,2,m).lt.nsamp) then
		n=kstep(nkeep+1)
		call defolti(n,def)
            call qdialog(ibox,'& Keep also every nth point after last
     &bit: n ',def,icqd,cans)
	      call getinpi(cans,n)
	      if(n.ge.1) kstep(nkeep+1)=int2(n)
	      n1=int4(ikeep(nkeep,2,m)+kstep(nkeep+1))
	      n=int4(kstep(nkeep+1))
	      do 7 k=n1,nsamp,n
7	      nsamp1=nsamp1+1
	   endif
c
	   call intconv(nsamp1,getint)
	   nm=nblank1(getint)
	   call intconv(nsmax,getint1)
	   nm1=nblank1(getint1)
	if(nsamp1.gt.nsmax) then		!still too many points
	   call BELL(1)
	   call wdialog(ibox,getint(1:nm)//
     &   ' points: too many (max = '//getint(1:nm)//')',icwd)
	   goto 274		!try again
	else
	   call wdialog(ibox,getint(1:nm)//
     &   ' points now kept on disc',icwd)
	endif
c+++++++++++++++++++++++++++++++++++++++++

c Define other sweeps in cases where each sweep not specified separately
	if(nsweep.eq.1) goto 999
	if(jkeep.eq.0) then	!define ikeep for m>1
	   do 276 m=2,nsweep
	   do 276 i=1,nkeep
	    ikeep(i,1,m)=ikeep(i,1,1)
	    ikeep(i,2,m)=ikeep(i,2,1)
276	    continue
	endif
	if(jkeep.le.-1) then	!define ikeep for m>1
	   do 277 m=1,nsweep
	   t0=float(itPREc)
	   if(jkeep.eq.-2) t0=float(itPREv)
	   t0=t0/1000.		!time (ms) to start of 1st jump for sweep m
	   do 278 i=1,nkeep
	    t1= t0 - tkpre
	    t2= t0 + tkpost
	    i1=1 + ifixr(t1/dx)
	    i2=1 + ifixr(t2/dx)
	    if(i1.lt.1) i1=1
	    if(i2.gt.nsamp) i2=nsamp
	    if(i.gt.1.and.i1.le.i2last) then
		call intconv(m,getint)
		nm=nblank1(getint)
		call intconv(i,getint1)
		nm1=nblank1(getint1)
		call intconv(i1,getint2)
		nm2=nblank1(getint2)
		call intconv(i2last,getint3)
		nm3=nblank1(getint3)
		call intconv(i-1,getint4)
		nm4=nblank1(getint4)
		call BELL(2)
		call wdialog(ibox,' For sweep #'//getint(1:nm)//
     &      ', kept section #'//getint1(1:nm1)//'starts at point #'
     &      //getint2(1:nm2)//' which is before the last point, #'
     &      //getint3(1:nm3)//', of kept section #'//
     &      getint4(1:nm4),icwd)
		   ans='Y'
		   call defolta(ans,def)
		call intconv(i2last+1,getint)
		nm=nblank1(getint)
            call qdialog(ibox,'1. Start kept section #'//
     &	getint1(1:nm)//
     &      ' at point #'//getint(1:nm),def,icqd,
     &	cans)
		call getinpa(cans,ans)
		if(UC(ans).eq.'Y') then
		   i1=i2last+1
		   goto 320
		endif
		   ans='Y'
		   call defolta(ans,def)
     	      call qdialog(ibox,'Redefine times to be kept',def,
     &	icqd,cans)
		call getinpa(cans,ans)
		if(UC(ans).eq.'Y') goto 470
	    endif
320	    i2last=i2
	    ikeep(i,1,m)=int4(i1)
	    ikeep(i,2,m)=int4(i2)
	    if(jkeep.eq.-1) then
	      if(ismode.eq.4) then
		  t0=t0+float(ilenc(i))/1000. + swval(m)     !swval=gap for sweep m
	      else
		  t0=t0+float(ilenc(i)+igapc(i))/1000.  !time to start of next jump
	      endif
	    else if(jkeep.eq.-2) then
		t0=t0+float(ilenv(i)+igapv(i))/1000.	!time to start of next jump
	    endif
278	   continue
277	   continue
	endif
999	continue
	RETURN
	end                         !MODKEEP

c========================================================================

	subroutine CALCNS0(ikeep,nkeep,kstep,nsamp,nsamp1,m)
	integer*4 ikeep(4,2,30)
      integer*2 kstep(5)
	logical keepall
c
c Most simplified version of MODKEEP that returns nsamp1 =number of points
c kept on disc for sweep #m
c
	keepall=nkeep.eq.1.and.ikeep(1,1,m).eq.1.and.
     &  ikeep(1,2,m).eq.nsamp
c
	if(keepall) then
	   nsamp1=nsamp
	   RETURN
	endif
c
	nsamp1=0	!# of points actually kept
c
	do 28 i=1,nkeep
c	  nsamp1=nsamp1+int4(ikeep(i,2,m)-ikeep(i,1,m)+1)
	   n1=int4(ikeep(i,1,m))      !1st point of kept bit #i
	   n2=int4(ikeep(i,2,m))      !last point of kept bit #i
	   do 11 j=n1,n2
		nsamp1=nsamp1+1
11	   continue
c Now the bits outside the 'kept sections'
	  if(i.eq.1.and.ikeep(1,1,m).gt.1) then
c	add these extra points to nsamp1
	    n2=int4(ikeep(1,1,m))-1
	    n=int4(kstep(1))
	    do 5 k=1,n2,n
	    nsamp1=nsamp1+1
5	    continue
	  endif
	  if(i.gt.1) then
	   	n1=int4(ikeep(i-1,2,m)+kstep(i))
	   	n2=int4(ikeep(i,1,m))-1    !1st point of next kept bit -1
	      n=int4(kstep(i))
	      do 6 k=n1,n2,n
	      nsamp1=nsamp1+1
6		continue
	  endif
28	continue	!end of i=1,nkeep loop
c
c points kept after the last bit
	  if(ikeep(nkeep,2,m).lt.nsamp) then
c	add these extra points to nsamp1
	    n1=int4(ikeep(nkeep,2,m)+kstep(nkeep+1))
	    n=int4(kstep(nkeep+1))
	    do 7 k=n1,nsamp,n
	    nsamp1=nsamp1+1
7	    continue
	  endif
c
	RETURN
	end                             !CALCNS0

c=================================================
	subroutine CALCNS1(ikeep,nkeep,kstep,nsamp,nsamp1,jmask,keepall,
     & ikeep1,kmax,m)
c=     & ikeep1,m)
	integer*4 ikeep(4,2,30),ikeep1(4,2)
      integer*2 kstep(5)
      integer*1 jmask(kmax)
	logical keepall
c
c Much simplified version of MODKEEP that returns only (1) keepall, (2) nsamp1
c =number of points kept on disc, (3) JMASK() -array to speed location of
c points to be kept, and (4) ikeep1=values from ikeep for specied sweep (#m).
c (for multiple sweeps jmask may need to be reset each cycle (e.g. for double
c pulse expts) -can do by calling this subroutine)
c Define ikeep1 for mth sweep

	do 431 i=1,nkeep
	ikeep1(i,1)=ikeep(i,1,m)
431	ikeep1(i,2)=ikeep(i,2,m)

c
	keepall=nkeep.eq.1.and.ikeep(1,1,m).eq.1.and.
     &  ikeep(1,2,m).eq.nsamp
c      print*,nkeep,ikeep(1,1,m),ikeep(1,2,m)
c
	if(keepall) then
	   do 1 i=1,nsamp
1	   jmask(i)=1
	   nsamp1=nsamp
	   RETURN
	endif
c
	nsamp1=0	!# of points actually kept
	do 2 i=1,nsamp
2	jmask(i)=0
c
	do 28 i=1,nkeep
c	  nsamp1=nsamp1+int4(ikeep(i,2,m)-ikeep(i,1,m)+1)
	   n1=int4(ikeep(i,1,m))      !1st point of kept bit #i
	   n2=int4(ikeep(i,2,m))      !last point of kept bit #i
	   do 11 j=n1,n2
		nsamp1=nsamp1+1
		jmask(j)=1			!keep all points in 'kept section'
11	   continue
c Now the bits outside the 'kept sections'
	  if(i.eq.1.and.ikeep(1,1,m).gt.1) then
c	add these extra points to nsamp1
	    n2=int4(ikeep(1,1,m))-1
	    n=int4(kstep(1))
	    do 5 k=1,n2,n
	    nsamp1=nsamp1+1
	    jmask(k)=1
5	    continue
	  endif
	  if(i.gt.1) then
	   	n1=int4(ikeep(i-1,2,m)+kstep(i))
	   	n2=int4(ikeep(i,1,m))-1    !1st point of next kept bit -1
	      n=int4(kstep(i))
	      do 6 k=n1,n2,n
	      nsamp1=nsamp1+1
	      jmask(k)=1
6		continue
	  endif
28	continue	!end of i=1,nkeep loop
c
c points kept after the last bit
	  if(ikeep(nkeep,2,m).lt.nsamp) then
c	add these extra points to nsamp1
	    n1=int4(ikeep(nkeep,2,m)+kstep(nkeep+1))
	    n=int4(kstep(nkeep+1))
	    do 7 k=n1,nsamp,n
	    nsamp1=nsamp1+1
	    jmask(k)=1
7	    continue
	  endif
c
	RETURN                           !CALCNS1
	end


c========================================================================

c	subroutine CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,keepall)
	subroutine CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,
     & kmax,keepall)
	integer*4 ikeep1(4,2)
      integer*2 kstep(5)
      integer*1 jmask(kmax)
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
c	 print*,nkeep,ikeep1(1,1),ikeep1(1,1)
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
c	  nsamp1=nsamp1+int4(ikeep1(i,2)-ikeep1(i,1)+1)
	   n1=int4(ikeep1(i,1))      !1st point of kept bit #i
	   n2=int4(ikeep1(i,2))      !last point of kept bit #i
	   do 11 j=n1,n2
		nsamp1=nsamp1+1
		jmask(j)=1			!keep all points in 'kept section'
11	   continue
c Now the bits outside the 'kept sections'
	  if(i.eq.1.and.ikeep1(1,1).gt.1) then
c	add these extra points to nsamp1
	    n2=int4(ikeep1(1,1))-1
	    n=int4(kstep(1))
	    do 5 k=1,n2,n
	    nsamp1=nsamp1+1
	    jmask(k)=1
5	    continue
	  endif
	  if(i.gt.1) then
	   	n1=int4(ikeep1(i-1,2)+kstep(i))
	   	n2=int4(ikeep1(i,1))-1    !1st point of next kept bit -1
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
	    n1=int4(ikeep1(nkeep,2)+kstep(nkeep+1))
	    n=int4(kstep(nkeep+1))
	    do 7 k=n1,nsamp,n
	    nsamp1=nsamp1+1
	    jmask(k)=1
7	    continue
	  endif
c
	RETURN
	end                      !CALCNS2
c=====================================================================

	subroutine ABORTJ(reverse,ivdac2,ivhold,comfac,auto,
     & isweep,prt)
c to abort jump while waiting for trigger. Now sets isweep=0 so after
c calling this, call NEWAV and goto to 4571 to reload everything.
c
c Earlier version sent astring=char(27)//',I,'//char(13)//';' (esc,I,CR) to
c abort jumps while waiting for trigger, but MEMDAC,K plus ADCMEM,K work
c as well (and less drastic). Now also use ESC,F,CR to flush 1401 i/o
c buffer (seems to have cured the e0=-128 type of crash that sometimes
c occurred when jumps aborted before; this must result from values produced
c by 'ADCMEM,?' not always being read back properly (as this is only
c thing that could give value of -128 which is not a legal value for e0!).
c If this IS cured it is not clear whether abandoning ASTRING did it, or
c whether ESC,F,CR did it!
c
	integer*2 ivhold
	logical auto,discprt,prt,reverse
	character*6 fstring
	common/dp/discprt
c
	fstring=char(27)//',F,'//char(13)//';'	!esc,F,CR to flush 1401 i/o
	if(auto) auto=.false.
	if(prt) print 63,isweep
	if(discprt) write(8,63) isweep
63	format(
     & ' ************JUMP ABORTED (sweep ',i3,')*************')
c	isweep=nsweep	!so terminates multiple sweep series
	isweep=0
c	call cls()
c	pause
c
c  Now kill MEMDAC and ADCmem before going on (completion test skipped)
c (need to stop TIMER2 also??)
c	 write(10,1014)
c1014	 format ('ADCMEM,K;')
c	 write(10,1015)
c1015	 format ('MEMDAC,K;')
c	call forsendstring('ADCMEM,K;',30,ierr)
c	call forsendstring('MEMDAC,K;',30,ierr)
	call forsendstring('ADCMEM,K;',ierr)
	call forsendstring('MEMDAC,K;',ierr)
c
c If MEMDAC is killed with piezo on, or in middle of V-jump the DACS will
c be left set wrongly so restore resting posn of piezo
	if(reverse) then
	   call SETDAC2(float(ivdac2))	!set to full scale
	else
	   call SETDAC2(0.0)		!set DAC2 to 0mv
	endif
c and restore holding potential
	call SETPOT(float(ivhold),comfac,ivhdac)
c
c	write(10,1010) fstring
c1010	format(a6)	!flush 1401 i/o buffers
c	call forsendstring(fstring,50,ierr)
	call CLRKB()
c	write(10,1005)
c1005	format('CLEAR;')
c
c	call forsendstring('CLEAR;',10,IERR)
	call forsendstring('CLEAR;',IERR)

	RETURN
	end                                  !ABORTJ

c=====================================================================

	subroutine NEWAV(kmax,naver,navc,tcur,tvolt,tcurcon,
     &  tvoltcon,control,cnaver,title1,title,n1,n2)
c	real*4 tcur(2*kmax),tcurcon(2*kmax)	!total current (drug,control)
	real*4 tcur(kmax),tcurcon(kmax)	!total current (drug,control)
	real*4 tvolt(kmax),tvoltcon(kmax)	!total voltage (drug,control)
	integer*2 videotyp
      character cnaver*11
	character title*60,title1*70
	logical control,alpha
	common/mousval/mouse_on,nbutton
c To initialise when new average started (==need option to restart drug and
c control averages separately?)
	alpha=videotyp().eq.3
	naver=0
	navc=0
c=	do 1 i=1,2*kmax
	do 1 i=1,kmax
	tcur(i)=0.0
	tcurcon(i)=0.0
1	continue
	do 2 i=1,kmax
	tvolt(i)=0.0
	tvoltcon(i)=0.0
2	continue
c Redefine title1
	if(control) then               	!put 0 on display if next jump aborted
	   call INTCONV(navc,cnaver)
	else
	   call INTCONV(naver,cnaver)
	endif
	title1=title(n1:n2)//':  #'//cnaver
	if(.not.alpha) then
	  call NEWPEN(15)		!for wrstring18 call- white
	  call wrstring18(8,464,title1,15,0)
	  laba=1
	endif
	RETURN
	end                            !NEWAV
c==========================================================

	subroutine PPAR2(iprt,idprt,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,
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
	integer*4 ikeep(4,2,30)
      integer*2 kstep(5),videotyp
	character*11 cdate,ctime
	logical pon,slock,vjump,ramp,sampv,control
	logical discprt
	common/dp/discprt
	common/mousval/mouse_on,nbutton
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
      if(iprt.eq.2) print 61,cdate,ctime,irate,itsamp1,nsamp,iDd
      if(pon()) write(7,61)cdate,ctime,irate,itsamp1,nsamp,iDd
      if(idp.eq.2) write(8,61)cdate,ctime,irate,itsamp1,nsamp,iDd
61	format(1x,a11,3x,a11,/,' Sample rate (Hz) = ',i8,
     & ' Sample length = ',i6,' ms (',i6,' points)',/,
     & ' Microseconds between DAC points = ',i10)
c
	nline=3
c Print IKEEP etc
292	continue
	dx=1.e3/float(irate)
	if(nkeep.eq.1.and.ikeep(1,1,1).eq.1.and.
     & int4(ikeep(1,2,1)).eq.nsamp) then
	   if(iprt.eq.2) print 29
	   if(pon()) write(7,29)
         if(idp.eq.2) write(8,29)
29	    format(' Whole ADC sample kept')
	else
	   if(iprt.eq.2) print 281,nkeep,nsamp1
	   if(pon()) write(7,281) nkeep,nsamp1
         if(idp.eq.2) write(8,281) nkeep,nsamp1
281	   format(
     & ' Number of sections of ADC sample kept = ',i3,' (',i5,
     & ' points)')
	   if(VIDEOTYP().eq.18) goto 981	!don't type rest if in graphics
	   nline=nline+2
	   if(iprt.eq.2) print 98,(kstep(i),i=1,nkeep+1)
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
	     if(iprt.eq.2) print 2831
	     if(pon()) write(7,2831)
           if(idp.eq.2) write(8,2831)
2831	     format(' For all sweeps:')
	   else if(jkeep.eq.-1) then
	     if(iprt.eq.2) print 284,tkpre,tkpost
	     if(pon()) write(7,284) tkpre,tkpost
           if(idp.eq.2) write(8,284) tkpre,tkpost
284	     format(
     &	' Keep from ',f8.1,'ms before each C-jump to ',f8.1,
     &	'ms after each')
	   else if(jkeep.eq.-2) then
	     if(iprt.eq.2) print 285,tkpre,tkpost
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
	     if(iprt.eq.2) print 283,m,nsamp1
	     if(pon()) write(7,283) m,nsamp1
           if(idp.eq.2) write(8,283) m,nsamp1
283	     format(' For sweep number ',i3,' (',i5,' points kept)')
	   endif
	   do 28 i=1,nkeep
		t1=float(ikeep(i,1,m)-1)*dx
		t2=float(ikeep(i,2,m)-1)*dx
	      if(iprt.eq.2) print 282,i,t1,t2,ikeep(i,1,m),ikeep(i,2,m)
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
c
981	continue
	if(ncjump.eq.0) goto 66
      if(iprt.eq.2) print 65,tPREc1,nAc,dnAc
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
      if(iprt.eq.2) print 63,i,alenc(i)
      if(pon()) write(7,63) i,alenc(i)
      if(idp.eq.2) write(8,63) i,alenc(i)
63	format(
     & ' Concentration pulse ',i2,': duration (ms) = ',f10.1)
	if(i.eq.ncjump) goto 62
      if(iprt.eq.2) print 64,agapc(i)
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
      if(iprt.eq.2) print 611,ivhold
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
      if(iprt.eq.2) print 612,iTPREv1,nAv,dnAv
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
         if(iprt.eq.2) print 631,i,alenv(i),ivolt1(i)
         if(pon()) write(7,631) i,alenv(i),ivolt1(i)
         if(idp.eq.2) write(8,631) i,alenv(i),ivolt1(i)
631	   format(' #',i2,
     & ': Voltage jump; duration (ms) = ',f10.1,'; potential (mV)',i4)
	   nline=nline+1
	else
         if(iprt.eq.2) print 632,i,alenv(i),ivolt1(i),ivolt2(i),vstep(i)
         if(pon()) write(7,632) i,alenv(i),ivolt1(i),ivolt2(i),vstep(i)
         if(idp.eq.2) write(8,632)i,alenv(i),ivolt1(i),ivolt2(i),
     &	vstep(i)
632	   format(' #',i2,
     & ': Voltage ramp; duration (ms) = ',f10.1,'; from',i5,' mV to',i5,
     &  ' mV',/,'   (step size = ',f10.3,' mV)')
	   nline=nline+2
	endif
	if(i.eq.nvjump) goto 6211
      if(iprt.eq.2) print 641,agapv(i)
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
        if(iprt.eq.2) print 51
        if(pon()) write(7,51)
        if(idp.eq.2) write(8,51)
51	  format(' Membrane potential sampled on ADC1')
	endif
	if(control) then
        if(iprt.eq.2) print 52
        if(pon()) write(7,52)
        if(idp.eq.2) write(8,52)
52	  format(' CONTROL: V-jump only (no C-jump)')
	endif
c
c Details for multiple sweeps (nsweep,swtime,ismode,swval)
	if(nsweep.gt.1) then
         if(iprt.eq.2) print 53,nsweep,swtime
         if(pon()) write(7,53) nsweep,swtime
         if(idp.eq.2) write(8,53) nsweep,swtime
53	   format(
     & ' SERIES of ',i3,' sweeps at intervals of ',f8.2,' seconds')
	   nline=nline+2
	   if(ismode.eq.2) then
            if(iprt.eq.2) print 54
            if(pon()) write(7,54)
            if(idp.eq.2) write(8,54)
54		format(' Jump potentials (mV) = ')
	      nchar=23
	   else if(ismode.eq.3) then
            if(iprt.eq.2) print 55
            if(pon()) write(7,55)
            if(idp.eq.2) write(8,55)
55		format(' C-jump lengths (ms) = ')
	      nchar=22
	   else if(ismode.eq.4) then
            if(iprt.eq.2) print 56
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
               if(iprt.eq.2) print 81,swval(i)
               if(pon()) write(7,81) swval(i)
               if(idp.eq.2) write(8,81) swval(i)
		   nline=nline+1
		   ncl=7
		else
               if(iprt.eq.2) print 80,swval(i)
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
c
c Print calibration
	nline=nline+3
	if(nline.gt.20.and.iprt.gt.0) then
	   print 650
	   call ANYKEY
	   nline=0
	endif
      if(iprt.eq.2) print 50,amVpA1,ftape,gain,calfac,calvolt
      if(pon()) write(7,50) amVpA1,ftape,gain,calfac,calvolt
      if(idp.eq.2) write(8,50) amVpA1,ftape,gain,calfac,calvolt
50	format(
     & ' Calibration: mV/pA = ',f7.1,': tape factor, gain = ',2g13.6,/,
     & ' Current units per ADC unit; calfac = ',g13.8,/,
     & ' mV out from clamp per mV membrane pot = ',g13.6,/)
c
	RETURN      !from PPAR
	end

c=====================================================================
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
	integer*2 ilen2v(10),igap2v(10),videotyp
	logical discprt,pon,slock,alpha
	common/dp/discprt
	common/mousval/mouse_on,nbutton
c
	pon()=slock()
	alpha=videotyp().eq.3
c
	if(iDd.ne.10.and.iDd.ne.100.and.iDd.ne.1000.and.iDd.ne.10000) then
	   iDd=1000
	   if(.not.alpha) then
		call newpen(13)
		call wrstring18(8,160,' DAC rate assumed to be 1 kHz',
     &	13,0)
		goto 8040
	   endif
	   print 1
8040	   if(pon()) write(7,1)
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
	end           !ilconv

c==============================================

	subroutine SETOPT(ncjump,nvjump,nvramp,ispec,tspec)
	integer*2	videotyp
	logical alpha
	integer ispec(5)
	logical notdef
	character*14 tspec(5)
	character ckey*2,title*30,ans
      character*40 que(10),qopt(10),helps(3)
      character*70 opt(10)
	common/mousval/mouse_on,nbutton
c
c Keys 9 and 10 of main menu boxes, and key 10 of 2ry menu boxes can provide
c 3 of the 4 options as specified by ispec() respectively (ispec=0 means
c no box to be shown)
c If all options possible then must choose which to use
c ispec(1)=main menu key 9
c ispec(2)=main menu key 10
c ispec(3)=2ry menu key 10
c and similarly for tspec()
c If ncjump=0 then options 3,4 not needed
c If ncjump>1 then option 3 not needed
c If nvj=0 or nvj>1 then option 2 not needed
c
	alpha=videotyp().eq.3
	notdef=ispec(1).eq.0.and.ispec(2).eq.0.and.ispec(3).eq.0
	nopt=6			!number of options at present
	nvj=nvjump-nvramp		!number of V-jumps (excluding ramps)
c
c Use defaults if ispec() not defined (e.g by read of parameters from disc)
	if(notdef) then
	 if(ncjump.ne.1.and.nvj.ne.1) then
	   ispec(1)=1		!set pot
	   ispec(2)=4		!piezo voltage
	   if(ncjump.eq.0) ispec(2)=0		!piezo voltage
	   ispec(3)=0
	 else if(ncjump.eq.1.and.nvj.gt.1) then
	   ispec(1)=1		!set pot
	   ispec(2)=3		!change c-jump length
	   ispec(3)=4		!piezo voltage
	 else if(ncjump.eq.1.and.nvj.eq.0) then
	   ispec(1)=3		!change c-jump length
	   ispec(2)=4		!piezo voltage
	   ispec(3)=1		!set holding pot
	 else if(ncjump.ne.1.and.nvj.eq.1) then
	   ispec(1)=1		!set pot
	   ispec(2)=2		!change jump potential
	   ispec(3)=4		!piezo voltage
	   if(ncjump.eq.0) ispec(3)=0		!piezo voltage
	 endif
	endif


c
c	if(ncjump.eq.1.and.nvj.eq.1) then
	if(.not.alpha) then
	   qopt(1)='(1) Change Main Menu Box   # 9'
	   qopt(2)='(2) Change Main Menu Box   #10'
	   qopt(3)='(3) Change Second Menu Box #10'
	   qopt(4)='(4) Continue'

         opt(1)='(1) Set holding potential'
         opt(2)='(2) Change voltage of potential step (when doing one V-
     &jump)'
         opt(3)='(3) Change length of c-jump (for all if >1 c-jump)'
         opt(4)='(4) Change voltage output to piezo (if doing c-jumps)'
         opt(5)='(5) Calibration change '
         opt(6)='(6) RECORD the jump on screen (if RECORD not on)'
         title='SPECIAL OPTIONS'
	   nopt=4
	   nrow=4
	   nval=4
	   no=6
	   ic=14
	   ibk=1
	   ich=12
	   icf=15

	   icm=15
	   ibkm=5
	   ichm=15
	   icfm=0

	   ixlo=16
	   iyhi=400
8026	   continue
	   do 11 i=1,3
		ckey='0.'
		if(i.eq.1) ckey='9.'
		if(ispec(i).eq.1) then
	   	   tspec(i)=ckey//'HOLDING POT '
		else if(ispec(i).eq.2) then
	   	   tspec(i)=ckey//' JUMP POT  '
		else if(ispec(i).eq.3) then
	   	   tspec(i)=ckey//'CJUMP LENGTH'
		else if(ispec(i).eq.4) then
	   	   tspec(i)=ckey//'PIEZO VOLTS '
		else if(ispec(i).eq.5) then
	   	   tspec(i)=ckey//'CALIBRATION '
		else if(ispec(i).eq.6) then
	   	   tspec(i)=ckey//'RECORD SWEEP'
		endif
11	   continue
	   nhelp=1
         que(1)='Main Menu Box   # 9 : '//tspec(1)
	   que(2)='Main Menu Box   #10 : '//tspec(2)
         que(3)='Second Menu Box #10 : '//tspec(3)
	   call POPPARS(ixlo,-1,iyhi,que,title,helps,nhelp,
     &	nrow,nval,ic,ibk,icf,ich,qopt,nopt,iopt,ans,ival)
	   if(iopt.lt.1.or.iopt.gt.4) iopt=1
	   if(iopt.eq.4.or.ival.eq.27.or.ival.eq.13) goto 8025
	   call POPMENU(ixlo,-1,iyhi,opt,no,icm,icfm,ichm,ibkm,
     &   title,helps,nhelp,j,charout,ival)
	   ispec(iopt)=j
	   goto 8026
	endif
	   print 86,ispec(1)
86	   format(
     &' The SPECIAL OPTIONS keys (#9 and 10 on main menu boxes and  ',/,
     &' #10 on 2ry menu) allow rapid access to options              ',/,
     &' needed for particular sorts of experiments. These must be   ',/,
     &' programmed as needed. So far the options are:',/,
     &' (1) Set holding potential                                   ',/,
     &' (2) Change voltage of potential step (available when doing  ',/,
     &'     one V-jump)                                             ',/,
     &' (3) Change length of c-jump (for all if >1 c-jump)          ',/,
     &' (4) Change voltage output to piezo (if doing c-jumps)       ',/,
     &' (5) Calibration change                                      ',/,
     &' (6) RECORD the jump on screen (if RECORD not on)            ',/,
     & /,' Option number for main menu key 9  [',i2,'] = ')
	   read 4,i
4	   format(i8)
	   if(i.ge.1.and.i.le.nopt) ispec(1)=i
88	   print 87,ispec(2)
87	   format(
     &'&Option number for main menu key 10 [',i2,'] = ')
	   read 4,i
	   if(i.eq.ispec(1)) goto 88
	   if(i.ge.1.and.i.le.nopt) ispec(2)=i
90	   print 89,ispec(3)
89	   format(
     &'&Option number for 2ry  menu key 10 [',i2,'] = ')
	   read 4,i
	   if(i.eq.ispec(1).or.i.eq.ispec(2)) goto 90
	   if(i.ge.1.and.i.le.nopt) ispec(3)=i
c	endif
c Define key labels
8025	continue
	do 10 i=1,3
	ckey='0.'
	if(i.eq.1) ckey='9.'
	if(ispec(i).eq.1) then
	   tspec(i)=ckey//'HOLDING POT '
	else if(ispec(i).eq.2) then
	   tspec(i)=ckey//' JUMP POT  '
	else if(ispec(i).eq.3) then
	   tspec(i)=ckey//'CJUMP LENGTH'
	else if(ispec(i).eq.4) then
	   tspec(i)=ckey//'PIEZO VOLTS '
	else if(ispec(i).eq.5) then
	   tspec(i)=ckey//'CALIBRATION '
	else if(ispec(i).eq.6) then
	   tspec(i)=ckey//'RECORD SWEEP'
	endif
10	continue
c
	RETURN
	end

c===================================================================

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
	common/mousval/mouse_on,nbutton
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
c=======================================================

