 	subroutine PSIN(istart,iend,ndata,nint,ngood,nchan,ndev,rate,filt,
     & filt1,calfac,calfac1,CONDAT,ioff,titdat,ibcor,nepsc,madc1,idest)
c To do input for PSREAL.FOR
c Modified 02/06/91 12:45pm to take simulated currents in CONSIM.DAT (from
c EPSCSIM) when ibcor=3.
c Note: ndev1=disc for CONSAM (or CONSIM); ndev=disc for NOISED.DAT and
c for NOISPAR.DAT (N.B. may want to edit same CONSAM more than once,
c and keep results separate)
c 08/30/90 02:25pm Lahey version
	integer istart(512),iend(512)
c for read from CONSAM:
	character CONDAT*30
	character cdate*11,cdate1*11,ndev*2,ndev1*2
c	character header*28,adcdate*8
	character cs*3,adctime*8
	character*76 title,titdat
	logical present,madc1
	integer*2 idt,inc,id1,id2
	integer*4 ioff,ilen
c
	character*1 ans,UC
	logical discprt,pon,slock,debug,caplock
	common/dp/discprt
c
c
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
	print 44
44	format(' Go straight to disc-write [N] ? ')
	read 101,ans
	idest=0
	if(UC(ans).ne.'Y') goto 47
	idest=1		!store spectrum from scratch file
	RETURN		! eg if disc was full at last go
c
47	continue
c
	if(ibcor.eq.3) then
51	   print 50
50	   format(
     &   ' Choose disk for simulated data (CONSIM.DAT):')
	   ndev1='E:'
	   call DISCNUM(idisc,ndev1,0,1)
	   if(idisc.eq.-1) goto 999	!if ESC hit in discnum
	   CONDAT=ndev1//'\CONSIM.DAT'
	   INQUIRE(file=CONDAT,exist=present,flen=len)
	   if(.not.present.or.len.eq.0) then
	     call BELL(1)
	     print 2534,CONDAT
2534	   format('&File ',a15,' not present')
	     goto 51
	   endif
         OPEN(unit=17,file=CONDAT,status='UNKNOWN',
     &   access='TRANSPARENT')
	   read(17,rec=1) nepsc,irate,iepsc		!12 bytes
	   print 52,iepsc,nepsc,irate,condat(1:15)
52	   format(
     &' File contains ',i3,' simulated events, each with ',i5,' points',
     & /,' at ',i7,' Hz, and their mean, in  file ',a15)
	   print 11		!' O.K. [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') goto 51
	   CLOSE(unit=17)		!for now
	   print 53
53	   format(/,' Specify disc to put results of fitting:')
	   ndev=ndev1		!default = same disc as for CONSIM
	   call DISCNUM(id,ndev,0,1)
	   if(id.eq.-1) goto 999		!ESC was hit
c Now make a 'simulated' istart(),iend() for the simulated EPSCs. Need
c this in case only part of the simulated record is used in NOISFIT
c (in fact only the decay is used). This is defined in NOISFIT by
c altering the original istart(i),iend(i) values, the altered values
c being read back in PSREAL, together with fit, when spectra are calculated.
c Pretty arbitrary how istart(),iend() are defined in this case; easier
c not to read CONSIM in sections, but to read each EPSC separately, so
c just define every istart=1,iend=nepsc
	   ngood=iepsc
	   do 54 i=1,ngood
	   istart(i)=1
	   iend(i)=nepsc
54	   continue
	   rate=float(irate)
	   nchan=1
	   calfac=1.0	!already in pA
	   goto 99		!RETURN
	endif
c
c Normal analysis of experimental data
c Read the values that define good bits from disc, where written by
c NOISED.FOR (or, later, by SCAMP.FOR and SCINT also). Also get disc
c where original data (CONSAM) is located, and get sample rate etc from it.
12	print 19
19	format(' Specify disc that contains raw data (CONSAM):',/,
     & ' List names of existing CONSAM files [N] ? ')
	read 101,ans
      if(UC(ans).eq.'Y') then
	do 8 i=1,11		!1=C,2=D,3=E,4=F,5=G, 6=H, 7=I, 8=J, 9=K, 10=L, 11=M
	  ndev1=char(i+66)//':'
	  CONDAT=ndev1//'\CONSAM.DAT'
	  INQUIRE(file=CONDAT,exist=present)
	  if(.not.present) goto 8
        OPEN(unit=14,file=CONDAT,status='UNKNOWN',access='TRANSPARENT',
     &   err=8)
	  read(14,rec=1) title,cdate1,adctime	!don't overwrite today's CDATE
	  CLOSE(unit=14)
	  print 10,ndev1,cdate1,adctime,title
10	  format(' Disk ',a2,' Date= ',a11,' time= ',a8,/,1x,a76)
8	  continue
c	  pause
	endif
c Which disk to take CONSAM from?
c	if(ndev1.eq.'  ') ndev1='D:'		!default winchester
	call DISCNUM(id,ndev1,0,0)	!ask for winchester (only)- no default
	if(id.eq.-1) goto 999		!ESC was hit
	CONDAT=ndev1//'\CONSAM.DAT'
c
c Open data file, and read header
      OPEN(unit=14,file=CONDAT,status='UNKNOWN',access='TRANSPARENT')
	read(14,rec=1) title,cdate,adctime,idt,ioff,ilen,inc,
     & id1,id2,cs(1:3),calfac,srate,filt,filt1,calfac1
	CLOSE(unit=14)
	nbyte=ilen		!NB ilen is integer*4
	ndata=nbyte/2	!total number of int*2 values in record
	nchan=int4(inc)	!for use below
	nint=ndata/nchan	!number of values per channel
c
c	if(cs(2:2).eq.'C') then
c	   clock=1.0d6      !clock rate
c	else if(cs(2:2).eq.'H') then
c	   clock=4.0d6      !clock rate
c	endif
c	div=dble(float(id1)*float(id2))
c	rate=clock/div	!sample rate (Hz)
	rate=srate		!actual sample rate
c
	print 100,ndev1,cdate,adctime,title
100	format(' Disk ',a2,' Date= ',a11,' time= ',a8,/,1x,a76)
	print 11
11	format('  O.K. [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') goto 12
	titdat=title		!for transmission to PSWRT
      print 13,ndev1,cdate,adctime,title,nchan,rate,nint,calfac,filt
      if(pon()) write(7,13) ndev1,cdate,adctime,title,nchan,rate,
     & nint,calfac,filt
      if(discprt) write(8,13) ndev1,cdate,adctime,title,nchan,rate,
     & nint,calfac,filt
13	format(' Data from disk ',a2,' Date= ',a11,' time= ',a8,/,
     & 1x,a76,/,1x,i3,' channels sampled, at ',g13.6,' Hz',/,
     & 1x,i12,' integers (per channel)',/,
     & ' calibration = ',g13.6,' pA/ADC;  filter (Hz) = ',g13.6)
	if(nchan.eq.2) then
         print 14,calfac1,filt1
         if(pon()) write(7,14) calfac1,filt1
         if(discprt) write(8,14) calfac1,filt1
14	   format(' For ADC1:',/,
     & ' calibration = ',g13.6,' pA/ADC;  filter (Hz) = ',g13.6)
	endif
cc
c	print 36,idt,ioff,ilen,ilen/(2*inc),inc,id1,id2,rate,
c     & cs(2:2),cs(3:3)
c36	format(' Data type (0=cont; 1=intermittent) = ',i8,/,
c     & ' Offset (bytes) to start of data = ',i8,/,
c     & ' Length (bytes)',i8,' = ',i8,' values on each channel',/,
c     & ' Number of channels = ',i8,/,
c     & ' Rate divisors = ',2i8,'  Sample rate (Hz)= ',g14.7,/,
c     & ' Clock rate (C=1 MHz, H=4MHz) = ',a1,/,
c     & ' Trigger control (T= sample triggered from ev4) = ',a1)
c
451	print 45
45	format(/,' Specify disc that contains result of editing:')
c	ndev='D:'		!default = same disc as for CONSAM
	ndev=ndev1		!default = same disc as for CONSAM
	call DISCNUM(id,ndev,0,1)
	if(id.eq.-1) goto 999		!ESC was hit
      OPEN(unit=15,file=ndev//'NOISED.DAT',status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=4096)
c NB recl is in bytes (4*value for PDP) 2*512(int*4)*4=4096
	read(15,rec=1) istart,iend
      close(unit=15)
c Find ngood:
	do 20 i=1,512
	 i1=i
	 if(istart(i).eq.-1) goto 21		!skip out
20	continue
21	ngood=i1-1
	if(debug()) then
	   do 22 i=1,ngood
	   print 23,i,istart(i),iend(i)
23	   format(i5,2i16)
22	   continue
	endif
c
c RATE=effective sample rate (Hz); FILT=effective filter
c setting (as on box if damped) (Hz); CALFAC in pA/ADC unit
c NB 'volts in computer' means ADC*5./32768.
c	calfac=pAV/6553.6	!=[pA/V]/[ADC/V] = pA per ADC unit
	if(calfac.le.0.) goto 104
	if(nchan.ne.1) goto 17
	print 103,calfac*6553.6
103	FORMAT(
     & ' Calibration factor (pA/volt input to CED)= ',g12.5,/,
     & ' O.K. [Y] ? ')
	read 101,ans
	if(UC(ans).ne.'N') goto 102
	goto 104
17	if(nchan.eq.2) print 1031,calfac*6553.6,calfac1*6553.6
1031	FORMAT(
     & ' Calib factors for ADC0,ADC1 (pA/volt input to CED)= ',2g12.5,
     & /,' O.K. [Y] ? ')
	read 101,ans
	if(UC(ans).ne.'N') goto 102
104	if(nchan.ne.1) goto 18
	print 105
105	FORMAT(' Calibration factor (pA per volt input to CED)= ')
	call INPUTr(cal)
c	read 106,cal
106	format(g13.6)
	goto 1052
18	if(nchan.eq.2) print 1051
1051	FORMAT(
     & ' Calib factors for ADC0,ADC1 (pA per volt input to CED)= ')
	call INPUT2r(cal,cal1)
c	read 1061,cal,cal1
1061	format(2g13.6)
	calfac1=cal1/6553.6		!pA per ADC unit
1052	calfac=cal/6553.6			!pA per ADC unit
c Write altered calfac back to disc
      OPEN(unit=14,file=CONDAT,status='UNKNOWN',access='TRANSPARENT')
	write(14,rec=1) title,cdate,adctime,idt,ioff,ilen,inc,
     & id1,id2,cs(1:3),calfac,srate,filt,filt1,calfac1
	CLOSE(unit=14)
102	continue		!jump here if calfac from disc O.K.
c	calfac2=(calfac*1.e-12)**2	!(Amps/ADC unit)**2
c
	if(nchan.eq.1) then
	   madc1=.false.		!take mean current from ADC0
	else if(nchan.eq.2) then
	   if(ibcor.eq.1) then
		print 112
112		format(' Take mean current from the ADC1 sample [Y] ? ')
	      read 101,ans
	      madc1=UC(ans).ne.'N'
	   else if(ibcor.eq.2) then
		print 113
113		format(
     &  '   (At present answer to next question is Y whatever you say)',
     &	/,' Mean current and fit from the ADC0 sample [Y] ? ')
	      read 101,ans
	      madc1=UC(ans).eq.'N'
c		Could also ask here if control current to be from asymptote
c		-ask later, at present, after each fitting
	   endif
	endif
	if(madc1) then
	   if(pon()) write(7,114)
	   if(discprt) write(8,114)
114	   format(' Mean current taken from sample on ADC1')
	else
	   if(pon()) write(7,115)
	   if(discprt) write(8,115)
115	   format(' Mean current taken from sample on ADC0')
	endif
c
99	continue
	RETURN
999	continue
	idest=999
	RETURN
	end


