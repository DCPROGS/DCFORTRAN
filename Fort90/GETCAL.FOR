	subroutine GETCAL(calfac,amVpA,amVpA1,VpA,VpA1,pAV,pAV1,
     & itape,ftape,gain,errfac)
c To read in calibration factor
c Scaling: 1401 units are -32768 t0 +32752 in 16 unit increments; dividing
c by 16 gives range as -2048 to +2047 (as for CED502) =-5.0V to +4.9976V
c So 32768/5=6553.6 ADC units/volt
c Modified 05/14/91 12:20pm so that if called with ftape negative then
c the initial question is skipped and separate clamp gain etc assumed (and
c sign of FTAPE rectified)
	logical discprt,pon,slock
	character*1 ans,UC
	common/dp/discprt
c
	pon()=slock()
c
101	format(a1)
	if(ftape.lt.0.) then
	   ftape=abs(ftape)
	   print 1071
1071	   format(' Set the calibration:',/)
	   goto 108
	endif
	print 107
107	format(' Set the calibration:',/,
     & ' (1) Give separate clamp gain etc',/,
     & ' (2) Give overall calibration factor',/,
     & ' (3) Do not specify calibration here (CALFAC=-1.)',/,
     & ' Option number [1] = ')
	read 2,i
2	format(i8)
	if(i.le.0.or.i.gt.3) i=1
	if(i.eq.3) then
	   calfac=-1.
	   goto 99
	endif
	if(i.eq.1) goto 108
c	print 105,calfac
c105	FORMAT(
c     & '&Calibration factor (pA per volt in computer) [',g13.6,'] = ')
	print 45,amVpA/1000.
45	format(
     & ' Calibration: Volts per pA (at 1401 input) [',f10.1,'] = ')
	call INPUTr(x)
c	read 106,x
c106	format(g13.6)
c	if(x.ne.0.) amVpA=x
c 	VpA=amVpA/1000.		!Volts (at 1401) per pA
	if(x.ne.0.) VpA=x
 	amVpA=VpA*1000.		!mV (at 1401) per pA
	pAV=1.0/VpA			!pA per volt
	calfac=pAV/6553.6	!=[pA/V]/[ADC/V] = pA per ADC unit
	goto 109
c
108	continue
	i=ifixr(amVpA1)
	print 110,i		!mV from clamp (not at 1401 input)
110	format('&Clamp setting (mV/pA) [',i5,'] = ')
	call INPUTr(amVpA1)
c	itape=-1		!not taped  use input
	if(itape.eq.1) then	!itape=0 if not taped ;=1 if taped
	   print 1101
1101	   format('&Sampled from tape recorder [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') itape=0
	else
	   print 1102
1102	   format('&Sampled from tape recorder [N] ? ')
	   read 101,ans
	   if(UC(ans).eq.'Y') itape=1
	endif
	if(itape.eq.0) then
	   ftape=1.0
	else
	   i=ifixr(ftape)
	   print 111,i
111	   FORMAT('&Tape recorder (V peak) [',i4,'] = ')
	   call INPUTr(ftape)
	endif
	i=ifixr(gain)
	print 112,i
112	FORMAT('&Amplifier gain [',i4,'] = ')
	call INPUTr(gain)
	print 113,errfac
113	FORMAT('&Error factor (eg from CALADC: nominally 1.0)= ',/,
     & '   [Volts(in computer)/Volts(input)] [',f7.1,'] = ')
	call INPUTr(x)
	if(x.ne.0.) errfac=x
	VPa1=amVpA1/1000.		!V (from clamp)/pA
	pAV1=1.0/VpA1		!pA per V (from clamp)
	pAV1=pAV1*ftape	!correct for tape
	pAV1=pAV1/gain	!correct for amplifier
c pAV1 is now pA per Volt(input). Correct for errors in ADC and
c other non-ideality by dividing by error factor i.e.
c [Volts(in computer)/Volts(input)]
	pAV=pAV1/errfac	!pA per Volt in computer
c NB 'volts in computer' means ADC*5./32768.
	calfac=pAV/6553.6	!=[pA/V]/[ADC/V] = pA per ADC unit
c
	print 114,amVpA1,ftape,gain
	if(pon()) write(7,114) amVpA1,ftape,gain
      if(discprt) write(8,114) amVpA1,ftape,gain
114	format(' Clamp setting (mV/pA)= ',g13.6,/,
     & '  tape (V peak) = ',g13.6,/,
     & '  amplifier gain= ',g13.6)
109	continue
	print 115,pAV,calfac
	if(pon()) write(7,115) pAV,calfac
      if(discprt) write(8,115) pAV,calfac
115	format(
     & ' Calibration factor (pA per Volt in computer)= ',g13.6,/,
     & ' Calibration factor (pA per ADC unit        )= ',g13.6,/)
c
99	continue
	RETURN
	end

