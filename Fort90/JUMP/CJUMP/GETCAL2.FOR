	subroutine GETCAL2(calfac,amVpA,amVpA1,VpA,VpA1,pAV,pAV1,
     & itape,ftape,gain,errfac,ibox,icwd,icqd)
c To read in calibration factor
c Scaling: 1401 units are -32768 t0 +32752 in 16 unit increments; dividing
c by 16 gives range as -2048 to +2047 (as for CED502) =-5.0V to +4.9976V
c So 32768/5=6553.6 ADC units/volt
c Modified 05/14/91 12:20pm so that if called with ftape negative then
c the initial question is skipped and separate clamp gain etc assumed (and
c sign of FTAPE rectified)
	logical discprt,pon,slock
	character*1 ans,UC
	character*30 def,cans
      character*13 getreal
	common/dp/discprt

	pon()=slock()
      call wdialog(ibox,'Set the calibration:',icwd)
	if(ftape.lt.0.) goto 108
	call qdialog(ibox,'Specify calibration here', ' [Y] = ',icqd,
     &cans)
	ans=cans(1:1)
	if(UC(ans).eq.'N') then
         call wdialog(ibox,' CALFAC = -1 ',icwd)
	   calfac=-1.
	   goto 99
	endif
	call qdialog(ibox,'Give overall calibration factor', ' [Y] = ',
     &icqd,cans)
	ans=cans(1:1)
108	if(UC(ans).eq.'N'.or.ftape.lt.0) then
	   if (ftape.lt.0) ftape=abs(ftape)
	   call qdialog(ibox,'Give separate clamp gain etc ',' [Y] = ',
     &   icqd,cans)
	   ans=cans(1:1)
	   if(UC(ans).eq.'N') then
	   	calfac=-1.
	   	goto 99
	   endif

	   call defoltr(amvpa1,def)
	   call qdialog(ibox,'Clamp setting (mV/pA) ',def,icqd,cans)
	   call getINPr(cans,amvpa1)
c	   amvpa1=float(i)

	   if(itape.eq.1) then	!itape=0 if not taped ;=1 if taped
		call qdialog(ibox,'Sampled from tape recorder',' [Y] = ',
     &	icqd,cans)
		ans=cans(1:1)
	   	if(UC(ans).eq.'N') itape=0
	   else
	   	call qdialog(ibox,'Sampled from tape recorder',' [N] = ',
     &	icqd,cans)
		ans=cans(1:1)
	   	if(UC(ans).eq.'Y') itape=1
	   endif
	   if(itape.eq.0) then
	   	ftape=1.0
	   else
	   	i=ifixr(ftape)
		call defolti(i,def)
   		call qdialog(ibox,'Tape recorder (V peak) ',def,icqd,cans)
	   	call getINPr(cans,ftape)
	   endif
	   call defoltr(gain,def)
	   call qdialog(ibox,'Amplifier gain ',def,icqd,cans)
	   call getINPr(cans,gain)

         call wdialog(ibox,'Volts(in computer)/Volts(input): ',icwd)
	   call defoltr(errfac,def)
	   call qdialog(ibox,
     &   'Error factor (eg from CALADC: nominally 1.0)',def,icqd,cans)
	   call getINPr(cans,x)

	   if(x.ne.0.) errfac=x
	   VPa1=amVpA1/1000.		!V (from clamp)/pA
	   pAV1=1.0/VpA1		!pA per V (from clamp)
	   pAV1=pAV1*ftape	!correct for tape
	   pAV1=pAV1/gain	!correct for amplifier
	   pAV=pAV1/errfac	!pA per Volt in computer
	   calfac=pAV/6553.6	!=[pA/V]/[ADC/V] = pA per ADC unit
	   call wdialog(ibox,'Clamp setting (mV/pA)= '//
     &   getreal(amvpa1,nm)(1:nm),icwd)
	   call wdialog(ibox,'tape (V peak) = '//getreal(ftape,nm)(1:nm)
     &   //'  amplifier gain = '//getreal(gain,nm)(1:nm),icwd)
	   if(pon()) write(7,114) amVpA1,ftape,gain
         if(discprt) write(8,114) amVpA1,ftape,gain
114	   format(' Clamp setting (mV/pA)= ',g13.6,/,
     & 	'  tape (V peak) = ',g13.6,/,
     & 	'  amplifier gain= ',g13.6)
	   goto 109
	endif
	call defoltr(amVpA/1000,def)
      call qdialog(ibox,'Calibration: Volts per pA (at 1401 input) ',
     &def,icqd,cans)
	call getINPr(cans,x)
	if(x.ne.0.) VpA=x
 	amVpA=VpA*1000.		!mV (at 1401) per pA
	pAV=1.0/VpA			!pA per volt
	calfac=pAV/6553.6	!=[pA/V]/[ADC/V] = pA per ADC unit
109	continue
	call wdialog(ibox,'Calibration factor (pA per Volt in computer)='
     &//getreal(pav,nm)(1:nm),icwd)
	call wdialog(ibox,'Calibration factor (pA per ADC unit)='
     &//getreal(calfac,nm)(1:nm),icwd)
	if(pon()) write(7,115) pAV,calfac
      if(discprt) write(8,115) pAV,calfac
115	format(
     & ' Calibration factor (pA per Volt in computer)= ',g13.6,/,
     & ' Calibration factor (pA per ADC unit        )= ',g13.6,/)
c
99	continue
	RETURN
	end

