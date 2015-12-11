	program TADC0
c To test reading of specified ADC channel on Amplicon board, via assembler
c subroutine READADC.ASM:  TADC0 is version that reads channel 0,1 alternately
c to see if this causes jitter
	integer*2 ichan,iadc,iadc0,iadc1,ichan0,ichan1
	integer*2 jchan(0:15),jadc(0:15),nc
	integer*2 iad(1000),iadclast
	logical kbhit,useadc
c
	ntest=1000
	calfac=4000./4096.	!mV/ADC unit
c For READAMP (to read channels 0,1)
	nc=2
	do i=0,15
	  jchan(i)=0
	enddo
	jchan(1)=1
c
c	print 1
c1	format(' ichan = ')
c	read *,ichan
	print 10
10	format(' Use (1) READADC or (2) READAMP: n = ')
	read *,i
	useadc=i.eq.1
	ichan0=0
	ichan1=1
	print 5
5	format(' number of samples [continuous] = ')
	read 8,nsamp
8	format(i8)
	if(nsamp.eq.0) then
	  if(useadc) then
	      call READADC(ichan0,iadclast)
	  else
	      call READAMP(jchan,nc,jadc)
	     iadclast=jadc(0)
	   endif
	   call READADC(ichan0,iadclast)
	   i=0		!index for iad()
3	   continue
	   if(useadc) then
	      call READADC(ichan0,iadc0)
	      call READADC(ichan1,iadc1)
	   else
	      call READAMP(jchan,nc,jadc)
		iadc0=jadc(0)
		iadc1=jadc(1)
	   endif
	   i=i+1
	   iad(i)=iadc0-iadclast
	   iadclast=iadc0
	   if(i.eq.ntest) then
		i=0	!reset
		imax=0
		do k=1,ntest
		  iadi=iabs(int4(iad(k)))
		  if(iadi.gt.imax) imax=iadi
		enddo
		print 9,ntest,imax
9		format(' Maximum jitter in last ',i4,' readings = ',i8)
		pause
		if(useadc) then
	         call READADC(ichan0,iadclast)
		else
	         call READAMP(jchan,nc,jadc)
		   iadclast=jadc(0)
		endif
	   endif
c Amplicon board in Unipolar (0-4V) mode has 0 at +2V, and numbers output
c from READADC (after some struggle with sign bits etc) are -2048 to +2047
c for 0V to 4V
	   mv0=float(iadc0+2048)*calfac
	   mv1=float(iadc1+2048)*calfac
c	   print 21,iadc0,mV0
	   print 2,iadc0,mV0,iadc1,mV1
2	   format(' iadc, mV = ',i8,g13.6,2x,i8,g13.6)
	   if(KBHIT()) goto 99
	   goto 3
	else
	   call TIMER(n1)
	   do 6 ic=1,nsamp
c	   ic=0
c61	   ic=ic+1
	   call READADC(ichan,iadc)
c	   if(ic.lt.nsamp) goto 61
6	   continue
	   call TIMER(n2)
	   mv=float(iadc+2048)*calfac
	   print 21,iadc,mV
21	   format(' iadc, mV = ',i8,g13.6)
	   time=float(n2-n1)*10.		!msec
	   print 7,time
7	   format(' time (ms) = ',f12.2)
	endif
99	continue
	end

