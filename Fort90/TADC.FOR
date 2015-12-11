	program TADC
c To test reading of specified ADC channel on Amplicon board, via assembler
c subroutine READADC.ASM
c Modif to look for max jitter every ntest readings
	integer*2 ichan,iadc,iad(1000),iadclast
	logical kbhit
c
	ntest=1000
	calfac=4000./4096.	!mV/ADC unit
	print 1
1	format(' ichan = ')
	read *,ichan
	print 5
5	format(' number of samples [continuous] = ')
	read 8,nsamp
8	format(i8)
	call READADC(ichan,iadclast)
	i=0		!index for iad()
	if(nsamp.eq.0) then
3	   call READADC(ichan,iadc)
	   i=i+1
	   iad(i)=iadc-iadclast
	   iadclast=iadc
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
	   endif
c Amplicon board in Unipolar (0-4V) mode has 0 at +2V, and numbers output
c from READADC (after some struggle with sign bits etc) are -2048 to +2047
c for 0V to 4V
	   mv=float(iadc+2048)*calfac
	   print 2,iadc,mV
2	   format(' iadc, mV = ',i8,g13.6)
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
	   print 2,iadc,mV
	   time=float(n2-n1)*10.		!msec
	   print 7,time
7	   format(' time (ms) = ',f12.2)
	endif
99	continue
	end

