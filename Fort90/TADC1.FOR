	program TADC1
c To test reading of specified ADC channels on Amplicon board, via assembler
c subroutine READAMP.ASM
	integer*2 ichan(0:15),iadc(0:15),iadc1(0:15),n
	logical kbhit
c
	calfac=4000./4096.	!mV/ADC unit
c initialise
	do 11 i=0,15
	 ichan(i)=0
	 iadc(i)=0
11	continue
c
	print 1
1	format(' number of channels to be read = ')
	read *,n
	do 10 i=0,n-1
	print 5,i
5	format(1x,i3,': channel number = ')
	read 8,ichan(i)
8	format(i8)
10	continue
c
17	call READAMP(ichan,n,iadc)
	do 18 i=0,n-1
18	call READADC(ichan(i),iadc1(ichan(i)))
c
	do 12 i=0,15
	 print 13,i,ichan(i),iadc(i),iadc1(i)
13	 format(4i8)
12	continue
	if(.not.kbhit()) goto 17
	end

