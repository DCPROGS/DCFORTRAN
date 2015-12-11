	program TADC3
c LOOP WORKS IN THIS VERSION
c To test reading of specified ADC channels on Amplicon board, via assembler
c subroutine READADC1.ASM
	integer*2 ichan,iadc
c
	print 1
1	format(' channel number = ')
	read *,ichan
	print 5
5	format(' number of samples = ')
	read 8,nsamp
8	format(i8)
c
	ic=0
61	ic=ic+1
	call READADC1(ichan,iadc)
	print 2,iadc
2	format(' iadc = ',i8)
	if(ic.lt.nsamp) goto 61
	end

