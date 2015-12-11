	program TBEEP
c to test spindrift beep etc
	integer if(2,10)
c
2	call beep
	pause
	call BELL(3)	!test new version
	pause
	print 1
1	format(' ifreq1,ifreq2 = ')
	read *,ifreq1,ifreq2
	if(ifreq1.eq.0) STOP
	call tone(ifreq1,50)
	pause
	call tone2(ifreq1,ifreq2,50)
	pause
	n=1
	if(1,n)=500		!=freq
	if(2,n)=8		!=duration
	n=n+1
	if(1,n)=0		!=freq
	if(2,n)=3		!=duration
	n=n+1
	if(1,n)=500		!=freq
	if(2,n)=8		!=duration
	n=n+1
	if(1,n)=0		!=freq
	if(2,n)=3		!=duration
	n=n+1
	if(1,n)=500		!=freq
	if(2,n)=8		!=duration
	n=n+1
	if(1,n)=0		!=freq
	if(2,n)=3		!=duration
	n=n+1
	if(1,n)=410		!=freq
	if(2,n)=50		!=duration
c	n=5
	call TUNE(if,n)
	pause
4	print 3
3	format(' ifreq2 = ')
	read *,ifreq2
	ifreq1=440
	call tone2(ifreq1,ifreq2,50)
	goto 4
	end
