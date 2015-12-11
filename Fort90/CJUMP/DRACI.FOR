	program CADCMEM

c	I use subroutine for debugging purposes insteed of functions
c	integer*2 labgo,Ld
c	integer*2 labgo
c	integer*2 ToHost,To1401

	integer*2 ierr,input(20000)
	integer*2 iwork(2048)		      !4096 bytes
	integer*2 ioutput(20000)		!for DAC output
	integer*2 ibyte,ist,isz,ichan,irept,ipre,icount	!args for ADCMEM
	integer*2 istout,iszout,ipre1,icount1
	integer*2 istatus,istat(5000)
	character*60		str1
	character*60		fmt
	common input,ioutput,iwork

	do i=1,20000
		input(i)=0
	end do

c-----------------------------------------
c	ierr = LabGo()
	call ForOpen1401(ierr)
c-----------------------------------------

	IF (ierr.NE.0) THEN
	   	PRINT *,'1401 open failed',ierr
	   	CALL EXIT(2)
	ENDIF

c Set up array for output voltage; as test say we want 50ms pulse to +5V
c at moment of trigger, then 0V thereafter. Use clock rate of 1 kHz and
c set 1st 50 points= 32752 = +4.9976V
	iszout=2000		!bytes
	istout=0
	iszout4=int4(iszout)
	istout4=int4(istout)
	do 61 i=1,1000
	ioutput(i)=0
	if(i.gt.10.and.i.le.20) ioutput(i)=32752
	if(i.gt.60.and.i.le.70) ioutput(i)=32752
61	continue

c Download IOUTPUT to 1401 in memory pos=0-999

c------------------------------------------------------
c	ierr=To1401(ioutput,iszout4,istout4)
	call ForTo1401(ioutput,iszout4,istout4,2,ierr)
c------------------------------------------------------

	print 62,ierr
62	format(' For transfer to 1401: ierr= ',i8)
	call ForToHost(input,isz4,ist4,2,ierr)
	print 63,ierr
63	format(' For transfer to Host: ierr= ',i8)
	do i=1,5
		print*,ioutput(i),input(i)
	end do
	pause

c Load commands that are needed
c NB the array IWORK used in Ld() is temporary work space only- needs to be
c about 1.5*size of the largest command loaded.  No need to use separate
c array (iwork here)- could use say INPUT() array which can be set to 0 after
c Ld() call ready for its real use to hold sampled data.  This saves space.

c-----------------------------------------------------------------------
c	ierr = Ld(' ','KILL,MEMDAC,ADCMEM',iwork,4096)	!4096 bytes
	call ForLd('c:\1401\','KILL,MEMDAC,ADCMEM',1,20,ies,ierr)	!4096 bytes
c--------------------------------------------------------------------------

	IF (ierr.NE.0) THEN
	   	PRINT *,'1401 command load failed',ierr

c---------------------------------------------------------
c	   	CALL LabEnd
		call ForClose1401
c----------------------------------------------------------
	   	CALL EXIT(2)
	ENDIF

c Set up MEMDAC command to wait for trigger (arguments are exactly same as
c for ADCMEM
	ibyte=2
	ichanOUT=2		!DAC 2
c	irept=1
c	ipre1=100
c	icount1=10  	!1 kHz
	irept=100
	ipre1=3
	icount1=5  	!1 kHz

c--------------------------------------------------------------
	fmt='(a10,a1,i6,a1,i6,a1,i6,a1,i6,a1,a2,a1,i6,a1,i6,a1)'
	write(str1,fmt) 'MEMDAC,I,2',',',istout,',',iszout,',',ichanout,
     &',',irept,',','CT',',',ipre1,',',icount1,';'
	print*,str1
	call ForSendString(str1,40,IERR)
c---------------------------------------------------------------

	print*,'memdac err=',ierr
	a=getch(b)
C
c Invoke ADCMEM command
c byte=2 (12 bit data); st=start address=0; sz=10000 (bytes=5000 data values-
c must be multiple of 4),chan=0 only; rpt=1; C=clock 1MHz, immediate start;
c pre,count=2,10 so rate=1MHz/20=50kHz
c QUERIES- which sent as numbers, which as strings?
c	-for 2 channels need '0 1', so send i6,' ',i6?  or just '0 1'
c	-why i6?
c	-why is input declared 20000, but 10000 in call?
c	ist=0
	ibyte=2
	ist=2000	!DAC data is in bytes 0-1998
	isz=10000
	ichan=0
	irept=1
	ipre=10	!10 kHz
	icount=10
	print 1
1	format(' Trigger ev3 and ev4 to start DAC and ADC resp')

c NB writing IBYTE as an integer inserts too many spaces in front of the '2'
c (as can be seen by dumping 200g in DEBUGGER), and command does not work.
c If IBYTE is written as character, as in 1002, works fine.
c Use 4Mhz clock, triggered from ev4 ='HT'

c---------------------------------------------------------------------------
	fmt='(a10,a1,i6,a1,i6,a1,i6,a1,i6,a1,a2,a1,i6,a1,i6,a1)'
	write(str1,fmt) 'ADCMEM,I,2',',',ist,',',isz,',',ichan,
     &',',irept,',','CT',',',ipre,',',icount,';'
	print*,str1
	call ForSendString(str1,40,IERR)
c--------------------------------------------------------------------------

	print*,'adcmem err=',ierr
	a=getch(b)

	call FLUSH(10)


c50	format(' reached ',i4)
c Query -how to get error status (e0,e1) after this call
c	j=0
c21	continue
c	pause '1'
C	write(10,1001)
C1001	FORMAT ('ADCMEM,?;')

	CALL FORSENDSTRING('ADCMEM,?;',15,IERR)
	print*,'adcmem? err=',ierr
	a=getch(b)


c	pause '2'
c	print*, 'enter status='
c	read*,istatus
c	pause
c	print 2,istatus
c2	format(' status = 'i4)
c	j=j+1
c	istat(j)=istatus
c	if(istatus.ne.0.and.istatus.ne.-1) goto 21
c
c	print 9,j
c9	format(' status checked ',i5,' times')
c	do 7 i=1,j,10
c7	print 8,(istat(m),m=i,i+9)
8	format(10i6)
c	pause
c Transfer data to host
c	INTEGER*2 FUNCTION ToHost(object,size,addr1401)
c	print 50,3

	isz4=int4(isz)
	ist4=int4(ist)

c-------------------------------------------------------------
c	ierr=ToHost(input,isz4,ist4)
	call ForToHost(input,isz4,ist4,2,ierr)
c-------------------------------------------------------------

32	print 3,ierr
3	format(' For transfer to host ierr= ',i8,/,
     & ' print input(i1) to input(i2) [0 to end]; i1,i2 = ')
	input 31,i1,i2
31	format(2i8)
	if(i1.le.0) goto 99
	n=i2-i1+1
	do 4 i=1,n
	print 5,i,input(i)
5	format(1x,2i8)
4	continue
	goto 32
99	continue

c---------------------------------------
c	CALL LabEnd
	call ForClose1401()
c---------------------------------------
	end

