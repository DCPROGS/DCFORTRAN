	program iADCMEM
c	integer*2 labgo,Ld
c	integer*2 labgo
c	integer*2 ToHost,To1401
c	integer*2 ierr,input(20480)
 	integer*2 ierr,input(20000)
c	integer*2 ioutput(1000)		!for DAC output
	integer*2 iwork(2048)		!4096 bytes
c at present array in LD() command must have dimension 20000- can this
c be altered?
	integer*2 ioutput(20000)		!for DAC output
	integer*2 ibyte,ist,isz,ichan,irept,ipre,icount	!args for ADCMEM
	integer*2 istout,iszout,ipre1,icount1
	integer*2 istatus,istat(5000)
	common input,ioutput,iwork
c
	do 6 i=1,20000
6	input(i)=0
c
c
c Open 1401
c	ierr = LabGo()
	call ForOpen1401(ierr)
	IF (ierr.NE.0) THEN
	   PRINT *,'1401 open failed',ierr
	   CALL EXIT(2)
	ENDIF
c
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
c	ierr=To1401(ioutput,iszout4,istout4)
	call ForTo1401(ioutput,iszout4,istout4,2,ierr)
	print 62,ierr
62	format(' For transfer to 1401: ierr= ',i8)
c
	pause
c
c Load commands that are needed
c NB the array IWORK used in Ld() is temporary work space only- needs to be
c about 1.5*size of the largest command loaded.  No need to use separate
c array (iwork here)- could use say INPUT() array which can be set to 0 after
c Ld() call ready for its real use to hold sampled data.  This saves space.
c	ierr = Ld(' ','KILL,MEMDAC,ADCMEM',iwork,4096)	!4096 bytes
	call ForLd('g:\1401\','KILL,MEMDAC,ADCMEM',1,20,ies,ierr)	!4096 bytes
	IF (ierr.NE.0) THEN
	   PRINT *,'1401 command load failed',ierr
c	   CALL LabEnd
	call ForClose1401
	   CALL EXIT(2)
	ENDIF
c Set up MEMDAC command to wait for trigger (arguments are exactly same as
c for ADCMEM
	ibyte=2
c	istout=0	!defined above
c	iszout=2000
	ichanOUT=2		!DAC 2
	irept=1
	ipre1=100
	icount1=10  !1 kHz
	print 1003, istout,iszout,ichanout,irept,ipre1,icount1
1003	FORMAT (' MEMDAC,I,2,',i6,',',i6,',',i6,',',i6,',',
     & 'CT',',',i6,',',i6,';')
	pause
	call ForSendString('MEMDAC,I,2,0,2000,2,1,CT,100,10;',40,IERR)
	print*,'memdac err=',ierr
C
c Invoke ADCMEM command
c byte=2 (12 bit data); st=start address=0; sz=10000 (bytes=5000 data values-
c must be multiple of 4),chan=0 only; rpt=1; C=clock 1MHz, immediate start;
c pre,count=2,10 so rate=1MHz/20=50kHz
c QUERIES- which sent as numbers, which as strings?
c	-for 2 channels need '0 1', so send i6,' ',i6?  or just '0 1'
c	-why i6?
c	-why is input declared 20000, but 10000 in call?
	ibyte=2
c	ist=0
	ist=2000	!DAC data is in bytes 0-1998
	isz=10000
	ichan=0
	irept=1
c	ipre=2
	ipre=10	!10 kHz
	icount=10
	print 1
1	format(' Trigger ev3 and ev4 to start DAC and ADC resp')
c	call ANYKEY
c
c NB writing IBYTE as an integer inserts too many spaces in front of the '2'
c (as can be seen by dumping 200g in DEBUGGER), and command does not work.
c If IBYTE is written as character, as in 1002, works fine.
c Use 4Mhz clock, triggered from ev4 ='HT'
	print 1002, ist,isz,ichan,irept,ipre,icount
1002	FORMAT (' ADCMEM,I,2,',i6,',',i6,',',i6,',',i6,',',
     & 'CT',',',i6,',',i6,';')
	pause
	call ForSendString('ADCMEM,2000,10000,0,1,CT,10,10',40,IERR)
	print*,'adcmem err=',ierr
	pause
	call FLUSH(10)
c	print 50,1
50	format(' reached ',i4)
c Query -how to get error status (e0,e1) after this call
	j=0
21	continue
c	pause '1'
C	write(10,1001)
C1001	FORMAT ('ADCMEM,?;')
c	pause
	CALL FORSENDSTRING('ADCMEM,?;',15,IERR)
	print*,'adcmem? err=',ierr
	pause
c	pause '2'
c	print*, 'enter status='
c	read*,istatus
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
	isz4=int4(isz)
	ist4=int4(ist)
	print 50,3
c	ierr=ToHost(input,isz4,ist4)
	call ForToHost(input,isz4,ist4,2,ierr)
c
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
c
99	continue
c	CALL LabEnd
	call ForClose1401()
	end

