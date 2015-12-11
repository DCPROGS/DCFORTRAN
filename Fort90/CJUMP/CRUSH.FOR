
	program crush

 	integer*2 ierr,iADC(20480),idac(400)
	integer*2 istatus,ktype
	integer*4 lval(0:3)
	character*1  buff(0:20),e0,e1,ch,getch
	character*11 clk,ctime
	logical kbhit

c Open 1401
88	call ForOpen1401(ierr)
	if(ierr.ne.0) then
	   	print*,'1401 open failed',ierr
		select case (ierr)
			case (-500)
				print*,'1401 SWITCHED OFF'
				print*,'PLEASE SWITCH ON and PRESS ANYKEY'
				ch=getch(ktype)
				goto 88
			case (-501)
				print*,'1401 NOT CONNECTED'
			case (-502)
				print*,'1401 ILL'
			case (-503)
				print*,'I/F CARD MISSING'
			case (-505)
				print*,'I/F CARD BAD SWITCHES'
			case (-506)
				print*,'1401+ FAILED TO COME READY'
			case (-508)
				print*,'1401 IS ALREADY IN USE : PLEASE WAIT'
				call ForClose1401()
				goto 88
			case (-509)
				print*,'COULD NOT GET DMA CHANNELS'
			case(-580)
				print*,'NOT 386 ENHANCED MODE'
			case(-581)
				print*,'NO DEVICE DRIVER'
			case(-582)
				print*,'DEVICE DRIVER TOO OLD'
		end select

	   	call EXIT(2)
	else
	  	print*,'1401-plus opened succesfully'
	endif

888	call ForLd('c:\1401\','KILL,MEMDAC,ADCMEM,TIMER2',ies,ierr)
	if(ierr.ne.0) then
	   	print*,'1401 command load failed',ierr
		if(ierr.eq.(-540)) print*,'COMMAND FILE NOT FOUND'
		if(ierr.eq.(-590)) then
			print*,'TIMEOUT OCCURED; WAIT : WILL TRY AGAIN;'
			ind=ind+1
			if(ind.ge.3) goto 8888
			goto 888
		endif
8888	   	call ForClose1401()
	   	call EXIT(2)
	else
	   	print*,'1401 command LOAD OK!',ierr
	endif

c Set size of ADC data array



	call ForTo1401(iDAC,iszout,istout,2,ierr)
	call	ForSendString('ERR;',ierr1)
	call ForGetString(buff,10,ierr1)
	if(ierr.ne.0) then
		if(ierr.eq.-560) print*,'TO1401 FAILED'
		if(ierr.eq.-590) print*,'TIMEOUT OCCURED'
	   	call ForClose1401()
	   	call EXIT(2)
	else
		print*,'ForTo1401 OK!'
	endif

	call ForSendString('EVENT,D,24;',IERR)!disable external events 3,4
	call ForSendString('EVENT,M,128;',IERR)!so responds to internal pulse 'EVENT,I'
	call	ForSendString('ERR;',ierr)
	call ForGetString(buff,10,ierr)
	pause
	isweep=1
	call DATE1(cdate)		!DC subroutine
	ctime=clk()
	print 2225,cdate,ctime
2225	format(' Date of analysis: ',a11,/,' Time of analysis: ',a11)

42    continue
	print*,'TIMER STARTED'
	print*,'isweep=',isweep
	call ForSendString('TIMER2,C,0,100,10,12000;',IERR)
	print*,'TIMER2,C,0,100,10,12000;',IERR
	call ForSendString('MEMDAC,I,2,0,24,2,1,HT,100,40;',IERR)
	print*,'MEMDAC,I,2,0,24,2,1,HT,100,40;',IERR
	call ForSendString('ERR;',ierr)
	call ForGetString(buff,15,ierr)
	e0=buff(0)
	e1=buff(2)

	if(e0.ne.'0'.or.e1.ne.'0') then
		print 72,buff(0),buff(1),buff(2),buff(3),buff(4),buff(5),
     &               buff(6)
72		format( ' After MEMDAC: e0,e1 = ',7a1)
	endif

	call ForSendString('ADCMEM,I,2,26,40000,0,1,HT,2,250;',IERR)
	print*,'ADCMEM,I,2,26,40000,0,1,HT,2,250;',IERR
	call ForSendString('ERR;',ierr)
	call ForGetString(buff,15,ierr)
	e0=buff(0)
	e1=buff(2)

	if(e0.ne.'0'.or.e1.ne.'0') then
		print 73,buff(0),buff(1),buff(2),buff(3),buff(4),buff(5),
     &               buff(6)
73		format( ' After ADCMEM: e0,e1 = ',7a1)
	endif

	call ForSendString('EVENT,I,24;',IERR)
	call ForSendString('ERR;',ierr)
	call ForGetString(buff,15,ierr)

21	continue
	call ForSendString('ADCMEM,?;',IERR)
	call ForLongsFrom1401(lval,2,ierr)
	istatus=lval(0)
	do k=1,10000		!insert slight pause to make sure istatus read
	   	s=123456./float(k)
	end do
	if(istatus.ne.0.and.istatus.ne.-1) goto 21

	if(istatus.eq.-1) then
	   	call BELL(3)
	   	print 92
92	   	format(' SAMPLES MISSED -SAMPLING TOO FAST')
	   	pause
	endif

	CALL ForToHost(IADC,ISZ,IST,2,IERR)
	call ForSendString('ERR;',ierr1)
	call ForGetString(buff,15,ierr1)
	e0=buff(0)
	e1=buff(2)

	if(e0.ne.'0'.or.e1.ne.'0') then
		print 78,buff(0),buff(1),buff(2),buff(3),buff(4),buff(5),
     &               buff(6)
78		format( ' After ToHost: e0,e1 = ',7a1)
	endif


	if(ierr.ne.0.) then
		if(ierr.eq.-560) print*,'TOHost FAILED'
		if(ierr.eq.-590) print*,'TIMEOUT OCCURED'
	else
		print*,'ForToHost OK!'
	endif

	i=0
121	continue
	i=i+1
	call ForSendString('TIMER2,R,0;',ierr)
	call ForLongsFrom1401(lval,2,ierr)
	iflag=lval(0)
	if(i.eq.1.and.iflag.eq.1) then
		call BELL(2)
		print 120
120		format(
     &  	' CYCLE TOO SHORT - time already elapsed at end of cycle')
	endif
	if(iflag.eq.0) goto 121		!wait until time up
	ctime=clk()
	print*,'TIME:',ctime
   	isweep=isweep+1 !Want to restart clock as soon as possible
 				    !after prev time period has elapsed, ie at label 42 or 4571
	if(kbhit()) then
		ch=getch(ktype)
		call clrkb()
		if (ktype.eq.16) goto 1111
	endif

	goto 42

1111	call ForClose1401()
	end



