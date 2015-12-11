c=======================================================================
	program phili
c=======================================================================
c	Program to automize solenoids sequence using 1401;
c
c=======================================================================


	character   char,getch,ans
	integer*4  	x_valve(30),y_valve(30),box_color
      integer*4   v_length,v_width,v_heigth,v_color
	integer*4  	x_button(10),y_button(10)
      integer*4   b_length,b_width,b_heigth,b_color
	integer*4  	xc(30),yc(30),c_border,c_color,c_seq,c_on
      integer*4	valve(30),xcm(5),ycm(5),count(30)
	integer*4   lval(0:3)

	character*13	getint
	character*60      helpst(20)
	character*72	str1
	logical mouse_on,mous_set,outext,kbhit
	common/mousval/mouse_on,nbutton

	mouse_on=.true.
	ibk=1
	box_color=3

	n_valve=25
	v_color=11

	n_button=3
	b_color=15

	c_border=7
	c_color=8
	c_seq=14
	c_on=10
	rmask=0
	n_pin=5
	do i=1,n_pin
		rmask=rmask+2**(i-1)
	enddo
	mask=int(rmask)
	call mode(18)
	call clears(0,480,ibk)
1     continue

	call chooseq(x_valve,y_valve,n_valve,v_length,v_width,v_heigth,
     &     x_button,y_button,n_button,b_length,b_heigth,b_width,xc,yc,
     &     irc,xcm,ycm,box_color,v_color,b_color,c_color,c_border,c_seq,
     &     c_on,valve,n_seq,count,ixw,iyw)
2	continue
	goto 77
88	call ForOpen1401(ierr)
	irhi=1
	iclo=1
	nrow=1
	ncol=78
	ibkd=0
	icb1=1
	icqd=13
      call DEFDIALOG(1,irhi,iclo,nrow,ncol,ibkd)
	ibox=1
	call opendialog(1,icb1,.true.)
	if(ierr.ne.0) then
	   call wdialog(ibox,'1401 OPEN FAILED : ERR = '//
     &   getint(ierr,nm)(1:nm),12)
	   select case (ierr)
	      case (-500)
		   call wdialog(ibox,'1401 SWITCHED OFF ; '//
     &	   'PLEASE SWITCH ON and PRESS ANYKEY',12)
		   ans=getch(b)
		   ind5=ind5+1
		   if(ind5.ge.3) goto 9888
		   goto 88
		case (-501)
		   call wdialog(ibox,'1401 NOT CONNECTED: CHECK AND RESTART'
     &         ,12)
		case (-502)
		   call wdialog(ibox,'1401 ILL ( CALL THE DOCTOR ? ): '//
     &	   'SWITCH OFF/ON 1401 AND REBOOT ',12)
		case (-503)
		   call wdialog(ibox,'I/F CARD MISSING: CHECK AND RESTART',
     &	   12)
		case (-505)
		   call wdialog(ibox,'I/F CARD BAD SWITCHES: '//
     &	   'CHECK AND RESTART ',12)
		case (-506)
		   call wdialog(ibox,'1401+ FAILED TO COME READY : '//
     & 	   'SWITCH OFF/ON 1401 AND REBOOT',12)
		case (-508)
		   call wdialog(ibox,'1401 IS ALREADY IN USE : PLEASE WAIT',
     &         12)
		   call ForClose1401()
		   indwait=indwait+1
		   if(indwait.gt.15) then
	   		call wdialog(ibox,'SORRY , EXIT PROGRAM !',12)
			call EXIT(2)
		   endif
		   goto 88
		case (-509)
		   call wdialog(ibox,'COULD NOT GET DMA CHANNELS : '//
     &	   'SWITCH OFF/ON 1401 AND REBOOT',12)
		case(-580)
		call wdialog(ibox,'NOT 386 ENHANCED MODE: CHECK AND RESTART'
     &	,12)
		case(-581)
		   call wdialog(ibox,'NO DEVICE DRIVER : CHECK AND RESTART',
     &         12)
		case(-582)
		   call wdialog(ibox,'DEVICE DRIVER TOO OLD : NO COMMENTS !'
     &         ,12)
	   end select
9888	   call wdialog(ibox,'SORRY , EXIT PROGRAM !',12)
	   call EXIT(2)
	else
	   call wdialog(ibox,'1401-PLUS OPENED SUCCESFULLY',10)
	endif


888	call ForLd('c:\1401\','DIGTIM,MEMDAC,ADCMEM,TIMER2',ies,ierr)
	if(ierr.ne.0) then
	   call wdialog(ibox,'1401 COMMAND LOAD FAILED'//
     &   getint(ierr,nm)(1:nm),12)
		if(ierr.eq.(-540)) call wdialog(ibox,
     &	   'COMMAND FILE NOT FOUND',12)
		if(ierr.eq.(-590)) then
		   call wdialog(ibox,'TIMEOUT OCCURED ! WAIT : TRYING AGAIN'
     &         ,12)
		   ind=ind+1
		   if(ind.ge.3) goto 8888
		   goto 888
		endif
		call wdialog(ibox,'ADVISE: SWICH OFF/ON 1401 AND REBOOT',12)
8888	   	continue
	      call wdialog(ibox,'SORRY , EXIT PROGRAM !',12)
		call ForClose1401()
	   	call EXIT(2)
	else
	   	call wdialog(ibox,'1401 COMMAND LOAD OK!',10)
	endif

	isol=1
	isz=(n_seq+1)*16
	str1='DIGTIM,SI,2048,'//getint(isz,nm)(1:nm)//';'
	call ForSendString(str1,ierr)
	call select_state(valve(isol),mask,count(isol),str1)
	call ForSendString(str1,ierr)
	do isol=2,n_seq
	    call select_state(valve(isol),mask,count(isol),str1)
	    call ForSendString(str1,ierr)
	enddo
	call ForSendString('DIGTIM,OD;',IERR)
	call ForSendString('DIGTIM,C,100,1000;',IERR)
	islice=1
33    continue
	call ForSendString('DIGTIM,?;',IERR)
	call ForLongsFrom1401(lval,2,ierr)
	isol=lval(3)
      if(isol.eq.islice) goto 33
	call butonoff(x_valve(valve(isol)),y_valve(valve(isol)),
     &    v_length,v_width,v_heigth,v_color,
     &    xc(valve(isol)),yc(valve(isol)),irc,b_color,c_on,c_seq,
     &    box_color,c_color,valve(isol),
     &    x_valve(valve(isol-1)),y_valve(valve(isol-1)),
     &    xc(valve(isol-1)),
     &    yc(valve(isol-1)),valve(isol-1),ixw,iyw,n_valve+1)
	islice=isol
	if(isol.le.n_seq) goto 33
c=================================================================
77    continue
	isol=1
	call select_state(valve(isol),mask,count(isol),str1)
	call wrstring18(320,440,str1,13,8)
	call timer(it1)
	do isol=2,n_seq
55	call timer(it2)
	idt=it2-it1
	if(idt.lt.count(isol-1)) goto 55
	it1=it2
	call wrstring18(320,440,str1,8,13)
	call select_state(valve(isol),mask,count(isol),str1)
	call wrstring18(320,440,str1,13,8)
	call butonoff(x_valve(valve(isol)),y_valve(valve(isol)),
     &    v_length,v_width,v_heigth,v_color,
     &    xc(valve(isol)),yc(valve(isol)),irc,c_border,c_on,c_seq,
     &    box_color,c_color,valve(isol),
     &    x_valve(valve(isol-1)),y_valve(valve(isol-1)),
     &    xc(valve(isol-1)),
     &    yc(valve(isol-1)),valve(isol-1),ixw,iyw,n_valve+1)
	enddo
c	char=getch(ktype)
c===================================================================
56	call timer(it2)
	idt=it2-it1
	if(idt.lt.count(n_seq)) goto 56
	it1=it2
	valve(isol)=n_valve+1
	call butonoff(x_valve(valve(isol)),y_valve(valve(isol)),
     &    v_length,v_width,v_heigth,v_color,
     &    xc(valve(isol)),yc(valve(isol)),irc,c_border,c_on,c_seq,
     &    box_color,c_color,valve(isol),
     &    x_valve(valve(isol-1)),y_valve(valve(isol-1)),
     &    xc(valve(isol-1)),
     &    yc(valve(isol-1)),valve(isol-1),ixw,iyw,n_valve+1)


	call fillcircle(xcm(1),ycm(1),irc,c_border,c_color)
	call draw_valve(x_button(1),y_button(1),b_length,b_heigth,b_width
     &,b_color,v_color,c_color)
	call wrstring18(x_button(1)+int(b_length/2)-16,y_button(1)+
     &int(b_heigth/2)-16,
     &'On/Off',c_color,b_color)
	call wrstring18(x_button(1)+int(b_length/2)-16,y_button(1)+
     &int(b_heigth/2),'Timer',c_color,b_color)
	call wrstring18(320,440,str1,8,13)

	char=getch(ktype)

c	call clrdialog(1,0)
c	call enddialog(1,0)
c	CALL ForClose1401()

	end

