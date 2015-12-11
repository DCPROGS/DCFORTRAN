	program phil

	character  a,getch
	integer*4  	x(30),y(30),xp(30),yp(30),msol(30)
	integer*4  	xc(30),yc(30),iym(30),iymo(30)

	character*11	cnum
	character*60      helpst(20)



	nhelp=1
	helpst(nhelp+1)='CHOOSE SEQUENCE:'
	helpst(nhelp+2)='Move cursor/mouse to select valve'
	helpst(nhelp+3)='Press <CR> or click L mouse button'
	helpst(nhelp+4)='Press <Delete> or click on "Cancel"'
	helpst(nhelp+5)=' to cancel sequence'
	helpst(nhelp+6)='Press <End> or click on "Start" to'
	helpst(nhelp+7)=' start program'
	helpst(nhelp+8)='Press <Esc> or click R mouse button'
	helpst(nhelp+9)=' to exit without starting'
	nhelp1=nhelp+9


	call mode(18)
	call clears(0,480,15)
	call draw_valve(200,200,100,70,40,12,4,8)
	call draw_valve(400,20,30,30,20,12,4,8)
	call draw_valve(20,20,24,29,4,12,4,8)
	a=getch(b)



	call fillwin(400,20,440,60,15)
	call zbar(405,25,30,30,10,4,12)

	call wrstring18(100,400,helpst(2),15,15)
	a=getch(b)
	call wrstring18(100,400,helpst(2),1,15)
	a=getch(b)
	call wrstring18(100,400,helpst(2),15,1)

	print*,mod(31,16)
	print*,mod(25,16)
	print*,mod(16,16)
	print*,mod(15,16)
	call system('type e:\cjump.prt|more')
	a=getch(b)
	call clears(0,480,1)

c	call solbox(ixlo,iylo,ixhi,iyhi,icborder,icbox,nvalves,
c     &nrows,icvalve,icshad,x,y,xp,yp,lv,ihv,iwv,xc,yc,irc,ic,icfill,
c     &nbut,ixm,iym,ixmo,iymo)
	nbut=3
	call solbox(0,0,640,480,3,11,25,
     &  3,12,0,x,y,xp,yp,lv,ihv,iwv,xc,yc,irc,2,10,nbut,ixm,iym,ixmo,
     &  iymo,ixco,klm,kihm,kiwm)

	call zsymbol(x(1)+int(0.7*lv),y(1)+int(0.8*ihv),8,6,6,4,12)
	l=1
	k=1
c	call movcur(ix_down,iy_down,isizex,isizey,idy,n_rows,
c     &	idx,n_columns,icur,ibk,isym,idot,irow,icolumn)
	n_rows=3
	idy=int(4*ihv)
	ixlo=0
	iyhi=480
	ixhi=640
	call wrstring18(ixlo+16,iyhi-32,
     &   'ENTER SEQUENCE ( F1 = HELP ) :',14,8)
	ixw=ixlo+16
	iyw=iyhi-48
	isol=1
		ixloh=ixlo+50     !position for help box
		iyhih=iyhi-50
		if(ixhi.gt.639-50) ixloh=ixlo-50
c		if(iylo.lt.50) iyhih=iyhi+50
		if(iyhih.gt.479) iyhih=470

100	continue

	a=getch(ktype)
	ival=ichar(a)
      if(ktype.eq.0.and.ival.eq.59) then   !help
		call jSYMBOL(x,y,isym,csize,icol,idev)
		call fillcircle(ixco,yc(15),irc,7,10)
		call fillwin(ixm,iym(2),ixm+klm+int(kiwm/2),
     &      iym(2)+kihm+int(kiwm/2),3)
		call drawrec(ixm+int(kiwm/2),iym(2)+int(kiwm/2),
     &  	ixm+klm+int(kiwm/2),iym(2)+kihm+int(kiwm/2),11,15)
		call wrstring18(ixm+int(klm/2),iym(2)+int(kihm/2),
     &	'Help',8,15)
		call wrstring18(ixm+int(klm/2)+8,iym(2)+int(kihm/2)+16,
     &      '?',12,15)
		call POPTEXT(ixloh,-1,iyhih,helpst,nhelp1,14,1,15)	!=popkeys
		call fillcircle(ixco,yc(15),irc,7,8)
	      call draw_valve(ixm,iym(2),klm,kihm,kiwm,15,11,8)
		call wrstring18(ixm+klm/2-8,iym(2)+kihm/2-16,'Help',8,15)
		call wrstring18(ixm+int(klm/2),iym(2)+int(kihm/2),
     &      '?',12,15)
		goto 100
	else if(ktype.eq.4.and.ival.eq.13) then		!enter
		call fillcircle(ixco,yc(25),irc,7,8)
		call fillcircle(ixco,yc(15),irc,7,8)
	   goto 101
	else if(ival.eq.27) then		!ESC
	   goto 102

	else if(ktype.eq.8) then		!up/down arrow to scroll highlight
		call fillcircle(ixco,yc(15),irc,7,8)
	   if (ival.eq.83) then 		!delete
		if (isol.lt.2) goto 100
		call timer(it1)
	      call fillwin(ixlo+16,iyw,ixhi-16,iyw+16,8)
	      ixw=ixlo+16
		isol=1
	      do i=1,25
		   call fillcircle(xc(i),yc(i),irc,7,8)
	      enddo
		call fillwin(ixm,iym(3),ixm+klm+int(kiwm/2),
     &      iym(3)+kihm+int(kiwm/2),3)
		call drawrec(ixm+int(kiwm/2),iym(3)+int(kiwm/2),
     &  	ixm+klm+int(kiwm/2),iym(3)+kihm+int(kiwm/2),11,15)
		call fillcircle(ixco,yc(25),irc,7,10)
		call wrstring18(ixm+klm/2-8,iym(3)+kihm/2+16,'Cancel',8,15)
		call wrstring18(ixm+int(klm/2)+8,iym(3)+int(kihm/2)+24,
     &      'X',4,15)
88		call timer(it2)
		idt=it2-it1
		if(idt.lt.30) goto 88
		call fillcircle(ixco,yc(25),irc,7,8)
	      call draw_valve(ixm,iym(3),klm,kihm,kiwm,15,11,8)
		call wrstring18(ixm+klm/2-16,iym(3)+kihm/2,'Cancel',8,15)
		call wrstring18(ixm+int(klm/2),iym(3)+int(kihm/2)+8,
     &      'X',4,15)
	      goto 100
	   endif
	   if (ival.eq.79) then     !start
		if(isol.gt.1) goto 103
	      goto 100
	   endif
	   call movcross(x(1)+int(0.7*lv),y(1)+int(0.8*ihv),6,6,idy,
     &	n_rows,int(1.45*lv),10,12,11,8,1,l,k,ival)
		goto 100
	else
		goto 100
	endif
101	if (l.le.3.and.k.le.9) then
	    msol(isol)=k+(l-1)*9
	    if(msol(isol).le.25) then
		 call intconv(msol(isol),cnum)
		 call wrstring18(ixw,iyw,cnum,14,8)
		 call fillcircle(xc(msol(isol)),yc(msol(isol)),irc,
     &       7,14)
		 ixw=ixw+24
		 isol=isol+1
	    endif
	else
	    if(l.eq.2) then    		!help
		call fillcircle(ixco,yc(15),irc,7,10)
		call fillwin(ixm,iym(2),ixm+klm+int(kiwm/2),
     &      iym(2)+kihm+int(kiwm/2),3)
		call drawrec(ixm+int(kiwm/2),iym(2)+int(kiwm/2),
     &  	ixm+klm+int(kiwm/2),iym(2)+kihm+int(kiwm/2),11,15)
		call wrstring18(ixm+int(klm/2),iym(2)+int(kihm/2),
     &	'Help',8,15)
		call wrstring18(ixm+int(klm/2)+8,iym(2)+int(kihm/2)+16,
     &      '?',12,15)
		call POPTEXT(ixloh,-1,iyhih,helpst,nhelp1,14,1,15)	!=popkeys
		call fillcircle(ixco,yc(15),irc,7,8)
	      call draw_valve(ixm,iym(2),klm,kihm,kiwm,15,11,8)
		call wrstring18(ixm+klm/2-8,iym(2)+kihm/2-16,'Help',8,15)
		call wrstring18(ixm+int(klm/2),iym(2)+int(kihm/2),
     &      '?',12,15)
	    else if(l.eq.3) then      !cancel
		if(isol.lt.2) goto 100
		call timer(it1)
	      call fillwin(ixlo+16,iyw,ixhi-16,iyw+16,8)
	      ixw=ixlo+16
		isol=1
	      do i=1,25
		   call fillcircle(xc(i),yc(i),irc,7,8)
	      enddo
		call fillcircle(ixco,yc(25),irc,7,10)
		call fillwin(ixm,iym(3),ixm+klm+int(kiwm/2),
     &      iym(3)+kihm+int(kiwm/2),3)
		call drawrec(ixm+int(kiwm/2),iym(3)+int(kiwm/2),
     &  	ixm+klm+int(kiwm/2),iym(3)+kihm+int(kiwm/2),11,15)
		call wrstring18(ixm+klm/2-8,iym(3)+kihm/2+16,'Cancel',8,15)
		call wrstring18(ixm+int(klm/2)+8,iym(3)+int(kihm/2)+24,
     &      'X',12,15)
887		call timer(it2)
		idt=it2-it1
		if(idt.lt.30) goto 887
		call fillcircle(ixco,yc(25),irc,7,8)
	      call draw_valve(ixm,iym(3),klm,kihm,kiwm,15,11,8)
		call wrstring18(ixm+klm/2-16,iym(3)+kihm/2,'Cancel',8,15)
		call wrstring18(ixm+int(klm/2),iym(3)+int(kihm/2)+8,
     &      'X',12,15)
	    else
		 if (isol.gt.1) goto 103
	    endif
	endif
	goto 100
103	continue
	nsol=isol-1
	isol=1
	call wrstring18(ixlo+16,iyw-16,'START PROGRAM : ',10,8)
	iyw=iyw-32
	ixw=ixlo+16
      call intconv(msol(isol),cnum)
      call wrstring18(ixw,iyw,cnum,10,8)
	ixw=ixw+24
	call fillcircle(ixco,yc(1),irc,7,10)
	call fillwin(x(msol(isol)),y(msol(isol)),x(msol(isol))+lv+
     &int(iwv/2),y(msol(isol))+ihv+int(iwv/2),11)
c	pause
	call drawframe(x(msol(isol))+int(iwv/2),y(msol(isol))+int(iwv/2),
     &x(msol(isol))+lv+int(iwv/2),y(msol(isol))+ihv+int(iwv/2),3)
	call fillcircle(xc(msol(isol)),yc(msol(isol)),irc,7,10)
      call intconv(msol(isol),cnum)
	call wrstring18(x(msol(isol))+lv/2+8,y(msol(isol))+ihv,
     & cnum,0,11)
	iakx=ixm+klm+int(kiwm/2)
	iaky=iym(1)+kihm+int(kiwm/2)
	call fillwin(ixm,iym(1),iakx,iaky,3)
	call drawrec(ixm+int(kiwm/2),iym(1)+int(kiwm/2),
     &  ixm+klm+int(kiwm/2),iym(1)+kihm+int(kiwm/2),11,15)
	call wrstring18(ixm+int(klm/2),iym(1)+int(kihm/2),'Start',8,15)

c	call zbar(x(msol(isol)),y(msol(isol)),
c     &	lv,ihv,int(iwv/2),11,3)





102	a=getch(b)
	call clears(0,480,0)
99	end
